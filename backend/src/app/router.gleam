import gleam/dynamic/decode
import gleam/http.{Get, Options, Post}
import gleam/int
import gleam/json
import gleam/list
import gleam/order
import gleam/string

import cors_builder as cors
import wisp.{type Request, type Response}

import bibi/bitboard as b

pub const connect_4_width = 7

pub const connect_4_height = 6

pub fn allow_cors() {
  cors.new()
  |> cors.allow_origin("http://localhost:1234")
  |> cors.allow_method(Get)
  |> cors.allow_method(Post)
  |> cors.allow_header("content-type")
}

pub fn handle_request(req: Request) -> Response {
  use <- wisp.log_request(req)
  use req <- cors.wisp_middleware(req, allow_cors())

  case req.method, wisp.path_segments(req) {
    Get, ["ping"] ->
      wisp.json_response(json.to_string_tree(json.string("pong")), 200)
    Post, ["move"] -> handle_move(req)
    Options, ["move"] ->
      wisp.json_response(json.to_string_tree(json.string("pong")), 200)
    _, _ -> wisp.not_found()
  }
}

pub type Turn {
  X(b: b.Bitboard)
  O(b: b.Bitboard)
}

pub type Game {
  Game(active: Turn, inactive: Turn)
}

pub fn game_model_decoder() -> decode.Decoder(Game) {
  use x <- decode.field("x", decode.int)
  use o <- decode.field("o", decode.int)
  use active <- decode.field("play_for", decode.string)
  // The following bitboard creation should succeed
  let assert Ok(empty_board) = b.new(connect_4_width, connect_4_height)
  let x_bitboard = b.Bitboard(connect_4_width, connect_4_height, x)
  let o_bitboard = b.Bitboard(connect_4_width, connect_4_height, o)

  case string.uppercase(active) {
    "X" -> decode.success(Game(X(x_bitboard), O(o_bitboard)))
    "O" -> decode.success(Game(O(o_bitboard), X(x_bitboard)))

    _ -> decode.failure(Game(X(empty_board), O(empty_board)), "GameModel")
  }
}

fn handle_move(req: Request) -> Response {
  use req_body <- wisp.require_json(req)
  let assert Ok(game) = decode.run(req_body, game_model_decoder())
  let move = minimax(game, 5)

  let resp_body =
    [#("move", json.int(move))] |> json.object |> json.to_string_tree

  wisp.json_response(resp_body, 200)
}

type Move =
  Int

fn available_moves(game: Game) -> List(Move) {
  let assert Ok(full_board) = b.bitboard_or(game.active.b, game.inactive.b)
  let moves =
    list.range(0, connect_4_width - 1)
    |> list.fold([], fn(moves, i: Int) {
      let assert Ok(mask) = b.file(game.active.b, i)
      let assert Ok(file) = b.bitboard_and(full_board, mask)
      case file != mask {
        True -> list.append(moves, [i])
        False -> moves
      }
    })
  moves
}

pub fn random(game: Game) -> Move {
  let assert Ok(next_move) = available_moves(game) |> list.shuffle |> list.first
  next_move
}

fn check_consecutive_pieces_in_one_direction(
  turn: Turn,
  shift: fn(b.Bitboard, Int) -> Result(b.Bitboard, String),
  iterations: Int,
) -> Bool {
  let final_board =
    list.range(0, iterations - 1)
    |> list.fold(turn.b, fn(board, i) {
      let assert Ok(shifted_board) = shift(turn.b, i)
      let assert Ok(board) = b.bitboard_and(board, shifted_board)
      board
    })
  final_board.val > 0
}

fn check_win(turn: Turn) -> Bool {
  [b.shift_east, b.shift_north, b.shift_northeast, b.shift_northwest]
  |> list.map(fn(shift) {
    check_consecutive_pieces_in_one_direction(turn, shift, 4)
  })
  |> list.any(fn(b) { b })
}

pub type GameEnd {
  Winner(game: Game, t: Turn)
  Draw(game: Game)
  NotEnded(game: Game)
}

fn has_ended(game: Game) -> GameEnd {
  let active_is_winner = check_win(game.active)

  let inactive_is_winner = check_win(game.inactive)

  case active_is_winner, inactive_is_winner {
    True, False -> Winner(game, game.active)
    False, True -> Winner(game, game.inactive)
    _, _ ->
      case available_moves(game) {
        [] -> Draw(game)
        _ -> NotEnded(game)
      }
  }
}

pub fn update_turn(g: Game, next_move: Move) {
  let assert Ok(full_board) = b.bitboard_or(g.active.b, g.inactive.b)
  let assert Ok(mask) = b.file(full_board, next_move)
  let assert Ok(col) = b.bitboard_and(full_board, mask)
  let assert Ok(empty_slots) = b.bitboard_xor(col, mask)
  let assert Ok(square) = empty_slots |> b.to_squares |> list.last

  let assert Ok(bitboard_of_move) =
    b.from_square(connect_4_width, connect_4_height, square)
  let assert Ok(updated_bitboard) = b.bitboard_or(g.active.b, bitboard_of_move)

  case g.active {
    X(_) -> X(updated_bitboard)
    O(_) -> O(updated_bitboard)
  }
}

fn update_game(game: Game, move: Move) {
  let updated_turn = update_turn(game, move)
  Game(active: game.inactive, inactive: updated_turn)
}

/// strategies
fn score(game_end: GameEnd, depth: Int) -> Int {
  case game_end {
    NotEnded(_) | Draw(_) -> 0
    Winner(_, _) -> int.max(10 - depth, 0)
  }
}

fn minimax(game: Game, max_depth: Int) {
  minimax_iter(game, 0, to_char(game.active), max_depth).0
}

fn compare_moves(move1: #(Move, Int), move2: #(Move, Int)) -> order.Order {
  case move1.1 == move2.1 {
    True -> order.Eq
    False ->
      case move1.1 < move2.1 {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

fn minimax_iter(
  game: Game,
  depth: Int,
  player: String,
  max_depth: Int,
) -> #(Move, Int) {
  // Get the score of each move

  let moves_and_scores =
    available_moves(game)
    |> list.fold([], fn(scores, move) {
      let updated_game = update_game(game, move)
      let has_game_ended = has_ended(updated_game)

      let score = case has_game_ended {
        Winner(_, winner) ->
          case to_char(winner) == player {
            True -> score(has_game_ended, depth)
            False -> -score(has_game_ended, depth)
          }
        Draw(_) -> 0
        NotEnded(_) ->
          case depth >= max_depth {
            False -> minimax_iter(updated_game, depth + 1, player, max_depth).1
            True -> 0
          }
      }
      [#(move, score), ..scores]
    })
  let assert Ok(best_move) =
    moves_and_scores
    |> list.max(fn(move1, move2) {
      case player == to_char(game.active) {
        True -> compare_moves(move1, move2)
        False -> order.negate(compare_moves(move1, move2))
      }
    })

  let assert Ok(random_best_move) =
    moves_and_scores
    |> list.filter(fn(move_and_score) { move_and_score.1 == best_move.1 })
    |> list.shuffle
    |> list.first

  random_best_move
}

pub fn to_char(turn: Turn) -> String {
  case turn {
    X(_) -> "X"
    O(_) -> "O"
  }
}
