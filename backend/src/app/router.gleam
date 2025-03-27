import gleam/dynamic/decode
import gleam/http.{Get, Options, Post}
import gleam/int
import gleam/json
import gleam/list
import gleam/order
import gleam/set
import gleam/string

import bibi/bitboard as b
import cors_builder as cors
import wisp.{type Request, type Response}

import shared.{
  type Game, type GameState, type Turn, Continue, Draw, Game, Player, Red, Win,
  Yellow, available_moves, check_game_state, connect_4_height, connect_4_width,
  update_game,
}

fn allow_cors() {
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
    Post, ["move"] -> handle_move(req, 5)
    Post, ["minimax", depth] -> {
      case int.parse(depth) {
        Ok(parsed) -> handle_move(req, parsed)
        Error(_) -> wisp.not_found()
      }
    }
    Options, ["move"] ->
      wisp.json_response(json.to_string_tree(json.string("pong")), 200)
    _, _ -> wisp.not_found()
  }
}

pub fn game_model_decoder() -> decode.Decoder(Game) {
  use red <- decode.field("red", decode.int)
  use yellow <- decode.field("yellow", decode.int)
  use active <- decode.field("play_for", decode.string)
  // The following bitboard creation should succeed
  let assert Ok(empty_board) = b.new(connect_4_width, connect_4_height)
  let x_bitboard = b.Bitboard(connect_4_width, connect_4_height, red)
  let o_bitboard = b.Bitboard(connect_4_width, connect_4_height, yellow)

  let red_player = Player(Red, x_bitboard)
  let yellow_player = Player(Yellow, o_bitboard)

  case string.lowercase(active) {
    "red" ->
      decode.success(Game(
        red_player,
        yellow_player,
        check_game_state(red_player, yellow_player),
      ))
    "yellow" ->
      decode.success(Game(
        yellow_player,
        red_player,
        check_game_state(yellow_player, red_player),
      ))

    _ -> {
      decode.failure(
        Game(Player(Red, empty_board), Player(Yellow, empty_board), Continue),
        "GameModel",
      )
    }
  }
}

fn handle_move(req: Request, depth: Int) -> Response {
  use req_body <- wisp.require_json(req)
  let assert Ok(game) = decode.run(req_body, game_model_decoder())
  let move = minimax(game, depth)

  let resp_body =
    [#("move", json.int(move))] |> json.object |> json.to_string_tree

  wisp.json_response(resp_body, 200)
}

type Move =
  Int

/// strategies
fn score(game_state: GameState, depth: Int) -> Int {
  case game_state {
    Win(_) -> int.max(10 - depth, 0)
    _ -> 0
  }
}

fn minimax(game: Game, max_depth: Int) {
  minimax_iter(game, 0, game.active.turn, max_depth).0
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
  player: Turn,
  max_depth: Int,
) -> #(Move, Int) {
  // Get the score of each move

  let assert Ok(full_board) =
    b.bitboard_or(game.active.board, game.inactive.board)
  let moves_and_scores =
    available_moves(full_board)
    |> set.to_list
    |> list.fold([], fn(scores, move) {
      let #(updated_game, _) = update_game(game, move)
      let has_game_ended = check_game_state(game.active, game.inactive)

      let score = case has_game_ended {
        Win(winner) ->
          case winner == player {
            True -> score(has_game_ended, depth)
            False -> -score(has_game_ended, depth)
          }
        Draw -> 0
        Continue ->
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
      case player == game.active.turn {
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
