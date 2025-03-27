import gleam/list
import gleam/set.{type Set}

import bibi/bitboard as b

pub const connect_4_width = 7

pub const connect_4_height = 6

pub type Turn {
  Red
  Yellow
}

pub type Player {
  Player(turn: Turn, board: b.Bitboard)
}

pub type GameState {
  Win(t: Turn)
  Draw
  Continue
}

pub type Game {
  Game(active: Player, inactive: Player, state: GameState)
}

type MoveBitboard =
  b.Bitboard

pub fn available_moves(full_board: b.Bitboard) -> Set(Int) {
  list.range(0, connect_4_width - 1)
  |> list.fold(set.new(), fn(moves, i) {
    let assert Ok(mask) = b.file(full_board, i)
    let assert Ok(file) = b.bitboard_and(mask, full_board)
    case mask == file {
      True -> moves
      False -> set.insert(moves, i)
    }
  })
}

pub fn column_to_move(game: Game, column: Int) -> MoveBitboard {
  let assert Ok(full_board) =
    b.bitboard_or(game.active.board, game.inactive.board)
  let assert Ok(mask) = b.file(full_board, column)
  let assert Ok(col) = b.bitboard_and(full_board, mask)
  let assert Ok(empty_slots) = b.bitboard_xor(col, mask)
  let assert Ok(square) = empty_slots |> b.to_squares |> list.last
  let assert Ok(move) = b.from_square(connect_4_width, connect_4_height, square)
  move
}

fn check_consecutive(
  bitboard,
  shift: fn(b.Bitboard, Int) -> Result(b.Bitboard, String),
  iterations: Int,
) -> Bool {
  let final_board =
    list.range(0, iterations - 1)
    |> list.fold(bitboard, fn(board, i) {
      let assert Ok(shifted_board) = shift(bitboard, i)
      let assert Ok(board) = b.bitboard_and(board, shifted_board)
      board
    })
  final_board.val > 0
}

fn check_win(board: b.Bitboard) -> Bool {
  [b.shift_north, b.shift_east, b.shift_northeast, b.shift_northwest]
  |> list.map(fn(shift) { check_consecutive(board, shift, 4) })
  |> list.any(fn(bool) { bool })
}

fn check_draw(active: Player, inactive: Player) -> Bool {
  let assert Ok(board) = b.bitboard_or(active.board, inactive.board)
  let full_mask = b.full_mask(active.board)
  full_mask == board
}

pub fn check_game_state(active: Player, inactive: Player) -> GameState {
  case check_win(active.board) {
    True -> Win(active.turn)
    False ->
      case check_draw(active, inactive) {
        True -> Draw
        False -> Continue
      }
  }
}

pub type LastCellUpdated =
  Int

pub fn update_game(game: Game, column: Int) -> #(Game, LastCellUpdated) {
  let assert Ok(full_board) =
    b.bitboard_or(game.active.board, game.inactive.board)

  let moves = available_moves(full_board)
  let is_legal = set.contains(moves, column)
  case is_legal {
    True -> {
      let move = column_to_move(game, column)
      let assert Ok(updated_board) = b.bitboard_or(move, game.active.board)
      let active = Player(..game.active, board: updated_board)
      let updated_game =
        Game(game.inactive, active, check_game_state(active, game.inactive))
      let assert Ok(last_cell_updated) = move |> b.to_squares |> list.last
      #(updated_game, last_cell_updated)
    }
    False -> #(game, -1)
  }
}
