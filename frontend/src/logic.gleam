import gleam/list
import gleam/set.{type Set}

import bibi/bitboard as b

pub const connect_4_width = 7

pub const connect_4_height = 6

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

pub fn get_move(
  active: b.Bitboard,
  inactive: b.Bitboard,
  column: Int,
) -> MoveBitboard {
  let assert Ok(full_board) = b.bitboard_or(active, inactive)
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

pub fn check_win(board: b.Bitboard) -> Bool {
  [b.shift_north, b.shift_east, b.shift_northeast, b.shift_northwest]
  |> list.map(fn(shift) { check_consecutive(board, shift, 4) })
  |> list.any(fn(bool) { bool })
}
