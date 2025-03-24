import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/list
import gleeunit
import gleeunit/should

import bibi/bitboard as b

import app/router as r

pub fn main() {
  gleeunit.main()
}

pub fn game_model_decoder_test() {
  let assert Ok(bitboard) = b.new(r.connect_4_width, r.connect_4_height)
  [
    #(
      1,
      1,
      "x",
      r.Game(
        r.X(b.Bitboard(..bitboard, val: 1)),
        r.O(b.Bitboard(..bitboard, val: 1)),
      ),
    ),
    #(
      2,
      1,
      "x",
      r.Game(
        r.X(b.Bitboard(..bitboard, val: 2)),
        r.O(b.Bitboard(..bitboard, val: 1)),
      ),
    ),
    #(
      2,
      1,
      "o",
      r.Game(
        r.X(b.Bitboard(..bitboard, val: 1)),
        r.O(b.Bitboard(..bitboard, val: 2)),
      ),
    ),
  ]
  |> list.map(fn(test_case) {
    let undecoded =
      dynamic.from(
        dict.from_list([
          #("x", dynamic.from(test_case.0)),
          #("o", dynamic.from(test_case.1)),
          #("play_for", dynamic.from(test_case.2)),
        ]),
      )
    let assert Ok(decoded) = decode.run(undecoded, r.game_model_decoder())
    should.equal(decoded, test_case.3)
  })
}
