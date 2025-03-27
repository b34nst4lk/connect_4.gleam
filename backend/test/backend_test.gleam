import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/list
import gleeunit
import gleeunit/should

import bibi/bitboard as b

import app/router as r
import shared as s

pub fn main() {
  gleeunit.main()
}

pub fn game_model_decoder_test() {
  let assert Ok(bitboard) = b.new(s.connect_4_width, s.connect_4_height)
  [
    #(
      1,
      1,
      "x",
      s.Game(
        s.Player(s.Red, b.Bitboard(..bitboard, val: 1)),
        s.Player(s.Yellow, b.Bitboard(..bitboard, val: 1)),
        s.Continue,
      ),
    ),
    #(
      2,
      1,
      "x",
      s.Game(
        s.Player(s.Red, b.Bitboard(..bitboard, val: 2)),
        s.Player(s.Yellow, b.Bitboard(..bitboard, val: 1)),
        s.Continue,
      ),
    ),
    #(
      2,
      1,
      "o",
      s.Game(
        s.Player(s.Yellow, b.Bitboard(..bitboard, val: 1)),
        s.Player(s.Red, b.Bitboard(..bitboard, val: 2)),
        s.Continue,
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
