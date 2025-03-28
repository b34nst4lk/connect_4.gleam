import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/set

import bibi/bitboard as b
import lustre/attribute
import lustre/effect
import lustre/element
import lustre/element/html
import shared.{type Turn, Red, Yellow, connect_4_height, connect_4_width}

import backend_api.{get_move_api}
import models.{type GameModel, AI, new_game}

// Model

pub type Model {
  Model(games: Dict(Int, GameModel))
}

pub fn new(red: String, yellow: String, number_of_games: String) -> Model {
  let assert Ok(number_of_games) = int.parse(number_of_games)
  list.range(1, number_of_games)
  |> list.fold(dict.new(), fn(acc, i) {
    dict.insert(acc, i, new_game(AI(red), AI(yellow)))
  })
  |> Model
}

pub fn init_get_first_moves(red: String, model: Model) {
  model.games
  |> dict.to_list
  |> list.map(fn(game_tuple) {
    let #(game_id, g) = game_tuple
    get_move_api(g, red, game_id)
  })
  |> effect.batch
}

// View
pub fn view(model: Model) {
  html.div([attribute.class("ai-battle")], games(model))
}

fn games(model: Model) {
  model.games
  |> dict.to_list
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  |> list.map(fn(game_tuple) {
    let #(game_id, g) = game_tuple
    game(g, game_id)
  })
}

fn game(g: GameModel, game_id: Int) -> element.Element(_) {
  html.div([attribute.class("vstack")], [
    html.h2([], [element.text(int.to_string(game_id))]),
    board(g),
  ])
}

fn convert_bitboard_to_set(bitboard: b.Bitboard) {
  set.from_list(b.to_squares(bitboard))
}

fn turn_to_color(t: Turn) -> String {
  case t {
    Red -> "red"
    Yellow -> "yellow"
  }
}

pub fn board(model: GameModel) -> element.Element(_) {
  let active_board = convert_bitboard_to_set(model.game.active.board)
  let inactive_board = convert_bitboard_to_set(model.game.inactive.board)
  let board_rows =
    list.range(connect_4_height - 1, 0)
    |> list.fold([], fn(cells, i) {
      let row =
        list.range(0, connect_4_width - 1)
        |> list.map(fn(j) {
          let cell_id = i * connect_4_width + j
          let color = case
            set.contains(active_board, cell_id),
            set.contains(inactive_board, cell_id)
          {
            True, False -> turn_to_color(model.game.active.turn)
            False, True -> turn_to_color(model.game.inactive.turn)
            _, _ -> "white"
          }
          let text = case dict.get(model.move_history, cell_id) {
            Ok(log) -> int.to_string(log.move_count)
            Error(_) -> ""
          }
          let cell_attributes = [attribute.class("cell")]
          html.div(cell_attributes, [
            html.div([attribute.class("circle" <> " " <> color)], [
              element.text(text),
            ]),
          ])
        })
      list.append(cells, row)
    })
  html.div([attribute.class("board")], board_rows)
}
