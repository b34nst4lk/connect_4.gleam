import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/order
import gleam/set

import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event
import lustre_http

import bibi/bitboard as b

import shared.{
  type Turn, Continue, Draw, Red, Win, Yellow, connect_4_height, connect_4_width,
  update_game,
}

import models.{
  type DebugLog, type GameModel, DebugLog, GameModel, GotoMainMenu,
  HighlightColumn, Human, Move, NewGame, ReceivedMove, UnhighlightColumn,
  get_active_player_type,
}

// model
// Update

pub fn get_move_api(model: GameModel) {
  let url = "http://localhost:8000/move"
  // prepare json body
  let req_body =
    case model.game.active.turn {
      Red -> [
        #("red", json.int(model.game.active.board.val)),
        #("yellow", json.int(model.game.inactive.board.val)),
        #("play_for", json.string("red")),
      ]
      Yellow -> [
        #("red", json.int(model.game.inactive.board.val)),
        #("yellow", json.int(model.game.active.board.val)),
        #("play_for", json.string("yellow")),
      ]
    }
    |> json.object

  // prepare decoder
  let decoder = {
    use move <- decode.field("move", decode.int)
    decode.success(move)
  }
  lustre_http.post(
    url,
    req_body,
    lustre_http.expect_json(decoder, ReceivedMove),
  )
}

pub fn update_model(model: GameModel, column: Int) -> GameModel {
  let #(updated_game, last_cell_updated) = update_game(model.game, column)
  let has_game_changed = updated_game != model.game
  case has_game_changed {
    True ->
      GameModel(
        ..model,
        game: updated_game,
        move_counter: model.move_counter + 1,
        move_history: dict.insert(
          model.move_history,
          last_cell_updated,
          DebugLog(
            model.move_counter,
            model.game.active.turn,
            updated_game.state,
          ),
        ),
        highlight_column: model.highlight_column,
      )
    False -> model
  }
}

pub fn update_highlighted_column(model: GameModel, column: Int) {
  GameModel(..model, highlight_column: column)
}

pub fn update_clear_highlighted_column(model: GameModel) {
  GameModel(..model, highlight_column: -1)
}

// View
pub fn view(model: GameModel) -> element.Element(_) {
  html.div([attribute.class("game")], [
    html.h1([], [element.text("VS AI")]),
    header(model),
    board(model),
    debug_log(model),
  ])
}

fn header(model: GameModel) -> element.Element(_) {
  let class = case model.game.state {
    Win(_) -> "win"
    Draw -> "draw"
    Continue -> "continue"
  }
  let text = case model.game.state {
    Win(winner) ->
      "Winner is "
      <> {
        case winner {
          Red -> "Red"
          Yellow -> "Yellow"
        }
      }
    Draw -> "Draw"
    Continue ->
      case model.game.active.turn {
        Red -> "Red's turn"
        Yellow -> "Yellow's turn"
      }
  }
  html.div([attribute.class("header" <> " " <> class)], [
    element.text(text),
    html.div([], [
      html.button([event.on_click(NewGame(model.player_types))], [
        element.text("Restart"),
      ]),
      html.button([event.on_click(GotoMainMenu)], [element.text("Main menu")]),
    ]),
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
          let cell_attributes = [
            attribute.class("cell"),
            event.on_mouse_over(HighlightColumn(j)),
            event.on_mouse_leave(UnhighlightColumn),
          ]
          let cell_attributes = case model.highlight_column == j {
            True -> list.append(cell_attributes, [attribute.class("highlight")])
            False -> cell_attributes
          }
          let cell_attributes = case
            get_active_player_type(model) == Human
            && model.game.state == Continue
          {
            True -> list.append(cell_attributes, [event.on_click(Move(j))])
            False -> cell_attributes
          }
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

fn turn_to_string(t: Turn) {
  case t {
    Red -> "Red"
    Yellow -> "Yellow"
  }
}

fn format_log(square: Int, log: DebugLog) {
  let square = int.to_string(square)
  let state = case log.state {
    Win(turn) -> turn_to_string(turn) <> ": wins"
    Draw -> "draw"
    Continue -> "continue"
  }
  let move_count = int.to_string(log.move_count)
  let turn = turn_to_string(log.turn)

  move_count <> " | " <> square <> " | " <> turn <> " | " <> state
}

fn debug_log(model: GameModel) -> element.Element(_) {
  let logs =
    model.move_history
    |> dict.to_list
    |> list.sort(fn(a, b) {
      order.negate(int.compare({ a.1 }.move_count, { b.1 }.move_count))
    })
    |> list.map(fn(log) {
      html.li([], [element.text(format_log(log.0, log.1))])
    })

  html.ul([], logs)
}
