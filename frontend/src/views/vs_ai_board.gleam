import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/set

import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event
import lustre_http

import bibi/bitboard as b

import logic.{
  available_moves, check_win, connect_4_height, connect_4_width, get_move,
}

import messages as msg

// model

pub type PlayerType {
  Human
  AI
}

pub type Turn {
  X(player: PlayerType)
  O(player: PlayerType)
}

pub type TurnState {
  TurnState(turn: Turn, board: b.Bitboard)
}

pub type GameState {
  Win(t: Turn)
  Draw
  Continue
}

pub type DebugLog {
  DebugLog(move_count: Int, turn: Turn, state: GameState)
}

pub type Model {
  Model(
    active: TurnState,
    inactive: TurnState,
    state: GameState,
    move_counter: Int,
    move_history: Dict(Int, DebugLog),
  )
}

pub fn new() -> Model {
  let assert Ok(bitboard) = b.new(connect_4_width, connect_4_height)
  Model(
    TurnState(X(Human), bitboard),
    TurnState(O(AI), bitboard),
    Continue,
    0,
    dict.new(),
  )
}

// Update

pub fn get_move_api(model: Model) {
  let url = "http://localhost:8000/move"
  // prepare json body
  let req_body =
    case model.active.turn {
      X(_) -> [
        #("x", json.int(model.active.board.val)),
        #("o", json.int(model.inactive.board.val)),
        #("play_for", json.string("x")),
      ]
      O(_) -> [
        #("x", json.int(model.inactive.board.val)),
        #("o", json.int(model.active.board.val)),
        #("play_for", json.string("o")),
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
    lustre_http.expect_json(decoder, msg.ReceivedMove),
  )
}

pub fn update_game(model: Model, column: Int) -> Model {
  let active = model.active
  let inactive = model.inactive
  let assert Ok(full_board) = b.bitboard_or(active.board, inactive.board)

  let moves = available_moves(full_board)
  let is_legal = set.contains(moves, column)
  case is_legal {
    True -> {
      let move = get_move(active.board, inactive.board, column)
      let assert Ok(updated_board) = b.bitboard_or(move, active.board)
      let assert Ok(updated_full_board) =
        b.bitboard_or(updated_board, inactive.board)
      let assert Ok(cell_id) = b.to_squares(move) |> list.first
      case
        check_win(updated_board),
        set.size(available_moves(updated_full_board))
      {
        True, _ ->
          Model(
            TurnState(turn: inactive.turn, board: inactive.board),
            TurnState(turn: active.turn, board: updated_board),
            Win(active.turn),
            model.move_counter,
            dict.insert(
              model.move_history,
              cell_id,
              DebugLog(model.move_counter, active.turn, Win(active.turn)),
            ),
          )
        False, 0 ->
          Model(
            inactive,
            TurnState(turn: active.turn, board: updated_board),
            Draw,
            model.move_counter,
            dict.insert(
              model.move_history,
              cell_id,
              DebugLog(model.move_counter, active.turn, Draw),
            ),
          )
        False, _ ->
          Model(
            inactive,
            TurnState(turn: active.turn, board: updated_board),
            Continue,
            model.move_counter + 1,
            dict.insert(
              model.move_history,
              cell_id,
              DebugLog(model.move_counter, active.turn, Continue),
            ),
          )
      }
    }
    False -> model
  }
}

// View

pub fn header(
  active: TurnState,
  _: TurnState,
  state: GameState,
) -> element.Element(_) {
  let class = case state {
    Win(_) -> "win"
    Draw -> "draw"
    Continue -> "continue"
  }
  let text = case state {
    Win(winner) ->
      "Winner is "
      <> {
        case winner {
          O(_) -> "Yellow"
          X(_) -> "Red"
        }
      }
    Draw -> "Draw"
    Continue ->
      case active.turn {
        O(_) -> "Yellow's turn"
        X(_) -> "Red's turn"
      }
  }
  html.div([attribute.class("header" <> " " <> class)], [
    element.text(text),
    html.div([], [
      html.button([event.on_click(msg.NewOnlineGame)], [element.text("Restart")]),
      html.button([event.on_click(msg.GotoMainMenu)], [
        element.text("Main menu"),
      ]),
    ]),
  ])
}

pub fn move_picker(
  active: TurnState,
  inactive: TurnState,
  state: GameState,
) -> element.Element(_) {
  let assert Ok(full_board) = b.bitboard_or(active.board, inactive.board)
  let game_ended = case state {
    Win(_) -> True
    Draw -> True
    _ -> False
  }

  let buttons =
    list.range(0, connect_4_width - 1)
    |> list.map(fn(i) {
      html.button(
        [
          attribute.class("drop-button"),
          event.on_click(msg.Move(i)),
          attribute.disabled(
            game_ended
            || !set.contains(available_moves(full_board), i)
            || active.turn.player == AI,
          ),
        ],
        [element.text(" ⬇ ")],
      )
    })

  html.div(
    [
      attribute.class("board"),
      attribute.class("move-picker"),
      attribute.class("board"),
    ],
    buttons,
  )
}

fn convert_bitboard_to_set(bitboard: b.Bitboard) {
  set.from_list(b.to_squares(bitboard))
}

fn turn_to_color(t: Turn) -> String {
  case t {
    X(_) -> "red"
    O(_) -> "yellow"
  }
}

pub fn board(
  active: TurnState,
  inactive: TurnState,
  move_history: Dict(Int, DebugLog),
) -> element.Element(_) {
  let active_board = convert_bitboard_to_set(active.board)
  let inactive_board = convert_bitboard_to_set(inactive.board)
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
            True, False -> turn_to_color(active.turn)
            False, True -> turn_to_color(inactive.turn)
            _, _ -> "white"
          }
          let text = case dict.get(move_history, cell_id) {
            Ok(log) -> int.to_string(log.move_count)
            Error(_) -> ""
          }

          html.div([attribute.class("cell")], [
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
    X(_) -> "Red"
    O(_) -> "Yellow"
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

fn debug_log(model: Model) -> element.Element(_) {
  let logs =
    model.move_history
    |> dict.to_list
    |> list.sort(fn(a, b) {
      int.compare({ a.1 }.move_count, { b.1 }.move_count)
    })
    |> list.map(fn(log) {
      html.li([], [element.text(format_log(log.0, log.1))])
    })

  html.ul([], logs)
}

pub fn view(model: Model) -> element.Element(_) {
  html.div([attribute.class("game")], [
    html.h1([], [element.text("VS AI")]),
    header(model.active, model.inactive, model.state),
    move_picker(model.active, model.inactive, model.state),
    board(model.active, model.inactive, model.move_history),
    debug_log(model),
  ])
}
