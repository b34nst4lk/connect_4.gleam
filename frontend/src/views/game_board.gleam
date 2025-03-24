import gleam/list
import gleam/set

import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event

import bibi/bitboard as b

import logic.{
  available_moves, check_win, connect_4_height, connect_4_width, get_move,
}
import messages as msg

// model

pub type Turn {
  X
  O
}

pub type TurnState {
  TurnState(turn: Turn, board: b.Bitboard)
}

pub type GameState {
  Win(t: Turn)
  Draw
  Continue
}

pub type Model {
  Model(active: TurnState, inactive: TurnState, state: GameState)
}

pub fn new() -> Model {
  let assert Ok(bitboard) = b.new(connect_4_width, connect_4_height)
  Model(TurnState(X, bitboard), TurnState(O, bitboard), Continue)
}

// Update
pub fn update_game(model: Model, column: Int) -> Model {
  let active = model.active
  let inactive = model.inactive
  let state = model.state
  let assert Ok(full_board) = b.bitboard_or(active.board, inactive.board)

  let moves = available_moves(full_board)
  let is_legal = set.contains(moves, column)
  case is_legal {
    True -> {
      let move = get_move(active.board, inactive.board, column)
      let assert Ok(updated_board) = b.bitboard_or(move, active.board)
      let assert Ok(updated_full_board) =
        b.bitboard_or(updated_board, inactive.board)

      case
        check_win(updated_board),
        set.size(available_moves(updated_full_board))
      {
        True, _ ->
          Model(
            TurnState(turn: active.turn, board: updated_board),
            TurnState(turn: inactive.turn, board: inactive.board),
            Win(active.turn),
          )
        False, 0 ->
          Model(
            TurnState(turn: active.turn, board: updated_board),
            inactive,
            Draw,
          )
        False, _ ->
          Model(
            inactive,
            TurnState(turn: active.turn, board: updated_board),
            Continue,
          )
      }
    }
    False -> Model(active, inactive, state)
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
          O -> "O"
          X -> "X"
        }
      }
    Draw -> "Draw"
    Continue ->
      case active.turn {
        O -> "Yellow's turn"
        X -> "Red's turn"
      }
  }
  html.div([attribute.class("header" <> " " <> class)], [
    element.text(text),
    html.div([], [
      html.button([event.on_click(msg.NewGame)], [element.text("Restart")]),
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
  let moves = case state {
    Win(_) -> set.new()
    Draw -> set.new()
    _ -> available_moves(full_board)
  }

  let buttons =
    list.range(0, connect_4_width - 1)
    |> list.map(fn(i) {
      html.button(
        [
          attribute.class("drop-button"),
          event.on_click(msg.Move(i)),
          attribute.disabled(!set.contains(moves, i)),
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
    X -> "red"
    O -> "yellow"
  }
}

pub fn board(
  active: TurnState,
  inactive: TurnState,
  _: GameState,
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
          html.div([attribute.class("cell")], [
            html.div([attribute.class("circle" <> " " <> color)], []),
          ])
        })
      list.append(cells, row)
    })
  html.div([attribute.class("board")], board_rows)
}

pub fn view(model: Model) -> element.Element(_) {
  html.div([attribute.class("game")], [
    html.h1([], [element.text("Local play")]),
    header(model.active, model.inactive, model.state),
    move_picker(model.active, model.inactive, model.state),
    board(model.active, model.inactive, model.state),
  ])
}
