import gleam/list
import gleam/set.{type Set}

import lustre
import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event

import bibi/bitboard as b

const connect_4_width = 7

const connect_4_height = 6

pub fn main() {
  let app = lustre.simple(new, update, game)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

//  model

type Turn {
  X
  O
}

type TurnState {
  TurnState(turn: Turn, board: b.Bitboard)
}

type GameState {
  Win(t: Turn)
  Draw
  Continue
}

type GameModel {
  GameModel(active: TurnState, inactive: TurnState, state: GameState)
}

fn new(_) -> GameModel {
  let bitboard = b.new(connect_4_width, connect_4_height)
  case bitboard {
    Ok(board) ->
      GameModel(
        active: TurnState(X, board),
        inactive: TurnState(O, board),
        state: Continue,
      )
    // This should never happen unless the connect_4 width and heights are wrongly configured
    Error(error) -> panic as error
  }
}

//  message

type GameMessage {
  Move(column: Int)
  Restart
}

fn available_moves(full_board: b.Bitboard) -> Set(Int) {
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

type MoveBitboard =
  b.Bitboard

fn get_move(model: GameModel, column: Int) -> MoveBitboard {
  let assert Ok(full_board) =
    b.bitboard_or(model.active.board, model.inactive.board)
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

fn update(model: GameModel, msg: GameMessage) -> GameModel {
  case msg {
    Move(column) -> {
      let assert Ok(full_board) =
        b.bitboard_or(model.active.board, model.inactive.board)

      let moves = available_moves(full_board)
      let is_legal = set.contains(moves, column)
      case is_legal {
        True -> {
          let move = get_move(model, column)
          let assert Ok(updated_board) = b.bitboard_or(move, model.active.board)
          let assert Ok(updated_full_board) =
            b.bitboard_or(updated_board, model.inactive.board)

          case
            check_win(updated_board),
            set.size(available_moves(updated_full_board))
          {
            True, _ ->
              GameModel(
                ..model,
                active: TurnState(turn: model.active.turn, board: updated_board),
                state: Win(model.active.turn),
              )
            False, 0 -> GameModel(..model, state: Draw)
            False, _ ->
              GameModel(
                active: model.inactive,
                inactive: TurnState(
                  turn: model.active.turn,
                  board: updated_board,
                ),
                state: Continue,
              )
          }
        }
        False -> model
      }
    }
    Restart -> new(msg)
  }
}

// view

fn game(model: GameModel) -> element.Element(_) {
  html.div([attribute.class("game")], [
    header(model),
    move_picker(model),
    board(model),
  ])
}

fn header(model: GameModel) -> element.Element(_) {
  let class = case model.state {
    Win(_) -> "win"
    Draw -> "draw"
    Continue -> "continue"
  }
  let text = case model.state {
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
      case model.active.turn {
        O -> "Yellow's turn"
        X -> "Red's turn"
      }
  }
  html.div([attribute.class("header" <> " " <> class)], [
    element.text(text),
    html.button([event.on_click(Restart)], [element.text("restart")]),
  ])
}

fn move_picker(model: GameModel) -> element.Element(_) {
  let assert Ok(full_board) =
    b.bitboard_or(model.active.board, model.inactive.board)
  let moves = case model.state {
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
          event.on_click(Move(i)),
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

fn board(model: GameModel) -> element.Element(_) {
  let active_board = convert_bitboard_to_set(model.active.board)
  let inactive_board = convert_bitboard_to_set(model.inactive.board)
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
            True, False -> turn_to_color(model.active.turn)
            False, True -> turn_to_color(model.inactive.turn)
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
