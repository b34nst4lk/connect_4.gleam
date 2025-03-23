import gleam/int
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

type State {
  Win(t: Turn)
  Draw
  Continue
}

type GameModel {
  GameModel(x: b.Bitboard, o: b.Bitboard, active: Turn, state: State)
}

fn new(_) -> GameModel {
  let assert Ok(bitboard) = b.new(connect_4_width, connect_4_height)
  GameModel(bitboard, bitboard, X, Continue)
}

//  message

type GameMessage {
  Move(column: Int)
}

fn available_moves(model: GameModel) -> Set(Int) {
  let assert Ok(full_board) = b.bitboard_or(model.x, model.o)
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
  let assert Ok(full_board) = b.bitboard_or(model.x, model.o)
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
  let moves = available_moves(model)
  let is_legal = set.contains(moves, msg.column)
  case is_legal {
    True -> {
      let move = get_move(model, msg.column)
      let updated_game = case model.active {
        X -> {
          let assert Ok(updated_board) = b.bitboard_or(move, model.x)
          let winner = case check_win(updated_board) {
            True -> Win(X)
            False -> Continue
          }
          GameModel(..model, x: updated_board, active: O, state: winner)
        }
        O -> {
          let assert Ok(updated_board) = b.bitboard_or(move, model.o)
          let state = case check_win(updated_board) {
            True -> Win(O)
            False -> Continue
          }
          GameModel(..model, o: updated_board, active: X, state: state)
        }
      }
      case updated_game.state {
        Win(_) -> updated_game
        _ ->
          case set.size(available_moves(updated_game)) {
            0 -> GameModel(..updated_game, state: Draw)
            _ -> updated_game
          }
      }
    }
    False -> model
  }
}

// view

fn game(model: GameModel) -> element.Element(_) {
  html.div([attribute.class("game")], [
    end_game(model),
    move_picker(model),
    board(model),
  ])
}

fn end_game(model: GameModel) -> element.Element(_) {
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
      case model.active {
        O -> "Yellow's turn"
        X -> "Red's turn"
      }
  }
  html.div([attribute.class(class)], [element.text(text)])
}

fn move_picker(model: GameModel) -> element.Element(_) {
  let moves = case model.state {
    Win(_) -> set.new()
    Draw -> set.new()
    _ -> available_moves(model)
  }

  let buttons =
    list.range(0, connect_4_width - 1)
    |> list.map(fn(i) {
      html.button(
        [event.on_click(Move(i)), attribute.disabled(!set.contains(moves, i))],
        [element.text(int.to_string(i))],
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

fn board(model: GameModel) -> element.Element(_) {
  let x = convert_bitboard_to_set(model.x)
  let o = convert_bitboard_to_set(model.o)
  let board_rows =
    list.range(connect_4_height - 1, 0)
    |> list.fold([], fn(cells, i) {
      let row =
        list.range(0, connect_4_width - 1)
        |> list.map(fn(j) {
          let cell_id = i * connect_4_width + j
          let color = case set.contains(x, cell_id), set.contains(o, cell_id) {
            True, False -> "red"
            False, True -> "yellow"
            _, _ -> "white"
          }
          html.div([attribute.class("cell")], [
            html.div([attribute.class("circle" <> " " <> color)], [
              element.text(int.to_string(cell_id)),
            ]),
          ])
        })
      list.append(cells, row)
    })
  html.div([attribute.class("board")], board_rows)
}
