import * as $b from "../bibi/bibi/bitboard.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $set from "../gleam_stdlib/gleam/set.mjs";
import * as $lustre from "../lustre/lustre.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import * as $html from "../lustre/lustre/element/html.mjs";
import * as $event from "../lustre/lustre/event.mjs";
import { toList, CustomType as $CustomType, makeError, isEqual } from "./gleam.mjs";

class X extends $CustomType {}

class O extends $CustomType {}

class TurnState extends $CustomType {
  constructor(turn, board) {
    super();
    this.turn = turn;
    this.board = board;
  }
}

class Win extends $CustomType {
  constructor(t) {
    super();
    this.t = t;
  }
}

class Draw extends $CustomType {}

class Continue extends $CustomType {}

class GameModel extends $CustomType {
  constructor(active, inactive, state) {
    super();
    this.active = active;
    this.inactive = inactive;
    this.state = state;
  }
}

class Move extends $CustomType {
  constructor(column) {
    super();
    this.column = column;
  }
}

class Restart extends $CustomType {}

function check_consecutive(bitboard, shift, iterations) {
  let final_board = (() => {
    let _pipe = $list.range(0, iterations - 1);
    return $list.fold(
      _pipe,
      bitboard,
      (board, i) => {
        let $ = shift(bitboard, i);
        if (!$.isOk()) {
          throw makeError(
            "let_assert",
            "connect_4",
            99,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $ }
          )
        }
        let shifted_board = $[0];
        let $1 = $b.bitboard_and(board, shifted_board);
        if (!$1.isOk()) {
          throw makeError(
            "let_assert",
            "connect_4",
            100,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $1 }
          )
        }
        let board$1 = $1[0];
        return board$1;
      },
    );
  })();
  return final_board.val > 0;
}

function check_win(board) {
  let _pipe = toList([
    $b.shift_north,
    $b.shift_east,
    $b.shift_northeast,
    $b.shift_northwest,
  ]);
  let _pipe$1 = $list.map(
    _pipe,
    (shift) => { return check_consecutive(board, shift, 4); },
  );
  return $list.any(_pipe$1, (bool) => { return bool; });
}

function header(model) {
  let class$ = (() => {
    let $ = model.state;
    if ($ instanceof Win) {
      return "win";
    } else if ($ instanceof Draw) {
      return "draw";
    } else {
      return "continue";
    }
  })();
  let text = (() => {
    let $ = model.state;
    if ($ instanceof Win) {
      let winner = $.t;
      return "Winner is " + (() => {
        if (winner instanceof O) {
          return "O";
        } else {
          return "X";
        }
      })();
    } else if ($ instanceof Draw) {
      return "Draw";
    } else {
      let $1 = model.active.turn;
      if ($1 instanceof O) {
        return "Yellow's turn";
      } else {
        return "Red's turn";
      }
    }
  })();
  return $html.div(
    toList([$attribute.class$(("header" + " ") + class$)]),
    toList([
      $element.text(text),
      $html.button(
        toList([$event.on_click(new Restart())]),
        toList([$element.text("restart")]),
      ),
    ]),
  );
}

function convert_bitboard_to_set(bitboard) {
  return $set.from_list($b.to_squares(bitboard));
}

function turn_to_color(t) {
  if (t instanceof X) {
    return "red";
  } else {
    return "yellow";
  }
}

const connect_4_width = 7;

function available_moves(full_board) {
  let _pipe = $list.range(0, connect_4_width - 1);
  return $list.fold(
    _pipe,
    $set.new$(),
    (moves, i) => {
      let $ = $b.file(full_board, i);
      if (!$.isOk()) {
        throw makeError(
          "let_assert",
          "connect_4",
          68,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $ }
        )
      }
      let mask = $[0];
      let $1 = $b.bitboard_and(mask, full_board);
      if (!$1.isOk()) {
        throw makeError(
          "let_assert",
          "connect_4",
          69,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $1 }
        )
      }
      let file = $1[0];
      let $2 = isEqual(mask, file);
      if ($2) {
        return moves;
      } else {
        return $set.insert(moves, i);
      }
    },
  );
}

function move_picker(model) {
  let $ = $b.bitboard_or(model.active.board, model.inactive.board);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "connect_4",
      195,
      "move_picker",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let full_board = $[0];
  let moves = (() => {
    let $1 = model.state;
    if ($1 instanceof Win) {
      return $set.new$();
    } else if ($1 instanceof Draw) {
      return $set.new$();
    } else {
      return available_moves(full_board);
    }
  })();
  let buttons = (() => {
    let _pipe = $list.range(0, connect_4_width - 1);
    return $list.map(
      _pipe,
      (i) => {
        return $html.button(
          toList([
            $attribute.class$("drop-button"),
            $event.on_click(new Move(i)),
            $attribute.disabled(!$set.contains(moves, i)),
          ]),
          toList([$element.text(" ⬇ ")]),
        );
      },
    );
  })();
  return $html.div(
    toList([
      $attribute.class$("board"),
      $attribute.class$("move-picker"),
      $attribute.class$("board"),
    ]),
    buttons,
  );
}

const connect_4_height = 6;

function new$(_) {
  let bitboard = $b.new$(connect_4_width, connect_4_height);
  if (bitboard.isOk()) {
    let board$1 = bitboard[0];
    return new GameModel(
      new TurnState(new X(), board$1),
      new TurnState(new O(), board$1),
      new Continue(),
    );
  } else {
    let error = bitboard[0];
    throw makeError("panic", "connect_4", 54, "new", error, {})
  }
}

function get_move(model, column) {
  let $ = $b.bitboard_or(model.active.board, model.inactive.board);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "connect_4",
      81,
      "get_move",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  let full_board = $[0];
  let $1 = $b.file(full_board, column);
  if (!$1.isOk()) {
    throw makeError(
      "let_assert",
      "connect_4",
      83,
      "get_move",
      "Pattern match failed, no pattern matched the value.",
      { value: $1 }
    )
  }
  let mask = $1[0];
  let $2 = $b.bitboard_and(full_board, mask);
  if (!$2.isOk()) {
    throw makeError(
      "let_assert",
      "connect_4",
      84,
      "get_move",
      "Pattern match failed, no pattern matched the value.",
      { value: $2 }
    )
  }
  let col = $2[0];
  let $3 = $b.bitboard_xor(col, mask);
  if (!$3.isOk()) {
    throw makeError(
      "let_assert",
      "connect_4",
      85,
      "get_move",
      "Pattern match failed, no pattern matched the value.",
      { value: $3 }
    )
  }
  let empty_slots = $3[0];
  let $4 = (() => {
    let _pipe = empty_slots;
    let _pipe$1 = $b.to_squares(_pipe);
    return $list.last(_pipe$1);
  })();
  if (!$4.isOk()) {
    throw makeError(
      "let_assert",
      "connect_4",
      86,
      "get_move",
      "Pattern match failed, no pattern matched the value.",
      { value: $4 }
    )
  }
  let square = $4[0];
  let $5 = $b.from_square(connect_4_width, connect_4_height, square);
  if (!$5.isOk()) {
    throw makeError(
      "let_assert",
      "connect_4",
      87,
      "get_move",
      "Pattern match failed, no pattern matched the value.",
      { value: $5 }
    )
  }
  let move = $5[0];
  return move;
}

function update(model, msg) {
  if (msg instanceof Move) {
    let column = msg.column;
    let $ = $b.bitboard_or(model.active.board, model.inactive.board);
    if (!$.isOk()) {
      throw makeError(
        "let_assert",
        "connect_4",
        115,
        "update",
        "Pattern match failed, no pattern matched the value.",
        { value: $ }
      )
    }
    let full_board = $[0];
    let moves = available_moves(full_board);
    let is_legal = $set.contains(moves, column);
    if (is_legal) {
      let move = get_move(model, column);
      let $1 = $b.bitboard_or(move, model.active.board);
      if (!$1.isOk()) {
        throw makeError(
          "let_assert",
          "connect_4",
          123,
          "update",
          "Pattern match failed, no pattern matched the value.",
          { value: $1 }
        )
      }
      let updated_board = $1[0];
      let $2 = $b.bitboard_or(updated_board, model.inactive.board);
      if (!$2.isOk()) {
        throw makeError(
          "let_assert",
          "connect_4",
          124,
          "update",
          "Pattern match failed, no pattern matched the value.",
          { value: $2 }
        )
      }
      let updated_full_board = $2[0];
      let $3 = check_win(updated_board);
      let $4 = $set.size(available_moves(updated_full_board));
      if ($3) {
        let _record = model;
        return new GameModel(
          new TurnState(model.active.turn, updated_board),
          _record.inactive,
          new Win(model.active.turn),
        );
      } else if (!$3 && $4 === 0) {
        let _record = model;
        return new GameModel(_record.active, _record.inactive, new Draw());
      } else {
        return new GameModel(
          model.inactive,
          new TurnState(model.active.turn, updated_board),
          new Continue(),
        );
      }
    } else {
      return model;
    }
  } else {
    return new$(msg);
  }
}

function board(model) {
  let active_board = convert_bitboard_to_set(model.active.board);
  let inactive_board = convert_bitboard_to_set(model.inactive.board);
  let board_rows = (() => {
    let _pipe = $list.range(connect_4_height - 1, 0);
    return $list.fold(
      _pipe,
      toList([]),
      (cells, i) => {
        let row = (() => {
          let _pipe$1 = $list.range(0, connect_4_width - 1);
          return $list.map(
            _pipe$1,
            (j) => {
              let cell_id = i * connect_4_width + j;
              let color = (() => {
                let $ = $set.contains(active_board, cell_id);
                let $1 = $set.contains(inactive_board, cell_id);
                if ($ && !$1) {
                  return turn_to_color(model.active.turn);
                } else if (!$ && $1) {
                  return turn_to_color(model.inactive.turn);
                } else {
                  return "white";
                }
              })();
              return $html.div(
                toList([$attribute.class$("cell")]),
                toList([
                  $html.div(
                    toList([$attribute.class$(("circle" + " ") + color)]),
                    toList([]),
                  ),
                ]),
              );
            },
          );
        })();
        return $list.append(cells, row);
      },
    );
  })();
  return $html.div(toList([$attribute.class$("board")]), board_rows);
}

function game(model) {
  return $html.div(
    toList([$attribute.class$("game")]),
    toList([header(model), move_picker(model), board(model)]),
  );
}

export function main() {
  let app = $lustre.simple(new$, update, game);
  let $ = $lustre.start(app, "#app", undefined);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "connect_4",
      18,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  return undefined;
}
