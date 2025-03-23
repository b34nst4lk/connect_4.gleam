import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $coords from "../bibi/coords.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "../gleam.mjs";

export class Bitboard extends $CustomType {
  constructor(width, height, val) {
    super();
    this.width = width;
    this.height = height;
    this.val = val;
  }
}

function validate_equal_dimensions(bitboard_1, bitboard_2) {
  return $bool.guard(
    bitboard_1.width !== bitboard_2.width,
    new Error("bitboard widths must be equal"),
    () => {
      return $bool.guard(
        bitboard_1.height !== bitboard_2.height,
        new Error("bitboard heights must be equal"),
        () => { return new Ok(undefined); },
      );
    },
  );
}

function validate_coords(coords, width, height) {
  return $bool.guard(
    coords.x < 0,
    new Error("Coords.x must be positive"),
    () => {
      return $bool.guard(
        coords.y < 0,
        new Error("Coords.y must be positive"),
        () => {
          return $bool.guard(
            coords.x >= width,
            new Error("Coords.x must be less than width"),
            () => {
              return $bool.guard(
                coords.y >= height,
                new Error("Coords.y must be less than height"),
                () => { return new Ok(undefined); },
              );
            },
          );
        },
      );
    },
  );
}

function validate_coords_list(loop$coords_list, loop$width, loop$height) {
  while (true) {
    let coords_list = loop$coords_list;
    let width = loop$width;
    let height = loop$height;
    if (coords_list.atLeastLength(1)) {
      let first = coords_list.head;
      let remaining = coords_list.tail;
      let result = validate_coords(first, width, height);
      if (result.isOk()) {
        loop$coords_list = remaining;
        loop$width = width;
        loop$height = height;
      } else {
        return result;
      }
    } else {
      return new Ok(undefined);
    }
  }
}

export function new$(width, height) {
  return $bool.guard(
    width < 0,
    new Error("width must be positive"),
    () => {
      return $bool.guard(
        height < 0,
        new Error("height must be positive"),
        () => { return new Ok(new Bitboard(width, height, 0)); },
      );
    },
  );
}

export function from_base2(width, height, bits) {
  return $bool.guard(
    width < 0,
    new Error("width must be positive"),
    () => {
      return $bool.guard(
        height < 0,
        new Error("height must be positive"),
        () => {
          let $ = $int.base_parse(bits, 2);
          if (!$.isOk()) {
            throw makeError(
              "let_assert",
              "bibi/bitboard",
              133,
              "",
              "Pattern match failed, no pattern matched the value.",
              { value: $ }
            )
          }
          let val = $[0];
          return $bool.guard(
            val >= $int.bitwise_shift_left(1, width * height),
            new Error("bits must be less than 1 << width * height"),
            () => { return new Ok(new Bitboard(width, height, val)); },
          );
        },
      );
    },
  );
}

export function from_coords(width, height, coords) {
  return $bool.guard(
    width < 0,
    new Error("width must be positive"),
    () => {
      return $bool.guard(
        height < 0,
        new Error("height must be positive"),
        () => {
          let result = validate_coords(coords, width, height);
          if (result.isOk()) {
            let val = $int.bitwise_shift_left(1, width * coords.y + coords.x);
            return new Ok(new Bitboard(width, height, val));
          } else {
            let message = result[0];
            return new Error(message);
          }
        },
      );
    },
  );
}

export function from_list_of_coords(width, height, coords_list) {
  return $bool.guard(
    width < 0,
    new Error("width must be positive"),
    () => {
      return $bool.guard(
        height < 0,
        new Error("height must be positive"),
        () => {
          let result = validate_coords_list(coords_list, width, height);
          if (result.isOk()) {
            let val = (() => {
              let _pipe = coords_list;
              return $list.fold(
                _pipe,
                0,
                (acc, coords) => {
                  return $int.bitwise_or(
                    acc,
                    $int.bitwise_shift_left(1, width * coords.y + coords.x),
                  );
                },
              );
            })();
            return new Ok(new Bitboard(width, height, val));
          } else {
            let message = result[0];
            return new Error(message);
          }
        },
      );
    },
  );
}

export function from_square(width, height, square) {
  return $bool.guard(
    width < 0,
    new Error("width must be positive"),
    () => {
      return $bool.guard(
        height < 0,
        new Error("height must be positive"),
        () => {
          return $bool.guard(
            square > width * height,
            new Error(
              (((((("square (" + $int.to_string(square)) + ") must be less than width (") + $int.to_string(
                width,
              )) + ") *") + "height (") + $int.to_string(height)) + ")",
            ),
            () => {
              return new Ok(
                new Bitboard(width, height, $int.bitwise_shift_left(1, square)),
              );
            },
          );
        },
      );
    },
  );
}

export function to_string(bitboard) {
  let _pipe = bitboard.val;
  let _pipe$1 = $int.to_base2(_pipe);
  let _pipe$2 = $string.pad_start(
    _pipe$1,
    bitboard.width * bitboard.height,
    "0",
  );
  let _pipe$3 = $string.split(_pipe$2, "");
  let _pipe$4 = $list.fold(
    _pipe$3,
    toList([""]),
    (acc, str) => {
      if (!acc.atLeastLength(1)) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          229,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: acc }
        )
      }
      let first = acc.head;
      let rest = acc.tail;
      let $ = $string.length(first) >= bitboard.width;
      if ($) {
        return listPrepend(str, listPrepend(first, rest));
      } else {
        return listPrepend(str + first, rest);
      }
    },
  );
  let _pipe$5 = $list.reverse(_pipe$4);
  return $string.join(_pipe$5, "\n");
}

export function to_squares(b) {
  let result = (() => {
    let _pipe = $list.range(0, b.width * b.height - 1);
    return $list.fold_until(
      _pipe,
      [b.val, toList([])],
      (acc, i) => {
        let board = acc[0];
        let l = acc[1];
        let l$1 = (() => {
          let $ = $int.bitwise_and(1, board) > 0;
          if ($) {
            return listPrepend(i, l);
          } else {
            return l;
          }
        })();
        let board$1 = $int.bitwise_shift_right(board, 1);
        let $ = board$1 > 0;
        if ($) {
          return new $list.Continue([board$1, l$1]);
        } else {
          return new $list.Stop([board$1, l$1]);
        }
      },
    );
  })();
  return result[1];
}

export function to_bools(b) {
  let _pipe = $list.range(0, b.width * b.height - 1);
  return $list.fold(
    _pipe,
    toList([]),
    (acc, i) => {
      let has_bit = $int.bitwise_and($int.bitwise_shift_left(1, i), b.val) > 0;
      return $list.append(acc, toList([has_bit]));
    },
  );
}

function int_full_mask(b) {
  return $int.bitwise_shift_left(1, b.width * b.height) - 1;
}

export function full_mask(b) {
  let _record = b;
  return new Bitboard(_record.width, _record.height, int_full_mask(b));
}

function first_rank(loop$bitboard, loop$counter, loop$val) {
  while (true) {
    let bitboard = loop$bitboard;
    let counter = loop$counter;
    let val = loop$val;
    let $ = counter >= bitboard.width;
    if ($) {
      let _record = bitboard;
      return new Bitboard(_record.width, _record.height, val);
    } else {
      loop$bitboard = bitboard;
      loop$counter = counter + 1;
      loop$val = $int.bitwise_or($int.bitwise_shift_left(1, counter), val);
    }
  }
}

export function rank(bitboard, rank_no) {
  return $bool.guard(
    rank_no < 0,
    new Error("rank_no must be positive"),
    () => {
      return $bool.guard(
        rank_no >= bitboard.height,
        new Error("rank_no must be less than bitboard.height"),
        () => {
          let first_rank$1 = first_rank(bitboard, 0, 0);
          let rank$1 = $int.bitwise_shift_left(
            first_rank$1.val,
            rank_no * bitboard.width,
          );
          return new Ok(
            (() => {
              let _record = bitboard;
              return new Bitboard(_record.width, _record.height, rank$1);
            })(),
          );
        },
      );
    },
  );
}

function first_file(loop$bitboard, loop$counter, loop$val) {
  while (true) {
    let bitboard = loop$bitboard;
    let counter = loop$counter;
    let val = loop$val;
    let $ = counter >= bitboard.height;
    if ($) {
      let _record = bitboard;
      return new Bitboard(_record.width, _record.height, val);
    } else {
      loop$bitboard = bitboard;
      loop$counter = counter + 1;
      loop$val = $int.bitwise_or(
        $int.bitwise_shift_left(1, counter * bitboard.width),
        val,
      );
    }
  }
}

export function file(bitboard, file_no) {
  return $bool.guard(
    file_no < 0,
    new Error("file_no must be positive"),
    () => {
      return $bool.guard(
        file_no >= bitboard.width,
        new Error("file_no must be less than bitboard.width"),
        () => {
          let first_file$1 = first_file(bitboard, 0, 0);
          let file$1 = $int.bitwise_shift_left(first_file$1.val, file_no);
          return new Ok(
            (() => {
              let _record = bitboard;
              return new Bitboard(_record.width, _record.height, file$1);
            })(),
          );
        },
      );
    },
  );
}

function diagonal_from_south_west(b) {
  let length = $int.min(b.width, b.height);
  let val = (() => {
    let _pipe = $list.range(0, length - 1);
    return $list.fold(
      _pipe,
      0,
      (acc, i) => {
        let new_bit = $int.bitwise_shift_left(1, b.width * i + i);
        return $int.bitwise_or(acc, new_bit);
      },
    );
  })();
  let _record = b;
  return new Bitboard(_record.width, _record.height, val);
}

function antidiagonal_from_south_east(b) {
  let length = $int.min(b.width, b.height);
  let seed = $int.bitwise_shift_left(1, b.width - 1);
  let val = (() => {
    let _pipe = $list.range(0, length - 1);
    return $list.fold(
      _pipe,
      0,
      (acc, _) => {
        let acc$1 = $int.bitwise_shift_left(acc, b.width - 1);
        return $int.bitwise_or(acc$1, seed);
      },
    );
  })();
  let b$1 = (() => {
    let _record = b;
    return new Bitboard(_record.width, _record.height, val);
  })();
  return b$1;
}

export function bitboard_and(bitboard_1, bitboard_2) {
  let $ = validate_equal_dimensions(bitboard_1, bitboard_2);
  if (!$.isOk()) {
    let err = $[0];
    return new Error(err);
  } else {
    return new Ok(
      (() => {
        let _record = bitboard_1;
        return new Bitboard(
          _record.width,
          _record.height,
          $int.bitwise_and(bitboard_1.val, bitboard_2.val),
        );
      })(),
    );
  }
}

export function bitboard_or(bitboard_1, bitboard_2) {
  let $ = validate_equal_dimensions(bitboard_1, bitboard_2);
  if (!$.isOk()) {
    let err = $[0];
    return new Error(err);
  } else {
    return new Ok(
      (() => {
        let _record = bitboard_1;
        return new Bitboard(
          _record.width,
          _record.height,
          $int.bitwise_or(bitboard_1.val, bitboard_2.val),
        );
      })(),
    );
  }
}

export function bitboard_xor(bitboard_1, bitboard_2) {
  let $ = validate_equal_dimensions(bitboard_1, bitboard_2);
  if (!$.isOk()) {
    let err = $[0];
    return new Error(err);
  } else {
    return new Ok(
      (() => {
        let _record = bitboard_1;
        return new Bitboard(
          _record.width,
          _record.height,
          $int.bitwise_exclusive_or(bitboard_1.val, bitboard_2.val),
        );
      })(),
    );
  }
}

export function bitboard_not(bitboard) {
  let full_board = $int.bitwise_shift_left(1, bitboard.width * bitboard.height) - 1;
  let val = $int.bitwise_exclusive_or(bitboard.val, full_board);
  let _record = bitboard;
  return new Bitboard(_record.width, _record.height, val);
}

function shift_north_unvalidated(bitboard, i) {
  let val = (() => {
    let _pipe = bitboard.val;
    let _pipe$1 = $int.bitwise_shift_left(_pipe, i * bitboard.width);
    return $int.bitwise_and(_pipe$1, int_full_mask(bitboard));
  })();
  let _record = bitboard;
  return new Bitboard(_record.width, _record.height, val);
}

export function shift_north(bitboard, i) {
  return $bool.guard(
    i === 0,
    new Ok(bitboard),
    () => {
      return $bool.guard(
        i < 0,
        new Error("shift_north by must be >= 0"),
        () => { return new Ok(shift_north_unvalidated(bitboard, i)); },
      );
    },
  );
}

function shift_south_unvalidated(bitboard, i) {
  let val = (() => {
    let _pipe = bitboard.val;
    return $int.bitwise_shift_right(_pipe, i * bitboard.width);
  })();
  let _record = bitboard;
  return new Bitboard(_record.width, _record.height, val);
}

export function shift_south(bitboard, i) {
  return $bool.guard(
    i === 0,
    new Ok(bitboard),
    () => {
      return $bool.guard(
        i < 0,
        new Error("shift_south by must be >= 0"),
        () => { return new Ok(shift_south_unvalidated(bitboard, i)); },
      );
    },
  );
}

function shift_west_unvalidated(bitboard, i) {
  let mask = (() => {
    let _pipe = $list.range(0, i - 1);
    return $list.fold(
      _pipe,
      0,
      (m, i) => {
        let $ = file(bitboard, i);
        if (!$.isOk()) {
          throw makeError(
            "let_assert",
            "bibi/bitboard",
            635,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $ }
          )
        }
        let r = $[0];
        return $int.bitwise_or(m, r.val);
      },
    );
  })();
  let updated_val = bitboard.val - $int.bitwise_and(mask, bitboard.val);
  let val = (() => {
    let _pipe = updated_val;
    return $int.bitwise_shift_right(_pipe, i);
  })();
  let _record = bitboard;
  return new Bitboard(_record.width, _record.height, val);
}

export function shift_west(bitboard, i) {
  return $bool.guard(
    i === 0,
    new Ok(bitboard),
    () => {
      return $bool.guard(
        i < 0,
        new Error("shift_west by must be >= 0"),
        () => {
          return $bool.guard(
            i >= bitboard.width,
            new Error("shift_west by must be < bitboard.width"),
            () => { return new Ok(shift_west_unvalidated(bitboard, i)); },
          );
        },
      );
    },
  );
}

function shift_east_unvalidated(bitboard, i) {
  let mask = (() => {
    let _pipe = $list.range(bitboard.width - 1, bitboard.width - i);
    return $list.fold(
      _pipe,
      0,
      (m, i) => {
        let $ = file(bitboard, i);
        if (!$.isOk()) {
          throw makeError(
            "let_assert",
            "bibi/bitboard",
            667,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $ }
          )
        }
        let r = $[0];
        return $int.bitwise_or(m, r.val);
      },
    );
  })();
  let updated_val = bitboard.val - $int.bitwise_and(mask, bitboard.val);
  let val = (() => {
    let _pipe = updated_val;
    return $int.bitwise_shift_left(_pipe, i);
  })();
  let _record = bitboard;
  return new Bitboard(_record.width, _record.height, val);
}

export function shift_east(bitboard, i) {
  return $bool.guard(
    i === 0,
    new Ok(bitboard),
    () => {
      return $bool.guard(
        i < 0,
        new Error("shift_east by must be >= 0"),
        () => {
          return $bool.guard(
            i >= bitboard.width,
            new Error("shift_east by must be < bitboard.width"),
            () => { return new Ok(shift_east_unvalidated(bitboard, i)); },
          );
        },
      );
    },
  );
}

export function diagonal(bitboard, diagonal_no) {
  let max_diagonal_no = (bitboard.width + bitboard.height) - 2;
  return $bool.guard(
    diagonal_no < 0,
    new Error("diagonal_no must be positive"),
    () => {
      return $bool.guard(
        diagonal_no > max_diagonal_no,
        new Error(
          "diagonal_no must be less than bitboard.width + bitboard.height - 1",
        ),
        () => {
          let main_diagonal = diagonal_from_south_west(bitboard);
          let $ = diagonal_no < bitboard.width;
          let $1 = bitboard.width < bitboard.height;
          if ($ && $1) {
            return shift_south(
              main_diagonal,
              (bitboard.width - diagonal_no) - 1,
            );
          } else if ($ && !$1) {
            return shift_east(main_diagonal, (bitboard.width - diagonal_no) - 1);
          } else if (!$ && $1) {
            return shift_north(
              main_diagonal,
              (diagonal_no - bitboard.width) + 1,
            );
          } else {
            return shift_west(main_diagonal, (diagonal_no - bitboard.width) + 1);
          }
        },
      );
    },
  );
}

export function antidiagonal(bitboard, antidiagonal_no) {
  let max_antidiagonal_no = (bitboard.width + bitboard.height) - 2;
  return $bool.guard(
    antidiagonal_no < 0,
    new Error("antidiagonal_no must be positive"),
    () => {
      return $bool.guard(
        antidiagonal_no > max_antidiagonal_no,
        new Error(
          "antidiagonal_no must be less than bitboard.width + bitboard.height - 1",
        ),
        () => {
          let main_diagonal = antidiagonal_from_south_east(bitboard);
          let $ = antidiagonal_no < bitboard.width;
          let $1 = bitboard.width < bitboard.height;
          if ($ && $1) {
            return shift_south(
              main_diagonal,
              (bitboard.width - antidiagonal_no) - 1,
            );
          } else if ($ && !$1) {
            return shift_west(
              main_diagonal,
              (bitboard.width - antidiagonal_no) - 1,
            );
          } else if (!$ && $1) {
            return shift_north(
              main_diagonal,
              (antidiagonal_no - bitboard.width) + 1,
            );
          } else {
            return shift_east(
              main_diagonal,
              (antidiagonal_no - bitboard.width) + 1,
            );
          }
        },
      );
    },
  );
}

export function shift_northeast(bitboard, i) {
  return $bool.guard(
    i === 0,
    new Ok(bitboard),
    () => {
      return $bool.guard(
        i < 0,
        new Error("shift_northeast by must be >= 0"),
        () => {
          let _pipe = bitboard;
          let _pipe$1 = shift_east_unvalidated(_pipe, i);
          let _pipe$2 = shift_north_unvalidated(_pipe$1, i);
          return new Ok(_pipe$2);
        },
      );
    },
  );
}

export function shift_northwest(bitboard, i) {
  return $bool.guard(
    i === 0,
    new Ok(bitboard),
    () => {
      return $bool.guard(
        i < 0,
        new Error("shift_northwest by must be >= 0"),
        () => {
          let _pipe = bitboard;
          let _pipe$1 = shift_west_unvalidated(_pipe, i);
          let _pipe$2 = shift_north_unvalidated(_pipe$1, i);
          return new Ok(_pipe$2);
        },
      );
    },
  );
}

export function shift_southeast(bitboard, i) {
  return $bool.guard(
    i === 0,
    new Ok(bitboard),
    () => {
      return $bool.guard(
        i < 0,
        new Error("shift_southeast by must be >= 0"),
        () => {
          let _pipe = bitboard;
          let _pipe$1 = shift_east_unvalidated(_pipe, i);
          let _pipe$2 = shift_south_unvalidated(_pipe$1, i);
          return new Ok(_pipe$2);
        },
      );
    },
  );
}

export function shift_southwest(bitboard, i) {
  return $bool.guard(
    i === 0,
    new Ok(bitboard),
    () => {
      return $bool.guard(
        i < 0,
        new Error("shift_southwest by must be >= 0"),
        () => {
          let _pipe = bitboard;
          let _pipe$1 = shift_west_unvalidated(_pipe, i);
          let _pipe$2 = shift_south_unvalidated(_pipe$1, i);
          return new Ok(_pipe$2);
        },
      );
    },
  );
}

export function flip_vertically(bitboard) {
  let _pipe = $list.range(0, bitboard.height - 1);
  return $list.fold(
    _pipe,
    (() => {
      let _record = bitboard;
      return new Bitboard(_record.width, _record.height, 0);
    })(),
    (b, i) => {
      let $ = rank(bitboard, i);
      if (!$.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          754,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $ }
        )
      }
      let rank_mask = $[0];
      let $1 = bitboard_and(bitboard, rank_mask);
      if (!$1.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          755,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $1 }
        )
      }
      let rank$1 = $1[0];
      let $2 = shift_south(rank$1, i);
      if (!$2.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          756,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $2 }
        )
      }
      let rank$2 = $2[0];
      let $3 = shift_north(rank$2, (bitboard.height - i) - 1);
      if (!$3.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          757,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $3 }
        )
      }
      let rank$3 = $3[0];
      let $4 = bitboard_or(b, rank$3);
      if (!$4.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          758,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $4 }
        )
      }
      let updated_bitboard = $4[0];
      return updated_bitboard;
    },
  );
}

export function flip_horizontally(bitboard) {
  let _pipe = $list.range(0, bitboard.width - 1);
  return $list.fold(
    _pipe,
    (() => {
      let _record = bitboard;
      return new Bitboard(_record.width, _record.height, 0);
    })(),
    (b, i) => {
      let $ = file(bitboard, i);
      if (!$.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          774,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $ }
        )
      }
      let file_mask = $[0];
      let $1 = bitboard_and(bitboard, file_mask);
      if (!$1.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          775,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $1 }
        )
      }
      let file$1 = $1[0];
      let $2 = shift_west(file$1, i);
      if (!$2.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          776,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $2 }
        )
      }
      let file$2 = $2[0];
      let $3 = shift_east(file$2, (bitboard.width - i) - 1);
      if (!$3.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          777,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $3 }
        )
      }
      let file$3 = $3[0];
      let $4 = bitboard_or(b, file$3);
      if (!$4.isOk()) {
        throw makeError(
          "let_assert",
          "bibi/bitboard",
          778,
          "",
          "Pattern match failed, no pattern matched the value.",
          { value: $4 }
        )
      }
      let updated_bitboard = $4[0];
      return updated_bitboard;
    },
  );
}
