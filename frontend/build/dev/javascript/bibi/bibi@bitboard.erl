-module(bibi@bitboard).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/2, from_base2/3, from_coords/3, from_list_of_coords/3, from_square/3, to_string/1, to_squares/1, to_bools/1, full_mask/1, rank/2, file/2, bitboard_and/2, bitboard_or/2, bitboard_xor/2, bitboard_not/1, shift_north/2, shift_south/2, shift_west/2, shift_east/2, diagonal/2, antidiagonal/2, shift_northeast/2, shift_northwest/2, shift_southeast/2, shift_southwest/2, flip_vertically/1, flip_horizontally/1]).
-export_type([bitboard/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " The bibi/bitboard module provides the ability to create and manipulate bitboards.\n"
    " Bitboards have a defined width and height, and an integer that represents the\n"
    " state of the bitboard when in binary\n"
    "\n"
    " Suppose you are representing a game of tic-tac-toe that looks like\n"
    "\n"
    "```\n"
    " X | O | _\n"
    " - + - + -\n"
    " O | X | _\n"
    " - + - + -\n"
    " O | _ | X\n"
    " ```\n"
    " Representing the X's as a bitboard, it would look like\n"
    " ```\n"
    " 100\n"
    " 010\n"
    " 001\n"
    " ```\n"
    "\n"
    " In binary, this would be `001010100`, which translates to 84\n"
    "\n"
    " Notice that the positions of the 1's when the bitboard is translated into its\n"
    " binary integer format\n"
    "\n"
    " The following diagram shows how the individual bits are ordered from right to left\n"
    " ```\n"
    " 6 7 8\n"
    " 3 4 5\n"
    " 0 1 2\n"
    " ```\n"
    "\n"
    " To disambiguate between the bitwise shift operations in the `int` modules and bitboard shifts\n"
    " we use cardinal directions when describing and manipulating bitboards.\n"
    "\n"
    " ```\n"
    "      north\n"
    "\n"
    "       000\n"
    " west  000  east\n"
    "       000\n"
    "\n"
    "      south\n"
    " ```\n"
    "\n"
).

-type bitboard() :: {bitboard, integer(), integer(), integer()}.

-file("src/bibi/bitboard.gleam", 63).
?DOC(
    " Internal validator for ensuring bitboards are of the same dimension before bitboard\n"
    " operations are performed\n"
).
-spec validate_equal_dimensions(bitboard(), bitboard()) -> {ok, nil} |
    {error, binary()}.
validate_equal_dimensions(Bitboard_1, Bitboard_2) ->
    gleam@bool:guard(
        erlang:element(2, Bitboard_1) /= erlang:element(2, Bitboard_2),
        {error, <<"bitboard widths must be equal"/utf8>>},
        fun() ->
            gleam@bool:guard(
                erlang:element(3, Bitboard_1) /= erlang:element(3, Bitboard_2),
                {error, <<"bitboard heights must be equal"/utf8>>},
                fun() -> {ok, nil} end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 80).
?DOC(
    " Internal validator for ensuring that coordinates are on the bitboard before\n"
    " bitboard operations are performed\n"
).
-spec validate_coords(bibi@coords:coords(), integer(), integer()) -> {ok, nil} |
    {error, binary()}.
validate_coords(Coords, Width, Height) ->
    gleam@bool:guard(
        erlang:element(2, Coords) < 0,
        {error, <<"Coords.x must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                erlang:element(3, Coords) < 0,
                {error, <<"Coords.y must be positive"/utf8>>},
                fun() ->
                    gleam@bool:guard(
                        erlang:element(2, Coords) >= Width,
                        {error, <<"Coords.x must be less than width"/utf8>>},
                        fun() ->
                            gleam@bool:guard(
                                erlang:element(3, Coords) >= Height,
                                {error,
                                    <<"Coords.y must be less than height"/utf8>>},
                                fun() -> {ok, nil} end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 101).
?DOC(
    " Internal validator for ensuring that coordinates are on the bitboard before\n"
    " bitboard operations are performed\n"
).
-spec validate_coords_list(list(bibi@coords:coords()), integer(), integer()) -> {ok,
        nil} |
    {error, binary()}.
validate_coords_list(Coords_list, Width, Height) ->
    case Coords_list of
        [First | Remaining] ->
            Result = validate_coords(First, Width, Height),
            case Result of
                {ok, _} ->
                    validate_coords_list(Remaining, Width, Height);

                _ ->
                    Result
            end;

        _ ->
            {ok, nil}
    end.

-file("src/bibi/bitboard.gleam", 119).
?DOC(" Create an empty bitboard of a given width and height\n").
-spec new(integer(), integer()) -> {ok, bitboard()} | {error, binary()}.
new(Width, Height) ->
    gleam@bool:guard(
        Width < 0,
        {error, <<"width must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                Height < 0,
                {error, <<"height must be positive"/utf8>>},
                fun() -> {ok, {bitboard, Width, Height, 0}} end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 129).
?DOC(
    " Create a bitboard of a given width and height, and a binary string\n"
    "\n"
    " i.e.\n"
    " `from_base2(3, 3, \"000000111\")` --> `Bitboard(width: 3, height: 3, val: 7)`\n"
).
-spec from_base2(integer(), integer(), binary()) -> {ok, bitboard()} |
    {error, binary()}.
from_base2(Width, Height, Bits) ->
    gleam@bool:guard(
        Width < 0,
        {error, <<"width must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                Height < 0,
                {error, <<"height must be positive"/utf8>>},
                fun() ->
                    _assert_subject = gleam@int:base_parse(Bits, 2),
                    {ok, Val} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail,
                                        module => <<"bibi/bitboard"/utf8>>,
                                        function => <<"from_base2"/utf8>>,
                                        line => 133})
                    end,
                    gleam@bool:guard(
                        Val >= erlang:'bsl'(1, Width * Height),
                        {error,
                            <<"bits must be less than 1 << width * height"/utf8>>},
                        fun() -> {ok, {bitboard, Width, Height, Val}} end
                    )
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 145).
?DOC(
    " Create a bitboard of a given width and height, and a Coords\n"
    "\n"
    " i.e.\n"
    " `from_coords(3, 3, Coords(0, 0))` --> `Bitboard(width: 3, height: 3, val: 1)`\n"
).
-spec from_coords(integer(), integer(), bibi@coords:coords()) -> {ok,
        bitboard()} |
    {error, binary()}.
from_coords(Width, Height, Coords) ->
    gleam@bool:guard(
        Width < 0,
        {error, <<"width must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                Height < 0,
                {error, <<"height must be positive"/utf8>>},
                fun() ->
                    Result = validate_coords(Coords, Width, Height),
                    case Result of
                        {ok, _} ->
                            Val = erlang:'bsl'(
                                1,
                                (Width * erlang:element(3, Coords)) + erlang:element(
                                    2,
                                    Coords
                                )
                            ),
                            {ok, {bitboard, Width, Height, Val}};

                        {error, Message} ->
                            {error, Message}
                    end
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 162).
?DOC(
    " Create a bitboard of a given width and height, and a list of Coords\n"
    "\n"
    " i.e.\n"
    " `from_coords(3, 3, [Coords(0, 0), Coords(1, 0)])` --> `Bitboard(width: 3, height: 3, val: 3)`\n"
).
-spec from_list_of_coords(integer(), integer(), list(bibi@coords:coords())) -> {ok,
        bitboard()} |
    {error, binary()}.
from_list_of_coords(Width, Height, Coords_list) ->
    gleam@bool:guard(
        Width < 0,
        {error, <<"width must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                Height < 0,
                {error, <<"height must be positive"/utf8>>},
                fun() ->
                    Result = validate_coords_list(Coords_list, Width, Height),
                    case Result of
                        {ok, _} ->
                            Val = begin
                                _pipe = Coords_list,
                                gleam@list:fold(
                                    _pipe,
                                    0,
                                    fun(Acc, Coords) ->
                                        erlang:'bor'(
                                            Acc,
                                            erlang:'bsl'(
                                                1,
                                                (Width * erlang:element(
                                                    3,
                                                    Coords
                                                ))
                                                + erlang:element(2, Coords)
                                            )
                                        )
                                    end
                                )
                            end,
                            {ok, {bitboard, Width, Height, Val}};

                        {error, Message} ->
                            {error, Message}
                    end
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 196).
?DOC(
    " Create a bitboard of a given with and height, and the nth square\n"
    "\n"
    " i.e. `from_square(3, 3, 1)` --> `Bitboard(width: 3, height: 3, val: 2)`\n"
    "\n"
    " Squares are indexed from bottom left to top right. A 3 by 3 bitboard will be indexed as follows\n"
    " ```\n"
    " 6 7 8\n"
    " 3 4 5\n"
    " 0 1 2\n"
    " ```\n"
).
-spec from_square(integer(), integer(), integer()) -> {ok, bitboard()} |
    {error, binary()}.
from_square(Width, Height, Square) ->
    gleam@bool:guard(
        Width < 0,
        {error, <<"width must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                Height < 0,
                {error, <<"height must be positive"/utf8>>},
                fun() ->
                    gleam@bool:guard(
                        Square > (Width * Height),
                        {error,
                            <<<<<<<<<<<<<<"square ("/utf8,
                                                        (erlang:integer_to_binary(
                                                            Square
                                                        ))/binary>>/binary,
                                                    ") must be less than width ("/utf8>>/binary,
                                                (erlang:integer_to_binary(Width))/binary>>/binary,
                                            ") *"/utf8>>/binary,
                                        "height ("/utf8>>/binary,
                                    (erlang:integer_to_binary(Height))/binary>>/binary,
                                ")"/utf8>>},
                        fun() ->
                            {ok,
                                {bitboard,
                                    Width,
                                    Height,
                                    erlang:'bsl'(1, Square)}}
                        end
                    )
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 223).
?DOC(
    " Converts a bitboard into a width * height, breakpoint separated string of 1s and 0s\n"
    "\n"
    " i.e. `to_string(Bitboard(width: 3, height: 3, val: 2))` -->\n"
    " ```\n"
    " 000\n"
    " 000\n"
    " 010\n"
    " ````\n"
).
-spec to_string(bitboard()) -> binary().
to_string(Bitboard) ->
    _pipe = erlang:element(4, Bitboard),
    _pipe@1 = gleam@int:to_base2(_pipe),
    _pipe@2 = gleam@string:pad_start(
        _pipe@1,
        erlang:element(2, Bitboard) * erlang:element(3, Bitboard),
        <<"0"/utf8>>
    ),
    _pipe@3 = gleam@string:split(_pipe@2, <<""/utf8>>),
    _pipe@4 = gleam@list:fold(
        _pipe@3,
        [<<""/utf8>>],
        fun(Acc, Str) ->
            [First | Rest] = case Acc of
                [_ | _] -> Acc;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"to_string"/utf8>>,
                                line => 229})
            end,
            case string:length(First) >= erlang:element(2, Bitboard) of
                true ->
                    [Str, First | Rest];

                false ->
                    [<<Str/binary, First/binary>> | Rest]
            end
        end
    ),
    _pipe@5 = lists:reverse(_pipe@4),
    gleam@string:join(_pipe@5, <<"\n"/utf8>>).

-file("src/bibi/bitboard.gleam", 242).
?DOC(
    " Converts a bitboard into a list of integers representing where a square is occupied\n"
    "\n"
    " i.e `to_squares(Bitboard(width: 3, height: 3, val: 3))` --> `[0, 1]`\n"
).
-spec to_squares(bitboard()) -> list(integer()).
to_squares(B) ->
    Result = begin
        _pipe = gleam@list:range(
            0,
            (erlang:element(2, B) * erlang:element(3, B)) - 1
        ),
        gleam@list:fold_until(
            _pipe,
            {erlang:element(4, B), []},
            fun(Acc, I) ->
                {Board, L} = Acc,
                L@1 = case erlang:'band'(1, Board) > 0 of
                    true ->
                        [I | L];

                    false ->
                        L
                end,
                Board@1 = erlang:'bsr'(Board, 1),
                case Board@1 > 0 of
                    true ->
                        {continue, {Board@1, L@1}};

                    false ->
                        {stop, {Board@1, L@1}}
                end
            end
        )
    end,
    erlang:element(2, Result).

-file("src/bibi/bitboard.gleam", 265).
?DOC(
    " Convers a bitboard into a list of booleans representing where a square is occupied\n"
    "\n"
    " i.e\n"
    " `to_bools(Bitboard(width: 3, height: 3, val: 3))` --> `[True, True, False,... ]` (False repeats 7 times in this example)\n"
).
-spec to_bools(bitboard()) -> list(boolean()).
to_bools(B) ->
    _pipe = gleam@list:range(
        0,
        (erlang:element(2, B) * erlang:element(3, B)) - 1
    ),
    gleam@list:fold(
        _pipe,
        [],
        fun(Acc, I) ->
            Has_bit = erlang:'band'(erlang:'bsl'(1, I), erlang:element(4, B)) > 0,
            lists:append(Acc, [Has_bit])
        end
    ).

-file("src/bibi/bitboard.gleam", 273).
-spec int_full_mask(bitboard()) -> integer().
int_full_mask(B) ->
    erlang:'bsl'(1, erlang:element(2, B) * erlang:element(3, B)) - 1.

-file("src/bibi/bitboard.gleam", 285).
?DOC(
    " This returns a bitboard that is fully occupied\n"
    "\n"
    " i.e.\n"
    " ```\n"
    "                                  111\n"
    " full_mask(Bitboard(3, 3, 1)) --> 111\n"
    "                                  111\n"
    " ```\n"
).
-spec full_mask(bitboard()) -> bitboard().
full_mask(B) ->
    _record = B,
    {bitboard,
        erlang:element(2, _record),
        erlang:element(3, _record),
        int_full_mask(B)}.

-file("src/bibi/bitboard.gleam", 290).
-spec first_rank(bitboard(), integer(), integer()) -> bitboard().
first_rank(Bitboard, Counter, Val) ->
    case Counter >= erlang:element(2, Bitboard) of
        true ->
            _record = Bitboard,
            {bitboard,
                erlang:element(2, _record),
                erlang:element(3, _record),
                Val};

        false ->
            first_rank(
                Bitboard,
                Counter + 1,
                erlang:'bor'(erlang:'bsl'(1, Counter), Val)
            )
    end.

-file("src/bibi/bitboard.gleam", 312).
?DOC(
    " Returns a bitboard with the nth rank occupied of the provided bitboard.\n"
    " Ranks are indexed from 0 to height - 1, and start from the north side of the board.\n"
    "\n"
    " i.e.\n"
    " ```\n"
    "                             000\n"
    " rank(Bitboard(3, 3, 1)) --> 111\n"
    "                             000\n"
    " ````\n"
).
-spec rank(bitboard(), integer()) -> {ok, bitboard()} | {error, binary()}.
rank(Bitboard, Rank_no) ->
    gleam@bool:guard(
        Rank_no < 0,
        {error, <<"rank_no must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                Rank_no >= erlang:element(3, Bitboard),
                {error, <<"rank_no must be less than bitboard.height"/utf8>>},
                fun() ->
                    First_rank = first_rank(Bitboard, 0, 0),
                    Rank = erlang:'bsl'(
                        erlang:element(4, First_rank),
                        Rank_no * erlang:element(2, Bitboard)
                    ),
                    {ok,
                        begin
                            _record = Bitboard,
                            {bitboard,
                                erlang:element(2, _record),
                                erlang:element(3, _record),
                                Rank}
                        end}
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 325).
?DOC(" File Masks\n").
-spec first_file(bitboard(), integer(), integer()) -> bitboard().
first_file(Bitboard, Counter, Val) ->
    case Counter >= erlang:element(3, Bitboard) of
        true ->
            _record = Bitboard,
            {bitboard,
                erlang:element(2, _record),
                erlang:element(3, _record),
                Val};

        false ->
            first_file(
                Bitboard,
                Counter + 1,
                erlang:'bor'(
                    erlang:'bsl'(1, Counter * erlang:element(2, Bitboard)),
                    Val
                )
            )
    end.

-file("src/bibi/bitboard.gleam", 347).
?DOC(
    " Returns a bitboard with the nth file occupied of the provided bitboard\n"
    " Files are indexed from 0 to width - 1, and start from the west side of the board\n"
    "\n"
    " i.e.\n"
    " ```\n"
    "                              010\n"
    " file(Bitboard(3, 3, 1)) --> 010\n"
    "                              010\n"
    " ````\n"
).
-spec file(bitboard(), integer()) -> {ok, bitboard()} | {error, binary()}.
file(Bitboard, File_no) ->
    gleam@bool:guard(
        File_no < 0,
        {error, <<"file_no must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                File_no >= erlang:element(2, Bitboard),
                {error, <<"file_no must be less than bitboard.width"/utf8>>},
                fun() ->
                    First_file = first_file(Bitboard, 0, 0),
                    File = erlang:'bsl'(erlang:element(4, First_file), File_no),
                    {ok,
                        begin
                            _record = Bitboard,
                            {bitboard,
                                erlang:element(2, _record),
                                erlang:element(3, _record),
                                File}
                        end}
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 359).
-spec diagonal_from_south_west(bitboard()) -> bitboard().
diagonal_from_south_west(B) ->
    Length = gleam@int:min(erlang:element(2, B), erlang:element(3, B)),
    Val = begin
        _pipe = gleam@list:range(0, Length - 1),
        gleam@list:fold(
            _pipe,
            0,
            fun(Acc, I) ->
                New_bit = erlang:'bsl'(1, (erlang:element(2, B) * I) + I),
                erlang:'bor'(Acc, New_bit)
            end
        )
    end,
    _record = B,
    {bitboard, erlang:element(2, _record), erlang:element(3, _record), Val}.

-file("src/bibi/bitboard.gleam", 370).
-spec antidiagonal_from_south_east(bitboard()) -> bitboard().
antidiagonal_from_south_east(B) ->
    Length = gleam@int:min(erlang:element(2, B), erlang:element(3, B)),
    Seed = erlang:'bsl'(1, erlang:element(2, B) - 1),
    Val = begin
        _pipe = gleam@list:range(0, Length - 1),
        gleam@list:fold(
            _pipe,
            0,
            fun(Acc, _) ->
                Acc@1 = erlang:'bsl'(Acc, erlang:element(2, B) - 1),
                erlang:'bor'(Acc@1, Seed)
            end
        )
    end,
    B@1 = begin
        _record = B,
        {bitboard, erlang:element(2, _record), erlang:element(3, _record), Val}
    end,
    B@1.

-file("src/bibi/bitboard.gleam", 490).
?DOC(
    " Perfroms the `and` operation on two bitboards and returns a new bitboard.\n"
    " If a square is occupied on both bitboards, it will be occupied in the\n"
    " resulting bitboard. Both bitboards must have the same width and height.\n"
    " ```\n"
    " 010     000     000\n"
    " 010 and 111 --> 010\n"
    " 010     000     000\n"
    " ```\n"
).
-spec bitboard_and(bitboard(), bitboard()) -> {ok, bitboard()} |
    {error, binary()}.
bitboard_and(Bitboard_1, Bitboard_2) ->
    case validate_equal_dimensions(Bitboard_1, Bitboard_2) of
        {error, Err} ->
            {error, Err};

        {ok, _} ->
            {ok,
                begin
                    _record = Bitboard_1,
                    {bitboard,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        erlang:'band'(
                            erlang:element(4, Bitboard_1),
                            erlang:element(4, Bitboard_2)
                        )}
                end}
    end.

-file("src/bibi/bitboard.gleam", 514).
?DOC(
    " Perfroms the `or` operation on two bitboards and returns a new bitboard.\n"
    " If a square is occupied on both bitboards, it will be occupied in the\n"
    " resulting bitboard. Both bitboards must have the same width and height.\n"
    " ```\n"
    " 010     000     010\n"
    " 010 and 111 --> 111\n"
    " 010     000     010\n"
    " ```\n"
).
-spec bitboard_or(bitboard(), bitboard()) -> {ok, bitboard()} |
    {error, binary()}.
bitboard_or(Bitboard_1, Bitboard_2) ->
    case validate_equal_dimensions(Bitboard_1, Bitboard_2) of
        {error, Err} ->
            {error, Err};

        {ok, _} ->
            {ok,
                begin
                    _record = Bitboard_1,
                    {bitboard,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        erlang:'bor'(
                            erlang:element(4, Bitboard_1),
                            erlang:element(4, Bitboard_2)
                        )}
                end}
    end.

-file("src/bibi/bitboard.gleam", 535).
?DOC(
    " Perfroms the `xor` operation on two bitboards and returns a new bitboard.\n"
    " If a square is occupied only on on of the two  bitboards, it will be occupied in the\n"
    " resulting bitboard. Both bitboards must have the same width and height.\n"
    " ```\n"
    " 010     000     010\n"
    " 010 and 111 --> 101\n"
    " 010     000     010\n"
    " ```\n"
).
-spec bitboard_xor(bitboard(), bitboard()) -> {ok, bitboard()} |
    {error, binary()}.
bitboard_xor(Bitboard_1, Bitboard_2) ->
    case validate_equal_dimensions(Bitboard_1, Bitboard_2) of
        {error, Err} ->
            {error, Err};

        {ok, _} ->
            {ok,
                begin
                    _record = Bitboard_1,
                    {bitboard,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        erlang:'bxor'(
                            erlang:element(4, Bitboard_1),
                            erlang:element(4, Bitboard_2)
                        )}
                end}
    end.

-file("src/bibi/bitboard.gleam", 560).
?DOC(
    " Performs the `not` operation on a bitboard. All occupied squares will become unoccupied,\n"
    " and vice versa.\n"
    "\n"
    " i.e.\n"
    " ```\n"
    " 010    101\n"
    " 101 -> 010\n"
    " 010    101\n"
    " ```\n"
).
-spec bitboard_not(bitboard()) -> bitboard().
bitboard_not(Bitboard) ->
    Full_board = erlang:'bsl'(
        1,
        erlang:element(2, Bitboard) * erlang:element(3, Bitboard)
    )
    - 1,
    Val = erlang:'bxor'(erlang:element(4, Bitboard), Full_board),
    _record = Bitboard,
    {bitboard, erlang:element(2, _record), erlang:element(3, _record), Val}.

-file("src/bibi/bitboard.gleam", 582).
-spec shift_north_unvalidated(bitboard(), integer()) -> bitboard().
shift_north_unvalidated(Bitboard, I) ->
    Val = begin
        _pipe = erlang:element(4, Bitboard),
        _pipe@1 = erlang:'bsl'(_pipe, I * erlang:element(2, Bitboard)),
        erlang:'band'(_pipe@1, int_full_mask(Bitboard))
    end,
    _record = Bitboard,
    {bitboard, erlang:element(2, _record), erlang:element(3, _record), Val}.

-file("src/bibi/bitboard.gleam", 576).
?DOC(
    " Shifts the entire board towards the north by `i`\n"
    "\n"
    " i.e. shift_north by 1\n"
    " ````\n"
    " 100    100\n"
    " 100 -> 100\n"
    " 100    000\n"
    " ````\n"
).
-spec shift_north(bitboard(), integer()) -> {ok, bitboard()} | {error, binary()}.
shift_north(Bitboard, I) ->
    gleam@bool:guard(
        I =:= 0,
        {ok, Bitboard},
        fun() ->
            gleam@bool:guard(
                I < 0,
                {error, <<"shift_north by must be >= 0"/utf8>>},
                fun() -> {ok, shift_north_unvalidated(Bitboard, I)} end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 604).
-spec shift_south_unvalidated(bitboard(), integer()) -> bitboard().
shift_south_unvalidated(Bitboard, I) ->
    Val = begin
        _pipe = erlang:element(4, Bitboard),
        erlang:'bsr'(_pipe, I * erlang:element(2, Bitboard))
    end,
    _record = Bitboard,
    {bitboard, erlang:element(2, _record), erlang:element(3, _record), Val}.

-file("src/bibi/bitboard.gleam", 598).
?DOC(
    " Shifts the entire board towards the south by `i`\n"
    "\n"
    " i.e. shift_south by 1\n"
    " ````\n"
    " 100    000\n"
    " 100 -> 100\n"
    " 100    100\n"
    " ````\n"
).
-spec shift_south(bitboard(), integer()) -> {ok, bitboard()} | {error, binary()}.
shift_south(Bitboard, I) ->
    gleam@bool:guard(
        I =:= 0,
        {ok, Bitboard},
        fun() ->
            gleam@bool:guard(
                I < 0,
                {error, <<"shift_south by must be >= 0"/utf8>>},
                fun() -> {ok, shift_south_unvalidated(Bitboard, I)} end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 631).
-spec shift_west_unvalidated(bitboard(), integer()) -> bitboard().
shift_west_unvalidated(Bitboard, I) ->
    Mask = begin
        _pipe = gleam@list:range(0, I - 1),
        gleam@list:fold(
            _pipe,
            0,
            fun(M, I@1) ->
                _assert_subject = file(Bitboard, I@1),
                {ok, R} = case _assert_subject of
                    {ok, _} -> _assert_subject;
                    _assert_fail ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    value => _assert_fail,
                                    module => <<"bibi/bitboard"/utf8>>,
                                    function => <<"shift_west_unvalidated"/utf8>>,
                                    line => 635})
                end,
                erlang:'bor'(M, erlang:element(4, R))
            end
        )
    end,
    Updated_val = erlang:element(4, Bitboard) - erlang:'band'(
        Mask,
        erlang:element(4, Bitboard)
    ),
    Val = begin
        _pipe@1 = Updated_val,
        erlang:'bsr'(_pipe@1, I)
    end,
    _record = Bitboard,
    {bitboard, erlang:element(2, _record), erlang:element(3, _record), Val}.

-file("src/bibi/bitboard.gleam", 620).
?DOC(
    " Shifts the entire board towards the west by `i`. Note that westwards\n"
    " shifts will result in westmost occupied squares to be removed completely\n"
    "\n"
    " i.e. shift_west by 1\n"
    " ```\n"
    " 111    110\n"
    " 000 -> 000\n"
    " 000    000\n"
    " ```\n"
).
-spec shift_west(bitboard(), integer()) -> {ok, bitboard()} | {error, binary()}.
shift_west(Bitboard, I) ->
    gleam@bool:guard(
        I =:= 0,
        {ok, Bitboard},
        fun() ->
            gleam@bool:guard(
                I < 0,
                {error, <<"shift_west by must be >= 0"/utf8>>},
                fun() ->
                    gleam@bool:guard(
                        I >= erlang:element(2, Bitboard),
                        {error,
                            <<"shift_west by must be < bitboard.width"/utf8>>},
                        fun() -> {ok, shift_west_unvalidated(Bitboard, I)} end
                    )
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 663).
-spec shift_east_unvalidated(bitboard(), integer()) -> bitboard().
shift_east_unvalidated(Bitboard, I) ->
    Mask = begin
        _pipe = gleam@list:range(
            erlang:element(2, Bitboard) - 1,
            erlang:element(2, Bitboard) - I
        ),
        gleam@list:fold(
            _pipe,
            0,
            fun(M, I@1) ->
                _assert_subject = file(Bitboard, I@1),
                {ok, R} = case _assert_subject of
                    {ok, _} -> _assert_subject;
                    _assert_fail ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    value => _assert_fail,
                                    module => <<"bibi/bitboard"/utf8>>,
                                    function => <<"shift_east_unvalidated"/utf8>>,
                                    line => 667})
                end,
                erlang:'bor'(M, erlang:element(4, R))
            end
        )
    end,
    Updated_val = erlang:element(4, Bitboard) - erlang:'band'(
        Mask,
        erlang:element(4, Bitboard)
    ),
    Val = begin
        _pipe@1 = Updated_val,
        erlang:'bsl'(_pipe@1, I)
    end,
    _record = Bitboard,
    {bitboard, erlang:element(2, _record), erlang:element(3, _record), Val}.

-file("src/bibi/bitboard.gleam", 652).
?DOC(
    " Shifts the entire board towards the east by `i`. Note that eastwards\n"
    " shifts will result in eastmost occupied squares to be removed completely\n"
    "\n"
    " i.e. shift_east by 1\n"
    " ```\n"
    " 111    011\n"
    " 000 -> 000\n"
    " 000    000\n"
    " ```\n"
).
-spec shift_east(bitboard(), integer()) -> {ok, bitboard()} | {error, binary()}.
shift_east(Bitboard, I) ->
    gleam@bool:guard(
        I =:= 0,
        {ok, Bitboard},
        fun() ->
            gleam@bool:guard(
                I < 0,
                {error, <<"shift_east by must be >= 0"/utf8>>},
                fun() ->
                    gleam@bool:guard(
                        I >= erlang:element(2, Bitboard),
                        {error,
                            <<"shift_east by must be < bitboard.width"/utf8>>},
                        fun() -> {ok, shift_east_unvalidated(Bitboard, I)} end
                    )
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 410).
?DOC(
    " Diagonals are made up of squares that touch at thr corners, and stretch from the\n"
    " south eastern corner and towards the north western corner.\n"
    "\n"
    " In rectangular bitboards, the diagonals will appear as follows\n"
    "\n"
    " ```\n"
    " 0010\n"
    " 0100\n"
    " 1000\n"
    " ```\n"
    "\n"
    " Diagonals are indexed in the `width + rank - file`. In a bitboard of width 3 and\n"
    " height 4, the diagonals will be indexed as\n"
    " ```\n"
    " 5 . .\n"
    " 4 . .\n"
    " 3 . .\n"
    " 2 1 0\n"
    " ```\n"
    "\n"
    " A diagonal of index 3 in the above bitboard will look like this\n"
    " ```\n"
    " 001\n"
    " 010\n"
    " 100\n"
    " 000\n"
    " ```\n"
).
-spec diagonal(bitboard(), integer()) -> {ok, bitboard()} | {error, binary()}.
diagonal(Bitboard, Diagonal_no) ->
    Max_diagonal_no = (erlang:element(2, Bitboard) + erlang:element(3, Bitboard))
    - 2,
    gleam@bool:guard(
        Diagonal_no < 0,
        {error, <<"diagonal_no must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                Diagonal_no > Max_diagonal_no,
                {error,
                    <<"diagonal_no must be less than bitboard.width + bitboard.height - 1"/utf8>>},
                fun() ->
                    Main_diagonal = diagonal_from_south_west(Bitboard),
                    case {Diagonal_no < erlang:element(2, Bitboard),
                        erlang:element(2, Bitboard) < erlang:element(
                            3,
                            Bitboard
                        )} of
                        {true, true} ->
                            shift_south(
                                Main_diagonal,
                                (erlang:element(2, Bitboard) - Diagonal_no) - 1
                            );

                        {true, false} ->
                            shift_east(
                                Main_diagonal,
                                (erlang:element(2, Bitboard) - Diagonal_no) - 1
                            );

                        {false, true} ->
                            shift_north(
                                Main_diagonal,
                                (Diagonal_no - erlang:element(2, Bitboard)) + 1
                            );

                        {false, false} ->
                            shift_west(
                                Main_diagonal,
                                (Diagonal_no - erlang:element(2, Bitboard)) + 1
                            )
                    end
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 455).
?DOC(
    " Antidiagonals are made up of squares that touch at thr corners, and stretch from the\n"
    " south eastern corner and towards the north western corner.\n"
    "\n"
    " In rectangular bitboards, the anti-diagonals will appear as follows\n"
    "\n"
    " ```\n"
    " 1000\n"
    " 0100\n"
    " 0010\n"
    " ```\n"
    "\n"
    " Antidiagonals are indexed in the `rank + file`. In a bitboard of width 3 and\n"
    " height 4, the anti-diagonals will be indexed as\n"
    " ```\n"
    " . . 5\n"
    " . . 4\n"
    " . . 3\n"
    " 0 1 2\n"
    " ```\n"
    "\n"
    " A anti-diagonal of index 3 in the above bitboard will look like this\n"
    " ```\n"
    " 100\n"
    " 010\n"
    " 001\n"
    " 000\n"
    " ```\n"
).
-spec antidiagonal(bitboard(), integer()) -> {ok, bitboard()} |
    {error, binary()}.
antidiagonal(Bitboard, Antidiagonal_no) ->
    Max_antidiagonal_no = (erlang:element(2, Bitboard) + erlang:element(
        3,
        Bitboard
    ))
    - 2,
    gleam@bool:guard(
        Antidiagonal_no < 0,
        {error, <<"antidiagonal_no must be positive"/utf8>>},
        fun() ->
            gleam@bool:guard(
                Antidiagonal_no > Max_antidiagonal_no,
                {error,
                    <<"antidiagonal_no must be less than bitboard.width + bitboard.height - 1"/utf8>>},
                fun() ->
                    Main_diagonal = antidiagonal_from_south_east(Bitboard),
                    case {Antidiagonal_no < erlang:element(2, Bitboard),
                        erlang:element(2, Bitboard) < erlang:element(
                            3,
                            Bitboard
                        )} of
                        {true, true} ->
                            shift_south(
                                Main_diagonal,
                                (erlang:element(2, Bitboard) - Antidiagonal_no)
                                - 1
                            );

                        {true, false} ->
                            shift_west(
                                Main_diagonal,
                                (erlang:element(2, Bitboard) - Antidiagonal_no)
                                - 1
                            );

                        {false, true} ->
                            shift_north(
                                Main_diagonal,
                                (Antidiagonal_no - erlang:element(2, Bitboard))
                                + 1
                            );

                        {false, false} ->
                            shift_east(
                                Main_diagonal,
                                (Antidiagonal_no - erlang:element(2, Bitboard))
                                + 1
                            )
                    end
                end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 683).
?DOC(
    " Shifts the entire boards towards the northeast by `i`.\n"
    "\n"
    " i.e. shift_northeast by 1\n"
    " ```\n"
    " 001    001\n"
    " 011 -> 000\n"
    " 000    000\n"
    " ```\n"
).
-spec shift_northeast(bitboard(), integer()) -> {ok, bitboard()} |
    {error, binary()}.
shift_northeast(Bitboard, I) ->
    gleam@bool:guard(
        I =:= 0,
        {ok, Bitboard},
        fun() ->
            gleam@bool:guard(
                I < 0,
                {error, <<"shift_northeast by must be >= 0"/utf8>>},
                fun() -> _pipe = Bitboard,
                    _pipe@1 = shift_east_unvalidated(_pipe, I),
                    _pipe@2 = shift_north_unvalidated(_pipe@1, I),
                    {ok, _pipe@2} end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 700).
?DOC(
    " Shifts the entire boards towards the northwest by `i`.\n"
    "\n"
    " i.e. shift_northwest by 1\n"
    " ```\n"
    " 001    110\n"
    " 011 -> 000\n"
    " 000    000\n"
    " ```\n"
).
-spec shift_northwest(bitboard(), integer()) -> {ok, bitboard()} |
    {error, binary()}.
shift_northwest(Bitboard, I) ->
    gleam@bool:guard(
        I =:= 0,
        {ok, Bitboard},
        fun() ->
            gleam@bool:guard(
                I < 0,
                {error, <<"shift_northwest by must be >= 0"/utf8>>},
                fun() -> _pipe = Bitboard,
                    _pipe@1 = shift_west_unvalidated(_pipe, I),
                    _pipe@2 = shift_north_unvalidated(_pipe@1, I),
                    {ok, _pipe@2} end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 717).
?DOC(
    " Shifts the entire boards towards the southeast by `i`.\n"
    "\n"
    " i.e. shift_southeast by 1\n"
    " ```\n"
    " 001    000\n"
    " 011 -> 000\n"
    " 000    001\n"
    " ```\n"
).
-spec shift_southeast(bitboard(), integer()) -> {ok, bitboard()} |
    {error, binary()}.
shift_southeast(Bitboard, I) ->
    gleam@bool:guard(
        I =:= 0,
        {ok, Bitboard},
        fun() ->
            gleam@bool:guard(
                I < 0,
                {error, <<"shift_southeast by must be >= 0"/utf8>>},
                fun() -> _pipe = Bitboard,
                    _pipe@1 = shift_east_unvalidated(_pipe, I),
                    _pipe@2 = shift_south_unvalidated(_pipe@1, I),
                    {ok, _pipe@2} end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 734).
?DOC(
    " Shifts the entire boards towards the southwest by `i`.\n"
    "\n"
    " i.e. shift_southwest by 1\n"
    " ```\n"
    " 001    000\n"
    " 011 -> 010\n"
    " 000    110\n"
    " ```\n"
).
-spec shift_southwest(bitboard(), integer()) -> {ok, bitboard()} |
    {error, binary()}.
shift_southwest(Bitboard, I) ->
    gleam@bool:guard(
        I =:= 0,
        {ok, Bitboard},
        fun() ->
            gleam@bool:guard(
                I < 0,
                {error, <<"shift_southwest by must be >= 0"/utf8>>},
                fun() -> _pipe = Bitboard,
                    _pipe@1 = shift_west_unvalidated(_pipe, I),
                    _pipe@2 = shift_south_unvalidated(_pipe@1, I),
                    {ok, _pipe@2} end
            )
        end
    ).

-file("src/bibi/bitboard.gleam", 751).
?DOC(
    " Flips a bitboard vertically\n"
    "\n"
    " i.e\n"
    " ````\n"
    " 111    000\n"
    " 000 -> 000\n"
    " 000    111\n"
    " ````\n"
).
-spec flip_vertically(bitboard()) -> bitboard().
flip_vertically(Bitboard) ->
    _pipe = gleam@list:range(0, erlang:element(3, Bitboard) - 1),
    gleam@list:fold(
        _pipe,
        begin
            _record = Bitboard,
            {bitboard,
                erlang:element(2, _record),
                erlang:element(3, _record),
                0}
        end,
        fun(B, I) ->
            _assert_subject = rank(Bitboard, I),
            {ok, Rank_mask} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_vertically"/utf8>>,
                                line => 754})
            end,
            _assert_subject@1 = bitboard_and(Bitboard, Rank_mask),
            {ok, Rank} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_vertically"/utf8>>,
                                line => 755})
            end,
            _assert_subject@2 = shift_south(Rank, I),
            {ok, Rank@1} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@2,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_vertically"/utf8>>,
                                line => 756})
            end,
            _assert_subject@3 = shift_north(
                Rank@1,
                (erlang:element(3, Bitboard) - I) - 1
            ),
            {ok, Rank@2} = case _assert_subject@3 of
                {ok, _} -> _assert_subject@3;
                _assert_fail@3 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@3,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_vertically"/utf8>>,
                                line => 757})
            end,
            _assert_subject@4 = bitboard_or(B, Rank@2),
            {ok, Updated_bitboard} = case _assert_subject@4 of
                {ok, _} -> _assert_subject@4;
                _assert_fail@4 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@4,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_vertically"/utf8>>,
                                line => 758})
            end,
            Updated_bitboard
        end
    ).

-file("src/bibi/bitboard.gleam", 771).
?DOC(
    " Flips a bitboard horizontally\n"
    "\n"
    " i.e\n"
    " ````\n"
    " 100    001\n"
    " 100 -> 001\n"
    " 100    001\n"
    " ````\n"
).
-spec flip_horizontally(bitboard()) -> bitboard().
flip_horizontally(Bitboard) ->
    _pipe = gleam@list:range(0, erlang:element(2, Bitboard) - 1),
    gleam@list:fold(
        _pipe,
        begin
            _record = Bitboard,
            {bitboard,
                erlang:element(2, _record),
                erlang:element(3, _record),
                0}
        end,
        fun(B, I) ->
            _assert_subject = file(Bitboard, I),
            {ok, File_mask} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_horizontally"/utf8>>,
                                line => 774})
            end,
            _assert_subject@1 = bitboard_and(Bitboard, File_mask),
            {ok, File} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_horizontally"/utf8>>,
                                line => 775})
            end,
            _assert_subject@2 = shift_west(File, I),
            {ok, File@1} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@2,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_horizontally"/utf8>>,
                                line => 776})
            end,
            _assert_subject@3 = shift_east(
                File@1,
                (erlang:element(2, Bitboard) - I) - 1
            ),
            {ok, File@2} = case _assert_subject@3 of
                {ok, _} -> _assert_subject@3;
                _assert_fail@3 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@3,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_horizontally"/utf8>>,
                                line => 777})
            end,
            _assert_subject@4 = bitboard_or(B, File@2),
            {ok, Updated_bitboard} = case _assert_subject@4 of
                {ok, _} -> _assert_subject@4;
                _assert_fail@4 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@4,
                                module => <<"bibi/bitboard"/utf8>>,
                                function => <<"flip_horizontally"/utf8>>,
                                line => 778})
            end,
            Updated_bitboard
        end
    ).
