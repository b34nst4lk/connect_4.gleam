-module(glexer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, discard_whitespace/1, discard_comments/1, to_source/1, unescape_string/1, lex/1]).
-export_type([lexer/0, lexer_mode/0, position/0, lex_number_mode/0]).

-opaque lexer() :: {lexer,
        binary(),
        integer(),
        boolean(),
        boolean(),
        lexer_mode()}.

-type lexer_mode() :: normal |
    check_for_minus |
    check_for_nested_dot |
    check_for_nested_dot_or_minus |
    has_nested_dot.

-type position() :: {position, integer()}.

-type lex_number_mode() :: lex_int | lex_float | lex_float_exponent.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 30).
-spec new(binary()) -> lexer().
new(Source) ->
    {lexer, Source, 0, true, true, normal}.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 40).
-spec discard_whitespace(lexer()) -> lexer().
discard_whitespace(Lexer) ->
    erlang:setelement(4, Lexer, false).

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 44).
-spec discard_comments(lexer()) -> lexer().
discard_comments(Lexer) ->
    erlang:setelement(5, Lexer, false).

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 685).
-spec advance(lexer(), binary(), integer()) -> lexer().
advance(Lexer, Source, Offset) ->
    erlang:setelement(
        3,
        erlang:setelement(2, Lexer, Source),
        erlang:element(3, Lexer) + Offset
    ).

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 668).
-spec take_while(lexer(), binary(), fun((binary()) -> boolean())) -> {lexer(),
    binary()}.
take_while(Lexer, Content, Predicate) ->
    case gleam_stdlib:string_pop_grapheme(erlang:element(2, Lexer)) of
        {error, _} ->
            {Lexer, Content};

        {ok, {Grapheme, Source}} ->
            case Predicate(Grapheme) of
                true ->
                    _pipe = advance(Lexer, Source, erlang:byte_size(Grapheme)),
                    take_while(
                        _pipe,
                        <<Content/binary, Grapheme/binary>>,
                        Predicate
                    );

                false ->
                    {Lexer, Content}
            end
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 689).
-spec advanced({glexer@token:token(), position()}, lexer(), binary(), integer()) -> {lexer(),
    {glexer@token:token(), position()}}.
advanced(Token, Lexer, Source, Offset) ->
    {advance(Lexer, Source, Offset), Token}.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 698).
-spec token(lexer(), glexer@token:token(), binary(), integer()) -> {lexer(),
    {glexer@token:token(), position()}}.
token(Lexer, Token, Source, Offset) ->
    _pipe = {Token, {position, erlang:element(3, Lexer)}},
    advanced(_pipe, Lexer, Source, Offset).

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 349).
-spec check_for_minus(lexer()) -> {ok,
        {lexer(), {glexer@token:token(), position()}}} |
    {error, nil}.
check_for_minus(Lexer) ->
    case erlang:element(2, Lexer) of
        <<"-"/utf8, Source/binary>> ->
            {Lexer@1, Token} = token(Lexer, minus, Source, 1),
            {ok, {erlang:setelement(6, Lexer@1, normal), Token}};

        _ ->
            {error, nil}
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 361).
-spec check_for_nested_dot(lexer()) -> {ok,
        {lexer(), {glexer@token:token(), position()}}} |
    {error, nil}.
check_for_nested_dot(Lexer) ->
    case erlang:element(2, Lexer) of
        <<".."/utf8, Source/binary>> ->
            {ok, token(Lexer, dot_dot, Source, 2)};

        <<"."/utf8, Source@1/binary>> ->
            case Source@1 of
                <<"0"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                <<"1"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                <<"2"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                <<"3"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                <<"4"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                <<"5"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                <<"6"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                <<"7"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                <<"8"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                <<"9"/utf8, _/binary>> ->
                    {Lexer@1, Token} = token(Lexer, dot, Source@1, 1),
                    {ok, {erlang:setelement(6, Lexer@1, has_nested_dot), Token}};

                _ ->
                    {ok, token(Lexer, dot, Source@1, 1)}
            end;

        _ ->
            {error, nil}
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 439).
-spec lex_binary(lexer(), binary(), integer()) -> {lexer(),
    {glexer@token:token(), position()}}.
lex_binary(Lexer, Content, Start) ->
    case erlang:element(2, Lexer) of
        <<C:1/binary, Source/binary>> when C =:= <<"_"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_binary(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"0"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_binary(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"1"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_binary(_pipe, <<Content/binary, C/binary>>, Start);

        _ ->
            {Lexer, {{int, Content}, {position, Start}}}
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 453).
-spec lex_octal(lexer(), binary(), integer()) -> {lexer(),
    {glexer@token:token(), position()}}.
lex_octal(Lexer, Content, Start) ->
    case erlang:element(2, Lexer) of
        <<C:1/binary, Source/binary>> when C =:= <<"_"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"0"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"1"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"2"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"3"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"4"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"5"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"6"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"7"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_octal(_pipe, <<Content/binary, C/binary>>, Start);

        _ ->
            {Lexer, {{int, Content}, {position, Start}}}
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 475).
-spec lex_hexadecimal(lexer(), binary(), integer()) -> {lexer(),
    {glexer@token:token(), position()}}.
lex_hexadecimal(Lexer, Content, Start) ->
    case erlang:element(2, Lexer) of
        <<C:1/binary, Source/binary>> when C =:= <<"_"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"0"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"1"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"2"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"3"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"4"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"5"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"6"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"7"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"8"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"9"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"a"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"A"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"b"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"B"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"c"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"C"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"d"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"D"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"e"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"E"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"f"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"F"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_hexadecimal(_pipe, <<Content/binary, C/binary>>, Start);

        _ ->
            {Lexer, {{int, Content}, {position, Start}}}
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 517).
-spec lex_number(lexer(), binary(), lex_number_mode(), integer()) -> {lexer(),
    {glexer@token:token(), position()}}.
lex_number(Lexer, Content, Mode, Start) ->
    case {erlang:element(2, Lexer), Mode} of
        {<<C:1/binary, Source/binary>>, _} when C =:= <<"_"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"0"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"1"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"2"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"3"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"4"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"5"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"6"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"7"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"8"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C:1/binary, Source/binary>>, _} when C =:= <<"9"/utf8>> ->
            _pipe = advance(Lexer, Source, 1),
            lex_number(_pipe, <<Content/binary, C/binary>>, Mode, Start);

        {<<C@1:1/binary, Source@1/binary>>, lex_int} when C@1 =:= <<"."/utf8>> ->
            _pipe@1 = advance(Lexer, Source@1, 1),
            lex_number(
                _pipe@1,
                <<Content/binary, C@1/binary>>,
                lex_float,
                Start
            );

        {<<C@2:2/binary, Source@2/binary>>, lex_float} when C@2 =:= <<"e-"/utf8>> ->
            _pipe@2 = advance(Lexer, Source@2, 2),
            lex_number(
                _pipe@2,
                <<Content/binary, C@2/binary>>,
                lex_float_exponent,
                Start
            );

        {<<C@3:1/binary, Source@3/binary>>, lex_float} when C@3 =:= <<"e"/utf8>> ->
            _pipe@3 = advance(Lexer, Source@3, 1),
            lex_number(
                _pipe@3,
                <<Content/binary, C@3/binary>>,
                lex_float_exponent,
                Start
            );

        {_, lex_int} ->
            Lexer@1 = erlang:setelement(6, Lexer, check_for_minus),
            {Lexer@1, {{int, Content}, {position, Start}}};

        {_, lex_float} ->
            Lexer@2 = erlang:setelement(6, Lexer, check_for_minus),
            {Lexer@2, {{float, Content}, {position, Start}}};

        {_, lex_float_exponent} ->
            Lexer@2 = erlang:setelement(6, Lexer, check_for_minus),
            {Lexer@2, {{float, Content}, {position, Start}}}
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 566).
-spec lex_string(lexer(), binary(), integer()) -> {lexer(),
    {glexer@token:token(), position()}}.
lex_string(Lexer, Content, Start) ->
    case erlang:element(2, Lexer) of
        <<"\""/utf8, Source/binary>> ->
            _pipe = {{string, Content}, {position, Start}},
            advanced(_pipe, Lexer, Source, 1);

        <<C:1/binary, Source@1/binary>> when C =:= <<"\\"/utf8>> ->
            case gleam_stdlib:string_pop_grapheme(Source@1) of
                {error, _} ->
                    _pipe@1 = advance(Lexer, Source@1, 1),
                    lex_string(_pipe@1, <<Content/binary, C/binary>>, Start);

                {ok, {Grapheme, Source@2}} ->
                    Offset = 1 + erlang:byte_size(Grapheme),
                    _pipe@2 = advance(Lexer, Source@2, Offset),
                    lex_string(
                        _pipe@2,
                        <<<<Content/binary, C/binary>>/binary, Grapheme/binary>>,
                        Start
                    )
            end;

        _ ->
            case gleam_stdlib:string_pop_grapheme(erlang:element(2, Lexer)) of
                {error, _} ->
                    {Lexer, {{unterminated_string, Content}, {position, Start}}};

                {ok, {Grapheme@1, Source@3}} ->
                    _pipe@3 = advance(
                        Lexer,
                        Source@3,
                        erlang:byte_size(Grapheme@1)
                    ),
                    lex_string(
                        _pipe@3,
                        <<Content/binary, Grapheme@1/binary>>,
                        Start
                    )
            end
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 659).
-spec to_source(list({glexer@token:token(), position()})) -> binary().
to_source(Tokens) ->
    gleam@list:fold(
        Tokens,
        <<""/utf8>>,
        fun(Source, _use1) ->
            {Tok, _} = _use1,
            <<Source/binary, (glexer@token:to_source(Tok))/binary>>
        end
    ).

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 639).
-spec unescape_codepoint(binary(), binary(), binary()) -> {ok, binary()} |
    {error, nil}.
unescape_codepoint(Escaped, Unescaped, Codepoint) ->
    case gleam_stdlib:string_pop_grapheme(Escaped) of
        {ok, {<<"}"/utf8>>, Escaped@1}} ->
            gleam@result:'try'(
                gleam@int:base_parse(Codepoint, 16),
                fun(Codepoint@1) ->
                    gleam@result:'try'(
                        gleam@string:utf_codepoint(Codepoint@1),
                        fun(Codepoint@2) ->
                            Codepoint@3 = gleam_stdlib:utf_codepoint_list_to_string(
                                [Codepoint@2]
                            ),
                            unescape_loop(
                                Escaped@1,
                                <<Unescaped/binary, Codepoint@3/binary>>
                            )
                        end
                    )
                end
            );

        {ok, {C, Escaped@2}} ->
            unescape_codepoint(
                Escaped@2,
                Unescaped,
                <<Codepoint/binary, C/binary>>
            );

        {error, nil} ->
            {error, nil}
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 620).
-spec unescape_loop(binary(), binary()) -> {ok, binary()} | {error, nil}.
unescape_loop(Escaped, Unescaped) ->
    case Escaped of
        <<"\\\""/utf8, Escaped@1/binary>> ->
            unescape_loop(Escaped@1, <<Unescaped/binary, "\""/utf8>>);

        <<"\\\\"/utf8, Escaped@2/binary>> ->
            unescape_loop(Escaped@2, <<Unescaped/binary, "\\"/utf8>>);

        <<"\\f"/utf8, Escaped@3/binary>> ->
            unescape_loop(Escaped@3, <<Unescaped/binary, "\f"/utf8>>);

        <<"\\n"/utf8, Escaped@4/binary>> ->
            unescape_loop(Escaped@4, <<Unescaped/binary, "\n"/utf8>>);

        <<"\\r"/utf8, Escaped@5/binary>> ->
            unescape_loop(Escaped@5, <<Unescaped/binary, "\r"/utf8>>);

        <<"\\t"/utf8, Escaped@6/binary>> ->
            unescape_loop(Escaped@6, <<Unescaped/binary, "\t"/utf8>>);

        <<"\\u{"/utf8, Escaped@7/binary>> ->
            unescape_codepoint(Escaped@7, Unescaped, <<""/utf8>>);

        <<"\\"/utf8, _/binary>> ->
            {error, nil};

        _ ->
            case gleam_stdlib:string_pop_grapheme(Escaped) of
                {error, _} ->
                    {ok, Unescaped};

                {ok, {Grapheme, Escaped@8}} ->
                    unescape_loop(
                        Escaped@8,
                        <<Unescaped/binary, Grapheme/binary>>
                    )
            end
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 616).
-spec unescape_string(binary()) -> {ok, binary()} | {error, nil}.
unescape_string(String) ->
    unescape_loop(String, <<""/utf8>>).

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 390).
-spec whitespace(lexer(), binary(), integer()) -> {lexer(),
    {glexer@token:token(), position()}}.
whitespace(Lexer, Content, Start) ->
    case erlang:element(2, Lexer) of
        <<C:1/binary, Source/binary>> when C =:= <<" "/utf8>> ->
            _pipe = advance(Lexer, Source, erlang:byte_size(C)),
            whitespace(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"\t"/utf8>> ->
            _pipe = advance(Lexer, Source, erlang:byte_size(C)),
            whitespace(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"\n"/utf8>> ->
            _pipe = advance(Lexer, Source, erlang:byte_size(C)),
            whitespace(_pipe, <<Content/binary, C/binary>>, Start);

        <<C:1/binary, Source/binary>> when C =:= <<"\r"/utf8>> ->
            _pipe = advance(Lexer, Source, erlang:byte_size(C)),
            whitespace(_pipe, <<Content/binary, C/binary>>, Start);

        _ ->
            case erlang:element(4, Lexer) of
                false ->
                    next(Lexer);

                true ->
                    {Lexer, {{space, Content}, {position, Start}}}
            end
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 60).
-spec next(lexer()) -> {lexer(), {glexer@token:token(), position()}}.
next(Lexer) ->
    case erlang:element(6, Lexer) of
        check_for_minus ->
            case check_for_minus(Lexer) of
                {ok, {Lexer@1, Token}} ->
                    {Lexer@1, Token};

                {error, nil} ->
                    next(erlang:setelement(6, Lexer, normal))
            end;

        check_for_nested_dot ->
            case check_for_nested_dot(Lexer) of
                {ok, {Lexer@2, Token@1}} ->
                    {Lexer@2, Token@1};

                {error, nil} ->
                    next(erlang:setelement(6, Lexer, normal))
            end;

        check_for_nested_dot_or_minus ->
            case check_for_nested_dot(Lexer) of
                {ok, {Lexer@3, Token@2}} ->
                    {Lexer@3, Token@2};

                {error, nil} ->
                    case check_for_minus(Lexer) of
                        {ok, {Lexer@4, Token@3}} ->
                            {Lexer@4, Token@3};

                        {error, nil} ->
                            next(erlang:setelement(6, Lexer, normal))
                    end
            end;

        has_nested_dot ->
            case erlang:element(2, Lexer) of
                <<C:1/binary, Source/binary>> when C =:= <<"0"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                <<C:1/binary, Source/binary>> when C =:= <<"1"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                <<C:1/binary, Source/binary>> when C =:= <<"2"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                <<C:1/binary, Source/binary>> when C =:= <<"3"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                <<C:1/binary, Source/binary>> when C =:= <<"4"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                <<C:1/binary, Source/binary>> when C =:= <<"5"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                <<C:1/binary, Source/binary>> when C =:= <<"6"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                <<C:1/binary, Source/binary>> when C =:= <<"7"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                <<C:1/binary, Source/binary>> when C =:= <<"8"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                <<C:1/binary, Source/binary>> when C =:= <<"9"/utf8>> ->
                    Byte_offset = erlang:element(3, Lexer),
                    {Lexer@5, Int} = begin
                        _pipe = advance(Lexer, Source, 1),
                        take_while(
                            _pipe,
                            C,
                            fun glexer@internal@predicates:is_digit/1
                        )
                    end,
                    Lexer@6 = erlang:setelement(
                        6,
                        Lexer@5,
                        check_for_nested_dot
                    ),
                    {Lexer@6, {{int, Int}, {position, Byte_offset}}};

                _ ->
                    next(erlang:setelement(6, Lexer, normal))
            end;

        normal ->
            case erlang:element(2, Lexer) of
                <<C@1:1/binary, Source@1/binary>> when C@1 =:= <<" "/utf8>> ->
                    _pipe@1 = advance(Lexer, Source@1, 1),
                    whitespace(_pipe@1, C@1, erlang:element(3, Lexer));

                <<C@1:1/binary, Source@1/binary>> when C@1 =:= <<"\n"/utf8>> ->
                    _pipe@1 = advance(Lexer, Source@1, 1),
                    whitespace(_pipe@1, C@1, erlang:element(3, Lexer));

                <<C@1:1/binary, Source@1/binary>> when C@1 =:= <<"\r"/utf8>> ->
                    _pipe@1 = advance(Lexer, Source@1, 1),
                    whitespace(_pipe@1, C@1, erlang:element(3, Lexer));

                <<C@1:1/binary, Source@1/binary>> when C@1 =:= <<"\t"/utf8>> ->
                    _pipe@1 = advance(Lexer, Source@1, 1),
                    whitespace(_pipe@1, C@1, erlang:element(3, Lexer));

                <<"////"/utf8, Source@2/binary>> ->
                    _pipe@2 = advance(Lexer, Source@2, 4),
                    comment(
                        _pipe@2,
                        fun(Field@0) -> {comment_module, Field@0} end,
                        <<""/utf8>>,
                        erlang:element(3, Lexer)
                    );

                <<"///"/utf8, Source@3/binary>> ->
                    _pipe@3 = advance(Lexer, Source@3, 3),
                    comment(
                        _pipe@3,
                        fun(Field@0) -> {comment_doc, Field@0} end,
                        <<""/utf8>>,
                        erlang:element(3, Lexer)
                    );

                <<"//"/utf8, Source@4/binary>> ->
                    _pipe@4 = advance(Lexer, Source@4, 2),
                    comment(
                        _pipe@4,
                        fun(Field@0) -> {comment_normal, Field@0} end,
                        <<""/utf8>>,
                        erlang:element(3, Lexer)
                    );

                <<"("/utf8, Source@5/binary>> ->
                    token(Lexer, left_paren, Source@5, 1);

                <<")"/utf8, Source@6/binary>> ->
                    token(Lexer, right_paren, Source@6, 1);

                <<"{"/utf8, Source@7/binary>> ->
                    token(Lexer, left_brace, Source@7, 1);

                <<"}"/utf8, Source@8/binary>> ->
                    token(Lexer, right_brace, Source@8, 1);

                <<"["/utf8, Source@9/binary>> ->
                    token(Lexer, left_square, Source@9, 1);

                <<"]"/utf8, Source@10/binary>> ->
                    token(Lexer, right_square, Source@10, 1);

                <<"@"/utf8, Source@11/binary>> ->
                    token(Lexer, at, Source@11, 1);

                <<":"/utf8, Source@12/binary>> ->
                    token(Lexer, colon, Source@12, 1);

                <<","/utf8, Source@13/binary>> ->
                    token(Lexer, comma, Source@13, 1);

                <<".."/utf8, Source@14/binary>> ->
                    token(Lexer, dot_dot, Source@14, 2);

                <<"."/utf8, Source@15/binary>> ->
                    token(Lexer, dot, Source@15, 1);

                <<"#"/utf8, Source@16/binary>> ->
                    token(Lexer, hash, Source@16, 1);

                <<"!="/utf8, Source@17/binary>> ->
                    token(Lexer, not_equal, Source@17, 2);

                <<"!"/utf8, Source@18/binary>> ->
                    token(Lexer, bang, Source@18, 1);

                <<"=="/utf8, Source@19/binary>> ->
                    token(Lexer, equal_equal, Source@19, 2);

                <<"="/utf8, Source@20/binary>> ->
                    token(Lexer, equal, Source@20, 1);

                <<"|>"/utf8, Source@21/binary>> ->
                    token(Lexer, pipe, Source@21, 2);

                <<"||"/utf8, Source@22/binary>> ->
                    token(Lexer, v_bar_v_bar, Source@22, 2);

                <<"|"/utf8, Source@23/binary>> ->
                    token(Lexer, v_bar, Source@23, 1);

                <<"&&"/utf8, Source@24/binary>> ->
                    token(Lexer, amper_amper, Source@24, 2);

                <<"<<"/utf8, Source@25/binary>> ->
                    token(Lexer, less_less, Source@25, 2);

                <<">>"/utf8, Source@26/binary>> ->
                    token(Lexer, greater_greater, Source@26, 2);

                <<"<-"/utf8, Source@27/binary>> ->
                    token(Lexer, left_arrow, Source@27, 2);

                <<"->"/utf8, Source@28/binary>> ->
                    token(Lexer, right_arrow, Source@28, 2);

                <<"<>"/utf8, Source@29/binary>> ->
                    token(Lexer, less_greater, Source@29, 2);

                <<"+."/utf8, Source@30/binary>> ->
                    token(Lexer, plus_dot, Source@30, 2);

                <<"-."/utf8, Source@31/binary>> ->
                    token(Lexer, minus_dot, Source@31, 2);

                <<"*."/utf8, Source@32/binary>> ->
                    token(Lexer, star_dot, Source@32, 2);

                <<"/."/utf8, Source@33/binary>> ->
                    token(Lexer, slash_dot, Source@33, 2);

                <<"<=."/utf8, Source@34/binary>> ->
                    token(Lexer, less_equal_dot, Source@34, 3);

                <<"<."/utf8, Source@35/binary>> ->
                    token(Lexer, less_dot, Source@35, 2);

                <<">=."/utf8, Source@36/binary>> ->
                    token(Lexer, greater_equal_dot, Source@36, 3);

                <<">."/utf8, Source@37/binary>> ->
                    token(Lexer, greater_dot, Source@37, 2);

                <<C@2:2/binary, Source@38/binary>> when C@2 =:= <<"0b"/utf8>> ->
                    _pipe@5 = advance(Lexer, Source@38, 2),
                    lex_binary(_pipe@5, C@2, erlang:element(3, Lexer));

                <<C@3:2/binary, Source@39/binary>> when C@3 =:= <<"0o"/utf8>> ->
                    _pipe@6 = advance(Lexer, Source@39, 2),
                    lex_octal(_pipe@6, C@3, erlang:element(3, Lexer));

                <<C@4:2/binary, Source@40/binary>> when C@4 =:= <<"0x"/utf8>> ->
                    _pipe@7 = advance(Lexer, Source@40, 2),
                    lex_hexadecimal(_pipe@7, C@4, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"0"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"1"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"2"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"3"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"4"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"5"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"6"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"7"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"8"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@5:1/binary, Source@41/binary>> when C@5 =:= <<"9"/utf8>> ->
                    _pipe@8 = advance(Lexer, Source@41, 1),
                    lex_number(_pipe@8, C@5, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-0"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-1"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-2"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-3"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-4"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-5"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-6"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-7"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-8"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<C@6:2/binary, Source@42/binary>> when C@6 =:= <<"-9"/utf8>> ->
                    _pipe@9 = advance(Lexer, Source@42, 2),
                    lex_number(_pipe@9, C@6, lex_int, erlang:element(3, Lexer));

                <<"+"/utf8, Source@43/binary>> ->
                    token(Lexer, plus, Source@43, 1);

                <<"-"/utf8, Source@44/binary>> ->
                    token(Lexer, minus, Source@44, 1);

                <<"*"/utf8, Source@45/binary>> ->
                    token(Lexer, star, Source@45, 1);

                <<"/"/utf8, Source@46/binary>> ->
                    token(Lexer, slash, Source@46, 1);

                <<"<="/utf8, Source@47/binary>> ->
                    token(Lexer, less_equal, Source@47, 2);

                <<"<"/utf8, Source@48/binary>> ->
                    token(Lexer, less, Source@48, 1);

                <<">="/utf8, Source@49/binary>> ->
                    token(Lexer, greater_equal, Source@49, 2);

                <<">"/utf8, Source@50/binary>> ->
                    token(Lexer, greater, Source@50, 1);

                <<"%"/utf8, Source@51/binary>> ->
                    token(Lexer, percent, Source@51, 1);

                <<"\""/utf8, Source@52/binary>> ->
                    _pipe@10 = advance(Lexer, Source@52, 1),
                    lex_string(_pipe@10, <<""/utf8>>, erlang:element(3, Lexer));

                <<"_"/utf8, Source@53/binary>> ->
                    Byte_offset@1 = erlang:element(3, Lexer),
                    {Lexer@7, Name} = begin
                        _pipe@11 = advance(Lexer, Source@53, 1),
                        take_while(
                            _pipe@11,
                            <<""/utf8>>,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    {Lexer@7, {{discard_name, Name}, {position, Byte_offset@1}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"a"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"b"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"c"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"d"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"e"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"f"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"g"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"h"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"i"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"j"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"k"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"l"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"m"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"n"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"o"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"p"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"q"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"r"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"s"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"t"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"u"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"v"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"w"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"x"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"y"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@7:1/binary, Source@54/binary>> when C@7 =:= <<"z"/utf8>> ->
                    Byte_offset@2 = erlang:element(3, Lexer),
                    {Lexer@8, Name@1} = begin
                        _pipe@12 = advance(Lexer, Source@54, 1),
                        take_while(
                            _pipe@12,
                            C@7,
                            fun glexer@internal@predicates:is_name_grapheme/1
                        )
                    end,
                    Token@4 = case Name@1 of
                        <<"as"/utf8>> ->
                            as;

                        <<"assert"/utf8>> ->
                            assert;

                        <<"auto"/utf8>> ->
                            auto;

                        <<"case"/utf8>> ->
                            'case';

                        <<"const"/utf8>> ->
                            const;

                        <<"delegate"/utf8>> ->
                            delegate;

                        <<"derive"/utf8>> ->
                            derive;

                        <<"echo"/utf8>> ->
                            echo;

                        <<"else"/utf8>> ->
                            'else';

                        <<"fn"/utf8>> ->
                            fn;

                        <<"if"/utf8>> ->
                            'if';

                        <<"implement"/utf8>> ->
                            implement;

                        <<"import"/utf8>> ->
                            import;

                        <<"let"/utf8>> ->
                            'let';

                        <<"macro"/utf8>> ->
                            macro;

                        <<"opaque"/utf8>> ->
                            opaque;

                        <<"panic"/utf8>> ->
                            panic;

                        <<"pub"/utf8>> ->
                            pub;

                        <<"test"/utf8>> ->
                            test;

                        <<"todo"/utf8>> ->
                            todo;

                        <<"type"/utf8>> ->
                            type;

                        <<"use"/utf8>> ->
                            use;

                        _ ->
                            {name, Name@1}
                    end,
                    Lexer@9 = erlang:setelement(
                        6,
                        Lexer@8,
                        check_for_nested_dot_or_minus
                    ),
                    {Lexer@9, {Token@4, {position, Byte_offset@2}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"A"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"B"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"C"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"D"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"E"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"F"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"G"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"H"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"I"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"J"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"K"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"L"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"M"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"N"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"O"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"P"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"Q"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"R"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"S"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"T"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"U"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"V"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"W"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"X"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"Y"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                <<C@8:1/binary, Source@55/binary>> when C@8 =:= <<"Z"/utf8>> ->
                    Byte_offset@3 = erlang:element(3, Lexer),
                    {Lexer@10, Name@2} = begin
                        _pipe@13 = advance(Lexer, Source@55, 1),
                        take_while(
                            _pipe@13,
                            C@8,
                            fun glexer@internal@predicates:is_upname_grapheme/1
                        )
                    end,
                    {Lexer@10,
                        {{upper_name, Name@2}, {position, Byte_offset@3}}};

                _ ->
                    case gleam_stdlib:string_pop_grapheme(
                        erlang:element(2, Lexer)
                    ) of
                        {error, _} ->
                            {Lexer,
                                {end_of_file,
                                    {position, erlang:element(3, Lexer)}}};

                        {ok, {Grapheme, Source@56}} ->
                            token(
                                Lexer,
                                {unexpected_grapheme, Grapheme},
                                Source@56,
                                erlang:byte_size(Grapheme)
                            )
                    end
            end
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 53).
-spec do_lex(lexer(), list({glexer@token:token(), position()})) -> list({glexer@token:token(),
    position()}).
do_lex(Lexer, Tokens) ->
    case next(Lexer) of
        {_, {end_of_file, _}} ->
            Tokens;

        {Lexer@1, Token} ->
            do_lex(Lexer@1, [Token | Tokens])
    end.

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 48).
-spec lex(lexer()) -> list({glexer@token:token(), position()}).
lex(Lexer) ->
    _pipe = do_lex(Lexer, []),
    lists:reverse(_pipe).

-file("/Users/danielle/Repositories/Projects/glexer/src/glexer.gleam", 411).
-spec comment(
    lexer(),
    fun((binary()) -> glexer@token:token()),
    binary(),
    integer()
) -> {lexer(), {glexer@token:token(), position()}}.
comment(Lexer, Token, Content, Start) ->
    case erlang:element(2, Lexer) of
        <<"\n"/utf8, _/binary>> ->
            case erlang:element(5, Lexer) of
                true ->
                    {Lexer, {Token(Content), {position, Start}}};

                false ->
                    next(Lexer)
            end;

        <<"\r\n"/utf8, _/binary>> ->
            case erlang:element(5, Lexer) of
                true ->
                    {Lexer, {Token(Content), {position, Start}}};

                false ->
                    next(Lexer)
            end;

        _ ->
            case gleam_stdlib:string_pop_grapheme(erlang:element(2, Lexer)) of
                {error, _} ->
                    case erlang:element(5, Lexer) of
                        true ->
                            {Lexer, {Token(Content), {position, Start}}};

                        false ->
                            next(Lexer)
                    end;

                {ok, {Grapheme, Source}} ->
                    _pipe = advance(Lexer, Source, erlang:byte_size(Grapheme)),
                    comment(
                        _pipe,
                        Token,
                        <<Content/binary, Grapheme/binary>>,
                        Start
                    )
            end
    end.
