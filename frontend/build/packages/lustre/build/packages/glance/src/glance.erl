-module(glance).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([precedence/1, module/1]).
-export_type([definition/1, attribute/0, module_/0, function_/0, span/0, statement/0, assignment_kind/0, pattern/0, expression/0, clause/0, bit_string_segment_option/1, binary_operator/0, fn_parameter/0, function_parameter/0, assignment_name/0, import/0, constant/0, unqualified_import/0, publicity/0, type_alias/0, custom_type/0, variant/0, record_update_field/1, variant_field/0, field/1, type/0, error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type definition(GON) :: {definition, list(attribute()), GON}.

-type attribute() :: {attribute, binary(), list(expression())}.

-type module_() :: {module,
        list(definition(import())),
        list(definition(custom_type())),
        list(definition(type_alias())),
        list(definition(constant())),
        list(definition(function_()))}.

-type function_() :: {function,
        binary(),
        publicity(),
        list(function_parameter()),
        gleam@option:option(type()),
        list(statement()),
        span()}.

-type span() :: {span, integer(), integer()}.

-type statement() :: {use, list(pattern()), expression()} |
    {assignment,
        assignment_kind(),
        pattern(),
        gleam@option:option(type()),
        expression()} |
    {expression, expression()}.

-type assignment_kind() :: 'let' | assert.

-type pattern() :: {pattern_int, binary()} |
    {pattern_float, binary()} |
    {pattern_string, binary()} |
    {pattern_discard, binary()} |
    {pattern_variable, binary()} |
    {pattern_tuple, list(pattern())} |
    {pattern_list, list(pattern()), gleam@option:option(pattern())} |
    {pattern_assignment, pattern(), binary()} |
    {pattern_concatenate, binary(), assignment_name()} |
    {pattern_bit_string,
        list({pattern(), list(bit_string_segment_option(pattern()))})} |
    {pattern_constructor,
        gleam@option:option(binary()),
        binary(),
        list(field(pattern())),
        boolean()}.

-type expression() :: {int, binary()} |
    {float, binary()} |
    {string, binary()} |
    {variable, binary()} |
    {negate_int, expression()} |
    {negate_bool, expression()} |
    {block, list(statement())} |
    {panic, gleam@option:option(expression())} |
    {todo, gleam@option:option(expression())} |
    {tuple, list(expression())} |
    {list, list(expression()), gleam@option:option(expression())} |
    {fn, list(fn_parameter()), gleam@option:option(type()), list(statement())} |
    {record_update,
        gleam@option:option(binary()),
        binary(),
        expression(),
        list(record_update_field(expression()))} |
    {field_access, expression(), binary()} |
    {call, expression(), list(field(expression()))} |
    {tuple_index, expression(), integer()} |
    {fn_capture,
        gleam@option:option(binary()),
        expression(),
        list(field(expression())),
        list(field(expression()))} |
    {bit_string,
        list({expression(), list(bit_string_segment_option(expression()))})} |
    {'case', list(expression()), list(clause())} |
    {binary_operator, binary_operator(), expression(), expression()}.

-type clause() :: {clause,
        list(list(pattern())),
        gleam@option:option(expression()),
        expression()}.

-type bit_string_segment_option(GOO) :: bytes_option |
    int_option |
    float_option |
    bits_option |
    utf8_option |
    utf16_option |
    utf32_option |
    utf8_codepoint_option |
    utf16_codepoint_option |
    utf32_codepoint_option |
    signed_option |
    unsigned_option |
    big_option |
    little_option |
    native_option |
    {size_value_option, GOO} |
    {size_option, integer()} |
    {unit_option, integer()}.

-type binary_operator() :: 'and' |
    'or' |
    eq |
    not_eq |
    lt_int |
    lt_eq_int |
    lt_float |
    lt_eq_float |
    gt_eq_int |
    gt_int |
    gt_eq_float |
    gt_float |
    pipe |
    add_int |
    add_float |
    sub_int |
    sub_float |
    mult_int |
    mult_float |
    div_int |
    div_float |
    remainder_int |
    concatenate.

-type fn_parameter() :: {fn_parameter,
        assignment_name(),
        gleam@option:option(type())}.

-type function_parameter() :: {function_parameter,
        gleam@option:option(binary()),
        assignment_name(),
        gleam@option:option(type())}.

-type assignment_name() :: {named, binary()} | {discarded, binary()}.

-type import() :: {import,
        binary(),
        gleam@option:option(assignment_name()),
        list(unqualified_import()),
        list(unqualified_import())}.

-type constant() :: {constant,
        binary(),
        publicity(),
        gleam@option:option(type()),
        expression()}.

-type unqualified_import() :: {unqualified_import,
        binary(),
        gleam@option:option(binary())}.

-type publicity() :: public | private.

-type type_alias() :: {type_alias,
        binary(),
        publicity(),
        list(binary()),
        type()}.

-type custom_type() :: {custom_type,
        binary(),
        publicity(),
        boolean(),
        list(binary()),
        list(variant())}.

-type variant() :: {variant, binary(), list(variant_field())}.

-type record_update_field(GOP) :: {record_update_field,
        binary(),
        gleam@option:option(GOP)}.

-type variant_field() :: {labelled_variant_field, type(), binary()} |
    {unlabelled_variant_field, type()}.

-type field(GOQ) :: {labelled_field, binary(), GOQ} |
    {shorthand_field, binary()} |
    {unlabelled_field, GOQ}.

-type type() :: {named_type,
        binary(),
        gleam@option:option(binary()),
        list(type())} |
    {tuple_type, list(type())} |
    {function_type, list(type()), type()} |
    {variable_type, binary()} |
    {hole_type, binary()}.

-type error() :: unexpected_end_of_input |
    {unexpected_token, glexer@token:token(), glexer:position()}.

-file("src/glance.gleam", 188).
-spec precedence(binary_operator()) -> integer().
precedence(Operator) ->
    case Operator of
        'or' ->
            1;

        'and' ->
            2;

        eq ->
            3;

        not_eq ->
            3;

        lt_int ->
            4;

        lt_eq_int ->
            4;

        lt_float ->
            4;

        lt_eq_float ->
            4;

        gt_eq_int ->
            4;

        gt_int ->
            4;

        gt_eq_float ->
            4;

        gt_float ->
            4;

        concatenate ->
            5;

        pipe ->
            6;

        add_int ->
            7;

        add_float ->
            7;

        sub_int ->
            7;

        sub_float ->
            7;

        mult_int ->
            8;

        mult_float ->
            8;

        div_int ->
            8;

        div_float ->
            8;

        remainder_int ->
            8
    end.

-file("src/glance.gleam", 358).
-spec push_variant(custom_type(), variant()) -> custom_type().
push_variant(Custom_type, Variant) ->
    _record = Custom_type,
    {custom_type,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        [Variant | erlang:element(6, Custom_type)]}.

-file("src/glance.gleam", 362).
-spec expect(
    glexer@token:token(),
    list({glexer@token:token(), glexer:position()}),
    fun((glexer:position(), list({glexer@token:token(), glexer:position()})) -> {ok,
            GOY} |
        {error, error()})
) -> {ok, GOY} | {error, error()}.
expect(Expected, Tokens, Next) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{Token, Position} | Tokens@1] when Token =:= Expected ->
            Next(Position, Tokens@1);

        [{Other, Position@1} | _] ->
            {error, {unexpected_token, Other, Position@1}}
    end.

-file("src/glance.gleam", 375).
-spec expect_upper_name(
    list({glexer@token:token(), glexer:position()}),
    fun((binary(), list({glexer@token:token(), glexer:position()})) -> {ok, GPD} |
        {error, error()})
) -> {ok, GPD} | {error, error()}.
expect_upper_name(Tokens, Next) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{{upper_name, Name}, _} | Tokens@1] ->
            Next(Name, Tokens@1);

        [{Other, Position} | _] ->
            {error, {unexpected_token, Other, Position}}
    end.

-file("src/glance.gleam", 386).
-spec expect_name(
    list({glexer@token:token(), glexer:position()}),
    fun((binary(), list({glexer@token:token(), glexer:position()})) -> {ok, GPI} |
        {error, error()})
) -> {ok, GPI} | {error, error()}.
expect_name(Tokens, Next) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{{name, Name}, _} | Tokens@1] ->
            Next(Name, Tokens@1);

        [{Other, Position} | _] ->
            {error, {unexpected_token, Other, Position}}
    end.

-file("src/glance.gleam", 397).
-spec until(
    glexer@token:token(),
    GPN,
    list({glexer@token:token(), glexer:position()}),
    fun((GPN, list({glexer@token:token(), glexer:position()})) -> {ok,
            {GPN, list({glexer@token:token(), glexer:position()})}} |
        {error, error()})
) -> {ok, {GPN, list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
until(Limit, Acc, Tokens, Callback) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{Token, _} | Tokens@1] when Token =:= Limit ->
            {ok, {Acc, Tokens@1}};

        [_ | _] ->
            case Callback(Acc, Tokens) of
                {ok, {Acc@1, Tokens@2}} ->
                    until(Limit, Acc@1, Tokens@2, Callback);

                {error, Error} ->
                    {error, Error}
            end
    end.

-file("src/glance.gleam", 507).
-spec module_name(binary(), list({glexer@token:token(), glexer:position()})) -> {ok,
        {binary(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
module_name(Name, Tokens) ->
    case Tokens of
        [{slash, _}, {{name, S}, _} | Tokens@1] when Name =/= <<""/utf8>> ->
            module_name(
                <<<<Name/binary, "/"/utf8>>/binary, S/binary>>,
                Tokens@1
            );

        [{{name, S@1}, _} | Tokens@2] when Name =:= <<""/utf8>> ->
            module_name(S@1, Tokens@2);

        [] when Name =:= <<""/utf8>> ->
            {error, unexpected_end_of_input};

        [{Other, Position} | _] when Name =:= <<""/utf8>> ->
            {error, {unexpected_token, Other, Position}};

        _ ->
            {ok, {Name, Tokens}}
    end.

-file("src/glance.gleam", 524).
-spec optional_module_alias(list({glexer@token:token(), glexer:position()})) -> {gleam@option:option(assignment_name()),
    list({glexer@token:token(), glexer:position()})}.
optional_module_alias(Tokens) ->
    case Tokens of
        [{as, _}, {{name, Alias}, _} | Tokens@1] ->
            {{some, {named, Alias}}, Tokens@1};

        [{as, _}, {{discard_name, Alias@1}, _} | Tokens@2] ->
            {{some, {discarded, Alias@1}}, Tokens@2};

        _ ->
            {none, Tokens}
    end.

-file("src/glance.gleam", 846).
-spec unexpected_error(list({glexer@token:token(), glexer:position()})) -> {ok,
        any()} |
    {error, error()}.
unexpected_error(Tokens) ->
    case Tokens of
        [{Token, Position} | _] ->
            {error, {unexpected_token, Token, Position}};

        [] ->
            {error, unexpected_end_of_input}
    end.

-file("src/glance.gleam", 853).
-spec binary_operator(glexer@token:token()) -> {ok, binary_operator()} |
    {error, nil}.
binary_operator(Token) ->
    case Token of
        amper_amper ->
            {ok, 'and'};

        equal_equal ->
            {ok, eq};

        greater ->
            {ok, gt_int};

        greater_dot ->
            {ok, gt_float};

        greater_equal ->
            {ok, gt_eq_int};

        greater_equal_dot ->
            {ok, gt_eq_float};

        less ->
            {ok, lt_int};

        less_dot ->
            {ok, lt_float};

        less_equal ->
            {ok, lt_eq_int};

        less_equal_dot ->
            {ok, lt_eq_float};

        less_greater ->
            {ok, concatenate};

        minus ->
            {ok, sub_int};

        minus_dot ->
            {ok, sub_float};

        not_equal ->
            {ok, not_eq};

        percent ->
            {ok, remainder_int};

        v_bar_v_bar ->
            {ok, 'or'};

        pipe ->
            {ok, pipe};

        plus ->
            {ok, add_int};

        plus_dot ->
            {ok, add_float};

        slash ->
            {ok, div_int};

        slash_dot ->
            {ok, div_float};

        star ->
            {ok, mult_int};

        star_dot ->
            {ok, mult_float};

        _ ->
            {error, nil}
    end.

-file("src/glance.gleam", 882).
-spec pop_binary_operator(list({glexer@token:token(), glexer:position()})) -> {ok,
        {binary_operator(), list({glexer@token:token(), glexer:position()})}} |
    {error, nil}.
pop_binary_operator(Tokens) ->
    case Tokens of
        [{Token, _} | Tokens@1] ->
            gleam@result:map(
                binary_operator(Token),
                fun(Op) -> {Op, Tokens@1} end
            );

        [] ->
            {error, nil}
    end.

-file("src/glance.gleam", 922).
?DOC(" Simple-Precedence-Parser, handle seeing an operator or end\n").
-spec handle_operator(
    gleam@option:option(binary_operator()),
    list(binary_operator()),
    list(expression())
) -> {gleam@option:option(expression()),
    list(binary_operator()),
    list(expression())}.
handle_operator(Next, Operators, Values) ->
    case {Next, Operators, Values} of
        {{some, Operator}, [], _} ->
            {none, [Operator], Values};

        {{some, Next@1}, [Previous | Operators@1], [A, B | Rest_values]} ->
            case precedence(Previous) >= precedence(Next@1) of
                true ->
                    Values@1 = [{binary_operator, Previous, B, A} | Rest_values],
                    handle_operator({some, Next@1}, Operators@1, Values@1);

                false ->
                    {none, [Next@1, Previous | Operators@1], Values}
            end;

        {none, [Operator@1 | Operators@2], [A@1, B@1 | Values@2]} ->
            Values@3 = [{binary_operator, Operator@1, B@1, A@1} | Values@2],
            handle_operator(none, Operators@2, Values@3);

        {none, [], [Expression]} ->
            {{some, Expression}, Operators, Values};

        {none, [], []} ->
            {none, Operators, Values};

        {_, _, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"parser bug, expression not full reduced"/utf8>>,
                    module => <<"glance"/utf8>>,
                    function => <<"handle_operator"/utf8>>,
                    line => 949})
    end.

-file("src/glance.gleam", 1402).
-spec list(
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {GVG, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    gleam@option:option(GVG),
    list(GVG),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(GVG),
            gleam@option:option(GVG),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
list(Parser, Discard, Acc, Tokens) ->
    case Tokens of
        [{right_square, _} | Tokens@1] ->
            {ok, {lists:reverse(Acc), none, Tokens@1}};

        [{comma, _}, {right_square, _} | Tokens@2] when Acc =/= [] ->
            {ok, {lists:reverse(Acc), none, Tokens@2}};

        [{dot_dot, _}, {right_square, _} = Close | Tokens@3] ->
            case Discard of
                none ->
                    unexpected_error([Close | Tokens@3]);

                {some, Discard@1} ->
                    {ok, {lists:reverse(Acc), {some, Discard@1}, Tokens@3}}
            end;

        [{dot_dot, _} | Tokens@4] ->
            gleam@result:'try'(
                Parser(Tokens@4),
                fun(_use0) ->
                    {Rest, Tokens@5} = _use0,
                    expect(
                        right_square,
                        Tokens@5,
                        fun(_, Tokens@6) ->
                            {ok, {lists:reverse(Acc), {some, Rest}, Tokens@6}}
                        end
                    )
                end
            );

        _ ->
            gleam@result:'try'(
                Parser(Tokens),
                fun(_use0@1) ->
                    {Element, Tokens@7} = _use0@1,
                    Acc@1 = [Element | Acc],
                    case Tokens@7 of
                        [{right_square, _} | Tokens@8] ->
                            {ok, {lists:reverse(Acc@1), none, Tokens@8}};

                        [{comma, _}, {right_square, _} | Tokens@8] ->
                            {ok, {lists:reverse(Acc@1), none, Tokens@8}};

                        [{comma, _},
                            {dot_dot, _},
                            {right_square, _} = Close@1 |
                            Tokens@9] ->
                            case Discard of
                                none ->
                                    unexpected_error([Close@1 | Tokens@9]);

                                {some, Discard@2} ->
                                    {ok,
                                        {lists:reverse(Acc@1),
                                            {some, Discard@2},
                                            Tokens@9}}
                            end;

                        [{comma, _}, {dot_dot, _} | Tokens@10] ->
                            gleam@result:'try'(
                                Parser(Tokens@10),
                                fun(_use0@2) ->
                                    {Rest@1, Tokens@11} = _use0@2,
                                    expect(
                                        right_square,
                                        Tokens@11,
                                        fun(_, Tokens@12) ->
                                            {ok,
                                                {lists:reverse(Acc@1),
                                                    {some, Rest@1},
                                                    Tokens@12}}
                                        end
                                    )
                                end
                            );

                        [{comma, _} | Tokens@13] ->
                            list(Parser, Discard, Acc@1, Tokens@13);

                        [{Other, Position} | _] ->
                            {error, {unexpected_token, Other, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 312).
-spec push_constant(module_(), list(attribute()), constant()) -> module_().
push_constant(Module, Attributes, Constant) ->
    _record = Module,
    {module,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        [{definition, lists:reverse(Attributes), Constant} |
            erlang:element(5, Module)],
        erlang:element(6, _record)}.

-file("src/glance.gleam", 323).
-spec push_function(module_(), list(attribute()), function_()) -> module_().
push_function(Module, Attributes, Function) ->
    _record = Module,
    {module,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        [{definition, lists:reverse(Attributes), Function} |
            erlang:element(6, Module)]}.

-file("src/glance.gleam", 334).
-spec push_custom_type(module_(), list(attribute()), custom_type()) -> module_().
push_custom_type(Module, Attributes, Custom_type) ->
    Custom_type@1 = begin
        _record = Custom_type,
        {custom_type,
            erlang:element(2, _record),
            erlang:element(3, _record),
            erlang:element(4, _record),
            erlang:element(5, _record),
            lists:reverse(erlang:element(6, Custom_type))}
    end,
    _record@1 = Module,
    {module,
        erlang:element(2, _record@1),
        [{definition, lists:reverse(Attributes), Custom_type@1} |
            erlang:element(3, Module)],
        erlang:element(4, _record@1),
        erlang:element(5, _record@1),
        erlang:element(6, _record@1)}.

-file("src/glance.gleam", 347).
-spec push_type_alias(module_(), list(attribute()), type_alias()) -> module_().
push_type_alias(Module, Attributes, Type_alias) ->
    _record = Module,
    {module,
        erlang:element(2, _record),
        erlang:element(3, _record),
        [{definition, lists:reverse(Attributes), Type_alias} |
            erlang:element(4, Module)],
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("src/glance.gleam", 545).
-spec unqualified_imports(
    list(unqualified_import()),
    list(unqualified_import()),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(unqualified_import()),
            list(unqualified_import()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
unqualified_imports(Types, Values, Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{right_brace, _} | Tokens@1] ->
            {ok, {lists:reverse(Types), lists:reverse(Values), Tokens@1}};

        [{{upper_name, Name}, _},
            {as, _},
            {{upper_name, Alias}, _},
            {comma, _} |
            Tokens@2] ->
            Import_ = {unqualified_import, Name, {some, Alias}},
            unqualified_imports(Types, [Import_ | Values], Tokens@2);

        [{{name, Name}, _}, {as, _}, {{name, Alias}, _}, {comma, _} | Tokens@2] ->
            Import_ = {unqualified_import, Name, {some, Alias}},
            unqualified_imports(Types, [Import_ | Values], Tokens@2);

        [{{upper_name, Name@1}, _},
            {as, _},
            {{upper_name, Alias@1}, _},
            {right_brace, _} |
            Tokens@3] ->
            Import_@1 = {unqualified_import, Name@1, {some, Alias@1}},
            {ok,
                {lists:reverse(Types),
                    lists:reverse([Import_@1 | Values]),
                    Tokens@3}};

        [{{name, Name@1}, _},
            {as, _},
            {{name, Alias@1}, _},
            {right_brace, _} |
            Tokens@3] ->
            Import_@1 = {unqualified_import, Name@1, {some, Alias@1}},
            {ok,
                {lists:reverse(Types),
                    lists:reverse([Import_@1 | Values]),
                    Tokens@3}};

        [{{upper_name, Name@2}, _}, {comma, _} | Tokens@4] ->
            Import_@2 = {unqualified_import, Name@2, none},
            unqualified_imports(Types, [Import_@2 | Values], Tokens@4);

        [{{name, Name@2}, _}, {comma, _} | Tokens@4] ->
            Import_@2 = {unqualified_import, Name@2, none},
            unqualified_imports(Types, [Import_@2 | Values], Tokens@4);

        [{{upper_name, Name@3}, _}, {right_brace, _} | Tokens@5] ->
            Import_@3 = {unqualified_import, Name@3, none},
            {ok,
                {lists:reverse(Types),
                    lists:reverse([Import_@3 | Values]),
                    Tokens@5}};

        [{{name, Name@3}, _}, {right_brace, _} | Tokens@5] ->
            Import_@3 = {unqualified_import, Name@3, none},
            {ok,
                {lists:reverse(Types),
                    lists:reverse([Import_@3 | Values]),
                    Tokens@5}};

        [{type, _},
            {{upper_name, Name@4}, _},
            {as, _},
            {{upper_name, Alias@2}, _},
            {comma, _} |
            Tokens@6] ->
            Import_@4 = {unqualified_import, Name@4, {some, Alias@2}},
            unqualified_imports([Import_@4 | Types], Values, Tokens@6);

        [{type, _},
            {{upper_name, Name@5}, _},
            {as, _},
            {{upper_name, Alias@3}, _},
            {right_brace, _} |
            Tokens@7] ->
            Import_@5 = {unqualified_import, Name@5, {some, Alias@3}},
            {ok,
                {lists:reverse([Import_@5 | Types]),
                    lists:reverse(Values),
                    Tokens@7}};

        [{type, _}, {{upper_name, Name@6}, _}, {comma, _} | Tokens@8] ->
            Import_@6 = {unqualified_import, Name@6, none},
            unqualified_imports([Import_@6 | Types], Values, Tokens@8);

        [{type, _}, {{upper_name, Name@7}, _}, {right_brace, _} | Tokens@9] ->
            Import_@7 = {unqualified_import, Name@7, none},
            {ok,
                {lists:reverse([Import_@7 | Types]),
                    lists:reverse(Values),
                    Tokens@9}};

        [{Other, Position} | _] ->
            {error, {unexpected_token, Other, Position}}
    end.

-file("src/glance.gleam", 535).
-spec optional_unqualified_imports(
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(unqualified_import()),
            list(unqualified_import()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_unqualified_imports(Tokens) ->
    case Tokens of
        [{dot, _}, {left_brace, _} | Tokens@1] ->
            unqualified_imports([], [], Tokens@1);

        _ ->
            {ok, {[], [], Tokens}}
    end.

-file("src/glance.gleam", 493).
-spec import_statement(
    module_(),
    list(attribute()),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
import_statement(Module, Attributes, Tokens) ->
    gleam@result:'try'(
        module_name(<<""/utf8>>, Tokens),
        fun(_use0) ->
            {Module_name, Tokens@1} = _use0,
            gleam@result:'try'(
                optional_unqualified_imports(Tokens@1),
                fun(_use0@1) ->
                    {Ts, Vs, Tokens@2} = _use0@1,
                    {Alias, Tokens@3} = optional_module_alias(Tokens@2),
                    Import_ = {import, Module_name, Alias, Ts, Vs},
                    Definition = {definition,
                        lists:reverse(Attributes),
                        Import_},
                    Module@1 = begin
                        _record = Module,
                        {module,
                            [Definition | erlang:element(2, Module)],
                            erlang:element(3, _record),
                            erlang:element(4, _record),
                            erlang:element(5, _record),
                            erlang:element(6, _record)}
                    end,
                    {ok, {Module@1, Tokens@3}}
                end
            )
        end
    ).

-file("src/glance.gleam", 1070).
-spec bit_string_segment_options(
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {GTD, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    list(bit_string_segment_option(GTD)),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(bit_string_segment_option(GTD)),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
bit_string_segment_options(Parser, Options, Tokens) ->
    gleam@result:'try'(case Tokens of
            [{{int, I}, Position} | Tokens@1] ->
                case gleam_stdlib:parse_int(I) of
                    {ok, I@1} ->
                        {ok, {{size_option, I@1}, Tokens@1}};

                    {error, _} ->
                        {error, {unexpected_token, {int, I}, Position}}
                end;

            [{{name, <<"size"/utf8>>}, _}, {left_paren, _} | Tokens@2] ->
                gleam@result:'try'(
                    Parser(Tokens@2),
                    fun(_use0) ->
                        {Value, Tokens@3} = _use0,
                        expect(
                            right_paren,
                            Tokens@3,
                            fun(_, Tokens@4) ->
                                {ok, {{size_value_option, Value}, Tokens@4}}
                            end
                        )
                    end
                );

            [{{name, <<"unit"/utf8>>}, Position@1},
                {left_paren, _},
                {{int, I@2}, _},
                {right_paren, _} |
                Tokens@5] ->
                case gleam_stdlib:parse_int(I@2) of
                    {ok, I@3} ->
                        {ok, {{unit_option, I@3}, Tokens@5}};

                    {error, _} ->
                        {error, {unexpected_token, {int, I@2}, Position@1}}
                end;

            [{{name, <<"bytes"/utf8>>}, _} | Tokens@6] ->
                {ok, {bytes_option, Tokens@6}};

            [{{name, <<"binary"/utf8>>}, _} | Tokens@7] ->
                {ok, {bytes_option, Tokens@7}};

            [{{name, <<"int"/utf8>>}, _} | Tokens@8] ->
                {ok, {int_option, Tokens@8}};

            [{{name, <<"float"/utf8>>}, _} | Tokens@9] ->
                {ok, {float_option, Tokens@9}};

            [{{name, <<"bits"/utf8>>}, _} | Tokens@10] ->
                {ok, {bits_option, Tokens@10}};

            [{{name, <<"bit_string"/utf8>>}, _} | Tokens@11] ->
                {ok, {bits_option, Tokens@11}};

            [{{name, <<"utf8"/utf8>>}, _} | Tokens@12] ->
                {ok, {utf8_option, Tokens@12}};

            [{{name, <<"utf16"/utf8>>}, _} | Tokens@13] ->
                {ok, {utf16_option, Tokens@13}};

            [{{name, <<"utf32"/utf8>>}, _} | Tokens@14] ->
                {ok, {utf32_option, Tokens@14}};

            [{{name, <<"utf8_codepoint"/utf8>>}, _} | Tokens@15] ->
                {ok, {utf8_codepoint_option, Tokens@15}};

            [{{name, <<"utf16_codepoint"/utf8>>}, _} | Tokens@16] ->
                {ok, {utf16_codepoint_option, Tokens@16}};

            [{{name, <<"utf32_codepoint"/utf8>>}, _} | Tokens@17] ->
                {ok, {utf32_codepoint_option, Tokens@17}};

            [{{name, <<"signed"/utf8>>}, _} | Tokens@18] ->
                {ok, {signed_option, Tokens@18}};

            [{{name, <<"unsigned"/utf8>>}, _} | Tokens@19] ->
                {ok, {unsigned_option, Tokens@19}};

            [{{name, <<"big"/utf8>>}, _} | Tokens@20] ->
                {ok, {big_option, Tokens@20}};

            [{{name, <<"little"/utf8>>}, _} | Tokens@21] ->
                {ok, {little_option, Tokens@21}};

            [{{name, <<"native"/utf8>>}, _} | Tokens@22] ->
                {ok, {native_option, Tokens@22}};

            [{Other, Position@2} | _] ->
                {error, {unexpected_token, Other, Position@2}};

            [] ->
                {error, unexpected_end_of_input}
        end, fun(_use0@1) ->
            {Option, Tokens@23} = _use0@1,
            Options@1 = [Option | Options],
            case Tokens@23 of
                [{minus, _} | Tokens@24] ->
                    bit_string_segment_options(Parser, Options@1, Tokens@24);

                _ ->
                    {ok, {lists:reverse(Options@1), Tokens@23}}
            end
        end).

-file("src/glance.gleam", 1060).
-spec optional_bit_string_segment_options(
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {GSW, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(bit_string_segment_option(GSW)),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_bit_string_segment_options(Parser, Tokens) ->
    case Tokens of
        [{colon, _} | Tokens@1] ->
            bit_string_segment_options(Parser, [], Tokens@1);

        _ ->
            {ok, {[], Tokens}}
    end.

-file("src/glance.gleam", 1050).
-spec bit_string_segment(
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {GSP, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {{GSP, list(bit_string_segment_option(GSP))},
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
bit_string_segment(Parser, Tokens) ->
    gleam@result:'try'(
        Parser(Tokens),
        fun(_use0) ->
            {Value, Tokens@1} = _use0,
            Result = optional_bit_string_segment_options(Parser, Tokens@1),
            gleam@result:'try'(
                Result,
                fun(_use0@1) ->
                    {Options, Tokens@2} = _use0@1,
                    {ok, {{Value, Options}, Tokens@2}}
                end
            )
        end
    ).

-file("src/glance.gleam", 1371).
-spec delimited(
    list(GUW),
    list({glexer@token:token(), glexer:position()}),
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {GUW, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    glexer@token:token()
) -> {ok, {list(GUW), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
delimited(Acc, Tokens, Parser, Delimeter) ->
    gleam@result:'try'(
        Parser(Tokens),
        fun(_use0) ->
            {T, Tokens@1} = _use0,
            Acc@1 = [T | Acc],
            case Tokens@1 of
                [{Token, _} | Tokens@2] when Token =:= Delimeter ->
                    delimited(Acc@1, Tokens@2, Parser, Delimeter);

                _ ->
                    {ok, {lists:reverse(Acc@1), Tokens@1}}
            end
        end
    ).

-file("src/glance.gleam", 1531).
-spec comma_delimited(
    list(GVZ),
    list({glexer@token:token(), glexer:position()}),
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {GVZ, list({glexer@token:token(), glexer:position()})}} |
        {error, error()}),
    glexer@token:token()
) -> {ok, {list(GVZ), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
comma_delimited(Items, Tokens, Parser, Final) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{Token, _} | Tokens@1] when Token =:= Final ->
            {ok, {lists:reverse(Items), Tokens@1}};

        _ ->
            gleam@result:'try'(
                Parser(Tokens),
                fun(_use0) ->
                    {Element, Tokens@2} = _use0,
                    case Tokens@2 of
                        [{comma, _} | Tokens@3] ->
                            comma_delimited(
                                [Element | Items],
                                Tokens@3,
                                Parser,
                                Final
                            );

                        [{Token@1, _} | Tokens@4] when Token@1 =:= Final ->
                            {ok, {lists:reverse([Element | Items]), Tokens@4}};

                        [{Other, Position} | _] ->
                            {error, {unexpected_token, Other, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 1687).
-spec name(list({glexer@token:token(), glexer:position()})) -> {ok,
        {binary(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
name(Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{{name, Name}, _} | Tokens@1] ->
            {ok, {Name, Tokens@1}};

        [{Token, Position} | _] ->
            {error, {unexpected_token, Token, Position}}
    end.

-file("src/glance.gleam", 1677).
-spec optional_type_parameters(list({glexer@token:token(), glexer:position()})) -> {ok,
        {list(binary()), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_type_parameters(Tokens) ->
    case Tokens of
        [{left_paren, _} | Tokens@1] ->
            comma_delimited([], Tokens@1, fun name/1, right_paren);

        _ ->
            {ok, {[], Tokens}}
    end.

-file("src/glance.gleam", 1731).
-spec field(
    list({glexer@token:token(), glexer:position()}),
    fun((list({glexer@token:token(), glexer:position()})) -> {ok,
            {GXM, list({glexer@token:token(), glexer:position()})}} |
        {error, error()})
) -> {ok, {field(GXM), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
field(Tokens, Parser) ->
    case Tokens of
        [{{name, Name}, _}, {colon, _} | Tokens@1] ->
            case Tokens@1 of
                [{comma, _} | _] ->
                    {ok, {{shorthand_field, Name}, Tokens@1}};

                [{right_paren, _} | _] ->
                    {ok, {{shorthand_field, Name}, Tokens@1}};

                _ ->
                    gleam@result:'try'(
                        Parser(Tokens@1),
                        fun(_use0) ->
                            {T, Tokens@2} = _use0,
                            {ok, {{labelled_field, Name, T}, Tokens@2}}
                        end
                    )
            end;

        _ ->
            gleam@result:'try'(
                Parser(Tokens),
                fun(_use0@1) ->
                    {T@1, Tokens@3} = _use0@1,
                    {ok, {{unlabelled_field, T@1}, Tokens@3}}
                end
            )
    end.

-file("src/glance.gleam", 706).
-spec statement(list({glexer@token:token(), glexer:position()})) -> {ok,
        {statement(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
statement(Tokens) ->
    case Tokens of
        [{'let', _}, {assert, _} | Tokens@1] ->
            assignment(assert, Tokens@1);

        [{'let', _} | Tokens@2] ->
            assignment('let', Tokens@2);

        [{use, _} | Tokens@3] ->
            use_(Tokens@3);

        Tokens@4 ->
            gleam@result:'try'(
                expression(Tokens@4),
                fun(_use0) ->
                    {Expression, Tokens@5} = _use0,
                    {ok, {{expression, Expression}, Tokens@5}}
                end
            )
    end.

-file("src/glance.gleam", 842).
-spec expression(list({glexer@token:token(), glexer:position()})) -> {ok,
        {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
expression(Tokens) ->
    expression_loop(Tokens, [], []).

-file("src/glance.gleam", 892).
-spec expression_loop(
    list({glexer@token:token(), glexer:position()}),
    list(binary_operator()),
    list(expression())
) -> {ok, {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
expression_loop(Tokens, Operators, Values) ->
    gleam@result:'try'(
        expression_unit(Tokens),
        fun(_use0) ->
            {Expression, Tokens@1} = _use0,
            case Expression of
                none ->
                    unexpected_error(Tokens@1);

                {some, E} ->
                    Values@1 = [E | Values],
                    case pop_binary_operator(Tokens@1) of
                        {ok, {Operator, Tokens@2}} ->
                            case handle_operator(
                                {some, Operator},
                                Operators,
                                Values@1
                            ) of
                                {{some, Expression@1}, _, _} ->
                                    {ok, {Expression@1, Tokens@2}};

                                {none, Operators@1, Values@2} ->
                                    expression_loop(
                                        Tokens@2,
                                        Operators@1,
                                        Values@2
                                    )
                            end;

                        _ ->
                            case erlang:element(
                                1,
                                handle_operator(none, Operators, Values@1)
                            ) of
                                none ->
                                    unexpected_error(Tokens@1);

                                {some, Expression@2} ->
                                    {ok, {Expression@2, Tokens@1}}
                            end
                    end
            end
        end
    ).

-file("src/glance.gleam", 953).
-spec expression_unit(list({glexer@token:token(), glexer:position()})) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
expression_unit(Tokens) ->
    gleam@result:'try'(case Tokens of
            [{{name, Module}, _},
                {dot, _},
                {{upper_name, Constructor}, _},
                {left_paren, _},
                {dot_dot, _} |
                Tokens@1] ->
                record_update({some, Module}, Constructor, Tokens@1);

            [{{upper_name, Constructor@1}, _},
                {left_paren, _},
                {dot_dot, _} |
                Tokens@2] ->
                record_update(none, Constructor@1, Tokens@2);

            [{{upper_name, Name}, _} | Tokens@3] ->
                {ok, {{some, {variable, Name}}, Tokens@3}};

            [{{int, Value}, _} | Tokens@4] ->
                {ok, {{some, {int, Value}}, Tokens@4}};

            [{{float, Value@1}, _} | Tokens@5] ->
                {ok, {{some, {float, Value@1}}, Tokens@5}};

            [{{string, Value@2}, _} | Tokens@6] ->
                {ok, {{some, {string, Value@2}}, Tokens@6}};

            [{{name, Name@1}, _} | Tokens@7] ->
                {ok, {{some, {variable, Name@1}}, Tokens@7}};

            [{fn, _} | Tokens@8] ->
                fn_(Tokens@8);

            [{'case', _} | Tokens@9] ->
                case_(Tokens@9);

            [{panic, _} | Tokens@10] ->
                todo_panic(Tokens@10, fun(Field@0) -> {panic, Field@0} end);

            [{todo, _} | Tokens@11] ->
                todo_panic(Tokens@11, fun(Field@0) -> {todo, Field@0} end);

            [{left_square, _} | Tokens@12] ->
                Result = list(fun expression/1, none, [], Tokens@12),
                gleam@result:map(
                    Result,
                    fun(_use0) ->
                        {Elements, Rest, Tokens@13} = _use0,
                        {{some, {list, Elements, Rest}}, Tokens@13}
                    end
                );

            [{hash, _}, {left_paren, _} | Tokens@14] ->
                Result@1 = comma_delimited(
                    [],
                    Tokens@14,
                    fun expression/1,
                    right_paren
                ),
                gleam@result:map(
                    Result@1,
                    fun(_use0@1) ->
                        {Expressions, Tokens@15} = _use0@1,
                        {{some, {tuple, Expressions}}, Tokens@15}
                    end
                );

            [{bang, _} | Tokens@16] ->
                gleam@result:map(
                    expression(Tokens@16),
                    fun(_use0@2) ->
                        {Expression, Tokens@17} = _use0@2,
                        {{some, {negate_bool, Expression}}, Tokens@17}
                    end
                );

            [{minus, _} | Tokens@18] ->
                gleam@result:map(
                    expression(Tokens@18),
                    fun(_use0@3) ->
                        {Expression@1, Tokens@19} = _use0@3,
                        Expression@2 = case Expression@1 of
                            {float, Amount} ->
                                {float, <<"-"/utf8, Amount/binary>>};

                            _ ->
                                {negate_int, Expression@1}
                        end,
                        {{some, Expression@2}, Tokens@19}
                    end
                );

            [{left_brace, _} | Tokens@20] ->
                gleam@result:map(
                    statements([], Tokens@20),
                    fun(_use0@4) ->
                        {Statements, _, Tokens@21} = _use0@4,
                        {{some, {block, Statements}}, Tokens@21}
                    end
                );

            [{less_less, _} | Tokens@22] ->
                Parser = fun(_capture) ->
                    bit_string_segment(fun expression/1, _capture)
                end,
                Result@2 = comma_delimited(
                    [],
                    Tokens@22,
                    Parser,
                    greater_greater
                ),
                gleam@result:map(
                    Result@2,
                    fun(_use0@5) ->
                        {Segments, Tokens@23} = _use0@5,
                        {{some, {bit_string, Segments}}, Tokens@23}
                    end
                );

            _ ->
                {ok, {none, Tokens}}
        end, fun(_use0@6) ->
            {Parsed, Tokens@24} = _use0@6,
            case Parsed of
                {some, Expression@3} ->
                    case after_expression(Expression@3, Tokens@24) of
                        {ok, {Expression@4, Tokens@25}} ->
                            {ok, {{some, Expression@4}, Tokens@25}};

                        {error, Error} ->
                            {error, Error}
                    end;

                none ->
                    {ok, {none, Tokens@24}}
            end
        end).

-file("src/glance.gleam", 692).
-spec statements(
    list(statement()),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(statement()),
            integer(),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
statements(Acc, Tokens) ->
    case Tokens of
        [{right_brace, {position, End}} | Tokens@1] ->
            {ok, {lists:reverse(Acc), End + 1, Tokens@1}};

        _ ->
            gleam@result:'try'(
                statement(Tokens),
                fun(_use0) ->
                    {Statement, Tokens@2} = _use0,
                    statements([Statement | Acc], Tokens@2)
                end
            )
    end.

-file("src/glance.gleam", 415).
-spec attribute(list({glexer@token:token(), glexer:position()})) -> {ok,
        {attribute(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
attribute(Tokens) ->
    gleam@result:'try'(case Tokens of
            [{{name, Name}, _} | Tokens@1] ->
                {ok, {Name, Tokens@1}};

            [{Other, Position} | _] ->
                {error, {unexpected_token, Other, Position}};

            [] ->
                {error, unexpected_end_of_input}
        end, fun(_use0) ->
            {Name@1, Tokens@2} = _use0,
            case Tokens@2 of
                [{left_paren, _} | Tokens@3] ->
                    Result = comma_delimited(
                        [],
                        Tokens@3,
                        fun expression/1,
                        right_paren
                    ),
                    gleam@result:'try'(
                        Result,
                        fun(_use0@1) ->
                            {Parameters, Tokens@4} = _use0@1,
                            {ok, {{attribute, Name@1, Parameters}, Tokens@4}}
                        end
                    );

                _ ->
                    {ok, {{attribute, Name@1, []}, Tokens@2}}
            end
        end).

-file("src/glance.gleam", 1037).
-spec todo_panic(
    list({glexer@token:token(), glexer:position()}),
    fun((gleam@option:option(expression())) -> expression())
) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
todo_panic(Tokens, Constructor) ->
    case Tokens of
        [{as, _} | Tokens@1] ->
            gleam@result:'try'(
                expression(Tokens@1),
                fun(_use0) ->
                    {Reason, Tokens@2} = _use0,
                    {ok, {{some, Constructor({some, Reason})}, Tokens@2}}
                end
            );

        _ ->
            {ok, {{some, Constructor(none)}, Tokens}}
    end.

-file("src/glance.gleam", 1298).
-spec record_update_field(list({glexer@token:token(), glexer:position()})) -> {ok,
        {record_update_field(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
record_update_field(Tokens) ->
    case Tokens of
        [{{name, Name}, _}, {colon, _} | Tokens@1] ->
            case Tokens@1 of
                [{comma, _} | _] ->
                    {ok, {{record_update_field, Name, none}, Tokens@1}};

                [{right_paren, _} | _] ->
                    {ok, {{record_update_field, Name, none}, Tokens@1}};

                _ ->
                    gleam@result:'try'(
                        expression(Tokens@1),
                        fun(_use0) ->
                            {Expression, Tokens@2} = _use0,
                            {ok,
                                {{record_update_field, Name, {some, Expression}},
                                    Tokens@2}}
                        end
                    )
            end;

        [{Other, Position} | _] ->
            {error, {unexpected_token, Other, Position}};

        [] ->
            {error, unexpected_end_of_input}
    end.

-file("src/glance.gleam", 1277).
-spec record_update(
    gleam@option:option(binary()),
    binary(),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
record_update(Module, Constructor, Tokens) ->
    gleam@result:'try'(
        expression(Tokens),
        fun(_use0) ->
            {Record, Tokens@1} = _use0,
            case Tokens@1 of
                [{right_paren, _} | Tokens@2] ->
                    {ok,
                        {{some,
                                {record_update, Module, Constructor, Record, []}},
                            Tokens@2}};

                [{comma, _} | Tokens@3] ->
                    Result = comma_delimited(
                        [],
                        Tokens@3,
                        fun record_update_field/1,
                        right_paren
                    ),
                    gleam@result:'try'(
                        Result,
                        fun(_use0@1) ->
                            {Fields, Tokens@4} = _use0@1,
                            {ok,
                                {{some,
                                        {record_update,
                                            Module,
                                            Constructor,
                                            Record,
                                            Fields}},
                                    Tokens@4}}
                        end
                    );

                _ ->
                    {ok, {none, Tokens@1}}
            end
        end
    ).

-file("src/glance.gleam", 1325).
-spec case_subjects(
    list(expression()),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {list(expression()), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
case_subjects(Subjects, Tokens) ->
    gleam@result:'try'(
        expression(Tokens),
        fun(_use0) ->
            {Subject, Tokens@1} = _use0,
            Subjects@1 = [Subject | Subjects],
            case Tokens@1 of
                [{comma, _} | Tokens@2] ->
                    case_subjects(Subjects@1, Tokens@2);

                _ ->
                    {ok, {lists:reverse(Subjects@1), Tokens@1}}
            end
        end
    ).

-file("src/glance.gleam", 1359).
-spec optional_clause_guard(list({glexer@token:token(), glexer:position()})) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_clause_guard(Tokens) ->
    case Tokens of
        [{'if', _} | Tokens@1] ->
            gleam@result:'try'(
                expression(Tokens@1),
                fun(_use0) ->
                    {Expression, Tokens@2} = _use0,
                    {ok, {{some, Expression}, Tokens@2}}
                end
            );

        _ ->
            {ok, {none, Tokens}}
    end.

-file("src/glance.gleam", 1167).
-spec call(
    list(field(expression())),
    expression(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
call(Arguments, Function, Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{right_paren, _} | Tokens@1] ->
            Call = {call, Function, lists:reverse(Arguments)},
            after_expression(Call, Tokens@1);

        [{{name, Label}, _},
            {colon, _},
            {{discard_name, <<""/utf8>>}, _},
            {comma, _},
            {right_paren, _} |
            Tokens@2] ->
            Capture = {fn_capture,
                {some, Label},
                Function,
                lists:reverse(Arguments),
                []},
            after_expression(Capture, Tokens@2);

        [{{name, Label}, _},
            {colon, _},
            {{discard_name, <<""/utf8>>}, _},
            {right_paren, _} |
            Tokens@2] ->
            Capture = {fn_capture,
                {some, Label},
                Function,
                lists:reverse(Arguments),
                []},
            after_expression(Capture, Tokens@2);

        [{{name, Label@1}, _},
            {colon, _},
            {{discard_name, <<""/utf8>>}, _},
            {comma, _} |
            Tokens@3] ->
            fn_capture(
                {some, Label@1},
                Function,
                lists:reverse(Arguments),
                [],
                Tokens@3
            );

        [{{name, Label@1}, _},
            {colon, _},
            {{discard_name, <<""/utf8>>}, _} |
            Tokens@3] ->
            fn_capture(
                {some, Label@1},
                Function,
                lists:reverse(Arguments),
                [],
                Tokens@3
            );

        [{{discard_name, <<""/utf8>>}, _},
            {comma, _},
            {right_paren, _} |
            Tokens@4] ->
            Capture@1 = {fn_capture,
                none,
                Function,
                lists:reverse(Arguments),
                []},
            after_expression(Capture@1, Tokens@4);

        [{{discard_name, <<""/utf8>>}, _}, {right_paren, _} | Tokens@4] ->
            Capture@1 = {fn_capture,
                none,
                Function,
                lists:reverse(Arguments),
                []},
            after_expression(Capture@1, Tokens@4);

        [{{discard_name, <<""/utf8>>}, _}, {comma, _} | Tokens@5] ->
            fn_capture(none, Function, lists:reverse(Arguments), [], Tokens@5);

        [{{discard_name, <<""/utf8>>}, _} | Tokens@5] ->
            fn_capture(none, Function, lists:reverse(Arguments), [], Tokens@5);

        _ ->
            gleam@result:'try'(
                field(Tokens, fun expression/1),
                fun(_use0) ->
                    {Argument, Tokens@6} = _use0,
                    Arguments@1 = [Argument | Arguments],
                    case Tokens@6 of
                        [{comma, _} | Tokens@7] ->
                            call(Arguments@1, Function, Tokens@7);

                        [{right_paren, _} | Tokens@8] ->
                            Call@1 = {call,
                                Function,
                                lists:reverse(Arguments@1)},
                            after_expression(Call@1, Tokens@8);

                        [{Other, Position} | _] ->
                            {error, {unexpected_token, Other, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 1139).
-spec after_expression(
    expression(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
after_expression(Parsed, Tokens) ->
    case Tokens of
        [{dot, _}, {{name, Label}, _} | Tokens@1] ->
            after_expression({field_access, Parsed, Label}, Tokens@1);

        [{dot, _}, {{upper_name, Label}, _} | Tokens@1] ->
            after_expression({field_access, Parsed, Label}, Tokens@1);

        [{dot, _}, {{int, Value} = Token, Position} | Tokens@2] ->
            case gleam_stdlib:parse_int(Value) of
                {ok, I} ->
                    after_expression({tuple_index, Parsed, I}, Tokens@2);

                {error, _} ->
                    {error, {unexpected_token, Token, Position}}
            end;

        [{left_paren, _} | Tokens@3] ->
            call([], Parsed, Tokens@3);

        _ ->
            {ok, {Parsed, Tokens}}
    end.

-file("src/glance.gleam", 1242).
-spec fn_capture(
    gleam@option:option(binary()),
    expression(),
    list(field(expression())),
    list(field(expression())),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {expression(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
fn_capture(Label, Function, Before, After, Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{right_paren, _} | Tokens@1] ->
            Capture = {fn_capture,
                Label,
                Function,
                Before,
                lists:reverse(After)},
            after_expression(Capture, Tokens@1);

        _ ->
            gleam@result:'try'(
                field(Tokens, fun expression/1),
                fun(_use0) ->
                    {Argument, Tokens@2} = _use0,
                    After@1 = [Argument | After],
                    case Tokens@2 of
                        [{comma, _} | Tokens@3] ->
                            fn_capture(
                                Label,
                                Function,
                                Before,
                                After@1,
                                Tokens@3
                            );

                        [{right_paren, _} | Tokens@4] ->
                            Call = {fn_capture,
                                Label,
                                Function,
                                Before,
                                lists:reverse(After@1)},
                            after_expression(Call, Tokens@4);

                        [{Other, Position} | _] ->
                            {error, {unexpected_token, Other, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 761).
-spec pattern_constructor_arguments(
    list(field(pattern())),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {list(field(pattern())),
            boolean(),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
pattern_constructor_arguments(Arguments, Tokens) ->
    case Tokens of
        [{right_paren, _} | Tokens@1] ->
            {ok, {Arguments, false, Tokens@1}};

        [{dot_dot, _}, {comma, _}, {right_paren, _} | Tokens@2] ->
            {ok, {Arguments, true, Tokens@2}};

        [{dot_dot, _}, {right_paren, _} | Tokens@2] ->
            {ok, {Arguments, true, Tokens@2}};

        Tokens@3 ->
            gleam@result:'try'(
                field(Tokens@3, fun pattern/1),
                fun(_use0) ->
                    {Pattern, Tokens@4} = _use0,
                    Arguments@1 = [Pattern | Arguments],
                    case Tokens@4 of
                        [{right_paren, _} | Tokens@5] ->
                            {ok, {Arguments@1, false, Tokens@5}};

                        [{comma, _}, {dot_dot, _}, {right_paren, _} | Tokens@6] ->
                            {ok, {Arguments@1, true, Tokens@6}};

                        [{comma, _} | Tokens@7] ->
                            pattern_constructor_arguments(Arguments@1, Tokens@7);

                        [{Token, Position} | _] ->
                            {error, {unexpected_token, Token, Position}};

                        [] ->
                            {error, unexpected_end_of_input}
                    end
                end
            )
    end.

-file("src/glance.gleam", 792).
-spec pattern(list({glexer@token:token(), glexer:position()})) -> {ok,
        {pattern(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
pattern(Tokens) ->
    gleam@result:'try'(case Tokens of
            [{{upper_name, Name}, _} | Tokens@1] ->
                pattern_constructor(none, Name, Tokens@1);

            [{{name, Module}, _},
                {dot, _},
                {{upper_name, Name@1}, _} |
                Tokens@2] ->
                pattern_constructor({some, Module}, Name@1, Tokens@2);

            [{{string, V}, _}, {less_greater, _}, {{name, N}, _} | Tokens@3] ->
                {ok, {{pattern_concatenate, V, {named, N}}, Tokens@3}};

            [{{string, V@1}, _},
                {less_greater, _},
                {{discard_name, N@1}, _} |
                Tokens@4] ->
                {ok, {{pattern_concatenate, V@1, {discarded, N@1}}, Tokens@4}};

            [{{int, Value}, _} | Tokens@5] ->
                {ok, {{pattern_int, Value}, Tokens@5}};

            [{{float, Value@1}, _} | Tokens@6] ->
                {ok, {{pattern_float, Value@1}, Tokens@6}};

            [{{string, Value@2}, _} | Tokens@7] ->
                {ok, {{pattern_string, Value@2}, Tokens@7}};

            [{{discard_name, Name@2}, _} | Tokens@8] ->
                {ok, {{pattern_discard, Name@2}, Tokens@8}};

            [{{name, Name@3}, _} | Tokens@9] ->
                {ok, {{pattern_variable, Name@3}, Tokens@9}};

            [{left_square, _} | Tokens@10] ->
                Result = list(
                    fun pattern/1,
                    {some, {pattern_discard, <<""/utf8>>}},
                    [],
                    Tokens@10
                ),
                gleam@result:map(
                    Result,
                    fun(_use0) ->
                        {Elements, Rest, Tokens@11} = _use0,
                        {{pattern_list, Elements, Rest}, Tokens@11}
                    end
                );

            [{hash, _}, {left_paren, _} | Tokens@12] ->
                Result@1 = comma_delimited(
                    [],
                    Tokens@12,
                    fun pattern/1,
                    right_paren
                ),
                gleam@result:'try'(
                    Result@1,
                    fun(_use0@1) ->
                        {Patterns, Tokens@13} = _use0@1,
                        {ok, {{pattern_tuple, Patterns}, Tokens@13}}
                    end
                );

            [{less_less, _} | Tokens@14] ->
                Parser = fun(_capture) ->
                    bit_string_segment(fun pattern/1, _capture)
                end,
                Result@2 = comma_delimited(
                    [],
                    Tokens@14,
                    Parser,
                    greater_greater
                ),
                gleam@result:'try'(
                    Result@2,
                    fun(_use0@2) ->
                        {Segments, Tokens@15} = _use0@2,
                        {ok, {{pattern_bit_string, Segments}, Tokens@15}}
                    end
                );

            [{Other, Position} | _] ->
                {error, {unexpected_token, Other, Position}};

            [] ->
                {error, unexpected_end_of_input}
        end, fun(_use0@3) ->
            {Pattern, Tokens@16} = _use0@3,
            case Tokens@16 of
                [{as, _}, {{name, Name@4}, _} | Tokens@17] ->
                    {ok, {{pattern_assignment, Pattern, Name@4}, Tokens@17}};

                _ ->
                    {ok, {Pattern, Tokens@16}}
            end
        end).

-file("src/glance.gleam", 741).
-spec pattern_constructor(
    gleam@option:option(binary()),
    binary(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {pattern(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
pattern_constructor(Module, Constructor, Tokens) ->
    case Tokens of
        [{left_paren, _} | Tokens@1] ->
            Result = pattern_constructor_arguments([], Tokens@1),
            gleam@result:'try'(
                Result,
                fun(_use0) ->
                    {Patterns, Spread, Tokens@2} = _use0,
                    Arguments = lists:reverse(Patterns),
                    Pattern = {pattern_constructor,
                        Module,
                        Constructor,
                        Arguments,
                        Spread},
                    {ok, {Pattern, Tokens@2}}
                end
            );

        _ ->
            Pattern@1 = {pattern_constructor, Module, Constructor, [], false},
            {ok, {Pattern@1, Tokens}}
    end.

-file("src/glance.gleam", 718).
-spec use_(list({glexer@token:token(), glexer:position()})) -> {ok,
        {statement(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
use_(Tokens) ->
    gleam@result:'try'(case Tokens of
            [{left_arrow, _} | _] ->
                {ok, {[], Tokens}};

            _ ->
                delimited([], Tokens, fun pattern/1, comma)
        end, fun(_use0) ->
            {Patterns, Tokens@1} = _use0,
            expect(
                left_arrow,
                Tokens@1,
                fun(_, Tokens@2) ->
                    gleam@result:'try'(
                        expression(Tokens@2),
                        fun(_use0@1) ->
                            {Function, Tokens@3} = _use0@1,
                            {ok, {{use, Patterns, Function}, Tokens@3}}
                        end
                    )
                end
            )
        end).

-file("src/glance.gleam", 1349).
-spec case_clause(list({glexer@token:token(), glexer:position()})) -> {ok,
        {clause(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
case_clause(Tokens) ->
    Multipatterns = fun(_capture) ->
        delimited([], _capture, fun pattern/1, comma)
    end,
    Result = delimited([], Tokens, Multipatterns, v_bar),
    gleam@result:'try'(
        Result,
        fun(_use0) ->
            {Patterns, Tokens@1} = _use0,
            gleam@result:'try'(
                optional_clause_guard(Tokens@1),
                fun(_use0@1) ->
                    {Guard, Tokens@2} = _use0@1,
                    expect(
                        right_arrow,
                        Tokens@2,
                        fun(_, Tokens@3) ->
                            gleam@result:map(
                                expression(Tokens@3),
                                fun(_use0@2) ->
                                    {Expression, Tokens@4} = _use0@2,
                                    {{clause, Patterns, Guard, Expression},
                                        Tokens@4}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1337).
-spec case_clauses(
    list(clause()),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {list(clause()), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
case_clauses(Clauses, Tokens) ->
    gleam@result:'try'(
        case_clause(Tokens),
        fun(_use0) ->
            {Clause, Tokens@1} = _use0,
            Clauses@1 = [Clause | Clauses],
            case Tokens@1 of
                [{right_brace, _} | Tokens@2] ->
                    {ok, {lists:reverse(Clauses@1), Tokens@2}};

                _ ->
                    case_clauses(Clauses@1, Tokens@1)
            end
        end
    ).

-file("src/glance.gleam", 1318).
-spec case_(list({glexer@token:token(), glexer:position()})) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
case_(Tokens) ->
    gleam@result:'try'(
        case_subjects([], Tokens),
        fun(_use0) ->
            {Subjects, Tokens@1} = _use0,
            expect(
                left_brace,
                Tokens@1,
                fun(_, Tokens@2) ->
                    gleam@result:'try'(
                        case_clauses([], Tokens@2),
                        fun(_use0@1) ->
                            {Clauses, Tokens@3} = _use0@1,
                            {ok,
                                {{some, {'case', Subjects, Clauses}}, Tokens@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1630).
-spec named_type(
    binary(),
    gleam@option:option(binary()),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {type(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
named_type(Name, Module, Tokens) ->
    gleam@result:'try'(case Tokens of
            [{left_paren, _} | Tokens@1] ->
                comma_delimited([], Tokens@1, fun type_/1, right_paren);

            _ ->
                {ok, {[], Tokens}}
        end, fun(_use0) ->
            {Parameters, Tokens@2} = _use0,
            T = {named_type, Name, Module, Parameters},
            {ok, {T, Tokens@2}}
        end).

-file("src/glance.gleam", 1603).
-spec type_(list({glexer@token:token(), glexer:position()})) -> {ok,
        {type(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
type_(Tokens) ->
    case Tokens of
        [] ->
            {error, unexpected_end_of_input};

        [{fn, _}, {left_paren, _} | Tokens@1] ->
            fn_type(Tokens@1);

        [{hash, _}, {left_paren, _} | Tokens@2] ->
            tuple_type(Tokens@2);

        [{{name, Module}, _}, {dot, _}, {{upper_name, Name}, _} | Tokens@3] ->
            named_type(Name, {some, Module}, Tokens@3);

        [{{upper_name, Name@1}, _} | Tokens@4] ->
            named_type(Name@1, none, Tokens@4);

        [{{discard_name, Name@2}, _} | Tokens@5] ->
            {ok, {{hole_type, Name@2}, Tokens@5}};

        [{{name, Name@3}, _} | Tokens@6] ->
            {ok, {{variable_type, Name@3}, Tokens@6}};

        [{Token, Position} | _] ->
            {error, {unexpected_token, Token, Position}}
    end.

-file("src/glance.gleam", 679).
-spec optional_return_annotation(
    integer(),
    list({glexer@token:token(), glexer:position()})
) -> {ok,
        {gleam@option:option(type()),
            integer(),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_return_annotation(End, Tokens) ->
    case Tokens of
        [{right_arrow, {position, End@1}} | Tokens@1] ->
            gleam@result:'try'(
                type_(Tokens@1),
                fun(_use0) ->
                    {Return_type, Tokens@2} = _use0,
                    {ok, {{some, Return_type}, End@1, Tokens@2}}
                end
            );

        _ ->
            {ok, {none, End, Tokens}}
    end.

-file("src/glance.gleam", 1519).
-spec optional_type_annotation(list({glexer@token:token(), glexer:position()})) -> {ok,
        {gleam@option:option(type()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_type_annotation(Tokens) ->
    case Tokens of
        [{colon, _} | Tokens@1] ->
            gleam@result:map(
                type_(Tokens@1),
                fun(_use0) ->
                    {Annotation, Tokens@2} = _use0,
                    {{some, Annotation}, Tokens@2}
                end
            );

        _ ->
            {ok, {none, Tokens}}
    end.

-file("src/glance.gleam", 729).
-spec assignment(
    assignment_kind(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {statement(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
assignment(Kind, Tokens) ->
    gleam@result:'try'(
        pattern(Tokens),
        fun(_use0) ->
            {Pattern, Tokens@1} = _use0,
            gleam@result:'try'(
                optional_type_annotation(Tokens@1),
                fun(_use0@1) ->
                    {Annotation, Tokens@2} = _use0@1,
                    expect(
                        equal,
                        Tokens@2,
                        fun(_, Tokens@3) ->
                            gleam@result:'try'(
                                expression(Tokens@3),
                                fun(_use0@2) ->
                                    {Expression, Tokens@4} = _use0@2,
                                    Statement = {assignment,
                                        Kind,
                                        Pattern,
                                        Annotation,
                                        Expression},
                                    {ok, {Statement, Tokens@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1455).
-spec fn_parameter(list({glexer@token:token(), glexer:position()})) -> {ok,
        {fn_parameter(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
fn_parameter(Tokens) ->
    gleam@result:'try'(case Tokens of
            [{{name, Name}, _} | Tokens@1] ->
                {ok, {{named, Name}, Tokens@1}};

            [{{discard_name, Name@1}, _} | Tokens@2] ->
                {ok, {{discarded, Name@1}, Tokens@2}};

            [{Other, Position} | _] ->
                {error, {unexpected_token, Other, Position}};

            [] ->
                {error, unexpected_end_of_input}
        end, fun(_use0) ->
            {Name@2, Tokens@3} = _use0,
            gleam@result:'try'(
                optional_type_annotation(Tokens@3),
                fun(_use0@1) ->
                    {Type_, Tokens@4} = _use0@1,
                    {ok, {{fn_parameter, Name@2, Type_}, Tokens@4}}
                end
            )
        end).

-file("src/glance.gleam", 1386).
-spec fn_(list({glexer@token:token(), glexer:position()})) -> {ok,
        {gleam@option:option(expression()),
            list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
fn_(Tokens) ->
    expect(
        left_paren,
        Tokens,
        fun(_, Tokens@1) ->
            Result = comma_delimited(
                [],
                Tokens@1,
                fun fn_parameter/1,
                right_paren
            ),
            gleam@result:'try'(
                Result,
                fun(_use0) ->
                    {Parameters, Tokens@2} = _use0,
                    gleam@result:'try'(
                        optional_return_annotation(0, Tokens@2),
                        fun(_use0@1) ->
                            {Return, _, Tokens@3} = _use0@1,
                            expect(
                                left_brace,
                                Tokens@3,
                                fun(_, Tokens@4) ->
                                    gleam@result:'try'(
                                        statements([], Tokens@4),
                                        fun(_use0@2) ->
                                            {Body, _, Tokens@5} = _use0@2,
                                            {ok,
                                                {{some,
                                                        {fn,
                                                            Parameters,
                                                            Return,
                                                            Body}},
                                                    Tokens@5}}
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1471).
-spec function_parameter(list({glexer@token:token(), glexer:position()})) -> {ok,
        {function_parameter(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
function_parameter(Tokens) ->
    gleam@result:'try'(case Tokens of
            [] ->
                {error, unexpected_end_of_input};

            [{{name, Label}, _}, {{discard_name, Name}, _} | Tokens@1] ->
                {ok, {{some, Label}, {discarded, Name}, Tokens@1}};

            [{{discard_name, Name@1}, _} | Tokens@2] ->
                {ok, {none, {discarded, Name@1}, Tokens@2}};

            [{{name, Label@1}, _}, {{name, Name@2}, _} | Tokens@3] ->
                {ok, {{some, Label@1}, {named, Name@2}, Tokens@3}};

            [{{name, Name@3}, _} | Tokens@4] ->
                {ok, {none, {named, Name@3}, Tokens@4}};

            [{Token, Position} | _] ->
                {error, {unexpected_token, Token, Position}}
        end, fun(_use0) ->
            {Label@2, Parameter, Tokens@5} = _use0,
            gleam@result:'try'(
                optional_type_annotation(Tokens@5),
                fun(_use0@1) ->
                    {Type_, Tokens@6} = _use0@1,
                    {ok,
                        {{function_parameter, Label@2, Parameter, Type_},
                            Tokens@6}}
                end
            )
        end).

-file("src/glance.gleam", 649).
-spec function_definition(
    module_(),
    list(attribute()),
    publicity(),
    binary(),
    integer(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
function_definition(Module, Attributes, Publicity, Name, Start, Tokens) ->
    expect(
        left_paren,
        Tokens,
        fun(_use0, Tokens@1) ->
            {position, End} = _use0,
            Result = comma_delimited(
                [],
                Tokens@1,
                fun function_parameter/1,
                right_paren
            ),
            gleam@result:'try'(
                Result,
                fun(_use0@1) ->
                    {Parameters, Tokens@2} = _use0@1,
                    Result@1 = optional_return_annotation(End, Tokens@2),
                    gleam@result:'try'(
                        Result@1,
                        fun(_use0@2) ->
                            {Return_type, End@1, Tokens@3} = _use0@2,
                            gleam@result:'try'(case Tokens@3 of
                                    [{left_brace, _} | Tokens@4] ->
                                        statements([], Tokens@4);

                                    _ ->
                                        {ok, {[], End@1, Tokens@3}}
                                end, fun(_use0@3) ->
                                    {Body, End@2, Tokens@5} = _use0@3,
                                    Location = {span, Start, End@2},
                                    Function = {function,
                                        Name,
                                        Publicity,
                                        Parameters,
                                        Return_type,
                                        Body,
                                        Location},
                                    Module@1 = push_function(
                                        Module,
                                        Attributes,
                                        Function
                                    ),
                                    {ok, {Module@1, Tokens@5}}
                                end)
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1497).
-spec const_definition(
    module_(),
    list(attribute()),
    publicity(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
const_definition(Module, Attributes, Publicity, Tokens) ->
    expect_name(
        Tokens,
        fun(Name, Tokens@1) ->
            gleam@result:'try'(
                optional_type_annotation(Tokens@1),
                fun(_use0) ->
                    {Annotation, Tokens@2} = _use0,
                    expect(
                        equal,
                        Tokens@2,
                        fun(_, Tokens@3) ->
                            gleam@result:'try'(
                                expression(Tokens@3),
                                fun(_use0@1) ->
                                    {Expression, Tokens@4} = _use0@1,
                                    Constant = {constant,
                                        Name,
                                        Publicity,
                                        Annotation,
                                        Expression},
                                    Module@1 = push_constant(
                                        Module,
                                        Attributes,
                                        Constant
                                    ),
                                    {ok, {Module@1, Tokens@4}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1589).
-spec type_alias(
    module_(),
    list(attribute()),
    binary(),
    list(binary()),
    publicity(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
type_alias(Module, Attributes, Name, Parameters, Publicity, Tokens) ->
    gleam@result:'try'(
        type_(Tokens),
        fun(_use0) ->
            {Type_, Tokens@1} = _use0,
            Alias = {type_alias, Name, Publicity, Parameters, Type_},
            Module@1 = push_type_alias(Module, Attributes, Alias),
            {ok, {Module@1, Tokens@1}}
        end
    ).

-file("src/glance.gleam", 1645).
-spec fn_type(list({glexer@token:token(), glexer:position()})) -> {ok,
        {type(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
fn_type(Tokens) ->
    Result = comma_delimited([], Tokens, fun type_/1, right_paren),
    gleam@result:'try'(
        Result,
        fun(_use0) ->
            {Parameters, Tokens@1} = _use0,
            expect(
                right_arrow,
                Tokens@1,
                fun(_, Tokens@2) ->
                    gleam@result:'try'(
                        type_(Tokens@2),
                        fun(_use0@1) ->
                            {Return, Tokens@3} = _use0@1,
                            {ok,
                                {{function_type, Parameters, Return}, Tokens@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1653).
-spec tuple_type(list({glexer@token:token(), glexer:position()})) -> {ok,
        {type(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
tuple_type(Tokens) ->
    Result = comma_delimited([], Tokens, fun type_/1, right_paren),
    gleam@result:'try'(
        Result,
        fun(_use0) ->
            {Types, Tokens@1} = _use0,
            {ok, {{tuple_type, Types}, Tokens@1}}
        end
    ).

-file("src/glance.gleam", 1718).
-spec variant_field(list({glexer@token:token(), glexer:position()})) -> {ok,
        {variant_field(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
variant_field(Tokens) ->
    case Tokens of
        [{{name, Name}, _}, {colon, _} | Tokens@1] ->
            gleam@result:'try'(
                type_(Tokens@1),
                fun(_use0) ->
                    {Type_, Tokens@2} = _use0,
                    {ok, {{labelled_variant_field, Type_, Name}, Tokens@2}}
                end
            );

        Tokens@3 ->
            gleam@result:'try'(
                type_(Tokens@3),
                fun(_use0@1) ->
                    {Type_@1, Tokens@4} = _use0@1,
                    {ok, {{unlabelled_variant_field, Type_@1}, Tokens@4}}
                end
            )
    end.

-file("src/glance.gleam", 1706).
-spec optional_variant_fields(list({glexer@token:token(), glexer:position()})) -> {ok,
        {list(variant_field()), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
optional_variant_fields(Tokens) ->
    case Tokens of
        [{left_paren, _}, {right_paren, _} | Tokens@1] ->
            {ok, {[], Tokens@1}};

        [{left_paren, _} | Tokens@2] ->
            comma_delimited(
                [],
                Tokens@2,
                fun(_capture) -> variant_field(_capture) end,
                right_paren
            );

        _ ->
            {ok, {[], Tokens}}
    end.

-file("src/glance.gleam", 1695).
-spec variants(custom_type(), list({glexer@token:token(), glexer:position()})) -> {ok,
        {custom_type(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
variants(Ct, Tokens) ->
    until(
        right_brace,
        Ct,
        Tokens,
        fun(Ct@1, Tokens@1) ->
            expect_upper_name(
                Tokens@1,
                fun(Name, Tokens@2) ->
                    gleam@result:'try'(
                        optional_variant_fields(Tokens@2),
                        fun(_use0) ->
                            {Parameters, Tokens@3} = _use0,
                            Ct@2 = push_variant(
                                Ct@1,
                                {variant, Name, Parameters}
                            ),
                            {ok, {Ct@2, Tokens@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/glance.gleam", 1659).
-spec custom_type(
    module_(),
    list(attribute()),
    binary(),
    list(binary()),
    publicity(),
    boolean(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
custom_type(Module, Attributes, Name, Parameters, Publicity, Opaque_, Tokens) ->
    Ct = {custom_type, Name, Publicity, Opaque_, Parameters, []},
    gleam@result:'try'(
        variants(Ct, Tokens),
        fun(_use0) ->
            {Ct@1, Tokens@1} = _use0,
            Module@1 = push_custom_type(Module, Attributes, Ct@1),
            {ok, {Module@1, Tokens@1}}
        end
    ).

-file("src/glance.gleam", 1562).
-spec type_definition(
    module_(),
    list(attribute()),
    publicity(),
    boolean(),
    list({glexer@token:token(), glexer:position()})
) -> {ok, {module_(), list({glexer@token:token(), glexer:position()})}} |
    {error, error()}.
type_definition(Module, Attributes, Publicity, Opaque_, Tokens) ->
    expect_upper_name(
        Tokens,
        fun(Name, Tokens@1) ->
            gleam@result:'try'(
                optional_type_parameters(Tokens@1),
                fun(_use0) ->
                    {Parameters, Tokens@2} = _use0,
                    case Tokens@2 of
                        [{equal, _} | Tokens@3] ->
                            type_alias(
                                Module,
                                Attributes,
                                Name,
                                Parameters,
                                Publicity,
                                Tokens@3
                            );

                        [{left_brace, _} | Tokens@4] ->
                            _pipe = Module,
                            custom_type(
                                _pipe,
                                Attributes,
                                Name,
                                Parameters,
                                Publicity,
                                Opaque_,
                                Tokens@4
                            );

                        _ ->
                            Ct = {custom_type,
                                Name,
                                Publicity,
                                Opaque_,
                                Parameters,
                                []},
                            Module@1 = push_custom_type(Module, Attributes, Ct),
                            {ok, {Module@1, Tokens@2}}
                    end
                end
            )
        end
    ).

-file("src/glance.gleam", 433).
-spec slurp(
    module_(),
    list(attribute()),
    list({glexer@token:token(), glexer:position()})
) -> {ok, module_()} | {error, error()}.
slurp(Module, Attributes, Tokens) ->
    case Tokens of
        [{at, _} | Tokens@1] ->
            gleam@result:'try'(
                attribute(Tokens@1),
                fun(_use0) ->
                    {Attribute, Tokens@2} = _use0,
                    slurp(Module, [Attribute | Attributes], Tokens@2)
                end
            );

        [{import, _} | Tokens@3] ->
            Result = import_statement(Module, Attributes, Tokens@3),
            gleam@result:'try'(
                Result,
                fun(_use0@1) ->
                    {Module@1, Tokens@4} = _use0@1,
                    slurp(Module@1, [], Tokens@4)
                end
            );

        [{pub, _}, {type, _} | Tokens@5] ->
            Result@1 = type_definition(
                Module,
                Attributes,
                public,
                false,
                Tokens@5
            ),
            gleam@result:'try'(
                Result@1,
                fun(_use0@2) ->
                    {Module@2, Tokens@6} = _use0@2,
                    slurp(Module@2, [], Tokens@6)
                end
            );

        [{pub, _}, {opaque, _}, {type, _} | Tokens@7] ->
            Result@2 = type_definition(
                Module,
                Attributes,
                public,
                true,
                Tokens@7
            ),
            gleam@result:'try'(
                Result@2,
                fun(_use0@3) ->
                    {Module@3, Tokens@8} = _use0@3,
                    slurp(Module@3, [], Tokens@8)
                end
            );

        [{type, _} | Tokens@9] ->
            Result@3 = type_definition(
                Module,
                Attributes,
                private,
                false,
                Tokens@9
            ),
            gleam@result:'try'(
                Result@3,
                fun(_use0@4) ->
                    {Module@4, Tokens@10} = _use0@4,
                    slurp(Module@4, [], Tokens@10)
                end
            );

        [{pub, _}, {const, _} | Tokens@11] ->
            Result@4 = const_definition(Module, Attributes, public, Tokens@11),
            gleam@result:'try'(
                Result@4,
                fun(_use0@5) ->
                    {Module@5, Tokens@12} = _use0@5,
                    slurp(Module@5, [], Tokens@12)
                end
            );

        [{const, _} | Tokens@13] ->
            Result@5 = const_definition(Module, Attributes, private, Tokens@13),
            gleam@result:'try'(
                Result@5,
                fun(_use0@6) ->
                    {Module@6, Tokens@14} = _use0@6,
                    slurp(Module@6, [], Tokens@14)
                end
            );

        [{pub, Start}, {fn, _}, {{name, Name}, _} | Tokens@15] ->
            {position, Start@1} = Start,
            Result@6 = function_definition(
                Module,
                Attributes,
                public,
                Name,
                Start@1,
                Tokens@15
            ),
            gleam@result:'try'(
                Result@6,
                fun(_use0@7) ->
                    {Module@7, Tokens@16} = _use0@7,
                    slurp(Module@7, [], Tokens@16)
                end
            );

        [{fn, Start@2}, {{name, Name@1}, _} | Tokens@17] ->
            {position, Start@3} = Start@2,
            Result@7 = function_definition(
                Module,
                Attributes,
                private,
                Name@1,
                Start@3,
                Tokens@17
            ),
            gleam@result:'try'(
                Result@7,
                fun(_use0@8) ->
                    {Module@8, Tokens@18} = _use0@8,
                    slurp(Module@8, [], Tokens@18)
                end
            );

        [] ->
            {ok, Module};

        Tokens@19 ->
            unexpected_error(Tokens@19)
    end.

-file("src/glance.gleam", 304).
-spec module(binary()) -> {ok, module_()} | {error, error()}.
module(Src) ->
    _pipe = glexer:new(Src),
    _pipe@1 = glexer:discard_comments(_pipe),
    _pipe@2 = glexer:discard_whitespace(_pipe@1),
    _pipe@3 = glexer:lex(_pipe@2),
    slurp({module, [], [], [], [], []}, [], _pipe@3).
