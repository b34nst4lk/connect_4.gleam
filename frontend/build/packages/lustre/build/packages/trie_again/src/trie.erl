-module(trie).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([fold/3, get/2, has_path/2, map/2, new/0, delete/2, insert/3, from_list/1, paths/1, singleton/2, size/1, is_empty/1, subtrie/2, to_list/1, update/3, values/1]).
-export_type([trie/2]).

-opaque trie(FOF, FOG) :: {trie,
        gleam@option:option(FOG),
        gleam@dict:dict(FOF, trie(FOF, FOG))}.

-spec do_delete(trie(FOO, FOP), list(FOO)) -> gleam@option:option(trie(FOO, FOP)).
do_delete(Trie, Path) ->
    case {Path, Trie} of
        {[], {trie, _, Children_map}} ->
            case maps:size(Children_map) of
                0 ->
                    none;

                _ ->
                    {some, {trie, none, Children_map}}
            end;

        {[First | Rest], {trie, Entry, Children_map@1}} ->
            New_children = case gleam@dict:get(Children_map@1, First) of
                {error, _} ->
                    Children_map@1;

                {ok, Child} ->
                    case do_delete(Child, Rest) of
                        none ->
                            gleam@dict:delete(Children_map@1, First);

                        {some, Trie@1} ->
                            gleam@dict:insert(Children_map@1, First, Trie@1)
                    end
            end,
            case {Entry, maps:size(New_children)} of
                {none, 0} ->
                    none;

                {_, _} ->
                    {some, {trie, Entry, New_children}}
            end
    end.

-spec fold(trie(FOW, FOX), FPA, fun((FPA, list(FOW), FOX) -> FPA)) -> FPA.
fold(Trie, Initial, Fun) ->
    gleam@dict:fold(
        erlang:element(3, Trie),
        begin
            _pipe = erlang:element(2, Trie),
            _pipe@1 = gleam@option:map(
                _pipe,
                fun(_capture) -> Fun(Initial, [], _capture) end
            ),
            gleam@option:unwrap(_pipe@1, Initial)
        end,
        fun(Acc, First, Trie@1) ->
            fold(
                Trie@1,
                Acc,
                fun(Acc@1, Rest, Value) -> Fun(Acc@1, [First | Rest], Value) end
            )
        end
    ).

-spec get(trie(FPI, FPJ), list(FPI)) -> {ok, FPJ} | {error, nil}.
get(From, Path) ->
    case {Path, From} of
        {[], {trie, none, _}} ->
            {error, nil};

        {[], {trie, {some, Value}, _}} ->
            {ok, Value};

        {[First | Rest], {trie, _, Children_map}} ->
            _pipe = Children_map,
            _pipe@1 = gleam@dict:get(_pipe, First),
            gleam@result:then(_pipe@1, fun(_capture) -> get(_capture, Rest) end)
    end.

-spec has_path(trie(FPP, any()), list(FPP)) -> boolean().
has_path(Trie, Path) ->
    case get(Trie, Path) of
        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

-spec map(trie(FQF, FQG), fun((FQG) -> FQJ)) -> trie(FQF, FQJ).
map(Trie, Fun) ->
    {trie,
        gleam@option:map(erlang:element(2, Trie), Fun),
        gleam@dict:map_values(
            erlang:element(3, Trie),
            fun(_, T) -> map(T, Fun) end
        )}.

-spec new() -> trie(any(), any()).
new() ->
    {trie, none, gleam@dict:new()}.

-spec delete(trie(FOH, FOI), list(FOH)) -> trie(FOH, FOI).
delete(Trie, Path) ->
    _pipe = do_delete(Trie, Path),
    gleam@option:unwrap(_pipe, new()).

-spec insert(trie(FPU, FPV), list(FPU), FPV) -> trie(FPU, FPV).
insert(Trie, Path, Value) ->
    case {Path, Trie} of
        {[], {trie, _, Children_map}} ->
            {trie, {some, Value}, Children_map};

        {[First | Rest], {trie, Entry, Children_map@1}} ->
            _pipe = gleam@dict:get(Children_map@1, First),
            _pipe@1 = gleam@result:unwrap(_pipe, new()),
            _pipe@2 = insert(_pipe@1, Rest, Value),
            _pipe@3 = gleam@dict:insert(Children_map@1, First, _pipe@2),
            {trie, Entry, _pipe@3}
    end.

-spec from_list(list({list(FPC), FPE})) -> trie(FPC, FPE).
from_list(List) ->
    gleam@list:fold(
        List,
        new(),
        fun(Trie, Pair) ->
            insert(Trie, erlang:element(1, Pair), erlang:element(2, Pair))
        end
    ).

-spec paths(trie(FQQ, any())) -> list(list(FQQ)).
paths(Trie) ->
    fold(Trie, [], fun(Rest, Path, _) -> [Path | Rest] end).

-spec singleton(list(FQW), FQY) -> trie(FQW, FQY).
singleton(Path, Value) ->
    insert(new(), Path, Value).

-spec size(trie(any(), any())) -> integer().
size(Trie) ->
    fold(Trie, 0, fun(Acc, _, _) -> Acc + 1 end).

-spec is_empty(trie(any(), any())) -> boolean().
is_empty(Trie) ->
    size(Trie) =:= 0.

-spec subtrie(trie(FRF, FRG), list(FRF)) -> {ok, trie(FRF, FRG)} | {error, nil}.
subtrie(Trie, Prefix) ->
    case {Prefix, Trie} of
        {[], _} ->
            {ok, Trie};

        {[First | Rest], {trie, _, Children_map}} ->
            _pipe = Children_map,
            _pipe@1 = gleam@dict:get(_pipe, First),
            _pipe@2 = gleam@result:'try'(
                _pipe@1,
                fun(_capture) -> subtrie(_capture, Rest) end
            ),
            gleam@result:map(
                _pipe@2,
                fun(Subtrie) -> _pipe@3 = gleam@dict:new(),
                    _pipe@4 = gleam@dict:insert(_pipe@3, First, Subtrie),
                    {trie, none, _pipe@4} end
            )
    end.

-spec to_list(trie(FRO, FRP)) -> list({list(FRO), FRP}).
to_list(Trie) ->
    fold(Trie, [], fun(Rest, Path, Value) -> [{Path, Value} | Rest] end).

-spec do_update(
    trie(FSD, FSE),
    list(FSD),
    fun((gleam@option:option(FSE)) -> gleam@option:option(FSE))
) -> gleam@option:option(trie(FSD, FSE)).
do_update(Trie, Path, Fun) ->
    case {Path, Trie} of
        {[], {trie, Entry, Children_map}} ->
            case {Fun(Entry), maps:size(Children_map)} of
                {none, 0} ->
                    none;

                {_ = New_entry, _} ->
                    {some, {trie, New_entry, Children_map}}
            end;

        {[First | Rest], {trie, Entry@1, Children_map@1}} ->
            New_children = case gleam@dict:get(Children_map@1, First) of
                {ok, Child} ->
                    case do_update(Child, Rest, Fun) of
                        none ->
                            gleam@dict:delete(Children_map@1, First);

                        {some, New_child} ->
                            gleam@dict:insert(Children_map@1, First, New_child)
                    end;

                {error, _} ->
                    case Fun(none) of
                        none ->
                            Children_map@1;

                        {some, Value} ->
                            gleam@dict:insert(
                                Children_map@1,
                                First,
                                singleton(Rest, Value)
                            )
                    end
            end,
            case {Entry@1, maps:size(New_children)} of
                {none, 0} ->
                    none;

                {_, _} ->
                    {some, {trie, Entry@1, New_children}}
            end
    end.

-spec update(
    trie(FRU, FRV),
    list(FRU),
    fun((gleam@option:option(FRV)) -> gleam@option:option(FRV))
) -> trie(FRU, FRV).
update(Trie, Path, Fun) ->
    _pipe = do_update(Trie, Path, Fun),
    gleam@option:unwrap(_pipe, new()).

-spec values(trie(any(), FSO)) -> list(FSO).
values(Trie) ->
    fold(Trie, [], fun(Values, _, Value) -> [Value | Values] end).
