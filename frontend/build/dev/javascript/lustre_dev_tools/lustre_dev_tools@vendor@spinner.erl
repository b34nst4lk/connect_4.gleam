-module(lustre_dev_tools@vendor@spinner).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([with_frames/2, with_colour/2, set_text/2, stop/1, start/1, new/1]).
-export_type([spinner/0, state/0, builder/0]).

-opaque spinner() :: {spinner, repeatedly:repeater(state()), list(binary())}.

-type state() :: {state,
        binary(),
        gleam@deque:deque(binary()),
        fun((binary()) -> binary())}.

-opaque builder() :: {builder,
        list(binary()),
        binary(),
        fun((binary()) -> binary())}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/vendor/spinner.gleam", 225).
-spec with_frames(builder(), list(binary())) -> builder().
with_frames(Builder, Frames) ->
    erlang:setelement(2, Builder, Frames).

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/vendor/spinner.gleam", 229).
-spec with_colour(builder(), fun((binary()) -> binary())) -> builder().
with_colour(Builder, Colour) ->
    erlang:setelement(4, Builder, Colour).

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/vendor/spinner.gleam", 258).
-spec set_text(spinner(), binary()) -> nil.
set_text(Spinner, Text) ->
    repeatedly_ffi:update_state(
        erlang:element(2, Spinner),
        fun(State) -> erlang:setelement(2, State, Text) end
    ).

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/vendor/spinner.gleam", 270).
-spec stop(spinner()) -> nil.
stop(Spinner) ->
    repeatedly_ffi:stop(erlang:element(2, Spinner)),
    gleam_stdlib:print(
        <<<<"\x{001b}[2K"/utf8, "\r"/utf8>>/binary, "\x{001b}[?25h"/utf8>>
    ).

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/vendor/spinner.gleam", 235).
-spec start(builder()) -> spinner().
start(Builder) ->
    Frames = gleam@deque:from_list(erlang:element(2, Builder)),
    Init = {state,
        erlang:element(3, Builder),
        Frames,
        erlang:element(4, Builder)},
    Repeater = (repeatedly_ffi:call(
        80,
        Init,
        fun(_use0, _) ->
            {state, Text, _, Colour} = _use0,
            _assert_subject = gleam@deque:pop_front(Frames),
            {ok, {Frame, Frames@1}} = case _assert_subject of
                {ok, {_, _}} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre_dev_tools/vendor/spinner"/utf8>>,
                                function => <<"start"/utf8>>,
                                line => 240})
            end,
            Frames@2 = gleam@deque:push_back(Frames@1, Frame),
            gleam_stdlib:print(
                <<<<<<<<<<"\x{001b}[?25l"/utf8, "\x{001b}[2K"/utf8>>/binary,
                                "\r"/utf8>>/binary,
                            (Colour(Frame))/binary>>/binary,
                        " "/utf8>>/binary,
                    Text/binary>>
            ),
            {state, Text, Frames@2, Colour}
        end
    )),
    {spinner, Repeater, erlang:element(2, Builder)}.

-file("/home/runner/work/dev-tools/dev-tools/src/lustre_dev_tools/vendor/spinner.gleam", 221).
-spec new(binary()) -> builder().
new(Text) ->
    {builder,
        [<<"⠋"/utf8>>,
            <<"⠙"/utf8>>,
            <<"⠹"/utf8>>,
            <<"⠸"/utf8>>,
            <<"⠼"/utf8>>,
            <<"⠴"/utf8>>,
            <<"⠦"/utf8>>,
            <<"⠧"/utf8>>,
            <<"⠇"/utf8>>,
            <<"⠏"/utf8>>],
        Text,
        fun gleam_community@ansi:magenta/1}.
