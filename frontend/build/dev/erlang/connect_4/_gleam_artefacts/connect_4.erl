-module(connect_4).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-file("src/connect_4.gleam", 3).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<"Hello from connect_4!"/utf8>>).
