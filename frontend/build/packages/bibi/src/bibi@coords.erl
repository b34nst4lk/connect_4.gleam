-module(bibi@coords).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([coords/0]).

-type coords() :: {coords, integer(), integer()}.


