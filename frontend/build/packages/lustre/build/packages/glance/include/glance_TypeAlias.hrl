-record(type_alias, {
    name :: binary(),
    publicity :: glance:publicity(),
    parameters :: list(binary()),
    aliased :: glance:type()
}).
