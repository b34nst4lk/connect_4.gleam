-record(custom_type, {
    name :: binary(),
    publicity :: glance:publicity(),
    opaque_ :: boolean(),
    parameters :: list(binary()),
    variants :: list(glance:variant())
}).
