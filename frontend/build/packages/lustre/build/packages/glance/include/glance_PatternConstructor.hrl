-record(pattern_constructor, {
    module :: gleam@option:option(binary()),
    constructor :: binary(),
    arguments :: list(glance:field(glance:pattern())),
    with_spread :: boolean()
}).
