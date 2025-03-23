-record(constant, {
    name :: binary(),
    publicity :: glance:publicity(),
    annotation :: gleam@option:option(glance:type()),
    value :: glance:expression()
}).
