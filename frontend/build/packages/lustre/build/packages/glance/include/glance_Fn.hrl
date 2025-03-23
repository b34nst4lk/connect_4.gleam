-record(fn, {
    arguments :: list(glance:fn_parameter()),
    return_annotation :: gleam@option:option(glance:type()),
    body :: list(glance:statement())
}).
