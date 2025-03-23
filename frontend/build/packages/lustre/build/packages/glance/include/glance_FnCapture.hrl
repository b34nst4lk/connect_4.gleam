-record(fn_capture, {
    label :: gleam@option:option(binary()),
    function :: glance:expression(),
    arguments_before :: list(glance:field(glance:expression())),
    arguments_after :: list(glance:field(glance:expression()))
}).
