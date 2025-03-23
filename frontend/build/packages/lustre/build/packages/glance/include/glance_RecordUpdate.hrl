-record(record_update, {
    module :: gleam@option:option(binary()),
    constructor :: binary(),
    record :: glance:expression(),
    fields :: list(glance:record_update_field(glance:expression()))
}).
