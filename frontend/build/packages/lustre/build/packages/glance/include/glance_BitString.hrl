-record(bit_string, {
    segments :: list({glance:expression(),
        list(glance:bit_string_segment_option(glance:expression()))})
}).
