-record(pattern_bit_string, {
    segments :: list({glance:pattern(),
        list(glance:bit_string_segment_option(glance:pattern()))})
}).
