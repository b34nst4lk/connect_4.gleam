-record(lexer, {
    source :: binary(),
    byte_offset :: integer(),
    preserve_whitespace :: boolean(),
    preserve_comments :: boolean(),
    mode :: glexer:lexer_mode()
}).
