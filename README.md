
# input event

    pin         :: integer()
    pin_reg = 0 :: integer()
    interrupt   :: rising | falling | both
    polarity  = false :: boolean()
    direct    = false :: boolean()

# output event

    pin         :: integer()
    pin_reg = 0 :: integer()
    value       :: 0 | 1 | boolean()
    init_value  :: high | low
    polarity  = false :: boolean()
    direct    = false :: boolean()
