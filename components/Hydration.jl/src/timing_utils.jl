"""
    is_it_time!(last_time_ref, period=Hour(1))

Checks if `period` has passed since the last time this function returned true.
The last time it returned true is stored in `last_time_ref` which should be a `Ref` to a `DateTime`.
"""
function is_it_time!(last_time_ref, period=Hour(1))
    if now() - last_time_ref[] < period
        return false
    else
        last_time_ref[] = now()
        return true
    end
end