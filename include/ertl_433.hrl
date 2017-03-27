-ifndef(ertl_433).
-define(ertl_433, ok).

-record(device, {id, address, unit, state, last_state_change_time}).
-record(sensor, {id, value, prev_value, min, max, today, battery, last_update_time}).


-endif.
