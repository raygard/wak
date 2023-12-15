FNR<5 { next }
#/#if MODULAR/, /#endif/ { next }
/#ifndef MONO/, /#endif.*MONO/ { next }
/#include "/ { next }
/int trace_sw = 0/ { next }
/^EXTERN/ { sub(/EXTERN/, "static") }
/remove_for_monolithic/, /END_remove_for_monolithic/ { next }
/EXTERN/ { next }

1       # copy remaining lines
