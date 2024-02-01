FNR<6 { next }
NR==6 {printf \
    "// wak.c -- \"monolithic\" (one-file) awk implementation\n"\
    "// Copyright 2024 Ray Gardner\n"\
    "// License: 0BSD\n"\
    "// vi: tabstop=2 softtabstop=2 shiftwidth=2\n"\
    "\n"
}
#/#if MODULAR/, /#endif/ { next }
/#ifndef MONO/, /#endif.*MONO/ { next }
/#include "/ { next }
/int trace_sw = 0/ { next }
/^EXTERN/ { sub(/EXTERN/, "static") }
/remove_for_monolithic/, /END_remove_for_monolithic/ { next }
/EXTERN/ { next }

1       # copy remaining lines
