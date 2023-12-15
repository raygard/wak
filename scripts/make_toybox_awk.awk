BEGIN {while ((getline < "./toybox_awk_parts/awk1.c") > 0) print}
END {while ((getline < "./toybox_awk_parts/awk2.c") > 0) print}
/#ifndef FOR_TOYBOX/, /#endif.*TOYBOX/ {next}
1
