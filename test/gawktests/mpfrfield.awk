#! /bin/gawk -Mf

NR == 1 {
	min = $1
}

{
	if ($1 < min)
		min = $1
}

END {
	print "min", min
}
