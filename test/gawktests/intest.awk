BEGIN {
	bool_result = ((b = 1) in c);
	print bool_result, b	# gawk-3.0.1 prints "0 "; should print "0 1"
} 
