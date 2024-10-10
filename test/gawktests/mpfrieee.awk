# Test IEEE-754 binary double format
BEGIN {
	x = 1.0e-320
	i = 0
	while (x > 0) {
		printf("%.15e\n", x)
		x /= 2

		# terminate early when the test is going to fail.
		if (++i > 50)
			break
	}
}
