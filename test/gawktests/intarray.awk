BEGIN {
	nf = split("5 |05|0x4|00| 5|-0x12| 011|-013|1.0|5.1e1|-5|-05|+2", f, "|")
	for (i = 1; i <= nf; i++) {
		delete g
		g[f[i]]
		for (x in g) {
			if (x"" != f[i]"")
				printf "Error in string test: [%s] != [%s]\n", x, f[i]
		}

		delete g
		z = f[i]+0	# trigger numeric conversion
		g[f[i]]
		for (x in g) {
			if (x"" != f[i]"")
				printf "Error in strnum test: [%s] != [%s]\n", x, f[i]
		}
	}
}
