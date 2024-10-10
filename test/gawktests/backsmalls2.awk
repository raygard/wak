BEGIN {
	pat["^\\s*$"] = pat["^\\s+$"] = pat["^\\s?$"] = pat["^\\s{1}$"] = 1
	for (i in pat) {
		if (" " !~ i) {
			printf("pattern \"%s\" failed!\n", i) > "/dev/stderr"
			exit 1
		}
	}
	exit 0
}
