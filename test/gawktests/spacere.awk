BEGIN {
	c[" "] = "\" \""
	c["\a"] = "\\a"
	c["\b"] = "\\b"
	c["\f"] = "\\f"
	c["\n"] = "\\n"
	c["\r"] = "\\r"
	c["\t"] = "\\t"
	c["\v"] = "\\v"

  # 20251224 rdg  Force sort to use byte-order sorting
	sort = "LC_ALL=C sort"

	for (i in c)
		printf("%s %s [[:space:]]\n", c[i],
			i ~ /[[:space:]]/ ? "~" : "!~") | sort

	for (i in c)
		printf("%s %s [[:blank:]]\n", c[i],
			i ~ /[[:blank:]]/ ? "~" : "!~") | sort

	close(sort)
}
