BEGIN {
	str1 = "aabbbc"
	str2 = "aaabcc"
	if (str1 ~ /b{2,}/) printf("\"%s\" matches /b{2,}/\n", str1)
	if (str2 ~ /b{2,}/) printf("\"%s\" matches /b{2,}/\n", str2)
}
