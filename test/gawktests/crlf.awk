BEGIN {
	print \
		"hi there"
	print "hello \
world"
	if ("foo" ~ /fo\
o/)
		print "matches"
	else
		print "does not match!"
}
