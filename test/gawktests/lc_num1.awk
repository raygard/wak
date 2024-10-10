# Bug reported by Ulrich Drepper.
# BEGIN {
#   printf("%'d %d\n", 1000000, 1000000)
# }

# April 2010:
# This needs to be a smarter test so that systems without the %'d flag
# don't generate a needless failure.

BEGIN {
	s = sprintf("%'d", 1234)
	if (s == "1,234" || s == "1234")
		print "ok, or at least the quote flag isn't supported"
	else {
		command = "od -c"
		print("fail:", s) | command
		close(command)
	}
}
