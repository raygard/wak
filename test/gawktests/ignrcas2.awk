# Based on test program submitted by:
# Date: Sun, 7 Sep 2003 23:11:51 +0200
# From: Michael Mauch <michael.mauch@gmx.de>
# To: bug-gawk@gnu.org
# Subject: Internal error in gawk-3.1.3 with character class

BEGIN {
	IGNORECASE = 1
	if ("a" ~ /[[:alnum:]]/)
		print "OK"
	else
		print "NOT OK"
}
