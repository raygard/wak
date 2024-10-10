#!/usr/bin/gawk -f
   
function f(x) {
	return x;
}

BEGIN {
	print "a[b] is " (a["b"] ? "true" : "false");

	f(a["b"]);

	print "a[b] is " (a["b"] ? "true" : "false");

	print a["b"];
}
