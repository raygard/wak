function foo(gensub)
{
	print gensub
	print lshift(1, 1)
}

BEGIN {
	x = 5
	foo(x)
}
