BEGIN {
	RS = ""
	FPAT = "\\w+"
}

{
	print
	$2 = "-"
	print
}
