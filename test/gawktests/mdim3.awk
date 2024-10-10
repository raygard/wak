BEGIN {
   inp[0] = "blah"
   inp[1] = "blah"
   inp[2] = ""
   inp[3] = "Summary Statistics"
   inp[4] = "temperature,0"
   inp[5] = "rain,1"

   for (i = 1; i <= 40; i++) {
      print "i =", i
      mode = 0
      nr = 0
      delete val
      for (j = 0; j < 6; j++) {
	 x = inp[j]
	 print "\tj =", j
#	 if (i == 27 && j == 3)
#	 	stopme()
	 nf = split(x,f,",")
	 print "\tnf =", nf
	 if (++nr > 1) {
	    if (!nf)
	       mode = 1
	    else if (mode == 1)
	       val[f[1]] = f[2]
	 }
      }
   }
}
