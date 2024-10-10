# Date: Wed, 14 Mar 2012 08:10:48 -0500
# From: John Haque <j.eh@mchsi.com>
# To: arnold@skeeve.com
# Subject: gawk printf format bug 
# 
# Hi.
# 
# I think this is a bug:
# 
# $ gawk 'BEGIN { printf("%.0x%#x%#x\n", 0, 167, 167)}'
# 570xa7
# 
# It should print 0xa70xa7.
# 
# The solution is to initialize base to 0 in the beginning
# of the while loop along with other stuff (format_tree).
# 
# Thanks.
# 
# John
 
BEGIN { printf(">>%.0x<< >>%#x<< >>%#x<<\n", 0, 167, 167) }
