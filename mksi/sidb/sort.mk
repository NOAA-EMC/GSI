#!/usr/bin/make

all: actvchan.sort-by-dt usblchan.sort-by-dt

# The target below, "actvchan.sort-by-dt:", can be used to create a
# version of table active_channels.tbl, sorted by the beginning time of
# each record entry for inspection purpose.

actvchan.sort-by-dt: active_channels.tbl do-it-anyway
	@ sed -e'/^[ 	]*$$/d' -e'/^[ 	]*#/d' < active_channels.tbl |\
	  sort -s    -k1,1 		|\
	  sort -s -n -k5,5		|\
	  sort -s -n -k4,4		|\
	  sort -s -n -k3,3		|\
	  sort -s -n -k2,2 >$@

usblchan.sort-by-dt: usable_channels.tbl do-it-anyway
	@ sed -e'/^[ 	]*$$/d' -e'/^[ 	]*#/d' < usable_channels.tbl |\
	  sort -s    -k1,1 		|\
	  sort -s -n -k5,5		|\
	  sort -s -n -k4,4		|\
	  sort -s -n -k3,3		|\
	  sort -s -n -k2,2 >$@

do-it-anyway:
