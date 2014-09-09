
set -e
if [ $# -ne 1 ] ; then
	echo "Usage: $0 MM|mmm"
	exit 8
fi
nchar=`expr $1 : '.*'`
if [ $nchar -eq 2 ] ; then
	mm=`expr $1 + 0`
	case $mm in
		1)	echo "jan";;
		2)	echo "feb";;
		3)	echo "mar";;
		4)	echo "apr";;
		5)	echo "may";;
		6)	echo "jun";;
		7)	echo "jul";;
		8)	echo "aug";;
		9)	echo "sep";;
		10) echo "oct";;
		11) echo "nov";;
		12) echo "dec";;
	esac
elif [ $nchar -eq 3 ] ; then
    mm=` echo $1| tr "[A-Z]" "[a-z]" `
	case $mm in
		"jan") echo 01;;
		"feb") echo 02;;
		"mar") echo 03;;
		"apr") echo 04;;
		"may") echo 05;;
		"jun") echo 06;;
		"jul") echo 07;;
		"aug") echo 08;;
		"sep") echo 09;;
		"oct") echo 10;;
		"nov") echo 11;;
		"dec") echo 12;;
	esac
else
	echo "Usage: $0 MM|mmm"
	exit 8
fi
