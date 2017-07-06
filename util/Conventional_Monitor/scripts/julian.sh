:
#set -x
if [ $# -eq 0 ] ; then
  echo "$0 yyyymmdd"
  echo "prints YYYYjday"
  exit 8
fi
year=`echo $1 | cut -c1-4`
month=`echo $1 | cut -c5-6`
day=`echo $1 | cut -c7-8`
day1=`expr $1 % 100`

if [ "$day" -ne "$day1" ] ; then
   echo "date error"
   exit 8
fi


case $month in
   01) mday=0;;
   02) mday=31;;
   03) mday=59;;
   04) mday=90;;
   05) mday=120;;
   06) mday=151;;
   07) mday=181;;
   08) mday=212;;
   09) mday=243;;
   10) mday=273;;
   11) mday=304;;
   12) mday=334;;
    *) echo "month error"
       exit 8;
esac
jday=`expr $mday + $day`
leap=`expr $year % 4`
[ `expr $year % 100` -eq 0 ] && leap=1
[ `expr $year % 400` -eq 0 ] && leap=0
[ $month -gt 2 -a $leap -eq 0 ] && jday=`expr $jday + 1`

[ "$jday" -le 9 ] && jday=0$jday
[ "$jday" -le 99 ] && jday=0$jday
echo "$year$jday"
exit 0
