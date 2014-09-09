#!/bin/ksh
#set -x

## anomaly type (SAL1L2 and VAL1L2)

if [ $# -lt 8 ] ; then
 echo "Usage : $0 exp VFDAY HH fmin fmax fout obtype gd"
 exit
fi

## exp and obtype must be capital letters
exp=`echo $1 |tr "[a-z]" "[A-Z]" `       ##exp name
VFDAY=$2                                 ##target verification day
HH=$3                                    ##target verification cycle
fmin=$4                                  ##minimum  forecast hour
fmax=$5                                  ##maximum  forecast hour
fout=$6                                  ##forecast output frequency
obtype=`echo $7 |tr "[a-z]" "[A-Z]" `
gd=`echo $8 |tr "[a-z]" "[A-Z]" `


fmin=$(( $((fmin/fout)) *fout ))
fmax=$(( $((fmax/fout)) *fout ))
fnum=$(( $(($((fmax-fmin))/fout)) + 1 ))


cat > anom_$1.ctl << EOF
V01   10
    1  $exp
    1  $fnum $fmin $fmax $fout
    1  ${VFDAY}${HH}  
    1  ${obtype}     
    5  ${gd}           
       ${gd}/NHX 0  20  360  80
       ${gd}/SHX 0 -80  360 -20
       ${gd}/TRO 0 -20  360  20
       ${gd}/PNA 180 20 320  75
    2  SAL1L2
       VAL1L2
   10  HGT            7  100  0
       HGT_WV1/0-3    7  100  0
       HGT_WV1/4-9    7  100  0
       HGT_WV1/10-20  7  100  0
       HGT_WV1/0-20   7  100  0
       PMSL          2  102  0
       T             11 100  0
       U             33 100  0
       V             34 100  0
       WIND          32 100  0
   7   P1000
       P850
       P700
       P500
       P250
       P200
       P100
EOF

exit
