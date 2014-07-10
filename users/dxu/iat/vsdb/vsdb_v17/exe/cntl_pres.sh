#!/bin/ksh
#set -x

## total field on isobaric layers (SL1L2)

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


cat > pres_$1.ctl << EOF
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
    2  SL1L2
       VL1L2
    6  HGT            7  100  0
       O3           154 100  0
       T             11 100  0
       U             33 100  0
       V             34 100  0
       WIND          32 100  0
   16  P1000 
       P925 
       P850 
       P700 
       P500 
       P400 
       P300 
       P250 
       P200 
       P150 
       P100 
       P70 
       P50 
       P30 
       P20 
       P10
EOF

exit
