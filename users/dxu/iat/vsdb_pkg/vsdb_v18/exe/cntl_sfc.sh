#!/bin/ksh
#set -x

## total field at a single layer   (SL1L2)

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


cat > sfc_$1.ctl << EOF
V01   10
    1  $exp
    1  $fnum $fmin $fmax $fout
    1  ${VFDAY}${HH}  
    1  ${obtype}     
    12 ${gd}           
       ${gd}/NHX 0  20  360  80
       ${gd}/SHX 0 -80  360 -20
       ${gd}/TRO 0 -20  360  20
       ${gd}/N60 0  60  360  90
       ${gd}/S60 0 -90  360 -60
       ${gd}/NPO
       ${gd}/SPO
       ${gd}/NAO
       ${gd}/SAO
       ${gd}/CAM
       ${gd}/NSA
    1  SL1L2
   46  ALBSFC        84   1     0 
       CPRAT        214   1     0 
       PRAT          59   1     0   
       ACPC          63   1     0 
       APCP          61   1     0 
       CAPE         157   1     0 
       CWAT          76   200   0 
       PWAT          54   200   0 
       DLWSFC       205   1     0 
       ULWSFC       212   1     0 
       ULWTOA       212   8     0 
       DSWSFC       204   1     0 
       USWSFC       211   1     0 
       USWTOA       211   8     0 
       LH           121   1     0 
       SH           122   1     0 
       GFLX         155   1     0 
       HGTTRP         7   7     0 
       PRESTRP        1   7     0 
       TMPTRP        11   7     0 
       HPBL         221   1     0 
       PSFC           1   1     0 
       PSL            2   102   0 
       RH2m          52   105   2 
       SPFH2m        51   105   2 
       TCLD          71   200   0 
       LCLD          71   214   0 
       MCLD          71   224   0 
       HCLD          71   234   0 
       BCLD          71   211   0 
       T2m           11   105   2 
       TMAX2m        15   105   2 
       TMIN2m        16   105   2 
       TG            11   1     0 
       TOZNE         10   200   0 
       UFLX         124   1     0 
       VFLX         125   1     0 
       U10m          33   105   10
       V10m          34   105   10
       UGWD         147   1     10
       VGWD         148   1     10
       WEASD         65   1     0 
       RUNOFF        90   1     0 
       TSOILT        11   112   10
       TSOILT        85   112   10
       WSOILT       144   112   10
   1   P1000
EOF

exit
