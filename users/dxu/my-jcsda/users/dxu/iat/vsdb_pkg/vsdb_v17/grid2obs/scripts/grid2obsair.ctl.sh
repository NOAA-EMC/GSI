#!/bin/ksh
#set -x

## ONLYSF:  All surface data(ADPSFC, SFCSHP etc), not including the data from upper­air profiles
## ADPUPA:  upper air data including  rawinsonde,  pibals and profilers
## AIRCAR:  ACARS data
## AIRCFT:  Aircraft data  
## ANYAIR:  AIRCAR and AIRCFT etc

fhour1=${1:-00}     ;#first verification hour    
vlength=${2:-192}   ;#verification length

hint=24
vlength=$((vlength/hint*hint))
nfcst=$(( (vlength-fhour1)/24 + 1 ))

n=1
fh[1]=$fhour1
fhour=$fhour1
while [ $fhour -le $vlength ]; do
 fhour=$((fhour+hint))
 n=$((n+1))
 fh[n]=$fhour
done

echo "1st fcst hour: ${fh[1]};  Last fcst hour: ${fh[nfcst]}"
gdtype=${gdtype:-3}   ;# global grid type 2->2.5-deg; 3->1-deg, 4->0.5-deg


echo "V01   10" >header
echo "    1  CTLF/$gdtype" >>header
echo "    $nfcst  ${fh[1]}"  >>header
n=2; while [ $n -le $nfcst ]; do
 echo "  ${fh[n]}"          >>header       
 n=$((n+1))
done

#-----------------------------------------------------------
#  create upper air ADPUPA control file over large regions
#-----------------------------------------------------------
cat >ctlair1 <<EOF3
    1  19
    1  ADPUPA
   11  G236
  GGLB      
  GEUR
  GASI
  GAFR
  GAUS
  GNA
  GSA
  GNH
  GSH
  GTRP
    1  SL1L2
    5  Z  
  T
  RH
  Q
  VWND
   14  P1000
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
  P50
  P20
  P10
EOF3


#-----------------------------------------------------------
#  create upper air ANYAIR control file over large regions
#-----------------------------------------------------------
cat >ctlair3 <<EOF5
    1  19
    3  ANYAIR
  PROFLR
  VADWND
   11  G236
  GGLB      
  GEUR
  GASI
  GAFR
  GAUS
  GNA
  GSA
  GNH
  GSH
  GTRP
    1  SL1L2
    3  Z  
  T
  VWND
    9  P1000-850
  P850-700
  P700-550
  P550-400
  P400-300
  P300-250
  P250-200
  P200-150
  P150-50
EOF5
#----------------------------

cat header ctlair1 header ctlair3 > grid2obsair.ctl
#cat header ctlair1  > grid2obsair.ctl
rm ctlair1 ctlair3 header

exit
