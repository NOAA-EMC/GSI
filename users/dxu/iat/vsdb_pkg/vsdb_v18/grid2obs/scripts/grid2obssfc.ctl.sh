#!/bin/ksh
#set -x

## ONLYSF:  All surface data(ADPSFC, SFCSHP etc), not including the data from upper­air profiles
## ADPUPA:  upper air data including  rawinsonde,  pibals and profilers
## AIRCAR:  ACARS data
## AIRCFT:  Aircraft data  

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
gdtype=${gdtype:-2}   ;# global grid type 2->2.5-deg; 3->1-deg, 4->0.5-deg


echo "V01   10" >header
echo "    1  CTLF/$gdtype" >>header
echo "    $nfcst  ${fh[1]}"  >>header
n=2; while [ $n -le $nfcst ]; do
 echo "  ${fh[n]}"          >>header       
 n=$((n+1))
done

#--------------------------------------------------------------------
# 1. create sfc grid2obs control file, over grid#104 and sub-regions  
#--------------------------------------------------------------------
cat >ctlsfc1 <<EOF1
    1  19
    1  ONLYSF
   14  G104/NWC
  G104/SWC
  G104/NMT
  G104/SMT
  G104/GRB
  G104/SWD
  G104/NPL
  G104/SPL
  G104/MDW
  G104/LMV
  G104/GMC
  G104/NEC
  G104/SEC
  G104/APL
    1  SL1L2
    8  SLP   
  T
  DPT
  RH
  TCLD
  PWO
  CLDBT
  VWND
    1  SFC
EOF1

cat >ctlsfc2 <<EOF2
    1  19
    1  ONLYSF
    7  G104/WCA
  G104/ECA
  G104/ATC
  G104/NAK
  G104/SAK
  G104/NPO
  G104/MEX
    1  SL1L2
    8  SLP
  T
  DPT
  RH
  TCLD
  PWO
  CLDBT
  VWND
    1  SFC
EOF2

#----------------------------

cat  header ctlsfc1  header ctlsfc2  > grid2obssfc.ctl
rm ctlsfc1 ctlsfc2  header

exit
