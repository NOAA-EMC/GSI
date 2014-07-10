#!/bin/ksh
#set -x

#------------------------------------------------------------
#--for getting vector wind correlation and rms on a single layer
#------------------------------------------------------------
#  A=sum[(uf-uc)*(uo-uc)+(vf-vc)*(vo-vc)]
#  B=sum[(uf-uc)**2+(vf-vc)**2], C=sum[(uo-uc)**2+(vo-vc)**2]
#  cor=A/sqrt(B*C), rms=sqrt(B+C-2*A)
#-------------------------------------------------------


export exedir=${exedir:-/stmp/$LOGNAME/vsdb_stats}
if [ ! -s $exedir ]; then mkdir -p $exedir; fi
cd $exedir

export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data06}
export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export FC=${FC:-xlf90}
export FFLAG=${FFLAG:-" "}


## verification type: anom, pres, sfc
export vtype=${1:-anom}

## verification variable parameters: e.g. WIND
export vnam=${2:-WIND}
export reg=${3:-G2/TRO}
export lev=${4:-P850}

## verification ending date and number of days back 
export edate=${5:-20120831}
export ndays=${6:-31}
       nhours=`expr $ndays \* 24 - 24`
       tmp=`$ndate -$nhours ${edate}00 `
       sdate=`echo $tmp | cut -c 1-8`

## forecast cycles to be vefified: 00Z, 06Z, 12Z, 18Z
export cyclist=${7:-"00"}
       ncyc=`echo $cyclist | wc -w`

## forecast length in days, excluding 00Z forecasts (gfs default=16, 384 hours) 
export fdays=${8:-16}
       fdaysp1=`expr $fdays + 1 `
       vlength=`expr $fdays \* 24 `

## forecast output frequency requried for verification
export fhout=${9:-6}
       nfcst=`expr $vlength \/ $fhout + 1`

## create output name (first remove / from parameter names)
vnam1=`echo $vnam | sed "s?/??g" |sed "s?_WV1?WV?g"`  
reg1=`echo $reg | sed "s?/??g"`
outname1=${vnam1}_${lev}_${reg1}_${sdate}${edate}
outname=${10:-$outname1}

## remove missing data from all models to unify sample size, 0-->NO, 1-->Yes
maskmiss=${maskmiss:-${11:-"1"}}

## model names and number of models
export mdlist=${mdlist:-${12:-"gfs"}}
nmd0=`echo $mdlist | wc -w`                       
nmdcyc=`expr $nmd0 \* $ncyc `

set -A mdname $mdlist
set -A cycname $cyclist
if [ -s modelname.txt ]; then rm modelname.txt ;fi
>modelname.txt
n=0
while [ $n -lt $nmd0 ]; do
 m=0
 while [ $m -lt $ncyc ]; do
  echo "${mdname[n]}${cycname[m]}" >>modelname.txt
  m=`expr $m + 1 `
 done
 n=`expr $n + 1 `
done

#--------------------------------------------------
# search data
#--------------------------------------------------
if [ -s ${outname}.txt ]; then rm ${outname}.txt ;fi
if [ -s ${outname}.bin ]; then rm ${outname}.bin ;fi
if [ -s ${outname}.ctl ]; then rm ${outname}.ctl ;fi

for model in $mdlist; do
mdl=`echo $model |tr "[a-z]" "[A-Z]" `
for cyc in $cyclist; do

cdate=$sdate
while [ $cdate -le $edate ]; do
  fhour=00; vhr=$cyc
  while [ $fhour -le $vlength ]; do
    datadir=${vsdb_data}/${vtype}/${vhr}Z/${model}
    vsdbname=${datadir}/${model}_${cdate}.vsdb
    string=" $mdl $fhour ${cdate}${vhr} $mdl $reg VAL1L2 $vnam $lev "
    mycheck=$( grep "$string"  $vsdbname )
    if [ $? -ne 0 ]; then
      echo "missing" >>$outname.txt
    else
      grep "$string" $vsdbname |cat >>$outname.txt
    fi
    fhour=` expr $fhour + $fhout `
    if [ $fhour -lt 10 ]; then fhour=0$fhour ; fi
    vhr=` expr $vhr + $fhout `
    if [ $vhr -ge 24 ]; then vhr=`expr $vhr - 24 `; fi
    if [ $vhr -lt 10 ]; then vhr=0$vhr ; fi
  done
cdate=`$ndate +24 ${cdate}00 | cut -c 1-8 `
done   ;#end of cdate
done   ;#end of cycle
done   ;#end of model


#------------------------------------------------------------
# compute skill scores (anomaly correlations) and rms errors
# save output in binary format for GrADS 
#------------------------------------------------------------
rm convert.f convert.x tmp.txt
yyyymm=`echo $edate | cut -c 1-6`

cat >convert.f <<EOF
!
! read data from vsdb database, compute anomaly correlation
! and rms bias, write out in binary format for graphic display
       integer, parameter :: nday=${ndays}, fday=${nfcst}, bin=nday
       integer, parameter :: nmd=${nmdcyc}
       integer, parameter :: bin0=20, ns=7   
       real*4             :: points(fday,nday)
       real*8             :: vsdb(ns,fday,nday)
       real*4             :: cor(fday,nday,nmd), rms(fday,nday,nmd), bias(fday,nday,nmd)   
       real*4             :: num(fday,nmd), mcor(fday,nmd), mrms(fday,nmd), mbias(fday,nmd)   
       real*4             :: binbnd(bin+1), binbnd0(bin0+1)
       real*4             :: bincor(fday,bin,nmd),bincor0(fday,bin0)
       real*4             :: fmiss(fday,nday)    
       integer          :: nchar(fday,nday,nmd),nhead(fday,nday,nmd)
       character (1000) :: string
       character(1)     :: substring
       character*20     :: mdname(nmd)
       data bad/-99.9/,substring/"="/
       data maskmiss /${maskmiss}/             

       open(9,file="modelname.txt",form="formatted",status="old")
       open(10,file="${outname}.txt",form="formatted",status="old")
       open(11,file="tmp.txt",form="formatted",status="new")
       open(20,file="${outname}.bin",form="unformatted",status="new")

       do n=1,nmd
        read(9,'(a)') mdname(n)
       enddo

! create bounds of bins for frequency distribution of anomaly correlations (0,1)
! for ndays <=bin0 cases
       delcor=1.0/bin
       do i=1,bin+1
        binbnd(i)=(i-1)*delcor
       enddo
! for ndays >bin0 cases, use maximum bin0 
       delcor0=1.0/bin0
       do i=1,bin0+1
        binbnd0(i)=(i-1)*delcor0
       enddo

       rewind (10)
! find length of character header
       do n=1,nmd
       do j=1,nday
       do i=1,fday
         read(10,'(1A)') string
         nchar(i,j,n)=len_trim(string)
         nhead(i,j,n)=index(string,substring)  !find character header length before "="
         write(11,'(a)') string(nhead(i,j,n)+1:nchar(i,j,n))
!         write(12,'(a)') trim(string), nchar(i,j,n), nhead(i,j,n) 
       enddo
       enddo
       enddo

! read data
       rewind (11)
       num=0; fmiss=0.0; bincor=0.0
       mcor=0; mrms=0; mbias=0

      do 100 n=1,nmd 
       bincor0=0.0
       do j=1,nday
       do i=1,fday
         if(nhead(i,j,n).eq.0) then
           read(11,'(1A)') string(1:nchar(i,j,n))    !data missing 
           cor(i,j,n)=bad
           rms(i,j,n)=bad
           bias(i,j,n)=bad
           fmiss(i,j)=bad
         else
           read(11,*)points(i,j),(vsdb(k,i,j),k=1,ns)
           if(points(i,j).eq.0) then
             cor(i,j,n)=bad
             rms(i,j,n)=bad
             bias(i,j,n)=bad
             fmiss(i,j)=bad
           else
             cor(i,j,n)=vsdb(5,i,j)/sqrt(vsdb(6,i,j)*vsdb(7,i,j))
            if(cor(i,j,n).ge.-0.999 .and. cor(i,j,n).le.1.0) then
             rms(i,j,n)=sqrt(max(0.0d0,vsdb(6,i,j)+vsdb(7,i,j)-2*vsdb(5,i,j)))
             bias(i,j,n)=sqrt(vsdb(6,i,j))-sqrt(vsdb(7,i,j))
             num(i,n)=num(i,n)+1
             mcor(i,n)=mcor(i,n)+cor(i,j,n)
             mrms(i,n)=mrms(i,n)+rms(i,j,n)
             mbias(i,n)=mbias(i,n)+bias(i,j,n)
             do k=1,bin
              if(cor(i,j,n).gt.binbnd(k).and.cor(i,j,n).le.binbnd(k+1)) bincor(i,k,n)=bincor(i,k,n)+1.0
             enddo
             do k=1,bin0
              if(cor(i,j,n).gt.binbnd0(k).and.cor(i,j,n).le.binbnd0(k+1)) bincor0(i,k)=bincor0(i,k)+1.0
             enddo
            else
             cor(i,j,n)=bad
             rms(i,j,n)=bad
             bias(i,j,n)=bad
             fmiss(i,j)=bad
            endif
           endif
         endif
       enddo
       enddo

! mean scores in ndays (number of valid cases for each model may differ), and normalied bins
       do i=1,fday
        if(num(i,n).gt.0) then
         mcor(i,n)=mcor(i,n)/num(i,n)
         mrms(i,n)=mrms(i,n)/num(i,n)
         mbias(i,n)=mbias(i,n)/num(i,n)
         bincor(i,:,n)=bincor(i,:,n)/num(i,n)
         bincor0(i,:)=bincor0(i,:)/num(i,n)
        else
         mcor(i,n)=bad
         mrms(i,n)=bad
         mbias(i,n)=bad
         bincor(i,:,n)=bad
         bincor0(i,:)=bad
         num(i,n)=bad
        endif
       enddo
! use maximum 20 bins for frequency
       if(nday.gt.bin0) then
        do i=1,fday
        do j=1,bin0
          bincor(i,j,n)=bincor0(i,j)
        enddo
        do j=bin0+1,nday
          bincor(i,j,n)=0
        enddo
        enddo
       endif
 100  continue


!--derive mean scores, mask out missing cases from all models 
!  to force all models to have the same sample size.
      if(maskmiss .gt. 0) then
       num=0; mcor=0; mrms=0; mbias=0
       do 200 n=1,nmd 
       do 200 i=1,fday
        do j=1,nday
         if(fmiss(i,j).ne.bad) then
          num(i,n)=num(i,n)+1
          mcor(i,n)=mcor(i,n)+cor(i,j,n)
          mrms(i,n)=mrms(i,n)+rms(i,j,n)
          mbias(i,n)=mbias(i,n)+bias(i,j,n)
         else
          cor(i,j,n)=bad
          rms(i,j,n)=bad
          bias(i,j,n)=bad
         endif
        enddo
        if(num(i,n).gt.0) then
          mcor(i,n)=mcor(i,n)/num(i,n)
          mrms(i,n)=mrms(i,n)/num(i,n)
          mbias(i,n)=mbias(i,n)/num(i,n)
        else
          mcor(i,n)=bad
          mrms(i,n)=bad
          mbias(i,n)=bad
          num(i,n)=bad
        endif
 200   continue
      endif


! write output

       do j=1,nday
         write(20) ((cor(i,j,n),n=1,nmd),i=1,fday)
         write(20) ((rms(i,j,n),n=1,nmd),i=1,fday)
         write(20) ((bias(i,j,n),n=1,nmd),i=1,fday)
         write(20) ((bincor(i,j,n),n=1,nmd),i=1,fday)
       enddo

! save mean scores as the nday+1 record in time
       write(20) ((mcor(i,n),n=1,nmd),i=1,fday)
       write(20) ((mrms(i,n),n=1,nmd),i=1,fday)
       write(20) ((mbias(i,n),n=1,nmd),i=1,fday)
       write(20) ((num(i,n),n=1,nmd),i=1,fday)   !note: num of records instead of bincor

       do n=1,nmd
        write(13,123) $yyyymm, trim(mdname(n)),"_cor", (mcor(i,n),i=1,fday)                               
        write(14,123) $yyyymm, trim(mdname(n)),"_rms", (mrms(i,n),i=1,fday)                               
        write(15,123) $yyyymm, trim(mdname(n)),"_bia", (mbias(i,n),i=1,fday)                               
       enddo
 123   format(i10,2x,A,A,${nfcst}f10.3)
       close (10)
       close (11)
       close (20)

      end
EOF

$FC $FFLAG -o convert.x convert.f
./convert.x
if [ $? -ne 0 ]; then
  echo "convert.x exec error, exit "
  exit 8
fi

meantxt=${vnam1}_${lev}_${reg1}_${yyyymm}
mv fort.13 meancor_${meantxt}.txt
mv fort.14 meanrms_${meantxt}.txt
mv fort.15 meanbias_${meantxt}.txt


#------------------------------------------------------------
# create GrADS control file
#------------------------------------------------------------
ndaysp1=`expr $ndays + 1 `
YYYY=`echo $sdate | cut -c 1-4`
MM=`echo $sdate | cut -c 5-6`
DD=`echo $sdate | cut -c 7-8`

set -A MONCHAR Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
MMM1=`expr $MM - 1 `
MON=${MONCHAR[$MMM1]}

cat >${outname}.ctl <<EOF1
dset ^${outname}.bin  
undef -99.9   
options big_endian sequential 
title scores
xdef   $nmdcyc linear 1  1                
ydef    $nfcst linear 0 $fhout
zdef     1 levels  0
tdef $ndaysp1 Linear $DD$MON$YYYY 1dy
vars    4
cor     0 0  correlation
rms     0 0  rms errror 
bias    0 0  mean bias   
bincor  0 0  frequency distribution of AC
endvars

EOF1

exit
