#!/bin/ksh
set -x

#------------------------------------------------------------
#--for scalar variable correlation and rms on multiple layer  
#------------------------------------------------------------
#  A=sum[(f-c)*(o-c)], B=sum[(f-c)**2], C=sum[(o-c)**2]
#  cor=A/sqrt(B*C), rms=sqrt(B+C-2*A)
#-------------------------------------------------------


export exedir=${exedir:-/stmp/$LOGNAME/g2oair/plot/T}
if [ ! -s $exedir ]; then mkdir -p $exedir; fi
cd $exedir

export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data/grid2obs}
export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export FC=${FC:-/usrx/local/intel/composer_xe_2011_sp1.11.339/bin/intel64/ifort}  ;#compiler
export FFLAG=${FFLAG:-"-O2 -convert big_endian -FR"}                              ;#compiler options

export obtype=${1:-ADPUPA}     ;#ADPUPA or ANYAIR
export vnam=${2:-T}            ;#verification variable: e.g. T,RH,Q,VWND
export regname=${3:-GGLB}
export reglist=${4:-"GGLB"}
export levlist=${5:-"P1000 P925 P850 P700 P500 P400 P300 P250 P200 P150 P100 P50 P20 P10"}
nreg=`echo $reglist | wc -w`                       
nlev=`echo $levlist | wc -w`                       

## verification ending date and number of days back 
export edate=${6:-20110831}
export ndays=${7:-5}
       nhours=`expr $ndays \* 24 - 24`
       tmp=`$ndate -$nhours ${edate}00 `
       sdate=`echo $tmp | cut -c 1-8`

## forecast cycle to be vefified: 00Z, 06Z, 12Z, 18Z
export cyc=${8:-00}

## forecast length in hours
export vlength=${9:-168}

## forecast output frequency in hours
export fhout=${10:-6}
       fnum=$((vlength/fhout))  ;#number of fcsts up to (vlength-fhout) hours
       #-- specify verification hours in a day
       export vhnum=$((24/fhout))
       if [ $fhout -eq 12 ]; then
        export vhlist="00 12"
       elif [ $fhout -eq 6 ]; then
        export vhlist="00 06 12 18"
       elif [ $fhout -eq 3 ]; then
        export vhlist="00 03 06 09 12 15 18 21"
       elif [ $fhout -eq 1 ]; then
        export vhlist="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23"
       else
        echo " fhout=$fhout hours is not supported, exit"
        exit
       fi

## create output name 
outname1=${vnam}_${regname}_${cyc}Z${sdate}${edate}
outname=${11:-$outname1}

## remove missing data from all models to unify sample size, 0-->NO, 1-->Yes
maskmiss=${12:-"1"}

## model names and number of models
export mdlist=${13:-"gfs"}
nmd=`echo $mdlist | wc -w`                       

set -A mdname none $mdlist
rm modelname.txt
>modelname.txt
n=1
while [ $n -le $nmd ]; do
 echo "${mdname[n]}" >>modelname.txt
 n=`expr $n + 1 `
done

#--------------------------------------------------
# search data
#--------------------------------------------------
if [ -s ${outname}.txt ]; then rm ${outname}.txt ;fi
if [ -s ${outname}.bin ]; then rm ${outname}.bin ;fi
if [ -s ${outname}.ctl ]; then rm ${outname}.ctl ;fi

for model in $mdlist; do
 datadir=${vsdb_data}/${cyc}Z/${model}
 mdl=`echo $model |tr "[a-z]" "[A-Z]" `
 cdate=$sdate
while [ $cdate -le $edate ]; do
 vsdbname=${datadir}/${model}_air_${cdate}.vsdb
 fhour=00           
 while [ $fhour -lt $vlength ]; do
  for lev in $levlist ; do
   for reg in $reglist; do
    levid="$lev "
    if [ $obtype = ANYAIR ]; then levid="${lev}-" ;fi
    mycheck=$( grep ${mdl} $vsdbname |grep " $fhour $cdate" |grep "$obtype $reg SL1L2 $vnam $levid" )
    if [ $? -ne 0 ]; then
     echo "missing" >>$outname.txt
    else
      grep ${mdl} $vsdbname |grep " $fhour $cdate" |grep "$obtype $reg SL1L2 $vnam $levid" |cat >>$outname.txt
    fi
   done
  done
 fhour=` expr $fhour + $fhout `
 if [ $fhour -lt 10 ]; then fhour=0$fhour ; fi
 done  ;#end fhour

cdate=`echo $($ndate +24 ${cdate}00) | cut -c 1-8`
done   ;#end of cdae
done   ;#end of model


#------------------------------------------------------------
# compute skill scores (i.e., anomaly correlations) and rms errors
# save output in binary format for GrADS 
#------------------------------------------------------------
rm convert.f convert.x tmp.txt
yyyymm=`echo $edate | cut -c 1-6`

cat >convert.f <<EOF
!
! read data from vsdb database, compute anomaly correlation
! rms and bias, write out in binary format for graphic display.
! please note even though "obs" and "points" might be the same for
! all models/experiments, they are still saved for each model/exp
! for universal applications. 
       integer, parameter :: nday=${ndays}, fnum=${fnum}, nreg=${nreg}
       integer, parameter :: nmd=${nmd}, nlev=${nlev}
       real*4             :: points0(nreg,nlev,fnum,nday)
       real*8             :: vsdb0(5,nreg,nlev,fnum,nday)
       real*4             :: points(nlev,fnum,nday,nmd)
       real*8             :: vsdb(5,nlev,fnum,nday)
       real*4             :: fcst(nlev,fnum,nday,nmd), obs(nlev,fnum,nday,nmd)
       real*4             :: cor(nlev,fnum,nday,nmd), rms(nlev,fnum,nday,nmd), bias(nlev,fnum,nday,nmd)   
       real*4             :: num(nlev,fnum,nmd), mcor(nlev,fnum,nmd), mrms(nlev,fnum,nmd), mbias(nlev,fnum,nmd)   
       real*4             :: mfcst(nlev,fnum,nmd), mobs(nlev,fnum,nmd),mpoints(nlev,fnum,nmd)
       real*4             :: fmiss(nlev,fnum,nday)    
       integer          :: nchar(nreg,nlev,fnum,nday,nmd),nhead(nreg,nlev,fnum,nday,nmd)
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


       rewind (10)
! find length of character header
       do n=1,nmd
       do j=1,nday
       do i=1,fnum
       do l=1,nlev 
       do m=1,nreg 
         read(10,'(1A)') string
         nchar(m,l,i,j,n)=len_trim(string)
         nhead(m,l,i,j,n)=index(string,substring)  !find character header length before "="
         write(11,'(A)') string(nhead(m,l,i,j,n)+1:nchar(m,l,i,j,n))
!         write(12,*) trim(string), nchar(m,l,i,j,n), nhead(m,l,i,j,n) 
       enddo
       enddo
       enddo
       enddo
       enddo

! read data
       rewind (11)
       num=0; fmiss=0.0
       mcor=0; mrms=0
       mfcst=0; mobs=0
       points=0; mpoints=0       

      do 100 n=1,nmd 
       vsdb=0.0
       do j=1,nday
       do i=1,fnum
       do l=1,nlev

! accumulate over all sub-regions
           do m=1,nreg
            if(nhead(m,l,i,j,n).eq.0) then
              read(11,'(1A)') string(1:nchar(m,l,i,j,n))    !data missing 
            else
              read(11,*)points0(m,l,i,j),(vsdb0(k,m,l,i,j),k=1,5)
              if (points0(m,l,i,j).ne.0) then
               points(l,i,j,n)=points(l,i,j,n)+points0(m,l,i,j)     !partial sum
               do k=1,5
                vsdb(k,l,i,j)=vsdb(k,l,i,j)+points0(m,l,i,j)*vsdb0(k,m,l,i,j)   !partial sum
               enddo
              endif
            endif
           enddo

           if(points(l,i,j,n).eq.0) then
             fcst(l,i,j,n)=bad
             cor(l,i,j,n)=bad
             rms(l,i,j,n)=bad
             bias(l,i,j,n)=bad
             fmiss(l,i,j)=bad
             obs(l,i,j,n)=bad
           else
             do k=1,5
               vsdb(k,l,i,j)=vsdb(k,l,i,j)/points(l,i,j,n)
             enddo
             fcst(l,i,j,n)=vsdb(1,l,i,j)
             obs(l,i,j,n)=vsdb(2,l,i,j)
             cor(l,i,j,n)=(vsdb(3,l,i,j)-vsdb(1,l,i,j)*vsdb(2,l,i,j))/  &
                      sqrt((vsdb(4,l,i,j)-vsdb(1,l,i,j)**2)*(vsdb(5,l,i,j)-vsdb(2,l,i,j)**2))
             rms(l,i,j,n)=sqrt(max(0.0d0,vsdb(4,l,i,j)+vsdb(5,l,i,j)-2*vsdb(3,l,i,j)))
             bias(l,i,j,n)=vsdb(1,l,i,j)-vsdb(2,l,i,j)
             num(l,i,n)=num(l,i,n)+1
             mfcst(l,i,n)=mfcst(l,i,n)+fcst(l,i,j,n)
             mobs(l,i,n)=mobs(l,i,n)+obs(l,i,j,n)
             mcor(l,i,n)=mcor(l,i,n)+cor(l,i,j,n)
             mrms(l,i,n)=mrms(l,i,n)+rms(l,i,j,n)
             mbias(l,i,n)=mbias(l,i,n)+bias(l,i,j,n)
             mpoints(l,i,n)=mpoints(l,i,n)+points(l,i,j,n)
           endif
       enddo
       enddo
       enddo

! mean scores in ndays (number of valid cases for each model may differ)
       do i=1,fnum
       do l=1,nlev
        if(num(l,i,n).gt.0) then
         mfcst(l,i,n)=mfcst(l,i,n)/num(l,i,n)
         mobs(l,i,n)=mobs(l,i,n)/num(l,i,n)
         mcor(l,i,n)=mcor(l,i,n)/num(l,i,n)
         mrms(l,i,n)=mrms(l,i,n)/num(l,i,n)
         mbias(l,i,n)=mbias(l,i,n)/num(l,i,n)
         mpoints(l,i,n)=mpoints(l,i,n)/num(l,i,n)
        else
         mfcst(l,i,n)=bad
         mobs(l,i,n)=bad
         mcor(l,i,n)=bad
         mrms(l,i,n)=bad
         mbias(l,i,n)=bad
         mpoints(l,i,n)=bad
         num(l,i,n)=bad
        endif
       enddo
       enddo
 100  continue


!--derive mean scores, mask out missing cases from all models 
!  to force all models to have the same sample size.
      if(maskmiss .gt. 0) then
       num=0; mfcst=0; mobs=0; mcor=0; mrms=0; mbias=0; mpoints=0
       do 200 n=1,nmd 
       do 200 i=1,fnum
       do 200 l=1,nlev
        do j=1,nday
         if(fmiss(l,i,j).ne.bad) then
          num(l,i,n)=num(l,i,n)+1
          mfcst(l,i,n)=mfcst(l,i,n)+fcst(l,i,j,n)
          mobs(l,i,n)=mobs(l,i,n)+obs(l,i,j,n)
          mcor(l,i,n)=mcor(l,i,n)+cor(l,i,j,n)
          mrms(l,i,n)=mrms(l,i,n)+rms(l,i,j,n)
          mbias(l,i,n)=mbias(l,i,n)+bias(l,i,j,n)
          mpoints(l,i,n)=mpoints(l,i,n)+points(l,i,j,n)
         else
          fcst(l,i,j,n)=bad
          obs(l,i,j,n)=bad
          cor(l,i,j,n)=bad
          rms(l,i,j,n)=bad
          bias(l,i,j,n)=bad
          points(l,i,j,n)=bad
         endif
        enddo
        if(num(l,i,n).gt.0) then
          mfcst(l,i,n)=mfcst(l,i,n)/num(l,i,n)
          mobs(l,i,n)=mobs(l,i,n)/num(l,i,n)
          mcor(l,i,n)=mcor(l,i,n)/num(l,i,n)
          mrms(l,i,n)=mrms(l,i,n)/num(l,i,n)
          mbias(l,i,n)=mbias(l,i,n)/num(l,i,n)
          mpoints(l,i,n)=mpoints(l,i,n)/num(l,i,n)
        else
          mfcst(l,i,n)=bad
          mobs(l,i,n)=bad
          mcor(l,i,n)=bad
          mrms(l,i,n)=bad
          mbias(l,i,n)=bad
          mpoints(l,i,n)=bad
          num(l,i,n)=bad
        endif
 200   continue
      endif


! write output

       do j=1,nday
        do l=1,nlev
         write(20) ((fcst(l,i,j,n),n=1,nmd),i=1,fnum)
        enddo
        do l=1,nlev
         write(20) ((obs(l,i,j,n),n=1,nmd),i=1,fnum)
        enddo
        do l=1,nlev
         write(20) ((cor(l,i,j,n),n=1,nmd),i=1,fnum)
        enddo
        do l=1,nlev
         write(20) ((rms(l,i,j,n),n=1,nmd),i=1,fnum)
        enddo
        do l=1,nlev
         write(20) ((points(l,i,j,n),n=1,nmd),i=1,fnum)
        enddo
       enddo

! save mean scores as the nday+1 record in time
       do l=1,nlev
        write(20) ((mfcst(l,i,n),n=1,nmd),i=1,fnum)
       enddo
       do l=1,nlev
       write(20) ((mobs(l,i,n),n=1,nmd),i=1,fnum)
       enddo
       do l=1,nlev
       write(20) ((mcor(l,i,n),n=1,nmd),i=1,fnum)
       enddo
       do l=1,nlev
       write(20) ((mrms(l,i,n),n=1,nmd),i=1,fnum)
       enddo
       do l=1,nlev
       write(20) ((mpoints(l,i,n),n=1,nmd),i=1,fnum)
       enddo

       do l=1,nlev
       do n=1,nmd
        write(13,123) l, $yyyymm, trim(mdname(n)),"_cor", (mcor(l,i,n),i=1,fnum)                               
        write(14,123) l, $yyyymm, trim(mdname(n)),"_rms", (mrms(l,i,n),i=1,fnum)                               
        write(16,123) l, $yyyymm, trim(mdname(n)),"_fcst", (mfcst(l,i,n),i=1,fnum)                               
        write(17,123) l, $yyyymm, trim(mdname(n)),"_obs", (mobs(l,i,n),i=1,fnum)                               
        write(18,124) l, $yyyymm, trim(mdname(n)),"_points", (int(mpoints(l,i,n)),i=1,fnum)                               
       enddo
       enddo

 123   format(i3, i10,2x,A,A,60f10.3)
 124   format(i3, i10,2x,A,A,60i10)
       close (9)
       close (10)
       close (11)
       close (20)

      end
EOF

${FC} ${FFLAG}  -o convert.x convert.f
./convert.x
if [ $? -ne 0 ]; then
  echo "convert.x exec error, exit "
  exit 8
fi

meantxt=${vnam}_${regname}_${cyc}Z${yyyymm}
mv fort.13 meancor_${meantxt}.txt
mv fort.14 meanrms_${meantxt}.txt
mv fort.16 meanfcst_${meantxt}.txt
mv fort.17 meanobs_${meantxt}.txt
mv fort.18 meanpts_${meantxt}.txt


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
options sequential 
format big_endian
title scores
xdef   $nmd linear 1  1                
ydef   $fnum linear 0 $fhout
zdef    $nlev levels  `echo $levlist | sed "s?P??g"`
tdef $ndaysp1 Linear $DD$MON$YYYY 1dy
vars    5
fcst    $nlev 0  forecasts  
obs     $nlev 0  observations
cor     $nlev 0  correlation
rms     $nlev 0  rms errror 
pts     $nlev 0  number of observations
endvars

EOF1

exit
