#!/bin/ksh
#set -x

#------------------------------------------------------------
#--for getting vector wind correlation and rms on a single layer
#------------------------------------------------------------
#  A=sum[(uf-uc)*(uo-uc)+(vf-vc)*(vo-vc)]
#  B=sum[(uf-uc)**2+(vf-vc)**2], C=sum[(uo-uc)**2+(vo-vc)**2]
#  cor=A/sqrt(B*C), rms=sqrt(B+C-2*A)
#-------------------------------------------------------

export exedir=${exedir:-/stmp/$LOGNAME/g2osfc/plot/WIND}
if [ ! -s $exedir ]; then mkdir -p $exedir; fi
cd $exedir

export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data/grid2obs}
export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export FC=${FC:-/usrx/local/intel/composer_xe_2011_sp1.11.339/bin/intel64/ifort}  ;#compiler
export FFLAG=${FFLAG:-"-O2 -convert big_endian -FR"}                              ;#compiler options

## verification variable parameters: e.g. VWND 
export vnam=${1:-VWND}
export regname=${2:-west}
export reglist=${3:-"NWC SWC GRB NMT SMT SWD NPL SPL"}
export lev=${4:-SFC}
nreg=`echo $reglist | wc -w`                       

## verification ending date and number of days back 
export edate=${5:-20110831}
export ndays=${6:-5}
       nhours=`expr $ndays \* 24 - 24`
       tmp=`$ndate -$nhours ${edate}00 `
       sdate=`echo $tmp | cut -c 1-8`

## forecast cycle to be vefified: 00Z, 06Z, 12Z, 18Z
export cyc=${7:-00}

## forecast length in hours
export vlength=${8:-168}

## forecast output frequency in hours
export fhout=${9:-6}
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
outname1=${vnam}_${lev}_${regname}_${cyc}Z${sdate}${edate}
outname=${10:-$outname1}

## remove missing data from all models to unify sample size, 0-->NO, 1-->Yes
maskmiss=${11:-"1"}

## model names and number of models
export mdlist=${12:-"gfs"}
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
 vsdbname=${datadir}/${model}_sfc_${cdate}.vsdb
 fhour=00           
 while [ $fhour -lt $vlength ]; do
   for reg in $reglist; do
    mycheck=$( grep ${mdl} $vsdbname |grep " $fhour $cdate" |grep "ONLYSF G104/$reg VL1L2 $vnam $lev " )
    if [ $? -ne 0 ]; then
     echo "missing" >>$outname.txt
    else
      grep ${mdl} $vsdbname |grep " $fhour $cdate" |grep "ONLYSF G104/$reg VL1L2 $vnam $lev " |cat >>$outname.txt
    fi
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
       integer, parameter :: nmd=${nmd}
       integer, parameter :: ns=7
       real*4             :: points0(nreg,fnum,nday)
       real*8             :: vsdb0(nreg,ns,fnum,nday)
       real*4             :: points(fnum,nday,nmd)
       real*8             :: vsdb(ns,fnum,nday)
       real*4             :: fcst(fnum,nday,nmd), obs(fnum,nday,nmd)
       real*4             :: cor(fnum,nday,nmd), rms(fnum,nday,nmd), bias(fnum,nday,nmd)   
       real*4             :: num(fnum,nmd), mcor(fnum,nmd), mrms(fnum,nmd), mbias(fnum,nmd)   
       real*4             :: mfcst(fnum,nmd), mobs(fnum,nmd),mpoints(fnum,nmd)
       real*4             :: fmiss(fnum,nday)    
       integer          :: nchar(nreg,fnum,nday,nmd),nhead(nreg,fnum,nday,nmd)
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
       do m=1,nreg 
         read(10,'(1A)') string
         nchar(m,i,j,n)=len_trim(string)
         nhead(m,i,j,n)=index(string,substring)  !find character header length before "="
         write(11,'(A)') string(nhead(m,i,j,n)+1:nchar(m,i,j,n))
!         write(12,*) trim(string), nchar(m,i,j,n), nhead(m,i,j,n) 
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

! accumulate over all sub-regions
           do m=1,nreg
            if(nhead(m,i,j,n).eq.0) then
              read(11,'(1A)') string(1:nchar(m,i,j,n))    !data missing 
            else
              read(11,*)points0(m,i,j),(vsdb0(m,k,i,j),k=1,ns)
              if (points0(m,i,j).ne.0) then
               points(i,j,n)=points(i,j,n)+points0(m,i,j)     !partial sum
               do k=1,ns
                vsdb(k,i,j)=vsdb(k,i,j)+points0(m,i,j)*vsdb0(m,k,i,j)   !partial sum
               enddo
              endif
            endif
           enddo

           if(points(i,j,n).eq.0) then
             fcst(i,j,n)=bad
             cor(i,j,n)=bad
             rms(i,j,n)=bad
             bias(i,j,n)=bad
             fmiss(i,j)=bad
             obs(i,j,n)=bad
           else
             do k=1,ns
               vsdb(k,i,j)=vsdb(k,i,j)/points(i,j,n)
             enddo
             fcst(i,j,n)=sqrt(vsdb(6,i,j))
             obs(i,j,n)=sqrt(vsdb(7,i,j))
             cor(i,j,n)=vsdb(5,i,j)/sqrt(vsdb(6,i,j)*vsdb(7,i,j))
             rms(i,j,n)=sqrt(max(0.0d0,vsdb(6,i,j)+vsdb(7,i,j)-2*vsdb(5,i,j)))
             bias(i,j,n)=sqrt(vsdb(6,i,j))-sqrt(vsdb(7,i,j))
             num(i,n)=num(i,n)+1
             mfcst(i,n)=mfcst(i,n)+fcst(i,j,n)
             mobs(i,n)=mobs(i,n)+obs(i,j,n)
             mcor(i,n)=mcor(i,n)+cor(i,j,n)
             mrms(i,n)=mrms(i,n)+rms(i,j,n)
             mbias(i,n)=mbias(i,n)+bias(i,j,n)
             mpoints(i,n)=mpoints(i,n)+points(i,j,n)
           endif
       enddo
       enddo

! mean scores in ndays (number of valid cases for each model may differ)
       do i=1,fnum
        if(num(i,n).gt.0) then
         mfcst(i,n)=mfcst(i,n)/num(i,n)
         mobs(i,n)=mobs(i,n)/num(i,n)
         mcor(i,n)=mcor(i,n)/num(i,n)
         mrms(i,n)=mrms(i,n)/num(i,n)
         mbias(i,n)=mbias(i,n)/num(i,n)
         mpoints(i,n)=mpoints(i,n)/num(i,n)
        else
         mfcst(i,n)=bad
         mobs(i,n)=bad
         mcor(i,n)=bad
         mrms(i,n)=bad
         mbias(i,n)=bad
         mpoints(i,n)=bad
         num(i,n)=bad
        endif
       enddo
 100  continue


!--derive mean scores, mask out missing cases from all models 
!  to force all models to have the same sample size.
      if(maskmiss .gt. 0) then
       num=0; mfcst=0; mobs=0; mcor=0; mrms=0; mbias=0; mpoints=0
       do 200 n=1,nmd 
       do 200 i=1,fnum
        do j=1,nday
         if(fmiss(i,j).ne.bad) then
          num(i,n)=num(i,n)+1
          mfcst(i,n)=mfcst(i,n)+fcst(i,j,n)
          mobs(i,n)=mobs(i,n)+obs(i,j,n)
          mcor(i,n)=mcor(i,n)+cor(i,j,n)
          mrms(i,n)=mrms(i,n)+rms(i,j,n)
          mbias(i,n)=mbias(i,n)+bias(i,j,n)
          mpoints(i,n)=mpoints(i,n)+points(i,j,n)
         else
          fcst(i,j,n)=bad
          obs(i,j,n)=bad
          cor(i,j,n)=bad
          rms(i,j,n)=bad
          bias(i,j,n)=bad
          points(i,j,n)=bad
         endif
        enddo
        if(num(i,n).gt.0) then
          mfcst(i,n)=mfcst(i,n)/num(i,n)
          mobs(i,n)=mobs(i,n)/num(i,n)
          mcor(i,n)=mcor(i,n)/num(i,n)
          mrms(i,n)=mrms(i,n)/num(i,n)
          mbias(i,n)=mbias(i,n)/num(i,n)
          mpoints(i,n)=mpoints(i,n)/num(i,n)
        else
          mfcst(i,n)=bad
          mobs(i,n)=bad
          mcor(i,n)=bad
          mrms(i,n)=bad
          mbias(i,n)=bad
          mpoints(i,n)=bad
          num(i,n)=bad
        endif
 200   continue
      endif


! write output

       do j=1,nday
         write(20) ((fcst(i,j,n),n=1,nmd),i=1,fnum)
         write(20) ((obs(i,j,n),n=1,nmd),i=1,fnum)
         write(20) ((cor(i,j,n),n=1,nmd),i=1,fnum)
         write(20) ((rms(i,j,n),n=1,nmd),i=1,fnum)
         write(20) ((points(i,j,n),n=1,nmd),i=1,fnum)
       enddo

! save mean scores as the nday+1 record in time
       write(20) ((mfcst(i,n),n=1,nmd),i=1,fnum)
       write(20) ((mobs(i,n),n=1,nmd),i=1,fnum)
       write(20) ((mcor(i,n),n=1,nmd),i=1,fnum)
       write(20) ((mrms(i,n),n=1,nmd),i=1,fnum)
       write(20) ((mpoints(i,n),n=1,nmd),i=1,fnum)

       do n=1,nmd
        write(13,123) $yyyymm, trim(mdname(n)),"_cor", (mcor(i,n),i=1,fnum)                               
        write(14,123) $yyyymm, trim(mdname(n)),"_rms", (mrms(i,n),i=1,fnum)                               
        write(16,123) $yyyymm, trim(mdname(n)),"_fcst", (mfcst(i,n),i=1,fnum)                               
        write(17,123) $yyyymm, trim(mdname(n)),"_obs", (mobs(i,n),i=1,fnum)                               
        write(18,124) $yyyymm, trim(mdname(n)),"_points", (int(mpoints(i,n)),i=1,fnum)                               
       enddo
 123   format(i10,2x,A,A,60f10.3)
 124   format(i10,2x,A,A,60i10)
       close (9)
       close (10)
       close (11)
       close (20)

      end
EOF

${FC} ${FFLAG} -o convert.x convert.f
./convert.x
if [ $? -ne 0 ]; then
  echo "convert.x exec error, exit "
  exit 8
fi

meantxt=${vnam}_${lev}_${regname}_${cyc}Z${yyyymm}
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
zdef   1 levels  0
tdef $ndaysp1 Linear $DD$MON$YYYY 1dy
vars    5
fcst    0 0  forecasts  
obs     0 0  observations
cor     0 0  correlation
rms     0 0  rms errror 
pts     0 0  number of observations
endvars

EOF1

exit
