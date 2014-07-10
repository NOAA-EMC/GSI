#!/bin/ksh
#set -x

#-----------------------------------------------------------------------------
#--compute scalar variable pattern correlation, bias, mean squared error (MSE), 
#  variances, MSE by mean difference, MSE by pattern variation,
#  and MSE Skill Score (Murphy, MWR 1988). Write out RMSE instead of MSE.
#-----------------------------------------------------------------------------
# VSDB Record:   
#    X1=MEAN[f], X2=MEAN[a], X3=MEAN[f*a], X4=MEAN[f*f], X5=MEAN[a*a]
#      where f is forecast and a is analysis/observation, MEAN is domain mean.
#
#  Pattern correlation : R=( X3 - X1*X2 ) / sqrt{var(f)*var(a)}
#    where var(f)={ X4 - X1*X1 } 
#          var(a)={ X5 - X2*X2 } 
#  Mean biases:    bias= ( X1 - X2 )
#  RMSE:           rms= sqrt( X4 + X5 - 2*X3 )
#  MSE:            mse= ( X4 + X5 - 2*X3 )
#  MSE by mean difference: e_m=(X1-X2)**2
#  MSE by pattern variation: 
#     e_p={ var(f) + var(a) - 2*sqrt[var(f)*var(a)]*R }
#        =[ mse - e_m ]
#  Murphy's MSE Skill Score: 
#     msess=1-MSE/var(a)
#          =2*R*sqrt[var(f)/var(a)]-[var(f)/var(a)]-e_m/var(a)
#-------------------------------------------------------


export exedir=${exedir:-/stmp/$LOGNAME/vsdb_stats}
if [ ! -s $exedir ]; then mkdir -p $exedir; fi
cd $exedir

export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data06}
export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export FC=${FC:-xlf90}
export FFLAG=${FFLAG:-" "}


## verification type: pres
export vtype=${1:-pres}

## verification variable parameters: e.g. HGT G2/NHX 
export vnam=${2:-HGT}
export reg=${3:-G2/NHX}
export levlist=${4:-"P1000 P925 P850 P700 P500 P400 P300 P250 P200 P150 P100 P50 P20 P10"}
       nlev=`echo $levlist |wc -w`
       rm fort.99; echo $levlist | sed "s?P??g"  > fort.99

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
outname1=${vnam1}_${reg1}_${sdate}${edate}
outname=${10:-$outname1}

## remove missing data from all models to unify sample size, 0-->NO, 1-->Yes
maskmiss=${maskmiss:-${11:-"1"}}

## model names and number of models
export mdlist=${mdlist:-${12:-"gfs fim"}}
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
    for lev1 in $levlist ; do
      string=" $mdl $fhour ${cdate}${vhr} $mdl $reg SL1L2 $vnam $lev1 "
      mycheck=$( grep "$string"  $vsdbname )
      if [ $? -ne 0 ]; then      
        echo "missing" >>$outname.txt
      else
         grep "$string"  $vsdbname | cat >>$outname.txt
      fi
    done
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
# compute scores and save output in binary format for GrADS 
#------------------------------------------------------------
rm convert.f convert.x tmp.txt
yyyymm=`echo $edate | cut -c 1-6`

cat >convert.f <<EOF
!
! read data from vsdb database, compute anomaly correlation, mean-saured-error MSE, bias, 
! squared error of mean bias (e_m) and squared error of pattern variation (e_p),
! and variance of forecast (var_f) and variance of analysis (var_a)
! write out in binary format for graphic display
       integer, parameter :: nlev=${nlev}
       integer, parameter :: nday=${ndays}, fday=${nfcst}, bin=nday
       integer, parameter :: nmd=${nmdcyc}, ns=5
       integer, parameter :: bin0=20
       real*4             :: points(nlev,fday,nday)
       real*8             :: vsdb(ns,nlev,fday,nday)
       real*4             :: cor(nlev,fday,nday,nmd), rms(nlev,fday,nday,nmd)
       real*4             :: mse(nlev,fday,nday,nmd), bias(nlev,fday,nday,nmd)   
       real*4             :: e_m(nlev,fday,nday,nmd), e_p(nlev,fday,nday,nmd),msess(nlev,fday,nday,nmd)
       real*4             :: var_f(nlev,fday,nday,nmd), var_a(nlev,fday,nday,nmd), rvar(nlev,fday,nday,nmd)    
       real*4             :: num(nlev,fday,nmd), mcor(nlev,fday,nmd)
       real*4             :: mrms(nlev,fday,nmd), mmse(nlev,fday,nmd), mbias(nlev,fday,nmd)   
       real*4             :: me_m(nlev,fday,nmd),me_p(nlev,fday,nmd),mvar_f(nlev,fday,nmd)
       real*4             :: mvar_a(nlev,fday,nmd),mrvar(nlev,fday,nmd),mmsess(nlev,fday,nmd)
       real*4             :: bincor(nlev,fday,bin,nmd), binbnd(bin+1)
       real*4             :: bincor0(nlev,fday,bin0), binbnd0(bin0+1)
       real*4             :: fmiss(nlev,fday,nday)
       integer            :: plev(nlev)
       integer            :: nchar(nlev,fday,nday,nmd),nhead(nlev,fday,nday,nmd)
       character (1000)   :: string
       character(1)       :: substring
       character*20       :: mdname(nmd)
       data bad/-99.9/,substring/"="/
       data maskmiss /${maskmiss}/

       open(9,file="modelname.txt",form="formatted",status="old")
       open(10,file="${outname}.txt",form="formatted",status="old")
       open(11,file="tmp.txt",form="formatted",status="new")
       open(20,file="${outname}.bin",form="unformatted",status="new")

       do m=1,nmd
        read(9,'(a)') mdname(m)
       enddo
       read(99,*)(plev(n),n=1,nlev)

! create bounds of bins for frequency distribution of anomaly correlations (0,1)
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
       do m=1,nmd
       do j=1,nday
       do i=1,fday
       do n=1,nlev
         read(10,'(1A)') string
         nchar(n,i,j,m)=len_trim(string)
         nhead(n,i,j,m)=index(string,substring)  !find character header length before "="
         write(11,'(a)') string(nhead(n,i,j,m)+1:nchar(n,i,j,m))
!        write(12,'(a)') trim(string), nchar(n,i,j,m), nhead(n,i,j,m) 
       enddo
       enddo
       enddo
       enddo

! read data
       rewind (11)
       num=0; mcor=0; mrms=0; mmse=0; mbias=0; fmiss=0.0; bincor=0.0
       me_m=0; me_p=0; mvar_f=0; mvar_a=0; mrvar=0; mmsess=0

      do 100 m=1,nmd
       bincor0=0.0
       do j=1,nday
       do i=1,fday
       do n=1,nlev
         if(nhead(n,i,j,m).eq.0) then
           read(11,'(1A)') string(1:nchar(n,i,j,m))    !data missing 
           cor(n,i,j,m)=bad
           rms(n,i,j,m)=bad
           mse(n,i,j,m)=bad
           bias(n,i,j,m)=bad
           e_m(n,i,j,m)=bad
           e_p(n,i,j,m)=bad
           msess(n,i,j,m)=bad
           var_f(n,i,j,m)=bad
           var_a(n,i,j,m)=bad
           rvar(n,i,j,m)=bad
           fmiss(n,i,j)=bad
         else
           read(11,*)points(n,i,j),(vsdb(k,n,i,j),k=1,ns)
           if(points(n,i,j).eq.0) then
             cor(n,i,j,m)=bad
             rms(n,i,j,m)=bad
             mse(n,i,j,m)=bad
             bias(n,i,j,m)=bad
             e_m(n,i,j,m)=bad
             e_p(n,i,j,m)=bad
             msess(n,i,j,m)=bad
             var_f(n,i,j,m)=bad
             var_a(n,i,j,m)=bad
             rvar(n,i,j,m)=bad
             fmiss(n,i,j)=bad
           else
             var_f(n,i,j,m)=max(0.0d0,vsdb(4,n,i,j)-vsdb(1,n,i,j)**2)
             var_a(n,i,j,m)=max(0.0d0,vsdb(5,n,i,j)-vsdb(2,n,i,j)**2)
             cor(n,i,j,m)=(vsdb(3,n,i,j)-vsdb(1,n,i,j)*vsdb(2,n,i,j))/  &
                        sqrt(var_f(n,i,j,m)*var_a(n,i,j,m))
             mse(n,i,j,m)=max(0.0d0,(vsdb(4,n,i,j)+vsdb(5,n,i,j)-2*vsdb(3,n,i,j)))
             rms(n,i,j,m)=sqrt(mse(n,i,j,m))                                            
             bias(n,i,j,m)=vsdb(1,n,i,j)-vsdb(2,n,i,j)
             e_m(n,i,j,m)=bias(n,i,j,m)*bias(n,i,j,m)                                    
             e_p(n,i,j,m)=max(0.0,mse(n,i,j,m)-e_m(n,i,j,m))
             if(var_a(n,i,j,m).ne.0) then 
               rvar(n,i,j,m)=var_f(n,i,j,m)/var_a(n,i,j,m)                              
               msess(n,i,j,m)=1.0-mse(n,i,j,m)/var_a(n,i,j,m)                            
             else
               rvar(n,i,j,m)=bad
               msess(n,i,j,m)=bad
             endif

             num(n,i,m)=num(n,i,m)+1
             mcor(n,i,m)=mcor(n,i,m)+cor(n,i,j,m)
             mrms(n,i,m)=mrms(n,i,m)+rms(n,i,j,m)
             mmse(n,i,m)=mmse(n,i,m)+mse(n,i,j,m)
             mbias(n,i,m)=mbias(n,i,m)+bias(n,i,j,m)
             me_m(n,i,m)=me_m(n,i,m)+e_m(n,i,j,m)
             me_p(n,i,m)=me_p(n,i,m)+e_p(n,i,j,m)
             mvar_f(n,i,m)=mvar_f(n,i,m)+var_f(n,i,j,m)
             mvar_a(n,i,m)=mvar_a(n,i,m)+var_a(n,i,j,m)

             do k=1,bin
              if(cor(n,i,j,m).gt.binbnd(k).and.cor(n,i,j,m).le.binbnd(k+1)) bincor(n,i,k,m)=bincor(n,i,k,m)+1.0
             enddo
             do k=1,bin0
              if(cor(n,i,j,m).gt.binbnd0(k).and.cor(n,i,j,m).le.binbnd0(k+1)) bincor0(n,i,k)=bincor0(n,i,k)+1.0
             enddo

           endif
         endif
       enddo
       enddo
       enddo

! mean scores in ndays, and normalized bins 
!
       do i=1,fday
       do n=1,nlev
        if(num(n,i,m).gt.0) then
         mcor(n,i,m)=mcor(n,i,m)/num(n,i,m)
         mrms(n,i,m)=mrms(n,i,m)/num(n,i,m)
         mmse(n,i,m)=mmse(n,i,m)/num(n,i,m)
         mbias(n,i,m)=mbias(n,i,m)/num(n,i,m)
         me_m(n,i,m)=me_m(n,i,m)/num(n,i,m)
         me_p(n,i,m)=me_p(n,i,m)/num(n,i,m)
         mvar_f(n,i,m)=mvar_f(n,i,m)/num(n,i,m)
         mvar_a(n,i,m)=mvar_a(n,i,m)/num(n,i,m)
         mrvar(n,i,m)=mvar_f(n,i,m)/mvar_a(n,i,m)
         mmsess(n,i,m)=1.0-mmse(n,i,m)/mvar_a(n,i,m)
         bincor(n,i,:,m)=bincor(n,i,:,m)/num(n,i,m)
         bincor0(n,i,:)=bincor0(n,i,:)/num(n,i,m)
        else
         mcor(n,i,m)=bad
         mrms(n,i,m)=bad
         mmse(n,i,m)=bad
         mbias(n,i,m)=bad
         me_m(n,i,m)=bad
         me_p(n,i,m)=bad
         mvar_f(n,i,m)=bad
         mvar_a(n,i,m)=bad
         mrvar(n,i,m)=bad
         mmsess(n,i,m)=bad
         bincor(n,i,:,m)=bad
         bincor0(n,i,:)=bad
         num(n,i,m)=bad
        endif
       enddo
       enddo

! use maximum 20 bins for frequency
       if(nday.gt.bin0) then
        do n=1,nlev
        do i=1,fday
         do j=1,bin0
          bincor(n,i,j,m)=bincor0(n,i,j)
         enddo
         do j=bin0+1,nday
           bincor(n,i,j,m)=0
         enddo
        enddo
        enddo
       endif

100   continue

!--derive mean scores, mask out missing cases from all models
!  to force all models to have the same sample size.
      if(maskmiss .gt. 0) then
       num=0; mcor=0; mrms=0; mmse=0; mbias=0
       me_m=0; me_p=0; mvar_f=0; mvar_a=0; mrvar=0; mmsess=0

       do 200 m=1,nmd
       do 200 i=1,fday
       do 200 n=1,nlev
        do j=1,nday
        if(fmiss(n,i,j).ne.bad) then
         num(n,i,m)=num(n,i,m)+1
         mcor(n,i,m)=mcor(n,i,m)+cor(n,i,j,m)
         mrms(n,i,m)=mrms(n,i,m)+rms(n,i,j,m)
         mmse(n,i,m)=mmse(n,i,m)+mse(n,i,j,m)
         mbias(n,i,m)=mbias(n,i,m)+bias(n,i,j,m)
         me_m(n,i,m)=me_m(n,i,m)+e_m(n,i,j,m)
         me_p(n,i,m)=me_p(n,i,m)+e_p(n,i,j,m)
         mvar_f(n,i,m)=mvar_f(n,i,m)+var_f(n,i,j,m)
         mvar_a(n,i,m)=mvar_a(n,i,m)+var_a(n,i,j,m)
        else
         cor(n,i,j,m)=bad
         rms(n,i,j,m)=bad
         mse(n,i,j,m)=bad
         bias(n,i,j,m)=bad
         e_m(n,i,j,m)=bad
         e_p(n,i,j,m)=bad
         msess(n,i,j,m)=bad
         var_f(n,i,j,m)=bad
         var_a(n,i,j,m)=bad
         rvar(n,i,j,m)=bad
        endif
        enddo

        if(num(n,i,m).gt.0) then
         mcor(n,i,m)=mcor(n,i,m)/num(n,i,m)
         mrms(n,i,m)=mrms(n,i,m)/num(n,i,m)
         mmse(n,i,m)=mmse(n,i,m)/num(n,i,m)
         mbias(n,i,m)=mbias(n,i,m)/num(n,i,m)
         me_m(n,i,m)=me_m(n,i,m)/num(n,i,m)
         me_p(n,i,m)=me_p(n,i,m)/num(n,i,m)
         mvar_f(n,i,m)=mvar_f(n,i,m)/num(n,i,m)
         mvar_a(n,i,m)=mvar_a(n,i,m)/num(n,i,m)
         mrvar(n,i,m)=mvar_f(n,i,m)/mvar_a(n,i,m)
         mmsess(n,i,m)=1.0-mmse(n,i,m)/mvar_a(n,i,m)
        else
         mcor(n,i,m)=bad
         mrms(n,i,m)=bad
         mmse(n,i,m)=bad
         mbias(n,i,m)=bad
         me_m(n,i,m)=bad
         me_p(n,i,m)=bad
         mvar_f(n,i,m)=bad
         mvar_a(n,i,m)=bad
         mrvar(n,i,m)=bad
         mmsess(n,i,m)=bad
         num(n,i,m)=bad
        endif
 200   continue
      endif


!
!write out correlation, bias, RMSE, ratio of standard deviaiton
!
       do j=1,nday
         do n=1,nlev
           write(20) ((cor(n,i,j,m),m=1,nmd),i=1,fday)
         enddo
         do n=1,nlev
           write(20) ((rms(n,i,j,m),m=1,nmd),i=1,fday)
         enddo
         do n=1,nlev
           write(20) ((bias(n,i,j,m),m=1,nmd),i=1,fday)
         enddo
         do n=1,nlev
           do i=1,fday
           do m=1,nmd 
            if (e_m(n,i,j,m).ne.bad) e_m(n,i,j,m)=sqrt(e_m(n,i,j,m))
           enddo
           enddo
           write(20) ((e_m(n,i,j,m),m=1,nmd),i=1,fday)
         enddo
         do n=1,nlev
           do i=1,fday
           do m=1,nmd 
            if (e_p(n,i,j,m).ne.bad) e_p(n,i,j,m)=sqrt(e_p(n,i,j,m))
           enddo
           enddo
           write(20) ((e_p(n,i,j,m),m=1,nmd),i=1,fday)
         enddo
         do n=1,nlev
           do i=1,fday
           do m=1,nmd 
            if (rvar(n,i,j,m).ne.bad) rvar(n,i,j,m)=sqrt(rvar(n,i,j,m))
           enddo
           enddo
           write(20) ((rvar(n,i,j,m),m=1,nmd),i=1,fday)
         enddo
         do n=1,nlev
           write(20) ((msess(n,i,j,m),m=1,nmd),i=1,fday)
         enddo
         do n=1,nlev
           write(20) ((bincor(n,i,j,m),m=1,nmd),i=1,fday)
         enddo
       enddo

! save mean scores as the nday+1 record in time
       do n=1,nlev
         write(20) ((mcor(n,i,m),m=1,nmd),i=1,fday)
       enddo
       do n=1,nlev
         write(20) ((mrms(n,i,m),m=1,nmd),i=1,fday)
       enddo
       do n=1,nlev
         write(20) ((mbias(n,i,m),m=1,nmd),i=1,fday)  
       enddo
       do n=1,nlev
         do i=1,fday
         do m=1,nmd
          if (me_m(n,i,m).ne.bad) me_m(n,i,m)=sqrt(me_m(n,i,m))  
         enddo
         enddo
         write(20) ((me_m(n,i,m),m=1,nmd),i=1,fday)  
       enddo
       do n=1,nlev
         do i=1,fday
         do m=1,nmd
          if (me_p(n,i,m).ne.bad) me_p(n,i,m)=sqrt(me_p(n,i,m))  
         enddo
         enddo
         write(20) ((me_p(n,i,m),m=1,nmd),i=1,fday)  
       enddo
       do n=1,nlev
         do i=1,fday
         do m=1,nmd
          if (mrvar(n,i,m).ne.bad) mrvar(n,i,m)=sqrt(mrvar(n,i,m))  
         enddo
         enddo
         write(20) ((mrvar(n,i,m),m=1,nmd),i=1,fday)  
       enddo
       do n=1,nlev
         write(20) ((mmsess(n,i,m),m=1,nmd),i=1,fday)  
       enddo
       do n=1,nlev
         write(20) ((num(n,i,m),m=1,nmd),i=1,fday)    !note: num of records instead of bincor
       enddo

       do m=1,nmd
        do n=1,nlev
          write(13,123) $yyyymm, plev(n), trim(mdname(m)),"_cor", (mcor(n,i,m),i=1,fday) 
        enddo
        do n=1,nlev
          write(14,123) $yyyymm, plev(n), trim(mdname(m)),"_rms", (mrms(n,i,m),i=1,fday)  
        enddo
        do n=1,nlev
          write(15,123) $yyyymm, plev(n), trim(mdname(m)),"_bia", (mbias(n,i,m),i=1,fday) 
        enddo
       enddo
 123   format(i10,i10,"MB ", 2x,A,A, ${nfcst}f10.3)

       close (9)
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

meantxt=${vnam1}_${reg1}_${yyyymm}
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
xdef    $nmdcyc linear 1  1
ydef    $nfcst linear 0 $fhout
zdef    $nlev levels  `echo $levlist | sed "s?P??g"`
tdef    $ndaysp1 Linear $DD$MON$YYYY 1dy
vars    8
pcor    $nlev 0 correlation
rms     $nlev 0  root-mean squared error (RMSE)
bias    $nlev 0  mean bias   
emd     $nlev 0  RMSE by mean difference
epv     $nlev 0  RMSE by pattern variation
rsd     $nlev 0  ratio of standard deviation between forecast and analysis
msess   $nlev 0  murphy's mean-squared-error skill score                    
bincor  $nlev 0  frequency distribution of AC
endvars

EOF1

exit
