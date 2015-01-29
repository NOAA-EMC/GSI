#!/bin/sh
#set -x

#----------------------------------------------------------------
#--for getting vector wind correlation and rms on a single layer
#----------------------------------------------------------------
#  A=sum[(uf-uc)*(uo-uc)+(vf-vc)*(vo-vc)]
#  B=sum[(uf-uc)**2+(vf-vc)**2], C=sum[(uo-uc)**2+(vo-vc)**2]
#  cor=A/sqrt(B*C), rms=sqrt(B+C-2*A)

export exedir=${exedir:-/stmp/$LOGNAME/vsdb_stats}
if [ ! -s $exedir ]; then mkdir -p $exedir; fi
cd $exedir

export vsdb_data=${vsdb_data:-/climate/save/wx24fy/VRFY/vsdb_data}

## verification type: anom, pres, sfc
export vtype=${1:-anom}

## model name
export model=${2:-gfs}

## verification variable parameters: e.g. WIND                         
export vnam=${3:-WIND}
export reg=${4:-G2/TRO}
export lev=${5:-P850}

## verification ending date and number of days back 
export edate=${6:-20060930}
export ndays=${7:-30}
       nhours=`expr $ndays \* 24 - 24`
       tmp=`${ndate_dir}/ndate  -$nhours ${edate}00 `
       sdate=`echo $tmp | cut -c 1-8`

## forecast cycle to be vefified: 00Z, 06Z, 12Z, 18Z
export cyc=${8:-00}

## forecast length in days, excluding 00Z forecasts (gfs default=16, 384 hours) 
export fdays=${9:-16}
       fdaysp1=`expr $fdays + 1 `

## create output name (first remove / from parameter names)
vnam1=`echo $vnam | sed "s?/??g" |sed "s?_WV1?WV?g"`  
reg1=`echo $reg | sed "s?/??g"`
outname1=${vnam1}_${lev}_${reg1}_${cyc}Z${sdate}${edate}_${model}
outname=${10:-$outname1}

#--------------------------------------------------
# search data
#--------------------------------------------------
if [ -s ${outname}.txt ]; then rm ${outname}.txt ;fi
if [ -s ${outname}.bin ]; then rm ${outname}.bin ;fi
if [ -s ${outname}.ctl ]; then rm ${outname}.ctl ;fi

datadir=${vsdb_data}/${vtype}/${cyc}Z/${model}
mdl=`echo $model |tr "[a-z]" "[A-Z]" `

cdate=$sdate
while [ $cdate -le $edate ]; do

  dropflag=0
  for dropcase in $dropdays ; do
   echo $dropcase
   if [ $cdate  -eq $dropcase ]; then dropflag=1 ; fi
  done

  vsdbname=${datadir}/${model}_${cdate}.vsdb
  vhour=00
  endhour=` expr $fdays \* 24 `
  while [ $vhour -le $endhour ]; do
    grep " $vnam " $vsdbname |grep " $lev " |grep " $mdl $vhour " |grep " $reg "  > outtmp
    fsize=`ls -l outtmp | awk '{print $5}'`
    if [ $fsize -le 1 -o $dropflag -eq 1 ]; then
      echo "missing" >>$outname.txt
    else
      cat outtmp >>$outname.txt
    fi 
    vhour=` expr $vhour + 24 `
  done

xdate=`${ndate_dir}/ndate   +24 ${cdate}00`
cdate=`echo ${xdate} | cut -c 1-8`
done


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
       integer, parameter :: ns=7
       integer, parameter :: nday=${ndays}, fday=${fdaysp1}, bin=nday
       real*4             :: points(fday,nday)
       real*8             :: vsdb(ns,fday,nday)
       real*4             :: cor(fday,nday), rms(fday,nday), bias(fday,nday)   
       real*4             :: num(fday), mcor(fday), mrms(fday), mbias(fday)   
       real*4             :: bincor(fday,bin), binbnd(bin+1)
       integer          :: nchar(fday,nday),nhead(fday,nday)
       character (1000) :: string
       character(1)     :: substring
       data bad/-99.9/,substring/"="/

       open(10,file="${outname}.txt",form="formatted",status="old")
       open(11,file="tmp.txt",form="formatted",status="new")
       open(20,file="${outname}.bin",form="unformatted",status="new")
       rewind (10)

! create bounds of bins for frequency distribution of anomaly correlations (0,1)
! for recording convenience, the number of bins are set to be the verification days 
       delcor=1.0/bin
       do i=1,bin+1
        binbnd(i)=(i-1)*delcor
       enddo
       bincor=0.

! find length of character header
       do j=1,nday
       do i=1,fday
         read(10,'(1A)') string
         nchar(i,j)=len_trim(string)
         nhead(i,j)=index(string,substring)  !find character header length before "="
         write(11,*) string(nhead(i,j)+1:nchar(i,j))
!         write(12,*) trim(string), nchar(i,j), nhead(i,j) 
       enddo
       enddo

! read data
       rewind (11)
       num=0; mcor=0; mrms=0; mbias=0
       do j=1,nday
       do i=1,fday
         if(nhead(i,j).eq.0) then
           read(11,'(1A)') string(1:nchar(i,j))    !data missing 
           cor(i,j)=bad
           rms(i,j)=bad
           bias(i,j)=bad
         else
           read(11,*)points(i,j),(vsdb(k,i,j),k=1,ns)
           if(points(i,j).eq.0) then
             cor(i,j)=bad
             rms(i,j)=bad
             bias(i,j)=bad
           else
             cor(i,j)=vsdb(5,i,j)/sqrt(vsdb(6,i,j)*vsdb(7,i,j))
             rms(i,j)=sqrt(max(0.0d0,vsdb(6,i,j)+vsdb(7,i,j)-2*vsdb(5,i,j)))
!!           bias(i,j)=sqrt(vsdb(1,i,j)**2+vsdb(2,i,j)**2)-sqrt(vsdb(3,i,j)**2+vsdb(4,i,j)**2)
             bias(i,j)=sqrt(vsdb(6,i,j))-sqrt(vsdb(7,i,j))
             num(i)=num(i)+1
             mcor(i)=mcor(i)+cor(i,j)
             mrms(i)=mrms(i)+rms(i,j)
             mbias(i)=mbias(i)+bias(i,j)
             do k=1,bin
              if(cor(i,j).gt.binbnd(k).and.cor(i,j).le.binbnd(k+1)) bincor(i,k)=bincor(i,k)+cor(i,j)
             enddo
           endif
         endif
       enddo
       enddo

! mean scores in ndays, and normalied bins 
       do i=1,fday
        if(num(i).gt.0) then
         mcor(i)=mcor(i)/num(i)
         mrms(i)=mrms(i)/num(i)
         mbias(i)=mbias(i)/num(i)
         bincor(i,:)=bincor(i,:)/num(i)
        else
         mcor(i)=bad
         mrms(i)=bad
         mbias(i)=bad
         bincor(i,:)=bad
         num(i)=bad
        endif
       enddo

       do j=1,nday
         write(20) (cor(i,j),i=1,fday)
         write(20) (rms(i,j),i=1,fday)
         write(20) (bias(i,j),i=1,fday)
         write(20) (bincor(i,j),i=1,fday)
       enddo

! save mean scores as the nday+1 record in time
       write(20) (mcor(i),i=1,fday)
       write(20) (mrms(i),i=1,fday)
       write(20) (mbias(i),i=1,fday)  
       write(20) (num(i),i=1,fday)    !note: num of records instead of bincor

!!     write(13,123) $yyyymm, "${model}_num", (num(i),i=1,fday)                               
       write(13,123) $yyyymm, "${model}_cor", (mcor(i),i=1,fday)                               
!!     write(14,123) $yyyymm, "${model}_num", (num(i),i=1,fday)                               
       write(14,123) $yyyymm, "${model}_rms", (mrms(i),i=1,fday)                               
!!     write(15,123) $yyyymm, "${model}_num", (num(i),i=1,fday)                               
       write(15,123) $yyyymm, "${model}_bia", (mbias(i),i=1,fday)                               

 123   format(i10,2x,A,30f10.3)
       close (10)
       close (11)
       close (20)
      end
EOF

xlf90 -o convert.x convert.f
./convert.x

meantxt=${vnam1}_${lev}_${reg1}_${cyc}Z${yyyymm}
cat fort.13 >>meancor_${meantxt}.txt
cat fort.14 >>meanrms_${meantxt}.txt
cat fort.15 >>meanbias_${meantxt}.txt


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
title scores
xdef   1 levels   0                 
ydef    $fdaysp1 linear 0 24
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
