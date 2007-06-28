subroutine read_prepbufr(nread,ndata,nodata,infile,obstype,lunout,twindin,sis,&
     prsl_full)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_prepbuf                read obs from prepbufr file
!   prgmmr: parrish          org: np22                date: 1990-10-07
!
! abstract:  This routine reads conventional data found in the prepbufr
!            file.  Specific observation types read by this routine 
!            include surface pressure, temperature, winds (components
!            and speeds), moisture and total precipitable water.  
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   1990-10-07  parrish
!   1998-05-15  weiyu yang 
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-02-13  derber, j. - clean up and modify vertical weighting
!   2004-06-16  treadon - update documentation
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-07-30  derber  - generalize number of data records per obs type
!   2004-08-26  derber  - fix many errors in reading of sst data
!   2004-08-27  kleist  - modify pressure calculation
!   2004-10-28  kleist  - correct array index bug in hybrid pressure calculation
!   2004-11-16  treadon - deallocate(etabl) prior to exiting routine
!   2005-02-10  treadon - add call destroygrids for obstype = sst
!   2005-05-24  pondeca - add surface analysis option
!   2005-02-24  treadon - remove hardwired setting of metar ps obs error
!   2005-05-27  derber  - reduce t, uv, ps error limits
!   2005-05-27  kleist/derber - add option to read in new ob error table
!   2005-07-19  derber - clean up code a bit
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-10-11  treadon - change convinfo read to free format
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-26  treadon - add routine tag to convinfo printout
!   2006-02-03  derber  - modify to count read/keep data and new obs control
!   2006-02-03  treadon - use interpolated guess 3d pressure field in errormod
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-09  treadon - save height for wind observations
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-03  derber  - modify to properly handle height of surface obs
!   2006-04-05  wu - changes to read in GPS IPW (type 153)
!   2006-05-18  middlecoff/treadon - add huge_i_kind upper limit on nint
!   2006-05-29  treadon - increase nreal to pass more information to setup routines
!   2006-06-08  su - added the option to turn off oiqc
!   2006-06-21  wu - deallocate etabl array
!   2006-07-28  derber  - temporarily add subtype for meteosat winds based on sat ID
!   2006-07-31  kleist  - change to surface pressure ob error from ln(ps) to ps(cb)
!   2007-02-20  wu - correct errors in quality mark checks
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     prsl_full- 3d pressure on full domain grid
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     nodata   - number of individual "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     twindin  - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,fv,t0c,half,&
       three,four,rad2deg,tiny_r_kind,huge_r_kind,huge_i_kind
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
       tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
       rlats,rlons,twodvar_regional
  use convinfo, only: nconvtype,ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype
  use obsmod, only: iadate,oberrflg
  use qcmod, only: errormod,noiqc
  implicit none

! Declare passed variables
  character(10),intent(in):: infile,obstype
  character(20),intent(in):: sis
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata,nodata
  real(r_kind),intent(in):: twindin
  real(r_kind),dimension(nlat,nlon,nsig),intent(in):: prsl_full

! Declare local parameters
  real(r_kind),parameter:: r0_001=0.001_r_kind
  real(r_kind),parameter:: r0_5 = 0.5_r_kind
  real(r_kind),parameter:: r0_75 = 0.75_r_kind
  real(r_kind),parameter:: r0_7 = 0.7_r_kind
  real(r_kind),parameter:: r0_8 = 0.8_r_kind
  real(r_kind),parameter:: r1_2 = 1.2_r_kind
  real(r_kind),parameter:: r1_5 = 1.5_r_kind
  real(r_kind),parameter:: r10  = 10.0_r_kind
  real(r_kind),parameter:: r20  = 20.0_r_kind
  real(r_kind),parameter:: r50  = 50.0_r_kind
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r100 = 100.0_r_kind
  real(r_kind),parameter:: r300 = 300.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r500 = 500.0_r_kind
  real(r_kind),parameter:: r999 = 999.0_r_kind
  real(r_kind),parameter:: r1100= 1100.0_r_kind
  real(r_kind),parameter:: r2000= 2000.0_r_kind
  real(r_kind),parameter:: convert= 1.0e-6_r_kind
  real(r_kind),parameter:: emerr= 0.2_r_kind

! Declare local variables
  logical tob,qob,uvob,spdob,sstob,pwob,psob
  logical outside,driftl,convobs,inflate_error

  character(40) drift,hdstr,qcstr,oestr,sststr
  character(80) obstr
  character(10) date
  character(8) subset

  integer(i_kind) lunin,i,maxobs,sstmeas,j
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) jdate,ihh,idd,idate,iret,im,iy,k,levs
  integer(i_kind) kx,nreal,nchanl,ilat,ilon,ithin
  integer(i_kind) cat,zqm,pwq,sstq,qm,lim_qm,lim_zqm
  integer(i_kind) lim_tqm,lim_qqm
  integer(i_kind),dimension(255):: pqm,qqm,tqm,wqm

  real(r_kind) time,timex,time_drift
  real(r_kind) qtflg,tdry,rmesh,ediff,usage
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) qoe,qobcon,pwoe,pwmerr,dlnpob,ppb,poe,qmaxerr,wtype
  real(r_kind) toe,woe,errout,oelev,dlat,dlon,sstoe,dlat_earth,dlon_earth
  real(r_kind) selev,elev,stnelev
  real(r_kind),dimension(nsig):: presl
  real(r_kind),dimension(nsig-1):: dpres
  real(r_kind),allocatable,dimension(:,:):: cdata_all

  real(r_double) rstation_id

  real(r_double),dimension(7):: hdr
  real(r_double),dimension(8,255):: drfdat,qcmark,obserr
  real(r_double),dimension(9,255):: obsdat
  real(r_double),dimension(8,1):: sstdat
  real(r_kind),dimension(255)::plevs
  real(r_kind),dimension(1000):: twind,gross,ermax,ermin,var_b,var_pg
  integer(i_kind),dimension(1000):: itype,iuse,numgrp,ngroup,nmiter
  
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterr,vdisterrmax,u00,v00
  integer(i_kind) ntest,nvtest,iosub,ixsub,isubsub,iosubsub,iobsub

  real(4),allocatable::etabl(:,:,:)
  integer(i_kind) ietabl,lcount,itypex,ierrcode,numbcast,kl,k1,k2
  integer(i_kind) l,m,ikx
  real(r_kind) del,terrmin,werrmin,perrmin,qerrmin,pwerrmin

  data hdstr  /'SID XOB YOB DHR TYP ELV SAID '/
  data obstr  /'POB QOB TOB ZOB UOB VOB PWO CAT PRSS' /
  data drift  /'XDR YDR HRDR                    '/
  data sststr /'MSST DBSS SST1 SSTQM SSTOE           '/
  data qcstr  /'PQM QQM TQM ZQM WQM NUL PWQ     '/
  data oestr  /'POE QOE TOE NUL WOE NUL PWE     '/

  data lunin / 10 /
  data ithin / -9 /
  data rmesh / -99.999 /
  

!**************************************************************************
! Initialize variables
  disterrmax=zero
  vdisterrmax=zero
  ntest=0
  nvtest=0
  maxobs=2e6
  nread=0
  nchanl=0
  ilon=2
  ilat=3

  nreal=0
  tob = obstype == 't'
  uvob = obstype == 'uv'
  spdob = obstype == 'spd'
  psob = obstype == 'ps'
  qob = obstype == 'q'
  pwob = obstype == 'pw'
  sstob = obstype == 'sst'
  convobs = tob .or. uvob .or. spdob .or. qob
  if(tob)then
     nreal=16
  else if(uvob) then 
     nreal=16
  else if(spdob) then
     nreal=16
  else if(psob) then
     nreal=15
  else if(qob) then
     nreal=17
  else if(pwob) then
     nreal=16
  else if(sstob) then
     nreal=16
  else 
     write(6,*) ' illegal obs type in READ_PREPBUFR '
     call stop2(94)
  end if

  if(oberrflg)then
     allocate(etabl(300,33,6))
     ietabl=19
     open(ietabl,file='errtable',form='formatted')
     rewind ietabl
     etabl=1.e9_r_kind
     lcount=0
     do l=1,300
        read(ietabl,100,end=120,err=120)itypex
100     format(1x,i3)
        lcount=lcount+1
        do k=1,33
           read(ietabl,110)(etabl(itypex,k,m),m=1,6)
110        format(1x,6e12.5)
        end do
     end do
120  continue
     if(lcount.le.0) then
       write(6,*)'READ_PREPBUFR:  ***WARNING*** obs error table not available to 3dvar.'
       oberrflg=.false.
       deallocate(etabl)
     else
       write(6,*)'READ_PREPBUFR:  using observation errors from user provided table'
     endif
     close(ietabl)

!    Set lower limits for observation errors
     terrmin=r0_5
     werrmin=one
     perrmin=r0_5
     qerrmin=one_tenth
     pwerrmin=one
     
  endif

! Open, then read date from bufr data
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
  call readmg(lunin,subset,idate,iret)
  if(iret/=0) goto 1020

! On temperature task, write out date information.  If date in prepbufr
! file does not agree with analysis date, print message and stop program
! execution.

  if(tob) then
     write(date,'( i10)') idate
     read (date,'(i4,3i2)') iy,im,idd,ihh
     write(6,*)'READ_PREPBUFR: bufr file date is ',iy,im,idd,ihh
     if(iy/=iadate(1).or.im/=iadate(2).or.idd/=iadate(3).or.&
          ihh/=iadate(4)) then
        write(6,*)'***READ_PREPBUFR ERROR*** incompatable analysis ',&
             'and observation date/time'
        write(6,*)' year  anal/obs ',iadate(1),iy
        write(6,*)' month anal/obs ',iadate(2),im
        write(6,*)' day   anal/obs ',iadate(3),idd
        write(6,*)' hour  anal/obs ',iadate(4),ihh
        call stop2(94)
     end if
  end if

  allocate(cdata_all(nreal,maxobs))

! Big loop over buffer file	

10 call readsb(lunin,iret) 
     if(iret/=0) then
        call readmg(lunin,subset,jdate,iret)
        if(iret/=0) go to 1000
        go to 10
     end if

!    Extract type, date, and location information
     call ufbint(lunin,hdr,7,1,iret,hdstr)
     rstation_id=hdr(1)
     if(hdr(2)>= r360)hdr(2)=hdr(2)-r360
     if(hdr(2) < zero)hdr(2)=hdr(2)+r360
     dlon_earth=hdr(2)*deg2rad
     dlat_earth=hdr(3)*deg2rad
     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
        if(diagnostic_reg) then
           call txy2ll(dlon,dlat,rlon00,rlat00)
           ntest=ntest+1
           cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
           cdist=max(-one,min(cdist,one))
           disterr=acosd(cdist)
           disterrmax=max(disterrmax,disterr)
        end if
        if(outside) go to 10   ! check to see if outside regional domain
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,1,rlats,nlat,1)
        call grdcrd(dlon,1,rlons,nlon,1)
     endif

     time=hdr(4)
     kx=hdr(5)
     stnelev=hdr(6)
     iobsub = 0           ! temporary until put in bufr file
     if(kx == 243 .or. kx == 253 .or. kx == 254) then
        iobsub = hdr(7)
     end if

     ikx=0
     do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. kx == ictype(i)  &
                     .and. abs(icuse(i))== 1)then
           if(icsubtype(i) == iobsub)ikx=i
       end if
     end do
     if(ikx == 0)then
      do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. kx == ictype(i)  &
                     .and. abs(icuse(i))== 1)then
           ixsub=icsubtype(i)/10
           iosub=iobsub/10
           isubsub=icsubtype(i)-ixsub*10
           if(ixsub == iosub .and. isubsub == 0)ikx=i
       end if
      end do
     end if
     if(ikx == 0)then
      do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. kx == ictype(i)  &
                     .and. abs(icuse(i))== 1)then
           if(icsubtype(i) == 0)ikx=i
       end if
      end do
     end if
     if(ikx == 0) go to 10             ! not ob type used

!    If running in 2d-var (surface analysis) mode, check to see if observation
!    is surface type.  If not, read next observation report from bufr file
     if ( twodvar_regional .and. &
          (kx<180 .or. kx>289 .or. (kx>189 .and. kx<280)) ) go to 10

!    Balloon drift information available for these data
     driftl=kx==120.or.kx==220.or.kx==221
     if((real(abs(time)) > real(ctwind(ikx)) .or. real(abs(time)) > real(twindin)) &
             .and. .not. driftl)go to 10 ! outside time window
     timex=time
     
!    Extract data information on levels
     call ufbint(lunin,obsdat,9,255,levs,obstr)
     call ufbint(lunin,qcmark,8,255,levs,qcstr)
     call ufbint(lunin,obserr,8,255,levs,oestr)
     nread=nread+levs
     if(uvob)nread=nread+levs
     sstdat=1.e11
     if(sstob)call ufbint(lunin,sstdat,8,1,levs,sststr)


!    If available, get obs errors from error table
     if(oberrflg)then
        do k=1,levs
           itypex=kx
           ppb=obsdat(1,k)
           if(kx==153)ppb=obsdat(9,k)*0.01
           ppb=max(zero,min(ppb,r2000))
           if(ppb.ge.etabl(itypex,1,1)) k1=1
           do kl=1,32
              if(ppb.ge.etabl(itypex,kl+1,1).and.ppb.le.etabl(itypex,kl,1)) k1=kl
           end do
           if(ppb.le.etabl(itypex,33,1)) k1=5
           k2=k1+1
           ediff = etabl(itypex,k2,1)-etabl(itypex,k1,1)
           if (abs(ediff) > tiny_r_kind) then
              del = (ppb-etabl(itypex,k1,1))/ediff
           else
              del = huge_r_kind
           endif
           del=max(zero,min(del,one))
           obserr(3,k)=(one-del)*etabl(itypex,k1,2)+del*etabl(itypex,k2,2)
           obserr(2,k)=(one-del)*etabl(itypex,k1,3)+del*etabl(itypex,k2,3)
           obserr(5,k)=(one-del)*etabl(itypex,k1,4)+del*etabl(itypex,k2,4)
           obserr(1,k)=(one-del)*etabl(itypex,k1,5)+del*etabl(itypex,k2,5)
           obserr(7,k)=(one-del)*etabl(itypex,k1,6)+del*etabl(itypex,k2,6)
           
           obserr(3,k)=max(obserr(3,k),terrmin)
           obserr(2,k)=max(obserr(2,k),qerrmin)
           obserr(5,k)=max(obserr(5,k),werrmin)
           obserr(1,k)=max(obserr(1,k),perrmin)
           obserr(7,k)=max(obserr(7,k),pwerrmin)
        enddo
     endif
     

!    If data with drift position, get drift information
     if(driftl)call ufbint(lunin,drfdat,8,255,iret,drift)
!    Loop over levels       
     do k=1,levs
        do i=1,8
           qcmark(i,k) = min(qcmark(i,k),huge_i_kind)
        end do
        
      plevs(k)=one_tenth*obsdat(1,k)   ! convert mb to cb
      pqm(k)=nint(qcmark(1,k))
      qqm(k)=nint(qcmark(2,k))
      tqm(k)=nint(qcmark(3,k))
      wqm(k)=nint(qcmark(5,k))
     end do
     LOOP_K_LEVS: do k=1,levs

!       Extract quality marks
        if(tob)then
           qm=tqm(k)
        else if(uvob) then 
           qm=wqm(k)
        else if(spdob) then
           qm=wqm(k)
        else if(psob) then
           qm=pqm(k)
        else if(qob) then
           if(obsdat(2,k) > 1.0e9)cycle loop_k_levs
           qm=qqm(k)
        else if(pwob) then
           pwq=nint(qcmark(7,k))
           qm=pwq
        else if(sstob) then
           sstq=100
           if (k==1) sstq=nint(min(sstdat(4,k),huge_i_kind))
           qm=sstq
        end if

!       Set inflate_error logical and qc limits based on noiqc flag
        inflate_error=.false.
        if (noiqc) then
           lim_qm=8
           if (qm==3 .or. qm==7) inflate_error=.true.
           if (psob) lim_zqm=7
           if (qob)  lim_tqm=7
           if (tob)  lim_qqm=8
        else
           lim_qm=4
           if (qm==3) inflate_error=.true.
           if (psob) lim_zqm=4
           if (qob)  lim_tqm=4
           if (tob)  lim_qqm=4
        endif

!       Check qc marks to see if obs should be processed or skipped
        if (psob) then
           zqm=nint(qcmark(4,k))
           cat=nint(min(obsdat(8,k),huge_i_kind))
           if ( (zqm>=lim_zqm .and. zqm/=15 .and. zqm/=9) .or. cat /=0 .or. obsdat(1,k) < r500)cycle loop_k_levs
        endif

        if(convobs .and. pqm(k) >=lim_qm .and. qm/=15 .and. qm/=9 )cycle loop_k_levs
        if(qm >=lim_qm .and. qm /=15 .and. qm /=9)cycle loop_k_levs


!       Set usage variable              
        usage = 0.
        if(icuse(ikx) < 0)usage=100.
        if(qm == 15 .or. qm == 9)usage=100.
        if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
          if(mod(ndata+1,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
        end if


!       If needed, extract drift information.   
        if(driftl)then
           if(drfdat(1,k) >= r360)drfdat(1,k)=drfdat(1,k)-r360
           if(drfdat(1,k) <  zero)drfdat(1,k)=drfdat(1,k)+r360
           if(abs(drfdat(2,k)) > r90 .or. drfdat(1,k) > r360 .or. drfdat(1,k) < zero)then
              drfdat(2,k)=hdr(3)
              drfdat(1,k)=hdr(2)
           end if

!          Check to ensure header lat and drift lat similar
           if(abs(drfdat(2,k)-hdr(3)) > r10 .and.  &
              abs(drfdat(1,k)-hdr(2)) > r10)then
              drfdat(2,k)=hdr(3)
              drfdat(1,k)=hdr(2)
           end if

!          Check to see if the time is outrageous if so set to header value
           time_drift = drfdat(3,k)
           if (abs(time_drift)>four) time_drift = time

!          Check to see if the time is outside range
           if(abs(time_drift) > ctwind(ikx) .or. abs(time_drift) > twindin)then
              time_drift=timex
              if(abs(timex) > ctwind(ikx) .or. abs(timex) > twindin) CYCLE LOOP_K_LEVS
           end if
           time=time_drift
           
           dlat_earth = drfdat(2,k) * deg2rad
           dlon_earth = drfdat(1,k) * deg2rad

           if(regional)then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if(outside) cycle LOOP_K_LEVS 
           else
              dlat = dlat_earth
              dlon = dlon_earth
              call grdcrd(dlat,1,rlats,nlat,1)
              call grdcrd(dlon,1,rlons,nlon,1)
           endif
        end if

!       Interpolate guess pressure profile to observation location
        klon1= int(dlon);  klat1= int(dlat)
        dx   = dlon-klon1; dy   = dlat-klat1
        dx1  = one-dx;     dy1  = one-dy
        w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

        klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
        if (klon1==0) klon1=nlon
        klatp1=min(nlat,klat1+1); klonp1=klon1+1
        if (klonp1==nlon+1) klonp1=1

        do kk=1,nsig
           presl(kk)=w00*prsl_full(klat1,klon1,kk) + w10*prsl_full(klatp1,klon1,kk) + &
                w01*prsl_full(klat1,klonp1,kk) + w11*prsl_full(klatp1,klonp1,kk)
        end do

!       Compute depth of guess pressure layersat observation location
        if (.not.twodvar_regional) then
           do kk=1,nsig-1
              dpres(kk)=presl(kk)-presl(kk+1)
           end do
        endif

!       Extract pressure level and quality marks
        dlnpob=log(plevs(k))  ! ln(pressure in cb)
        ndata=ndata+1
        nodata=nodata+1
        if(uvob)nodata=nodata+1
        if(ndata > maxobs) then
             write(6,*)'READ_PREPBUFR:  ***WARNING*** ndata > maxobs for ',obstype
             ndata = maxobs
        end if
  
!       Temperature
        if(tob) then
           ppb=obsdat(1,k)
           qtflg=zero
           if(qqm(k)>=lim_qqm.or.ppb<r300) qtflg=one
           if(kx==130 .or. kx == 131 .or. kx == 133) qtflg=one
           call errormod(pqm,tqm,levs,plevs,errout,k,presl,dpres,nsig)
           toe=obserr(3,k)*errout
           if (inflate_error) toe=toe*r1_2
           if(ppb < r100)toe=toe*r1_2
           cdata_all(1,ndata)=toe                     ! temperature error
           cdata_all(2,ndata)=dlon                    ! grid relative longitude
           cdata_all(3,ndata)=dlat                    ! grid relative latitude
           cdata_all(4,ndata)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,ndata)=obsdat(3,k)+t0c         ! temperature ob.
           cdata_all(6,ndata)=rstation_id             ! station id
           cdata_all(7,ndata)=time                    ! time
           cdata_all(8,ndata)=ikx                     ! type
           cdata_all(9,ndata)=qtflg                   ! qtflg
           cdata_all(10,ndata)=tqm(k)                 ! quality mark
           cdata_all(11,ndata)=obserr(3,k)            ! original obs error
           cdata_all(12,ndata)=usage                  ! usage parameter
           cdata_all(13,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(14,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(15,ndata)=stnelev                ! station elevation (m)
           cdata_all(16,ndata)=obsdat(4,k)            ! observation height (m)

!       Winds 
        else if(uvob) then 
           call errormod(pqm,wqm,levs,plevs,errout,k,presl,dpres,nsig)
           woe=obserr(5,k)*errout
           if (inflate_error) woe=woe*r1_2
           if(obsdat(1,k) < r50)woe=woe*r1_2
           selev=stnelev
           oelev=obsdat(4,k)
           if(selev == oelev)oelev=r10+selev
           if(kx >= 280 .and. kx < 290 )then
             if (kx == 280) oelev=r20+selev
             if (kx == 281) oelev=r10+selev
             if (kx == 282) oelev=r20+selev
             if (kx == 284) oelev=r10+selev
             if (kx == 285) oelev=selev
             if (kx == 285) selev=zero
             if (kx == 286) oelev=r10+selev
             if (kx == 287) oelev=r10+selev
             if (kx == 288) oelev=r10+selev
             if (kx == 289) oelev=r10+selev
           end if

!          Rotate winds to rotated coordinate
           uob=obsdat(5,k)
           vob=obsdat(6,k)
           if(regional)then
              u0=uob
              v0=vob
              call rotate_wind_ll2xy(u0,v0,uob,vob,dlon_earth,dlat_earth,dlon,dlat)
              if(diagnostic_reg) then
                 call rotate_wind_xy2ll(uob,vob,u00,v00,dlon_earth,dlat_earth,dlon,dlat)
                 nvtest=nvtest+1
                 disterr=sqrt((u0-u00)**2+(v0-v00)**2)
                 vdisterrmax=max(vdisterrmax,disterr)
              end if
           endif
           
           cdata_all(1,ndata)=woe                     ! wind error
           cdata_all(2,ndata)=dlon                    ! grid relative longitude
           cdata_all(3,ndata)=dlat                    ! grid relative latitude
           cdata_all(4,ndata)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,ndata)=oelev                   ! height of observation
           cdata_all(6,ndata)=uob                     ! u obs
           cdata_all(7,ndata)=vob                     ! v obs
           cdata_all(8,ndata)=rstation_id             ! station id
           cdata_all(9,ndata)=time                    ! time
           cdata_all(10,ndata)=ikx                    ! type
           cdata_all(11,ndata)=selev                  ! station elevation
           cdata_all(12,ndata)=wqm(k)                 ! quality mark
           cdata_all(13,ndata)=obserr(5,k)            ! original obs error
           cdata_all(14,ndata)=usage                  ! usage parameter
           cdata_all(15,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(16,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)

        else if(spdob) then 
           woe=obserr(5,k)
           if (inflate_error) woe=woe*r1_2
           elev=r20

           cdata_all(1,ndata)=woe                     ! wind error
           cdata_all(2,ndata)=dlon                    ! grid relative longitude
           cdata_all(3,ndata)=dlat                    ! grid relative latitude
           cdata_all(4,ndata)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,ndata)=obsdat(5,k)             ! u obs
           cdata_all(6,ndata)=obsdat(6,k)             ! v obs
           cdata_all(7,ndata)=rstation_id             ! station id
           cdata_all(8,ndata)=time                    ! time
           cdata_all(9,ndata)=ikx                     ! type
           cdata_all(10,ndata)=elev                   ! elevation of observation
           cdata_all(11,ndata)=wqm(k)                 ! quality mark
           cdata_all(12,ndata)=obserr(5,k)            ! original obs error
           cdata_all(13,ndata)=usage                  ! usage parameter
           cdata_all(14,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(15,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(16,ndata)=stnelev                ! station elevation (m)

!       Surface pressure 
        else if(psob) then
 
           poe=obserr(1,k)*one_tenth                  ! convert from mb to cb
           if (inflate_error) poe=poe*r1_2
           cdata_all(1,ndata)=poe                     ! surface pressure error (cb)
           cdata_all(2,ndata)=dlon                    ! grid relative longitude
           cdata_all(3,ndata)=dlat                    ! grid relative latitude
           cdata_all(4,ndata)=exp(dlnpob)             ! pressure (in cb)
           cdata_all(5,ndata)=obsdat(4,k)             ! surface height
           cdata_all(6,ndata)=obsdat(3,k)+t0c         ! surface temperature
           cdata_all(7,ndata)=rstation_id             ! station id
           cdata_all(8,ndata)=time                    ! time
           cdata_all(9,ndata)=ikx                     ! type
           cdata_all(10,ndata)=pqm(k)                 ! quality mark
           cdata_all(11,ndata)=obserr(1,k)*one_tenth  ! original obs error (cb)
           cdata_all(12,ndata)=usage                  ! usage parameter
           cdata_all(13,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(14,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(15,ndata)=stnelev                ! station elevation (m)

!       Specific humidity 
        else if(qob) then
           qmaxerr=emerr
           call errormod(pqm,qqm,levs,plevs,errout,k,presl,dpres,nsig)
           qoe=obserr(2,k)*one_tenth*errout
           if (inflate_error) then
              qmaxerr=emerr*r0_7; qoe=qoe*r1_2
           end if
           qobcon=obsdat(2,k)*convert
           tdry=r999
           if (tqm(k)<lim_tqm) tdry=(obsdat(3,k)+t0c)/(one+fv*qobcon)
           cdata_all(1,ndata)=qoe                     ! q error   
           cdata_all(2,ndata)=dlon                    ! grid relative longitude
           cdata_all(3,ndata)=dlat                    ! grid relative latitude
           cdata_all(4,ndata)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,ndata)=qobcon                  ! q ob
           cdata_all(6,ndata)=rstation_id             ! station id
           cdata_all(7,ndata)=time                    ! time
           cdata_all(8,ndata)=ikx                     ! type
           cdata_all(9,ndata)=qmaxerr                 ! q max error
           cdata_all(10,ndata)=tdry                   ! dry temperature (obs is tv)
           cdata_all(11,ndata)=qqm(k)                 ! quality mark
           cdata_all(12,ndata)=obserr(2,k)*one_tenth  ! original obs error
           cdata_all(13,ndata)=usage                  ! usage parameter
           cdata_all(14,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(15,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(16,ndata)=stnelev                ! station elevation (m)
           cdata_all(17,ndata)=obsdat(4,k)            ! observation height (m)

!       Total precipitable water (ssm/i)
        else if(pwob) then

           pwoe=obserr(7,k)
           pwmerr=pwoe*three
           cdata_all(1,ndata)=pwoe                    ! pw error
           cdata_all(2,ndata)=dlon                    ! grid relative longitude
           cdata_all(3,ndata)=dlat                    ! grid relative latitude
           cdata_all(4,ndata)=obsdat(7,k)             ! pw obs
           cdata_all(5,ndata)=rstation_id             ! station id
           cdata_all(6,ndata)=time                    ! time
           cdata_all(7,ndata)=ikx                     ! type
           cdata_all(8,ndata)=pwmerr                  ! pw max error
           cdata_all(9,ndata)=pwq                     ! quality mark
           cdata_all(10,ndata)=obserr(7,k)            ! original obs error
           cdata_all(11,ndata)=usage                  ! usage parameter
           cdata_all(12,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(13,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(14,ndata)=stnelev                ! station elevation (m)
           cdata_all(15,ndata)=obsdat(1,k)            ! observation pressure (hPa)
           cdata_all(16,ndata)=obsdat(4,k)            ! observation height (m)

!       Conventional sst observations
        else if(sstob) then

!          Locate the observation on the analysis grid.  Get land/sea/ice
!          mask at nearest analysis grid points.

           sstoe=r0_75
!          sstoe=sstdat(5,k)

           cdata_all(1,ndata)=sstoe                   ! sst error
           cdata_all(2,ndata)=dlon                    ! grid relative longitude
           cdata_all(3,ndata)=dlat                    ! grid relative latitude
           cdata_all(4,ndata)=sstdat(3,k)             ! sst obs
           cdata_all(5,ndata)=rstation_id             ! station id
           cdata_all(6,ndata)=time                    ! time
           cdata_all(7,ndata)=ikx                     ! type
           cdata_all(8,ndata)=sstoe*three             ! pw max error
           cdata_all(9,ndata)=sstdat(2,k)             ! depth of measurement
           cdata_all(10,ndata)=sstdat(1,k)            ! measurement type
           cdata_all(11,ndata)=sstq                   ! quality mark
           cdata_all(12,ndata)=sstdat(5,k)            ! original obs error
           cdata_all(13,ndata)=usage                  ! usage parameter
           cdata_all(14,ndata)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(15,ndata)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(16,ndata)=stnelev                ! station elevation (m)

!          if(kx == 120 .or. kx == 282)then
!             write(6,*)'READ_PREPBUFR:  kx,rstation_id,sstq=',&
!                  kx,rstation_id,sstq
!             do i=1,10
!                write(6,*)'READ_PREPBUFR:  i,cdata_all=',i,cdata_all(i,ndata)
!             end do
!          end if

!          Measurement types
!             0       Ship intake
!             1       Bucket
!             2       Hull contact sensor
!             3       Reversing Thermometer
!             4       STD/CTD sensor
!             5       Mechanical BT
!             6       Expendable BT
!             7       Digital BT
!             8       Thermistor chain
!             9       Infra-red scanner
!             10      Micro-wave scanner
!             11-14   Reserved

        end if

!
!    End k loop over levs
     end do  LOOP_K_LEVS

!
!   End of bufr read loop
  go to 10

! Normal exit
1000 continue


! Write header record and data to output file for further processing
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)

  deallocate(cdata_all)

! Close unit to bufr file
1020 continue
  if (oberrflg) deallocate(etabl)
  call closbf(lunin)

  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_PREPBUFR:  ',&
       'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_PREPBUFR:  ',&
       'nvtest,vdisterrmax=',ntest,vdisterrmax


! End of routine
  return
end subroutine read_prepbufr
