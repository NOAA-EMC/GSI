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
!   2006-02-23  kistler - modify to add optional data thinning
!   2006-02-23  kistler - raob instument as subtype and solar elv angle computed
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-03  derber  - modify to properly handle height of surface obs
!   2006-04-05  wu - changes to read in GPS IPW (type 153)
!   2006-05-18  middlecoff/treadon - add huge_i_kind upper limit on nint
!   2006-05-29  treadon - increase nreal to pass more information to setup routines
!   2006-06-08  su - added the option to turn off oiqc
!   2006-06-21  wu - deallocate etabl array
!   2006-07-28  derber  - temporarily add subtype for meteosat winds based on sat ID
!   2006-07-31  kleist  - change to surface pressure ob error from ln(ps) to ps(cb)
!   2006-10-25  sienkiewicz - add blacklist of raob data
!   2006-12-01  todling - embedded blacklist into a module
!   2007-02-13  parrish - add ability to use obs files with ref time different from analysis time
!   2007-02-20  wu - correct errors in quality mark checks
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2007-03-15  su - remove the error table reading part to a subroutine
!   2007-04-24  wu - add TAMDAR (134) to be used as sensible T
!   2007-05-17  kleist - generalize flag for virtual/sensible temperature obs
!   2007-09-28  treadon - truncate/expand obs time to remove extraneous bits 
!   2007-10-03  su  -   Add reading qc mark from satellite wind
!   2007-10-24  Pondeca - add ability to use use_list on mesonet winds
!   2007-11-03  su  -   modify conventional thinning algorithm
!   2008-03-28  wu - add code to generate optional observation perturbations
!   2008-03-31  li.bi - add ascat
!   2008-05-27  safford - rm unused vars and uses
!   2008-06-02  treadon - check iret from inital readmg and act accordingly
!   2008-09-08  lueken  - merged ed's changges into q1fy09 code
!   2008-21-25  todling - adapted Tremolet 2007-03-01 change of time window
!                       - remove unused vars
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
       three,four,rad2deg,tiny_r_kind,huge_r_kind,huge_i_kind,&
       izero
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
       tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
       rlats,rlons,twodvar_regional,check_rotate_wind
  use convinfo, only: nconvtype,ctwind, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype, &
		ithin_conv,rmesh_conv,pmesh_conv, &
		id_bias_ps,id_bias_t,conv_bias_ps,conv_bias_t

  use obsmod, only: iadate,oberrflg,offtime_data,perturb_obs,perturb_fact,ran01dom
  use obsmod, only: blacklst
  use converr,only: etabl
  use gsi_4dvar, only: l4dvar,idmodel,iadatebgn,iadateend,time_4dvar,winlen
  use qcmod, only: errormod,noiqc
  use convthin, only: make3grids,map3grids,del3grids,use_all
  use blacklist, only : blacklist_read,blacklist_destroy
  use blacklist, only : blkstns,blkkx,ibcnt

  implicit none

! Declare passed variables
  character(len=*),intent(in):: infile,obstype
  character(len=*),intent(in):: sis
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata,nodata
  real(r_kind),intent(in):: twindin
  real(r_kind),dimension(nlat,nlon,nsig),intent(in):: prsl_full

! Declare local parameters
  real(r_kind),parameter:: r0_5 = 0.5_r_kind
  real(r_kind),parameter:: r0_75 = 0.75_r_kind
  real(r_kind),parameter:: r0_7 = 0.7_r_kind
  real(r_kind),parameter:: r1_2 = 1.2_r_kind
  real(r_kind),parameter:: r3_33= three + one/three
  real(r_kind),parameter:: r6   = 6.0_r_kind
  real(r_kind),parameter:: r10  = 10.0_r_kind
  real(r_kind),parameter:: r20  = 20.0_r_kind
  real(r_kind),parameter:: r50  = 50.0_r_kind
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r100 = 100.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r500 = 500.0_r_kind
  real(r_kind),parameter:: r999 = 999.0_r_kind
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: r2000= 2000.0_r_kind
  real(r_kind),parameter:: convert= 1.0e-6_r_kind
  real(r_kind),parameter:: emerr= 0.2_r_kind

! Declare local variables
  logical tob,qob,uvob,spdob,sstob,pwob,psob
  logical outside,driftl,convobs,inflate_error
  logical listexist,lprov,asort

  character(40) drift,hdstr,qcstr,oestr,sststr,satqcstr,levstr
  character(80) obstr
  character(10) date
  character(8) subset

  character(8) prvstr,sprvstr     
  character(8) c_prvstg,c_sprvstg 
  character(16) cprovider(200)    
  character(80) cstring           

  integer(i_kind) lunin,i,maxobs,j,idomsfc,itemp
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nc,nx,id,isflg,ntread,itx
  integer(i_kind) ihh,idd,idate,iret,im,iy,k,levs
  integer(i_kind) kx,nreal,nchanl,ilat,ilon,ithin
  integer(i_kind) cat,zqm,pwq,sstq,qm,lim_qm,lim_zqm
  integer(i_kind) lim_tqm,lim_qqm
  integer(i_kind),dimension(255):: pqm,qqm,tqm,wqm
  integer(i_kind) nlevp         ! vertical level for thinning
  integer(i_kind),dimension(nconvtype+1)::ntx

! bias and thinning variables
  logical luse
  integer(i_kind) ntmp,iout
  integer(i_kind):: pflag
  real(r_kind) crit1,timedif,xmesh,pmesh

! solar angle variables
  real(r_kind) dangl,sdgl,cdgl,tsnoon
  integer(i_kind) idayr,idaysy

  real(r_kind) time,timex,time_drift,timeobs,toff,t4dv,zeps
  real(r_kind) qtflg,tdry,rmesh,ediff,usage
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) qoe,qobcon,pwoe,pwmerr,dlnpob,ppb,poe,qmaxerr
  real(r_kind) toe,woe,errout,oelev,dlat,dlon,sstoe,dlat_earth,dlon_earth
  real(r_kind) selev,elev,stnelev
  real(r_kind) :: tsavg,sty,ff10,sfcr
  real(r_kind),dimension(nsig):: presl
  real(r_kind),dimension(nsig-1):: dpres
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),allocatable,dimension(:,:):: cdata_all,cdata_out
  integer(i_kind),allocatable,dimension(:):: isort,iloc

  real(r_double) rstation_id,qcmark_huge

  real(r_double),dimension(8):: hdr
  real(r_double),dimension(8,255):: drfdat,qcmark,obserr
  real(r_double),dimension(9,255):: obsdat
  real(r_double),dimension(8,1):: sstdat
  real(r_double),dimension(4):: satqc
  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg 
  real(r_double),dimension(1,255):: levdat
  real(r_kind),dimension(255)::plevs

  real(r_kind),dimension(255):: tvflg
  real(r_double),dimension(255,20):: tpc
  real(r_double) vtcd
  real(r_kind) bmiss/10.e10/
  
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax,u00,v00
  integer(i_kind) ntest,nvtest,iosub,ixsub,isubsub,iobsub

  integer(i_kind) kl,k1,k2
  integer(i_kind) m,ikx,itypex
  integer(i_kind) nprov
  real(r_kind) del,terrmin,werrmin,perrmin,qerrmin,pwerrmin

  character(len=8) stnid
  logical black

  integer(i_kind) idate5(5),minobs,minan
  real(r_kind) time_correction

  equivalence(r_prvstg(1,1),c_prvstg) 
  equivalence(r_sprvstg(1,1),c_sprvstg) 

  data hdstr  /'SID XOB YOB DHR TYP ELV SAID '/
  data obstr  /'POB QOB TOB ZOB UOB VOB PWO CAT PRSS' /
  data drift  /'XDR YDR HRDR                    '/
  data sststr /'MSST DBSS SST1 SSTQM SSTOE           '/
  data qcstr  /'PQM QQM TQM ZQM WQM NUL PWQ     '/
  data oestr  /'POE QOE TOE NUL WOE NUL PWE     '/
  data satqcstr  /'RFFL QIFY QIFN EEQF'/
  data prvstr /'PRVSTG'/   
  data sprvstr /'SPRVSTG'/ 
  data levstr  /'POB'/ 

  data lunin / 13 /
  data ithin / -9 /
  data rmesh / -99.999 /
  
!**************************************************************************
! added by jw for speedup
  integer(i_kind) ntb
  integer(i_kind),parameter:: mxtb=5000000           
  integer(i_kind) tab(mxtb)
! added by woollen, kistler for speedup
  integer(i_kind),parameter:: nmsgmax=10000 ! max message count
  logical*1 lmsg(nmsgmax)           ! set true when convinfo entry id found in a message
  integer(i_kind),dimension(nmsgmax):: nrep ! number of reports per message - enables skipping
  integer(i_kind) nmsg                ! message index
!**************************************************************************
! Initialize variables

  lmsg = .false.
  nreal=0
  satqc=zero
  tob = obstype == 't'
  uvob = obstype == 'uv'
  spdob = obstype == 'spd'
  psob = obstype == 'ps'
  qob = obstype == 'q'
  pwob = obstype == 'pw'
  sstob = obstype == 'sst'
  convobs = tob .or. uvob .or. spdob .or. qob
  if(tob)then
     nreal=20
  else if(uvob) then 
     nreal=20
  else if(spdob) then
     nreal=20
  else if(psob) then
     nreal=19
  else if(qob) then
     nreal=21
  else if(pwob) then
     nreal=20
  else if(sstob) then
     nreal=20
  else 
     write(6,*) ' illegal obs type in READ_PREPBUFR '
     call stop2(94)
  end if

  if(perturb_obs .and. (tob .or. psob .or. qob))nreal=nreal+1
  if(perturb_obs .and. uvob )nreal=nreal+2

  qcmark_huge = huge_i_kind


! Read in mesonet provider names from the uselist
 nprov=0
 inquire(file='mesonetuselist',exist=listexist)
 if(listexist) then
     open (88,file='mesonetuselist',form='formatted')
     do m=1,3
      read(88,*,end=135) cstring
     enddo
     nprov=0
130  continue
     nprov=nprov+1
     read(88,*,end=135) cprovider(nprov)
     goto 130
135  continue
     nprov=nprov-1
     close(88)
  endif

  if (blacklst) call blacklist_read(obstype)

!------------------------------------------------------------------------
! Open, then read date from bufr data
  call closbf(lunin)
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
  call readmg(lunin,subset,idate,iret)
  if (iret/=0) goto 900

! Time offset
  call time_4dvar(idate,toff)

! On temperature task, write out date information.  If date in prepbufr
!    file does not agree with analysis date, print message and stop program
!    execution.

  if(tob) then
     write(date,'( i10)') idate
     read (date,'(i4,3i2)') iy,im,idd,ihh
     write(6,*)'READ_PREPBUFR: bufr file date is ',idate
     IF (idate<iadatebgn.OR.idate>iadateend) THEN
            if(offtime_data) then
                write(6,*)'***READ_PREPBUFR analysis and data file date differ, but use anyway'
            else
                write(6,*)'***READ_PREPBUFR ERROR*** incompatable analysis ',&
                     'and observation date/time'
            endif
            write(6,*)'Analysis start  :',iadatebgn
            write(6,*)'Analysis end    :',iadateend
            write(6,*)'Observation time:',idate
            write(6,*)' month anal/obs ',iadate(2),im
            write(6,*)' day   anal/obs ',iadate(3),idd
            write(6,*)' hour  anal/obs ',iadate(4),ihh
            if(.not.offtime_data) call stop2(94)
     END IF
     write(6,*)'READ_PREPBUFR: bufr file date is ',idate
     write(6,*)'READ_PREPBUFR: time offset is ',toff,' hours.'

!    compute day of the year, # days in year, true solar noon for today's date

!    call w3fs13(iadate(1),iadate(2),iadate(3),idayr)  ! day of year
!    call w3fs13(iadate(1),12,31,idaysy)               ! #days in year
!    dangl  = 6.2831853_r_kind * (real(idayr) - 79._r_kind)/real(idaysy)
!    sdgl   = sin(dangl)
!    cdgl   = cos(dangl)
!    tsnoon = -.030_r_kind*sdgl-.120_r_kind*cdgl+.330_r_kind*sdgl*cdgl+ &
!         .0016_r_kind*sdgl**2-.0008_r_kind

  end if


  ntread=1
  ntx(ntread)=izero
  do nc=1,nconvtype
    if(trim(ioctype(nc)) == trim(obstype) .and. abs(icuse(nc)) <= 1)then
      ithin=ithin_conv(nc)
      if(ithin > 0)then
        ntread=ntread+1
        ntx(ntread)=nc
      end if
    end if
  end do

! Obtain program code (VTCD) associated with "VIRTMP" step
  call ufbqcd(lunin,'VIRTMP',vtcd)

  maxobs=izero
  tab=-999
  nmsg=1
  ntb = 0
  nrep(1)=0
  loop_report: do 
     call readsb(lunin,iret)
       if(iret/=0) then
         call readmg(lunin,subset,idate,iret)
         if(iret/=0) then
           exit loop_report ! end of file
         else
           nmsg=nmsg+1
           nrep(nmsg) = 0
           if (nmsg>nmsgmax) then
             write(6,*)'READ_PREPBUFR: messages exceed maximum ',nmsgmax
             call stop2(50)
           endif
         endif
         cycle loop_report
       else
         nrep(nmsg)=nrep(nmsg)+1  ! count reports per message 
       endif
!    Extract type, date, and location information
       call ufbint(lunin,hdr,7,1,iret,hdstr)
       kx=hdr(5)

!      If running in 2d-var (surface analysis) mode, check to see if observation
!      is surface type.  If not, read next observation report from bufr file
       if ( twodvar_regional .and. &
            (kx<180 .or. kx>289 .or. (kx>189 .and. kx<280)) ) cycle loop_report

       iobsub = 0           ! temporary until put in bufr file
       if(kx == 243 .or. kx == 253 .or. kx == 254) iobsub = hdr(7)

       ntb = ntb+1
       if (ntb>mxtb) then
         write(6,*)'READ_PREPBUFR: reports exceed maximum ',mxtb
         call stop2(50)
       endif
       ikx=-999

       loop_convinfo_test: do nc=1,nconvtype
         if (kx /= ictype(nc)) cycle loop_convinfo_test
         if (trim(ioctype(nc)) /= trim(obstype))cycle loop_convinfo_test
         if(icsubtype(nc) == iobsub) then
            ikx=nc
            call ufbint(lunin,levdat,1,255,levs,levstr)
            maxobs=maxobs+max(1,levs)
            exit loop_convinfo_test
         else
            ixsub=icsubtype(nc)/10
            iosub=iobsub/10
            isubsub=icsubtype(nc)-ixsub*10
            if(ixsub == iosub .and. isubsub == 0) then
               ikx=nc
               call ufbint(lunin,levdat,1,255,levs,levstr)
               maxobs=maxobs+max(1,levs)
               exit loop_convinfo_test
            else if (icsubtype(nc) == 0 .and. (kx == 243 .or. kx == 253 .or. kx == 254) ) then
              if (iobsub /= 55 .and. iobsub /= 56) then
                 ikx=nc
                 call ufbint(lunin,levdat,1,255,levs,levstr)
                 maxobs=maxobs+max(1,levs)
                 exit loop_convinfo_test
              end if
            endif
         end if

       end do loop_convinfo_test
       tab(ntb)=ikx
       if(ikx < izero) cycle loop_report
       lmsg(nmsg) = .true.
  enddo loop_report
  write(6,*)'READ_PREPBUFR: messages/reports = ',nmsg,'/',ntb,' ntread = ',ntread

!------------------------------------------------------------------------

! loop over convinfo file entries; operate on matches
  
  allocate(cdata_all(nreal,maxobs),isort(maxobs))
  nread=0
  ntest=0
  nvtest=0
  nchanl=0
  ilon=2
  ilat=3
  loop_convinfo: do nx=1, ntread

    use_all = .true.
    ithin=0
    if(nx > 1) then
      nc=ntx(nx)
      ithin=ithin_conv(nc)
      if (ithin > 0 ) then
        rmesh=rmesh_conv(nc)
        pmesh=pmesh_conv(nc)
        use_all = .false.
        if(pmesh > zero) then
          pflag=1
          nlevp=r1200/pmesh
        else
          pflag=0
          nlevp=nsig
        endif
        xmesh=rmesh

        call make3grids(xmesh,pmesh,nlevp)

        if (.not.use_all) then
          allocate(presl_thin(nlevp))
          if (pflag==1) then
            do k=1,nlevp
              presl_thin(k)=(r1200-(k-1)*pmesh)*one_tenth
            enddo
          endif
        endif
     
        write(6,*)'READ_PREPBUFR: obstype,ictype(nc),rmesh,pflag,nlevp,pmesh=',&
            trim(ioctype(nc)),ictype(nc),rmesh,pflag,nlevp,pmesh
      endif
    endif

    call closbf(lunin)
    open(lunin,file=infile,form='unformatted')
    call openbf(lunin,'IN',lunin)
    call datelen(10)

!   Big loop over prepbufr file	

    ntb = 0
    nmsg = 0
    loop_readsb: do
!      use msg lookup table to decide which messages to skip
!      use report id lookup table to only process matching reports
       call readsb(lunin,iret) ! read next report of current message
       if(iret/=0) then ! read next message 
         loop_msg: do
           call readmg(lunin,subset,idate,iret)
           if(iret/=0) exit loop_readsb ! end of file
           nmsg=nmsg+1
           if (lmsg(nmsg)) then
             exit loop_msg ! match found, break message loop
           else
             ntb=ntb+nrep(nmsg) ! no reports this mesage, skip ahead report count
           endif
         enddo loop_msg
         cycle loop_readsb
       endif
       ntb = ntb+1
       nc=tab(ntb)
       if(nc < 0) cycle loop_readsb
       ithin=ithin_conv(nc)
       if(ithin > 0)then
         if(nx == 1)then
           cycle loop_readsb
         else
           if(nc /= ntx(nx)) cycle loop_readsb
         end if
       else
         if(nx > 1)cycle loop_readsb
       end if
 
!      For the satellite wind to get quality information
       if( subset == 'SATWND' ) then
         id=ictype(nc)
         if( id ==243 .or. id == 253 .or. id ==254 ) then
             call ufbint(lunin,satqc,4,1,iret,satqcstr)
             if(satqc(3) <  85.0) cycle loop_readsb   ! QI w/o fcst (su's setup
!!           if(satqc(2) <= 80.0) cycle loop_readsb   ! QI w/ fcst (old prepdata)
         endif
       endif
                 
!      Extract type, date, and location information
       call ufbint(lunin,hdr,7,1,iret,hdstr)
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
             disterr=acos(cdist)*rad2deg
             disterrmax=max(disterrmax,disterr)
          end if
          if(outside) cycle loop_readsb   ! check to see if outside regional domain
       else
          dlat = dlat_earth
          dlon = dlon_earth
          call grdcrd(dlat,1,rlats,nlat,1)
          call grdcrd(dlon,1,rlons,nlon,1)
       endif

!------------------------------------------------------------------------

       if(offtime_data) then

!        in time correction for observations to account for analysis
!                 time being different from obs file time.
         write(date,'( i10)') idate
         read (date,'(i4,3i2)') iy,im,idd,ihh
         idate5(1)=iy
         idate5(2)=im
         idate5(3)=idd
         idate5(4)=ihh
         idate5(5)=0
         call w3fs21(idate5,minobs)    !  obs ref time in minutes relative to historic date
         idate5(1)=iadate(1)
         idate5(2)=iadate(2)
         idate5(3)=iadate(3)
         idate5(4)=iadate(4)
         idate5(5)=0
         call w3fs21(idate5,minan)    !  analysis ref time in minutes relative to historic date

!        Add obs reference time, then subtract analysis time to get obs time relative to analysis

         time_correction=float(minobs-minan)/60._r_kind
            
       else
         time_correction=zero
       end if

       timeobs=real(real(hdr(4),4),8)
       t4dv=timeobs + toff
       zeps=1.0e-8_r_kind
       if (t4dv<zero  .and.t4dv>      -zeps) t4dv=zero
       if (t4dv>winlen.and.t4dv<winlen+zeps) t4dv=winlen
       t4dv=t4dv + time_correction
       time=timeobs + time_correction
       kx=hdr(5)

!      If ASCAT data, determine primary surface type.  If not open sea,
!      skip this observation.  This check must be done before thinning.
       if (kx==290 .or. kx==289 .or. kx==285) then
          call deter_sfc_type(dlat_earth,dlon_earth,t4dv,isflg,tsavg)
          if (isflg /= 0) cycle loop_readsb
          if (tsavg <= 273.0_r_kind) cycle loop_readsb
       endif


!      For mesonet winds, check to see if mesonet provider is in the
!      uselist. If not, read next observation report from bufr file
       if (kx == 288 .and. listexist) then
          call ufbint(lunin,r_prvstg,1,1,iret,prvstr)
          call ufbint(lunin,r_sprvstg,1,1,iret,sprvstr)

          lprov=.false.
          do m=1,nprov
           if (trim(c_prvstg//c_sprvstg) == trim(cprovider(m))) then
            lprov=.true.
           endif
          enddo
          if (.not.lprov) cycle loop_readsb
!         write(6,*)'READ_PREPBUFR:  kx,prvstg=',kx,c_prvstg
!         write(6,*)'READ_PREPBUFR:  kx,sprvstg=',kx,c_sprvstg
       endif

!    Balloon drift information available for these data
     driftl=kx==120.or.kx==220.or.kx==221

     if (l4dvar) then
       if ((t4dv<zero.OR.t4dv>winlen) .and. .not.driftl) cycle loop_readsb ! outside time window
     else
       if((real(abs(time)) > real(ctwind(nc)) .or. real(abs(time)) > real(twindin)) &
               .and. .not. driftl)cycle loop_readsb ! outside time window
     endif

     timex=time
     
!    Extract data information on levels
     call ufbint(lunin,obsdat,9,255,levs,obstr)
     call ufbint(lunin,qcmark,8,255,levs,qcstr)
     call ufbint(lunin,obserr,8,255,levs,oestr)
     nread=nread+levs
     if(uvob)nread=nread+levs
     if(sstob)then 
       sstdat=1.e11
       call ufbint(lunin,sstdat,8,1,levs,sststr)
     end if


!    If available, get obs errors from error table
     if(oberrflg)then

!      Set lower limits for observation errors
       terrmin=r0_5
       werrmin=one
       perrmin=r0_5
       qerrmin=one_tenth
       pwerrmin=one

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

!    Check for blacklisting of station ID
     black = .false.
     if (blacklst .and. ibcnt > 0) then
        stnid = transfer(hdr(1),stnid)
        do i = 1,ibcnt
           if( kx == blkkx(i) .and. stnid == blkstns(i) ) then
              black = .true.
              write(6,*)'READ_PREPBUFR: blacklist station ',stnid, &
                   'for obstype ',trim(obstype),' and kx=',kx
           endif
        enddo
     endif

!    Loop over levels       
     do k=1,levs
       do i=1,8
          qcmark(i,k) = min(qcmark(i,k),qcmark_huge)
       end do

       if (kx == id_bias_ps) then
         plevs(k)=one_tenth*obsdat(1,k)+conv_bias_ps   ! convert mb to cb
       else
         plevs(k)=one_tenth*obsdat(1,k)   ! convert mb to cb
       endif
       if (kx == 290) plevs(k)=101.0_r_kind  ! Assume 1010 mb = 101.0 cb
       pqm(k)=nint(qcmark(1,k))
       qqm(k)=nint(qcmark(2,k))
       tqm(k)=nint(qcmark(3,k))
       wqm(k)=nint(qcmark(5,k))
     end do

!    If temperature ob, extract information regarding virtual
!    versus sensible temperature
     if(tob) then
        call ufbevn(lunin,tpc,1,255,20,levs,'TPC')
        do k=1,levs
           tvflg(k)=one                               ! initialize as sensible
           do j=1,20
              if (tpc(k,j)==vtcd) tvflg(k)=zero       ! reset flag if virtual
              if (tpc(k,j)>=bmiss) exit               ! end of stack
           end do
        end do
     end if

     rstation_id=hdr(1)
     stnelev=hdr(6)
     if(.not. driftl .and. (levs > 1 .or. ithin > 0))then
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
           presl(kk)=w00*prsl_full(klat1 ,klon1 ,kk) +  &
                     w10*prsl_full(klatp1,klon1 ,kk) + &
                     w01*prsl_full(klat1 ,klonp1,kk) + &
                     w11*prsl_full(klatp1,klonp1,kk)
         end do

!       Compute depth of guess pressure layersat observation location
         if (.not.twodvar_regional .and. levs > 1) then
           do kk=1,nsig-1
             dpres(kk)=presl(kk)-presl(kk+1)
           end do
         endif
     end if
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
           if (k==1) sstq=nint(min(sstdat(4,k),qcmark_huge))
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
           cat=nint(min(obsdat(8,k),qcmark_huge))
           if ( cat /=0 ) cycle loop_k_levs
           if ( obsdat(1,k)< r500) qm=100
           zqm=nint(qcmark(4,k))
           if (zqm>=lim_zqm .and. zqm/=15 .and. zqm/=9) qm=9
        endif

!       if(convobs .and. pqm(k) >=lim_qm .and. qm/=15 .and. qm/=9 )cycle loop_k_levs
!       if(qm >=lim_qm .and. qm /=15 .and. qm /=9)cycle loop_k_levs
        if(qm > 15 .or. qm < 0) cycle loop_k_levs



!       Set usage variable              
        usage = 0.
        if(icuse(nc) <= 0)usage=100.
        if(qm == 15 .or. qm == 9)usage=100.
        if(ncnumgrp(nc) > 0 )then                     ! cross validation on
          if(mod(ndata+1,ncnumgrp(nc))== ncgroup(nc)-1)usage=ncmiter(nc)
        end if
        if(qm >=lim_qm )usage=101.
        if(convobs .and. pqm(k) >=lim_qm )usage=102.


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
           timeobs = real(real(drfdat(3,k),4),8)
           time_drift = timeobs + time_correction
           if (abs(time_drift-time)>four) time_drift = time

!          Check to see if the time is outside range
           if (l4dvar) then
             t4dv=toff+time_drift
             if (t4dv<zero .or. t4dv>winlen) then
                t4dv=toff+timex
                if (t4dv<zero .or. t4dv>winlen) CYCLE LOOP_K_LEVS
             end if
           else
             if(abs(time_drift) > ctwind(nc) .or. abs(time_drift) > twindin)then
                time_drift=timex
                if(abs(timex) > ctwind(nc) .or. abs(timex) > twindin) CYCLE LOOP_K_LEVS
             end if
             t4dv = toff + time_drift
           endif
           
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

           if(levs > 1 .or. ithin > 0)then
!           Interpolate guess pressure profile to observation location
            klon1= int(dlon);  klat1= int(dlat)
            dx   = dlon-klon1; dy   = dlat-klat1
            dx1  = one-dx;     dy1  = one-dy
            w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy
 
            klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
            if (klon1==0) klon1=nlon
            klatp1=min(nlat,klat1+1); klonp1=klon1+1
            if (klonp1==nlon+1) klonp1=1

            do kk=1,nsig
               presl(kk)=w00*prsl_full(klat1 ,klon1 ,kk) +  &
                         w10*prsl_full(klatp1,klon1 ,kk) + &
                         w01*prsl_full(klat1 ,klonp1,kk) + &
                         w11*prsl_full(klatp1,klonp1,kk)
            end do

!        Compute depth of guess pressure layersat observation location
            if (.not.twodvar_regional .and. levs > 1) then
              do kk=1,nsig-1
                dpres(kk)=presl(kk)-presl(kk+1)
              end do
            endif
           end if
        end if

!       Extract pressure level and quality marks
        dlnpob=log(plevs(k))  ! ln(pressure in cb)


!       Special block for data thinning - if requested
        if (ithin > 0) then
           ntmp=ndata  ! counting moved to map3gridS
           
!          Set data quality index for thinning
           if (l4dvar) then
              timedif = zero
           else
              timedif=abs(t4dv-toff)
           endif
           if(kx == 243 .or. kx == 253 .or. kx ==254) then
              crit1 = timedif/r6+half + four*(one-satqc(3)/r100)*r3_33
           else
              crit1 = timedif/r6+half
           endif

           if (pflag==0) then
              do kk=1,nsig
                 presl_thin(kk)=presl(kk)
              end do
           endif

           call map3grids(pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                plevs(k),crit1,ithin,ndata,iout,luse)

           if (ndata > ntmp) then
              nodata=nodata+1
              if(uvob)nodata=nodata+1
           endif
           if (.not. luse) cycle loop_readsb

        else
           ndata=ndata+1
           nodata=nodata+1
           if(uvob)nodata=nodata+1
           iout=ndata
        endif

        if(ndata > maxobs) then
           write(6,*)'READ_PREPBUFR:  ***WARNING*** ndata > maxobs for ',obstype
           ndata = maxobs
        end if


! Get information from surface file necessary for conventional data here
        call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr)


!       Temperature
        if(tob) then
           ppb=obsdat(1,k)
           call errormod(pqm,tqm,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
           toe=obserr(3,k)*errout
           qtflg=tvflg(k) 
           if (inflate_error) toe=toe*r1_2
           if(ppb < r100)toe=toe*r1_2
           cdata_all(1,iout)=toe                     ! temperature error
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)

           if (kx == id_bias_t) then
              cdata_all(5,iout)=obsdat(3,k)+t0c+conv_bias_t   ! temperature ob.+bias
           else
              cdata_all(5,iout)=obsdat(3,k)+t0c               ! temperature ob.
           endif

           cdata_all(6,iout)=rstation_id             ! station id
           cdata_all(7,iout)=t4dv                    ! time
           cdata_all(8,iout)=nc                      ! type
           cdata_all(9,iout)=qtflg                   ! qtflg (virtual temperature flag)
           cdata_all(10,iout)=tqm(k)                 ! quality mark
           cdata_all(11,iout)=obserr(3,k)            ! original obs error
           cdata_all(12,iout)=usage                  ! usage parameter
           cdata_all(13,iout)=idomsfc                ! dominate surface type
           cdata_all(14,iout)=tsavg                  ! skin temperature
           cdata_all(15,iout)=ff10                   ! 10 meter wind factor
           cdata_all(16,iout)=sfcr                   ! surface roughness
           cdata_all(17,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(18,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(19,iout)=stnelev                ! station elevation (m)
           cdata_all(20,iout)=obsdat(4,k)            ! observation height (m)
           if(perturb_obs)cdata_all(21,iout)=ran01dom()*perturb_fact ! t perturbation

!       Winds 
        else if(uvob) then 
           call errormod(pqm,wqm,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
           woe=obserr(5,k)*errout
           if (inflate_error) woe=woe*r1_2
           if(obsdat(1,k) < r50)woe=woe*r1_2
           selev=stnelev
           oelev=obsdat(4,k)
           if(kx >= 280 .and. kx < 300 )then
             oelev=r10+selev
             if (kx == 280) oelev=r20+selev
             if (kx == 282) oelev=r20+selev
             if (kx == 285 .or. kx == 289 .or. kx == 290) then
                oelev=selev
                selev=zero
             endif
           else
             if((kx >= 221 .and.  kx <= 229) &
                 .and. selev >= oelev) oelev=r10+selev
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
           
           cdata_all(1,iout)=woe                     ! wind error
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,iout)=oelev                   ! height of observation
           cdata_all(6,iout)=uob                     ! u obs
           cdata_all(7,iout)=vob                     ! v obs
           cdata_all(8,iout)=rstation_id             ! station id
           cdata_all(9,iout)=t4dv                    ! time
           cdata_all(10,iout)=nc                     ! type
           cdata_all(11,iout)=selev                  ! station elevation
           cdata_all(12,iout)=wqm(k)                 ! quality mark
           cdata_all(13,iout)=obserr(5,k)            ! original obs error
           cdata_all(14,iout)=usage                  ! usage parameter
           cdata_all(15,iout)=idomsfc                ! dominate surface type
           cdata_all(16,iout)=tsavg                  ! skin temperature
           cdata_all(17,iout)=ff10                   ! 10 meter wind factor
           cdata_all(18,iout)=sfcr                   ! surface roughness
           cdata_all(19,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(20,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           if(perturb_obs)then
              cdata_all(21,iout)=ran01dom()*perturb_fact ! u perturbation
              cdata_all(22,iout)=ran01dom()*perturb_fact ! v perturbation
           endif

        else if(spdob) then 
           woe=obserr(5,k)
           if (inflate_error) woe=woe*r1_2
           elev=r20

           cdata_all(1,iout)=woe                     ! wind error
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,iout)=obsdat(5,k)             ! u obs
           cdata_all(6,iout)=obsdat(6,k)             ! v obs
           cdata_all(7,iout)=rstation_id             ! station id
           cdata_all(8,iout)=t4dv                    ! time
           cdata_all(9,iout)=nc                      ! type
           cdata_all(10,iout)=elev                   ! elevation of observation
           cdata_all(11,iout)=wqm(k)                 ! quality mark
           cdata_all(12,iout)=obserr(5,k)            ! original obs error
           cdata_all(13,iout)=usage                  ! usage parameter
           cdata_all(14,iout)=idomsfc                ! dominate surface type
           cdata_all(15,iout)=tsavg                  ! skin temperature
           cdata_all(16,iout)=ff10                   ! 10 meter wind factor
           cdata_all(17,iout)=sfcr                   ! surface roughness
           cdata_all(18,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(19,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(20,iout)=stnelev                ! station elevation (m)

!       Surface pressure 
        else if(psob) then
 
           poe=obserr(1,k)*one_tenth                  ! convert from mb to cb
           if (inflate_error) poe=poe*r1_2
           cdata_all(1,iout)=poe                     ! surface pressure error (cb)
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude

           cdata_all(4,iout)=exp(dlnpob)             ! pressure (in cb)
		   
           cdata_all(5,iout)=obsdat(4,k)             ! surface height
           cdata_all(6,iout)=obsdat(3,k)+t0c         ! surface temperature
           cdata_all(7,iout)=rstation_id             ! station id
           cdata_all(8,iout)=t4dv                    ! time
           cdata_all(9,iout)=nc                      ! type
           cdata_all(10,iout)=pqm(k)                 ! quality mark
           cdata_all(11,iout)=obserr(1,k)*one_tenth  ! original obs error (cb)
           cdata_all(12,iout)=usage                  ! usage parameter
           cdata_all(13,iout)=idomsfc                ! dominate surface type
           cdata_all(14,iout)=tsavg                  ! skin temperature
           cdata_all(15,iout)=ff10                   ! 10 meter wind factor
           cdata_all(16,iout)=sfcr                   ! surface roughness
           cdata_all(17,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(18,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(19,iout)=stnelev                ! station elevation (m)
           if(perturb_obs)cdata_all(20,iout)=ran01dom()*perturb_fact ! ps perturbation

!       Specific humidity 
        else if(qob) then
           qmaxerr=emerr
           call errormod(pqm,qqm,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
           qoe=obserr(2,k)*one_tenth*errout
           if (inflate_error) then
              qmaxerr=emerr*r0_7; qoe=qoe*r1_2
           end if
           qobcon=obsdat(2,k)*convert
           tdry=r999
           if (tqm(k)<lim_tqm) tdry=(obsdat(3,k)+t0c)/(one+fv*qobcon)
           cdata_all(1,iout)=qoe                     ! q error   
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,iout)=qobcon                  ! q ob
           cdata_all(6,iout)=rstation_id             ! station id
           cdata_all(7,iout)=t4dv                    ! time
           cdata_all(8,iout)=nc                      ! type
           cdata_all(9,iout)=qmaxerr                 ! q max error
           cdata_all(10,iout)=tdry                   ! dry temperature (obs is tv)
           cdata_all(11,iout)=qqm(k)                 ! quality mark
           cdata_all(12,iout)=obserr(2,k)*one_tenth  ! original obs error
           cdata_all(13,iout)=usage                  ! usage parameter
           cdata_all(14,iout)=idomsfc                ! dominate surface type
           cdata_all(15,iout)=tsavg                  ! skin temperature
           cdata_all(16,iout)=ff10                   ! 10 meter wind factor
           cdata_all(17,iout)=sfcr                   ! surface roughness
           cdata_all(18,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(19,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(20,iout)=stnelev                ! station elevation (m)
           cdata_all(21,iout)=obsdat(4,k)            ! observation height (m)
           if(perturb_obs)cdata_all(22,iout)=ran01dom()*perturb_fact ! q perturbation

!       Total precipitable water (ssm/i)
        else if(pwob) then

           pwoe=obserr(7,k)
           pwmerr=pwoe*three
           cdata_all(1,iout)=pwoe                    ! pw error
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=obsdat(7,k)             ! pw obs
           cdata_all(5,iout)=rstation_id             ! station id
           cdata_all(6,iout)=t4dv                    ! time
           cdata_all(7,iout)=nc                      ! type
           cdata_all(8,iout)=pwmerr                  ! pw max error
           cdata_all(9,iout)=pwq                     ! quality mark
           cdata_all(10,iout)=obserr(7,k)            ! original obs error
           cdata_all(11,iout)=usage                  ! usage parameter
           cdata_all(12,iout)=idomsfc                ! dominate surface type
           cdata_all(13,iout)=tsavg                  ! skin temperature
           cdata_all(14,iout)=ff10                   ! 10 meter wind factor
           cdata_all(15,iout)=sfcr                   ! surface roughness
           cdata_all(16,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(17,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(18,iout)=stnelev                ! station elevation (m)
           cdata_all(19,iout)=obsdat(1,k)            ! observation pressure (hPa)
           cdata_all(20,iout)=obsdat(4,k)            ! observation height (m)


!       Conventional sst observations
        else if(sstob) then

!          Locate the observation on the analysis grid.  Get land/sea/ice
!          mask at nearest analysis grid points.

           sstoe=r0_75
!          sstoe=sstdat(5,k)

           cdata_all(1,iout)=sstoe                   ! sst error
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=sstdat(3,k)             ! sst obs
           cdata_all(5,iout)=rstation_id             ! station id
           cdata_all(6,iout)=t4dv                    ! time
           cdata_all(7,iout)=nc                      ! type
           cdata_all(8,iout)=sstoe*three             ! pw max error
           cdata_all(9,iout)=sstdat(2,k)             ! depth of measurement
           cdata_all(10,iout)=sstdat(1,k)            ! measurement type
           cdata_all(11,iout)=sstq                   ! quality mark
           cdata_all(12,iout)=sstdat(5,k)            ! original obs error
           cdata_all(13,iout)=usage                  ! usage parameter
           cdata_all(14,iout)=idomsfc                ! dominate surface type
           cdata_all(15,iout)=tsavg                  ! skin temperature
           cdata_all(16,iout)=ff10                   ! 10 meter wind factor
           cdata_all(17,iout)=sfcr                   ! surface roughness
           cdata_all(18,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(19,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(20,iout)=stnelev                ! station elevation (m)


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
        isort(iout)=ntb*1000+k

!
!    End k loop over levs
     end do  LOOP_K_LEVS

!
!   End of bufr read loop
  enddo loop_readsb
! Close unit to bufr file
  call closbf(lunin)

! Deallocate arrays used for thinning data
  if (.not.use_all) then
     deallocate(presl_thin)
     call del3grids
  endif

! Normal exit

enddo loop_convinfo! loops over convinfo entry matches

! Write header record and data to output file for further processing
900 continue
  allocate(iloc(ndata))
  do i=1,ndata
    iloc(i)=i
  end do
  sort:do i=1,ndata
     asort=.true.
     do j=1,ndata-1
        if(isort(j+1) < isort(j))then
            itemp=isort(j+1)
            isort(j+1)=isort(j)
            isort(j)=itemp
            itemp=iloc(j+1)
            iloc(j+1)=iloc(j)
            iloc(j)=itemp
            asort=.false.
         end if
     end do
     if(asort) exit sort
  end do sort
  allocate(cdata_out(nreal,ndata))
  do i=1,ndata
    itx=iloc(i)
    do k=1,nreal
      cdata_out(k,i)=cdata_all(k,itx)
    end do
  end do
  deallocate(iloc,isort,cdata_all)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) cdata_out

  deallocate(cdata_out)


  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_PREPBUFR:  ',&
       'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_PREPBUFR:  ',&
       'nvtest,vdisterrmax=',ntest,vdisterrmax


! Generate stats on regional wind rotation
  if (regional .and. uvob) call check_rotate_wind('read_prepbufr')


  if (ndata == 0) then 
	call closbf(lunin)
	write(6,*)'READ_PREPBUFR:  closbf(',lunin,')'
  endif

  close(lunin)

! End of routine
  return

end subroutine read_prepbufr
