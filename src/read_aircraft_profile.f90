subroutine read_aircraft_profile(nread,ndata,nodata,infile,obstype,lunout,twindin,sis,&
     prsl_full)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_aircraft_profile   read aircraft temperature obs from prepbufr profile file
!   prgmmr: yanqiu zhu          org: np22                date: 2013-06-01
!
! abstract:  This routine reads conventional aircraft temperature data in 
!            the prepbufr profile file.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2013-06-01  zhu  - modified from read_prepbufr
!                    - match aircraft obs with temperature bias file 
!                    - add new tail number info if there is any
!
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
  use kinds, only: r_single,r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,fv,t0c,half,&
      two,three,four,rad2deg,tiny_r_kind,huge_r_kind,huge_i_kind,&
      r60inv,r10,r100,r2000
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
      tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
      rlats,rlons
  use convinfo, only: nconvtype,ctwind, &
      ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype, &
      ithin_conv,rmesh_conv,pmesh_conv, &
      id_bias_ps,id_bias_t,conv_bias_ps,conv_bias_t,use_prepb_satwnd

  use obsmod, only: iadate,oberrflg,perturb_obs,perturb_fact,ran01dom
  use obsmod, only: blacklst,offtime_data,bmiss
  use aircraftinfo, only: aircraft_t_bc,ntail,taillist,idx_tail,npredt,predt,ntail_update,max_tail
  use converr,only: etabl
  use gsi_4dvar, only: l4dvar,time_4dvar,winlen
  use qcmod, only: errormod,noiqc
  use convthin, only: make3grids,map3grids,del3grids,use_all
  use blacklist, only : blacklist_read,blacklist_destroy
  use blacklist, only : blkstns,blkkx,ibcnt
  use ndfdgrids,only: init_ndfdgrid,destroy_ndfdgrid,relocsfcob,adjust_error
  use jfunc, only: tsensible
  use deter_sfc_mod, only: deter_sfc2
  use aircraftobsqc, only: init_aircraft_rjlists,get_aircraft_usagerj,&
                           destroy_aircraft_rjlists

  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=*)                      ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  real(r_kind)                          ,intent(in   ) :: twindin
  real(r_kind),dimension(nlat,nlon,nsig),intent(in   ) :: prsl_full

! Declare local parameters
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r1_2 = 1.2_r_kind
  real(r_kind),parameter:: r6   = 6.0_r_kind
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r500 = 500.0_r_kind
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: r0_01_bmiss=r0_01*bmiss
  character(80),parameter:: cspval= '88888888'

  integer(i_kind),parameter:: mxtb=5000000
  integer(i_kind),parameter:: nmsgmax=10000 ! max message count

! Declare local variables
  logical tob
  logical outside,convobs,inflate_error
  logical luse,ithinp,windcorr
  logical patch_fog
  logical aircraftobs
  logical,allocatable,dimension(:,:):: lmsg           ! set true when convinfo entry id found in a message

  character(40) drift,hdstr,qcstr,oestr,levstr,hdstr2
  character(80) obstr
  character(10) date
  character(8) subset
  character(8) prvstr,sprvstr     
  character(8) c_prvstg,c_sprvstg 
  character(8) c_station_id
  character(1) sidchr(8)
  character(8) stnid
  character(10) aircraftstr

  integer(i_kind) ireadmg,ireadsb,icntpnt,icntpnt2,icount,iiout
  integer(i_kind) lunin,i,maxobs,j,idomsfc,itemp,it29
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nc,nx,id,isflg,ntread,itx,ii,ncsave
  integer(i_kind) ihh,idd,idate,iret,im,iy,k,levs
  integer(i_kind) kx,kx0,nreal,nchanl,ilat,ilon,ithin
  integer(i_kind) cat,zqm,sstq,qm,lim_qm
  integer(i_kind) nlevp         ! vertical level for thinning
  integer(i_kind) ntmp,iout
  integer(i_kind) pflag
  integer(i_kind) ntest,nvtest,iosub,ixsub,isubsub,iobsub
  integer(i_kind) kl,k1,k2
  integer(i_kind) itypex
  integer(i_kind) minobs,minan
  integer(i_kind) ntb,ntmatch,ncx
  integer(i_kind) nmsg                ! message index
  integer(i_kind) idx                 ! order index of aircraft temperature bias 
  integer(i_kind) tab(mxtb,3)
  integer(i_kind),dimension(5):: idate5
  integer(i_kind),dimension(nmsgmax):: nrep
  integer(i_kind),dimension(255):: pqm,qqm,tqm,wqm
  integer(i_kind),dimension(nconvtype)::ntxall
  integer(i_kind),dimension(nconvtype+1)::ntx
  integer(i_kind),allocatable,dimension(:):: isort,iloc

  real(r_kind) time,timex,time_drift,timeobs,toff,t4dv,zeps
  real(r_kind) qtflg,tdry,rmesh,ediff,usage
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) qoe,qobcon,pwoe,pwmerr,dlnpob,ppb,poe,qmaxerr
  real(r_kind) toe,woe,errout,oelev,dlat,dlon,sstoe,dlat_earth,dlon_earth
  real(r_kind) selev,elev,stnelev
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax,u00,v00
  real(r_kind) del,terrmin,werrmin,perrmin,qerrmin,pwerrmin
  real(r_kind) tsavg,ff10,sfcr,zz
  real(r_kind) crit1,timedif,xmesh,pmesh
  real(r_kind) time_correction
  real(r_kind),dimension(nsig):: presl
  real(r_kind),dimension(nsig-1):: dpres
  real(r_kind),dimension(255)::plevs
  real(r_kind),dimension(255):: tvflg
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),allocatable,dimension(:,:):: cdata_all,cdata_out

  real(r_double) rstation_id,qcmark_huge
  real(r_double) vtcd
  real(r_double),dimension(8):: hdr
  real(r_double),dimension(8,255):: drfdat,qcmark,obserr
  real(r_double),dimension(11,255):: obsdat
  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg 
  real(r_double),dimension(1,255):: levdat
  real(r_double),dimension(255,20):: tpc
  real(r_double),dimension(2,255,20):: tobaux
  real(r_double),dimension(2,255):: aircraftwk

!  equivalence to handle character names
  equivalence(r_prvstg(1,1),c_prvstg) 
  equivalence(r_sprvstg(1,1),c_sprvstg) 
  equivalence(rstation_id,c_station_id)
  equivalence(rstation_id,sidchr)

!  data statements
  data hdstr  /'SID XOB YOB DHR TYP ELV SAID T29'/
  data hdstr2 /'TYP SAID T29 SID'/
  data obstr  /'POB QOB TOB ZOB UOB VOB PWO MXGS HOVI CAT PRSS' /
  data drift  /'XDR YDR HRDR                    '/
  data qcstr  /'PQM QQM TQM ZQM WQM NUL PWQ     '/
  data oestr  /'POE QOE TOE NUL WOE NUL PWE     '/
  data prvstr /'PRVSTG'/   
  data sprvstr /'SPRVSTG'/ 
  data levstr  /'POB'/ 
  data aircraftstr /'POAF IALR'/           ! phase of aircraft flight and vertical velocity

  data lunin / 13 /
  data ithin / -9 /
  data rmesh / -99.999_r_kind /

! Initialize variables

  pflag=0                  !  dparrish debug compile run flags pflag as not defined ???????????
  nreal=0
  tob = obstype == 't'
  convobs = tob 
  if(tob)then
     nreal=24
  end if

!  Set qc limits based on noiqc flag
  if (noiqc) then
     lim_qm=8
  else
     lim_qm=4
  endif

  if (tob .and. aircraft_t_bc) nreal=nreal+3
  if(perturb_obs .and. tob)nreal=nreal+1

  qcmark_huge = huge_i_kind

  if (blacklst) call blacklist_read(obstype)

!------------------------------------------------------------------------
! Open, then read date from bufr data
  call closbf(lunin)
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)

  ntread=1
  ntmatch=0
  ntx(ntread)=0
  ntxall=0
  do nc=1,nconvtype
     if(trim(ioctype(nc)) == trim(obstype))then
       if(.not.use_prepb_satwnd .and. trim(ioctype(nc)) == 'uv' .and. ictype(nc) >=241 &
          .and. ictype(nc) <260) then 
          cycle
       else
           ntmatch=ntmatch+1
           ntxall(ntmatch)=nc
       endif
     end if
     if(trim(ioctype(nc)) == trim(obstype) .and. abs(icuse(nc)) <= 1)then
        if(.not.use_prepb_satwnd .and. trim(ioctype(nc)) == 'uv' .and. ictype(nc) >=241 &
            .and. ictype(nc) <260) then
            cycle
        else
           ithin=ithin_conv(nc)
           if(ithin > 0)then
              ntread=ntread+1
              ntx(ntread)=nc
           end if
        endif
     end if
  end do
  if(ntmatch == 0)then
     write(6,*) ' no matching obstype found in obsinfo ',obstype
     return
  end if

  allocate(lmsg(nmsgmax,ntread))
  lmsg = .false.
  maxobs=0
  tab=0
  nmsg=0
  nrep=0
  ntb = 0
  msg_report: do while (ireadmg(lunin,subset,idate) == 0)
     if(.not. (trim(subset)=='AIRCFT' .or. trim(subset)=='AIRCAR')) cycle msg_report
!    Time offset
     if(nmsg == 0) call time_4dvar(idate,toff)
     nmsg=nmsg+1
     if (nmsg>nmsgmax) then
        write(6,*)'read_aircraft_profile: messages exceed maximum ',nmsgmax
        call stop2(50)
     endif
     loop_report: do while (ireadsb(lunin) == 0)
        ntb = ntb+1
        nrep(nmsg)=nrep(nmsg)+1
        if (ntb>mxtb) then
           write(6,*)'read_aircraft_profile: reports exceed maximum ',mxtb
           call stop2(50)
        endif

!       Extract type information
        call ufbint(lunin,hdr,4,1,iret,hdstr2)
        kx0=hdr(1)
        kx=130
        if (kx0==431 .or. kx0==531) kx=131
        if (kx0==433 .or. kx0==533) kx=133
        if (kx==130) cycle loop_report

! temporary specify iobsub until put in bufr file
        iobsub = 0                                                  

!       Check for blacklisting of station ID
        if (blacklst .and. ibcnt > 0) then
           stnid = transfer(hdr(4),stnid)
           do i = 1,ibcnt
              if( kx == blkkx(i) .and. stnid == blkstns(i) ) then
                 write(6,*)'read_aircraft_profile: blacklist station ',stnid, &
                    'for obstype ',trim(obstype),' and kx=',kx
                 cycle loop_report
              endif
           enddo
        endif

!  Match ob to proper convinfo type
        ncsave=0
        matchloop:do ncx=1,ntmatch
           nc=ntxall(ncx)
           if (kx /= ictype(nc))cycle 

!  Find convtype which match ob type and subtype
           if(icsubtype(nc) == iobsub) then
              ncsave=nc
              exit matchloop
           else
!  Find convtype which match ob type and subtype group (isubtype == ?*)
!       where ? specifies the group and icsubtype = ?0)
              ixsub=icsubtype(nc)/10
              iosub=iobsub/10
              isubsub=icsubtype(nc)-ixsub*10
              if(ixsub == iosub .and. isubsub == 0) then
                 ncsave=nc
!  Find convtype which match ob type and subtype is all remaining 
!       (icsubtype(nc) = 0)
              else if (ncsave == 0 .and. icsubtype(nc) == 0) then
                 ncsave=nc
              end if
           end if
        end do matchloop

!  Save information for next read
        if(ncsave /= 0) then

           call ufbint(lunin,levdat,1,255,levs,levstr)
           maxobs=maxobs+max(1,levs)
           nx=1
           if(ithin_conv(ncsave) > 0)then
              do ii=2,ntread
                 if(ntx(ii) == ncsave)nx=ii
              end do
           end if
           tab(ntb,1)=ncsave
           tab(ntb,2)=nx
           tab(ntb,3)=levs
           lmsg(nmsg,nx) = .true.
        end if

     end do loop_report
  enddo msg_report
  if (nmsg==0) goto 900
  write(6,*)'read_aircraft_profile: messages/reports = ',nmsg,'/',ntb,' ntread = ',ntread



  if(tob) write(6,*)'read_aircraft_profile: time offset is ',toff,' hours.'
!------------------------------------------------------------------------

! Obtain program code (VTCD) associated with "VIRTMP" step
  if(tob)call ufbqcd(lunin,'VIRTMP',vtcd)

  call init_aircraft_rjlists

! loop over convinfo file entries; operate on matches
  
  allocate(cdata_all(nreal,maxobs),isort(maxobs))
  isort = 0
  cdata_all=zero
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

           call make3grids(xmesh,nlevp)

           if (.not.use_all) then
              allocate(presl_thin(nlevp))
              if (pflag==1) then
                 do k=1,nlevp
                    presl_thin(k)=(r1200-(k-1)*pmesh)*one_tenth
                 enddo
              endif
           endif
     
           write(6,*)'read_aircraft_profile: obstype,ictype(nc),rmesh,pflag,nlevp,pmesh=',&
              trim(ioctype(nc)),ictype(nc),rmesh,pflag,nlevp,pmesh
        endif
     endif
       

     call closbf(lunin)
     open(lunin,file=infile,form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)

!    Big loop over prepbufr file	

     ntb = 0
     nmsg = 0
     icntpnt=0
     icntpnt2=0
     loop_msg: do while (ireadmg(lunin,subset,idate)== 0)
        if(.not. (trim(subset)=='AIRCFT' .or. trim(subset)=='AIRCAR')) cycle loop_msg
        nmsg = nmsg+1
        if(.not.lmsg(nmsg,nx)) then
           do i=ntb+1,ntb+nrep(nmsg)
              icntpnt2=icntpnt2+tab(i,3)
           end do
           ntb=ntb+nrep(nmsg)
           cycle loop_msg ! no useable reports this mesage, skip ahead report count
        end if 

        loop_readsb: do while(ireadsb(lunin) == 0)
!          use msg lookup table to decide which messages to skip
!          use report id lookup table to only process matching reports
           ntb = ntb+1
           if(icntpnt < icntpnt2)icntpnt=icntpnt2
           icntpnt2=icntpnt2+tab(ntb,3)
           nc=tab(ntb,1)
           if(nc <= 0 .or. tab(ntb,2) /= nx) cycle loop_readsb
                 
!          Extract type, date, and location information
           call ufbint(lunin,hdr,8,1,iret,hdstr)
           if(abs(hdr(3))>r90 .or. abs(hdr(2))>r360) cycle loop_readsb
           if(hdr(2)== r360)hdr(2)=hdr(2)-r360
           if(hdr(2) < zero)hdr(2)=hdr(2)+r360
           dlon_earth=hdr(2)*deg2rad
           dlat_earth=hdr(3)*deg2rad
           kx0=hdr(5)
           kx=130
           if (kx0==431 .or. kx0==531) kx=131
           if (kx0==433 .or. kx0==533) kx=133
           if (kx==130) cycle loop_readsb

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
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif

!------------------------------------------------------------------------

           if(offtime_data) then
 
!             in time correction for observations to account for analysis
!                      time being different from obs file time.
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
 
!             Add obs reference time, then subtract analysis time to get obs time relative to analysis
 
              time_correction=float(minobs-minan)*r60inv

           else
              time_correction=zero
           end if

           timeobs=real(real(hdr(4),r_single),r_double)
           t4dv=timeobs + toff
           zeps=1.0e-8_r_kind
           if (t4dv<zero  .and.t4dv>      -zeps) t4dv=zero
           if (t4dv>winlen.and.t4dv<winlen+zeps) t4dv=winlen
           t4dv=t4dv + time_correction
           time=timeobs + time_correction
 
           if (l4dvar) then
              if ((t4dv<zero.OR.t4dv>winlen)) cycle loop_readsb ! outside time window
           else
              if((real(abs(time)) > real(ctwind(nc)) .or. real(abs(time)) > real(twindin))) &
                 cycle loop_readsb ! outside time window
           endif

           timex=time

           c_prvstg=cspval
           c_sprvstg=cspval
     
!          aircraft data
!          aircraftobs=(kx>129.and.kx<140) .or. (kx>229.and.kx<240)
           aircraftobs=(kx==131) .or. (kx==133)

!          Extract data information on levels
           call ufbint(lunin,obsdat,11,255,levs,obstr)
           call ufbint(lunin,qcmark,8,255,levs,qcstr)
           call ufbint(lunin,obserr,8,255,levs,oestr)
           nread=nread+levs
           if(tob) then 
              aircraftwk = bmiss
              if (aircraftobs .and. aircraft_t_bc) then 
                 call ufbint(lunin,aircraftwk,2,255,levs,aircraftstr)
                 print*, 'levs=',levs
                 do k=1,levs
                    if (aircraftwk(1,k)<5.0_r_kind .or. aircraftwk(1,k)>6.0_r_kind) then 
                       print*, 'aircraft P=',obsdat(1,k), ' T=',obsdat(3,k), &
                         ' pof=',aircraftwk(1,k), ' ialr=', aircraftwk(2,k), ' kx=',kx0
                       cycle
                    end if
                 end do
                 print*
              end if
           endif

!          Check for valid reported pressure (POB).  Set POB=bmiss if POB<tiny_r_kind
           rstation_id=hdr(1)
           do k=1,levs
              if (obsdat(1,k)<tiny_r_kind) then
                 write(6,*)'read_aircraft_profile:  ***WARNING*** invalid pressure pob=',&
                    obsdat(1,k),' at k=',k,' for obstype=',obstype,' kx=',kx,&
                    ' c_station_id=',c_station_id,' reset pob=',bmiss
                 obsdat(1,k)=bmiss
              endif
           end do

!          If available, get obs errors from error table
           if(oberrflg)then

!             Set lower limits for observation errors
              terrmin=half
              werrmin=one
              perrmin=half
              qerrmin=one_tenth
              pwerrmin=one

              do k=1,levs
                 itypex=kx
                 ppb=obsdat(1,k)
                 if(kx==153)ppb=obsdat(11,k)*0.01_r_kind
                 ppb=max(zero,min(ppb,r2000))
                 if(ppb>=etabl(itypex,1,1)) k1=1
                 do kl=1,32
                    if(ppb>=etabl(itypex,kl+1,1).and.ppb<=etabl(itypex,kl,1)) k1=kl
                 end do
                 if(ppb<=etabl(itypex,33,1)) k1=5
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
     

!          Loop over levels       
           do k=1,levs
              do i=1,8
                 qcmark(i,k) = min(qcmark(i,k),qcmark_huge)
              end do

              if (kx == id_bias_ps) then
                 plevs(k)=one_tenth*obsdat(1,k)+conv_bias_ps   ! convert mb to cb
              else
                 plevs(k)=one_tenth*obsdat(1,k)   ! convert mb to cb
              endif
              pqm(k)=nint(qcmark(1,k))
              qqm(k)=nint(qcmark(2,k))
              tqm(k)=nint(qcmark(3,k))
              wqm(k)=nint(qcmark(5,k))
           end do

!          If temperature ob, extract information regarding virtual
!          versus sensible temperature
           if(tob) then
              call ufbevn(lunin,tpc,1,255,20,levs,'TPC')
              if (.not.tsensible) then
                 do k=1,levs
                    tvflg(k)=one                               ! initialize as sensible
                    do j=1,20
                       if (tpc(k,j)==vtcd) tvflg(k)=zero       ! reset flag if virtual
                       if (tpc(k,j)>=bmiss) exit               ! end of stack
                    end do
                 end do
              else         !peel back events to store sensible temp in case temp is virtual
                 call ufbevn(lunin,tobaux,2,255,20,levs,'TOB TQM')
                 do k=1,levs
                    tvflg(k)=one                              ! initialize as sensible
                    do j=1,20
                       if (tpc(k,j)==vtcd) then
                          obsdat(3,k)=tobaux(1,k,j+1)
                          qcmark(3,k)=min(tobaux(2,k,j+1),qcmark_huge)
                          tqm(k)=nint(qcmark(3,k))
                       end if
                       if (tpc(k,j)>=bmiss) exit              ! end of stack
                    end do
                 end do
              end if
           end if

           stnelev=hdr(6)
           ithin=ithin_conv(nc)
           ithinp = ithin > 0 .and. pflag /= 0
           if(levs > 1 .or. ithinp)then
!             Interpolate guess pressure profile to observation location
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

!             Compute depth of guess pressure layersat observation location
              if (levs > 1) then
                 do kk=1,nsig-1
                    dpres(kk)=presl(kk)-presl(kk+1)
                 end do
              endif
           end if
           LOOP_K_LEVS: do k=1,levs

              icntpnt=icntpnt+1

!             Extract quality marks
              if(tob)then
                 qm=tqm(k)
              end if
 

!             Check qc marks to see if obs should be processed or skipped

!             if(convobs .and. pqm(k) >=lim_qm .and. qm/=15 .and. qm/=9 )cycle loop_k_levs
!             if(qm >=lim_qm .and. qm /=15 .and. qm /=9)cycle loop_k_levs
              if(qm > 15 .or. qm < 0) cycle loop_k_levs

!             Special block for data thinning - if requested
              if (ithin > 0) then
                 ntmp=ndata  ! counting moved to map3gridS
           
!                Set data quality index for thinning
                 if (l4dvar) then
                    timedif = zero
                 else
                    timedif=abs(t4dv-toff)
                 endif
                 crit1 = timedif/r6+half

                 if (pflag==0) then
                    do kk=1,nsig
                       presl_thin(kk)=presl(kk)
                    end do
                 endif

                 call map3grids(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                    plevs(k),crit1,ithin,ndata,iout,icntpnt,iiout,luse)

                 if (.not. luse) then
                    if(k==levs) then
                       cycle loop_readsb
                    else
                       cycle LOOP_K_LEVS
                    endif
                 endif
                 if(iiout > 0) isort(iiout)=0
                 if(ndata >  ntmp)then
                    nodata=nodata+1
                 end if
                 isort(icntpnt)=iout

              else
                 ndata=ndata+1
                 nodata=nodata+1
                 iout=ndata
                 isort(icntpnt)=iout
              endif

              if(ndata > maxobs) then
                 write(6,*)'read_aircraft_profile:  ***WARNING*** ndata > maxobs for ',obstype
                 ndata = maxobs
              end if

!             Set usage variable              
              usage = zero


              if(icuse(nc) <= 0)usage=100._r_kind
              if(qm == 15 .or. qm == 12 .or. qm == 9)usage=100._r_kind
              if(qm >=lim_qm )usage=101._r_kind
              if(convobs .and. pqm(k) >=lim_qm )usage=102._r_kind

              if ((kx>129.and.kx<140).or.(kx>229.and.kx<240) ) then
                 call get_aircraft_usagerj(kx,obstype,c_station_id,usage)
              endif

              if(ncnumgrp(nc) > 0)then  
                 if(mod(ndata+1,ncnumgrp(nc))== ncgroup(nc)-1)usage=ncmiter(nc)
              end if

! Get information from surface file necessary for conventional data here
              call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

!             Extract pressure level and quality marks
              dlnpob=log(plevs(k))  ! ln(pressure in cb)

!             Set inflate_error logical based on qm flag
              inflate_error=.false.
              if (qm==3 .or. qm==7) inflate_error=.true.
 
!             Temperature
              if(tob) then
                 ppb=obsdat(1,k)
                 call errormod(pqm,tqm,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
                 toe=obserr(3,k)*errout
                 qtflg=tvflg(k) 
                 if (inflate_error) toe=toe*r1_2
                 if(ppb < r100)toe=toe*r1_2

                 idx = 0
                 if (aircraftobs .and. aircraft_t_bc) then
                    do j = 1,ntail_update
!                      if (rstation_id == taillist(j)) then
                       if (c_station_id == taillist(j)) then
                          idx = j
                          exit
                       end if
                    end do

!                   append new tail number at the end of existing tail numbers.
!                   at 1st analysis, the obs will be used without bias correction but
!                   bias coefficients will be generated for this new tail number.
!                   if (idx > ntail) toe = two*toe
                    if (idx == 0) then
!                      toe = two*toe
                       ntail_update = ntail_update+1
                       if (ntail_update > max_tail) then
                          write(6,*)'read_aircraft_profile: ***ERROR*** tail number exceeds maximum'
                          write(6,*)'read_aircraft_profile: stop program execution'
                          call stop2(339)
                       end if
                       idx=ntail_update
                       idx_tail(ntail_update) = ntail_update
!                      taillist(ntail_update) = rstation_id
                       taillist(ntail_update) = c_station_id
                       do j = 1,npredt
                          predt(j,ntail_update) = zero
                       end do
                    end if
                 end if
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
                 cdata_all(21,iout)=zz                     ! terrain height at ob location
                 cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(24,iout)=obsdat(10,k)            ! cat
                 if (aircraft_t_bc) then 
                    cdata_all(25,iout)=aircraftwk(1,k)     ! phase of flight
                    cdata_all(26,iout)=aircraftwk(2,k)     ! vertical velocity
                    cdata_all(27,iout)=idx                 ! index of temperature bias
                 end if
                 if(perturb_obs)cdata_all(nreal,iout)=ran01dom()*perturb_fact ! t perturbation
                 if (aircraftobs) write(55,*) nc,kx,c_station_id,t4dv,tqm(k),cdata_all(17,iout),cdata_all(18,iout),ppb,cdata_all(5,iout),cdata_all(25,iout)

              end if

!
!    End k loop over levs
           end do  LOOP_K_LEVS
        end do loop_readsb

!
!   End of bufr read loop
     enddo loop_msg
!    Close unit to bufr file
     call closbf(lunin)

!    Deallocate arrays used for thinning data
     if (.not.use_all) then
        deallocate(presl_thin)
        call del3grids
     endif

! Normal exit

  enddo loop_convinfo! loops over convinfo entry matches
  deallocate(lmsg)

! Write header record and data to output file for further processing
  allocate(iloc(ndata))
  icount=0
  do i=1,maxobs
     if(isort(i) > 0)then
       icount=icount+1
       iloc(icount)=isort(i)
     end if
  end do
  if(ndata /= icount)then
     write(6,*) ' PREPBUFR: mix up in read_aircraft_profile ,ndata,icount ',ndata,icount
     call stop2(50)
  end if
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
  call destroy_aircraft_rjlists

900 continue
  if(diagnostic_reg .and. ntest>0) write(6,*)'read_aircraft_profile:  ',&
     'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'read_aircraft_profile:  ',&
     'nvtest,vdisterrmax=',ntest,vdisterrmax

  if (ndata == 0) then 
     call closbf(lunin)
     write(6,*)'read_aircraft_profile:  closbf(',lunin,')'
  endif

  close(lunin)

  close(55)

! End of routine
  return

end subroutine read_aircraft_profile
