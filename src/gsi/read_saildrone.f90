subroutine read_saildrone(nread,ndata,nodata,infile,obstype,lunout,gstime,twindin,sis,&
     nobs,nrec_start)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_saildrone          read obs from saildron bufr file
!   prgmmr: ######          org: np22                date: 2025-03-29
!
! abstract: This routine reads saildrone
!           in a bufr file format. Specific observation types read by this
!           routine include pressure, temperature, winds (components or
!           speed), and moisture.
!
! program history log:
!   2025-04-25: Collard: Original code based on read_gsb.
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     nrec_start - number of subsets without useful information
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     nodata    - number of individual "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     gstime   - analysis time in minutes from reference date
!     twindin  - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,r_kind,r_double,i_kind
  use constants, only: zero,one,deg2rad,rad2deg,r60inv,tiny_r_kind,huge_r_kind, half,&
          hvap,eps,omeps,rv,t0c,one_tenth,r100
  use aircraftinfo, only: aircraft_t_bc,aircraft_t_bc_pof,aircraft_t_bc_ext
  use convinfo, only: nconvtype, ioctype, icuse, ictype, icsubtype, ithin_conv
  use converr,only: etabl
  use convb_ps,only: btabl_ps
  use convb_q,only: btabl_q
  use convb_t,only: btabl_t
  use convb_uv,only: btabl_uv
  use qcmod, only: njqc
  use deter_sfc_mod, only: deter_sfc2
  use gridmod, only: nlon,nlat,rlats,rlons,regional,fv3_regional,diagnostic_reg,& 
                     rotate_wind_ll2xy,rotate_wind_xy2ll,tll2xy,txy2ll
  use gsi_4dvar, only: time_4dvar, iwinbgn, l4dvar,l4densvar,winlen
  use gsi_io, only: verbose
  use mpimod, only: npe
  use obsmod, only: bmiss,perturb_obs,perturb_fact,ran01dom
  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=20)                     ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout,nrec_start
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe)        ,intent(inout) :: nobs
  real(r_kind)                          ,intent(in   ) :: gstime
  real(r_kind)                          ,intent(in   ) :: twindin

! Declare local parameters
  real(r_kind),parameter:: one_minute = 0.01666667_r_kind
  real(r_kind),parameter:: minus_one_minute=-0.01666667_r_kind
  real(r_kind),parameter:: r0_001  =  0.001_r_kind
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r2000 = 2000.0_r_kind
  real(r_kind),parameter:: emerr= 0.2_r_kind

! Declare local variables
  character(8) :: subset
  character(80) :: hdstr, obstr

  integer(i_kind) :: ireadsb,ireadmg, ithin
  integer(i_kind) :: lunin, nc, ncsave
  integer(i_kind) :: nmsgmax, mxtb, maxobs, nmsg
  integer(i_kind) :: i, k, kl, kx, k1, k2, nreal, ntread, ntmatch 
  integer(i_kind) :: iobsub
  integer(i_kind) :: ncount_ps, ncount_q, ncount_t, ncount_uv
  integer(i_kind) :: irec, iret, ncx, levs, iout, ntest, nvtest
  integer(i_kind) :: idate, nmind, ilat, ilon, nchanl, idomsfc
  integer(i_kind),dimension(5):: idate5
  integer(i_kind),dimension(nconvtype)::ntxall
  integer(i_kind),dimension(nconvtype+1)::ntx
  integer(i_kind),allocatable,dimension(:,:):: tab
  integer(i_kind),allocatable,dimension(:):: nrep

  logical :: tob,qob,uvob, psob
  logical :: outside      
  logical :: print_verbose
  
  real(r_kind) :: dlon, dlat, dlat_earth_deg, dlon_earth_deg, dlat_earth, dlon_earth
  real(r_kind) :: rlon00, rlat00, cdist, disterr, disterrmax, vdisterrmax
  real(r_kind) :: dlnpob, pob_cb, rhob_calc, es, dummy, qsat
  real(r_kind) :: temperature_ob, dew_point_temperature_ob
  real(r_kind) :: relative_humidity_ob, humidity_ob, rhob  
  real(r_kind) :: toff, t4dv, tdiff
  real(r_kind) :: uwind, vwind, u0, v0, u00, v00, ppb, usage
  real(r_kind) :: obserr, var_jb, del, ediff
  real(r_kind) :: tsavg,ff10,sfcr,zz
  real(r_kind) :: qjbmin,tjbmin,wjbmin,pjbmin
  real(r_kind) :: terrmin=half
  real(r_kind) :: perrmin=0.3_r_kind
  real(r_kind) :: werrmin=one
  real(r_kind) :: qerrmin=0.05_r_kind
  real(r_kind),allocatable,dimension(:,:):: cdata_all   !,cdata_out
  real(r_double) :: rstation_id
  real(r_double),dimension(8,1):: hdr
  real(r_double),dimension(6,1):: obsdat

! data statements
  data hdstr  /'YEAR MNTH DAYS HOUR MINU CLATH CLONH LSTN'/
  data obstr  /'PMSL TMDB TMDP WDIR WSPD REHU'/ ! Saildrone does not have Pressure (but PMSL)
                                                
  data lunin / 13 /

! Initialize variables

  print_verbose=.true.   ! Temporary
  if(verbose) print_verbose=.true.
  ilon = 2
  ilat = 3
  nchanl = 0
  qjbmin = zero
  tjbmin = zero
  wjbmin = zero
  pjbmin = zero

  if (print_verbose) write(6,*) 'Entering READ_SAILDRONE, obstype =',obstype 
  tob = obstype == 't'
  uvob = obstype == 'uv'
  qob = obstype == 'q'
  psob = obstype == 'ps'
  if(tob)then
     nreal=25
  else if(uvob) then
     nreal=26
  else if(qob) then
     nreal=26
  else if(psob) then
     nreal=20
  else
     write(6,*) ' illegal obs type in READ_SAILDRONE ',obstype
     call stop2(94)
  end if

  if (tob .AND. (aircraft_t_bc_pof .or. aircraft_t_bc .or.aircraft_t_bc_ext)) nreal = nreal +3
  if(perturb_obs .and. (tob .or. qob))nreal=nreal+1
  if(perturb_obs .and. uvob )nreal=nreal+2

  if(tob)then
    kx = 302
  else if(psob) then
    kx = 302
  else if(uvob) then
    kx = 402
  else if(qob) then
    kx = 302
  end if

!------------------------------------------------------------------------
  ntread=1
  ntmatch=0
  ntx(ntread)=0
  ntxall=0
  var_jb=zero
  do nc=1,nconvtype
     if(trim(ioctype(nc)) == trim(obstype))then
       ntmatch=ntmatch+1
       ntxall(ntmatch)=nc
     end if
     if(trim(ioctype(nc)) == trim(obstype) .and. abs(icuse(nc)) <= 1)then
       ithin=ithin_conv(nc)
       if(ithin > 0 .and. ithin <5)then
         ntread=ntread+1
         ntx(ntread)=nc
       end if
     end if
  end do
  if(ntmatch == 0)then
     write(6,*) ' no matching obstype found in obsinfo ',obstype
     return
  end if
!!


!! get message and subset counts

  call getcount_bufr(infile,nmsgmax,mxtb)
  allocate(tab(mxtb,3),nrep(nmsgmax))

  maxobs=0
  tab=0
  nmsg=0
  nrep=0
  ndata = 0

  irec = 0
  ncount_ps=0;ncount_q=0;ncount_t=0;ncount_uv=0

! Open, then read date from bufr data
  open(lunin,file=trim(infile),form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)

  usage = 100.0_r_kind
  msg_report: do while (ireadmg(lunin,subset,idate) == 0)
     irec = irec + 1
     if(irec < nrec_start) cycle msg_report
!    Time offset
     if(nmsg == 0) call time_4dvar(idate,toff)
     nmsg=nmsg+1
     if (nmsg>nmsgmax) then
        write(6,*)'READ_SAILDRONE: messages exceed maximum ',nmsgmax
        call stop2(50)
     endif
     loop_report: do while (ireadsb(lunin) == 0)
        ndata = ndata+1
        nrep(nmsg)=nrep(nmsg)+1
        if (ndata>mxtb) then
           write(6,*)'READ_SAILDRONE: reports exceed maximum ',mxtb
           call stop2(50)
        endif

!       Extract type information
        call ufbint(lunin,hdr,8,1,iret,hdstr)
        iobsub = 0
!  Match ob to proper convinfo type
        ncsave=1 ! tst
        matchloop:do ncx=1,ntmatch
           nc=ntxall(ncx)
           if (kx /= ictype(nc))cycle

!  Find convtype which match ob type and subtype
           if(icsubtype(nc) == iobsub) then
              ncsave=nc
              if (icuse(nc) >= 1) usage = zero
              exit matchloop
           else
             cycle
           end if
        end do matchloop
        maxobs = maxobs + 1

     end do loop_report
  enddo msg_report

  if (nmsg==0) then
     call closbf(lunin)
     close(lunin)
     if(print_verbose)write(6,*)'READ_SAILDRONE: no messages/reports '
     return
  end if
  if(print_verbose)write(6,*)'READ_SAILDRONE: messages/reports = ',nmsg,'/',ndata,' ntread = ',ntread


  if(tob .and. print_verbose) write(6,*)'READ_SAILDRONE: time offset is ',toff,' hours.'

  call closbf(lunin)
  close(lunin)  
  
  !------------------------------------------------------------------------

  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)

!  Big loop over bufr file    
  allocate(cdata_all(nreal,maxobs))

  nmsg = 0
  disterrmax=-9999.0_r_kind
  irec = 0
  iout = 0
  loop_msg: do while (ireadmg(lunin,subset,idate)== 0)
     irec = irec + 1
     if(irec < nrec_start) cycle loop_msg

     nmsg = nmsg+1

     loop_readsb: do while(ireadsb(lunin) == 0)
     !  Extract type, date, and location information
       call ufbint(lunin,hdr,8,1,iret,hdstr) ! YEAR MNTH DAYS HOUR MINU CLATH CLONH LSTN
       if(abs(hdr(6,1))>r90 .or. abs(hdr(7,1))>r360) cycle loop_readsb
       dlon_earth_deg=hdr(7,1)
       dlat_earth_deg=hdr(6,1)
       if (dlon_earth_deg == r360) dlon_earth_deg = dlon_earth_deg-r360
       if (dlon_earth_deg < zero) dlon_earth_deg = dlon_earth_deg+r360
       dlon_earth=dlon_earth_deg*deg2rad
       dlat_earth=dlat_earth_deg*deg2rad

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

!  Extract date information.  If time outside window, skip this obs
       idate5(1) = hdr(1,1) !year
       idate5(2) = hdr(2,1) !month
       idate5(3) = hdr(3,1) !day
       idate5(4) = hdr(4,1) !hour
       idate5(5) = hdr(5,1) !minute
       call w3fs21(idate5,nmind)
       t4dv= (real((nmind-iwinbgn),r_kind))*r60inv 
       tdiff=t4dv+(iwinbgn-gstime)*r60inv
 
       if (l4dvar.or.l4densvar) then
          if (t4dv<minus_one_minute .OR. t4dv>winlen+one_minute) &
              cycle loop_readsb
       else
          if(abs(tdiff) > twindin+one_minute) cycle loop_readsb
       endif

       !Set station ID
       rstation_id=hdr(8,1)  

       ! Set surface type
       call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

       call ufbint(lunin,obsdat,6,1,levs,obstr)  ! PMSL TMDB TMDP WDIR WSPD REHU
       dlnpob=log(obsdat(1,1)/1000_r_kind)


       ! Read in the data
       if (psob .AND. obsdat(1,1) > zero .AND. obsdat(1,1) < 1.4e5_r_kind) then
  
           ! Assign obs error from error table
           ppb=obsdat(1,1)/r100
           ppb=max(zero,min(ppb,r2000))
           if(ppb>=etabl(kx,1,1)) k1=1
           do kl=1,32
               if(ppb>=etabl(kx,kl+1,1).and.ppb<=etabl(kx,kl,1)) k1=kl
           end do
           if(ppb<=etabl(kx,33,1)) k1=5
           k2=k1+1
           ediff = etabl(kx,k2,1)-etabl(kx,k1,1)
           if (abs(ediff) > tiny_r_kind) then
               del = (ppb-etabl(kx,k1,1))/ediff
           else
               del = huge_r_kind
           endif
           del=max(zero,min(del,one))
           ! Pressure error
           obserr=(one-del)*etabl(kx,k1,5)+del*etabl(kx,k2,5)
           obserr=max(obserr,perrmin)
           ! Varjb
           if (njqc) then
               var_jb=(one-del)*btabl_ps(kx,k1,2)+del*btabl_ps(kx,k2,2)
               var_jb=max(var_jb,pjbmin) 
               if (var_jb >=10.0_r_kind) var_jb=zero
           else
               var_jb=zero
           endif

           ! Write to output array
           iout = iout + 1
  
           cdata_all(1,iout)=obserr                  ! surface pressure error (cb)
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=obsdat(1,1)/1000_r_kind ! pressure (in cb)
           cdata_all(5,iout)=zero                    ! surface height
           cdata_all(6,iout)=obsdat(2,1)+t0c         ! surface temperature
           cdata_all(7,iout)=rstation_id             ! station id
           cdata_all(8,iout)=t4dv                    ! time
           cdata_all(9,iout)=nc                      ! type
           cdata_all(10,iout)=zero                   ! quality mark
           cdata_all(11,iout)=obserr                 ! original obs error (cb)
           cdata_all(12,iout)=usage                  ! usage parameter
           cdata_all(13,iout)=idomsfc                ! dominate surface type
           cdata_all(14,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
           cdata_all(15,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
           cdata_all(16,iout)=zero                   ! station elevation (m)
           cdata_all(17,iout)=zz                     ! terrain height at ob location
           cdata_all(18,iout)=bmiss                 ! provider name
           cdata_all(19,iout)=bmiss                  ! subprovider name
           cdata_all(20,iout)=var_jb                 ! non linear qc b parameter 
           if(perturb_obs)cdata_all(21,iout)=ran01dom()*perturb_fact ! ps perturbation
       end if 
 

       if (tob .AND. abs(obsdat(2,1)-225.0_r_kind) < 125.0_r_kind .AND. &
           obsdat(1,1) > zero .AND. obsdat(1,1) < 1.4e5_r_kind) then
  
           ! Assign obs error from error table
           ppb=obsdat(1,1)/r100
           ppb=max(zero,min(ppb,r2000))
           if(ppb>=etabl(kx,1,1)) k1=1
           do kl=1,32
               if(ppb>=etabl(kx,kl+1,1).and.ppb<=etabl(kx,kl,1)) k1=kl
           end do
           if(ppb<=etabl(kx,33,1)) k1=5
           k2=k1+1
           ediff = etabl(kx,k2,1)-etabl(kx,k1,1)
           if (abs(ediff) > tiny_r_kind) then
               del = (ppb-etabl(kx,k1,1))/ediff
           else
               del = huge_r_kind
           endif
           del=max(zero,min(del,one))
           ! Temperature error
           obserr=(one-del)*etabl(kx,k1,2)+del*etabl(kx,k2,2)
           obserr=max(obserr,terrmin)
           ! Varjb
           if (njqc) then
               var_jb=(one-del)*btabl_t(kx,k1,2)+del*btabl_t(kx,k2,2)
               var_jb=max(var_jb,tjbmin) 
               if (var_jb >=10.0_r_kind) var_jb=zero
           else
               var_jb=zero
           endif

           ! Write to output array
           iout = iout + 1
           cdata_all(1,iout)=obserr                  ! temperature error
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,iout)=obsdat(2,1)             ! temperature ob.
           cdata_all(6,iout)=rstation_id             ! station id
           cdata_all(7,iout)=t4dv                    ! time
           cdata_all(8,iout)=nc                      ! type
           cdata_all(9,iout)=one                     ! obs are sensible/dry bulb
           cdata_all(10,iout)=zero                   ! quality mark
           cdata_all(11,iout)=obserr                 ! original obs error
           cdata_all(12,iout)=usage                  ! usage parameter
           cdata_all(13,iout)=idomsfc                ! dominate surface type
           cdata_all(14,iout)=tsavg                  ! skin temperature
           cdata_all(15,iout)=ff10                   ! 10 meter wind factor
           cdata_all(16,iout)=sfcr                   ! surface roughness
           cdata_all(17,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
           cdata_all(18,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
           cdata_all(19,iout)=bmiss                  ! station elevation (m)
           cdata_all(20,iout)=zero                   ! observation height (m)
           cdata_all(21,iout)=zz                     ! terrain height at ob location
           cdata_all(22,iout)=bmiss                  ! provider name
           cdata_all(23,iout)=bmiss                  ! subprovider name
           cdata_all(24,iout)=bmiss                  ! cat
           cdata_all(25,iout)=var_jb                 ! non linear qc for T
           if (aircraft_t_bc_pof .or. aircraft_t_bc .or.aircraft_t_bc_ext) then  ! These are obviously not used but are here to
                                                                                 ! keep the array sizes consistent
              cdata_all(26,iout)=zero     ! phase of flight
              cdata_all(27,iout)=zero     ! vertical velocity
              cdata_all(28,iout)=zero     ! index of temperature bias
           end if
           if(perturb_obs)cdata_all(nreal,iout)=ran01dom()*perturb_fact ! t perturbation
   
       end if 
 
       rhob = obsdat(6,1)       ! Relative humidity from BUFR (REHU), expected units: % 
       if (qob .AND. &
           abs(obsdat(2,1)-225.0_r_kind) < 125.0_r_kind .AND. &
           obsdat(1,1) > zero .AND. obsdat(1,1) < 1.4e5_r_kind) then

           !          Convert raw moisture data from dew point temperature to specific humidity
           pob_cb = obsdat(1,1) * r0_001  ! convert [Pa] to [cb]
           dew_point_temperature_ob =  obsdat(3,1)
           temperature_ob = obsdat(2,1)
           call fpvsx_ad(temperature_ob,es,dummy,dummy,.false.)
           qsat = eps*es/(pob_cb-omeps*es)

           ! Prefer observed RH (REHU) when it is valid; otherwise fall back to deriving RH
           ! from TMDB/TMDP (some Saildrone acting as buoy may provide only dewpoint).
           if (rhob >= 0.0_r_kind .AND. rhob <= 100.0_r_kind) then
              relative_humidity_ob = rhob * 0.01_r_kind   ! convert % to 0-1
           else if (abs(dew_point_temperature_ob) < 320.0_r_kind) then
              rhob_calc = exp((one-temperature_ob/dew_point_temperature_ob)*(hvap/rv)/temperature_ob) ! rh in 0-1
              relative_humidity_ob = rhob_calc
           else
              cycle   ! no usable moisture information
           end if

           humidity_ob  = relative_humidity_ob * qsat

           ! Assign obs error from error table
           ppb=obsdat(1,1)/r100
           ppb=max(zero,min(ppb,r2000))
           if(ppb>=etabl(kx,1,1)) k1=1
           do kl=1,32
               if(ppb>=etabl(kx,kl+1,1).and.ppb<=etabl(kx,kl,1)) k1=kl
           end do
           if(ppb<=etabl(kx,33,1)) k1=5
           k2=k1+1
           ediff = etabl(kx,k2,1)-etabl(kx,k1,1)
           if (abs(ediff) > tiny_r_kind) then
               del = (ppb-etabl(kx,k1,1))/ediff
           else
               del = huge_r_kind
           endif
           del=max(zero,min(del,one))
           ! Spc Hum error
           obserr=(one-del)*etabl(kx,k1,3)+del*etabl(kx,k2,3)
           obserr=max(obserr,qerrmin)
           ! Varjb
           if (njqc) then
               var_jb=(one-del)*btabl_q(kx,k1,2)+del*btabl_q(kx,k2,2)
               var_jb=max(var_jb,qjbmin) 
               if (var_jb >=10.0_r_kind) var_jb=zero
           else
               var_jb=zero
           endif
  
           ! Write to output array
           iout = iout + 1
           cdata_all(1,iout)=obserr*one_tenth        ! specific humidity error
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,iout)=humidity_ob             ! specific humidity ob.
           cdata_all(6,iout)=rstation_id             ! station id
           cdata_all(7,iout)=t4dv                    ! time
           cdata_all(8,iout)=nc                      ! type
           cdata_all(9,iout)=emerr                   ! q max error
           cdata_all(10,iout)= obsdat(2,1)           ! dry temperature
           cdata_all(11,iout)= zero                  ! quality mark
           cdata_all(12,iout)= obserr*one_tenth      ! original obs error
           cdata_all(13,iout)= usage                 ! usage parameter
           cdata_all(14,iout)= idomsfc               ! dominate surface type
           cdata_all(15,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
           cdata_all(16,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
           cdata_all(17,iout)=bmiss                  ! station elevation (m)
           cdata_all(18,iout)=zero                   ! observation height (m)
           cdata_all(19,iout)=zz                     ! terrain height at ob location
           cdata_all(20,iout)= bmiss                 ! provider name
           cdata_all(21,iout)= bmiss                 ! subprovider name
           cdata_all(22,iout)= bmiss                 ! cat
           cdata_all(23,iout)= bmiss                 ! non linear qc b parameter
           cdata_all(24,iout)=bmiss                  ! cat
           cdata_all(25,iout)=var_jb                 ! non linear qc for T
           cdata_all(26,iout)=bmiss                  ! Dummy                
           if(perturb_obs)cdata_all(nreal,iout)=ran01dom()*perturb_fact ! q perturbation
  
       end if 
  
       if (uvob .AND. abs(obsdat(4,1)-180.0_r_kind) <= 180.0_r_kind .AND. abs(obsdat(5,1)) < 200.0_r_kind .AND. &
           obsdat(1,1) > zero .AND. obsdat(1,1) < 1.4e5_r_kind) then
  
           ! Assign obs error from error table
           ppb=obsdat(1,1)/r100
           ppb=max(zero,min(ppb,r2000))
           if(ppb>=etabl(kx,1,1)) k1=1
           do kl=1,32
               if(ppb>=etabl(kx,kl+1,1).and.ppb<=etabl(kx,kl,1)) k1=kl
           end do
           if(ppb<=etabl(kx,33,1)) k1=5
           k2=k1+1
           ediff = etabl(kx,k2,1)-etabl(kx,k1,1)
           if (abs(ediff) > tiny_r_kind) then
               del = (ppb-etabl(kx,k1,1))/ediff
           else
               del = huge_r_kind
           endif
           del=max(zero,min(del,one))
           ! Wind error
           obserr=(one-del)*etabl(kx,k1,4)+del*etabl(kx,k2,4)
           obserr=max(obserr,werrmin)
           ! Varjb
           if (njqc) then
               var_jb=(one-del)*btabl_uv(kx,k1,2)+del*btabl_uv(kx,k2,2)
               var_jb=max(var_jb,wjbmin) 
               if (var_jb >=10.0_r_kind) var_jb=zero
           else
               var_jb=zero
           endif
  
           ! Write to output array
           uwind = -obsdat(5,1) * sin(obsdat(4,1)*deg2rad) 
           vwind = -obsdat(5,1) * cos(obsdat(4,1)*deg2rad)
  
           if(regional .and. .not. fv3_regional)then
                   u0 = uwind
                   v0 = vwind
                   call rotate_wind_ll2xy(u0,v0,uwind,vwind,dlon_earth,dlon,dlat)
                   if(diagnostic_reg) then
                      call rotate_wind_xy2ll(uwind,vwind,u00,v00,dlon_earth,dlon,dlat)
                      nvtest      = nvtest+1
                      disterr     = sqrt((uwind-u00)**2+(vwind-v00)**2)
                      vdisterrmax = max(vdisterrmax,disterr)
                   end if
           endif
  
           iout = iout + 1
           cdata_all(1,iout)=obserr                  ! wind error
           cdata_all(2,iout)=dlon                    ! grid relative longitude
           cdata_all(3,iout)=dlat                    ! grid relative latitude
           cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,iout)=zero                    ! observation height (m)
           cdata_all(6,iout)=uwind                   ! u-wind ob.
           cdata_all(7,iout)=vwind                   ! v-wind ob.
           cdata_all(8,iout)=rstation_id             ! station id
           cdata_all(9,iout)=t4dv                    ! time
           cdata_all(10,iout)=nc                     ! type
           cdata_all(11,iout)=bmiss                  ! station elevation (m)
           cdata_all(12,iout)=zero                   ! quality mark
           cdata_all(13,iout)=obserr                 ! original obs error
           cdata_all(14,iout)=usage                  ! usage parameter
           cdata_all(15,iout)=idomsfc                ! dominate surface type
           cdata_all(16,iout)=tsavg                  ! skin temperature
           cdata_all(17,iout)=ff10                   ! 10 meter wind factor
           cdata_all(18,iout)=sfcr                   ! surface roughness
           cdata_all(19,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
           cdata_all(20,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
           cdata_all(21,iout)=zz                     ! terrain height at ob location
           cdata_all(22,iout)=bmiss                  ! provider name
           cdata_all(23,iout)=bmiss                  ! subprovider name
           cdata_all(24,iout)=bmiss                  ! cat
           cdata_all(25,iout)=var_jb                 ! non linear qc for uv
           cdata_all(26,iout)=one                    ! hilbert curve weight, modified later
           if(perturb_obs)then
              cdata_all(27,iout)=ran01dom()*perturb_fact ! u perturbation
              cdata_all(28,iout)=ran01dom()*perturb_fact ! v perturbation
           endif
  
       end if 
    end do loop_readsb  
  enddo loop_msg   
  call closbf(lunin)
  close(lunin)


  nread = iout
  ndata = nread
  if(uvob)then
     nodata=2*ndata
  else
     nodata=ndata
  end if
  call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)

  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)

  if(print_verbose)write(6,*)'READ_SAILDRONE:  closbf(',lunin,'), number of obs=',iout
    
  deallocate(cdata_all)
end subroutine read_saildrone
