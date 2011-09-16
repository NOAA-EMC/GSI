subroutine read_cris(mype,val_cris,ithin,isfcalc,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_cris                  read bufr format cris data
! prgmmr :   mccarty          org: gmao                date: 2011-05-18
!
! abstract:  This routine reads BUFR format radiance 
!            files.  Optionally, the data are thinned to 
!            a specified resolution using simple quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2010-10-12  zhu     - use radstep and radstart from radinfo
!   2011-05-18  mccarty - read_cris copied from read_iasi r10572 
!   2011-07-04  todling  - fixes to run either single or double precision
!   2011-08-01  lueken  - added module use deter_sfc_mod
!
!   input argument list:
!     mype     - mpi task id
!     val_cris - weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - when set to one, calculate surface characteristics using
!                method that accounts for the size/shape of the fov. 
!                when not one, calculate surface characteristics using
!                bilinear interpolation.
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite id
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - sensor/instrument/satellite indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!
!   output argument list:
!     nread    - number of BUFR CRIS observations read
!     ndata    - number of BUFR CRIS profiles retained for further processing
!     nodata   - number of BUFR CRIS observations retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
! Use modules
  use type_kinds, only: crtm_kind => fp
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      finalcheck,checkob,score_crit
  use radinfo, only:iuse_rad,nusis,jpch_rad,crtm_coeffs_path,use_edges, &
      find_edges,radstart,radstep
  use crtm_planck_functions, only: crtm_planck_temperature
  use crtm_module, only: crtm_destroy,crtm_init,crtm_channelinfo_type, success
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,&
      tll2xy,txy2ll,rlats,rlons
  use constants, only: zero,deg2rad,rad2deg,r60inv,one
  use gsi_4dvar, only: l4dvar, iwinbgn, winlen
  use calc_fov_crosstrk, only: instrument_init, fov_check, fov_cleanup
  use deter_sfc_mod, only: deter_sfc_fov,deter_sfc

  implicit none


! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: nchanl = 358      !--- 358 subset ch out of 1305 ch for AIRS
  integer(i_kind),parameter :: n_totchan  = 358
  integer(i_kind),parameter :: maxinfo    =  33


! BUFR format for CRISSPOT 
! Input variables
  integer(i_kind)  ,intent(in   ) :: mype
  integer(i_kind)  ,intent(in   ) :: ithin
  integer(i_kind)  ,intent(in   ) :: isfcalc
  integer(i_kind)  ,intent(in   ) :: lunout
  integer(i_kind)  ,intent(in   ) :: mype_root
  integer(i_kind)  ,intent(in   ) :: mype_sub
  integer(i_kind)  ,intent(in   ) :: npe_sub
  integer(i_kind)  ,intent(in   ) :: mpi_comm_sub  
  character(len=10),intent(in   ) :: infile
  character(len=10),intent(in   ) :: jsatid
  character(len=10),intent(in   ) :: obstype
  character(len=20),intent(in   ) :: sis
  real(r_kind)     ,intent(in   ) :: twind
  real(r_kind)     ,intent(inout) :: val_cris
  real(r_kind)     ,intent(in   ) :: gstime
  real(r_kind)     ,intent(in   ) :: rmesh

! Output variables
  integer(i_kind)  ,intent(inout) :: nread
  integer(i_kind)  ,intent(  out) :: ndata,nodata
  

! BUFR file sequencial number
!  character(len=512)  :: table_file
  integer(i_kind)     :: lnbufr = 10

! Variables for BUFR IO    
  real(r_double),dimension(6)  :: linele
  real(r_double),dimension(13) :: allspot
  real(r_double),dimension(2,n_totchan) :: allchan
  real(r_double),dimension(3,3):: chanbound
  real(r_double),dimension(6):: cloud_frac
  
  real(r_kind)      :: step, start,step_adjust
  character(len=8)  :: subset
  character(len=4)  :: senname
  character(len=80) :: allspotlist
  integer(i_kind)   :: nchanlr,jstart
  integer(i_kind)   :: iret,ireadsb,ireadmg,irec,isub,next


! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  real(r_kind)      :: sstime, tdiff, t4dv
  integer(i_kind)   :: nmind


! Other work variables
  real(r_kind)     :: clr_amt,piece,ten
  real(r_kind)     :: dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth,dlon_earth_deg,dlat_earth_deg
  real(r_kind)     :: lza, lzaest,sat_height_ratio
  real(r_kind)     :: timedif, pred, crit1, dist1
  real(r_kind)     :: sat_zenang
  real(crtm_kind)  :: radiance
  real(r_kind)     :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr
  real(r_kind),dimension(0:4) :: rlndsea
  real(r_kind),dimension(0:3) :: sfcpct
  real(r_kind),dimension(0:3) :: ts
  real(r_kind),dimension(10) :: sscale
  real(crtm_kind),dimension(n_totchan) :: temperature
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind) disterr,disterrmax,rlon00,rlat00,r01

  logical          :: outside,iuse,assim,valid
  logical          :: cris
  logical          :: data_on_edges

  integer(i_kind)  :: ifov, ifor, instr, iscn, ioff, ilat, ilon, sensorindex
  integer(i_kind)  :: i, j, l, iskip, ifovn, bad_line
  integer(i_kind)  :: nreal, isflg
  integer(i_kind)  :: itx, k, nele, itt, n
  integer(i_kind):: iexponent
  integer(i_kind):: idomsfc(1)
  integer(i_kind):: ntest
  integer(i_kind):: error_status
  character(len=20),dimension(1):: sensorlist


  type(crtm_channelinfo_type),dimension(1) :: channelinfo

! Set standard parameters
  character(8),parameter:: fov_flag="crosstrk"
  integer(i_kind),parameter:: ichan=-999  ! fov-based surface code is not channel specific for cris 
  real(r_kind),parameter:: expansion=one         ! exansion factor for fov-based surface code.
                                                 ! use one for ir sensors.
  real(r_kind),parameter:: R90    =  90._r_kind
  real(r_kind),parameter:: R360   = 360._r_kind
  real(r_kind),parameter:: tbmin  = 50._r_kind
  real(r_kind),parameter:: tbmax  = 550._r_kind
  real(r_kind),parameter:: earth_radius = 6371000._r_kind

! Initialize variables
  disterrmax=zero
  ntest=0
  nreal  = maxinfo
  ndata = 0
  nodata = 0
  cris=      obstype == 'cris'
  r01=0.01_r_kind
  ten=10.0_r_kind

  ilon=3
  ilat=4
  bad_line=-1

!  write(6,*)'READ_CRIS: mype, mype_root,mype_sub, npe_sub,mpi_comm_sub', &
!          mype, mype_root,mype_sub,mpi_comm_sub
  do i=1,jpch_rad
     if (trim(nusis(i))==trim(sis)) then
        step  = radstep(i)
        start = radstart(i)
        exit
     endif
  end do
  step_adjust = 0.625_r_kind
  senname = 'CRIS'
  nchanlr = nchanl
  if (isfcalc==1)then
     rlndsea = zero
  else
     rlndsea(0) = zero                       
     rlndsea(1) = 10._r_kind
     rlndsea(2) = 15._r_kind
     rlndsea(3) = 10._r_kind
     rlndsea(4) = 30._r_kind
  endif
  
  allspotlist= &
     'SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ SOZA SOLAZI'
  
  sensorlist(1)=sis
  if( crtm_coeffs_path /= "" ) then
     if(mype_sub==mype_root) write(6,*)'READ_CRIS: crtm_init() on path "'//trim(crtm_coeffs_path)//'"'
     error_status = crtm_init(sensorlist,channelinfo,&
        Process_ID=mype_sub,Output_Process_ID=mype_root, &
        Load_CloudCoeff=.FALSE.,Load_AerosolCoeff=.FALSE., &
        File_Path = crtm_coeffs_path )
  else
     error_status = crtm_init(sensorlist,channelinfo,&
        Process_ID=mype_sub,Output_Process_ID=mype_root, &
        Load_CloudCoeff=.FALSE.,Load_AerosolCoeff=.FALSE.)
  endif
  if (error_status /= success) then
     write(6,*)'READ_CRIS:  ***ERROR*** crtm_init error_status=',error_status,&
        '   TERMINATE PROGRAM EXECUTION'
     call stop2(71)
  endif

!  find CRIS sensorindex
  sensorindex = 0
  if ( channelinfo(1)%sensor_id == 'cris358_npp' )then  ! This is just a guess...to update
     sensorindex = 1
  else
     write(6,*)'READ_CRIS: sensorindex not set  NO CRIS DATA USED'
     return
  end if
  ioff=jpch_rad
  do i=1,jpch_rad
     if(nusis(i)==sis)ioff=min(ioff,i)
  end do
  ioff=ioff-1
  if (mype_sub==mype_root)write(6,*)'READ_CRIS:  cris offset ',ioff

! Calculate parameters needed for FOV-based surface calculation.
  if (isfcalc==1)then
     instr=18
     call instrument_init(instr, jsatid, expansion)
  endif

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((nusis(i)==sis) .and. (iuse_rad(i)>0)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_cris=zero

! Make thinning grids
  call makegrids(rmesh,ithin)

! Open BUFR file
  open(lnbufr,file=infile,form='unformatted')

! Open BUFR table
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Allocate arrays to hold data
  nele=nreal+nchanl
  allocate(data_all(nele,itxmax))

! Big loop to read data file
  next=0
  do while(ireadmg(lnbufr,subset,idate)>=0)
     next=next+1
     if(next == npe_sub)next=0
     if(next /= mype_sub)cycle
     read_loop: do while (ireadsb(lnbufr)==0)

!    Read CRIS FOV information
        call ufbint(lnbufr,linele,6,1,iret,'FOVN SLNM QMRKH MJFC HMSL FORN')
        if ( linele(3) /= zero) cycle read_loop  ! problem with profile (QMRKH)
                                                 ! May want to eventually set to 
                                                 ! QMRHK <= 1, as data is, and I
                                                 ! quote, 'slightly suspect'


        if ( bad_line == nint(linele(2))) then
!        zenith angle/scan spot mismatch, reject entire line
           cycle read_loop
        else
           bad_line = -1
        endif

        call ufbint(lnbufr,allspot,13,1,iret,allspotlist)
        if(iret /= 1) cycle read_loop

!    Check observing position
        dlat_earth = allspot(8)   ! latitude
        dlon_earth = allspot(9)   ! longitude
        if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
           (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
           write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
              ' STRANGE OBS POINT (LAT,LON):', dlat_earth, dlon_earth
           cycle read_loop
        endif

!    Retrieve observing position
        if(dlon_earth >= R360)then
           dlon_earth = dlon_earth - R360
        else if(dlon_earth < ZERO)then
           dlon_earth = dlon_earth + R360
        endif

        dlat_earth_deg = dlat_earth
        dlon_earth_deg = dlon_earth
        dlat_earth = dlat_earth * deg2rad
        dlon_earth = dlon_earth * deg2rad

!    If regional, map obs lat,lon to rotated grid.
        if(regional)then

!    Convert to rotated coordinate.  dlon centered on 180 (pi),
!    so always positive for limited area
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if(diagnostic_reg) then
              call txy2ll(dlon,dlat,rlon00,rlat00)
              ntest=ntest+1
              disterr=acos(sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                   (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00)))*rad2deg
              disterrmax=max(disterrmax,disterr)
           end if

!    Check to see if in domain.  outside=.true. if dlon_earth,
!    dlat_earth outside domain, =.false. if inside
           if(outside) cycle read_loop

!    Global case 
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd(dlat,1,rlats,nlat,1)
           call grdcrd(dlon,1,rlons,nlon,1)
        endif

!    Check obs time
        idate5(1) = nint(allspot(2)) ! year
        idate5(2) = nint(allspot(3)) ! month
        idate5(3) = nint(allspot(4)) ! day
        idate5(4) = nint(allspot(5)) ! hour
        idate5(5) = nint(allspot(6)) ! minute

        if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
            idate5(2) < 1    .or. idate5(2) >   12 .or. &
            idate5(3) < 1    .or. idate5(3) >   31 .or. &
            idate5(4) <0     .or. idate5(4) >   24 .or. &
            idate5(5) <0     .or. idate5(5) >   60 )then

           write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
              ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
           cycle read_loop

        endif

!    Retrieve obs time
        call w3fs21(idate5,nmind)
        t4dv = (real(nmind-iwinbgn,r_kind) + real(allspot(7),r_kind)*r60inv)*r60inv ! add in seconds
        if (l4dvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           sstime = real(nmind,r_kind) + real(allspot(7),r_kind)*r60inv ! add in seconds
           tdiff = (sstime - gstime)*r60inv
           if (abs(tdiff)>twind) cycle read_loop
        endif
     
!   Increment nread counter by n_totchan
        nread = nread + n_totchan

        if (l4dvar) then
           crit1 = 0.01_r_kind
        else
           timedif = 6.0_r_kind*abs(tdiff)        ! range:  0 to 18
           crit1 = 0.01_r_kind+timedif
        endif
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
        if(.not. iuse)cycle read_loop

!    Observational info
        sat_zenang  = allspot(10)            ! satellite zenith angle
        ifov = nint(linele(1))               ! field of view
        ifor = nint(linele(6))               ! field of regard

!  CRIS field-of-view ranges from 1 to 9, corresponding to the 9 sensors measured
!  per field-of-regard.  The field-of-regard ranges from 1 to 30.  For now, 
!  rotation of entire FOR w.r.t. position is neglected.  For reference, FOV 
!  pattern within the FOR is :
!FOV#      7 8 9|7 8 9
!FOV#      4 5 6|4 5 6
!FOV#      1 2 3|1 2 3 (spacecraft velocity up the screen)
!----------------------
!FOR#        x    x+1
!  FORs are scanned from left limb (FOR=1) to right limb (FOR=30)
!
!  For now, the FORs & FOVs are treated from 1-90, but this isn't correct because
!  of FOR rotation.  At nadir, the FOR is square-shaped, but at the limb, the FOR
!  is diamond shaped.  See Fig. 58 of CrIS SDR ATBD (Rev. D) for a picture.

        ifovn = (mod(ifov-1,3)) + ((ifor-1)*3) + 1
        iscn = nint(linele(2))               ! scan line

!    Remove data on edges
        if (.not. use_edges) then 
           call find_edges(sis,ifovn,data_on_edges)
           if (data_on_edges) cycle read_loop
        end if

!    Check field of view (FOVN), field-of-regard (FORN), and satellite zenith angle (SAZA)
        if( ifov < 1 .or. ifov > 9  .or. & ! FOVN not betw. 1 & 9
            ifor < 1 .or. ifor > 30 .or. & ! FORN not betw. 1 & 30
            sat_zenang > 90._r_kind ) then
           write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
              ' STRANGE OBS INFO(FOVN,FORN,SLNM,SAZA):', ifov, ifor, iscn, allspot(10)
           cycle read_loop
        endif
        if ( ifor <= 15 ) sat_zenang = -sat_zenang

!    Compare CRIS satellite scan angle and zenith angle
!  wm - this is just the IASI method...needs to be re-assessed.
        piece = -step_adjust
!        if ( mod(ifovn,2) == 1) piece = step_adjust
        lza = ((start + float(ifovn-1)*step) + piece)*deg2rad

        sat_height_ratio = (earth_radius + linele(5))/earth_radius
        lzaest = asin(sat_height_ratio*sin(lza))*rad2deg
        if (abs(sat_zenang - lzaest) > one) then
           write(6,*)' READ_CRIS WARNING uncertainty in lza ', &
              lza*rad2deg,sat_zenang,sis,ifov,start,step,allspot(11),allspot(12),allspot(13)
           bad_line = iscn
           cycle read_loop
        endif

!   Clear Amount  (percent clear) 
!WM  - disabled as simulated data appears to a) not have clouds and
!        b) be outside of any expected range at the moment.  May be 
!        bufr scaling issue.  For now, clr_amt hardcoded to 100
!        call ufbrep(lnbufr,cloud_frac,1,6,iret,'TOCC')
!        clr_amt = 1.0 - cloud_frac(1)
!!       if ( clr_amt < zero .or. clr_amt > 100.0_r_kind ) clr_amt = zero
!        clr_amt=max(clr_amt,zero)
!        clr_amt=min(clr_amt,100.0_r_kind)
        clr_amt = 100.0_r_kind     
!    Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
        pred = 100.0_r_kind - clr_amt

        crit1 = crit1 + pred
 
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

!   "Score" observation.  We use this information to identify "best" obs
!    Locate the observation on the analysis grid.  Get sst and land/sea/ice
!    mask.  
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed 

!    When using FOV-based surface code, must screen out obs with bad fov numbers.
        if (isfcalc == 1) then
           call fov_check(ifov,instr,valid)
           if (.not. valid) cycle read_loop
        endif

!    When isfcalc is set to one, calculate surface fields using size/shape of fov.
!    Otherwise, use bilinear interpolation.

        if (isfcalc == 1) then
           call deter_sfc_fov(fov_flag,ifov,instr,ichan,real(allspot(11),r_kind),dlat_earth_deg, &
                              dlon_earth_deg,expansion,t4dv,isflg,idomsfc(1), &
                              sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
        else
           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc(1),sfcpct, &
                      ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
        endif

!    Set common predictor parameters

        crit1 = crit1 + rlndsea(isflg)
 
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

! CrIS data unscaled, section removed from read_iasi

!    Read CRIS channel number(CHNM) and radiance (SRAD)

        call ufbint(lnbufr,allchan,2,n_totchan,iret,'SRAD CHNM')
        if( iret /= n_totchan)then
           write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
              iret, ' CH DATA IS READ INSTEAD OF ',n_totchan
           cycle read_loop
        endif

        iskip = 0
        jstart=1
        do i=1,n_totchan
!     check that channel number is within reason
           if (( allchan(1,i) > zero .and. allchan(1,i) < 99999._r_kind) .and. &  ! radiance bounds
               (allchan(2,i) < 1400._r_kind .and. allchan(2,i) > zero )) then     ! chan # bounds
!         radiance to BT calculation
! WM - units of radiance need to be double checked.  Also, there is a potential issue w/ CHNM 
!       relative to discarded channels in conversion from interferogram to spectrum
              radiance = allchan(1,i)
              call crtm_planck_temperature(sensorindex,i,radiance,temperature(i))
              if(temperature(i) < tbmin .or. temperature(i) > tbmax ) then
                 temperature(i) = min(tbmax,max(zero,temperature(i)))
                 if(iuse_rad(ioff+i) >= 0)iskip = iskip + 1
                 write(6,*)'READ_CRIS:  invalid Tb-ic,Tb,CHNM: ',i,temperature(i),allchan(2,i)
              endif
           else           ! error with channel number or radiance
              write(6,*)'READ_CRIS:  invalid radiance-ic,SRAD,CHNM: ',i,allchan(1,i), allchan(2,i)
              temperature(i) = min(tbmax,max(zero,temperature(i)))
              if(iuse_rad(ioff+i) >= 0)iskip = iskip + 1
           endif
        end do

        if(iskip > 0)write(6,*) ' READ_CRIS : iskip > 0 ',iskip
!       if( iskip >= 10 )cycle read_loop 

        crit1=crit1 + 10.0_r_kind*float(iskip)


!    Map obs to grids
        call finalcheck(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

        data_all(1,itx) = 4                         ! satellite ID (temp. 49)
        data_all(2,itx) = t4dv                      ! time diff (obs-anal) (hrs)
        data_all(3,itx) = dlon                      ! grid relative longitude
        data_all(4,itx) = dlat                      ! grid relative latitude
        data_all(5,itx) = sat_zenang*deg2rad        ! satellite zenith angle (rad)
        data_all(6,itx) = allspot(11)               ! satellite azimuth angle (deg)
        data_all(7,itx) = lza                       ! look angle (rad)
        data_all(8,itx) = ifovn                     ! fov number
        data_all(9,itx) = allspot(12)               ! solar zenith angle (deg)
        data_all(10,itx)= allspot(13)               ! solar azimuth angle (deg)
        data_all(11,itx) = sfcpct(0)                ! sea percentage of
        data_all(12,itx) = sfcpct(1)                ! land percentage
        data_all(13,itx) = sfcpct(2)                ! sea ice percentage
        data_all(14,itx) = sfcpct(3)                ! snow percentage
        data_all(15,itx)= ts(0)                     ! ocean skin temperature
        data_all(16,itx)= ts(1)                     ! land skin temperature
        data_all(17,itx)= ts(2)                     ! ice skin temperature
        data_all(18,itx)= ts(3)                     ! snow skin temperature
        data_all(19,itx)= tsavg                     ! average skin temperature
        data_all(20,itx)= vty                       ! vegetation type
        data_all(21,itx)= vfr                       ! vegetation fraction
        data_all(22,itx)= sty                       ! soil type
        data_all(23,itx)= stp                       ! soil temperature
        data_all(24,itx)= sm                        ! soil moisture
        data_all(25,itx)= sn                        ! snow depth
        data_all(26,itx)= zz                        ! surface height
        data_all(27,itx)= idomsfc(1) + 0.001_r_kind ! dominate surface type
        data_all(28,itx)= sfcr                      ! surface roughness
        data_all(29,itx)= ff10                      ! ten meter wind factor
        data_all(30,itx)= dlon_earth*rad2deg        ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth*rad2deg        ! earth relative latitude (degrees)

        data_all(32,itx)= val_cris
        data_all(33,itx)= itt

        do l=1,nchanl
           data_all(l+nreal,itx) = temperature(l)   ! brightness temerature
        end do


     enddo read_loop
  enddo
  call closbf(lnbufr)

! deallocate crtm info
  error_status = crtm_destroy(channelinfo)
  if (error_status /= success) &
     write(6,*)'OBSERVER:  ***ERROR*** crtm_destroy error_status=',error_status

! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit)


! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root.and.ndata>0) then

!    Identify "bad" observation (unreasonable brightness temperatures).
!    Update superobs sum according to observation location

     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin .and. &
              data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
        itt=nint(data_all(nreal,n))
        super_val(itt)=super_val(itt)+val_cris
     end do

!    Write final set of "best" observations to output file
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif


  deallocate(data_all) ! Deallocate data arrays
  call destroygrids    ! Deallocate satthin arrays

! Deallocate arrays and nullify pointers.
  if(isfcalc == 1) then
     call fov_cleanup
  endif

  if(diagnostic_reg .and. ntest > 0 .and. mype_sub==mype_root) &
     write(6,*)'READ_CRIS:  mype,ntest,disterrmax=',&
        mype,ntest,disterrmax
  
  return
end subroutine read_cris
