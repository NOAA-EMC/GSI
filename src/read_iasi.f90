subroutine read_iasi(mype,val_iasi,ithin,isfcalc,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_iasi                  read bufr format iasi data
! prgmmr :   tahara          org: np20                date: 2002-12-03
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
!   2002-12-03  tahara  - read aqua data in new bufr format
!   2004-05-28  kleist  - subroutine call update
!   2004-06-16  treadon - update documentation
!   2004-07-23  derber  - make changes to eliminate obs. earlier in thinning
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-08-25  eliu    - added option to read separate bufr table
!   2004-10-15  derber  - increase weight given to surface channel check
!                         in AIRS data selection algorithm
!   2005-01-26  derber - land/sea determination and weighting for data selection
!   2005-07-07  derber - clean up code and improve selection criteria
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-22  derber  - include mean in bias correction
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-03-07  derber - correct error in nodata count
!   2006-03-09  jung - correct sat zenith angle error (used before defined)
!   2006-04-21  keyser/treadon - modify ufbseq calls to account for change
!                                in NCEP bufr sequence for AIRS data
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber - modify reads so ufbseq not necessary
!                      - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!   2008-11-28  todling - measure time from beginning of assimilation window
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!   2009-12-28  gayno - add option to calculate surface characteristics using
!                       method that accounts for the size/shape of the fov.
!   2010-02-25  collard - changes to call to crtm_init for CRTM v2.0
!
!   input argument list:
!     mype     - mpi task id
!     val_iasi - weighting factor applied to super obs
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
!     nread    - number of BUFR IASI observations read
!     ndata    - number of BUFR IASI profiles retained for further processing
!     nodata   - number of BUFR IASI observations retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
! Use modules
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
               finalcheck,checkob,score_crit
  use radinfo, only:iuse_rad,nusis,jpch_rad,crtm_coeffs_path
  use crtm_planck_functions, only: crtm_planck_temperature
  use crtm_module, only: crtm_destroy,crtm_init,crtm_channelinfo_type, success
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,&
       tll2xy,txy2ll,rlats,rlons
  use constants, only: izero,ione,zero,deg2rad,rad2deg,r60inv,one
  use gsi_4dvar, only: l4dvar, iwinbgn, winlen
  use calc_fov_crosstrk, only: instrument_init, fov_check, fov_cleanup

  implicit none


! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: nchanl = 616_i_kind        !--- 616 subset ch out of 8078 ch for AIRS
  integer(i_kind),parameter :: n_totchan  = 616_i_kind
  integer(i_kind),parameter :: maxinfo    =  33_i_kind


! BUFR format for IASISPOT 
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
  real(r_kind)     ,intent(inout) :: val_iasi
  real(r_kind)     ,intent(in   ) :: gstime
  real(r_kind)     ,intent(in   ) :: rmesh

! Output variables
  integer(i_kind)  ,intent(inout) :: nread
  integer(i_kind)  ,intent(  out) :: ndata,nodata
  

! BUFR file sequencial number
!  character(len=512)  :: table_file
  integer(i_kind)     :: lnbufr = 10_i_kind

! Variables for BUFR IO    
  real(r_double),dimension(5)  :: linele
  real(r_double),dimension(13) :: allspot
  real(r_double),dimension(2,n_totchan) :: allchan
  real(r_double),dimension(3,10):: cscale
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
  real(r_kind)     :: radiance
  real(r_kind)     :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr
  real(r_kind),dimension(0:4) :: rlndsea
  real(r_kind),dimension(0:3) :: sfcpct
  real(r_kind),dimension(0:3) :: ts
  real(r_kind),dimension(10) :: sscale
  real(r_kind),dimension(n_totchan) :: temperature
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind) disterr,disterrmax,rlon00,rlat00,r01

  logical          :: outside,iuse,assim,valid
  logical          :: iasi

  integer(i_kind)  :: ifov, instr, iscn, ioff, ilat, ilon, sensorindex
  integer(i_kind)  :: i, j, l, iskip, ifovn, bad_line
  integer(i_kind)  :: nreal, isflg
  integer(i_kind)  :: itx, k, nele, itt, n
  integer(i_kind):: iexponent
  integer(i_kind):: idomsfc
  integer(i_kind):: ntest
  integer(i_kind):: error_status
  character(len=20),dimension(1):: sensorlist


  type(crtm_channelinfo_type),dimension(1) :: channelinfo

! Set standard parameters
  character(8),parameter:: fov_flag="crosstrk"
  integer(i_kind),parameter:: ichan=-999_i_kind  ! fov-based surface code is not channel specific for iasi 
  real(r_kind),parameter:: expansion=one         ! exansion factor for fov-based surface code.
                                                 ! use one for ir sensors.
  real(r_kind),parameter:: R90    =  90._r_kind
  real(r_kind),parameter:: R360   = 360._r_kind
  real(r_kind),parameter:: tbmin  = 50._r_kind
  real(r_kind),parameter:: tbmax  = 550._r_kind
  real(r_kind),parameter:: earth_radius = 6371000._r_kind

! Initialize variables
  disterrmax=zero
  ntest=izero
  nreal  = maxinfo
  ndata = izero
  nodata = izero
  iasi=      obstype == 'iasi'
  r01=0.01_r_kind
  ten=10.0_r_kind

  ilon=3_i_kind
  ilat=4_i_kind
  bad_line=-ione

!  write(6,*)'READ_IASI: mype, mype_root,mype_sub, npe_sub,mpi_comm_sub', &
!          mype, mype_root,mype_sub,mpi_comm_sub
  step   = 3.334_r_kind 
  start = -48.33_r_kind 
  step_adjust = 0.625_r_kind
  senname = 'IASI'
  nchanlr = nchanl
  if (isfcalc==ione)then
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
     if(mype_sub==mype_root) write(6,*)'READ_IASI: crtm_init() on path "'//trim(crtm_coeffs_path)//'"'
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
     write(6,*)'READ_IASI:  ***ERROR*** crtm_init error_status=',error_status,&
          '   TERMINATE PROGRAM EXECUTION'
     call stop2(71)
  endif

!  find IASI sensorindex
  sensorindex = izero
  if ( channelinfo(1)%sensor_id == 'iasi616_metop-a' )then
     sensorindex = ione
  else
     write(6,*)'READ_IASI: sensorindex not set  NO IASI DATA USED'
     return
  end if
  ioff=jpch_rad
  do i=1,jpch_rad
     if(nusis(i)==sis)ioff=min(ioff,i)
  end do
  ioff=ioff-ione
  if (mype_sub==mype_root)write(6,*)'READ_IASI:  iasi offset ',ioff

! Calculate parameters needed for FOV-based surface calculation.
  if (isfcalc==ione)then
     instr=18_i_kind
     call instrument_init(instr, jsatid, expansion)
  endif

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((nusis(i)==sis) .and. (iuse_rad(i)>izero)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_iasi=zero

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
  next=izero
  do while(ireadmg(lnbufr,subset,idate)>=izero)
     next=next+1
     if(next == npe_sub)next=izero
     if(next /= mype_sub)cycle
     read_loop: do while (ireadsb(lnbufr)==izero)

!    Read IASI FOV information
        call ufbint(lnbufr,linele,5_i_kind,ione,iret,'FOVN SLNM QGFQ MJFC SELV')
        if ( linele(3) /= zero) cycle read_loop  ! problem with profile (QGFQ)

        if ( bad_line == nint(linele(2))) then
!        zenith angle/scan spot mismatch, reject entire line
           cycle read_loop
        else
           bad_line = -ione
        endif

        call ufbint(lnbufr,allspot,13_i_kind,ione,iret,allspotlist)
        if(iret /= ione) cycle read_loop

!    Check observing position
        dlat_earth = allspot(8)   ! latitude
        dlon_earth = allspot(9)   ! longitude
        if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
           (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
           write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
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
              ntest=ntest+ione
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
           call grdcrd(dlat,ione,rlats,nlat,ione)
           call grdcrd(dlon,ione,rlons,nlon,ione)
        endif

!    Check obs time
        idate5(1) = nint(allspot(2)) ! year
        idate5(2) = nint(allspot(3)) ! month
        idate5(3) = nint(allspot(4)) ! day
        idate5(4) = nint(allspot(5)) ! hour
        idate5(5) = nint(allspot(6)) ! minute

        if( idate5(1) < 1900_i_kind .or. idate5(1) > 3000_i_kind .or. &
            idate5(2) < ione        .or. idate5(2) >   12_i_kind .or. &
            idate5(3) < ione        .or. idate5(3) >   31_i_kind .or. &
            idate5(4) <izero        .or. idate5(4) >   24_i_kind .or. &
            idate5(5) <izero        .or. idate5(5) >   60_i_kind )then

           write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
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

!    IASI fov ranges from 1 to 120.   Current angle dependent bias
!    correction has a maximum of 90 scan positions.   Geometry
!    of IASI scan allows us to remap 1-120 to 1-60.   Variable
!    ifovn below contains the remapped IASI fov.  This value is
!    passed on to and used in setuprad
        ifovn = (ifov-ione)/2 + ione
        iscn = nint(linele(2))               ! scan line

!    Check field of view (FOVN) and satellite zenith angle (SAZA)
        if( ifov <= izero .or. ifov > 120_i_kind .or. sat_zenang > 90._r_kind ) then
           write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                ' STRANGE OBS INFO(FOVN,SLNM,SAZA):', ifov, iscn, allspot(10)
           cycle read_loop
        endif
        if ( ifov <= 60_i_kind ) sat_zenang = -sat_zenang

!    Compare IASI satellite scan angle and zenith angle
        piece = -step_adjust
        if ( mod(ifovn,2_i_kind) == ione) piece = step_adjust
        lza = ((start + float((ifov-ione)/4)*step) + piece)*deg2rad
        sat_height_ratio = (earth_radius + linele(5))/earth_radius
        lzaest = asin(sat_height_ratio*sin(lza))*rad2deg
        if (abs(sat_zenang - lzaest) > one) then
           write(6,*)' READ_IASI WARNING uncertainty in lza ', &
               lza*rad2deg,sat_zenang,sis,ifov,start,step,allspot(11),allspot(12),allspot(13)
           bad_line = iscn
           cycle read_loop
        endif

!   Clear Amount  (percent clear)
        call ufbrep(lnbufr,cloud_frac,ione,6_i_kind,iret,'FCPH')
        clr_amt = cloud_frac(1)
!       if ( clr_amt < zero .or. clr_amt > 100.0_r_kind ) clr_amt = zero
        clr_amt=max(clr_amt,zero)
        clr_amt=min(clr_amt,100.0_r_kind)
     
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
        if (isfcalc == ione) then
           call fov_check(ifov,instr,valid)
           if (.not. valid) cycle read_loop
        endif

!    When isfcalc is set to one, calculate surface fields using size/shape of fov.
!    Otherwise, use bilinear interpolation.

        if (isfcalc == ione) then
           call deter_sfc_fov(fov_flag,ifov,instr,ichan,allspot(11),dlat_earth_deg, &
                              dlon_earth_deg,expansion,t4dv,isflg,idomsfc, &
                              sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
        else
           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
                      ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
        endif

!    Set common predictor parameters

        crit1 = crit1 + rlndsea(isflg)
 
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

        call ufbrep(lnbufr,cscale,3_i_kind,10_i_kind,iret,'STCH ENCH CHSF')
        if(iret /= 10_i_kind) then
           write(6,*) 'READ_IASI  read scale error ',iret
           cycle read_loop
        end if

! The scaling factors are as follows, cscale(1) is the start channel number,
!                                     cscale(2) is the end channel number,
!                                     cscale(3) is the exponent scaling factor
! In our case (616 channels) there are 10 groups of cscale (dimension :: cscale(3,10))
!  The units are W/m2..... you need to convert to mW/m2.... (subtract 5 from cscale(3)
        do i=1,10  ! convert exponent scale factor to int and change units
           iexponent = -(nint(cscale(3,i)) - 5_i_kind)
           sscale(i)=ten**iexponent
        end do

!    Read IASI channel number(CHNM) and radiance (SCRA)

        call ufbint(lnbufr,allchan,2_i_kind,n_totchan,iret,'SCRA CHNM')
        if( iret /= n_totchan)then
           write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                iret, ' CH DATA IS READ INSTEAD OF ',n_totchan
           cycle read_loop
        endif

        iskip = izero
        jstart=1
        do i=1,n_totchan
!     check that channel number is within reason
           if (( allchan(1,i) > zero .and. allchan(1,i) < 99999._r_kind) .and. &  ! radiance bounds
               (allchan(2,i) < 8462._r_kind .and. allchan(2,i) > zero )) then     ! chan # bounds
!         radiance to BT calculation
              radiance = allchan(1,i)
              scaleloop: do j=jstart,10
                 if(allchan(2,i) >= cscale(1,j) .and. allchan(2,i) <= cscale(2,j))then
                    radiance = allchan(1,i)*sscale(j)
                    jstart=j
                    exit scaleloop
                 end if
              end do scaleloop

              call crtm_planck_temperature(sensorindex,i,radiance,temperature(i))
              if(temperature(i) < tbmin .or. temperature(i) > tbmax ) then
                 temperature(i) = zero
                 if(iuse_rad(ioff+i) >= izero)iskip = iskip + ione
                 write(6,*)'READ_IASI:  skipped',i,temperature(i),allchan(2,1),allchan(2,i-ione)
              endif
           else           ! error with channel number or radiance
!             write(6,*)'READ_IASI:  iasi chan error',i,allchan(1,i), allchan(2,i)
              temperature(i) = zero
              if(iuse_rad(ioff+i) >= izero)iskip = iskip + ione
           endif
        end do

        if(iskip > izero)write(6,*) ' READ_IASI : iskip > 0 ',iskip
!       if( iskip >= 10_i_kind )cycle read_loop 

        crit1=crit1 + 10.0_r_kind*float(iskip)


!    Map obs to grids
        call finalcheck(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

        data_all(1,itx) = 4                      ! satellite ID (temp. 49)
        data_all(2,itx) = t4dv                   ! time diff (obs-anal) (hrs)
        data_all(3,itx) = dlon                   ! grid relative longitude
        data_all(4,itx) = dlat                   ! grid relative latitude
        data_all(5,itx) = sat_zenang*deg2rad     ! satellite zenith angle (rad)
        data_all(6,itx) = allspot(11)            ! satellite azimuth angle (deg)
        data_all(7,itx) = lza                    ! look angle (rad)
        data_all(8,itx) = ifovn                  ! fov number
        data_all(9,itx) = allspot(12)            ! solar zenith angle (deg)
        data_all(10,itx)= allspot(13)            ! solar azimuth angle (deg)
        data_all(11,itx) = sfcpct(0)             ! sea percentage of
        data_all(12,itx) = sfcpct(1)             ! land percentage
        data_all(13,itx) = sfcpct(2)             ! sea ice percentage
        data_all(14,itx) = sfcpct(3)             ! snow percentage
        data_all(15,itx)= ts(0)                  ! ocean skin temperature
        data_all(16,itx)= ts(1)                  ! land skin temperature
        data_all(17,itx)= ts(2)                  ! ice skin temperature
        data_all(18,itx)= ts(3)                  ! snow skin temperature
        data_all(19,itx)= tsavg                  ! average skin temperature
        data_all(20,itx)= vty                    ! vegetation type
        data_all(21,itx)= vfr                    ! vegetation fraction
        data_all(22,itx)= sty                    ! soil type
        data_all(23,itx)= stp                    ! soil temperature
        data_all(24,itx)= sm                     ! soil moisture
        data_all(25,itx)= sn                     ! snow depth
        data_all(26,itx)= zz                     ! surface height
        data_all(27,itx)= idomsfc + 0.001_r_kind ! dominate surface type
        data_all(28,itx)= sfcr                   ! surface roughness
        data_all(29,itx)= ff10                   ! ten meter wind factor
        data_all(30,itx)= dlon_earth*rad2deg     ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth*rad2deg     ! earth relative latitude (degrees)

        data_all(32,itx)= val_iasi
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
  if (mype_sub==mype_root.and.ndata>izero) then

!    Identify "bad" observation (unreasonable brightness temperatures).
!    Update superobs sum according to observation location

     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin .and. &
              data_all(i+nreal,n) < tbmax)nodata=nodata+ione
        end do
        itt=nint(data_all(nreal,n))
        super_val(itt)=super_val(itt)+val_iasi
     end do

!    Write final set of "best" observations to output file
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif


  deallocate(data_all) ! Deallocate data arrays
  call destroygrids    ! Deallocate satthin arrays

! Deallocate arrays and nullify pointers.
  if(isfcalc == ione) then
     call fov_cleanup
  endif

  if(diagnostic_reg .and. ntest > izero .and. mype_sub==mype_root) &
       write(6,*)'READ_IASI:  mype,ntest,disterrmax=',&
       mype,ntest,disterrmax
  
  return
end subroutine read_iasi
