subroutine read_amsr2(mype,val_amsr2,ithin,isfcalc,rmesh,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs)

! subprogram:    read_amsr2                  read bufr format amsr2 data
!   prgmmr: ejones         copied from read_amsre.f90         date: 2014-03-15
! abstract:  This routine reads BUFR format AMSR2 radiance (brightness
!            temperature) files that contain the first 14 channels of AMSR2 
!            (all at the same resolution).
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2014-03-15  ejones   - read amsr2  
!   2015-09-17  Thomas   - add l4densvar and thin4d to data selection procedure
! 
!
! input argument list:
!     mype     - mpi task id
!     val_amsre- weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - flag to specify method to calculate sfc fields within FOV
!                when set to one, account for size/shape of FOV.  otherwise
!                use bilinear interpolation.
!     rmesh    - thinning mesh size (km)
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!
! output argument list:
!     nread    - number of BUFR AMSR2 observations read
!     ndata    - number of BUFR AMSR2 profiles retained for further processing
!     nodata   - number of BUFR AMSR2 observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use radinfo, only: iuse_rad,nusis,jpch_rad,nst_gsi,nstinfo 
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,rlats,rlons,&
      tll2xy
  use constants, only: deg2rad,rad2deg,zero,one,three,r60inv,two
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen,l4densvar,thin4d
  use calc_fov_conical, only: instrument_init
  use deter_sfc_mod, only: deter_sfc_fov,deter_sfc
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe

  implicit none

! Input variables
  character(len=*) ,intent(in   ) :: infile
  character(len=*) ,intent(in   ) :: obstype
  integer(i_kind)  ,intent(in   ) :: mype
  integer(i_kind)  ,intent(inout) :: isfcalc
  integer(i_kind)  ,intent(in   ) :: ithin
  integer(i_kind)  ,intent(in   ) :: lunout
  real(r_kind)     ,intent(inout) :: val_amsr2
  real(r_kind)     ,intent(in   ) :: gstime,twind
  real(r_kind)     ,intent(in   ) :: rmesh
  character(len=20),intent(in   ) :: sis
  integer(i_kind)  ,intent(in   ) :: mype_root
  integer(i_kind)  ,intent(in   ) :: mype_sub
  integer(i_kind)  ,intent(in   ) :: npe_sub
  integer(i_kind)  ,intent(in   ) :: mpi_comm_sub

! Output variables
  integer(i_kind)  ,intent(inout) :: nread
  integer(i_kind)  ,intent(inout) :: ndata,nodata
  integer(i_kind),dimension(npe)  ,intent(inout) :: nobs

! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: N_AMSRCH  =  14  ! only channels 1-14 processed
  integer(i_kind) :: said, GCOMW1_SAID  = 122  !WMO satellite identifier 
  integer(i_kind) :: siid, AMSR2_SIID = 478   !WMO instrument identifier 
  integer(i_kind),parameter :: maxinfo    =  33

! BUFR file sequencial number
  character(len=8)  :: subset
  character(len=4)  :: senname
  integer(i_kind)   :: lnbufr = 10
  integer(i_kind)   :: nchanl
  integer(i_kind)   :: iret,isflg,idomsfc(1)

! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  integer(i_kind)   :: nmind
  real(r_kind)      :: sstime, tdiff, t4dv

! Other work variables
  logical           :: outside,iuse,assim
  integer(i_kind)   :: nreal, kidsat
  integer(i_kind)   :: itx, nele, itt, k
  integer(i_kind)   :: ifov, ilat, ilon
  integer(i_kind)   :: i, l, n
  integer(i_kind),dimension(n_amsrch) :: kchamsr2
  real(r_kind)     :: sfcr
  real(r_kind)     :: dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth
  real(r_kind)     :: timedif, pred, crit1, dist1
  real(r_kind),allocatable,dimension(:,:):: data_all
  integer(i_kind),allocatable,dimension(:)::nrec
  integer(i_kind):: irec,next
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  character(len=7),parameter:: fov_flag="conical"
  character(len=3) :: fov_satid
  
  integer(i_kind) :: ichan, instr, idum

  logical :: valid
  real(r_kind) :: clath_sun_glint_calc , clonh_sun_glint_calc 
  real(r_kind) :: date5_4_sun_glint_calc

  real(r_kind) :: expansion, dlat_earth_deg, dlon_earth_deg

! Set standard parameters
  integer(i_kind) ntest
  integer(i_kind) :: nscan,iskip,kskip,kch,kchanl
  real(r_kind),parameter :: R90      =  90._r_kind
  real(r_kind),parameter :: R360     = 360._r_kind
  real(r_kind),parameter :: tbmin    = 3._r_kind           
  real(r_kind),parameter :: tbmax    = 340._r_kind         

  real(r_kind),dimension(N_AMSRCH) :: tbob_org
  real(r_kind) :: clath, clonh, fovn, soel, solazi, saz         

! BUFR format for AMSRSPOT
  integer(i_kind),parameter :: N_AMSRSPOT_LIST = 12

! BUFR format for AMSRCHAN
  integer(i_kind),parameter :: N_AMSRCHAN_LIST = 3

! Variables for BUFR IO
  real(r_double),dimension(4):: gcomspot_d
  real(r_double),dimension(12):: amsrspot_d               
  real(r_double),dimension(3,14):: amsrchan_d             

! ---- sun glint ----
  integer(i_kind) doy,mlen(12),mday(12),mon,m
  real(r_kind) sun_azimuth,sun_zenith
  data  mlen/31,28,31,30,31,30, &
             31,31,30,31,30,31/ 

  integer(i_kind) :: ireadsb, ireadmg 
  real(r_kind),parameter:: one_minute=0.01666667_r_kind
  real(r_kind),parameter:: minus_one_minute=-0.01666667_r_kind

! ----------------------------------------------------------------------
! Initialize variables

  ilon = 3
  ilat = 4

  if (nst_gsi > 0 ) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  m = 0
  do mon=1,12 
     mday(mon) = m 
     m = m + mlen(mon) 
  end do 
  ntest = 0
  nreal = maxinfo+nstinfo
  ndata = 0
  nodata = 0
  sstime = zero
  kchanl=14
  kchamsr2(1:14)=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14/)

  senname = 'AMSR'
  nchanl  = N_AMSRCH
  nscan  = 243  
  kidsat = 549
  rlndsea(0) = zero
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 100._r_kind

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
  if (.not.assim) val_amsr2=zero

! note, fov-based surface code does not have equations for amsr2. In the event
! that isfcalc != 0 (it should be 0), then use ssmi/s for fov code until
! something else can be devised.
  if(isfcalc==1)then
     instr=26
     fov_satid='f16'
     idum = -999  ! dummy variable for fov number. not used for conical instr.
     ichan=14
     expansion=2.9_r_kind
     call instrument_init(instr, fov_satid, expansion, valid)
     if (.not. valid) then
       if (assim) then
         write(6,*)'READ_AMSR2:  ***ERROR*** IN SETUP OF FOV-SFC CODE. STOP'
         call stop2(71)
       else
         isfcalc = 0
         write(6,*)'READ_AMSR2:  ***ERROR*** IN SETUP OF FOV-SFC CODE'
       endif
    endif
  endif

! Make thinning grids
  call makegrids(rmesh,ithin)

! Open BUFR file
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Allocate local array to contain observation information
  nele=nreal+nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))

! Big loop to read data file
  next=0
  nrec=999999
  irec=0
  do while(ireadmg(lnbufr,subset,idate)>=0)
     irec=irec+1
     next=next+1
     if(next == npe_sub)next=0
     if(next /= mype_sub)cycle
     read_loop: do while (ireadsb(lnbufr)==0)

!    Retrieve bufr 1/4 :get gcomspot (said,orbn,solazi,soel)
        call ufbint(lnbufr,gcomspot_d,4,1,iret,'SAID ORBN SOLAZI SOEL')    !???

        said = nint(gcomspot_d(1))
        if(said /= GCOMW1_SAID)    cycle read_loop

!       Retrieve bufr 2/4 :get amsrspot (siid,ymdhs,lat,lon)
        call ufbrep(lnbufr,amsrspot_d,N_AMSRSPOT_LIST,1,iret, &
           'SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH AANG IANG FOVN')!???

        siid = nint(amsrspot_d(1)) 
        if(siid /= AMSR2_SIID)   cycle read_loop


!       Check obs time
        idate5(1) = amsrspot_d(02)! year
        idate5(2) = amsrspot_d(03)! month
        idate5(3) = amsrspot_d(04)! day
        idate5(4) = amsrspot_d(05)! hour
        idate5(5) = amsrspot_d(06)! min
        if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
            idate5(2) < 1    .or. idate5(2) >   12 .or. &
            idate5(3) < 1    .or. idate5(3) >   31 .or. &
            idate5(4) < 0    .or. idate5(4) >   24 .or. &
            idate5(5) < 0    .or. idate5(5) >   60 )then
            write(6,*)'READ_AMSR2:  ### ERROR IN READING BUFR DATA:', &
              ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
            cycle read_loop   
        endif

        call w3fs21(idate5,nmind)
        t4dv = (real((nmind-iwinbgn),r_kind) + amsrspot_d(7)*r60inv)*r60inv ! add in seconds
        sstime = real(nmind,r_kind) + amsrspot_d(7)*r60inv ! add in seconds
        tdiff  = (sstime - gstime)*r60inv

        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if (abs(tdiff)>twind) cycle read_loop  
        endif
        if (thin4d) then
           timedif = zero
        else
           timedif = 6.0_r_kind*abs(tdiff) ! range:  0 to 18 
        endif


!     --- Check observing position -----
        clath= amsrspot_d(08)
        clonh= amsrspot_d(09)

        if( abs(clath) > R90  .or. abs(clonh) > R360 .or. &
          ( abs(clath) == R90 .and. clonh /= ZERO) )  then
           write(6,*)'READ_AMSR2:  ### ERROR IN READING BUFR DATA:',&
              ' STRANGE OBS POINT (LAT,LON):', clath, clonh
              cycle read_loop
        endif

        fovn = amsrspot_d(12)
     
!    Set position in a given region
        if(clonh >= R360)then
           clonh = clonh - R360
        else if(clonh < ZERO)then
           clonh = clonh + R360
        endif
     
!    If regional, map obs lat,lon to rotated grid.
        dlat_earth = clath * deg2rad
        dlon_earth = clonh * deg2rad
        dlat_earth_deg = clath
        dlon_earth_deg = clonh

        if(regional)then
!       Convert to rotated coordinate.  dlon centered on 180 (pi),
!       so always positive for limited area
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)

!       Check to see if in domain.  outside=.true. if dlon_earth,
!       dlat_earth outside domain, =.false. if inside
           if(outside) cycle read_loop

!    Global case 
        else
           dlat=dlat_earth
           dlon=dlon_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif

!    Sum number of read obs before thinning step.  Note that this number will contain
!    some observations that may be rejected later due to bad BTs.
        nread=nread+kchanl
    
        crit1 = 0.01_r_kind+timedif

        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
        if (.not.iuse) cycle read_loop

!    QC:  "Score" observation.  We use this information to identify "best" obs

!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask   

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                       

!       When isfcalc is one, calculate surface fields based on size/shape of fov.
!       Otherwise, use bilinear interpolation.

        if (isfcalc==1)then
           call deter_sfc_fov(fov_flag,idum,instr,ichan,real(amsrspot_d(11),r_kind),dlat_earth_deg,&
              dlon_earth_deg,expansion,t4dv,isflg,idomsfc(1), &
              sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
        else
           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc(1),sfcpct, &
              ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

           if(isflg/=0) cycle read_loop                            ! use data over water only
        endif

        crit1 = crit1 +rlndsea(isflg)

        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

!       Retrieve bufr 3/4 : get amsrchan (chnm,tbb)
        call ufbrep(lnbufr,amsrchan_d,3,14,iret,'SCCF ACQF TMBR')   

        tbob_org(1)=amsrchan_d(3,1)          
        tbob_org(2)=amsrchan_d(3,2)
        tbob_org(3)=amsrchan_d(3,3)
        tbob_org(4)=amsrchan_d(3,4)
        tbob_org(5)=amsrchan_d(3,5)
        tbob_org(6)=amsrchan_d(3,6)
        tbob_org(7)=amsrchan_d(3,7)
        tbob_org(8)=amsrchan_d(3,8)
        tbob_org(9)=amsrchan_d(3,9)
        tbob_org(10)=amsrchan_d(3,10)
        tbob_org(11)=amsrchan_d(3,11)
        tbob_org(12)=amsrchan_d(3,12)
        tbob_org(13)=amsrchan_d(3,13)
        tbob_org(14)=amsrchan_d(3,14)

!       Set obs information
        iskip = 0
        do l=1,nchanl
           if(tbob_org(l)<tbmin .or. tbob_org(l)>tbmax)then
              iskip = iskip + 1
           end if
        end do
        kskip = 0
        do l=1,kchanl
           kch=kchamsr2(l)
           if(tbob_org(kch)<tbmin .or. tbob_org(kch)>tbmax)then
              kskip = kskip + 1
           endif
        end do
        if(kskip == kchanl .or. iskip == nchanl) cycle read_loop

!    Set data quality predictor 
        pred = zero

!    Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"

!    Map obs to grids
        crit1 = crit1+pred
        call finalcheck(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

        solazi = gcomspot_d(3)     !solar azimuth angle
        soel = gcomspot_d(4)       !solar elevation angle

!    Check observational info 

        if( soel < -180._r_kind .or. soel > 180._r_kind )then
           write(6,*)'READ_AMSR2:  ### ERROR IN READING BUFR DATA:', &
              ' STRANGE OBS INFO(FOV,SOLAZI,SOEL):', fovn, solazi, soel
           cycle read_loop       
        endif


! Work on this:
! Sun glint and solar zenith code from AMSR-E, use this while no sun glint angle
! is present in the AMSR2 data stream.
!  -------- Retreive Sun glint angle -----------
        doy = mday( int(idate5(2)) ) + int(idate5(3))
        if ((mod( int(idate5(1)),4)==0).and.( int(idate5(2)) > 2))  then 
           doy = doy + 1
        end if 

        ifov = nint(fovn)

        clath_sun_glint_calc = clath
        clonh_sun_glint_calc = clonh
        if(clonh>180_r_kind) clonh_sun_glint_calc = clonh -360.0_r_kind
        date5_4_sun_glint_calc =  &                                                                                                
        real(idate5(4),r_kind)+real(idate5(5),r_kind)*r60inv+real(amsrspot_d(7),r_kind)*r60inv*r60inv   

        call zensun(doy,date5_4_sun_glint_calc,clath_sun_glint_calc,clonh_sun_glint_calc,sun_zenith,sun_azimuth)

        saz = amsrspot_d(11)*deg2rad    ! satellite zenith/incidence angle(rad)

!       interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
        if ( nst_gsi > 0 ) then
           tref  = ts(0)
           dtw   = zero
           dtc   = zero
           tz_tr = one
           if ( sfcpct(0) > zero ) then
             call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr) 
           endif
        endif

        data_all(1,itx) = 122.00                     ! satellite ID
        data_all(2,itx) = t4dv                       ! time diff (obs - anal) (hours)
        data_all(3,itx) = dlon                       ! grid relative longitude
        data_all(4,itx) = dlat                       ! grid relative latitude
        data_all(5,itx) = saz                        ! satellite zenith angle (rad)
        data_all(6,itx) = amsrspot_d(10)             ! satellite azimuth angle
        data_all(7,itx) = zero                       ! look angle (rad)
        data_all(8,itx) = ifov                       ! fov number    1-243
        data_all(9,itx) = sun_zenith                 ! solar zenith angle (deg)
        data_all(10,itx)= sun_azimuth                ! solar azimuth angle (deg)
        data_all(11,itx) = sfcpct(0)                 ! sea percentage of
        data_all(12,itx) = sfcpct(1)                 ! land percentage
        data_all(13,itx) = sfcpct(2)                 ! sea ice percentage
        data_all(14,itx) = sfcpct(3)                 ! snow percentage
        data_all(15,itx)= ts(0)                      ! ocean skin temperature
        data_all(16,itx)= ts(1)                      ! land skin temperature
        data_all(17,itx)= ts(2)                      ! ice skin temperature
        data_all(18,itx)= ts(3)                      ! snow skin temperature
        data_all(19,itx)= tsavg                      ! average skin temperature
        data_all(20,itx)= vty                        ! vegetation type
        data_all(21,itx)= vfr                        ! vegetation fraction
        data_all(22,itx)= sty                        ! soil type
        data_all(23,itx)= stp                        ! soil temperature
        data_all(24,itx)= sm                         ! soil moisture
        data_all(25,itx)= sn                         ! snow depth
        data_all(26,itx)= zz                         ! surface height
        data_all(27,itx)= idomsfc(1) + 0.001_r_kind  ! dominate surface type
        data_all(28,itx)= sfcr                       ! surface roughness
        data_all(29,itx)= ff10                       ! ten meter wind factor
        data_all(30,itx)= dlon_earth*rad2deg         ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth*rad2deg         ! earth relative latitude (degrees)

        data_all(32,itx)= val_amsr2
        data_all(33,itx)= itt

        if ( nst_gsi > 0 ) then
           data_all(maxinfo+1,itx) = tref                ! foundation temperature
           data_all(maxinfo+2,itx) = dtw                 ! dt_warm at zob
           data_all(maxinfo+3,itx) = dtc                 ! dt_cool at zob
           data_all(maxinfo+4,itx) = tz_tr               ! d(Tz)/d(Tr)
        endif

        do l=1,nchanl
           data_all(l+nreal,itx) = tbob_org(l)
        end do
        nrec(itx)=irec

     enddo read_loop
  enddo
  call closbf(lnbufr)

! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit,nrec)

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
        itt=nint(data_all(maxinfo,n))
        super_val(itt)=super_val(itt)+val_amsr2

     end do

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  endif

  deallocate(data_all,nrec) ! Deallocate data arrays
  call destroygrids    ! Deallocate satthin arrays
  if(diagnostic_reg.and.ntest>0 .and. mype_sub==mype_root) &
     write(6,*)'READ_AMSR2:  ',&
        'mype,ntest=',mype,ntest

  return


end subroutine read_amsr2

