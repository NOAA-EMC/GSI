#ifdef RR_CLOUDANALYSIS
SUBROUTINE  gsdcloudanalysis(mype)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsdcloudanalysis      driver for generalized cloud/hydrometeor analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT: 
!  This subroutine serves as a driver for generalized cloud/hydrometeor analysis
!
! PROGRAM HISTORY LOG:
!    2008-12-20  Hu  Add NCO document block
!    2010-04-30  Hu  Clean the code to meet GSI standard
!    2011-05-29  Todling - extra cloud-guess from MetGuess-Bundle (see Remark 1)
!                          some fields now from wrf_mass_guess_mod
!
!
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!  1. Notice that now the fields point to the instance of the GUESS at ntguessig
!     no longer wired to 1 (as originally) - however, make sure when running RUC
!     ntguessig is set correctly (see questions around definition of itsig)
!  2. Notice that some WRF-variable and grid specific is now defined in wrf_guess_mod
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET) at NOAA/ESRL - Boulder, CO
!
!$$$
!
!_____________________________________________________________________
!
! 
  use constants, only: zero,one,rad2deg
  use kinds,   only: r_single,i_kind, r_kind
  use gridmod, only: pt_ll,eta1_ll,aeta1_ll
  use gridmod, only: regional,wrf_mass_regional,regional_time
  use gridmod, only: nsig,lat2,lon2,istart,jstart
  use obsmod,  only: obs_setup,nsat1,ndat,dtype
  use guess_grids, only: ntguessig,ntguessfc
  use wrf_mass_guess_mod, only: soil_temp_cld,isli_cld,ges_xlon,ges_xlat,ges_tten
  use guess_grids, only: ges_q,ges_z,ges_ps,ges_tv
  use rapidrefresh_cldsurf_mod, only: dfi_radar_latent_heat_time_period,  &
                                      metar_impact_radius,                &
                                      metar_impact_radius_lowCloud
  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
!
! background
!
  real(r_single),allocatable:: t_bk(:,:,:)
  real(r_single),allocatable:: h_bk(:,:,:)
  real(r_single),allocatable:: p_bk(:,:,:)
  real(r_single),allocatable:: ps_bk(:,:)
  real(r_single),allocatable:: zh(:,:)
  real(r_single),allocatable:: q_bk(:,:,:)

  real(r_single),allocatable:: xlon(:,:)        ! 2D longitude in each grid
  real(r_single),allocatable:: xlat(:,:)        ! 2D latitude in each grid
  real(r_single),allocatable:: gsfc(:,:,:)
  real(i_kind),  allocatable:: xland(:,:)
  real(r_single),allocatable:: soiltbk(:,:)
!  real(r_single),allocatable:: z_lcl(:,:)       ! lifting condensation level
!
!  surface observation
!
  INTEGER(i_kind) :: NVARCLD_P
  PARAMETER (NVARCLD_P=13)

  INTEGER(i_kind)              :: numsao
  real(r_single), allocatable  :: OI(:)
  real(r_single), allocatable  :: OJ(:)
  INTEGER(i_kind),allocatable  :: OCLD(:,:)
  CHARACTER*10,   allocatable  :: OWX(:)
  real(r_single), allocatable  :: Oelvtn(:)
  real(r_single), allocatable  :: Odist(:)
  character(8),   allocatable  :: cstation(:)

!
!  lightning observation: 2D field in RR grid
!
  real(r_single),allocatable  :: lightning(:,:)
!
!  GOES - NASA LaRc cloud products: several 2D fields in RR grid
!
  real(r_single),allocatable  :: nasalarc_cld(:,:,:)

!
!  radar observation : 3D reflectvity in RR grid
!
  real(r_kind),allocatable :: ref_mos_3d(:,:,:)
  real(r_kind),allocatable :: ref_mosaic31(:,:,:)
  INTEGER(i_kind)          :: Nmsclvl_radar 
!
!  GOES - NESDIS cloud products : 2d fields
!
  real(r_single), allocatable :: sat_ctp(:,:)
  real(r_single), allocatable :: sat_tem(:,:)
  real(r_single), allocatable :: w_frac(:,:)
  integer(i_kind),allocatable :: nlev_cld(:,:)
!
! cloud/hydrometeor analysis variables
!
!=========================================================================
!  cld_cover_3d in the Generalized Cloud/Hydrometeor Analysis code
!   Definition:  3-d gridded observation-based information
!      including 0-1 cloud-fraction (with zero value for clear)
!      and negative values indicating "unknown" status.
!   cld_cover_3d is initialized with negative values.
!   cld_type_3d, pcp_type_3d, wthr_type_2d - similar to cld_cover_3d
!=========================================================================

  REAL(r_single), allocatable :: cld_cover_3d(:,:,:)  ! cloud cover
  INTEGER(i_kind),allocatable :: cld_type_3d(:,:,:)   ! cloud type
  INTEGER(i_kind),allocatable :: pcp_type_3d(:,:,:)   ! precipitation type
  INTEGER(i_kind),allocatable :: wthr_type_2d(:,:)    ! weather type type
  integer(i_kind),allocatable :: cloudlayers_i(:,:,:) ! 5 different layers 
                                                      ! 1= the number of layers
                                                      ! 2,4,... bottom
                                                      ! 3,5,... top
!
  REAL(r_single),allocatable :: cldwater_3d(:,:,:)    ! cloud water
  REAL(r_single),allocatable :: cldice_3d(:,:,:)      ! cloud ice
  REAL(r_single),allocatable :: rain_3d(:,:,:)        ! rain
  REAL(r_single),allocatable :: snow_3d(:,:,:)        ! snow
  REAL(r_single),allocatable :: graupel_3d(:,:,:)     ! graupel
  REAL(r_single),allocatable :: cldtmp_3d(:,:,:)      ! cloud temperature

  REAL(r_kind)    ::  thunderRadius=2.5_r_kind
  REAL(r_single)  ::  r_radius          ! influence radius of cloud based on METAR obs
  real(r_single)  ::  r_radius_lowCloud ! influence radius of low cloud to cloud top pressure
  INTEGER(i_kind) :: miss_obs_int
  REAL(r_kind)    :: miss_obs_real
  PARAMETER ( miss_obs_int = -99999  )
  PARAMETER ( miss_obs_real = -99999.0_r_kind )

!  REAL(r_kind)    :: Cloud_def_p
!  data  Cloud_def_p       / 0.000001_r_kind/
   REAL(r_kind),allocatable :: sumq(:,:,:)  ! total liquid water
!
! option in namelist
!
  INTEGER(i_kind) :: opt_cloudwaterice_retri  ! method for cloud water retrieval
  INTEGER(i_kind) :: opt_hydrometeor_retri    ! method for precipitation retrieval
  INTEGER(i_kind) :: opt_cloudtemperature     ! if open temperature adjustment scheme
  INTEGER(i_kind) :: istat_Surface,istat_NESDIS,istat_radar    ! 1 has observation
  INTEGER(i_kind) :: istat_NASALaRC,istat_lightning            ! 0 no observation
!
  REAL(r_kind), pointer :: ges_ql(:,:,:)  ! cloud water
  REAL(r_kind), pointer :: ges_qi(:,:,:)  ! could ice
  REAL(r_kind), pointer :: ges_qr(:,:,:)  ! rain
  REAL(r_kind), pointer :: ges_qs(:,:,:)  ! snow
  REAL(r_kind), pointer :: ges_qg(:,:,:)  ! graupel
!
!  misc.
!
  INTEGER(i_kind) :: i,j,k,itsig,itsfc
  REAL(r_kind)    :: qrlimit
  character(10)   :: obstype
  integer(i_kind) :: lunin, is, ier, istatus
  integer(i_kind) :: nreal,nchanl,ilat1s,ilon1s
  character(20)   :: isis

  real(r_kind)    :: refmax,snowtemp,raintemp
  integer(i_kind) :: imax, jmax
  real(r_kind)    :: dfi_lhtp
!
!
  itsig=1 ! _RT shouldn't this be ntguessig?
  itsfc=1 ! _RT shouldn't this be ntguessig?
!
  ier=0
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'ql',ges_ql,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qi',ges_qi,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qr',ges_qr,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qs',ges_qs,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qg',ges_qg,istatus);ier=ier+istatus
  if(ier/=0) return ! no guess, nothing to do

  if(mype==0) then
     write(6,*) '========================================'
     write(6,*) 'gsdcloudanalysis: Start generalized cloud analysis '
     write(6,*) '========================================'
  endif
!
!
!
  r_radius=metar_impact_radius
  r_radius_lowCloud=metar_impact_radius_lowCloud

  opt_hydrometeor_retri=2       ! 1=Kessler 2=Lin 3=Thompson
  opt_cloudtemperature=3        ! 3=latent heat, 4,5,6 = adiabat profile
  opt_cloudwaterice_retri=1     ! 1 = RUC layer saturation and autoconvert
                                ! 2 = convective 
!
! initialize the observation flag  
!
  istat_Surface=0
  istat_NESDIS=0
  istat_radar=0
  istat_lightning=0
  istat_NASALaRC=0
!
!  check consistency of the options
!

! Now either stratiform or cumulus cloud is considered in the cloud
!  water calculation. This leads to a limitation for the temperature
!  adjustment when stratiform cloud is chosen because adiabat profile
!  scheme based on the convection. This limitation may change when 
!  stratiform and cumulus cloud are both considered at the same time in the future.

  if(opt_cloudwaterice_retri == 1 .and. opt_cloudtemperature >= 4) then
     write(6,*) 'gsdcloudanalysis: ',&
       'inconsistent option for opt_cloudwaterice_retri and opt_cloudtemperature'
     write(6,*) 'gsdcloudanalysis: ',&
       'opt_cloudtemperature must be set to 3 when opt_cloudwaterice_retri =1'
     call stop2(113)
  endif
!
!----------------------------------------------
! 2. read observations                  
!----------------------------------------------
!
! 1.1   allocate observation fields
!

  allocate(ref_mos_3d(lon2,lat2,nsig))
  ref_mos_3d=miss_obs_real

  allocate(lightning(lon2,lat2))
  lightning=-9999.0_r_kind

  allocate(sat_ctp(lon2,lat2))
  allocate(sat_tem(lon2,lat2))
  allocate(w_frac(lon2,lat2))
  allocate(nlev_cld(lon2,lat2))
  sat_ctp=miss_obs_real
  sat_tem=miss_obs_real
  w_frac=miss_obs_real
  nlev_cld=miss_obs_int
!
! 1.2 start to read observations                 
!
  Nmsclvl_radar = -999
  lunin=55
  open(lunin,file=obs_setup,form='unformatted')
  rewind lunin

  numsao=0
  do is=1,ndat

     if(dtype(is) /= ' ' .and. nsat1(is) > 0)then
!
!  1.2.2 read in surface observations
!
        if( dtype(is) == 'mta_cld' ) then
           numsao=nsat1(is) 
           allocate(OI(numsao))
           allocate(OJ(numsao))
           allocate(OCLD(NVARCLD_P,numsao))
           allocate(OWX(numsao))
           allocate(Oelvtn(numsao))
           allocate(Odist(numsao))
           allocate(cstation(numsao))
           call read_Surface(mype,lunin,regional_time,istart(mype+1),jstart(mype+1),lon2,lat2, &
                             numsao,NVARCLD_P,OI,OJ,OCLD,OWX,Oelvtn,Odist,cstation)
           if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                                  &
                        'Surface cloud observations are read in successfully'
           istat_Surface=1

        elseif( dtype(is) == 'gos_ctp' ) then 
!
!  1.2.4 read in NESDIS cloud products
!

           call read_NESDIS(mype,lunin,nsat1(is),regional_time,istart(mype+1),            &
                            jstart(mype+1),lon2,lat2,sat_ctp,sat_tem,w_frac)
           if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                             &
                         'NESDIS cloud products are read in successfully'
           istat_NESDIS = 1 

        elseif( dtype(is) == 'rad_ref' ) then
!
!  1.2.6 read in reflectivity mosaic
!
           allocate( ref_mosaic31(lon2,lat2,31) )
           ref_mosaic31=-9999.0_r_kind

           call read_radar_ref(mype,lunin,regional_time,istart(mype+1),jstart(mype+1), &
                              lon2,lat2,Nmsclvl_radar,nsat1(is),ref_mosaic31)
           if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                         &
                         ' radar reflectivity is read in successfully'
           istat_radar=1

        elseif( dtype(is)=='lghtn' ) then
!
!  1.2.8 read in lightning
!
           call read_Lightning2cld(mype,lunin,regional_time,istart(mype+1),jstart(mype+1), &
                                   lon2,lat2,nsat1(is),lightning)
           if(mype == 0) write(6,*) 'gsdcloudanalysis: Lightning is read in successfully'
           istat_lightning = 1 

        elseif( dtype(is) =='larccld' ) then
!
!  1.2.9 read in NASA LaRC cloud products
!
           allocate(nasalarc_cld(lon2,lat2,5))
           nasalarc_cld=miss_obs_real

           call read_NASALaRC(mype,lunin,nsat1(is),regional_time,istart(mype+1),   &
                              jstart(mype+1),lon2,lat2,nasalarc_cld)
           if(mype == 0) write(6,*) 'gsdcloudanalysis:',                       &
                         'NASA LaRC cloud products are read in successfully'
           istat_NASALaRC = 1

        else
!
!  1.2.12  all other observations 
!
           read(lunin)  obstype,isis,nreal,nchanl
           read(lunin)
        endif   ! dtype
     endif
  enddo   ! is
  close(lunin)
!
!  1.4  if there are NASA LaRC cloud products, use them to replace NESDIS ones.
!       So we use NASA LaRC data in the same way as NESDIS ones
!
  if(istat_NASALaRC == 1 ) then
     sat_ctp(:,:) = nasalarc_cld(:,:,1)
     sat_tem(:,:) = nasalarc_cld(:,:,2)
     w_frac(:,:)  = nasalarc_cld(:,:,3)
     nlev_cld(:,:)= int(nasalarc_cld(:,:,5))
     istat_NESDIS =istat_NASALaRC
  endif
!
!  1.6 check if data available: if no data in this subdomain, return. 
!
  if( (istat_radar + istat_Surface + istat_NESDIS + istat_lightning ) == 0 ) then
     write(6,*) ' No cloud observations available, return', mype
     deallocate(ref_mos_3d,lightning,sat_ctp,sat_tem,w_frac,nlev_cld)
     return
  endif

!
!----------------------------------------------
! 2. allocated background arrays and read background  
!    further observation data process before cloud analysis
!----------------------------------------------
!
! 2.2   allocate background and analysis fields
!
  allocate(t_bk(lon2,lat2,nsig))
  allocate(h_bk(lon2,lat2,nsig))
  allocate(p_bk(lon2,lat2,nsig))
  allocate(ps_bk(lon2,lat2))
  allocate(zh(lon2,lat2))
  allocate(q_bk(lon2,lat2,nsig))
  allocate(sumq(lon2,lat2,nsig))
  
  allocate(xlon(lon2,lat2))
  allocate(xlat(lon2,lat2))
  allocate(xland(lon2,lat2))
  allocate(soiltbk(lon2,lat2))
!  allocate(z_lcl(lon2,lat2))    
  
  allocate(cldwater_3d(lon2,lat2,nsig))
  allocate(cldice_3d(lon2,lat2,nsig))
  allocate(rain_3d(lon2,lat2,nsig))
  allocate(snow_3d(lon2,lat2,nsig))
  allocate(graupel_3d(lon2,lat2,nsig))
  allocate(cldtmp_3d(lon2,lat2,nsig))
  cldwater_3d=miss_obs_real
  cldice_3d=miss_obs_real
  rain_3d=miss_obs_real
  snow_3d=miss_obs_real
  graupel_3d=miss_obs_real
  cldtmp_3d=miss_obs_real
!          
! 2.4 read in background fields
!          
  do j=1,lat2                
     do i=1,lon2
        zh(i,j)     =ges_z(j,i,itsfc)               !  terrain in meter
        ps_bk(i,j)  =ges_ps(j,i,itsfc)*10.0_r_single!  surace pressure in mb
        xland(i,j)  =isli_cld(j,i,itsfc)            !  0=water, 1=land, 2=ice
        soiltbk(i,j)=soil_temp_cld(j,i,itsfc)       !  soil temperature
        xlon(i,j)   =ges_xlon(j,i,itsfc)*rad2deg    !  longitude back to degree
        xlat(i,j)   =ges_xlat(j,i,itsfc)*rad2deg    !  latitude  back to degree
     ENDDO
  ENDDO

  do k=1,nsig
     do j=1,lat2
        do i=1,lon2
           q_bk(i,j,k)=ges_q(j,i,k,itsig)                           ! specific humidity
           t_bk(i,j,k)=ges_tv(j,i,k,itsig)/                                  &
                     (1.0_r_single+0.61_r_single*q_bk(i,j,k))   ! virtual temp to temp
        ENDDO
     ENDDO
  ENDDO
  call BackgroundCld(mype,lon2,lat2,nsig,t_bk,p_bk,ps_bk,q_bk,h_bk,    &
             zh,pt_ll,eta1_ll,aeta1_ll,regional,wrf_mass_regional)

!
!  2.6 vertical interpolation of radar reflectivity
!
  if(istat_radar ==  1 ) then
     call vinterp_radar_ref(mype,lon2,lat2,nsig,Nmsclvl_radar, &
                          ref_mos_3d,ref_mosaic31,h_bk,zh)
     deallocate( ref_mosaic31 )
     call build_missing_REFcone(mype,lon2,lat2,nsig,ref_mos_3d,h_bk)
  endif
!
!  2.8 convert lightning to reflectivity 
!
  if(istat_lightning ==  1 ) then
     call convert_lghtn2ref(mype,lon2,lat2,nsig,ref_mos_3d,lightning,h_bk)
  endif
!
!
!----------------------------------------------
! 3.  Calculate 3-d cloud cover obs-information field (cld_cover_3d), 
!               cloud type, precipitation type 
!----------------------------------------------
!
  allocate(cld_cover_3d(lon2,lat2,nsig))
  allocate(cld_type_3d(lon2,lat2,nsig))
  allocate(wthr_type_2d(lon2,lat2))
  allocate(pcp_type_3d(lon2,lat2,nsig))
  allocate(cloudlayers_i(lon2,lat2,21))
  cld_cover_3d=miss_obs_real
  cld_type_3d =miss_obs_int
  wthr_type_2d=miss_obs_int
  pcp_type_3d =miss_obs_int
!
!
  if(istat_Surface ==  1) then
     call cloudCover_surface(mype,lat2,lon2,nsig,r_radius,thunderRadius,  &
              t_bk,p_bk,q_bk,h_bk,zh,                                     &
              numsao,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn,Odist,        &
              cld_cover_3d,cld_type_3d,wthr_type_2d,pcp_type_3d)
     if(mype == 0) write(6,*) 'gsdcloudanalysis:',                        &  
                   'success in cloud cover analysis using surface data'
  endif

  if(istat_NESDIS == 1 ) then
     call cloudCover_NESDIS(mype,regional_time,lat2,lon2,nsig,            &
                         xlon,xlat,t_bk,p_bk,h_bk,zh,xland,               &
                         soiltbk,sat_ctp,sat_tem,w_frac,nlev_cld,         &
                         cld_cover_3d,cld_type_3d,wthr_type_2d)
     if(mype == 0) write(6,*) 'gsdcloudanalysis:',                        & 
                   ' success in cloud cover analysis using NESDIS data'
  endif

  if(istat_radar == 1 .or. istat_lightning == 1 ) then
     call cloudCover_radar(mype,lat2,lon2,nsig,h_bk,zh,ref_mos_3d,  &
                           cld_cover_3d,cld_type_3d,wthr_type_2d)
     if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                 & 
                   ' success in cloud cover analysis using radar data'
  endif

!
!----------------------------------------------
! 4.  Calculate 3-d cloud water and ice  
!     Calculate 3-d hydrometeors 
!     Calculate radar temperature tendency
!     Calculate in cloud temperature
!     moisture adjustment (to do)
!----------------------------------------------
!
! 4.2 find the cloud layers
!

  call cloudLayers(lat2,lon2,nsig,h_bk,zh,cld_cover_3d,               &
                   cld_type_3d,cloudlayers_i)
  if(mype==0) write(6,*) 'gsdcloudanalysis: success in finding cloud layers'
!
! 4.4 decide the cloud type
!
  call cloudType(lat2,lon2,nsig,h_bk,t_bk,p_bk,ref_mos_3d,            &
                 cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i)
  if(mype==0)  write(6,*) 'gsdcloudanalysis: success in deciding cloud types'
!
! 4.6 calculate liquid water content
!
  if(opt_cloudwaterice_retri == 1 ) then
     call cloudLWC_stratiform(mype,lat2,lon2,nsig,q_bk,t_bk,p_bk,      &
                  cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i, &
                  cldwater_3d,cldice_3d)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                      &
                 'success in modifying hydrometeors for stratiform clouds '

  elseif (opt_cloudwaterice_retri == 2) then
     call cloudLWC_Cumulus(lat2,lon2,nsig,h_bk,t_bk,p_bk,              &
                  cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i, &
                  cldwater_3d,cldice_3d,cldtmp_3d)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                      &
                 ' success in modifying hydrometeors from radar reflectivity'
  else
     write(6,*)'gsdcloudanalysis: ',                                   &
      ' Invalid cloud water calculation option, check opt_cloudwaterice_retri'
     call stop2(113)
  endif
!
! 4.8 calculate hydrometeors
!

  if(istat_radar == 1 .or. istat_lightning == 1) then
     call PrecipType(lat2,lon2,nsig,t_bk,p_bk,q_bk,ref_mos_3d,         &
                    wthr_type_2d,pcp_type_3d)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                      &
                 ' success in deciding precipitation type'

     call PrecipMxR_radar(mype,lat2,lon2,nsig,t_bk,p_bk,ref_mos_3d,pcp_type_3d, &
                  rain_3d,snow_3d,graupel_3d,opt_hydrometeor_retri)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                               &
                 ' success in determining hydrometeor types from radar refl'
  endif
!
! 4.10 radar temperature tendency for DFI
!
  dfi_lhtp=dfi_radar_latent_heat_time_period
  if (istat_NESDIS==1) then
     call radar_ref2tten(mype,istat_radar,istat_lightning,lon2,lat2,nsig,ref_mos_3d, &
                       cld_cover_3d,p_bk,t_bk,ges_tten(:,:,:,itsig),dfi_lhtp,sat_ctp)
  else
     call radar_ref2tten_nosat(mype,istat_radar,istat_lightning,lon2,lat2,nsig,      &
                       ref_mos_3d,cld_cover_3d,p_bk,t_bk,ges_tten(:,:,:,itsig),dfi_lhtp)
  endif

!
! 4.12  temperature adjustment
!
!  call TempAdjust(mype,lat2,lon2,nsig,opt_cloudtemperature, t_bk, p_bk, w_bk, q_bk, &
!                   cldwater_3d,cldice_3d,cldtmp_3d)
!
!----------------------------------------------
! 5.  the final analysis or update background
!----------------------------------------------
!
!  the final analysis of cloud 
!

  DO k=1,nsig
     DO j=2,lat2-1
        DO i=2,lon2-1
           sumq(i,j,k)= ges_ql(j,i,k) + ges_qi(j,i,k)
           if( cld_cover_3d(i,j,k) > -0.001_r_kind ) then 
              if( cld_cover_3d(i,j,k) > 0.6_r_kind ) then 
                 cldwater_3d(i,j,k) = max(0.001_r_kind*cldwater_3d(i,j,k),ges_ql(j,i,k))
                 cldice_3d(i,j,k)   = max(0.001_r_kind*cldice_3d(i,j,k),ges_qi(j,i,k))
              else   ! clean  cloud
                 cldwater_3d(i,j,k) = zero
                 cldice_3d(i,j,k) = zero
              endif
           else   ! unknown, using background values
              cldwater_3d(i,j,k) = ges_ql(j,i,k)
              cldice_3d(i,j,k) = ges_qi(j,i,k)
           endif
        END DO
     END DO
  END DO
!
!  the final analysis of precipitation
!

  qrlimit=15.0_r_kind
  DO k=1,nsig
     DO j=2,lat2-1
        DO i=2,lon2-1
           snowtemp=snow_3d(i,j,k) 
           raintemp=rain_3d(i,j,k) 
           rain_3d(i,j,k) = ges_qr(j,i,k)
           snow_3d(i,j,k) = ges_qs(j,i,k)
           graupel_3d(i,j,k) = ges_qg(j,i,k)
           if(ref_mos_3d(i,j,k) > zero ) then
              snow_3d(i,j,k) = 0.001_r_kind*MIN(max(snowtemp,zero),qrlimit)
              rain_3d(i,j,k) = 0.001_r_kind*MIN(max(raintemp,zero),qrlimit)
           elseif(ref_mos_3d(i,j,k) <= zero .and. ref_mos_3d(i,j,k) > -100.0_r_kind ) then
              rain_3d(i,j,k) = zero
              snow_3d(i,j,k) = zero
              graupel_3d(i,j,k) = zero
           else
              rain_3d(i,j,k) = ges_qr(j,i,k)
              snow_3d(i,j,k) = ges_qs(j,i,k)
              graupel_3d(i,j,k) = ges_qg(j,i,k)
           endif
        END DO
     END DO
  END DO
!
!  remove any negative hydrometeor mixing ratio values
!
  DO k=1,nsig
     DO j=2,lat2-1
        DO i=2,lon2-1
           cldwater_3d(i,j,k)= max(0.0_r_single,cldwater_3d(i,j,k))
           cldice_3d(i,j,k)  = max(0.0_r_single,cldice_3d(i,j,k))
           rain_3d(i,j,k)    = max(0.0_r_single,rain_3d(i,j,k))
           snow_3d(i,j,k)    = max(0.0_r_single,snow_3d(i,j,k))
           graupel_3d(i,j,k) = max(0.0_r_single,graupel_3d(i,j,k))
        END DO
     END DO
  END DO
 
!
!
  call cloud_saturation(mype,lat2,lon2,nsig,q_bk,t_bk,p_bk,      &
                 cld_cover_3d,wthr_type_2d,cldwater_3d,cldice_3d)

!  call check_cloud(mype,lat2,lon2,nsig,q_bk,rain_3d,snow_3d,graupel_3d, &
!             cldwater_3d,cldice_3d,t_bk,p_bk,h_bk,                      &
!             numsao,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn,cstation,    &
!             sat_ctp,cld_cover_3d)
!----------------------------------------------
! 6.  save the analysis results
!----------------------------------------------
!
  do k=1,nsig
     do j=1,lat2
        do i=1,lon2
           ges_q(j,i,k,itsig)=q_bk(i,j,k)/(1+q_bk(i,j,k))   ! Here q is mixing ratio kg/kg, 
                                                        ! need to convert to specific humidity
           ges_qr(j,i,k)=rain_3d(i,j,k)
           ges_qs(j,i,k)=snow_3d(i,j,k)
           ges_qg(j,i,k)=graupel_3d(i,j,k)
           ges_ql(j,i,k)=cldwater_3d(i,j,k)
           ges_qi(j,i,k)=cldice_3d(i,j,k)
        ENDDO
     ENDDO
  ENDDO
!
!----------------------------------------------
! 7.  release space
!----------------------------------------------
!
  deallocate(cld_cover_3d,cld_type_3d,wthr_type_2d, &
             pcp_type_3d,cloudlayers_i)
  deallocate(t_bk,h_bk,p_bk,ps_bk,zh,q_bk,sumq)
  deallocate(xlon,xlat,xland,soiltbk)
  deallocate(cldwater_3d,cldice_3d,rain_3d,snow_3d,graupel_3d,cldtmp_3d)

  if(istat_Surface ==  1 ) then
     deallocate(OI,OJ,OCLD,OWX,Oelvtn,Odist,cstation)
  endif
  if(istat_NASALaRC == 1 ) then
     deallocate(nasalarc_cld)
  endif

  deallocate(sat_ctp,sat_tem,w_frac,nlev_cld)
  deallocate(ref_mos_3d,lightning)

  if(mype==0) then
     write(6,*) '========================================'
     write(6,*) 'gsdcloudanalysis: generalized cloud analysis finished:',mype
     write(6,*) '========================================'
  endif

END SUBROUTINE gsdcloudanalysis
#else /* Start no RR cloud analysis library block */
SUBROUTINE  gsdcloudanalysis(mype)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsdcloudanalysis      driver for generalized cloud/hydrometeor analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT:
!  This subroutine serves as a driver for generalized cloud/hydrometeor analysis
!
! PROGRAM HISTORY LOG:
!    2008-12-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
!
! USAGE:
!   INPUT FILES:
!     mype - processor ID that does this IO
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Linux cluster (WJET) at NOAA/ESRL - Boulder, CO
!
!$$$
!_____________________________________________________________________
!

  use kinds, only: i_kind
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
!

  if( mype == 0) write(6,*)'gsdcloudanalysis:  dummy routine, does nothing!'

END SUBROUTINE gsdcloudanalysis
#endif /* End no RR cloud analysis library block */
