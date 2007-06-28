!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  guess_grids --- Guess-related grid definitions
!
! !INTERFACE:
!

module guess_grids

! !USES:
 
  use kinds, only: r_single,r_kind,i_kind

  use gridmod, only: regional
  use gridmod, only: wrf_nmm_regional
  use gridmod, only: eta1_ll
  use gridmod, only: eta2_ll
  use gridmod, only: aeta1_ll
  use gridmod, only: aeta2_ll
  use gridmod, only: pdtop_ll
  use gridmod, only: pt_ll
  use regional_io, only: update_pint

! !DESCRIPTION: module containing variables related to the guess fields
!
! !REVISION HISTORY:
!
!   2003-12-01  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2005-06-01  treadon - add routine add_rtm_layers
!   2005-06-03  parrish - add horizontal derivatives of guess fields
!   2005-06-10  devenyi/treadon - initialize nfldsig and nfldsfc
!   2005-07-06  parrish - add update_pint, arrays ges_pint, ges_pd
!   2005-08-03  parrish - add array to hold roughness length
!   2005-09-29  kleist - add derivatives of terrain, move prsi allocation
!   2005-11-21  kleist - add tendency arrays
!   2005-11-29  derber - add ozmz remove psfcg
!   2005-11-30  derber - combine create_atm_grids and create_pcp_grids (and destroys)
!   2006-02-02  treadon - prefix prsi,prsl,lnprsl,prslk with "ges_"
!   2006-03-07  treadon - remove ges_prslk (no longer needed)
!   2006-04-14  treadon - add bias_tskin
!   2006-04-17  treadon - add ges_psfcavg and ges_prslavg
!   2006-04-21  kleist - add ges divt and agvt arrays
!   2006-07-28  derber  - clean up add_rtm_layers routine 
!   2006-07-28  derber  - add ges_tsen (sensible temperature) array
!   2006-07-31  kleist - use ges_ps instead of ln(ps)
!   2006-09-20  cucurull - add ln(ges_prsi) array
!   2006-09-29  treadon - add flags to control 10m wind factor recompute
!
! !AUTHOR: 
!   kleist           org: np20                date: 2003-12-01
!
!EOP
!-------------------------------------------------------------------------

  logical:: sfcmod_gfs = .false.    ! .true. = recompute 10m wind factor using gfs physics
  logical:: sfcmod_mm5 = .false.    ! .true. = recompute 10m wind factor using mm5 physics

  integer(i_kind) ntguessig         ! location of actual guess time for sigma fields
  integer(i_kind):: nfldsig = 100   ! number of guess sigma times read in
  integer(i_kind) ntguessfc         ! location of actual guess time for sfc fields
  integer(i_kind):: nfldsfc = 100   ! number of guess surface times read in
  integer(i_kind):: ifact10 = 0     ! 0 = use 10m wind factor from guess
  
  integer(i_kind),dimension(100)::ifilesfc  ! array used to open the correct surface guess files
  integer(i_kind),dimension(100)::ifilesig  ! array used to open the correct sigma guess files
  
  integer(i_kind),allocatable,dimension(:,:,:):: isli    ! snow/land/ice mask
  integer(i_kind),allocatable,dimension(:,:,:):: isli_g  ! isli on horiz/global grid

 
  real(r_single)::  tracers  ! number or tracers read in/written out
  real(r_single)::  vtid     ! tracer variable id from sigma header
  real(r_single)::  pdryini  ! global mean dry mass of the atmosphere in kPa
  real(r_single)::  xncld    ! number of clouds from sigma header


  real(r_kind),dimension(100):: hrdifsig  ! times for guess sigma fields
  real(r_kind),dimension(100):: hrdifsfc  ! times for guess surface fields

  real(r_kind):: ges_psfcavg                            ! average guess surface pressure 
  real(r_kind),allocatable,dimension(:):: ges_prslavg   ! average guess pressure profile

  real(r_kind),allocatable,dimension(:,:):: tropprs ! guess tropopause pressure

  real(r_kind),allocatable,dimension(:,:,:):: fact10    ! 10 meter wind factor
  real(r_kind),allocatable,dimension(:,:,:):: sfct      ! guess skin temperature
  real(r_kind),allocatable,dimension(:,:,:):: sfct_lat  ! guess skin temperature/lat
  real(r_kind),allocatable,dimension(:,:,:):: sfct_lon  ! guess skin temperature/lon
  real(r_kind),allocatable,dimension(:,:,:):: sno       ! snow-ice mask
  real(r_kind),allocatable,dimension(:,:,:):: veg_type  ! vegetation type
  real(r_kind),allocatable,dimension(:,:,:):: veg_frac  ! vegetation fraction(0-1.0)
  real(r_kind),allocatable,dimension(:,:,:):: sfc_rough ! sfc roughness length
  real(r_kind),allocatable,dimension(:,:,:):: soil_type ! soil type
  real(r_kind),allocatable,dimension(:,:,:):: soil_temp ! soil temperature of first layer
  real(r_kind),allocatable,dimension(:,:,:):: soil_moi  ! soil moisture of first layer
  

  real(r_kind),allocatable,dimension(:,:,:,:):: geop_hgtl ! guess geopotential height at mid-layers
  real(r_kind),allocatable,dimension(:,:,:,:):: geop_hgti ! guess geopotential height at level interfaces


! Bias and ges arrays
                                                        ! Bias Fields ...
  real(r_kind),allocatable,dimension(:,:)  :: bias_ps
  real(r_kind),allocatable,dimension(:,:)  :: bias_tskin
  real(r_kind),allocatable,dimension(:,:,:):: bias_vor
  real(r_kind),allocatable,dimension(:,:,:):: bias_div
  real(r_kind),allocatable,dimension(:,:,:):: bias_cwmr
  real(r_kind),allocatable,dimension(:,:,:):: bias_q
  real(r_kind),allocatable,dimension(:,:,:):: bias_oz
  real(r_kind),allocatable,dimension(:,:,:):: bias_tv
  real(r_kind),allocatable,dimension(:,:,:):: bias_u
  real(r_kind),allocatable,dimension(:,:,:):: bias_v
  
  real(r_kind),allocatable,dimension(:,:,:):: ges_z        ! topography
  real(r_kind),allocatable,dimension(:,:,:):: ges_z_lat    ! dz/dy
  real(r_kind),allocatable,dimension(:,:,:):: ges_z_lon    ! dz/dx
  real(r_kind),allocatable,dimension(:,:,:):: ges_ps     ! log(surface pressure)
  real(r_kind),allocatable,dimension(:,:,:):: ges_ps_lat ! log(ps)/lat for pcp routine
  real(r_kind),allocatable,dimension(:,:,:):: ges_ps_lon ! log(ps)/lon for pcp routine

                                                         ! Guess Fields ...
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsi  ! interface pressure
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsl  ! layer midpoint pressure
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_lnprsl! log(layer midpoint pressure)
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_lnprsi! log(interface pressure)
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_u     ! zonal wind
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_u_lat ! zonal wind/lat
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_u_lon ! zonal wind/lon
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_v     ! meridional wind
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_v_lat ! meridional wind/lat
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_v_lon ! meridional wind/lon
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_vor   ! vorticity
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_div   ! divergence
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_cwmr  ! cloud condensate mixing ratio 
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_cwmr_lat  ! cloud condensate mixing ratio/lat
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_cwmr_lon  ! cloud condensate mixing ratio/lon
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_q     ! vapor mixing ratio
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_qlon  ! q/lat for pcp routine advection calc
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_qlat  ! q/lon for pcp routine advection calc
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_oz    ! ozone mixing ratio
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_ozlat ! ozone mixing ratio/lat
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_ozlon ! ozone mixing ratio/lon
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_pint  ! pint variable (nmm only)
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_tv    ! virtual temperature
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_tsen  ! sensible temperature
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_tvlat ! tv/lat for pcp routine advection calc
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_tvlon ! tv/lon for pcp routine advection calc 

  real(r_kind),allocatable,dimension(:,:,:)::ges_pd        ! pdges (for nmm only)
  real(r_kind),allocatable,dimension(:,:,:):: ges_prs_ten  ! 3d pressure tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_u_ten    ! u tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_v_ten    ! v tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_tv_ten   ! Tv tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_q_ten    ! q tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_oz_ten   ! ozone tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_cwmr_ten ! cloud water tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_div_ten  ! divergence tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_agv_ten  ! ageostrophic vorticity tendency
  real(r_kind),allocatable,dimension(:):: ozmz ! global mean profile of ozone mixing ratio (used in background error)
  
contains

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: create_sfc_grids --- Allocate memory for surface related grids
!
! !INTERFACE:
!
  subroutine create_sfc_grids

! !USES:

   use gridmod, only: lat2,lon2,nlat,nlon
   use constants, only: zero

   implicit none

! !DESCRIPTION: allocate memory for surface related grids
!
! !REVISION HISTORY:
!   2003-12-01  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-28  treadon - remove subroutine call list, pass variables via modules
!   2005-06-03  parrish - allocate and initialize sfct_lat and sfct_lon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist          org: w/nmc20     date: 2003-12-01
!
!EOP
!-------------------------------------------------------------------------
   integer(i_kind):: i,j,k,istatus

    allocate( isli(lat2,lon2,nfldsfc),fact10(lat2,lon2,nfldsfc),&
         sfct(lat2,lon2,nfldsfc),sno(lat2,lon2,nfldsfc),&
         veg_type(lat2,lon2,nfldsfc),veg_frac(lat2,lon2,nfldsfc),&
         sfc_rough(lat2,lon2,nfldsfc),&
         soil_type(lat2,lon2,nfldsfc),soil_temp(lat2,lon2,nfldsfc),&
         soil_moi(lat2,lon2,nfldsfc),sfct_lat(lat2,lon2,nfldsfc),&
         sfct_lon(lat2,lon2,nfldsfc),isli_g(nlat,nlon,nfldsfc),&
         stat=istatus)
    if (istatus/=0) write(6,*)'CREATE_SFC_GRIDS:  allocate error, istatus=',&
         istatus,lat2,lon2,nlat,nlon,nfldsfc

    do k=1,nfldsfc
       do j=1,lon2
          do i=1,lat2
             sfct_lat(i,j,k)=zero
             sfct_lon(i,j,k)=zero
          end do
       end do
    end do
    return
  end subroutine create_sfc_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_ges_bias_grids --- Alloc grid for guess and bias
!
! !INTERFACE:
!
  subroutine create_ges_bias_grids(switch_on_derivatives,tendsflag,biascor)

! !USES:

    use constants,only: zero
    use gridmod, only: lat2,lon2,nsig
    implicit none

! !INPUT PARAMETERS:

    logical:: switch_on_derivatives    ! for for horizontal derivatives
    logical:: tendsflag                ! for time tendencies
    real(r_kind),intent(in):: biascor  ! flag for guess bias correction


! !OUTPUT PARAMETERS:

! !DESCRIPTION: allocate grids to hold guess and bias correction fields
!
! !REVISION HISTORY:
!   2004-06-03  treadon, original code
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2004-07-28  treadon - remove subroutine call list, pass variables via modules
!   2005-06-03  parrish - allocate/initialize _lat,_lon derivatives for u,v,cwmr,oz
!   2005-06-08  treadon - pass switch_on_derivatives via argument list
!   2005-07-06  parrish - add update_pint, arrays ges_pint, ges_pd
!   2005-07-27  kleist  - modified to include some shared arrays
!   2006-01-10  treadon - remove mype from calling list (not used)
!   2006-07-31  kleist  - use ges_ps arrays instead of ln(ps)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20     date: 2004-06-03
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,n,istatus

!   Allocate and zero guess grids
    allocate (ges_z(lat2,lon2,nfldsig),ges_ps(lat2,lon2,nfldsig),&
         ges_prsi(lat2,lon2,nsig+1,nfldsig),ges_prsl(lat2,lon2,nsig,nfldsig),&
         ges_lnprsl(lat2,lon2,nsig,nfldsig),ges_lnprsi(lat2,lon2,nsig+1,nfldsig),&
         ges_u(lat2,lon2,nsig,nfldsig),ges_v(lat2,lon2,nsig,nfldsig),&
         ges_vor(lat2,lon2,nsig,nfldsig),ges_div(lat2,lon2,nsig,nfldsig),&
         ges_cwmr(lat2,lon2,nsig,nfldsig),ges_q(lat2,lon2,nsig,nfldsig),&
         ges_oz(lat2,lon2,nsig,nfldsig),ges_tv(lat2,lon2,nsig,nfldsig),&
         ges_tsen(lat2,lon2,nsig,nfldsig),&
         ozmz(nsig),geop_hgtl(lat2,lon2,nsig,nfldsig), &
         geop_hgti(lat2,lon2,nsig+1,nfldsig),ges_prslavg(nsig),&
         tropprs(lat2,lon2),stat=istatus)
    if (istatus/=0) write(6,*)'CREATE_GES_BIAS_GRIDS:  allocate error1, istatus=',&
         istatus,lat2,lon2,nsig,nfldsig
    if(update_pint) then
       allocate(ges_pint(lat2,lon2,nsig+1,nfldsig),ges_pd(lat2,lon2,nfldsig),&
            stat=istatus)
       if (istatus/=0) write(6,*)'CREATE_GES_BIAS_GRIDS:  allocate error2, istatus=',&
         istatus,lat2,lon2,nsig,nfldsig
    endif


!  Default for ozmz (currently used for regional)
    ges_psfcavg=zero
    do i=1,nsig
       ozmz(i)=20.0_r_kind
       ges_prslavg(i)=zero
    end do

    do j=1,lon2
       do i=1,lat2
          tropprs(i,j)=zero
       end do
    end do

    do n=1,nfldsig
       do j=1,lon2
          do i=1,lat2
             ges_z(i,j,n)=zero
             ges_ps(i,j,n)=zero
          end do
       end do
    end do
    do n=1,nfldsig
       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                ges_prsl(i,j,k,n)=zero
                ges_lnprsl(i,j,k,n)=zero
                ges_u(i,j,k,n)=zero
                ges_v(i,j,k,n)=zero
                ges_vor(i,j,k,n)=zero
                ges_div(i,j,k,n)=zero
                ges_cwmr(i,j,k,n)=zero
                ges_q(i,j,k,n)=zero
                ges_oz(i,j,k,n)=zero
                ges_tv(i,j,k,n)=zero
                ges_tsen(i,j,k,n)=zero
!                ges_pint(i,j,k,n)=zero
                geop_hgtl(i,j,k,n)=zero
             end do
          end do
       end do
       do k=1,nsig+1
          do j=1,lon2
             do i=1,lat2
                ges_prsi(i,j,k,n)=zero
                ges_lnprsi(i,j,k,n)=zero
                geop_hgti(i,j,k,n)=zero
             end do
          end do
       end do
    end do
    if(update_pint) then
       do n=1,nfldsig
          do k=1,nsig+1
             do j=1,lon2
                do i=1,lat2
                   ges_pint(i,j,k,n)=zero
                end do
             end do
          end do
          do j=1,lon2
             do i=1,lat2
                ges_pd(i,j,n)=zero
             end do
          end do
       end do
    end if
    
!   If tendencies option on, allocate/initialize _ten arrays to zero
    if (tendsflag) then
      allocate(ges_prs_ten(lat2,lon2,nsig+1),ges_u_ten(lat2,lon2,nsig),&
               ges_v_ten(lat2,lon2,nsig),ges_tv_ten(lat2,lon2,nsig),&
               ges_q_ten(lat2,lon2,nsig),ges_oz_ten(lat2,lon2,nsig),&
               ges_cwmr_ten(lat2,lon2,nsig),ges_div_ten(lat2,lon2,nsig),&
               ges_agv_ten(lat2,lon2,nsig),stat=istatus)
      if (istatus/=0) write(6,*)'CREATE_GES_BIAS_GRIDS:  allocate error3, istatus=',&
           istatus,lat2,lon2,nsig
      do k=1,nsig
        do j=1,lon2
          do i=1,lat2
            ges_u_ten(i,j,k)=zero
            ges_v_ten(i,j,k)=zero
            ges_tv_ten(i,j,k)=zero
            ges_q_ten(i,j,k)=zero
            ges_cwmr_ten(i,j,k)=zero
            ges_oz_ten(i,j,k)=zero
            ges_prs_ten(i,j,k)=zero
            ges_div_ten(i,j,k)=zero
            ges_agv_ten(i,j,k)=zero
          end do
        end do
      end do
      do j=1,lon2
        do i=1,lat2
          ges_prs_ten(i,j,nsig+1)=zero
        end do
      end do
    end if

!   If derivatives option on, allocate and initialize derivatives arrays to 0.0
    if (switch_on_derivatives) then
       allocate(ges_u_lat(lat2,lon2,nsig,nfldsig),ges_u_lon(lat2,lon2,nsig,nfldsig),&
            ges_v_lat(lat2,lon2,nsig,nfldsig),ges_v_lon(lat2,lon2,nsig,nfldsig),&
            ges_cwmr_lat(lat2,lon2,nsig,nfldsig),ges_cwmr_lon(lat2,lon2,nsig,nfldsig),&
            ges_ozlat(lat2,lon2,nsig,nfldsig),ges_ozlon(lat2,lon2,nsig,nfldsig),&
            ges_ps_lat(lat2,lon2,nfldsig),ges_ps_lon(lat2,lon2,nfldsig),&
            ges_tvlat(lat2,lon2,nsig,nfldsig),ges_tvlon(lat2,lon2,nsig,nfldsig),&
            ges_qlat(lat2,lon2,nsig,nfldsig),ges_qlon(lat2,lon2,nsig,nfldsig),&
            ges_z_lat(lat2,lon2,nfldsig),ges_z_lon(lat2,lon2,nfldsig),&
            stat=istatus)
       if (istatus/=0) write(6,*)'CREATE_GES_BIAS_GRIDS:  allocate error4, istatus=',&
            istatus,lat2,lon2,nsig,nfldsig
       do n=1,nfldsig
          do k=1,nsig
             do j=1,lon2
                do i=1,lat2
                   ges_u_lat(i,j,k,n)=zero
                   ges_u_lon(i,j,k,n)=zero
                   ges_v_lat(i,j,k,n)=zero
                   ges_v_lon(i,j,k,n)=zero
                   ges_cwmr_lat(i,j,k,n)=zero
                   ges_cwmr_lon(i,j,k,n)=zero
                   ges_ozlat(i,j,k,n)=zero
                   ges_ozlon(i,j,k,n)=zero
                   ges_tvlat(i,j,k,n)=zero
                   ges_tvlon(i,j,k,n)=zero
                   ges_qlat(i,j,k,n)=zero
                   ges_qlon(i,j,k,n)=zero
                end do
             end do
          end do
          do j=1,lon2
             do i=1,lat2
                ges_ps_lat(i,j,n)=zero
                ges_ps_lon(i,j,n)=zero
                ges_z_lat(i,j,n)=zero
                ges_z_lon(i,j,n)=zero
             end do
          end do
       end do
    endif  ! end if switch_derivatives block

!   If requested, allocate and zero bias grids
    if (biascor>zero) then
       allocate(bias_ps(lat2,lon2),bias_tskin(lat2,lon2),&
            bias_vor(lat2,lon2,nsig),&
            bias_div(lat2,lon2,nsig),bias_cwmr(lat2,lon2,nsig),&
            bias_oz(lat2,lon2,nsig),bias_q(lat2,lon2,nsig),&
            bias_tv(lat2,lon2,nsig),bias_u(lat2,lon2,nsig),&
            bias_v(lat2,lon2,nsig),stat=istatus)
       if (istatus/=0) write(6,*)'CREATE_GES_BIAS_GRIDS:  allocate error5, istatus=',&
            istatus,lat2,lon2,nsig
       do j=1,lon2
          do i=1,lat2
             bias_ps(i,j)=zero
             bias_tskin(i,j)=zero
          end do
       end do
       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                bias_vor(i,j,k)=zero
                bias_div(i,j,k)=zero
                bias_tv(i,j,k)=zero
                bias_q(i,j,k)=zero
                bias_oz(i,j,k)=zero
                bias_cwmr(i,j,k)=zero
                bias_u(i,j,k)=zero
                bias_v(i,j,k)=zero
             end do
          end do
       end do
    endif

    return
  end subroutine create_ges_bias_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_ges_bias_grids --- Dealloc guess and bias fields
!
! !INTERFACE:
!
  subroutine destroy_ges_bias_grids(switch_on_derivatives,tendsflag,biascor)

! !USES:

    use constants,only:zero
    implicit none

! !INPUT PARAMETERS:
    logical:: switch_on_derivatives    ! flag for horizontal derivatives
    logical:: tendsflag                ! flag for tendency
    real(r_kind),intent(in):: biascor  ! flag for guess bias correction
    
! !DESCRIPTION: deallocate guess and bias grids
!
! !REVISION HISTORY:
!   2003-12-01  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2005-06-03  parrish - deallocate _lat,_lon arrays for u,v,cwmr,oz
!   2005-06-08  treadon - check flag to see if need to deallocate derivatives
!   2005-07-06  parrish - add update_pint, arrays ges_pint, ges_pd
!   2005-07-27  kleist  - modified to include some shared arrays
!   2006-07-31  kleist  - use ges_ps arrays instead of ln(ps)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   kleist          org: w/nmc20     date: 2003-12-01
!
!EOP
!-------------------------------------------------------------------------
    integer(i_kind):: istatus

    deallocate(ges_z,ges_ps,ges_prsi,ges_prsl,ges_lnprsl,ges_lnprsi,&
         ges_u,ges_v,ges_vor,ges_div,ges_cwmr,ges_q,&
         ges_oz,ges_tv,ges_tsen,ozmz,geop_hgtl,geop_hgti,ges_prslavg,&
         tropprs,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_GES_BIAS_GRIDS:  deallocate error1, istatus=',&
         istatus
    if(update_pint) then
       deallocate(ges_pint,ges_pd,stat=istatus)
       if (istatus/=0) &
            write(6,*)'DESTROY_GES_BIAS_GRIDS:  deallocate error2, istatus=',&
            istatus
    endif
    if (switch_on_derivatives) then
       deallocate(ges_u_lat,ges_u_lon,ges_v_lat,ges_v_lon,ges_cwmr_lat,&
            ges_cwmr_lon,ges_ozlat,ges_ozlon,&
            ges_ps_lat,ges_ps_lon,ges_tvlat,ges_tvlon,&
            ges_qlat,ges_qlon,ges_z_lat,ges_z_lon,stat=istatus)
       if (istatus/=0) &
            write(6,*)'DESTROY_GES_BIAS_GRIDS:  deallocate error3, istatus=',&
            istatus
    endif
    if (tendsflag) then
       deallocate(ges_u_ten,ges_v_ten,ges_tv_ten,ges_prs_ten,ges_q_ten,&
            ges_oz_ten,ges_cwmr_ten,ges_div_ten,ges_agv_ten,stat=istatus)
       if (istatus/=0) &
            write(6,*)'DESTROY_GES_BIAS_GRIDS:  deallocate error4, istatus=',&
            istatus
    endif
    if (biascor>zero) then
       deallocate(bias_ps,bias_tskin,bias_vor,bias_div,&
            bias_tv,bias_q,bias_oz,bias_cwmr,bias_u,bias_v,stat=istatus)
       if (istatus/=0) &
            write(6,*)'DESTROY_GES_BIAS_GRIDS:  deallocate error5, istatus=',&
            istatus
    endif
    return
  end subroutine destroy_ges_bias_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_sfc_grids --- Deallocate surface fields
!
! !INTERFACE:
!
  subroutine destroy_sfc_grids

! !USES:

   implicit none
   
! !DESCRIPTION: deallocate surface related grids
!
! !REVISION HISTORY:
!   2003-12-01  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-06-03  parrish - deallocate sfct_lat and sfct_lon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   kleist          org: w/nmc20     date: 2003-12-01
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind):: istatus

    deallocate(isli,isli_g,fact10,sno,veg_type,veg_frac,soil_type,&
         sfc_rough,soil_temp,soil_moi,sfct,sfct_lat,sfct_lon,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_SFC_GRIDS:  deallocate error, istatus=',&
         istatus

    return
  end subroutine destroy_sfc_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_prsges --- Populate guess pressure arrays
!
! !INTERFACE:
!
  subroutine load_prsges

! !USES:

    use constants,only: zero,one,rd_over_cp,one_tenth
    use gridmod, only: lat2,lon2,nsig,ak5,bk5,ck5,&
         regional,wrf_nmm_regional,wrf_mass_regional,pt_ll,aeta2_ll,&
         aeta1_ll,eta2_ll,pdtop_ll,eta1_ll,twodvar_regional
    implicit none

! !DESCRIPTION: populate guess pressure arrays
!
! !REVISION HISTORY:
!   2003-10-15  kleist
!   2004-03-22  parrish, regional capability added
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2004-07-28  treadon - remove subroutine call list, pass variables via modules
!   2005-05-24  pondeca - add regional surface analysis option
!   2006-04-14  treadon - unify global calculations to use ak5,bk5
!   2006-04-17  treadon - add ges_psfcavg and ges_prslavg for regional
!   2006-07-31  kleist  - use ges_ps instead of ln(ps)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist          org: w/nmc20     date: 2003-10-15
!
!EOP
!-------------------------------------------------------------------------

!   Declare local parameter
    real(r_kind),parameter:: ten = 10.0_r_kind
    real(r_kind),parameter:: r1013=1013.0_r_kind

!   Declare local variables
    real(r_kind) kap1,kapr
    integer(i_kind) i,j,k,jj

    kap1=rd_over_cp+one
    kapr=one/rd_over_cp

    do jj=1,nfldsig
      do k=1,nsig+1
        do j=1,lon2
          do i=1,lat2
            if(regional) then
              if (wrf_nmm_regional) &
                ges_prsi(i,j,k,jj)=one_tenth* &
                            (eta1_ll(k)*pdtop_ll + &
                             eta2_ll(k)*(ten*ges_ps(i,j,jj)-pdtop_ll-pt_ll) + &
                            pt_ll)
              if (wrf_mass_regional .or. twodvar_regional) &
                ges_prsi(i,j,k,jj)=one_tenth*(eta1_ll(k)*(ten*ges_ps(i,j,jj)-pt_ll) + pt_ll)
            else
                ges_prsi(i,j,k,jj)=ak5(k)+(bk5(k)*ges_ps(i,j,jj))
            endif
            ges_lnprsi(i,j,k,jj)=log(ges_prsi(i,j,k,jj))
          end do
        end do
      end do
    end do

    if(regional) then
      if (wrf_nmm_regional) then
! load using aeta coefficients
        do jj=1,nfldsig
          do k=1,nsig
            do j=1,lon2
              do i=1,lat2
                ges_prsl(i,j,k,jj)=one_tenth* &
                            (aeta1_ll(k)*pdtop_ll + &
                             aeta2_ll(k)*(ten*ges_ps(i,j,jj)-pdtop_ll-pt_ll) + &
                             pt_ll)
                ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))
              end do
            end do
          end do
        end do
      end if   ! end if wrf_nmm regional block
      if (wrf_mass_regional .or. twodvar_regional) then
! load using aeta coefficients
        do jj=1,nfldsig
          do k=1,nsig
            do j=1,lon2
              do i=1,lat2
                ges_prsl(i,j,k,jj)=one_tenth*(aeta1_ll(k)*(ten*ges_ps(i,j,jj)-pt_ll)+pt_ll)
                ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))
              end do
            end do
          end do
        end do
      end if   ! end if wrf_mass regional block

    else

!       load mid-layer pressure by using phillips vertical interpolation
        do jj=1,nfldsig
          do j=1,lon2
            do i=1,lat2
              do k=1,nsig
                ges_prsl(i,j,k,jj)=((ges_prsi(i,j,k,jj)**kap1-ges_prsi(i,j,k+1,jj)**kap1)/&
                      (kap1*(ges_prsi(i,j,k,jj)-ges_prsi(i,j,k+1,jj))))**kapr
                ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))
              end do
            end do
          end do
        end do

    end if  !  end regional/global block

! For regional applications only, load variables containing mean
! surface pressure and pressure profile at the layer midpoints
    if (regional) then
       ges_psfcavg = r1013
       if (wrf_nmm_regional) then
          do k=1,nsig
             ges_prslavg(k)=aeta1_ll(k)*pdtop_ll+aeta2_ll(k)*(r1013-pdtop_ll-pt_ll)+pt_ll
          end do
       else
          do k=1,nsig
             ges_prslavg(k)=aeta1_ll(k)*(r1013-pt_ll)+pt_ll
          end do
       endif
    endif


    return
  end subroutine load_prsges

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_geop_hgt --- Populate guess geopotential height
!
! !INTERFACE:
!
  subroutine load_geop_hgt

! !USES:

    use constants, only: rd, grav, half
    use gridmod, only: nlat, nlon, lat2, lon2, nsig, istart, rlats, &
         twodvar_regional

    implicit none

! !INPUT PARAMETERS:


! !DESCRIPTION: populate guess geopotential height
!
! !REVISION HISTORY:
!   2003-10-15  treadon
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-10-28  treadon - replace "tiny" with "tiny_r_kind"
!   2004-12-15  treadon - replace use of Paul van Delst's Geopotential
!                         function with simple integration of hydrostatic
!                         equation (done to be consistent with Lidia
!                         Cucurull's GPS work)
!   2005-05-24  pondeca - add regional surface analysis option
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20      date: 2003-10-15
!
!EOP
!-------------------------------------------------------------------------

!   Declare local parameter
    real(r_kind),parameter:: ten = 10.0_r_kind

    integer(i_kind) error_status,i,j,k,jj
    real(r_kind) h,dz,rdog
    real(r_kind),dimension(nsig+1):: height

    if (twodvar_regional) return

!   Compute geopotential height at midpoint of each layer
    rdog = rd/grav
    do jj=1,nfldsig
       do j=1,lon2
          do i=1,lat2
             k  = 1
             h  = rdog * ges_tv(i,j,k,jj)
             dz = h * log(ges_prsi(i,j,k,jj)/ges_prsl(i,j,k,jj))
             height(k) = ges_z(i,j,jj) + dz

             do k=2,nsig
                h  = rdog * half * (ges_tv(i,j,k-1,jj)+ges_tv(i,j,k,jj))
                dz = h * log(ges_prsl(i,j,k-1,jj)/ges_prsl(i,j,k,jj))
                height(k) = height(k-1) + dz
             end do

             do k=1,nsig
                geop_hgtl(i,j,k,jj)=height(k) - ges_z(i,j,jj)
             end do
          end do
       end do
    end do

!   Compute geopotential height at interface between layers
    do jj=1,nfldsig
       do j=1,lon2
          do i=1,lat2
             k=1
             height(k) = ges_z(i,j,jj)

             do k=2,nsig
                h  = rdog * ges_tv(i,j,k-1,jj)
                dz = h * log(ges_prsi(i,j,k-1,jj)/ges_prsi(i,j,k,jj))
                height(k) = height(k-1) + dz
             end do

             k=nsig+1
             h = rdog * ges_tv(i,j,k-1,jj)
             dz = h * log(ges_prsi(i,j,k-1,jj)/ges_prsl(i,j,k-1,jj))
             height(k) = height(k-1) + dz

             do k=1,nsig+1
                geop_hgti(i,j,k,jj)=height(k) - ges_z(i,j,jj)
             end do
          end do
       end do
    end do

    return
  end subroutine load_geop_hgt

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: add_rtm_layers --- Add pressure layers for RTM use
!
! !INTERFACE:
!
  subroutine add_rtm_layers(prsitmp,prsltmp,prsitmp_ext,prsltmp_ext,klevel)

! !USES:

    use constants, only: zero,half
    use gridmod, only: nsig,msig,nlayers
    use crtm_parameters, only: toa_pressure

    implicit none

! !INPUT PARAMETERS:
    integer(i_kind),dimension(msig),intent(out):: klevel

    real(r_kind),dimension(nsig+1),intent(in):: prsitmp
    real(r_kind),dimension(nsig),intent(in):: prsltmp

    real(r_kind),dimension(msig+1),intent(out):: prsitmp_ext
    real(r_kind),dimension(msig),intent(out):: prsltmp_ext


! !DESCRIPTION:  Add pressure layers for use in RTM
!
! !REVISION HISTORY:
!   2005-06-01  treadon
!   2006-05-10  derber modify how levels are added above model top
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20      date: 2005-06-01
!
!EOP
!-------------------------------------------------------------------------

!   Declare local variables
    integer(i_kind) k,kk,l
    real(r_kind) dprs

!   Check if model top pressure above rtm top pressure
    if (prsitmp(nsig) < toa_pressure)then
       write(6,*)'ADD_RTM_LAYERS:  model top pressure=',prsitmp(nsig),&
            ' above rtm top presure=',toa_pressure
       call stop2(35)
    end if

!   Linear in pressure sub-divsions
    kk=0
    do k = 1,nsig
       if (nlayers(k)<=1) then
          kk = kk + 1
          prsltmp_ext(kk) = prsltmp(k)
          prsitmp_ext(kk) = prsitmp(k)
          klevel(kk) = k
       else
          if (k/=nsig) then
             dprs = (prsitmp(k+1)-prsitmp(k))/nlayers(k)
          else
             dprs = (toa_pressure-prsitmp(k))/nlayers(k)
          end if
          prsitmp_ext(kk+1) = prsitmp(k)
          do l=1,nlayers(k)
             kk=kk +1
             prsitmp_ext(kk+1) = prsitmp(k) + dprs*l
             prsltmp_ext(kk) = half*(prsitmp_ext(kk+1)+prsitmp_ext(kk))
             klevel(kk) = k
          end do
       endif
    end do

!   Set top of atmosphere pressure
    prsitmp_ext(msig+1) = toa_pressure

  end subroutine add_rtm_layers

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_fact10 --- Compute 10m wind factor
!
! !INTERFACE:
!
  subroutine load_fact10

! !USES:

    use gridmod, only: iglobal,nlat,nlon,ijn,displs_g,itotsub,&
         lat1,lon1,lat2,lon2
    implicit none

! !INPUT PARAMETERS:

! !DESCRIPTION: compute 10m wind factor
!
! !REVISION HISTORY:
!   2006-09-26  treadon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20      date: 2006-09-26
!
!EOP
!-------------------------------------------------------------------------

!   Declare local parameters
    logical,parameter:: gfs_sfcmod = .true.

!   Declare local variables
    logical iqtflg
    integer(i_kind):: i,j,it,itt,nt
    integer(i_kind),dimension(nfldsfc):: indx
    real(r_kind):: u10ges,v10ges,t2ges,q2ges,regime,slimsk

    nt=0
    indx=1
    do i=1,nfldsfc
       if(abs(hrdifsfc(i)-hrdifsig(i))<0.001_r_kind) then
          nt=nt+1
          indx(nt) = i
       endif
    end do

    if (sfcmod_gfs) then
       do it=1,nt
          itt=indx(it)
          call compute_fact10(ges_u(1,1,1,itt),ges_v(1,1,1,itt),&
               ges_tsen(1,1,1,itt),ges_q(1,1,1,itt),&
               ges_ps(1,1,itt),ges_prsi(1,1,1,itt),&
               sfct(1,1,itt),sfc_rough(1,1,itt),&
               isli(1,1,itt),fact10(1,1,itt))
       end do
    endif

    if (sfcmod_mm5) then
       iqtflg=.true.
       do it=1,nt
          itt=indx(it)
          do j=1,lon2
             do i=1,lat2
                slimsk=isli(i,j,itt)
                call SFC_WTQ_FWD (&
                     ges_ps(i,j,itt),&
                     sfct(i,j,itt),&
                     ges_lnprsl(i,j,1,itt),&
                     ges_tv(i,j,1,itt),&
                     ges_q(i,j,1,itt),&
                     ges_u(i,j,1,itt),&
                     ges_v(i,j,1,itt),&
                     ges_lnprsl(i,j,2,itt),&
                     ges_tv(i,j,2,itt),&
                     ges_q(i,j,2,itt),&
                     geop_hgtl(i,j,1,itt),&
                     sfc_rough(i,j,itt),&
                     slimsk,&
                     fact10(i,j,itt),&
                     u10ges,v10ges,t2ges,q2ges,regime,iqtflg)
             end do
          end do
       end do
    endif

    return
  end subroutine load_fact10
    
end module guess_grids
