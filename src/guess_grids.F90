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
  use gridmod, only: wrf_nmm_regional,nems_nmmb_regional
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
!   2007-05-30  h.liu - remove ozmz
!   2007-06-21  rancic - add pbl (ges_teta)
!   2006-12-01  todling - remove bias stuff; merging GMAO bias correction scheme
!   2006-12-15  todling - add _initilized parameters to control allocations
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes 
!   2008-02-07  eliu    - fixed the unit difference between prsitmp
!                         (kPa) and toa_pressure (hPa).
!
! !AUTHOR: 
!   kleist           org: np20                date: 2003-12-01
!
!EOP
!-------------------------------------------------------------------------

  logical:: sfcmod_gfs = .false.    ! .true. = recompute 10m wind factor using gfs physics
  logical:: sfcmod_mm5 = .false.    ! .true. = recompute 10m wind factor using mm5 physics

  logical, save :: ges_initilized = .false.
  logical, save :: tnd_initilized = .false.
  logical, save :: drv_initilized = .false.

  integer(i_kind) ntguessig         ! location of actual guess time for sigma fields
  integer(i_kind) ntguessfc         ! location of actual guess time for sfc fields

  integer(i_kind):: ifact10 = 0     ! 0 = use 10m wind factor from guess

  ! number of guess sigma/surface times are set in GSI_gridComp.rc
  integer(i_kind):: nfldsig
  integer(i_kind):: nfldsfc
  real(r_kind), allocatable, dimension(:):: hrdifsig  ! times for
                                                      ! guess sigma fields
  real(r_kind), allocatable, dimension(:):: hrdifsfc  ! times for

  integer(i_kind),allocatable, dimension(:)::ifilesfc  ! array used to open the correct surface guess files
  integer(i_kind),allocatable, dimension(:)::ifilesig  ! array used to open the correct sigma guess files

  integer(i_kind),allocatable,dimension(:,:,:):: isli    ! snow/land/ice mask
  integer(i_kind),allocatable,dimension(:,:,:):: isli_g  ! isli on horiz/global grid
  integer(i_kind),allocatable,dimension(:,:):: isli2     ! snow/land/ice mask at analysis time

  real(r_kind),allocatable,dimension(:,:,:):: sno2  ! sno depth on subdomain

 
  real(r_single)::  tracers  ! number or tracers read in/written out
  real(r_single)::  vtid     ! tracer variable id from sigma header
  real(r_single)::  pdryini  ! global mean dry mass of the atmosphere in kPa
  real(r_single)::  xncld    ! number of clouds from sigma header


  real(r_kind):: ges_psfcavg                            ! average guess surface pressure 
  real(r_kind),allocatable,dimension(:):: ges_prslavg   ! average guess pressure profile

  real(r_kind),allocatable,dimension(:,:):: tropprs     ! guess tropopause pressure
  real(r_kind),allocatable,dimension(:,:,:):: dsfct     ! delta skin temperature

  real(r_kind),allocatable,dimension(:,:,:):: fact10    ! 10 meter wind factor
  real(r_kind),allocatable,dimension(:,:,:):: sfct      ! guess skin temperature
  real(r_kind),allocatable,dimension(:,:,:):: sno       ! snow-ice mask
  real(r_kind),allocatable,dimension(:,:,:):: veg_type  ! vegetation type
  real(r_kind),allocatable,dimension(:,:,:):: veg_frac  ! vegetation fraction(0-1.0)
  real(r_kind),allocatable,dimension(:,:,:):: sfc_rough ! sfc roughness length
  real(r_kind),allocatable,dimension(:,:,:):: soil_type ! soil type
  real(r_kind),allocatable,dimension(:,:,:):: soil_temp ! soil temperature of first layer
  real(r_kind),allocatable,dimension(:,:,:):: soil_moi  ! soil moisture of first layer
  

  real(r_kind),allocatable,dimension(:,:,:,:):: geop_hgtl ! guess geopotential height at mid-layers
  real(r_kind),allocatable,dimension(:,:,:,:):: geop_hgti ! guess geopotential height at level interfaces

  real(r_kind),allocatable,dimension(:,:,:):: ges_z      ! topography
  real(r_kind),allocatable,dimension(:,:,:):: ges_ps     ! log(surface pressure)
  real(r_kind),allocatable,dimension(:,:,:):: ges_ps_lat ! log(ps)/lat for pcp routine
  real(r_kind),allocatable,dimension(:,:,:):: ges_ps_lon ! log(ps)/lon for pcp routine

                                                         ! Guess Fields ...
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsi  ! interface pressure
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsl  ! layer midpoint pressure
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_lnprsl! log(layer midpoint pressure)
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_lnprsi! log(interface pressure)
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_u     ! zonal wind
  real(r_kind),allocatable,dimension(:,:,:):: ges_u_lat ! zonal wind/lat
  real(r_kind),allocatable,dimension(:,:,:):: ges_u_lon ! zonal wind/lon
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_v     ! meridional wind
  real(r_kind),allocatable,dimension(:,:,:):: ges_v_lat ! meridional wind/lat
  real(r_kind),allocatable,dimension(:,:,:):: ges_v_lon ! meridional wind/lon
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_vor   ! vorticity
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_div   ! divergence
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_cwmr  ! cloud condensate mixing ratio 
  real(r_kind),allocatable,dimension(:,:,:):: ges_cwmr_lat  ! cloud condensate mixing ratio/lat
  real(r_kind),allocatable,dimension(:,:,:):: ges_cwmr_lon  ! cloud condensate mixing ratio/lon
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_q     ! specific humidity
  real(r_kind),allocatable,dimension(:,:,:):: ges_qlon  ! q/lat for pcp routine advection calc
  real(r_kind),allocatable,dimension(:,:,:):: ges_qlat  ! q/lon for pcp routine advection calc
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_oz    ! ozone mixing ratio
  real(r_kind),allocatable,dimension(:,:,:):: ges_ozlat ! ozone mixing ratio/lat
  real(r_kind),allocatable,dimension(:,:,:):: ges_ozlon ! ozone mixing ratio/lon
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_pint  ! pint variable (nmm only)
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_tv    ! virtual temperature
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_tsen  ! sensible temperature
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_teta  ! potential temperature
  real(r_kind),allocatable,dimension(:,:,:):: ges_tvlat ! tv/lat for pcp routine advection calc
  real(r_kind),allocatable,dimension(:,:,:):: ges_tvlon ! tv/lon for pcp routine advection calc 

  real(r_kind),allocatable,dimension(:,:,:)::ges_pd        ! pdges (for nmm only)
  real(r_kind),allocatable,dimension(:,:,:):: ges_prs_ten  ! 3d pressure tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_u_ten    ! u tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_v_ten    ! v tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_tv_ten   ! Tv tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_q_ten    ! q tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_oz_ten   ! ozone tendency
  real(r_kind),allocatable,dimension(:,:,:):: ges_cwmr_ten ! cloud water tendency
  real(r_kind),allocatable,dimension(:,:,:):: fact_tv      ! 1./(one+fv*ges_q) for virt to sen calc.
  
  interface guess_grids_print
     module procedure print1r8_
     module procedure print2r8_
     module procedure print3r8_
     module procedure print4r8_
  end interface
  interface guess_grids_stats
     module procedure guess_grids_stats3d_
     module procedure guess_grids_stats2d_
  end interface

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
   use constants, only: zero,izero

   implicit none

! !DESCRIPTION: allocate memory for surface related grids
!
! !REVISION HISTORY:
!   2003-12-01  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-28  treadon - remove subroutine call list, pass variables via modules
!   2005-06-03  parrish - allocate and initialize sfct_lat and sfct_lon
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2008-12-5   todling - add time dimension to dsfct
!   2009-01-23  todling - zero out arrays
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

    integer(i_kind) :: i,j,it,istatus

    allocate( isli_g(nlat,nlon,nfldsfc),&
         isli2(lat2,lon2),sno2(lat2,lon2,nfldsfc),&
         stat=istatus)
    if (istatus/=0) write(6,*)'CREATE_SFC_GRIDS(1):  allocate error, istatus=',&
         istatus,lat2,lon2,nlat,nlon,nfldsfc

#ifndef HAVE_ESMF
    allocate( isli(lat2,lon2,nfldsfc),fact10(lat2,lon2,nfldsfc),&
         dsfct(lat2,lon2,nfldsfc),sfct(lat2,lon2,nfldsfc),sno(lat2,lon2,nfldsfc),&
         veg_type(lat2,lon2,nfldsfc),veg_frac(lat2,lon2,nfldsfc),&
         sfc_rough(lat2,lon2,nfldsfc),&
         soil_type(lat2,lon2,nfldsfc),soil_temp(lat2,lon2,nfldsfc),&
         soil_moi(lat2,lon2,nfldsfc), &
         stat=istatus)
    if (istatus/=0) write(6,*)'CREATE_SFC_GRIDS(2):  allocate error, istatus=',&
         istatus,lat2,lon2,nlat,nlon,nfldsfc
#endif /* HAVE_ESMF */

    do it=1,nfldsfc
       do j=1,nlon
          do i=1,nlat
             isli_g(i,j,it)=izero
          end do
       end do
    end do

#ifndef HAVE_ESMF
    do it=1,nfldsfc
       do j=1,lon2
          do i=1,lat2
             isli(i,j,it)=izero
             fact10(i,j,it)=zero
             sfct(i,j,it)=zero
             sno(i,j,it)=zero
             veg_type(i,j,it)=zero
             veg_frac(i,j,it)=zero
             soil_type(i,j,it)=zero
             soil_temp(i,j,it)=zero
             soil_moi(i,j,it)=zero
          end do
       end do
    end do
#endif

    do it=1,nfldsfc
       do j=1,lon2
          do i=1,lat2
             dsfct(i,j,it)=zero
             sno2(i,j,it)=zero
          end do
       end do
    end do

    do j=1,lon2
       do i=1,lat2
          isli2(i,j)=izero
       end do
    end do

    return
  end subroutine create_sfc_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_ges_grids --- Alloc grid for guess and bias
!
! !INTERFACE:
!
  subroutine create_ges_grids(switch_on_derivatives,tendsflag)

! !USES:

    use constants,only: zero,one
    use gridmod, only: lat2,lon2,nsig
    implicit none

! !INPUT PARAMETERS:

    logical:: switch_on_derivatives    ! for for horizontal derivatives
    logical:: tendsflag                ! for time tendencies


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
!   2006-06-08  zhang,b - change "biascor>0" to "biascor>=0" for debug purposes
!   2006-12-04  todling - remove bias initialization; rename routine
!   2006-12-15  todling - protection to allow initializing ges/tnd/drv at will
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
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

    if ( .not. ges_initilized ) then

!   Allocate and zero guess grids
    allocate ( ges_prsi(lat2,lon2,nsig+1,nfldsig),ges_prsl(lat2,lon2,nsig,nfldsig),&
         ges_lnprsl(lat2,lon2,nsig,nfldsig),ges_lnprsi(lat2,lon2,nsig+1,nfldsig),&
         ges_tsen(lat2,lon2,nsig,nfldsig),&
         ges_teta(lat2,lon2,nsig,nfldsig),&
         geop_hgtl(lat2,lon2,nsig,nfldsig), &
         geop_hgti(lat2,lon2,nsig+1,nfldsig),ges_prslavg(nsig),&
         tropprs(lat2,lon2),fact_tv(lat2,lon2,nsig),stat=istatus)
    if (istatus/=0) write(6,*)'CREATE_GES_GRIDS(1):  allocate error1, istatus=',&
         istatus,lat2,lon2,nsig,nfldsig
#ifndef HAVE_ESMF
    allocate (ges_z(lat2,lon2,nfldsig),ges_ps(lat2,lon2,nfldsig),&
         ges_u(lat2,lon2,nsig,nfldsig),ges_v(lat2,lon2,nsig,nfldsig),&
         ges_vor(lat2,lon2,nsig,nfldsig),ges_div(lat2,lon2,nsig,nfldsig),&
         ges_cwmr(lat2,lon2,nsig,nfldsig),ges_q(lat2,lon2,nsig,nfldsig),&
         ges_oz(lat2,lon2,nsig,nfldsig),ges_tv(lat2,lon2,nsig,nfldsig),&
         stat=istatus)
    if (istatus/=0) write(6,*)'CREATE_GES_GRIDS(2):  allocate error1, istatus=',&
         istatus,lat2,lon2,nsig,nfldsig
#endif /* HAVE_ESMF */
    if(update_pint) then
       allocate(ges_pint(lat2,lon2,nsig+1,nfldsig),ges_pd(lat2,lon2,nfldsig),&
            stat=istatus)
       if (istatus/=0) write(6,*)'CREATE_GES_GRIDS:  allocate error2, istatus=',&
         istatus,lat2,lon2,nsig,nfldsig
    endif

    ges_initilized = .true.

!  Default for ges_psfcavg
    ges_psfcavg=zero
    do i=1,nsig
       ges_prslavg(i)=zero
    end do

    do j=1,lon2
       do i=1,lat2
          tropprs(i,j)=zero
       end do
    end do

    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
           fact_tv(i,j,k)=one
        end do
      end do
    end do

#ifndef HAVE_ESMF
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
                ges_u(i,j,k,n)=zero
                ges_v(i,j,k,n)=zero
                ges_vor(i,j,k,n)=zero
                ges_div(i,j,k,n)=zero
                ges_cwmr(i,j,k,n)=zero
                ges_q(i,j,k,n)=zero
                ges_oz(i,j,k,n)=zero
                ges_tv(i,j,k,n)=zero
!                ges_pint(i,j,k,n)=zero
             end do
          end do
       end do
    end do
#endif /* HAVE_ESMF */
    do n=1,nfldsig
       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                ges_prsl(i,j,k,n)=zero
                ges_lnprsl(i,j,k,n)=zero
                ges_tsen(i,j,k,n)=zero
                ges_teta(i,j,k,n)=zero
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

    end if ! ges_initilized
    
!   If tendencies option on, allocate/initialize _ten arrays to zero
    if (.not.tnd_initilized .and. tendsflag) then
      allocate(ges_prs_ten(lat2,lon2,nsig+1),ges_u_ten(lat2,lon2,nsig),&
               ges_v_ten(lat2,lon2,nsig),ges_tv_ten(lat2,lon2,nsig),&
               ges_q_ten(lat2,lon2,nsig),ges_oz_ten(lat2,lon2,nsig),&
               ges_cwmr_ten(lat2,lon2,nsig),stat=istatus)
      if (istatus/=0) write(6,*)'CREATE_GES_GRIDS:  allocate error3, istatus=',&
           istatus,lat2,lon2,nsig
      tnd_initilized = .true.
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
    if (.not.drv_initilized .and. switch_on_derivatives) then
       allocate(ges_u_lat(lat2,lon2,nsig),ges_u_lon(lat2,lon2,nsig),&
            ges_v_lat(lat2,lon2,nsig),ges_v_lon(lat2,lon2,nsig),&
            ges_cwmr_lat(lat2,lon2,nsig),ges_cwmr_lon(lat2,lon2,nsig),&
            ges_ozlat(lat2,lon2,nsig),ges_ozlon(lat2,lon2,nsig),&
            ges_ps_lat(lat2,lon2,nfldsig),ges_ps_lon(lat2,lon2,nfldsig),&
            ges_tvlat(lat2,lon2,nsig),ges_tvlon(lat2,lon2,nsig),&
            ges_qlat(lat2,lon2,nsig),ges_qlon(lat2,lon2,nsig),&
            stat=istatus)
       if (istatus/=0) write(6,*)'CREATE_GES_GRIDS:  allocate error4, istatus=',&
            istatus,lat2,lon2,nsig,nfldsig
       drv_initilized = .true.
       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                ges_u_lat(i,j,k)=zero
                ges_u_lon(i,j,k)=zero
                ges_v_lat(i,j,k)=zero
                ges_v_lon(i,j,k)=zero
                ges_cwmr_lat(i,j,k)=zero
                ges_cwmr_lon(i,j,k)=zero
                ges_ozlat(i,j,k)=zero
                ges_ozlon(i,j,k)=zero
                ges_tvlat(i,j,k)=zero
                ges_tvlon(i,j,k)=zero
                ges_qlat(i,j,k)=zero
                ges_qlon(i,j,k)=zero
             end do
          end do
       end do
       do n=1,nfldsig
          do j=1,lon2
             do i=1,lat2
                ges_ps_lat(i,j,n)=zero
                ges_ps_lon(i,j,n)=zero
             end do
          end do
       end do
    endif  ! end if switch_derivatives block

    return
  end subroutine create_ges_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_ges_grids --- Dealloc guess and bias fields
!
! !INTERFACE:
!
  subroutine destroy_ges_grids(switch_on_derivatives,tendsflag)

! !USES:

    use constants,only:zero
    implicit none

! !INPUT PARAMETERS:
    logical:: switch_on_derivatives    ! flag for horizontal derivatives
    logical:: tendsflag                ! flag for tendency
    
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
!   2006-12-04  todling - remove bias destroy; rename routine
!   2006-12-15  todling - using internal switches to deallc(tnds/drvs)
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
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

    deallocate(ges_prsi,ges_prsl,ges_lnprsl,ges_lnprsi,&
         ges_tsen,ges_teta,geop_hgtl,geop_hgti,ges_prslavg,&
         tropprs,fact_tv,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_GES_GRIDS(1):  deallocate error1, istatus=',&
         istatus
#ifndef HAVE_ESMF
    deallocate(ges_z,ges_ps,&
         ges_u,ges_v,ges_vor,ges_div,ges_cwmr,ges_q,&
         ges_oz,ges_tv,&
         stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_GES_GRIDS(2):  deallocate error1, istatus=',&
         istatus
#endif /* HAVE_ESMF */
    if(update_pint) then
       deallocate(ges_pint,ges_pd,stat=istatus)
       if (istatus/=0) &
            write(6,*)'DESTROY_GES_GRIDS:  deallocate error2, istatus=',&
            istatus
    endif
    if (drv_initilized .and.switch_on_derivatives) then
       deallocate(ges_u_lat,ges_u_lon,ges_v_lat,ges_v_lon,ges_cwmr_lat,&
            ges_cwmr_lon,ges_ozlat,ges_ozlon,&
            ges_ps_lat,ges_ps_lon,ges_tvlat,ges_tvlon,&
            ges_qlat,ges_qlon,stat=istatus)
       if (istatus/=0) &
            write(6,*)'DESTROY_GES_GRIDS:  deallocate error3, istatus=',&
            istatus
    endif
    if (tnd_initilized .and. tendsflag) then
       deallocate(ges_u_ten,ges_v_ten,ges_tv_ten,ges_prs_ten,ges_q_ten,&
            ges_oz_ten,ges_cwmr_ten,stat=istatus)
       if (istatus/=0) &
            write(6,*)'DESTROY_GES_GRIDS:  deallocate error4, istatus=',&
            istatus
    endif
    return
  end subroutine destroy_ges_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_sfct --- Deallocate sfct only
!
! !INTERFACE:
!
  subroutine destroy_sfct

! !USES:

   implicit none

! !DESCRIPTION: deallocate surface temperature field
!
! !REVISION HISTORY:
!   2008-06-30  derber
!   2008-09-05  lueken - add subprogram doc block
!   2009-01-02  todling - replaced doc block with protex-prologue
!   2009-01-17  todling - dealloc isli2,sno2 was misplaced
!
! !REMARKS:
!   language: f90
!   machine:ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   derber          org: w/nmc2     date: 2008-06-30
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind):: istatus

    deallocate(isli2,sno2,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_SFCT:  deallocate error, istatus=',&
         istatus
#ifndef HAVE_ESMF
    deallocate(sfct,dsfct,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_SFCT:  deallocate error, istatus=',&
         istatus
#endif /* HAVE_ESMF */

    return
  end subroutine destroy_sfct

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
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2008-06-30  derber - remove sfct deallocate to allow earlier call
!   2009-01-17  todling - move isli2,sno2 into destroy_sfct
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

    deallocate(isli_g,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_SFC_GRIDS:  deallocate error, istatus=',&
         istatus
#ifndef HAVE_ESMF
    deallocate(isli,fact10,sno,veg_type,veg_frac,soil_type,&
         sfc_rough,soil_temp,soil_moi,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_SFC_GRIDS:  deallocate error, istatus=',&
         istatus
#endif /* HAVE_ESMF */

    return
  end subroutine destroy_sfc_grids


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: create_gesfinfo --- Allocate guess-files information arrays
!
! !INTERFACE:
!
  subroutine create_gesfinfo

! !USES:

   implicit none

! !DESCRIPTION: allocate guess-files information arrays
!
! !REVISION HISTORY:
!   2009-01-08  todling
!
! !REMARKS:
!   language: f90
!   machine:ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   todling          org: w/nmc2     date: 2009-01-08
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind):: istatus

#ifndef HAVE_ESMF
  allocate(hrdifsfc(nfldsfc),ifilesfc(nfldsfc), &
           hrdifsig(nfldsig),ifilesig(nfldsig),stat=istatus)
    if (istatus/=0) &
         write(6,*)'CREATE_GESFINFO:  allocate error, istatus=',&
         istatus
#endif /* HAVE_ESMF */

    return
  end subroutine create_gesfinfo

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_gesfinfo --- Deallocate guess-files information
!
! !INTERFACE:
!
  subroutine destroy_gesfinfo

! !USES:

   implicit none

! !DESCRIPTION: deallocate guess-files information
!
! !REVISION HISTORY:
!   2009-01-08  todling
!
! !REMARKS:
!   language: f90
!   machine:ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   todling          org: w/nmc2     date: 2009-01-08
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind):: istatus

#ifndef HAVE_ESMF
    deallocate(hrdifsfc,ifilesfc,hrdifsig,ifilesig,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_GESFINFO:  deallocate error, istatus=',&
         istatus
#endif /* HAVE_ESMF */

    return
  end subroutine destroy_gesfinfo


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

    use constants,only: zero,one,rd_over_cp,one_tenth,half
    use gridmod, only: lat2,lon2,nsig,ak5,bk5,ck5,tref5,idvc5,&
         regional,wrf_nmm_regional,nems_nmmb_regional,wrf_mass_regional,pt_ll,aeta2_ll,&
         aeta1_ll,eta2_ll,pdtop_ll,eta1_ll,twodvar_regional,idsl5
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
!   2007-05-08  kleist  - add fully generalized coordinate for pressure calculation
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
    real(r_kind) kap1,kapr,trk
    integer(i_kind) i,j,k,jj

    kap1=rd_over_cp+one
    kapr=one/rd_over_cp

    do jj=1,nfldsig
      do k=1,nsig+1
        do j=1,lon2
          do i=1,lat2
            if(regional) then
              if (wrf_nmm_regional.or.nems_nmmb_regional) &
                ges_prsi(i,j,k,jj)=one_tenth* &
                            (eta1_ll(k)*pdtop_ll + &
                             eta2_ll(k)*(ten*ges_ps(i,j,jj)-pdtop_ll-pt_ll) + &
                            pt_ll)
              if (wrf_mass_regional .or. twodvar_regional) &
                ges_prsi(i,j,k,jj)=one_tenth*(eta1_ll(k)*(ten*ges_ps(i,j,jj)-pt_ll) + pt_ll)
            else
              if (idvc5.eq.1 .or. idvc5.eq.2) then
                ges_prsi(i,j,k,jj)=ak5(k)+(bk5(k)*ges_ps(i,j,jj))
              else if (idvc5.eq.3) then
                if (k.eq.1) then
                   ges_prsi(i,j,k,jj)=ges_ps(i,j,jj)
                else if (k.eq.nsig+1) then
                   ges_prsi(i,j,k,jj)=zero
                else
                  trk=(half*(ges_tv(i,j,k-1,jj)+ges_tv(i,j,k,jj))/tref5(k))**kapr
                  ges_prsi(i,j,k,jj)=ak5(k)+(bk5(k)*ges_ps(i,j,jj))+(ck5(k)*trk)
                end if
              end if
            endif
            ges_lnprsi(i,j,k,jj)=log(ges_prsi(i,j,k,jj))
          end do
        end do
      end do
    end do

    if(regional) then
      if (wrf_nmm_regional.or.nems_nmmb_regional) then
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

!      load mid-layer pressure by using phillips vertical interpolation
       if (idsl5/=2) then
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

!      load mid-layer pressure by simple averaging
       else
          do jj=1,nfldsig
             do j=1,lon2
                do i=1,lat2
                   do k=1,nsig
                      ges_prsl(i,j,k,jj)=(ges_prsi(i,j,k,jj)+ges_prsi(i,j,k+1,jj))*half
                      ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))
                   end do
                end do
             end do
          end do
       endif

    end if  !  end regional/global block

! For regional applications only, load variables containing mean
! surface pressure and pressure profile at the layer midpoints
    if (regional) then
       ges_psfcavg = r1013
       if (wrf_nmm_regional.or.nems_nmmb_regional) then
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

    integer(i_kind) i,j,k,jj
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

    real(r_kind),parameter:: ten = 10.0_r_kind

!   Declare local variables
    integer(i_kind) k,kk,l
    real(r_kind) dprs

!   Check if model top pressure above rtm top pressure, where prsitmp
!   is in kPa and toa_pressure is in hPa.
    if (ten*prsitmp(nsig) < toa_pressure)then
       write(6,*)'ADD_RTM_LAYERS:  model top pressure(hPa)=', &
            ten*prsitmp(nsig),&
            ' above rtm top pressure(hPa)=',toa_pressure
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
    real(r_kind):: u10ges,v10ges,t2ges,q2ges,regime

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
          do j=1,lon2
             do i=1,lat2
                call compute_fact10(ges_u(i,j,1,itt),ges_v(i,j,1,itt),&
                     ges_tsen(i,j,1,itt),ges_q(i,j,1,itt),&
                     ges_ps(i,j,itt),ges_prsi(i,j,1,itt), &
                     ges_prsi(i,j,2,itt),sfct(i,j,itt), &
                     sfc_rough(i,j,itt),isli(i,j,itt),fact10(i,j,itt))
             end do
          end do
       end do
    endif

    if (sfcmod_mm5) then
       iqtflg=.true.
       do it=1,nt
          itt=indx(it)
          do j=1,lon2
             do i=1,lat2
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
                     isli(i,j,itt),&
                     fact10(i,j,itt),&
                     u10ges,v10ges,t2ges,q2ges,regime,iqtflg)
             end do
          end do
       end do
    endif

    return
  end subroutine load_fact10

!
  subroutine comp_fact10(dlat,dlon,dtime,skint,sfcrough,islimsk,mype,factw)

! !USES:

    use gridmod, only: iglobal,nlat,nlon,ijn,displs_g,itotsub,&
         lat1,lon1,lat2,lon2,nsig,istart,jstart
    use constants, only: zero,one
    implicit none

! !INPUT PARAMETERS:

! !DESCRIPTION: compute 10m wind factor
!
! !REVISION HISTORY:
!   2006-09-26  treadon
!   2008-12-05  todling - use dsfct(:,:,ntguessfc) for calculation
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

!   Declare local variables
    logical iqtflg
    real(r_kind),intent(in):: dlat,dlon,dtime,skint,sfcrough
    real(r_kind),intent(inout):: factw
    integer(i_kind),intent(in)::mype,islimsk
    integer(i_kind) ix,ix1,ixp,iy,iy1,iyp
    integer(i_kind) itsig,itsigp,j,m1,islimsk2
    real(r_kind) w00,w01,w10,w11
    real(r_kind) delx,dely,delx1,dely1,dtsig,dtsigp
    real(r_kind):: u10ges,v10ges,t2ges,q2ges,regime
    real(r_kind):: pgesin,ugesin,vgesin,qgesin,tgesin,prsigesin1
    real(r_kind):: prsigesin2,lnpgesin1,lnpgesin2,tgesin2,qgesin2,geopgesin,ts

    islimsk2=islimsk
    if(islimsk2 > 2)islimsk2=islimsk2-3
    m1=mype+1
!   Set spatial interpolation indices and weights
    ix1=dlat
    ix1=max(1,min(ix1,nlat))
    delx=dlat-ix1
    delx=max(zero,min(delx,one))
    ix=ix1-istart(m1)+2
    ixp=ix+1
    if(ix1==nlat) then
       ixp=ix
    end if
    delx1=one-delx

    iy1=dlon
    dely=dlon-iy1
    iy=iy1-jstart(m1)+2
    if(iy<1) then
       iy1=iy1+nlon
       iy=iy1-jstart(m1)+2
    end if
    if(iy>lon1+1) then
       iy1=iy1-nlon
       iy=iy1-jstart(m1)+2
    end if
    iyp=iy+1
    dely1=one-dely


    w00=delx1*dely1; w10=delx*dely1; w01=delx1*dely; w11=delx*dely
!   Get time interpolation factors for sigma files
    if(dtime > hrdifsig(1) .and. dtime < hrdifsig(nfldsig))then
      do j=1,nfldsig-1
        if(dtime > hrdifsig(j) .and. dtime <= hrdifsig(j+1))then
           itsig=j
           itsigp=j+1
           dtsig=((hrdifsig(j+1)-dtime)/(hrdifsig(j+1)-hrdifsig(j)))
        end if
      end do
    else if(dtime <=hrdifsig(1))then
      itsig=1
      itsigp=1
      dtsig=one
    else
      itsig=nfldsig
      itsigp=nfldsig
      dtsig=one
    end if
    dtsigp=one-dtsig

    ts =(dsfct(ix,iy ,ntguessfc)*w00 + dsfct(ixp,iy ,ntguessfc)*w10 +          &
         dsfct(ix,iyp,ntguessfc)*w01 + dsfct(ixp,iyp,ntguessfc)*w11) + skint

    pgesin=(ges_ps(ix,iy ,itsig )*w00+ges_ps(ixp,iy ,itsig )*w10+ &
            ges_ps(ix,iyp,itsig )*w01+ges_ps(ixp,iyp,itsig )*w11)*dtsig + &
           (ges_ps(ix,iy ,itsigp)*w00+ges_ps(ixp,iy ,itsigp)*w10+ &
            ges_ps(ix,iyp,itsigp)*w01+ges_ps(ixp,iyp,itsigp)*w11)*dtsigp
    ugesin=(ges_u(ix,iy ,1,itsig )*w00+ges_u(ixp,iy ,1,itsig )*w10+ &
            ges_u(ix,iyp,1,itsig )*w01+ges_u(ixp,iyp,1,itsig )*w11)*dtsig + &
           (ges_u(ix,iy ,1,itsigp)*w00+ges_u(ixp,iy ,1,itsigp)*w10+ &
            ges_u(ix,iyp,1,itsigp)*w01+ges_u(ixp,iyp,1,itsigp)*w11)*dtsigp
    vgesin=(ges_v(ix,iy ,1,itsig )*w00+ges_v(ixp,iy ,1,itsig )*w10+ &
            ges_v(ix,iyp,1,itsig )*w01+ges_v(ixp,iyp,1,itsig )*w11)*dtsig + &
           (ges_v(ix,iy ,1,itsigp)*w00+ges_v(ixp,iy ,1,itsigp)*w10+ &
            ges_v(ix,iyp,1,itsigp)*w01+ges_v(ixp,iyp,1,itsigp)*w11)*dtsigp
    qgesin=(ges_q(ix,iy ,1,itsig )*w00+ges_q(ixp,iy ,1,itsig )*w10+ &
            ges_q(ix,iyp,1,itsig )*w01+ges_q(ixp,iyp,1,itsig )*w11)*dtsig + &
           (ges_q(ix,iy ,1,itsigp)*w00+ges_q(ixp,iy ,1,itsigp)*w10+ &
            ges_q(ix,iyp,1,itsigp)*w01+ges_q(ixp,iyp,1,itsigp)*w11)*dtsigp


    if (sfcmod_gfs) then
       tgesin    =(ges_tsen(ix ,iy ,1,itsig )*w00+ &
                   ges_tsen(ixp,iy ,1,itsig )*w10+ &
                   ges_tsen(ix ,iyp,1,itsig )*w01+ &
                   ges_tsen(ixp,iyp,1,itsig )*w11)*dtsig + &
                  (ges_tsen(ix ,iy ,1,itsigp)*w00+ &
                   ges_tsen(ixp,iy ,1,itsigp)*w10+ &
                   ges_tsen(ix ,iyp,1,itsigp)*w01+ &
                   ges_tsen(ixp,iyp,1,itsigp)*w11)*dtsigp
       prsigesin1=(ges_prsi(ix ,iy ,1,itsig )*w00+ &
                   ges_prsi(ixp,iy ,1,itsig )*w10+ &
                   ges_prsi(ix ,iyp,1,itsig )*w01+ &
                   ges_prsi(ixp,iyp,1,itsig )*w11)*dtsig + &
                  (ges_prsi(ix ,iy ,1,itsigp)*w00+ &
                   ges_prsi(ixp,iy ,1,itsigp)*w10+ &
                   ges_prsi(ix ,iyp,1,itsigp)*w01+ &
                   ges_prsi(ixp,iyp,1,itsigp)*w11)*dtsigp
       prsigesin2=(ges_prsi(ix ,iy ,2,itsig )*w00+ &
                   ges_prsi(ixp,iy ,2,itsig )*w10+ &
                   ges_prsi(ix ,iyp,2,itsig )*w01+ &
                   ges_prsi(ixp,iyp,2,itsig )*w11)*dtsig + &
                  (ges_prsi(ix ,iy ,2,itsigp)*w00+ &
                   ges_prsi(ixp,iy ,2,itsigp)*w10+ &
                   ges_prsi(ix ,iyp,2,itsigp)*w01+ &
                   ges_prsi(ixp,iyp,2,itsigp)*w11)*dtsigp
       call compute_fact10(ugesin,vgesin,tgesin,qgesin,pgesin, &
            prsigesin1,prsigesin2,ts,sfcrough,islimsk,factw)
    else if (sfcmod_mm5)then
       iqtflg=.true.
       lnpgesin1 =(ges_lnprsl(ix ,iy ,1,itsig )*w00+ &
                   ges_lnprsl(ixp,iy ,1,itsig )*w10+ &
                   ges_lnprsl(ix ,iyp,1,itsig )*w01+ &
                   ges_lnprsl(ixp,iyp,1,itsig )*w11)*dtsig + &
                  (ges_lnprsl(ix ,iy ,1,itsigp)*w00+ &
                   ges_lnprsl(ixp,iy ,1,itsigp)*w10+ &
                   ges_lnprsl(ix ,iyp,1,itsigp)*w01+ &
                   ges_lnprsl(ixp,iyp,1,itsigp)*w11)*dtsigp
       lnpgesin2 =(ges_lnprsl(ix ,iy ,2,itsig )*w00+ &
                   ges_lnprsl(ixp,iy ,2,itsig )*w10+ &
                   ges_lnprsl(ix ,iyp,2,itsig )*w01+ &
                   ges_lnprsl(ixp,iyp,2,itsig )*w11)*dtsig + &
                  (ges_lnprsl(ix ,iy ,2,itsigp)*w00+ &
                   ges_lnprsl(ixp,iy ,2,itsigp)*w10+ &
                   ges_lnprsl(ix ,iyp,2,itsigp)*w01+ &
                   ges_lnprsl(ixp,iyp,2,itsigp)*w11)*dtsigp
       tgesin    =(ges_tv(ix ,iy ,1,itsig )*w00+ &
                   ges_tv(ixp,iy ,1,itsig )*w10+ &
                   ges_tv(ix ,iyp,1,itsig )*w01+ &
                   ges_tv(ixp,iyp,1,itsig )*w11)*dtsig + &
                  (ges_tv(ix ,iy ,1,itsigp)*w00+ &
                   ges_tv(ixp,iy ,1,itsigp)*w10+ &
                   ges_tv(ix ,iyp,1,itsigp)*w01+ &
                   ges_tv(ixp,iyp,1,itsigp)*w11)*dtsigp
       tgesin2   =(ges_tv(ix ,iy ,2,itsig )*w00+ &
                   ges_tv(ixp,iy ,2,itsig )*w10+ &
                   ges_tv(ix ,iyp,2,itsig )*w01+ &
                   ges_tv(ixp,iyp,2,itsig )*w11)*dtsig + &
                  (ges_tv(ix ,iy ,2,itsigp)*w00+ &
                   ges_tv(ixp,iy ,2,itsigp)*w10+ &
                   ges_tv(ix ,iyp,2,itsigp)*w01+ &
                   ges_tv(ixp,iyp,2,itsigp)*w11)*dtsigp
       qgesin2   =(ges_q(ix ,iy ,2,itsig )*w00+ &
                   ges_q(ixp,iy ,2,itsig )*w10+ &
                   ges_q(ix ,iyp,2,itsig )*w01+ &
                   ges_q(ixp,iyp,2,itsig )*w11)*dtsig + &
                  (ges_q(ix ,iy ,2,itsigp)*w00+ &
                   ges_q(ixp,iy ,2,itsigp)*w10+ &
                   ges_q(ix ,iyp,2,itsigp)*w01+ &
                   ges_q(ixp,iyp,2,itsigp)*w11)*dtsigp
       geopgesin =(geop_hgtl(ix ,iy ,1,itsig )*w00+ &
                   geop_hgtl(ixp,iy ,1,itsig )*w10+ &
                   geop_hgtl(ix ,iyp,1,itsig )*w01+ &
                   geop_hgtl(ixp,iyp,1,itsig )*w11)*dtsig + &
                  (geop_hgtl(ix ,iy ,1,itsigp)*w00+ &
                   geop_hgtl(ixp,iy ,1,itsigp)*w10+ &
                   geop_hgtl(ix ,iyp,1,itsigp)*w01+ &
                   geop_hgtl(ixp,iyp,1,itsigp)*w11)*dtsigp
       call SFC_WTQ_FWD (pgesin,ts,lnpgesin1,tgesin,qgesin,ugesin,vgesin, &
                lnpgesin2,tgesin2,qgesin2,geopgesin,sfcrough,islimsk, &
                factw,u10ges,v10ges,t2ges,q2ges,regime,iqtflg)
    endif

    return
  end subroutine comp_fact10


!-------------------------------------------------------------------------
   subroutine guess_grids_stats3d_(name,a,mype)
!-------------------------------------------------------------------------

   use constants, only: zero, one
   use mpimod, only: ierror,mpi_rtype,mpi_sum,mpi_comm_world
   use gridmod, only: lon1,lat1,nsig

   implicit none

   character(len=*), intent(in) :: name
   real(r_kind), intent(in), dimension(:,:,:) :: a
   integer, intent(in)                      :: mype


! local variables
   integer :: i,j,k
   real(r_kind),dimension(nsig+1):: work_a,work_a1
   real(r_kind),dimension(nsig):: amz ! global mean profile of a
   real(r_kind) :: rms

! start

!  Calculate global means for a

!  Calculate sums for a to estimate variance.
   work_a = zero
   do k = 1,nsig
     do j = 2,lon1+1
        do i = 2,lat1+1
             work_a(k) = work_a(k) + a(i,j,k)
        end do
     end do
   end do
   work_a(nsig+1)=float(lon1*lat1)

   call mpi_allreduce(work_a,work_a1,nsig+1,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)

   amz=zero
   do k=1,nsig
      if (work_a1(nsig+1)>zero) amz(k)=work_a1(k)/work_a1(nsig+1)
      rms=sqrt(amz(k)**2/work_a1(nsig+1))      
      if (mype==0) write(*,100) trim(name),k,amz(k),rms
   enddo
100 format(a,': Level, Global mean, RMS = ',i3,1P2E16.8)

   end subroutine guess_grids_stats3d_

!-------------------------------------------------------------------------
   subroutine guess_grids_stats2d_(name,a,mype)
!-------------------------------------------------------------------------

   use constants, only: zero, one
   use mpimod, only: ierror,mpi_rtype,mpi_sum,mpi_comm_world
   use gridmod, only: lon1,lat1

   implicit none

   character(len=*), intent(in) :: name
   real(r_kind), intent(in), dimension(:,:) :: a
   integer, intent(in)                      :: mype


! local variables
   integer :: i,j
   real(r_kind),dimension(2):: work_a,work_a1
   real(r_kind) :: amz, rms

! start

!  Calculate global means for a

!  Calculate sums for a to estimate variance.
   work_a = zero
   do j = 2,lon1+1
      do i = 2,lat1+1
         work_a(1) = work_a(1) + a(i,j)
      end do
   end do
   work_a(2)=float(lon1*lat1)

   call mpi_allreduce(work_a,work_a1,2,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)

   amz=zero
   if (work_a1(2)>zero) amz=work_a1(1)/work_a1(2)
   rms=sqrt(amz**2/work_a1(2))      
   if (mype==0) write(*,100) trim(name),amz,rms
100 format(a,': Global mean, RMS = ',1P2E16.8)

   end subroutine guess_grids_stats2d_

!-------------------------------------------------------------------------
   subroutine pstats_(a,amiss,avg,rms)
!-------------------------------------------------------------------------

   use constants, only: zero
   implicit none

   real(r_kind), intent(in), dimension(:,:) :: a      ! array var
   real, intent(in)                         :: amiss  ! undef
   real, intent(out)                        :: avg,rms


! local variables
   integer :: i,j
   integer :: allcnt,cnt

! start

   allcnt=0
   cnt=0
   avg=zero
   rms=zero
   do i=1,size(a,1)
      do j=1,size(a,2)
         if(a(i,j).ne.amiss) then
            cnt=cnt+1
            avg=avg+a(i,j)
         endif
         allcnt = allcnt+1
      end do
   end do
   avg=avg/max(1,cnt)
   rms=sqrt(avg*avg/max(1,cnt))      

   end subroutine pstats_

!-------------------------------------------------------------------------
   subroutine print1r8_(name,fld,undef)
!-------------------------------------------------------------------------
   implicit none
   character(len=*), intent(in) :: name
   real(r_kind),intent(in),dimension(:) :: fld
   real, intent(in)                :: undef
! 
   write(6,100) trim(name),minval(fld),maxval(fld),sum(fld),undef
100 format(a,': range,sum = ',1P3E16.4)
   end subroutine print1r8_

!-------------------------------------------------------------------------
   subroutine print2r8_(name,fld,undef)
!-------------------------------------------------------------------------
   implicit none
   character(len=*), intent(in) :: name
   real(r_kind),intent(in),dimension(:,:) :: fld
   real, intent(in)                :: undef
! 
   real avg,rms
   write(6,100) trim(name),minval(fld),maxval(fld),sum(fld)
   call pstats_(fld,UNDEF,avg,rms)
   write(6,99) trim(name),avg,rms
100 format(a,': range,sum = ',1P3E16.4)
99  format(a,': avg, rms = ',1P2E16.4)
   end subroutine print2r8_

!-------------------------------------------------------------------------
   subroutine print3r8_(name,fld,undef,allk)
!-------------------------------------------------------------------------
   implicit none
   character(len=*), intent(in) :: name
   real(r_kind),intent(in),dimension(:,:,:) :: fld
   real, intent(in)                :: undef
   logical, intent(in), optional :: allk
! 
   logical prntlevs
   integer k
   real avg,rms
   if(present(allk)) prntlevs=allk
   if(prntlevs) then
      do k=1,size(fld,3)
         write(6,101) trim(name),k,minval(fld(:,:,k)),maxval(fld(:,:,k)),sum(fld(:,:,k))
         call pstats_(fld(:,:,k),UNDEF,avg,rms)
         write(6,99) trim(name),avg,rms
      end do
   else
      write(6,100) trim(name),minval(fld),maxval(fld),sum(fld)
   end if
101 format(a,': time or lev,range,sum = ',i3,1P3E16.4)
100 format(a,': range,sum = ',1P3E16.4)
99  format(a,': avg, rms = ',1P2E16.4)
   end subroutine print3r8_

!-------------------------------------------------------------------------
   subroutine print4r8_(name,fld,undef,allk)
!-------------------------------------------------------------------------
   implicit none
   character(len=*), intent(in) :: name
   real(r_kind),intent(in),dimension(:,:,:,:) :: fld
   real, intent(in)                :: undef
   logical, intent(in), optional :: allk
! 
   logical prntlevs
   integer k,it
   real avg,rms
   if(present(allk)) prntlevs=allk
   if(prntlevs) then
      do it=1,size(fld,4)
         do k=1,size(fld,3)
            write(6,101) trim(name),it,k,minval(fld(:,:,k,it)),maxval(fld(:,:,k,it)),sum(fld(:,:,k,it))
            call pstats_(fld(:,:,k,it),UNDEF,avg,rms)
            write(6,99) trim(name),avg,rms
         end do
      end do
   else
      write(6,100) trim(name),minval(fld),maxval(fld),sum(fld)
   end if
101 format(a,': time,lev,range,sum = ',i3,i3,1P3E16.4)
100 format(a,': range,sum = ',1P3E16.4)
99  format(a,': avg, rms = ',1P2E16.4)
   end subroutine print4r8_
    
end module guess_grids
