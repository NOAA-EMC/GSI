!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  gridmod --- GSI grid related variable declarations
!
! !INTERFACE:
!
module gridmod

! !USES:

  use kinds, only: i_byte,r_kind,r_single,i_kind
  implicit none

! !DESCRIPTION: module containing grid related variable declarations
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2003-xx-xx  parrish,wu  regional components added
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-12-23  treadon - add routines get_ij and get_ijk
!   2005-01-20  okamoto - add nsig5p1
!   2005-03-04  derber  - add nsig3p3,nsig3p2
!   2005-03-07  dee     - add gmao_intfc option for gmao interface
!   2005-05-24  pondeca - regional surface component added
!   2005-06-01  treadon - add variables msig and array nlayers
!   2005-09-28  derber  - put grid calculations into get_ij and get_ijk
!   2006-01-09  derber  - add sigsum
!   2006-02-01  parrish - correct error to dx_an, dy_an when using filled_grid
!   2006-04-14  treadon - remove global sigi,sigl; add ntracter,ncloud,ck5
!   2006-04-17  treadon - remove regional sigi_ll,sigl_ll
!   2006-10-17  kleist  - add lat dependent coriolis parameter
!   2007-05-07  treadon - add ncep_sigio, ncepgfs_head(v)
!   2007-05-08  kleist  - add variables for fully generalized vertical coordinate
!   2007-10-24  parrish - fix error in wind rotation reference angle field
!   2009-01-28  todling - remove original GMAO interface
!   2009-01-09  gayno   - added variables lpl_gfs and dx_gfs
!   2010-03-06  parrish - add logical flag use_gfs_ozone for option to read gfs ozone for regional run
!   2010-03-09  parrish - add logical flag check_gfs_ozone_date--if true, date check against analysis time
!   2010-03-10  lueken  - remove hires_b variables, section, and subroutines
!   2010-03-15  parrish - add logical flag regional_ozone to turn on ozone in regional analysis
!
! !AUTHOR: 
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------

! set default to private
  private
! set subroutines to public
  public :: init_grid
  public :: init_grid_vars
  public :: init_subdomain_vars
  public :: create_grid_vars
  public :: destroy_grid_vars
  public :: create_mapping
  public :: destroy_mapping
  public :: init_reg_glob_ll
  public :: init_general_transform
  public :: tll2xy
  public :: txy2ll
  public :: nearest_3
  public :: get_xytilde_domain
  public :: half_nmm_grid2a
  public :: fill_nmm_grid2a3
  public :: rotate_wind_ll2xy
  public :: rotate_wind_xy2ll
  public :: load_grid
  public :: fill_ns
  public :: filluv_ns
  public :: get_ij
  public :: get_ijk
  public :: check_rotate_wind
! set passed variables to public
  public :: nnnn1o,iglobal,itotsub,ijn,ijn_s,lat2,lon2,lat1,lon1,nsig
  public :: ncloud,nlat,nlon,ntracer,displs_s,displs_g,ltosj_s,ltosi_s
  public :: ltosj,ltosi,bk5,regional,latlon11,latlon1n,twodvar_regional
  public :: netcdf,nems_nmmb_regional,wrf_mass_regional,wrf_nmm_regional
  public :: aeta2_ll,pdtop_ll,pt_ll,eta1_ll,eta2_ll,aeta1_ll,idsl5,ck5,ak5
  public :: tref5,idvc5,nlayers,msig,jstart,istart,region_lat,nsig1o,rlats
  public :: region_dy,region_dx,region_lon,rlat_min_dd,rlat_max_dd,rlon_max_dd
  public :: rlon_min_dd,coslon,sinlon,rlons,ird_s,irc_s,periodic,idthrm5
  public :: cp5,idvm5,ncep_sigio,ncepgfs_head,idpsfc5,nlon_sfc,nlat_sfc
  public :: rlons_sfc,rlats_sfc,jlon1,ilat1,periodic_s,latlon1n1,nsig3p2
  public :: nsig3p3,nsig2,nsig3p1,wgtlats,corlats,rbs2,ncepgfs_headv,regional_time
  public :: regional_fhr,region_dyi,coeffx,region_dxi,coeffy,nsig_hlf
  public :: nlat_regional,nlon_regional,update_regsfc,half_grid,gencode
  public :: diagnostic_reg,nmmb_reference_grid,hybrid,filled_grid
  public :: grid_ratio_nmmb,isd_g,isc_g,dx_gfs,lpl_gfs,nsig5,nmmb_verttype
  public :: nsig4,nsig3
  public :: use_gfs_ozone,check_gfs_ozone_date,regional_ozone

  logical regional          ! .t. for regional background/analysis
  logical diagnostic_reg    ! .t. to activate regional analysis diagnostics

  logical ncep_sigio        ! .t. if using ncep sigio format file

  logical wrf_nmm_regional  !
  logical nems_nmmb_regional! .t. to run with NEMS NMMB model
  logical wrf_mass_regional !
  logical twodvar_regional  ! .t. to run code in regional 2D-var mode
  logical use_gfs_ozone     ! .t. to use gfs ozone in regional analysis
  logical check_gfs_ozone_date ! .t. to date check gfs ozone against regional
  logical regional_ozone    !    .t. to turn on ozone for regional analysis
  logical netcdf            ! .t. for regional netcdf i/o

  logical hybrid            ! .t. to set hybrid vertical coordinates
  logical filled_grid       ! 
  logical half_grid         !
  logical update_regsfc     !

  character(1) nmmb_reference_grid      ! ='H': use nmmb H grid as reference for analysis grid
                                        ! ='V': use nmmb V grid as reference for analysis grid
  real(r_kind) grid_ratio_nmmb ! ratio of analysis grid to nmmb model grid in nmmb model grid units.
  character(3) nmmb_verttype   !   'OLD' for old vertical coordinate definition
                               !                old def: p = eta1*pdtop+eta2*(psfc-pdtop-ptop)+ptop
                               !   'NEW' for new vertical coordinate definition
                               !                new def: p = eta1*pdtop+eta2*(psfc-ptop)+ptop

  integer(i_kind) nsig1o            ! max no. of levels distributed on each processor
  integer(i_kind) nnnn1o            ! actual of levels distributed on current processor
  integer(i_kind) nlat              ! no. of latitudes
  integer(i_kind) nlon              ! no. of longitudes
  integer(i_kind) nlat_sfc          ! no. of latitudes surface files
  integer(i_kind) nlon_sfc          ! no. of longitudes surface files
  integer(i_kind) nsig              ! no. of levels
  integer(i_kind) idvc5             ! vertical coordinate identifier
!                                        1: sigma
!                                        2: sigma-pressure
!                                        3: sigma-pressure-theta
  integer(i_kind) idvm5             
  integer(i_kind) idpsfc5           ! surface pressure identifier
!                                      0/1: ln(ps)
!                                        2: ps
  integer(i_kind) idthrm5           ! thermodynamic variable identifier
!                                      0/1: virtual temperature
!                                        2: sensible temperature
!                                        3: enthalpy (CpT)
  integer(i_kind) idsl5             ! midlayer pressure definition
!                                        1: Philips
!                                        2: average
  integer(i_kind) nsig2             ! 2 times number of levels
  integer(i_kind) nsig3             ! 3 times number of levels
  integer(i_kind) nsig3p1           ! 3 times number of levels plus 1
  integer(i_kind) nsig3p2           ! 3 times number of levels plus 2
  integer(i_kind) nsig3p3           ! 3 times number of levels plus 3
  integer(i_kind) nsig4             ! 4 times number of levels
  integer(i_kind) nsig5             ! 5 times number of levels
  integer(i_kind) nsig5p1           ! 5 times number of levels plus 1
  integer(i_kind) nsig_hlf          ! half number of levels

  integer(i_kind) ntracer           ! number of tracers
  integer(i_kind) ncloud            ! number of cloud types

  integer(i_kind) ns1               ! 2 times number of levels plus 1
  integer(i_kind) lat1              ! no. of lats on subdomain (no buffer)
  integer(i_kind) lon1              ! no. of lons on subdomain (no buffer)
  integer(i_kind) lat2              ! no. of lats on subdomain (buffer points on ends)
  integer(i_kind) lon2              ! no. of lons on subdomain (buffer points on ends)
  integer(i_kind) latlon11          ! horizontal points in subdomain (with buffer)
  integer(i_kind) latlon1n          ! no. of points in subdomain (with buffer)
  integer(i_kind) latlon1n1         ! no. of points in subdomain for 3d prs (with buffer)
  integer(i_kind) iglobal           ! number of horizontal points on global grid
  integer(i_kind) itotsub           ! number of horizontal points of all subdomains combined
  integer(i_kind) msig              ! number of profile layers to use when calling RTM



  logical periodic                              ! logical flag for periodic e/w domains
  logical,allocatable,dimension(:):: periodic_s ! logical flag for periodic e/w subdomain (all tasks)

  integer(i_kind),allocatable,dimension(:):: lpl_gfs ! number grid points for each row, GFS grid
  integer(i_kind),allocatable,dimension(:):: jstart  ! start lon of the whole array on each pe
  integer(i_kind),allocatable,dimension(:):: istart  ! start lat of the whole array on each pe
  integer(i_kind),allocatable,dimension(:):: ilat1   ! no. of lats for each subdomain (no buffer)
  integer(i_kind),allocatable,dimension(:):: jlon1   ! no. of lons for each subdomain (no buffer)
  integer(i_kind),allocatable,dimension(:):: ijn_s   ! no. of horiz. points for each subdomain (with buffer)
  integer(i_kind),allocatable,dimension(:):: ijn     ! no. of horiz. points for each subdomain (no buffer)
  integer(i_kind),allocatable,dimension(:):: isc_g   ! no. array, count for send to global; size of subdomain

                                               ! comm. array ...
  integer(i_kind),allocatable,dimension(:):: irc_s     !   count for receive on subdomain
  integer(i_kind),allocatable,dimension(:):: ird_s     !   displacement for receive on subdomain
  integer(i_kind),allocatable,dimension(:):: isd_g     !   displacement for send to global
  integer(i_kind),allocatable,dimension(:):: displs_s  !   displacement for send from subdomain
  integer(i_kind),allocatable,dimension(:):: displs_g  !   displacement for receive on global grid

                                             ! array element indices for location of ...
  integer(i_kind),allocatable,dimension(:):: ltosi   !   lats in iglobal array excluding buffer
  integer(i_kind),allocatable,dimension(:):: ltosj   !   lons in iglobal array excluding buffer
  integer(i_kind),allocatable,dimension(:):: ltosi_s !   lats in itotsub array including buffer
  integer(i_kind),allocatable,dimension(:):: ltosj_s !   lons in itotsub array including buffer

  integer(i_kind),dimension(100):: nlayers        ! number of RTM layers per model layer
                                                  ! (k=1 is near surface layer), default is 1



  real(r_kind) gencode

  real(r_kind),allocatable,dimension(:):: dx_gfs  ! resolution of GFS grid in degrees
  real(r_kind),allocatable,dimension(:):: rlats   ! grid latitudes (radians)
  real(r_kind),allocatable,dimension(:):: rlons   ! grid longitudes (radians)
  real(r_kind),allocatable,dimension(:):: rlats_sfc   ! grid latitudes (radians) surface
  real(r_kind),allocatable,dimension(:):: rlons_sfc   ! grid longitudes (radians) surface
  real(r_kind),allocatable,dimension(:):: ak5,bk5,ck5,tref5 ! coefficients for generalized vertical coordinate
  real(r_kind),allocatable,dimension(:):: cp5     ! specific heat for tracers
  real(r_kind),allocatable,dimension(:):: coslon  ! cos(grid longitudes (radians))
  real(r_kind),allocatable,dimension(:):: sinlon  ! sin(grid longitudes (radians))
  real(r_kind),allocatable,dimension(:):: wgtlats !  gaussian integration weights
  real(r_kind),allocatable,dimension(:):: corlats ! coriolis parameter by latitude
  real(r_kind),allocatable,dimension(:):: rbs2    ! 1./sin(grid latitudes))**2

! additional variables for regional mode
  real(r_kind),allocatable::  eta1_ll(:)          !
  real(r_kind),allocatable:: aeta1_ll(:)          !
  real(r_kind),allocatable::  eta2_ll(:)          !
  real(r_kind),allocatable:: aeta2_ll(:)          !
  real(r_kind),allocatable::region_lon(:,:)       !
  real(r_kind),allocatable::region_lat(:,:)       !
  real(r_kind),allocatable::region_dx(:,:)        !
  real(r_kind),allocatable::region_dy(:,:)        !
  real(r_kind),allocatable::region_dxi(:,:)       !
  real(r_kind),allocatable::region_dyi(:,:)       !
  real(r_kind),allocatable::coeffx(:,:)           !
  real(r_kind),allocatable::coeffy(:,:)           !

  real(r_kind) rlon_min_ll,rlon_max_ll,rlat_min_ll,rlat_max_ll
  real(r_kind) rlon_min_dd,rlon_max_dd,rlat_min_dd,rlat_max_dd
  real(r_kind) dt_ll,pdtop_ll,pt_ll

  integer(i_kind) nlon_regional,nlat_regional
  real(r_kind) regional_fhr
  integer(i_kind) regional_time(6)

! The following is for the generalized transform
  real(r_kind) pihalf,sign_pole,rlambda0
  real(r_kind) atilde_x,btilde_x,atilde_y,btilde_y
  real(r_kind) btilde_xinv,btilde_yinv
  integer(i_kind) nxtilde,nytilde
  real(r_kind),allocatable::xtilde0(:,:),ytilde0(:,:)
  real(r_kind),allocatable::beta_ref(:,:),cos_beta_ref(:,:),sin_beta_ref(:,:)
  integer(i_kind),allocatable::i0_tilde(:,:),j0_tilde(:,:)
  integer(i_byte),allocatable::ip_tilde(:,:),jp_tilde(:,:)
!----------temporary variables to keep track of number of observations falling in beta_ref jump zone
  real(r_kind):: count_beta_diff,count_beta_diff_gt_20
  real(r_kind) beta_diff_max,beta_diff_min,beta_diff_rms
  real(r_kind) beta_diff_max_gt_20

! Define structure to hold NCEP sigio/gfsio header information
  type:: ncepgfs_head
     integer(i_kind):: ivs
     integer(i_kind):: version
     real(r_single) :: fhour
     integer(i_kind):: idate(4)
     integer(i_kind):: nrec
     integer(i_kind):: latb
     integer(i_kind):: lonb
     integer(i_kind):: levs
     integer(i_kind):: jcap
     integer(i_kind):: itrun
     integer(i_kind):: iorder
     integer(i_kind):: irealf
     integer(i_kind):: igen
     integer(i_kind):: latf
     integer(i_kind):: lonf
     integer(i_kind):: latr
     integer(i_kind):: lonr
     integer(i_kind):: ntrac
     integer(i_kind):: icen2
     integer(i_kind):: iens(2)
     integer(i_kind):: idpp
     integer(i_kind):: idsl
     integer(i_kind):: idvc
     integer(i_kind):: idvm
     integer(i_kind):: idvt
     integer(i_kind):: idrun
     integer(i_kind):: idusr
     real(r_single) :: pdryini
     integer(i_kind):: ncldt
     integer(i_kind):: ixgr
     integer(i_kind):: nvcoord
     integer(i_kind):: idrt
  end type ncepgfs_head

  type:: ncepgfs_headv
     real(r_single),allocatable:: vcoord(:,:)
     real(r_single),allocatable:: cpi(:)    
  end type ncepgfs_headv

contains
   
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_grid --- Initialize defaults for grid related variables
!
! !INTERFACE:
!
  subroutine init_grid

! !DESCRIPTION: initialize defaults for grid related variables
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!   2005-06-01  treadon - add initialization of msig and nlayers
!   2010-03-06  parrish - add initialization of use_gfs_ozone flag
!   2010-03-09  parrish - add initialization of check_gfs_ozone_date flag
!   2010-03-15  parrish - add initialization of regional_ozone flag
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    use constants, only: izero,ione,two
    implicit none

    integer(i_kind) k

    nsig = 42_i_kind
    nsig1o = 7_i_kind
    nlat = 96_i_kind
    nlon = 384_i_kind
    idvc5 = ione
    idvm5 = izero
    idpsfc5 = ione
    idthrm5 = ione
    idsl5 = ione
    ntracer = ione
    ncloud = izero
    gencode = 80_i_kind
    regional = .false.
    ncep_sigio = .true.
    periodic = .false.
    wrf_nmm_regional = .false.
    wrf_mass_regional = .false.
    nems_nmmb_regional = .false.
    twodvar_regional = .false. 
    use_gfs_ozone = .false.
    check_gfs_ozone_date = .false.
    regional_ozone = .false.
    netcdf = .false.
    hybrid = .false.
    filled_grid = .false.
    half_grid = .false.
    grid_ratio_nmmb = sqrt(two)
    nmmb_reference_grid = 'H'
    nmmb_verttype = 'OLD'
    lat1 = nlat
    lon1 = nlon
    lat2 = lat1+2_i_kind
    lon2 = lon1+2_i_kind

    diagnostic_reg = .false.
    update_regsfc = .false.
    nlon_regional = izero
    nlat_regional = izero

    msig = nsig
    do k=1,100
       nlayers(k) = ione
    end do

    return
  end subroutine init_grid
  
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_grid_vars --- Set grid related variables
!
! !INTERFACE:
!
  subroutine init_grid_vars(jcap,npe)

! !USES:

    use constants, only: izero,ione
    implicit none

! !INPUT PARAMETERS:

   integer(i_kind),intent(in   ) :: jcap   ! spectral truncation
   integer(i_kind),intent(in   ) :: npe    ! number of mpi tasks

! !DESCRIPTION: set grid related variables (post namelist read)
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-06-01  treadon - add computation of msig
!
!   input argument list:
!
!   output argument list:
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    integer(i_kind) vlevs,k

    if(jcap==62_i_kind) gencode=80.0_r_kind
    ns1=2*nsig+ione
    nsig2=2*nsig
    nsig3=3*nsig
    nsig3p1=3*nsig+ione
    nsig3p2=3*nsig+2_i_kind
    nsig3p3=3*nsig+3_i_kind
    nsig4=4*nsig
    nsig5=5*nsig
    nsig5p1=5*nsig+ione
    nsig_hlf=nsig/2
    iglobal=nlat*nlon

! Initialize nsig1o to distribute levs/variables
! as evenly as possible over the tasks
    vlevs=(6*nsig)+4_i_kind
    nsig1o=vlevs/npe
    if(mod(vlevs,npe)/=izero) nsig1o=nsig1o+ione
    nnnn1o=nsig1o                  ! temporarily set the number of levels to nsig1o

! Sum total number of vertical layers for RTM call
    msig = izero
    do k=1,nsig
       msig = msig + nlayers(k)
    end do

    return
  end subroutine init_grid_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_subdomain_vars --- Initialize variables related to subdomains
!
! !INTERFACE:
!
  subroutine init_subdomain_vars

! !DESCRIPTION: initialize variables related to subdomains
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!   2008-11-28  todling - latlon1n1 (for 3d prs)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    implicit none

    lat2 = lat1+2_i_kind
    lon2 = lon1+2_i_kind
    latlon11 = lat2*lon2
    latlon1n = latlon11*nsig
    latlon1n1= latlon1n+latlon11

    return
  end subroutine init_subdomain_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_grid_vars --- Allocate memory for grid related variables
!
! !INTERFACE:
!
  subroutine create_grid_vars

! !DESCRIPTION: allocate memory for grid related variables
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    use constants, only: ione
    implicit none

    allocate(rlats(nlat),rlons(nlon),coslon(nlon),sinlon(nlon),&
             wgtlats(nlat),rbs2(nlat),corlats(nlat))
    allocate(ak5(nsig+ione),bk5(nsig+ione),ck5(nsig+ione),tref5(nsig))
    return
  end subroutine create_grid_vars
    
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_grid_vars --- Deallocate memory for grid related variables
!
! !INTERFACE:
!
  subroutine destroy_grid_vars

! !DESCRIPTION: deallocate memory for grid related variables
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!   2009-12-20  gayno - add variable lpl_gfs
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    implicit none

    deallocate(rlats,rlons,corlats,coslon,sinlon,wgtlats,rbs2)
    deallocate(ak5,bk5,ck5,tref5)
    if (allocated(cp5)) deallocate(cp5)
    if (allocated(dx_gfs)) deallocate(dx_gfs)
    if (allocated(lpl_gfs)) deallocate(lpl_gfs)
    return
  end subroutine destroy_grid_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_mapping --- Init vars mapping between global domain/subd.
!
! !INTERFACE:
!
  subroutine create_mapping(npe)

! !USES:

    use constants, only: izero
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind),intent(in   ) :: npe   ! number of mpi tasks

! !DESCRIPTION: allocate and initialize variables that create mapping
!           between global domain and subdomains
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    integer(i_kind) i

    allocate(periodic_s(npe),jstart(npe),istart(npe),&
         ilat1(npe),jlon1(npe),&
       ijn_s(npe),irc_s(npe),ird_s(npe),displs_s(npe),&
       ijn(npe),isc_g(npe),isd_g(npe),displs_g(npe))

    do i=1,npe
       periodic_s(i)= .false.
       jstart(i)    = izero
       istart(i)    = izero
       ilat1(i)     = izero
       jlon1(i)     = izero
       ijn_s(i)     = izero
       irc_s(i)     = izero
       ird_s(i)     = izero
       displs_s(i)  = izero
       ijn(i)       = izero
       isc_g(i)     = izero
       isd_g(i)     = izero
       displs_g(i)  = izero
    end do

    return
  end subroutine create_mapping
  
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_mapping --- Dealloc global/subdomain mapping arrays
!
! !INTERFACE:
!
  subroutine destroy_mapping

! !DESCRIPTION: deallocate memory for global/subdomain mapping variables
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!   2007-02-20  todling - somehow dealloc for irc_s,ird_s got lost
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    implicit none

    deallocate(ltosi,ltosj,ltosi_s,ltosj_s)
    deallocate(periodic_s,jstart,istart,ilat1,jlon1,&
       ijn_s,irc_s,ird_s,displs_s,&
       ijn,isc_g,isd_g,displs_g)

    return
  end subroutine destroy_mapping


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_reg_glob_ll --- In case regional, initialize setting
!
! !INTERFACE:
!
  subroutine init_reg_glob_ll(mype,lendian_in)

! !USES:

    use constants, only: izero,ione,zero, one, three, deg2rad,pi,half, two
    use mod_nmmb_to_a, only: init_nmmb_to_a,nxa,nya,nmmb_h_to_a
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind), intent(in   ) :: mype          ! mpi task id
    integer(i_kind), intent(in   ) :: lendian_in    ! unit number reserved for
                                                    !  little endian input

! !DESCRIPTION: decide if regional run or not, and initialize constants 
!           required for rotation transformation
!
!
!   output argument list:
!
!   Notes about grid definition:
!   \begin{enumerate}
!   \item  The origin of the analysis coordinate system is always $rlon=180.$, $rlat=0.$, 
!          whether this is a global or regional run.  The point $rlon=180$, $rlat=0$ in 
!          the rotated coordinate coincides with the point rlon0\_origin, rlat0\_origin 
!          in earth coordinates.  This is why $rlon0_origin=180$. in the global case.
!
!   \item  For regional runs, the rotated coordinate origin and extent of the domain are read
!          in from the NMM restart file.
!
!   \item  The reason for having the longitude of the origin = 180 is because there are
!          places in the global analysis that depend on $0 < lon < 360$.  So to minimize changes
!          to the global code, this approach has been adopted.
!
!   \item  The regional analysis domain is larger than the corresponding NMM grid.  A halo is included
!          whose width is a function of the interpolation order for transfers between grids.
!          This is so the analysis increment is always being interpolated and added on to the
!          full model domain.
!   \end{enumerate}
!
! !REVISION HISTORY:
!   2003-08-28  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-12-15  treadon - explicity set value for inges
!   2005-05-24  pondeca - add the surface analysis option
!   2006-04-06  middlecoff - changed inges from 21 to lendian_in so it can be set to little endian.
!   2009-01-02  todling - remove unused vars
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   parrish          org: np22                date: 2003-08-28
!
!EOP
!-------------------------------------------------------------------------

    logical fexist
    integer(i_kind) i,j,k
    real(r_single)pt,pdtop
    real(r_single),allocatable:: deta1(:),aeta1(:),eta1(:),deta2(:),aeta2(:),eta2(:)
    real(r_single) dlmd,dphd
    real(r_single),allocatable:: glat(:,:),glon(:,:)
    real(r_single),allocatable:: dx_nmm(:,:),dy_nmm(:,:)
    real(r_single),allocatable:: dx_mc(:,:),dy_mc(:,:)

    real(r_kind),parameter:: r0_01=0.01_r_kind
    real(r_kind),parameter:: r1_5=1.5_r_kind
    real(r_kind),parameter:: six=6.0_r_kind
    real(r_kind),parameter:: r90=90.0_r_kind
    real(r_kind),parameter:: r360=360.0_r_kind

    real(r_kind),allocatable::glat_an(:,:),glon_an(:,:)
    real(r_kind),allocatable:: dx_an(:,:),dy_an(:,:)
    character(6) filename
    integer(i_kind) ihr
    real(r_kind),allocatable::gxtemp(:,:),gytemp(:,:)
    real(r_single),allocatable::gxtemp4(:,:),gytemp4(:,:)
    real(r_kind),allocatable::gxtemp_an(:,:),gytemp_an(:,:)
    real(r_kind) rtemp

    if(.not.regional) then
! This is global run
       rlat_min_ll=-r90*deg2rad
       rlat_max_ll=r90*deg2rad
       rlon_min_ll=zero*deg2rad
       rlon_max_ll=r360*deg2rad
       rlon_min_dd=rlon_min_ll-deg2rad
       rlon_max_dd=rlon_max_ll+deg2rad
       rlat_min_dd=rlat_min_ll-deg2rad
       rlat_max_dd=rlat_max_ll+deg2rad
       dt_ll=zero
    end if

    if(wrf_nmm_regional) then     ! begin wrf_nmm section
! This is a wrf_nmm regional run.
       if(diagnostic_reg.and.mype==izero)  &
          write(6,*)' in init_reg_glob_ll, initializing for wrf nmm regional run'

! Get regional constants
       ihr=-999_i_kind
       do i=0,12
          write(filename,'("sigf",i2.2)')i
          inquire(file=filename,exist=fexist)
          if(fexist) then
             ihr=i
             exit
          end if
       end do
       if(ihr<izero) then
          write(6,*)' NO INPUT FILE AVAILABLE FOR REGIONAL (WRFNMM) ANALYSIS.  PROGRAM STOPS'
          call stop2(99)
       end if

       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, lendian_in=',lendian_in
       open(lendian_in,file=filename,form='unformatted')
       rewind lendian_in
       read(lendian_in) regional_time,nlon_regional,nlat_regional,nsig, &
                   dlmd,dphd,pt,pdtop
       regional_fhr=zero  !  with wrf nmm fcst hr is not currently available.

       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, yr,mn,dy,h,m,s=",6i6)') &
                regional_time
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nlon_regional=",i6)') &
                nlon_regional
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nlat_regional=",i6)') &
                nlat_regional
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nsig=",i6)') nsig 
 
! Get vertical info for hybrid coordinate and sigma coordinate we will interpolate to
       allocate(aeta1_ll(nsig),eta1_ll(nsig+ione),aeta2_ll(nsig),eta2_ll(nsig+ione))
       allocate(deta1(nsig),aeta1(nsig),eta1(nsig+ione),deta2(nsig),aeta2(nsig),eta2(nsig+ione))
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
       allocate(dx_nmm(nlon_regional,nlat_regional),dy_nmm(nlon_regional,nlat_regional))
       read(lendian_in) deta1
       read(lendian_in) aeta1
       read(lendian_in) eta1
       read(lendian_in) deta2
       read(lendian_in) aeta2
       read(lendian_in) eta2

       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, pdtop,pt=',pdtop,pt
       if(diagnostic_reg.and.mype==izero) then
          write(6,*)' in init_reg_glob_ll, aeta1 aeta2 follow:'
          do k=1,nsig
             write(6,'(" k,aeta1,aeta2=",i3,2f10.4)') k,aeta1(k),aeta2(k)
          end do
          write(6,*)' in init_reg_glob_ll, deta1 deta2 follow:'
          do k=1,nsig
             write(6,'(" k,deta1,deta2=",i3,2f10.4)') k,deta1(k),deta2(k)
          end do
          write(6,*)' in init_reg_glob_ll, deta1 deta2 follow:'
          do k=1,nsig+ione
             write(6,'(" k,eta1,eta2=",i3,2f10.4)') k,eta1(k),eta2(k)
          end do
       end if

       pdtop_ll=r0_01*pdtop                    !  check units--this converts to mb
       pt_ll=r0_01*pt                          !  same here
 
       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, pdtop_ll,pt_ll=',pdtop_ll,pt_ll
       eta1_ll=eta1
       aeta1_ll=aeta1
       eta2_ll=eta2
       aeta2_ll=aeta2
       read(lendian_in) glat,dx_nmm
       read(lendian_in) glon,dy_nmm
       close(lendian_in)

       rlon_min_ll=one
       rlat_min_ll=one
       if(filled_grid) then
          nlon=2*nlon_regional-ione
          nlat=nlat_regional
          rlon_max_ll=nlon
          rlat_max_ll=nlat
          rlat_min_dd=rlat_min_ll+three
          rlat_max_dd=rlat_max_ll-three
          rlon_min_dd=rlon_min_ll+six
          rlon_max_dd=rlon_max_ll-six
       end if
       if(half_grid) then
          nlon=nlon_regional
          nlat=ione+nlat_regional/2
          rlon_max_ll=nlon
          rlat_max_ll=nlat
          rlat_min_dd=rlat_min_ll+r1_5
          rlat_max_dd=rlat_max_ll-r1_5
          rlon_min_dd=rlon_min_ll+three
          rlon_max_dd=rlon_max_ll-three
       end if

       if(diagnostic_reg.and.mype==izero) then
          write(6,*)' in init_reg_glob_ll, rlat_min_dd=',rlat_min_dd
          write(6,*)' in init_reg_glob_ll, rlat_max_dd=',rlat_max_dd
          write(6,*)' in init_reg_glob_ll, rlon_min_dd=',rlon_min_dd
          write(6,*)' in init_reg_glob_ll, rlon_max_dd=',rlon_max_dd
          write(6,*)' in init_reg_glob_ll, rlat_min_ll=',rlat_min_ll
          write(6,*)' in init_reg_glob_ll, rlat_max_ll=',rlat_max_ll
          write(6,*)' in init_reg_glob_ll, rlon_min_ll=',rlon_min_ll
          write(6,*)' in init_reg_glob_ll, rlon_max_ll=',rlon_max_ll
          write(6,*)' in init_reg_glob_ll, filled_grid,half_grid=',filled_grid,half_grid
          write(6,*)' in init_reg_glob_ll, nlon,nlat=',nlon,nlat
       end if

       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       allocate(region_dy(nlat,nlon),region_dx(nlat,nlon))
       allocate(region_dyi(nlat,nlon),region_dxi(nlat,nlon))
       allocate(coeffy(nlat,nlon),coeffx(nlat,nlon))

!   generate earth lats and lons on analysis grid

       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
       allocate(dx_an(nlon,nlat),dy_an(nlon,nlat))
 
       if(half_grid) then
          call half_nmm_grid2a(glon,nlon_regional,nlat_regional,glon_an,ione)
          call half_nmm_grid2a(glat,nlon_regional,nlat_regional,glat_an,ione)
          call half_nmm_grid2a(dx_nmm,nlon_regional,nlat_regional,dx_an,ione)
          call half_nmm_grid2a(dy_nmm,nlon_regional,nlat_regional,dy_an,ione)
          dx_an=two*dx_an
          dy_an=two*dy_an
       end if

       if(filled_grid) then
          allocate(gxtemp(nlon_regional,nlat_regional))
          allocate(gytemp(nlon_regional,nlat_regional))
          allocate(gxtemp_an(nlon,nlat))
          allocate(gytemp_an(nlon,nlat))
          sign_pole=-one
          pihalf=half*pi
          if(maxval(glat)/deg2rad<zero) sign_pole=one
          do j=1,nlat_regional
             do i=1,nlon_regional
                rtemp=pihalf+sign_pole*glat(i,j)
                gxtemp(i,j)=rtemp*cos(one*glon(i,j))
                gytemp(i,j)=rtemp*sin(one*glon(i,j))
             end do
          end do
          call fill_nmm_grid2a3(gxtemp,nlon_regional,nlat_regional,gxtemp_an)
          call fill_nmm_grid2a3(gytemp,nlon_regional,nlat_regional,gytemp_an)
          do j=1,nlat
             do i=1,nlon
                rtemp=sqrt(gxtemp_an(i,j)**2+gytemp_an(i,j)**2)
                glat_an(i,j)=sign_pole*(rtemp-pihalf)
                glon_an(i,j)=atan2(gytemp_an(i,j),gxtemp_an(i,j))
             end do
          end do
          gxtemp=dx_nmm
          gytemp=dy_nmm
          call fill_nmm_grid2a3(gxtemp,nlon_regional,nlat_regional,dx_an)
          call fill_nmm_grid2a3(gytemp,nlon_regional,nlat_regional,dy_an)
          deallocate(gxtemp,gytemp,gxtemp_an,gytemp_an)
       end if

       do k=1,nlon
          do i=1,nlat
             region_lat(i,k)=glat_an(k,i)
             region_lon(i,k)=glon_an(k,i)
             region_dy(i,k)=dy_an(k,i)
             region_dx(i,k)=dx_an(k,i)
             region_dyi(i,k)=one/dy_an(k,i)
             region_dxi(i,k)=one/dx_an(k,i)
             coeffy(i,k)=half/dy_an(k,i)
             coeffx(i,k)=half/dx_an(k,i)
          end do
       end do

! ???????  later change glat_an,glon_an to region_lat,region_lon, with dimensions flipped
       call init_general_transform(glat_an,glon_an,mype)

       deallocate(deta1,aeta1,eta1,deta2,aeta2,eta2,glat,glon,glat_an,glon_an)
       deallocate(dx_nmm,dy_nmm,dx_an,dy_an)

    end if   ! end if wrf_nmm section

    if(wrf_mass_regional) then     ! begin wrf mass core section
! This is a wrf_mass regional run.
       if(diagnostic_reg.and.mype==izero) &
          write(6,*)' in init_reg_glob_ll, initializing for wrf mass core regional run'

! Get regional constants
       ihr=-999_i_kind
       do i=0,12
          write(filename,'("sigf",i2.2)')i
          inquire(file=filename,exist=fexist)
          if(fexist) then
             ihr=i
             exit
          end if
       end do
       if(ihr<izero) then
          write(6,*)' NO INPUT FILE AVAILABLE FOR REGIONAL (WRF MASS CORE) ANALYSIS.  PROGRAM STOPS'
          call stop2(99)
       end if
       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, lendian_in=',lendian_in
       open(lendian_in,file=filename,form='unformatted')
       rewind lendian_in
       read(lendian_in) regional_time,nlon_regional,nlat_regional,nsig,pt
       regional_fhr=zero  !  with wrf mass core fcst hr is not currently available.

       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, yr,mn,dy,h,m,s=",6i6)') &
                regional_time
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nlon_regional=",i6)') &
                nlon_regional
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nlat_regional=",i6)') &
                nlat_regional
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nsig=",i6)') nsig 
 
! Get vertical info for wrf mass core
       allocate(aeta1_ll(nsig),eta1_ll(nsig+ione))
       allocate(aeta1(nsig),eta1(nsig+ione))
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
       allocate(dx_mc(nlon_regional,nlat_regional),dy_mc(nlon_regional,nlat_regional))
       read(lendian_in) aeta1
       read(lendian_in) eta1
 
       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, pt=',pt
       if(diagnostic_reg.and.mype==izero) then
          write(6,*)' in init_reg_glob_ll, aeta1 follows:'
          do k=1,nsig
             write(6,'(" k,aeta1=",i3,f10.4)') k,aeta1(k)
          end do
          write(6,*)' in init_reg_glob_ll, eta1 follows:'
          do k=1,nsig+ione
             write(6,'(" k,eta1=",i3,f10.4)') k,eta1(k)
          end do
       end if

       pt_ll=r0_01*pt                    !  check units--this converts to mb

       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, pt_ll=',pt_ll
       eta1_ll=eta1
       aeta1_ll=aeta1
       read(lendian_in) glat,dx_mc
       read(lendian_in) glon,dy_mc
       close(lendian_in)
 
       rlon_min_ll=one
       rlat_min_ll=one
       nlon=nlon_regional
       nlat=nlat_regional
       rlon_max_ll=nlon
       rlat_max_ll=nlat
       rlat_min_dd=rlat_min_ll+r1_5
       rlat_max_dd=rlat_max_ll-r1_5
       rlon_min_dd=rlon_min_ll+r1_5
       rlon_max_dd=rlon_max_ll-r1_5
 
       if(diagnostic_reg.and.mype==izero) then
          write(6,*)' in init_reg_glob_ll, rlat_min_dd=',rlat_min_dd
          write(6,*)' in init_reg_glob_ll, rlat_max_dd=',rlat_max_dd
          write(6,*)' in init_reg_glob_ll, rlon_min_dd=',rlon_min_dd
          write(6,*)' in init_reg_glob_ll, rlon_max_dd=',rlon_max_dd
          write(6,*)' in init_reg_glob_ll, rlat_min_ll=',rlat_min_ll
          write(6,*)' in init_reg_glob_ll, rlat_max_ll=',rlat_max_ll
          write(6,*)' in init_reg_glob_ll, rlon_min_ll=',rlon_min_ll
          write(6,*)' in init_reg_glob_ll, rlon_max_ll=',rlon_max_ll
          write(6,*)' in init_reg_glob_ll, nlon,nlat=',nlon,nlat
       end if

       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       allocate(region_dy(nlat,nlon),region_dx(nlat,nlon))
       allocate(region_dyi(nlat,nlon),region_dxi(nlat,nlon))
       allocate(coeffy(nlat,nlon),coeffx(nlat,nlon))

!   trasfer earth lats and lons to arrays region_lat, region_lon

       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
       do k=1,nlon
          do i=1,nlat
             glat_an(k,i)=glat(k,i)
             glon_an(k,i)=glon(k,i)
             region_lat(i,k)=glat(k,i)
             region_lon(i,k)=glon(k,i)
             region_dx(i,k)=dx_mc(k,i)
             region_dy(i,k)=dy_mc(k,i)
             region_dxi(i,k)=one/dx_mc(k,i)
             region_dyi(i,k)=one/dy_mc(k,i)
             coeffx(i,k)=half/dx_mc(k,i)
             coeffy(i,k)=half/dy_mc(k,i)
          end do
       end do

! ???????  later change glat_an,glon_an to region_lat,region_lon, with dimensions flipped
       call init_general_transform(glat_an,glon_an,mype)

       deallocate(aeta1,eta1,glat,glon,glat_an,glon_an)
       deallocate(dx_mc,dy_mc)
 
    end if   ! end if wrf mass core section

    if(nems_nmmb_regional) then     ! begin nems nmmb section
! This is a nems_nmmb regional run.
       if(diagnostic_reg.and.mype==izero)  &
          write(6,*)' in init_reg_glob_ll, initializing for nems nmmb regional run'

! Get regional constants
       ihr=-999_i_kind
       do i=0,12
          write(filename,'("sigf",i2.2)')i
          inquire(file=filename,exist=fexist)
          if(fexist) then
             ihr=i
             exit
          end if
       end do
       if(ihr<izero) then
          write(6,*)' NO INPUT FILE AVAILABLE FOR REGIONAL (NEMS NMMB) ANALYSIS.  PROGRAM STOPS'
          call stop2(99)
       end if

       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, lendian_in=',lendian_in
       open(lendian_in,file=filename,form='unformatted')
       rewind lendian_in
       read(lendian_in) regional_time,regional_fhr,nlon_regional,nlat_regional,nsig, &
                   dlmd,dphd,pt,pdtop
 
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, yr,mn,dy,h,m,s=",6i6)') &
                regional_time
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nlon_regional=",i6)') &
                nlon_regional
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nlat_regional=",i6)') &
                nlat_regional
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nsig=",i6)') nsig
 
       call init_nmmb_to_a(nmmb_reference_grid,grid_ratio_nmmb,nlon_regional,nlat_regional)
       nlon=nxa ; nlat=nya
 
       rlon_min_ll=one
       rlat_min_ll=one
       rlon_max_ll=nlon
       rlat_max_ll=nlat
    !  rlat_min_dd=rlat_min_ll+three/grid_ratio_nmmb
    !  rlat_max_dd=rlat_max_ll-three/grid_ratio_nmmb
    !  rlon_min_dd=rlon_min_ll+six/grid_ratio_nmmb
    !  rlon_max_dd=rlon_max_ll-six/grid_ratio_nmmb
       rlat_min_dd=rlat_min_ll+r1_5*1.412_r_kind/grid_ratio_nmmb
       rlat_max_dd=rlat_max_ll-r1_5*1.412_r_kind/grid_ratio_nmmb
       rlon_min_dd=rlon_min_ll+three*1.412_r_kind/grid_ratio_nmmb
       rlon_max_dd=rlon_max_ll-three*1.412_r_kind/grid_ratio_nmmb

       if(diagnostic_reg.and.mype==izero) then
          write(6,*)' in init_reg_glob_ll, rlat_min_dd=',rlat_min_dd
          write(6,*)' in init_reg_glob_ll, rlat_max_dd=',rlat_max_dd
          write(6,*)' in init_reg_glob_ll, rlon_min_dd=',rlon_min_dd
          write(6,*)' in init_reg_glob_ll, rlon_max_dd=',rlon_max_dd
          write(6,*)' in init_reg_glob_ll, rlat_min_ll=',rlat_min_ll
          write(6,*)' in init_reg_glob_ll, rlat_max_ll=',rlat_max_ll
          write(6,*)' in init_reg_glob_ll, rlon_min_ll=',rlon_min_ll
          write(6,*)' in init_reg_glob_ll, rlon_max_ll=',rlon_max_ll
          write(6,*)' in init_reg_glob_ll, nmmb_reference_grid=',nmmb_reference_grid
          write(6,*)' in init_reg_glob_ll, grid_ratio_nmmb=',grid_ratio_nmmb
          write(6,*)' in init_reg_glob_ll, nlon,nlat=',nlon,nlat
       end if

! Get vertical info for hybrid coordinate and sigma coordinate we will interpolate to
       allocate(aeta1_ll(nsig),eta1_ll(nsig+ione),aeta2_ll(nsig),eta2_ll(nsig+ione))
       allocate(deta1(nsig),aeta1(nsig),eta1(nsig+ione),deta2(nsig),aeta2(nsig),eta2(nsig+ione))
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
       allocate(dx_nmm(nlon_regional,nlat_regional),dy_nmm(nlon_regional,nlat_regional))
       read(lendian_in) deta1
       read(lendian_in) aeta1
       read(lendian_in) eta1
       read(lendian_in) deta2
       read(lendian_in) aeta2
       read(lendian_in) eta2
!----------------------------------------detect if new nmmb coordinate:
       nmmb_verttype='OLD'
       if(aeta1(1)<.6_r_single) then
          if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, detect new nmmb vert coordinate'
          aeta1=aeta1+aeta2
          eta1=eta1+eta2
          nmmb_verttype='NEW'
       end if

       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, pdtop,pt=',pdtop,pt
       if(diagnostic_reg.and.mype==izero) then
          write(6,*)' in init_reg_glob_ll, aeta1 aeta2 follow:'
          do k=1,nsig
             write(6,'(" k,aeta1,aeta2=",i3,2f10.4)') k,aeta1(k),aeta2(k)
          end do
          write(6,*)' in init_reg_glob_ll, deta1 deta2 follow:'
          do k=1,nsig
             write(6,'(" k,deta1,deta2=",i3,2f10.4)') k,deta1(k),deta2(k)
          end do
          write(6,*)' in init_reg_glob_ll, deta1 deta2 follow:'
          do k=1,nsig+ione
             write(6,'(" k,eta1,eta2=",i3,2f10.4)') k,eta1(k),eta2(k)
          end do
       end if

       pdtop_ll=r0_01*pdtop                    !  check units--this converts to mb
       pt_ll=r0_01*pt                          !  same here

       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, pdtop_ll,pt_ll=',pdtop_ll,pt_ll
       eta1_ll=eta1
       aeta1_ll=aeta1
       eta2_ll=eta2
       aeta2_ll=aeta2
       read(lendian_in) glat,dx_nmm
       read(lendian_in) glon,dy_nmm
       close(lendian_in)

       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       allocate(region_dy(nlat,nlon),region_dx(nlat,nlon))
       allocate(region_dyi(nlat,nlon),region_dxi(nlat,nlon))
       allocate(coeffy(nlat,nlon),coeffx(nlat,nlon))

!   generate earth lats and lons on analysis grid

       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
       allocate(dx_an(nlat,nlon),dy_an(nlat,nlon))
 
       allocate(gxtemp4(nlon_regional,nlat_regional))
       allocate(gytemp4(nlon_regional,nlat_regional))
       allocate(gxtemp_an(nlat,nlon))
       allocate(gytemp_an(nlat,nlon))
       sign_pole=-one
       pihalf=half*pi
       if(maxval(glat)/deg2rad<zero) sign_pole=one
       do j=1,nlat_regional
          do i=1,nlon_regional
             rtemp=pihalf+sign_pole*glat(i,j)
             gxtemp4(i,j)=rtemp*cos(one*glon(i,j))
             gytemp4(i,j)=rtemp*sin(one*glon(i,j))
          end do
       end do
       call nmmb_h_to_a(gxtemp4,gxtemp_an)
       call nmmb_h_to_a(gytemp4,gytemp_an)
       do i=1,nlon
          do j=1,nlat
             rtemp=sqrt(gxtemp_an(j,i)**2+gytemp_an(j,i)**2)
             glat_an(i,j)=sign_pole*(rtemp-pihalf)
             glon_an(i,j)=atan2(gytemp_an(j,i),gxtemp_an(j,i))
          end do
       end do
       gxtemp4=dx_nmm
       gytemp4=dy_nmm
       call nmmb_h_to_a(gxtemp4,dx_an)
       call nmmb_h_to_a(gytemp4,dy_an)
       dx_an=grid_ratio_nmmb*dx_an
       dy_an=grid_ratio_nmmb*dy_an
       deallocate(gxtemp4,gytemp4,gxtemp_an,gytemp_an)

       do k=1,nlon
          do i=1,nlat
             region_lat(i,k)=glat_an(k,i)
             region_lon(i,k)=glon_an(k,i)
             region_dy(i,k)=dy_an(i,k)
             region_dx(i,k)=dx_an(i,k)
             region_dyi(i,k)=one/dy_an(i,k)
             region_dxi(i,k)=one/dx_an(i,k)
             coeffy(i,k)=half/dy_an(i,k)
             coeffx(i,k)=half/dx_an(i,k)
          end do
       end do

! ???????  later change glat_an,glon_an to region_lat,region_lon, with dimensions flipped
       call init_general_transform(glat_an,glon_an,mype)

       deallocate(deta1,aeta1,eta1,deta2,aeta2,eta2,glat,glon,glat_an,glon_an)
       deallocate(dx_nmm,dy_nmm,dx_an,dy_an)

    end if   ! end if nems nmmb section

!   Begin surface analysis section (regional 2D-var)
    if(twodvar_regional) then 

! This is a surface analysis regional run.
       if(diagnostic_reg.and.mype==izero) &
          write(6,*)' in init_reg_glob_ll, initializing for surface analysis regional run'

! Get regional constants
       ihr=-999_i_kind
       do i=0,12
          write(filename,'("sigf",i2.2)')i
          inquire(file=filename,exist=fexist)
          if(fexist) then
             ihr=i
             exit
          end if
       end do
       if(ihr<izero) then
          write(6,*)' NO INPUT FILE AVAILABLE FOR REGIONAL (SURFACE) ANALYSIS.  PROGRAM STOPS'
          call stop2(99)
       end if
       if(diagnostic_reg.and.mype==izero) write(6,*)' in init_reg_glob_ll, lendian_in=',lendian_in
       open(lendian_in,file=filename,form='unformatted')
       rewind lendian_in
       read(lendian_in) regional_time,nlon_regional,nlat_regional,nsig
       regional_fhr=zero  !  with twodvar analysis fcst hr is not currently available.
 
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, yr,mn,dy,h,m,s=",6i6)') &
                regional_time
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nlon_regional=",i6)') &
                nlon_regional
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nlat_regional=",i6)') &
                nlat_regional
       if(diagnostic_reg.and.mype==izero) write(6,'(" in init_reg_glob_ll, nsig=",i6)') nsig 
 
! Get vertical info 
       allocate(aeta1_ll(nsig),eta1_ll(nsig+ione))
       allocate(aeta1(nsig),eta1(nsig+ione))
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
       allocate(dx_mc(nlon_regional,nlat_regional),dy_mc(nlon_regional,nlat_regional))
 
       aeta1=one                ! set to this value for convenience
       eta1=one                 ! set to this value for convenience
       pt=0._r_single           ! set to this value for convenience

       pt_ll=r0_01*pt
       eta1_ll=eta1
       aeta1_ll=aeta1
 
       read(lendian_in) glat,dx_mc
       read(lendian_in) glon,dy_mc
       close(lendian_in)
 
       rlon_min_ll=one
       rlat_min_ll=one
       nlon=nlon_regional
       nlat=nlat_regional
       rlon_max_ll=nlon
       rlat_max_ll=nlat
       rlat_min_dd=rlat_min_ll+r1_5
       rlat_max_dd=rlat_max_ll-r1_5
       rlon_min_dd=rlon_min_ll+r1_5
       rlon_max_dd=rlon_max_ll-r1_5

       if(diagnostic_reg.and.mype==izero) then
          write(6,*)' in init_reg_glob_ll, rlat_min_dd=',rlat_min_dd
          write(6,*)' in init_reg_glob_ll, rlat_max_dd=',rlat_max_dd
          write(6,*)' in init_reg_glob_ll, rlon_min_dd=',rlon_min_dd
          write(6,*)' in init_reg_glob_ll, rlon_max_dd=',rlon_max_dd
          write(6,*)' in init_reg_glob_ll, rlat_min_ll=',rlat_min_ll
          write(6,*)' in init_reg_glob_ll, rlat_max_ll=',rlat_max_ll
          write(6,*)' in init_reg_glob_ll, rlon_min_ll=',rlon_min_ll
          write(6,*)' in init_reg_glob_ll, rlon_max_ll=',rlon_max_ll
          write(6,*)' in init_reg_glob_ll, nlon,nlat=',nlon,nlat
       end if

       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       allocate(region_dy(nlat,nlon),region_dx(nlat,nlon))
       allocate(region_dyi(nlat,nlon),region_dxi(nlat,nlon))
       allocate(coeffy(nlat,nlon),coeffx(nlat,nlon))

!   transfer earth lats and lons to arrays region_lat, region_lon

       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
       do k=1,nlon
          do i=1,nlat
             glat_an(k,i)=glat(k,i)
             glon_an(k,i)=glon(k,i)
             region_lat(i,k)=glat(k,i)
             region_lon(i,k)=glon(k,i)
             region_dx(i,k)=dx_mc(k,i)
             region_dy(i,k)=dy_mc(k,i)
             region_dxi(i,k)=one/dx_mc(k,i)
             region_dyi(i,k)=one/dy_mc(k,i)
             coeffx(i,k)=half/dx_mc(k,i)
             coeffy(i,k)=half/dy_mc(k,i)
          end do
       end do

! ???????  later change glat_an,glon_an to region_lat,region_lon, with dimensions flipped
       call init_general_transform(glat_an,glon_an,mype)

       deallocate(aeta1,eta1,glat,glon,glat_an,glon_an)
       deallocate(dx_mc,dy_mc)

    end if   ! end if twodvar analysis section

    return
  end subroutine init_reg_glob_ll

 subroutine init_general_transform(glats,glons,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_general_transform
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    glons,glats - lons,lats of input grid points of dimesion nlon,nlat
!    mype        - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: izero,ione,zero,one,half,pi,deg2rad,two
  implicit none

  real(r_kind)   ,intent(in   ) :: glats(nlon,nlat),glons(nlon,nlat)
  integer(i_kind),intent(in   ) :: mype

  real(r_kind),parameter:: r0_01=0.01_r_kind
  real(r_kind),parameter:: rbig =1.0e30_r_kind
  real(r_kind) xbar_min,xbar_max,ybar_min,ybar_max
  real(r_kind) clon,slon,r_of_lat,xbar,ybar
  integer(i_kind) i,j,istart0,iend,iinc,itemp,ilast,jlast
  real(r_kind) cosalpha,sinalpha,denom,epslon,r0,r1,x0,x1,x2,y0,y1,y2
  integer(i_kind) ip

  pihalf=half*pi

!  define xtilde, ytilde grid, transform

!      glons,glats are lons, lats of input grid points of dimension nlon,nlat
  if(mype==izero) write(6,*)' at  1 in init_general_transform'
  call get_xytilde_domain(nlon,nlat,glons,glats,nxtilde,nytilde, &
                   xbar_min,xbar_max,ybar_min,ybar_max)
  allocate(i0_tilde(nxtilde,nytilde),j0_tilde(nxtilde,nytilde))
  allocate(ip_tilde(nxtilde,nytilde),jp_tilde(nxtilde,nytilde))
  allocate(xtilde0(nlon,nlat),ytilde0(nlon,nlat))

! define atilde_x, btilde_x, atilde_y, btilde_y

  btilde_x   =(nxtilde -one     )/(xbar_max-xbar_min)
  btilde_xinv=(xbar_max-xbar_min)/(nxtilde -one     )
  atilde_x   =one-btilde_x*xbar_min
  btilde_y   =(nytilde -one     )/(ybar_max-ybar_min)
  btilde_yinv=(ybar_max-ybar_min)/(nytilde -one     )
  atilde_y   =one-btilde_y*ybar_min

! define xtilde0,ytilde0
  do j=1,nlat
     do i=1,nlon
        r_of_lat=pihalf+sign_pole*glats(i,j)
        clon=cos(glons(i,j)+rlambda0)
        slon=sin(glons(i,j)+rlambda0)
        xbar=r_of_lat*clon
        ybar=r_of_lat*slon
        xtilde0(i,j)=atilde_x+btilde_x*xbar
        ytilde0(i,j)=atilde_y+btilde_y*ybar
     end do
  end do

!  now get i0_tilde, j0_tilde, ip_tilde,jp_tilde
  ilast=ione ; jlast=ione
  istart0=nxtilde
  iend=ione
  iinc=-ione
  do j=1,nytilde
     itemp=istart0
     istart0=iend
     iend=itemp
     iinc=-iinc
     ybar=j
     do i=istart0,iend,iinc
        xbar=i
        call nearest_3(ilast,jlast,i0_tilde(i,j),j0_tilde(i,j), &
                       ip_tilde(i,j),jp_tilde(i,j),xbar,ybar,nlon,nlat,xtilde0,ytilde0)
     end do
  end do

!  now compute beta_ref, used in alpha = beta_ref + sign_pole*earth_lon, and alpha is 
!   angle between earth positive east and grid positive x.  This is needed
!   for rotation of u,v from earth to grid coordinate.
  allocate(beta_ref(nlon,nlat),cos_beta_ref(nlon,nlat),sin_beta_ref(nlon,nlat))
  epslon=r0_01*deg2rad
  do j=1,nlat
     do i=1,nlon-ione
        ip=i+ione
        r0=two*cos(glats(i,j ))/(one-sign_pole*sin(glats(i,j )))
        x0=r0 *cos(glons(i,j ))
        y0=r0 *sin(glons(i,j ))
        r1=two*cos(glats(ip,j))/(one-sign_pole*sin(glats(ip,j)))
        x1=r1 *cos(glons(ip,j))
        y1=r1 *sin(glons(ip,j))
        x2=r0 *cos(glons(i,j)+epslon)
        y2=r0 *sin(glons(i,j)+epslon)
        denom=one/sqrt(((x1-x0)**2+(y1-y0)**2)*((x2-x0)**2+(y2-y0)**2))
        cosalpha=((x2-x0)*(x1-x0)+(y2-y0)*(y1-y0))*denom
        sinalpha=((x2-x0)*(y1-y0)-(y2-y0)*(x1-x0))*denom
        beta_ref(i,j)=atan2(sinalpha,cosalpha)-sign_pole*glons(i,j)
        cos_beta_ref(i,j)=cos(beta_ref(i,j))
        sin_beta_ref(i,j)=sin(beta_ref(i,j))
     end do
     i=nlon
     ip=nlon-ione
     r0=two*cos(glats(i,j ))/(one-sign_pole*sin(glats(i,j )))
     x0=r0 *cos(glons(i,j ))
     y0=r0 *sin(glons(i,j ))
     r1=two*cos(glats(ip,j))/(one-sign_pole*sin(glats(ip,j)))
     x1=r1 *cos(glons(ip,j))
     y1=r1 *sin(glons(ip,j))
     x2=r0 *cos(glons(i,j)-epslon)
     y2=r0 *sin(glons(i,j)-epslon)
     denom=one/sqrt(((x1-x0)**2+(y1-y0)**2)*((x2-x0)**2+(y2-y0)**2))
     cosalpha=((x2-x0)*(x1-x0)+(y2-y0)*(y1-y0))*denom
     sinalpha=((x2-x0)*(y1-y0)-(y2-y0)*(x1-x0))*denom
     beta_ref(i,j)=atan2(sinalpha,cosalpha)-sign_pole*glons(i,j)
     cos_beta_ref(i,j)=cos(beta_ref(i,j))
     sin_beta_ref(i,j)=sin(beta_ref(i,j))
  end do
  beta_diff_max=-rbig
  beta_diff_max_gt_20=-rbig
  beta_diff_min= rbig
  beta_diff_rms=zero
  count_beta_diff=zero
  count_beta_diff_gt_20=zero

end subroutine init_general_transform

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  tll2xy --- convert earth lon-lat to x-y grid coordinates
!
! !INTERFACE:
!
  subroutine tll2xy(rlon,rlat,x,y,outside)

! !USES:

    use constants, only: ione,one
    implicit none

    real(r_kind),intent(in   ) :: rlon  ! earth longitude (radians)
    real(r_kind),intent(in   ) :: rlat  ! earth latitude  (radians)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: x  ! x-grid coordinate (grid units)
    real(r_kind),intent(  out) :: y  ! y-grid coordinate (grid units)
    logical     ,intent(  out) :: outside     ! .false., then point is inside x-y domain
                                              ! .true.,  then point is outside x-y domain

! !DESCRIPTION: to convert earth lon-lat to x-y grid units of a 
!           general regional rectangular domain.  Also, decide if
!           point is inside this domain.  As a result, there is
!           no restriction on type of horizontal coordinate for
!           a regional run, other than that it not have periodicity
!           or polar singularities.
!           This is done by first converting rlon, rlat to an
!           intermediate coordinate xtilde,ytilde, which has
!           precomputed pointers and constants for final conversion
!           to the desired x,y via 3 point inverse interpolation.
!           All of the information needed is derived from arrays
!           specifying earth latitude and longitude of every point
!           on the input grid.  Currently, the input x-y grid that
!           this is based on must be non-staggered.  This restriction
!           will eventually be lifted so we can run directly from
!           model grids that are staggered without first resorting
!           to interpolation of the guess to a non-staggered grid.
!
! !REVISION HISTORY:
!   2003-08-28  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-23  parrish - new routine
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-08-28
!
!EOP
!-------------------------------------------------------------------------

    real(r_kind) clon,slon,r_of_lat,xtilde,ytilde
    real(r_kind) dtilde,etilde
    real(r_kind) d1tilde,d2tilde,e1tilde,e2tilde,detinv
    integer(i_kind) itilde,jtilde
    integer(i_kind) i0,j0,ip,jp

!   first compute xtilde, ytilde

    clon=cos(rlon+rlambda0)
    slon=sin(rlon+rlambda0)
    r_of_lat=pihalf+sign_pole*rlat

    xtilde=atilde_x+btilde_x*r_of_lat*clon
    ytilde=atilde_y+btilde_y*r_of_lat*slon

!  next get interpolation information

    itilde=max(ione,min(nint(xtilde),nxtilde))
    jtilde=max(ione,min(nint(ytilde),nytilde))

    i0     =   i0_tilde(itilde,jtilde)
    j0     =   j0_tilde(itilde,jtilde)
    ip     =i0+ip_tilde(itilde,jtilde)
    jp     =j0+jp_tilde(itilde,jtilde)
    dtilde =xtilde-xtilde0(i0,j0)
    etilde =ytilde-ytilde0(i0,j0)
    d1tilde=(xtilde0(ip,j0)-xtilde0(i0,j0))*(ip-i0)
    d2tilde=(xtilde0(i0,jp)-xtilde0(i0,j0))*(jp-j0)
    e1tilde=(ytilde0(ip,j0)-ytilde0(i0,j0))*(ip-i0)
    e2tilde=(ytilde0(i0,jp)-ytilde0(i0,j0))*(jp-j0)
    detinv =one/(d1tilde*e2tilde-d2tilde*e1tilde)
    x = i0+detinv*(e2tilde*dtilde-d2tilde*etilde)
    y = j0+detinv*(d1tilde*etilde-e1tilde*dtilde)

    outside=x < rlon_min_dd .or. x > rlon_max_dd .or. &
            y < rlat_min_dd .or. y > rlat_max_dd

 end subroutine tll2xy

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  txy2ll ---  convert x-y grid units to earth lat-lon coordinates
!
! !INTERFACE:
!
  subroutine txy2ll(x,y,rlon,rlat)

! !USES:

    use constants, only: ione,one
    implicit none

! !INPUT PARAMETERS:

    real(r_kind),intent(in   ) :: x      ! x-grid coordinate (grid units)
    real(r_kind),intent(in   ) :: y      ! y_grid coordinate (grid units)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: rlon   ! earth longitude (radians)
    real(r_kind),intent(  out) :: rlat   ! earth latitude  (radians)

! !DESCRIPTION: to convert earth lon-lat to x-y grid units of a
!           general regional rectangular domain.  Also, decide if
!           point is inside this domain.  As a result, there is
!           no restriction on type of horizontal coordinate for
!           a regional run, other than that it not have periodicity
!           or polar singularities.
!           This is done by first converting rlon, rlat to an
!           intermediate coordinate xtilde,ytilde, which has
!           precomputed pointers and constants for final conversion
!           to the desired x,y via 3 point inverse interpolation.
!           All of the information needed is derived from arrays
!           specifying earth latitude and longitude of every point
!           on the input grid.  Currently, the input x-y grid that
!           this is based on must be non-staggered.  This restriction
!           will eventually be lifted so we can run directly from
!           model grids that are staggered without first resorting
!           to interpolation of the guess to a non-staggered grid.
!
! !REVISION HISTORY:
!   2003-08-28  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-20  todling, fixed description
!   2004-07-23  parrish - new routine
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-08-28
!
!EOP
!-------------------------------------------------------------------------

    real(r_kind) r_of_lat,xtilde,ytilde
    real(r_kind) dtilde,etilde,xbar,ybar
    real(r_kind) d1tilde,d2tilde,e1tilde,e2tilde
    integer(i_kind) i0,j0,ip,jp

    i0=nint(x)
    j0=nint(y)
    i0=max(ione,min(i0,nlon))
    j0=max(ione,min(j0,nlat))
    ip=i0+nint(sign(one,x-i0))
    jp=j0+nint(sign(one,y-j0))
    if(ip<ione) then
       i0=2_i_kind
       ip=ione
    end if
    if(jp<ione) then
       j0=2_i_kind
       jp=ione
    end if
    if(ip>nlon) then
       i0=nlon-ione
       ip=nlon
    end if
    if(jp>nlat) then
       j0=nlat-ione
       jp=nlat
    end if
    d1tilde=(xtilde0(ip,j0)-xtilde0(i0,j0))*(ip-i0)
    d2tilde=(xtilde0(i0,jp)-xtilde0(i0,j0))*(jp-j0)
    e1tilde=(ytilde0(ip,j0)-ytilde0(i0,j0))*(ip-i0)
    e2tilde=(ytilde0(i0,jp)-ytilde0(i0,j0))*(jp-j0)
    dtilde =d1tilde*(x-i0) +d2tilde*(y-j0)
    etilde =e1tilde*(x-i0) +e2tilde*(y-j0)
    xtilde =dtilde         +xtilde0(i0,j0)
    ytilde =etilde         +ytilde0(i0,j0)

    xbar=(xtilde-atilde_x)*btilde_xinv
    ybar=(ytilde-atilde_y)*btilde_yinv
    r_of_lat=sqrt(xbar**2+ybar**2)
    rlat=(r_of_lat-pihalf)*sign_pole
    rlon=atan2(ybar,xbar)-rlambda0

 end subroutine txy2ll

 subroutine nearest_3(ilast,jlast,i0,j0,ip,jp,x,y,nx0,ny0,x0,y0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nearest_3
!   prgmmr:
!
! abstract: find closest 3 points to (x,y) on grid defined by x0,y0
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ilast,jlast
!    nx0,ny0
!    x,y
!    x0,y0
!
!   output argument list:
!    ilast,jlast
!    i0,j0
!    ip,jp
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: izero,ione
  implicit none

  integer(i_kind),intent(inout) :: ilast,jlast
  integer(i_kind),intent(  out) :: i0,j0
  integer(i_byte),intent(  out) :: ip,jp
  integer(i_kind),intent(in   ) :: nx0,ny0
  real(r_kind)   ,intent(in   ) :: x,y
  real(r_kind)   ,intent(in   ) :: x0(nx0,ny0),y0(nx0,ny0)
 
  real(r_kind) dista,distb,dist2,dist2min
  integer(i_kind) i,inext,j,jnext

  do
     i0=ilast
     j0=jlast
     dist2min=huge(dist2min)
     inext=izero
     jnext=izero
     do j=max(j0-ione,ione),min(j0+ione,ny0)
        do i=max(i0-ione,ione),min(i0+ione,nx0)
           dist2=(x-x0(i,j))**2+(y-y0(i,j))**2
           if(dist2<dist2min) then
              dist2min=dist2
              inext=i
              jnext=j
           end if
        end do
     end do
     if(inext==i0.and.jnext==j0) exit
     ilast=inext
     jlast=jnext
  end do

!  now find which way to go in x for second point

  ip=izero
  if(i0==nx0)  ip=-ione
  if(i0==ione) ip=ione
  if(ip==izero) then
     dista=(x-x0(i0-ione,j0))**2+(y-y0(i0-ione,j0))**2
     distb=(x-x0(i0+ione,j0))**2+(y-y0(i0+ione,j0))**2
     if(distb<dista) then
        ip=ione
     else
        ip=-ione
     end if
  end if

!  repeat for y for 3rd point

  jp=izero
  if(j0==ny0  ) jp=-ione
  if(j0==ione ) jp=ione
  if(jp==izero) then
     dista=(x-x0(i0,j0-ione))**2+(y-y0(i0,j0-ione))**2
     distb=(x-x0(i0,j0+ione))**2+(y-y0(i0,j0+ione))**2
     if(distb<dista) then
        jp=ione
     else
        jp=-ione
     end if
  end if

  ilast=i0
  jlast=j0
    
 end subroutine nearest_3

 subroutine get_xytilde_domain(nx0,ny0,rlons0,rlats0, &
                                  nx,ny,xminout,xmaxout,yminout,ymaxout)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_xytilde_domain
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    nx0,ny0
!    rlons0,rlats0
!
!   output argument list:
!    nx,ny
!    xminout,xmaxout,yminout,ymaxout
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   use constants, only: ione,one, deg2rad,half,zero
!  define parameters for xy domain which optimally overlays input grid

  implicit none
  integer(i_kind),intent(in   ) :: nx0,ny0
  real(r_kind)   ,intent(in   ) :: rlons0(nx0,ny0),rlats0(nx0,ny0)

  integer(i_kind),intent(  out) :: nx,ny
  real(r_kind)   ,intent(  out) :: xminout,xmaxout,yminout,ymaxout

  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r37=37.0_r_kind

  real(r_kind) area,areamax,areamin,extra,rlats0max,rlats0min,testlambda
  real(r_kind) xthis,ythis
  integer(i_kind) i,ip1,j,jp1,m

  real(r_kind) coslon0(nx0,ny0),sinlon0(nx0,ny0)
  real(r_kind) coslat0(nx0,ny0),sinlat0(nx0,ny0)
  real(r_kind) count,delbar
  real(r_kind) dx,dy,disti,distj,distmin,distmax
  real(r_kind) xmin,xmax,ymin,ymax

!  get range of lats for input grid

  rlats0max=maxval(rlats0) ; rlats0min=minval(rlats0)

!   assign hemisphere ( parameter sign_pole )

  if(rlats0min>-r37*deg2rad) sign_pole=-one   !  northern hemisphere xy domain
  if(rlats0max< r37*deg2rad) sign_pole= one   !  southern hemisphere xy domain

!   get optimum rotation angle rlambda0

  areamin= huge(areamin)
  areamax=-huge(areamax)
  do m=0,359
     testlambda=m*deg2rad
     xmax=-huge(xmax)
     xmin= huge(xmin)
     ymax=-huge(ymax)
     ymin= huge(ymin)
     do j=1,ny0,ny0-ione
        do i=1,nx0
           xthis=(pihalf+sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(pihalf+sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     do j=1,ny0
        do i=1,nx0,nx0-ione
           xthis=(pihalf+sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(pihalf+sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     area=(xmax-xmin)*(ymax-ymin)
     areamax=max(area,areamax)
     if(area<areamin) then
        areamin =area
        rlambda0=testlambda
        xmaxout =xmax
        xminout =xmin
        ymaxout =ymax
        yminout =ymin
     end if
  end do


!   now determine resolution of input grid and choose nx,ny of xy grid accordingly
!                 (currently hard-wired at 1/2 the average input grid increment)

  do j=1,ny0
     do i=1,nx0
        coslon0(i,j)=cos(one*rlons0(i,j)) ; sinlon0(i,j)=sin(one*rlons0(i,j))
        coslat0(i,j)=cos(one*rlats0(i,j)) ; sinlat0(i,j)=sin(one*rlats0(i,j))
     end do
  end do

  delbar=zero
  count =zero
  do j=1,ny0-ione
     jp1=j+ione
     do i=1,nx0-ione
        ip1=i+ione
        disti=acos(sinlat0(i,j)*sinlat0(ip1,j)+coslat0(i,j)*coslat0(ip1,j)* &
                  (sinlon0(i,j)*sinlon0(ip1,j)+coslon0(i,j)*coslon0(ip1,j)))
        distj=acos(sinlat0(i,j)*sinlat0(i,jp1)+coslat0(i,j)*coslat0(i,jp1)* &
                  (sinlon0(i,j)*sinlon0(i,jp1)+coslon0(i,j)*coslon0(i,jp1)))
        distmax=max(disti,distj)
        distmin=min(disti,distj)
        delbar=delbar+distmax
        count=count+one
     end do
  end do
  delbar=delbar/count
  dx=half*delbar
  dy=dx

!   add extra space to computational grid to push any boundary problems away from
!     area of interest

  extra=r10*dx
  xmaxout=xmaxout+extra
  xminout=xminout-extra
  ymaxout=ymaxout+extra
  yminout=yminout-extra
  nx=ione+(xmaxout-xminout)/dx
  ny=ione+(ymaxout-yminout)/dy
 
 end subroutine get_xytilde_domain

 subroutine half_nmm_grid2a(gin,nx,ny,gout,igtype)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    half_nmm_grid2a same as half_nmm_grid2, but output not reorganized
!   prgmmr: parrish         org: w/emc2               date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered E grid used by the wrf nmm.
!           This is done by keeping every other row of the original E grid.  If this 
!           is a mass variable (igtype=1), then no interpolation is required.  If this
!           is a wind variable (igtype=2), then interpolation is necessary.  This procedure
!           is necessary because the gsi is not yet able to work with anything other than
!           unstaggered grids.  This solution introduces greater interpolation error
!           compared to the option fill_nmm_grid2, but has the advantage of 4 times fewer
!           grid points compared to the output of fill_nmm__grid2.  This routine will be
!           eliminated when the gsi has the capability to work directly with staggered grids.
!
! program history log:
!   2004-06-22  parrish, document
!   2005-03-03  treadon - add implicit none
!
!   input argument list:
!     gin      - input staggered E grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!     igtype   - =1, then (1,1) on staggered grid is at corner of grid (mass point for nmm)
!              - =2, then (1,1) is staggered (wind point for nmm, see illustration below)

!                   igtype=1:
!
!
!
!       ^   3             x     x     x     x
!       |
!       y   2                x     x     x     x
!
!           1             x     x     x     x
!
!                         1     2     3
!
!                           x -->

!                   igtype=2:
!
!
!
!       ^   3             x     x     x     x
!       |
!       y   2          x     x     x     x
!
!           1             x     x     x     x
!
!                         1     2     3
!
!                           x -->
!
!   output argument list
!     gout     - output unstaggered half grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use constants, only: izero,ione,quarter
  implicit none
 
  integer(i_kind),intent(in   ) :: nx,ny,igtype
  real(r_single) ,intent(in   ) :: gin(nx,ny)
  real(r_kind)   ,intent(  out) :: gout(nx,*)

  integer(i_kind) i,i0,im,j,jj,jm,jp

  if(igtype==ione) then
     jj=izero
     do j=1,ny,2
        jj=jj+ione
        do i=1,nx
           gout(i,jj)=gin(i,j)
        end do
     end do
  else
     jj=izero
     do j=1,ny,2
        jj=jj+ione
        jp=j+ione ; if(jp>ny)   jp=j-ione
        jm=j-ione ; if(jm<ione) jm=j+ione
        do i=1,nx
           im=i-ione ; if(im<ione) im=i
           i0=i      ; if(i==nx)   i0=im
           gout(i,jj)=quarter*(gin(im,j)+gin(i0,j)+gin(i,jp)+gin(i,jm))
        end do
     end do
  end if

 end subroutine half_nmm_grid2a

 subroutine fill_nmm_grid2a3(gin,nx,ny,gout)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_nmm_grid2a3
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    nx,ny
!    gin
!
!   output argument list:
!    gout
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: ione
  implicit none

  integer(i_kind),intent(in   ) :: nx,ny
  real(r_kind)   ,intent(in   ) :: gin(nx,ny)
  real(r_kind)   ,intent(  out) :: gout(2*nx-ione,ny)
 
  integer(i_kind) i,j
  integer(i_kind) i1a(2*nx-ione),i2a(2*nx-ione)
  integer(i_kind) i3a(2*nx-ione),i4a(2*nx-ione)
  real(r_kind) r1a(2*nx-ione),r2a(2*nx-ione)
  real(r_kind) r3a(2*nx-ione),r4a(2*nx-ione)
  real(r_kind) x,x1,x2,x3,x4

!  first transfer all staggered points to appropriate
!   points on filled output grid

  do j=1,ny,2
     do i=1,nx
        gout(2*i-ione,j)=gin(i,j)
     end do
  end do
  do j=2,ny,2
     do i=1,nx-ione
        gout(2*i,j)=gin(i,j)
     end do
  end do

!   compute all interpolation constants for even x points on odd y rows

  i=2_i_kind
  i1a(i)=i-ione ; i2a(i)=i+ione ; i3a(i)=i+3_i_kind ; i4a(i)=i+5_i_kind
  x=i           ; x1=i1a(i)     ; x2=i2a(i)         ; x3=i3a(i)         ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  do i=4,2*nx-4_i_kind,2
     i1a(i)=i-3_i_kind ; i2a(i)=i-ione ; i3a(i)=i+ione ; i4a(i)=i+3_i_kind
     x=i               ; x1=i1a(i)     ; x2=i2a(i)     ; x3=i3a(i)         ; x4=i4a(i)
     r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
     r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
     r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
     r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )
  end do

  i=2*nx-2_i_kind
  i1a(i)=i-5_i_kind ; i2a(i)=i-3_i_kind ; i3a(i)=i-ione ; i4a(i)=i+ione
  x=i               ; x1=i1a(i)         ; x2=i2a(i)     ; x3=i3a(i)     ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

!   now get all interpolation constants for odd x points on even y rows

  i=ione
  i1a(i)=i+ione ; i2a(i)=i+3_i_kind ; i3a(i)=i+5_i_kind ; i4a(i)=i+7_i_kind
  x=i           ; x1=i1a(i)         ; x2=i2a(i)         ; x3=i3a(i)     ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  i=3_i_kind
  i1a(i)=i-ione ; i2a(i)=i+ione ; i3a(i)=i+3_i_kind ; i4a(i)=i+5_i_kind
  x=i           ; x1=i1a(i)     ; x2=i2a(i)         ; x3=i3a(i)         ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  do i=5,2*nx-5_i_kind,2
     i1a(i)=i-3_i_kind ; i2a(i)=i-ione ; i3a(i)=i+ione ; i4a(i)=i+3_i_kind
     x=i               ; x1=i1a(i)     ; x2=i2a(i)     ; x3=i3a(i)         ; x4=i4a(i)
     r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
     r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
     r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
     r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )
  end do

  i=2*nx-3_i_kind
  i1a(i)=i-5_i_kind ; i2a(i)=i-3_i_kind ; i3a(i)=i-ione ; i4a(i)=i+ione
  x=i               ; x1=i1a(i)         ; x2=i2a(i)     ; x3=i3a(i)     ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  i=2*nx-ione
  i1a(i)=i-7_i_kind ; i2a(i)=i-5_i_kind ; i3a(i)=i-3_i_kind ; i4a(i)=i-ione
  x=i               ; x1=i1a(i)         ; x2=i2a(i)         ; x3=i3a(i) ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  do j=1,ny,2
     do i=2,2*nx-2_i_kind,2
        gout(i,j)=r1a(i)*gout(i1a(i),j)+r2a(i)*gout(i2a(i),j)+ &
                  r3a(i)*gout(i3a(i),j)+r4a(i)*gout(i4a(i),j)
     end do
  end do
  do j=2,ny,2
     do i=1,2*nx-1,2
        gout(i,j)=r1a(i)*gout(i1a(i),j)+r2a(i)*gout(i2a(i),j)+ &
                  r3a(i)*gout(i3a(i),j)+r4a(i)*gout(i4a(i),j)
     end do
  end do

 end subroutine fill_nmm_grid2a3

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rotate_wind_ll2xy ---  Rotate earth vector wind
!
! !INTERFACE:
!
  subroutine rotate_wind_ll2xy(u0,v0,u,v,rlon0,x,y)

! !USES:

    use constants, only: ione,one,two,pi,rad2deg,one_tenth
    implicit none

! !INPUT PARAMETERS:

    real(r_kind),intent(in   ) :: u0,v0        ! earth wind component
    real(r_kind),intent(in   ) :: rlon0        ! earth   lon (radians)
    real(r_kind),intent(in   ) :: x,y          ! local x,y coordinate (grid units)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: u,v          ! rotated coordinate of winds

! !DESCRIPTION: to convert earth vector wind components to corresponding
!           local x,y coordinate
!
! !REVISION HISTORY:
!   2003-09-30  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------

  real(r_kind) beta,delx,delxp,dely,delyp
  real(r_kind) beta_old,two_pi,sin_beta,cos_beta,thisdiff
  integer(i_kind) ix,iy,k

!  interpolate departure from longitude part of angle between earth positive east and local positive x

  ix=x
  iy=y
  ix=max(ione,min(ix,nlon-ione))
  iy=max(ione,min(iy,nlat-ione))
  delx=x-ix
  dely=y-iy
  delxp=one-delx
  delyp=one-dely
  beta_old=    beta_ref(ix  ,iy     )*delxp*delyp+    beta_ref(ix+ione,iy     )*delx *delyp+ &
               beta_ref(ix  ,iy+ione)*delxp*dely +    beta_ref(ix+ione,iy+ione)*delx *dely
  cos_beta=cos_beta_ref(ix  ,iy     )*delxp*delyp+cos_beta_ref(ix+ione,iy     )*delx *delyp+ &
           cos_beta_ref(ix  ,iy+ione)*delxp*dely +cos_beta_ref(ix+ione,iy+ione)*delx *dely
  sin_beta=sin_beta_ref(ix  ,iy     )*delxp*delyp+sin_beta_ref(ix+ione,iy     )*delx *delyp+ &
           sin_beta_ref(ix  ,iy+ione)*delxp*dely +sin_beta_ref(ix+ione,iy+ione)*delx *dely
  beta=atan2(sin_beta,cos_beta)
  thisdiff=huge(thisdiff)
  two_pi=two*pi
  do k=-6,6
     thisdiff=min(abs(beta-beta_old+k*two_pi),thisdiff)
  end do
  if(thisdiff*rad2deg>one_tenth) then
     count_beta_diff_gt_20=count_beta_diff_gt_20 + one
     beta_diff_max_gt_20=max(beta_diff_max_gt_20,thisdiff)
  else
     beta_diff_max=max(beta_diff_max,thisdiff)
     beta_diff_min=min(beta_diff_min,thisdiff)
     beta_diff_rms=beta_diff_rms+thisdiff**2
     count_beta_diff=count_beta_diff+one
  end if

!  now rotate;

  u= u0*cos(beta+rlon0*sign_pole)+v0*sin(beta+rlon0*sign_pole)
  v=-u0*sin(beta+rlon0*sign_pole)+v0*cos(beta+rlon0*sign_pole)

 end subroutine rotate_wind_ll2xy

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rotate_wind_xy2ll ---  Unrotate earth vector wind
!
! !INTERFACE:
!
  subroutine rotate_wind_xy2ll(u,v,u0,v0,rlon0,x,y)

! !USES:

    use constants, only: ione,one
    implicit none

! !INPUT PARAMETERS:

    real(r_kind),intent(in   ) :: u,v         ! rotated coordinate winds
    real(r_kind),intent(in   ) :: rlon0       ! earth   lon     (radians)
    real(r_kind),intent(in   ) :: x,y         ! rotated lon/lat (radians)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: u0,v0       ! earth winds

! !DESCRIPTION: rotate u,v in local x,y coordinate to u0,v0 in earth 
!           lat, lon coordinate
!
! !REVISION HISTORY:
!   2003-09-30  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-20  todling, fixed description
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------
  real(r_kind) beta,delx,delxp,dely,delyp
  real(r_kind) sin_beta,cos_beta
  integer(i_kind) ix,iy

!  interpolate departure from longitude part of angle between earth 
!  positive east and local positive x

  ix=x
  iy=y
  ix=max(ione,min(ix,nlon-ione))
  iy=max(ione,min(iy,nlat-ione))
  delx=x-ix
  dely=y-iy
  delxp=one-delx
  delyp=one-dely
  cos_beta=cos_beta_ref(ix  ,iy     )*delxp*delyp+cos_beta_ref(ix+ione,iy     )*delx *delyp+ &
           cos_beta_ref(ix  ,iy+ione)*delxp*dely +cos_beta_ref(ix+ione,iy+ione)*delx *dely
  sin_beta=sin_beta_ref(ix  ,iy     )*delxp*delyp+sin_beta_ref(ix+ione,iy     )*delx *delyp+ &
           sin_beta_ref(ix  ,iy+ione)*delxp*dely +sin_beta_ref(ix+ione,iy+ione)*delx *dely
  beta=atan2(sin_beta,cos_beta)

!  now rotate;

  u0= u*cos(beta+rlon0*sign_pole)-v*sin(beta+rlon0*sign_pole)
  v0= u*sin(beta+rlon0*sign_pole)+v*cos(beta+rlon0*sign_pole)

 end subroutine rotate_wind_xy2ll

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  load_grid --- strip off south/north latitude rows
!
! !INTERFACE:
!
 subroutine load_grid(grid_in,grid_out)

! !USES:

   use constants, only: ione
   implicit none

! !INPUT PARAMETERS:

   real(r_kind),dimension(max(iglobal,itotsub)),intent(in   ) :: grid_in  ! input grid
   real(r_kind),dimension(nlon,nlat-2_i_kind)  ,intent(  out) :: grid_out ! output grid

! !DESCRIPTION: This routine prepares grids for use in splib
!               grid to spectral tranforms.  This preparation
!               entails to two steps
!                  1) reorder indexing of the latitude direction.
!                     The GSI ordering is south to north.  The 
!                     ordering assumed in splib routines is north
!                     to south.
!                  2) The global GSI adds two latitude rows, one
!                     for each pole.  These latitude rows are not
!                     needed in the grid to spectral transforms of
!                     splib.  The code below strips off these
!                     "pole rows"
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
   integer(i_kind) i,j,k,nlatm1,jj
   real(r_kind),dimension(nlon,nlat):: grid

!  Transfer input grid from 1d to 2d local array.  As loading
!  local array, reverse direction of latitude index.  Coming
!  into the routine the order is south --> north.  On exit
!  the order is north --> south
   do k=1,iglobal
      i=nlat-ltosi(k)+ione
      j=ltosj(k)
      grid(j,i)=grid_in(k)
   end do
   
!  Transfer contents of local array to output array.
   nlatm1=nlat-ione
   do j=2,nlatm1
      jj=j-ione
      do i=1,nlon
         grid_out(i,jj)=grid(i,j)
      end do
   end do
   
   return
 end subroutine load_grid

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  fill_ns --- add southern/northern latitude rows
!
! !INTERFACE:
!
 subroutine fill_ns(grid_in,grid_out)

! !USES:

   use constants, only: ione,zero,one
   implicit none

! !INPUT PARAMETERS:

   real(r_kind),dimension(nlon,nlat-2_i_kind),intent(in   ) :: grid_in  ! input grid
   real(r_kind),dimension(itotsub)           ,intent(  out) :: grid_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the input grid.  The southern row contains
!               the longitudinal mean of the adjacent latitude row.
!               The northern row contains the longitudinal mean of
!               the adjacent northern row.
!               
!               The added rows correpsond to the south and north poles.
!
!               In addition to adding latitude rows corresponding to the
!               south and north poles, the routine reorder the output 
!               array so that it is a one-dimensional array read in
!               an order consisten with that assumed for total domain
!               gsi grids.
!
!               The assumed order for the input grid is longitude as
!               the first dimension with array index increasing from 
!               east to west.  The second dimension is latitude with
!               the index increasing from north to south.  This ordering
!               differs from that used in the GSI.  
!
!               The GSI ordering is latitude first with the index 
!               increasing from south to north.  The second dimension is
!               longitude with the index increasing from east to west.
!
!               Thus, the code below also rearranges the indexing and
!               order of the dimensions to make the output grid 
!               consistent with that which is expected in the rest of
!               gsi.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k,jj,nlatm2
   real(r_kind) rnlon,sumn,sums
   real(r_kind),dimension(nlon,nlat):: grid

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,nlat-ione
      jj=nlat-j
      do i=1,nlon
         grid(i,j)=grid_in(i,jj)
      end do
   end do
   
!  Compute mean along southern and northern latitudes
   sumn=zero
   sums=zero
   nlatm2=nlat-2_i_kind
   do i=1,nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlatm2)
   end do
   rnlon=one/float(nlon)
   sumn=sumn*rnlon
   sums=sums*rnlon

!  Load means into local work array
   do i=1,nlon
      grid(i,1)   =sums
      grid(i,nlat)=sumn
   end do
   
!  Transfer local work array to output grid
   do k=1,itotsub
      i=ltosi_s(k)
      j=ltosj_s(k)
      grid_out(k)=grid(j,i)
   end do
   
   return
 end subroutine fill_ns

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  filluv_ns --- add southern/northern latitude rows
!
! !INTERFACE:
!
 subroutine filluv_ns(gridu_in,gridv_in,gridu_out,gridv_out)

! !USES:

   use constants, only: ione,zero
   implicit none

! !INPUT PARAMETERS:

   real(r_kind),dimension(nlon,nlat-2_i_kind),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real(r_kind),dimension(itotsub)           ,intent(  out) :: gridu_out,gridv_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the input grid.  The southern row contains
!               the longitudinal mean of the adjacent latitude row.
!               The northern row contains the longitudinal mean of
!               the adjacent northern row.
!               
!               The added rows correpsond to the south and north poles.
!
!               In addition to adding latitude rows corresponding to the
!               south and north poles, the routine reorder the output 
!               array so that it is a one-dimensional array read in
!               an order consisten with that assumed for total domain
!               gsi grids.
!
!               The assumed order for the input grid is longitude as
!               the first dimension with array index increasing from 
!               east to west.  The second dimension is latitude with
!               the index increasing from north to south.  This ordering
!               differs from that used in the GSI.  
!
!               The GSI ordering is latitude first with the index 
!               increasing from south to north.  The second dimension is
!               longitude with the index increasing from east to west.
!
!               Thus, the code below also rearranges the indexing and
!               order of the dimensions to make the output grid 
!               consistent with that which is expected in the rest of
!               gsi.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k,jj
   real(r_kind) polnu,polnv,polsu,polsv
   real(r_kind),dimension(nlon,nlat):: grid,grid2

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,nlat-ione
      jj=nlat-j
      do i=1,nlon
         grid(i,j)=gridu_in(i,jj)
         grid2(i,j)=gridv_in(i,jj)
      end do
   end do
   
!  Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   do i=1,nlon
      polnu=polnu+grid(i,nlat-ione)*coslon(i)-grid2(i,nlat-ione)*sinlon(i)
      polnv=polnv+grid(i,nlat-ione)*sinlon(i)+grid2(i,nlat-ione)*coslon(i)
      polsu=polsu+grid(i,2        )*coslon(i)+grid2(i,2        )*sinlon(i)
      polsv=polsv+grid(i,2        )*sinlon(i)-grid2(i,2        )*coslon(i)
   end do
   polnu=polnu/float(nlon)
   polnv=polnv/float(nlon)
   polsu=polsu/float(nlon)
   polsv=polsv/float(nlon)
   do i=1,nlon
      grid (i,nlat)= polnu*coslon(i)+polnv*sinlon(i)
      grid2(i,nlat)=-polnu*sinlon(i)+polnv*coslon(i)
      grid (i,1   )= polsu*coslon(i)+polsv*sinlon(i)
      grid2(i,1   )= polsu*sinlon(i)-polsv*coslon(i)
   end do

!  Transfer local work array to output grid
   do k=1,itotsub
      i=ltosi_s(k)
      j=ltosj_s(k)
      gridu_out(k)=grid(j,i)
      gridv_out(k)=grid2(j,i)
   end do
   
   return
 end subroutine filluv_ns


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  get_ij --- get (i,j) grid indices and interpolation weights
!
! !INTERFACE:
!
 subroutine get_ij(mm1,obs_lat,obs_lon,jgrd,wgrd,jjlat,jjlon)

! !USES:

   use constants, only: izero,ione,one
   implicit none

! !INPUT PARAMETERS:

   integer(i_kind)             ,intent(in   ) ::  mm1
   integer(i_kind),dimension(4),intent(  out) :: jgrd
   integer(i_kind),optional    ,intent(  out) :: jjlat,jjlon

   real(r_kind)                ,intent(in   ) :: obs_lat,obs_lon
   real(r_kind),dimension(4)   ,intent(  out) :: wgrd

   integer(i_kind):: jlat,jlon
   real(r_kind):: dx,dy,dx1,dy1

! !DESCRIPTION: This routine returns the sub-domain grid relative 
!               i,j index of a given observation (lat,lon).  The
!               routine also returns weights needed for bilinear
!               from the four surrounding analysis grid points to
!               the observation location.
!
! !REVISION HISTORY:
!   2004-12-23  treadon
!   2006-01-06  treadon - add optional arguments jjlat,jjlon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------

!  Set (i,j) indices of guess gridpoint that bound obs location
   jlat = obs_lat
   jlon = obs_lon

!  Compute weights for bilinear interpolation
   dy  = obs_lat-jlat
   dx  = obs_lon-jlon
   dx1 = one-dx
   dy1 = one-dy

!  Bound lat and lon indices to fall within analysis grid limits   
   jlat = min(max(ione ,jlat),nlat)
   jlon = min(max(izero,jlon),nlon)

!  Handle special case of e/w periodicity
   if (jstart(mm1)==ione .and. jlon==nlon) jlon=izero
   if (jstart(mm1)+jlon1(mm1)==nlon+ione .and. jlon==izero) jlon=nlon

!  Convert global (i,j) indices to sub-domain specific (i,j) indices
   jlat=jlat-istart(mm1)+2_i_kind
   jlon=jlon-jstart(mm1)+2_i_kind

   jgrd(1)=jlat+(jlon-ione)*lat2
   jgrd(2)=jgrd(1)+ione
   jgrd(3)=jgrd(1)+lat2
   jgrd(4)=jgrd(3)+ione

   wgrd(1)=dx1*dy1
   wgrd(2)=dx1*dy
   wgrd(3)=dx *dy1
   wgrd(4)=dx *dy

   if (present(jjlat)) jjlat=jlat
   if (present(jjlon)) jjlon=jlon

   return
 end subroutine get_ij

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  get_ijk --- get (i,j,k) grid indices and interpolation weights
!
! !INTERFACE:
!
 subroutine get_ijk(mm1,obs_lat,obs_lon,obs_sig,jgrd,wgrd)

! !USES:

   use constants, only: izero,ione,one
   implicit none

! !INPUT PARAMETERS:

   integer(i_kind)             ,intent(in   ) ::  mm1
   integer(i_kind),dimension(8),intent(  out) :: jgrd

   real(r_kind)                ,intent(in   ) :: obs_lat,obs_lon,obs_sig
   real(r_kind)   ,dimension(8),intent(  out) :: wgrd

   integer(i_kind):: jlat,jlon,jsig,latlon11_l
   real(r_kind) :: dx,dy,dx1,dy1,ds,ds1

! !DESCRIPTION: This routine returns the sub-domain grid relative
!               i,j,k index of a given observation (lat,lon,sig).  
!               The routine also returns weights needed for bilinear
!               from the eight surrounding analysis grid points to
!               the observation location
!
! !REVISION HISTORY:
!   2004-12-23  treadon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   real(r_kind) obs_s


!  Special handling for vertical coordinate
   obs_s = obs_sig
   if (obs_s < one) obs_s = one

!  Set (i,j,k) indices of guess gridpoint that bound obs location
   jlat = obs_lat
   jlon = obs_lon
   jsig = obs_s

!  Compute weights for bilinear interpolation
   dy  = obs_lat-jlat
   dx  = obs_lon-jlon
   ds  = obs_s-jsig

   dx1 = one-dx
   dy1 = one-dy
   ds1 = one-ds

!  Bound lat and lon indices to fall within analysis grid limits   
   jlat = min(max(ione ,jlat),nlat)
   jlon = min(max(izero,jlon),nlon)

!  Handle special case of e/w periodicity
   if (jstart(mm1)==ione .and. jlon==nlon) jlon=izero
   if (jstart(mm1)+jlon1(mm1)==nlon+ione .and. jlon==izero) jlon=nlon

!  Convert global (i,j) indices to sub-domain specific (i,j) indices
   jlat=jlat-istart(mm1)+2_i_kind
   jlon=jlon-jstart(mm1)+2_i_kind

!  Set number of points on horizontal layer
   latlon11_l = latlon11
   if(jsig==nsig) latlon11_l=izero
   jgrd(1)=jlat+(jlon-ione)*lat2+(jsig-ione)*latlon11
   jgrd(2)=jgrd(1)+ione
   jgrd(3)=jgrd(1)+lat2
   jgrd(4)=jgrd(3)+ione
   jgrd(5)=jgrd(1)+latlon11_l
   jgrd(6)=jgrd(5)+ione
   jgrd(7)=jgrd(5)+lat2
   jgrd(8)=jgrd(7)+ione

   wgrd(1)=dx1*dy1*ds1
   wgrd(2)=dx1*dy *ds1
   wgrd(3)=dx *dy1*ds1
   wgrd(4)=dx *dy *ds1
   wgrd(5)=dx1*dy1*ds
   wgrd(6)=dx1*dy *ds
   wgrd(7)=dx *dy1*ds
   wgrd(8)=dx *dy *ds

   return
 end subroutine get_ijk


 subroutine check_rotate_wind(string)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    check_rotate_wind
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    string
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block 

   use constants, only: zero,one,rad2deg
   implicit none

   character(len=*),intent(in   ) :: string

   if(count_beta_diff>zero.or.count_beta_diff_gt_20>zero) then
      beta_diff_rms=sqrt(beta_diff_rms/(max(one,count_beta_diff)))
      write(6,*)'CHECK_ROTATE_WIND:  called from routine ',trim(string)
      write(6,100) beta_diff_max*rad2deg, beta_diff_min*rad2deg, beta_diff_rms*rad2deg
      write(6,110) count_beta_diff, count_beta_diff_gt_20
      write(6,115) beta_diff_max_gt_20*rad2deg
100   format('   beta_diff_mass,min,rms(deg)          = ',3(g18.12,2x))
110   format('   count_beta_diff,count_beta_diff_gt_20= ',2(g18.12,2x))
115   format('   beta_diff_max_gt_20(deg)             = ',g18.12)
   end if
 end subroutine check_rotate_wind

end module gridmod
