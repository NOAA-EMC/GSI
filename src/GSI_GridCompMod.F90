#include "MAPL_ErrLog.h" 
!#define PRINT_STATES
!#define SFCverbose
!#define UPAverbose
!#define VERBOSE
!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! !MODULE: GSI_GridCompMod -- Implements ESMF wrapper to invoke GSI
!
! !DESCRIPTION: 
! {\tt GSI\_GridComp} is the ESMF Gridded Component module for NCEP's GSI
! It defines the ESMF Initialize/Run/Finalize methods as well as ancillary routines.
! {\tt GSI\_GridComp} acts as a wrapper around NCEP's GSI. The GSI internal fields
! are defined from the component's import state which are, on import,
! already decomposed as required by the GSI.
! An important function of the {\tt GSI\_GridComp} is to take the import fields
! and use them to populate the GSI internal fields. Since these latter are defined
! on a different grid and with different units some additional manipulations
! are required. These require the need to access the GSI's global data segments
! guess_grids, grid_mod, and mpimod.
!

!
! !INTERFACE:

   module GSI_GridCompMod

! !USES:

   use ESMF_Mod              ! ESMF
   use MAPL_Mod              ! MAPL Generic
   use gsimod                ! GSI original interface
   use mpeu_util, only: StrTemplate         ! grads style templates
   use constants, only : grav
   use general_specmod,only : general_init_spec_vars
   use general_specmod,only : general_destroy_spec_vars
   use m_tick, only: tick
   use obsmod, only: lobserver

   use gsi_bundlemod, only : GSI_BundleGetPointer
   use gsi_bundlemod, only : GSI_Bundle
   use gsi_bundlemod, only : GSI_BundlePrint

! Access to GSI's global data segments

   ! guess fields and related variables

   	!>>> sfc_grids <<< all of them
   use guess_grids, only: isli,         & ! snow/land/ice mask
                          fact10,       & ! 10 meter wind factor
                          sfct,         & ! guess skin temp
                          dsfct,        & ! delta guess skin temp
                          sno,          & ! snow-ice mask
                          veg_type,     & ! vegetation type  
                          veg_frac,     & ! vegetation frac
                          soil_type,    & ! soil type
                          soil_temp,    & ! soil temperature
                          soil_moi,     & ! soil moisture
                          sfc_rough       ! surface roughness

   	!>>> ges_grids <<< 
   use guess_grids, only: ges_z,        & ! topography
                          ges_ps,       & ! surface pressure
                          ges_u,        & ! zonal wind
                          ges_v,        & ! meridional wind
                          ges_vor,      & ! vorticity
                          ges_div,      & ! divergence
                          ges_cwmr,     & ! cloud condensate mixing ratio 
                          ges_q,        & ! vapor mixing ratio
                          ges_oz,       & ! ozone mixing ratio
                          ges_tv          ! virtual temperature

   use guess_grids, only: nfldsig,      & ! number of guess sigma times
                          nfldsfc,      & ! number of guess surface times
                          hrdifsig,     & ! times for guess sigma fields
                          hrdifsfc        ! times for guess surface fields 
                                          ! atmosphere in kPa
   use guess_grids, only: ntguessig,    & ! These variables are used  
                          ntguessfc,    & ! to handle file names and are
                          ifilesfc,     & ! included for compatibility
                          ifilesig        ! with the legacy code.

   use guess_grids, only: nfldsig_all,	& ! all background times count
   			  nfldsfc_all,	&
			  nfldsig_now,	& ! filled guess_grids count
			  nfldsfc_now,	&
   			  hrdifsig_all,	& ! hour list of all backgrounds
			  hrdifsfc_all,	&
			  extrap_intime	  ! do time-extrapolation or not

   use guess_grids, only: ntguessig_ref	! a fixed reference ntguessig value
   use guess_grids, only: ntguessfc_ref	! a fixed reference ntguessfc value

   use guess_grids, only: guess_grids_stats

   use guess_grids, only: create_chemges_grids, &
                          destroy_chemges_grids

   ! routines from gridmod
   use gridmod,   only : create_mapping,     &
                         create_grid_vars,   &
                         destroy_mapping,    &
                         init_subdomain_vars,& 
   ! variables for create_grid_vars
                         rlats,    & ! grid latitudes (radians)
                         rlons,    & ! grid longitudes (radians)
                         coslon,   & ! cos(grid longitudes (radians))
                         sinlon,   & ! sin(grid longitudes (radians))
                         wgtlats,  & ! gaussian integration weights
                         rbs2,     & ! 1./sin(grid latitudes))**2
   ! variables for create_mapping and deter_subdomain
                         nlat_sfc, & ! no. of latitudes surface files
                         nlon_sfc, & ! no. of longitudes surface files
                         nlat,     & ! no. of analysis grid latitudes
                         nlon,     & ! no. of analysis grid longitudes
                         nsig,     & ! no. of levels
                         istart,   & ! start lat of the whole array on each pe
                         jstart,   & ! start lon of the whole array on each pe
                         ilat1,    & ! no. of lats for each subdomain (no buffer)
                         jlon1,    & ! no. of lons for each subdomain (no buffer)
                         lat1,     & ! no. of lats on subdomain (no buffer)
                         lon1,     & ! no. of lons on subdomain (no buffer)
                         ltosi,    & ! lats in iglobal array excluding buffer
                         ltosj,    & ! lons in iglobal array excluding buffer
                         ltosi_s,  & ! lats in itotsub array including buffer
                         ltosj_s,  & ! lons in itotsub array including buffer
                         ijn_s,    & ! no. of horiz. points for each subdomain 
                                     ! (with buffer)
                         ijn,      & ! no. of horiz. points for each subdomain 
                                     ! (no buffer)
                         irc_s,    & ! count for receive on subdomain
                         ird_s,    & ! displacement for receive on subdomain
                         isc_g,    & ! no. array, count for send to global; size of subdomain
                         isd_g,    & !   displacement for send to global
                         displs_s, & !   displacement for send from subdomain
                         displs_g, & !   displacement for receive on global grid
                         lat2,     & ! lat2: no. of lats on subdomain 
                                     ! (buffer points on ends)
                         lon2,     & ! lon2: no. of lons on subdomain 
                                     ! (buffer points on ends)
                         ak5,bk5,ck5, & ! coefficients for hybrid vertical coordinate
                         idvc5,    & ! vertical coordinate identifier
                         idpsfc5,  & ! surface pressure identifier
                         itotsub,  & ! number of horizontal points of all subdomains 
                                     ! combined
                         nsig1o,   & ! no. of levels distributed on each processor
                         iglobal,  &  ! number of horizontal points on global grid
                         jcap,     &
                         sp_a

   ! from mpimod
   use mpimod,     only: npe,nxpe,nype,  & ! num of MPI tasks (total, along x, along y)
                                           ! (nxpe, nype new for ESMF)
                         mpi_comm_world    ! MPI communicator

   use gsi_4dvar, only: ibdate, iedate, iadatebgn, iadateend, iwinbgn, &
                         nhr_assimilation,& ! size of assimilation window (hrs)
                         min_offset,&       ! offset minutes from analysis time
                         lwrtinc,&          ! when .t., writes out increment
                         l4dvar             ! when .t., will run 4d-var

   ! others...
   use constants, only: pi, rearth, zero, one, half, izero
   use kinds,     only: r_kind,r_single,i_kind
   use obsmod,    only: iadate, ianldate, ndat, ndatmax, ndat_times

   ! chem trace gases
   use gsi_chemtracer_mod, only: gsi_chemtracer_create_grids
   use gsi_chemtracer_mod, only: gsi_chemtracer_destroy_grids
   use gsi_chemtracer_mod, only: gsi_chemtracer_get
   use gsi_chemtracer_mod, only: gsi_chem_bundle

   implicit none

   private

! !PUBLIC ROUTINES:

   public GSI_GridCompSetServices
   public GSI_GridCompSetupSpecs
   public GSI_GridCompSetAlarms

!
! !REVISION HISTORY:
!
!   19Mar2007 Todling  Removed sigi,sigl; added module w/ 4dvar timers
!   05Apr2007 CCruz/RT Handle for many background fields
!   14Apr2007 Todling  Calculating idate4 internally; removed from rc file
!   17Apr2007 Todling  Added swap of vertical levels as internal feature;
!                      changed reference times in hrdifsig/sfc - per YT
!   25Apr2007 Todling  Added ts and lwi as export for FileSpec compliance
!   08May2007 Todling  Removed reference to global surface arrays
!   05Jul2007 Todling  Analysis time set in rc file (defining ntguess)
!   10Jul2007 Todling  Adjustment to cope w/ write out of increment
!   08Jul2008 Todling  Merge fdda-b1p3 w/ das-215 (MAPL update)
!   18Nov2008 Todling  Merge with NCEP-May-2008; add sfc_rough
!   09Oct2009 Wu       replace nhr_offset with min_offset since it's 1.5 hr for regional
!   10Jul2009 Todling  Remove vertical halo
!   19Aug2009 Guo      Added hrdifsig_all etc. and other changes for
!		       multi-pass observer.
!   31Mar2010 Treadon  replace specmod routines with general_specmod
!   21Apr2010 Todling  - Add chem tracers capability
!                      - Rename Initalize/Run/Finalize
!   20May2010 Todling  Change initialization of chem
!
!EOP
!-------------------------------------------------------------------------

! GLOBAL scope vars

   ! parameters
   integer, parameter  :: HW = 1    ! horizontal halowidth of distributed GSI fields
   real, parameter     :: UNDEF_SSI_      = -9.9899991E33 
   real, parameter     :: UNDEF_SOIL_TEMP = 1.E+15
   real, parameter     :: UNDEF_SNOW_DEP  = 1.E+12
   real, parameter     :: PPMV2DU         = 1.657E-6
   real, parameter     :: kPa_per_Pa      = 0.001
   real, parameter     :: Pa_per_kPa      = 1000.
   logical, parameter  :: verbose         = .false.

   integer, save       :: nbkgfreq        = 1
   integer, save       :: nthTimeIndex    = 0
   integer, save       :: BKGfreq_hr,BKGfreq_mn,BKGfreq_sc
   integer, save       :: ANAfreq_hr,ANAwndw_hr
   integer(i_kind), save :: MYIDATE(4)
   integer(i_kind), save :: MYHOURG = 0.

   logical, save       :: doVflip = .true.

   integer(i_kind) :: ntgases                          ! number of tracer gases (namelist)
   character(len=ESMF_MAXSTR),allocatable:: tgases(:)  ! names of tracer gases

   character(len=ESMF_MAXSTR),save,allocatable :: obstab(:,:)

   integer,parameter :: NFLDSLOT=2 ! in lobserver mode, internal guess grid size is controlled

! local utilities

   ! transpose IJ-JI
   interface GSI_GridCompSwapIJ_
     module procedure SwapIJ2r_
     module procedure SwapIJ2i_
     module procedure SwapIJ3r_
   end interface
   ! transpose JI-IJ
   interface GSI_GridCompSwapJI_
     module procedure SwapJI2r_
     module procedure SwapJI3r_
   end interface
   ! swap vertical levels
   interface GSI_GridCompSwapV_
     module procedure SwapVr_
     module procedure SwapV4_
   end interface
   ! swap vertical levels
   interface GSI_GridCompFlipLons_
     module procedure GSI_GridCompFlipLons2_
   end interface

   logical, save :: rc_obstable_initialized_=.false.

!---------------------------------------------------------------------------

   CONTAINS

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: GSI_GridCompSetServices -- Sets ESMF services for the GSI

! !INTERFACE:

   subroutine GSI_GridCompSetServices ( gc, rc )

!
! !USES:
!
  implicit NONE

! !ARGUMENTS:

   type(ESMF_GridComp)               :: gc  ! gridded component
   integer            , intent(out ) :: rc

! !DESCRIPTION: This version uses the MAPL_GenericSetServices,
!        which sets the Run, Initialize, and Finalize services, 
!       as well as allocating our instance of a generic state and putting it in the 
!       gridded component (GC). 
!
!EOPI
!-------------------------------------------------------------------------

   integer                               :: STATUS
   logical                               :: IamRoot    
   character(len=*), parameter :: IAm='GSI_GridCompSetServices'

! start

   IamRoot = MAPL_AM_I_ROOT()

   if(IamRoot) print *, Iam,": Start ",trim(Iam)

! Set the Initialize, Run, Finalize entry points

   call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETINIT, Initialize, &
                                                       STATUS)
   VERIFY_(STATUS)

   call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETRUN, Run,         &
                                                       STATUS)
   VERIFY_(STATUS)

   call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETFINAL, Finalize,  &
                                                       STATUS)
   VERIFY_(STATUS)

! Set Import/Export Coupling SPECS through the Generic Internal State

   call GSI_GridCompSetupSpecs (GC, rc=STATUS) 
   VERIFY_(STATUS)

! Set analysis timer

   call MAPL_TimerAdd(gc, name='Analysis time', rc=STATUS)
   VERIFY_(STATUS)

! Generic SetServices

   call MAPL_GenericSetServices ( gc, RC=STATUS)
   VERIFY_(STATUS)

   if(IamRoot.and.verbose) print *,Iam,": End ",trim(Iam)

   RETURN_(ESMF_SUCCESS)

   end subroutine GSI_GridCompSetServices

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: Initialize -- Initialize the GSI Gridded Comp

! !INTERFACE:

   subroutine Initialize ( gc, import, export, clock, rc )

!
! !USES:
!

   implicit NONE

! !ARGUMENTS:

   type(ESMF_GridComp), intent(INOUT) :: gc     ! Gridded component
   type(ESMF_State),    intent(INOUT) :: import ! Import state
   type(ESMF_State),    intent(INOUT) :: export ! Export state
   type(ESMF_Clock),    intent(INOUT) :: clock  ! The clock
   integer, optional,   intent(  OUT) :: rc     ! Error code:

! !DESCRIPTION: This function initializes the GSI gridded component.
!       It  creates its ESMF grid, defines the import/export specs, and 
!       initializes other variables associated with the GSI legacy code.
!
! !REVISION HISTORY:
!
!   05Apr2007 CCruz/RT  Removed clock advance; add to AlarmSet interface
!   19Apr2007 Todling   Placed call to CompGetVertParms a bit sooner
!
!EOPI
!-------------------------------------------------------------------------

   integer                          :: STATUS    ! error code STATUS
   type(MAPL_MetaComp), pointer     :: GENSTATE  ! GEOS Generic state
   type(ESMF_Config)                :: CF        ! configuration data
   type(ESMF_VM)                    :: VM        ! virtual machine
   type(ESMF_Grid)                  :: GSIGrid   ! this component's grid
   logical                          :: IamRoot
   integer                          :: GSIGridType
   type(ESMF_Time)                  :: AnaTime
   type(ESMF_Time)                  :: CurrTime
   type(ESMF_TimeInterval)          :: alarmInterval
   type (MAPL_VarSpec), pointer     :: import_spec(:)
   type (MAPL_VarSpec), pointer     :: export_spec(:)
   integer                          :: i, mype, atime
   integer                          :: yy, mm, dd, h, m
   character(len=ESMF_MAXSTR)       :: aname
   character(len=*), parameter :: IAm='Initialize'

! start

   IamRoot =  MAPL_AM_I_ROOT()
   if(IamRoot.and.verbose) print *, Iam,": Start ",trim(Iam)

! Get vm, config objects

   call ESMF_GridCompGet( gc, config=CF, RC=STATUS )
   VERIFY_(STATUS)
   call ESMF_VMGetGlobal(vm=vm, rc=STATUS)
   VERIFY_(STATUS)
   call ESMF_VMGet(vm, petCount=npe, localPet=mype, &
        mpiCommunicator=mpi_comm_world, &
        rc=STATUS)
   VERIFY_(STATUS)

!                       ---------------------
!                       Initialize Legacy GSI
!                       ---------------------

! initialize internal GSI variables and read namelists
   
   if(IamRoot.and.verbose) print *, trim(Iam),': gsimain_Initialize'
   call gsimain_Initialize

! REMARKS:
! 1) In the legacy code, domain decomposition, grid setup, etc, are
!    performed in routine gsisub(), which is now our run() method.
! 2) These are now done here during initialization as we need this
!    information to implement the couplers.
   
! domain decomposition

    call GSI_GridCompDeterSubdomain_()
     
! get GSI parameters that are usually obtained from a file header
! (vertical grid parameters ak, bk, sigma levels, etc.)

   call GSI_GridCompGetVertParms_(rc=STATUS)

! create GSI ESMF_GRID - using GSI domain decomposition
! Also set grid lons/lats.

   call GSI_GridCompGridCreate_(gsigrid)

! Make sure GSI G.C. gets this new grid

   call ESMF_GridCompSet(gc, grid=gsigrid, rc=STATUS)
   VERIFY_(STATUS)

! Determine how many global atmospheric and surface guess times are
! used (no. of first guess time levels, interval).

   call GSI_GridCompGetBKGTimes_()

! Allocate memory for GSI internal variables (first guess time levels)

   call GSI_GridCompAlloc_()

!                       -----------------------
!                       Initialize MAPL Generic
!                       -----------------------

! GEOS generic initialize
   
   call MAPL_GenericInitialize(gc, import, export, clock, rc=STATUS)
   VERIFY_(STATUS)

!                       --------------
!                       Set GSI alarms
!                       --------------

   call GSI_GridCompSetAlarms (GENSTATE, GC, cf, clock)

   if(IamRoot.and.verbose) print *, Iam,": End ",trim(Iam)

#ifdef PRINT_STATES
    call WRITE_PARALLEL ( trim(Iam)//": IMPORT State" )
    if ( MAPL_am_I_root() ) call ESMF_StatePrint ( import, rc=STATUS )
    call WRITE_PARALLEL ( trim(Iam)//": EXPORT State" )
    if ( MAPL_am_I_root() ) call ESMF_StatePrint ( export, rc=STATUS )
#endif

   RETURN_(STATUS)

   CONTAINS

!-------------------------------------------------------------------------
   subroutine GSI_GridCompDeterSubdomain_()
!-------------------------------------------------------------------------
   character(len=*), parameter :: IAm='GSI_GridCompDeterSubdomain_'

   if(IamRoot.and.verbose) print *,trim(Iam),': determine GSI domain decomposition'

   call ESMF_ConfigGetAttribute( CF, nxpe, label ='NX:', rc = STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( CF, nype, label ='NY:', rc = STATUS )
   VERIFY_(STATUS)

   ! Note
   ! nlat: no. of latitudes
   ! nlon: no. of longitudes
   ! nsig: no. of levels
   ! npe:  no. of PEs
   ! These are defined in gsimain_Initialize
   
   ASSERT_(mod(nlon,8)==0)	! as required by GSI (e.g. 192,288,512,544)

   nlat_sfc = nlat
   nlon_sfc = nlon

   ! the following routines are called in gsisub:

   call create_grid_vars()
   
   call create_mapping(npe)

   ! decompose GSI grid into npe subdomains. Also calculate lat1,lon1
   ! lat1: no. of lats on subdomain (no buffer)
   ! lon1: no. of lons on subdomain (no buffer)
   call deter_subdomain(mype)

   ! initialize subdomain variables
   call init_subdomain_vars
    
   ! set comm variables used between tasks from vert colums in
   ! subdomains to horz slabs on the global domain
   call init_commvars(mype)
 
   end subroutine GSI_GridCompDeterSubdomain_

!-------------------------------------------------------------------------
   subroutine GSI_GridCompGridCreate_ ( grid )
!-------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  19Apr2007 Todling  Added ak and bk as attribute to file header
!  06Jan2009 RT/Guo   Redefine spectral coefficients on lat-lon grid
!
!-------------------------------------------------------------------------

   type (ESMF_Grid),  intent(inout)   :: grid    ! component grid

! Local vars

   type(ESMF_Logical)  :: flip_poles
   type(ESMF_Logical)  :: flip_lons
   type(ESMF_DELayout) :: LAYOUT    ! DE layout
   integer, allocatable:: imxy(:), jmxy(:), lmxy(:)
   integer(kind(nlon)) :: i,j,k,nzpe
   integer(i_kind)     :: ROOT
   real                :: lon0, lat0
   real                :: lon0d, lat0d
   real                :: pi, d2r
   real(ESMF_KIND_R8)  :: minCoord(3)
   real(ESMF_KIND_R8)  :: deltaZ
   real(ESMF_KIND_R8), allocatable  :: deltaX(:), deltaY(:)
   real(kind(half))    :: dlon,dlat,pih
   character(len=*), parameter :: IAm='GSI_GridCompGridCreate'

! start

    rc  = 0
    pi  = 4.0 * atan ( 1.0 ) 
   d2r  = pi / 180.

! Horizontal grid : uniform (e.g. Equally spaced) or non-uniform (e.g. Gaussian)
! 0 = uniform, 1 = non-uniform

   call ESMF_ConfigGetAttribute( CF, GSIGridType, label ='GSIGridType:', rc = STATUS )
   VERIFY_(STATUS)

   if(IamRoot.and.verbose) print *,trim(Iam),': Will create GSI grid of TYPE  ',GsiGridType

! Query start longitude and latitude

   call ESMF_ConfigGetAttribute( CF, LON0, label ='ORIGIN_CENTER_LON:', rc = STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( CF, LAT0, label ='ORIGIN_CENTER_LAT:', rc = STATUS )
   VERIFY_(STATUS)

   lon0d= lon0; lat0d=lat0
   lon0 = lon0 * d2r
   lat0 = lat0 * d2r
   dlon=(pi+pi)/nlon	! in radians
   dlat=pi/(nlat-1)
   deltaZ = 1.0

   if(GsiGridType==0) then  ! equally spaced dgrid


! Set grid longitude array used by GSI.
      do i=1,nlon			! from 0 to 2pi
         rlons (i)=(i-1)*dlon
         coslon(i)=cos(rlons(i))
         sinlon(i)=sin(rlons(i))
      end do

! Set grid latitude array used by GSI.
      pih =half*pi
      do j=1,nlat			! from -pi/2 to +pi/2
         rlats(j)=(j-1)*dlat - pih
      end do

! wgtlats is used by spectral code. The values are used as divisor in the
! compact_diffs::inisph() routine.  Therefore, set to TINY instead of ZERO.
      wgtlats(:)=TINY(wgtlats)

! rbs2=1/cos^2(rlats)) is used in pcp.  polar points are set to zeroes.
      rbs2(1       )=zero
      rbs2(2:nlat-1)=cos(rlats(2:nlat-1))
      rbs2(2:nlat-1)=one/(rbs2(2:nlat-1)*rbs2(2:nlat-1))
      rbs2(  nlat  )=zero

   else                      ! Gaussian grid

! Set Gaussian grid lon/lat arrays used by GSI.
      call gengrid_vars  

  end if
 
! Re-Define South-West Corner of First Grid-Box
! ---------------------------------------------
!
!   NW---------NE
!   |           |
!   |     C     |
!   |           |
!   SW---------SE
!

! Create Grid. The deltas define whether is is uniform or non-uniform

   grid = MAPL_LatLonGridCreate (Name='GSI Grid', vm=vm, Nx=nxpe, Ny=nype,           &
                                 IM_World=nlon, BegLon=lon0d, &
                                 JM_World=nlat, BegLat=lat0d, &
                                 LM_World=nsig, &
                                 RC=STATUS)
!!                                IM_World, BegLon, DelLon, &
!!                                JM_World, BegLat, DelLat, &
   VERIFY_(STATUS)

! distribute grid
! ---------------

   call ESMF_VMBroadcast(vm, rlons, size(rlons), MAPL_Root, RC=STATUS); VERIFY_(STATUS)
   call ESMF_VMBroadcast(vm, rlats, size(rlats), MAPL_Root, RC=STATUS); VERIFY_(STATUS)

   ! Reinitialize specmod for spectral transformation whereever needed.
   if(GsiGridType==0) then 
   	! re-initialize spec_vars, but leave hires_b untouched (for now).
      call general_destroy_spec_vars(sp_a)
      call general_init_spec_vars(sp_a,jcap,jcap,nlat,nlon,eqspace=.true.)
   endif

   ! Fields (bkg files) on a uniform grid have longitude range on [-pi,pi]
   ! and the latitudes are "correctly" oriented, so:
   if(GsiGridType==0) then 
      flip_lons  = ESMF_TRUE    ! flip to [0,2*pi]
      flip_poles = ESMF_FALSE
   end if   
   ! Fields on a Gaussian grid have longitude range on [0,2*pi]
   ! and the latitudes are oriented in reverse, so:
   if(GsiGridType==1) then 
      flip_lons  = ESMF_FALSE
      flip_poles = ESMF_TRUE
   end if   
   call ESMF_AttributeSet(grid, "FLIP_LONS", flip_lons, rc=STATUS)
   VERIFY_(STATUS)
   call ESMF_AttributeSet(grid, "FLIP_POLES", flip_poles, rc=STATUS)
   VERIFY_(STATUS)

!!! TODO: Set some grid attributes to perform these actions during coupling:
!!! GSI distributed fields are transposed from IJK to JIK
!   index_order = 2-1-3  ! e.g.
!   call ESMF_AttributeSet(grid, "GRID_INDEX_ORDER", index_order , rc= STATUS)
!   VERIFY_(STATUS)

   call ESMF_AttributeSet(grid, 'ak', nsig+1, ak5, rc=STATUS)
   VERIFY_(STATUS)
   call ESMF_AttributeSet(grid, 'bk', nsig+1, bk5, rc=STATUS)
   VERIFY_(STATUS)
!  call ESMF_AttributeSet(grid, 'ck', nsig+1, ck5, rc=STATUS)
!  VERIFY_(STATUS)

   end subroutine GSI_GridCompGridCreate_

!-------------------------------------------------------------------------
   subroutine GSI_GridCompGetVertParms_(rc)
!-------------------------------------------------------------------------
!
! !REVISION HISTORY:
!
!  21Mar2007 Todling  Added ck5
!
!-------------------------------------------------------------------------
   integer, optional,   intent(  OUT) :: rc     ! Error code:
   integer                            :: k
   real, allocatable,  dimension(:)   :: ak5r4(:),bk5r4(:)
   integer, allocatable, dimension(:) :: mylevs(:)
   character(len=16)                  :: vgridlabl
   character(len=2)                   :: cnsig
   character(len=*), parameter :: IAm='GSI_GridCompGetVertParms'

! start

   if(IamRoot.and.verbose) print *,trim(Iam),': Get GSI g.c. parameters '

! Create the label to be searched for in the RC file based on nsig

   write(cnsig,'(i2)',iostat=STATUS)nsig
   VERIFY_(STATUS)
   vgridlabl = "VERTGRID"//cnsig//":"

   CALL ESMF_ConfigFindLabel(CF, label = trim(vgridlabl), rc = STATUS)
   VERIFY_(STATUS)
   allocate ( ak5r4(nsig+1), bk5r4(nsig+1), stat=STATUS )
   VERIFY_(STATUS)
   DO i = 1, nsig+1
      CALL ESMF_ConfigNextLine    (CF, rc = STATUS)
      VERIFY_(STATUS)
      CALL ESMF_ConfigGetAttribute(CF, k, rc = STATUS)
      VERIFY_(STATUS)
      CALL ESMF_ConfigGetAttribute(CF, ak5r4(i), rc = STATUS) 
      VERIFY_(STATUS)  
      ak5(i)=ak5r4(i)
      CALL ESMF_ConfigGetAttribute(CF, bk5r4(i), rc = STATUS)
      VERIFY_(STATUS)
      bk5(i)=bk5r4(i)
      ck5(i)=zero
   END DO
   deallocate ( ak5r4, bk5r4, stat=STATUS )
   VERIFY_(STATUS)

   if(IamRoot.and.verbose) then
      print *,trim(IAm),' - lev, ak, bk - '
      do i=1,nsig+1
         write(*,'(1x,i3,2f16.6)') i,ak5(i),bk5(i)
      end do
   end if

! Since sigma = P/Ps => P = sigma*Ps
! but P = ak + bk*Ps
! hence for Gaussian grid type set ak=0 => sigma=bk

   idvc5 = 2    ! set to sigma-pressure
   if(GsiGridType==1) then
      idvc5 = 1 ! reset to sigma-only
      if(nsig/=64) then
         print *,trim(IAm),' GSIsa_Gaussian only supports 64 levels; nsig = ',nsig
         RETURN_(ESMF_FAILURE)
      end if
   end if

! these parameters are usually obtained for a first guess' file header:

!  CALL ESMF_ConfigGetAttribute(CF, nchemtr, label = 'tracers:', RC=STATUS); VERIFY_(STATUS)
!  CALL ESMF_ConfigGetAttribute(CF, vtid   , label = 'vtid:'   , RC=STATUS); VERIFY_(STATUS)
!  CALL ESMF_ConfigGetAttribute(CF, pdryini, label = 'pdryini:', RC=STATUS); VERIFY_(STATUS)
!  CALL ESMF_ConfigGetAttribute(CF, xncld  , label = 'xncld:'  , RC=STATUS); VERIFY_(STATUS)

   end subroutine GSI_GridCompGetVertParms_

!-------------------------------------------------------------------------
   subroutine GSI_GridCompGetBKGTimes_()
!-------------------------------------------------------------------------
   use mpeu_util, only: tell
   implicit none
   
! local variables

   integer(i_kind)  :: i, bkgbits, bkgbits_hr
   integer(i_kind)  :: JOB_SGMT(2)
   integer(i_kind)  :: BKGfreq
   integer(i_kind)  :: ANAfreq
   integer(i_kind)  :: RUN_DT
   character(len=*),parameter :: IAm='GSI_GridCompGetBKGTimes'

! Begin...

   if(IamRoot.and.verbose) print *,trim(Iam),': Get GSI background times'

   ! proceed as in GSI's read_files.f90 
   
   call ESMF_ConfigGetAttribute(CF, JOB_SGMT, 2, label='JOB_SGMT:', rc=STATUS)
        VERIFY_(STATUS)

   ANAwndw_hr  = JOB_SGMT(2)/10000 + JOB_SGMT(1)*24

   CALL ESMF_ConfigGetAttribute(CF, BKGfreq, label = 'BKG_FREQUENCY:', rc=STATUS)
        VERIFY_(STATUS)

   BKGfreq_hr = BKGfreq/10000
   BKGfreq_mn = mod(BKGfreq,10000)/100
   BKGfreq_sc = mod(BKGfreq,100)
   BKGfreq_sc = BKGfreq_hr*3600 + BKGFreq_mn*60 + BKGfreq_sc
 
   call ESMF_ConfigGetAttribute(CF, RUN_DT, label='RUN_DT:', rc=STATUS)
        VERIFY_(STATUS)

   nbkgfreq  = BKGfreq_sc / RUN_DT    ! bkg per dt

   CALL ESMF_ConfigGetAttribute(CF, ANAfreq, label = 'ANA_FREQUENCY:', rc=STATUS)
        VERIFY_(STATUS)

   ANAfreq_hr = ANAfreq / 10000

   if ( mod(BKGfreq_sc,6*3600)==0) then
        nfldsig_all = nhr_assimilation*3600 / BKGfreq_sc
   else
        nfldsig_all = nhr_assimilation*3600 / BKGfreq_sc + 1
   endif
   nfldsfc_all = nfldsig_all   ! In GEOS-5 upper-air bkg and surface bkg come equal numbers

	! to use gsimod in a clock-driving mode, only upto two time slots are
	! needed for guess_grid

   nfldsig = nfldsig_all
   nfldsfc = nfldsfc_all
   if(lobserver) then
     nfldsig = min(NFLDSLOT,nfldsig_all)
     nfldsfc = min(NFLDSLOT,nfldsfc_all)
   endif

   extrap_intime = .not.lobserver .or. nfldsig_all==1

   nfldsig_now = 0	! no data has filled into guess_grids
   nfldsfc_now = 0

   ! these are internal GSI global variables
   ! and are deallocated in Finalize
   allocate(hrdifsig    (1:nfldsig    ), &
   	    hrdifsfc    (1:nfldsfc    ), &
	    hrdifsig_all(1:nfldsig_all), &
   	    hrdifsfc_all(1:nfldsfc_all), stat=STATUS)
   VERIFY_(STATUS)
   allocate(ifilesig(1:nfldsig_all), ifilesfc(1:nfldsfc_all), stat=STATUS)
   VERIFY_(STATUS)

   ifilesig(1:nfldsig_all)=6    ! it doesn't matter what goes in ifilesig
   ifilesfc(1:nfldsig_all)=6    ! it doesn't matter what goes in ifilesfc

#ifdef VERBOSE
   call tell(Iam,'nfldsig_all=',nfldsig_all)
   call tell(Iam,'nfldsfc_all=',nfldsfc_all)
   call tell(Iam,'BKGfreq_hr =',BKGfreq_hr )
#endif
   bkgbits = 0
   do i = 1, nfldsig_all
      hrdifsig_all(i)  = bkgbits
      bkgbits      = bkgbits + BKGfreq_hr
#ifdef VERBOSE
      call tell(Iam,'             i =',i)
      call tell(Iam,'hrdifsig_all(i)=',hrdifsig_all(i))
#endif
   enddo

   bkgbits = 0
   do i = 1, nfldsfc_all
      hrdifsfc_all(i)  = bkgbits
      bkgbits      = bkgbits + BKGfreq_hr
#ifdef VERBOSE
      call tell(Iam,'             i =',i)
      call tell(Iam,'hrdifsfc_all(i)=',hrdifsfc_all(i))
#endif
   enddo

   end subroutine GSI_GridCompGetBKGTimes_

!-------------------------------------------------------------------------
   subroutine GSI_GridCompAlloc_()
!-------------------------------------------------------------------------

! allocation of GSI internal state variables

   character(len=*), parameter :: IAm='GSI_GridCompAlloc'
   integer(i_kind) ii,jj,kk,nn

   allocate( isli     (lat2,lon2,nfldsfc),&
             fact10   (lat2,lon2,nfldsfc),&
             sfct     (lat2,lon2,nfldsfc),&
             dsfct    (lat2,lon2,nfldsfc),&
             sno      (lat2,lon2,nfldsfc),&
             veg_type (lat2,lon2,nfldsfc),&
             veg_frac (lat2,lon2,nfldsfc),&
             soil_type(lat2,lon2,nfldsfc),&
             soil_temp(lat2,lon2,nfldsfc),&
             soil_moi (lat2,lon2,nfldsfc),&
             sfc_rough(lat2,lon2,nfldsfc),&
             stat=STATUS)
   VERIFY_(STATUS)
   do nn=1,nfldsfc
      do jj=1,lon2
         do ii=1,lat2
            isli(ii,jj,nn)=izero
            fact10(ii,jj,nn)=zero
            sfct(ii,jj,nn)=zero
            dsfct(ii,jj,nn)=zero
            sno(ii,jj,nn)=zero
            veg_type(ii,jj,nn)=zero
            veg_frac(ii,jj,nn)=zero
            soil_type(ii,jj,nn)=zero
            soil_temp(ii,jj,nn)=zero
            soil_moi(ii,jj,nn)=zero
         end do
      end do
   end do

   allocate( ges_z   (lat2,lon2,nfldsig),     &
             ges_ps  (lat2,lon2,nfldsig),     &
             ges_u   (lat2,lon2,nsig,nfldsig),&
             ges_v   (lat2,lon2,nsig,nfldsig),&
             ges_vor (lat2,lon2,nsig,nfldsig),&
             ges_div (lat2,lon2,nsig,nfldsig),&
             ges_cwmr(lat2,lon2,nsig,nfldsig),&
             ges_q   (lat2,lon2,nsig,nfldsig),&
             ges_oz  (lat2,lon2,nsig,nfldsig),&
             ges_tv  (lat2,lon2,nsig,nfldsig),&
             stat=STATUS)
   VERIFY_(STATUS)
    do nn=1,nfldsig
       do jj=1,lon2
          do ii=1,lat2
             ges_z(ii,jj,nn)=zero
             ges_ps(ii,jj,nn)=zero
          end do
       end do
    end do
    do nn=1,nfldsig
       do kk=1,nsig
          do jj=1,lon2
             do ii=1,lat2
                ges_u   (ii,jj,kk,nn)=zero
                ges_v   (ii,jj,kk,nn)=zero
                ges_vor (ii,jj,kk,nn)=zero
                ges_div (ii,jj,kk,nn)=zero
                ges_cwmr(ii,jj,kk,nn)=zero
                ges_q   (ii,jj,kk,nn)=zero
                ges_oz  (ii,jj,kk,nn)=zero
                ges_tv  (ii,jj,kk,nn)=zero
             end do
          end do
       end do
    end do

!  When proper connection to ESMF is complete,
!  the following will not be needed here
!  ------------------------------------------
   call gsi_chemtracer_get('dim',ntgases,status)
   VERIFY_(STATUS)
   if (ntgases>0) then
       allocate (tgases(ntgases))
       call gsi_chemtracer_get('shortnames',tgases,status)
       VERIFY_(STATUS)

       call create_chemges_grids(status)
       VERIFY_(STATUS)
   endif

   RETURN_(ESMF_SUCCESS)

   end subroutine GSI_GridCompAlloc_

   end subroutine Initialize

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: Run -- Run the GSI Gridded Component

! !INTERFACE:

   subroutine Run ( gc, import, export, clock, rc )

!
! !USES:
!
   use mpeu_util, only: tell
   implicit NONE

! !ARGUMENTS:

   type(ESMF_GridComp), intent(INOUT) :: gc     ! Gridded component
   type(ESMF_State),    intent(INOUT) :: import ! Import state
   type(ESMF_State),    intent(INOUT) :: export ! Export state
   type(ESMF_Clock),    intent(INOUT) :: clock  ! The clock
   integer, optional,   intent(  OUT) :: rc     ! Error code:

! !DESCRIPTION: set up and run the GSI analysis.
!
!  05Apr2007 Cruz    Handle for many background fields  
!  20Apr2007 Todling Added phis to export state
!  23Apr2007 Todling Added export of delp
!
!EOPI
!-------------------------------------------------------------------------

! local variables

   integer                           :: i, j, k, L
   integer                           :: im,jm,km
   integer                           :: mype
   integer                           :: GSIGridType
   integer                           :: STATUS    ! error code STATUS
   type(MAPL_MetaComp), pointer      :: GENSTATE  ! GEOS Generic state
   type(ESMF_Config)                 :: CF        ! coneiguration data
   type(ESMF_VM)                     :: VM        ! virtual machine
   type(ESMF_Grid)                   :: GSIGrid   ! this component's grid
   type(ESMF_Alarm)                  :: GSIALARM
   logical                           :: do_analysis
   logical                           :: do_observer
   logical                           :: get_background
   logical                           :: IamRoot
   ! import state upper air pointers
   real(4),dimension(:,:  ), pointer :: hsp  ! terrain
   real(4),dimension(:,:  ), pointer :: psp  ! surf. pressure
   real(4),dimension(:,:,:), pointer :: up   ! u wind
   real(4),dimension(:,:,:), pointer :: vp   ! v wind
   real(4),dimension(:,:,:), pointer :: tp   ! virtual temp.
   real(4),dimension(:,:,:), pointer :: qp   ! spec. hum.
   real(4),dimension(:,:,:), pointer :: ozp  ! ozone
   real(4),dimension(:,:,:), pointer :: qimr ! cloud ice    mixing ratio
   real(4),dimension(:,:,:), pointer :: qlmr ! cloud liquid mixing ratio
   ! import chem tracers ... preliminary (dummy implementation)
   real(4),dimension(:,:,:), pointer :: cop   ! carbone monoxide
   real(4),dimension(:,:,:), pointer :: co2p  ! carbone dioxide
   ! import state surface pointers
   real(4),dimension(:,:  ), pointer :: f10p ! 10m winf factors
   real(4),dimension(:,:  ), pointer :: tskp ! skin Temp.
   real(4),dimension(:,:  ), pointer :: snop ! snow depth
   real(4),dimension(:,:  ), pointer :: sotp ! soil Temp.
   real(4),dimension(:,:  ), pointer :: soqp ! soil moist. 
   real(4),dimension(:,:  ), pointer :: frland    ! land fraction
   real(4),dimension(:,:  ), pointer :: frlandice ! land-ice fraction
   real(4),dimension(:,:  ), pointer :: frlake    ! lake fraction
   real(4),dimension(:,:  ), pointer :: frocean   ! ocean fraction
   real(4),dimension(:,:  ), pointer :: frseaice  ! sea-ice fraction
   real(4),dimension(:,:  ), pointer :: vtyp ! veg. type
   real(4),dimension(:,:  ), pointer :: styp ! soil type
   real(4),dimension(:,:  ), pointer :: vfrp ! veg. frac.
   real(4),dimension(:,:  ), pointer :: sz0p ! surf roughness
   ! u10m and v10m used to calculate 10m wind factors
   real(4),dimension(:,:  ), pointer :: u10p, v10p
   ! export state pointers - tendencies
   real(4),dimension(:,:  ), pointer :: df10  ! factor 10m
   real(4),dimension(:,:  ), pointer :: dsli  ! land/water/ice mask
   real(4),dimension(:,:  ), pointer :: dts   ! skin/surface temperature
   real(4),dimension(:,:  ), pointer :: dhs   ! terrain
   real(4),dimension(:,:  ), pointer :: dps   ! surf pressure
   real(4),dimension(:,:,:), pointer :: ddp   ! del pressure
   real(4),dimension(:,:,:), pointer :: du    ! u wind
   real(4),dimension(:,:,:), pointer :: dv    ! v wind
   real(4),dimension(:,:,:), pointer :: dt    ! virtual Temp.
   real(4),dimension(:,:,:), pointer :: dq    ! spec. hum.
   real(4),dimension(:,:,:), pointer :: doz   ! ozone
   real(4),dimension(:,:,:), pointer :: dqimr ! cloud ice    mixing ratio
   real(4),dimension(:,:,:), pointer :: dqlmr ! cloud liquid mixing ratio
   real(4),dimension(:,:  ), pointer :: dfrland    ! land fraction
   real(4),dimension(:,:  ), pointer :: dfrlandice ! land-ice fraction
   real(4),dimension(:,:  ), pointer :: dfrlake    ! lake fraction
   real(4),dimension(:,:  ), pointer :: dfrocean   ! ocean fraction
   real(4),dimension(:,:  ), pointer :: dfrseaice  ! sea-ice fraction
   ! export chem tracers ... preliminary (dummy implementation)
   real(4),dimension(:,:,:), pointer :: dcop  ! carbone monoxide
   real(4),dimension(:,:,:), pointer :: dco2p ! carbone dioxide
   character(len=ESMF_MAXSTR)        :: aname
   character(len=ESMF_MAXSTR)        :: opt
   character(len=*), parameter :: IAm='Run'
   integer :: ier
   integer :: atime

! start

   IamRoot = MAPL_AM_I_ROOT() 
   if(IamRoot.and.verbose) print *, Iam,": Start ",trim(Iam)

   call ESMF_VMGetGlobal(vm=vm, rc=STATUS)
   VERIFY_(STATUS)
   call ESMF_VMGet(vm, petCount=npe, localPet=mype, rc=STATUS)
   VERIFY_(STATUS)
   call ESMF_GridCompGet( gc, config=CF, RC=STATUS )
   VERIFY_(STATUS)
   call ESMF_GridCompGet( gc, grid=GSIgrid, RC=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( CF, GSIGridType, label ='GSIGridType:', rc = STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( cf, opt, label ='ANALYSIS_INCREMENT:', rc = STATUS )
   VERIFY_(STATUS)
   opt = adjustl(opt)
   if ( scan(opt(1:1),"Yy") /= 0 ) then
        if(.not.lwrtinc) then
            if(IamRoot) print *,trim(Iam)//': overwrite GSI namelist setting, lwrtinc = ', .not.lwrtinc
        endif
        lwrtinc = .true.
   endif



! Get ALARMs
!-----------

   call MAPL_GetObjectFromGC ( GC, GENSTATE, RC=STATUS); VERIFY_(STATUS)

   nthTimeIndex = nthTimeIndex + 1
   if(IamRoot) print *,trim(Iam)//': time index = ',nthTimeIndex, ' bkgfreq = ', nbkgfreq

!  Determine if it's time do gather background
!  -------------------------------------------
!  if ( mod(nthTimeIndex-1,nbkgfreq) .ne. 0 ) then
!      if(IamRoot) print *,trim(Iam),': no time for bkg'
!      RETURN_(ESMF_SUCCESS)
!  endif
!  L = (nthTimeIndex-1)/nbkgfreq + 1
   L =  nthTimeIndex

!  Determine if it's time to actually do the analysis
!  --------------------------------------------------
   do_analysis = .false.
   call MAPL_StateAlarmGet(GENSTATE, GSIALARM, NAME='last-bkg', RC=STATUS); VERIFY_(STATUS)
   do_analysis = ESMF_AlarmIsRinging(GSIALARM, RC=STATUS); VERIFY_(STATUS)
   do_analysis = do_analysis .and. (.not.lobserver)

   do_observer = L>=nfldsig  .and. lobserver

#ifdef VERBOSE
   if(IamRoot) then
     call tell(Iam,'lobserver =',lobserver)
     call tell(Iam,'do_analysis =',do_analysis)
     call tell(Iam,'do_observer =',do_observer)
     call tell(Iam,'nthTimeIndex =',nthTimeIndex)
     call tell(Iam,"L =",L)
     call tell(Iam,"nfldsig_all",nfldsig_all)
     call tell(Iam,"nfldsig_now",nfldsig_now)
     call tell(Iam,"nfldsig",nfldsig)
     call tell(Iam,'init_pass(L==nfldsig) =',(L==nfldsig))
     call tell(Iam,'last_pass(L==nfldsig_all) =',(L==nfldsig_all))
   endif
#endif

   call GSI_GridCompGetPointers_()
   call GSI_GridCompCopyImportDyn2Internal_(L)
   call GSI_GridCompComputeVorDiv_(L)
   call GSI_GridCompCopyImportSfc2Internal_(L)
   call GSI_GridCompGetNCEPsfcFromFile_(L)

!  Set alarm
!  ---------

   call GSI_GridCompSetAnaTime_()
#ifdef VERBOSE
   call tell(Iam,"returned from GSI_GridCompSetAnaTime_()")
#endif

!  Set observations input
!  ----------------------
   if(L==1.and.IamRoot) call GSI_GridCompSetObsNames_(L)
#ifdef VERBOSE
   call tell(Iam,"returned from GSI_GridCompSetObsNames_(L) at L =",L)
#endif

!  Run observer or analysis
!  ------------
   if(lobserver) then
     if(.not. do_observer) then
#ifdef VERBOSE
       if(IamRoot) call tell(Iam,'skip back to AANA with do_observer =',do_observer)
#endif
       RETURN_(ESMF_SUCCESS)
     endif

     call gsimain_run(  init_pass=(L==nfldsig), &
     			last_pass=(L==nfldsig_all)) 	! if in "observer" mode
#ifdef VERBOSE
     call tell(Iam,"returned from gsimain_run()")
#endif

   else

     if(.not. do_analysis) then
#ifdef VERBOSE
       if(IamRoot) call tell(Iam,'skip back to AANA with do_analysis =',do_analysis)
#endif
       RETURN_(ESMF_SUCCESS)
     endif

     call gsimain_run( init_pass=.true.,last_pass=.true.)
#ifdef VERBOSE
     call tell(Iam,"returned from gsimain_run()")
#endif

!  Copy analysis to export state
!  -----------------------------
     call GSI_GridCompCopyInternal2Export_(ntguessig)
#ifdef VERBOSE
     call tell(Iam,"returned from GSI_GridCompCopyInternal2Export_(), ntguessig=",ntguessig)
#endif

   endif

!  Done
!  ----
#ifdef VERBOSE
   if(IamRoot) call tell(Iam,'Exiting ...')
#endif

   RETURN_(ESMF_SUCCESS)

   CONTAINS

!-------------------------------------------------------------------------
   subroutine GSI_GridCompGetAlarms_(GENSTATE,nfldsig_all)
!-------------------------------------------------------------------------
   type(MAPL_MetaComp),intent(inout)   :: GENSTATE  ! GEOS Generic state
   integer,intent(in)               :: nfldsig_all

   type(ESMF_Alarm), pointer        :: ALARM(:)
   type(ESMF_Time)                  :: CurrTime
   type(ESMF_Time)                  :: AlaTime
   integer                          :: atime
   integer                          :: yy, mm, dd, hh, mn, sec, n
   character*2                      :: idxtime
   character(len=*), parameter :: IAm='GSI_GridCompGetAlarms'
                                                                                                                                
! start
                                                                                                                                
   call ESMF_ClockGet (clock, currTime=currTime, rc=STATUS)
   VERIFY_(STATUS)
   call ESMF_TimeGet(currTime, yy=YY, mm=MM, dd=DD, h=HH, m=MN, s=SEC, rc=status)
   VERIFY_(STATUS)
   if(IamRoot) then
      write(6,'(a,1x,i4.4,5(a,i2.2))') trim(Iam)//" The GSIgc RUN current TIME is ", &
                                      YY, "/", MM, "/", DD, " ", HH, ":", MN, ":", SEC
   end if
                                                                                                                                
!!$ Is this part of code doing anything useful?  It seems ALARM is first allocated
!!$ and (shallow) assigned to 'some-bkg' and 'lat-bkg' alarms of GENSTATE.  Then
!!$ throughed away after geting one of alarm for its ESMF_Time value, known only
!!$ locally, as AlaTime.
!!$ 

!  get alarm at ana-t

   allocate(ALARM(nfldsig_all), stat=STATUS); VERIFY_(STATUS)
                                                                                                                                
   do atime=1,nfldsig_all-1
     write(idxtime,'(i2)',iostat=STATUS) atime
     aname = "some-bkg " // idxtime
     VERIFY_(STATUS)
     call MAPL_StateAlarmGet(GENSTATE, ALARM(atime), NAME=aname, RC=STATUS)
     VERIFY_(STATUS)
   end do

   call MAPL_StateAlarmGet(GENSTATE, ALARM(nfldsig_all), NAME='last-bkg', RC=STATUS)
   VERIFY_(STATUS)
   call ESMF_AlarmGet(ALARM(nfldsig_all), ringTime=AlaTime, rc=status)
   VERIFY_(STATUS)
                                                                                                                                
   deallocate(ALARM, stat=STATUS); VERIFY_(STATUS)
   RETURN_(ESMF_SUCCESS)


   end subroutine GSI_GridCompGetAlarms_

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: GSI_GridCompGetPointers_  -- get pointers

! !INTERFACE:

   subroutine GSI_GridCompGetPointers_()

   implicit none

! !DESCRIPTION: get pointers to fields in import and export states
!
! !REVISION HISTORY:
!
!  19Mar2007 Todling  GSI no longer handles log(ps)
!  20Apr2007 Todling  Added phis(dhs) to export state
!  22Apr2007 Todling  Properly naming u/v/tv fields in file
!
!EOPI
!-------------------------------------------------------------------------
!   type(ESMF_Grid)                   :: GEOSGrid 
!   type(ESMF_Field)                  :: GEOSFld

   character(len=*), parameter :: IAm='GSI_GridCompGetPointers_'

   integer(i_kind) :: nt
   character(len=ESMF_MAXSTR) :: cvar
    

! imports
! -------

   call ESMFL_StateGetPointerToData(import, hsp   , 'phis',   rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, psp,     'ps',    rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, up,       'u',    rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, vp,       'v',    rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, tp,       'tv',   rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, qp,    'sphu',    rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, ozp,   'ozone',   rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, qimr,  'qitot',   rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, qlmr,  'qltot',   rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, tskp,     'ts',   rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, snop,  'SNOWDP',  rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, soqp,  'GWETTOP', rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, sotp,  'TSOIL1',  rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, frland,   'frland',     rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, frlandice,'frlandice',  rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, frlake,   'frlake',     rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, frocean,  'frocean',    rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, frseaice,'frseaice',    rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(import, sz0p,     'Z0M',  rc=STATUS)
   VERIFY_(STATUS)
   if(GsiGridType==0) then
      call ESMFL_StateGetPointerToData(import, u10p,  'U10M', rc=STATUS)
      VERIFY_(STATUS)
      call ESMFL_StateGetPointerToData(import, v10p,  'V10M', rc=STATUS)
      VERIFY_(STATUS)
   else ! f10m is provided in a file and stored in V10M
      call ESMFL_StateGetPointerToData(import, f10p,  'V10M', rc=STATUS)
      VERIFY_(STATUS)
   end if

! Chemistry tracer imports
! When proper connection w/ Tracer Bundel is made
! the following won't be necessary
! -----------------------------------------------
   do nt=1,ntgases
      cvar = trim(tgases(nt))
      select case (cvar)
         case ( 'co' )
            call ESMFL_StateGetPointerToData(import, cop, trim(cvar), rc=STATUS)
            VERIFY_(STATUS)
         case ('co2')
            call ESMFL_StateGetPointerToData(import, co2p, trim(cvar), rc=STATUS)
            VERIFY_(STATUS)
         case default
            if(mype==0) write(6,*) trim(Iam), ': no such chem variable, aborting ...'
            status = 1
            VERIFY_(STATUS)
      end select
   enddo

! exports
! -------

   call ESMFL_StateGetPointerToData(export, dhs,             'phis',  alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dps,               'ps',  alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dts,               'ts',  alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dfrland,        'frland', alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dfrlandice,  'frlandice', alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dfrlake,        'frlake', alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dfrocean,      'frocean', alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dfrseaice,    'frseaice', alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, ddp,             'delp',  alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, du,                 'u',  alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dv,                 'v',  alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dt,                 'tv', alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dq,              'sphu',  alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, doz,             'ozone', alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dqimr,           'qitot', alloc=.true., rc=STATUS)
   VERIFY_(STATUS)
   call ESMFL_StateGetPointerToData(export, dqlmr,           'qltot', alloc=.true., rc=STATUS)
   VERIFY_(STATUS)

! Chemistry tracer exports
! When proper connection w/ Tracer Bundel is made
! the following won't be necessary
! -----------------------------------------------
   do nt=1,ntgases
      cvar = trim(tgases(nt))
      select case (cvar)
         case ('co')
            call ESMFL_StateGetPointerToData(export, dcop, trim(cvar), alloc=.true., rc=STATUS)
            VERIFY_(STATUS)
         case ('co2')
            call ESMFL_StateGetPointerToData(export, dco2p, trim(cvar), alloc=.true., rc=STATUS)
            VERIFY_(STATUS)
         case default
            if(mype==0) write(6,*) trim(Iam), ': no such chem variable, aborting ...'
            status = 1
            VERIFY_(STATUS)
      end select
   enddo


   end subroutine GSI_GridCompGetPointers_

   subroutine Scale_Import_()
   character(len=*), parameter :: IAm='GSI_GridComp.Scale_Import_'

   where ( psp /= MAPL_UNDEF )
           psp = psp * kPa_per_Pa       ! convert ps to cb
   endwhere
   if(GsiGridType==0) then
      where ( hsp /= MAPL_UNDEF )
              hsp = hsp / grav          ! convert geop h to h
      endwhere
      where ( ozp /= MAPL_UNDEF )
              ozp = ozp * PPMV2DU       ! convert ozone unit
      endwhere
   endif

   end subroutine Scale_Import_ 
!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: GSI_GridCompCopyImportDyn2Internal  -- get vars from impGSI

! !INTERFACE:

   subroutine GSI_GridCompCopyImportDyn2Internal_(lit)

   use mpeu_util, only: tell
   implicit none

   integer, intent(in) :: lit ! logical index of first guess time level to copy

! !DESCRIPTION: Extract all upper air fields from GSI import state to
!                define the corresponding GSI internal variables
!
! !REVISION HISTORY:
!
!   17Apr2007 Todling  Swap vertical on way in
!   10Mar2009 Todling  Handle qi/qlmr instead of qctot
!
!EOPI
!-------------------------------------------------------------------------

! local variables

   character(len=*), parameter    :: &
            IAm='GSI_GridCompCopyImportDyn2Internal_'
   integer(i_kind) :: it,nn,nt,itguessig
   integer(i_kind) :: irank, ipnt
   character(len=ESMF_MAXSTR) :: cvar

! start   

   if(IamRoot) print *,trim(Iam),': Copy contents of import to internal state, it= ', lit

! The upper air fields from the GEOS import state are already decomposed 
! for the GSI. A final operation before updating the corresponding GSI 
! internal fields is to transpose the horizontal grid  from IJK (GEOS) 
! to JIK (GSI). TODO: this could be done during coupling.

   call Scale_Import_()

   nfldsig_now = lit
   if(lit > nfldsig) then	! rotate the storage

     nfldsig_now = nfldsig	! take over the last storage slot

     do nn = 1,nfldsig-1	! by first moving data slots up

       hrdifsig(nn) = hrdifsig(nn+1)

       ges_z (:,:,nn) = ges_z (:,:,nn+1)
       ges_ps(:,:,nn) = ges_ps(:,:,nn+1)

       ges_u   (:,:,:,nn) = ges_u   (:,:,:,nn+1)
       ges_v   (:,:,:,nn) = ges_v   (:,:,:,nn+1)
       ges_tv  (:,:,:,nn) = ges_tv  (:,:,:,nn+1)
       ges_q   (:,:,:,nn) = ges_q   (:,:,:,nn+1)
       ges_oz  (:,:,:,nn) = ges_oz  (:,:,:,nn+1)
       ges_cwmr(:,:,:,nn) = ges_cwmr(:,:,:,nn+1)
       if(ntgases>0) GSI_chem_bundle(nn)= GSI_chem_bundle(nn+1)
     enddo
   endif

   it = nfldsig_now
   ntguessig = ntguessig_ref - lit + it
   itguessig = max(1,min(ntguessig,nfldsig))
   if(IamRoot) call tell(Iam, &
   	'reset ntguessig, (from, to) =',(/ntguessig,itguessig/))
   ntguessig = itguessig

#ifdef VERBOSE
   call tell(Iam,'nfldsig.it  =',it)
   call tell(Iam,'nfldsig.lit =',lit)
   call tell(Iam,'nfldsig     =',nfldsig)
#endif
   hrdifsig(it)=hrdifsig_all(lit)
#ifdef VERBOSE
   call tell(Iam,'hrfldsig(it) =',hrdifsig(it))
   call tell(Iam,"ntguessig =",ntguessig)
#endif

! Terrain:

   call GSI_GridCompSwapIJ_(hsp,ges_z(:,:,it))

! sfc pressure:

   call GSI_GridCompSwapIJ_(psp,ges_ps(:,:,it))

! zonal wind

   call GSI_GridCompSwapIJ_(up,ges_u(:,:,:,it))

! meridional wind

   call GSI_GridCompSwapIJ_(vp,ges_v(:,:,:,it))

! (Virtual) temperature

   call GSI_GridCompSwapIJ_(tp,ges_tv(:,:,:,it))

! Water vapor mixing ratio

   call GSI_GridCompSwapIJ_(qp,ges_q(:,:,:,it))

! Ozone mixing ratio

   call GSI_GridCompSwapIJ_(ozp,ges_oz(:,:,:,it))

! Cloud liquid water content and cloud ice water content are added into
! cloud condensate mixing ratio - internal GSI field
   where(qimr.ne.MAPL_UNDEF .and. qlmr.ne.MAPL_UNDEF)
         qimr=qimr+qlmr
   elsewhere
         qimr=MAPL_UNDEF
   endwhere 
   call GSI_GridCompSwapIJ_(qimr,ges_cwmr(:,:,:,it))

!  Handle trace gases
!  ------------------
   do nt = 1,ntgases
      cvar=trim(tgases(nt))
      call GSI_BundleGetPointer ( GSI_chem_bundle(it), cvar, ipnt, status, irank=irank )
      VERIFY_(STATUS)
      if(irank/=3) status=1 ! better be 3d
      VERIFY_(STATUS)
      select case (cvar) ! unfortunately this package still separates chem tracers
         case ('co')
           call GSI_GridCompSwapIJ_(cop,GSI_chem_bundle(it)%r3(ipnt)%q)
         case ('co2')
           call GSI_GridCompSwapIJ_(co2p,GSI_chem_bundle(it)%r3(ipnt)%q)
      end select
#ifdef UPAverbose
       call guess_grids_stats(cvar, GSI_chem_bundle(it)%r3(ipnt)%q, mype)
#endif
   enddo

! simple statistics

#ifdef UPAverbose
   call guess_grids_stats('GCges_z',    ges_z   (:,:,  it), mype)
   call guess_grids_stats('GCges_ps',   ges_ps  (:,:,  it), mype)
   call guess_grids_stats('GCges_u',    ges_u   (:,:,:,it), mype)
   call guess_grids_stats('GCges_v',    ges_v   (:,:,:,it), mype)
   call guess_grids_stats('GCges_tv',   ges_tv  (:,:,:,it), mype)
   call guess_grids_stats('GCges_q',    ges_q   (:,:,:,it), mype)
   call guess_grids_stats('GCges_oz',   ges_oz  (:,:,:,it), mype)
   call guess_grids_stats('GCges_cwmr', ges_cwmr(:,:,:,it), mype)
#endif

#ifdef VERBOSE
   if(IamRoot) print *,trim(Iam),': Complete copy contents of import to internal state, it= ', lit
#endif

   end subroutine GSI_GridCompCopyImportDyn2Internal_

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: GSI_GridCompComputeVorDiv_

! !INTERFACE:

   subroutine GSI_GridCompComputeVorDiv_(lit)

#ifdef _GMAO_FVGSI_
   use m_ggGradient,only : ggGradient
   use m_ggGradient,only : ggGradient_init,clean
   use m_ggGradient,only : ggDivo
#else /* _GMAO_FVGSI_ */
   use compact_diffs, only: cdiff_created
   use compact_diffs, only: cdiff_initialized
   use compact_diffs, only: create_cdiff_coefs
   use compact_diffs, only: inisph
   use compact_diffs, only: uv2vordiv
   use xhat_vordivmod, only: xhat_vordiv_calc2
#endif /* _GMAO_FVGSI_ */

   implicit none

   integer, intent(in) :: lit ! logical index of first guess time level to copy

! !DESCRIPTION: vorticity-divergence calculation 
!
! !REVSION HISTORY:
!
!  18Feb2009 Todling - let GSI calc of vor/div when GMAO util not available
!
!EOPI
!-------------------------------------------------------------------------

! local variables

   character(len=*), parameter :: IAm='GSI_GridCompComputeVorDiv_'
   integer(i_kind) :: it,nn
#ifdef _GMAO_FVGSI_
   type(ggGradient) :: gr
   integer :: iGdim,iGloc,iGlen
   integer :: jGdim,jGloc,jGlen
   real(r_kind),dimension(:,:,:), pointer :: up8
   real(r_kind),dimension(:,:,:), pointer :: vp8
#endif /* _GMAO_FVGSI_ */

! start

   if(IamRoot.and.verbose) print *,trim(Iam),': Compute vorticity and divergence'

   it = nfldsig_now
   if(lit > nfldsig) then	! rotate the storage
     do nn=1,nfldsig-1
       ges_div(:,:,:,nn) = ges_div(:,:,:,nn+1)
       ges_vor(:,:,:,nn) = ges_vor(:,:,:,nn+1)
     enddo
   endif

#ifdef _GMAO_FVGSI_
   allocate(up8 (lat2,lon2,nsig), &
            vp8 (lat2,lon2,nsig), &
            stat=STATUS)
   VERIFY_(STATUS)

! define -real8 and swapped- uwnd,vwnd needed by ggDivo
  
   call GSI_GridCompSwapIJ_(up,up8)
   call GSI_GridCompSwapIJ_(vp,vp8)

   ges_vor(:,:,:,it) = zero
   ges_div(:,:,:,it) = zero

   iGloc=jstart(myPE+1)
   iGlen=jlon1 (myPE+1)
   jGloc=istart(myPE+1)
   jGlen=ilat1 (myPE+1)

   call ggGradient_init(gr,nlon,rlats(1:nlat),	&
  	iGloc,iGlen,jGloc,jGlen,nsig, mpi_comm_world)

   if(IamRoot.and.verbose) print *,Iam,': call ggDivo '
   call ggDivo(gr,		&
	up8  (:,:,:),	        &	! in: u
	vp8  (:,:,:),	        &	! in: v
	ges_div(:,:,:,it),	&	! div(u,v)
	ges_vor(:,:,:,it),	&	! vor(u,v)
	mpi_comm_world)		        ! communicator

   call clean(gr)
   deallocate(up8, vp8, stat=STATUS)
   VERIFY_(STATUS)

#else /* _GMAO_FVGSI_ */
   if(.not.cdiff_created()) call create_cdiff_coefs()
   if(.not.cdiff_initialized()) call inisph(rearth,rlats(2),wgtlats(2),nlon,nlat-2)
   call xhat_vordiv_calc2 (ges_u(:,:,:,it),ges_v(:,:,:,it),ges_vor(:,:,:,it),ges_div(:,:,:,it))
!!   call destroy_cdiff_coefs
#endif /* _GMAO_FVGSI_ */

! simple statistics

#ifdef UPAverbose 
   call guess_grids_stats('GCges_vor', ges_vor(:,:,:,it), mype)
   call guess_grids_stats('GCges_div', ges_div(:,:,:,it), mype)
#endif

   end subroutine GSI_GridCompComputeVorDiv_

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: GSI_GridCompCopyImportSfc2Internal_

! !INTERFACE:

   subroutine GSI_GridCompCopyImportSfc2Internal_(lit)

   implicit none

   integer, intent(in) :: lit ! logical index of first guess time level to copy

! !DESCRIPTION: Extract all surface fields from GSI import state to
!                define the corresponding GSI internal variables
! !REVISION HISTORY:
! 
!  24Apr2007 Todling  Renamed tskin to ts (getting from upa bkg file)
!  08May2007 Todling  Corrected calculation of fact10; do not replace
!                     soil temp w/ tskin - leave undefs in place; revised
!                     calculation of isli
!
!EOPI
!-------------------------------------------------------------------------

! local variables

   character(len=*), parameter :: &
            IAm='GSI_GridCompCopyImportSfc2Internal_'
   real                              :: wspd
   integer,allocatable,dimension(:,:):: islip
   integer(i_kind) :: it,nn,itguessfc

! start

   if(IamRoot) print *,trim(Iam),': Copy contents of import to internal state, it= ',lit

   nfldsfc_now = lit
   if(lit > nfldsfc) then
     nfldsfc_now = nfldsfc	! take over the last storage slot

     do nn=1,nfldsfc-1
       hrdifsfc(nn) = hrdifsfc(nn+1)

       dsfct(:,:,nn) = dsfct(:,:,nn+1)
        sfct(:,:,nn) =  sfct(:,:,nn+1)
	sno (:,:,nn) =  sno (:,:,nn+1)

       soil_moi (:,:,nn) = soil_moi (:,:,nn+1)
       soil_temp(:,:,nn) = soil_temp(:,:,nn+1)
       sfc_rough(:,:,nn) = sfc_rough(:,:,nn+1)

        isli(:,:,nn) =   isli(:,:,nn+1)
      fact10(:,:,nn) = fact10(:,:,nn+1)
     enddo
   endif

   it = nfldsfc_now
   ntguessfc = ntguessfc_ref - lit + it
   itguessfc = max(1,min(ntguessfc,nfldsfc))
   if(IamRoot) call tell(Iam, &
   	'reset ntguessfc, (from, to) =',(/ntguessfc,itguessfc/))
   ntguessfc = itguessfc

#ifdef VERBOSE
   call tell(Iam,'nfldsfc.it  =',it)
   call tell(Iam,'nfldsfc.lit =',lit)
#endif
   hrdifsfc(it)=hrdifsfc_all(lit)
#ifdef VERBOSE
   call tell(Iam,'hrfldsfc(it) =',hrdifsfc(it))
   call tell(Iam,"ntguessfc =",ntguessfc)
#endif

! The surface fields from the GEOS import state are already decomposed 
! for the GSI. A final operation before updating the corresponding GSI 
! internal fields is to transpose the horizontal grid  from IJK (GEOS) 
! to JIK (GSI).

! For consistency with the GMAO_INTFC results the surface fields have been
! "processed" in a similar fashion using undef_2ssi

! Skin temperature

   dsfct(:,:,it) = zero

   call GSI_GridCompSwapIJ_(tskp,sfct(:,:,it))

! Snow depth
   call undef_2ssi(snop,MAPL_UNDEF,trim(Iam),	&
        verb=.true.,vname='SNOWDP')
   call undef_2ssi(snop,UNDEF_SNOW_DEP,trim(Iam),	&
        verb=.false.,vname='SNOWDP')
   where(snop==UNDEF_SSI_)
      snop=0.
   elsewhere(snop <zero)
      snop=0.
   endwhere
   call GSI_GridCompSwapIJ_(snop,sno(:,:,it))

! Soil moisture
   call undef_2ssi(soqp,MAPL_UNDEF,trim(Iam),	&
        verb=.false.,vname='GWETTOP')
   where(soqp == UNDEF_SSI_)	! in case of any
      soqp=1.
   elsewhere(soqp <=zero)		! why not <.05?
      ! This block would undo any :=UNDEF_SSI_ == -9.99e33
      soqp=.05
   elsewhere(soqp > one)
      soqp=1.
   endwhere
   call GSI_GridCompSwapIJ_(soqp,soil_moi(:,:,it))

! Soil temperature
   call undef_2ssi(sotp,MAPL_UNDEF,trim(Iam),	&
        verb=.false.,vname='TSOIL1')
   call undef_2ssi(sotp,UNDEF_SOIL_TEMP,trim(Iam),	&
        verb=.true.,vname='TSOIL1')
! A hack solution to work around possible undesired undef values.
   where(sotp == UNDEF_SSI_)
      sotp=tskp
   endwhere

   call GSI_GridCompSwapIJ_(sotp,soil_temp(:,:,it))

! Surface roughness
   call undef_2ssi(sz0p,MAPL_UNDEF,trim(Iam),	&
        verb=.false.,vname='Z0M')
! A hack solution to work around possible undesired undef values.
   where(sz0p == UNDEF_SSI_ .or. sz0p<zero)
      sz0p=0.
   endwhere

   call GSI_GridCompSwapIJ_(sz0p,sfc_rough(:,:,it))

! SLI mask  (adaptation from L. Takacs change elsewhere)
   allocate(islip(lon2,lat2), stat=STATUS)
   VERIFY_(STATUS)
                                            islip = 1  ! Land
   where (  frocean+frlake >= 0.6         ) islip = 0  ! Water
   where (  islip==0 .and. frseaice > 0.5 ) islip = 2  ! Ice
   where (  islip==0 .and.   tskp < 271.4 ) islip = 2  ! Ice

! Set output (export) land/water/etc information
! RT: Is there a more decent way to set outputs from inputs in ESMF?
!     Why can't I use fr-arrays directly?
!   if ( it==ntguessfc ) then
!
! Check stored hrdifsfc(:) for expected time (J.G.)
   if ( hrdifsfc(it)==hrdifsfc_all(ntguessfc_ref) ) then
       dts        = tskp
       dfrland    = frland
       dfrlandice = frlandice
       dfrlake    = frlake
       dfrocean   = frocean
       dfrseaice  = frseaice
   endif

   call GSI_GridCompSwapIJ_(islip,isli(:,:,it))

   deallocate(islip, stat = STATUS)
   VERIFY_(STATUS)

   if(GSIGridType==0) then ! we need to calculate 10m wind factors
                           ! else they are provided in the import state

! F10M is derived from U10M,V10m,U,V and defined through GSI's
! internal variable fact10 (declared in guess_grids.f90)

   call undef_2ssi(u10p,MAPL_UNDEF,trim(Iam),	&
        verb=.false.,vname='U10M')
   call undef_2ssi(v10p,MAPL_UNDEF,trim(Iam),	&
        verb=.false.,vname='V10M')
#ifdef SFCverbose
   print *,Iam,':  u10m (PE,min,max,sum): ',mype,minval(u10p),maxval(u10p),sum(u10p)
   print *,Iam,':  v10m (PE,min,max,sum): ',mype,minval(v10p),maxval(v10p),sum(v10p)
#endif
   allocate(f10p(lon2,lat2), stat=STATUS)
   VERIFY_(STATUS)

   where(u10p == UNDEF_SSI_ .or. v10p == UNDEF_SSI_)	        ! in case there is any
     f10p(:,:)=one ! RT: this would happen over the halo only anyway 
   elsewhere
     f10p(:,:)=sqrt(u10p(:,:)*u10p(:,:)+v10p(:,:)*v10p(:,:))
   endwhere

   call GSI_GridCompSwapIJ_(f10p,fact10(:,:,it))

!       zero somewhere.  To compute factor at 10m, wind speed at the
!       bottom of the atmosphere is needed, which is level 1 of the
!       GSI grid.  It is debateable what fact10 should be where the
!       denominator is zero.  I don't have any good solution, but
!       simply mimic what has been done before.
                                                                                                                                      
        do j=lbound(fact10,2),ubound(fact10,2)
           do i=lbound(fact10,1),ubound(fact10,1)
              wspd=sqrt(ges_u(i,j,1,it)*ges_u(i,j,1,it) + &
                   ges_v(i,j,1,it)*ges_v(i,j,1,it))
                                                                                                                                      
!             Reset fact10 if fact10 > zero
              if(zero < fact10(i,j,it)) then
                 if(fact10(i,j,it) <= wspd) then
                    fact10(i,j,it)=fact10(i,j,it)/wspd
                 else
                    fact10(i,j,it)=one
                 endif
              endif
           end do
        end do
     deallocate(f10p, stat = STATUS)
     VERIFY_(STATUS)

   end if

! simple statistics

#ifdef SFCverbose
   call guess_grids_stats('GCsfct',      sfct     (:,:,it), mype)
   call guess_grids_stats('GCsno',       sno      (:,:,it), mype)
   call guess_grids_stats('GCsoil_moi',  soil_moi (:,:,it), mype)
   call guess_grids_stats('GCsfc_rough', sfc_rough(:,:,it), mype)
   call guess_grids_stats('GCsoil_temp', soil_temp(:,:,it), mype)
   call guess_grids_stats('GCfact10',    fact10   (:,:,it), mype)
#endif


   end subroutine GSI_GridCompCopyImportSfc2Internal_

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: GSI_GridCompGetNCEPsfcFromFile_

! !INTERFACE:

   subroutine GSI_GridCompGetNCEPsfcFromFile_(lit)

   implicit none

   integer, intent(in) :: lit ! logical index of first guess time level to copy

! !DESCRIPTION: read NCEP global surface fields, vegetation fraction,
!               vegetation and soil type,  from a file.
!
!EOPI
!-------------------------------------------------------------------------

! local variables

   integer                              :: glon,glat,glat2
   real(4), allocatable, dimension(:,:) :: vfrbuf
   real(4), allocatable, dimension(:,:) :: vtybuf
   real(4), allocatable, dimension(:,:) :: stybuf
   real(4), allocatable, dimension(:,:) :: sfcbuf
   real(4), allocatable, dimension(:,:) :: sfcbufpp
   real(4) yhour
   integer version
   integer,dimension(4):: igdate

   character(len=*), parameter    :: &
            IAm='GSI_GridCompGetNCEPsfcFromFile_'

   integer(i_kind) :: it,nn

! start

   if(IamRoot.and.verbose) print *,trim(Iam),': read NCEP global surface fields from a file'

   if(lit > nfldsfc) then
     do nn=1,nfldsfc-1
        veg_type(:,:,nn) = veg_type(:,:,nn+1)
       soil_type(:,:,nn) =soil_type(:,:,nn+1)   
        veg_frac(:,:,nn) = veg_frac(:,:,nn+1)
     enddo
   endif
   it = nfldsfc_now

! Now we use hinterp to regrid ncepsfc fields as well as swap lats, add poles
! and flip longitudes before using hinterp (also flip lons after hinterp).
!
   if(IamRoot) then

      ! NCEP surface fields are defined on a Gaussian grid...
      open(34,file='ncepsfc', form='unformatted')
      read (34) 
      read(34) yhour,igdate,glon,glat2,version
      close(34)
      if(verbose) print *,trim(Iam),': ncepsfc hour/date  : ',yhour,' / ',igdate
      if(verbose) print *,trim(Iam),': ncepsfc fields dims: ',glon,' x ',glat2
      glat=glat2+2 

      allocate(sfcbuf(glon,glat2),  & ! ncepsfc fields buffer
               sfcbufpp(glon,glat), & ! same but with poles added
               stat=STATUS)  
      VERIFY_(STATUS)
      allocate(vtybuf(nlon,nlat), &
               stybuf(nlon,nlat), &
               vfrbuf(nlon,nlat), & 
               stat=STATUS)
      VERIFY_(STATUS)

      call ncep_rwsurf_ ( .false., 'ncepsfc', glat2, glon, &
                          STATUS, jrec=12, fld=sfcbuf)
      VERIFY_(STATUS)
      call GSI_GridCompSP2NP_(sfcbufpp,sfcbuf)
      call GSI_GridCompFlipLons_(sfcbufpp)
      call hinterp ( sfcbufpp ,glon, glat   , &
                      vfrbuf, nlon, nlat, 1, &
                      UNDEF_SSI_)
      call GSI_GridCompFlipLons_(vfrbuf)
      where(vfrbuf<0.0)
        vfrbuf=0.0
      end where
      where(vfrbuf>1.0)
        vfrbuf=1.0
      end where

      call ncep_rwsurf_ ( .false., 'ncepsfc', glat2, glon, &
                          STATUS, jrec=15, fld=sfcbuf)
      VERIFY_(STATUS)
      call GSI_GridCompSP2NP_(sfcbufpp,sfcbuf)
      call GSI_GridCompFlipLons_(sfcbufpp)
      call hinterp ( sfcbufpp ,glon, glat   , &
                      vtybuf, nlon, nlat, 1, &
                      UNDEF_SSI_)
      call GSI_GridCompFlipLons_(vtybuf)

      call ncep_rwsurf_ ( .false., 'ncepsfc', glat2, glon, &
                          STATUS, jrec=16, fld=sfcbuf)
      VERIFY_(STATUS)
      call GSI_GridCompSP2NP_(sfcbufpp,sfcbuf)
      call GSI_GridCompFlipLons_(sfcbufpp)
      call hinterp ( sfcbufpp ,glon, glat   , &
                      stybuf, nlon, nlat, 1, &
                      UNDEF_SSI_)
      call GSI_GridCompFlipLons_(stybuf)

      deallocate(sfcbuf, sfcbufpp, stat=STATUS)
      VERIFY_(STATUS)

   end if
   
   allocate(vfrp(lon2,lat2), &
            vtyp(lon2,lat2), &
            styp(lon2,lat2), &
            stat=STATUS)
   VERIFY_(STATUS)

   call ArrayScatter(vfrp, vfrbuf, GSIGrid, hw=hw, rc=STATUS)
   VERIFY_(STATUS)
   call ArrayScatter(vtyp, vtybuf, GSIGrid, hw=hw, rc=STATUS)
   VERIFY_(STATUS)
   call ArrayScatter(styp, stybuf, GSIGrid, hw=hw, rc=STATUS)
   VERIFY_(STATUS)

!  Dirty fix to get rid of complete insanity (but still not correct)
!  -----------------------------------------
   where(vtyp <= zero) vtyp = one
   where(styp <= zero) styp = one

   call GSI_GridCompSwapIJ_(vtyp, veg_type(:,:,it))
   call GSI_GridCompSwapIJ_(styp,soil_type(:,:,it))   
   call GSI_GridCompSwapIJ_(vfrp, veg_frac(:,:,it))

   deallocate(vtyp, vfrp, styp, stat = STATUS)
   VERIFY_(STATUS)
   if(IamRoot) then
      deallocate(vtybuf, stybuf, vfrbuf, stat=STATUS)
      VERIFY_(STATUS)
   end if

#ifdef SFCverbose
   call guess_grids_stats('GCsoil_type', soil_type(:,:,it), mype) 
   call guess_grids_stats('GCveg_type',  veg_type (:,:,it), mype)
   call guess_grids_stats('GCveg_frac',  veg_frac (:,:,it), mype)
#endif

   end subroutine GSI_GridCompGetNCEPsfcFromFile_
   
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: GSI_GridCompCopyInternal2Export_  -- copy vars to expGSI

! !INTERFACE:

   subroutine GSI_GridCompCopyInternal2Export_(lit)

   implicit none

   integer, intent(in) :: lit ! logical index of first guess time level to copy

! !DESCRIPTION: Extract GSI internal variables and put into GSI
!                export state.
!
! !TO DO: for GSIsa export state contains analysis fields. However it can 
!         also contain increments. This is controlled in GSI routine 
!         update_guess.f90
!
! !REVISION HISTORY:
!
!   19Mar2007 Todling  GSI no longer uses log(ps)
!   17Apr2007 Todling  Swap vertical on way out
!   20Apr2007 Todling  Added phis(ges_z) to export state
!   05Dec2008 Todling  Update SST before writing out
!   10Mar2009 Todling  Place cwmr into qimr; zero out qltot
!   12Mar2009 Todling  Revamp skin temperature imp/exp
!   20Jan2010 Todling  Fix for dealing w/ delp when writing out ainc
!   21Apr2010 Todling  Add trace gases - eventually own GridComp
!
!EOPI
!-------------------------------------------------------------------------

! local variables

   character(len=*), parameter    :: &
            IAm='GSI_GridCompCopyInternal2Export_'
   integer kk
   real(r_kind),allocatable, dimension(:,:) :: wrk
   integer :: nt,it
   integer(i_kind) :: irank, ipnt
   character(len=ESMF_MAXSTR) :: cvar

   if(IamRoot) print *,trim(Iam),': Copy contents of internal state to export, it= ',lit

   it = 1
   do it=1,nfldsig
     if (hrdifsig(it) == hrdifsig_all(lit)) exit
   enddo

! simple statistics

#ifdef UPAverbose 
   call guess_grids_stats('GCges_z',    ges_z   (:,:,  it), mype)
   call guess_grids_stats('GCges_ps',   ges_ps  (:,:,  it), mype)
   call guess_grids_stats('GCges_u',    ges_u   (:,:,:,it), mype)
   call guess_grids_stats('GCges_v',    ges_v   (:,:,:,it), mype)
   call guess_grids_stats('GCges_tv',   ges_tv  (:,:,:,it), mype)
   call guess_grids_stats('GCges_q',    ges_q   (:,:,:,it), mype)
   call guess_grids_stats('GCges_oz',   ges_oz  (:,:,:,it), mype)
   call guess_grids_stats('GCges_cwmr', ges_cwmr(:,:,:,it), mype)
#endif

!  If so, save full skin temperature field to work array
!  -----------------------------------------------------
   if ( .not. lwrtinc ) then
       allocate(wrk(size(dts,1),size(dts,2)))
       wrk = dts
   endif

   call GSI_GridCompSwapJI_(dhs,  ges_z   (:,:,  it))
   call GSI_GridCompSwapJI_(dps,  ges_ps  (:,:,  it))
   call GSI_GridCompSwapJI_(dts,  dsfct   (:,:,  it))
   call GSI_GridCompSwapJI_(du,   ges_u   (:,:,:,it))
   call GSI_GridCompSwapJI_(dv,   ges_v   (:,:,:,it))
   call GSI_GridCompSwapJI_(dt,   ges_tv  (:,:,:,it))
   call GSI_GridCompSwapJI_(dq,   ges_q   (:,:,:,it))
   call GSI_GridCompSwapJI_(doz,  ges_oz  (:,:,:,it))
   call GSI_GridCompSwapJI_(dqimr,ges_cwmr(:,:,:,it))
   dqlmr = zero

!  Now handle trace gases
!  ----------------------
   do nt=1,ntgases
      cvar = trim(tgases(nt))
      call GSI_BundleGetPointer ( GSI_chem_bundle(it), cvar, ipnt, status, irank=irank )
      VERIFY_(STATUS)
      if(irank/=3) status=1 ! better be 3d
      VERIFY_(STATUS)
      select case (cvar)  ! unfortunately this package still separates varibles chem tracers
        case('co')
           call GSI_GridCompSwapJI_(dcop,GSI_chem_bundle(it)%r3(ipnt)%q)
        case ('co2')
           call GSI_GridCompSwapJI_(dco2p,GSI_chem_bundle(it)%r3(ipnt)%q)
      end select
   enddo

!  SST needs to be updated before being written out 
!  ------------------------------------------------
   if ( .not. lwrtinc ) then
       where (dts/=MAPL_UNDEF) dts = wrk + dts
       deallocate(wrk)
   endif
#ifdef SFCverbose
   print *,Iam,':  dts (PE,min,max,sum): ',mype,minval(dts),maxval(dts),sum(dts)
#endif

   call UnScale_Export_()

   if ( lwrtinc ) then
        do k=1,nsig
           kk = nsig-k+1
           ddp(:,:,kk)=(bk5(k+1)-bk5(k))*dps(:,:)
        end do
   else
        do k=1,nsig
           kk = nsig-k+1
           ddp(:,:,kk)=Pa_per_kPa*(ak5(k+1)-ak5(k)) +  &
                                  (bk5(k+1)-bk5(k))*dps(:,:)
        end do
   endif

!  If ak(1)+bk(1)*ps is the surface, then ak(2)+bk(2)*ps would
!  be smaller, then dp would be negtive and should be reset to -dp.

   if(sfcFirst_(Pa_per_kPa*ak5,bk5,maxval(dps),lwrtinc)) ddp(:,:,:)=-ddp(:,:,:)

   end subroutine GSI_GridCompCopyInternal2Export_

   subroutine UnScale_Export_()
   character(len=*), parameter :: IAm='GSI_GridComp.UnScale_Export_'
                      dps = dps  * Pa_per_kPa
   if(GsiGridType==0) dhs = grav * dhs
   if(GsiGridType==0) doz = doz  / PPMV2DU
   end subroutine UnScale_Export_

!-------------------------------------------------------------------------
   subroutine GSI_GridCompSetAnaTime_()
!-------------------------------------------------------------------------
   character(len=*), parameter :: IAm='GSI_GridCompSetAnaTime'
   integer(i_kind)              :: nmin_an
   integer(i_kind),dimension(8) :: ida,jda
   integer(i_kind)              :: iyr,ihourg
   integer(i_kind),dimension(4) :: idate4
   character(len=8)             :: cymd
   character(len=6)             :: chms
   real(r_single)               :: hourg4
   real(r_kind),dimension(5)    :: fha

! start

   if(IamRoot) print *,trim(Iam),': Set GSI analysis time '

   ihourg = MYHOURG
   idate4 = MYIDATE

   iyr=idate4(4)
   if ( iyr>=0 .and. iyr<=99 ) then
      if(iyr>51) then
         iyr=iyr+1900
      else
         iyr=iyr+2000
      end if
   end if
   fha(:)=0.0; ida=0; jda=0
   fha(2)=ihourg    ! relative time interval in hours
   ida(1)=iyr       ! year
   ida(2)=idate4(2) ! month
   ida(3)=idate4(3) ! day
   ida(4)=0         ! time zone
   ida(5)=idate4(1) ! hour
   ! Move date-time forward by nhr_assimilation hours
   call w3movdat(fha,ida,jda)

!  For the given time tag, nhr_assimilation, read input guess
!  data header to generate required grid specifications for the
!  GSI analysis grid data.
                                                                                                                                
   ! iadate(:) is for obsmod.
   iadate(1)=jda(1) ! year
   iadate(2)=jda(2) ! mon
   iadate(3)=jda(3) ! day
   iadate(4)=jda(5) ! hour
   iadate(5)=0      ! minute
   ianldate =jda(1)*1000000+jda(2)*10000+jda(3)*100+jda(5)
                                                                                                                                
!  Determine date and time at start of assimilation window
   ida(:)=0
   jda(:)=0
   fha(:)=0.0
   fha(2)=-real(min_offset/60)
   fha(3)=-real(mod(min_offset,60))
   ida(1:3)=iadate(1:3)
   ida(5:6)=iadate(4:5)
   call w3movdat(fha,ida,jda)

   ibdate(1:5)=(/jda(1),jda(2),jda(3),jda(5),jda(6)/)
   iadatebgn=jda(1)*1000000+jda(2)*10000+jda(3)*100+jda(5)

! Set the analysis time - this is output info...
! w3fs21(NCEP-w3) converts analysis time to minutes relative to a fixed date.
   call w3fs21(ibdate,nmin_an)
   iwinbgn = nmin_an

!  Determine date and time at end of assimilation window
   ida(:)=jda(:)
   jda(:)=0
   fha(:)=0.0
   if ( min_offset == 0 ) then
        fha(2)=0
   else
        fha(2)=nhr_assimilation
   endif
   call w3movdat(fha,ida,jda)
 
   iedate(1:5)=(/jda(1),jda(2),jda(3),jda(5),jda(6)/)
   iadateend=jda(1)*1000000+jda(2)*10000+jda(3)*100+jda(5)

   if(IamRoot) then ! print out some information.

     write(6,*)trim(IAm),':  Guess date is ',idate4,ihourg
     write(6,*)trim(IAm),':  Analysis date is ',iadate
     write(6,*)trim(IAm),':  Start of assimilation window ',ibdate,iadatebgn
     write(6,*)trim(IAm),':  End   of assimilation window ',iedate,iadateend

     write(cymd,'(i4.4,i2.2,i2.2)',iostat=STATUS) iadate(1),iadate(2),iadate(3)
     VERIFY_(STATUS)
     write(chms,'(i2.2,i2.2,i2.2)',iostat=STATUS) iadate(4),0,0
     VERIFY_(STATUS)
     write(6,*) trim(IAm),':        analysis yyyymmdd:hhmmss = '//cymd//':'//chms
     write(6,*) trim(IAm),':        analysis time in minutes =',nmin_an
     write(6,*) trim(IAm),':  time since start of var window =',iwinbgn
      
   endif

   end subroutine GSI_GridCompSetAnaTime_

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: GSI_GridCompSetObsNames  -- 

! !INTERFACE:

   subroutine GSI_GridCompSetObsNames_(lit)

   use mpeu_util, only: tell
   implicit none
   integer, intent(in) :: lit ! logical index of first guess time level

! !DESCRIPTION: temporary routine to set names of GSI obs data files
!
!EOPI
!-------------------------------------------------------------------------

! local variables
   integer             :: nymd, nhms, i, indx

! obs file name templates defined in GSI_GridComp.rc
! ...are there any more than 9?

   character(len=*), parameter :: IAm='GSI_GridCompSetObsNames_'
   character(len=ESMF_MAXSTR) :: DateStamp
   character(len=ESMF_MAXSTR) :: expid
   character(len=8)           :: cymd
   character(len=6)           :: chms
   integer(i_kind)            :: nobfiles
   integer(i_kind)            :: OBSfreq, OBSfreq_sc, OBSfreq_mn, OBSfreq_hr

! start
#ifdef VERBOSE
   call tell(Iam,'entered with lit =',lit)
#endif

   call ESMF_GridCompGet( gc, config=CF, RC=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( CF, expid, label ='expid:', rc = STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute( CF, OBSfreq, label ='OBS_FREQUENCY:', rc = STATUS )
   VERIFY_(STATUS)

#ifdef VERBOSE
   call tell(Iam,'returned from ESMF_ConfiGetAttribute()')
#endif

   OBSfreq_hr = OBSfreq/10000
   OBSfreq_mn = mod(OBSfreq,10000)/100
   OBSfreq_sc = mod(OBSfreq,100)
   OBSfreq_sc = OBSfreq_hr*3600 + OBSFreq_mn*60 + OBSfreq_sc
#ifdef VERBOSE
   call tell(Iam,'OBSfreq_hr =',OBSfreq_hr)
   call tell(Iam,'OBSfreq_mn =',OBSfreq_mn)
   call tell(Iam,'OBSfreq_sc =',OBSfreq_sc)
   call tell(Iam,'OBSfreq_sc =',OBSfreq_sc)
#endif

!_RT#ifdef USING_GEOS_FILE_MODEL
! temporarily we get it from iadate...
   write(cymd,'(i4.4,i2.2,i2.2)',iostat=STATUS) iadate(1),iadate(2),iadate(3)
   VERIFY_(STATUS)
   write(chms,'(i2.2,i2.2,i2.2)',iostat=STATUS) iadate(4),0,0
   VERIFY_(STATUS)
#ifdef VERBOSE
   call tell(Iam,'cymd =',trim(cymd))
   call tell(Iam,'chms =',trim(chms))
#endif
!_RT#else
! This is the way to do it if the GSI clock's current time is the analysis 
! time (should be available during coupling to model):
!_RT   call get_DateStamp ( clock, DateStamp, expid, rc=STATUS )
!_RT   VERIFY_(STATUS)
!_RT#endif


   read(cymd,'(i8.8)',iostat=STATUS) nymd
   VERIFY_(STATUS)
   read(chms,'(i6.6)',iostat=STATUS) nhms
   VERIFY_(STATUS)
#ifdef VERBOSE
   call tell(Iam,'nymd =',nymd)
   call tell(Iam,'nhms =',nhms)
#endif

!  Read in observation files table
!  -------------------------------

!     Allocate obs table array
!     ------------------------
      allocate( obstab(ndatmax,2) )
      obstab(:,:)=""

   call RC_obstable_ ( nobfiles )
#ifdef VERBOSE
   call tell(Iam,'returned from RC_obstable_()')

   call tell(Iam,'ndat_times =',ndat_times)
#endif
   if ( ndat_times > 1 ) then
#ifdef VERBOSE
      call tell(Iam,'ndat_times > 1')
#endif
      call tell(Iam,'observation time yyyymmdd:hhmmss = '//cymd//':'//chms)
      do indx = 1, ndat_times
#ifdef VERBOSE
         call tell(Iam,'indx =',indx)
         call tell(Iam,'nobfiles =',nobfiles)
#endif
         ! print *, trim(IAm),':  observation time yyyymmdd:hhmmss = '//cymd//':'//chms
         do i=1,nobfiles
#ifdef VERBOSE
            call tell(Iam,'iobfiles =',i)
#endif
            call setObsFile_( trim(obstab(i,2)), trim(obstab(i,1)), nymd, nhms, indx )
#ifdef VERBOSE
            call tell(Iam,'returned from setObsFile_(), iobfiles =',i)
#endif
         enddo
         call tick ( nymd, nhms, obsfreq_sc ) ! _RT: now this is cheating ... needs generalization
#ifdef VERBOSE
         call tell(Iam,'returned from tick()',i)
#endif
      enddo
   else
#ifdef VERBOSE
      call tell(Iam,'ndat_times !> 1')
#endif
      call tell(Iam,'observation time yyyymmdd:hhmmss = '//cymd//':'//chms)
      ! print *, trim(IAm),':  observation time yyyymmdd:hhmmss = '//cymd//':'//chms
#ifdef VERBOSE
      call tell(Iam,'nobfiles =',nobfiles)
#endif
      do i=1,nobfiles
#ifdef VERBOSE
         call tell(Iam,'iobfiles =',i)
#endif
         call setObsFile_( trim(obstab(i,2)), trim(obstab(i,1)), nymd, nhms )
#ifdef VERBOSE
         call tell(Iam,'returned from setObsFile_(), iobfiles =',i)
#endif
      enddo
   endif ! < ndat_times >
#ifdef VERBOSE
   call tell(Iam,'endif(ndat_times>1)')
   call tell(Iam,'exiting..')
#endif

   deallocate(obstab)
   end subroutine GSI_GridCompSetObsNames_

   subroutine RC_obstable_ ( nobfiles )

!  01Oct2007 Todling initial code
!  17Feb2009 Jing Guo	- removed mpeu dependency using m_inpak90
!			- removed VERIFY_() reference, since this
!			  code does not meet the requirements for
!			  VERIFY_() macro and VERIFY(rc/=0) will
!			  not produce an abort().
!  18Feb2009 Jing Guo	- replaced mpeu dependency using m_strtemplate
!			  with local module mpeu_util.

      use mpeu_util,only: warn,tell
      implicit none

      integer(i_kind), intent(out) :: nobfiles
 
      character(len=*), parameter :: IAm='GSI_GridComp.RC_obstable_'
      character(len=*), parameter :: myname_   = IAm
      character(len=*), parameter :: tablename = 'observation_files::'

      integer iret, ientry, iamroot
      character(len=ESMF_MAXSTR) :: token

!     Initialize
!     ----------
      iamroot = MAPL_AM_I_ROOT()
      iret    = 0

!     Read table with paired datatypes:
!     --------------------------------
      call ESMF_ConfigFindLabel(CF,trim(tablename),rc=iret)
         if (iret/=0) then
            write(6,'(2a,i5,3a)') myname_, ': ESMF_ConfigFindLabel error, iret=', iret, &
                                           ': trying to read "', trim(tablename),'"'
            call MAPL_Abort()
         end if

      ientry   = 0
      nobfiles = 0

      call getnexttoken1_(CF,token,myname_,iret)
      do while (iret==0)     ! read table entries

         ientry=ientry+1
	    if(ientry>ndatmax) then
               write(6,'(2a,i5)') myname_, ': number of entries in tabel exceed max allowed ', ndatmax
               write(6,'(2a,2a)') myname_, ': field #1 = "',trim(token),'"'
               iret = 99
	       call MAPL_Abort()
	    endif

	 	! Expect two tokens from this line
	 obstab(ientry,1)=token
	 call getnexttoken2_(CF,obstab(ientry,2),myname_)

	     	! echo the input
             if (iamroot) write(6,'(5a)') myname_, &
                                         ': observation data entries: ', &
                                          trim(obstab(ientry,1)), ' ',   &
					  trim(obstab(ientry,2))

	 	! try the next token
	 call getnexttoken1_(CF,token,myname_,iret)
      enddo

      nobfiles = ientry

!     Release resource file:
!     ---------------------

   end subroutine RC_obstable_

   subroutine getnexttoken1_(cf,token,myname,rc)
     implicit none
     type(ESMF_Config),intent(inout) :: cf
     character(len=*),intent(out) :: token
     character(len=*),intent(in ) :: myname
     integer         ,intent(out) :: rc
     character(len=*), parameter :: IAm='GSI_GridComp.getnexttoken1_'

     token=""
     call ESMF_ConfigNextLine(cf, rc=rc)
       if(rc/=0) return           ! end-of-file is expected.  No error message is produced.

     call ESMF_ConfigGetAttribute(cf, token, rc=rc)
       if(rc/=0) then
         write(6,'(2a,i5)') myname, ': ESMF_ConfigGetAttribute(field #1) error, rc=', rc
         call MAPL_Abort()
       endif

     if(token=='::') rc=-1	! end-of-table is expected.  No error message is produced
   end subroutine getnexttoken1_

   subroutine getnexttoken2_(cf,token,myname)
     implicit none
     type(ESMF_Config),intent(inout) :: cf
     character(len=*),intent(out) :: token
     character(len=*),intent(in ) :: myname
     character(len=*), parameter :: IAm='GSI_GridComp.getnexttoken2_'

     integer :: rc

     token=""
     call ESMF_ConfigGetAttribute(cf, token, rc=rc)
       if(rc/=0) then
         write(6,'(2a,i5)') myname, ': ESMF_ConfigGetAttribute(field #2) error, rc=', rc
         call MAPL_Abort()
       endif
   end subroutine getnexttoken2_


!-------------------------------------------------------------------------
   subroutine setObsFile_(tmpl,gsiname,nymd,nhms,indx)
!-------------------------------------------------------------------------
   use mpeu_util, only: tell,warn
! args
   character(len=*), intent(in)           :: tmpl, gsiname
   integer(i_kind),  intent(in)           :: nymd, nhms
   integer(i_kind),  intent(in), optional :: indx
! locals
   integer                               :: rc
   logical                               :: fexist
   character(len=ESMF_MAXSTR)            :: syscmd1,syscmd2
   character(len=ESMF_MAXSTR)            :: obsfile, obs_file
   character(len=*), parameter :: Iam='GSI_GridComp.setObsFile_'

   call StrTemplate ( obsfile, tmpl, nymd=nymd, nhms=nhms, stat=STATUS )
   VERIFY_(STATUS)
   inquire(file=obsfile, exist=fexist)
   syscmd1 = 'rm -f '//trim(gsiname)
   syscmd2 = 'ln -s '//trim(obsfile)//' '//trim(gsiname)
   if(present(indx)) then
      write(syscmd2,'(2a,i2.2)') trim(syscmd2), '.', indx
   endif
   if(fexist) then
      call system(syscmd1)
      call system(syscmd2)
   else
      call warn(Iam,"does not exist",obsfile)
   end if

   end subroutine setObsFile_

   function sfcFirst_(ak,bk,ps,amIinc)
     implicit none
     real(r_kind),dimension(:),intent(in) :: ak,bk
     real,intent(in) :: ps
     logical :: sfcFirst_
     logical :: amIinc
     integer :: m
     m=size(ak)
     if (amIinc) then
             !   p_sfc=bk(1) > p_top=bk(m)
       sfcFirst_= bk(1)      > bk(m)    ! simplified test when dealing w/ increment
     else
             !   p_sfc=pres(1)   >  p_top=pres(m)
       sfcFirst_= ak(1)+bk(1)*ps > ak(m)+bk(m)*ps
     endif
   end function sfcFirst_


   end subroutine Run

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: Finalize -- Finalizes the GSI gridded component

! !INTERFACE:

  subroutine Finalize ( gc, import, export, clock, rc )

!
! !USES:
!
  implicit NONE

! !ARGUMENTS:

   type(ESMF_GridComp), intent(INOUT) :: gc     ! Gridded component 
   type(ESMF_State),    intent(INOUT) :: import ! Import state
   type(ESMF_State),    intent(INOUT) :: export ! Export state
   type(ESMF_Clock),    intent(INOUT) :: clock  ! The clock
   integer, optional,   intent(  OUT) :: rc     ! Error code:

! !DESCRIPTION:
!
! !REVISION HISTORY:
!
!   14Apr2007 Todling Handling ifile arrays as allocatables 
!
!EOPI
!-------------------------------------------------------------------------

   integer                           :: STATUS    ! error code STATUS
   logical                           :: IamRoot    
   character(len=*), parameter :: IAm='Finalize'

! start

   IamRoot = MAPL_AM_I_ROOT()
   if(IamRoot.and.verbose) print *, Iam,": Start ",trim(Iam)

! Call GEOS Generic Finalize

   call MAPL_GenericFinalize ( gc, import, export, clock,  RC=STATUS)
   VERIFY_(STATUS)

! Deallocate memory for GSI variables

   call GSI_GridCompDealloc_()

   deallocate(ifilesig, ifilesfc, stat=STATUS)
   VERIFY_(STATUS)
   deallocate(hrdifsig, hrdifsfc, stat=STATUS)
   VERIFY_(STATUS)
   deallocate(hrdifsig_all, hrdifsfc_all, stat=STATUS)
   VERIFY_(STATUS)
   
   if(IamRoot) print *, Iam,": End ",trim(Iam)

   RETURN_(ESMF_SUCCESS)

   CONTAINS

!-------------------------------------------------------------------------
   subroutine GSI_GridCompDealloc_()
!-------------------------------------------------------------------------
   character(len=*), parameter :: IAm='GSI_GridCompDealloc'

!  Deallocate memory for guess files for trace gases
!  ------------------------------------------------
   call destroy_chemges_grids(status)
   VERIFY_(STATUS)

   deallocate( isli     ,&
               fact10   ,&
               sfct     ,&
               dsfct    ,&
               sno      ,&
               veg_type ,&
               veg_frac ,&
               soil_type,&
               soil_temp,&
               soil_moi ,&
               sfc_rough,&
               stat=STATUS)
   VERIFY_(STATUS)
   deallocate( ges_z   ,&
               ges_ps,  &
               ges_u   ,&
               ges_v   ,&
               ges_vor ,&
               ges_div ,&
               ges_cwmr,&
               ges_q   ,&
               ges_oz  ,&
               ges_tv  ,&
               stat=STATUS)
   VERIFY_(STATUS)

   end subroutine GSI_GridCompDealloc_

   end subroutine Finalize	

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOPI

! !IROUTINE: GSI_GridCompSetupSpecs -- Sets import/export specs

! !INTERFACE:

   subroutine GSI_GridCompSetupSpecs (GC, opthw, rc)

!
! !USES:
!
  implicit NONE

! !ARGUMENTS:

   type(ESMF_GridComp), intent(INOUT) :: gc    ! gridded component
   integer, optional, intent(IN)      :: opthw ! optional halowidth
   integer, optional, intent(OUT)     :: rc    ! return code

! !DESCRIPTION: Sets the import and export specs. The GSI expects (imports) 
!       the following fields:
!
!       UPA: Hs, Ps, Tsk, u, v, Tv, vor, div, q, oz, qi, ql
!       SFC: F10m, Sno, qg, Tg, SLI, Vfrac, Vtyp, Styp, Sfc_rough
! 
! GEOS does not export vor, div - calculated from u,v by GSI
!                      F10m     - calculated from u,v,u10,v10 (exported by GEOS)
!                      Vfrac, Vtyp, Styp - read from a file 
!
!  Whenever possible we have used the same SHORT_NAME between corresponding
!  GEOS-5 and GSI fields. In some cases, as when coupling, it clearly changes the
!  nature of the field, we have used GSI specific names, to emphasize 
!  the change.
!
! !REVISION HISTORY:
!
!  22Apr2007 Todling Properly naming u/v/tv fields
!  24Apr2007 Todling Added delp to export state; tskin to ts from bkg.eta
!  10Mar2009 Todling Add fraction (land/ice/etc) and replace qc w/ qi/qltot
!
!EOPI
!-------------------------------------------------------------------------

   integer                               :: STATUS, local_hw
   character(len=*), parameter :: IAm='GSI_GridCompSetupSpecs'

   integer(i_kind) ii

!  Declare import 2d-fields
!  ------------------------
   integer, parameter :: nin2d=14
   character(len=16), parameter :: insname2d(nin2d) = (/  &
                                   'phis            ',    &
                                   'ps              ',    &
                                   'ts              ',    &
                                   'U10M            ',    &
                                   'V10M            ',    &
                                   'SNOWDP          ',    &
                                   'GWETTOP         ',    &
                                   'TSOIL1          ',    &
                                   'Z0M             ',    &
                                   'frland          ',    &
                                   'frlandice       ',    &
                                   'frlake          ',    &
                                   'frocean         ',    &
                                   'frseaice        '    /)
   character(len=32), parameter :: inlname2d(nin2d) = (/  &
                      'geopotential height             ', &
                      'surface pressure                ', &
                      'skin temperature                ', &
                      'u 10 m-wind                     ', &
                      'v 10 m-wind                     ', &
                      'snow depth                      ', &
                      'surface soil wetness            ', &
                      'soil temperature                ', &
                      'roughness length                ', &
                      'fraction of land                ', &
                      'fraction of land ice            ', &
                      'fraction of lake                ', &
                      'fraction of ocean               ', &
                      'fraction of sea ice             ' /)
   character(len=16), parameter :: inunits2d(nin2d) = (/  &
                                   'm**2/s**2       ',    &
                                   'hPa             ',    &
                                   'K               ',    &
                                   'm/s             ',    &
                                   'm/s             ',    &
                                   'm               ',    &
                                   '1               ',    &
                                   'K               ',    &
                                   'm               ',    &
                                   '1               ',    &
                                   '1               ',    &
                                   '1               ',    &
                                   '1               ',    &
                                   '1               '    /)

!  Declare import 3d-fields
!  ------------------------
   integer, parameter :: nin3d=7
   character(len=16), parameter :: insname3d(nin3d) = (/  &
                                   'u               ',    &
                                   'v               ',    &
                                   'tv              ',    &
                                   'sphu            ',    &
                                   'ozone           ',    &
                                   'qitot           ',    &
                                   'qltot           '    /)
   character(len=40), parameter :: inlname3d(nin3d) = (/  &
                      'eastward wind                           ', &
                      'northward wind                          ', &
                      'air virtual temperature                 ', &
                      'specific humidity                       ', &
                      'ozone mass mixing ratio                 ', &
                      'mass fraction of cloud ice water        ', &
                      'mass fraction of cloud liquid water     ' /)
   character(len=16), parameter :: inunits3d(nin3d) = (/  &
                                   'm/s             ',    &
                                   'm/s             ',    &
                                   'K               ',    &
                                   'g/g             ',    &
                                   'g/g             ',    &
                                   '1               ',    &
                                   '1               '    /)

!  Declare import 3d-fields for trace gases - this needs 
!  to come from gsi_chemtracer_mod
!  -----------------------------------------------------
   integer, parameter :: nin3dg=2
   character(len=16), parameter :: insname3dg(nin3dg) = (/ &
                                   'co              ',     &
                                   'co2             '/)
   character(len=32), parameter :: inlname3dg(nin3dg) = (/ &
                      'carbon monoxide                 ',  &
                      'carbon dioxide                  '/)
   character(len=16), parameter :: inunits3dg(nin3dg) = (/ &
                                   'g/g             ',     &
                                   'g/g             '     /)


!  Declare export 2d-fields
!  ------------------------
   integer, parameter :: nex2d=8
   character(len=16), parameter :: exsname2d(nex2d) = (/  &
                                   'phis            ',    &
                                   'ps              ',    &
                                   'ts              ',    &
                                   'frland          ',    &
                                   'frlandice       ',    &
                                   'frlake          ',    &
                                   'frocean         ',    &
                                   'frseaice        '    /)
   character(len=32), parameter :: exlname2d(nex2d) = (/  &
                      'geopotential height             ', &
                      'surface pressure inc            ', &
                      'skin temperature inc            ', &
                      'fraction of land                ', &
                      'fraction of land ice            ', &
                      'fraction of lake                ', &
                      'fraction of ocean               ', &
                      'fraction of sea ice             ' /)
   character(len=16), parameter :: exunits2d(nex2d) = (/  &
                                   'm**2/s**2       ',    &
                                   'hPa             ',    &
                                   'K               ',    &
                                   '1               ',    &
                                   '1               ',    &
                                   '1               ',    &
                                   '1               ',    &
                                   '1               '    /)

!  Declare export 3d-fields
!  ------------------------
   integer, parameter :: nex3d=8
   character(len=16), parameter :: exsname3d(nex3d) = (/  &
                                   'u               ',    &
                                   'v               ',    &
                                   'tv              ',    &
                                   'delp            ',    &
                                   'sphu            ',    &
                                   'ozone           ',    &
                                   'qitot           ',    &
                                   'qltot           '    /)
   character(len=40), parameter :: exlname3d(nex3d) = (/  &
                      'u-wind inc                              ', &
                      'v-wind inc                              ', &
                      'virtual temperature inc                 ', &
                      'delta pressure inc                      ', &
                      'specific humidity inc                   ', &
                      'ozone mass mixing ratio inc             ', &
                      'mass fraction of cloud ice water inc    ', &
                      'mass fraction of cloud liquid water inc ' /)
   character(len=16), parameter :: exunits3d(nex3d) = (/  &
                                   'm/s             ',    &
                                   'm/s             ',    &
                                   'K               ',    &
                                   'hPa             ',    &
                                   'g/g             ',    &
                                   'g/g             ',    &
                                   '1               ',    &
                                   '1               '    /)

!  Declare import 3d-fields for trace gases - this needs 
!  to come from gsi_chemtracer_mod
!  -----------------------------------------------------
   integer, parameter :: nex3dg=2
   character(len=16), parameter :: exsname3dg(nin3dg) = (/ &
                                   'co              ',     &
                                   'co2             '/)
   character(len=32), parameter :: exlname3dg(nin3dg) = (/ &
                      'carbon monoxide inc             ',  &
                      'carbon dioxide inc              '/)
   character(len=16), parameter :: exunits3dg(nin3dg) = (/ &
                                   'g/g             ',     &
                                   'g/g             '     /)


! Begin

   if(present(opthw)) then
      local_hw=opthw
   else
      local_hw=HW
   end if

! Get variables from chem
! call gsi_chemtracer_get('shortnames')
! call gsi_chemtracer_get('longnames')
! call gsi_chemtracer_get('units')

! Imports


! Begin

   if(present(opthw)) then
      local_hw=opthw
   else
      local_hw=HW
   end if

! Imports

    do ii = 1, nin2d 
       call MAPL_AddImportSpec(GC,        &
         SHORT_NAME= trim(insname2d(ii)), &
         LONG_NAME = trim(inlname2d(ii)), &
         UNITS     = trim(inunits2d(ii)), &
         DIMS      = MAPL_DimsHorzOnly,   &
         VLOCATION = MAPL_VLocationNone,  &
         HALOWIDTH = local_hw,            &
         RC=STATUS  ); VERIFY_(STATUS)
    enddo
    do ii = 1, nin3d 
     call MAPL_AddImportSpec(GC,           &
         SHORT_NAME= trim(insname3d(ii)),  &
         LONG_NAME = trim(inlname3d(ii)),  &
         UNITS     = trim(inunits3d(ii)),  &
         DIMS      = MAPL_DimsHorzVert,    &
         VLOCATION = MAPL_VLocationCenter, &
         HALOWIDTH = local_hw,             &
         RC=STATUS  ); VERIFY_(STATUS)
    enddo
    do ii = 1, nin3dg 
      call MAPL_AddImportSpec(GC,           &
         SHORT_NAME= trim(insname3dg(ii)), &
         LONG_NAME = trim(inlname3dg(ii)), &
         UNITS     = trim(inunits3dg(ii)), &
         DIMS      = MAPL_DimsHorzVert,    &
         VLOCATION = MAPL_VLocationCenter, &
         HALOWIDTH = local_hw,             &
         RC=STATUS  ); VERIFY_(STATUS)
    enddo

! Exports

    do ii = 1, nex2d
       call MAPL_AddExportSpec(GC,         &
         SHORT_NAME= trim(exsname2d(ii)),  &
         LONG_NAME = trim(exlname2d(ii)),  &
         UNITS     = trim(exunits2d(ii)),  &
         DIMS      = MAPL_DimsHorzOnly,    &
         VLOCATION = MAPL_VLocationNone,   &
         HALOWIDTH = local_hw,             &
         RC=STATUS  ); VERIFY_(STATUS)
    enddo
    do ii = 1, nex3d
       call MAPL_AddExportSpec(GC,         &
         SHORT_NAME= trim(exsname3d(ii)),  &
         LONG_NAME = trim(exlname3d(ii)),  &
         UNITS     = trim(exunits3d(ii)),  &
         DIMS      = MAPL_DimsHorzVert,    &
         VLOCATION = MAPL_VLocationCenter, &
         HALOWIDTH = local_hw,             &
         RC=STATUS  );  VERIFY_(STATUS)
    enddo
    do ii = 1, nex3dg
       call MAPL_AddExportSpec(GC,         &
         SHORT_NAME= trim(exsname3dg(ii)), &
         LONG_NAME = trim(exlname3dg(ii)), &
         UNITS     = trim(exunits3dg(ii)), &
         DIMS      = MAPL_DimsHorzVert,    &
         VLOCATION = MAPL_VLocationCenter, &
         HALOWIDTH = local_hw,             &
         RC=STATUS  );  VERIFY_(STATUS)
    enddo

   end subroutine GSI_GridCompSetupSpecs

!-------------------------------------------------------------------------
   subroutine GSI_GridCompSetAlarms (GENSTATE, GC, cf, clock)
!-------------------------------------------------------------------------
   use mpeu_util,only : tell,perr,die
   type(MAPL_MetaComp), pointer     :: GENSTATE  ! GEOS Generic state
   type(ESMF_GridComp)              :: GC
   type(ESMF_Config)                :: cf
   type(ESMF_Clock)                 :: clock

!  locals
!  ------
   character(len=*), parameter :: IAm='GSI_GridCompSetAlarms'

   type(ESMF_Alarm), pointer        :: ALARM(:)  ! this component's alarms
   type(ESMF_Time)                  :: CurrTime
   type(ESMF_Time)                  :: AlarmTime
   type(ESMF_Time)                  :: AlarmTime0
   type(ESMF_Time)                  :: AlaTime
   type(ESMF_Time)                  :: AnaTime
   type(ESMF_Time)                  :: RefTime
   type(ESMF_TimeInterval)          :: FrqBKG
   type(ESMF_TimeInterval)          :: FrqANA
   type(ESMF_TimeInterval)          :: AnaOST
   integer                          :: atime, i
   integer                          :: REF_TIME(6), RREF_DATE, RREF_TIME
   integer                          :: status, rc
   integer                          :: yy, mm, dd, hh, mn, sec
   logical                          :: IamRoot
   character(len=ESMF_MAXSTR)       :: aname
   character(len=2)                 :: idxtime

!  start
!  -----
   IamRoot = MAPL_AM_I_ROOT()

   call MAPL_GetObjectFromGC ( GC, GENSTATE, RC=STATUS ); VERIFY_(STATUS)

   call ESMF_ClockAdvance(clock, rc=status)
   VERIFY_(STATUS)

!  Define analysis time from reference time in rc file
!  ---------------------------------------------------
   CALL ESMF_ConfigGetAttribute( cf, RREF_DATE, label = 'RECORD_REF_DATE:', rc=status )
     VERIFY_(status)
   CALL ESMF_ConfigGetAttribute( cf, RREF_TIME, label = 'RECORD_REF_TIME:', rc=status )
     VERIFY_(status)

   REF_TIME(1) =     RREF_DATE/10000
   REF_TIME(2) = mod(RREF_DATE,10000)/100
   REF_TIME(3) = mod(RREF_DATE,100)
   REF_TIME(4) =     RREF_TIME/10000
   REF_TIME(5) = mod(RREF_TIME,10000)/100
   REF_TIME(6) = mod(RREF_TIME,100)

! initialize current time
! -----------------------
   call ESMF_TimeSet(  RefTime, YY =  REF_TIME(1), &
                                MM =  REF_TIME(2), &
                                DD =  REF_TIME(3), &
                                H  =  REF_TIME(4), &
                                M  =  REF_TIME(5), &
                                S  =  REF_TIME(6), rc=status ); VERIFY_(STATUS)

!  Create num. FG time levels + 1 alarms centered on the analysis time
!  -------------------------------------------------------------------
   call ESMF_ClockGet (clock, currTime=currTime, rc=STATUS)
     VERIFY_(STATUS)
   call ESMF_TimeGet(currTime, yy=YY, mm=MM, dd=DD, h=HH, m=MN, s=SEC, rc=rc)
     if(IamRoot) write(6,'(a,1x,i4.4,5(a,i2.2))') trim(Iam)//" The current TIME is ", &
                                       YY, "/", MM, "/", DD, " ", HH, ":", MN, ":", SEC

   call ESMF_TimeSet(AlarmTime, yy=yy, mm=mm, dd=dd, h=hh, rc=STATUS)
     VERIFY_(STATUS)

   call ESMF_TimeIntervalSet(FrqBKG, s=BKGfreq_sc, rc=STATUS)
     VERIFY_(STATUS)

   call ESMF_TimeIntervalSet(FrqANA, h=ANAfreq_hr, rc=STATUS)
     VERIFY_(STATUS)

   call ESMF_TimeIntervalSet(AnaOST, h=min_offset/60,m=mod(min_offset,60), rc=STATUS)
     VERIFY_(STATUS)


!
!  n+1 alarms (n=number of BKG time levels) are set around the analysis time:
!
! n=1                      *
! n=3                *     *     *
! n=5          *     *     *     *     *
! etc                      |
!                        ana-t
!
! n+1 th alarm rings when all BKG time levels have been collected
!

   allocate(ALARM(nfldsig_all), stat=STATUS); VERIFY_(STATUS)

   AnaTime    = AlarmTime + AnaOST  ! alarm to indicate analysis time
   AlarmTime0 = AlarmTime
   do atime = 1,nfldsig_all-1
      write(idxtime,'(i2)',iostat=STATUS) atime;                                     VERIFY_(STATUS)
      aname = "some-bkg " // idxtime
      ALARM(atime) = ESMF_AlarmCreate(aname, clock, ringTime=alarmTime0, rc=rc);     VERIFY_(STATUS)
      call MAPL_StateAlarmAdd(GENSTATE,ALARM(atime),RC=status);                      VERIFY_(STATUS)
      call ESMF_AlarmGet(ALARM(atime), ringTime=AlaTime, rc=status);                 VERIFY_(STATUS)
      call ESMF_TimeGet(AlaTime, yy=YY, mm=MM, dd=DD, h=HH, m=MN, s=SEC, rc=status); VERIFY_(STATUS)
      if(IamRoot) print *, Iam,": ",trim(aname)," background time set to ", YY, "/", MM, "/", DD, " ", HH, ":", MN, ":", SEC
      if( AlaTime == RefTime ) then
          ntguessig_ref = atime
          ntguessfc_ref = atime
          if (IamRoot) print *, Iam,": Upper-air updated pointer (ntguessig_ref): ", ntguessig_ref
          if (IamRoot) print *, Iam,": Surface   updated pointer (ntguessig_ref): ", ntguessfc_ref
      endif
#ifdef VERBOSE
      call tell(Iam,"if(Alatime==AnaTime)")
#endif
      if( AlaTime == AnaTime ) then
          MYIDATE = (/ HH, MM, DD, YY /)
      endif
#ifdef VERBOSE
      call tell(Iam,"endif(Alatime==AnaTime)")
#endif
      alarmTime0 = alarmTime0 + FrqBKG
   end do
#ifdef VERBOSE
   call tell(Iam,"enddo")
#endif

   	! locate where the slot is available
#ifdef VERBOSE
   call tell(Iam,"nfldsig =",nfldsig)
   call tell(Iam,"ntguessig_ref =",ntguessig_ref)
   call tell(Iam,"allocated(hrdifsig) =",allocated(hrdifsig))
   call tell(Iam,"allocated(hrdifsig_all) =",allocated(hrdifsig_all))
   call tell(Iam,"size(hrdifsig,1) =",size(hrdifsig,1))
   call tell(Iam,"size(hrdifsig_all,1) =",size(hrdifsig_all,1))
   call tell(Iam,"hrdifsig_all(ntguessig_ref) =",hrdifsig_all(ntguessig_ref))
#endif

   ! the last alarm notifies aana that analysis is done

   atime = nfldsig_all
   aname = "last-bkg"
   ALARM(atime) = ESMF_AlarmCreate(aname, clock, ringTime=alarmTime0, rc=rc);     VERIFY_(STATUS)
   call MAPL_StateAlarmAdd(GENSTATE,ALARM(atime),RC=status);                      VERIFY_(STATUS)
   call ESMF_AlarmGet(ALARM(atime), ringTime=AlaTime, rc=status);                 VERIFY_(STATUS)
   call ESMF_TimeGet(AlaTime, yy=YY, mm=MM, dd=DD, h=HH, m=MN, s=SEC, rc=status); VERIFY_(STATUS)
   if(IamRoot) print *,Iam,": ",trim(aname)," at ana time set to ", YY, "/", MM, "/", DD, " ", HH, ":", MN, ":", SEC
   if( AlaTime == RefTime ) then
       ntguessig_ref = atime
       ntguessfc_ref = atime
       if (IamRoot) print *, Iam,": Upper-air updated pointer (ntguessig_ref): ", ntguessig_ref
       if (IamRoot) print *, Iam,": Surface   updated pointer (ntguessig_ref): ", ntguessfc_ref

   	! locate where the slot is available
#ifdef VERBOSE
   call tell(Iam,"nfldsig =",nfldsig)
   call tell(Iam,"ntguessig_ref =",ntguessig_ref)
   call tell(Iam,"allocated(hrdifsig) =",allocated(hrdifsig))
   call tell(Iam,"allocated(hrdifsig_all) =",allocated(hrdifsig_all))
   call tell(Iam,"size(hrdifsig,1) =",size(hrdifsig,1))
   call tell(Iam,"size(hrdifsig_all,1) =",size(hrdifsig_all,1))
   call tell(Iam,"hrdifsig_all(ntguessig_ref) =",hrdifsig_all(ntguessig_ref))
#endif
   endif

   deallocate(ALARM, stat=STATUS); VERIFY_(STATUS)

   call ESMF_ClockSet(clock, direction=ESMF_MODE_REVERSE, rc=status); VERIFY_(STATUS)
   call ESMF_ClockAdvance(clock, rc=STATUS)                         ; VERIFY_(STATUS)
   call ESMF_ClockSet(clock, direction=ESMF_MODE_FORWARD, rc=status); VERIFY_(STATUS) 

   RETURN_(ESMF_SUCCESS)

   end subroutine GSI_GridCompSetAlarms

! Utilities: 

!-------------------------------------------------------------------------
  subroutine GSI_GridCompSP2NP_(rbufr,sfcin)
!-------------------------------------------------------------------------
! NCEP native Gaussian gridded fields have latitudes reversed and the poles
! excluded. This routine reverses latitudes and adds poles.
  implicit none
  real(4),dimension(:,:),intent(inout) :: rbufr
  real(4),dimension(:,:),intent(in ) :: sfcin
  integer :: im,jm,j
  character(len=*), parameter :: IAm='GSI_GridCompSP2NP_'

  im=size(rbufr,1)
  jm=size(rbufr,2)
  rbufr(:, 1)=sum(sfcin(:,jm-2))/im	! add South Pole points
  do j=2,jm-1
				! for j :=    2,   3, ..., jm-2,jm-1
				!  jm-j == jm-2,jm-3, ...,    2,   1
    rbufr(:,j)=sfcin(:,jm-j) 
  end do
  rbufr(:,jm)=sum(sfcin(:,   1))/im	! add North Pole points
  end subroutine GSI_GridCompSP2NP_

!-------------------------------------------------------------------------
   subroutine SwapVr_(fld)
!-------------------------------------------------------------------------
   implicit none
   real(r_kind),intent(inout) ::  fld(:,:,:)
   real(r_kind),allocatable   :: work(:,:,:)
   character(len=*), parameter :: IAm='GSI_GridComp.SwapVr_'
   integer im, jm, km
   if(.not.doVflip)return
   im   = size(fld,1)
   jm   = size(fld,2)
   km   = size(fld,3)
   allocate (work(im,jm,km))
   work = fld
   fld(:,:,km:1:-1) = work(:,:,1:km:+1)
   deallocate (work)
   end subroutine SwapVr_
!-------------------------------------------------------------------------
   subroutine SwapV4_(fld)
!-------------------------------------------------------------------------
   implicit none
   real(4),intent(inout) ::  fld(:,:,:)
   real(4),allocatable   :: work(:,:,:)
   character(len=*), parameter :: IAm='GSI_GridComp.SwapV4_'
   integer im, jm, km
   if(.not.doVflip)return
   im   = size(fld,1)
   jm   = size(fld,2)
   km   = size(fld,3)
   allocate (work(im,jm,km))
   work = fld
   fld(:,:,km:1:-1) = work(:,:,1:km:+1)
   deallocate (work)
   end subroutine SwapV4_
!-------------------------------------------------------------------------
   subroutine SwapIJ3r_(aij,aji)
!-------------------------------------------------------------------------
! transpose IJK-ordered array to JIK-ordered array
   implicit none
   real(4),dimension(:,:,:),intent(in) :: aij
   real(r_kind),  dimension(:,:,:),intent(inout) :: aji
   character(len=*), parameter :: IAm='GSI_GridComp.SwapIJ3r_'
   integer :: i,j,k,isz,jsz,ksz
!
   isz=size(aij,1)
   jsz=size(aij,2)
   ksz=size(aij,3)
   do k=1,ksz
      do i=1,isz
         aji(1:jsz,i,k)=aij(i,1:jsz,k)
      end do
   end do
   call Halo3d_Undef_OutJI_(aji(:,:,1:ksz))
   call GSI_GridCompSwapV_(aji(:,:,1:ksz))
   end subroutine SwapIJ3r_

!-------------------------------------------------------------------------
   subroutine SwapJI3r_(aij,aji)
!-------------------------------------------------------------------------
! transpose JI-ordered array to IJ-ordered array
   implicit none
   real(4),dimension(:,:,:),intent(inout) :: aij
   real(r_kind),  dimension(:,:,:),intent(in) :: aji
   character(len=*), parameter :: IAm='GSI_GridComp.SwapJI3r_'
   integer :: i,j,k,isz,jsz,ksz
!
   isz=size(aji,2)
   jsz=size(aji,1)
   ksz=size(aji,3)
   do k=1,ksz
      do i=1,isz
         aij(i,1:jsz,k)=aji(1:jsz,i,k)
      end do
   end do
   call GSI_GridCompSwapV_(aij(:,:,1:ksz))

   end subroutine SwapJI3r_

!-------------------------------------------------------------------------
   subroutine SwapIJ2r_(aij,aji)
!-------------------------------------------------------------------------
! transpose IJ-ordered array to JI-ordered array
   implicit none
   real(4),dimension(:,:),intent(in) :: aij
   real(r_kind),  dimension(:,:),intent(inout) :: aji
   character(len=*), parameter :: IAm='GSI_GridComp.SwapIJ2r_'
!
   integer :: i,j,isz,jsz
!
   isz=size(aij,1)
   jsz=size(aij,2)
   do i=1,isz
      aji(1:jsz,i)=aij(i,1:jsz)
   end do
   call Halo2d_Undef_OutJI_(aji)

   end subroutine SwapIJ2r_

!-------------------------------------------------------------------------
   subroutine SwapIJ2i_(aij,aji)
!-------------------------------------------------------------------------
! transpose IJ-ordered int-array to JI-ordered int-array
   implicit none
   integer, dimension(:,:),intent(in   ) :: aij
   integer, dimension(:,:),intent(inout) :: aji
!
   character(len=*), parameter :: IAm='GSI_GridComp.SwapIJ2i_'
   integer :: i,j,isz,jsz
!
     isz=size(aij,1)
     jsz=size(aij,2)
     do i=1,isz
       aji(1:jsz,i)=aij(i,1:jsz)
     end do

   end subroutine SwapIJ2i_

!-------------------------------------------------------------------------
   subroutine SwapJI2r_(aij,aji)
!-------------------------------------------------------------------------
! transpose IJ-ordered array to JI-ordered array
   implicit none
   real(4),dimension(:,:),intent(inout) :: aij
   real(r_kind), dimension(:,:),intent(in) :: aji
!
   character(len=*), parameter :: IAm='GSI_GridComp.SwapJI2r_'
   integer :: i,j,isz,jsz
!
   jsz=size(aji,1)
   isz=size(aji,2)
   do i=1,isz
     aij(i,1:jsz)=aji(1:jsz,i)
   end do

   end subroutine SwapJI2r_

!-------------------------------------------------------------------------
   subroutine Halo2d_Undef_OutJI_(aji)
!-------------------------------------------------------------------------
!  !REVISION HISTORY:
!    21Aug2007 Todling Implemented to remove MAPL_UNDEF from halo
!                      This is a temporary fix that needs better handling
!                      to differentiate between scalar and vector fields
!  THIS ASSUMES horizontal HW=1
!-------------------------------------------------------------------------
   implicit none
   real(r_kind),  dimension(:,:),intent(inout) :: aji
   character(len=*), parameter :: IAm='GSI_GridComp.Halo2d_Undef_OutJI_'
   integer :: j,i,k,isz,jsz
   jsz=size(aji,1)
   isz=size(aji,2)
   do i=1,isz
      if(abs(aji(1  ,i)-MAPL_UNDEF).lt.1.e-2) aji(1  ,i) = aji(2    ,i)
      if(abs(aji(jsz,i)-MAPL_UNDEF).lt.1.e-2) aji(jsz,i) = aji(jsz-1,i)
   end do
   end subroutine Halo2d_Undef_OutJI_

!-------------------------------------------------------------------------
   subroutine Halo3d_Undef_OutJI_(aji)
!-------------------------------------------------------------------------
!  !REVISION HISTORY:
!    21Aug2007 Todling Implemented to remove MAPL_UNDEF from halo
!                      This is a temporary fix that needs better handling
!                      to differentiate between scalar and vector fields
!  THIS ASSUMES horizontal HW=1
!-------------------------------------------------------------------------
   implicit none
   real(r_kind),  dimension(:,:,:),intent(inout) :: aji
   character(len=*), parameter :: IAm='GSI_GridComp.Halo3d_Undef_OutJI_'
   integer :: j,i,k,isz,jsz,ksz
   jsz=size(aji,1)
   isz=size(aji,2)
   ksz=size(aji,3)
   do k = 1,ksz
      do i=1,isz
         if(abs(aji(1  ,i,k)-MAPL_UNDEF).lt.1.e-2) aji(1  ,i,k) = aji(2    ,i,k)
         if(abs(aji(jsz,i,k)-MAPL_UNDEF).lt.1.e-2) aji(jsz,i,k) = aji(jsz-1,i,k) 
      end do
   end do
   end subroutine Halo3d_Undef_OutJI_
                                                                                                                      
!-------------------------------------------------------------------------
   subroutine GSI_GridCompFlipLons2_(q)
!-------------------------------------------------------------------------
     implicit none
     real,dimension(:,:),intent(inout) :: q
     character(len=*), parameter :: IAm='GSI_GridCompFlipLons2_'
     integer :: j
     do j=1,size(q,2)
        call GSI_GridCompFlipLonsi_(q(:,j))
     end do
   end subroutine GSI_GridCompFlipLons2_

!-------------------------------------------------------------------------
   subroutine GSI_GridCompFlipLonsi_(q)
!-------------------------------------------------------------------------
     implicit none
     real,dimension(:),intent(inout) :: q
     character(len=*), parameter :: IAm='GSI_GridCompFlipLonsi_'
     integer :: im
     real,dimension(size(q,1)/2) :: d
     im=size(q,1)
     d(     1:im/2) = q(     1:im/2)
     q(     1:im/2) = q(im/2+1:im  )
     q(im/2+1:im  ) = d(     1:im/2)
   end subroutine GSI_GridCompFlipLonsi_

! The following routines exist somewhere else , but are not readily
! available

!-------------------------------------------------------------------
   subroutine get_DateStamp (clock, DateStamp, expid, offset, rc)
!-------------------------------------------------------------------

! This can be found in GEOShistory.

    type (ESMF_Clock)                 :: clock
    character(len=ESMF_MAXSTR)        :: DateStamp
    character(len=ESMF_MAXSTR)        :: expid
    type(ESMF_TimeInterval), optional :: offset
    integer, optional                 :: rc

    character(len=*), parameter :: IAm='GSI_GridComp.get_DateStamp'

    type(ESMF_Time)                   :: currentTime
    type(ESMF_TimeInterval)           :: TimeStep
    character(len=ESMF_MAXSTR)        :: TimeString
    integer                           :: secs
    character(len=ESMF_MAXSTR)        :: TimeStamp
    character                         :: String(ESMF_MAXSTR)

    character*4 year
    character*2 month
    character*2 day
    character*2 hour
    character*2 minute
    character*2 second

    equivalence ( string(01),TimeString )
    equivalence ( string(01),year       )
    equivalence ( string(06),month      )
    equivalence ( string(09),day        )
    equivalence ( string(12),hour       )
    equivalence ( string(15),minute     )
    equivalence ( string(18),second     )

    call ESMF_ClockGet (clock, currTime=currentTime, rc=rc)
    if (present(offset)) then
       currentTime = currentTime - offset
    end if
    call ESMF_TimeGet  (currentTime, timeString=TimeString, rc=rc)
    call ESMF_ClockGet (clock, timeStep=TimeStep, rc=rc)
    call ESMF_TimeIntervalGet (TimeStep, S=secs, rc=rc)

    DateStamp = year // month // day // '_' // hour // minute // second // 'z'
    TimeStamp = '   Date: ' // year // '/' // month // '/' // day
    TimeStamp = trim(TimeStamp) // '  Time: ' // timestring(12:19)

    if (.not. present(OFFSET)) then
       call WRITE_PARALLEL ( 'Expid: ' // trim(expid) // trim(TimeStamp) )
    endif

   end subroutine get_DateStamp

!-------------------------------------------------------------------------
!   NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, DAS  !
!-------------------------------------------------------------------------
!BOPI
!
! !ROUTINE: ncep_rwsurf_ - read and/or create SSI surface background file
!
! !INTERFACE:

   subroutine ncep_rwsurf_ ( verbose_, infile, glat2, glon, rc, & 
                             wout, jrec, fld )

! !USES:

   implicit none

! INPUT PARAMETERS:

   logical :: verbose_
   integer :: glat2, glon
   character(len=*), intent(in)           :: infile
   logical,          intent(in), optional :: wout
   integer,                      optional :: jrec     ! set to record 
                                                      ! of fld to be returned
! OUTPUT PARAMETERS:

   real(4),intent(inout),optional :: fld(glon,glat2) ! requested output field from file
   integer, intent(out) :: rc                        ! return error code

! !DESCRIPTION: This module handles surface fields form the NCEP surface files.
!
!EOPI
!-------------------------------------------------------------------------

   character(len=*), parameter :: IAm='GSI_GridComp.get_ncep_rwsurf_'

   real(4) :: sdum4
   real(4), allocatable :: fdum4(:,:)
   real(4), allocatable :: fdum42(:,:,:)
   real(4), allocatable :: albedo4(:,:,:)
   real(4), allocatable :: zenith2(:,:,:)

   logical :: writeout
   integer :: icount, irec, status
   character(8),dimension(4):: labfix
   real(4) yhour
   integer latd,lonl,version
   integer,dimension(4):: igdate,iadate
   integer,allocatable,dimension(:):: lonsperlat

   rc       = 0
   irec     = 0
   writeout = .false.
   if (present(jrec)) then
       if(.not.present(fld))then
          print *, trim(Iam), ': Error, missing fld array'
          rc = 1
          return
       endif
       irec = jrec
   endif

   ! unit 34 is a temporary file that contains surface fields
   ! not available from GMAO and so the corresponding NCEP fields are used

   open(34,file=infile,form='unformatted')
   if(present(wout)) then
      if(wout)then
         writeout = .true.
      endif
   endif
   allocate ( fdum4  (glon,glat2),   &
              fdum42 (glon,glat2,2), & 
              albedo4(glon,glat2,4), &
              zenith2(glon,glat2,2), &
              stat=STATUS)
   VERIFY_(STATUS)
   read(34) 
   read(34) yhour,IgDATE,LONL,LATD,version
   allocate ( lonsperlat(latd/2), stat=STATUS)
   VERIFY_(STATUS)

   rewind(34)
   read(34) labfix
   read(34) yhour,IgDATE,LONL,LATD,version,lonsperlat
   icount = 0
   read(34) fdum4                      ! record 1  tsf
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum42                     ! record 2  soilm
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum42
          fld = fdum42(:,:,1)
       end if
   read(34) fdum4                      ! record 3  snow
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum42                     ! record 4  soilt
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum42
          fld = fdum42(:,:,1)
       end if
   read(34) fdum4                      ! record 5  tg3
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 6  zor
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 7  cv
       icount = icount + 1
       if(verbose_) print*,Iam,': ',icount, ' ', maxval(fdum4)
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 8  cvb
       icount = icount + 1
       if(verbose_) print*,Iam,': ',icount,' ', maxval(fdum4)
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 9  cvt
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) albedo4                    ! record 10 albedo
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) albedo4
          fld = albedo4(:,:,1)
       end if
   read(34) fdum4                      ! record 11 slimsk
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 12 vegetation cover
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 13 canopy water
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 14 f10m
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 15 vegetation type
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 16 soil type
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) zenith2                    ! record 17 zenith
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) zenith2
          fld = zenith2(:,:,1)
       end if
   read(34) fdum4                      ! record 18 ustar
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 19 ffmm
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if
   read(34) fdum4                      ! record 20 ffhh
       icount = icount + 1
       if(irec==icount) then
          if(verbose_) print *,Iam,': reading record ',irec
          if(writeout) write(35) fdum4
          fld = fdum4
       end if

   close(34)

   deallocate ( lonsperlat, stat=STATUS)
   VERIFY_(STATUS) 
   deallocate ( fdum4,fdum42,albedo4,zenith2 , stat=STATUS)
   VERIFY_(STATUS)

   if(verbose_) print * , trim(Iam), ': done'
   end subroutine ncep_rwsurf_

! undef_2ssi exists in transf?
! Unfortunately present code needs it r4; transf has it r8.

!-------------------------------------------------------------------------
   subroutine undef_2ssi (var, undef_in, where, reltol, verb, vname )
!-------------------------------------------------------------------------

     implicit none

     real,dimension(:,:),intent(inout) ::     var
     real             ,intent(in) :: undef_in
     character(len=*) ,intent(in) :: where
     real,optional    ,intent(in) :: reltol
     logical,optional ,intent(in) :: verb
     character(len=*),optional,intent(in) :: vname

     character(len=*), parameter :: IAm='GSI_GridComp.undef_2ssi_'

     real :: reltol_

     reltol_=.01
     if(present(reltol)) reltol_=reltol

     if(verb==.true.) then
        call lookfor_(fundef_ssi() ,reltol_,var,6,where,vname)	! -.999e33
        call lookfor_(undef_in     ,reltol_,var,6,where,vname)	! given
        call lookfor_(1.e12        ,reltol_,var,6,where,vname)	! 1.e12
     endif
     
     call udf2udf2d_(var, undef_in,fundef_ssi(),where,	&
          reltol=reltol, verb=verb, vname=vname )

   contains

     function fundef_ssi()
       implicit none
       real :: fundef_ssi
       fundef_ssi=UNDEF_SSI_
     end function fundef_ssi

     subroutine lookfor_(spval,reltol,v,lu,where,vname)
       implicit none
       real,intent(in) :: spval
       real,intent(in) :: reltol
       real,dimension(:,:),intent(in) :: v
       integer,intent(in) :: lu
       character(len=*),intent(in) :: where,vname
       character(len=*), parameter :: IAm='GSI_GridComp.lookfor_'

       integer :: n,m
       n=count(abs(v-spval)<=abs(reltol*spval))
       m=size(v)
!       if(n/=0) write(lu,'(2a,e12.6,3a,i6,a,i6)') trim(where),	&
!            ': find ',spval,' in variable "',trim(vname),'", ',	&
!            n,' out of ',m
     end subroutine lookfor_

   end subroutine undef_2ssi


!-------------------------------------------------------------------------
   subroutine udf2udf2d_ (var,undef_in,undef_out,where,	&
      	reltol,verb,vname)
!-------------------------------------------------------------------------
     implicit none

     real,intent(in) :: undef_in
     real,intent(in) :: undef_out
     character(len=*),intent(in) :: where

     real,dimension(:,:),intent(inout) ::     var

     real,optional,intent(in) :: reltol
     logical,optional,intent(in) :: verb
     character(len=*),optional,intent(in) :: vname

     character(len=*), parameter :: IAm='GSI_GridComp.udf2udf2d_'

     integer  i,j
     integer  nonmiss
     real     tol
     real     val_max_in, val_max_out
     logical :: verbose_

     verbose_=.false.
     if(present(verb)) verbose_=verb
     
      tol   = 0.01
      if(present(reltol)) tol=reltol
      tol   = tol*undef_in

!     Make sure there is no precision problem with undef's
!     ----------------------------------------------------
      nonmiss = 0
      val_max_in  = maxval(var)
      do j = 1, size(var,2)
         do i = 1, size(var,1)
            if ( abs(var(i,j)-undef_in) .lt. tol ) then
               nonmiss = nonmiss + 1
               var(i,j) = undef_out
            end if
         end do
      end do
      val_max_out = maxval(abs(var))
      
      if (nonmiss.ne.0) then
         if(verbose_) then
	   if(present(vname)) then
              if(MAPL_AM_I_ROOT().and.verbose) write(6,*) &
                   'No. of UNDEF_in fixed to UNDEF_out for "'//	&
                   trim(vname)//'"',nonmiss
	   else
              if(MAPL_AM_I_ROOT().and.verbose) write(6,*) &
                   'No. of UNDEF_in fixed to UNDEF_out',nonmiss
	   endif
        endif
        
        if ( val_max_out .gt. abs(undef_out) ) then
           if (MAPL_AM_I_ROOT()) then
               write(6,*) &
	       	     ' Largest  abs(value) on  input: ',  val_max_in
               write(6,*) &
	   	     ' Largest  abs(value) on output: ',  val_max_out
               write(6,*) &
		     ' Undef_in     value spec.: ',  undef_in
               write(6,*) &
		     ' Undef_out    value spec.: ',  undef_out
           endif
        end if
     end if

     return
   end subroutine udf2udf2d_

   end module GSI_GridCompMod
