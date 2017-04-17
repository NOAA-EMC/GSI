!   2016-04-20  Modify to handle the updated nemsio sig file (P, DP & DPDT removed)
integer(i_kind),public :: nlevs_pres,idvc
integer(i_kind) nlevsin, ierr, iunit, nvar, k, nn, idvc
real(r_single),allocatable,dimension(:,:,:) :: nems_vcoord
                             dimz=nlevsin,jcap=ntrunc,idvc=idvc)

!       Extract vertical coordinate descriptions nems_vcoord.
!       nems_vcoord(gfshead%levs+1,3,2) dimension is hardwired here.
!       Present NEMSIO modules do not allow flexibility of 2nd and 3rd
!       array dimension for nems_vcoord, for now, it is hardwired as
!       (levs,3,2) If NEMS changes the setting of vcoord dimension,
!       GSI needs to update its setting of nems_vcoord accordingly.

        if (allocated(nems_vcoord))     deallocate(nems_vcoord)
        allocate(nems_vcoord(nlevs_pres,3,2))
        call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
        if ( iret /= 0 ) then
           write(6,*)' gridinfo:  ***ERROR*** problem reading header ', &
              'vcoord, Status = ',iret
           call stop2(99)
        endif


      allocate(ak(nlevs+1),bk(nlevs+1))

      if ( idvc == 0 ) then                         ! sigma coordinate, old file format.
         ak = zero
         bk = nems_vcoord(1:nlevs+1,1,1)
      elseif ( idvc == 1 ) then                     ! sigma coordinate
         ak = zero
         bk = nems_vcoord(1:nlevs+1,2,1)
      elseif ( idvc == 2 .or. idvc == 3 ) then      ! hybrid coordinate
         ak = 0.01_r_kind*nems_vcoord(1:nlevs+1,1,1) ! convert to mb
         bk = nems_vcoord(1:nlevs+1,2,1)
      else
         write(6,*)'gridinfo:  ***ERROR*** INVALID value for idvc=',idvc
         call stop2(85)
      endif

      ! pressure at interfaces
      do k=1,nlevs+1
         pressimn(:,k) = ak(k)+bk(k)*spressmn(:)
      ptop = ak(nlevs+1)
      deallocate(ak,bk)
      if (sighead%idvc == 0) then                              ! sigma coordinate, old file format.
      else if (sighead%idvc == 1) then                         ! sigma coordinate
      else if (sighead%idvc == 2 .or. sighead%idvc == 3) then  ! hybrid coordinate
         ak = 0.01_r_kind*sighead%vcoord(1:nlevs+1,1)          ! convert to mb
         bk = sighead%vcoord(1:nlevs+1,2) 
module gridinfo

  !========================================================================

  !$$$ Module documentation block
  ! 
  ! This module contains various routines to ingest, define, and compute 
  ! variables from Weather Research and Forecasting (WRF) model Advanced
  ! Research WRF (ARW) and Non-hydrostatic Mesoscale Model (NMM) dynamical
  ! cores which are required by the Ensemble Kalman Filter (ENKF) currently
  ! designed for operations within the National Centers for Environmental
  ! Prediction (NCEP) Global Forecasting System (GFS)
  !
  ! prgmmr: Winterbottom        org: ESRL/PSD1       date: 2011-11-30
  !
  ! program history log:
  !   
  !   2011-11-30  Initial version.
  !   2013-01-02  Updated subroutine getgridinfo_nmm.f90 such that the 
  !               pressure profile (i.e., presslmn) is computed as it is in  
  !               within the GSI subroutine get_wrf_nmm_ensperts.F90.
  !               Henry R. Winterbottom
  !
  ! attributes:
  !   language:  f95
  !
  !$$$

  !=========================================================================

  ! Define associated modules

  use constants, only: rearth_equator, omega, pi, deg2rad, zero, rad2deg,    &
                       rearth
  use kinds,     only: i_kind, r_kind, r_single, i_long, r_double
  use enkf_obsmod,    only: obloc, obloclat, obloclon, nobstot
  use params,    only: datapath, nlevs, nvars, ndim, nlons, nlats,           &
                       arw, nmm, nbackgrounds, fgfileprefixes
  use mpisetup
  use netcdf_io

  implicit none

  !--------------------------------------------------------------------------

  ! Define data structures

  type griddimensions
     integer                                                   :: xdim
     integer                                                   :: ydim
     integer                                                   :: zdim
  end type griddimensions

  ! Define structure types available to all routines

  type(griddimensions),                             public     :: dimensions

  ! Define variables defining the grid attributes (typically from the
  ! ensemble mean)

  real(r_single),      dimension(:,:), allocatable, public     :: logp
  real(r_single),      dimension(:,:), allocatable, public     :: gridloc
  real(r_single),      dimension(:),   allocatable, public     :: lonsgrd
  real(r_single),      dimension(:),   allocatable, public     :: latsgrd
  real(r_single),                                   public     :: ptop
  integer(i_kind),     dimension(:),   allocatable, public     :: index_pres
  integer(i_long),                                  public     :: npts
  integer(i_kind),                                  public     :: nvarhumid ! specific hum is the nvarhumid'th var
  integer(i_kind),                                  public     :: nvarozone=0 ! ozone is the nvarhumid'th var
  integer(i_kind),                                  public     :: nlevs_pres

  ! Define variables indicating analysis variables to be updated
  ! during EnKF experiments (eventually the logical variables will be
  ! collected from the namelist (i.e., params module)

  character(len=12),   dimension(:),   allocatable, public     :: gridvarstring

  !-------------------------------------------------------------------------

  ! Define all public subroutines within this module

  private
  public :: definegridvariables
  public :: getgridinfo
  public :: gridinfo_cleanup
  public :: cross2dot
  public :: dot2cross

contains

  subroutine getgridinfo()
    if (arw) then
       call getgridinfo_arw()
    else
       call getgridinfo_nmm()
    end if
  end subroutine getgridinfo

  !=========================================================================

  ! definegridvariables.f90: This subroutine will define all analysis
  ! variables to be updated for the respective EnKF experiments; this
  ! subroutine can handle either ARW or NMM dynamical core input
  ! files; the respective dynamical core is the determined by the
  ! logical variables defined above

  !-------------------------------------------------------------------------
 
  ! *USER*: Edit the following subroutines to correspond to the
  ! 3-dimensional variables to be updated during the data assimilation

  ! *NOTE (for ARW)*: the dry surface pressure prognostic variable
  ! (MU) **MUST** be the last variable; the total number of variables
  ! is nvars+1

  ! *NOTE (for NMM)*: the dry surface pressure prognostic variable
  ! (PD) **MUST** be the last variable; the total number of variables
  ! is nvars+1

  !-------------------------------------------------------------------------
  
  subroutine definegridvariables()

    !-----------------------------------------------------------------------
    
    ! Define the total number of analysis variables to be updated for
    ! the respective EnKF experiments

    if(.not. allocated(gridvarstring)) allocate(gridvarstring(nvars+1))

    !-----------------------------------------------------------------------

    ! Define netcdf variable strings corresponding to variables to be
    ! updated for the WRF ARW dynamical core

    if(arw) then

       if (nvars .eq. 3) then

          if (nproc .eq. 0) then

             ! Print message to user

             write(6,*) 'Updating U, V, T, and MU for WRF-ARW...'

          endif ! if (nproc .eq. 0) then

          ! *USER*: The following variables will be updated using the
          ! innovations and increments produced by the data
          ! assimilation

          gridvarstring(1) = "U"
          gridvarstring(2) = "V"
          gridvarstring(3) = "T"
          gridvarstring(4) = "MU"
          nvarhumid = 0

       else if (nvars .eq. 4) then

          if (nproc .eq. 0) then

             ! Print message to user

             write(6,*) 'Updating U, V, T, QVAPOR, and MU for WRF-ARW...'

          endif ! if (nproc .eq. 0) then

          ! *USER*: The following variables will be updated using the
          ! innovations and increments produced by the data
          ! assimilation

          gridvarstring(1) = "U"
          gridvarstring(2) = "V"
          gridvarstring(3) = "T"
          gridvarstring(4) = "QVAPOR"
          gridvarstring(5) = "MU"
          nvarhumid = 4

       else if (nvars .eq. 5) then

          if (nproc .eq. 0) then

             ! Print message to user

             print *,'Updating U, V, T, QVAPOR, PH, and MU for WRF-ARW...'

          endif ! if (nproc .eq. 0) then

          ! *USER*: The following variables will be updated using the
          ! innovations and increments produced by the data
          ! assimilation

          gridvarstring(1) = "U"
          gridvarstring(2) = "V"
          gridvarstring(3) = "T"
          gridvarstring(4) = "QVAPOR"
          gridvarstring(5) = "PH"
          gridvarstring(6) = "MU"
          nvarhumid = 4

       else if (nvars .eq. 6) then

          if (nproc .eq. 0) then

             ! Print message to user

             write(6,*) 'Updating U, V, T, QVAPOR, W, PH, and MU for WRF-ARW...'

          endif ! if (nproc .eq. 0) then

          ! *USER*: The following variables will be updated using the
          ! innovations and increments produced by the data
          ! assimilation

          gridvarstring(1) = "U"
          gridvarstring(2) = "V"
          gridvarstring(3) = "T"
          gridvarstring(4) = "QVAPOR"
          gridvarstring(5) = "W"
          gridvarstring(6) = "PH"
          gridvarstring(7) = "MU"
          nvarhumid = 4

       else 

          if (nproc .eq. 0) then

             ! Print message to user

             write(6,*) '!!! USER !!! You need to specify ',nvars,' 3d variables to update, plus MU!'
             write(6,*) '!!! USER !!! Edit subroutine definegridvariables in gridinfo.F90 and recompile'

          endif ! if (nproc .eq. 0) then

          ! Exit routine

          call stop2(22)

       endif ! if (nvars .eq. 3) then

    end if ! if(arw) then

    !-----------------------------------------------------------------------

    ! Define netcdf variable strings corresponding to variables to be
    ! updated for the WRF NMM dynamical core

    if(nmm) then

       if (nvars .eq. 3) then

          if (nproc .eq. 0) then

             ! Print message to user

             write(6,*) 'Updating U, V, T, and PD for WRF-NMM...'

          endif ! if (nproc .eq. 0) then

          ! *USER*: The following variables will be updated using the
          ! innovations and increments produced by the data
          ! assimilation

          gridvarstring(1) = "U"
          gridvarstring(2) = "V"
          gridvarstring(3) = "T"
          gridvarstring(4) = "PD"
          nvarhumid = 0

       else if (nvars .eq. 4) then

          if (nproc .eq. 0) then

             ! Print message to user

             write(6,*) 'Updating U, V, T, Q, and PD for WRF-NMM...'

          endif ! if (nproc .eq. 0) then

          ! *USER*: The following variables will be updated using the
          ! innovations and increments produced by the data
          ! assimilation

          gridvarstring(1) = "U"
          gridvarstring(2) = "V"
          gridvarstring(3) = "T"
          gridvarstring(4) = "Q"
          gridvarstring(5) = "PD"
          nvarhumid = 4

       else if (nvars .eq. 5) then

          if (nproc .eq. 0) then

             ! Print message to user

             write(6,*) 'Updating U, V, T, Q, CWM, and PD for WRF-NMM...'

          endif ! if (nproc .eq. 0) then

          ! *USER*: The following variables will be updated using the
          ! innovations and increments produced by the data
          ! assimilation

          gridvarstring(1) = "U"
          gridvarstring(2) = "V"
          gridvarstring(3) = "T"
          gridvarstring(4) = "Q"
          gridvarstring(5) = "CWM"
          gridvarstring(6) = "PD"
          nvarhumid = 4

       else 

          if (nproc .eq. 0) then

             ! Print message to user

             write(6,*) '!!! USER !!! You need to specify ',nvars,' 3d variables to update, plus MU!'
             write(6,*) '!!! USER !!! Edit subroutine definegridvariables in gridinfo.F90 and recompile'

          endif ! if (nproc .eq. 0) then

          ! Exit routine

          call stop2(22)

       endif ! if (nvars .eq. 3) then

    end if ! if(nmm) then

  !-------------------------------------------------------------------------

    ! Provide error checking to make sure that the user has choosen
    ! one of the available dynamical core options via the namelist
    ! variables

    if(.not. arw .and. .not. nmm) then

       ! Print message to user

       write(6,*) '!!! USER !!! You have not defined the logical variables appropriately which '
       write(6,*) '             state that you are using the WRF ARW or NMM dynamical cores    '
       write(6,*) '             within the namelist. Aborting routine.'

       ! Exit routine

       call stop2(22)

    end if ! if(.not. arw .and. .not. nmm)

  !-------------------------------------------------------------------------

  end subroutine definegridvariables
    
  !=========================================================================

  ! getgridinfo_arw.f90: This subroutine will define, compute, and
  ! return all attributes required from WRF ARW model grid, provided
  ! the file name string; typically this subroutine will operate on
  ! the 'ensemble mean file' since it returns the longitude, latitude,
  ! and log(pressure) values which is currently defaulted to the
  ! ensemble mean in the EnKF implication of J. Whitaker

  ! NOTE: the EnKF assumes all variables are defined along mass (i.e.,
  ! unstaggered grid points) and thus we statically assign array
  ! dimensions during allocation process

  !-------------------------------------------------------------------------

  subroutine getgridinfo_arw()

    ! Define variables ingested from external file

    real,      dimension(:,:,:),  allocatable :: wrfarw_mu
    real,      dimension(:,:,:),  allocatable :: wrfarw_mub
    real,      dimension(:,:,:),  allocatable :: wrfarw_znu

   ! Define variables returned by subroutine

    real(r_kind),      dimension(:,:),    allocatable :: presslmn
    real(r_kind),      dimension(:),      allocatable :: spressmn
    
    ! Define variables computed within subroutine

    character(len=500)                                :: filename
    real,      dimension(:,:,:),  allocatable :: workgrid
    real(r_kind)                                      :: radlon
    real(r_kind)                                      :: radlat
    real(r_kind)                                      :: dlon
    real(r_kind)                                      :: dlat
    real(r_kind)                                      :: recenter_xlat
    real                                              :: recenter_dx
    real(r_kind)                                      :: lonsgrdmin
    real(r_kind)                                      :: lonsgrdmax
    real(r_kind)                                      :: latsgrdmin
    real(r_kind)                                      :: latsgrdmax
    integer                                           :: nlevsin
    integer                                           :: nlonsin
    integer                                           :: nlatsin
    integer                                           :: nn
    integer                                           :: ierr
    integer                                           :: nvar

    ! Define variables required for netcdf I/O

    character(len=12)                                 :: varstringname
    character(len=50)                                 :: attstringname
    character(len=20), dimension(3)                   :: dimstring
    integer,           dimension(3)                   :: dims

    ! Define counting variables 
    
    integer                                           :: i, j, k
    integer                                           :: count, nob

    !======================================================================

    ! Define prognostic model variable structure type

    call definegridvariables()

    ! Define local values and prepare for array dimension definitions

    dimstring(1) = "west_east"
    dimstring(2) = "south_north"
    dimstring(3) = "bottom_top"

    ! Build the ensemble mean filename expected by routine

    filename = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nbackgrounds/2+1)))//"ensmean"

    ! Obtain unstaggered grid dimensions from ingested variable file

    call netcdfdimension(filename,3,dimstring,dims)

    ! Define array dimension structure type

    dimensions = griddimensions(dims(1),dims(2),dims(3))

    ! Compute all variables required by subsequent routines

    npts = dimensions%xdim*dimensions%ydim
    nlevsin = dimensions%zdim
    nlonsin = dimensions%xdim
    nlatsin = dimensions%ydim

    !----------------------------------------------------------------------

    ! Perform sanity and error checks for variable dimensions; proceed
    ! accordingly

    if (nlons .ne. nlonsin) then
       
       ! Print message to user

       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlons ingested from file = ', nlonsin
       write(6,*) '      nlons specified in namelist = ', nlons
       write(6,*) 'Failed in subroutine getgridinfo_arw; Aborting!'

       ! Exit routine

       call stop2(22)

    end if ! if (nlons .ne. nlonsin)

    ! Perform sanity and error checks for variable dimensions; proceed
    ! accordingly

    if (nlats .ne. nlatsin) then

       ! Print message to user

       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlats ingested from file = ', nlatsin
       write(6,*) '      nlats specified in namelist = ', nlats
       write(6,*) 'Failed in subroutine getgridinfo_arw; Aborting!'

       ! Exit routine

       call stop2(22)

    end if ! if (nlats .ne. nlatsin)

    ! Perform sanity and error checks for variable dimensions; proceed
    ! accordingly

    if (nlevs .ne. nlevsin) then

       ! Print message to user

       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlevs ingested from file = ', nlevsin
       write(6,*) '      nlevs specified in namelist = ', nlevs
       write(6,*) 'Failed in subroutine getgridinfo_arw; Aborting!'

       ! Exit routine

       call stop2(22)

    end if ! if (nlevs .ne. nlevsin)

    !----------------------------------------------------------------------

    ! Compute local variable; number of model levels plus surface
    ! (levels at which ens. mean log pressure defined for localization
    ! via array logp)

    nlevs_pres=dimensions%zdim+1

    ! Allocate memory for global arrays

    if(.not. allocated(lonsgrd)) allocate(lonsgrd(npts))
    if(.not. allocated(latsgrd)) allocate(latsgrd(npts))
    if(.not. allocated(logp))    allocate(logp(npts,nlevs_pres))

    !======================================================================

    ! Begin: Ingest all grid variables required for EnKF routines and
    ! perform necessary conversions; the data is only ingested on the
    ! master node and then subsequently passed to the slave nodes

    !----------------------------------------------------------------------

    if (nproc  .eq. 0) then ! only read data on root.

       ! Allocate memory for all global arrays

       if(.not. allocated(presslmn)) allocate(presslmn(npts,nlevs))
       if(.not. allocated(spressmn)) allocate(spressmn(npts))

       ! Allocate memory for all local arrays

       if(.not. allocated(wrfarw_mu))                                       &
            & allocate(wrfarw_mu(dimensions%xdim,dimensions%ydim,1))
       if(.not. allocated(wrfarw_mub))                                      &
            & allocate(wrfarw_mub(dimensions%xdim,dimensions%ydim,1))
       if(.not. allocated(wrfarw_znu))                                      &
            & allocate(wrfarw_znu(1,1,dimensions%zdim))

    !----------------------------------------------------------------------


       ! Allocate memory for local variable grid

       if(.not. allocated(workgrid)) allocate(workgrid(dimensions%xdim,  &
            & dimensions%ydim,1))

       ! Ingest variable from external file

       varstringname = 'XLONG'
       call readnetcdfdata(filename,workgrid,varstringname,              &
            & dimensions%xdim,dimensions%ydim,1)

       ! Initialize counting variable

       count = 1

       ! Loop through meridional horizontal coordinate

       do j = 1, dimensions%ydim

          ! Loop through zonal horizontal coordinate

          do i = 1, dimensions%xdim
       
             ! Convert from degrees to radians and update the
             ! global longitude array

             lonsgrd(count) = workgrid(i,j,1)*deg2rad
          
             ! Update counting variable

             count = count + 1

          end do ! do i = 1, dimensions%xdim

       end do ! do j = 1, dimensions%ydim

       ! Deallocate memory for local variable grid

       if(allocated(workgrid)) deallocate(workgrid)

       ! Allocate memory for local variable grid

       if(.not. allocated(workgrid)) allocate(workgrid(dimensions%xdim,  &
            & dimensions%ydim,1))
       
       ! Ingest variable from external file

       varstringname = 'XLAT'
       call readnetcdfdata(filename,workgrid,varstringname,              &
            & dimensions%xdim,dimensions%ydim,1)

       ! Initialize counting variable

       count = 1

       ! Loop through meridional horizontal coordinate

       do j = 1, dimensions%ydim

          ! Loop through zonal horizontal coordinate

          do i = 1, dimensions%xdim
       
             ! Convert from degrees to radians and update the
             ! global latitude array
             
             latsgrd(count) = workgrid(i,j,1)*deg2rad

             ! Update counting variable

             count = count + 1

          end do ! do i = 1, dimensions%xdim

       end do ! do j = 1, dimensions%ydim

       ! Deallocate memory for local variable grid

       if(allocated(workgrid)) deallocate(workgrid)

    !----------------------------------------------------------------------


       ! Ingest surface pressure grid from external file

       varstringname = gridvarstring(nvars+1)

       ! Check that the last variable element in array is (at least
       ! analogous) to surface pressure

       if(trim(varstringname) .ne. 'MU' .and. trim(varstringname) .ne.      &
            & 'PSFC') then

          ! Print message to user

          write(6,*) 'The last variable must be MU or PSFC. However, the ', &
               & 'character string is ', trim(varstringname), '. ',         &
               & 'Aborting!'
          
          ! Exit routine
          
          call stop2(22)

       endif ! if(trim(varstringname) .ne. 'MU' .and. trim(varstringname)   &
             ! .ne. 'PSFC') 

    !----------------------------------------------------------------------
       
       ! Ingest the model vertical (eta) levels from the external file

       varstringname = 'ZNU'
       call readnetcdfdata(filename,wrfarw_znu,varstringname,1,1,           &
            & dimensions%zdim)

       ! Ingest the model perturbation dry air mass from the external
       ! file

       varstringname = 'MU'
       call readnetcdfdata(filename,wrfarw_mu,varstringname,                &
            & dimensions%xdim,dimensions%ydim,1)

       ! Ingest the model base state dry air mass from the external
       ! file

       varstringname = 'MUB'
       call readnetcdfdata(filename,wrfarw_mub,varstringname,               &
            & dimensions%xdim,dimensions%ydim,1)

       ! Allocate memory for local variable grid

       if(.not. allocated(workgrid)) allocate(workgrid(1,1,1))

       ! Ingest variable from external file

       varstringname = 'P_TOP'
       call readnetcdfdata(filename,workgrid,varstringname,1,1,1)
    
       ! Define local variable

       ptop = workgrid(1,1,1)

       ! Rescale pressure from Pa to hPa

       ptop = ptop/100.0

       ! Deallocate memory for local variable grid
       
       if(allocated(workgrid)) deallocate(workgrid)

    !----------------------------------------------------------------------

       ! Initialize counting variable

       count = 1

       ! Loop through meridional horizontal coordinate

       do j = 1, dimensions%ydim

          ! Loop through zonal horizontal coordinate

          do i = 1, dimensions%xdim
          
             ! Convert from Pa to hPa and update the global surface
             ! pressure array

             spressmn(count) = (wrfarw_mu(i,j,1) + &
                                wrfarw_mub(i,j,1) + ptop*100.0)/100.0

             ! Update counting variable

             count = count + 1
             
          end do ! do i = 1, dimensions%xdim
          
       end do ! do j = 1, dimensions%ydim

    !----------------------------------------------------------------------


       ! Initialize counting variable

       nn = 0

       ! Loop through vertical coordinate

       do k = 1, dimensions%zdim
       
          ! Update counting variable

          nn = nn + 1
    
          ! Initialize counting variable
       
          count = 1
       
          ! Loop through meridional horizontal coordinate
          
          do j = 1, dimensions%ydim
          
             ! Loop through zonal horizontal coordinate
          
             do i = 1, dimensions%xdim
             
                ! Compute the pressure within the respective layer
                ! (dry hydrostatic pressure)
             
                presslmn(count,k) = wrfarw_znu(1,1,k)*(wrfarw_mu(i,j,1) +   &
                     & wrfarw_mub(i,j,1)) + ptop*100.0
             
                ! Rescale pressure from Pa to hPa
             
                presslmn(count,k) = presslmn(count,k)/100.0
             
                ! Compute the log of the pressure within the
                ! respective layer
             
                logp(count,k) = -log(presslmn(count,k))

                ! Update counting variable

                count = count + 1

             end do ! do i = 1, dimensions%xdim
       
          end do ! do j = 1, dimensions%ydim

       end do ! do k = 1, dimensions%zdim

       ! Compute local variable

       logp(:,nlevs_pres) = -log(spressmn(:))

       ! Print message to user

       write(6,*) 'Surface pressure (spressmn) min/max range:',             &
            & minval(spressmn),maxval(spressmn)

    !----------------------------------------------------------------------

       ! Deallocate memory for all local arrays

       if(allocated(wrfarw_mu))  deallocate(wrfarw_mu)
       if(allocated(wrfarw_mub)) deallocate(wrfarw_mub)
       if(allocated(wrfarw_znu)) deallocate(wrfarw_znu)
       if(allocated(presslmn))   deallocate(presslmn)
       if(allocated(spressmn))   deallocate(spressmn)

    !----------------------------------------------------------------------

    end if ! nproc == 0

    !----------------------------------------------------------------------

    ! Define index_pres which is an index array to determine pressure
    ! value for given analysis variable
 
    if(.not. allocated(index_pres)) allocate(index_pres(ndim))

    ! Initialize counting variable

    nn = 0

    ! Loop through total number of analysis variables

    do nvar = 1, nvars
       
       ! Loop through vertical levels

      do k = 1, dimensions%zdim

         ! Update counting variable

        nn = nn + 1

        ! Update global variable array

        index_pres(nn) = k

      end do ! do k = 1, dimensions%zdim

    end do ! do nvar = 1, nvars

    ! Update global variable array

    index_pres(ndim) = nlevs+1

    !----------------------------------------------------------------------

    ! End: Ingest all grid variables required for EnKF routines and
    ! perform necessary conversions; the data is only ingested on the
    ! master node and then subsequently passed to the slave nodes

    !======================================================================

    ! Broadcast all common variables these out to all nodes

    call MPI_Bcast(logp,npts*nlevs_pres,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(ptop,1,mpi_real4,0,MPI_COMM_WORLD,ierr)

    !----------------------------------------------------------------------


    ! Allocate memory for local variable

    if(.not. allocated(gridloc)) allocate(gridloc(3,npts))

    ! Loop through each grid coordinate and perform the coordinate
    ! transform for regular simulation domains 

    do nn = 1, npts

       ! Compute local variable

       gridloc(1,nn) = cos(latsgrd(nn))*cos(lonsgrd(nn))

       ! Compute local variable

       gridloc(2,nn) = cos(latsgrd(nn))*sin(lonsgrd(nn))

       ! Compute local variable

       gridloc(3,nn) = sin(latsgrd(nn))

    end do ! do nn = 1, npts

  end subroutine getgridinfo_arw

  !=========================================================================

  ! getgridinfo_nmm.f90: This subroutine will define, compute, and
  ! return all attributes required from WRF-NMM model grid, provided
  ! the file name string; typically this subroutine will operate on
  ! the 'ensemble mean file' since it returns the longitude, latitude,
  ! and log(pressure) values which is currently defaulted to the
  ! ensemble mean in the EnKF implication of J. Whitaker

  !-------------------------------------------------------------------------

  subroutine getgridinfo_nmm()

    ! Define variables ingested from external file

    real,      dimension(:,:,:),  allocatable :: wrfnmm_eta
    real,      dimension(:,:,:),  allocatable :: wrfnmm_pd
    real,      dimension(:,:,:),  allocatable :: wrfnmm_pdtop
    real,      dimension(:,:,:),  allocatable :: wrfnmm_pt
    real,      dimension(:,:,:),  allocatable :: wrfnmm_aeta1
    real,      dimension(:,:,:),  allocatable :: wrfnmm_aeta2

   ! Define variables returned by subroutine

    real(r_kind),      dimension(:,:),    allocatable :: presslmn
    real(r_kind),      dimension(:),      allocatable :: spressmn
    
    ! Define variables computed within subroutine

    character(len=500)                                :: filename
    real,      dimension(:,:,:),  allocatable :: workgrid
    integer                                           :: nlevsin
    integer                                           :: nlonsin
    integer                                           :: nlatsin
    integer                                           :: nn
    integer                                           :: ierr
    integer                                           :: nvar

    ! Define variables required for netcdf I/O

    character(len=12)                                 :: varstringname
    character(len=20), dimension(3)                   :: dimstring
    integer,           dimension(3)                   :: dims

    ! Define counting variables 
    
    integer                                           :: i, j, k
    integer                                           :: count

    !======================================================================

    ! Define prognostic model variable structure type

    call definegridvariables()

    ! Define local values and prepare for array dimension definitions

    dimstring(1) = "west_east"
    dimstring(2) = "south_north"
    dimstring(3) = "bottom_top"

    ! Build the ensemble mean filename expected by routine

    filename = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nbackgrounds/2+1)))//"ensmean"

    ! Obtain unstaggered grid dimensions from ingested variable file

    call netcdfdimension(filename,3,dimstring,dims)

    ! Define array dimension structure type

    dimensions = griddimensions(dims(1),dims(2),dims(3))

    ! Compute all variables required by subsequent routines

    npts = dimensions%xdim*dimensions%ydim
    nlevsin = dimensions%zdim
    nlonsin = dimensions%xdim
    nlatsin = dimensions%ydim

    !----------------------------------------------------------------------

    ! Perform sanity and error checks for variable dimensions; proceed
    ! accordingly

    if (nlons .ne. nlonsin) then
       
       ! Print message to user

       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlons ingested from file = ', nlonsin
       write(6,*) '      nlons specified in namelist = ', nlons
       write(6,*) 'Failed in subroutine getgridinfo_nmm; Aborting!'

       ! Exit routine

       call stop2(22)

    end if ! if (nlons .ne. nlonsin)

    ! Perform sanity and error checks for variable dimensions; proceed
    ! accordingly

    if (nlats .ne. nlatsin) then

       ! Print message to user

       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlats ingested from file = ', nlatsin
       write(6,*) '      nlats specified in namelist = ', nlats
       write(6,*) 'Failed in subroutine getgridinfo_nmm; Aborting!'

       ! Exit routine

       call stop2(22)

    end if ! if (nlats .ne. nlatsin)

    ! Perform sanity and error checks for variable dimensions; proceed
    ! accordingly

    if (nlevs .ne. nlevsin) then

       ! Print message to user

       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlevs ingested from file = ', nlevsin
       write(6,*) '      nlevs specified in namelist = ', nlevs
       write(6,*) 'Failed in subroutine getgridinfo_nmm; Aborting!'

       ! Exit routine

       call stop2(22)

    end if ! if (nlevs .ne. nlevsin)

    !----------------------------------------------------------------------

    ! Compute local variable; number of model levels plus surface
    ! (levels at which ens. mean log pressure defined for localization
    ! via array logp)

    nlevs_pres=dimensions%zdim+1

    ! Allocate memory for global arrays

    if(.not. allocated(lonsgrd)) allocate(lonsgrd(npts))
    if(.not. allocated(latsgrd)) allocate(latsgrd(npts))
    if(.not. allocated(logp))    allocate(logp(npts,nlevs_pres))

    !======================================================================

    ! Begin: Ingest all grid variables required for EnKF routines and
    ! perform necessary conversions; the data is only ingested on the
    ! master node and then subsequently passed to the slave nodes

    !----------------------------------------------------------------------

    if (nproc  .eq. 0) then ! only read data on root.

       ! Allocate memory for all global arrays

       if(.not. allocated(presslmn)) allocate(presslmn(npts,nlevs))
       if(.not. allocated(spressmn)) allocate(spressmn(npts))

       ! Allocate memory for all local arrays

       if(.not. allocated(wrfnmm_eta))                                      &
            & allocate(wrfnmm_eta(1,1,dimensions%zdim))
       if(.not. allocated(wrfnmm_pd))                                       &
            & allocate(wrfnmm_pd(dimensions%xdim,dimensions%ydim,1))
       if(.not. allocated(wrfnmm_pdtop))                                    &
            & allocate(wrfnmm_pdtop(1,1,1))
       if(.not. allocated(wrfnmm_pt))                                       &
            & allocate(wrfnmm_pt(1,1,1))
       if(.not. allocated(wrfnmm_aeta1))                                    &
            & allocate(wrfnmm_aeta1(1,1,dimensions%zdim))
       if(.not. allocated(wrfnmm_aeta2))                                    &
            & allocate(wrfnmm_aeta2(1,1,dimensions%zdim))

    !----------------------------------------------------------------------

       ! Allocate memory for local variable grid

       if(.not. allocated(workgrid)) allocate(workgrid(dimensions%xdim,     &
            & dimensions%ydim,1))

       ! Ingest variable from external file

       varstringname = 'GLON'
       call readnetcdfdata(filename,workgrid,varstringname,dimensions%xdim, &
            & dimensions%ydim,1)

       ! Initialize counting variable

       count = 1

       ! Loop through meridional horizontal coordinate

       do j = 1, dimensions%ydim

          ! Loop through zonal horizontal coordinate

          do i = 1, dimensions%xdim
          
             ! Convert from degrees to radians and update the global
             ! longitude array

             lonsgrd(count) = workgrid(i,j,1)
             
             ! Update counting variable
             
             count = count + 1

          end do ! do i = 1, dimensions%xdim

       end do ! do j = 1, dimensions%ydim

       ! Deallocate memory for local variable grid

       if(allocated(workgrid)) deallocate(workgrid)

       ! Allocate memory for local variable grid

       if(.not. allocated(workgrid)) allocate(workgrid(dimensions%xdim,     &
            & dimensions%ydim,1))
       
       ! Ingest variable from external file

       varstringname = 'GLAT'
       call readnetcdfdata(filename,workgrid,varstringname,                 &
            & dimensions%xdim,dimensions%ydim,1)

       ! Initialize counting variable

       count = 1

       ! Loop through meridional horizontal coordinate

       do j = 1, dimensions%ydim

          ! Loop through zonal horizontal coordinate

          do i = 1, dimensions%xdim
          
             ! Convert from degrees to radians and update the global
             ! latitude array
                
             latsgrd(count) = workgrid(i,j,1)

             ! Update counting variable

             count = count + 1

          end do ! do i = 1, dimensions%xdim

       end do ! do j = 1, dimensions%ydim

       ! Deallocate memory for local variable grid

       if(allocated(workgrid)) deallocate(workgrid)

    !----------------------------------------------------------------------

       ! Ingest surface pressure grid from external file

       varstringname = gridvarstring(nvars+1)

       ! Check that the last variable element in array is (at least
       ! analogous) to surface pressure

       if(trim(varstringname) .ne. 'PD' .and. trim(varstringname) .ne.      &
            & 'PSFC') then

          ! Print message to user

          write(6,*) 'The last variable must be PD or PSFC. However, the ', &
               & 'character string is ', trim(varstringname), '. ',         &
               & 'Aborting!'
          
          ! Exit routine
          
          call stop2(22)

       endif ! if(trim(varstringname) .ne. 'PD' .and. trim(varstringname)   &
             ! .ne. 'PSFC') 

       ! Ingest variable from external file

       varstringname = 'PD'
       call readnetcdfdata(filename,wrfnmm_pd,varstringname,                &
            & dimensions%xdim,dimensions%ydim,1)

       ! Ingest variable from external file

       varstringname = 'PDTOP'
       call readnetcdfdata(filename,wrfnmm_pdtop,varstringname,1,1,1)

       ! Ingest variable from external file

       varstringname = 'PT'
       call readnetcdfdata(filename,wrfnmm_pt,varstringname,1,1,1)

       ! Ingest variable from external file

       varstringname = 'AETA1'
       call readnetcdfdata(filename,wrfnmm_aeta1,varstringname,1,1,dimensions%zdim)

       ! Ingest variable from external file

       varstringname = 'AETA2'
       call readnetcdfdata(filename,wrfnmm_aeta2,varstringname,1,1,dimensions%zdim)

       ! Initialize counting variable

       count = 1

       ! Loop through meridional horizontal coordinate

       do j = 1, dimensions%ydim

          ! Loop through zonal horizontal coordinate

          do i = 1, dimensions%xdim
          
             ! Convert from Pa to hPa and update the global surface
             ! pressure array

             spressmn(count) = (wrfnmm_pd(i,j,1) + wrfnmm_pdtop(1,1,1) +    &
                  & wrfnmm_pt(1,1,1))/100.0

             ! Update counting variable

             count = count + 1
             
          end do ! do i = 1, dimensions%xdim
          
       end do ! do j = 1, dimensions%ydim

       ! Define local variable and rescale pressure from Pa to hPa

       ptop = wrfnmm_pt(1,1,1)/100.0

    !----------------------------------------------------------------------

       ! Initialize counting variable

       nn = 0

       ! Loop through vertical coordinate

       do k = 1, dimensions%zdim
       
          ! Update counting variable

          nn = nn + 1
    
          ! Initialize counting variable
       
          count = 1
       
          ! Loop through meridional horizontal coordinate
          
          do j = 1, dimensions%ydim
          
             ! Loop through zonal horizontal coordinate
          
             do i = 1, dimensions%xdim
             
                ! Compute the pressure within the respective layer
                ! (dry hydrostatic pressure)
             
                presslmn(count,k) = (wrfnmm_aeta1(1,1,k)*                   &
                     & wrfnmm_pdtop(1,1,1)) + wrfnmm_aeta2(1,1,k)*(         &
                     & spressmn(count)*100.0 - wrfnmm_pdtop(1,1,1) -        &
                     & wrfnmm_pt(1,1,1)) + wrfnmm_pt(1,1,1)
             
                ! Rescale pressure from Pa to hPa
             
                presslmn(count,k) = presslmn(count,k)/100.0
             
                ! Compute the log of the pressure within the
                ! respective layer
             
                logp(count,k) = -log(presslmn(count,k))

                ! Update counting variable

                count = count + 1

             end do ! do i = 1, dimensions%xdim
       
          end do ! do j = 1, dimensions%ydim

       end do ! do k = 1, dimensions%zdim

       ! Compute local variable

       logp(:,nlevs_pres) = -log(spressmn(:))

       ! Print message to user

       write(6,*) 'Surface pressure (spressmn) min/max range:',             &
            & minval(spressmn),maxval(spressmn)
       write(6,*) 'Longitude range (min/max): ', minval(lonsgrd*rad2deg),   &
            & maxval(lonsgrd*rad2deg)
       write(6,*) 'Latitude range (min/max): ', minval(latsgrd*rad2deg),    & 
            & maxval(latsgrd*rad2deg)

    !----------------------------------------------------------------------

       ! Deallocate memory for all local arrays

       if(allocated(wrfnmm_pd))    deallocate(wrfnmm_pd)
       if(allocated(wrfnmm_pdtop)) deallocate(wrfnmm_pdtop)
       if(allocated(wrfnmm_pt))    deallocate(wrfnmm_pt)
       if(allocated(wrfnmm_aeta1)) deallocate(wrfnmm_aeta1)
       if(allocated(wrfnmm_aeta2)) deallocate(wrfnmm_aeta2)
       if(allocated(presslmn))     deallocate(presslmn)
       if(allocated(spressmn))     deallocate(spressmn)

    !----------------------------------------------------------------------

    end if ! nproc == 0

    !----------------------------------------------------------------------

    ! Define index_pres which is an index array to determine pressure
    ! value for given analysis variable
 
    if(.not. allocated(index_pres)) allocate(index_pres(ndim))

    ! Initialize counting variable

    nn = 0

    ! Loop through total number of analysis variables

    do nvar = 1, nvars
       
       ! Loop through vertical levels

      do k = 1, dimensions%zdim

         ! Update counting variable

        nn = nn + 1

        ! Update global variable array

        index_pres(nn) = k

      end do ! do k = 1, dimensions%zdim

    end do ! do nvar = 1, nvars

    ! Update global variable array

    index_pres(ndim) = nlevs+1

    !----------------------------------------------------------------------

    ! End: Ingest all grid variables required for EnKF routines and
    ! perform necessary conversions; the data is only ingested on the
    ! master node and then subsequently passed to the slave nodes

    !======================================================================

    ! Broadcast all common variables these out to all nodes

    call MPI_Bcast(logp,npts*nlevs_pres,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(ptop,1,mpi_real4,0,MPI_COMM_WORLD,ierr)

    !----------------------------------------------------------------------

    ! Allocate memory for local variable

    if(.not. allocated(gridloc)) allocate(gridloc(3,npts))

    ! Loop through each grid coordinate and perform the coordinate
    ! transform for regular simulation domains

    do nn = 1, npts

       ! Compute local variable

       gridloc(1,nn) = cos(latsgrd(nn))*cos(lonsgrd(nn))

       ! Compute local variable

       gridloc(2,nn) = cos(latsgrd(nn))*sin(lonsgrd(nn))

       ! Compute local variable

       gridloc(3,nn) = sin(latsgrd(nn))

    end do ! do nn = 1, npts

  end subroutine getgridinfo_nmm

  !========================================================================

  ! cross2dot.f90: This subroutine will ingest an array of values and
  ! the respective input dimensions and will return an array of
  ! staggered points on a grid of unstaggered points

  !-------------------------------------------------------------------------

  subroutine cross2dot(varin,sxdim,sydim,szdim,xdim,ydim,zdim,varout)

    ! Define array dimension variables

    integer,                                        intent(in)  :: sxdim, sydim, szdim
    integer,                                        intent(in)  :: xdim, ydim, zdim
    
    ! Define variables passed to subroutine
    
    real,     dimension(sxdim,sydim,szdim), intent(in)  :: varin

    ! Define variables returned by subroutine

    real,     dimension(xdim,ydim,zdim),    intent(out) :: varout    

    !======================================================================

    ! Check in which dimension variable grid is staggered and
    ! interpolate from staggered grid to unstaggered grid

    if (sxdim .gt. xdim) then

       varout = 0.5*(varin(1:xdim,:,:)+varin(2:xdim+1,:,:))
       
    else if (sydim .gt. ydim) then

       varout = 0.5*(varin(:,1:ydim,:)+varin(:,2:ydim+1,:))
       
    else if (szdim .gt. zdim) then

       varout = 0.5*(varin(:,:,1:zdim)+varin(:,:,2:zdim+1))

    else

       varout = varin

    end if

    !======================================================================

    ! Return calculated value

    return

    !======================================================================

  end subroutine cross2dot

  !========================================================================

  ! dot2cross.f90: This subroutine will ingest an array of values and
  ! the respective input dimensions and will return an array of
  ! staggered points on a grid of unstaggered points via linear interpolation.

  !-------------------------------------------------------------------------

  subroutine dot2cross(xdim,ydim,zdim,sxdim,sydim,szdim,varin,   &
       &     varout)

    ! Define array dimension variables

    integer,                                        intent(in)  :: sxdim, sydim, szdim
    integer,                                        intent(in)  :: xdim, ydim, zdim
    
    ! Define variables passed to subroutine
    
    real,     dimension(xdim,ydim,zdim), intent(in)  :: varin

    ! Define variables returned by subroutine

    real,     dimension(sxdim,sydim,szdim),    intent(out) :: varout    

    !======================================================================

    if(sxdim .gt. xdim) then
       ! inverse of:
       ! varout = 0.5*(varin(1:xdim,:,:)+varin(2:xdim+1,:,:))
       varout(2:sxdim-1,:,:) = 0.5*(varin(1:xdim-1,:,:) + varin(2:xdim,:,:))
       varout(1,:,:) = 1.5*varin(1,:,:) - 0.5*varin(2,:,:)
       ! linear extrapolation to outer points (outside of mass grid)
       varout(sxdim,:,:)  = 1.5*varin(xdim,:,:) - 0.5*varin(xdim-1,:,:)

    else if(sydim .gt. ydim) then
       ! inverse of:
       ! varout = 0.5*(varin(:,1:ydim,:)+varin(:,2:ydim+1,:))
       varout(:,2:sydim-1,:) = 0.5*(varin(:,1:ydim-1,:) + varin(:,2:ydim,:))
       varout(:,1,:) = 1.5*varin(:,1,:) - 0.5*varin(:,2,:)
       varout(:,sydim,:)  = 1.5*varin(:,ydim,:) - 0.5*varin(:,ydim-1,:)
    
    else if(szdim .gt. zdim) then

       ! inverse of:
       ! varout = 0.5*(varin(:,:,1:zdim)+varin(:,:,2:zdim+1))
       varout(:,:,2:szdim-1) = 0.5*(varin(:,:,1:zdim-1) + varin(:,:,2:zdim))
       varout(:,:,1) = 1.5*varin(:,:,1) - 0.5*varin(:,:,2)
       varout(:,:,szdim)  = 1.5*varin(:,:,zdim) - 0.5*varin(:,:,zdim-1)

    else
       
       varout = varin

    end if ! if(sxdim .gt. xdim)

    !=======================================================================

    ! Return calculated value

    return

    !=======================================================================

  end subroutine dot2cross

  !=========================================================================

  subroutine gridinfo_cleanup()
    if (allocated(index_pres))    deallocate(index_pres)
    if (allocated(lonsgrd))       deallocate(lonsgrd)
    if (allocated(latsgrd))       deallocate(latsgrd)
    if (allocated(logp))          deallocate(logp)
    if (allocated(gridloc))       deallocate(gridloc)
    if (allocated(gridvarstring)) deallocate(gridvarstring)
  end subroutine gridinfo_cleanup

  !=========================================================================

end module gridinfo
                  nmmb,regional,nlons,nlats,nbackgrounds,fgfileprefixes
