!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_diag                          read rad diag file
!   prgmmr: tahara           org: np20                date: 2003-01-01
!
! abstract:  This module contains code to process radiance
!            diagnostic files.  The module defines structures
!            to contain information from the radiance
!            diagnostic files and then provides two routines
!            to access contents of the file.
!
! program history log:
!   2005-07-22 treadon - add this doc block
!
! contains
!   read_diag_header - read radiance diagnostic file header
!   read_diag_data   - read radiance diagnostic file data
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!------------------------------------------------------------
!
! Module to read new satellite diagnostic files
!
! Example:
!   !--- initialize
!   use read_diag
!   type(diag_header_fix_list )         :: header_fix
!   type(diag_header_chan_list),pointer :: header_chan(:)
!   type(diag_data_fix_list   )         :: data_fix
!   type(diag_data_chan_list  ),pointer :: data_chan(:)
!   !--- read
!   open(ftin,FILE='filename',STATUS='OLD',ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
!   call read_diag_header( ftin, header_fix, header_chan )
!   nchan = header_fix%nchan
!   freq1 = header_chan(1)%freq
!   do
!     call read_diag_data( ftin, header_fix, data_fix, data_chan, iflag )
!     if( iflag /= 0 )exit
!     rlat = data_fix%lat
!     rlat = data_fix%lon
!     obs1 = data_chan(1)%tbobs
!   enddo
!   close(ftin)
!
! Created by Y.Tahara in Jan,2003
!
!------------------------------------------------------------


module read_diag

  !--- implicit

  implicit none


  !--- public & private

  private

  public :: diag_header_fix_list
  public :: diag_header_chan_list
  public :: diag_data_fix_list
  public :: diag_data_chan_list
  public :: diag_data_extra_list

  public :: read_diag_header
  public :: read_diag_data


  !--- fp_diag
  
  integer,parameter :: fp_diag = 4


  !--- diagnostic file format - header
  
  type diag_header_fix_list
    sequence
    character(len=20) :: isis       ! sat and sensor type
    character(len=10) :: id         ! sat type
    character(len=10) :: obstype	! observation type
    integer           :: jiter      ! outer loop counter
    integer           :: nchan		! number of channels in the sensor
    integer           :: npred		! number of updating bias correction predictors
    integer           :: idate		! time (yyyymmddhh)
    integer           :: ireal		! # of real elements in the fix part of a data record
    integer           :: ipchan		! # of elements for each channel except for bias correction terms
    integer           :: iextra		! # of extra elements for each channel
    integer           :: jextra     ! # of extra elements
  end type diag_header_fix_list

  type diag_header_chan_list
    sequence
    real(fp_diag) :: freq		! frequency (Hz)
    real(fp_diag) :: polar		! polarization
    real(fp_diag) :: wave		! wave number (cm^-1)
    real(fp_diag) :: varch		! error variance (or SD error?)
    real(fp_diag) :: tlapmean   ! mean lapse rate
    integer       :: iuse		! use flag
    integer       :: nuchan		! sensor relative channel number
    integer       :: iochan     ! satinfo relative channel number
  end type diag_header_chan_list

  !--- diagnostic file format - data

  integer,parameter :: IREAL_RESERVE  = 26
  integer,parameter :: IPCHAN_RESERVE = 7
  integer,parameter :: NPRED_RESERVE  = 5
  
  type diag_data_fix_list
    sequence
    real(fp_diag) :: lat           	! latitude (deg)
    real(fp_diag) :: lon           	! longitude (deg)
    real(fp_diag) :: zsges          ! guess elevation at obs location (m)
    real(fp_diag) :: obstime        ! observation time relative to analysis
    real(fp_diag) :: senscn_pos     ! sensor scan position (integer)
    real(fp_diag) :: satzen_ang     ! satellite zenith angle (deg)
    real(fp_diag) :: satazm_ang     ! satellite azimuth angle (deg)
    real(fp_diag) :: solzen_ang     ! solar zenith angle (deg)
    real(fp_diag) :: solazm_ang     ! solar azimumth angle (deg)
    real(fp_diag) :: sungln_ang     ! sun glint angle (deg)
    real(fp_diag) :: water_frac     ! fractional coverage by water
    real(fp_diag) :: land_frac      ! fractional coverage by land
    real(fp_diag) :: ice_frac       ! fractional coverage by ice
    real(fp_diag) :: snow_frac      ! fractional coverage by snow
    real(fp_diag) :: water_temp     ! surface temperature over water (K)
    real(fp_diag) :: land_temp      ! surface temperature over land (K)
    real(fp_diag) :: ice_temp       ! surface temperature over ice (K)
    real(fp_diag) :: snow_temp      ! surface temperature over snow (K)
    real(fp_diag) :: soil_temp      ! soil temperature (K)
    real(fp_diag) :: soil_mois      ! soil moisture 
    real(fp_diag) :: land_type      ! land type (integer)
    real(fp_diag) :: veg_frac       ! vegetation fraction
    real(fp_diag) :: snow_depth     ! snow depth
    real(fp_diag) :: sfc_wndspd     ! surface wind speed
    real(fp_diag) :: qcdiag1        ! ir=cloud fraction, mw=cloud liquid water
    real(fp_diag) :: qcdiag2        ! ir=cloud top pressure, mw=total column water
  end type diag_data_fix_list

  type diag_data_chan_list
    sequence
    real(fp_diag) :: tbobs              ! Tb (obs) (K)
    real(fp_diag) :: omgbc              ! Tb_(obs) - Tb_(simulated w/ bc)  (K)
    real(fp_diag) :: omgnbc             ! Tb_(obs) - Tb_(simulated_w/o bc) (K)
    real(fp_diag) :: errinv             ! inverse error (K**(-1))
    real(fp_diag) :: qcmark             ! quality control mark
    real(fp_diag) :: emiss              ! surface emissivity
    real(fp_diag) :: tlap               ! temperature lapse rate
    real(fp_diag) :: bifix              ! fixed angle dependent bias
    real(fp_diag) :: bilap              ! lapse rate bias correction term
    real(fp_diag) :: bilap2             ! square lapse rate bias correction term
    real(fp_diag) :: bicons             ! constant bias correction term
    real(fp_diag) :: biang              ! scan angle bias correction term
    real(fp_diag) :: biclw              ! CLW bias correction term
  end type diag_data_chan_list

  type diag_data_extra_list
  sequence
    real(fp_diag) :: extra              ! extra information
  end type diag_data_extra_list


 contains


  !------------------------------------------------------------
  ! Read a header record of a diagnostic file
  !------------------------------------------------------------

  subroutine read_diag_header( ftin, header_fix, header_chan )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(out) :: header_fix
    type(diag_header_chan_list),pointer     :: header_chan(:)
    
    !--- variables
    
    integer,save :: nchan_last = -1
    
    integer :: ich
    
    !--- read header (fix part)

    read(ftin) header_fix

    !--- check header
    
    if( header_fix%ireal  /= IREAL_RESERVE  .or. &
        header_fix%ipchan /= IPCHAN_RESERVE .or. &
        header_fix%npred  /= NPRED_RESERVE  ) then

      print *, '### ERROR: UNEXPECTED DATA RECORD FORMAT'
      print *, 'ireal  =', header_fix%ireal  
      print *, 'ipchan =', header_fix%ipchan 
      print *, 'npred  =', header_fix%npred  
      stop 99

    endif

    if (header_fix%iextra /= 0) then
       write(6,*)'READ_DIAG_HEADER:  extra diagnostic information available, ',&
            'iextra=',header_fix%iextra
    endif

    !--- allocate if necessary

    if( header_fix%nchan /= nchan_last )then
      if( nchan_last > 0 )then
        deallocate( header_chan )
      endif
      allocate( header_chan( header_fix%nchan ) )
      nchan_last = header_fix%nchan
    endif

    !--- read header (channel part)
    
    do ich=1, header_fix%nchan
      read(ftin) header_chan(ich)
    enddo

  end subroutine read_diag_header



  !------------------------------------------------------------
  ! Read a data record of the diagnostic file
  !------------------------------------------------------------

  subroutine read_diag_data( ftin, header_fix, data_fix, data_chan, data_extra, iflag )
  
    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(in)  :: header_fix
    type(diag_data_fix_list)   ,intent(out) :: data_fix
    type(diag_data_chan_list)  ,pointer     :: data_chan(:)
    type(diag_data_extra_list) ,pointer     :: data_extra(:)
    integer                    ,intent(out) :: iflag
    
    !--- variables

    integer,save :: nchan_last = -1
    integer,save :: iextra_last = -1
   
    integer :: ich

    !--- allocate if necessary

    if( header_fix%nchan /= nchan_last )then
      if( nchan_last > 0 )then
        deallocate( data_chan )
      endif
      allocate( data_chan( header_fix%nchan ) )
      nchan_last = header_fix%nchan
    endif

    if (header_fix%iextra /= iextra_last) then
       if (iextra_last > 0) then
          deallocate (data_extra)
       endif
       allocate( data_extra(header_fix%iextra) )
       iextra_last = header_fix%iextra
    endif

    !--- read a record

    if (header_fix%iextra == 0) then
       read(ftin,IOSTAT=iflag) data_fix, data_chan
    else
       read(ftin,IOSTAT=iflag) data_fix, data_chan, data_extra
    endif
    
  end subroutine read_diag_data


end module read_diag

