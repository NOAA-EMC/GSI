!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_raddiag                       read rad diag file
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
!   2010-10-05 treadon - refactor code to GSI standard
!   2010-10-08 zhu     - use data_tmp to handle various npred values
!
! contains
!   read_radiag_header - read radiance diagnostic file header
!   read_radiag_data   - read radiance diagnostic file data
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
!     call read_radiag_data( ftin, header_fix, data_fix, data_chan, ipchan_radiag,iflag )
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

  use kinds, only:  i_kind,r_single
  implicit none

! Declare public and private
  private

  public :: diag_header_fix_list
  public :: diag_header_chan_list
  public :: diag_data_fix_list
  public :: diag_data_chan_list
  public :: diag_data_extra_list
  public :: read_radiag_header
  public :: read_radiag_data



! Declare structures for radiance diagnostic file information
  type diag_header_fix_list
     sequence
     character(len=20) :: isis           ! sat and sensor type
     character(len=10) :: id             ! sat type
     character(len=10) :: obstype        ! observation type
     integer(i_kind) :: jiter            ! outer loop counter
     integer(i_kind) :: nchan            ! number of channels in the sensor
     integer(i_kind) :: npred            ! number of updating bias correction predictors
     integer(i_kind) :: idate            ! time (yyyymmddhh)
     integer(i_kind) :: ireal            ! # of real elements in the fix part of a data record
     integer(i_kind) :: ipchan           ! # of elements for each channel except for bias correction terms
     integer(i_kind) :: iextra           ! # of extra elements for each channel
     integer(i_kind) :: jextra           ! # of extra elements
  end type diag_header_fix_list
  
  type diag_header_chan_list
     sequence
     real(r_single) :: freq              ! frequency (Hz)
     real(r_single) :: polar             ! polarization
     real(r_single) :: wave              ! wave number (cm^-1)
     real(r_single) :: varch             ! error variance (or SD error?)
     real(r_single) :: tlapmean          ! mean lapse rate
     integer(i_kind):: iuse              ! use flag
     integer(i_kind):: nuchan            ! sensor relative channel number
     integer(i_kind):: iochan            ! satinfo relative channel number
  end type diag_header_chan_list

  type diag_data_fix_list
     sequence
     real(r_single) :: lat               ! latitude (deg)
     real(r_single) :: lon               ! longitude (deg)
     real(r_single) :: zsges             ! guess elevation at obs location (m)
     real(r_single) :: obstime           ! observation time relative to analysis
     real(r_single) :: senscn_pos        ! sensor scan position (integer(i_kind))
     real(r_single) :: satzen_ang        ! satellite zenith angle (deg)
     real(r_single) :: satazm_ang        ! satellite azimuth angle (deg)
     real(r_single) :: solzen_ang        ! solar zenith angle (deg)
     real(r_single) :: solazm_ang        ! solar azimumth angle (deg)
     real(r_single) :: sungln_ang        ! sun glint angle (deg)
     real(r_single) :: water_frac        ! fractional coverage by water
     real(r_single) :: land_frac         ! fractional coverage by land
     real(r_single) :: ice_frac          ! fractional coverage by ice
     real(r_single) :: snow_frac         ! fractional coverage by snow
     real(r_single) :: water_temp        ! surface temperature over water (K)
     real(r_single) :: land_temp         ! surface temperature over land (K)
     real(r_single) :: ice_temp          ! surface temperature over ice (K)
     real(r_single) :: snow_temp         ! surface temperature over snow (K)
     real(r_single) :: soil_temp         ! soil temperature (K)
     real(r_single) :: soil_mois         ! soil moisture 
     real(r_single) :: land_type         ! land type (integer(i_kind))
     real(r_single) :: veg_frac          ! vegetation fraction
     real(r_single) :: snow_depth        ! snow depth
     real(r_single) :: sfc_wndspd        ! surface wind speed
     real(r_single) :: qcdiag1           ! ir=cloud fraction, mw=cloud liquid water
     real(r_single) :: qcdiag2           ! ir=cloud top pressure, mw=total column water
  end type diag_data_fix_list

  type diag_data_chan_list
     sequence
     real(r_single) :: tbobs              ! Tb (obs) (K)
     real(r_single) :: omgbc              ! Tb_(obs) - Tb_(simulated w/ bc)  (K)
     real(r_single) :: omgnbc             ! Tb_(obs) - Tb_(simulated_w/o bc) (K)
     real(r_single) :: errinv             ! inverse error (K**(-1))
     real(r_single) :: qcmark             ! quality control mark
     real(r_single) :: emiss              ! surface emissivity
     real(r_single) :: tlap               ! temperature lapse rate
     real(r_single) :: bicons             ! constant bias correction term
     real(r_single) :: biang              ! scan angle bias correction term
     real(r_single) :: biclw              ! CLW bias correction term
     real(r_single) :: bilap2             ! square lapse rate bias correction term
     real(r_single) :: bilap              ! lapse rate bias correction term
     real(r_single) :: bifix              ! fixed angle dependent bias
  end type diag_data_chan_list

  type diag_data_extra_list
     sequence
     real(r_single) :: extra              ! extra information
  end type diag_data_extra_list
  

contains

subroutine read_radiag_header(ftin,npred,ireal_radiag,ipchan_radiag,header_fix,header_chan,iflag )
!                .      .    .                                       .
! subprogram:    read_diag_header                 read rad diag header
!   prgmmr: tahara           org: np20                date: 2003-01-01
!
! abstract:  This routine reads the header record from a radiance
!            diagnostic file
!
! program history log:
!   2010-10-05 treadon - add this doc block
!
! input argument list:
!   ftin          - unit number connected to diagnostic file 
!   npred         - number of bias correction terms
!   ireal_radiag  - number of real entries per spot in radiance diagnostic file
!   ipchan_radiag - number of entries per channel per spot in radiance diagnostic file
!
! output argument list:
!   header_fix    - header information structure
!   header_chan   - channel information structure
!   iflag         - error code
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! Declare passed arguments
  integer(i_kind),intent(in)             :: ftin
  integer(i_kind),intent(in)             :: npred
  integer(i_kind),intent(in)             :: ireal_radiag
  integer(i_kind),intent(in)             :: ipchan_radiag
  type(diag_header_fix_list ),intent(out):: header_fix
  type(diag_header_chan_list),pointer    :: header_chan(:)
  integer(i_kind),intent(out)            :: iflag
    

!  Declare local variables
  integer(i_kind),save :: nchan_last = -1
  integer(i_kind) :: ich
  
  external:: stop2
    
! Read header (fixed part)
  read(ftin,IOSTAT=iflag) header_fix
  if (iflag/=0) return

! Check header
  if (header_fix%ireal /= ireal_radiag  .or. &
       header_fix%ipchan /= ipchan_radiag) then
     write(6,*) 'READ_RADIAG_HEADER:  ***ERROR*** UNEXPECTED DATA RECORD FORMAT ',&
          ' ireal,ireal_radiag=',header_fix%ireal,ireal_radiag,&
          ' ipchan,ipchan_radiag=',header_fix%ipchan ,ipchan_radiag
     call stop2(335)
  endif

  if (header_fix%npred  /= npred) &
       write(6,*) 'READ_RADIAG_HEADER:  **WARNING** header_fix%npred,npred=',header_fix%npred,npred
  
  if (header_fix%iextra /= 0) &
       write(6,*)'READ_RADIAG_HEADER:  extra diagnostic information available, ',&
       'iextra=',header_fix%iextra


! Allocate as needed
  if (header_fix%nchan /= nchan_last)then
     if (nchan_last > 0) deallocate(header_chan)
     allocate(header_chan( header_fix%nchan))
     nchan_last = header_fix%nchan
  endif

! Read header (channel part)
  do ich=1, header_fix%nchan
     read(ftin,IOSTAT=iflag) header_chan(ich)
     if (iflag/=0) return
  end do
     
end subroutine read_radiag_header

subroutine read_radiag_data(ftin,header_fix,data_fix,data_chan,data_extra,ipchan_radiag,iflag )
!                .      .    .                                       .
! subprogram:    read_radiag_dat                    read rad diag data
!   prgmmr: tahara           org: np20                date: 2003-01-01
!
! abstract:  This routine reads the data record from a radiance
!            diagnostic file
!
! program history log:
!   2010-10-05 treadon - add this doc block
!
! input argument list:
!   ftin - unit number connected to diagnostic file
!   header_fix - header information structure
!
! output argument list:
!   data_fix   - spot header information structure
!   data_chan  - spot channel information structure
!   data_extra - spot extra information
!   iflag      - error code
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! Declare passed arguments
  integer(i_kind),intent(in)             :: ftin
  integer(i_kind),intent(in)             :: ipchan_radiag
  type(diag_header_fix_list ),intent(in) :: header_fix
  type(diag_data_fix_list)   ,intent(out):: data_fix
  type(diag_data_chan_list)  ,pointer    :: data_chan(:)
  type(diag_data_extra_list) ,pointer    :: data_extra(:)
  integer(i_kind),intent(out)            :: iflag
    
! Declare local variables
  integer(i_kind),save :: nchan_last = -1
  integer(i_kind),save :: iextra_last = -1
  integer(i_kind) :: ich,in,idiag
  real(r_single),dimension(:,:),allocatable :: data_tmp

! Allocate arrays as needed
  if (header_fix%nchan /= nchan_last) then
     if (nchan_last > 0) deallocate(data_chan)
     allocate(data_chan( header_fix%nchan))
     nchan_last = header_fix%nchan
  endif

  if (header_fix%iextra /= iextra_last) then
     if (iextra_last > 0) deallocate(data_extra)
     allocate(data_extra(header_fix%iextra))
     iextra_last = header_fix%iextra
  endif

! Read data record
  idiag=ipchan_radiag+header_fix%npred+2
  allocate(data_tmp(idiag,header_fix%nchan))
  if (header_fix%iextra == 0) then
     read(ftin,IOSTAT=iflag) data_fix, data_tmp
  else
     read(ftin,IOSTAT=iflag) data_fix, data_tmp, data_extra
  endif

  do ich=1,header_fix%nchan
     data_chan(ich)%tbobs =data_tmp(1,ich)
     data_chan(ich)%omgbc =data_tmp(2,ich)
     data_chan(ich)%omgnbc=data_tmp(3,ich)
     data_chan(ich)%errinv=data_tmp(4,ich)
     data_chan(ich)%qcmark=data_tmp(5,ich)
     data_chan(ich)%emiss =data_tmp(6,ich)
     data_chan(ich)%tlap  =data_tmp(7,ich)
     data_chan(ich)%bicons=data_tmp(8,ich)
     data_chan(ich)%biang =data_tmp(9,ich)
     data_chan(ich)%biclw =data_tmp(10,ich)
     data_chan(ich)%bilap2=data_tmp(11,ich)
     data_chan(ich)%bilap =data_tmp(12,ich)
     data_chan(ich)%bifix =data_tmp(idiag-1,ich)
  end do
  deallocate(data_tmp)
    
end subroutine read_radiag_data

end module read_diag

