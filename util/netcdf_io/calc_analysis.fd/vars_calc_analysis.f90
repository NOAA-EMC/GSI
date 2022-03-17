!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module vars_calc_analysis
!!        contains variables shared between modules/subroutines
!!        for the calc_analysis utility
!! Original: 2019-09-18   martin   - original module
!!           2019-09-26   martin   - add support for netCDF read/write
!!           2019-10-24   martin   - support NEMSIO output write
!!           2020-01-17   martin   - parallel IO support added
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module vars_calc_analysis
  use nemsio_module, only: nemsio_gfile
  use module_ncio, only: Dataset
  implicit none
  private

  public :: anal_file, fcst_file, incr_file
  public :: idate, jdate
  public :: idate6, jdate6
  public :: nfday, nfhour, nfminute, nfsecondn, nfsecondd
  public :: nlon, nlat, nlev, nframe, nsoil, ntrac
  public :: lats, lons, vcoord
  public :: anlfile
  public :: work1
  public :: nhrs_assim
  public :: use_nemsio_anl
  public :: fcstncfile, anlncfile, incncfile
  public :: fhrs_pe
  public :: fhr
  public :: mype, npes
  public :: levpe

  character(len=500) :: anal_file, fcst_file, incr_file
  integer, dimension(7) :: idate, jdate
  integer, dimension(6) :: idate6, jdate6
  integer :: nfday, nfhour, nfminute, nfsecondn, nfsecondd
  integer :: nlon, nlat, nlev, nframe, nsoil, ntrac
  real, allocatable, dimension(:) :: lats, lons
  real, allocatable, dimension(:,:,:) :: vcoord
  type(nemsio_gfile) :: anlfile
  real, allocatable, dimension(:) :: work1
  integer :: nhrs_assim, fhr
  logical :: use_nemsio_anl
  type(Dataset) :: fcstncfile, anlncfile, incncfile
  integer, dimension(7) :: fhrs_pe
  integer :: mype, npes
  integer, allocatable, dimension(:) :: levpe

end module vars_calc_analysis
