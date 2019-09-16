!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module vars_calc_analysis
!!        contains variables shared between modules/subroutines
!!        for the calc_analysis utility
!! Original: 2019-09-16   martin   - original module
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module vars_calc_analysis
  use nemsio_module
  implicit none
  private

  public :: anal_file, fcst_file, incr_file
  public :: idate, nrec, nfday, nfhour, nfminute, nfsecondn, nfsecondd
  public :: nlon, nlat, nlev, nframe, nsoil, ntrac
  public :: lats, lons
  public :: recname, reclevtyp, reclev
  public :: fcstfile, anlfile

  character(len=500) :: anal_file, fcst_file, incr_file
  integer, dimension(7) :: idate
  integer :: nrec, nfday, nfhour, nfminute, nfsecondn, nfsecondd
  integer :: nlon, nlat, nlev, nframe, nsoil, ntrac
  real, allocatable, dimension(:) :: lats, lons
  character(10), allocatable, dimension(:) :: recname, reclevtyp
  integer, allocatable, dimension(:) :: reclev
  type(nemsio_gfile) :: fcstfile, anlfile

end module vars_calc_analysis
