module smooth_mod

use mpisetup
use params, only:  nlons,nlats,smoothparm
use statevec, only: ndim
use kinds, only: r_kind
use gridinfo, only: npts

implicit none

private
public :: smooth

contains

subroutine smooth(grids)
real(r_single), intent(inout) :: grids(npts,ndim) ! there are ndim 2d grids.
! stub - not yet implemented.
if (nproc .eq. 0) print *,'nmmb inflation smoothing not yet implemented!'
end subroutine smooth

end module smooth_mod
