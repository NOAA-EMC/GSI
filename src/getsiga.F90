subroutine getsiga ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getsiga
!   prgmmr: todling
!
! abstract:  Calculate analysis errors from Lanczos-CG results
!
! program history log:
!   2010-03-16  todling  - initial code
!   2010-05-14  todling  - update to use gsi_bundle
!   2010-05-27  todling  - gsi_4dcoupler; remove all user-specific TL-related references
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use constants, only: zero,one
use gsi_4dvar, only: nsubwin,ibdate,lsqrtb,idmodel,l4dvar
use jfunc, only: jiter,miter
use lanczos, only : congrad_siga
use state_vectors
use gsi_4dcouplermod, only: gsi_4dcoupler_putpert
use gsi_bundlemod, only: gsi_bundle
implicit none
! declare local variables
type(gsi_bundle)     :: siga                      ! vector to analysis errors
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: nvecs
integer(i_kind)      :: ier

! consistency checks
if (jiter/=miter-1) return
if (.not.lsqrtb) then
   write(6,*)'getsiga: must set lsqrt=.t. to get analysis errors'
   call stop2(331)
end if

nymd = 10000*ibdate(1)+ibdate(2)*100+ibdate(3)
nhms = 10000*ibdate(4)
if(mype==0) write(6,'(a,i8.8,2x,i6.6)')'getsiga: starting to calculate analysis errors at ',&
             nymd, nhms; call flush(6)

! allocate memory for working arrays
call allocate_state(siga)

! calculate estimate of analysis errors
call congrad_siga(siga,nvecs,ier)

! write out analysis errors
if(ier==0) then
   call gsi_4dcoupler_putpert (siga,nymd,nhms,'tlm','siga')
   if(mype==0) write(6,'(a,i5,a)')'getsiga: complete calculating analysis errors using ',&
                                   nvecs, ' eigenvectors'
else
   if(mype==0) write(6,'(a,i6)')'getsiga: failed to calculate analysis errors, ier= ', ier
endif

! clean up
call deallocate_state(siga)

return
end subroutine getsiga
