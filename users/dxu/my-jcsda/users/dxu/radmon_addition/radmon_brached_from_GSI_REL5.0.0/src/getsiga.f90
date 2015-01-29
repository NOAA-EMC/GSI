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
!   2010-08-19  lueken   - add only to module use;no machine code, so use .f90
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
use gsi_4dvar, only: nsubwin,ibdate,lsqrtb,l4dvar,jsiga
use jfunc, only: jiter,miter
use lanczos, only : congrad_siga
use state_vectors, only: allocate_state,deallocate_state
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
if (jiter/=jsiga) return
if (.not.lsqrtb) then
   write(6,*)'getsiga: must set lsqrt=.t. to get analysis errors'
   call stop2(331)
end if

nymd = 10000*ibdate(1)+ibdate(2)*100+ibdate(3)
nhms = 10000*ibdate(4)
if(mype==0) write(6,'(a,i8.8,2x,i6.6)')'getsiga: starting to calculate analysis errors at ',&
             nymd, nhms

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

subroutine view_cv (xhat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    view_cv
!   prgmmr: todling
!
! abstract:  this allow writing CV to file for visualization
!
! program history log:
!   2011-02-23  todling  - initial code
!                          (not sure we'll keep this here)
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
use gsi_4dvar, only: nsubwin,ibdate,lsqrtb,l4dvar,jsiga
use jfunc, only: jiter,miter
use lanczos, only : congrad_siga
use state_vectors, only: allocate_state,deallocate_state
use gsi_4dcouplermod, only: gsi_4dcoupler_putpert
use gsi_bundlemod, only: gsi_bundle
use control_vectors, only: control_vector
use state_vectors, only: allocate_state,deallocate_state
use bias_predictors, only: predictors,allocate_preds,deallocate_preds
implicit none
type(control_vector)     :: xhat
! declare local variables
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: nvecs
integer(i_kind)      :: ii,ier
type(gsi_bundle) :: mval(nsubwin)
type(predictors) :: sbias

! consistency checks
if (.not.lsqrtb) then
   write(6,*)'view_cv: must set lsqrt=.t. to get analysis errors'
   call stop2(331)
end if

nymd = 10000*ibdate(1)+ibdate(2)*100+ibdate(3)
nhms = 10000*ibdate(4)
if(mype==0) write(6,'(a,i8.8,2x,i6.6)')'getsiga: starting to calculate analysis errors at ',&
             nymd, nhms

! Allocate local variables
do ii=1,nsubwin
   call allocate_state(mval(ii))
end do
call allocate_preds(sbias)

call control2model(xhat,mval,sbias)

! write out analysis errors
call gsi_4dcoupler_putpert (mval(1),nymd,nhms,'tlm','cvinc')

! Allocate local variables
call deallocate_preds(sbias)
do ii=1,nsubwin
   call deallocate_state(mval(ii))
end do

return
end subroutine view_cv
