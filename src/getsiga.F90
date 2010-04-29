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
#ifdef GEOS_PERT
use geos_pertmod, only :  model_init, model_clean, gsi2pgcm
#endif
implicit none
! declare local variables
type(state_vector)   :: siga                      ! vector to analysis errors
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: nvecs,ierr
character(len=80)    :: sigafname

! consistency checks
if (jiter/=miter-1) return
if (.not.lsqrtb) then
   write(6,*)'getsiga: must set lsqrt=.t. to get analysis errors'
   call stop2(331)
end if

ierr=0
nymd = 10000*ibdate(1)+ibdate(2)*100+ibdate(3)
nhms = 10000*ibdate(4)
if(mype==0) write(6,*)'getsiga: starting to calculate analysis errors at ',& 
             nymd, nhms; call flush(6)

! allocate memory for working arrays
call allocate_state(siga)

! calculate estimate of analysis errors
call congrad_siga(siga,nvecs)

! write out analysis errors
#ifdef GEOS_PERT
  call model_init(ierr,skiptraj=.true.)
  write(sigafname,'(a,i3.3,a)') 'siga', jiter, '.eta'
  call gsi2pgcm(nymd,nhms,siga,'tlm',ierr,filename=sigafname)
  call model_clean()
  if(ierr/=0)then
    if(mype==0) write(6,'(a)') 'getsiga: trouble writing analysis errors'
     call stop2(331)
  endif
#else 
  if(mype==0) write(6,'(a)') 'getsiga: sorry, siga output on NCEP grid not yet implemented'
#endif

! clean up
call deallocate_state(siga)

if(mype==0) write(6,'(a,i5,a)')'getsiga: complete calculating analysis errors using ',&
                                nvecs, ' eigenvectors'

return
end subroutine getsiga
