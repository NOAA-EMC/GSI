subroutine evaljcdfi(svalue,pjc,rvalue)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    evaljcdfi    calculate Jc DFI terms and contribution to gradient
!   prgmmr: tremolet
!
! program history log:
!   2007-10-18  tremolet - initial version
!   2009-01-18  todling  - carry sommation in quad precision
!   2009-08-14  lueken   - updte documentation
!
!   input argument list:
!    svalue
!    rvalue
!
!   output argument list:
!    rvalue
!    pjc
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use constants, only: izero,ione,zero,one
use jcmod, only: wgtdfi,alphajc
use gsi_4dvar, only: nobs_bins
use mpimod, only: mype
use state_vectors
implicit none

! Declare passed variables
type(state_vector), intent(in   ) :: svalue(nobs_bins)
type(state_vector), intent(inout) :: rvalue(nobs_bins)
real(r_quad)      , intent(  out) :: pjc

! Declare local variables
integer(i_kind) :: jj,idfi
real(r_quad),parameter :: half_quad=0.5_r_quad
type(state_vector) :: sfilter,afilter

!************************************************************************************  

idfi = (nobs_bins-ione)/2+ione
call allocate_state(sfilter)
call allocate_state(afilter)

! Compute filtered state
sfilter=zero
do jj=1,nobs_bins
   call self_add(sfilter,wgtdfi(jj),svalue(jj))
enddo

! Compute difference from filtered state
call self_add(sfilter,-one,svalue(idfi))

! Apply Jc multiplicative factor
call self_mul(sfilter,alphajc)

! Compute Jc (norm of difference)
! Jc = 1/2 * wgt * sfilter *sfilter
! afilter = wgt * sfilter
call enorm_state(sfilter,pjc,afilter)
pjc=half_quad*pjc
if (mype==izero) write(6,*)'Jc DFI=',pjc

! Adjoint Jc multiplicative factor
call self_mul(afilter,alphajc)

! Adjoint of difference from filtered state
call self_add(rvalue(idfi),-one,afilter)

! Compute filtered state
do jj=1,nobs_bins
   call self_add(rvalue(jj),wgtdfi(jj),afilter)
enddo

call deallocate_state(sfilter)
call deallocate_state(afilter)

return
end subroutine evaljcdfi
