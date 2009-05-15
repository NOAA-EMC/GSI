subroutine state2control(rval,bval,grad)
!$$$  subprogram documentation block
!
! abstract:  Converts variables from physical space to control space
!            This is also the adjoint of control2state
!
! program history log:
!   2007-04-16  tremolet - initial code
!   2008-11-28  todling  - update to GSI May 2008: add tsen and p3d
!   2009-01-15  todling  - handle predictors in quad precision
!   2009-04-21  derber   - modify call to getstvp to call to getuv
!
!   input argument list:
!     rval - State variable
!   output argument list:
!     grad - Control variable
!
!$$$
use kinds, only: r_kind,i_kind,r_quad
use constants, only: zero
use control_vectors
use state_vectors
use bias_predictors
use gsi_4dvar, only: nsubwin, lsqrtb
use gridmod, only: latlon1n,latlon11
use jfunc, only: nsclen,npclen,nrclen
use mpl_allreducemod, only: mpl_allreduce
implicit none

! Declare passed variables
type(state_vector)  , intent(inout) :: rval(nsubwin)
type(predictors)    , intent(in)    :: bval
type(control_vector), intent(inout) :: grad

! Declare local variables
integer(i_kind) :: ii,jj

!******************************************************************************

if (lsqrtb) then
  write(6,*)'state2control: not for sqrt(B)'
  call stop2(311)
end if

! Loop over control steps
do jj=1,nsubwin

! Adjoint of control to initial state
  do ii=1,latlon1n
    grad%step(jj)%t (ii)=rval(jj)%t (ii)
    grad%step(jj)%oz(ii)=rval(jj)%oz(ii)
    grad%step(jj)%cw(ii)=rval(jj)%cw(ii)
  enddo

  do ii=1,latlon11
    grad%step(jj)%p(ii)  =rval(jj)%p(ii)
    grad%step(jj)%sst(ii)=rval(jj)%sst(ii)
  enddo

! Convert RHS calculations for u,v to st/vp for application of
! background error
  call getuv(rval(jj)%u,rval(jj)%v,grad%step(jj)%st,grad%step(jj)%vp,1)

! Calculate sensible temperature
  call tv_to_tsen_ad(grad%step(jj)%t,rval(jj)%q,rval(jj)%tsen)

! Adjoint of convert input normalized RH to q to add contribution of moisture
! to t, p , and normalized rh
  call normal_rh_to_q_ad(grad%step(jj)%rh,grad%step(jj)%t,rval(jj)%p3d,rval(jj)%q)

! Adjoint to convert ps to 3-d pressure
  call getprs_ad(grad%step(jj)%p,grad%step(jj)%t,rval(jj)%p3d)

end do

do ii=1,nsclen
  grad%predr(ii)=bval%predr(ii)
enddo
do ii=1,npclen
  grad%predp(ii)=bval%predp(ii)
enddo

return
end subroutine state2control
