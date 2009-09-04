subroutine control2state(xhat,sval,bval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    control2state
!   prgmmr: tremolet
!
! abstract:  Converts control variable to physical space
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2008-11-28  todling  - add calc of 3dp; upd rh_to_q (Cucurull 2007-07-26)
!   2009-04-21  derber   - modify call to getuv to getuv(*,0)
!   2009-08-14  lueken   - update documentation
!
!   input argument list:
!     xhat - Control variable
!     sval - State variable
!     bval - Bias predictors
!
!   output argument list:
!     sval - State variable
!     bval - Bias predictors
!
!$$$ end documentation block
use kinds, only: r_kind,i_kind
use control_vectors
use state_vectors
use bias_predictors
use gsi_4dvar, only: nsubwin, nobs_bins, l4dvar, lsqrtb
use gridmod, only: latlon1n,latlon11
use jfunc, only: nsclen,npclen,nrclen
implicit none
  
! Declare passed variables  
type(control_vector), intent(in)    :: xhat
type(state_vector)  , intent(inout) :: sval(nsubwin)
type(predictors)    , intent(inout) :: bval

! Declare local variables  	
integer(i_kind) :: ii,jj

!******************************************************************************

if (lsqrtb) then
  write(6,*)'control2state: not for sqrt(B)'
  call stop2(106)
end if
if (nsubwin/=1 .and. .not.l4dvar) then
  write(6,*)'control2state: error 3dvar',nsubwin,l4dvar
  call stop2(107)
end if

! Loop over control steps
do jj=1,nsubwin

! Get 3d pressure
  call getprs_tl(xhat%step(jj)%p,xhat%step(jj)%t,sval(jj)%p3d)

! Convert input normalized RH to q
  call normal_rh_to_q(xhat%step(jj)%rh,xhat%step(jj)%t,sval(jj)%p3d,sval(jj)%q)

! Calculate sensible temperature
  call tv_to_tsen(xhat%step(jj)%t,sval(jj)%q,sval(jj)%tsen)

! Convert streamfunction and velocity potential to u,v
  call getuv(sval(jj)%u,sval(jj)%v,xhat%step(jj)%st,xhat%step(jj)%vp,0)

! Copy other variables
  do ii=1,latlon1n
    sval(jj)%t (ii)=xhat%step(jj)%t(ii)
    sval(jj)%oz(ii)=xhat%step(jj)%oz(ii)
    sval(jj)%cw(ii)=xhat%step(jj)%cw(ii)
  enddo

  do ii=1,latlon11
    sval(jj)%p(ii)=xhat%step(jj)%p(ii)
    sval(jj)%sst(ii)=xhat%step(jj)%sst(ii)
  enddo

end do

! Biases
do ii=1,nsclen
  bval%predr(ii)=xhat%predr(ii)
enddo

do ii=1,npclen
  bval%predp(ii)=xhat%predp(ii)
enddo

return
end subroutine control2state
