subroutine control2model(xhat,sval,bval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    control2model
!   prgmmr: tremolet
!
! abstract:  Converts control variable to physical space
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2007-04-27  tremolet - multiply by sqrt(B) (from ckerror D. Parrish)
!   2008-12-04  todling  - update interface to ckgcov; add tsen/p3d
!   2008-12-29  todling  - add call to strong balance contraint
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
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use kinds, only: r_kind,i_kind
use constants, only: izero,ione
use control_vectors
use state_vectors
use bias_predictors
use gsi_4dvar, only: nsubwin, l4dvar, lsqrtb
use gridmod, only: lat2,lon2,nsig,nnnn1o
use jfunc, only: nsclen,npclen
use berror, only: varprd,fpsproj
use balmod, only: balance
implicit none
  
! Declare passed variables  
type(control_vector), intent(in)    :: xhat
type(state_vector)  , intent(inout) :: sval(nsubwin)
type(predictors)    , intent(inout) :: bval

! Declare local variables  	
real(r_kind),dimension(lat2,lon2,nsig) :: workst,workvp,workrh
integer(i_kind) :: ii,jj

!******************************************************************************

if (.not.lsqrtb) then
  write(6,*)'control2model: assumes lsqrtb'
  call stop2(104)
end if
if (nsubwin/=ione .and. .not.l4dvar) then
  write(6,*)'control2model: error 3dvar',nsubwin
  call stop2(105)
end if

! Loop over control steps
do jj=1,nsubwin

! Multiply by sqrt of background error (ckerror)
! -----------------------------------------------------------------------------
! Apply sqrt of variance, as well as vertical & horizontal parts of background
! error
  call ckgcov(xhat%step(jj)%values(:),workst,workvp, &
              sval(jj)%t,sval(jj)%p,workrh,sval(jj)%oz, &
              sval(jj)%sst,sval(jj)%cw,nnnn1o)

! Balance equation
  call balance(sval(jj)%t,sval(jj)%p,workst,workvp,fpsproj)

! -----------------------------------------------------------------------------

! Get 3d pressure
  call getprs_tl(sval(jj)%p,sval(jj)%t,sval(jj)%p3d)

! Convert input normalized RH to q
  call normal_rh_to_q(workrh,sval(jj)%t,sval(jj)%p3d,sval(jj)%q)

! Calculate sensible temperature
  call tv_to_tsen(sval(jj)%t,sval(jj)%q,sval(jj)%tsen)

! Convert streamfunction and velocity potential to u,v
  call getuv(sval(jj)%u,sval(jj)%v,workst,workvp,izero)

end do

! Bias correction terms
do ii=1,nsclen
  bval%predr(ii)=xhat%predr(ii)*sqrt(varprd(ii))
enddo

do ii=1,npclen
  bval%predp(ii)=xhat%predp(ii)*sqrt(varprd(nsclen+ii))
enddo

return
end subroutine control2model
