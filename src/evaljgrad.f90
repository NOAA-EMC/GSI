subroutine evaljgrad(xhat,fjcost,gradx,lupdfgs,nprt,calledby)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    evaljgrad
!   prgmmr: tremolet
!
! abstract: Evaluate cost function and its gradient at point xhat.
!
! program history log:
!   2007-04-27  tremolet - initial code
!   2007-07-09  tremolet - observation sensitivity
!   2007-10-18  tremolet - Jc DFI
!   2008-12-04  todling - update interface to intjo
!   2009-01-18  todling - calc dot-prods in quad precision
!                       - add diagnostic call when using strong constraint
!   2009-08-14  lueken  - update documentation
!   2009-11-20  todling - add geos_pgcmtest
!   2010-01-11  todling - bypass call to model_xx based on idmodel as well
!
!   input argument list:
!    xhat - current state estimate (in control space)
!    nprt - print level
!    lupdfgs
!    calledby
!
!   output argument list:
!    fjcost - value of cost function
!    gradx  - gradient (in control space)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use gsi_4dvar, only: nobs_bins, nsubwin, l4dvar, ltlint, lwrtinc, idmodel
use constants, only: zero,zero_quad
use mpimod, only: mype
use jfunc, only: xhatsave
use jcmod, only: ljcdfi
use gridmod, only: lat2,lon2,nsig
use obsmod, only: yobs, lsaveobsens, l_do_adjoint
use obs_sensitivity, only: fcsens
use mod_strong, only: jcstrong,baldiag_inc
use control_vectors
use state_vectors
use bias_predictors
use intjomod, only: intjo
use xhat_vordivmod, only : xhat_vordiv_init, xhat_vordiv_calc, xhat_vordiv_clean

implicit none

! Declare passed variables
type(control_vector), intent(in   ) :: xhat
real(r_quad)        , intent(  out) :: fjcost
type(control_vector), intent(inout) :: gradx
logical             , intent(in   ) :: lupdfgs
integer(i_kind)     , intent(in   ) :: nprt
character(len=*)    , intent(in   ) :: calledby

! Declare local variables  
character(len=*), parameter :: myname='evaljgrad'
type(state_vector) :: sval(nobs_bins), rval(nobs_bins)
type(state_vector) :: mval(nsubwin)
type(predictors) :: sbias, rbias
real(r_quad) :: zjb,zjo,zjc,zjl
integer(i_kind) :: ii,iobs,ibin
logical :: llprt,llouter
character(len=255) :: seqcalls

!**********************************************************************

llprt=(mype==0.and.nprt>=2)
llouter=.false.
seqcalls = trim(calledby)//'::'//trim(myname)

! Allocate local variables
do ii=1,nobs_bins
   call allocate_state(sval(ii))
   call allocate_state(rval(ii))
end do
do ii=1,nsubwin
   call allocate_state(mval(ii))
end do
call allocate_preds(sbias)
call allocate_preds(rbias)

! Contribution from background term
gradx = xhat

if (lsaveobsens) then
! Observation sensitivity right hand side
   do ii=1,gradx%lencv
      gradx%values(ii) = gradx%values(ii) - fcsens%values(ii)
   enddo
else
! Contribution from previous background term
   do ii=1,gradx%lencv
      gradx%values(ii) = gradx%values(ii) + xhatsave%values(ii)
   enddo
endif

zjb=dot_product(gradx,gradx,r_quad)

! Convert from control space to model space
call control2model(xhat,mval,sbias)

if (nprt>=2) then
   do ii=1,nsubwin
      call prt_state_norms(mval(ii),'mval')
   enddo
endif

! Run TL model to fill sval
if ((.not.idmodel).and.l4dvar) then
   call model_tl(mval,sval,llprt)
else
   do ii=1,nobs_bins
      sval(ii)=mval(1)
   enddo
end if

! Perform test of AGCM TLM and ADM
call geos_pgcmtest(mval,sval,.true.)

if (nprt>=2) then
   do ii=1,nobs_bins
      call prt_state_norms(sval(ii),'sval')
   enddo
endif

! Zero gradient
do ii=1,nobs_bins
   rval(ii)=zero
end do
rbias=zero
do ii=1,nsubwin
   mval(ii)=zero
end do

! Compare obs to solution and transpose back to grid (H^T R^{-1} H)
do ibin=1,nobs_bins
   call intjo(yobs(ibin),rval(ibin),rbias,sval(ibin),sbias,ibin)
end do

! Evaluate Jo
call evaljo(zjo,iobs,nprt,llouter)

if (l_do_adjoint) then
! Moisture constraint
   zjl=zero_quad
   if (.not.ltlint) then
      do ibin=1,nobs_bins
         call evalqlim(sval(ibin)%q,zjl,rval(ibin)%q)
      enddo
   endif

   if (ljcdfi) then
      call evaljcdfi(sval,zjc,rval)
   else
! Jc and other 3D-Var terms
! Don't know how to deal with Jc term so comment for now...
!     call eval3dvar(sval,zjc,rval,zdummy)
      zjc=zero_quad
   endif

   if (nprt>=2) then
      do ii=1,nobs_bins
         call prt_state_norms(rval(ii),'rval')
      enddo
   endif

!  Run adjoint model
   if ((.not.idmodel).and.l4dvar) then
      call model_ad(mval,rval,llprt)
   else
      mval(1)=rval(1)
      do ii=2,nobs_bins
         call self_add(mval(1),rval(ii))
      enddo
   end if

   if (nprt>=2) then
      do ii=1,nsubwin
         call prt_state_norms(mval(ii),'mval')
      enddo
   endif

!  Adjoint of convert control var to physical space
   call model2control(mval,rbias,gradx)

!  Cost function
   fjcost=zjb+zjo+zjc+zjl

!  Print diagnostics
   if (nprt>=2) call prt_control_norms(gradx,'gradx')
   if (nprt>=1.and.mype==0) write(6,999)trim(seqcalls),': grepcost J,Jb,Jo,Jc,Jl=',&
                                      fjcost,zjb,zjo,zjc,zjl
endif

! Produce diagnostic when applying strong constraint
if (lupdfgs.and.jcstrong.and.baldiag_inc) call strong_baldiag_inc(sval)

! Save increment (update guess)
if (lupdfgs) then
   call xhat_vordiv_init
   call xhat_vordiv_calc(sval)
   if (nprt>=1.and.mype==0) write(6,*)trim(seqcalls),': evaljgrad: Updating guess'
   call update_guess(sval,sbias)
   call write_all(.false.,mype)
   if (lwrtinc) then
      call inc2guess(sval)
      call write_all(lwrtinc,mype)
   endif
   call xhat_vordiv_clean
endif

! Release memory
call deallocate_preds(rbias)
call deallocate_preds(sbias)
do ii=1,nsubwin
   call deallocate_state(mval(ii))
end do
do ii=1,nobs_bins
   call deallocate_state(rval(ii))
   call deallocate_state(sval(ii))
end do

999 format(2A,5(1X,ES24.18))

return
end subroutine evaljgrad
