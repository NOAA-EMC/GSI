subroutine jgrad(xhat,yhat,fjcost,gradx,lupdfgs,nprt,calledby)

!$$$  subprogram documentation block
!
! abstract: Evaluate cost function and its gradient at point xhat.
!
! program history log:
!   2009-08-15  tremolet - initial code
!   2010-09-31  el akkraoui - re-examine and update gradient calculation
!$$$

use kinds, only: r_kind,i_kind,r_quad
use gsi_4dvar, only: nobs_bins, nsubwin, l4dvar, ltlint, iwrtinc, idmodel
use constants, only: zero,zero_quad
use mpimod, only: mype
use jfunc, only : xhatsave,yhatsave
use jcmod, only: ljcdfi
use jfunc, only: nclen,l_foto,xhat_dt,jiter,jiterend
use gridmod, only: lat2,lon2,nsig
use obsmod, only: yobs, lsaveobsens, l_do_adjoint
use obs_sensitivity, only: fcsens
use mod_strong, only: l_tlnmc,baldiag_inc
use control_vectors, only: control_vector
use control_vectors, only: allocate_cv,deallocate_cv,prt_control_norms
use control_vectors, only: dot_product,assignment(=)
use state_vectors, only: allocate_state,deallocate_state,prt_state_norms
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: self_add,assignment(=)
use bias_predictors, only: predictors,allocate_preds,deallocate_preds,assignment(=)
use intjomod, only: intjo
use gsi_4dcouplermod, only: gsi_4dcoupler_grtests
use xhat_vordivmod, only : xhat_vordiv_init, xhat_vordiv_calc, xhat_vordiv_clean

implicit none

! Declare passed variables
type(control_vector), intent(in   ) :: xhat, yhat
real(r_quad)        , intent(  out) :: fjcost
type(control_vector), intent(inout) :: gradx
logical             , intent(in   ) :: lupdfgs
integer(i_kind)     , intent(in   ) :: nprt
character(len=*)    , intent(in   ) :: calledby

! Declare local variables  
character(len=*), parameter :: myname='jgrad'
type(control_vector)   :: xnew, ynew 
type(gsi_bundle)     :: sval(nobs_bins), rval(nobs_bins)
type(gsi_bundle)     :: mval(nsubwin)
type(predictors)       :: sbias, rbias
real(r_quad)           :: zjb,zjo,zjc,zjl
integer(i_kind)        :: i,ii,iobs,ibin
real(r_kind)           :: zdummy(lat2,lon2,nsig)
logical                :: llprt,llouter
character(len=255)     :: seqcalls

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

call allocate_cv(xnew)
call allocate_cv(ynew)

!
zjl=zero_quad  ! Moisture constraint???



if (l4dvar) then
! Convert from control space to model space
  call control2state(xhat,mval,sbias)

! Perform test of AGCM TLM and ADM
  call gsi_4dcoupler_grtests(mval,sval,nsubwin,nobs_bins)

! Run TL model to fill sval
  call model_tl(mval,sval,llprt)
else

! Convert from control space directly to physical
! space for comparison with obs.
  call control2state(xhat,sval,sbias)
end if

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
  gradx=zero
! Moisture constraint
   zjl=zero_quad
   if (.not.ltlint) then
      do ibin=1,nobs_bins
         call evalqlim(sval(ibin),zjl,rval(ibin))
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

 if (l4dvar) then
!   Run adjoint model
    call model_ad(mval,rval,llprt)

!   Adjoint of convert control var to physical space
    call state2control(mval,rbias,gradx)
 else

!   Convert to control space directly from physical space.
    if (nobs_bins>1) then
       do ii=nobs_bins,2,-1
          call self_add(rval(1),rval(ii))
       end do
    end if
    call state2control(rval,rbias,gradx)
 end if

 
 ! Contribution from current and previous backgroun term
   do i=1,nclen
     xnew%values(i) = xhat%values(i)+ xhatsave%values(i)
     ynew%values(i) = yhat%values(i)+ yhatsave%values(i)
   end do
  zjb=dot_product(xnew,ynew,r_quad)
!$omp parallel do
  do i=1,nclen
    gradx%values(i)=gradx%values(i)+yhat%values(i)+ yhatsave%values(i)
  end do
!$omp end parallel do

! Cost function
  fjcost=zjb+zjo+zjc+zjl

! Print diagnostics
  if (nprt>=2) then
    if (l4dvar) then
      do ii=1,nsubwin
         call prt_state_norms(mval(ii),'mval')
      enddo
    endif
    call prt_control_norms(gradx,'gradx')
  endif
  if (nprt>=1.and.mype==0) write(6,999)trim(seqcalls),': grepcost J,Jb,Jo,Jc,Jl=',&
                              fjcost,zjb,zjo,zjc,zjl
endif

! Produce diagnostic when applying strong constraint
if (lupdfgs.and.l_tlnmc.and.baldiag_inc) call strong_baldiag_inc(sval,size(sval))

! Save increment (update guess)
if (lupdfgs) then
! Calculate increments of vorticity/divergence
   call xhat_vordiv_init
   call xhat_vordiv_calc(sval)

! Overwrite guess with increment (4d-var only, for now)
  if (iwrtinc>0) then
    if (mype==0) write(6,*)trim(seqcalls),': Saving increment to file'
    call inc2guess(sval)
    call write_all(iwrtinc,mype)
    call prt_guess('increment')
  else ! Update guess (model background, bias correction) fields
     if (mype==0) write(6,*)trim(seqcalls),': Updating guess'
     call update_guess(sval,sbias)
     if(jiter == jiterend)call write_all(iwrtinc,mype)
     call prt_guess('analysis')
  endif

! Clean up increments of vorticity/divergence
  call xhat_vordiv_clean
endif

! Release memory
call deallocate_cv(xnew)
call deallocate_cv(ynew)
call deallocate_preds(rbias)
call deallocate_preds(sbias)
do ii=1,nsubwin
   call deallocate_state(mval(ii))
end do
do ii=1,nobs_bins
   call deallocate_state(rval(ii))
   call deallocate_state(sval(ii))
end do

999 format(2A,5(1X,ES25.18))

return
end subroutine jgrad
