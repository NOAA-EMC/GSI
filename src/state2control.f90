subroutine state2control(rval,bval,grad)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    state2control
!   prgmmr: tremolet
!
! abstract:  Converts variables from physical space to control space
!            This is also the adjoint of control2state
!
! program history log:
!   2007-04-16  tremolet - initial code
!   2008-11-28  todling  - update to GSI May 2008: add tsen and p3d
!   2009-01-15  todling  - handle predictors in quad precision
!   2009-04-21  derber   - modify call to getstvp to call to getuv
!   2009-06-15  parrish  - add call to strong_bk_ad when l_hyb_ens=.true. (hybrid ensemble run)
!   2009-08-12  lueken   - update documentation
!   2009-11-27  parrish  - for uv_hyb_ens=.true., then ensemble perturbations contain u,v instead of st,vp
!                            so introduce extra code to handle this case.
!   2010-02-20  parrish  - introduce modifications to allow dual resolution capability when running
!                            in hybrid ensemble mode.
!   2010-03-24  zhu      - use cstate for generalizing control variable
!
!   input argument list:
!     rval - State variable
!     bval
!   output argument list:
!     grad - Control variable
!
!$$$
use kinds, only: i_kind
use constants, only: ione,zero,izero
use control_vectors
use state_vectors
use bias_predictors
use gsi_4dvar, only: nsubwin, lsqrtb
use gridmod, only: latlon1n,latlon11
use jfunc, only: nsclen,npclen
use hybrid_ensemble_parameters, only: l_hyb_ens,uv_hyb_ens,dual_res
use balmod, only: strong_bk_ad
use hybrid_ensemble_isotropic_regional, only: ensemble_forward_model_ad
use hybrid_ensemble_isotropic_regional, only: ensemble_forward_model_ad_dual_res
implicit none

! Declare passed variables
type(state_vector)  , intent(inout) :: rval(nsubwin)
type(predictors)    , intent(in   ) :: bval
type(control_vector), intent(inout) :: grad

! Declare local variables
integer(i_kind) :: ii,jj
real(r_kind),dimension(latlon11):: p,sst
real(r_kind),dimension(latlon1n):: t,vp,st,rh,oz,cw,u,v
type(control_state) :: cstate

!******************************************************************************

if (lsqrtb) then
   write(6,*)'state2control: not for sqrt(B)'
   call stop2(311)
end if

! Loop over control steps
do jj=1,nsubwin

   call allocate_cs(cstate)

!  Adjoint of control to initial state
   do ii=1,latlon1n
      cstate%st(ii)=zero
      cstate%vp(ii)=zero
      cstate%t (ii)=rval(jj)%t (ii)
      cstate%rh(ii)=zero
      if (nrf3_oz>izero) cstate%oz(ii)=rval(jj)%oz(ii)
      if (nrf3_cw>izero) cstate%cw(ii)=rval(jj)%cw(ii)
   enddo

   do ii=1,latlon11
      cstate%p(ii)  =rval(jj)%p(ii)
      if (nrf2_sst>izero) cstate%sst(ii)=rval(jj)%sst(ii)
   enddo

!  Convert RHS calculations for u,v to st/vp for application of
!  background error
   if(l_hyb_ens.and.uv_hyb_ens) then
      cstate%st=rval(jj)%u
      cstate%vp=rval(jj)%v
   else
      call getuv(rval(jj)%u,rval(jj)%v,cstate%st,cstate%vp,ione)
   end if

!  Calculate sensible temperature
   call tv_to_tsen_ad(cstate%t,rval(jj)%q,rval(jj)%tsen)

!  Adjoint of convert input normalized RH to q to add contribution of moisture
!  to t, p , and normalized rh
   call normal_rh_to_q_ad(cstate%rh,cstate%t,rval(jj)%p3d,rval(jj)%q)

!  Adjoint to convert ps to 3-d pressure
   call getprs_ad(cstate%p,cstate%t,rval(jj)%p3d)

!  If this is ensemble run, then add ensemble contribution sum(a(k)*xe(k)),  where a(k) are the ensemble
!    control variables and xe(k), k=1,n_ens are the ensemble perturbations.
   if(l_hyb_ens) then
!     Adjoint apply strong constraint to sum of static background and ensemble background combinations to
!     reduce imbalances introduced by ensemble localization in addition to known imbalances from
!     static background
      call strong_bk_ad(cstate%st,cstate%vp,cstate%p,cstate%t)
      if(dual_res) then
         call ensemble_forward_model_ad_dual_res(cstate,grad%step(jj)%a_en)
      else
         call ensemble_forward_model_ad(cstate,grad%step(jj)%a_en)
      end if
      if(uv_hyb_ens) then
         u(:)=cstate%st(:)
         v(:)=cstate%vp(:)
         cstate%st(:)=zero
         cstate%vp(:)=zero
         call getuv(u,v,cstate%st,cstate%vp,ione)
      end if
   end if

!  Adjoint of transfer variables

   grad%step(jj)%values(:)=cstate%values(:)+grad%step(jj)%values(:)
   call deallocate_cs(cstate)

end do

do ii=1,nsclen
  grad%predr(ii)=bval%predr(ii)
enddo
do ii=1,npclen
  grad%predp(ii)=bval%predp(ii)
enddo

return
end subroutine state2control
