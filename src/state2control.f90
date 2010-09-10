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
!   2010-04-29  todling  - update to use gsi_bundle; rename cstate to wbundle
!   2010-05-31  todling  - better consistency checks; add co/co2
!                        - ready to bypass analysis of (any) meteorological fields
!   2010-06-15  todling  - generalized handling of chemistry
!
!   input argument list:
!     rval - State variable
!     bval
!   output argument list:
!     grad - Control variable
!
!$$$
use kinds, only: i_kind,r_kind
use constants, only: zero
use control_vectors, only: control_vector
use control_vectors, only: cvars3d,cvars2d
use bias_predictors, only: predictors
use gsi_4dvar, only: nsubwin, lsqrtb
use gridmod, only: latlon1n,latlon11
use jfunc, only: nsclen,npclen
use hybrid_ensemble_parameters, only: l_hyb_ens,uv_hyb_ens,dual_res
use balmod, only: strong_bk_ad
use hybrid_ensemble_isotropic, only: ensemble_forward_model_ad
use hybrid_ensemble_isotropic, only: ensemble_forward_model_ad_dual_res
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundlegetvar
use gsi_bundlemod, only: gsi_bundleputvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_chemtracer_mod, only: gsi_chemtracer_get
use mpeu_util, only: getindex
implicit none

! Declare passed variables
type(gsi_bundle)    , intent(inout) :: rval(nsubwin)
type(predictors)    , intent(in   ) :: bval
type(control_vector), intent(inout) :: grad

! Declare local variables
character(len=*),parameter::myname='state2control'
character(len=10),allocatable,dimension(:) :: gases
integer(i_kind) :: ii,jj,i,j,k,im,jm,km,ic,id,ngases,istatus
real(r_kind),dimension(:,:,:),allocatable:: u,v
type(gsi_bundle) :: wbundle ! work bundle

! Note: The following does not aim to get all variables in
!       the state and control vectors, but rather the ones
!       this routines knows how to handle.
integer(i_kind), parameter :: ncvars = 5
integer(i_kind) :: icps(ncvars)
character(len=3), parameter :: mycvars(ncvars) = (/  &
                               'sf ', 'vp ', 'ps ', 't  ', 'q  '/)
logical :: lc_sf,lc_vp,lc_ps,lc_t,lc_rh,lc_oz,lc_cw,lc_sst
real(r_kind),pointer,dimension(:,:)   :: cv_ps
real(r_kind),pointer,dimension(:,:,:) :: cv_sf,cv_vp,cv_t,cv_rh

! Declare required local state variables
integer(i_kind), parameter :: nsvars = 5
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from ST needed here
                               'u   ', 'v   ', 'p3d ', 'q   ', 'tsen' /)
logical :: ls_u,ls_v,ls_p3d,ls_q,ls_tsen
real(r_kind),pointer,dimension(:,:)   :: rv_ps,rv_sst
real(r_kind),pointer,dimension(:,:,:) :: rv_u,rv_v,rv_p3d,rv_q,rv_tsen,rv_tv,rv_oz,rv_cw
real(r_kind),pointer,dimension(:,:,:) :: rv_rank3
real(r_kind),pointer,dimension(:,:)   :: rv_rank2

logical :: musthave ! for now, pointers to meteorl variables must be defined
logical :: do_getuv,do_tv_to_tsen_ad,do_normal_rh_to_q_ad,do_getprs_ad,do_strong_bk_ad

!******************************************************************************

if (lsqrtb) then
   write(6,*)trim(myname),': not for sqrt(B)'
   call stop2(311)
end if

im=grad%step(1)%grid%im
jm=grad%step(1)%grid%jm
km=grad%step(1)%grid%km

! Inquire about chemistry
call gsi_chemtracer_get('dim',ngases,istatus)
if (ngases>0) then
    allocate(gases(ngases))
    call gsi_chemtracer_get('shortnames',gases,istatus)
endif

! Since each internal vector [step(jj)] of grad has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (grad%step(1),mycvars,icps,istatus)
lc_sf =icps(1)>0;lc_vp =icps(2)>0;lc_ps=icps(3)>0;lc_t  =icps(4)>0
lc_rh =icps(5)>0

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (rval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_p3d=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0

! Define what to do depending on what's in CV and SV
do_getuv            =lc_sf.and.lc_vp.and.ls_u  .and.ls_v
do_tv_to_tsen_ad    =lc_t .and.ls_q .and.ls_tsen
do_normal_rh_to_q_ad=lc_t .and.lc_rh.and.ls_p3d.and.ls_q
do_getprs_ad        =lc_t .and.lc_ps.and.ls_p3d
do_strong_bk_ad     =lc_sf.and.lc_vp.and.lc_ps .and.lc_t

! Loop over control steps
do jj=1,nsubwin

!  Create a work bundle similar to grad control vector's bundle
   call gsi_bundlecreate ( wbundle, grad%step(jj), 'state2control work', istatus )
   if (istatus/=0) then
      write(6,*) trim(myname),': trouble creating work bundle'
      call stop2(999)
   endif

!  Get pointers to required control variables
   call gsi_bundlegetpointer (wbundle,'sf' ,cv_sf ,istatus)
   call gsi_bundlegetpointer (wbundle,'vp' ,cv_vp ,istatus)
   call gsi_bundlegetpointer (wbundle,'ps' ,cv_ps ,istatus)
   call gsi_bundlegetpointer (wbundle,'t'  ,cv_t,  istatus)
   call gsi_bundlegetpointer (wbundle,'q'  ,cv_rh ,istatus)

!  Get pointers to this subwin require state variables
   call gsi_bundlegetpointer (rval(jj),'u'   ,rv_u,   istatus)
   call gsi_bundlegetpointer (rval(jj),'v'   ,rv_v,   istatus)
   call gsi_bundlegetpointer (rval(jj),'ps'  ,rv_ps,  istatus)
   call gsi_bundlegetpointer (rval(jj),'p3d' ,rv_p3d, istatus)
   call gsi_bundlegetpointer (rval(jj),'tv'  ,rv_tv,  istatus)
   call gsi_bundlegetpointer (rval(jj),'tsen',rv_tsen,istatus)
   call gsi_bundlegetpointer (rval(jj),'q'   ,rv_q ,  istatus)
   call gsi_bundlegetpointer (rval(jj),'oz'  ,rv_oz , istatus)
   call gsi_bundlegetpointer (rval(jj),'cw'  ,rv_cw , istatus)
   call gsi_bundlegetpointer (rval(jj),'sst' ,rv_sst, istatus)

!  Adjoint of control to initial state
   call gsi_bundleputvar ( wbundle, 'sf',  zero,   istatus )
   call gsi_bundleputvar ( wbundle, 'vp',  zero,   istatus )
   call gsi_bundleputvar ( wbundle, 't' ,  rv_tv,  istatus )
   call gsi_bundleputvar ( wbundle, 'q' ,  zero,   istatus )
   call gsi_bundleputvar ( wbundle, 'ps',  rv_ps,  istatus )
   call gsi_bundleputvar ( wbundle, 'oz',  rv_oz,  istatus )
   call gsi_bundleputvar ( wbundle, 'cw',  rv_cw,  istatus )
   call gsi_bundleputvar ( wbundle, 'sst', rv_sst, istatus )

!  Take care of chemistry
   do ic=1,ngases
      id=getindex(cvars3d,gases(ic))
      if (ic>0) then
          call gsi_bundlegetpointer (rval(jj),gases(ic),rv_rank3,istatus)
          call gsi_bundleputvar     (wbundle, gases(ic),rv_rank3,istatus)
      endif
      id=getindex(cvars2d,gases(ic))
      if (ic>0) then
          call gsi_bundlegetpointer (rval(jj),gases(ic),rv_rank2,istatus)
          call gsi_bundleputvar     (wbundle, gases(ic),rv_rank2,istatus)
      endif
   enddo

!  Convert RHS calculations for u,v to st/vp for application of
!  background error
   if (do_getuv) then
      if(l_hyb_ens.and.uv_hyb_ens) then
         ii=0
         call gsi_bundleputvar ( wbundle, 'sf', rv_u, istatus )
         call gsi_bundleputvar ( wbundle, 'vp', rv_v, istatus )
      else
         call getuv(rv_u,rv_v,cv_sf,cv_vp,1)
      end if
   end if

!  Calculate sensible temperature
   if(do_tv_to_tsen_ad) call tv_to_tsen_ad(cv_t,rv_q,rv_tsen)

!  Adjoint of convert input normalized RH to q to add contribution of moisture
!  to t, p , and normalized rh
   if(do_normal_rh_to_q_ad) call normal_rh_to_q_ad(cv_rh,cv_t,rv_p3d,rv_q)

!  Adjoint to convert ps to 3-d pressure
   if(do_getprs_ad) call getprs_ad(cv_ps,cv_t,rv_p3d)

!  If this is ensemble run, then add ensemble contribution sum(a(k)*xe(k)),  where a(k) are the ensemble
!    control variables and xe(k), k=1,n_ens are the ensemble perturbations.
   if(l_hyb_ens) then
!     Adjoint apply strong constraint to sum of static background and ensemble background combinations to
!     reduce imbalances introduced by ensemble localization in addition to known imbalances from
!     static background
      if(do_strong_bk_ad) call strong_bk_ad(cv_sf,cv_vp,cv_ps,cv_t)
      if(dual_res) then
         call ensemble_forward_model_ad_dual_res(wbundle,grad%aens(jj,:))
      else
         call ensemble_forward_model_ad(wbundle,grad%aens(jj,:))
      end if
      if(do_getuv) then
         if(uv_hyb_ens) then
            allocate(u(im,jm,km))
            allocate(v(im,jm,km))
            call gsi_bundlegetvar ( wbundle, 'sf', u, istatus )
            call gsi_bundlegetvar ( wbundle, 'vp', v, istatus )
            call gsi_bundleputvar ( wbundle, 'sf', zero, istatus )
            call gsi_bundleputvar ( wbundle, 'vp', zero, istatus )
            call getuv(u,v,cv_sf,cv_vp,1)
            deallocate(v)
            deallocate(u)
         end if
      end if
   end if

!  Adjoint of transfer variables

   do ii=1,wbundle%ndim
      grad%step(jj)%values(ii)=wbundle%values(ii)+grad%step(jj)%values(ii)
   enddo
   call gsi_bundledestroy(wbundle,istatus)
   if (istatus/=0) then
      write(6,*) trim(myname),': trouble destroying work bundle'
      call stop2(999)
   endif

end do

do ii=1,nsclen
  grad%predr(ii)=bval%predr(ii)
enddo
do ii=1,npclen
  grad%predp(ii)=bval%predp(ii)
enddo

! Clean up
if (ngases>0) then
    deallocate(gases)
endif

return
end subroutine state2control
