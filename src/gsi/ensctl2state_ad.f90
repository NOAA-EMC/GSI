subroutine ensctl2state_ad(eval,mval,grad)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensctl2state_ad
!   prgmmr: kleist
!
! abstract:  Contribution from state space to ensemble control vector
!
! program history log:
!   2011-11-17  kleist - initial code
!   2013-10-28  todling - rename p3d to prse
!   2013-11-22  kleist - add option for q perturbations
!   2014-12-03  derber   - introduce parallel regions for optimization
!   2017-05-12  Y. Wang and X. Wang - add w as state variable for rw DA, POC: xuguang.wang@ou.edu
!   2019-07-11  Todling - there should be no need to check on the existence of w and dw
!
!   input argument list:
!     eval - Ensemble state variable variable
!     grad - Control variable
!
!   output argument list:
!     grad - Control variable
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use control_vectors, only: control_vector,cvars3d
use gsi_4dvar, only: ibin_anl
use hybrid_ensemble_parameters, only: uv_hyb_ens,dual_res,ntlevs_ens,q_hyb_ens
use hybrid_ensemble_isotropic, only: ensemble_forward_model_ad
use hybrid_ensemble_isotropic, only: ensemble_forward_model_ad_dual_res
use balmod, only: strong_bk_ad
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundlegetvar
use gsi_bundlemod, only: gsi_bundleputvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: assignment(=)
use gsi_bundlemod, only : self_add
use constants, only: zero,max_varname_length
use mpeu_util, only: getindex
use gsi_metguess_mod, only: gsi_metguess_get
use mod_strong, only: tlnmc_option
use cwhydromod, only: cw2hydro_ad
use cwhydromod, only: cw2hydro_ad_hwrf
use timermod, only: timer_ini,timer_fnl
use gridmod, only: nems_nmmb_regional
implicit none

! Declare passed variables
type(control_vector), intent(inout) :: grad
type(gsi_bundle)    , intent(inout) :: mval
type(gsi_bundle)    , intent(in   ) :: eval(ntlevs_ens)

! Declare local variables
character(len=*),parameter::myname='ensctl2state_ad'
character(len=max_varname_length),allocatable,dimension(:) :: clouds
integer(i_kind) :: jj,ic,id,istatus,nclouds

integer(i_kind), parameter :: ncvars = 8 
integer(i_kind) :: icps(ncvars)
type(gsi_bundle):: wbundle_c ! work bundle
character(len=3), parameter :: mycvars(ncvars) = (/  &  ! vars from CV needed here
                               'sf ', 'vp ', 'ps ', 't  ',    &
                               'q  ', 'cw ', 'w  ', 'dw '/)
logical :: lc_sf,lc_vp,lc_ps,lc_t,lc_rh,lc_cw
logical :: lc_w,lc_dw
real(r_kind),pointer,dimension(:,:,:) :: cv_sf=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_vp=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_rh=>NULL()
! Declare required local state variables
integer(i_kind), parameter :: nsvars = 13
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from ST needed here
                               'u   ', 'v   ', 'prse', 'q   ', 'tsen','ql  ','qi  ', &
                               'qr  ', 'qs  ', 'qg  ', 'qh  ', 'w   ','dw  ' /)                                
logical :: ls_u,ls_v,ls_prse,ls_q,ls_tsen,ls_ql,ls_qi
logical :: ls_qr,ls_qs,ls_qg,ls_qh
logical :: ls_w,ls_dw
real(r_kind),pointer,dimension(:,:)   :: rv_ps=>NULL()
real(r_kind),pointer,dimension(:,:)   :: rv_sst=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_u=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_v=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_prse=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_q=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_tsen=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_tv=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_oz=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_rank3=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_w=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_dw=>NULL()

logical :: do_getuv,do_tv_to_tsen_ad,do_normal_rh_to_q_ad,do_getprs_ad,lstrong_bk_vars
logical :: do_tlnmc,do_q_copy
logical :: do_cw_to_hydro_ad
logical :: do_cw_to_hydro_ad_hwrf
logical :: wdw_exist

!****************************************************************************

! Initialize timer
!call timer_ini(trim(myname))

! Inquire about chemistry
call gsi_metguess_get('clouds::3d',nclouds,istatus)
if (nclouds>0) then
    allocate(clouds(nclouds))
    call gsi_metguess_get('clouds::3d',clouds,istatus)
endif

! Since each internal vector of grad has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (grad%step(1),mycvars,icps,istatus)
lc_sf =icps(1)>0; lc_vp =icps(2)>0; lc_ps =icps(3)>0
lc_t  =icps(4)>0; lc_rh =icps(5)>0; lc_cw =icps(6)>0
lc_w  =icps(7)>0; lc_dw =icps(8)>0

! Since each internal vector of grad has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (eval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0; ls_ql =isps(6)>0; ls_qi =isps(7)>0                                      
ls_qr  =isps(8)>0; ls_qs  =isps(9)>0
ls_qg  =isps(10)>0; ls_qh =isps(11)>0
ls_w   =isps(12)>0; ls_dw =isps(13)>0

! Define what to do depending on what's in CV and SV
lstrong_bk_vars     =lc_sf.and.lc_vp.and.lc_ps .and.lc_t
do_getuv            =lc_sf.and.lc_vp.and.ls_u  .and.ls_v
do_tv_to_tsen_ad    =lc_t .and.ls_q .and.ls_tsen
do_normal_rh_to_q_ad=(.not.q_hyb_ens).and.&
                     lc_t .and.lc_rh.and.ls_prse.and.ls_q
do_q_copy=.false.
if(.not. do_normal_rh_to_q_ad) then
  do_q_copy = lc_rh.and.lc_t .and.ls_prse.and.ls_q.and.q_hyb_ens
end if
do_getprs_ad        =lc_t .and.lc_ps.and.ls_prse

do_cw_to_hydro_ad=.false.
do_cw_to_hydro_ad=lc_cw.and.ls_ql.and.ls_qi
do_cw_to_hydro_ad_hwrf=.false.
do_cw_to_hydro_ad_hwrf=lc_cw.and.ls_ql.and.ls_qi.and.ls_qr.and.ls_qs.and.ls_qg.and.ls_qh

wdw_exist = lc_w.and.lc_dw.and.ls_w.and.ls_dw

! Initialize
mval%values=zero
!  Create a temporary bundle similar to grad, and copy contents of grad into it
call gsi_bundlecreate ( wbundle_c, grad%step(1), 'ensctl2state_ad work', istatus )
if(istatus/=0) then
   write(6,*) trim(myname), ': trouble creating work bundle'
   call stop2(999)
endif

do jj=1,ntlevs_ens

! If calling TLNMC, already have u,v (so set last argument to true)
   do_tlnmc = lstrong_bk_vars .and. ( (tlnmc_option==3) .or. &
            (jj==ibin_anl .and. tlnmc_option==2))  

   wbundle_c%values=zero

! Get sv pointers here
!  Get pointers to required state variables
   call gsi_bundlegetpointer (eval(jj),'u'   ,rv_u,   istatus)
   call gsi_bundlegetpointer (eval(jj),'v'   ,rv_v,   istatus)
   call gsi_bundlegetpointer (eval(jj),'ps'  ,rv_ps,  istatus)
   call gsi_bundlegetpointer (eval(jj),'prse',rv_prse,istatus)
   call gsi_bundlegetpointer (eval(jj),'tv'  ,rv_tv,  istatus)
   call gsi_bundlegetpointer (eval(jj),'tsen',rv_tsen,istatus)
   call gsi_bundlegetpointer (eval(jj),'q'   ,rv_q ,  istatus)
   call gsi_bundlegetpointer (wbundle_c,'q'  ,cv_rh ,istatus)

!  Adjoint of consistency for sensible temperature, calculate sensible temperature
   if(do_tv_to_tsen_ad) call tv_to_tsen_ad(rv_tv,rv_q,rv_tsen)

   if(do_tlnmc) then

      ! Adjoint to convert ps to 3-d pressure
      if(do_getprs_ad) call getprs_ad(rv_ps,rv_tv,rv_prse)
      rv_prse=zero

      ! Adjoint of strong_bk
      call strong_bk_ad(rv_u,rv_v,rv_ps,rv_tv,.true.)

   end if

   call self_add(mval,eval(jj))

!$omp parallel sections private(ic,id,istatus)

!$omp section

!  Convert RHS calculations for u,v to st/vp
   if (do_getuv) then
      if(uv_hyb_ens) then
         call gsi_bundleputvar ( wbundle_c, 'sf', rv_u, istatus )
         call gsi_bundleputvar ( wbundle_c, 'vp', rv_v, istatus )
      else
         call gsi_bundlegetpointer (wbundle_c,'sf' ,cv_sf ,istatus)
         call gsi_bundlegetpointer (wbundle_c,'vp' ,cv_vp ,istatus)
         call getuv(rv_u,rv_v,cv_sf,cv_vp,1)
      end if
   end if

!$omp section

   call gsi_bundlegetpointer (eval(jj),'oz'  ,rv_oz , istatus)
   call gsi_bundlegetpointer (eval(jj),'sst' ,rv_sst, istatus)
   call gsi_bundleputvar ( wbundle_c, 'oz',  rv_oz,  istatus )
   call gsi_bundleputvar ( wbundle_c, 'sst', rv_sst, istatus )
   if(wdw_exist)then
     call gsi_bundlegetpointer (eval(jj),'w' ,rv_w, istatus)
     call gsi_bundleputvar ( wbundle_c, 'w', rv_w, istatus )
     if(nems_nmmb_regional)then
       call gsi_bundlegetpointer (eval(jj),'dw' ,rv_dw, istatus)
       call gsi_bundleputvar ( wbundle_c, 'dw', rv_dw, istatus )
     end if
   end if

!$omp section

   if (do_cw_to_hydro_ad .and. .not.do_cw_to_hydro_ad_hwrf) then
!     Case when cloud-vars do not map one-to-one
!     e.g. cw-to-ql&qi
      call cw2hydro_ad(eval(jj),wbundle_c,clouds,nclouds)
   elseif (do_cw_to_hydro_ad_hwrf) then
!!     Case when cloud-vars do not map one-to-one
!!     e.g. cw-to-ql&qi&qr&qs&qg&qh
      call cw2hydro_ad_hwrf(eval(jj),wbundle_c,rv_tsen)
   else
!  Since cloud-vars map one-to-one, take care of them together
      do ic=1,nclouds
         id=getindex(cvars3d,clouds(ic))
         if (id>0) then
            call gsi_bundlegetpointer (eval(jj),clouds(ic),rv_rank3,istatus)
            call gsi_bundleputvar     (wbundle_c, clouds(ic),rv_rank3,istatus)
         endif
      enddo
   endif

!  Calculate sensible temperature
   if(do_q_copy) then
      call gsi_bundleputvar (wbundle_c, 'q', rv_q, istatus )
   else

!     Adjoint of convert input normalized RH to q to add contribution of moisture
!     to t, p , and normalized rh
      if(do_normal_rh_to_q_ad) call normal_rh_to_q_ad(cv_rh,rv_tv,rv_prse,rv_q)

!     Adjoint to convert ps to 3-d pressure
      if(do_getprs_ad) call getprs_ad(rv_ps,rv_tv,rv_prse)
   end if

!  Adjoint of control to initial state
   call gsi_bundleputvar ( wbundle_c, 't' ,  rv_tv,  istatus )
   call gsi_bundleputvar ( wbundle_c, 'ps',  rv_ps,  istatus )
!  call gsi_bundleputvar ( wbundle_c, 'q' ,  zero,   istatus )  !mjk                    
!$omp end parallel sections

   if(dual_res) then
      call ensemble_forward_model_ad_dual_res(wbundle_c,grad%aens(1,:),jj)
   else
      call ensemble_forward_model_ad(wbundle_c,grad%aens(1,:),jj)
   end if

end do

call gsi_bundledestroy(wbundle_c,istatus)
if (istatus/=0) then
   write(6,*) trim(myname),': trouble destroying work bundle'
   call stop2(999)
endif

if (nclouds>0) deallocate(clouds)

! Finalize timer
!call timer_fnl(trim(myname))

return 
end subroutine ensctl2state_ad
