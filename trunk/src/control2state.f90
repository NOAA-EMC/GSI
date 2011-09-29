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
!   2009-06-16  parrish  - for l_hyb_ens=.true., add calls to ensemble_forward_model and strong_bk
!   2009-08-14  lueken   - update documentation
!   2009-11-27  parrish  - for uv_hyb_ens=.true., then ensemble perturbations contain u,v instead of st,vp
!                            so introduce extra code to handle this case.
!   2010-02-21  parrish  - introduce changes to allow dual resolution, with ensemble computation on
!                            lower resolution grid compared to analysis grid.
!                            new parameter dual_res=.true. if ensemble grid is different from analysis grid.
!   2010-03-23  zhu      - use cstate for generalizing control variable
!   2010-04-29  todling  - update to use gsi_bundle; some changes toward bypassing standard atmos analysis
!   2010-05-12  todling  - rename cstate to wbundle; state_vector now a bundle
!   2010-05-31  todling  - better consistency checks; add co/co2
!                        - ready to bypass analysis of (any) meteorological fields
!   2010-06-04  parrish  - bug fix: u,v copy to wbundle after getuv for hyb ensemble
!   2010-06-15  todling  - generalized handling of chemistry
!   2011-05-15  auligne/todling - generalized cloud handling
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
use control_vectors, only: control_vector
use control_vectors, only: cvars3d,cvars2d
use bias_predictors, only: predictors
use gsi_4dvar, only: nsubwin, nobs_bins, l4dvar, lsqrtb
use gridmod, only: latlon1n,latlon11
use jfunc, only: nsclen,npclen,nrclen
use hybrid_ensemble_parameters, only: l_hyb_ens,uv_hyb_ens,dual_res
use balmod, only: strong_bk
use hybrid_ensemble_isotropic, only: ensemble_forward_model,ensemble_forward_model_dual_res
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundlegetvar
use gsi_bundlemod, only: gsi_bundleputvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: assignment(=)
use gsi_chemguess_mod, only: gsi_chemguess_get
use gsi_metguess_mod, only: gsi_metguess_get
use mpeu_util, only: getindex
use constants, only : max_varname_length
implicit none
  
! Declare passed variables  
type(control_vector), intent(in   ) :: xhat
type(gsi_bundle)    , intent(inout) :: sval(nsubwin)
type(predictors)    , intent(inout) :: bval

! Declare local variables  	
character(len=*),parameter::myname='control2state'
character(len=max_varname_length),allocatable,dimension(:) :: gases
character(len=max_varname_length),allocatable,dimension(:) :: clouds
integer(i_kind) :: i,j,k,ii,jj,im,jm,km,ic,id,ngases,nclouds,istatus
real(r_kind),dimension(:,:,:),allocatable:: u,v
type(gsi_bundle):: wbundle ! work bundle

! Note: The following does not aim to get all variables in
!       the state and control vectors, but rather the ones
!       this routines knows how to handle.
! Declare required local control variables
integer(i_kind), parameter :: ncvars = 5
integer(i_kind) :: icps(ncvars)
character(len=3), parameter :: mycvars(ncvars) = (/  &  ! vars from CV needed here
                               'sf ', 'vp ', 'ps ', 't  ',    &
                               'q  '/)
logical :: lc_sf,lc_vp,lc_ps,lc_t,lc_rh
real(r_kind),pointer,dimension(:,:)   :: cv_ps
real(r_kind),pointer,dimension(:,:,:) :: cv_sf,cv_vp,cv_t,cv_rh

! Declare required local state variables
integer(i_kind), parameter :: nsvars = 5
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from ST needed here
                               'u   ', 'v   ', 'p3d ', 'q   ', 'tsen' /)
logical :: ls_u,ls_v,ls_p3d,ls_q,ls_tsen
real(r_kind),pointer,dimension(:,:)   :: sv_ps,sv_sst
real(r_kind),pointer,dimension(:,:,:) :: sv_u,sv_v,sv_p3d,sv_q,sv_tsen,sv_tv,sv_oz
real(r_kind),pointer,dimension(:,:,:) :: sv_rank3
real(r_kind),pointer,dimension(:,:)   :: sv_rank2

logical :: do_strong_bk,do_getprs_tl,do_normal_rh_to_q,do_tv_to_tsen,do_getuv

!******************************************************************************

if (lsqrtb) then
   write(6,*)trim(myname),': not for sqrt(B)'
   call stop2(106)
end if
if (nsubwin/=1 .and. .not.l4dvar) then
   write(6,*)trim(myname),': error 3dvar',nsubwin,l4dvar
   call stop2(107)
end if

im=xhat%step(1)%grid%im
jm=xhat%step(1)%grid%jm
km=xhat%step(1)%grid%km

! Inquire about cloud-vars
call gsi_metguess_get('clouds::3d',nclouds,istatus)
if (nclouds>0) then
    allocate(clouds(nclouds))
    call gsi_metguess_get('clouds::3d',clouds,istatus)
endif

! Inquire about chemistry
call gsi_chemguess_get('dim',ngases,istatus)
if (ngases>0) then
    allocate(gases(ngases))
    call gsi_chemguess_get('gsinames',gases,istatus)
endif

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (xhat%step(1),mycvars,icps,istatus)
lc_sf =icps(1)>0; lc_vp =icps(2)>0; lc_ps =icps(3)>0
lc_t  =icps(4)>0; lc_rh =icps(5)>0

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (sval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_p3d=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0

! Define what to do depending on what's in CV and SV
do_strong_bk     =lc_ps.and.lc_sf.and.lc_vp .and.lc_t
do_getprs_tl     =lc_ps.and.lc_t .and.ls_p3d
do_normal_rh_to_q=lc_rh.and.lc_t .and.ls_p3d.and.ls_q
do_tv_to_tsen    =lc_t .and.ls_q .and.ls_tsen
do_getuv         =lc_sf.and.lc_vp.and.ls_u.and.ls_v

! Loop over control steps
do jj=1,nsubwin

!  Create a temporary bundle similar to xhat, and copy contents of xhat into it
   call gsi_bundlecreate ( wbundle, xhat%step(jj), 'control2state work', istatus )
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble creating work bundle'
      call stop2(999)
   endif
   wbundle=xhat%step(jj)

!  Get pointers to required control variables
   call gsi_bundlegetpointer (wbundle,'sf' ,cv_sf ,istatus)
   call gsi_bundlegetpointer (wbundle,'vp' ,cv_vp ,istatus)
   call gsi_bundlegetpointer (wbundle,'ps' ,cv_ps ,istatus)
   call gsi_bundlegetpointer (wbundle,'t'  ,cv_t,  istatus)
   call gsi_bundlegetpointer (wbundle,'q'  ,cv_rh ,istatus)

!  Get pointers to required state variables
   call gsi_bundlegetpointer (sval(jj),'u'   ,sv_u,   istatus)
   call gsi_bundlegetpointer (sval(jj),'v'   ,sv_v,   istatus)
   call gsi_bundlegetpointer (sval(jj),'ps'  ,sv_ps,  istatus)
   call gsi_bundlegetpointer (sval(jj),'p3d' ,sv_p3d, istatus)
   call gsi_bundlegetpointer (sval(jj),'tv'  ,sv_tv,  istatus)
   call gsi_bundlegetpointer (sval(jj),'tsen',sv_tsen,istatus)
   call gsi_bundlegetpointer (sval(jj),'q'   ,sv_q ,  istatus)
   call gsi_bundlegetpointer (sval(jj),'oz'  ,sv_oz , istatus)
   call gsi_bundlegetpointer (sval(jj),'sst' ,sv_sst, istatus)

! If this is ensemble run, then add ensemble contribution sum(a_en(k)*xe(k)),  where a_en(k) are the ensemble
!   control variables and xe(k), k=1,n_ens are the ensemble perturbations.
   if(l_hyb_ens) then
      if(uv_hyb_ens) then
!        Convert streamfunction and velocity potential to u,v
         if (do_getuv) then
            allocate(u(im,jm,km))
            allocate(v(im,jm,km))
            call getuv(u,v,cv_sf,cv_vp,0)
            call gsi_bundleputvar ( wbundle, 'sf', u, istatus )
            call gsi_bundleputvar ( wbundle, 'vp', v, istatus )
            deallocate(v)
            deallocate(u)
         endif
      end if
      if(dual_res) then
         call ensemble_forward_model_dual_res(wbundle,xhat%aens(jj,:))
      else
         call ensemble_forward_model(wbundle,xhat%aens(jj,:))
      end if
!     Apply strong constraint to sum of static background and ensemble background combinations to
!     reduce imbalances introduced by ensemble localization in addition to known imbalances from
!     static background
      if(do_strong_bk) call strong_bk(cv_sf,cv_vp,cv_ps,cv_t)
   end if

!  Get 3d pressure
   if(do_getprs_tl) call getprs_tl(cv_ps,cv_t,sv_p3d)

!  Convert input normalized RH to q
   if(do_normal_rh_to_q) call normal_rh_to_q(cv_rh,cv_t,sv_p3d,sv_q)

!  Calculate sensible temperature
   if(do_tv_to_tsen) call tv_to_tsen(cv_t,sv_q,sv_tsen)

!  Convert streamfunction and velocity potential to u,v
   if(do_getuv) then
      if(l_hyb_ens.and.uv_hyb_ens) then
         call gsi_bundlegetvar ( wbundle, 'sf', sv_u, istatus )
         call gsi_bundlegetvar ( wbundle, 'vp', sv_v, istatus )
      else
         call getuv(sv_u,sv_v,cv_sf,cv_vp,0)
      end if
   end if

!  Copy other variables
   call gsi_bundlegetvar ( wbundle, 't'  , sv_tv,  istatus )
   call gsi_bundlegetvar ( wbundle, 'oz' , sv_oz,  istatus )
   call gsi_bundlegetvar ( wbundle, 'ps' , sv_ps,  istatus )
   call gsi_bundlegetvar ( wbundle, 'sst', sv_sst, istatus )

!  Since cloud-vars map one-to-one, take care of them together
   do ic=1,nclouds
      id=getindex(cvars3d,clouds(ic))
      if (id>0) then
          call gsi_bundlegetpointer (sval(jj),clouds(ic),sv_rank3,istatus)
          call gsi_bundlegetvar     (wbundle, clouds(ic),sv_rank3,istatus)
      endif
   enddo

!  Same one-to-one map for chemistry-vars; take care of them together 
   do ic=1,ngases
      id=getindex(cvars3d,gases(ic))
      if (id>0) then
          call gsi_bundlegetpointer (sval(jj),gases(ic),sv_rank3,istatus)
          call gsi_bundlegetvar     (wbundle, gases(ic),sv_rank3,istatus)
      endif
      id=getindex(cvars2d,gases(ic))
      if (id>0) then
          call gsi_bundlegetpointer (sval(jj),gases(ic),sv_rank2,istatus)
          call gsi_bundlegetvar     (wbundle, gases(ic),sv_rank2,istatus)
      endif
   enddo

   call gsi_bundledestroy(wbundle,istatus)
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble destroying work bundle'
      call stop2(999)
   endif

end do

! Biases
do ii=1,nsclen
   bval%predr(ii)=xhat%predr(ii)
enddo

do ii=1,npclen
   bval%predp(ii)=xhat%predp(ii)
enddo

! Clean up
if (ngases>0) then
    deallocate(gases)
endif

return
end subroutine control2state
