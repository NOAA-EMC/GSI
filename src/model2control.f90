subroutine model2control(rval,bval,grad)
!$$$  subprogram documentation block
!
! abstract:  Converts variables from physical space to control space
!            This is also the adjoint of control2model
!
! program history log:
!   2007-04-16  tremolet - initial code
!   2007-04-27  tremolet - multiply by sqrt(B)^T (from ckerror_ad D. Parrish)
!   2008-12-04  todling  - update interface to ckgcov_ad; add tsen and p3d
!   2008-12-29  todling  - add call to strong balance contraint
!   2009-04-21  derber   - modify call to getstvp to getuv(*,1)
!   2009-11-10  todling  - remove preditors call to mpl_addreduce
!   2010-03-15  zhu      - make changes to ckgcov_ad, use cstate
!   2010-04-29  todling  - update to use gsi_bundle
!   2010-05-31  todling  - better consistency checks; add co/co2
!                        - ready to bypass analysis of (any) meteorological fields
!   2010-06-15  todling  - generalized handling of chemistry
!
!   input argument list:
!     rval - State variable
!   output argument list:
!     grad - Control variable
!
!$$$
use kinds, only: r_kind,i_kind
use constants, only: zero
use control_vectors, only: control_vector
use control_vectors, only: cvars3d,cvars2d
use bias_predictors, only: predictors
use gsi_4dvar, only: nsubwin, lsqrtb
use gridmod, only: lat2,lon2,nsig,nnnn1o
use berror, only: varprd,fpsproj
use balmod, only: tbalance
use jfunc, only: nsclen,npclen,nrclen,nval_lenz
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundleputvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_chemtracer_mod, only: gsi_chemtracer_get
use mpeu_util, only: getindex
implicit none

! Declare passed variables
type(gsi_bundle)    ,intent(inout) :: rval(nsubwin)
type(predictors)    ,intent(in   ) :: bval
type(control_vector),intent(inout) :: grad

! Declare local variables
character(len=*),parameter::myname='model2control'
character(len=10),allocatable,dimension(:) :: gases
real(r_kind),dimension(lat2,lon2,nsig) :: workst,workvp,workrh
integer(i_kind) :: ii,jj,i,j,k,ic,id,ngases,istatus
real(r_kind) :: gradz(nval_lenz)
type(gsi_bundle) :: wbundle

! Note: The following does not aim to get all variables in
!       the state and control vectors, but rather the ones
!       explicitly needed by this routine.
! Declare required local state variables
logical :: ls_u,ls_v,ls_p3d,ls_q,ls_tsen,ls_tv,ls_ps
integer(i_kind), parameter :: nsvars = 7
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &
                               'u   ', 'v   ', 'p3d ', 'q   ', 'tsen',   &
                               'tv  ', 'ps  ' /)

real(r_kind),pointer,dimension(:,:)   :: rv_ps,rv_sst
real(r_kind),pointer,dimension(:,:,:) :: rv_u,rv_v,rv_p3d,rv_q,rv_tsen,rv_tv,rv_oz,rv_cw
real(r_kind),pointer,dimension(:,:,:) :: rv_rank3
real(r_kind),pointer,dimension(:,:)   :: rv_rank2

logical :: do_getuv,do_tv_to_tsen_ad,do_normal_rh_to_q_ad,do_getprs_ad,do_tbalance

!******************************************************************************

if (.not.lsqrtb) then
   write(6,*)trim(myname),': assumes lsqrtb'
   call stop2(146)
end if

! Inquire about chemistry
call gsi_chemtracer_get('dim',ngases,istatus)
if (ngases>0) then
    allocate(gases(ngases))
    call gsi_chemtracer_get('list',gases,istatus)
endif

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (rval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_p3d=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0; ls_tv =isps(6)>0
ls_ps =isps(7)>0

! Define what to do depending on what's in SV and
! what's explictly needed in this routine
do_getuv            =ls_u .and.ls_v
do_tv_to_tsen_ad    =ls_tv.and.ls_q  .and.ls_tsen
do_normal_rh_to_q_ad=ls_tv.and.ls_p3d.and.ls_q
do_getprs_ad        =ls_ps.and.ls_tv .and.ls_p3d
do_tbalance         =ls_tv.and.ls_ps

! Loop over control steps
do jj=1,nsubwin

   workst(:,:,:)=zero
   workvp(:,:,:)=zero
   workrh(:,:,:)=zero

!  Get pointers to require state variables
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

!  Convert RHS calculations for u,v to st/vp for application of
!  background error
   if(do_getuv) call getuv(rv_u,rv_v,workst,workvp,1)

!  Calculate sensible temperature
   if(do_tv_to_tsen_ad) call tv_to_tsen_ad(rv_tv,rv_q,rv_tsen)

!  Adjoint of convert input normalized RH to q to add contribution of moisture
!  to t, p , and normalized rh
   if(do_normal_rh_to_q_ad) call normal_rh_to_q_ad(workrh,rv_tv,rv_p3d,rv_q)

!  Adjoint to convert ps to 3-d pressure
   if(do_getprs_ad) call getprs_ad(rv_ps,rv_tv,rv_p3d)

!  Multiply by sqrt of background error adjoint (ckerror_ad)
!  -----------------------------------------------------------------------------

!  Transpose of balance equation
   if(do_tbalance) call tbalance(rv_tv,rv_ps,workst,workvp,fpsproj)

!  Apply variances, as well as vertical & horizontal parts of background error
   gradz(:)=zero

!  create an internal structure w/ the same vars as those in the control vector
   call gsi_bundlecreate (wbundle,grad%step(1),'model2control work',istatus)
   if (istatus/=0) then
      write(6,*)trim(myname),': trouble creating work bundle'
      call stop2(999)
   endif

!  Adjoint of control to initial state
   call gsi_bundleputvar ( wbundle, 'sf', workst, istatus )
   call gsi_bundleputvar ( wbundle, 'vp', workvp, istatus )
   call gsi_bundleputvar ( wbundle, 't' , rv_tv,  istatus )
   call gsi_bundleputvar ( wbundle, 'q' , workrh, istatus )
   call gsi_bundleputvar ( wbundle, 'ps', rv_ps,  istatus )
   call gsi_bundleputvar ( wbundle, 'oz', rv_oz,  istatus )
   call gsi_bundleputvar ( wbundle, 'cw', rv_cw,  istatus )
   call gsi_bundleputvar ( wbundle, 'sst',rv_sst, istatus )

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

!  Apply adjoint of sqrt-B
   call ckgcov_ad(gradz,wbundle,nnnn1o)

!  Clean up
   call gsi_bundledestroy(wbundle,istatus)
   if (istatus/=0) then
      write(6,*)trim(myname),': trouble destroying work bundle'
      call stop2(999)
   endif

   do ii=1,nval_lenz
      grad%step(jj)%values(ii)=grad%step(jj)%values(ii)+gradz(ii)
   enddo

! -----------------------------------------------------------------------------

end do

! Bias predictors are duplicated
do ii=1,nsclen
   grad%predr(ii)=grad%predr(ii)+bval%predr(ii)*sqrt(varprd(ii))
enddo
do ii=1,npclen
   grad%predp(ii)=grad%predp(ii)+bval%predp(ii)*sqrt(varprd(nsclen+ii))
enddo

! Clean up
if (ngases>0) then
    deallocate(gases)
endif

return
end subroutine model2control
