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
!   2010-03-15  zhu      - make changes to ckbcov, add assign_cs2array
!   2010-04-28  todling  - update to use gsi_bundle
!   2010-05-31  todling  - better consistency checks; add co/co2
!                        - ready to bypass analysis of (any) meteorological fields
!   2010-06-15  todling  - generalized handling of chemistry
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
use control_vectors, only: control_vector
use control_vectors, only: cvars3d,cvars2d
use bias_predictors, only: predictors
use gsi_4dvar, only: nsubwin, l4dvar, lsqrtb
use gridmod, only: lat2,lon2,nsig,nnnn1o
use jfunc, only: nsclen,npclen
use berror, only: varprd,fpsproj
use balmod, only: balance
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundlegetvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_chemtracer_mod, only: gsi_chemtracer_get
use mpeu_util, only: getindex
implicit none
  
! Declare passed variables  
type(control_vector), intent(in   ) :: xhat
type(gsi_bundle)    , intent(inout) :: sval(nsubwin)
type(predictors)    , intent(inout) :: bval

! Declare local variables  	
character(len=*),parameter:: myname ='control2model'
real(r_kind),dimension(lat2,lon2,nsig) :: workst,workvp,workrh
type(gsi_bundle) :: wbundle
integer(i_kind) :: ii,jj,i,j,k,ic,id,ngases,istatus
character(len=10),allocatable,dimension(:) :: gases

! Note: The following does not aim to get all variables in
!       the state and control vectors, but rather the ones
!       explicitly needed by this routine.
! Declare required local state variables
logical :: ls_u,ls_v,ls_p3d,ls_q,ls_tsen,ls_tv,ls_ps
integer(i_kind), parameter :: nsvars = 7
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from SV needed here
                               'u   ', 'v   ', 'p3d ', 'q   ', 'tsen',  'tv  ', 'ps  ' /)
real(r_kind),pointer,dimension(:,:)   :: sv_ps,sv_sst
real(r_kind),pointer,dimension(:,:,:) :: sv_u,sv_v,sv_p3d,sv_q,sv_tsen,sv_tv,sv_oz,sv_cw
real(r_kind),pointer,dimension(:,:,:) :: sv_rank3
real(r_kind),pointer,dimension(:,:)   :: sv_rank2

logical :: do_balance,do_getprs_tl,do_normal_rh_to_q,do_tv_to_tsen,do_getuv

!******************************************************************************

if (.not.lsqrtb) then
   write(6,*)trim(myname),': assumes lsqrtb'
   call stop2(104)
end if
if (nsubwin/=1 .and. .not.l4dvar) then
   write(6,*)trim(myname),': error 3dvar',nsubwin
   call stop2(105)
end if

! Inquire about chemistry
call gsi_chemtracer_get('dim',ngases,istatus)
if (ngases>0) then
    allocate(gases(ngases))
    call gsi_chemtracer_get('shortnames',gases,istatus)
endif

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (sval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_p3d=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0; ls_tv =isps(6)>0
ls_ps =isps(7)>0

! Define what to do depending on what's in SV and 
! what's explictly needed in this routine
do_balance       =ls_tv.and.ls_ps
do_getprs_tl     =ls_ps.and.ls_tv .and.ls_p3d
do_normal_rh_to_q=ls_tv.and.ls_p3d.and.ls_q
do_tv_to_tsen    =ls_tv.and.ls_q  .and.ls_tsen
do_getuv         =ls_u .and.ls_v

! Loop over control steps
do jj=1,nsubwin

!  create an internal structure w/ the same vars as those in the control vector
   call gsi_bundlecreate (wbundle,xhat%step(jj),'control2model work',istatus)
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble creating work bundle'
      call stop2(999)
   endif

!  Multiply by sqrt of background error (ckerror)
!  -----------------------------------------------------------------------------
!  Apply sqrt of variance, as well as vertical & horizontal parts of background
!  error

   call ckgcov(xhat%step(jj)%values(:),wbundle,nnnn1o)

!  Get pointers to required state variables
   call gsi_bundlegetpointer (sval(jj),'u'   ,sv_u,   istatus)
   call gsi_bundlegetpointer (sval(jj),'v'   ,sv_v,   istatus)
   call gsi_bundlegetpointer (sval(jj),'ps'  ,sv_ps,  istatus)
   call gsi_bundlegetpointer (sval(jj),'p3d' ,sv_p3d, istatus)
   call gsi_bundlegetpointer (sval(jj),'tv'  ,sv_tv,  istatus)
   call gsi_bundlegetpointer (sval(jj),'tsen',sv_tsen,istatus)
   call gsi_bundlegetpointer (sval(jj),'q'   ,sv_q ,  istatus)
   call gsi_bundlegetpointer (sval(jj),'oz'  ,sv_oz , istatus)
   call gsi_bundlegetpointer (sval(jj),'cw'  ,sv_cw , istatus)
   call gsi_bundlegetpointer (sval(jj),'sst' ,sv_sst, istatus)

!  Copy variables from CV to SV
   call gsi_bundlegetvar ( wbundle, 'sf' , workst, istatus )
   call gsi_bundlegetvar ( wbundle, 'vp' , workvp, istatus )
   call gsi_bundlegetvar ( wbundle, 'q'  , workrh, istatus )
   call gsi_bundlegetvar ( wbundle, 't'  , sv_tv,  istatus )
   call gsi_bundlegetvar ( wbundle, 'oz' , sv_oz,  istatus )
   call gsi_bundlegetvar ( wbundle, 'cw' , sv_cw,  istatus )
   call gsi_bundlegetvar ( wbundle, 'ps' , sv_ps,  istatus )
   call gsi_bundlegetvar ( wbundle, 'sst', sv_sst, istatus )

!  Take care of chemistry
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

!  destroy temporary bundle
   call gsi_bundledestroy(wbundle,istatus)
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble destroying work bundle'
      call stop2(999)
   endif

!  Balance equation
   if(do_balance) call balance(sv_tv,sv_ps,workst,workvp,fpsproj)

!  -----------------------------------------------------------------------------

!  Get 3d pressure
   if(do_getprs_tl) call getprs_tl(sv_ps,sv_tv,sv_p3d)

!  Convert input normalized RH to q
   if(do_normal_rh_to_q) call normal_rh_to_q(workrh,sv_tv,sv_p3d,sv_q)

!  Calculate sensible temperature
   if(do_tv_to_tsen) call tv_to_tsen(sv_tv,sv_q,sv_tsen)

!  Convert streamfunction and velocity potential to u,v
   if(do_getuv) call getuv(sv_u,sv_v,workst,workvp,0)

end do

! Bias correction terms
do ii=1,nsclen
   bval%predr(ii)=xhat%predr(ii)*sqrt(varprd(ii))
enddo

do ii=1,npclen
   bval%predp(ii)=xhat%predp(ii)*sqrt(varprd(nsclen+ii))
enddo

! Clean up
if (ngases>0) then
    deallocate(gases)
endif

return
end subroutine control2model
