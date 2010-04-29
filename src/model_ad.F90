!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1     !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: model_ad:  
!
! !INTERFACE:

subroutine model_ad(xini,xobs,ldprt)

! !USES:

use kinds, only: r_kind,i_kind
use gsi_4dvar, only: nsubwin,nobs_bins,winlen,winsub,hr_obsbin
use gsi_4dvar, only: iadateend,idmodel
use constants, only: zero,r3600
use state_vectors
use geos_pertmod, only: ndtpert
use m_tick, only: tick
use timermod, only: timer_ini,timer_fnl

#ifdef GEOS_PERT
use prognostics, only: dyn_prog
use prognostics, only: prognostics_initial
use prognostics, only: prognostics_final
use geos_pertmod, only: gsi2pgcm
use geos_pertmod, only: pgcm2gsi
use m_model_ad, only: initial_ad
use m_model_ad, only: amodel_ad
use m_model_ad, only: final_ad
#endif /* GEOS_PERT */

use lag_fields, only: nlocal_orig_lag, ntotal_orig_lag
use lag_fields, only: lag_ad_vec,lag_tl_spec_i,lag_tl_spec_r
use lag_fields, only: lag_u_full,lag_v_full
use lag_fields, only: lag_ADscatter_stateuv
use lag_traj, only: lag_rk2iter_ad
! use lag_traj, only: lag_rk4iter_ad

implicit none

! !INPUT PARAMETERS:

type(state_vector), intent(in   ) :: xobs(nobs_bins) ! Adjoint state variable at observations times
logical           , intent(in   ) :: ldprt           ! Print-out flag

! !INPUT/OUTPUT PARAMETERS:

type(state_vector), intent(inout) :: xini(nsubwin)   ! Adjoint state variable at control times

! !DESCRIPTION: Run AGCM adjoint model.
!
! !REVISION HISTORY:
!
!  19Apr2007  tremolet - initial code
!  29May2007  todling  - add actual calls to interface and AGCM AD model
!  29Jun2007  todling  - adm verified against tlm
!  30Sep2007  todling  - add timer
!  30Apr2009  meunier  - add trajectory model for lagrangian data
!
!EOP
!-----------------------------------------------------------------------

! Declare local variables
character(len=*), parameter :: myname = 'model_ad'

type(state_vector) :: xx
integer(i_kind)    :: nstep,istep,nfrctl,nfrobs,ii,jj,ierr
integer(i_kind)    :: nymdi,nhmsi,ndt,dt
real(r_kind)       :: d0,tstep

! Temporary vector for lagrangian backward integration
real(r_kind),dimension(3):: ad_tmp_locvect

#ifdef GEOS_PERT
type(dyn_prog) :: xpert
#endif /* GEOS_PERT */

!******************************************************************************

! Initialize timer
call timer_ini('model_ad')

! Initialize variables
if (idmodel) then
   tstep  = r3600
   dt     = tstep
else
   ndt    = NINT(hr_obsbin*r3600/ndtpert)
   dt     = ndt*ndtpert
   tstep  = dt
endif
nstep  = NINT(winlen*r3600/tstep)
nfrctl = NINT(winsub*r3600/tstep)
nfrobs = NINT(hr_obsbin*r3600/tstep)
nymdi  =  iadateend/100
nhmsi  = (iadateend-100*nymdi)*10000

if (ldprt) write(6,'(a,3(1x,i4))')'model_ad: nstep,nfrctl,nfrobs=',nstep,nfrctl,nfrobs

call allocate_state(xx)
xx = zero
d0 = zero

! Initialize GCM ADM and its perturbation vector
#ifdef GEOS_PERT
if (.not.idmodel ) then
   call initial_ad
   call prognostics_initial(xpert)
endif
#endif /* GEOS_PERT */

ii=nobs_bins

! Post-process final state
if (nobs_bins>1) then
   if (ldprt) write(6,'(a,i8.8,1x,i6.6,2(1x,i4))')'model_ad: retrieving state nymdi,nhmsi,nobs_bins=',nymdi,nhmsi,nobs_bins
   call self_add(xx,xobs(nobs_bins))
   if (.not.idmodel) then
      d0=dot_product(xobs(1),xobs(1))
   endif
endif
! Run AD model
do istep=nstep-1,0,-1
   call tick(nymdi,nhmsi,-dt)

!  Apply AD model
#ifdef GEOS_PERT
   if (.not.idmodel) then
      call gsi2pgcm  (xx,xpert,'adm',ierr)                          ! T'(-1)
      call amodel_ad (xpert,nymdi,nhmsi,ntsteps=ndt,g5pert=.true.)  ! M'
      call pgcm2gsi  (xpert,xx,'adm',ierr)                          ! T'
   endif
#endif /* GEOS_PERT */

! Apply AD trajectory model (same time steps as obsbin)
   if (MOD(istep,nfrobs)==0 .and. ntotal_orig_lag>0) then
      ii=istep/nfrobs+1
      if (ldprt) write(6,'(a,i8.8,1x,i6.6,2(1x,i4))')'model_ad: trajectory model nymd,nhms,istep,ii=',nymdi,nhmsi,istep,ii
      if (ii<1.or.ii>nobs_bins) call abor1('model_ad: error xobs')
      ! Execute AD model for each balloon (loop step insensitive)
      do jj=1,nlocal_orig_lag
         ad_tmp_locvect = lag_ad_vec(jj,ii+1,:)
         ! if (.not.idmodel) then
         call lag_rk2iter_ad(lag_tl_spec_i(jj,ii,:),lag_tl_spec_r(jj,ii,:),&
           &ad_tmp_locvect(1),ad_tmp_locvect(2),ad_tmp_locvect(3),&
           &lag_u_full(:,:,ii),lag_v_full(:,:,ii))
         print '(A,I3,A,F16.6,F16.6)',"ADiter: ",ii," location",lag_ad_vec(jj,ii,1),lag_ad_vec(jj,ii,2)
         ! end if
         lag_ad_vec(jj,ii,:)=lag_ad_vec(jj,ii,:)+ad_tmp_locvect
      end do
      ! Give the sensitivity back to the GCM
      call lag_ADscatter_stateuv(xx%u,xx%v,ii)
      ! To not add the contribution 2 times
      lag_u_full(:,:,ii)=zero; lag_v_full(:,:,ii)=zero;
   endif

!  Post-process x_{istep}
   if (MOD(istep,nfrobs)==0) then
      ii=istep/nfrobs+1
      if (ldprt) write(6,'(2a,i8.8,1x,i6.6,2(1x,i4))')myname,': retrieving state nymd,nhms,istep,ii=',nymdi,nhmsi,istep,ii
      call self_add(xx,xobs(ii))
   endif

!  Apply control vector to x_{istep}
   if (MOD(istep,nfrctl)==0) then
      ii=istep/nfrctl+1
      if (ldprt) write(6,'(2a,i8.8,1x,i6.6,2(1x,i4))')myname,': adj adding WC nymd,nhms,istep,ii=',nymdi,nhmsi,istep,ii
      xini(ii)=xx
      d0=dot_product(xobs(ii),xx)
   endif

enddo

if(ldprt) print *, myname, ': total (gsi) dot product ', d0
#ifdef GEOS_PERT
if (.not.idmodel ) then
   call prognostics_final(xpert)
   call final_ad
endif
#endif /* GEOS_PERT */

call deallocate_state(xx)

! Finalize timer
call timer_fnl('model_ad')

return
end subroutine model_ad
