!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1     !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: model_tl: Main interface to AGCM tangent linear model 
!
! !INTERFACE:
                                                                                                                           
subroutine model_tl(xini,xobs,ldprt)

! !USES:

use kinds, only: r_kind,i_kind
use mpimod, only: mype
use gsi_4dvar, only: nsubwin,nobs_bins,winlen,winsub,hr_obsbin
use gsi_4dvar, only: iadatebgn,idmodel
use jfunc, only: nclen1
use constants, only: zero,one,izero
use state_vectors
use geos_pertmod, only: ndtpert
use m_tick, only: tick
use timermod, only: timer_ini,timer_fnl

#ifdef GEOS_PERT
use prognostics, only: dyn_prog
use prognostics, only: prognostics_initial
use prognostics, only: prognostics_final
use prognostics, only: prognostics_dotp
use geos_pertmod, only: gsi2pgcm
use geos_pertmod, only: pgcm2gsi
use m_model_tl, only: initial_tl
use m_model_tl, only: amodel_tl
use m_model_tl, only: final_tl
#endif /* GEOS_PERT */

use lag_fields, only: nlocal_orig_lag, ntotal_orig_lag
use lag_fields, only: lag_tl_vec,lag_ad_vec,lag_tl_spec_i,lag_tl_spec_r
use lag_fields, only: lag_u_full,lag_v_full
use lag_fields, only: lag_gather_stateuv
use lag_traj, only: lag_rk2iter_tl
! use lag_traj, only: lag_rk4iter_tl

implicit none

! !INPUT PARAMETERS:

type(state_vector), intent(in)    :: xini(nsubwin)   ! State variable at control times
logical,            intent(in)    :: ldprt           ! Print-out flag 

! !OUTPUT PARAMETERS:

type(state_vector), intent(inout) :: xobs(nobs_bins) ! State variable at observations times

! !DESCRIPTION: Run AGCM tangent linear model.
!
! !REMARKS:
!
! !REVISION HISTORY:
!
!  19Apr2007  tremolet - initial code
!  29May2007  todling  - add actual calls to interface and AGCM TL model
!  30Sep2007  todling  - add timer
!  30Apr2009  meunier  - add trajectory model for lagrangian data
!
!EOP
!-----------------------------------------------------------------------

! Declare local variables
character(len=*), parameter :: myname = 'model_tl'
real(r_kind),     parameter :: R3600  = 3600.0_r_kind

type(state_vector) :: xx
integer(i_kind)    :: nstep,istep,nfrctl,nfrobs,ii,jj,ierr
integer(i_kind)    :: nymdi,nhmsi,ndt,dt
integer(i_kind)    :: nymdt,nhmst
real(r_kind)       :: tstep,zz,d0

#ifdef GEOS_PERT
type(dyn_prog) :: xpert
#endif /* GEOS_PERT */

!******************************************************************************

! Initialize timer
  call timer_ini('model_tl')

! Initialize variables
d0=zero
if (idmodel) then
   tstep  = R3600
   dt     = tstep
   ndt    = 1
else
!  tstep  = REAL(ndtpert,r_kind)
   ndt    = NINT(hr_obsbin*R3600/ndtpert)
   dt     = ndt*ndtpert
   tstep  = dt
endif
nstep  = NINT(winlen*R3600/tstep)
nfrctl = NINT(winsub*R3600/tstep)
nfrobs = NINT(hr_obsbin*R3600/tstep)
nymdi  =  iadatebgn/100
nhmsi  = (iadatebgn-100*nymdi)*10000

call allocate_state(xx)
xx=zero

! Checks
zz=real(nstep,r_kind)*tstep
if (ABS(winlen*R3600   -zz)>epsilon(zz)) then
  write(6,*)'model_tl: error nstep',winlen,zz
  call stop2(147)
end if
zz=real(nfrctl,r_kind)*tstep
if (ABS(winsub*R3600   -zz)>epsilon(zz)) then
  write(6,*)'model_tl: error nfrctl',winsub,zz
  call stop2(148)
end if
zz=real(nfrobs,r_kind)*tstep
if (ABS(hr_obsbin*R3600-zz)>epsilon(zz)) then
  write(6,*)'model_tl: error nfrobs',hr_obsbin,zz
  call stop2(149)
end if
if (ndt<1)then
  write(6,*)'model_tl: error ndt',ndt
  call stop2(150)
end if

if (ldprt) write(6,'(a,3(1x,i4))')'model_tl: nstep,nfrctl,nfrobs=',nstep,nfrctl,nfrobs

! Initialize GCM TLM and its perturbation vector
#ifdef GEOS_PERT
if (.not.idmodel) then
  call initial_tl
  call prognostics_initial ( xpert )
endif
#endif /* GEOS_PERT */

! Initialize trajectory TLM and vectors
lag_tl_vec(:,:,:)=0
lag_ad_vec(:,:,:)=0

! Run TL model
do istep=0,nstep-1

! Apply control vector to x_{istep}
  if (MOD(istep,nfrctl)==0) then
    ii=istep/nfrctl+1
    if (ldprt) write(6,'(a,i8.8,1x,i6.6,2(1x,i4))')'model_tl: adding WC nymd,nhms,istep,ii=',nymdi,nhmsi,istep,ii
    if (ii<1.or.ii>nsubwin) then
      write(6,*)'model_tl: error xini',ii,nsubwin
      call stop2(151)
    end if
    call self_add(xx,xini(ii))
  endif

! Post-process x_{istep}
  if (MOD(istep,nfrobs)==0) then
    ii=istep/nfrobs+1
    if (ldprt) write(6,'(a,i8.8,1x,i6.6,2(1x,i4))')'model_tl: saving state nymd,nhms,istep,ii=',nymdi,nhmsi,istep,ii
    if (ii<1.or.ii>nobs_bins) then
      write(6,*)'model_tl: error xobs',ii,nobs_bins
      call stop2(152)
    end if
    xobs(ii) = xx
    d0=d0+dot_product(xx,xx)
  endif

! Apply TL trajectory model (same time steps as obsbin)
  if (MOD(istep,nfrobs)==0 .and. ntotal_orig_lag>izero) then
    ii=istep/nfrobs+1
    if (ldprt) write(6,'(a,i8.8,1x,i6.6,2(1x,i4))')'model_tl: trajectory model nymd,nhms,istep,ii=',nymdi,nhmsi,istep,ii
    if (ii<1.or.ii>nobs_bins) call abor1('model_tl: error xobs')
    ! Gather winds from the increment
    call lag_gather_stateuv(xx%u,xx%v,ii)
    ! Execute TL model
    do jj=1,nlocal_orig_lag
      lag_tl_vec(jj,ii+1,:)=lag_tl_vec(jj,ii,:)
      ! if (.not.idmodel) then
        call lag_rk2iter_tl(lag_tl_spec_i(jj,ii,:),lag_tl_spec_r(jj,ii,:),&
              &lag_tl_vec(jj,ii+1,1),lag_tl_vec(jj,ii+1,2),lag_tl_vec(jj,ii+1,3),&
              &lag_u_full(:,:,ii),lag_v_full(:,:,ii))
        print '(A,I3,A,F14.6,F14.6)',"TLiter: ",ii+1," location",lag_tl_vec(jj,ii+1,1),lag_tl_vec(jj,ii+1,2)
      ! endif
    end do
  endif

! Apply TL model
#ifdef GEOS_PERT
  if (.not.idmodel) then
     call gsi2pgcm ( xx, xpert, 'tlm', ierr )                                    ! T
     call amodel_tl (xpert,nymdi=nymdi,nhmsi=nhmsi,ntsteps=ndt,g5pert=.true.)    ! M
     call pgcm2gsi ( xpert, xx, 'tlm', ierr )                                    ! T(-1)
  endif
#endif /* GEOS_PERT */

  call tick (nymdi,nhmsi,dt)
enddo

! Post-process final state
if (nobs_bins>1) then
  if (ldprt) write(6,'(a,i8.8,1x,i6.6,2(1x,i4))')'model_tl: saving state nymdi,nhmsi,nobs_bins=',nymdi,nhmsi,nobs_bins
  xobs(nobs_bins) = xx
  d0=d0+dot_product(xx,xx)
endif
if(ldprt) print *, myname, ': total (gsi) dot product ', d0

! Finalize GCM TLM and its perturbation vector
#ifdef GEOS_PERT
if (.not.idmodel) then
  call prognostics_final ( xpert )
  call final_tl
endif
#endif /* GEOS_PERT */

call deallocate_state(xx)

! Finalize timer
  call timer_fnl('model_tl')

return
end subroutine model_tl
