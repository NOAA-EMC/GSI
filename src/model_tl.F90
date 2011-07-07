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
use gsi_4dvar, only: nsubwin,nobs_bins,winlen,winsub,hr_obsbin
use gsi_4dvar, only: iadatebgn,idmodel
use constants, only: zero,r3600
use state_vectors, only: allocate_state,deallocate_state,dot_product
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: self_add,assignment(=)
use gsi_4dcouplermod, only: gsi_4dcoupler_init_model_tl
use gsi_4dcouplermod, only: gsi_4dcoupler_model_tl
use gsi_4dcouplermod, only: gsi_4dcoupler_final_model_tl
use m_tick, only: tick
use timermod, only: timer_ini,timer_fnl
use mpeu_util,only: die

#ifdef _LAG_MODEL_
use lag_fields, only: nlocal_orig_lag, ntotal_orig_lag
use lag_fields, only: lag_tl_vec,lag_ad_vec,lag_tl_spec_i,lag_tl_spec_r
use lag_fields, only: lag_u_full,lag_v_full
use lag_fields, only: lag_gather_stateuv
use lag_traj, only: lag_rk2iter_tl
! use lag_traj, only: lag_rk4iter_tl
#endif

implicit none

! !INPUT PARAMETERS:

type(gsi_bundle), target, intent(in   ) :: xini(nsubwin)   ! State variable at control times
logical         , intent(in   ) :: ldprt           ! Print-out flag 

! !OUTPUT PARAMETERS:

type(gsi_bundle), target, intent(inout) :: xobs(nobs_bins) ! State variable at observations times

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
!  13May2010  todling  - update to use gsi_bundle
!  27May2010  todling  - gsi_4dcoupler; remove all user-specific TL-related references
!  31Aug2010  Guo      - new implementation of model_tl, which separates
!			 full perturbation vector xx, to become xini for
!			 an input increment perturbation and xobs for an
!  			 output perturbation.
!  13Oct2010  Guo      - cleaned up idmodel related operations.  idmodel
!			 mode of pertmod is now controled by its actual
!			 implementation behind module gsi_4dcouplermod.
!
!EOP
!-----------------------------------------------------------------------

! Declare local variables
character(len=*), parameter :: myname = 'model_tl'

integer(i_kind)    :: nstep,istep,nfrctl,nfrobs,ii,jj,ierr,n
integer(i_kind)    :: nymdi,nhmsi,ndt,dt,ndtpert
real(r_kind)       :: tstep,zz,d0
real(r_kind),pointer,dimension(:,:,:)  :: xx_u,xx_v

type(gsi_bundle), pointer :: p_xini
type(gsi_bundle), pointer :: p_xobs
type(gsi_bundle) :: xxpert	! perturbation state, persistent between steps

!******************************************************************************

! Initialize timer
call timer_ini('model_tl')
	n = size(xini)
	if(n<1) call die(myname,'unexpected size, size(xini) =',n)

! Initialize TL model
	! Get [date,time]
nymdi  =  iadatebgn/100
nhmsi  = (iadatebgn-100*nymdi)*10000

!----	 call gsi_4dcoupler_init_model_tl()
	! Get ndtpert for pertmod_TL time step in seconds.  Then create a
	! persistent state (xxpert) and initialize it to zero.
call gsi_4dcoupler_init_model_tl(xini(1),xxpert,nymdi,nhmsi,ndtpert,rc=ierr)
	if(ierr/=0) call die(myname,'gsi_4dcoupler_init_model_tl(), rc =',ierr)

xxpert = zero	! this initialization is made explicit

! Determine corresponding GSI time step parameters.
! A GSI time step is a hr_obsbin time interval.
ndt    = NINT(hr_obsbin*r3600/ndtpert)	! count of pertmod_TL time step in 1 hr_obsbin
dt     = ndt*ndtpert			! one GSI time step in seconds
tstep  = dt				! one GSI time step in seconds

nstep  = NINT(winlen*r3600/tstep)
nfrctl = NINT(winsub*r3600/tstep)
nfrobs = NINT(hr_obsbin*r3600/tstep)

if (ldprt) write(6,'(a,3(1x,i4))')'model_tl: nstep,nfrctl,nfrobs=',nstep,nfrctl,nfrobs

d0=zero

! Checks
zz=real(nstep,r_kind)*tstep
if (ABS(winlen*r3600   -zz)>epsilon(zz)) then
   write(6,*)'model_tl: error nstep',winlen,zz
   call stop2(147)
end if
zz=real(nfrctl,r_kind)*tstep
if (ABS(winsub*r3600   -zz)>epsilon(zz)) then
   write(6,*)'model_tl: error nfrctl',winsub,zz
   call stop2(148)
end if
zz=real(nfrobs,r_kind)*tstep
if (ABS(hr_obsbin*r3600-zz)>epsilon(zz)) then
   write(6,*)'model_tl: error nfrobs',hr_obsbin,zz
   call stop2(149)
end if
if (ndt<1)then
   write(6,*)'model_tl: error ndt',ndt
   call stop2(150)
end if

#ifdef _LAG_MODEL_
! Initialize trajectory TLM and vectors
   lag_tl_vec(:,:,:)=zero
   lag_ad_vec(:,:,:)=zero
#endif

! Locate (istep=0) in xini, if any.  Then add this increment to the
! current state (xxpert).
	p_xini => istep_locate_(xini,0,nfrctl, &
		ldprt,myname//'::xini[0]',nymdi,nhmsi)

if(associated(p_xini)) call self_add(xxpert,p_xini)

! Locate (istep=0) in xobs, if any.  Then store the current state (xxpert)
! to xobs.
	p_xobs => istep_locate_(xobs,0,nfrobs, &
		ldprt,myname//'::xobs[0]',nymdi,nhmsi)

if(associated(p_xobs)) then
  p_xobs = xxpert
  d0=d0+dot_product(p_xobs,p_xobs)
endif

! Run TL model
do istep=0,nstep-1

#ifdef _LAG_MODEL_
! Apply TL trajectory model (same time steps as obsbin)
  if (ntotal_orig_lag>0) then
  	! When there is a lagmod to do, integrate from this istep to the next
	! istep.
      ii=istep+1	! off the lagmod array index by one

      ! Gather winds from the istep perturnation state
      call gsi_bundlegetpointer(xxpert,'u',xx_u,ierr)
      call gsi_bundlegetpointer(xxpert,'v',xx_v,ierr)
      call lag_gather_stateuv(xx_u,xx_v,ii)

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
#endif

  ! Locate (istep) in xini, if any.  Then apply TL model from istep
  ! (p_xini and xxpert) to istep+1 (xxpert).
  	p_xini => istep_locate_(xini,istep,nfrctl, &
		ldprt,myname//'::xini[istep]',nymdi,nhmsi)

  call gsi_4dcoupler_model_tl(p_xini,xxpert,nymdi,nhmsi,ndt,rc=ierr)
   	if(ierr/=0) call die(myname,'gsi_4dcoupler_model_tl(), rc =',ierr)

  ! Locate (istep+1) in xini, if any.  Then add this increment to the
  ! current state (xxpert).
  	p_xini => istep_locate_(xini,istep+1,nfrctl, &
  		ldprt,myname//'::xini[istep+1]',nymdi,nhmsi)

  if(associated(p_xini)) call self_add(xxpert,p_xini)

  ! Locate istep in xobs at (istep+1), if any.  Then store the current
  ! state (xxpert) to xobs.
  	p_xobs => istep_locate_(xobs,istep+1,nfrobs, &
		ldprt,myname//'::xobs[istep+1]',nymdi,nhmsi)

  if(associated(p_xobs)) then
    p_xobs = xxpert
    d0=d0+dot_product(p_xobs,p_xobs)
  endif

  ! Update the clock to (istep+1)
  call tick (nymdi,nhmsi,dt)
enddo

if(ldprt) print *, myname, ': total (gsi) dot product ', d0

! Finalize TL model, and destroy xxpert at the same time.
call gsi_4dcoupler_final_model_tl(xini(1),xxpert,nymdi,nhmsi,rc=ierr)
   	if(ierr/=0) call die(myname,'gsi_4dcoupler_final_model_tl(), rc =',ierr)

! Finalize timer
call timer_fnl('model_tl')

return
contains

  function istep_locate_(x,istep,intvl, verbose,which,nymdi,nhmsi) result(p_)
  	!-- locate istep-th element in x, which is defined only at every intvl
	!-- isteps.  i.e., istep:i=0:1, intvl:2, 2*intvl:3, 3*intvl:4, etc.

    use mpeu_util,only: tell,warn
    implicit none
    type(gsi_bundle),pointer:: p_
    type(gsi_bundle),target,dimension(:),intent(in):: x
    integer(i_kind),intent(in):: istep
    integer(i_kind),intent(in):: intvl	! istep interval of two x(:) elements

    logical         ,intent(in):: verbose	! if information is needed
    character(len=*),intent(in):: which		! for which this call is made.
    integer(i_kind) ,intent(in):: nymdi,nhmsi	! current clock time

    integer:: i,isize_,intvl_

    isize_= size(x)
    intvl_= max(1,intvl)

    p_   =>null()
    if (MOD(istep,intvl_)/=0) return

    i=istep/intvl_+1

    if (i<1.or.i>isize_) then
      if(verbose) call warn(which, &
	'nymd, nhms, istep, intvl, i, size =',(/nymdi,nhmsi,istep,intvl_,i,isize_/))
      return
    endif

    if(verbose) call tell(which, &
	'nymd, nhms, istep, intvl, i, size =',(/nymdi,nhmsi,istep,intvl_,i,isize_/))

    p_ => x(i)
  end function istep_locate_
end subroutine model_tl
