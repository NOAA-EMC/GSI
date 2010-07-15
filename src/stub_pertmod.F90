!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  stub_4dCouplerMod ---
!
! !DESCRIPTION: This module intents to provide stub implicit interfaces to the
!               necessary ingredients for 4dvar.
!
! !REVISION HISTORY:
!
!  27Apr2010 Todling - Initial code
!  27Apr2010 Guo     - Separated stub implementation from interface module.
!
!EOP
!-------------------------------------------------------------------------

subroutine parallel_init_
use kinds, only: i_kind
implicit none
integer(i_kind) ierror
logical already_init_mpi
  call mpi_initialized(already_init_mpi,ierror)
  if(.not.already_init_mpi) call mpi_init(ierror)
end subroutine parallel_init_
!------------------------------------------------------------------------------------
subroutine init_traj_ (idmodel)
! stub for user initialization of perturbation model trajectory
implicit none
logical,intent(in):: idmodel
end subroutine init_traj_

!------------------------------------------------------------------------------------
subroutine init_pertmod_tl_(ndt_tl)
use kinds,only: i_kind
use constants,only: R3600
implicit none
integer(i_kind),intent(out):: ndt_tl    ! TL model time step
ndt_tl=R3600 ! default to 1hr
end subroutine init_pertmod_tl_
!------------------------------------------------------------------------------------
subroutine pertmod_tl_ (xx,nymdi,nhmsi,ndt)
use kinds, only: i_kind
use gsi_bundlemod, only: gsi_bundle
implicit none
type(gsi_bundle),intent(inout) :: xx    ! Perturbation propagated by TLM by ndt steps
integer(i_kind),intent(in)     :: nymdi ! Date of initial perturbation, as in YYYYMMDD
integer(i_kind),intent(in)     :: nhmsi ! Time of initial perturbation, as in HHMMSS
integer(i_kind),intent(in)     :: ndt   ! Number of time steps to integrate TLM for

end subroutine pertmod_tl_
!------------------------------------------------------------------------------------
subroutine final_pertmod_tl_ ()
implicit none
end subroutine final_pertmod_tl_

!------------------------------------------------------------------------------------
subroutine init_pertmod_ad_ (ndt_ad)
use kinds,only: i_kind
use constants,only: R3600
implicit none
integer(i_kind),intent(out):: ndt_ad    ! TL model time step
ndt_ad=R3600 ! default to 1hr
end subroutine init_pertmod_ad_
!------------------------------------------------------------------------------------
subroutine pertmod_ad_ (xx,nymdi,nhmsi,ndt)
use kinds,only: i_kind
use gsi_bundlemod, only: gsi_bundle
implicit none
type(gsi_bundle),intent(inout) :: xx    ! Perturbation propagated by ADM by ndt steps
integer(i_kind),intent(in)     :: nymdi ! Date of initial perturbation, as in YYYYMMDD
integer(i_kind),intent(in)     :: nhmsi ! Time of initial perturbation, as in HHMMSS
integer(i_kind),intent(in)     :: ndt   ! Number of time steps to integrate TLM for
end subroutine pertmod_ad_
!------------------------------------------------------------------------------------
subroutine final_pertmod_ad_ ()
implicit none
end subroutine final_pertmod_ad_

!------------------------------------------------------------------------------------
subroutine grtests_ (mval,sval,nsubwin,nobs_bins)
use kinds,only: i_kind
use gsi_bundlemod, only: gsi_bundle
implicit none
integer(i_kind),intent(in) :: nsubwin,nobs_bins
type(gsi_bundle),intent(inout):: mval(nsubwin)
type(gsi_bundle),intent(inout):: sval(nobs_bins)
! user-specific gradient tests related to TL and AD models
end subroutine grtests_
!------------------------------------------------------------------------------------
subroutine get_1pert_ (xx,what)
! get perturbation from user's model and convert it to relevant gsi bundle
use constants, only: zero
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
implicit none
type(gsi_bundle),intent(inout) :: xx
character(len=*),intent(in) :: what   ! indicates whether tl or ad type perturbation
xx=zero
end subroutine get_1pert_
!------------------------------------------------------------------------------------
subroutine put_1pert_ (xx,nymd,nhms,what,label)
! convert xx to the user's model perturbation and write it out
use kinds, only: i_kind
use gsi_bundlemod, only: gsi_bundle
implicit none
type(gsi_bundle),intent(inout) :: xx     ! gsi perturbation (bundle) vector
character(len=*),intent(in)    :: what   ! indicates whether tl or ad type perturbation
character(len=*),intent(in)    :: label  ! label used to identify output filename
integer(i_kind), intent(in)    :: nymd   ! date to write out field, as in, YYYYMMDD
integer(i_kind), intent(in)    :: nhms   ! time to write out field, as in, HHMMSS
end subroutine put_1pert_
!------------------------------------------------------------------------------------
subroutine get_Npert_ (xx,n,what)
! get perturbation from user's model and convert it to relevant gsi bundle
use kinds,only: i_kind
use constants, only: zero
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
implicit none
integer(i_kind) ,intent(in) :: n
type(gsi_bundle),intent(inout) :: xx(n)
character(len=*),intent(in) :: what   ! indicates whether tl or ad type perturbation
integer(i_kind) ii
do ii=1,n
   xx(ii)=zero
enddo
end subroutine get_Npert_
!------------------------------------------------------------------------------------
subroutine put_Npert_ (xx,n,what)
! convert xx to the user's model perturbation and write it out
use kinds,only: i_kind
use gsi_bundlemod, only: gsi_bundle
implicit none
integer(i_kind),intent(in) :: n
type(gsi_bundle),intent(in) :: xx(n)     ! gsi perturbation (bundle) vector
character(len=*),intent(in) :: what      ! indicates whether tl or ad type perturbation
end subroutine put_Npert_
!------------------------------------------------------------------------------------
subroutine final_traj_ ()
implicit none
end subroutine final_traj_
