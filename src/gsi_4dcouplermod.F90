!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_4dCouplerMod ---
!
! !INTERFACE:

module GSI_4dCouplerMod

! !USES:

implicit none
private

!
! !PUBLIC MEMBER FUNCTIONS:
!
public GSI_4dCoupler_parallel_init
public GSI_4dCoupler_init_traj
public GSI_4dCoupler_init_model_tl
public GSI_4dCoupler_model_tl
public GSI_4dCoupler_final_model_tl
public GSI_4dCoupler_init_model_ad
public GSI_4dCoupler_model_ad
public GSI_4dCoupler_final_model_ad
public GSI_4dCoupler_grtests
public GSI_4dCoupler_getpert
public GSI_4dCoupler_putpert
public GSI_4dCoupler_final_traj

! !METHOD OVERLOADING:

interface GSI_4dCoupler_parallel_init
  subroutine parallel_init_ ()
  implicit none
  end subroutine parallel_init_
end interface

interface GSI_4dCoupler_init_traj
  subroutine init_traj_ (idmodel)
  ! stub for user initialization of perturbation model trajectory
  implicit none
  logical,intent(in):: idmodel	! choose the identity model
  end subroutine init_traj_
end interface

interface GSI_4dCoupler_init_model_tl
  subroutine init_pertmod_tl_ (ndt_tl)
  use kinds,only: i_kind
  implicit none
  integer(i_kind),intent(out):: ndt_tl    ! TL model time step
  end subroutine init_pertmod_tl_
end interface

interface GSI_4dCoupler_model_tl
  subroutine pertmod_tl_ (xx,nymdi,nhmsi,ndt)
  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  implicit none
  type(gsi_bundle),intent(inout) :: xx    ! Perturbation propagated by TLM by ndt steps
  integer(i_kind),intent(in)     :: nymdi ! Date of initial perturbation, as in YYYYMMDD
  integer(i_kind),intent(in)     :: nhmsi ! Time of initial perturbation, as in HHMMSS
  integer(i_kind),intent(in)     :: ndt   ! Number of time steps to integrate TLM for
  end subroutine pertmod_tl_
end interface

interface GSI_4dCoupler_final_model_tl
  subroutine final_pertmod_tl_ ()
  implicit none
  end subroutine final_pertmod_tl_
end interface

interface GSI_4dCoupler_init_model_ad
  subroutine init_pertmod_ad_ (ndt_ad)
  use kinds,only: i_kind
  implicit none
  integer(i_kind),intent(out):: ndt_ad    ! TL model time step
  end subroutine init_pertmod_ad_
end interface

interface GSI_4dCoupler_model_ad
  subroutine pertmod_ad_ (xx,nymdi,nhmsi,ndt)
  use kinds,only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  implicit none
  type(gsi_bundle),intent(inout) :: xx    ! Perturbation propagated by ADM by ndt steps
  integer(i_kind),intent(in)     :: nymdi ! Date of initial perturbation, as in YYYYMMDD
  integer(i_kind),intent(in)     :: nhmsi ! Time of initial perturbation, as in HHMMSS
  integer(i_kind),intent(in)     :: ndt   ! Number of time steps to integrate TLM for
  end subroutine pertmod_ad_
end interface

interface GSI_4dCoupler_final_model_ad
  subroutine final_pertmod_ad_ ()
  implicit none
  end subroutine final_pertmod_ad_
end interface

interface GSI_4dCoupler_grtests
  subroutine grtests_ (mval,sval,nsubwin,nobs_bins)
  use kinds,only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  implicit none
  integer(i_kind),intent(in) :: nsubwin,nobs_bins
  type(gsi_bundle),intent(inout) :: mval(nsubwin)
  type(gsi_bundle),intent(inout) :: sval(nobs_bins)
  ! user-specific gradient tests related to TL and AD models
  end subroutine grtests_
end interface

interface GSI_4dCoupler_getpert
  subroutine get_1pert_ (xx,what)
  ! get perturbation from user's model and convert it to relevant gsi bundle
  use constants, only: zero
  use gsi_bundlemod, only: gsi_bundle
  implicit none
  type(gsi_bundle),intent(inout) :: xx
  character(len=*),intent(in) :: what   ! indicates whether tl or ad type perturbation
  end subroutine get_1pert_
  !-------------------------!
  subroutine get_Npert_ (xx,n,what)
  ! get perturbation from user's model and convert it to relevant gsi bundle
  use kinds,only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  implicit none
  type(gsi_bundle),intent(inout) :: xx(n)
  integer(i_kind) ,intent(in) :: n
  character(len=*),intent(in) :: what   ! indicates whether tl or ad type perturbation
  end subroutine get_Npert_
end interface

interface GSI_4dCoupler_putpert
  subroutine put_1pert_ (xx,nymd,nhms,what,label)
  ! convert xx to the user's model perturbation and write it out
  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  implicit none
  type(gsi_bundle),intent(inout) :: xx     ! gsi perturbation (bundle) vector
  integer(i_kind), intent(in)    :: nymd   ! date to write out field, as in, YYYYMMDD
  integer(i_kind), intent(in)    :: nhms   ! time to write out field, as in, HHMMSS
  character(len=*),intent(in)    :: what   ! indicates whether tl or ad type perturbation
  character(len=*),intent(in)    :: label  ! label used to identify output filename
  end subroutine put_1pert_
  !-------------------------!
  subroutine put_Npert_ (xx,n,what)
  ! convert xx to the user's model perturbation and write it out
  use kinds,only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  implicit none
  type(gsi_bundle),intent(inout) :: xx(n)     ! gsi perturbation (bundle) vector
  integer(i_kind) ,intent(in) :: n
  character(len=*),intent(in) :: what      ! indicates whether tl or ad type perturbation
  end subroutine put_Npert_
end interface

interface GSI_4dCoupler_final_traj 
  subroutine final_traj_ ()
  implicit none
  end subroutine final_traj_
end interface

! !DESCRIPTION: This module intents to provide a general interface to the
!               necessary ingredients for 4dvar.
!
! !REVISION HISTORY:
!
!  27Apr2010 Todling - Initial code
!  16Jun2010 Guo     - Separated interface module from its stub implementaion
!
!EOP
!-------------------------------------------------------------------------
end module GSI_4dCouplerMod
