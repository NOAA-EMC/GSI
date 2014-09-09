!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  cplr_pertMod ---
!
! !DESCRIPTION: This module is an implicit interface implementation of
!		module gsi_4dCouplerMod.  Operations implemented here
!		support a g5pert based geos_pertmod, with a default
!		identity perturbation model for all unsupported variables.
!
! !REVISION HISTORY:
!
!  27Apr2010 Todling - Initial code
!  17Jun2010 Guo     - Separated implementations with implicit interface
!		       from explicit interfaces, and renamed to clpr_pertmod.
!  31Aug2010 Guo     - Added new arguments xini and xobs to the interfaces of
!			pertmod_tl_() and pertmod_ad_() to replace xx.  This
!			change forced pertmod to use internal space saved in
!			module geos_pertmod by the previous calls.
!		     - Added nymdi and nhmsi to init_pertmod_tl_() and
!			init_pertmod_ad_() for the initial time setting.
!  02Nov2010 Guo     - Enhanced the interfaces with a perturbation state,
!			which was initially stored as "internal_state".
!		     - This implementation is based on stub_pertmod.F90, to
!			simplify the maintenance.
!  07Mar2011 Guo     - Revised to keep this code module only a "thin"
!			implicit interface layer to actual explicit
!			module interfaces of geos_pertmod and
!			geos_pertStateIO.
!
!EOP
!-------------------------------------------------------------------------
#define MYNAME	"cplr_pertmod"
!#define DEBUG_VERBOSE
!#define DEBUG_TRACE
#include "mytrace.H"

subroutine parallel_init_()
!! Need an explanation why these MPI calls have to be made here instead of
!! doing them explicitly.  Making the calls through parallel_init_() would
!! only create an unnecessary dependency on to gsi_4dCouplerMod.
use kinds, only: i_kind
use mpeu_util, only: tell,die
implicit none
integer(i_kind):: ierror
logical:: already_init_mpi
character(len=*),parameter:: myname_=MYNAME//'::parallel_init_'
  call mpi_initialized(already_init_mpi,ierror)
  	if(ierror/=0) call die(myname_,'mpi_initialized(), ierror =',ierror)
  if(already_init_mpi) then
_ENTRY_(myname_)
  elseif(.not.already_init_mpi) then
  	call mpi_init(ierror)
  	if(ierror/=0) call die(myname_,'mpi_init(), ierror =',ierror)
_ENTRY_(myname_)
  endif
_EXIT_(myname_)
end subroutine parallel_init_

!------------------------------------------------------------------------------------
subroutine pertmod_setServices_(rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_setServices_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-11-05
!
! abstract: a place holder for pertmod component configuration under ESMF
!
! program history log:
!   2010-11-05  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use nonlinmod, only: ncep_model_nl_set
  implicit none
  integer(i_kind),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_setServices_'

  if(present(rc)) rc=0
  call ncep_model_nl_set

end subroutine pertmod_setServices_

!------------------------------------------------------------------------------------
subroutine pertmod_initialize_(idmodel,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_initialize_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: pertmod initialization
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use mpimod, only: mype
  use nonlinmod, only: ncep_model_nl_init
  use nonlinmod, only: ncep_model_nl
  use mpeu_util, only: tell,perr,die
  use obsmod, only: lobserver
  implicit none

  logical,optional,intent(in):: idmodel
  integer(i_kind),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_initialize_'
  logical:: idmodel_
  integer(i_kind):: ier

  if(present(rc)) rc=0

  idmodel_=.true.
  if(present(idmodel)) idmodel_=idmodel

  call ncep_model_nl_init(ier)
    	if(ier/=0) then
	  call perr(myname_,'pertmod_initialize(), rc =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
	  return
	endif

  if(idmodel_) return

! For now, the observer runs the non-linear trajectory model
  if(.not.lobserver) return ! _rt must be another param to control NL call 
  call ncep_model_nl

end subroutine pertmod_initialize_

!------------------------------------------------------------------------------------
subroutine pertmod_finalize_(rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_finalize_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: pertmod finalization
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use mpeu_util, only: tell,perr,die
  use nonlinmod, only: ncep_model_nl_final
  implicit none
  integer(i_kind),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_finalize_'
  integer(i_kind):: ier

  if(present(rc)) rc=0

  call ncep_model_nl_final(ier)
    	if(ier/=0) then
	  call perr(myname_,'ncep_model_nl_final(), rc =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
	  return
	endif

end subroutine pertmod_finalize_

!------------------------------------------------------------------------------------
subroutine pertmod_TLinit_(xini,xobs,iymd,ihms,ndtsec,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_TLinit_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: initialize a TLM integration process
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds     , only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundleCreate
  use gsi_bundlemod, only: assignment(=)
  use constants , only: ZERO
  use tends4pertmod, only: create_tend4pertvars
  use tends4pertmod, only: time_step
  use mpeu_util, only: tell,perr,die
  implicit none
  type(gsi_bundle),intent(in ):: xini	! a known state as a template
  type(gsi_bundle),intent(out):: xobs	! a state container to be defined as xini
  integer(i_kind ),intent(in ):: iymd	! initial date (YYYYMMDD) of the perturbation state
  integer(i_kind ),intent(in ):: ihms	! initial time (HHMMSS) of the perturbation state
  integer(i_kind ),intent(out):: ndtsec	! TL model time step in seconds
  integer(i_kind ),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_TLinit_'
  character(len=*),parameter :: xoname_='TLpertState'
  integer(i_kind):: ier

#ifdef DEBUG_VERBOSE
  call tell(myname_,'at (iymd,ihms) =',(/iymd,ihms/))
#endif

_ENTRY_(myname_)
  if(present(rc)) rc=0
  call create_tend4pertvars(xini,xobs,ier)
  	if(ier/=0) then
	  call perr(myname_,'pertmod_TLinit(), rc =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
	  return
	endif

  ndtsec=nint(time_step)
_EXIT_(myname_)

end subroutine pertmod_TLinit_

!------------------------------------------------------------------------------------
subroutine pertmod_TLrun_(p_xini,xobs,iymd,ihms,ntstep,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_TLrun_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: One TL model integration step
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use ncep_pertmod, only: ncep_4dmodel_tl
  use mpeu_util, only: tell,perr,die
  implicit none

  type(gsi_bundle),      pointer:: p_xini	! input: increment perturbation propagated by TLM
  type(gsi_bundle),intent(inout)::   xobs	! inout: TL perturbation state
  integer(i_kind ),intent(in ):: iymd	! staring date (YYYYMMDD) of the perturbation state
  integer(i_kind ),intent(in ):: ihms	! staring time (HHMMSS) of the perturbation state
  integer(i_kind ),intent(in ):: ntstep	! Number of time steps to integrate TLM for
  integer(i_kind ),optional,intent(out):: rc	! return status code

  	!! t := (nymdi,nhmsi); n:=ntstep; xi:=xini; yo:=xobs
  	!! e(t) = A(t)*xi(t)
	!! z(t+n) = M(t+n,t)*[z(t)+e(t)]
	!! yo(t+n) = G(t+n)*z(t)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_TLrun_'
  integer(i_kind):: ier

_ENTRY_(myname_)
#ifdef DEBUG_VERBOSE
  call tell(myname_,'with (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0

  call ncep_4dmodel_tl(p_xini,xobs,iymd,ihms,ntstep,ier)
  	if(ier/=0) then
	  call perr(myname_,'pertmod_TLrun(), rc =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
_EXIT_(myname_)
	  return
	endif

_EXIT_(myname_)
end subroutine pertmod_TLrun_

!------------------------------------------------------------------------------------
subroutine pertmod_TLfin_(xini,xobs,iymd,ihms,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_TLfin_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: end of TL model process
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use tends4pertmod, only: destroy_tend4pertvars
  use mpeu_util, only: tell,perr,die
  implicit none

  type(gsi_bundle),intent(in   ):: xini	! untouched perturbation increment
  type(gsi_bundle),intent(inout):: xobs	! destroyed perturbation state
  integer(i_kind ),intent(in   ):: iymd	! final date (YYYYMMDD) of the perturbation state
  integer(i_kind ),intent(in   ):: ihms	! final time (HHMMSS) of the perturbation state
  integer(i_kind ),optional,intent(out):: rc	! return status code
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter:: myname_=MYNAME//"::pertmod_TLfin_"
  integer(i_kind):: ier

_ENTRY_(myname_)
#ifdef DEBUG_VERBOSE
  call tell(myname_,'with (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0
  call destroy_tend4pertvars(xobs,ier)
  	if(ier/=0) then
	  call perr(myname_,'pertmod_TLfin(), rc =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
	  return
	endif
_EXIT_(myname_)

end subroutine pertmod_TLfin_

!------------------------------------------------------------------------------------
subroutine pertmod_ADinit_(xini,xobs,iymd,ihms,ndtsec,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_ADinit_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: initialize an ADM integration process
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds     , only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use tends4pertmod, only: create_tend4pertvars
  use tends4pertmod, only: time_step
  use mpeu_util, only: tell,perr,die
  implicit none
  type(gsi_bundle),intent(out):: xini	! a state container to be defined as xobs
  type(gsi_bundle),intent(in ):: xobs	! a known state as a template
  integer(i_kind ),intent(in ):: iymd	! initial date (YYYYMMDD) of the adjoint perturbation state
  integer(i_kind ),intent(in ):: ihms	! initial time (HHMMSS) of the adjoint perturbation state
  integer(i_kind ),intent(out):: ndtsec	! AD model time step in seconds
  integer(i_kind ),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_ADinit_'
  character(len=*),parameter :: xoname_='ADpertState'
  integer(i_kind):: ier

#ifdef DEBUG_VERBOSE
  call tell(myname_,'at (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0
  call create_tend4pertvars(xobs,xini,ier)
  	if(ier/=0) then
	  call perr(myname_,'pertmod_ADinit(), rc =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
	  return
	endif

  ndtsec=nint(time_step)

end subroutine pertmod_ADinit_

!------------------------------------------------------------------------------------
subroutine pertmod_ADrun_(xini,p_xobs,iymd,ihms,ntstep,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_ADrun_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: One TL model integration step
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use ncep_pertmod, only: ncep_4dmodel_ad
  use mpeu_util, only: tell,perr,die
  implicit none

  type(gsi_bundle),intent(inout)::   xini ! inout: adjoint increment perturbation
  type(gsi_bundle),      pointer:: p_xobs ! input: adjoint perturbation state
  integer(i_kind ),intent(in ):: iymd	! starting date (YYYYMMDD) of the adjoint perturbation state
  integer(i_kind ),intent(in ):: ihms	! starting time (HHMMSS) of the adjoint perturbation state
  integer(i_kind ),intent(in ):: ntstep	! Number of time steps to integrate TLM for
  integer(i_kind ),optional,intent(out):: rc	! return status code

  	!! t := (nymdi,nhmsi); n:=ntstep; xo:=xini; yi:=xobs
  	!! z(t+n) = G'(t+n)*yi(t+n)
	!! e(t) = M'(t+n,t)*[z(t+n)+e(t+n)]
	!! xo(t) = A'(t)*e(t)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_ADrun_'
  integer(i_kind):: ier

_ENTRY_(myname_)
#ifdef DEBUG_VERBOSE
  call tell(myname_,'with (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0

  call ncep_4dmodel_ad(xini,p_xobs,iymd,ihms,ntstep,ier)
	if(ier/=0) then
	  call perr(myname_,'pertmod_ADrun(), rc =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
_EXIT_(myname_)
	  return
	endif

_EXIT_(myname_)
end subroutine pertmod_ADrun_

!------------------------------------------------------------------------------------
subroutine pertmod_ADfin_(xini,xobs,iymd,ihms,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_ADfin_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: end of TL model process
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use tends4pertmod, only: destroy_tend4pertvars
  use mpeu_util, only: tell,perr,die
  implicit none

  type(gsi_bundle),intent(inout):: xini	! destroyed perturbation state
  type(gsi_bundle),intent(in   ):: xobs	! untouched perturbation increment
  integer(i_kind ),intent(in   ):: iymd	! final date (YYYYMMDD) of the adjoint perturbation state
  integer(i_kind ),intent(in   ):: ihms	! final time (HHMMSS) of the adjoint perturbation state
  integer(i_kind ),optional,intent(out):: rc	! return status code
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter:: myname_=MYNAME//"::pertmod_ADfin_"
  integer(i_kind):: ier

#ifdef DEBUG_VERBOSE
  call tell(myname_,'with (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0

  call destroy_tend4pertvars(xini,ier)
  	if(ier/=0) then
	  call perr(myname_,'pertmod_ADfin(), rc =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
	  return
	endif

end subroutine pertmod_ADfin_

!------------------------------------------------------------------------------------
subroutine grtests_ (mval,sval,nsubwin,nobs_bins)
use kinds,only: i_kind
use gsi_bundlemod, only: gsi_bundle
use mpeu_util, only: tell
implicit none
integer(i_kind),intent(in) :: nsubwin,nobs_bins
type(gsi_bundle),intent(inout):: mval(nsubwin)
type(gsi_bundle),intent(inout):: sval(nobs_bins)
! user-specific gradient tests related to TL and AD models
  character(len=*),parameter:: myname_=MYNAME//"::grtests_"
end subroutine grtests_

!------------------------------------------------------------------------------------
subroutine get_1pert_ (xx,what)
! get perturbation from user's model and convert it to relevant gsi bundle
use gsi_bundlemod, only: gsi_bundle
use mpeu_util, only: tell
implicit none
type(gsi_bundle),intent(inout) :: xx
character(len=*),intent(in) :: what   ! indicates whether tl or ad type perturbation
  character(len=*),parameter:: myname_=MYNAME//"::get_1pert_"
end subroutine get_1pert_
!------------------------------------------------------------------------------------
subroutine put_1pert_ (xx,nymd,nhms,what,label)
! convert xx to the user's model perturbation and write it out
use kinds, only: i_kind
use gsi_bundlemod, only: gsi_bundle
use mpeu_util, only: tell
implicit none
type(gsi_bundle),intent(inout) :: xx     ! gsi perturbation (bundle) vector
character(len=*),intent(in)    :: what   ! indicates whether tl or ad type perturbation
character(len=*),intent(in)    :: label  ! label used to identify output filename
integer(i_kind), intent(in)    :: nymd   ! date to write out field, as in, YYYYMMDD
integer(i_kind), intent(in)    :: nhms   ! time to write out field, as in, HHMMSS

  character(len=*),parameter:: myname_=MYNAME//"::put_1pert_"
end subroutine put_1pert_
!------------------------------------------------------------------------------------
subroutine get_Npert_ (xx,n,what)
! get perturbation from user's model and convert it to relevant gsi bundle
use kinds,only: i_kind
use gsi_bundlemod, only: gsi_bundle
use mpeu_util, only: tell
implicit none
integer(i_kind) ,intent(in) :: n
type(gsi_bundle),intent(inout) :: xx(n)
character(len=*),intent(in) :: what   ! indicates whether tl or ad type perturbation

  character(len=*),parameter:: myname_=MYNAME//"::get_Npert_"
end subroutine get_Npert_
!------------------------------------------------------------------------------------
subroutine put_Npert_ (xx,n,what)
! convert xx to the user's model perturbation and write it out
use kinds,only: i_kind
use gsi_bundlemod, only: gsi_bundle
use mpeu_util, only: tell
implicit none
integer(i_kind),intent(in) :: n
type(gsi_bundle),intent(in) :: xx(n)     ! gsi perturbation (bundle) vector
character(len=*),intent(in) :: what      ! indicates whether tl or ad type perturbation
  character(len=*),parameter:: myname_=MYNAME//"::put_Npert_"
end subroutine put_Npert_
!------------------------------------------------------------------------------------
!.
