module tends4pertmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    tends4pertmod       contains stuff for time tendencies
!
!   prgmmr: kleist           org: np20                date: 2005-10-28
!
! abstract:  contains routines and variables for time tendency 
!            calculations
!
! program history log:
!   2011-??-??  kleist
!
! subroutines included:
!   sub create_tendvars        - allocate load Jc related variables
!   sub destroy_tendvars       - deallocate Jc related arrays
!
! Variable Definitions:
!   def prsum9               - basic state value of p(k)+p(k+1)
!m---------------------------------------------------------------------B
!   def rdtop9               - basic state value of RdTv/(p(k)+p(k+1))
!m---------------------------------------------------------------------E
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: i_kind
  use kinds, only: r_kind
  implicit none

! set default to private
  private
! set subroutiens to public
  public :: create_tend4pertvars
  public :: destroy_tend4pertvars
! set passed variables to public
  public :: curvfct,rdtop9,prsum9
  public :: itime,itime_out,itime_max,time_step,time_step_half,ab_par
  public :: u1_t,v1_t,tv1_t,q1_t,oz1_t,cw1_t,ps1_t
  public :: u2_t,v2_t,tv2_t,q2_t,oz2_t,cw2_t,ps2_t
  public :: u3_t,v3_t,tv3_t,q3_t,oz3_t,cw3_t,ps3_t
  public :: istep4pert
  public :: itrajfreq

  logical, save :: tndvar_initialized_ = .false.

  real(r_kind),allocatable,dimension(:,:,:):: prsum9,rdtop9
  real(r_kind),allocatable,dimension(:,:):: curvfct  
           !  keep it for now for caltends_model
  integer(i_kind) itime,itime_out,itime_max,ab_par,istep4pert
  integer(i_kind) itrajfreq ! frequency of trajectory i/o
  real(r_kind) time_step,time_step_half

  real(r_kind),allocatable,dimension(:,:,:):: u1_t,v1_t,tv1_t,q1_t,oz1_t,cw1_t
  real(r_kind),allocatable,dimension(:,:):: ps1_t
  real(r_kind),allocatable,dimension(:,:,:):: u2_t,v2_t,tv2_t,q2_t,oz2_t,cw2_t
  real(r_kind),allocatable,dimension(:,:):: ps2_t
  real(r_kind),allocatable,dimension(:,:,:):: u3_t,v3_t,tv3_t,q3_t,oz3_t,cw3_t
  real(r_kind),allocatable,dimension(:,:):: ps3_t

contains

  subroutine create_tend4pertvars (xin,xout,status)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_tend4pertvars     allocate d/dt related arrays
!   prgmmr: kleist          org: np20                date: 2005-10-27
!
! abstract: allocate dynamic constraint term arrays
!
! program history log:
!   2005-10-27  kleist
!   2006-02-24  kleist, new arrays for mass variable tendency
!   2006-12-15  todling, protection against over-initizing
!   2008-04-03  safford - rm unused vars and uses
!   2011-05-23  todling - alloc arrays based on order of finite-difference scheme
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use gsi_4dvar, only: idmodel
    use gridmod, only: lat2,lon2,nsig
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlecreate
    implicit none   
    type(gsi_bundle),optional,intent(in)    :: xin
    type(gsi_bundle),optional,intent(inout) :: xout
    integer,optional,intent(out) :: status

    integer(i_kind) istatus

    if(present(status)) then
       status=0
    endif
    if(present(xout).and.present(xin)) then
       call gsi_bundlecreate( xout, xin, 'PertBundle', istatus )
       if(present(status)) then
          status=istatus
       endif
    endif

    istep4pert=0  ! general counter for pert models

    if(idmodel) return

    if(tndvar_initialized_) return

    allocate(prsum9(lat2,lon2,nsig),rdtop9(lat2,lon2,nsig),curvfct(lat2,lon2))
    allocate(u1_t(lat2,lon2,nsig),v1_t(lat2,lon2,nsig),tv1_t(lat2,lon2,nsig),&
             q1_t(lat2,lon2,nsig),oz1_t(lat2,lon2,nsig),cw1_t(lat2,lon2,nsig))
    allocate(ps1_t(lat2,lon2))
    if (ab_par>2) then
        allocate(u2_t(lat2,lon2,nsig),v2_t(lat2,lon2,nsig),tv2_t(lat2,lon2,nsig),&
                 q2_t(lat2,lon2,nsig),oz2_t(lat2,lon2,nsig),cw2_t(lat2,lon2,nsig))
        allocate(ps2_t(lat2,lon2))
    endif
    if (ab_par>3) then
        allocate(u3_t(lat2,lon2,nsig),v3_t(lat2,lon2,nsig),tv3_t(lat2,lon2,nsig),&
                 q3_t(lat2,lon2,nsig),oz3_t(lat2,lon2,nsig),cw3_t(lat2,lon2,nsig))
        allocate(ps3_t(lat2,lon2))
    endif

    tndvar_initialized_ = .true.
 
    return
  end subroutine create_tend4pertvars

  subroutine destroy_tend4pertvars(xx,status)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_tend4pertvars     deallocate d/dt related arrays
!   prgmmr: kleist          org: np20                date: 2005-10-27
!
! abstract: deallocate tendency arrays
!
! program history log:
!   2005-10-27  kleist
!   2006-02-24  kleist, new variables for mass variable tendency
!   2006-12-15  todling, protection against over-initizing
!   2011-05-23  todling, dealloc based on order of scheme (first-in, last-out)
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundledestroy
    implicit none
    type(gsi_bundle),optional,intent(inout) :: xx
    integer,optional,intent(out) :: status

    integer(i_kind) istatus

    if(present(status)) then
       status=0
    endif
 
    if(present(xx))then
       call gsi_bundledestroy( xx, istatus )
       status=istatus
    endif

    istep4pert=0  ! general counter for pert models

    if(.not.tndvar_initialized_) return

    if(ab_par>3) deallocate(u3_t,v3_t,tv3_t,q3_t,oz3_t,cw3_t,ps3_t)
    if(ab_par>2) deallocate(u2_t,v2_t,tv2_t,q2_t,oz2_t,cw2_t,ps2_t)
    deallocate(u1_t,v1_t,tv1_t,q1_t,oz1_t,cw1_t,ps1_t)
    deallocate(prsum9,rdtop9,curvfct)              
    tndvar_initialized_ = .false.

    return
  end subroutine destroy_tend4pertvars

end module tends4pertmod
