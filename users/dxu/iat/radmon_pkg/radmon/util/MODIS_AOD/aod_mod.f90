module aod_mod

!$$$ module documentation block
!           .      .    .                                       .
! module:   aod_mod
!   prgmmr: hclin      org: ncar/mmm                 date: 2010-10-20
!
! abstract: This module contains declaration/initialization of aod namelist variables
!
! program history log:
!   2010-10-20 hclin
!
! subroutines included:
!   sub init_aod        - initialize aod namelist variables
!
! Variable Definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds, only : r_kind, i_kind
   implicit none

! set default to private
   private

   public :: l_aoderr_table
   public :: laeroana_gocart
   public :: init_aod
   public :: ppmv_conv
   public :: aod_qa_limit
   public :: luse_deepblue

   logical :: l_aoderr_table
   logical :: laeroana_gocart
   logical :: luse_deepblue
   integer(i_kind) :: aod_qa_limit  ! qa >=  aod_qa_limit will be retained
   real(r_kind)    :: ppmv_conv = 96.06_r_kind/28.964_r_kind*1.0e+3_r_kind

contains

   subroutine init_aod
      implicit none
      laeroana_gocart = .false.
      l_aoderr_table = .false.
      aod_qa_limit = 3
      luse_deepblue = .false.
      return
   end subroutine init_aod

end module aod_mod
