module mod_strong
!$$$   module documentation block
!                .      .    .                                       .
! module:  mod_strong
! prgmmr:  parrish            org: np23               date: 2007-02-15
!
! abstract: high level module for carrying all parameters used for various
!           flavors of strong constraint.
!
! program history log:
!   2007-02-15 parrish
!
! Subroutines Included:
!   sub init_strongvars  - set default namelist variable values
!
! Variable Definitions:
!   def jcstrong         - if .true., strong contraint on
!   def jcstrong_option  - =1 for slow global strong constraint
!                          =2 for faster global strong constraint
!                          =3 for regional strong constraint
!   def nstrong          - number of iterations of strong constraint initialization
!   def mmax             - max value of zonal wave number
!   def scheme           - which scheme (B, C or D) is being used (see reference above)
!   def m                - current zonal wave number being processed
!   def gspeed           - current vertical mode phase speed being processed
!   def period_max       - max period (hours) of gravity modes to be balanced
!   def period_width     - width of smooth transition (hours, centered on period_max)
!                          from balanced to unbalanced gravity modes
!   def baldiag_full     - flag to toggle balance diagnostics for the full fields
!   def baldiag_inc      - flag to toggle balance diagnostics for the analysis increment
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds,only: r_kind,i_kind
implicit none

  integer(i_kind) nstrong
  logical jcstrong,baldiag_full,baldiag_inc
  integer(i_kind) jcstrong_option
  character(1) scheme
  real(r_kind) period_max,period_width

contains


  subroutine init_strongvars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_strongvars
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-05  safford -- add subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use constants, only: one_tenth
    implicit none

    jcstrong=.false.
    jcstrong_option=1
    nstrong=0
    period_max=1000000._r_kind
    period_width=one_tenth
    scheme='B'
    baldiag_full=.false.
    baldiag_inc =.false.

  end subroutine init_strongvars
          
end module mod_strong
