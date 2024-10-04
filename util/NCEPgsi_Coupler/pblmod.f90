module pblmod
!$$$   module documentation block
!                .      .    .
! module:   pblmod
!   prgrmmr:   rancic
!
! abstract:   create memory and store variables for pbl and 
!             surface drag processes used in nonlinear model 
!
! program history log:
!   2010-02-25  rancic
!
! subroutines included:
!   create_pblvars    -  create pbl variables
!   destroy_pblvars   -  destroy pbl variables
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use constants, only: one
  implicit none

  logical use_pbl
  integer :: nsig_pbl !_RT 64 need be placed correctly
  real(r_kind),allocatable,dimension(:,:,:):: dudz
  real(r_kind),allocatable,dimension(:,:,:):: dvdz
  real(r_kind),allocatable,dimension(:,:,:):: dodz
  real(r_kind),allocatable,dimension(:,:,:):: rdzi
  real(r_kind),allocatable,dimension(:,:,:):: rdzl
  real(r_kind),allocatable,dimension(:,:,:):: zi
  real(r_kind),allocatable,dimension(:,:,:):: uges0
  real(r_kind),allocatable,dimension(:,:,:):: vges0
  real(r_kind),allocatable,dimension(:,:,:):: oges0
  real(r_kind),allocatable,dimension(:,:,:):: tges0
  real(r_kind),allocatable,dimension(:,:,:):: pges0
  real(r_kind),allocatable,dimension(:,:,:):: uges1
  real(r_kind),allocatable,dimension(:,:,:):: vges1
  real(r_kind),allocatable,dimension(:,:,:):: oges1
  real(r_kind),allocatable,dimension(:,:):: cdu
  real(r_kind),allocatable,dimension(:,:):: cdv
  real(r_kind),allocatable,dimension(:,:):: adragu
  real(r_kind),allocatable,dimension(:,:):: bdragu
  real(r_kind),allocatable,dimension(:,:):: cdragu
  real(r_kind),allocatable,dimension(:,:):: ddragu
  real(r_kind),allocatable,dimension(:,:):: adragv
  real(r_kind),allocatable,dimension(:,:):: bdragv
  real(r_kind),allocatable,dimension(:,:):: cdragv
  real(r_kind),allocatable,dimension(:,:):: ddragv

  real(r_kind) eps_m,epxilon

! Constants used in Laroche PBL parameterization  
  real(r_kind) lmbd,karm,karm0,alph,beta

!!  parameter( eps_m   = 0.00001_r_kind ) 
  parameter( eps_m   = 0.0000001_r_kind ) 
  parameter( epxilon   = 1.0_r_kind )   ! Implicit scheme works only with this
!!  parameter( epxilon   = 0.5_r_kind ) ! Does not work with implicit scheme
!!  parameter( epxilon   = 0.0_r_kind ) ! Does not work with implicit scheme
  

! Definition of constants used in Laroche PBL parameterization  

!! parameter( lmbd=100._r_kind )
!! parameter( lmbd=10._r_kind )   ! good
 parameter( lmbd=15._r_kind )   ! passing
 parameter( karm=.4_r_kind )
 parameter( karm0=karm/lmbd )
 parameter( alph=.5_r_kind )
 parameter( beta=one-alph)

 public :: nsig_pbl
contains

  subroutine create_pblvars   
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    create_pblvars
!
!   prgrmmr:   rancic
!
! abstract:
!
! program history log:
!   2008-04-01  safford -- add subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
    implicit none
    
    nsig_pbl=nsig

    allocate(dudz (lat2,lon2,nsig_pbl) )
    allocate(dvdz (lat2,lon2,nsig_pbl) )
    allocate(dodz (lat2,lon2,nsig_pbl) )
    allocate(rdzi (lat2,lon2,nsig_pbl) )
    allocate(rdzl (lat2,lon2,nsig_pbl) )
    allocate(zi   (lat2,lon2,nsig_pbl+1) )
    allocate(cdu(lat2,lon2) )
    allocate(cdv(lat2,lon2) )
    allocate(adragu(lat2,lon2) )
    allocate(bdragu(lat2,lon2) )
    allocate(cdragu(lat2,lon2) )
    allocate(ddragu(lat2,lon2) )
    allocate(adragv(lat2,lon2) )
    allocate(bdragv(lat2,lon2) )
    allocate(cdragv(lat2,lon2) )
    allocate(ddragv(lat2,lon2) )

    allocate(uges0(lat2,lon2,nsig_pbl) )
    allocate(vges0(lat2,lon2,nsig_pbl) )
    allocate(oges0(lat2,lon2,nsig_pbl) )
    allocate(tges0(lat2,lon2,nsig_pbl) )
    allocate(pges0(lat2,lon2,nsig_pbl+1) )
    allocate(uges1(lat2,lon2,nsig_pbl) )
    allocate(vges1(lat2,lon2,nsig_pbl) )
    allocate(oges1(lat2,lon2,nsig_pbl) )

    return
  end subroutine create_pblvars

  subroutine destroy_pblvars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    destroy_pblvars   
!
!   prgrmmr:   rancic
!
! abstract:
!
! program history log:
!   2008-04-01  safford -- add subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
    implicit none

    deallocate(dudz  )
    deallocate(dvdz  )
    deallocate(dodz  )
    deallocate(rdzi  )
    deallocate(rdzl  )
    deallocate(zi    )
    deallocate(cdu   )
    deallocate(cdv   )
    deallocate(adragu)
    deallocate(bdragu)
    deallocate(cdragu)
    deallocate(ddragu)
    deallocate(adragv)
    deallocate(bdragv)
    deallocate(cdragv)
    deallocate(ddragv)
    deallocate(uges0 )
    deallocate(vges0 )
    deallocate(oges0 )
    deallocate(tges0 )
    deallocate(pges0 )
    deallocate(uges1 )
    deallocate(vges1 )
    deallocate(oges1 )

    return
  end subroutine destroy_pblvars

end module pblmod 
