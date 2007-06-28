module tendsmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    tendsmod           contains stuff for time tendencies
!
!   prgmmr: kleist           org: np20                date: 2005-10-28
!
! abstract:  contains routines and variables for time tendency 
!            calculations
!
! program history log:
!   2005-10-28  kleist
!   2006-02-24  kleist, additions for divergence and ageostrophic vorticity tendencies
!
! subroutines included:
!   sub create_tendvars        - allocate load Jc related variables
!   sub destroy_tendvars       - deallocate Jc related arrays
!
! Variable Definitions:
!   def what9                - basic state value vertical velocity
!   def prsth9               - basic state value for horizontal component of pressure tendency
!   def r_prsum9             - basic state value of 1/(p(k)+p(k+1))
!   def prdif9               - basic state value of p(k)-p(k+1)
!   def r_prdif9             - basic state value of 1/(p(k)-p(k+1))
!   def pr_xsum9             - basic state value of p_x(k)+p_x(k+1)
!   def pr_xdif9             - basic state value of p_x(k)-p_x(k+1)
!   def pr_ysum9             - basic state value of p_y(k)+p_y(k+1)
!   def pr_ydif9             - basic state value of p_y(k)-p_y(k+1)
!   def curvfct              - factor for curvature term in wind tendencies
!   def coriolis             - coriolis parameter
!   def t_over_pbar          - horizontal mean Tv/p for calculation of mass variable tendency
!   def dp_over_pbar         - horizontal mean dp/p for calculation of mass variable tendency
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

  real(r_kind),allocatable,dimension(:,:,:):: what9,prsth9,r_prsum9,prdif9,r_prdif9,&
     pr_xsum9,pr_xdif9,pr_ysum9,pr_ydif9
  real(r_kind),allocatable,dimension(:,:):: curvfct,coriolis
  real(r_kind),allocatable,dimension(:):: t_over_pbar,dp_over_pbar
  real(r_kind) ctph0,stph0,tlm0

contains

  subroutine create_tendvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_tendvars     allocate d/dt related arrays
!   prgmmr: kleist          org: np20                date: 2005-10-27
!
! abstract: allocate dynamic constraint term arrays
!
! program history log:
!   2005-10-27  kleist
!   2006-02-24  kleist, new arrays for mass variable tendency
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
    use constants, only: one,zero
    use gridmod, only: lat2,lon2,nsig
    use kinds, only: i_kind 
    implicit none   

    integer(i_kind) i,j,k

    allocate( what9(lat2,lon2,nsig+1),prsth9(lat2,lon2,nsig+1),&
             r_prsum9(lat2,lon2,nsig),prdif9(lat2,lon2,nsig),&
             r_prdif9(lat2,lon2,nsig),&
             pr_xsum9(lat2,lon2,nsig),pr_xdif9(lat2,lon2,nsig),&
             pr_ysum9(lat2,lon2,nsig),pr_ydif9(lat2,lon2,nsig) )
    allocate(coriolis(lat2,lon2),curvfct(lat2,lon2))
    allocate(t_over_pbar(nsig),dp_over_pbar(nsig))

    return
  end subroutine create_tendvars

  subroutine destroy_tendvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_tendvars     deallocate d/dt related arrays
!   prgmmr: kleist          org: np20                date: 2005-10-27
!
! abstract: deallocate tendency arrays
!
! program history log:
!   2005-10-27  kleist
!   2006-02-24  kleist, new variables for mass variable tendency
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
    deallocate(what9,prsth9,r_prsum9,r_prdif9,prdif9,pr_xsum9,&
       pr_xdif9,pr_ysum9,pr_ydif9)
    deallocate(curvfct,coriolis)
    deallocate(t_over_pbar,dp_over_pbar)

    return
  end subroutine destroy_tendvars

end module tendsmod
