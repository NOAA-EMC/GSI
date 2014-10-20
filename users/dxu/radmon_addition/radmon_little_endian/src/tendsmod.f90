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
!   2006-02-24  kleist - additions for divergence and ageostrophic vorticity tendencies
!   2006-12-15  todling - protection against over-initizing
!   2007-05-08  kleist - add arrays for generalized coordinate
!   2009-08-20  parrish - replace curvfct with curvx, curvy.  this allows tendency computation to
!                          work for any general orthogonal coordinate.
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
!   def curvx,curvy          - factors for curvature terms in wind tendencies
!   def coriolis             - coriolis parameter
!   def t_over_pbar          - horizontal mean Tv/p for calculation of mass variable tendency
!   def dp_over_pbar         - horizontal mean dp/p for calculation of mass variable tendency
!   def factk9               - basic state factor used in vertical velocity term
!   def adiag9               - basic state a-diagonal term used for vertical velocity
!   def bdiag9               - basic state b-diagonal term used for vertical velocity
!   def cdiag9               - basic state c-diagonal term used for vertical velocity
!   def r_bdiag9             - reciprocol of basic state b-diagonal term used for vertical velocity
!   def wint9                - intermediate basic state variable use for vertical velocity
!   def wint9_f              - intermediate basic state variable use for vertical velocity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind
  implicit none

! set default to private
  private
! set subroutiens to public
  public :: create_tendvars
  public :: destroy_tendvars
! set passed variables to public
  public :: pr_ydif9,pr_ysum9,pr_xdif9,coriolis,curvy,curvx,r_prsum9,prsth9
  public :: what9,pr_xsum9,prdif9,r_prdif9,wint9,wint9_f,r_bdiag9,factk9
  public :: adiag9,bdiag9,cdiag9,tlm0,stph0,ctph0

  logical, save :: tndvar_initilized = .false.

  real(r_kind),allocatable,dimension(:,:,:):: what9,prsth9,r_prsum9,prdif9,r_prdif9,&
     pr_xsum9,pr_xdif9,pr_ysum9,pr_ydif9
  real(r_kind),allocatable,dimension(:,:):: curvx,curvy,coriolis
  real(r_kind),allocatable,dimension(:,:,:):: factk9,adiag9,bdiag9,cdiag9,wint9,wint9_f,&
       r_bdiag9
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
!   2006-12-15  todling, protection against over-initizing
!   2008-04-03  safford - rm unused vars and uses
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
    use gridmod, only: lat2,lon2,nsig
    implicit none   

    if(tndvar_initilized) return

    allocate( what9(lat2,lon2,nsig+1),prsth9(lat2,lon2,nsig+1),&
             r_prsum9(lat2,lon2,nsig),prdif9(lat2,lon2,nsig),&
             r_prdif9(lat2,lon2,nsig),&
             pr_xsum9(lat2,lon2,nsig),pr_xdif9(lat2,lon2,nsig),&
             pr_ysum9(lat2,lon2,nsig),pr_ydif9(lat2,lon2,nsig) )
    allocate(factk9(lat2,lon2,nsig),adiag9(lat2,lon2,nsig),&
             bdiag9(lat2,lon2,nsig),cdiag9(lat2,lon2,nsig),&
             r_bdiag9(lat2,lon2,nsig),&
             wint9_f(lat2,lon2,nsig+1),wint9(lat2,lon2,nsig+1))
    allocate(coriolis(lat2,lon2),curvx(lat2,lon2),curvy(lat2,lon2))

    allocate(t_over_pbar(nsig),dp_over_pbar(nsig))

    tndvar_initilized = .true.
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
!   2006-12-15  todling, protection against over-initizing
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
    implicit none

    if(.not.tndvar_initilized) return
    deallocate(what9,prsth9,r_prsum9,r_prdif9,prdif9,pr_xsum9,&
       pr_xdif9,pr_ysum9,pr_ydif9)
    deallocate(factk9,adiag9,bdiag9,r_bdiag9,cdiag9,wint9_f,wint9)
    deallocate(curvx,curvy,coriolis)
    deallocate(t_over_pbar,dp_over_pbar)

    return
  end subroutine destroy_tendvars

end module tendsmod
