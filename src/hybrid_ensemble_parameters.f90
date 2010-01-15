module hybrid_ensemble_parameters
!$$$   module documentation block
!                .      .    .                                       .
! module:    hybrid_ensemble_parameters  parms for hybrid ensemble
!   prgmmr: parrish          org: np22                date: 2009-09-16
!
! abstract: contains parameters which define the details of the
!             hybrid 3dvar ensemble option.
!   the following is a brief description of the hybrid 3dvar ensemble option:
!=====================================================================================================
!initial documentation for hybrid ensemble option (2009-12-16):
!
!  Only implemented for 3dvar full B version of GSI.  Future extension to sqrt(B) and 4dvar will
!   require collaboration with GMAO.  As long as hybrid ensemble option is turned off, 4dvar option
!   should still work.  This is an initial working formulation.  It is expected that many changes
!   will be made over the next several years.
!
! This formulation is based on 
!    Wang, X.,  D. M. Barker, C. Snyder, and T. M. Hamill, 2008:  A Hybrid ETKF.3DVAR 
!	Data Assimilation Scheme for the WRF Model.  Part I: Observing System 
!	Simulation Experiment.  Mon. Wea. Rev., 136, 5116-5131.
!
!  only difference is that preconditioning is based on full B instead of sqrt(B):
!
!  To introduce ensemble information into the background, a new control variable a (a_en in code), with
!    corresponding background error A, is added to the cost function.
!
!       J(x1,a) = .5*beta1*x1_trans*B_inv*x1 + .5*beta2*a_trans*A_inv*a + Jo(x,yobs)
!
!   where beta1 and beta2 are tuning constants constrained by
!
!          0 <= (1/beta1) <= 1
!
!          (1/beta1) + (1/beta2) = 1
!
!   (1/beta1) = 1, then no influence from ensemble perturbations
!
!   (1/beta1) = 0, then no influence from static background B
!
!  The state variable x is recovered from the control variable pair (x1,a) using
!
!         x = L*(x1 + sum(n=1,n_ensemble (a(n) o x_ensemble(n)))  
!
!          where L is tlnmc or identity.
!
!                x_ensemble(n)  are n_ensemble sets of ensemble perturbations
!
!            and x o y is elementwise (Shur) product
!
!          Each a(n) is technically the same length as x_ensemble(n), but in practice
!            the actual stored size is one 3D grid.
!
!   Conversion from x1,a to x is implemented by  subroutine ensemble_forward_model, 
!                located in file hybrid_ensemble_isotropic_regional.f90
!
!
!  A = diag(S,S,...,S) is a block diagonal matrix, and each S is a correlation matrix, applied
!  identically to each a(n).  In the current version, S is implemented using recursive filters
!  in horizontal and vertical (spectral in horizontal for global application).
!
!  Full background precondioning is used:
!
!         x1 = (1/beta1)*B*y1
!
!         a  = (1/beta2)*A*b
!
!  The resulting cost function:
!
!       J = .5*x1_trans*y1 + .5*a_trans*b  + Jo(x,yobs)
!
!   How to control the hybrid ensemble option:
!
!   NAMELIST HYBRID_ENSEMBLE:
!
!      l_hyb_ens:  logical variable, if .true., then turn on hybrid ensemble option, default = .false. 
!      n_ens:      ensemble size, default = 0
!      beta1_inv:  value between 0 and 1, relative weight given to static background B, default = 1.0
!      s_ens_h:    horizontal localization correlation length (units of km), default = 2828.0
!      s_ens_v:    vertical localization correlation length (grid units), default = 30.0
!      generate_ens:  if .true., generate ensemble perturbations internally as random samples of background B.
!                       (used primarily for testing/debugging)
!                     if .false., read external ensemble perturbations (not active yet)
!      aniso_a_en: if .true., then allow anisotropic localization correlation (not active yet)
!      uv_hyb_ens: if .true., then ensemble perturbation wind stored as u,v
!                  if .false., ensemble perturbation wind stored as psi,chi.
!                   (this is useful for regional application, where there is ambiguity in how to
!                      define psi,chi from u,v)
!=====================================================================================================
!
!
! program history log:
!   2009-09-16  parrish
!
! subroutines included:

! Variable Definitions:
!   def l_hyb_ens          - logical switch to turn on hybrid ensemble 3dvar
!   def uv_hyb_ens         - if true, then ensemble perturbation wind represented by u,v
!                               otherwise, ensemble perturbation wind represented by stream, pot. functions
!   def aniso_a_en    - if true, then use anisotropic rf for localization
!   def generate_ens   - if true, then create ensemble members internally
!                              using sqrt of static background error acting on N(0,1) random vectors
!   def n_ens               - number of ensemble members
!   def beta1_inv           - 1/beta1, the weight given to static background error covariance
!                              beta2_inv = 1 - beta1_inv is weight given to ensemble derived covariance
!   def s_ens_h             - homogeneous isotropic horizontal ensemble localization scale (km)
!   def s_ens_v             - vertical localization scale (grid units for now)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_hybrid_ensemble_parameters
! set passed variables to public
  public :: generate_ens,n_ens,l_hyb_ens,s_ens_h
  public :: uv_hyb_ens,s_ens_v,beta1_inv,aniso_a_en

  logical l_hyb_ens,uv_hyb_ens
  logical aniso_a_en
  logical generate_ens
  integer(i_kind) n_ens
  real(r_kind) beta1_inv,s_ens_h,s_ens_v

contains

subroutine init_hybrid_ensemble_parameters
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_hybrid_ensemble_parameters
!   prgmmr: parrish
!
! abstract: initialize hybird 3dvar hybrid ensemble parameters
!
! program history log:
!   
!   2010-01-13  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: izero,one
  implicit none

  l_hyb_ens=.false.
  uv_hyb_ens=.false.
  aniso_a_en=.false.
  generate_ens=.true.
  n_ens=izero
  beta1_inv=one
  s_ens_h = 2828._r_kind     !  km (this was optimal value in 
                             !   Wang, X.,D. M. Barker, C. Snyder, and T. M. Hamill, 2008: A hybrid
                             !      ETKF.3DVAR data assimilation scheme for the WRF Model. Part II: 
                             !      Observing system simulation experiment. Mon.  Wea. Rev., 136, 5132-5147.)

  s_ens_v = 30._r_kind    ! grid units 

end subroutine init_hybrid_ensemble_parameters

end module hybrid_ensemble_parameters
