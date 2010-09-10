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
!                located in file hybrid_ensemble_isotropic.f90
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
!      nlon_ens            - number of longitudes to use for ensemble members and ensemble control vector
!      nlat_ens            - number of latitudes to use for ensemble members and ensemble control vector
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
!   2010-02-20  parrish - add changes to allow dual resolution capability.
!   2010-04-06  parrish - add 2nd option for units of vertical localization:
!                             if s_ens_v < 0, then abs(s_ens_v) is vertical localization length scale in
!                             units of ln(p).  otherwise, when s_ens_v > 0, localization is in vertical
!                             grid units.  The ln(p) distance measurement is approximate, based on a
!                             fixed surface pressure of 1000mb.  This is because at the point where filter
!                             constants are currently generated, the background 3d pressure field is not
!                             yet available.  A later update will correct this.
!                             For the current s_ens_v > 0, the measure is vertical grid units.
!                             s_ens_v = 20 and s_ens_v = -0.44 are roughly comparable, and
!                             connection of .44 is .44 = (sqrt(.15)/sqrt(2))*1.6, where 1.6 is the value used
!                             by Jeff Whitaker for his distance in which the Gaspari-Cohn function 1st = 0.
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
!   def nlon_ens            - number of longitudes to use for ensemble members and ensemble control vector
!   def nlat_ens            - number of latitudes to use for ensemble members and ensemble control vector
!   def jcap_ens            - for global spectral coef input, spectral truncation.
!   def jcap_ens_test       - for global spectral coef input, test spectral truncation.
!   def beta1_inv           - 1/beta1, the weight given to static background error covariance
!                              beta2_inv = 1 - beta1_inv is weight given to ensemble derived covariance
!   def s_ens_h             - homogeneous isotropic horizontal ensemble localization scale (km)
!   def s_ens_v             - vertical localization scale (grid units for now)
!   def grd_ens             - structure variable which is initialized by general_sub2grid_create_info in
!                              module general_sub2grid_mod.f90.  the information stored in grd_ens is
!                              used for subroutines general_grid2sub, general_sub2grid.  this has
!                              been created to make it easier to have two different resolution grids,
!                              one for analysis, and one for ensemble part.
!   def grd_loc             - specifically used for ensemble control variable a_en
!   def grd_anl             - same as grd_ens, but on analysis grid
!   def grd_a1              - same as grd_anl, but with communication set up for a single 3d grid
!   def grd_e1              - same as grd_ens, but with communication set up for a single 3d grid
!   def sp_ens              - spectral structure variable, for use with reading gefs spectral files.
!   def sp_loc              - spectral structure variable, for use with spectral localization filter.
!   def p_e2a               - structure variable for interpolation from ensemble grid to analysis grid
!                              when in dual resolution mode.
!   def dual_res            - if true, then ensemble grid is different from analysis grid.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind
  use general_sub2grid_mod, only: sub2grid_info
  use general_specmod, only: spec_vars
  use egrid2agrid_mod, only: egrid2agrid_parm
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_hybrid_ensemble_parameters,create_hybens_localization_parameters,&
       destroy_hybens_localization_parameters
! set passed variables to public
  public :: generate_ens,n_ens,nlon_ens,nlat_ens,jcap_ens,jcap_ens_test,l_hyb_ens,s_ens_h
  public :: uv_hyb_ens,s_ens_v,beta1_inv,aniso_a_en,s_ens_hv,s_ens_vv
  public :: grd_ens
  public :: grd_e1
  public :: grd_loc
  public :: grd_anl
  public :: grd_a1
  public :: sp_ens
  public :: sp_loc
  public :: p_e2a
  public :: dual_res

  logical l_hyb_ens,uv_hyb_ens
  logical aniso_a_en
  logical generate_ens
  logical dual_res
  integer(i_kind) n_ens,nlon_ens,nlat_ens,jcap_ens,jcap_ens_test
  real(r_kind) beta1_inv,s_ens_h,s_ens_v
  type(sub2grid_info),save :: grd_ens,grd_loc,grd_anl,grd_e1,grd_a1
  type(spec_vars),save :: sp_ens,sp_loc
  type(egrid2agrid_parm),save :: p_e2a
  real(r_kind),allocatable,dimension(:) :: s_ens_hv,s_ens_vv

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
  nlat_ens=izero
  jcap_ens=izero
  jcap_ens_test=izero
  nlon_ens=izero
  beta1_inv=one
  s_ens_h = 2828._r_kind     !  km (this was optimal value in 
                             !   Wang, X.,D. M. Barker, C. Snyder, and T. M. Hamill, 2008: A hybrid
                             !      ETKF.3DVAR data assimilation scheme for the WRF Model. Part II: 
                             !      Observing system simulation experiment. Mon.  Wea. Rev., 136, 5132-5147.)

  s_ens_v = 30._r_kind    ! grid units 

end subroutine init_hybrid_ensemble_parameters

subroutine create_hybens_localization_parameters
  implicit none
  
  allocate( s_ens_hv(grd_ens%nsig),s_ens_vv(grd_ens%nsig) )
  
end subroutine create_hybens_localization_parameters

subroutine destroy_hybens_localization_parameters
  implicit none
  
  deallocate(s_ens_vv,s_ens_hv) 

end subroutine destroy_hybens_localization_parameters

end module hybrid_ensemble_parameters
