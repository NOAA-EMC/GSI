module obsmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   obsmod
!   prgmmr: derber      org: np23                date: 2003-09-25
!
! abstract: This module contains variables and arrays pertinent for
!           observational data.
!
! program history log:
!   2003-09-25 derber
!   2004-05-13  kleist, documentation
!   2004-06-15  treadon  - update documentation
!   2004-07-23  derber   - add conventional sst observations
!   2004-11-22  derber   - remove weight, add logical for boundary point
!   2004-11-30  cucurull - add GPS RO data
!   2004-12-22  treadon  - rename logical "idiag_conv" as "diag_conv", 
!                          add write_diag logical, remove conv_diagsave 
!                          from module
!   2005-01-28  cucurull - clean up code handling gps local refractivity
!   2005-03-01  parrish  - nonlinear qc change to account for inflated obs error
!   2005-03-04  derber   - modify for cleaned up surface emissivity sensitivity
!   2005-03-16  derber   - add arrays to hold observation time
!   2005-05-27  derber   - add logical oberrflg
!   2005-06-14  wu       - add OMI oz (ozo)
!   2005-06-21  cucurull - add logical ref_obs (GPS), parameter grids_dim (GPS)
!                        - add GPS bending angle observations
!   2005-08-03  derber   - move var qc parameters b_ and pg_ for conventional 
!                          from qcmod to obsmod
!   2005-09-28  derber   - consolidate weights and locations for observations
!   2005-11-22  wu       - add variables and flag for perturbed conventional
!                          obs and a function routine to generate a random
!                          number with mean:0 variance:1
!   2005-11-29  derber   - move ozmz to guess_grids
!   2005-12-20  parrish  - add variables to enable boundary layer forward model
!                          option for surface temperature observations
!   2005-12-21  treadon  - add arrays and code for gps data
!   2006-02-03  derber   - add new obs control and simplify makecobs call
!   2006-02-17  treadon  - add stat check on all allocate and deallocate
!   2006-02-27  todling  - introduce ndatmax
!   2006-03-21  treadon  - modify optional perturbation to observations
!   2006-04-19  treadon  - add logical switch dtbduv_on
!   2006-05-05  treadon  - add maximum time window variable
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-20  cucurull - remove termt1in for GPS data, add gpstail%b_tkges 
!                          for GPS data, remove b_psges
!   2006-10-25  sienkiewicz - add blacklst flag
!   2007-01-10  sienkiewicz - define level ozone obs types
!   2007-02-02  tremolet - trying things
!   2007-02-13  parrish  - add logical switch offtime_data to allow use of obs files
!                          with ref time different from analysis time
!   2007-03-09        su - add observation perturbation paramters in the
!                          observation position structure
!   2007-03-19  tremolet - binning of observations
!   2007-04-17  todling  - getting nhr_assimilation from gsi_4dvar
!   2007-05-03  todling  - add reference to o3l
!   2007-05-30  h.liu    - change wij to 2d array for oz_ob_type, ozo_ob_type
!   2007-05-31  tremolet - add observation diagnostics structure
!   2007-06-22  cucurull - modify gps_all_ob_type structure
!   2007-06-26  tremolet - observation sensitivity
!   2007-07-11  tremolet - increment sensitivity to obs
!   2007-07-26  cucurull - modify pressure structure in gps_ob_type  
!   2007-10-24  todling  - add nchnperobs to obsdiag
!   2007-11-12  todling  - add interface to destroyobs
!   2008-03-24        wu - add variables and logical switch for adaptive oberror tuning
!   2008-11-19  todling  - define offtime_data default differently when doing 4dvar
!   2009-01-08  todling  - remove reference to ozohead/tail-structure
!   2009-01-09  gayno    - add variable dsfcalc
!   2009-02-02  kleist   - add variables for synthetic tc-mslp observations
!   2009-03-05  meunier  - add lagrangean observation type
!   2009-07-09  park,purser,pondeca - add logical variable hilbert_curve for
!                                     cross-validation in 2dvar
! 
! Subroutines Included:
!   sub init_obsmod_dflts   - initialize obs related variables to default values
!   sub init_directories    - create sub-directories for tasks specific i/o
!   sub create_obsmod_vars  - allocate obs related variables
!   sub init_obsmod_vars    - initialize obs related variables to actual values
!   sub destroyobs          - deallocate obs linked lists
!   sub destroy_obsmod_vars - deallocate obsmod arrays
!   sub destroy_genstats_gps- deallocate linked list for gsp statistics
!   sub inquire_obsdiags
!
! Functions Included:
!   fun ran01dom
!
! Variable Definitions:
!   def oberror_tune - namelist logical to tune (=true) oberror
!   def perturb_obs  - namelist logical to perturb (=true) observations
!   def perturb_fact - namelist scaling factor for observation perturbations
!   def write_diag   - namelist logical array to compute/write (=true) diag files
!   def obs_setup    - prefix for files passing pe relative obs data to setup routines
!   def dsfcalc      - specifies method to determine surface fields within a FOV
!   def dfile        - input observation file names
!   def dsis         - sensor/instrument/satellite flag from info files
!   def dtype        - observation types
!   def ditype       - observation group type (set in read_obs, e.g. rad,conv,etc)
!   def time_window  - half time window for obs type (hours)
!   def time_window_max - maximum half time window (hours)
!   def obsfile_all  - file containing observations after initial read
!   def ndat_types   - number of available data types
!   def ndat_times   - number of available synoptic times
!   def ndatmax      - maximum number of data types
!   def ndat         - total number of data types
!   def ipoint       - pointer to input namelist for particular processor
!   def iadate       - analysis date and time array
!   def ianldate     - analysis date in YYYYMMDDHH variable
!   def time_offset  - analysis relative time offset
!   def dplat        - satellite (platform) id
!   def dthin        - satellite group
!   def nsat1        - number of observations of satellites in each pe
!   def mype_diaghdr - pe id for writing out satellite diagnostic file
!   def dval         - relative value of each profile within group
!   def dmesh        - mesh size (km) for radiance thinning grid (used in satthin)
!   def pshead       - surface pressure linked list head
!   def pstail       - surface pressure linked list tail
!   def thead        - temperature linked list head
!   def ttail        - temperature linked list tail
!   def dwhead       - doppler wind linked list head
!   def dwtail       - doppler wind linked list tail
!   def rwhead       - radial wind linked list head
!   def rwtail       - radial wind linked list tail
!   def srwhead      - superobed radial wind linked list head
!   def srwtail      - superobed radial wind linked list tail
!   def whead        - conventional wind linked list head
!   def wtail        - conventional wind linked list tail
!   def qhead        - moisture linked list head
!   def qtail        - moisture linked list tail
!   def ssthead      - sea surface temperature linked list head
!   def ssttail      - sea surface temperature linked list tail
!   def pwhead       - precipitable water linked list head
!   def pwtail       - precipitable water linked list tail
!   def ozhead       - sbuv ozone profile linked list head
!   def oztail       - sbuv ozone profile linked list tail
!   def o3lhead      - ozone level data linked list head
!   def o3ltail      - ozone level data linked list tail
!   def radhead      - radiance linked list head
!   def radtail      - radiance linked list tail
!   def pcphead      - precipitation linked list head
!   def pcptail      - precipitation linked list tail
!   def laghead      - lagrangian data linked list head
!   def lagtail      - lagrangian data linked list tail
!   def lunobs_obs   - unit to save satellite observation
!   def iout_rad     - output unit for satellite stats
!   def iout_pcp     - output unit for precipitation stats
!   def iout_t       - output unit for temperature stats
!   def iout_q       - output unit for moisture stats
!   def iout_uv      - output unit for wind stats
!   def iout_oz      - output unit for ozone stats
!   def iout_ps      - output unit for surface pressure stats
!   def iout_pw      - output unit for precipitable water stats
!   def iout_rw      - output unit for radar wind stats
!   def iout_dw      - output unit for doppler wind stats
!   def iout_srw     - output unit for radar superob wind stats
!   def iout_gps     - output unit for gps refractivity or bending angle stats
!   def iout_sst     - output unit for conventional sst stats
!   def iout_lag     - output unit for conventional lag stats
!   def mype_t       - task to handle temperature stats
!   def mype_q       - task to handle moisture stats
!   def mype_uv      - task to handle wind stats
!   def mype_ps      - task to handle surface pressure stats
!   def mype_pw      - task to handle precipitable water stats
!   def mype_rw      - task to handle radar wind stats
!   def mype_dw      - task to handle doppler wind stats
!   def mype_srw     - task to handle radar superob wind stats
!   def mype_gps     - task to handle gps observation stats
!   def mype_sst     - task to handle conventional sst stats
!   def mype_lag     - task to handle conventional lag stats
!   def oberrflg     - logical for reading in new observation error table
!                      .true.  will read in obs errors from file 'errtable'
!                      .false. will not read in new obs errors
!   def blacklst     - logical for reading in station blacklist table
!                      .true.  will read in blacklist from file 'blacklist'
!                      .false. will not read in blacklist
!   def ref_obs      - logical for reading type of local GPS observation
!                      .true.  will read refractivity
!                      .false. will read bending angle
!   def nprof_gps    - total number of gps profiles over all tasks
!   def sfcmodel     - logical for switching on boundary model for surface data
!   def dtbduv_on    - logical for switching on (.true.) sensitivity of uv winds
!                      to microwave brightness temperatures
!   def rmiss_single - missing value in diagnostic file
!   def offtime_data - logical, if .true., then allow use of obs files with ref time
!                      different from analysis time.
!
!   def hilbert_curve - logical, if .true., then generate hilbert curve based
!                      cross-validation datasets in 2dvar mode.
!   def lread_obs_save - logical, if .true., then write out collective obs selection info
!   def lread_obs_skip - logical, if .true., then read in collective obs selection info
!   def obs_input_common - scratch file to receive collective obs selection info
!
! attributes:
!   langauge: f90
!   machgine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use gsi_4dvar, only: l4dvar
  use constants, only:  izero,ione,zero,one,two,three,four,five
  use mpimod, only: mpi_max,mpi_itype,mpi_comm_world,ierror,npe,mype
  implicit none

! set default as private
  private
! set subroutines and functions to public
  public :: init_obsmod_dflts
  public :: init_directories
  public :: create_obsmod_vars
  public :: init_obsmod_vars
  public :: destroyobs
  public :: destroy_obsmod_vars
  public :: ran01dom
  public :: destroy_genstats_gps
  public :: inquire_obsdiags
! set passed variables to public
  public :: iout_pcp,iout_rad,iadate,write_diag,oberrflg,ndat,dthin,dmesh,l_do_adjoint
  public :: lsaveobsens,lag_ob_type,o3l_ob_type,oz_ob_type,pcp_ob_type,dw_ob_type
  public :: sst_ob_type,srw_ob_type,spd_ob_type,rw_ob_type,gps_ob_type,tcp_ob_type
  public :: rad_ob_type,q_ob_type,pw_ob_type,ps_ob_type,w_ob_type,t_ob_type
  public :: obs_handle,yobs,i_ps_ob_type,i_t_ob_type,i_w_ob_type,i_q_ob_type
  public :: i_spd_ob_type,i_srw_ob_type,i_rw_ob_type,i_dw_ob_type,i_sst_ob_type
  public :: i_pw_ob_type,i_pcp_ob_type,i_oz_ob_type,i_o3l_ob_type,i_gps_ob_type
  public :: i_rad_ob_type,i_tcp_ob_type,i_lag_ob_type,obscounts,obsptr,nobs_type,obsdiags
  public :: cobstype,gpsptr,obs_diag,nprof_gps,gps_allhead,gps_allptr,time_offset,ianldate
  public :: iout_oz,dsis,ref_obs,obsfile_all,lobserver,perturb_obs,ditype,dsfcalc,dplat
  public :: time_window,dval,dtype,dfile,dirname,obs_setup,oberror_tune,offtime_data
  public :: lobsdiagsave,blacklst,hilbert_curve,lobskeep,time_window_max,sfcmodel
  public :: perturb_fact,dtbduv_on,ndatmax,nsat1,mype_diaghdr,wptr,whead,psptr,pshead
  public :: qptr,qhead,tptr,thead,lobsdiag_allocated,pstail,ttail,wtail,qtail,spdtail
  public :: spdhead,srwtail,srwhead,rwtail,rwhead,dwtail,dwhead,ssttail,ssthead,pwtail
  public :: pwhead,oztail,ozhead,o3ltail,o3lhead,pcptail,pcphead,gpstail,gpshead
  public :: radptr,radtail,radhead,lagtail,laghead,nloz_v8,nloz_v6,nobskeep,gps_alltail
  public :: grids_dim,rmiss_single,nchan_total,tcphead,tcptail,mype_sst,mype_gps
  public :: mype_uv,mype_dw,mype_rw,mype_srw,mype_q,mype_tcp,mype_lag,mype_ps,mype_t
  public :: mype_pw,iout_rw,iout_dw,iout_srw,iout_sst,iout_pw,iout_t,iout_q,iout_tcp
  public :: iout_lag,iout_uv,iout_gps,iout_ps,spdptr,srwptr,rwptr,dwptr,sstptr,pwptr
  public :: ozptr,o3lptr,pcpptr,lagptr,lread_obs_save,obs_input_common,lread_obs_skip
!
  public :: obs_diags,gps_all_ob_head,gps_all_ob_type,w_ob_head,ps_ob_head,q_ob_head
  public :: t_ob_head,spd_ob_head,rw_ob_head,dw_ob_head,sst_ob_head
  public :: pcp_ob_head,o3l_ob_head,gps_ob_head
  public :: lag_ob_head,srw_ob_head,pw_ob_head,oz_ob_head,rad_ob_head
  public :: tcp_ob_head,odiags

! Set parameters
  integer(i_kind),parameter:: ndatmax = 200_i_kind  ! maximum number of observation files
  real(r_single), parameter:: rmiss_single = -999.0_r_single

! Declare types

  integer(i_kind),parameter::  i_ps_ob_type= ione        ! ps_ob_type
  integer(i_kind),parameter::   i_t_ob_type= 2_i_kind    ! t_ob_type
  integer(i_kind),parameter::   i_w_ob_type= 3_i_kind    ! w_ob_type
  integer(i_kind),parameter::   i_q_ob_type= 4_i_kind    ! q_ob_type
  integer(i_kind),parameter:: i_spd_ob_type= 5_i_kind    ! spd_ob_type
  integer(i_kind),parameter:: i_srw_ob_type= 6_i_kind    ! srw_ob_type
  integer(i_kind),parameter::  i_rw_ob_type= 7_i_kind    ! rw_ob_type
  integer(i_kind),parameter::  i_dw_ob_type= 8_i_kind    ! dw_ob_type
  integer(i_kind),parameter:: i_sst_ob_type= 9_i_kind    ! sst_ob_type
  integer(i_kind),parameter::  i_pw_ob_type=10_i_kind    ! pw_ob_type
  integer(i_kind),parameter:: i_pcp_ob_type=11_i_kind    ! pcp_ob_type
  integer(i_kind),parameter::  i_oz_ob_type=12_i_kind    ! oz_ob_type
  integer(i_kind),parameter:: i_o3l_ob_type=13_i_kind    ! o3l_ob_type
  integer(i_kind),parameter:: i_gps_ob_type=14_i_kind    ! gps_ob_type
  integer(i_kind),parameter:: i_rad_ob_type=15_i_kind    ! rad_ob_type
  integer(i_kind),parameter:: i_tcp_ob_type=16_i_kind    ! tcp_ob_type
  integer(i_kind),parameter:: i_lag_ob_type=17_i_kind    ! lag_ob_type

  integer(i_kind),parameter:: nobs_type = 17_i_kind      ! number of observation types

! Structure for diagnostics

  type obs_diag
     sequence
     type(obs_diag), pointer :: next => NULL()
     real(r_kind), pointer :: nldepart(:)    ! (miter+ione)
     real(r_kind), pointer :: tldepart(:)    ! (miter)
     real(r_kind), pointer :: obssen(:)      ! (miter)
     real(r_kind) :: wgtjo
     integer(i_kind) :: indxglb
     integer(i_kind) :: nchnperobs           ! number of channels per observations
                                             !  (dummy, expect for radiances)
     logical, pointer :: muse(:)             ! (miter)
     logical :: luse
  end type obs_diag

  type obs_diags
     type(obs_diag), pointer :: head => NULL()
     type(obs_diag), pointer :: tail => NULL()
  end type obs_diags

  type odiags
     sequence
     type(obs_diag), pointer :: ptr => NULL()
  end type odiags

! Main observation data structure

  type ps_ob_type
     sequence
     type(ps_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  surface pressure residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind)    :: ppertb        !  random number adding to the obs
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: kx            !  ob type
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type ps_ob_type
 
  type ps_ob_head
     type(ps_ob_type),pointer :: head => NULL()
  end type ps_ob_head

  type tcp_ob_type
     sequence
     type(tcp_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  surface pressure residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind)    :: ppertb        !  random number adding to the obs
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: kx            !  ob type
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type tcp_ob_type

  type tcp_ob_head
     type(tcp_ob_type),pointer :: head => NULL()
  end type tcp_ob_head

  type t_ob_type
     sequence
     type(t_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL() 
     real(r_kind)    :: res           !  temperature residual
     real(r_kind)    :: err2          !  temperature error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: tlm_tsfc(6)   !  sensitivity vector for sfc temp 
                                      !  forward model
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     real(r_kind)    :: tpertb        !  random number adding to the obs
     logical         :: luse          !  flag indicating if ob is used in pen.
     logical         :: use_sfc_model !  logical flag for using boundary model
     logical         :: tv_ob         !  logical flag for virtual temperature or
     integer(i_kind) :: k1            !  level of errtable 1-33
     integer(i_kind) :: kx            !  ob type
     integer(i_kind) :: ij(8)         !  horizontal locations
  end type t_ob_type

  type t_ob_head
     type(t_ob_type),pointer :: head => NULL()
  end type t_ob_head
  
  type w_ob_type
     sequence
     type(w_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diagu => NULL()
     type(obs_diag), pointer :: diagv => NULL()
     real(r_kind)    :: ures          !  u component residual
     real(r_kind)    :: vres          !  v component residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     real(r_kind)    :: upertb        !  random number adding to the obs
     real(r_kind)    :: vpertb        !  random number adding to the obs
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: k1            !  level of errtable 1-33
     integer(i_kind) :: kx            !  ob type
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type w_ob_type

  type w_ob_head
     type(w_ob_type),pointer :: head => NULL()
  end type w_ob_head

  type q_ob_type
     sequence
     type(q_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL() 
     real(r_kind)    :: res           !  moisture residual
     real(r_kind)    :: err2          !  moisture error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     real(r_kind)    :: qpertb        !  random number adding to the obs
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: k1            !  level of errtable 1-33
     integer(i_kind) :: kx            !  ob type
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type q_ob_type

  type q_ob_head
     type(q_ob_type),pointer :: head => NULL()
  end type q_ob_head

  type spd_ob_type
     sequence
     type(spd_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  speed observation
     real(r_kind)    :: err2          !  speed error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind)    :: uges          !  guess u value        
     real(r_kind)    :: vges          !  guess v value        
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type spd_ob_type

  type spd_ob_head
     type(spd_ob_type),pointer :: head => NULL()
  end type spd_ob_head

  type srw_ob_type
     sequence
     type(srw_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diagu => NULL()
     type(obs_diag), pointer :: diagv => NULL()
     real(r_kind)    :: res1          !  first component residual
     real(r_kind)    :: res2          !  second component residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: ges1          !  first component guess
     real(r_kind)    :: ges2          !  second component guess
     real(r_kind)    :: rsrw(4)       !  forward model for radar superob wind 
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type srw_ob_type

  type srw_ob_head
     type(srw_ob_type),pointer :: head => NULL()
  end type srw_ob_head

  type rw_ob_type
     sequence
     type(rw_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()     
     real(r_kind)    :: res           !  radial wind residual
     real(r_kind)    :: err2          !  radial wind error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: cosazm        !  v factor
     real(r_kind)    :: sinazm        !  u factor
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type rw_ob_type

  type rw_ob_head    
     type(rw_ob_type),pointer :: head => NULL()
  end type rw_ob_head

  type dw_ob_type
     sequence
     type(dw_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  doppler wind residual
     real(r_kind)    :: err2          !  radial wind error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: cosazm        !  v factor
     real(r_kind)    :: sinazm        !  u factor
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type dw_ob_type

  type dw_ob_head
     type(dw_ob_type),pointer :: head => NULL()
  end type dw_ob_head

  type sst_ob_type
     sequence
     type(sst_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  sst residual
     real(r_kind)    :: err2          !  sst error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type sst_ob_type

  type sst_ob_head
     type(sst_ob_type),pointer :: head => NULL()
  end type sst_ob_head

  type pw_ob_type
     sequence
     type(pw_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  precipitable water residual
     real(r_kind)    :: err2          !  precipitable water error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: dp  => NULL()
                                      !  delta pressure at mid layers at obs locations
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type pw_ob_type

  type pw_ob_head
     type(pw_ob_type),pointer :: head => NULL()
  end type pw_ob_head

  type oz_ob_type
     sequence
     type(oz_ob_type),pointer :: llpoint => NULL()
     type(odiags), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  ozone residual
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  ozone error squared
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind),dimension(:,:),pointer :: wij => NULL()
                                      !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: prs => NULL()
                                      !  pressure levels
     integer(i_kind),dimension(:),pointer :: ipos  => NULL()
     integer(i_kind) :: nloz          ! number of levels for this profile
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type oz_ob_type

  type oz_ob_head    
     type(oz_ob_type),pointer :: head => NULL()
  end type oz_ob_head

  type o3l_ob_type
     sequence
     type(o3l_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  ozone residual
     real(r_kind)    :: err2          !  ozone obs error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type o3l_ob_type

  type o3l_ob_head
     type(o3l_ob_type),pointer :: head => NULL()
  end type o3l_ob_head

  type gps_ob_type
     sequence
     type(gps_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  gps residual
     real(r_kind)    :: err2          !  gps error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights

     real(r_kind),dimension(:),pointer :: jac_q => NULL()
                                      !  q jacobian 
     real(r_kind),dimension(:),pointer :: jac_t => NULL()
                                      !  t jacobian 
     real(r_kind),dimension(:),pointer :: jac_p => NULL()
                                      !  p jacobian
     integer(i_kind),dimension(:,:),pointer :: ij  => NULL()
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type gps_ob_type

  type gps_ob_head
     type(gps_ob_type),pointer :: head => NULL()
  end type gps_ob_head

  type gps_all_ob_type
     sequence
     type(gps_all_ob_type),pointer :: llpoint => NULL()
     type(gps_ob_type),pointer :: mmpoint => NULL()
     real(r_kind)    :: ratio_err                        
     real(r_kind)    :: obserr                        
     real(r_kind)    :: dataerr                       
     real(r_kind)    :: pg                       
     real(r_kind)    :: b                      
     real(r_kind)    :: loc                    
     real(r_kind)    :: type               

     real(r_kind),dimension(:),pointer :: rdiag => NULL()
     integer(i_kind) :: kprof
     logical         :: luse          !  flag indicating if ob is used in pen.
     logical         :: muse          !  flag indicating if ob is used in pen.
     character(8)    :: cdiag
  end type gps_all_ob_type

  type gps_all_ob_head
     type(gps_all_ob_type),pointer :: head => NULL()
  end type gps_all_ob_head

  type rad_ob_type
     sequence
     type(rad_ob_type),pointer :: llpoint => NULL()
     type(odiags), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  error variances squared (nchan)
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  error variances squared (nchan)
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  ratio of error variances squared (nchan)
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:,:),pointer :: pred => NULL()
                                      !  predictors (npred,nchan)
     real(r_kind),dimension(:,:),pointer :: dtb_dvar => NULL()
                                      !  error variances squared (nsig3p3,nchan)
     integer(i_kind),dimension(:),pointer :: icx  => NULL()
     integer(i_kind) :: nchan         !  number of channels for this profile
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type rad_ob_type

  type rad_ob_head   
     type(rad_ob_type),pointer :: head => NULL()
  end type rad_ob_head

  type pcp_ob_type
     sequence
     type(pcp_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: obs           !  observed precipitation value 
     real(r_kind)    :: err2          !  error variances squared
     real(r_kind)    :: raterr2       !  ratio of error variances squared 
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: ges           !  guess observation value
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: predp => NULL()
                                      !  predictors (npredp)
     real(r_kind),dimension(:),pointer :: dpcp_dvar => NULL()
                                      !  error variances squared (nsig5)
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: icxp          !  type of precipitation rate observation
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type pcp_ob_type

  type pcp_ob_head
     type(pcp_ob_type),pointer :: head => NULL()
  end type pcp_ob_head
 

  type lag_ob_type
     sequence
     type(lag_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diag_lon => NULL()
     type(obs_diag), pointer :: diag_lat => NULL()
     real(r_kind)    :: res_lon       ! residual
     real(r_kind)    :: res_lat       ! residual
     real(r_kind)    :: err2_lon      ! error squared
     real(r_kind)    :: err2_lat      ! error squared
     real(r_kind)    :: raterr2       ! square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: obslon        ! observed longitude (rad)
     real(r_kind)    :: obslat        ! observed latitude  (rad)
     real(r_kind)    :: geslon        ! guessed longitude (rad)
     real(r_kind)    :: geslat        ! guessed latitude  (rad)
     real(r_kind)   ,dimension(:),allocatable :: specr  ! TL parameter
     real(r_kind)    :: time          ! observation time in sec     
     real(r_kind)    :: b             ! variational quality control parameter
     real(r_kind)    :: pg            ! variational quality control parameter
     integer(i_kind),dimension(:),allocatable :: speci  ! TL parameter
     integer(i_kind) :: intnum        ! internal number of balloon
     logical         :: luse          ! flag indicating if ob is used in pen.
  end type lag_ob_type

  type lag_ob_head
     type(lag_ob_type),pointer :: head => NULL()
  end type lag_ob_head
  ! lfm --------------------------------------------------------------------

  type obs_handle
     sequence
     type(ps_ob_type),pointer  :: ps  => NULL() 
     type(t_ob_type),pointer   :: t   => NULL()
     type(w_ob_type),pointer   :: w   => NULL()
     type(q_ob_type),pointer   :: q   => NULL()
     type(spd_ob_type),pointer :: spd => NULL()
     type(srw_ob_type),pointer :: srw => NULL()
     type(rw_ob_type),pointer  :: rw  => NULL()
     type(dw_ob_type),pointer  :: dw  => NULL()
     type(sst_ob_type),pointer :: sst => NULL()
     type(pw_ob_type),pointer  :: pw  => NULL()
     type(oz_ob_type),pointer  :: oz  => NULL()
     type(o3l_ob_type),pointer :: o3l => NULL()
     type(gps_ob_type),pointer :: gps => NULL()
     type(rad_ob_type),pointer :: rad => NULL()
     type(pcp_ob_type),pointer :: pcp => NULL()
     type(tcp_ob_type),pointer :: tcp => NULL()
     type(lag_ob_type),pointer :: lag => NULL()
  end type obs_handle

! Declare types

  type(ps_ob_head),dimension(:),allocatable :: pshead
  type(ps_ob_head),dimension(:),allocatable :: pstail
  type(ps_ob_type),pointer :: psptr => NULL()
  type(tcp_ob_head),dimension(:),allocatable :: tcphead            
  type(tcp_ob_head),dimension(:),allocatable :: tcptail             
  type(tcp_ob_type),pointer :: tcpptr => NULL()
  type(t_ob_head),dimension(:),allocatable :: thead
  type(t_ob_head),dimension(:),allocatable :: ttail
  type(t_ob_type),pointer :: tptr => NULL()
  type(w_ob_head),dimension(:),pointer :: whead
  type(w_ob_head),dimension(:),pointer :: wtail
  type(w_ob_type),pointer :: wptr => NULL()
  type(q_ob_head),dimension(:),pointer :: qhead
  type(q_ob_head),dimension(:),pointer :: qtail
  type(q_ob_type),pointer :: qptr => NULL()
  type(spd_ob_head),dimension(:),pointer :: spdhead
  type(spd_ob_head),dimension(:),pointer :: spdtail
  type(spd_ob_type),pointer :: spdptr => NULL()
  type(srw_ob_head),dimension(:),pointer :: srwhead
  type(srw_ob_head),dimension(:),pointer :: srwtail
  type(srw_ob_type),pointer :: srwptr => NULL()
  type(rw_ob_head),dimension(:),pointer :: rwhead
  type(rw_ob_head),dimension(:),pointer :: rwtail
  type(rw_ob_type),pointer :: rwptr => NULL()
  type(dw_ob_head),dimension(:),pointer :: dwhead
  type(dw_ob_head),dimension(:),pointer :: dwtail
  type(dw_ob_type),pointer :: dwptr => NULL()
  type(sst_ob_head),dimension(:),pointer :: ssthead
  type(sst_ob_head),dimension(:),pointer :: ssttail
  type(sst_ob_type),pointer :: sstptr => NULL()
  type(pcp_ob_head),dimension(:),pointer :: pcphead
  type(pcp_ob_head),dimension(:),pointer :: pcptail
  type(pcp_ob_type),pointer :: pcpptr => NULL()
  type(pw_ob_head),dimension(:),pointer :: pwhead
  type(pw_ob_head),dimension(:),pointer :: pwtail
  type(pw_ob_type),pointer :: pwptr => NULL()
  type(oz_ob_head),dimension(:),pointer :: ozhead
  type(oz_ob_head),dimension(:),pointer :: oztail
  type(oz_ob_type),pointer :: ozptr => NULL()
  type(o3l_ob_head),dimension(:),pointer :: o3lhead
  type(o3l_ob_head),dimension(:),pointer :: o3ltail
  type(o3l_ob_type),pointer :: o3lptr => NULL()
  type(gps_ob_head),dimension(:),pointer :: gpshead
  type(gps_ob_head),dimension(:),pointer :: gpstail
  type(gps_ob_type),pointer :: gpsptr => NULL()
  type(gps_all_ob_head),dimension(:),pointer :: gps_allhead
  type(gps_all_ob_head),dimension(:),pointer :: gps_alltail
  type(gps_all_ob_type),pointer :: gps_allptr => NULL()
  type(rad_ob_head),dimension(:),pointer :: radhead
  type(rad_ob_head),dimension(:),pointer :: radtail
  type(rad_ob_type),pointer :: radptr => NULL()
  type(lag_ob_head),dimension(:),pointer :: laghead
  type(lag_ob_head),dimension(:),pointer :: lagtail
  type(lag_ob_type),pointer :: lagptr => NULL()

  type(obs_handle),dimension(:),pointer :: yobs

  type(obs_diags), pointer :: obsdiags(:,:)  ! (nobs_type,nobs_bins)
  type(obs_diag), pointer :: obsptr

! Declare interfaces
  interface destroyobs; module procedure destroyobs_; end interface

! Declare global variables

  real(r_kind) perturb_fact,time_window_max,time_offset
  real(r_kind),dimension(ndatmax):: dval,dmesh,time_window

  integer(i_kind) grids_dim,nchan_total,ianldate
  integer(i_kind) ndat,ndat_types,ndat_times,nprof_gps
  integer(i_kind) lunobs_obs,nloz_v6,nloz_v8,nobskeep
  integer(i_kind) iout_rad,iout_pcp,iout_t,iout_q,iout_uv, &
                  iout_oz,iout_ps,iout_pw,iout_rw
  integer(i_kind) iout_dw,iout_srw,iout_gps,iout_sst,iout_tcp,iout_lag
  integer(i_kind) mype_t,mype_q,mype_uv,mype_ps,mype_pw, &
                  mype_rw,mype_dw,mype_srw,mype_gps,mype_sst, &
                  mype_tcp,mype_lag
  integer(i_kind),dimension(5):: iadate
  integer(i_kind),dimension(ndatmax):: dsfcalc,dthin,ipoint
  integer(i_kind),allocatable,dimension(:)::  nsat1,mype_diaghdr
  integer(i_kind),allocatable :: obscounts(:,:)
  
  character(128) obs_setup
  character(128) dirname
  character(128) obs_input_common
  character(14),dimension(ndatmax):: obsfile_all
  character(10),dimension(ndatmax):: dtype,ditype,dplat
  character(13),dimension(ndatmax):: dfile
  character(20),dimension(ndatmax):: dsis
  character(len=20) :: cobstype(nobs_type)

  logical oberrflg,oberror_tune,perturb_obs,ref_obs,sfcmodel,dtbduv_on
  logical blacklst,lobsdiagsave,lobsdiag_allocated,lobskeep,lsaveobsens
  logical lobserver,l_do_adjoint
  logical,dimension(0:50):: write_diag
  logical offtime_data
  logical hilbert_curve
  logical lread_obs_save
  logical lread_obs_skip

contains

  subroutine init_obsmod_dflts
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_obsmod_dflts
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  set defaults for observation related variables
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!   2004-07-23  derber - add conventional sst observations
!   2004-12-22  treadon - add initialization of write_diag
!   2005-02-07  treadon - change mype_* for obs types 
!   2005-02-18  treadon - change write_diag(1) default to .true.
!   2005-05-27  yanqiu  - added obs_sen
!   2005-05-27  derber  - add oberrflg
!   2005-06-14  wu      - add OMI oz (ozo)
!   2006-10-25  sienkiewicz - introduce blacklst
!   2007-05-03  todling - use values def above as indexes to cobstype
!   2008-11-25  todling - remove line-by-line adj triggers
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
    implicit none

    integer(i_kind) i


!   Set logical flag
    perturb_obs = .false.   ! .true. = perturb observations
    oberror_tune = .false.   ! .true. = tune oberror
    perturb_fact = one 
    do i=0,50
       write_diag(i)=.false.
    end do
    write_diag(ione)=.true.
    lobsdiagsave=.false.
    lobsdiag_allocated=.false.
    lobskeep=.false.
    nobskeep=izero
    lsaveobsens=.false.
    l_do_adjoint=.true.     ! .true. = apply H^T when in int routines
    oberrflg  = .false.
    sfcmodel  = .false.     ! .false. = do not use boundary layer model 
    dtbduv_on = .true.      ! .true. = use microwave dTb/duv in inner loop
    if (l4dvar) then
       offtime_data = .true.   ! .true. = ignore difference in obs ref time
                               !            and analysis time
    else
       offtime_data = .false.  ! .false. = code fails if data files contain ref time
                               !            different from analysis time
    endif
    blacklst  = .false.
    lobserver = .false.     ! when .t., calculate departure vectors only

!   Specify unit numbers to which to write data counts, indication of quality control
!   decisions, and statistics summary of innovations.  For radiance data also write
!   bias correction coefficients to this unit (iout_rad)
    iout_ps=201_i_kind    ! surface pressure
    iout_uv=202_i_kind    ! u,v wind components
    iout_t=203_i_kind     ! virtual temperature
    iout_q=204_i_kind     ! moisure (specific humidity)
    iout_pw=205_i_kind    ! total column water (precipitable water)
    iout_oz=206_i_kind    ! ozone
    iout_rad=207_i_kind   ! radiance (brightness temperature)
    iout_pcp=208_i_kind   ! precipitation rate
    iout_rw=209_i_kind    ! radar radial wind
    iout_dw=210_i_kind    ! doppler lidar wind
    iout_srw=211_i_kind   ! radar superob wind
    iout_gps=212_i_kind   ! gps refractivity or bending angle
    iout_sst=213_i_kind   ! conventional sst
    iout_tcp=214_i_kind   ! synthetic tc-mslp
    iout_lag=215_i_kind   ! lagrangian tracers

    mype_ps = npe-ione                  ! surface pressure
    mype_uv = max(izero,npe-2_i_kind)   ! u,v wind components
    mype_t  = max(izero,npe-3_i_kind)   ! virtual temperature
    mype_q  = max(izero,npe-4_i_kind)   ! moisture (specific humidity)
    mype_pw = max(izero,npe-5_i_kind)   ! total column water
    mype_rw = max(izero,npe-6_i_kind)   ! radar radial wind
    mype_dw = max(izero,npe-7_i_kind)   ! doppler lidar wind
    mype_srw= max(izero,npe-8_i_kind)   ! radar superob wind
    mype_gps= max(izero,npe-9_i_kind)   ! gps refractivity or bending angle
    mype_sst= max(izero,npe-10_i_kind)  ! conventional sst
    mype_tcp= max(izero,npe-11_i_kind)  ! synthetic tc-mslp
    mype_lag= max(izero,npe-12_i_kind)  ! lagrangian tracers
    
!   Initialize arrays used in namelist obs_input 
    ndat = ndatmax          ! number of observation types (files)
    time_window_max = three ! set maximum time window to +/-three hours
    do i=ione,ndat
       dfile(i) = ' '     ! local file name from which to read observatinal data
       dtype(i) = ' '     ! character string identifying type of observation
       ditype(i)= ' '     ! character string identifying group type of ob
       dplat(i) = ' '     ! currently contains satellite id (no meaning for non-sat data)
       dsis(i)  = ' '     ! sensor/instrument/satellite identifier for info files
       dsfcalc(i)=izero   ! use orig bilinear FOV surface calculation (routine deter_sfc)
       ipoint(i)= izero   ! default pointer (values set in gsisub)
       dthin(i) = ione    ! thinning flag (1=thinning on; otherwise off)
       time_window(i) = time_window_max ! default to maximum time window
       write(obsfile_all(i),100) i      ! name of scratch file to hold obs data
    end do
100 format('obs_input.',i4.4)


!   Other initializations
    nloz_v6 = 12_i_kind               ! number of "levels" in ozone version8 data
    nloz_v8 = 21_i_kind               ! number of "levels" in ozone version6 data

    lunobs_obs = 2_i_kind             ! unit to which to write/read information
                                      ! related to brightness temperature and 
                                      ! precipitation rate observations

    grids_dim= 161_i_kind             ! grid points for integration of GPS bend

    nprof_gps = izero

!   Define a name for obs types
    cobstype( i_ps_ob_type)="surface pressure    " ! ps_ob_type
    cobstype(  i_t_ob_type)="temperature         " ! t_ob_type
    cobstype(  i_w_ob_type)="wind                " ! w_ob_type
    cobstype(  i_q_ob_type)="moisture            " ! q_ob_type
    cobstype(i_spd_ob_type)="wind speed          " ! spd_ob_type
    cobstype(i_srw_ob_type)="srw                 " ! srw_ob_type
    cobstype( i_rw_ob_type)="radial wind         " ! rw_ob_type
    cobstype( i_dw_ob_type)="doppler wind        " ! dw_ob_type
    cobstype(i_sst_ob_type)="sst                 " ! sst_ob_type
    cobstype( i_pw_ob_type)="precipitable water  " ! pw_ob_type
    cobstype(i_pcp_ob_type)="precipitation       " ! pcp_ob_type
    cobstype( i_oz_ob_type)="ozone               " ! oz_ob_type
    cobstype(i_o3l_ob_type)="level ozone         " ! o3l_ob_type
    cobstype(i_gps_ob_type)="gps                 " ! gps_ob_type
    cobstype(i_rad_ob_type)="radiance            " ! rad_ob_type
    cobstype(i_tcp_ob_type)="tcp (tropic cyclone)" ! tcp_ob_type
    cobstype(i_lag_ob_type)="lagrangian tracer   " ! lag_ob_type

    hilbert_curve=.false.

    obs_input_common = 'obs_input.common'
    lread_obs_save = .false.
    lread_obs_skip = .false.

    return
  end subroutine init_obsmod_dflts
  
  subroutine init_directories(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create sub-directories
!     prgmmr:    kleist      org: np23                date: 2007-06-08
!
! abstract:  set-up name for and create sub-directories to 
!            hold observation and diagnostic files.   Doing
!            so on IBM SP reduces wall clock and stabilizes
!            run times
!
! program history log:
!   2007-06-08  kleist
!   2008-06-02  safford - add doc block end
!
!   input argument list:
!      mype - pe task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
    implicit none

    integer(i_kind),intent(in   ) :: mype

    character(len=144):: command
    character(len=8):: pe_name

#ifdef ibm_sp
    write(pe_name,'(i4.4)') mype
    dirname = 'dir.'//trim(pe_name)//'/'
    command = 'mkdir -m 755 ' // trim(dirname)
    call system(command)
#else
    write(pe_name,100) mype
100 format('pe',i4.4,'.')
    dirname= trim(pe_name)
#endif
    return
  end subroutine init_directories
  
  subroutine create_obsmod_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_obsmod_vars
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  allocate arrays to hold observation related information
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
    use gsi_4dvar, only: nobs_bins
    implicit none

    allocate (nsat1(ndat),mype_diaghdr(ndat))

    ALLOCATE(thead  (nobs_bins))
    ALLOCATE(ttail  (nobs_bins))
    ALLOCATE(pshead (nobs_bins))
    ALLOCATE(pstail (nobs_bins))
    ALLOCATE(tcphead(nobs_bins))
    ALLOCATE(tcptail(nobs_bins))
    ALLOCATE(whead  (nobs_bins))
    ALLOCATE(wtail  (nobs_bins))
    ALLOCATE(qhead  (nobs_bins))
    ALLOCATE(qtail  (nobs_bins))
    ALLOCATE(spdhead(nobs_bins))
    ALLOCATE(spdtail(nobs_bins))
    ALLOCATE(srwhead(nobs_bins))
    ALLOCATE(srwtail(nobs_bins))
    ALLOCATE(rwhead (nobs_bins))
    ALLOCATE(rwtail (nobs_bins))
    ALLOCATE(dwhead (nobs_bins))
    ALLOCATE(dwtail (nobs_bins))
    ALLOCATE(ssthead(nobs_bins))
    ALLOCATE(ssttail(nobs_bins))
    ALLOCATE(pcphead(nobs_bins))
    ALLOCATE(pcptail(nobs_bins))
    ALLOCATE(pwhead (nobs_bins))
    ALLOCATE(pwtail (nobs_bins))
    ALLOCATE(ozhead (nobs_bins))
    ALLOCATE(oztail (nobs_bins))
    ALLOCATE(o3lhead(nobs_bins))
    ALLOCATE(o3ltail(nobs_bins))
    ALLOCATE(radhead(nobs_bins))
    ALLOCATE(radtail(nobs_bins))
    ALLOCATE(gpshead(nobs_bins))
    ALLOCATE(gpstail(nobs_bins))
    ALLOCATE(gps_allhead(nobs_bins))
    ALLOCATE(gps_alltail(nobs_bins))
    ALLOCATE(laghead(nobs_bins))
    ALLOCATE(lagtail(nobs_bins))

    ALLOCATE(yobs(nobs_bins))

    ALLOCATE(obsdiags(nobs_type,nobs_bins))

    return
  end subroutine create_obsmod_vars
! ----------------------------------------------------------------------
  subroutine init_obsmod_vars(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_obsmod_vars
!     prgmmr:    tremolet          org: GMAO           date: 2007-02-02
!
! abstract:  set values for observation related variables
!
! program history log:
!   2007-02-02  tremolet
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!
!$$$  end documentation block
    use gsi_4dvar, only: nhr_assimilation
    use gridmod, only: regional
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) ii,jj,ioff
    character(len=2) :: cind
    logical :: limit

!   Because obs come in 6-hour batches
    ndat_times=nhr_assimilation/6
    if(regional)ndat_times = nhr_assimilation/3
!   if (ndat_times*6/=nhr_assimilation) then
!       write(6,*)'OBSMOD:  ***ERROR*** in assimilation time window setting; ', &
!        ' must be a multiple of 6hrs: ndat_times= ', ndat_times, &
!        ' nhr_assimilation= ', nhr_assimilation, &
!              ' ***STOP IN OBSMOD***'
!       call stop2(70)
!   endif
    ndat_types=ndat

    ndat=ndat_times*ndat_types

!   The following was in gsimain.
!   Ensure time window specified in obs_input does not exceed
!   specified maximum value
    limit=.false.
    do ii=1,ndat_types
       if (time_window(ii)>time_window_max) then
          time_window(ii) = time_window_max
          limit = .true.
       endif
    end do
    if (mype==izero .and. limit) &
       write(6,*)'INIT_OBSMOD_VARS: reset time window for one or ',&
                 'more OBS_INPUT entries to ',time_window_max

!   Initialize arrays in obs_input if more than one synoptic time
    IF (ndat_times>ione) THEN
!      Copy other things
       DO ii=2,ndat_times
          write(cind,'(i2.2)')ii
          ioff=(ii-ione)*ndat_types
          DO jj=1,ndat_types
             dfile (ioff+jj) = trim(dfile(jj))//'.'//cind
             dmesh (ioff+jj) = dmesh(jj)
             dtype (ioff+jj) = dtype(jj)
             ditype(ioff+jj) = ditype(jj)
             dplat (ioff+jj) = dplat(jj)
             dsis  (ioff+jj) = dsis(jj)
             ipoint(ioff+jj) = ipoint(jj)
             dthin (ioff+jj) = dthin(jj)
             dval  (ioff+jj) = dval(jj)
             time_window(ioff+jj) = time_window(jj)
          ENDDO
       ENDDO
!      Then change name for first time slot
       IF (ndat_times>ione) THEN
          DO jj=ione,ndat_types
             dfile(jj) = trim(dfile(jj))//'.01'
          ENDDO
       ENDIF
    ENDIF

    IF (mype==izero) THEN
       write(6,*)'INIT_OBSMOD_VARS: ndat_times,ndat_types,ndat=', &
                                  & ndat_times,ndat_types,ndat
       write(6,*)'INIT_OBSMOD_VARS: nhr_assimilation=',nhr_assimilation
    ENDIF

    return
  end subroutine init_obsmod_vars
! ----------------------------------------------------------------------

  subroutine destroyobs_ ( skipit )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroyobs
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  deallocate arrays that hold observation information for
!            use in outer and inner loops
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!   2004-07-23  derber - add conventional sst observations
!   2005-06-14  wu      - add OMI oz (ozo)
!
!   input argument list:
!    skipit
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$  end documentation block
    use gsi_4dvar, only: nobs_bins

    implicit none

    logical,optional,intent(in   ) :: skipit

    integer(i_kind) :: ii,jj,istatus
    logical :: skipit_

    skipit_=.false.
    if (present(skipit)) then
       skipit_=skipit
    endif

    do ii=1,nobs_bins
       ttail(ii)%head => thead(ii)%head
       do while (associated(ttail(ii)%head))
          thead(ii)%head => ttail(ii)%head%llpoint
          deallocate(ttail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for t, istatus=',istatus
          ttail(ii)%head => thead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       pwtail(ii)%head => pwhead(ii)%head
       do while (associated(pwtail(ii)%head))
          pwhead(ii)%head => pwtail(ii)%head%llpoint
          deallocate(pwtail(ii)%head%dp,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for pw arrays, istatus=',istatus
          deallocate(pwtail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for pw, istatus=',istatus
          pwtail(ii)%head => pwhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       pstail(ii)%head => pshead(ii)%head
       do while (associated(pstail(ii)%head))
          pshead(ii)%head => pstail(ii)%head%llpoint
          deallocate(pstail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for ps, istatus=',istatus
          pstail(ii)%head => pshead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       wtail(ii)%head => whead(ii)%head
       do while (associated(wtail(ii)%head))
          whead(ii)%head => wtail(ii)%head%llpoint
          deallocate(wtail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for w, istatus=',istatus
          wtail(ii)%head => whead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       qtail(ii)%head => qhead(ii)%head
       do while (associated(qtail(ii)%head))
          qhead(ii)%head => qtail(ii)%head%llpoint
          deallocate(qtail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for q, istatus=',istatus
          qtail(ii)%head => qhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       spdtail(ii)%head => spdhead(ii)%head
       do while (associated(spdtail(ii)%head))
          spdhead(ii)%head => spdtail(ii)%head%llpoint
          deallocate(spdtail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for spd, istatus=',istatus
          spdtail(ii)%head => spdhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       srwtail(ii)%head => srwhead(ii)%head
       do while (associated(srwtail(ii)%head))
          srwhead(ii)%head => srwtail(ii)%head%llpoint
          deallocate(srwtail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for srw, istatus=',istatus
          srwtail(ii)%head => srwhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       rwtail(ii)%head => rwhead(ii)%head
       do while (associated(rwtail(ii)%head))
          rwhead(ii)%head => rwtail(ii)%head%llpoint
          deallocate(rwtail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for rw, istatus=',istatus
          rwtail(ii)%head => rwhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       dwtail(ii)%head => dwhead(ii)%head
       do while (associated(dwtail(ii)%head))
          dwhead(ii)%head => dwtail(ii)%head%llpoint
          deallocate(dwtail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for dw, istatus=',istatus
          dwtail(ii)%head => dwhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       ssttail(ii)%head => ssthead(ii)%head
       do while (associated(ssttail(ii)%head))
          ssthead(ii)%head => ssttail(ii)%head%llpoint
          deallocate(ssttail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for sst, istatus=',istatus
          ssttail(ii)%head => ssthead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       oztail(ii)%head => ozhead(ii)%head
       do while (associated(oztail(ii)%head))
          ozhead(ii)%head => oztail(ii)%head%llpoint
          deallocate(oztail(ii)%head%res, oztail(ii)%head%wij,&
                     oztail(ii)%head%err2,oztail(ii)%head%raterr2, &
                     oztail(ii)%head%prs,oztail(ii)%head%ipos,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for oz arrays, istatus=',istatus
          deallocate(oztail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for oz, istatus=',istatus
          oztail(ii)%head => ozhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       o3ltail(ii)%head => o3lhead(ii)%head
       do while (associated(o3ltail(ii)%head))
          o3lhead(ii)%head => o3ltail(ii)%head%llpoint
          deallocate(o3ltail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for o3l, istatus=',istatus
          o3ltail(ii)%head => o3lhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       gpstail(ii)%head => gpshead(ii)%head
       do while (associated(gpstail(ii)%head))
          gpshead(ii)%head => gpstail(ii)%head%llpoint
          deallocate(gpstail(ii)%head%jac_q,gpstail(ii)%head%jac_t, &
                     gpstail(ii)%head%jac_p, &
                     gpstail(ii)%head%ij,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for gps arrays, istatus=',istatus
          deallocate(gpstail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for gps, istatus=',istatus
          gpstail(ii)%head => gpshead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       radtail(ii)%head => radhead(ii)%head
       do while (associated(radtail(ii)%head))
          radhead(ii)%head => radtail(ii)%head%llpoint
          deallocate(radtail(ii)%head%res,radtail(ii)%head%err2, &
                     radtail(ii)%head%raterr2,radtail(ii)%head%pred, &
                     radtail(ii)%head%dtb_dvar, &
                     radtail(ii)%head%icx,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for rad arrays, istatus=',istatus
          deallocate(radtail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for rad, istatus=',istatus
          radtail(ii)%head => radhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       pcptail(ii)%head => pcphead(ii)%head
       do while (associated(pcptail(ii)%head))
          pcphead(ii)%head => pcptail(ii)%head%llpoint
          deallocate(pcptail(ii)%head%predp,pcptail(ii)%head%dpcp_dvar,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for pcp arrays, istatus=',istatus
          deallocate(pcptail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for pcp, istatus=',istatus
          pcptail(ii)%head => pcphead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       tcptail(ii)%head => tcphead(ii)%head
       do while (associated(tcptail(ii)%head))
          tcphead(ii)%head => tcptail(ii)%head%llpoint
          deallocate(tcptail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for tcp, istatus=',istatus
          tcptail(ii)%head => tcphead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       lagtail(ii)%head => laghead(ii)%head
       do while (associated(lagtail(ii)%head))
          laghead(ii)%head => lagtail(ii)%head%llpoint
          deallocate(lagtail(ii)%head%speci,lagtail(ii)%head%specr,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for lag arrays, istatus=',istatus
          deallocate(lagtail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROYOBS:  deallocate error for lag, istatus=',istatus
          lagtail(ii)%head => laghead(ii)%head
       end do
    end do

    if (l4dvar) then
       if (.not. skipit_) then
          do ii=1,nobs_bins
             do jj=1,nobs_type
                obsptr => obsdiags(jj,ii)%head
                do while (associated(obsptr))
                   obsdiags(jj,ii)%head => obsptr%next
                   deallocate(obsptr%nldepart,obsptr%tldepart,obsptr%obssen,obsptr%muse)
                   deallocate(obsptr)
                   obsptr => obsdiags(jj,ii)%head
                enddo
             enddo
          enddo
          lobsdiag_allocated=.false.
       endif
    endif

    if (allocated(obscounts)) deallocate(obscounts) 

    return
  end subroutine destroyobs_
  
  subroutine destroy_obsmod_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroyb_obsmod_vars
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  deallocate arrays that hold observation information
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$  end documentation block
    implicit none

    deallocate(nsat1,mype_diaghdr)
    return
  end subroutine destroy_obsmod_vars

  real(r_kind) function ran01dom()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ran01dom    generate a random number with mean:0 variance:1
!   prgmmr: wu               org: np22                date: 2005-10-27
!
! abstract:  generate a random number with mean:0 variance:1
!
! program history log:
!   2005-10-27  wu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block

    use constants, only: five
    implicit none

    integer(i_kind):: j
    real(r_kind),dimension(10):: a

    call random_number(a)
    ran01dom=zero
    do j=1,10
       ran01dom=ran01dom+a(j)
    enddo
    ran01dom=(ran01dom-five)/0.912345_r_kind
    return
  end function ran01dom

  subroutine destroy_genstats_gps
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_genstats_gps
!     prgmmr:    treadon     org: np20                date: 2005-12-21
!
! abstract:  deallocate arrays holding gps information
!
! program history log:
!   2005-12-21  treadon
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$  end documentation block
    use gsi_4dvar, only: nobs_bins
    implicit none

    integer(i_kind):: istatus,ii

    do ii=1,nobs_bins
       gps_alltail(ii)%head => gps_allhead(ii)%head
       do while (associated(gps_alltail(ii)%head))
          gps_allhead(ii)%head => gps_alltail(ii)%head%llpoint
          deallocate(gps_alltail(ii)%head,stat=istatus)
          if (istatus/=izero) write(6,*)'DESTROY_GENSTATS_GPS: deallocate error for gps_all, istatus=',istatus
          gps_alltail(ii)%head => gps_allhead(ii)%head
       end do
    end do

    return
  end subroutine destroy_genstats_gps

! ----------------------------------------------------------------------
subroutine inquire_obsdiags(kiter)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inquire_obsdiags
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-07  lueken - added  subprogram doc block
!
!   input argument list:
!    kiter
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

integer(i_kind), intent(in   ) :: kiter

real(r_kind) :: sizei, sizer, sizel, sizep, ziter, zsize, ztot
integer(i_kind) :: ii,jj,iobsa(2),iobsb(2)

! Any better way to determine size or i_kind, r_kind, etc... ?
sizei=four
sizer=8.0_r_kind
sizel=one
sizep=four

iobsa(:)=izero
do ii=1,size(obsdiags,2)
   do jj=1,size(obsdiags,1)
      obsptr => obsdiags(jj,ii)%head
      do while (associated(obsptr))
         iobsa(ione)=iobsa(ione)+ione
         if (ANY(obsptr%muse(:))) iobsa(2)=iobsa(2)+ione
         obsptr => obsptr%next
      enddo
   enddo
enddo

call mpi_reduce(iobsa,iobsb,2_i_kind,mpi_itype,mpi_max,izero,mpi_comm_world,ierror)

if (mype==izero) then
   ziter=real(kiter,r_kind)
   zsize = sizer*(three*ziter+two) + sizei + sizel*(ziter+one) + sizep*five
   ztot=real(iobsb(ione),r_kind)*zsize
   ztot=ztot/(1024.0_r_kind*1024.0_r_kind)
 
   write(6,*)'obsdiags: Bytes per element=',NINT(zsize)
   write(6,*)'obsdiags: length total, used=',iobsb(ione),iobsb(2_i_kind)
   write(6,'(A,F8.1,A)')'obsdiags: Estimated memory usage= ',ztot,' Mb'
endif

end subroutine inquire_obsdiags
! ----------------------------------------------------------------------

end module obsmod
