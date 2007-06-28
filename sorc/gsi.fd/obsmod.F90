module obsmod
!$$$   module documentation block
!                .      .    .                                       .
! module:  obsmod
! prgmmr:  derber             org: np23               date: 2003-09-25
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
! 
! Subroutines Included:
!   sub init_obsmod_dflts   - initialize obs related variables to default values
!   sub create_obsmod_vars  - allocate obs related variables
!   sub destroyobs          - deallocate obs linked lists
!   sub destroy_obsmod_vars - deallocate obsmod arrays
!   sub destroy_genstats_gps- deallocate linked list for gsp statistics
!
! Variable Definitions:
!   def diag_conv    - namelist logical for generation (=true) of conv data diag files
!   def perturb_obs  - namelist logical to perturb (=true) observations
!   def perturb_fact - namelist scaling factor for observation perturbations
!   def write_diag   - namelist logical array to compute/write (=true) diag files
!   def obs_setup    - prefix for files passing pe relative obs data to setup routines
!   def dfile        - input observation file names
!   def dsis         - sensor/instrument/satellite flag from info files
!   def dtype        - observation types
!   def ditype       - observation group type (set in read_obs, e.g. rad,conv,etc)
!   def time_window  - half time window for obs type (hours)
!   def time_window_max - maximum half time window (hours)
!   def obsfile_all  - file containing observations after initial read
!   def ndatmax      - maximum number of data types
!   def ndat         - number of data types
!   def ipoint       - pointer to input namelist for particular processor
!   def iadate       - analysis date and time array
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
!   def ozohead      - omi ozone linked list head
!   def ozotail      - omi ozone linked list tail
!   def radhead      - radiance linked list head
!   def radtail      - radiance linked list tail
!   def pcphead      - precipitation linked list head
!   def pcptail      - precipitation linked list tail
!   def lunobs_obs   - unit to save satellite observation
!   def nloz         - nloz+7 is the length of an ozone data
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
!   def oberrflg     - logical for reading in new observation error table
!                      .true.  will read in obs errors from file 'errtable'
!                      .false. will not read in new obs errors
!   def ref_obs      - logical for reading type of local GPS observation
!                      .true.  will read refractivity
!                      .false. will read bending angle
!   def nprof_gps    - total number of gps profiles over all tasks
!   def sfcmodel     - logical for switching on boundary model for surface data
!   def dtbduv_on    - logical for switching on (.true.) sensitivity of uv winds
!                      to microwave brightness temperatures
!   def rmiss_single - missing value in diagnostic file
!
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single

! Set parameters
  integer(i_kind),parameter:: ndatmax = 100  ! maximum number of observation files
  real(r_single), parameter:: rmiss_single = -999.0_r_single

! Declare types

  type ps_ob_type
     sequence
     type(ps_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  surface pressure residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type ps_ob_type

  type t_ob_type
     sequence
     type(t_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  temperature residual
     real(r_kind)    :: err2          !  temperature error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: tlm_tsfc(6)   !  sensitivity vector for sfc temp 
                                      !  forward model
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     logical         :: use_sfc_model !  logical flag for using boundary model
     logical         :: tv_ob         !  logical flag for virtual temperature or
                                      !  sensible temperature (true if virtual)
     
  end type t_ob_type

  type w_ob_type
     sequence
     type(w_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: ures          !  u component residual
     real(r_kind)    :: vres          !  v component residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type w_ob_type

  type q_ob_type
     sequence
     type(q_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  moisture residual
     real(r_kind)    :: err2          !  moisture error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type q_ob_type
  type spd_ob_type
     sequence
     type(spd_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  speed observation
     real(r_kind)    :: err2          !  speed error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind)    :: uges          !  guess u value        
     real(r_kind)    :: vges          !  guess v value        
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type spd_ob_type

  type srw_ob_type
     sequence
     type(srw_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res1          !  first component residual
     real(r_kind)    :: res2          !  second component residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: ges1          !  first component guess
     real(r_kind)    :: ges2          !  second component guess
     real(r_kind)    :: rsrw(4)       !  forward model for radar superob wind 
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type srw_ob_type

  type rw_ob_type
     sequence
     type(rw_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  radial wind residual
     real(r_kind)    :: err2          !  radial wind error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: cosazm        !  v factor
     real(r_kind)    :: sinazm        !  u factor
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type rw_ob_type

  type dw_ob_type
     sequence
     type(dw_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  doppler wind residual
     real(r_kind)    :: err2          !  radial wind error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: cosazm        !  v factor
     real(r_kind)    :: sinazm        !  u factor
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type dw_ob_type

  type sst_ob_type
     sequence
     type(sst_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  sst residual
     real(r_kind)    :: err2          !  sst error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type sst_ob_type

  type pw_ob_type
     sequence
     type(pw_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  precipitable water residual
     real(r_kind)    :: err2          !  precipitable water error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: dp  => NULL()
                                      !  delta pressure at mid layers at obs locations
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type pw_ob_type
  type oz_ob_type
     sequence
     type(oz_ob_type),pointer :: llpoint => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  ozone residual
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  ozone error squared
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: prs => NULL()
                                      !  pressure levels
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind),dimension(:),pointer :: ipos  => NULL()
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type oz_ob_type
  type ozo_ob_type
     sequence
     type(ozo_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  omi ozone residual
     real(r_kind)    :: err2          !  omi ozone error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: ipos          !              
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type ozo_ob_type
  type gps_ob_type
     sequence
     type(gps_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: res           !  gps residual
     real(r_kind)    :: err2          !  gps error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(12)       !  horizontal interpolation weights
     real(r_kind)    :: termqin       !  guess humidity coefficient for gps ref
     real(r_kind)    :: termpin       !  guess pressure coefficient for gps ref
     real(r_kind)    :: termtin       !  guess temperature coefficient 
                                      !    (at obs) for gps ref
     real(r_kind)    :: termtkin      !  guess temperature coefficient 
                                      !    (level k1) for gps ref
     real(r_kind),dimension(:),pointer :: termtlin => NULL()
                                      !  guess temperature coefficient 
                                      !    (profile) for gps ref
     real(r_kind)    :: gpsloc        !                                   
     real(r_kind)    :: b_imp         !  obs impact parameter 
     real(r_kind),dimension(:),pointer :: b_qin => NULL()
                                      !  guess humidity coefficient 
     real(r_kind),dimension(:),pointer :: b_pin => NULL()
                                      !  guess surface pressure coefficient 
     real(r_kind),dimension(:),pointer :: b_tin => NULL()
                                      !  guess temperature coefficient 
     real(r_kind),dimension(:),pointer :: b_rges => NULL()
                                      !  guess radius 
     real(r_kind),dimension(:),pointer :: b_gp2gm => NULL()
                                      !  guess geopot height coefficient 
     real(r_kind),dimension(:),pointer :: b_n => NULL()
                                      !  guess refractivity (bending angle)
     real(r_kind),dimension(:),pointer :: b_pkges => NULL()
     real(r_kind),dimension(:),pointer :: b_tkges => NULL()
     real(r_kind),dimension(:),pointer :: b_loc => NULL()
                                      !  guess coefficient (bending angle)
     real(r_kind),dimension(:),pointer :: b_xj => NULL()
                                      !  guess coefficient (bending angle)
     integer(i_kind),dimension(:,:),pointer :: ij  => NULL()
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type gps_ob_type

  type gps_all_ob_type
     type(gps_all_ob_type),pointer :: llpoint => NULL()
     type(gps_ob_type),pointer :: mmpoint => NULL()
     real(r_kind)    :: ratio_err                        
     real(r_kind)    :: obserr                        
     real(r_kind)    :: dataerr                       
     real(r_kind)    :: pg                       
     real(r_kind)    :: b                      
     real(r_kind)    :: loc                    
     real(r_kind)    :: pressure               
     real(r_kind)    :: type               
     real(r_kind)    :: rinc               
     integer(i_kind) :: kprof
     logical         :: luse          !  flag indicating if ob is used in pen.
     logical         :: muse          !  flag indicating if ob is used in pen.

  end type gps_all_ob_type

  type rad_ob_type
     type(rad_ob_type),pointer :: llpoint => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  error variances squared (nchan)
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  error variances squared (nchan)
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  ratio of error variances squared (nchan)
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: pred1 => NULL()
                                      !  predictors (not channel dependent)(npred-2)
     real(r_kind),dimension(:),pointer :: pred2 => NULL()
                                      !  predictors (channel dependent) (nchan)
     real(r_kind),dimension(:,:),pointer :: dtb_dvar => NULL()
                                      !  error variances squared (nsig3p3,nchan)
     integer(i_kind) :: nchan         !  number of channels for this profile
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind),dimension(:),pointer :: icx  => NULL()
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type rad_ob_type
  type pcp_ob_type
     type(pcp_ob_type),pointer :: llpoint => NULL()
     real(r_kind)    :: obs           !  observed precipitation value 
     real(r_kind)    :: err2          !  error variances squared
     real(r_kind)    :: raterr2       !  ratio of error variances squared 
     real(r_kind)    :: time          !  observation time
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

! Declare variables

  type(ps_ob_type),pointer :: pshead => NULL()
  type(ps_ob_type),pointer :: pstail => NULL()
  type(ps_ob_type),pointer :: psptr => NULL()
  type(t_ob_type),pointer :: thead => NULL()
  type(t_ob_type),pointer :: ttail => NULL()
  type(t_ob_type),pointer :: tptr => NULL()
  type(w_ob_type),pointer :: whead => NULL()
  type(w_ob_type),pointer :: wtail => NULL()
  type(w_ob_type),pointer :: wptr => NULL()
  type(q_ob_type),pointer :: qhead => NULL()
  type(q_ob_type),pointer :: qtail => NULL()
  type(q_ob_type),pointer :: qptr => NULL()
  type(spd_ob_type),pointer :: spdhead => NULL()
  type(spd_ob_type),pointer :: spdtail => NULL()
  type(spd_ob_type),pointer :: spdptr => NULL()
  type(srw_ob_type),pointer :: srwhead => NULL()
  type(srw_ob_type),pointer :: srwtail => NULL()
  type(srw_ob_type),pointer :: srwptr => NULL()
  type(rw_ob_type),pointer :: rwhead => NULL()
  type(rw_ob_type),pointer :: rwtail => NULL()
  type(rw_ob_type),pointer :: rwptr => NULL()
  type(dw_ob_type),pointer :: dwhead => NULL()
  type(dw_ob_type),pointer :: dwtail => NULL()
  type(dw_ob_type),pointer :: dwptr => NULL()
  type(sst_ob_type),pointer :: ssthead => NULL()
  type(sst_ob_type),pointer :: ssttail => NULL()
  type(sst_ob_type),pointer :: sstptr => NULL()
  type(rad_ob_type),pointer :: radhead => NULL()
  type(rad_ob_type),pointer :: radtail => NULL()
  type(rad_ob_type),pointer :: radptr => NULL()
  type(pcp_ob_type),pointer :: pcphead => NULL()
  type(pcp_ob_type),pointer :: pcptail => NULL()
  type(pcp_ob_type),pointer :: pcpptr => NULL()
  type(pw_ob_type),pointer :: pwhead => NULL()
  type(pw_ob_type),pointer :: pwtail => NULL()
  type(pw_ob_type),pointer :: pwptr => NULL()
  type(gps_ob_type),pointer :: gpshead => NULL()
  type(gps_ob_type),pointer :: gpstail => NULL()
  type(gps_ob_type),pointer :: gpsptr => NULL()
  type(gps_all_ob_type),pointer :: gps_allhead => NULL()
  type(gps_all_ob_type),pointer :: gps_alltail => NULL()
  type(gps_all_ob_type),pointer :: gps_allptr => NULL()
  type(oz_ob_type),pointer :: ozhead => NULL()
  type(oz_ob_type),pointer :: oztail => NULL()
  type(oz_ob_type),pointer :: ozptr => NULL()
  type(ozo_ob_type),pointer :: ozohead => NULL()
  type(ozo_ob_type),pointer :: ozotail => NULL()
  type(ozo_ob_type),pointer :: ozoptr => NULL()

  real(r_kind) perturb_fact,time_window_max
  real(r_kind),dimension(100):: dval,dmesh,time_window

  integer(i_kind) grids_dim,nchan_total
  integer(i_kind) ndat,nprof_gps
  integer(i_kind) lunobs_obs,nloz
  integer(i_kind) iout_rad,iout_pcp,iout_t,iout_q,iout_uv, &
                  iout_oz,iout_ps,iout_pw,iout_rw
  integer(i_kind) iout_dw,iout_srw,iout_gps,iout_sst
  integer(i_kind) mype_t,mype_q,mype_uv,mype_ps,mype_pw, &
                  mype_rw,mype_dw,mype_srw,mype_gps,mype_sst
  integer(i_kind),dimension(5):: iadate
  integer(i_kind),dimension(100):: dthin,ipoint
  integer(i_kind),allocatable,dimension(:)::  nsat1,mype_diaghdr
  integer(i_kind),allocatable,dimension(:,:) :: j_rad,j_pcp  
  
  character(128) obs_setup
  character(128) dirname
  character(14),dimension(100):: obsfile_all
  character(10),dimension(100):: dfile,dtype,ditype,dplat
  character(20),dimension(100):: dsis

  logical obs_sen
  logical diag_conv,oberrflg,perturb_obs,ref_obs,sfcmodel,dtbduv_on
  logical,dimension(0:50):: write_diag

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
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only:  izero,ione,zero,three
    use mpimod, only: npe
    implicit none
    integer(i_kind) i


!   Set logical flag
    diag_conv = .true.      ! .true. = generate diagnostic file for conventional data
    perturb_obs = .false.   ! .true. = perturb observations
    perturb_fact = zero
    do i=0,50
       write_diag(i)=.false.
    end do
    write_diag(1)=.true.
    oberrflg  = .false.
    sfcmodel  = .false.     ! .false. = do not use boundary layer model 
    dtbduv_on = .true.      ! .true. = use microwave dTb/duv in inner loop

!   Specify unit numbers to which to write data counts, indication of quality control
!   decisions, and statistics summary of innovations.  For radiance data also write
!   bias correction coefficients to this unit (iout_rad)
    iout_ps=201    ! surface pressure
    iout_uv=202    ! u,v wind components
    iout_t=203     ! virtual temperature
    iout_q=204     ! moisure (specific humidity)
    iout_pw=205    ! total column water (precipitable water)
    iout_oz=206    ! ozone
    iout_rad=207   ! radiance (brightness temperature)
    iout_pcp=208   ! precipitation rate
    iout_rw=209    ! radar radial wind
    iout_dw=210    ! doppler lidar wind
    iout_srw=211   ! radar superob wind
    iout_gps=212   ! gps refractivity or bending angle
    iout_sst=213   ! conventional sst

    mype_ps = npe-1          ! surface pressure
    mype_uv = max(0,npe-2)   ! u,v wind components
    mype_t  = max(0,npe-3)   ! virtual temperature
    mype_q  = max(0,npe-4)   ! moisture (specific humidity)
    mype_pw = max(0,npe-5)   ! total column water
    mype_rw = max(0,npe-6)   ! radar radial wind
    mype_dw = max(0,npe-7)   ! doppler lidar wind
    mype_srw= max(0,npe-8)   ! radar superob wind
    mype_gps= max(0,npe-9)   ! gps refractivity or bending angle
    mype_sst= max(0,npe-10)  ! conventional sst
    
!   Initialize arrays used in namelist obs_input 
    ndat = ndatmax          ! number of observation types (files)
    time_window_max = three ! set maximum time window to +/-three hours
    do i=1,ndat
       dfile(i) = ' '     ! local file name from which to read observatinal data
       dtype(i) = ' '     ! character string identifying type of observation
       ditype(i)= ' '     ! character string identifying group type of ob
       dplat(i) = ' '     ! currently contains satellite id (no meaning for non-sat data)
       dsis(i)  = ' '     ! sensor/instrument/satellite identifier for info files
       ipoint(i)= izero   ! default pointer (values set in gsisub)
       dthin(i) = ione    ! thinning flag (1=thinning on; otherwise off)
       time_window(i) = time_window_max ! default to maximum time window
       write(obsfile_all(i),100) i      ! name of scratch file to hold obs data
    end do
100 format('obs_input.',i4.4)


!   Other initializations
    nloz = 12                  ! number of "levels" in ozone data
    lunobs_obs = 2             ! unit to which to write/read information
!                                related to brightness temperature and 
!                                precipitation rate observations

    obs_sen = .false.          ! .t. turns on TLM; default no TLM
    grids_dim= 161             ! grid points for integration of GPS bend

    nprof_gps = 0

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

    integer(i_kind),intent(in):: mype
    character(len=8):: pe_name
    character(len=144):: command

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
!$$$
    implicit none
    allocate (nsat1(ndat),mype_diaghdr(ndat))
    return
  end subroutine create_obsmod_vars
  
  subroutine destroyobs
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
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none
    integer(i_kind):: istatus,i

    ttail => thead
    do while (associated(ttail))
      thead => ttail%llpoint
      deallocate(ttail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for t, istatus=',istatus
      ttail => thead
    end do

    pwtail => pwhead
    do while (associated(pwtail))
      pwhead => pwtail%llpoint
      deallocate(pwtail%dp,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pw arrays, istatus=',istatus
      deallocate(pwtail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pw, istatus=',istatus
      pwtail => pwhead
    end do

    pstail => pshead
    do while (associated(pstail))
      pshead => pstail%llpoint
      deallocate(pstail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for ps, istatus=',istatus
      pstail => pshead
    end do

    wtail => whead
    do while (associated(wtail))
      whead => wtail%llpoint
      deallocate(wtail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for w, istatus=',istatus
      wtail => whead
    end do

    qtail => qhead
    do while (associated(qtail))
      qhead => qtail%llpoint
      deallocate(qtail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for q, istatus=',istatus
      qtail => qhead
    end do

    spdtail => spdhead
    do while (associated(spdtail))
      spdhead => spdtail%llpoint
      deallocate(spdtail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for spd, istatus=',istatus
      spdtail => spdhead
    end do

    srwtail => srwhead
    do while (associated(srwtail))
      srwhead => srwtail%llpoint
      deallocate(srwtail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for srw, istatus=',istatus
      srwtail => srwhead
    end do

    rwtail => rwhead
    do while (associated(rwtail))
      rwhead => rwtail%llpoint
      deallocate(rwtail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for rw, istatus=',istatus
      rwtail => rwhead
    end do

    dwtail => dwhead
    do while (associated(dwtail))
      dwhead => dwtail%llpoint
      deallocate(dwtail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for dw, istatus=',istatus
      dwtail => dwhead
    end do

    ssttail => ssthead
    do while (associated(ssttail))
      ssthead => ssttail%llpoint
      deallocate(ssttail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for sst, istatus=',istatus
      ssttail => ssthead
    end do

    oztail => ozhead
    do while (associated(oztail))
      ozhead => oztail%llpoint
      deallocate(oztail%res,oztail%err2,oztail%raterr2,oztail%prs, &
                 oztail%ipos,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for oz arrays, istatus=',istatus
      deallocate(oztail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for oz, istatus=',istatus
      oztail => ozhead
    end do

    ozotail => ozohead
    do while (associated(ozotail))
      ozohead => ozotail%llpoint
      deallocate(ozotail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for ozo, istatus=',istatus
      ozotail => ozohead
    end do

    radtail => radhead
    do while (associated(radtail))
      radhead => radtail%llpoint
      deallocate(radtail%res,radtail%err2, &
                 radtail%raterr2,radtail%pred1, &
                 radtail%pred2,radtail%dtb_dvar, &
                 radtail%icx,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for rad arrays, istatus=',istatus
      deallocate(radtail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for rad, istatus=',istatus
      radtail => radhead
    end do

    pcptail => pcphead
    do while (associated(pcptail))
      pcphead => pcptail%llpoint
      deallocate(pcptail%predp,pcptail%dpcp_dvar,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pcp arrays, istatus=',istatus
      deallocate(pcptail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pcp, istatus=',istatus
      pcptail => pcphead
    end do

    gpstail => gpshead
    do while (associated(gpstail))
      gpshead => gpstail%llpoint
      deallocate(gpstail%termtlin,gpstail%b_qin,gpstail%b_pin,gpstail%b_tin, &
                 gpstail%b_rges,gpstail%b_gp2gm,gpstail%b_n,gpstail%b_pkges,gpstail%b_tkges,&
                 gpstail%b_loc,gpstail%b_xj,gpstail%ij,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for gps arrays, istatus=',istatus
      deallocate(gpstail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for gps, istatus=',istatus
      gpstail => gpshead
    end do
    
    return
  end subroutine destroyobs
  
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
!$$$
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
!$$$

    use kinds, only: r_kind,i_kind
    use constants, only: zero
    implicit none

    integer(i_kind):: j
    real(r_kind),dimension(10):: a

    call random_number(a)
    ran01dom=zero
    do j=1,10
       ran01dom=ran01dom+a(j)
    enddo
    ran01dom=(ran01dom-5.0_r_kind)/0.912345_r_kind
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
!$$$
    implicit none
    integer(i_kind):: istatus

    gps_alltail => gps_allhead
    do while (associated(gps_alltail))
      gps_allhead => gps_alltail%llpoint
      deallocate(gps_alltail,stat=istatus)
      if (istatus/=0) write(6,*)'DESTROY_GENSTATS_GPS:  deallocate error for gps_all, istatus=',istatus
      gps_alltail => gps_allhead
    end do
    
    return
  end subroutine destroy_genstats_gps

end module obsmod


