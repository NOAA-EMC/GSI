subroutine glbsoi(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    glbsoi               driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   1994-02-11  parrish
!   1998-05-15  weiyu yang       mpp version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1993-12-22  kleist,d., treadon, r.,derber, j.  modules, updates and comments
!   2004-06-21  treadon - update documentation
!   2004-07-08  treadon - fix vertical indexing bug in call set_ozone_var
!   2004-07-24  treadon - add only to module use, add intent in/out
!   2004-11-22  parrish - add code to handle regional netcdf i/o
!   2004-12-13  treadon - limit runtime output from CRTM & IRSSE initilizations
!   2004-12-22  treadon - add optional code to compute/write out innovation
!                         information following all outer loops
!   2005-01-22  parrish - add balmod, compact_diffs
!   2005-02-23  wu      - setup norm rh
!   2005-03-01  parrish - add parts of regional anisotropic background option 
!                         (not complete yet)
!   2005-04-14  yanqiu zhu - support for observation sensitivity study
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!   2005-05-27  kleist  - add call to destroy grids from patch interpolation
!   2005-06-08  treadon - add switch_on_derivatives for create/destroy_ges_bias_grids
!   2005-06-13  li/treadon - move destroy_sfc_grids after write_all
!   2005-07-12  kleist  - add Jc term
!   2005-08-16  guo - add gmao surface interface
!   2005-09-28  parrish - add logic to only allow NCEP global or WRF_NMM to use jc routines
!   2005-09-29  derber  - simplify observation file handling
!   2005-11-21  kleist - use tendency module, add call to get moisture diagnostics
!   2005-11-28  derber - move read_obs to glbsoi
!   2005-11-29  derber - move read guess and background error calculations outside 
!                        external iteration
!   2006-01-09  derber - absorb set_nrh_var into compute_derived
!   2006-01-10  treadon - move read*files into gesinfo, consolidate read*guess calls
!   2006-01-12  treadon - replace pCRTM with CRTM
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-12  treadon - remove mpimod (not used)
!   2006-04-21  kleist  - remove call to setupjc, no longer exists
!   2006-07-28  derber  - remove creation of obs inner loop data file name
!   2006-08-15  parrish - add call to create_vtrans (get vertical modes if nvmodes_keep > 0)
!   2007-06-08  kleist/treadon - add prefix (task id or path) to obs_setup
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: rearth
  use crtm_module, only: crtm_destroy,crtm_init,crtm_channelinfo_type
  use error_handler, only: success
  use jfunc, only: miter,jiter,jiterstart,iguess,biascor,&
       set_pointer,create_jfunc,read_guess_solution,destroy_jfunc,&
       switch_on_derivatives,tendsflag
  use jfunc_tl, only: create_jfunc_tl,destroy_jfunc_tl
  use anberror, only: anisotropic,create_anberror_vars,destroy_anberror_vars,&
       destroy_anberror_vars_reg
  use anisofilter, only: anprewgt_reg
  use berror, only: create_berror_vars_reg,create_berror_vars,&
       set_predictors_var,destroy_berror_vars_reg,&
       destroy_berror_vars
  use balmod, only: create_balance_vars_reg,create_balance_vars, &
       destroy_balance_vars_reg,destroy_balance_vars,prebal,prebal_reg
  use compact_diffs, only: create_cdiff_coefs,inisph,destroy_cdiff_coefs
  use gridmod, only: nlat,nlon,rlats,regional,wrf_mass_regional,&
       wrf_nmm_regional,twodvar_regional,wgtlats,netcdf
  use guess_grids, only: sfct,create_ges_bias_grids,create_sfc_grids,&
       destroy_sfc_grids,destroy_ges_bias_grids
  use tendsmod, only: create_tendvars,destroy_tendvars
  use obsmod, only: write_diag,obs_sen,obs_setup,ndat,dirname
  use smooth_polcarf, only: norsp,destroy_smooth_polcas
  use jcmod, only: jcterm,create_jcvars,destroy_jcvars,get_jcwts,jcdivt 
  use pcgsoimod
  use gridmod    , only: gmao_intfc
  use m_fvAnaGrid, only: fvAnaGrid_allgetlist
  use convinfo, only: convinfo_destroy
  use mod_inmi, only: jcstrong
  use mod_vtrans, only: nvmodes_keep,create_vtrans,destroy_vtrans

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  logical tlm_check,adj_check

  integer(i_kind) error_status,i
!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
  integer(i_kind),dimension(ndat,3):: ndata
  type(crtm_channelinfo_type):: channelinfo


!*******************************************************************************************
!
! Allocate arrays to hold surface and sigma guess fields.
  call create_ges_bias_grids(switch_on_derivatives,tendsflag,biascor)
  call create_sfc_grids

! Read model guess fields.
  call read_guess(mype)


! Set length of control vector and other control vector constants
  call set_pointer

! Allocate arrays used in minimization
  call create_jfunc
  if(obs_sen) call create_jfunc_tl
  if (tendsflag) call create_tendvars  
  if ( (jcstrong .or. jcdivt) .and. nvmodes_keep.gt.0) call create_vtrans(mype)

! If requested and if available, read guess solution.
  if (iguess==1 .or. iguess==2) call read_guess_solution(mype)

! Initialize CRTM.  Load satellite sensor array.
! The optional arguments Process_ID and Output_Process_ID limit
! generation of runtime informative output to mpi task
! Output_Process_ID (which here is set to be task 0)

  if (.not.twodvar_regional) then
     error_status = crtm_init(channelinfo, Process_ID=mype,Output_Process_ID=0)
     if (error_status /= success) &
          write(6,*)'GLBSOI:  ***ERROR*** crtm_init error_status=',error_status
  endif

! Create file names for pe relative observation data.  obs_setup files are used
! in outer loop setup routines. 
  obs_setup = trim(dirname) // 'obs_setup'


! Read observations
  call read_obs(ndata,mype)
  

! Locate observations within each subdomain.
  call obs_para(ndata,mype)


! Create/setup background error and background error balance
  if (regional)then
     call create_balance_vars_reg(mype)
     if(.not.anisotropic) call create_berror_vars_reg(mype)
     call prebal_reg(mype)
     if(anisotropic) then
        call anprewgt_reg(mype)
     else
        call prewgt_reg(mype)
     end if
  else
     call create_balance_vars
     if(anisotropic) then
        call create_anberror_vars(mype)
     else
        call create_berror_vars
     end if
     
!    Generate coefficients for compact differencing
     call create_cdiff_coefs
     call inisph(rearth,rlats(2),wgtlats(2),nlon,nlat-2)

     call prebal(mype)

!    Load background error arrays used by recursive filters
     if(anisotropic) then
   !     NOT AVAILABLE YET
   !   call anprewgt(mype)
     else
        call prewgt(mype)
     end if
  end if

! Set error (variance) for predictors (only use guess)
  call set_predictors_var

! Set errors and create variables for dynamical constraint
  if (jcterm) then
     call create_jcvars
     call get_jcwts(mype)
  end if


! Main outer analysis loop
  do jiter=jiterstart,miter

!    Set up right hand side of analysis equation
     call setuprhsall(ndata,channelinfo,mype)

!    Call inner minimization loop
     if (.not. obs_sen) then

!       Standard run, not exercising TLMC
        if (mype==0) write(6,*)'GLBSOI:  START pcgsoi'
        call pcgsoi(mype)

     else

!       Tangent linear of inner minimization
        call init_var_tl(mype,tlm_check,adj_check)
        if (mype==0) write(6,*)'GLBSOI:  START pcgsoi_tl'
        call pcgsoi_tl(mype, tlm_check)

     endif

! End of outer iteration loop
  end do


! If requested, write obs-anl information to output files
  jiter=miter+1
  if (write_diag(jiter)) call setuprhsall(ndata,channelinfo,mype)
       
! Deallocate arrays
  call convinfo_destroy
  if (regional) then
     if(anisotropic) then
        call destroy_anberror_vars_reg
     else
        call destroy_berror_vars_reg
     end if
     call destroy_balance_vars_reg
  else
     if(anisotropic) then
        call destroy_anberror_vars
     else
        call destroy_berror_vars
     end if
     call destroy_balance_vars
     call destroy_cdiff_coefs
     if (norsp > 0) call destroy_smooth_polcas
  endif
  if (jcterm) call destroy_jcvars
  if (tendsflag) call destroy_tendvars
  if ( (jcstrong .or. jcdivt) .and. nvmodes_keep.gt.0) call destroy_vtrans


! Write output files
  call write_all(mype)


! Deallocate remaining arrays
  call destroy_sfc_grids
  call destroy_ges_bias_grids(switch_on_derivatives,tendsflag,biascor)
  call destroy_jfunc
  if(obs_sen) call destroy_jfunc_tl
  if (.not.twodvar_regional) then
     error_status = crtm_destroy(channelinfo)
     if (error_status /= success) &
          write(6,*)'GLBSOI:  ***ERROR*** crtm_destroy error_status=',error_status
  endif


! End of routine
end subroutine glbsoi
