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
!   2006-12-04  todling - split bias and guess init/final (rename ges_bias routines)
!   2006-12-15  todling - no need to protect destroy call to tendvars
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2007-03-15  su      - to delocate the converr arrays
!   2007-05-08  kleist  - add preliminary verions of mp_compact_diffs module
!   2007-06-08  kleist/treadon - add prefix (task id or path) to obs_setup
!   2007-06-21  rancic - add pbl code
!   2007-06-27  tremolet- observation sensitivity
!   2007-06-29  jung    - update CRTM interface
!   2007-07-05  todling - skip calculating o-a when running in 4d-var mode
!   2007-08-27  tremolet- changed outer loop control for 4dvar
!   2007-09-30  todling - add timer
!   2007-10-25  todling - obsdiag files now written by observer
!   2007-11-12  todling - write sat bias moved from write_all here (ESMF-interface support)
!   2008-01-04  tremolet- outer loop for sensitivity computations
!   2008-11-03  sato    - enable use of global anisotropic mode
!   2008-12-02  todling - remove references to pcgsoi_tl and old obs_sen
!   2009-01-28  todling - move write_all to pcgsoi (for consistency w/ 4dvar branch of code)
!                       - use observer to avoid redundant code
!   2009-08-31  parrish - add call to fmg_initialize_e when jcstrong_option=4.  Initializes
!                          alternative regional tangent linear normal mode constraint which
!                          allows for variation of coriolis parameter and map factor.
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
  use mpimod, only: npe
  use jfunc, only: miter,jiter,jiterstart,jiterend,iguess,biascor,&
       set_pointer,create_jfunc,write_guess_solution,&
       tendsflag,xhatsave
  use anberror, only: anisotropic, ancovmdl, &
       create_anberror_vars_reg,destroy_anberror_vars_reg,&
       create_anberror_vars,destroy_anberror_vars
  use anisofilter, only: anprewgt_reg
  use anisofilter_glb, only: anprewgt
  use berror, only: create_berror_vars_reg,create_berror_vars,&
       set_predictors_var,destroy_berror_vars_reg,&
       destroy_berror_vars,bkgv_flowdep
  use balmod, only: create_balance_vars_reg,create_balance_vars, &
       destroy_balance_vars_reg,destroy_balance_vars,prebal,prebal_reg
  use compact_diffs, only: create_cdiff_coefs,inisph,destroy_cdiff_coefs
  use gridmod, only: nlat,nlon,nsig,rlats,regional,wrf_mass_regional,&
       wrf_nmm_regional,twodvar_regional,wgtlats,netcdf
  use guess_grids, only: sfct,create_ges_grids,create_sfc_grids,&
       nfldsig,ges_q
  use tendsmod, only: create_tendvars,destroy_tendvars
  use obsmod, only: write_diag,obs_setup,ndat,perturb_obs,dirname
  use turblmod, only: create_turblvars,destroy_turblvars
  use obs_sensitivity, only: lobsensfc, iobsconv, lsensrecompute, &
                             init_fc_sens, save_fc_sens
  use smooth_polcarf, only: norsp,destroy_smooth_polcas
  use jcmod, only: ljcdfi
  use gsi_4dvar, only: l4dvar, lsqrtb, lanczosave
  use pcgsoimod, only: pcgsoi
  use m_gsiBiases, only: create_bias_grids
  use control_vectors
  use radinfo, only: radinfo_write
  use pcpinfo, only: pcpinfo_write
  use converr, only: converr_destroy
  use mod_strong, only: jcstrong,jcstrong_option
  use mod_vtrans, only: nvmodes_keep,create_vtrans,destroy_vtrans
  use zrnmi_mod, only: zrnmi_initialize
  use strong_slow_global_mod, only: init_strongvars_1
  use strong_fast_global_mod, only: init_strongvars_2
  use mp_compact_diffs_mod1, only: init_mp_compact_diffs1,destroy_mp_compact_diffs1
  use observermod, only: observer_init,observer_set,observer_finalize,ndata
  use timermod, only: timer_ini, timer_fnl

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  logical slow_pole_in

  integer(i_kind) error_status,i,nlev_mp,jiterlast,mlat
  real(r_kind) :: zgg
  character(len=12) :: clfile

!*******************************************************************************************
!
! Initialize timer for this procedure
  call timer_ini('glbsoi')

! Initialize observer
  call observer_init

! Check GSI options against available number of guess time levels
  if (nfldsig == 1) then
     if (bkgv_flowdep) then
        bkgv_flowdep = .false.
        if (mype==0) &
             write(6,*)'GLBSOI: ***WARNING*** reset bkgv_flowdep=',bkgv_flowdep,&
             ', because only ',nfldsig,' guess time level available'
     endif
  endif

! Read observations and scatter
  call observer_set

! Create/setup background error and background error balance
  if (regional)then
     call create_balance_vars_reg(mype)
     if(anisotropic) then
       call create_anberror_vars_reg(mype)
     else
       call create_berror_vars_reg
     end if
     call prebal_reg(mlat)
     if(anisotropic) then
        call anprewgt_reg(mype)
     else
        call prewgt_reg
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

     slow_pole_in=.false.
     nlev_mp=nsig+1
     call init_mp_compact_diffs1(nlev_mp,mype,slow_pole_in)

     call prebal(mlat)

!    Load background error arrays used by recursive filters
     if(anisotropic) then
        call anprewgt(mype)
     else
        call prewgt(mype)
     end if
  end if

! Read output from previous min.
  if (l4dvar.and.jiterstart>1) then
    clfile='xhatsave.ZZZ'
    write(clfile(10:12),'(I3.3)') jiterstart-1
    call read_cv(xhatsave,clfile)
    zgg=dot_product(xhatsave,xhatsave)
    if (mype==0) write(6,*)'Norm xhatsave=',sqrt(zgg)
  endif

! Set error (variance) for predictors (only use guess)
  call set_predictors_var

  if (tendsflag) then
     call create_tendvars
     call create_turblvars
  endif
  if ( (jcstrong) .and. nvmodes_keep.gt.0) then
     call create_vtrans(mype)
     if(jcstrong_option.eq.1) call init_strongvars_1(mype)
     if(jcstrong_option.eq.2) call init_strongvars_2(mype)
     if(jcstrong_option.eq.3) call zrnmi_initialize(mype)
     if(jcstrong_option.eq.4) call fmg_initialize_e(mype)
  end if

! Set errors and create variables for dynamical constraint
  if (ljcdfi) call init_jcdfi

  jiterlast=miter
  if (lsensrecompute) jiterlast=jiterend
  if (l4dvar) jiterlast=jiterstart

! Main outer analysis loop
  do jiter=jiterstart,jiterlast

    if (mype==0) write(6,*)'GLBSOI: jiter,jiterstart,jiterlast,jiterend=', &
                                    jiter,jiterstart,jiterlast,jiterend

!   Set up right hand side of analysis equation
    call setuprhsall(ndata,mype)

    if (jiter<=miter) then

!     Set up right hand side of adjoint of analysis equation
      if (lsensrecompute) lobsensfc=(jiter==jiterend)
      if (lobsensfc.or.iobsconv>0) call init_fc_sens

!     Call inner minimization loop
      if (lsqrtb) then
        if (mype==0) write(6,*)'GLBSOI: Using sqrt(B), jiter=',jiter
        call sqrtmin
      else
!       Standard run
        if (mype==0) write(6,*)'GLBSOI:  START pcgsoi jiter=',jiter
        call pcgsoi
      end if

!     Save information for next minimization
      if (lobsensfc) then
        clfile='obsdiags.ZZZ'
        write(clfile(10:12),'(I3.3)') 100+jiter
        call write_obsdiags(clfile)
      else
        if (l4dvar.or.lanczosave) then
          clfile='obsdiags.ZZZ'
          write(clfile(10:12),'(I3.3)') jiter
          call write_obsdiags(clfile)

          clfile='xhatsave.ZZZ'
          write(clfile(10:12),'(I3.3)') jiter
          call write_cv(xhatsave,clfile)
          zgg=dot_product(xhatsave,xhatsave)
          if (mype==0) write(6,*)'Norm xhatsave=',sqrt(zgg)
        endif
      endif

!      Save output of adjoint of analysis equation
       if (lobsensfc.or.iobsconv>0) call save_fc_sens
    endif

! End of outer iteration loop
  end do

  if (.not.l4dvar) then
    jiter=jiterlast+1
!   If requested, write obs-anl information to output files
    if (write_diag(jiter)) call setuprhsall(ndata,mype)

!   Write xhat- and yhat-save for use as a guess for the solution
    if (iguess==0 .or. iguess==1) call write_guess_solution(mype)
  endif

! Deallocate arrays
  if(perturb_obs) call converr_destroy
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
     call destroy_mp_compact_diffs1
     if (norsp > 0) call destroy_smooth_polcas
  endif
  if (tendsflag) then
     call destroy_tendvars
     call destroy_turblvars
  endif
  if ( (jcstrong ) .and. nvmodes_keep.gt.0) call destroy_vtrans

! Write updated bias correction coefficients
  if (.not.twodvar_regional) then
    if (l4dvar) then
      if(mype == 0) call radinfo_write
      if(mype == npe-1) call pcpinfo_write
    else
      if (jiter==miter+1 ) then
         if(mype == 0) call radinfo_write
         if(mype == npe-1) call pcpinfo_write
      endif
    endif
  endif

! Finallize observer
  call observer_finalize

! Finalize timer for this procedure
  call timer_fnl('glbsoi')

! End of routine
end subroutine glbsoi
