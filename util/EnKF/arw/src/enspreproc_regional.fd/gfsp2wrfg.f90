PROGRAM gfsp2wrfg
!
!$$$  main program documentation block
!                .      .    .                                       .
! main program: gfsp2wrfg
!   PRGMMR: HU               ORG: GSD                 DATE: 2014-12-18
!
! abstract: This program reads in GFS forecast spectral coefficients
!   and convert them to WRF grids
!
! program history log:
!   2014-12-18  Hu    initial code based on GSI 
!
!EOP
!-------------------------------------------------------------------------

! !USES:
  use mpimod, only: npe,mpi_comm_world,ierror,mype
  use mpeu_util, only: die
!  use initial, only: miterrr
!  use initial, only: init_namelist
  use gridmod, only: wrf_mass_regional,diagnostic_reg,regional,use_gfs_nemsio
  use gridmod, only: init_grid,init_reg_glob_ll,init_grid_vars,final_grid_vars
  use gridmod, only: grid_ratio_wrfmass
  use constants, only: init_constants,init_constants_derived
  use guess_grids, only:create_ges_grids,destroy_ges_grids,nfldsig
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,regional,nsig_soil
  use gridmod, only: jcap,nlat_regional,nlon_regional
  use control_vectors, only: cvars3d,cvars2d,nrf_var
  use control_vectors, only: init_anacv,final_anacv
  use guess_grids, only: load_prsges,ges_prsl
  use guess_grids_enspro, only: load_prsges_enspro
  use gsi_metguess_mod, only: gsi_metguess_init,gsi_metguess_final
  use state_vectors, only: init_anasv,final_anasv
  use guess_grids, only: create_metguess_grids, destroy_metguess_grids
  use hybrid_ensemble_isotropic, only: hybens_grid_setup
  use hybrid_ensemble_isotropic, only: create_ensemble,destroy_ensemble
  use hybrid_ensemble_parameters, only: grid_ratio_ens,n_ens
  use hybrid_ensemble_parameters, only: uv_hyb_ens,grid_ratio_ens
  use hybrid_ensemble_parameters, only: ntlevs_ens,ensemble_path
  use guess_grids, only: ntguessig
  use gridmod, only: wrf_mass_hybridcord
  use gsi_4dvar, only: nhr_assimilation


  implicit none
  logical :: enpert4arw,wrt_pert_sub,wrt_pert_mem
  integer :: jcap_ens
!
! Declare variables.
!
  namelist/setup/ regional,wrf_mass_regional,diagnostic_reg, &
                  switch_on_derivatives,tendsflag,nfldsig,   &
                  grid_ratio_ens,n_ens,grid_ratio_ens,grid_ratio_wrfmass,&
                  enpert4arw,wrt_pert_sub,wrt_pert_mem,wrf_mass_hybridcord,&
                  use_gfs_nemsio,jcap_ens
!
!
!
  integer :: ios,k
  character(len=80) :: myname_
  logical switch_on_derivatives,tendsflag
!EOC

!---------------------------------------------------------------------------
!  NOAA/ESRL/GSD/EMB                                                     !
!-------------------------------------------------------------------------
!BOP

!  MPI
  call MPI_INIT(ierror)
  call mpi_comm_size(mpi_comm_world,npe,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)
!
!
  myname_='program gfsp2wrfg'

  if (mype==0) call w3tagb('GFSP2WRFG',1999,0232,0055,'GSD')
!
!
!  intialization
! 
  call gsi_metguess_init
  call init_anasv
  call init_anacv
  call init_constants_derived
  call init_grid
!
! default namelist value
!
  regional=.true.
  wrf_mass_regional=.true.
  diagnostic_reg=.true.
  switch_on_derivatives=.false.
  tendsflag=.false.
  nfldsig=1
  grid_ratio_ens=1
  grid_ratio_wrfmass=1
  enpert4arw=.true.
  wrt_pert_sub=.false.
  wrt_pert_mem=.false.
  wrf_mass_hybridcord=.false.
  jcap_ens=574

!
!  read in namelist
!
  open(11,file='namelist.input')
    read(11,setup,iostat=ios)
    if(ios/=0) call die(myname_,'read(setup)',ios)
  close(11)

! Write namelist output to standard out
  if(mype==0) then
     write(6,200)
200  format(' calling gfsp2wrfg with following input parameters:',//)
     write(6,setup)
  endif
   ntguessig=1
   ntlevs_ens=1
   uv_hyb_ens=.true.
   nhr_assimilation=1
!
!  read in regional background and convert it to binary intermediate file
!
  if (mype==0) call read_netcdf_mass4ens
!
  call mpi_barrier(mpi_comm_world,ierror)
!
  call init_constants(regional)
  call init_reg_glob_ll(mype,21)
  if(mype==0) write(*,*) size(cvars3d),size(cvars2d),size(nrf_var)
  call init_grid_vars(jcap,npe,cvars3d,cvars2d,nrf_var,mype)
!
!
  call create_metguess_grids(mype,ierror)
  call create_ges_grids(switch_on_derivatives,tendsflag)
  call mpi_barrier(mpi_comm_world,ierror)
!
  call read_wrf_mass_netcdf_guess4ens(mype)
  call mpi_barrier(mpi_comm_world,ierror)
  call load_prsges
!mhu  call load_prsges_enspro
!
  call mpi_barrier(mpi_comm_world,ierror)
  call hybens_grid_setup
  call create_ensemble
!
!mhu  call read_gfs_for_regional
  call get_gefs_for_regional_enspro(enpert4arw,wrt_pert_sub,wrt_pert_mem,jcap_ens)


! release space
  call destroy_ges_grids
  call destroy_metguess_grids(mype,ierror)

  call final_grid_vars
  call final_anacv
  call final_anasv
  call gsi_metguess_final

! Done
  if (mype==0)  call w3tage('GFSP2WRFG')

  call mpi_finalize(ierror)

END PROGRAM gfsp2wrfg
