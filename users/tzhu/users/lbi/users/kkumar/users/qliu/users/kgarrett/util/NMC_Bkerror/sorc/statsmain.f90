program statsmain
!$$$  main program documentation block
!                .      .    .                                       .
! main program: statsmain
!   prgrmmr: kleist           org: np20                date: 2005-01-15
!
! program history log:
!   2005-01-xx  kleist   initial version of stats code
!   2005-04-18  kleist   use sp library calls, add ability to easily run
!                        in single or double precision, modify post module
!                        to create byte addressable files for viewing using
!                        GRADS, compute variances for pseudo-RH and normalized
!                        RH, use IO on gridded fields to reduce memory usage
!   2009-02-xx  kleist   perform complete overhaul of MPI usage to use 
!                        subdomain structure of GSI and significanlty reduce 
!                        memory requirements
!
! abstract:
!   This code computes background error statistics to be used with the
!   GSI analysis code.  The NMC method, utilizing 24/48 hour forecast 
!   pairs valid at the same time, is used to compute variance, length
!   scale, and linear balance projection estimates.
!
! input files:
!   fort.10      - listing of NCEP global model forecast files
!   berror_sst   - fixed, global sst statistics file
!
! output files:
!   gsi.berror_stats - double precision stats output (serves as input to GSI)
!   bgstats_sp.grd   - single precision byte-addressable file for latidude 
!                      dependent variables
!   sststats_sp.grd  - single precision byte-addressable file SST statistics
!
! remarks:
!   This code is primarily a research code, and has not been well tested.  If 
!   problems are found, please contact Daryl (daryl.kleist@noaa.gov)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use variables, only: nsig,nlat,nlon,maxcases,hybrid,&
      smoothdeg,init_defaults,create_grids,destroy_grids,&
      destroy_variables,rearth,rlats,wgtlats,mype,npe,&
      create_mapping,destroy_mapping,biasrm,destroy_biasrm,&
      vertavg
  use specgrid, only: jcap,jcapin,jcapsmooth,init_spec_vars,destroy_spec_vars
  use postmod, only: writefiles
  use comm_mod, only: init_mpi_vars,destroy_mpi_vars
  implicit none
  include 'mpif.h'

  integer k,n,total,numcases,mycases,ierror

! define namelist
! NAMSTAT
!   jcapin    - spectral resolution of forecast pairs
!   jcap      - spectral resolution for spectral to grid transform(default = jcap)
!   jcapsmooth- spectral resolution for smoothing of input fields (default = min(jcap,jcapin))
!   nsig      - number of vertical levels
!   nlat      - number of latitudes
!   nlon      - number of longitudes
!   maxcases  - maximum number of forecast pairs to process
!   hybrid    - logical for hybrid vertical coordinate
!   smoothdeg - degree of horizontal smoothing to apply in latitudinal direction

  namelist/namstat/jcap,jcapin,jcapsmooth,nsig,nlat,nlon,maxcases, &
                   hybrid,smoothdeg,biasrm,vertavg

! MPI initial setup
  call mpi_init(ierror)
  call mpi_comm_size(mpi_comm_world,npe,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)

! Initialize defaults for namelist variables
  call init_defaults

! Read in namelist
  jcapin=0
  jcap=0
  jcapsmooth=99999
  open(11,file='stats.parm')
  read(11,namstat)
  if(jcapin == 0)jcapin=jcap
  if(jcap == 0)jcap=jcapin

  if(mype==0) then
    write(6,*)'starting computation of background stats with ',npe,' tasks'
    write(6,namstat)
  endif

  if(mype==0) write(6,*) 'INITIALIZE VARIABLES'
  call create_grids
  call create_mapping
  call init_spec_vars(nlat,nlon,nsig)
  call initvars(mype,npe)
  call init_mpi_vars(nsig,mype)

! need coefficients for finite differencing
  if(mype==0) write(6,*) 'GET FINITE DIFFERENCE COEFFS'
  call inisph(rearth,rlats(2),wgtlats(2),nlon,nlat-2)

! Call routine to do subdomain decomposition based on
! grid size and number of pe's
  call deter_subdomain(mype)
  call init_commvars(mype)


! Make call to see how many available files there are
  if(mype==0) write(6,*) 'COUNT NUMBER OF AVAILABLE CASES'
  call getcases(numcases,mype)

! Read in spectral coeffs and right out subdomain grids to scratch files
  call readpairs(npe,mype,numcases)
  
  if(biasrm) call biascor(numcases,mype)

! Get balance projection matrices
  call balprojs(numcases,mype)

! Compute Three-Dimensional Variances (Full variables
!!  call variances3d(numcases,mype)

! Get lat-dependend variances
  call variances(numcases,mype)

! Vertical length scales
  call vertsc(numcases,mype)

! Horizontal length scales
  call horizsc(numcases,mype)

! post Processing
  if (mype==0) then
    write(6,*) 'WRITE OUT BACKGROUND ERROR STATISTICS'
    call writefiles
  end if

! finish up, deallocate memory
  call destroy_grids
  call destroy_mapping
  call destroy_spec_vars
  call destroy_mpi_vars
  call destroy_variables
  if(biasrm) call destroy_biasrm

  if (mype==0) write(6,*) '*** STATS CODE COMPLETE! ***'
  call mpi_finalize(ierror)

end program statsmain 
