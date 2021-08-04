module statevec_efsoi
!$$$  module documentation block
!
! module: statevec_efsoi       read ensemble members, ensemble mean forecasts,
!                              ensemble mean analysis and verification. 
!                              Assign and compute forecast perturbations
!                              and ensemble mean forecast errors from 
!                              information read
!                              
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
! prgmmr: groff            org: emc                    date: 2018-05-14
!
! abstract: io needed for efsoi calculations
!
! Public Subroutines:
!  init_statevec_efsoi: read anavinfo table for defining EFSOI state vector
!  read_state_efsoi:    read ensemble members, ensemble mean forecasts, 
!                       ensemble mean analyses and verification.  Assign 
!                       and compute forecast perturbations and forecast errors 
!                       from information read.
!  statevec_efsoi_cleanup: deallocate allocatable arrays.
!
! Public Variables:
!  nanals: (integer scalar) number of ensemble members (from module params)
!  nlevs: number of analysis vertical levels (from module params).
!
!  nc3d: number of 3D control variables
!  nc2d: number of 2D control variables
!  cvars3d: names of 3D control variables
!  cvars2d: names of 2D control variables
!  ncdim: total number of 2D fields to update (nc3d*nlevs+nc2d)
!  index_pres: an index array with pressure value for given state variable
!   
! Modules Used: mpisetup, params, kinds, gridio, gridinfo_efsoi, mpeu_util, constants
!
! program history log:
!   2009-02-23  Initial version (as statevec).
!   2009-11-28  revamped to improve IO speed
!   2015-06-29  add multiple time levels to background
!   2016-05-02  shlyaeva: Modification for reading state vector from table
!   2016-09-07  shlyaeva: moved distribution of ens members to loadbal 
!   2016-11-29  shlyaeva: module renamed to controlvec (from statevec); gridinfo_efsoi
!               init and cleanup are called from here now
!   2018-05-14  Groff: Adapted from enkf controlvec.f90 to provide
!               io functionality necessary for efsoi calculations
!   2021-03-04  Eichmann: updated to work with FV3 GFS
!   2021-03-15  Eichmann: eliminated gridinfo_efsoi for src/enkf version 
!
! attributes:
!   language: f95
!
!$$$

use mpisetup, only: numproc,nproc,mpi_wtime
use mpimod, only: mpi_comm_world
use gridio_efsoi,    only: readgriddata_efsoi, get_weight, ncdim
use gridinfo,  only: getgridinfo, gridinfo_cleanup,              &
                     npts, vars3d_supported, vars2d_supported
use params,    only: nlevs, fgfileprefixes, reducedgrid, &
                     nanals, nlons, nlats, read_member_forecasts, &
                     read_verification, read_ensmean_forecast, &
                     read_analysis_mean, eft, andataname, &
                     datehr, forecast_impact, gdatehr
use kinds,     only: r_kind, i_kind, r_double, r_single
use mpeu_util, only: gettablesize, gettable, getindex
use constants, only: max_varname_length
implicit none

private

public :: read_state_efsoi, statevec_cleanup_efsoi, init_statevec_efsoi
real(r_single), public, allocatable, dimension(:,:) :: grdin, grdin2, grdin3, grdin4, grdin5

integer(i_kind), public :: nc2d, nc3d
character(len=max_varname_length), allocatable, dimension(:), public :: cvars3d
character(len=max_varname_length), allocatable, dimension(:), public :: cvars2d
integer(i_kind), public, allocatable, dimension(:) :: index_pres
integer(i_kind), public, allocatable, dimension(:) :: clevels
integer(i_kind),public, allocatable, dimension(:) :: id_u, id_v, id_t, id_q
integer(i_kind),public :: id_ps
contains

subroutine init_statevec_efsoi()
! read table with state vector variables for efsoi
! (code adapted from GSI state_vectors.f90 init_anasv routine
implicit none
character(len=*),parameter:: rcname='anavinfo'
character(len=*),parameter:: tbname='state_vector_efsoi::'
character(len=256),allocatable,dimension(:):: utable
character(len=20) var,source,funcof
integer(i_kind) luin,ii,i,ntot, k, nvars
integer(i_kind) ilev, itracer
integer(i_kind) u_ind,v_ind,tv_ind,q_ind,ps_ind

! load file
luin=914
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nvars)

! Get contents of table
allocate(utable(nvars))
call gettable(tbname,luin,ntot,nvars,utable)

! release file unit
close(luin)

! Retrieve each token of interest from table and define
! variables participating in control vector

! Count variables first
nc2d=0; nc3d=0; ncdim=0;
do ii=1,nvars
   read(utable(ii),*) var, ilev, itracer, source, funcof
   if(ilev==1) then
       nc2d=nc2d+1
       ncdim=ncdim+1
   else
       nc3d=nc3d+1
       ncdim=ncdim+ilev
   endif
enddo

allocate(cvars3d(nc3d),cvars2d(nc2d),clevels(0:nc3d))

! Now load information from table
nc2d=0;nc3d=0
clevels = 0
do ii=1,nvars
   read(utable(ii),*) var, ilev, itracer, source, funcof
   if(ilev==1) then
      nc2d=nc2d+1
      cvars2d(nc2d)=trim(adjustl(var))
   else if (ilev==nlevs .or. ilev==nlevs+1) then
      nc3d=nc3d+1
      cvars3d(nc3d) = trim(adjustl(var))
      clevels(nc3d) = ilev + clevels(nc3d-1)
   else
      if (nproc .eq. 0) print *,'Error: only ', nlevs, ' and ', nlevs+1,' number of levels is supported in current version, got ',ilev
      call stop2(503)
   endif
enddo

deallocate(utable)

allocate(index_pres(ncdim))
ii=0
do i=1,nc3d
  do k=1,clevels(i)-clevels(i-1)
    ii = ii + 1
    index_pres(ii)=k
  end do
end do
do i = 1,nc2d
  ii = ii + 1
  index_pres(ii) = nlevs+1
enddo

! sanity checks
if (ncdim == 0) then
  if (nproc == 0) print *, 'Error: there are no variables to update.'
  call stop2(501)
endif

do i = 1, nc2d
  if (getindex(vars2d_supported, cvars2d(i))<0) then
    if (nproc .eq. 0) then
      print *,'Error: 2D variable ', cvars2d(i), ' is not supported in current version.'
      print *,'Supported variables: ', vars2d_supported
    endif
    call stop2(502)
  endif
enddo
do i = 1, nc3d
  if (getindex(vars3d_supported, cvars3d(i))<0) then
    if (nproc .eq. 0) then 
       print *,'Error: 3D variable ', cvars3d(i), ' is not supported in current version.'
       print *,'Supported variables: ', vars3d_supported
    endif
    call stop2(502)
  endif
enddo

if (nproc == 0) then 
  print *, '2D control variables: ', cvars2d
  print *, '3D control variables: ', cvars3d
  print *, 'Control levels: ', clevels
  print *, 'nc2d: ', nc2d, ', nc3d: ', nc3d, ', ncdim: ', ncdim
endif


call getgridinfo(fgfileprefixes(1), reducedgrid)


! Identify EFSOI relevant state variable indices
u_ind   = getindex(vars3d_supported, 'u')   !< indices in the state var arrays
v_ind   = getindex(vars3d_supported, 'v')   ! U and V (3D)
tv_ind  = getindex(vars3d_supported, 'tv')  ! Tv (3D)
q_ind   = getindex(vars3d_supported, 'q')   ! Q (3D)
ps_ind  = getindex(vars2d_supported, 'ps')  ! Ps (2D)

! Index of each elements
allocate(id_u(nlevs))
allocate(id_v(nlevs))
allocate(id_t(nlevs))
allocate(id_q(nlevs))
do k=1,nlevs
   id_u(k) = clevels(u_ind-1) + k
   id_v(k) = clevels(v_ind-1) + k
   id_t(k) = clevels(tv_ind-1) + k
   id_q(k) = clevels(q_ind-1) + k
end do

id_ps = clevels(nc3d) + ps_ind

! Get grid weights for EFSOI
! calculation and evaluation
call get_weight()

end subroutine init_statevec_efsoi

! ====================================================================
subroutine read_state_efsoi()
! read ensemble members on IO tasks
implicit none
real(r_double)  :: t1,t2
integer(i_kind) :: nanal
integer(i_kind) :: ierr

if (numproc < nanals) then
   print *,'need at least nanals =',nanals,'MPI tasks,'
   print *,'have numproc=',numproc,', exiting ...'
   call mpi_barrier(mpi_comm_world,ierr)
   call mpi_finalize(ierr)
end if

if (npts < numproc) then
   print *,'cannot allocate more than npts =',npts,'MPI tasks, exiting ...'
   call mpi_barrier(mpi_comm_world,ierr)
   call mpi_finalize(ierr)
end if

! read in whole control vector on i/o procs - keep in memory
! (needed in write_ensemble)
if (nproc <= nanals-1) then
	
   allocate(grdin(npts,ncdim))
   allocate(grdin2(npts,ncdim))
   allocate(grdin3(npts,ncdim))
   allocate(grdin4(npts,ncdim))
   allocate(grdin5(npts,ncdim))
   
   nanal = nproc + 1
   t1 = mpi_wtime()
   
   
   ! Read ensemble member forecasts needed to obtain
   ! the forecast perturbations at evaluation forecast time (EFT)
   call readgriddata_efsoi(cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin,read_member_forecasts,nanal=nanal,ft=eft,hr=datehr) 


   if (nproc == 0) then
      t2 = mpi_wtime()
      print *,'time in readgridata on root',t2-t1,'secs'
   end if
   
   if (nproc==0) then
      ! Read ensemble mean forecast from analysis
      if(forecast_impact) call readgriddata_efsoi(cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin2, &
                                  read_ensmean_forecast,nanal=0,ft=eft,hr=datehr)
								  
      ! Read ensemble mean forecast from first guess
      call readgriddata_efsoi(cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin3,  &
              read_ensmean_forecast,nanal=0,ft=eft+6,hr=gdatehr)
			  
      ! Compute One half the sum of ensemble mean forecast quantities
      grdin3 = 0.5_r_kind * (grdin3 + grdin2)
	  
      ! Verification at evaluation time
      call readgriddata_efsoi(cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin4, &
              read_verification,infilename=andataname)
			  
      ! [(0.5*(e_f + e_g)) / (nanals - 1)]
      grdin3 = (grdin3 - grdin4) / real(nanals-1,r_kind)
      ! Normalize for surface pressure ----- (This needs to be corrected) -----
      ! AFE trying making this like EXP-efso_fv3-CWB (which has the line
      ! ommented - note reads: Delete the normalization of surface pressure;
      ! The calculation of (e_t|0 + e_t|-6) do not need normalization though
      ! dimensional analysis.
      !grdin3(:,ncdim) = grdin3(:,ncdim) / grdin4(:,3)
      ! Read ensemble mean analysis, used in evolving localization     
      if(forecast_impact) call readgriddata_efsoi(cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin5, &
                                  read_analysis_mean,nanal=0,ft=0,hr=datehr)
   end if
end if
   
end subroutine read_state_efsoi

subroutine statevec_cleanup_efsoi()
! deallocate module-level allocatable arrays.
if (allocated(cvars3d)) deallocate(cvars3d)
if (allocated(cvars2d)) deallocate(cvars2d)
if (allocated(clevels)) deallocate(clevels)
if (allocated(index_pres)) deallocate(index_pres)
if (nproc <= nanals-1 .and. allocated(grdin)) deallocate(grdin)
if (nproc <= nanals-1 .and. allocated(grdin2)) deallocate(grdin2)
if (nproc <= nanals-1 .and. allocated(grdin3)) deallocate(grdin3)
if (nproc <= nanals-1 .and. allocated(grdin4)) deallocate(grdin4)
if (nproc <= nanals-1 .and. allocated(grdin5)) deallocate(grdin5)
call gridinfo_cleanup()
if (allocated(id_u)) deallocate(id_u)
if (allocated(id_v)) deallocate(id_v)
if (allocated(id_t)) deallocate(id_t)
if (allocated(id_q)) deallocate(id_q)
end subroutine statevec_cleanup_efsoi

end module statevec_efsoi
