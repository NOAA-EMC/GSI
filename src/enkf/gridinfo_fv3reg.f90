module gridinfo
!$$$  module documentation block
!
! module: gridinfo                     read horizontal (lons, lats) and
!                                      vertical (pressure) information from
!                                      ensemble mean first guess file.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: This module reads the ensemble mean background file and
! extracts information about the analysis grid, including the
! longitudes and latitudes of the analysis grid points and
! the pressure on each grid point/vertical level.
!
! Public Subroutines:
!   getgridinfo: read latitudes, longitudes, pressures and orography for analysis grid,
!    broadcast to each task. Compute spherical cartesian coordinate values
!    for each analysis horizontal grid point.
!   gridinfo_cleanup: deallocate allocated module variables.
!
! Public Variables:
!   npts: number of analysis grid points in the horizontal (from module params).
!   nlevs: number of analysis vertical levels (from module params).
!    specific humidity, ozone and cloud condensate).
!   ptop: (real scalar) pressure (hPa) at top model layer interface.
!   lonsgrd(npts): real array of analysis grid longitudes (radians).
!   latsgrd(npts): real array of analysis grid latitudes (radians).
!   logp(npts,ndim):  -log(press) for all 2d analysis grids. Assumed invariant
!   in assimilation window, computed fro ensemble mean at middle of window.
!   gridloc(3,npts): spherical cartesian coordinates (x,y,z) for analysis grid.
!
! Modules Used: mpisetup, params, kinds
!
! program history log:
!   2009-02-23  Initial version.
!   2016-05-02: shlyaeva: Modification for reading state vector from table
!   2016-04-20  Modify to handle the updated nemsio sig file (P, DP & DPDT removed)
!   2021-02-08  CAPS(J. Park) - Modified 'vars3d_supported' for direct reflectivity DA capability
!
! attributes:
!   language: f95
!
!$$$

use mpi, only: mpi_real4,mpi_comm_world
use mpisetup, only: nproc
use params, only: datapath,nlevs,nlons,nlats,use_gfs_nemsio, fgfileprefixes, &
                  fv3fixpath, nx_res,ny_res, ntiles,l_fv3reg_filecombined
use kinds, only: r_kind, i_kind, r_double, r_single
use constants, only: one,zero,pi,cp,rd,grav,rearth,max_varname_length
use constants, only: half
use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
use netcdf, only: nf90_inq_dimid,nf90_inq_varid
use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
use netcdf_mod, only: nc_check
use read_fv3regional_restarts,only:read_fv3_restart_data1d,read_fv3_restart_data2d
use read_fv3regional_restarts,only:read_fv3_restart_data3d,read_fv3_restart_data4d

implicit none
private
public :: getgridinfo, gridinfo_cleanup
public :: ak,bk,eta1_ll,eta2_ll
real(r_single),public :: ptop
real(r_single),public, allocatable, dimension(:) :: lonsgrd, latsgrd
! arrays passed to kdtree2 routines must be single
real(r_single),public, allocatable, dimension(:,:) :: gridloc
real(r_single),public, allocatable, dimension(:,:) :: logp
integer(i_kind),                                  public     :: nlevs_pres
integer,public :: npts
integer,public :: ntrunc
! supported variable names in anavinfo
character(len=max_varname_length),public, dimension(15) :: &
  vars3d_supported = [character(len=max_varname_length) :: &
    'u', 'v', 'w', 't', 'q', 'oz', 'cw', 'tsen', 'prse', &
    'ql', 'qi', 'qr', 'qs', 'qg', 'qnr']
character(len=max_varname_length),public, dimension(3) :: &
  vars2d_supported = [character(len=max_varname_length) :: &
    'ps', 'pst', 'sst']
! supported variable names in anavinfo
real(r_single), allocatable, dimension(:) :: ak,bk,eta1_ll,eta2_ll
contains

subroutine getgridinfo(fileprefix, reducedgrid)
! read latitudes, longitudes and pressures for analysis grid,
! broadcast to each task.
use read_fv3regional_restarts, only: read_fv3_restart_data2d
implicit none

character(len=120), intent(in) :: fileprefix
logical, intent(in)            :: reducedgrid

integer(i_kind) ierr,  k, nn 
character(len=500) filename
integer(i_kind) i,j
real(r_kind), allocatable, dimension(:) :: spressmn
real(r_kind), allocatable, dimension(:,:) :: pressimn,presslmn
real(r_kind) kap,kapr,kap1

integer(i_kind) :: file_id,var_id,dim_id,nlevsp1,nx_tile,ny_tile,ntile
integer (i_kind):: nn_tile0
integer(i_kind) :: nlevsp1n
real(r_single), allocatable, dimension(:,:) :: lat_tile,lon_tile,ps
real(r_single), allocatable, dimension(:,:,:) :: delp,g_prsi
real(r_single) ptop
character(len=4) char_nxres
character(len=4) char_nyres
character(len=1) char_tile
character(len=24),parameter :: myname_ = 'fv3: getgridinfo'
write (6,*)"The input fileprefix, reducedgrid are not used in the current implementation", &
           fileprefix, reducedgrid
nlevsp1 = nlevs + 1
nlevs_pres = nlevsp1
npts = ntiles*nx_res*ny_res
kap = rd/cp
kapr = cp/rd
kap1 = kap + one

! read data on root task
if (nproc .eq. 0) then

   !  read ak,bk from ensmean fv_core.res.nc
   !  read nx,ny and nz from fv_core.res.nc
   filename = 'fv3sar_tile1_akbk.nc'
   call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
   myname_,'open: '//trim(adjustl(filename)) )
   call nc_check( nf90_inq_dimid(file_id,'xaxis_1',dim_id),&
       myname_,'inq_dimid xaxis_1 '//trim(filename) )
   call nc_check( nf90_inquire_dimension(file_id,dim_id,len=nlevsp1n),&
       myname_,'inquire_dimension xaxis_1 '//trim(filename) )
   if(nlevsp1n.ne.nlevsp1) then
     write(6,*)'the configure nlevsp1 is not consistent with the parameter &
                  read from the data files, stop'
     call stop2(25)
   endif



   allocate(ak(nlevsp1),bk(nlevsp1))
   allocate(eta1_ll(nlevsp1),eta2_ll(nlevsp1))
   call read_fv3_restart_data1d('ak',filename,file_id,ak)
   call read_fv3_restart_data1d('bk',filename,file_id,bk)

!!!!! change unit of ak,also reverse the 
     
    do i=1,nlevsp1
       eta1_ll(i)=ak(i)*0.01_r_kind
       eta2_ll(i)=bk(i)
    enddo




   ptop = eta1_ll(nlevsp1)
   call nc_check( nf90_close(file_id),&
   myname_,'close '//trim(filename) )
   filename = 'fv3sar_tile1_grid_spec.nc'
   call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
   myname_,'open: '//trim(adjustl(filename)) )

   call nc_check( nf90_inq_dimid(file_id,'grid_xt',dim_id),&
                 myname_,'inq_dimid grid_xt '//trim(filename) )
   call nc_check( nf90_inquire_dimension(file_id,dim_id,len=nx_tile),&
                 myname_,'inquire_dimension grid_xt '//trim(filename) )
   if(nx_res.ne.nx_tile) then
     write(6,*)"nx_tile and nx_res are ",nx_tile,nx_res
     write(6,*)'the readin nx_tile does not equal to nx_res as expected, stop'
     call stop2(25)
   endif

   call nc_check( nf90_inq_dimid(file_id,'grid_yt',dim_id),&
                 myname_,'inq_dimid grid_yt '//trim(filename) )
   call nc_check( nf90_inquire_dimension(file_id,dim_id,len=ny_tile),&
                 myname_,'inquire_dimension grid_yt '//trim(filename) )
   if(ny_res.ne.ny_tile) then
     write(6,*)'the readin ny_tile does not equal to ny_res as expected, stop'
     call stop2(25)
   endif


   
   !  read lats/lons from C###_oro_data.tile#.nc 
   ! (this requires path to FV3 fix dir)
   write(char_nxres, '(i4)') nx_res
   write(char_nyres, '(i4)') ny_res
   allocate(lat_tile(nx_res,ny_res),lon_tile(nx_res,ny_res))
   nn = 0
   allocate(latsgrd(npts),lonsgrd(npts))
   do ntile=1,ntiles
      nn_tile0=(ntile-1)*nx_res*ny_res
      write(char_tile, '(i1)') ntile
      filename='fv3sar_tile'//char_tile//'_grid_spec.nc'
      call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
      myname_,'open: '//trim(adjustl(filename)) )
      call read_fv3_restart_data2d('grid_lont',filename,file_id,lon_tile)
      !print *,'min/max lon_tile',ntile,minval(lon_tile),maxval(lon_tile)
      call read_fv3_restart_data2d('grid_latt',filename,file_id,lat_tile)
      !print *,'min/max lat_tile',ntile,minval(lat_tile),maxval(lat_tile)
      call nc_check( nf90_close(file_id),&
      myname_,'close '//trim(filename) )
      nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            latsgrd(nn) = lat_tile(i,j)
            lonsgrd(nn) = lon_tile(i,j)
         enddo
      enddo
   enddo  !loop for ntilet
   latsgrd = pi*latsgrd/180._r_single
   lonsgrd = pi*lonsgrd/180._r_single
   allocate(delp(nx_res,ny_res,nlevs),ps(nx_res,ny_res))
   allocate(g_prsi(nx_res,ny_res,nlevsp1))
   allocate(pressimn(npts,nlevsp1),presslmn(npts,nlevs))
   allocate(spressmn(npts))
   nn = 0
   do ntile=1,ntiles
      nn_tile0=(ntile-1)*nx_res*ny_res
      nn=nn_tile0
      write(char_tile, '(i1)') ntile
      if(l_fv3reg_filecombined) then
        filename = 'fv3sar_tile'//char_tile//"_ensmean_dynvartracer"
      else
        filename = 'fv3sar_tile'//char_tile//"_ensmean_dynvars"
      endif 
      !print *,trim(adjustl(filename))
      call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
      myname_,'open: '//trim(adjustl(filename)) )
      call read_fv3_restart_data3d('delp',filename,file_id,delp)
      !print *,'min/max delp',ntile,minval(delp),maxval(delp)
      call nc_check( nf90_close(file_id),&
      myname_,'close '//trim(filename) )
      g_prsi(:,:,nlevsp1)=eta1_ll(nlevsp1) !etal_ll is needed
        do i=nlevs,1,-1
        g_prsi(:,:,i)=delp(:,:,i)*0.01_r_kind+g_prsi(:,:,i+1)
       enddo

      ps = g_prsi(:,:,1)
      !print *,'min/max ps',ntile,minval(ps),maxval(ps)
      nn=nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            spressmn(nn) = ps(i,j)
         enddo
      enddo
   enddo
   ! pressure at interfaces
   do k=1,nlevsp1
      nn=nn_tile0
      do j=1,ny_res
      do i=1,nx_res  
        nn=nn+1
        pressimn(nn,k) = g_prsi(i,j,k)
      enddo
      enddo
   enddo
   do k=1,nlevs
      nn=nn_tile0
      do j=1,ny_res
      do i=1,nx_res  
        nn=nn+1
        presslmn(nn,k) = (pressimn(nn,k)+pressimn(nn,k+1)) *half
      enddo
      enddo
   end do
   print *,'ensemble mean first guess surface pressure:'
   print *,minval(spressmn),maxval(spressmn)
   ! logp holds log(pressure) or pseudo-height on grid, for each level/variable.
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   do k=1,nlevs
      ! all variables to be updated are on mid-layers, not layer interfaces.
      logp(:,k) = -log(presslmn(:,k))
      !print *,'min/max presslmn',k,minval(presslmn(:,k)),maxval(presslmn(:,k)),minval(logp(:,k)),maxval(logp(:,k))
   end do
   logp(:,nlevs_pres) = -log(spressmn(:))
   deallocate(spressmn,presslmn,pressimn)
   deallocate(ak,bk,ps)
   deallocate(g_prsi,delp)
   deallocate(lat_tile,lon_tile)
endif ! root task

   allocate(gridloc(3,npts))
if (nproc .ne. 0) then
   ! allocate arrays on other (non-root) tasks
   allocate(latsgrd(npts),lonsgrd(npts))
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   allocate(eta1_ll(nlevsp1),eta2_ll(nlevsp1))
endif
!call mpi_bcast(logp,npts*nlevs_pres,mpi_real4,0,MPI_COMM_WORLD,ierr)
do k=1,nlevs_pres
  call mpi_bcast(logp(1,k),npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
enddo
call mpi_bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(eta1_ll,nlevsp1,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(eta2_ll,nlevsp1,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(ptop,1,mpi_real4,0,MPI_COMM_WORLD,ierr)
  
!==> precompute cartesian coords of analysis grid points.
do nn=1,npts
   gridloc(1,nn) = cos(latsgrd(nn))*cos(lonsgrd(nn))
   gridloc(2,nn) = cos(latsgrd(nn))*sin(lonsgrd(nn))
   gridloc(3,nn) = sin(latsgrd(nn))
end do
end subroutine getgridinfo

subroutine gridinfo_cleanup()
if (allocated(lonsgrd)) deallocate(lonsgrd)
if (allocated(latsgrd)) deallocate(latsgrd)
if (allocated(logp)) deallocate(logp)
if (allocated(gridloc)) deallocate(gridloc)
end subroutine gridinfo_cleanup

end module gridinfo
