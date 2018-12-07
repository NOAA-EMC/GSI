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
!
! attributes:
!   language: f95
!
!$$$

use mpisetup, only: nproc, mpi_integer, mpi_real4, mpi_comm_world
use params, only: datapath,nlevs,nlons,nlats,use_gfs_nemsio, fgfileprefixes, &
                  fv3fixpath, res, ntiles
use kinds, only: r_kind, i_kind, r_double, r_single
use constants, only: one,zero,pi,cp,rd,grav,rearth,max_varname_length
use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
use netcdf, only: nf90_inq_dimid,nf90_inq_varid
use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
use netcdf_mod, only: nc_check

implicit none
private
public :: getgridinfo, gridinfo_cleanup
integer(i_kind),public :: nlevs_pres, idvc
real(r_single),public :: ptop
real(r_single),public, allocatable, dimension(:) :: lonsgrd, latsgrd
! arrays passed to kdtree2 routines must be single
real(r_single),public, allocatable, dimension(:,:) :: gridloc
real(r_single),public, allocatable, dimension(:,:) :: logp
integer,public :: npts
integer,public :: ntrunc
! supported variable names in anavinfo
character(len=max_varname_length),public, dimension(10) :: vars3d_supported = (/'u   ', 'v   ', 'tv  ', 'q   ', 'oz  ', 'cw  ', 'tsen', 'prse', 'ql  ', 'qi  '/)
character(len=max_varname_length),public, dimension(3)  :: vars2d_supported = (/'ps ', 'pst', 'sst' /)
! supported variable names in anavinfo
contains

subroutine getgridinfo(fileprefix, reducedgrid)
! read latitudes, longitudes and pressures for analysis grid,
! broadcast to each task.
use fv3_netcdf_mod, only: read_restart_data2d
implicit none

character(len=120), intent(in) :: fileprefix
logical, intent(in)            :: reducedgrid

integer(i_kind) nlevsin, ierr, iunit, k, nn, idvc 
character(len=500) filename
integer(i_kind) iret,i,j,nlonsin,nlatsin
real(r_kind), allocatable, dimension(:) :: spressmn,tmpspec
real(r_kind), allocatable, dimension(:,:) :: pressimn,presslmn
real(r_single),allocatable,dimension(:,:,:) :: nems_vcoord
real(r_kind) kap,kapr,kap1

integer(i_kind) i,j,nn,file_id,var_id,dim_id,nlevsp1
real(r_single), allocatable, dimension(:,:) :: ak,bk,lat_tile,lon_tile,ps
real(r_single), allocatable, dimension(:,:,:,:) :: delp
real(r_single) ptop
character(len=4) char_res
character(len=1) char_tile

nlevsp1 = nlevs + 1
nlevs_pres = nlevsp1
npts = ntiles*res*res
kap = rd/cp
kapr = cp/rd
kap1 = kap + one

! read data on root task
if (nproc .eq. 0) then

   !  read ak,bk from ensmean fv_core.res.nc
   filename = trim(adjustl(fileprefix))//'/ensmean/INPUT/fv_core.res.nc'
   call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
   myname_,'open: '//trim(adjustl(filename)) )
   call nc_check( nf90_inq_dimid(file_id,'xaxis_1',dim_id),&
       myname_,'inq_dimid xaxis_1 '//trim(filename) )
   call nc_check( nf90_inquire_dimension(file_id,dim_id,len=nlevsp1),&
       myname_,'inquire_dimension xaxis_1 '//trim(filename) )
   allocate(ak(nlevsp1,1),bk(nlevsp1,1))
   call read_fv3_restart_data2d('ak',filename,file_id,ak)
   call read_fv3_restart_data2d('bk',filename,file_id,bk)
   ptop = ak(1,1)
   
   !  read lats/lons from C###_oro_data.tile#.nc 
   ! (this requires path to FV3 fix dir)
   write(char_res, '(i4)') res
   allocate(lat_tile(res,res),lon_tile(res,res))
   nn = 0
   allocate(latsgrd(npts),lonsgrd(npts))
   do ntile=1,ntiles
      nn = nn + 1
      write(char_tile, '(i1)') ntile
      filename=trim(adjustl(fv3fixpath))//'/C'//trim(adjustl(char_res))//'/C'//trim(adjustl(char_res))//'_oro_data.tile'//char_tile//'.nc'
      call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
      myname_,'open: '//trim(adjustl(filename)) )
      call read_fv3_restart_data2d('geolon',filename,file_id,lon_tile)
      !print *,'min/max lon_tile',ntile,minval(lon_tile),maxval(lon_tile)
      call read_fv3_restart_data2d('geolat',filename,file_id,lat_tile)
      !print *,'min/max lat_tile',ntile,minval(lat_tile),maxval(lat_tile)
      call nc_check( nf90_close(file_id),&
      myname_,'close '//trim(filename) )
      do j=1,res
         do i=1,res
            latsgrd(nn) = lats_tile(i,j)
            lonsgrd(nn) = lons_tile(i,j)
         enddo
      enddo
      latsgrd = pi*latsgrd/180._r_single
      lonsgrd = pi*lonsgrd/180._r_single
   enddo

   allocate(delp(res,res,nlevs,1),ps(res,nres))
   allocate(pressimn(npts,nlevsp1),presslmn(npts,nlevs))
   nn = 0
   do ntile=1,ntiles
      nn = nn + 1
      write(char_tile, '(i1)') ntile
      filename = trim(adjustl(datapath))//'/ensmean/fv_core.res.tile'//char_tile//'.nc'
      !print *,trim(adjustl(filename))
      call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
      myname_,'open: '//trim(adjustl(filename)) )
      call read_fv3_restart_data4d('delp',filename,file_id,delp)
      !print *,'min/max delp',ntile,minval(delp),maxval(delp)
      call nc_check( nf90_close(file_id),&
      myname_,'close '//trim(filename) )
      ps = sum(delp,3) + ptop
      !print *,'min/max ps',ntile,minval(ps),maxval(ps)
      do j=1,res
         do i=1,res
            spressmn(nn) = ps(i,j)
         enddo
      enddo
   enddo
   ! pressure at interfaces
   do k=1,nlevsp1
      pressimn(:,k) = ak(nlevs-k,1)+bk(nlevs-k,1)*spressmn(:)
   enddo
   deallocate(delp,ak,bk,ps)
   do k=1,nlevs
     ! layer pressure from Phillips vertical interpolation.
     presslmn(:,k) = ((pressimn(:,k)**kap1-pressimn(:,k+1)**kap1)/&
                      (kap1*(pressimn(:,k)-pressimn(:,k+1))))**kapr
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

endif ! root task

if (nproc .ne. 0) then
   ! allocate arrays on other (non-root) tasks
   allocate(latsgrd(npts),lonsgrd(npts))
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   allocate(gridloc(3,npts))
   ! initialize reducedgrid_mod on other tasks.
   if (reducedgrid) then
      call reducedgrid_init(nlons,nlats,asin_gaulats)
   end if
endif
!call mpi_bcast(logp,npts*nlevs_pres,mpi_real4,0,MPI_COMM_WORLD,ierr)
do k=1,nlevs_pres
  call mpi_bcast(logp(1,k),npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
enddo
call mpi_bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
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
