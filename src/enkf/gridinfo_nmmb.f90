module gridinfo

use mpisetup
use params, only: datapath,nlevs,nvars,ndim,datestring,&
                  nmmb,regional,nlons,nlats,nbackgrounds,fgfileprefixes
use kinds, only: r_kind, i_kind, r_double, r_single
use constants, only: one,zero,pi,cp,rd,grav,rearth

implicit none
private
public :: getgridinfo, gridinfo_cleanup, wind2mass, mass2wind
integer(i_kind),public :: nlevs_pres
integer(i_kind),public, allocatable,dimension(:):: index_pres
real(r_single),public :: ptop
real(r_single),public, allocatable, dimension(:) :: lonsgrd, latsgrd
! arrays passed to kdtree2 routines must be single
real(r_single),public, allocatable, dimension(:,:) :: gridloc
real(r_single),public, allocatable, dimension(:,:) :: logp
integer,public :: npts
integer,public :: nvarhumid ! spec hum is the nvarhumid'th var
integer,public :: nvarozone ! ozone is the nvarozone'th var
contains

subroutine getgridinfo()
! read latitudes, longitudes and pressures for analysis grid,
! broadcast to each task.
use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                         nemsio_getfilehead,nemsio_getheadvar,&
                         nemsio_readrecv,nemsio_init,nemsio_realkind
implicit none
character(len=500) filename
integer(i_kind) iret,nlatsin,nlonsin,nlevsin,nlon_test,&
 ierr,nlon_test_with_halo,nlat_test_with_halo,nlat_test,nvar,k,nn
real(nemsio_realkind) pt,pdtop
real(r_kind), allocatable, dimension(:) :: spressmn
real(r_kind), allocatable, dimension(:,:) :: presslmn
real(nemsio_realkind) nems_wrk(nlons*nlats), field1(nlevs)
real(nemsio_realkind) aeta1(nlevs),aeta2(nlevs),lats(nlats*nlons),lons(nlons*nlats),&
  dphd_nems,dlmd_nems
type(nemsio_gfile) :: gfile

nlevs_pres=nlevs+1
nvarhumid = 4
nvarozone = 5 ! nmmb does not have ozone?

if (nproc .eq. 0) then

   ! get pressure, lat/lon information from ensemble mean file.
   npts = nlons*nlats
   allocate(presslmn(npts,nlevs))
   allocate(spressmn(npts))

   ! Build the ensemble mean filename expected by routine
  
   filename = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nbackgrounds/2+1)))//"_ensmean"
  
   call nemsio_init(iret=iret)
   if(iret/=0) then
      write(6,*)'gridinfo: nmmb model: problem with nemsio_init, iret=',iret
      call stop2(24)
   end if
   call nemsio_open(gfile,filename,'READ',iret=iret)
   if (iret/=0) then
      write(6,*)'gridinfo: nmmb model: problem with nemsio_open, iret=',iret
      call stop2(24)
   end if
   call nemsio_getfilehead(gfile,iret=iret,dimx=nlonsin,dimy=nlatsin, &
                           dimz=nlevsin,lat=lats,lon=lons)
   if (iret/=0) then
      write(6,*)'gridinfo: nmmb model: problem with nemsio_getfilehead, iret=',iret
      call stop2(24)
   endif
   if (.not. regional) then
      ! check to see if array dims include halo
      call nemsio_getheadvar(gfile,'DLMD',dlmd_nems,iret)
      nlon_test=1+nint(360./dlmd_nems)
      nlon_test_with_halo=nlon_test+2
      write(6,*)' nlon_test,nlon_test_with_halo,nlonsin=', &
                  nlon_test,nlon_test_with_halo,nlonsin
      if(nlonsin==nlon_test) then
        nlonsin=nlonsin+2
        write(6,*)' gridinfo: input nlonsin does not include halo--add 2'
      elseif(nlonsin==nlon_test_with_halo) then
        write(6,*)' gridinfo: input nlonsin includes halo'
      else
        write(6,*)' GESINFO: INPUT NLON_NEMS INCONSISTENT WITH DLMD_NEMS'
      end if
      write(6,*)' gridinfo: global nmmb model: dlmd_nems,iret=',dlmd_nems,iret
      write(6,*)' gridinfo: global nmmb model: 360/nlon=',360./(nlonsin-3)
      call nemsio_getheadvar(gfile,'DPHD',dphd_nems,iret)
      nlat_test=1+nint(180./dphd_nems)
      nlat_test_with_halo=nlat_test+2
      write(6,*)' nlat_test,nlat_test_with_halo,nlatsin=', &
                  nlat_test,nlat_test_with_halo,nlatsin
      if(nlatsin==nlat_test) then
        nlatsin=nlatsin+2
        write(6,*)' gridinfo: input nlatsin does not include halo--add 2'
      elseif(nlatsin==nlat_test_with_halo) then
        write(6,*)' gridinfo: input nlatsin includes halo'
      else
        write(6,*)' GESINFO: INPUT NLON_NEMS INCONSISTENT WITH DPHD_NEMS'
      end if
      write(6,*)' gridinfo: global nmmb model: dphd_nems,iret=',dphd_nems,iret
      write(6,*)' gridinfo: global nmmb model: 180/(nlat-2)=',180./(nlatsin-3)
   end if

   if (nlonsin .ne. nlons .or. nlatsin .ne. nlats .or. nlevsin .ne. nlevs) then
      write(6,*)'gridinfo: nmmb model: nlons,nlats,nlevs do not match'
      write(6,*)'nlons',nlonsin,nlons
      write(6,*)'nlats',nlatsin,nlats
      write(6,*)'nlevs',nlevsin,nlats
      call stop2(24)
   endif

   allocate(latsgrd(npts),lonsgrd(npts))
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   allocate(gridloc(3,npts))
   lonsgrd = lons; latsgrd = lats
   print *,'min/max lonsgrd',minval(lonsgrd),maxval(lonsgrd)
   print *,'min/max latsgrd',minval(latsgrd),maxval(latsgrd)
  
   call nemsio_getheadvar(gfile,'PT',pt,iret)
   pt = 0.01*pt
   ptop = pt
   call nemsio_getheadvar(gfile,'PDTOP',pdtop,iret)
   pdtop = 0.01*pdtop
   call nemsio_getheadvar(gfile,'SGML1',field1,iret)
   do k=1,nlevs
     aeta1(k)=field1(nlevs+1-k)
   enddo
   call nemsio_getheadvar(gfile,'SGML2',field1,iret)
   do k=1,nlevs
     aeta2(k)=field1(nlevs+1-k)
     aeta1(k) = aeta1(k) + aeta2(k)
   enddo
  
   call nemsio_readrecv(gfile,'dpres','hybrid sig lev',1,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridinfo: nmmb model: problem with nemsio_readrecv(dpres), iret=',iret
      call stop2(24)
   endif
   spressmn = 0.01*nems_wrk + pt ! surface pressure, units of hPa
   ! pressure on model levels
   do k=1,nlevs
      presslmn(:,k) = aeta1(k)*pdtop + aeta2(k)*(spressmn - pdtop - pt) + pt
   enddo
   do k=1,nlevs
      logp(:,k) = -log(presslmn(:,k))
   enddo
   logp(:,nlevs_pres) = -log(spressmn(:))
   deallocate(presslmn, spressmn)
   call nemsio_close(gfile, iret=iret)
endif

call mpi_bcast(npts,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
if (nproc .ne. 0) then
   ! allocate arrays on other (non-root) tasks
   allocate(latsgrd(npts),lonsgrd(npts))
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   allocate(gridloc(3,npts))
endif
!call mpi_bcast(logp,npts*nlevs_pres,mpi_real4,0,MPI_COMM_WORLD,ierr)
do k=1,nlevs_pres
  call mpi_bcast(logp(1,k),npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
enddo
call mpi_bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(ptop,1,mpi_real4,0,MPI_COMM_WORLD,ierr)

allocate(index_pres(ndim))

nn=0
do nvar=1,nvars
  do k=1,nlevs
    nn = nn + 1
    index_pres(nn)=k
  end do
end do

index_pres(ndim)=nlevs+1 ! ps
  
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
if (allocated(index_pres)) deallocate(index_pres)
end subroutine gridinfo_cleanup

subroutine wind2mass(dat,nlons,nlats)
 use nemsio_module, only: nemsio_realkind
 integer(i_kind), intent(in) :: nlons,nlats
 real(nemsio_realkind), dimension(nlons,nlats), intent(inout) :: dat
 real(nemsio_realkind), dimension(0:nlons,0:nlats) :: datsave
 integer i,j

 ! average wind points to mass grid on interior of grid
 ! linear extrapolation to get ghost row, column
 datsave(1:nlons,1:nlats) = dat(:,:)
 do j=1,nlats
    datsave(0,j) = 1.5*dat(1,j) - 0.5*dat(2,j)
 enddo
 do i=1,nlons
    datsave(i,0) = 1.5*dat(i,1) - 0.5*dat(i,2)
 enddo
 datsave(0,0) = 1.5*dat(1,1) - 0.5*dat(2,2)
 !datsave(0,0) = 0.5*(1.5*datsave(0,1)-0.5*datsave(0,2)) + &
 !               0.5*(1.5*datsave(1,0)-0.5*datsave(2,0))
 do j=1,nlats
    do i=1,nlons
       dat(i,j) = 0.25*(datsave(i,j)  +datsave(i-1,j)+&
                        datsave(i,j-1)+datsave(i-1,j-1))
    enddo
 enddo

end subroutine wind2mass

subroutine mass2wind(dat,nlons,nlats)
 use nemsio_module, only: nemsio_realkind
 integer(i_kind), intent(in) :: nlons,nlats
 real(nemsio_realkind), dimension(nlons,nlats), intent(inout) :: dat
 real(nemsio_realkind), dimension(nlons+1,nlats+1) :: datsave
 integer i,j
 ! average mass points to wind grid on interior of grid
 datsave(1:nlons,1:nlats) = dat(:,:)
 ! linear extrapolation to get ghost row, column
 do j=1,nlats
    datsave(nlons+1,j) = 1.5*dat(nlons,j) - 0.5*dat(nlons-1,j)
 enddo
 do i=1,nlons
    datsave(i,nlats+1) = 1.5*dat(i,nlats) - 0.5*dat(i,nlats-1)
 enddo
 datsave(nlons+1,nlats+1) = 1.5*dat(nlons,nlats) - 0.5*dat(nlons-1,nlats-1)
 !datsave(nlons+1,nlats+1) = 0.5*(1.5*datsave(nlons+1,nlats)-0.5*datsave(nlons+1,nlats-1)) + &
 !                           0.5*(1.5*datsave(nlons,nlats+1)-0.5*datsave(nlons-1,nlats+1))
 do j=1,nlats
    do i=1,nlons
       dat(i,j) = 0.25*(datsave(i,j)  +datsave(i+1,j)+&
                        datsave(i,j+1)+datsave(i+1,j+1))
    enddo
 enddo
end subroutine mass2wind

end module gridinfo
