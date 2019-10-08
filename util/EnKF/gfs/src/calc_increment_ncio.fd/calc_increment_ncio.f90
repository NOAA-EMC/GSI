PROGRAM calc_increment_ncio
!$$$  main program documentation block
!
! program:  calc_increment_ncio
!
! prgmmr: whitaker         org: esrl/psd               date: 2019-02-23
!
! abstract:  difference two ncio files, write out increment netcdf increment
! file for ingest into FV3.  The data in increment file must be oriented
! from south to north and from top to bottom in the vertical.
!
! program history log:
!   2019-02-12  Initial version.
!
! usage:
!   input files: filename_fg filename_anal (1st two command line args)
!
!   output files: filename_inc (3rd command line arg)

!   4th command line arg is logical for controlling whether microphysics
!   increment is computed.  5th command line argument is logical controlling
!   whether delz increment is computed hydrostatically from temp, humidity
!   and dpres.
!
! attributes:
!   language: f95
!
! ifort -O3 -xHOST -I${NCIO_INC} -I${NETCDF}/include calc_increment_ncio.f90
! ${NCIO_LIB} ${W3NCO_LIB4} ${BACIO_LIB4} -L${NETCDF}/lib -lnetcdf -lnetcdff
!
!$$$

  use module_fv3gfs_ncio, only: open_dataset, create_dataset, read_attribute, &
                           Dataset, Dimension, close_dataset, &
                           read_vardata, write_attribute, write_vardata, &
                           get_nvar, get_dim
  use netcdf

  implicit none

  character*500 filename_anal,filename_inc,filename_fg
  character(len=nf90_max_name) :: ncvarname
  integer k,nvar,ndims,nlats,nlons,nlevs,iret,nlons2,nlats2,nlevs2
  real, allocatable, dimension(:)     :: lats_tmp, lats, lons, ak, bk, ilevs, levs
  real, allocatable, dimension(:,:)   :: values_2d_fg,values_2d_anal,values_2d_inc
  real, allocatable, dimension(:,:,:) :: values_3d_fg,values_3d_anal,values_3d_inc
  type(Dataset) :: dset_anal,dset_fg
  type(Dimension) :: londim,latdim,levdim
  integer, dimension(3) :: dimid_3d
  integer, dimension(1) :: dimid_1d
  integer varid_lon,varid_lat,varid_lev,varid_ilev,varid_hyai,varid_hybi,&
          dimid_lon,dimid_lat,dimid_lev,dimid_ilev,ncfileid,ncstatus
  logical :: no_mpinc, has_dpres
  character(len=10) :: bufchar

  call getarg(1,filename_fg)    ! first guess ncio file
  call getarg(2,filename_anal)  ! analysis ncio file
  call getarg(3,filename_inc)   ! output increment file
  call getarg(4, bufchar)
  read(bufchar,'(L)') no_mpinc  ! if T, microsphysics increments computed

  write(6,*)'CALC_INCREMENT_NCIO:'
  write(6,*)'filename_fg=',trim(filename_fg)
  write(6,*)'filename_anal=',trim(filename_anal)
  write(6,*)'filename_inc=',trim(filename_inc)
  write(6,*)'no_mpinc',no_mpinc

  dset_fg = open_dataset(trim(filename_fg),errcode=iret)
  if (iret .ne. 0) then
    print *,'error opening ',trim(filename_fg)
    stop
  endif
  dset_anal = open_dataset(trim(filename_anal),errcode=iret)
  if (iret .ne. 0) then
    print *,'error opening ',trim(filename_anal)
    stop
  endif

  londim = get_dim(dset_fg,'grid_xt'); nlons = londim%len
  latdim = get_dim(dset_fg,'grid_yt'); nlats = latdim%len
  levdim = get_dim(dset_fg,'pfull');   nlevs = levdim%len
  londim = get_dim(dset_anal,'grid_xt'); nlons2 = londim%len
  latdim = get_dim(dset_anal,'grid_yt'); nlats2 = latdim%len
  levdim = get_dim(dset_anal,'pfull');   nlevs2 = levdim%len
  print *,'nlons,nlats,nlevs',nlons,nlats,nlevs
  print *,'nlons2,nlats2,nlevs2',nlons2,nlats2,nlevs2
  if ( nlons /= nlons2 .or. nlats /= nlats2 .or. &
      nlevs /= nlevs2) then
    print *,'expecting nlons,nlats,nlevs =',nlons,nlats,nlevs
    print *,'got nlons,nlats,nlevs =',nlons2,nlats2,nlevs2
    stop
  endif

  call read_vardata(dset_fg, 'grid_xt', lons)
  call read_vardata(dset_fg, 'grid_yt', lats_tmp)
  allocate(ak(nlevs+1),bk(nlevs+1),levs(nlevs),ilevs(nlevs+1))
  call read_attribute(dset_fg, 'ak', ak)
  call read_attribute(dset_fg, 'bk', bk)
  if (iret /= 0) then
    print *, 'problem with ncio_getfilehead getting vcoord, iret=', iret
    stop 
  endif
  lats = lats_tmp(nlats:1:-1)
  if (lats(1) .gt. lats(nlats)) then
    print *,'error: code assumes lats in ncio files are N to S'
    stop
  endif
  deallocate(lats_tmp)

! create netcdf increment file.
  ncstatus = nf90_create(trim(filename_inc),           &
       cmode=ior(NF90_CLOBBER,NF90_NETCDF4),ncid=ncfileid)
  if (ncstatus /= nf90_noerr) then
     print *, 'error opening file ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_def_dim(ncfileid,'lon',nlons,dimid_lon)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating lon dim ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_def_dim(ncfileid,'lat',nlats,dimid_lat)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating lat dim ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_def_dim(ncfileid,'lev',nlevs,dimid_lev)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating lev dim ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_def_dim(ncfileid,'ilev',nlevs+1,dimid_ilev)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating ilev dim ',trim(nf90_strerror(ncstatus))
     stop
  endif
  dimid_1d(1) = dimid_lon
  ncstatus = nf90_def_var(ncfileid,'lon',nf90_float,dimid_1d,   &
       & varid_lon)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating lon ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_put_att(ncfileid,varid_lon,'units','degrees_east')
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating lon units ',trim(nf90_strerror(ncstatus))
     stop
  endif
  dimid_1d(1) = dimid_lat
  ncstatus = nf90_def_var(ncfileid,'lat',nf90_float,dimid_1d,   &
       & varid_lat)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating lat ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_put_att(ncfileid,varid_lat,'units','degrees_north')
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating lat units ',trim(nf90_strerror(ncstatus))
     stop
  endif
  dimid_1d(1) = dimid_lev
  ncstatus = nf90_def_var(ncfileid,'lev',nf90_float,dimid_1d,   &
       & varid_lev)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating lev ',trim(nf90_strerror(ncstatus))
     stop
  endif
  dimid_1d(1) = dimid_ilev
  ncstatus = nf90_def_var(ncfileid,'ilev',nf90_float,dimid_1d,  &
       & varid_ilev)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating ilev ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_def_var(ncfileid,'hyai',nf90_float,dimid_1d,  &
       & varid_hyai)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating hyai ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_def_var(ncfileid,'hybi',nf90_float,dimid_1d,  &
       & varid_hybi)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating hybi ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_put_att(ncfileid,nf90_global,'source','GSI')
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating global attribute source',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_put_att(ncfileid,nf90_global,'comment','global analysis increment from calc_increment_ncio')
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating global attribute comment',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_enddef(ncfileid)
  if (ncstatus /= nf90_noerr) then
     print *,'enddef error ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_put_var(ncfileid,varid_lon,lons)
  if (ncstatus /= nf90_noerr) then
     print *, 'error writing lon ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_put_var(ncfileid,varid_lat,lats)
  if (ncstatus /= nf90_noerr) then
     print *, 'error writing lat ',trim(nf90_strerror(ncstatus))
     stop
  endif
  do k=1,nlevs
     levs(k)=k
  enddo
  ncstatus = nf90_put_var(ncfileid,varid_lev,levs)
  if (ncstatus /= nf90_noerr) then
     print *, 'error writing lev ',trim(nf90_strerror(ncstatus))
     stop
  endif
  do k=1,nlevs+1
     ilevs(k)=k
  enddo
  ncstatus = nf90_put_var(ncfileid,varid_ilev,ilevs)
  if (ncstatus /= nf90_noerr) then
     print *, 'error writing ilev ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ! note that levels go from top to bottom (opposite to ncio files)
  ncstatus = nf90_put_var(ncfileid,varid_hyai,ak)
  if (ncstatus /= nf90_noerr) then
     print *, 'error writing hyai ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_put_var(ncfileid,varid_hybi,bk)
  if (ncstatus /= nf90_noerr) then
     print *, 'error writing hybi ',trim(nf90_strerror(ncstatus))
     stop
  endif

  dimid_3d(1) = dimid_lon
  dimid_3d(2) = dimid_lat
  dimid_3d(3) = dimid_lev

  nvar = get_nvar(dset_fg,'dpres')
  if (nvar > 0) then 
    has_dpres = .true.
  else
    has_dpres = .false.
  endif
  print *,'has_dpres ',has_dpres

  ! ps increment.
  allocate(values_2d_inc(nlons,nlats))
  allocate(values_3d_inc(nlons,nlats,nlevs))
  do nvar=1,dset_fg%nvars
     ndims = dset_fg%variables(nvar)%ndims
     if (trim(dset_fg%variables(nvar)%name) == 'pressfc') then
        call read_vardata(dset_fg,trim(dset_fg%variables(nvar)%name),values_2d_fg)
        call read_vardata(dset_anal,trim(dset_fg%variables(nvar)%name),values_2d_anal)
        ! increment (flip lats)
        values_2d_inc(:,nlats:1:-1) = values_2d_anal - values_2d_fg
     endif
  enddo

  do nvar=1,dset_fg%nvars
     ndims = dset_fg%variables(nvar)%ndims
     if (ndims == 4) then ! all 3d vars
        ncvarname = 'none'
        !print *,trim(dset_fg%variables(nvar)%name)
        if (trim(dset_fg%variables(nvar)%name) == 'ugrd') then
           ncvarname = 'u_inc'
        else if (trim(dset_fg%variables(nvar)%name) == 'vgrd') then
           ncvarname = 'v_inc'
        else if (trim(dset_fg%variables(nvar)%name) == 'tmp') then
           ncvarname = 'T_inc'
        else if (trim(dset_fg%variables(nvar)%name) == 'dpres') then
           ncvarname = 'delp_inc'
        else if (trim(dset_fg%variables(nvar)%name) == 'spfh') then
           ncvarname = 'sphum_inc'
        else if (trim(dset_fg%variables(nvar)%name) == 'o3mr') then
           ncvarname = 'o3mr_inc'
        else if (trim(dset_fg%variables(nvar)%name) == 'delz') then
           ncvarname = 'delz_inc'
        else if (trim(dset_fg%variables(nvar)%name) == 'clwmr' .and. &
                 .not. no_mpinc) then
           ncvarname = 'liq_wat_inc'
        else if (trim(dset_fg%variables(nvar)%name) == 'icmr' .and. &
                 .not. no_mpinc) then
           ncvarname = 'ice_wat_inc'
        endif
        if (trim(ncvarname) /= 'none') then
           call read_vardata(dset_fg,trim(dset_fg%variables(nvar)%name),values_3d_fg)
           call read_vardata(dset_anal,trim(dset_fg%variables(nvar)%name),values_3d_anal)
           ! increment (flip lats)
           values_3d_inc(:,nlats:1:-1,:) = values_3d_anal - values_3d_fg
           call write_ncdata3d(values_3d_inc,ncvarname,nlons,nlats,nlevs,ncfileid,dimid_3d)
        endif
     endif ! ndims == 4
  enddo  ! nvars
  ! infer delp increment from ps increment
  if (.not. has_dpres) then
     ncvarname = 'delp_inc'
     do k=1,nlevs
        values_3d_inc(:,:,k) = values_2d_inc*(bk(k+1)-bk(k))
     enddo
     call write_ncdata3d(values_3d_inc,ncvarname,nlons,nlats,nlevs,ncfileid,dimid_3d)
  endif

  ncstatus = nf90_close(ncfileid)
  if (ncstatus /= nf90_noerr) then
     print *, 'error closing file:',trim(nf90_strerror(ncstatus))
     stop
  endif
  call close_dataset(dset_fg)
  call close_dataset(dset_anal)

END PROGRAM calc_increment_ncio

subroutine write_ncdata3d(incdata,ncvarname,&
                          nlons,nlats,nlevs,ncfileid,dimid_3d)
  use netcdf
  integer, intent(in) :: nlons,nlats,nlevs,ncfileid,dimid_3d(3)
  integer varid,ncstatus
  real, intent(in) ::  incdata(nlons,nlats,nlevs)
  character(len=nf90_max_name), intent(in) :: ncvarname

  ncstatus = nf90_redef(ncfileid)
  if (ncstatus /= nf90_noerr) then
     print *,'redef error ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_def_var(ncfileid,trim(ncvarname),nf90_float,dimid_3d,varid)
  if (ncstatus /= nf90_noerr) then
     print *, 'error creating',trim(ncvarname),' ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ! turn on compression (level 1)
  ncstatus = nf90_def_var_deflate(ncfileid, varid, 1,1,1)
  if (ncstatus /= nf90_noerr) then
     print *,'nc_def_var_deflate error ',trim(nf90_strerror(ncstatus))
     stop
  endif
  ncstatus = nf90_enddef(ncfileid)
  if (ncstatus /= nf90_noerr) then
     print *,'enddef error ',trim(nf90_strerror(ncstatus))
     stop
  endif
  print *,'writing ',trim(ncvarname),' min/max =',minval(incdata),maxval(incdata)
  ncstatus = nf90_put_var(ncfileid,varid,incdata)
  if (ncstatus /= nf90_noerr) then
     print *, trim(nf90_strerror(ncstatus))
     stop
  endif
end subroutine write_ncdata3d
