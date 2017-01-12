
program gen_be_nc2gsi

!------------------------------------------------------------------------
!  Purpose: converter, read be.nc and write binary for GSI applications 
!           gen_be (new features)
!
!  Auothor: Gael Descombes (MMM/NESL/NCAR)   Date: 10/02/2014
!
!  Note: Please acknowledge author/institute in work that uses this code.
!------------------------------------------------------------------------

!   use configure

   implicit none

   character (len=1024) :: filename
   character (len=32) :: varname
   character (len=5) :: variable, variable2
   character (len=3) :: cne, cinterval
   integer, parameter :: StrLen = 32
   integer :: StrLen0
   real, allocatable ,dimension(:,:) :: mapfac_m
   real, allocatable ,dimension(:) :: field1d
   real :: field0d

   integer :: nferr, VarID, rd_ncid
   integer, dimension(1) :: start1, count1
   integer, dimension(2) :: start2, count2

   integer :: rdVarIDvarnd
   integer :: ii, vv, kk, jj, bb

   integer :: bin_type,  num_bins, num_bins2d, nvar, nlon, nlat, nlev
   real :: binwidth_lat

   ! read the namelist parameter
   integer, allocatable, dimension(:,:) :: covarID
   integer, allocatable, dimension(:) :: vardimlist
   character (len=StrLen), allocatable, dimension(:) :: cvlist, cvlistu

   !==========================================================================================

   real, allocatable, dimension(:,:,:) :: corz3d, corz3d2, hwll3d, vztdq3d
   real, allocatable, dimension(:,:) :: cor2d, hwll2d
   integer :: qoption
   integer :: vdim, vind, nrf, nc3d, vind3d, vind2d, nc2d
   character (len=10) :: cvar

   integer :: nsig, mlat
   real, allocatable, dimension(:,:)    :: lat2d,alld_bin2d 
   real, allocatable, dimension(:,:,:)    :: agvi
   real, allocatable, dimension(:,:)      :: wgvi
   real, allocatable, dimension(:,:)      :: bvi
   real, allocatable, dimension(:,:)      :: varq2
   real, allocatable, dimension(:)        :: znu 
   integer :: mlat_min, mlat_max
   real*4  ::  lat_max0, lat_min0 
   logical :: nf_err0
   integer :: bin
   real :: degs
   logical :: do_smooth 

   !--------------------------------------------------------------------
   ! real simple
   integer :: nz
   real*4, allocatable  :: sigl_s(:),clat_s(:), rlats(:)
   real*4, allocatable  :: agv_s(:,:,:),wgv_s(:,:),bv_s(:,:)
   real*4, allocatable, dimension(:,:,:) :: corz3d_s, hwll3d_s, vztdq3d_s, corz3d2_s
   real*4, allocatable, dimension(:,:) :: cor2d_s, hwll2d_s
   real*4, allocatable, dimension(:,:)    :: tmp2,tmp 
   real*4, allocatable, dimension(:)    :: tmp1 
   real :: deg2rad
   parameter(deg2rad=acos(-1.0)/180.0)

   !---------------------------------------------------------------------

   integer :: vardim, numarg
#ifdef IARGC
   integer, intrinsic    :: iargc
#else
   integer, external     :: iargc
#endif
   integer :: ne, interval
   character*10 :: start_date, end_date
   character*10 :: be_area 

!--------------------------------------------------------------------

include 'netcdf.inc'


!----------------------------------------------------------------------------------------------------
! Read the variable in argument
!----------------------------------------------------------------------------------------------------
   numarg = iargc()
   if ( numarg /= 4 ) then
      write(UNIT=6,FMT='(a)') "the converter without any argument will select qoption=1"
      write(UNIT=6,FMT='(a)') "qoption=2 needs 5 arguments start_date end_date interval ne be_area(regional/global)"
      qoption=0
   else
      ! Initialise to stop false Cray compiler warnings
      start_date=""
      end_date=""
      cinterval=""
      cne=""
      qoption=1
      call getarg( 1, start_date )
      call getarg( 2, end_date )
      call getarg( 3, cinterval )
      call getarg( 4, cne )
      read(cne,'(i3)')ne
      read(cinterval,'(i3)')interval
   end if




!---------- GSI -----------------------------------------------------
be_area='regional'
nc3d = 0
nc2d = 0
nvar = 3
nrf  = 1
mlat = 4
nsig = 27
cvar = 'sf'
vdim = 3
vind = 1
mlat_min = 1
mlat_max = mlat + 2
qoption=1
start_date='2012060312'
end_date=start_date
interval=12
ne=1
degs = 0.

!--------------------------------------------------------
! read netcdf file and allocate
!--------------------------------------------------------

filename = 'be.nc'

call io_input_init(filename, rd_ncid)

call read_dimensions(rd_ncid, nlev, nlat, nlon, nvar, num_bins, num_bins2d, StrLen0)

allocate(vardimlist(nvar))
varname='vardim_list'
call read_field1dint(rd_ncid, nvar, varname, vardimlist)
do ii=1, nvar
   if (vardimlist(ii).eq.3) then
      nc3d = nc3d + 1
   else if (vardimlist(ii).eq.2) then
      nc2d = nc2d + 1
   end if
end do


! binning
!-----------------------------------------------
varname='bin_type'
call read_field0dint(rd_ncid, varname, bin_type)
if ( bin_type.eq.5 ) then
  write(6,*)'binning option 5, binning by uniform vertical level'
else if ( bin_type.eq.3 ) then
  write(6,*)'binning option 3, binning by uniform vertical level and latitude'
else if ( bin_type.eq.1 ) then
  write(6,*)'binning option 1, binning by uniform vertical level and latitude along x coordinate'
else
  write(6,*)'binning option not supported into GSI'  
  stop 
end if

varname='binwidth_lat'
call read_field0d(rd_ncid, varname, binwidth_lat)

allocate(lat2d(nlon,nlat))
varname='lat'
call read_field2d(rd_ncid, nlon, nlat, varname, lat2d)


lat_max0 = maxval(lat2d)
lat_min0 = minval(lat2d)
write(*,*)'lat_max0,lat_min0 ',lat_max0,lat_min0

mlat = floor (abs((lat_max0 - lat_min0) / binwidth_lat) ) + 1
if ( qoption.eq.2 ) then
  if ( bin_type.eq.5 ) then
     mlat = max(mlat,21)
     else if ((( bin_type.eq.1 ).or.(bin_type.eq.3 )).and.(mlat .lt. 21)) then
        write(*,*)'GSI can support qoption2 mlat < 21 , mlat=',mlat
        stop
   end if 
end if

mlat_min = 1
mlat_max = mlat + 2
allocate(rlats(mlat_min:mlat_max))
write(6,*)'mlat   ',mlat
allocate(clat_s(mlat))
clat_s(1) = lat_min0 + binwidth_lat/2.
do ii=2, mlat
   clat_s(ii) = clat_s(1) + binwidth_lat*(ii-1)
   rlats(ii)  = clat_s(ii)*deg2rad 
end do
rlats(1) = lat_min0*deg2rad
rlats(mlat_max) = lat_max0*deg2rad
write(6,*)'clat_s ',clat_s

write(*,*)'be_area ',trim(be_area)
write(*,*)'nlat, mlat, nlon : ',nlat,mlat,nlon


nsig = nlev
allocate(corz3d(mlat_min:mlat_max,nsig,nc3d))
allocate(corz3d2(mlat_min:mlat_max,nsig,nc3d))
allocate(hwll3d(mlat_min:mlat_max,nsig,nc3d))
allocate(vztdq3d(nsig,mlat_min:mlat_max,nc3d))
allocate(cor2d(mlat_min:mlat_max,nc2d))
allocate(hwll2d(mlat_min:mlat_max,nc2d))

allocate(agvi(mlat_min:mlat_max,nsig,nsig))
allocate(wgvi(mlat_min:mlat_max,nsig))
allocate(bvi(mlat_min:mlat_max,nsig))
allocate(znu(nsig))

!! simple array
nz=nlev
allocate(sigl_s(nz))
allocate(agv_s(mlat_min:mlat_max,nz,nz),wgv_s(mlat_min:mlat_max,nz))
allocate(bv_s(mlat_min:mlat_max,nz))

allocate(corz3d_s(mlat_min:mlat_max,nsig,nc3d))
allocate(corz3d2_s(mlat_min:mlat_max,nsig,nc3d))
allocate(hwll3d_s(mlat_min:mlat_max,nsig,nc3d))
allocate(vztdq3d_s(nsig,mlat_min:mlat_max,nc3d))
allocate(cor2d_s(mlat_min:mlat_max,nc2d))
allocate(hwll2d_s(mlat_min:mlat_max,nc2d))
hwll2d = 0.0
allocate(varq2(mlat_min:mlat_max,nsig))
varq2 = 0.0

allocate(alld_bin2d(mlat_min:mlat_max,nsig))

!----------------------------------------------
! read namelist parameter
!----------------------------------------------
allocate(cvlist(nvar))
allocate(cvlistu(nvar))

varname='cv_list'
call read_field1dstr(rd_ncid, nvar, StrLen0, varname, cvlist)
varname='cv_listu'
call read_field1dstr(rd_ncid, nvar, StrLen0, varname, cvlistu)
varname='vardim_list'
call read_field1dint(rd_ncid, nvar, varname, vardimlist)
write(*,*)'cv_listu ',cvlistu
   
!----------------------------------------------
! read regcoeff
!----------------------------------------------
write(*,*)'num_bins2d, mlat, ',num_bins2d, mlat

call  read_regcoeff_gsi(rd_ncid, nsig, num_bins2d, mlat, mlat_min, mlat_max, agvi, bvi, wgvi, znu)


!-----------------------------------------------
! if qoption = 2 
!-----------------------------------------------

if ( qoption.eq.2) then
   write(*,*)'qoption 2 selected'
   call specific_qoption2(varq2, ne, start_date, end_date, interval, nlon, nlat,nlev, mlat)
end if


!----------------------------------------------
! Open binary and write in simple 
!----------------------------------------------

nz = nlev
sigl_s = znu


agv_s = agvi
wgv_s = wgvi
bv_s  = bvi

do bin = 1, 2 

if ( bin.eq.1) then
   open(333,file='wrf-arw-gsi_be_big_endian.gcv',convert='big_endian',form='unformatted')
else
   open(333,file='wrf-arw-gsi_be_little_endian.gcv',convert='little_endian',form='unformatted')
end if
rewind 333

if ( trim(be_area).ne.'global'  ) then
    write(333)nz,mlat
    write(333)clat_s,sigl_s
else
    write(333)nz,mlat_max,nlon
    write(*,*)'nz,mlat_max,nlon ',nz,mlat_max,nlon
    write(*,*)'bnam nz,mlat_max,nlon ',nz,mlat_max,nlon
end if
write(333)agv_s,bv_s,wgv_s


!---------------------------------------------------------------
! write variable
!---------------------------------------------------------------
nf_err0 = .false.
vind3d = 0
vind2d = 0


do vv=1, nvar

      vdim = vardimlist(vv)
      if (vdim .eq. 3 ) then
          vind3d = vind3d + 1
          vind = vind3d
          kk = nsig
      else
          vind2d = vind2d + 1
          vind = vind2d
          kk = 1
      end if

      
      nf_err0 = .false.
      write(*,*)'***********************************************************'
      write(*,*)' ',trim(cvlistu(vv))
      write(*,*)'***********************************************************'
      call read_be_gsi(rd_ncid, be_area, cvlistu(vv), variable, vdim, vind, nf_err0, hwll2d, cor2d, &
                hwll3d, vztdq3d, corz3d, corz3d2, varq2, znu, qoption, num_bins2d, mlat, mlat_min, mlat_max, nsig, nvar, nc3d)


      if ( degs .gt. 1.  ) then

         write(6,*)'FILTERING STATISITICS HLS/VLS/VARIANCE'
 
         if (vdim .eq. 2 ) then
            alld_bin2d(:,1) = hwll2d(mlat_min+1:mlat+1,vind)
            call smoothlat(alld_bin2d,mlat_max,1,rlats,degs)
            hwll2d_s(:,vind)  = alld_bin2d(:,1)
            
            alld_bin2d(:,1) = cor2d(mlat_min+1:mlat+1,vind)
            call smoothlat(alld_bin2d,mlat_max,1,rlats,degs)
            cor2d_s(:,vind)  = alld_bin2d(:,vind)

         else
            call smoothlat(hwll3d(:,:,vind),mlat_max,nlev,rlats,degs)
            hwll3d_s(:,:,vind) = hwll3d(:,:,vind)
            call smoothlat(corz3d(:,:,vind),mlat_max,nlev,rlats,degs)
            corz3d_s(:,:,vind) = corz3d(:,:,vind)
            call smoothlat(corz3d2(:,:,vind),mlat_max,nlev,rlats,degs)
            corz3d2_s(:,:,vind) = corz3d2(:,:,vind)
            
            ! commented for vertical length scale constant over latitude 
            !do ii=1, nsig
            !  do bb = mlat_min+1, mlat+1
            !     alld_bin2d(bb,ii) = vztdq3d(ii,bb,vind)
            !  end do
            !end do
            !call smoothlat(alld_bin2d(:,:),mlat_max,nlev,rlats,degs)
            !do ii=1, nsig
            !  do bb = mlat_min, mlat_max
            !        vztdq3d_s(ii,bb,vind) = alld_bin2d(bb,ii) 
            !        vztdq3d(ii,bb,vind)   = alld_bin2d(bb,ii) 
            !  end do
            !end do

         end if

      else
         hwll2d_s  = hwll2d 
         cor2d_s   = cor2d
         hwll3d_s  = hwll3d
         corz3d_s  = corz3d
         corz3d2_s = corz3d2
         vztdq3d_s = vztdq3d

      end if

      if ( nf_err0 ) then 

         
         write(*,*)'========================================'
         write(6,*)'write variable ,nsig ',trim(variable),kk
         write(*,*)'======================================='
         write(333)variable,kk

         if ( trim(variable) .eq. 'q' ) then
               write(333)corz3d_s(mlat_min+1:mlat+1,:,vind),corz3d2_s(mlat_min+1:mlat+1,:,vind)
               write(6,*)'------------------------------------------------------'
               write(*,*)'stdq',maxval(corz3d_s(mlat_min+1:mlat+1,:,vind)),minval(corz3d_s(mlat_min+1:mlat+1,:,vind))
               write(6,*)'------------------------------------------------------'
               write(*,*)'stdq',corz3d_s(2,:,vind)
         else if ( vardimlist(vv) .eq. 2  ) then
               write(333)cor2d_s(mlat_min+1:mlat+1,vind)
               write(6,*)'------------------------------------------------------'
               write(*,*)'std ps',minval(cor2d_s(mlat_min+1:mlat+1,vind)),maxval(cor2d_s(mlat_min+1:mlat+1,vind))
               write(6,*)'------------------------------------------------------'
         else
               write(6,*)'------------------------------------------------------'
               write(*,*)'std',maxval(corz3d_s(mlat_min+1:mlat+1,:,vind)),minval(corz3d_s(mlat_min+1:mlat+1,:,vind))
               write(6,*)'------------------------------------------------------'
               write(*,*)'std',corz3d_s(2,:,vind)
               write(333)corz3d_s(mlat_min+1:mlat+1,:,vind)
         end if


         if ( vardimlist(vv) .eq. 2 ) then
            write(6,*)'------------------------------------------------------'
            write(*,*)'write 2d  horizontal length scale',minval(hwll2d_s),maxval(hwll2d_s)
            write(6,*)'------------------------------------------------------'
            write(333) hwll2d_s  ! From the hlt array
            !write(*,*) hwll2d_s(:,:)  ! From the hlt array
         else
            write(6,*)'------------------------------------------------------'
            write(6,*)'horizontal length scale :',maxval(hwll3d_s(1:mlat,:,vind)),minval(hwll3d_s(1:mlat,:,vind))
            !write(6,*)'horizontal length scale :',hwll3d_s(:,:,vind)
            write(6,*)'------------------------------------------------------'
            write(333) hwll3d_s(:,:,vind)  ! From the hlt array 
            write(6,*)'------------------------------------------------------'
            write(6,*)'vertical length scale :',maxval(vztdq3d_s(:,:,vind)),minval(vztdq3d_s(:,:,vind))
            !write(6,*)'vertical length scale :',vztdq3d_s(:,1:mlat,vind)
            write(6,*)'------------------------------------------------------'
            write(333) vztdq3d_s(:,:,vind)
         end if

      end if

end do ! loop over variables



close(333)

write(*,*)'after to close bin ',bin

end do ! loop over binary format

!----------------------------------------------------------------


contains

!================================================================================
! subroutines to read be.nc netcdf file, form gen_be (NCAR, gael DESCOMBES /MMM)
!================================================================================

subroutine read_be_gsi(rd_ncid, be_area, variable, cvar, vdim, vind, nf_err0, hwll2d, cor2d, &
                hwll3d, vztdq3d, corz3d, corz3d2, varq2 , znu, qoption, num_bins2d, mlat, mlat_min, mlat_max, nsig, nvars, nc3d)

   implicit none

   real, dimension(:,:,:), intent(inout) ::  hwll3d
   real, dimension(:,:,:), intent(inout) :: corz3d, corz3d2
   real, dimension(:,:,:), intent(inout) :: vztdq3d
   real, dimension(:,:), intent(inout)   :: hwll2d
   real, dimension(:,:), intent(inout)   :: cor2d
   real, dimension(1:nsig), intent(in)                :: znu
   real, dimension(:,:), intent(inout)         :: varq2
   integer, intent(inout) :: qoption
   integer, intent(in) :: mlat, nsig, nvars, nc3d, mlat_min, mlat_max,num_bins2d
   integer, intent(in) :: vdim, vind, rd_ncid
   logical, intent(inout) :: nf_err0
   character (len=*), intent(inout) :: cvar
   character (len=*), intent(in) :: variable 
   character (len=*), intent(in) :: be_area 

   character (len=64) :: filename, varname, variable2
 
   real, allocatable, dimension(:,:) :: vertvarce2d, vertvarce2d_2
   real, allocatable, dimension(:,:) :: horlen2d, verlen2d
   real, allocatable, dimension(:)   :: field1d
   real :: field0d
   integer :: i, j, k, b, dim3, nferr, i0, rdVarIDvarnd, imod  
   integer :: nlev, nlat, nlon, nvar, ncovar, num_bins
   integer, dimension(1) :: start1, count1
   real, allocatable    :: rsig(:), dsig(:)
   real(4) :: vlen 

   include 'netcdf.inc'


!    write(*,*)'cvar, vdim, vind, mlat, nsig, ',cvar, vdim, vind, mlat, nsig, nrf 

   !-------------------------------------
   ! before reading variable into be.nc
   !-------------------------------------

   nf_err0 = .true.
   select case( trim(adjustl(variable)) )
      case ('psi')
         cvar = 'sf'
      case ('chi_u')
         cvar = 'vp'
      case ('t_u')
         cvar = 't'
      case ('tv_u')
         cvar = 't'
      case ('qcwall')
         cvar = 'cw'
      case ('qrain')
         cvar = 'qr'
      case ('qcloud')
         cvar = 'ql'
      case ('qice')
         cvar = 'qi'
      case ('qsnow')
         cvar = 'qs'
      case ('qgraup')
         cvar = 'qg'
      case ('rh')
         cvar = 'q'
         variable2 = 'rhm'
      case ('ps_u')
         cvar = 'ps'
   case default;
        write(*,*)' The selected variable is not in the pre-defined list in read_be_gsi '
        nf_err0 = .false.
   end select

 
  if ( nf_err0  ) then

  ! allocation 
   if ( vdim .eq. 3 ) then
      dim3 = nsig
   else
      dim3 = 1
   end if
   !!num_bins2d = 1 !! = mlat

   allocate(horlen2d(dim3,num_bins2d))
   allocate(verlen2d(dim3,num_bins2d))
   allocate(vertvarce2d(dim3,num_bins2d))
   allocate(vertvarce2d_2(dim3,num_bins2d))
   horlen2d = 0.0
   verlen2d = 0.0
   vertvarce2d   = 0.0
   vertvarce2d_2 = 0.0
   vztdq3d       = 0.0

   !------------------------------------
   ! read the variables
   !------------------------------------
   if ( vdim .eq. 3 ) then

      ! read horizontal length scale
      varname = 'lenscale_'//trim(variable)
      call read_field2d(rd_ncid, nsig, num_bins2d,  trim(varname), horlen2d)
      ! read vertical length scale
      varname = 'vert_lenscale_'//trim(variable)
      call read_field2d(rd_ncid, nsig, num_bins2d, trim(varname), verlen2d)

      ! read verticale variance
      varname = 'vert_varce_'//trim(variable)
      call read_field2d(rd_ncid, nsig, num_bins2d, trim(varname), vertvarce2d)
      vertvarce2d = sqrt(vertvarce2d)
      if (cvar == 'q') then
         !varname = 'vert_varce_'//trim(variable2)
         !nferr   = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
         if ( qoption .eq.2 ) then
            do bb=1, num_bins2d
               vertvarce2d_2(:,bb) = sqrt(varq2(bb,:))
            end do
         else
             vertvarce2d_2 = sqrt(vertvarce2d)
         end if
      end if   

   else  !  vdim = 2
      
      ! read horizontal length scale
      varname = 'lenscale_'//trim(variable)
      !call read_field0d(rd_ncid, varname, field0d)
      !horlen2d(1,1) = field0d
      !call read_field0d(rd_ncid, varname, field1d)
      allocate(field1d(num_bins2d))
      field1d = 0.0 
      call read_field1d(rd_ncid, num_bins2d, trim(varname), field1d)
      horlen2d(1,:) = field1d
      ! read verticale variance
      varname = 'vert_varce_'//trim(variable)
      field1d = 0.0 
      call read_field1d(rd_ncid, num_bins2d, trim(varname), field1d)
      !write(*,*)'field1d ',field1d(:) 
      vertvarce2d(1,:) = sqrt(field1d(:))
      !write(*,*)'vertvarce2d PS ',vertvarce2d(1,:)
      deallocate(field1d)
      write(*,*)'after read 2d'
   end if

   ! remove lengthscale < 0
   !where ( horlen2d .le.0. )
   !   tmp = 1e20
   !elsewhere
   !   tmp = horlen2d
   !end where  
   !minval0 = ( minval(horlen2d) ) / 2

   !--------------------------------------------------------
   ! transform vertical length scale
   !--------------------------------------------------------
   !write(*,*)'verlen2d grid_point 1 ',verlen2d

   if ( vdim .eq. 3 ) then
      write(*,*)' compute  transform vertical length scale for regional application'

      if ( trim(be_area) .ne.'global') then      

      allocate(rsig(1:nsig))
      allocate(dsig(1:nsig))

      do b=1, num_bins2d
      ! Definition of vertical scale in log(sigma) unit
      do k=1,nsig
         rsig(k)=log(znu(k))
       end do

       dsig(1)=rsig(1)-rsig(2)
       do k=2,nsig-1
          dsig(k) = 0.5*(rsig(k-1)-rsig(k+1))
       end do
       dsig(nsig)=rsig(nsig-1)-rsig(nsig)
       !write(*,*)' vertvarce2d 1 ',vertvarce2d 

       ! compute vertical length scale
       do k=1, nsig
          if (verlen2d(k,b).ne.0) then
             verlen2d(k,b) = 1./verlen2d(k,b)/dsig(k)
          end if
       end do

       end do

       deallocate(rsig)
       deallocate(dsig)

       else
          do b=1, num_bins2d
          do k=1, nsig
            if (verlen2d(k,b).ne.0) then
                verlen2d(k,b) = 1./verlen2d(k,b)
            else
                verlen2d(k,b) = 0.0
            end if
          end do
          end do

       end if

    end if

  ! write(*,*)'verlen2d grid_point 2',verlen2d

   !--------------------------------------------------------
   ! transform dimensions for GSI
   !--------------------------------------------------------

   imod = int( mlat / num_bins2d )

   if ( vdim .eq. 3 ) then
        write(*,*)'transform dimensions for GSI 3d'
            do k = 1, nsig
               do i=1, mlat
                  if ( num_bins2d.eq.1  ) then
                      i0 = 1
                  else if ( num_bins2d.eq.mlat  ) then
                      i0 = i
                  else
                      i0 = min(int(i/imod)+ 1,num_bins2d) 
                  end if
                  if (cvar=='q') then
                     corz3d2(i+1,k,vind) = vertvarce2d_2(k,i0)
                  end if
                  corz3d(i+1,k,vind)  = vertvarce2d(k,i0)
                  vztdq3d(k,i+1,vind) =  verlen2d(k,i0)
                  hwll3d(i+1,k,vind)  =  horlen2d(k,i0)
                  !write(*,*)'corz3d ',vertvarce2d(:,i0) 
                  !write(*,*)'vztdq3d(k,i,vind)',k,i,vztdq3d(k,i,vind)
               end do
            end do
         write(*,*)'coucou 2'         
            hwll3d(mlat_min,:,vind)   =  hwll3d(mlat_min+1,:,vind)
            hwll3d(mlat_max,:,vind)   =  hwll3d(mlat+1,:,vind)
            vztdq3d(:,mlat_min,vind)  =  vztdq3d(:,mlat_min+ 1,vind)
            vztdq3d(:,mlat_max,vind)  =  vztdq3d(:,mlat+1,vind)
            corz3d(mlat_min,:,vind)   =  corz3d(mlat_min+1,:,vind)
            corz3d(mlat_max,:,vind)   =  corz3d(mlat+1,:,vind)
            if ( cvar == 'q' .and. qoption == 2 ) then
               corz3d2(mlat_min,:,vind)  =  corz3d2(mlat_min+1,:,vind)
               corz3d2(mlat_max,:,vind)  =  corz3d2(mlat+1,:,vind)
            end if
            ! write(*,*)'corz3d(mlat_min,:,vind) ',corz3d(2,:,vind)

   else  ! vdim = 2
            write(*,*)'transform dimensions for GSI 2d'
            do i=1, mlat
               if ( num_bins2d.eq.1  ) then
                   i0 = 1
               else if ( num_bins2d.eq.mlat  ) then
                   i0 = i
               else
                   i0 = min(int(i/imod)+ 1,num_bins2d)
               end if
               cor2d(i+1,vind)  = vertvarce2d(1,i0)
               hwll2d(i+1,vind) = horlen2d(1,i0)
            end do
            hwll2d(1,vind)        = hwll2d(mlat_min+1,vind)
            hwll2d(mlat_max,vind) = hwll2d(mlat+1,vind)
            cor2d(1,vind)         = cor2d(mlat_min+1,vind)
            cor2d(mlat_max,vind)  = cor2d(mlat+1,vind)
            !write(*,*)'hwll2d  ',hwll2d(:,vind)

   end if


   deallocate(horlen2d)
   deallocate(verlen2d)
   deallocate(vertvarce2d)
   deallocate(vertvarce2d_2)

   end if ! nf_err0

   !---------------------------------------------------------
   ! Close NetCdF file 
   !--------------------------------------------------------
   ! nferr = nf_close(rd_ncid)

end subroutine read_be_gsi

subroutine read_regcoeff_gsi(rd_ncid, nsig, num_bins2d, mlat, mlat_min, mlat_max, agvi, bvi, wgvi, sigma)

   implicit none

   integer ,intent(in) :: nsig, mlat, mlat_min, mlat_max,num_bins2d
   real, dimension(mlat_min:mlat_max,1:nsig,1:nsig),intent(inout) :: agvi
   !real*8, dimension(mlat_min:mlat_max,1:nsig,1:nsig) :: agvi8
   real, dimension(mlat_min:mlat_max,1:nsig)     ,intent(inout) :: wgvi
   real, dimension(mlat_min:mlat_max,1:nsig)     ,intent(inout) :: bvi
   real, dimension(1:nsig)     ,intent(inout) :: sigma
   integer, intent(in) :: rd_ncid

   real, allocatable, dimension(:,:,:) :: field3d
   real, allocatable, dimension(:,:) :: field2d
   real, allocatable, dimension(:) :: field1d
   integer :: ii, jj, kk, k2, nferr, imod
   integer :: num_bins
   character (len=1024) :: filename, varname

   include 'netcdf.inc'

   write(*,*)'mlat, nsig : ',mlat, nsig

   num_bins = nsig*num_bins2d
     

   ! Read regcoeff bvi
   allocate(field1d(num_bins)) 
   varname = 'regcoeff_psi_chi'
   call read_field1d(rd_ncid, num_bins, trim(varname), field1d)
   write(*,*)'regcoeff_psi_chi ',field1d
   write(*,*)'num_bins2d ',num_bins2d

   do ii = 1, mlat 
      do kk = 1, nsig
         if ( num_bins2d .eq. 1 ) then
            jj = (kk-1)*num_bins2d
         else
            jj = ii + (kk-1)*num_bins2d
         end if
         bvi(ii+mlat_min,kk) = field1d(jj)
      end do
   end do

   write (*,*)'mlat, num_bins2d ',mlat, num_bins2d,nsig
   imod = int( mlat / num_bins2d )
   write(*,*)'imod ',imod

!   allocate(field3d(nsig,nsig,num_bins2d))
!   field3d = 0.0
!   bvi = 0.0
!   write(*,*)'coucou 1'
!   call read_field3d(rd_ncid, nsig, nsig, num_bins2d, 0, trim(varname), field3d)
!   write(*,*)'coucou 1'
!   do ii = 1, mlat
!      jj = min(int(ii/imod)+ 1,num_bins2d)
!      !write(*,*)'jj, ii ',jj,ii
!      do kk = 1, nsig
!            if ( num_bins2d .eq. 1 ) then
!               bvi(ii+mlat_min,kk) = field3d(kk,kk,1)
!            else if ( num_bins2d .eq. mlat ) then
!               bvi(ii+mlat_min,kk) = field3d(kk,kk,ii)
!               write(*,*)'field3d(kk,kk,ii) ',ii,kk,field3d(kk,kk,ii)
!               !write(*,*)'bvi(ii+mlat_min,kk)',ii,bvi(ii+mlat_min,kk) 
!            else
!               bvi(ii+mlat_min,kk) = field3d(kk,kk,jj)
!            end if
!      end do
!   end do
!stop

!   bvi(mlat_min,:) = bvi(mlat_min+1,:)
!   bvi(mlat_max,:) = bvi(mlat+1,:)
!   deallocate(field3d)

   bvi(mlat_min,:) = bvi(mlat_min+1,:)
   bvi(mlat_max,:) = bvi(mlat+1,:)
   deallocate(field1d)

   ! Read regcoeff wgvi
   allocate(field2d(nsig,num_bins2d))
   varname = 'regcoeff_psi_ps'
   wgvi = 0.0
   call read_field2d(rd_ncid, nsig, num_bins2d, trim(varname), field2d)
   write(6,*)'read ',trim(varname)
   do ii = 1, mlat 
      jj = min(int(ii/imod)+ 1,num_bins2d)
      do kk = 1, nsig
         if ( num_bins2d .eq. 1 ) then
            wgvi(ii+mlat_min,kk) = field2d(kk,1)
         else if ( num_bins2d .eq. mlat ) then
           wgvi(ii+mlat_min,kk) = field2d(kk,ii)  
         else
            wgvi(ii+mlat_min,kk) = field2d(kk,jj)
         end if
      end do
   end do
   wgvi(mlat_min,:) = wgvi(mlat_min+1,:)
   wgvi(mlat_max,:) = wgvi(mlat+1,:)
   deallocate(field2d)

   ! Read regcoeff agvi
   allocate(field3d(nsig,nsig,num_bins2d))
   varname = 'regcoeff_psi_tv'
   if (nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd) /= 0) then
      varname = 'regcoeff_psi_t'
   end if

   field3d = 0.0
   agvi = 0.0
   write(6,*)'read ',trim(varname)
   call read_field3d(rd_ncid, nsig, nsig, num_bins2d, 0, trim(varname), field3d)
   do ii = 1, mlat 
      jj = int( ii / num_bins2d ) + 1
      do kk = 1, nsig
         do k2 = 1, nsig
            if ( num_bins2d .eq. 1 ) then
               agvi(ii+mlat_min,kk,k2) = field3d(kk,k2,1)
            else if ( num_bins2d .eq. mlat ) then
               agvi(ii+mlat_min,kk,k2) = field3d(kk,k2,ii)
            else
               agvi(ii+mlat_min,kk,k2) = field3d(kk,k2,jj)
            end if
         end do
      end do
   end do
   agvi(mlat_min,:,:) = agvi(mlat_min+1,:,:)
   agvi(mlat_max,:,:) = agvi(mlat+1,:,:)
   deallocate(field3d)


   ! Read sigma
   varname = 'znu'
   call read_field1d(rd_ncid, nsig, trim(varname), sigma)
   write(6,*)'',trim(varname)
   write(6,*)'sigma ',sigma


end subroutine read_regcoeff_gsi


subroutine io_input_init(filename, rd_ncid)

      implicit none

      character (len=*), intent(in) :: filename
      integer :: rd_ncid
      integer :: nferr

      include 'netcdf.inc'

      !nferr = nf_open(trim(filename), 0, rd_ncid)
      nferr = nf_open(trim(filename), ior(NF_SHARE,NF_64BIT_OFFSET), rd_ncid)
      if (nferr /= NF_NOERR) then
         write(0,*) ' '
         write(0,*) 'Error opening input file ''', trim(filename), ''''
         write(0,*) ' '
         stop
      end if
      write(*,*)'open ',trim(filename)

end subroutine io_input_init


subroutine read_dimensions(rd_ncid, nlev, nlat, nlon, nvar, num_bins, num_bins2d, StrLen)

   implicit none
   integer, intent(in)    :: rd_ncid
   integer, intent(out)   :: nlev, nlat, nlon, nvar, num_bins, num_bins2d, StrLen
   integer :: nferr, rdVarIDvarnd

   include 'netcdf.inc'

   nferr = nf_inq_dimid(rd_ncid, 'num_bins2d', rdVarIDvarnd)
   nferr = nf_inq_dimlen(rd_ncid, rdVarIDvarnd, num_bins2d)
   nferr = nf_inq_dimid(rd_ncid, 'num_bins', rdVarIDvarnd)
   nferr = nf_inq_dimlen(rd_ncid, rdVarIDvarnd, num_bins)
   nferr = nf_inq_dimid(rd_ncid, 'lev', rdVarIDvarnd)
   nferr = nf_inq_dimlen(rd_ncid, rdVarIDvarnd, nlev)
   nferr = nf_inq_dimid(rd_ncid, 'lat', rdVarIDvarnd)
   nferr = nf_inq_dimlen(rd_ncid, rdVarIDvarnd, nlat)
   nferr = nf_inq_dimid(rd_ncid, 'lon', rdVarIDvarnd)
   nferr = nf_inq_dimlen(rd_ncid, rdVarIDvarnd, nlon)
   nferr = nf_inq_dimid(rd_ncid, 'nvar', rdVarIDvarnd)
   nferr = nf_inq_dimlen(rd_ncid, rdVarIDvarnd, nvar)
   nferr = nf_inq_dimid(rd_ncid, 'StrLen', rdVarIDvarnd)
   nferr = nf_inq_dimlen(rd_ncid, rdVarIDvarnd, StrLen)

   write(*,*)'read dimensions of binning, num_bins, num_bins2d : ',num_bins, num_bins2d
   write(*,*)'read dimensions of grid, lev, lat, lon : ',nlev, nlat, nlon
   write(*,*)'read dimensions of the number of control variables nvar : ',nvar

end subroutine read_dimensions

!! read character

subroutine read_field1dstr(rd_ncid, dim1, StrLen, varname, field1d)
  
   implicit none
   integer, intent(in) :: rd_ncid, StrLen, dim1
   character (len=*), intent(in) :: varname
   character(len=*), dimension(1:dim1), intent(inout) :: field1d
   integer, dimension(2) :: start2, count2
   integer :: nferr, rdVarIDvarnd

   include 'netcdf.inc'

   nferr = nf_inq_varid(rd_ncid, varname, rdVarIDvarnd)
   start2(2) = 1
   count2(2) = dim1
   start2(1) = 1
   count2(1) = StrLen 
   nferr = nf_get_vara_text(rd_ncid, rdVarIDvarnd, start2, count2, field1d )
   call check_err(nferr,varname)

end subroutine  read_field1dstr

!! integer

subroutine read_field0dint(rd_ncid, varname, field0d)

   implicit none
   integer, intent(in) :: rd_ncid
   integer, intent(inout) :: field0d
   character (len=*), intent(in) :: varname
   integer, dimension(1) :: start1, count1
   integer :: nferr, rdVarIDvarnd

   include 'netcdf.inc'

   start1(1) = 1
   count1(1) = 1
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_int( rd_ncid, rdVarIDvarnd, start1, count1, field0d)
   call check_err(nferr,varname)

end subroutine read_field0dint


subroutine read_field1dint(rd_ncid, dim1, varname, field1d)

   implicit none
   integer, intent(in) :: dim1, rd_ncid
   integer, dimension(1:dim1), intent(inout) :: field1d
   character (len=*), intent(in) :: varname
   integer, dimension(1) :: start1, count1
   integer :: nferr, rdVarIDvarnd

   include 'netcdf.inc'

   start1(1) = 1
   count1(1) = dim1
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_int( rd_ncid, rdVarIDvarnd, start1, count1, field1d)
   call check_err(nferr,varname)

end subroutine read_field1dint

subroutine read_field2dint(rd_ncid, dim1, dim2, varname, field2d)

   implicit none
   integer, intent(in) :: dim1, dim2, rd_ncid
   integer, dimension(1:dim1,1:dim2), intent(inout) :: field2d
   character (len=*), intent(in) :: varname
   integer, dimension(2) :: start2, count2
   integer :: nferr, rdVarIDvarnd

   include 'netcdf.inc'

   start2(1) = 1
   count2(1) = dim1
   start2(2) = 1
   count2(2) = dim2
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_int( rd_ncid, rdVarIDvarnd, start2, count2, field2d)
   call check_err(nferr,varname)

end subroutine read_field2dint


!! Real

subroutine read_field0d(rd_ncid, varname, field0d)

   implicit none
   integer, intent(in) :: rd_ncid
   real, intent(inout) :: field0d
   real(kind=8) :: field0d8
   character (len=*), intent(in) :: varname
   integer, dimension(1) :: start1, count1
   integer :: nferr, rdVarIDvarnd

   include 'netcdf.inc'

   start1(1) = 1
   count1(1) = 1
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_double( rd_ncid, rdVarIDvarnd, start1, count1, field0d8)
   field0d = field0d8
   call check_err(nferr,varname)

end subroutine read_field0d

subroutine read_field1d(rd_ncid, dim1, varname, field1d)

   implicit none
   integer, intent(in) :: dim1, rd_ncid
   real, dimension(1:dim1), intent(inout) :: field1d
   real*8, dimension(1:dim1) :: field1d8
   character (len=*), intent(in) :: varname
   integer, dimension(1) :: start1, count1
   integer :: nferr, rdVarIDvarnd

   include 'netcdf.inc'

   start1(1) = 1
   count1(1) = dim1
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_double( rd_ncid, rdVarIDvarnd, start1, count1, field1d8)
   field1d = field1d8
   call check_err(nferr,varname)

end subroutine read_field1d


subroutine read_field2d(rd_ncid, dim1, dim2, varname, field2d)

   implicit none 
   integer, intent(in) :: dim1, dim2, rd_ncid
   real, dimension(1:dim1,1:dim2), intent(inout) :: field2d
   real*8, dimension(1:dim1,1:dim2) :: field2d8
   character (len=*), intent(in) :: varname
   integer, dimension(2) :: start2, count2
   integer :: nferr, rdVarIDvarnd

   include 'netcdf.inc'
  
   start2(1) = 1
   count2(1) = dim1
   start2(2) = 1
   count2(2) = dim2
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   nferr = nf_get_vara_double( rd_ncid, rdVarIDvarnd, start2, count2, field2d8)
   field2d = field2d8
   call check_err(nferr,varname)

end subroutine read_field2d


subroutine read_field3d(rd_ncid, dim1, dim2, dim3, itime, varname, field3d)

   implicit none
   integer, intent(in) :: dim1, dim2, dim3, rd_ncid
   real, dimension(1:dim1,1:dim2,1:dim3), intent(inout) :: field3d
   real*8, dimension(1:dim1,1:dim2,1:dim3) :: field3d8
   character (len=*), intent(in) :: varname
   integer, intent(in) :: itime
   integer, dimension(4) :: start4, count4
   integer :: nferr, rdVarIDvarnd

   include 'netcdf.inc'
 
!      start3(1:3)=start4(1:3)
!      count3(1:3)=count4(1:3)
!      write(*,*)'count3 ',count3
   start4(1) = 1
   count4(1) = dim1
   start4(2) = 1
   count4(2) = dim2
   start4(3) = 1
   count4(3) = dim3
   start4(4) = 1
   count4(4) = 1 


   
   nferr = nf_inq_varid(rd_ncid, trim(varname), rdVarIDvarnd)
   if ( itime == 0 ) then
      nferr = nf_get_vara_double( rd_ncid, rdVarIDvarnd, start4(1:3), count4(1:3), field3d8)
   else
      nferr = nf_get_vara_double( rd_ncid, rdVarIDvarnd, start4, count4, field3d8)
   end if 
   field3d = field3d8
   call check_err(nferr,varname)

end subroutine read_field3d

subroutine check_err(nferr,varname)

   implicit none
   integer, intent(in) :: nferr
   character(len=*), optional, intent(in) :: varname
   include "netcdf.inc"

   if (nferr /= NF_NOERR) then
         write(0,*) ' '
         write(0,*) 'Error nferr ', nferr
         write(0,*) 'nf_strerror ', nf_strerror(nferr)
         if (present(varname)) then
            write(0,*) ' problem ',trim(varname)
         end if
         write(0,*) ' '
         stop
   else
        if (present(varname)) then
            write(0,*) ' read ',trim(varname)
         end if
   end if

end subroutine check_err

!=============================================================

subroutine da_advance_cymdh (start_date, dh, end_date)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character (len=10), intent(in)  :: start_date ! In date (ccyymmddhh).
   integer, intent(in)             :: dh         ! Period to advance (-ve forpast).
   character (len=10), intent(out) :: end_date   ! Out date (ccyymmddhh).

   integer :: ccyy, mm, dd, hh

   read(start_date(1:10), fmt='(i4, 3i2)')  ccyy, mm, dd, hh

   hh = hh + dh

   do while (hh < 0)
      hh = hh + 24
      call da_change_date  (ccyy, mm, dd, -1)
   end do

   do while (hh > 23)
      hh = hh - 24
      call da_change_date  (ccyy, mm, dd, 1)
   end do

   write(unit=end_date(1:10), fmt='(i4.4, 3i2.2)')  ccyy, mm, dd, hh

end subroutine da_advance_cymdh

subroutine da_change_date(ccyy, mm, dd, delta)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(inout) :: ccyy, mm, dd
   integer, intent(in)    :: delta

   integer, dimension(12) :: mmday

   mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   mmday(2) = 28

   if (mod(ccyy,4) == 0) then
      mmday(2) = 29

      if (mod(ccyy,100) == 0) then
         mmday(2) = 28
      end if

      if (mod(ccyy,400) == 0) then
         mmday(2) = 29
      end if
   end if

   dd = dd + delta

   if (dd == 0) then
      mm = mm - 1

      if (mm == 0) then
         mm = 12
         ccyy = ccyy - 1
      end if

      dd = mmday(mm)
   elseif (dd .gt. mmday(mm)) then
      dd = 1
      mm = mm + 1
      if (mm > 12) then
         mm = 1
         ccyy = ccyy + 1
      end if
   end if
end subroutine da_change_date


!=============================================================

subroutine specific_qoption2(varq2, ne, start_date, end_date, interval, dim1, dim2, dim3, bins2d0)

  implicit none

  integer, intent(in)                     :: dim1, dim2, dim3, bins2d0
  real*4, intent(inout), dimension(1:bins2d0,1:dim3)   :: varq2
  integer, intent(in)                     :: ne, interval
  character*10, intent(in)                :: start_date, end_date

  integer                                 :: n, ii, jj, kk, counter, member, ee
  character(len=1024)                     :: filename, filename2, filename3
  character*3                             :: ce
  logical                                 :: file_here, file_here2, file_here3
  real, dimension(1:bins2d0,1:dim3)     :: varce8
  real, dimension(1:bins2d0,1:dim3)     :: qcount
  real, dimension(1:dim1,1:dim2,1:dim3) :: field_rh, field_bin, field_bin2 
  character*10                            :: new_date, date, varname
  integer                                 :: cdate, edate        ! Starting, current ending dates.   
  integer                                 :: rd_ncid, rd_ncid2, rd_ncid3

  counter = 0
  qcount(:,:) = 0
  read(start_date(1:10), fmt='(i10)')cdate
  read(end_date(1:10), fmt='(i10)')edate

  if ( bins2d0 .lt. 20  ) then
     write(6,*)'!!!! WARNING, qoption2 form GSI should work with number of bins > 20  !!!!'
     stop
  end if

  date = start_date
  do while ( cdate <= edate )
      do ee = 1, ne
        
        write(UNIT=ce,FMT='(i3.3)')ee 

        varname   = 'rhm'
        filename2  = trim(varname)//'.'//date(1:10)//'.e'//ce
        file_here2 = .false.
        inquire(file=trim(filename2),exist=file_here2)
 
        varname    = 'rh'
        filename   = trim(varname)//'/'//trim(varname)//'.'//date(1:10)//'.e'//ce
        file_here  = .false.
        inquire(file=trim(filename),exist=file_here)
    

        if (  (file_here).and. (file_here2) ) then
        
           ! read data
           call io_input_init(filename, rd_ncid)
           call io_input_init(filename2, rd_ncid2)
           varname='rh'
           call read_field3d(rd_ncid, dim1, dim2, dim3,1, trim(varname),field_rh)
           varname='rhm'
           call read_field3d(rd_ncid2, dim1, dim2, dim3,1, trim(varname),field_bin)
    
           ! compute variance
           do kk=1,dim3
              do jj=1,dim2
                 do ii=1,dim1
                    n = int(field_bin(ii,jj,kk)*20.+1 )
                    if ( n< bins2d0 .and. n> 0 ) then
                       varce8(n,kk)  = varce8(n,kk) + field_rh(ii,jj,kk)*field_rh(ii,jj,kk) 
                       qcount(n,kk)  = qcount(n,kk)+1.
                    end if
!                       write(*,*)'n, count(n,kk) : ',n,qcount(n,kk),field_bin(ii,jj,kk)
                 end do
              end do
           end do
           counter = counter + 1

        else if (.not.(file_here)) then
           write(*,*)'File is missing ',trim(filename)
        else if (.not.(file_here2)) then
           write(*,*)'File is missing ',trim(filename2)
        end if

     end do

     ! Calculate next date:
     call da_advance_cymdh( date, interval, new_date )
     date = new_date
     read(date(1:10), fmt='(i10)')cdate

   end do


   varq2 = 0.0
   do n = 1, bins2d0
      do kk = 1, dim3
         if ( qcount(n,kk).gt.10. ) then
            varq2(n,kk) = varce8(n,kk) / qcount(n,kk)
         else 
            varq2(n,kk) = 0.
         end if
      end do
  end do

!write(*,*)'Holm variance  : ',varq2(1,:)
!write(*,*)'Holm pert numb : ',qcount(1,:)


end subroutine specific_qoption2


subroutine smoothlat(field,nlat,nlevs,rlats,degs)

   implicit none

   real,dimension(nlat)     :: rlats
   real,dimension(nlat,nlevs):: field
   real,dimension(nlat):: field_sm
   real,dimension(nlat,nlat):: weights
   real degs
   integer j,j2,k,nlevs,nlat

! get weights for smoothing in lat direction
   call get_weights(degs,weights,nlat,rlats)

! smooth the field array based on weights computed 
   do k=1,nlevs
     field_sm=0.0
     do j2=2,nlat-1
       do j=2,nlat-1
         field_sm(j)=field_sm(j)+weights(j,j2)*field(j2,k)
       end do
     end do

! redefine field to be smoothed 
     do j=2,nlat-1
       field(j,k)=field_sm(j)
     end do
     field(1,k)=field(2,k)
     field(nlat,k)=field(nlat-1,k)

   end do   !end do loop over levs

 return
 end subroutine smoothlat


 subroutine get_weights(degs,wsmooth,nlat,rlats)

   implicit none

   integer nlat
   real,dimension(nlat)     :: rlats
   real,dimension(nlat)     :: rnorm,slat
   real,dimension(nlat,nlat):: wsmooth

   real sum,errmax,degs,arg,denom
   integer j,jj

  errmax = 0.0
! use difference in sin(lat) to calculate weighting
   do j=2,nlat-1
     slat(j)=sin(rlats(j))
   end do
   denom=1.0/(deg2rad*degs)
   rnorm=0.
   do j=2,nlat-1
     do jj=2,nlat-1
       arg=.5*(denom*(slat(j)-slat(jj)))**2
       wsmooth(j,jj)=exp(-arg)
       rnorm(j)=rnorm(j)+wsmooth(j,jj)
     end do
   end do

   do j=2,nlat-1
     rnorm(j)=1./rnorm(j)
   end do

   errmax=0.
   do j=2,nlat-1
     sum=0.0
     do jj=2,nlat-1
       wsmooth(j,jj)=rnorm(j)*wsmooth(j,jj)
       sum=sum+wsmooth(j,jj)
     end do
     errmax=max(abs(sum-1.),errmax)
     if ( errmax.gt.1e-6 ) then
        write(*,*)'GD errmax : ',j,rlats(j),errmax
        stop
     end if
   end do
    

 return
 end subroutine get_weights




!=============================================================

end program gen_be_nc2gsi 
