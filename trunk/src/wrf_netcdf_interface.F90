#ifdef WRF
subroutine convert_netcdf_mass
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_netcdf_mass   read wrf mass netcdf restart
!   prgmmr: parrish          org: np22                date: 2003-09-05
!
! abstract: using wrf library routines, read a wrf mass core netcdf
!             format restart file.  write the result to temporary netcdf
!             file expected by read_wrf_mass_guess.
!
! program history log:
!   2004-09-10  parrish
!   2004-11-05  treadon - add return code 74 for error stop
!   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
!   2005-07-06  parrish - add read of pint byte address
!   2005-12-09  middlecoff - initialize character variable staggering
!   2006-09-15  treadon - use nhr_assimilation to build local guess filename
!   2010-03-29  Hu  - add code to read 5 cloud/hydrometeor variables for cloud analysis
!   2010-03-29  Hu  - bug fix: replace XICE with SEAICE 
!
!   input argument list:
!
!   output argument list:
!
!     NOTES:  this is beginning of allowing direct connection of gsi to wrf files
!             without seperate external interface.  it is very inefficient, and
!             later versions will be made to reduce the total i/o involved.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_single,i_kind
  use constants, only: h300
  use gsi_4dvar, only: nhr_assimilation
  use rapidrefresh_cldsurf_mod, only: l_cloud_analysis

  implicit none

! Declare local parameters
  real(r_single),parameter:: one_single = 1.0_r_single
  real(r_single),parameter:: r45 = 45.0_r_single

  character(len=120) :: flnm1
  character(len=19)  :: DateStr1
  character(len=6)   :: filename
  integer(i_kind)            :: dh1
  
  integer(i_kind) :: iunit
  
  integer(i_kind) :: i,j,k
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering
  
  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo
 
  integer(i_kind) :: ierr, Status, Status_next_time
  
! binary stuff

! rmse stuff
  
  character (len=31) :: rmse_var
  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single) pt_regional
  real(r_single) rdx,rdy
  real(r_single),allocatable::field3(:,:,:),field2(:,:),field1(:),field2b(:,:),field2c(:,:)
  real(r_single),allocatable::field3u(:,:,:),field3v(:,:,:)
  integer(i_kind),allocatable::ifield2(:,:)
  real(r_single) rad2deg_single
  integer(i_kind) wrf_real
  data iunit / 15 /
  
  wrf_real=104
  end_index=0
  start_index=0
  

!   transfer code from diffwrf for converting netcdf wrf nmm restart file
!      to temporary binary format

  call ext_ncd_ioinit(sysdepinfo,status)
  call set_wrf_debug_level ( 5 )
  
  flnm1='wrf_inout'
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'CONVERT_NETCDF_MASS:  problem with flnm1 = ',&
          trim(flnm1),', Status = ', Status
     call stop2(74) 
  endif
  
  
  write(filename,100) nhr_assimilation
100 format('sigf',i2.2)
  open(iunit,file=filename,form='unformatted')

  write(6,*)' dh1  = ',dh1         !DEDE

!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

  write(6,*)' dh1  = ',dh1         !DEDE

!-------------  get grid info
  write(6,*)'before rmse var T'   !DEDE

  rmse_var='T'

  write(6,*)'after rmse var T'   !DEDE

  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )                !DEDE

  write(6,*)' dh1  = ',dh1         !DEDE
  write(6,*)'rmse_var = ',trim(rmse_var)
  write(6,*)'ndim1 = ',ndim1
  write(6,*)'ordering = ',trim(ordering)
  write(6,*)'staggering = ',trim(staggering)
  write(6,*)'start_index = ',start_index
  write(6,*)'end_index = ',end_index
  write(6,*)'WrfType = ',WrfType
  write(6,*)'ierr  = ',ierr   !DEDE

  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(field2(nlon_regional,nlat_regional),field3(nlon_regional,nlat_regional,nsig_regional+1))
  allocate(field3u(nlon_regional+1,nlat_regional,nsig_regional))
  allocate(field3v(nlon_regional,nlat_regional+1,nsig_regional))
  allocate(field2b(nlon_regional,nlat_regional),field2c(nlon_regional,nlat_regional))
  allocate(ifield2(nlon_regional,nlat_regional))
  allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))
  
  rmse_var='P_TOP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       pt_regional,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' p_top=',pt_regional
  
  write(iunit) iyear,imonth,iday,ihour,iminute,isecond, &
       nlon_regional,nlat_regional,nsig_regional,pt_regional
  
  rmse_var='ZNU'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field1,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,                    &
       start_index,end_index,                   & !dom
       start_index,end_index,                   & !mem
       start_index,end_index,                   & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,znu(k)=',k,field1(k)
  end do
  write(iunit)field1(1:nsig_regional)  ! ZNU
  
  rmse_var='ZNW'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field1,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,                    &
       start_index,end_index,                   & !dom
       start_index,end_index,                   & !mem
       start_index,end_index,                   & !pat
       ierr                                 )
  do k=1,nsig_regional+1
     write(6,*)' k,znw(k)=',k,field1(k)
  end do
  write(iunit)field1(1:nsig_regional+1)  ! ZNW
  
  rmse_var='RDX'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       rdx,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' 1/rdx=',one_single/rdx
  
  rmse_var='RDY'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       rdy,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' 1/rdy=',one_single/rdy
  
  rmse_var='MAPFAC_M'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min mapfac_m=',maxval(field2),minval(field2)
  write(6,*)' max,min MAPFAC_M(:,1)=',maxval(field2(:,1)),minval(field2(:,1))
  write(6,*)' max,min MAPFAC_M(1,:)=',maxval(field2(1,:)),minval(field2(1,:))
  write(6,*)' mapfac_m(1,1),mapfac_m(nlon,1)=',field2(1,1),field2(nlon_regional,1)
  write(6,*)' mapfac_m(1,nlat),mapfac_m(nlon,nlat)=', &
       field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
  field2b=one_single/(field2*rdx)  !DX_MC
  field2c=one_single/(field2*rdy)  !DY_MC
  
  rad2deg_single=r45/atan(one_single)
  rmse_var='XLAT'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min XLAT(:,1)=',maxval(field2(:,1)),minval(field2(:,1))
  write(6,*)' max,min XLAT(1,:)=',maxval(field2(1,:)),minval(field2(1,:))
  write(6,*)' xlat(1,1),xlat(nlon,1)=',field2(1,1),field2(nlon_regional,1)
  write(6,*)' xlat(1,nlat),xlat(nlon,nlat)=', &
       field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
  field2=field2/rad2deg_single
  write(iunit)field2,field2b   !XLAT,DX_MC
  
  rmse_var='XLONG'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min XLONG(:,1)=',maxval(field2(:,1)),minval(field2(:,1))
  write(6,*)' max,min XLONG(1,:)=',maxval(field2(1,:)),minval(field2(1,:))
  write(6,*)' xlong(1,1),xlong(nlon,1)=',field2(1,1),field2(nlon_regional,1)
  write(6,*)' xlong(1,nlat),xlong(nlon,nlat)=', &
       field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
  field2=field2/rad2deg_single
  write(iunit)field2,field2c   !XLONG,DY_MC
  
  rmse_var='MUB'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min MUB=',maxval(field2),minval(field2)
  
  rmse_var='MU'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2b,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min MU=',maxval(field2b),minval(field2b)
  field2=field2b+field2+pt_regional
  write(6,*)' max,min psfc0=',maxval(field2),minval(field2)
  write(iunit)field2   ! psfc0
  
  rmse_var='PHB'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  k=1
  write(6,*)' k,max,min,mid PHB=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
       field3(nlon_regional/2,nlat_regional/2,k)
  write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! PHB (zsfc*g)
  
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  field3=field3+h300
  do k=1,nsig_regional
     write(6,*)' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
     write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! POT TEMP (sensible??)
  end do
  
  rmse_var='QVAPOR'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
     write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
  end do
  
  rmse_var='U'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3u,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,max,min,mid U=',k,maxval(field3u(:,:,k)),minval(field3u(:,:,k)), &
          field3u(nlon_regional/2,nlat_regional/2,k)
     write(iunit)((field3u(i,j,k),i=1,nlon_regional+1),j=1,nlat_regional)   ! U
  end do
  
  rmse_var='V'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3v,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,max,min,mid V=',k,maxval(field3v(:,:,k)),minval(field3v(:,:,k)), &
          field3v(nlon_regional/2,nlat_regional/2,k)
     write(iunit)((field3v(i,j,k),i=1,nlon_regional),j=1,nlat_regional+1)   ! V
  end do
  
  rmse_var='LANDMASK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min sm=',maxval(field2),minval(field2)
  write(6,*)' landmask(1,1),landmask(nlon,1)=',field2(1,1),field2(nlon_regional,1)
  write(6,*)' landmask(1,nlat),landmask(nlon,nlat)=', &
       field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
  write(iunit)field2   !LANDMASK   (1=land, 0=water)
  
  rmse_var='SEAICE'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min SEAICE=',maxval(field2),minval(field2)
  write(iunit)field2   !  SEAICE
  
  rmse_var='SST'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min SST=',maxval(field2),minval(field2)
  write(iunit)field2   !SST
  
  rmse_var='IVGTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min IVGTYP=',maxval(ifield2),minval(ifield2)
  write(iunit)ifield2   !IVGTYP
  
  rmse_var='ISLTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min ISLTYP=',maxval(ifield2),minval(ifield2)
  write(iunit)ifield2   !ISLTYP
  
  rmse_var='VEGFRA'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min VEGFRA=',maxval(field2),minval(field2)
  write(iunit)field2   !VEGFRA
  
  rmse_var='SNOW'    !
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min SNOW=',maxval(field2),minval(field2)
  write(iunit)field2   !SNOW
  
  rmse_var='U10'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min U10=',maxval(field2),minval(field2)
  write(iunit)field2   !U10
  
  rmse_var='V10'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min V10=',maxval(field2),minval(field2)
  write(iunit)field2   !V10
  
  rmse_var='SMOIS'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  k=1
  write(6,*)' k,max,min,mid SMOIS=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
       field3(nlon_regional/2,nlat_regional/2,1)
  write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! SMOIS
  
  rmse_var='TSLB'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  k=1
  write(6,*)' k,max,min,mid TSLB=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
       field3(nlon_regional/2,nlat_regional/2,1)
  write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! TSLB
  
  rmse_var='TSK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min TSK=',maxval(field2),minval(field2)
  write(iunit)field2   !TSK

  if(l_cloud_analysis) then
    rmse_var='QCLOUD'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index=',end_index
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index,               & !dom
         start_index,end_index,               & !mem
         start_index,end_index,               & !pat
         ierr                                 )
    do k=1,nsig_regional
       write(6,*)' k,max,min,mid Qc=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &         
                field3(nlon_regional/2,nlat_regional/2,k)
       write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qc
    end do

    rmse_var='QRAIN'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index=',end_index
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index,               & !dom
         start_index,end_index,               & !mem
         start_index,end_index,               & !pat
         ierr                                 )
    do k=1,nsig_regional
       write(6,*)' k,max,min,mid Qr=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &         
                field3(nlon_regional/2,nlat_regional/2,k)
       write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qr
    end do

    rmse_var='QSNOW'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index=',end_index
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index,               & !dom
         start_index,end_index,               & !mem
         start_index,end_index,               & !pat
         ierr                                 )
    do k=1,nsig_regional
       write(6,*)' k,max,min,mid Qs=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                  field3(nlon_regional/2,nlat_regional/2,k)
       write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qs
    end do

    rmse_var='QICE'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index=',end_index
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index,               & !dom
         start_index,end_index,               & !mem
         start_index,end_index,               & !pat
         ierr                                 )
    do k=1,nsig_regional
       write(6,*)' k,max,min,mid Qi=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                  field3(nlon_regional/2,nlat_regional/2,k)
       write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qi
    end do

    rmse_var='QGRAUP'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index=',end_index
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index,               & !dom
         start_index,end_index,               & !mem
         start_index,end_index,               & !pat
         ierr                                 )
    do k=1,nsig_regional
       write(6,*)' k,max,min,mid Qg=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                  field3(nlon_regional/2,nlat_regional/2,k)
       write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qg
    end do

    rmse_var='RAD_TTEN_DFI'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index=',end_index
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index,               & !dom
         start_index,end_index,               & !mem
         start_index,end_index,               & !pat
         ierr                                 )
    field3=-20.0_r_single
    field3(:,:,nsig_regional)=-10.0_r_single
    do k=1,nsig_regional
       write(6,*)' k,max,min,mid TTEN=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                field3(nlon_regional/2,nlat_regional/2,k)
       write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! TTEN
    end do

  endif   ! l_cloud_analysis

  deallocate(field1,field2,field2b,field2c,ifield2,field3,field3u,field3v)
  close(iunit)
  call ext_ncd_ioclose(dh1, Status)
  
end subroutine convert_netcdf_mass

subroutine convert_netcdf_nmm(update_pint,ctph0,stph0,tlm0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_netcdf_nmm    read wrf nmm netcdf restart
!   prgmmr: parrish          org: np22                date: 2003-09-05
!
! abstract: using wrf library routines, read a wrf nmm netcdf
!             format restart file.  write the result to temporary netcdf
!             file expected by read_wrf_nmm_guess.
!
! program history log:
!   2004-09-10  parrish
!   2004-11-05  treadon - add return code 74 for error stop
!   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
!   2005-07-06  parrish - add read of pint byte address
!   2005-10-17  parrish - add ctph0,stph0,tlm0
!   2005-12-09  middlecoff - initialize character variable staggering
!   2006-09-15  treadon - use nhr_assimilation to build local guess filename
!
!   input argument list:
!     update_pint:   false on input
!
!   output argument list:
!     update_pint:   true on output if field pint (non-hydrostatic pressure in nmm model)
!                     is available, in which case pint gets updated by analysis increment of pd,
!                      the nmm hydrostatic pressure thickness variable.
!     ctph0,stph0:   cos and sin thp0, earth lat of center of nmm grid (0 deg lat in rotated nmm coordinate)
!                      (used by calctends routines)
!     tlm0
!
!     NOTES:  this is beginning of allowing direct connection of gsi to wrf files
!             without seperate external interface.  it is very inefficient, and
!             later versions will be made to reduce the total i/o involved.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!  create internal binary file from netcdf format wrf restart file

  use kinds, only: r_single,i_kind,r_kind
  use gsi_4dvar, only: nhr_assimilation
  use constants, only: half,rad2deg
! use wrf_data
  implicit none
! include 'wrf_status_codes.h'
! include 'netcdf.inc'

  logical     ,intent(inout) :: update_pint
  real(r_kind),intent(  out) :: ctph0,stph0,tlm0

  character(len=120) :: flnm1
  character(len=19)  :: DateStr1
  character(len=6)   :: filename
  integer(i_kind)            :: dh1
  
  integer(i_kind) :: iunit
  
  integer(i_kind) :: i,j,k
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index1
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering

  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo

  integer(i_kind) :: ierr, Status, Status_next_time

! binary stuff

! rmse stuff

  character (len=31) :: rmse_var
  
  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single) pt_regional,pdtop_regional,dy_nmm
  real(r_single) dlmd_regional,dphd_regional
  real(r_single),allocatable::field3(:,:,:),field2(:,:),field1(:),field2b(:,:)
  integer(i_kind),allocatable::ifield2(:,:)
  integer(i_kind) wrf_real
  data iunit / 15 /
  wrf_real=104
  end_index1=0


!   transfer code from diffwrf for converting netcdf wrf nmm restart file
!      to temporary binary format

  call ext_ncd_ioinit(sysdepinfo,status)
  call set_wrf_debug_level ( 1 )
  
  flnm1='wrf_inout'
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'CONVERT_NETCDF_NMM:  problem with flnm1 = ',&
          trim(flnm1),', Status = ', Status
     call stop2(74)
  endif
  
  
  write(filename,100) nhr_assimilation
100 format('sigf',i2.2)
  open(iunit,file=filename,form='unformatted')


!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond
  
!-------------  get grid info
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  nlon_regional=end_index1(1)
  nlat_regional=end_index1(2)
  nsig_regional=end_index1(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(field2(nlon_regional,nlat_regional),field3(nlon_regional,nlat_regional,nsig_regional+1))
  allocate(field2b(nlon_regional,nlat_regional),ifield2(nlon_regional,nlat_regional))
  allocate(field1(max(nlon_regional,nlat_regional,nsig_regional+1)))
  rmse_var='SMC'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  
  rmse_var='DLMD'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       dlmd_regional,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' dlmd=',dlmd_regional
  
  rmse_var='DPHD'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       dphd_regional,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' dphd=',dphd_regional
  
  rmse_var='PT'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       pt_regional,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' pt=',pt_regional
  rmse_var='PDTOP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       pdtop_regional,WRF_REAL,0,0,0,ordering,       &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' pdtop=',pdtop_regional
  
  write(iunit) iyear,imonth,iday,ihour,iminute,isecond, &
       nlon_regional,nlat_regional,nsig_regional, &
       dlmd_regional,dphd_regional,pt_regional,pdtop_regional
  rmse_var='DETA1'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field1,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,                    &
       start_index,end_index1,                   & !dom
       start_index,end_index1,                   & !mem
       start_index,end_index1,                   & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,deta1(k)=',k,field1(k)
  end do
  write(iunit)field1(1:nsig_regional)  ! DETA1
  
  rmse_var='AETA1'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field1,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,                    &
       start_index,end_index1,                   & !dom
       start_index,end_index1,                   & !mem
       start_index,end_index1,                   & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,aeta1(k)=',k,field1(k)
  end do
  write(iunit)field1(1:nsig_regional)  ! AETA1
  
  rmse_var='ETA1'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field1,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,                    &
       start_index,end_index1,                   & !dom
       start_index,end_index1,                   & !mem
       start_index,end_index1,                   & !pat
       ierr                                 )
  do k=1,nsig_regional+1
     write(6,*)' k, eta1(k)=',k,field1(k)
  end do
  write(iunit)field1(1:nsig_regional+1)  !  ETA1
  
  rmse_var='DETA2'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field1,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,                    &
       start_index,end_index1,                   & !dom
       start_index,end_index1,                   & !mem
       start_index,end_index1,                   & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,deta2(k)=',k,field1(k)
  end do
  write(iunit)field1(1:nsig_regional)  ! DETA2
  
  rmse_var='AETA2'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field1,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,                    &
       start_index,end_index1,                   & !dom
       start_index,end_index1,                   & !mem
       start_index,end_index1,                   & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,aeta2(k)=',k,field1(k)
  end do
  write(iunit)field1(1:nsig_regional)  ! AETA2
  
  rmse_var='ETA2'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field1,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,                    &
       start_index,end_index1,                   & !dom
       start_index,end_index1,                   & !mem
       start_index,end_index1,                   & !pat
       ierr                                 )
  do k=1,nsig_regional+1
     write(6,*)' k,eta2(k)=',k,field1(k)
  end do
  write(iunit)field1(1:nsig_regional+1)  ! ETA2
  
  rmse_var='GLAT'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min GLAT=',rad2deg*maxval(field2),rad2deg*minval(field2)
  write(6,*)' glat(1,1),glat(nlon,1)=',rad2deg*field2(1,1),rad2deg*field2(nlon_regional,1)
  write(6,*)' glat(1,nlat),glat(nlon,nlat)=', &
       rad2deg*field2(1,nlat_regional),rad2deg*field2(nlon_regional,nlat_regional)
  write(6,*)' my guess at tph0d = ',rad2deg*field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
  ctph0=cos(field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
  stph0=sin(field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
  
  rmse_var='DX_NMM'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2b,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min DX_NMM=',maxval(field2b),minval(field2b)
  write(6,*)' dx_nmm(1,1),dx_nmm(nlon,1)=',field2b(1,1),field2b(nlon_regional,1)
  write(6,*)' dx_nmm(1,nlat),dx_nmm(nlon,nlat)=', &
       field2b(1,nlat_regional),field2b(nlon_regional,nlat_regional)
  write(iunit)field2,field2b   !GLAT,DX_NMM
  
  rmse_var='GLON'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min GLON=',rad2deg*maxval(field2),rad2deg*minval(field2)
  write(6,*)' glon(1,1),glon(nlon,1)=',rad2deg*field2(1,1),rad2deg*field2(nlon_regional,1)
  write(6,*)' glon(1,nlat),glon(nlon,nlat)=', &
       rad2deg*field2(1,nlat_regional),rad2deg*field2(nlon_regional,nlat_regional)
  write(6,*)' my guess at tlm0d = ',rad2deg*field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
  tlm0=half*(field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)+ &
           field2(2+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
  
  rmse_var='DY_NMM'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       dy_nmm,WRF_REAL,0,0,0,ordering,       &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' dy_nmm=',dy_nmm
  field2b=dy_nmm
  write(iunit)field2,field2b   !GLON,DY_NMM
  
  rmse_var='PD'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min pd=',maxval(field2),minval(field2)
  write(iunit)field2   !PD
  
  rmse_var='FIS'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min FIS=',maxval(field2),minval(field2)
  write(iunit)field2   ! FIS

  update_pint=.false.
  rmse_var='PINT'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  if(ierr==0) then
    update_pint=.true.
    write(6,*)' rmse_var=',trim(rmse_var)
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    do k=1,nsig_regional+1
       write(6,*)' k,max,min,mid PINT=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
       write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! PINT
    end do
  end if
  
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' ierr,rmse_var=',ierr,trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
     write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
  end do
  
  rmse_var='Q'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
     write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
  end do
  
  rmse_var='U'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,max,min,mid U=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
     write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! U
  end do
  
  rmse_var='V'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  do k=1,nsig_regional
     write(6,*)' k,max,min,mid V=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
     write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
  end do
  
  rmse_var='SM'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min sm=',maxval(field2),minval(field2)
  write(6,*)' sm(1,1),sm(nlon,1)=',field2(1,1),field2(nlon_regional,1)
  write(6,*)' sm(1,nlat),sm(nlon,nlat)=', &
       field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
  write(iunit)field2   !SM
  
  rmse_var='SICE'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &

       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min SICE=',maxval(field2),minval(field2)
  write(iunit)field2   !SICE
  
  rmse_var='SST'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min SST=',maxval(field2),minval(field2)
  write(iunit)field2   !SST
  
  rmse_var='IVGTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min IVGTYP=',maxval(ifield2),minval(ifield2)
  write(iunit)ifield2   !IVGTYP
  
  rmse_var='ISLTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min ISLTYP=',maxval(ifield2),minval(ifield2)
  write(iunit)ifield2   !ISLTYP
  
  rmse_var='VEGFRC'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min VEGFRC=',maxval(field2),minval(field2)
  write(iunit)field2   !VEGFRC
  
  rmse_var='SNO'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min SNO=',maxval(field2),minval(field2)
  write(iunit)field2   !SNO
  
  rmse_var='U10'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min U10=',maxval(field2),minval(field2)
  write(iunit)field2   !U10
  
  rmse_var='V10'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min V10=',maxval(field2),minval(field2)
  write(iunit)field2   !V10
  
  rmse_var='SMC'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  k=1
  write(6,*)' k,max,min,mid SMC=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
       field3(nlon_regional/2,nlat_regional/2,1)
  write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! SMC
  
  rmse_var='STC'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  k=1
  write(6,*)' k,max,min,mid STC=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
       field3(nlon_regional/2,nlat_regional/2,1)
  write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! STC
  
  rmse_var='TSK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min TSK=',maxval(field2),minval(field2)
  write(iunit)field2   !TSK
  
  deallocate(field1,field2,field2b,ifield2,field3)
  close(iunit)
  call ext_ncd_ioclose(dh1, Status)
  
end subroutine convert_netcdf_nmm

subroutine update_netcdf_mass
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_netcdf_mass     create internal binary file from netcdf format wrf restart file
!   prgmmr:
!
! abstract: create internal binary file from netcdf format wrf restart file
!
! program history log:
!   2004-11-05  treadon - add return code 75 for error stop
!   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
!   2005-12-09  middlecoff - initialize character variable staggering and removed staggering1,staggering2
!   2006-04-06  middlecoff - added read of SM and SICE to match the writes in wrwrfmass.F90  
!                            and read in the rest of the fields to match the writes in wrwrfmass.F90  
!   2006-06-09  liu - bug fix: replace SM and SICE with SMOIS and XICE
!   2009-08-14  lueken - update documentation
!   2010-03-29  Hu  - add code to update 5 cloud/hydrometeor variables for cloud analysis
!   2008-03-29  Hu  - bug fix: replace XICE with SEAICE and 
!                              comment out update for SMOIS (the actually 
!                              variable is Landmask there).
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_single,i_kind
  use constants, only: h300
  use rapidrefresh_cldsurf_mod, only: l_cloud_analysis

  implicit none

! Declare local parameters

  character(len=120) :: flnm1,flnm2
  character(len=19)  :: DateStr1
  integer(i_kind)            :: dh1

  integer(i_kind) :: iunit

  integer(i_kind) :: i,j,k
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index1
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering

  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo


  integer(i_kind) :: ierr, Status, Status_next_time

! binary stuff

! rmse stuff

  character (len=31) :: rmse_var

  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single) pt_regional,pdtop_regional,dy_nmm
  real(r_single),allocatable::field3(:,:,:),field2(:,:),field1(:),field2b(:,:)
  real(r_single),allocatable::field3u(:,:,:),field3v(:,:,:)
  integer(i_kind),allocatable::ifield2(:,:)
  integer(i_kind) wrf_real
  data iunit / 15 /
  wrf_real=104
  end_index1=0


!   transfer code from diffwrf for converting netcdf wrf nmm restart file
!      to temporary binary format

!
!           update mass core netcdf file with analysis variables from 3dvar
!
  flnm1='wrf_inout'
  call ext_ncd_open_for_update( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'UPDATE_NETCDF_MASS:  problem with flnm1 = ',&
          trim(flnm1),', Status = ', Status
     call stop2(75)
  endif
        
  
  close(51)
  flnm2='siganl'
  open(iunit,file=flnm2,form='unformatted')

     
!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!-------------  get grid info
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
                               start_index,end_index1, WrfType, ierr    )
  nlon_regional=end_index1(1)
  nlat_regional=end_index1(2)
  nsig_regional=end_index1(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(field2(nlon_regional,nlat_regional),field3(nlon_regional,nlat_regional,nsig_regional))
  allocate(field3u(nlon_regional+1,nlat_regional,nsig_regional))
  allocate(field3v(nlon_regional,nlat_regional+1,nsig_regional))
  allocate(field2b(nlon_regional,nlat_regional))
  allocate(ifield2(nlon_regional,nlat_regional))
  allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))
  
  rmse_var='P_TOP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       pt_regional,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' p_top=',pt_regional
  read(iunit) ! iyear,imonth,iday,ihour,iminute,isecond, &
!        nlon_regional,nlat_regional,nsig_regional,pt_regional
  
  read(iunit) ! field1(1:nsig_regional)  ! AETA1  (ZNU)
  
  read(iunit) ! field1(1:nsig_regional+1)  !  ETA1 (ZNW)
  
  read(iunit) ! field2   !XLAT,DX_MC
  
  read(iunit) ! field2   !XLONG,DY_MC
  
  rmse_var='MUB'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min MUB=',maxval(field2),minval(field2)
  
  read(iunit)   field2b   !psfc
  write(6,*)' max,min psfc=',maxval(field2b),minval(field2b)
  field2b=field2b-field2-pt_regional
  write(6,*)' max,min MU=',maxval(field2b),minval(field2b)
  rmse_var='MU'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2b,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  read(iunit) ! field2   ! PHB (FIS)
  
  do k=1,nsig_regional
     read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
     write(6,*)' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
  end do
  field3=field3-h300
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  do k=1,nsig_regional
     read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
     write(6,*)' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='QVAPOR'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  do k=1,nsig_regional
     read(iunit)((field3u(i,j,k),i=1,nlon_regional+1),j=1,nlat_regional)   ! U
     write(6,*)' k,max,min,mid U=',k,maxval(field3u(:,:,k)),minval(field3u(:,:,k)), &
          field3u(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='U'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3u,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  do k=1,nsig_regional
     read(iunit)((field3v(i,j,k),i=1,nlon_regional),j=1,nlat_regional+1)   ! V
     write(6,*)' k,max,min,mid V=',k,maxval(field3v(:,:,k)),minval(field3v(:,:,k)), &
          field3v(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='V'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3v,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  read(iunit)   field2   !  LANDMASK
  write(6,*)'max,min LANDMASK=',maxval(field2),minval(field2)

  read(iunit)   field2   ! SEAICE
  write(6,*)'max,min SEAICE=',maxval(field2),minval(field2)
  rmse_var='SEAICE'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )

  read(iunit)   field2   !SST
  write(6,*)' max,min SST=',maxval(field2),minval(field2)
  rmse_var='SST'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
! Read in the rest of the fields
!  do k=4,10
  do k=4,11   ! corrected according to Ming Hu's finding

     read(iunit) field2 !Rest of the fields
     write(6,*)'read max,min REST',k,maxval(field2),minval(field2)
  end do

  read(iunit)   field2   !TSK
  write(6,*)' max,min TSK=',maxval(field2),minval(field2)
  rmse_var='TSK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  if (l_cloud_analysis) then
    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qc
       write(6,*)' k,max,min,mid Qc=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QCLOUD'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qr
       write(6,*)' k,max,min,mid Qr=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QRAIN'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qs
       write(6,*)' k,max,min,mid Qs=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QSNOW'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qi
       write(6,*)' k,max,min,mid Qi=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QICE'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qg
       write(6,*)' k,max,min,mid Qg=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QGRAUP'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! TTEN 
       write(6,*)' k,max,min,mid TTEN=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='RAD_TTEN_DFI'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

  endif     ! l_cloud_analysis

  deallocate(field1,field2,field2b,ifield2,field3,field3u,field3v)
  call ext_ncd_ioclose(dh1, Status)
  close(iunit)
  
end subroutine update_netcdf_mass

subroutine update_netcdf_nmm
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_netcdf_nmm   create internal binary file from netcdf format wrf restart file
!   pgrmmr:
!
! abstract: create internal binary file from netcdf format wrf restart file
!
! program history log:
!   2004-11-05  treadon - add return code 75 for error stop
!   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
!   2005-12-09  middlecoff - read in pint if update_pint=.true.
!   2005-12-09  middlecoff - initialize character variable staggering and removed staggering1,staggering2
!   2009-08-14  lueken - update documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_single,i_kind
  use regional_io, only: update_pint
! use wrf_data
  implicit none
! include 'wrf_status_codes.h'
! include 'netcdf.inc'

  character(len=120) :: flnm1,flnm2
  character(len=19)  :: DateStr1
  integer(i_kind)            :: dh1

  integer(i_kind) :: iunit

  integer(i_kind) :: i,j,k
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index1
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering

  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo

  
  integer(i_kind) :: ierr, Status, Status_next_time

! binary stuff

! rmse stuff

  character (len=31) :: rmse_var
  integer(i_kind) :: nallo

  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single) pt_regional,pdtop_regional,dy_nmm
  real(r_single),allocatable::field3(:,:,:),field2(:,:),field1(:)
  integer(i_kind),allocatable::ifield2(:,:)
  integer(i_kind) wrf_real
  data iunit / 15 /
  wrf_real=104
  start_index=0
  end_index1=0


!   transfer code from diffwrf for converting netcdf wrf nmm restart file
!      to temporary binary format

!
!           update nmm netcdf file with analysis variables from 3dvar
!
  flnm1='wrf_inout'
  call ext_ncd_open_for_update( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'UPDATE_NETCDF_NMM:  problem with flnm1 = ',&
          trim(flnm1),', Status = ', Status
     call stop2(75)
  endif
  
     
  close(51)
  flnm2='siganl'
  open(iunit,file=flnm2,form='unformatted')

!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!-------------  get grid info
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &    
                               start_index,end_index1, WrfType, ierr    )      

  nlon_regional=end_index1(1)
  nlat_regional=end_index1(2)
  nsig_regional=end_index1(3)
  nallo = nsig_regional
  if(update_pint) nallo = nallo+1   ! add contribution of PINT
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(field2(nlon_regional,nlat_regional))
  allocate(field3(nlon_regional,nlat_regional,nallo))
  allocate(ifield2(nlon_regional,nlat_regional))
  allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))
  
  read(iunit) ! iyear,imonth,iday,ihour,iminute,isecond, &
       !        nlon_regional,nlat_regional,nsig_regional, &
       !        dlmd_regional,dphd_regional,pt_regional,pdtop_regional
  read(iunit) ! field1(1:nsig_regional)  ! DETA1

  read(iunit) ! field1(1:nsig_regional)  ! AETA1

  read(iunit) ! field1(1:nsig_regional+1)  !  ETA1

  read(iunit) ! field1(1:nsig_regional)  ! DETA2

  read(iunit) ! field1(1:nsig_regional)  ! AETA2

  read(iunit) ! field1(1:nsig_regional+1)  ! ETA2

  read(iunit) ! field2   !GLAT,DX_NMM

  read(iunit) ! field2   !GLON,DY_NMM

  read(iunit)   field2   !PD 
  write(6,*)' max,min pd=',maxval(field2),minval(field2)
  rmse_var='PD'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  read(iunit) ! field2   ! FIS
  
  if(update_pint) then
     do k=1,nsig_regional+1
        read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! PINT
        write(6,*)' k,max,min,mid PINT=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                    field3(nlon_regional/2,nlat_regional/2,k)
     end do
     rmse_var='PINT'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
                                start_index,end_index1, WrfType, ierr    )
     write(6,*)' rmse_var=',trim(rmse_var)
     write(6,*)' ordering=',ordering
     write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
     write(6,*)' ndim1=',ndim1
     write(6,*)' staggering=',staggering
     write(6,*)' start_index=',start_index
     write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),          &
                              field3,WRF_REAL,0,0,0,ordering,       &
                              staggering, dimnames ,                &
                              start_index,end_index1,               & !dom
                              start_index,end_index1,               & !mem
                              start_index,end_index1,               & !pat
                              ierr                                  )
  endif

  do k=1,nsig_regional
     read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
     write(6,*)' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  do k=1,nsig_regional
     read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
     write(6,*)' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='Q'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  do k=1,nsig_regional
     read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! U
     write(6,*)' k,max,min,mid U=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='U'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  do k=1,nsig_regional
     read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
     write(6,*)' k,max,min,mid V=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='V'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  close(iunit)
  deallocate(field1,field2,ifield2,field3)
  call ext_ncd_ioclose(dh1, Status)
  
end subroutine update_netcdf_nmm

#else /* Start no WRF-library block */
subroutine convert_netcdf_nmm
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_netcdf_nmm
!   pgrmmr:
!
! abstract: dummy call... does nothing
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  write(6,*)'CONVERT_NETCDF_NMM:  ***WARNING*** dummy call ... does nothing!'
  return
end subroutine convert_netcdf_nmm

subroutine convert_netcdf_mass
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_netcdf_mass
!   pgrmmr:
!
! abstract: dummy call... does nothing
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  write(6,*)'CONVERT_NETCDF_MASS:  ***WARNING*** dummy call ... does nothing!'
  return
end subroutine convert_netcdf_mass

subroutine update_netcdf_nmm
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_netcdf_nmm
!   pgrmmr:
!
! abstract: dummy call... does nothing
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  write(6,*)'UPDATE_NETCDF_NMM:  ***WARNING*** dummy call ... does nothing!'
  return
end subroutine update_netcdf_nmm


subroutine update_netcdf_mass
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_netcdf_mass
!   pgrmmr:
!
! abstract: dummy call... does nothing
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  write(6,*)'UPDATE_NETCDF_MASS:  ***WARNING*** dummy call ... does nothing!'
  return
end subroutine update_netcdf_mass

#endif /* end NO WRF-library block */

