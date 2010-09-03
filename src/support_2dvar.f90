subroutine convert_binary_2d
!$$$  subprogram documentation block
!
! Adapted from convert_binary_mass
!   prgmmr: pondeca           org: np20                date: 2004-12-13
!
! abstract:
! Read in from restart file of 2dvar-only surface analysis and write
! the result to temporary binary file expected by read_2d_guess.
!
! program history log:
!   2004-12-13  pondeca
!   2006-04-06  middlecoff - change in_unit from 15 to 11 (big endian)
!                            and out_unit 55 to lendian_out
!   2006-09-15  treadon - use nhr_assimilation to build local guess filename
!   2007-03-13  derber - remove unused qsinv2 from jfunc use list
!   2008-04-03  safford - remove unused vars
!   2008-11-04  pondeca - add utility routines for 2dvar applications 
!                         on ndfd grid, ie., rtma applications
!   2008-11-04  pondeca - add routines for hilbert-curve based cross-validation
!   2008-11-04  pondeca - add routines for dew-point computation at
!                         station location. used for qc purposes in 2dvar.
!   2009-02-27  pondeca - add fgat to 2dvar
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_single,i_kind
  use gsi_4dvar, only: nhr_assimilation
  use gsi_io, only: lendian_out
  implicit none

! Declare local parameters
  real(r_single),parameter:: one_single = 1.0_r_single
  real(r_single),parameter:: r45 = 45.0_r_single

  character(6) filename
  character(9) wrfges

  integer(i_kind) in_unit,status_hdr
  integer(i_kind) hdrbuf(512)
  integer(i_kind) n

  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field2(:,:),field2b(:,:)
  real(r_single),allocatable::field2c(:,:)
  integer(i_kind),allocatable::ifield2(:,:)
  real(r_single) rad2deg_single

  data in_unit / 11 /

  n_loop: do n=1,3

     if(n==1)then
        wrfges = 'wrf_inout'
     else
        write(wrfges,'("wrf_inou",i1.1)')n
     endif
     open(in_unit,file=trim(wrfges),form='unformatted')
     write(6,*)' convert_binary_2d: in_unit,lendian_out=',in_unit,lendian_out
 
! Check for valid input file
     read(in_unit,iostat=status_hdr)hdrbuf
     if(n==1)then
        if(status_hdr /= 0) then
           write(6,*)'CONVERT_BINARY_2D:  problem with wrfges = ',&
                trim(wrfges),', Status = ',status_hdr
           call stop2(74)
        endif
     else
        if(status_hdr /= 0) then
           write(6,*)'CONVERT_BINARY_2D:  no off hour guess  ', trim(wrfges)
           close(in_unit)
           cycle n_loop
        endif
     endif

     write(filename,'("sigf",i2.2)')n+nhr_assimilation-1
     write(6,*)' CONVERT_BINARY_2D: in_unit,out_unit=',wrfges,',',filename
     open(lendian_out,file=filename,form='unformatted')
     rewind lendian_out
 
     read(in_unit) iyear,imonth,iday,ihour,iminute,isecond, &
                   nlon_regional,nlat_regional,nsig_regional
     write(6,*)' convert_binary_2d: iy,m,d,h,m,s=',&
                 iyear,imonth,iday,ihour,iminute,isecond
     write(6,*)' convert_binary_2d: nlon,lat,sig_regional=',&
                 nlon_regional,nlat_regional,nsig_regional
     write(lendian_out) iyear,imonth,iday,ihour,iminute,isecond, &
                 nlon_regional,nlat_regional,nsig_regional
 

     allocate(field2(nlon_regional,nlat_regional))
     allocate(field2b(nlon_regional,nlat_regional))
     allocate(field2c(nlon_regional,nlat_regional))
     allocate(ifield2(nlon_regional,nlat_regional))

     read(in_unit) field2b,field2c !DX_MC,DY_MC

!                  XLAT
     rad2deg_single=r45/atan(one_single)
     read(in_unit)field2
     write(6,*)' convert_binary_2d: max,min XLAT(:,1)=',&
                 maxval(field2(:,1)),minval(field2(:,1))
     write(6,*)' convert_binary_2d: max,min XLAT(1,:)=',&
                 maxval(field2(1,:)),minval(field2(1,:))
     write(6,*)' convert_binary_2d: xlat(1,1),xlat(nlon,1)=',&
                 field2(1,1),field2(nlon_regional,1)
     write(6,*)' convert_binary_2d: xlat(1,nlat),xlat(nlon,nlat)=', &
                 field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
     field2=field2/rad2deg_single
     write(lendian_out)field2,field2b    !XLAT,DX_MC
 

!                  XLONG
     read(in_unit)field2
     write(6,*)' convert_binary_2d: max,min XLONG(:,1)=',&
                 maxval(field2(:,1)),minval(field2(:,1))
     write(6,*)' convert_binary_2d: max,min XLONG(1,:)=',&
                 maxval(field2(1,:)),minval(field2(1,:))
     write(6,*)' convert_binary_2d: xlong(1,1),xlong(nlon,1)=',&
                 field2(1,1),field2(nlon_regional,1)
     write(6,*)' convert_binary_2d: xlong(1,nlat),xlong(nlon,nlat)=', &
                 field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
     field2=field2/rad2deg_single
     write(lendian_out)field2,field2c   !  XLONG,DY_MC


     read(in_unit)field2             !  psfc0
     write(6,*)' convert_binary_2d: max,min psfc0=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid psfc0=', &
                 field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2


     read(in_unit)field2             !  PHB (zsfc*g)
     write(6,*)' convert_binary_2d: max,min,mid PHB=', &
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2
 

     read(in_unit)field2             !  T  ! POT TEMP (sensible??)
     write(6,*)' convert_binary_2d: max,min,mid T=',&
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2


     read(in_unit)field2             !  Q
     write(6,*)' convert_binary_2d: max,min,mid Q=',&
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2


     read(in_unit)field2             !  U
     write(6,*)' convert_binary_2d: max,min,mid U=',&
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2


     read(in_unit)field2             !  V
     write(6,*)' convert_binary_2d: max,min,mid V=',&
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2


     read(in_unit)field2             !  LANDMASK  (1=land, 0=water)
     write(6,*)' convert_binary_2d: max,min landmask=', &
                 maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid landmask=', &
                 field2(nlon_regional/2,nlat_regional/2)
     write(6,*)' convert_binary_2d: landmask(1,1),landmask(nlon,1)=', &
                 field2(1,1),field2(nlon_regional,1)
     write(6,*)' convert_binary_2d: landmask(1,nlat),landmask(nlon,nlat)=', &
                 field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
     write(lendian_out)field2


     read(in_unit)field2             !  XICE
     write(6,*)' convert_binary_2d: max,min XICE=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid XICE=', &
                 field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2


     read(in_unit)field2             !  SST
     write(6,*)' convert_binary_2d: max,min SST=',&
                 maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid SST=', &
                 field2(nlon_regional/2,nlat_regional/2)
     write(6,*)' convert_binary_2d: sst(1,1),sst(nlon,1)=',&
                 field2(1,1),field2(nlon_regional,1)
     write(6,*)' convert_binary_2d: sst(1,nlat),sst(nlon,nlat)=', &
                 field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
     write(lendian_out)field2


     read(in_unit)ifield2            !  IVGTYP
     write(6,*)' convert_binary_2d: max,min IVGTYP=', &
                 maxval(ifield2),minval(ifield2)
     write(6,*)' convert_binary_2d: mid IVGTYP=', &
                 ifield2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)ifield2


     read(in_unit)ifield2            !  ISLTYP
     write(6,*)' convert_binary_2d: max,min ISLTYP=', &
                 maxval(ifield2),minval(ifield2)
     write(6,*)' convert_binary_2d: mid ISLTYP=', &
                 ifield2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)ifield2


     read(in_unit)field2             !  VEGFRA
     write(6,*)' convert_binary_2d: max,min VEGFRA=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid VEGFRA=', &
                 field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2


     read(in_unit)field2             !  SNOW
     write(6,*)' convert_binary_2d: max,min SNO=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid SNO=',field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2
 

     read(in_unit)field2             !  U10
     write(6,*)' convert_binary_2d: max,min U10=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid U10=',field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2


     read(in_unit)field2             !  V10
     write(6,*)' convert_binary_2d: max,min V10=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid V10=',field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2
 

     read(in_unit)field2             !  SMOIS
     write(6,*)' convert_binary_2d: max,min SMOIS=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid SMOIS=',field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2


     read(in_unit)field2             !  TSLB
     write(6,*)' convert_binary_2d: max,min TSLB=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid TSLB=',field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2
 

     read(in_unit)field2             !  TSK
     write(6,*)' convert_binary_2d: max,min TSK=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid TSK=',field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2
 
     close(in_unit)
     close(lendian_out)

     deallocate(field2)
     deallocate(field2b)
     deallocate(field2c)
     deallocate(ifield2)
  enddo n_loop
end subroutine convert_binary_2d

!----------------------------------------------------------------------------------
subroutine read_2d_files(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_2d_files   same as read_files, but for files used in 2dvar
!   Adapted from read_wrf_nmm_files
!   prgmmr: pondeca           org: np20                date: 2004-12-27
!
! abstract: figure out available time levels of background fields for
!             later input.
!
! program history log:
!   2004-12-27  pondeca
!   2006-04-06  middlecoff - remove mpi_request_null since not used
!   2008-04-03  safford    - remove uses mpi_status_size, zero_single (not used)
!   2009-10-09  pondeca - with the adding of 4dvar to the gsi, the time reference for the
!                         obs changed. Adjust guess times accordingly by using time_offset
!                         an thus ensure that fgat works properly
!   2010-04-20  jing    - set hrdifsig_all and hrdifsfc_all for non-ESMF cases.
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe
  use guess_grids, only: nfldsig,nfldsfc,ntguessig,ntguessfc,&
       ifilesig,ifilesfc,hrdifsig,hrdifsfc,create_gesfinfo
  use guess_grids, only: hrdifsig_all,hrdifsfc_all
  use gsi_4dvar, only: nhr_assimilation
  use constants, only: zero,one,r60inv
  use obsmod, only: iadate,time_offset
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local parameters
  real(r_kind),parameter:: r0_001=0.001_r_kind

! Declare local variables
  logical(4) fexist
  character(6) filename
  integer(i_kind) in_unit
  integer(i_kind) i,j,iwan,npem1
  integer(i_kind) nhr_half
  integer(i_kind) nminanl,nmings,nming2,ndiff
  integer(i_kind),dimension(5):: idate5
  real(r_kind) hourg,temp
  real(r_kind),dimension(202):: time_ges

!-----------------------------------------------------------------------------
! Start read_2d_files here.

  nhr_half=nhr_assimilation/2
  if(nhr_half*2<nhr_assimilation) nhr_half=nhr_half+1
  npem1=npe-1

  do i=1,202
     time_ges(i) = 999._r_kind
  end do

! Let a single task query the guess files.
  if(mype==npem1) then

!    Convert analysis time to minutes relative to fixed date
     call w3fs21(iadate,nminanl)
     write(6,*)'READ_2d_ FILES:  analysis date,minutes ',iadate,nminanl

!    Check for consistency of times from sigma guess files.
     in_unit=15
     iwan=0
     do i=0,99
        write(filename,100)i
100     format('sigf',i2.2)
        inquire(file=filename,exist=fexist)
        if(fexist)then
           open(in_unit,file=filename,form='unformatted')
           read(in_unit) idate5
           close(in_unit)
           idate5(5)=0
           call w3fs21(idate5,nmings)
           hourg=zero
           nming2=nmings+60*hourg
           write(6,*)' READ_2d_FILES:  sigma guess file, nming2 ',hourg,idate5,nming2
           ndiff=nming2-nminanl
           if(abs(ndiff) > 60*nhr_half ) go to 110
           iwan=iwan+1
           time_ges(iwan) = (nming2-nminanl)*r60inv + time_offset
           time_ges(iwan+100)=i+r0_001
        end if
110     continue
     end do
     time_ges(201)=one
     time_ges(202)=one
     if(iwan > 1)then
        do i=1,iwan
           do j=i+1,iwan
              if(time_ges(j) < time_ges(i))then
                 temp=time_ges(i+100)
                 time_ges(i+100)=time_ges(j+100)
                 time_ges(j+100)=temp
                 temp=time_ges(i)
                 time_ges(i)=time_ges(j)
                 time_ges(j)=temp
              end if
           end do
           if(abs(time_ges(i)-time_offset) < r0_001)time_ges(202) = i
        end do
     end if
     time_ges(201) = iwan+r0_001
  end if

! Broadcast guess file information to all tasks
  call mpi_bcast(time_ges,202,mpi_rtype,npem1,mpi_comm_world,ierror)

  nfldsig   = nint(time_ges(201))
  nfldsfc   = nfldsig

! Allocate space for guess information files
  call create_gesfinfo

  do i=1,nfldsig
     ifilesig(i) = -100
     hrdifsig(i) = zero
  end do

  do i=1,nfldsfc
     ifilesfc(i) = -100
     hrdifsfc(i) = zero
  end do

! Load time information for sigma guess field sinfo into output arrays
  ntguessig = nint(time_ges(202))
  do i=1,nfldsig
     hrdifsig(i) = time_ges(i)
     ifilesig(i) = nint(time_ges(i+100))
     hrdifsig_all(i) = hrdifsig(i)
  end do
  if(mype == 0) write(6,*)' READ_2d_FILES:  sigma fcst files used in analysis  :  ',&
       (ifilesig(i),i=1,nfldsig),(hrdifsig(i),i=1,nfldsig),ntguessig


! Think of guess sfcf files as coinciding with guess sigf files
  ntguessfc = ntguessig
  do i=1,nfldsig
     hrdifsfc(i) = hrdifsig(i)
     ifilesfc(i) = ifilesig(i)
     hrdifsfc_all(i) = hrdifsfc(i)
  end do
  if(mype == 0) write(6,*)' READ_2d_FILES:  surface fcst files used in analysis:  ',&
       (ifilesfc(i),i=1,nfldsfc),(hrdifsfc(i),i=1,nfldsfc),ntguessfc

!
! End of routine
  return
  end subroutine read_2d_files

!----------------------------------------------------------------------------------
subroutine read_2d_guess(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_2d_guess      read 2d interface file
!
!   Adapted from read_wrf_mass_guess
!   prgmmr: pondeca           org: np20                date: 2005-01-06
!
! abstract:   read guess from a binary file created in a previous step
!             that interfaces with the restart file which may be
!             written in a different format. The a-grid is assumed.
!             The guess is read in by complete horizontal fields, one field
!             per processor, in parallel.
!
! program history log:
!   2005-01-06  pondeca
!   2005-11-29  derber - remove external iteration dependent calculations
!   2006-02-02  treadon - remove unused quanities from use guess_grids
!   2006-04-06  middlecoff - changed nfcst from 11 to 15 so nfcst could be used as little endian
!   2006-07-30  kleist - make change to ges_ps from ln(ps)
!   2006-07-28  derber  - include sensible temperature
!   2008-04-02  safford - rm unused vars and uses
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use mpimod, only: mpi_sum,mpi_integer,mpi_real4,mpi_comm_world,npe,ierror
  use guess_grids, only: ges_z,ges_ps,ges_tv,ges_q,ges_cwmr,ges_vor,&
       ges_div,ges_u,ges_v,ges_tvlat,ges_tvlon,ges_qlat,ges_qlon,&
       fact10,soil_type,veg_frac,veg_type,sfct,sno,soil_temp,soil_moi,&
       isli,nfldsig,ifilesig,ges_tsen
  use gridmod, only: lon1,lat1,nlat_regional,nlon_regional,&
       nsig,ijn_s,displs_s,itotsub
  use constants, only: zero,one,grav,fv,zero_single,one_tenth
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local parameters
  real(r_kind),parameter:: r0_01=0.01_r_kind

! Declare local variables
  integer(i_kind) kt,kq,ku,kv

! 2D variable names stuck in here
  integer(i_kind) nfcst

! other internal variables
  real(r_single) tempa(itotsub)
  real(r_single),allocatable::temp1(:,:),temp1u(:,:),temp1v(:,:)
  real(r_single),allocatable::all_loc(:,:,:)
  integer(i_kind),allocatable::itemp1(:,:)
  integer(i_kind),allocatable::igtype(:),jsig_skip(:)
  character(60),allocatable::identity(:)
  character(6) filename
  integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)
  integer(i_kind) ifld,im,jm,lm,num_2d_fields
  integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
  integer(i_kind) i,icount,icount_prev,it,j,k
  integer(i_kind) i_0,i_psfc,i_fis,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smois,i_tslb
  integer(i_kind) i_sm,i_xice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
  integer(i_kind) isli_this
  real(r_kind) psfc_this,sm_this,xice_this
  integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all


!  RESTART FILE input grid dimensions in module gridmod
!      These are the following:
!          im -- number of x-points on C-grid
!          jm -- number of y-points on C-grid
!          lm -- number of vertical levels ( = nsig for now)


  num_doubtful_sfct=0
  if(mype==0) write(6,*)' at 0 in read_2d_guess'


! Big section of operations done only on first outer iteration

  if(mype==0) write(6,*)' at 0.1 in read_2d_guess'

  im=nlon_regional
  jm=nlat_regional
  lm=nsig

! Following is for convenient 2D input
  num_2d_fields=18! Adjust once exact content of RTMA restart file is known
  num_all_fields=num_2d_fields*nfldsig
  num_loc_groups=num_all_fields/npe
  if(mype==0) write(6,'(" at 1 in read_2d_guess, lm            =",i6)')lm
  if(mype==0) write(6,'(" at 1 in read_2d_guess, num_2d_fields=",i6)')num_2d_fields
  if(mype==0) write(6,'(" at 1 in read_2d_guess, nfldsig       =",i6)')nfldsig
  if(mype==0) write(6,'(" at 1 in read_2d_guess, num_all_fields=",i6)')num_all_fields
  if(mype==0) write(6,'(" at 1 in read_2d_guess, npe           =",i6)')npe
  if(mype==0) write(6,'(" at 1 in read_2d_guess, num_loc_groups=",i6)')num_loc_groups
  do
     num_all_pad=num_loc_groups*npe
     if(num_all_pad >= num_all_fields) exit
     num_loc_groups=num_loc_groups+1
  end do
  if(mype==0) write(6,'(" at 1 in read_2d_guess, num_all_pad   =",i6)')num_all_pad
  if(mype==0) write(6,'(" at 1 in read_2d_guess, num_loc_groups=",i6)')num_loc_groups

  allocate(all_loc(lat1+2,lon1+2,num_all_pad))
  allocate(jsig_skip(num_2d_fields))
  allocate(igtype(num_2d_fields))
  allocate(identity(num_2d_fields))

! igtype is a flag indicating whether each input field is h-, u-, or v-grid
! and whether integer or real
!  abs(igtype)=1 for h-grid
!             =2 for u-grid
!             =3 for v-grid
!  igtype < 0 for integer field

  i=0
  i=i+1 ; i_psfc=i                                            ! psfc
  write(identity(i),'("record ",i3,"--psfc")')i
  jsig_skip(i)=3     ! number of files to skip before getting to psfc
  igtype(i)=1
  i=i+1 ; i_fis=i                                             ! sfc geopotential
  write(identity(i),'("record ",i3,"--fis")')i
  jsig_skip(i)=0
  igtype(i)=1
  i_t=i+1
  do k=1,lm
     i=i+1                                                    ! t(k)  (sensible temp)
     write(identity(i),'("record ",i3,"--t(",i2,")")')i,k
     jsig_skip(i)=0
     igtype(i)=1
  end do
  i_q=i+1
  do k=1,lm
     i=i+1                                                    ! q(k)
     write(identity(i),'("record ",i3,"--q(",i2,")")')i,k
     jsig_skip(i)=0 ; igtype(i)=1
  end do
  i_u=i+1
  do k=1,lm
     i=i+1                                                    ! u(k)
     write(identity(i),'("record ",i3,"--u(",i2,")")')i,k
     jsig_skip(i)=0 ; igtype(i)=2
  end do
  i_v=i+1
  do k=1,lm
     i=i+1                                                    ! v(k)
     write(identity(i),'("record ",i3,"--v(",i2,")")')i,k
     jsig_skip(i)=0 ; igtype(i)=3
  end do
  i=i+1   ; i_sm=i                                            ! landmask
  write(identity(i),'("record ",i3,"--sm")')i
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_xice=i                                            ! xice
  write(identity(i),'("record ",i3,"--xice")')i
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_sst=i                                             ! sst
  write(identity(i),'("record ",i3,"--sst")')i
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_ivgtyp=i                                          ! ivgtyp
  write(identity(i),'("record ",i3,"--ivgtyp")')i
  jsig_skip(i)=0 ; igtype(i)=-1
  i=i+1 ; i_isltyp=i                                          ! isltyp
  write(identity(i),'("record ",i3,"--isltyp")')i
  jsig_skip(i)=0 ; igtype(i)=-1
  i=i+1 ; i_vegfrac=i                                         ! vegfrac
  write(identity(i),'("record ",i3,"--vegfrac")')i
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_sno=i                                             ! sno
  write(identity(i),'("record ",i3,"--sno")')i
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_u10=i                                             ! u10
  write(identity(i),'("record ",i3,"--u10")')i
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_v10=i                                             ! v10
  write(identity(i),'("record ",i3,"--v10")')i
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_smois=i                                           ! smois
  write(identity(i),'("record ",i3,"--smois(",i2,")")')i,k
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_tslb=i                                            ! tslb
  write(identity(i),'("record ",i3,"--tslb(",i2,")")')i,k
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_tsk=i                                             ! tsk
  write(identity(i),'("record ",i3,"--sst")')i
  jsig_skip(i)=0 ; igtype(i)=1

! End of stuff from 2D restart file

  allocate(temp1(im,jm),itemp1(im,jm),temp1u(im+1,jm),temp1v(im,jm+1))

  do i=1,npe
     irc_s_reg(i)=ijn_s(mype+1)
  end do
  ird_s_reg(1)=0
  do i=1,npe
     if(i /= 1) ird_s_reg(i)=ird_s_reg(i-1)+irc_s_reg(i-1)
  end do

! Read fixed format input file created from external interface
! This is done by reading in parallel from every pe, and redistributing
! to local domains once for every npe fields read in, using
! mpi_all_to_allv

  nfcst=15
  icount=0
  icount_prev=1
  do it=1,nfldsig
     write(filename,'("sigf",i2.2)')ifilesig(it)
     open(nfcst,file=filename,form='unformatted') ; rewind nfcst
     write(6,*)'READ_2d_GUESS:  open nfcst=',nfcst,' to file=',filename

!    Read, interpolate, and distribute 2D restart fields
     do ifld=1,num_2d_fields
        icount=icount+1
        if(jsig_skip(ifld) > 0) then
           do i=1,jsig_skip(ifld)
              read(nfcst)
           end do
        end if
        if(mype==mod(icount-1,npe)) then
           if(igtype(ifld)==1 .or. igtype(ifld)==2 .or. igtype(ifld)==3) then
              read(nfcst)((temp1(i,j),i=1,im),j=1,jm)
              write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
              call fill_mass_grid2t(temp1,im,jm,tempa,1)
           end if
           if(igtype(ifld) < 0) then
              read(nfcst)((itemp1(i,j),i=1,im),j=1,jm)
              do j=1,jm
                 do i=1,im
                    temp1(i,j)=itemp1(i,j)
                 end do
              end do
              write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
              call fill_mass_grid2t(temp1,im,jm,tempa,1)
           end if
        else
           read(nfcst)
        end if

!       Distribute to local domains everytime we have npe fields
        if(mod(icount,npe) == 0.or.icount==num_all_fields) then
           call mpi_alltoallv(tempa,ijn_s,displs_s,mpi_real4, &
                all_loc(1,1,icount_prev),irc_s_reg,ird_s_reg,mpi_real4,mpi_comm_world,ierror)
           icount_prev=icount+1
        end if
     end do
     close(nfcst)
  end do
!  do kv=i_v,i_v+nsig-1
!  if(mype==0) write(6,*)' at 1.15, kv,mype,j,i,v=', &
!       kv,mype,2,1,all_loc(2,1,kv)
!  end do


! Next do conversion of units as necessary and
! reorganize into WeiYu's format--

  do it=1,nfldsig
     i_0=(it-1)*num_2d_fields
     kt=i_0+i_t-1
     kq=i_0+i_q-1
     ku=i_0+i_u-1
     kv=i_0+i_v-1

     do k=1,nsig
        kt=kt+1
        kq=kq+1
        ku=ku+1
        kv=kv+1
        do i=1,lon1+2
           do j=1,lat1+2
              ges_u(j,i,k,it) = all_loc(j,i,ku)
              ges_v(j,i,k,it) = all_loc(j,i,kv)
              ges_vor(j,i,k,it) = zero
              ges_q(j,i,k,it)   = all_loc(j,i,kq)
              ges_tsen(j,i,k,it)  = all_loc(j,i,kt)
           end do
        end do
     end do
     do i=1,lon1+2
        do j=1,lat1+2
           ges_z(j,i,it)    = all_loc(j,i,i_0+i_fis)/grav ! surface elevation multiplied by g

!          convert input psfc to psfc in mb, and then to log(psfc) in cb

           psfc_this=r0_01*all_loc(j,i,i_0+i_psfc)
           ges_ps(j,i,it)=one_tenth*psfc_this   ! convert from mb to cb
           sno(j,i,it)=all_loc(j,i,i_0+i_sno)
           soil_moi(j,i,it)=all_loc(j,i,i_0+i_smois)
           soil_temp(j,i,it)=all_loc(j,i,i_0+i_tslb)
        end do
     end do

     if(mype==10) write(6,*)' in read_2d_guess, min,max(soil_moi)=', &
        minval(soil_moi),maxval(soil_moi)
     if(mype==10) write(6,*)' in read_2d_guess, min,max(soil_temp)=', &
        minval(soil_temp),maxval(soil_temp)


!    Convert sensible temp to virtual temp
     do k=1,nsig
           do i=1,lon1+2
              do j=1,lat1+2
                 ges_tv(j,i,k,it) = ges_tsen(j,i,k,it) * (one+fv*ges_q(j,i,k,it))
              end do
           end do
        end do
     end do


!    Zero out fields not used
     ges_div=zero
     ges_cwmr=zero
     ges_tvlat=zero
     ges_tvlon=zero
     ges_qlat=zero
     ges_qlon=zero


!    Transfer surface fields
     do it=1,nfldsig
        i_0=(it-1)*num_2d_fields
        do i=1,lon1+2
           do j=1,lat1+2
              fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
              veg_type(j,i,it)=all_loc(j,i,i_0+i_ivgtyp)
              veg_frac(j,i,it)=r0_01*all_loc(j,i,i_0+i_vegfrac)
              soil_type(j,i,it)=all_loc(j,i,i_0+i_isltyp)
              sm_this=zero
              if(all_loc(j,i,i_0+i_sm) /= zero_single) sm_this=one
              xice_this=zero
              if(all_loc(j,i,i_0+i_xice) /= zero_single) xice_this=one

              isli_this=0
              if(xice_this==one) isli_this=2
              if(xice_this==zero.and.sm_this==one) isli_this=1
              isli(j,i,it)=isli_this

              sfct(j,i,it)=all_loc(j,i,i_0+i_sst)
              if(isli(j,i,it) /= 0) sfct(j,i,it)=all_loc(j,i,i_0+i_tsk)
              if(sfct(j,i,it) < one) then

!             For now, replace missing skin temps with 1st sigma level temp
                 sfct(j,i,it)=all_loc(j,i,i_0+i_t)
!                write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
!                     j,i,mype,sfct(j,i,it)
                 num_doubtful_sfct=num_doubtful_sfct+1
              end if
           end do
        end do
     end do

     call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,1,mpi_integer,mpi_sum,&
          0,mpi_comm_world,ierror)
     if(mype==0)     write(6,*)' in read_2d_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype==10) write(6,*)' in read_2d_guess, min,max(sfct)=', &
          minval(sfct),maxval(sfct)
     if(mype==10) write(6,*)' in read_2d_guess, min,max(veg_type)=', &
          minval(veg_type),maxval(veg_type)
     if(mype==10) write(6,*)' in read_2d_guess, min,max(veg_frac)=', &
          minval(veg_frac),maxval(veg_frac)
     if(mype==10) write(6,*)' in read_2d_guess, min,max(soil_type)=', &
          minval(soil_type),maxval(soil_type)
     if(mype==10) write(6,*)' in read_2d_guess, min,max(isli)=', &
          minval(isli),maxval(isli)

     deallocate(all_loc,jsig_skip,igtype,identity)
     deallocate(temp1,itemp1,temp1u,temp1v)


     return
end subroutine read_2d_guess

!----------------------------------------------------------------------------------
subroutine wr2d_binary(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wr2d_binary              write out 2D restart file
! Adpated from wrwrfmassa.
!   prgmmr: pondeca           org: np20                date: 2005-2-7
!
!   abstract: read 2D guess restart interface file, add analysis
!             increment, and write out 2D analysis restart
!             interface file.
!
! program history log:
!   2005-02-07  pondeca
!   2006-04-06  middlecoff - Changed iog from 11 to 15 so iog could be little endian
!                          Changed ioan from 51 to 66 so ioan could be little endian
!   2006-07-28 derber - include sensible temperature
!   2006-07-31  kleist - make change to ges_ps instead of ln(ps)
!   2008-04-03  safford - rm unused vars and uses
!   2010-04-01  treadon - move strip_single to gridmod
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!     no output arguments
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,i_kind
  use guess_grids, only: ntguessfc,ntguessig,ifilesig,sfct,ges_ps,&
       ges_q,ges_u,ges_v,ges_tsen
  use mpimod, only: mpi_comm_world,ierror,mpi_real4
  use gridmod, only: lat2,iglobal,itotsub,update_regsfc,strip_single,&
       lon2,nsig,lon1,lat1,nlon_regional,nlat_regional,ijn,displs_g
  use constants, only: zero_single,r10,r100
  use jfunc, only: qsatg
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local parameters
  real(r_kind),parameter:: r225=225.0_r_kind

! Declare local variables

  integer(i_kind) im,jm,lm
  real(r_single),allocatable::temp1(:),temp1u(:),temp1v(:),tempa(:),tempb(:)
  real(r_single),allocatable::all_loc(:,:,:)
  real(r_single),allocatable::strp(:)
  character(6) filename
  integer(i_kind) iog,ioan,i,j,k,kt,kq,ku,kv,it,i_psfc,i_t,i_q,i_u,i_v
  integer(i_kind) i_sst,i_skt
  integer(i_kind) num_2d_fields,num_all_fields,num_all_pad
  integer(i_kind) regional_time0(6),nlon_regional0,nlat_regional0,nsig0
  real(r_kind) psfc_this
  real(r_single) glon0(nlon_regional,nlat_regional),glat0(nlon_regional,nlat_regional)
  real(r_single) dx_mc0(nlon_regional,nlat_regional),dy_mc0(nlon_regional,nlat_regional)
  real(r_single),allocatable::all_loc_ps(:,:),temp1_ps(:)
  real(r_single),allocatable::all_loc_qsatg(:,:,:),all_loc_prh(:,:,:),temp1_prh(:)

  im=nlon_regional
  jm=nlat_regional
  lm=nsig

  num_2d_fields=3+4*lm
  num_all_fields=num_2d_fields
  num_all_pad=num_all_fields
  allocate(all_loc(lat1+2,lon1+2,num_all_pad))
  allocate(strp(lat1*lon1))
  allocate(all_loc_ps(lat1+2,lon1+2))
  allocate(all_loc_qsatg(lat1+2,lon1+2,nsig),all_loc_prh(lat1+2,lon1+2,nsig))

  i_psfc=1
  i_t=2
  i_q=i_t+lm
  i_u=i_q+lm
  i_v=i_u+lm
  i_sst=i_v+lm
  i_skt=i_sst+1

  allocate(temp1(im*jm),temp1u((im+1)*jm),temp1v(im*(jm+1)))
  allocate(temp1_ps(im*jm))
  allocate(temp1_prh(im*jm))

  if(mype == 0) write(6,*)' at 2 in wr2d_binary'

  iog=15
  ioan=66
  if(mype == 0) then
     write(filename,'("sigf",i2.2)')ifilesig(ntguessig)
     open (iog,file=filename,form='unformatted')
     open (ioan,file='siganl',form='unformatted')
     rewind iog ; rewind ioan
  end if

! Convert analysis variables to 2D variables
  it=ntguessig

! Create all_loc from ges_*
  if(mype == 0) write(6,*)' at 3 in wr2d_binary'
  all_loc=zero_single
  kt=i_t-1
  kq=i_q-1
  ku=i_u-1
  kv=i_v-1
  do k=1,nsig
     kt=kt+1
     kq=kq+1
     ku=ku+1
     kv=kv+1
     do i=1,lon2
        do j=1,lat2
           all_loc(j,i,ku)=ges_u(j,i,k,it)
           all_loc(j,i,kv)=ges_v(j,i,k,it)
           all_loc(j,i,kq)=ges_q(j,i,k,it)
           all_loc(j,i,kt)=ges_tsen(j,i,k,it)   ! sensible temperature
           all_loc_qsatg(j,i,k)=qsatg(j,i,k)
           all_loc_prh(j,i,k)=ges_q(j,i,k,it)/qsatg(j,i,k)
        end do
     end do
  end do
  do i=1,lon2
     do j=1,lat2
        psfc_this=r10*ges_ps(j,i,it)   ! convert from cb to mb
        all_loc(j,i,i_psfc)=r100*psfc_this
        all_loc_ps(j,i)=ges_ps(j,i,it)
     end do
  end do

  if(mype == 0) then
     read(iog) regional_time0,nlon_regional0,nlat_regional0,nsig0
     write(ioan) regional_time0,nlon_regional0,nlat_regional0,nsig0
     read(iog) glat0,dx_mc0
     write(ioan) glat0,dx_mc0
     read(iog) glon0,dy_mc0
     write(ioan) glon0,dy_mc0
  end if

! Update psfc
  if(mype == 0) write(6,*)' at 6 in wr2d_binary'

  allocate(tempa(itotsub),tempb(itotsub))
  if(mype == 0) then
     read(iog)temp1
     temp1_ps=log(temp1/r100/r10)
  endif
  call strip_single(all_loc(1,1,i_psfc),strp,1)
  call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
       tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
  if(mype == 0) then
     call fill_mass_grid2t(temp1,im,jm,tempb,2)
     do i=1,iglobal
        tempa(i)=tempa(i)-tempb(i)
     end do
     call unfill_mass_grid2t(tempa,im,jm,temp1)
     write(ioan)temp1
  end if

  call strip_single(all_loc_ps,strp,1)
  call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
       tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
  if(mype == 0) then
     call fill_mass_grid2t(temp1_ps,im,jm,tempb,2)
     do i=1,iglobal
        tempa(i)=tempa(i)-tempb(i)
     end do
     temp1_ps=zero_single
     call unfill_mass_grid2t(tempa,im,jm,temp1_ps)
  end if

!  FIS read/write
  if(mype == 0) then
     read(iog)temp1
     write(ioan)temp1
  end if

! Update t
  kt=i_t-1
  do k=1,nsig
     kt=kt+1
     if(mype == 0) read(iog)temp1
     call strip_single(all_loc(1,1,kt),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(ioan)temp1
     end if
  end do

! Update q
  kq=i_q-1
  do k=1,nsig
     kq=kq+1
     if(mype == 0) then
        read(iog)temp1
        temp1_prh=temp1
     endif
     call strip_single(all_loc(1,1,kq),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(ioan)temp1
     end if

     call strip_single(all_loc_qsatg(1,1,k),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1_prh,im,jm,tempb,2)
        do i=1,iglobal
           tempb(i)=tempb(i)/tempa(i)
         end do
     end if
     call strip_single(all_loc_prh(1,1,k),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        temp1_prh=zero_single
        call unfill_mass_grid2t(tempa,im,jm,temp1_prh)
     end if
  end do

! Update u
  ku=i_u-1
  do k=1,nsig
     ku=ku+1
     if(mype == 0) read(iog)temp1
     call strip_single(all_loc(1,1,ku),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(ioan)temp1
     end if
  end do

! Update v
  kv=i_v-1
  do k=1,nsig
     kv=kv+1
     if(mype == 0) read(iog)temp1
     call strip_single(all_loc(1,1,kv),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(ioan)temp1
     end if
  end do

  if (mype==0) then
     write(ioan)temp1_ps !increment of ps
     write(ioan)temp1_prh  !increment of pseudo RH
  endif

! Load updated skin temperature array if writing out to analysis file
  if (update_regsfc) then ! set to .false.
     do i=1,lon1+2
        do j=1,lat1+2
           all_loc(j,i,i_sst)=sfct(j,i,ntguessfc)
           all_loc(j,i,i_skt)=sfct(j,i,ntguessfc)
        end do
     end do
  end if

  if(mype == 0) then
! SM
     read(iog)temp1
     write(ioan)temp1
! SICE
     read(iog)temp1
     write(ioan)temp1
  end if

! SST
  if(update_regsfc) then
     if (mype==0) read(iog)temp1
     if (mype==0) write(6,*)' at 9.1 in wr2d_binary,max,min(temp1)=',maxval(temp1),minval(temp1)
     call strip_single(all_loc(1,1,i_sst),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        if(mype == 0) write(6,*)' at 9.2 in wr2d_binary,max,min(tempa)=',maxval(tempa),minval(tempa)
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           if(tempb(i) < (r225)) then
              tempa(i)=zero_single
           else
              tempa(i)=tempa(i)-tempb(i)
           end if
        end do
        if(mype == 0) write(6,*)' at 9.4 in wr2d_binary,max,min(tempa)=',maxval(tempa),minval(tempa)
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(6,*)' at 9.6 in wr2d_binary,max,min(temp1)=',maxval(temp1),minval(temp1)
        write(ioan)temp1
     end if     !endif mype==0
  else
     if(mype==0) then
        read(iog)temp1
        write(ioan)temp1
     end if
  end if   !end if check updatesfc

! REST OF FIELDS
  if (mype == 0) then
     do k=4,11
        read(iog)temp1
        write(ioan)temp1
     end do
  end if

! Update SKIN TEMP
  if(update_regsfc) then
     if (mype==0) read(iog)temp1
     if (mype==0) write(6,*)' at 10.0 in wr2d_binary,max,min(temp1)=',maxval(temp1),minval(temp1)
     call strip_single(all_loc(1,1,i_skt),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           if(tempb(i) < (r225)) then
              tempa(i)=zero_single
           else
              tempa(i)=tempa(i)-tempb(i)
           end if
        end do
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(ioan)temp1
     end if
  else
     if (mype == 0) then
        read(iog)temp1
        write(ioan)temp1
     end if
  end if

  if (mype==0) then
     close(iog)
     close(ioan)
  endif

! Write out qsatg for gsi-2dvar post-processing purposes
  do k=1,nsig
     call strip_single(all_loc_qsatg(1,1,k),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        temp1=zero_single
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        open (94,file='bckg_qsat.dat',form='unformatted')
        write(94) temp1
        close(94)
     end if
  end do

  deallocate(all_loc)
  deallocate(all_loc_ps)
  deallocate(temp1_ps)
  deallocate(temp1)
  deallocate(tempa)
  deallocate(tempb)
  deallocate(temp1u)
  deallocate(temp1v)
  deallocate(temp1_prh)
  deallocate(all_loc_qsatg)
  deallocate(all_loc_prh)
  deallocate(strp)

end subroutine wr2d_binary
!----------------------------------------------------------------------------------
module ndfdgrids
!$$$ module documentation block
!           .      .    .                                       .
! module:   ndfdgrids
!   prgmmr: pondeca          org: np23                date: 2008-11-04
!
! abstract: get navigational information pertaining to ndfd grid and
!           initialize other variables used with the rtma applications
!           of the gsi.
!
! program history log:
!   2008-11-04  pondeca - consolidate scattered routines into current
!                         module
!
! subroutines included:
!   sub init_ndfdgrid
!   sub ndfdgrid_info
!   sub latlon_to_grid0
!   sub terrain_slmask
!   sub adjust_error
!   sub destroy_ndfdgrid
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_single,r_kind

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_ndfdgrid
  public :: ndfdgrid_info
  public :: latlon_to_grid0
  public :: terrain_slmask
  public :: adjust_error
  public :: destroy_ndfdgrid

  character(60) cgrid
  integer(i_kind) nx,ny
  real(r_single),allocatable::slmask(:,:)
  real(r_single),allocatable::terrain(:,:)
  real(r_kind) da8,alat18,elon18,elonv8,alatan8,xx8,yy8

  real(r_kind) oberrinflfact
  integer(i_kind) ineighbour,jneighbour
  logical ladjusterr
  logical fexist

contains

subroutine init_ndfdgrid
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: get navigational information pertaining to ndfd grid and
!           initialize other variables used with the rtma applications
!           of the gsi.
!
! program history log:
!   2008-11-04  pondeca
!
! input argument list:
!
! output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: five
  implicit none

  namelist/parmcardreadprepb/cgrid,ladjusterr,oberrinflfact, & 
                             ineighbour,jneighbour 

  cgrid='conus'
  ladjusterr=.false.
  oberrinflfact=five
  ineighbour=3
  jneighbour=3

  inquire(file='parmcard_input',exist=fexist)
  if (fexist) then
     open(55,file='parmcard_input',form='formatted')
     read(55,parmcardreadprepb)
     close(55)
  endif

  print*,'in init_ndfdgrid: cgrid=',cgrid
  print*,'in init_ndfdgrid: ladjusterr =',ladjusterr
  print*,'in init_ndfdgrid: oberrinflfact=',oberrinflfact
  print*,'in init_ndfdgrid: ineighbour=',ineighbour
  print*,'in init_ndfdgrid: jneighbour=',jneighbour

  call ndfdgrid_info

  print*,'in init_ndfdgrid: nx,ny,gridspacing=',nx,ny,da8
  print*,'in init_ndfdgrid: alat18,elon18,elonv8,alatan=',& 
                            alat18,elon18,elonv8,alatan8

  allocate(slmask(nx,ny))
  open (55,file='rtma_slmask.dat',form='unformatted')
  read(55) slmask
  close(55)

  allocate(terrain(nx,ny))
  open (55,file='rtma_terrain.dat',form='unformatted')
  read(55) terrain
  close(55)

end subroutine init_ndfdgrid

subroutine ndfdgrid_info
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: contains navigational information pertaining to ndfd grid
!
! program history log:
!   2008-11-04  pondeca  
!
! input argument list:
!
! output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none
       
  if (trim(cgrid) == 'conus') then
     nx=1073
     ny=689
     alat18=20.192_r_kind
     elon18=238.446_r_kind
     da8=5079.406_r_kind
     elonv8=265.000_r_kind
     alatan8=25.000_r_kind

  elseif (trim(cgrid) == 'alaska') then 
     nx=825
     ny=553
     alat18=40.530101_r_kind
     elon18=181.429000_r_kind
     da8=5953.125_r_kind
     elonv8=210.000000_r_kind
     alatan8=60.000000_r_kind
 
  elseif (trim(cgrid) == 'hawaii') then 
     nx=321
     ny=225
     alat18=18.066780_r_kind
     elon18=198.374755_r_kind
     da8=2500.000_r_kind
     elonv8=9999._r_kind
     alatan8=20.000000_r_kind

  elseif (trim(cgrid) == 'prico') then 
     nx=177
     ny=129
     alat18=16.828685_r_kind
     elon18=291.804687_r_kind
     da8=2500.000_r_kind
     elonv8=9999._r_kind
     alatan8=20.000000_r_kind

  elseif (trim(cgrid) == 'guam') then 
     nx=193
     ny=193
     alat18=12.349884_r_kind
     elon18=143.686538_r_kind
     da8=2500.000_r_kind
     elonv8=9999._r_kind
     alatan8=20.000000_r_kind

  else
     print*,'in ndfdgrid_info: unknown grid ',cgrid,'...aborting'
     call abort
  endif

end subroutine ndfdgrid_info
!****************************************************************
subroutine latlon_to_grid0(rlat8,rlon8,xx8,yy8)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: given the earth (lat,lon) for a selected point on the ndfd grid,
! use w3 subroutines to compute the (x,y) coordinates on the
! projected cartesian grid
!
! program history log:
!   2008-11-04  pondeca  
!
! input argument list:
!   rlon8  - east longitude in degrees
!   rlat8  - latitude in degrees
!
! output argument list:
!    xx8 - x coordinate on the plane projected grid 
!    yy8 - y coordinate on the plane projected grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

  real(r_kind),intent(in   ) :: rlat8,rlon8
  real(r_kind),intent(  out) :: xx8,yy8

  if (trim(cgrid)=='conus') then
     call w3fb11(rlat8,rlon8,alat18,elon18,da8,elonv8,alatan8,xx8,yy8)
  endif

  if (trim(cgrid)=='alaska') then
     call w3fb06(rlat8,rlon8,alat18,elon18,da8,elonv8,xx8,yy8) 
  endif

  if (trim(cgrid)=='hawaii' .or. trim(cgrid)=='guam' .or. trim(cgrid)=='prico') then
     call w3fb08(rlat8,rlon8,alat18,elon18,alatan8,da8,xx8,yy8)
  endif

end subroutine latlon_to_grid0
!****************************************************************
subroutine terrain_slmask(radrlat8,radrlon8,hgt0,slm0)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: given the earth (lat,lon) for a selected point on the ndfd grid,
! find the interpolated terrain value and the slmask value 
! of the nearest grid point.
!
! program history log:
!   2008-11-04  pondeca  
!
! input argument list:
!   radrlon8  - east longitude in radians
!   radrlat8  - latitude in radians
!
! output argument list:
!    hgt0 - interpolated terrain value
!    slm0 - slmask value of nearest point
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use constants, only: zero,one,rad2deg

  implicit none

  real(r_kind),intent(in   ) :: radrlat8,radrlon8
  real(r_kind),intent(  out) :: hgt0,slm0

! Declare local variables
  real(r_single) hgt04
  integer(i_kind) ii,jj
  real(r_kind) rlon8,rlat8,xx8,yy8
  real(r_single) xx,yy

  rlon8=radrlon8*rad2deg
  if (rlon8<zero) rlon8=rlon8+360._r_kind
  rlat8=radrlat8*rad2deg

  call latlon_to_grid0(rlat8,rlon8,xx8,yy8)

  xx=xx8
  yy=yy8

  hgt04=hgt0
  call bilinear_2d0(terrain,nx,ny,hgt04,yy,xx)!Note the reverse order "yy,xx"
  hgt0=hgt04*one

  ii=max(1,min(nx,nint(xx)))
  jj=max(1,min(ny,nint(yy)))

  slm0=slmask(ii,jj)*one

end subroutine terrain_slmask
!****************************************************************
subroutine adjust_error(alon,alat,oberr,oberr2)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: Inflate the observation error is the ob is near a land-water
! boundary
!
! program history log:
!   2008-11-04  pondeca  
!
! input argument list:
!   alon  - observation east longitude in radians 
!   alat  - observation latitude in radians
!   oberr  - observation error
!   oberr2  - observation error
!
! output argument list:
!   oberr  - observation error
!   oberr2  - observation error
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use constants, only: zero,zero_single

  implicit none

! Declare passed variables
  real (r_kind), intent(in   ) :: alon,alat
  real (r_kind), intent(inout) :: oberr,oberr2

! Declare local variables
  integer(i_kind) i,j,istart,jstart,is,ie,js,je
  real (r_single) rsign1,rsign2
  real(r_kind) rlat8,rlon8,xx8,yy8
  logical lcase1 !handle exception for Islands off of Southern California
 
  if (.not.ladjusterr) return

  rlon8=real(alon,r_kind)
  rlat8=real(alat,r_kind)

  if (rlon8>180._r_kind) rlon8=rlon8-360._r_kind

  lcase1=(rlon8>=-122._r_kind .and. rlon8<=-117._r_kind & 
            .and. rlat8>=32._r_kind .and. rlat8<=35._r_kind)

  if (trim(cgrid)=='conus') then
     if(lcase1) return
  endif

  if (rlon8<zero) rlon8=rlon8+360._r_kind

  call latlon_to_grid0(rlat8,rlon8,xx8,yy8)

  istart=floor(xx8)
  jstart=floor(yy8)

! print*,'in adjust_error: alon,rlon8,alat,rlat8,istart,jstart=', & 
!                          alon,rlon8,alat,rlat8,istart,jstart
! print*,'in adjust_error: slmask,min,max=',minval(slmask),maxval(slmask)
! print*,'in adjust_error: before, oberr,oberr2=',oberr,oberr2
  
  is=max(1,(istart-ineighbour))
  ie=min((istart+ineighbour),nx)
  js=max(1,(jstart-jneighbour))
  je=min((jstart+jneighbour),ny)

  if (slmask(is,js)<=0.5_r_single) rsign1=-1._r_single
  if (slmask(is,js)>0.5_r_single)  rsign1=+1._r_single

  do j=js,je
     do i=is,ie
        if (slmask(i,j)<=0.5_r_single) rsign2=-1._r_single
        if (slmask(i,j)>0.5_r_single)  rsign2=+1._r_single
        if (rsign1*rsign2<zero_single) then
           oberr=oberr*oberrinflfact
           oberr2=oberr2*oberrinflfact
           goto 100 
        endif
     enddo 
  enddo 
100 continue
! print*,'in adjust_error: after, oberr,oberr2=',oberr,oberr2
end subroutine adjust_error

subroutine  destroy_ndfdgrid
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_ndfdgrid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
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

  deallocate(slmask)
  deallocate(terrain)
end subroutine destroy_ndfdgrid

end module ndfdgrids
!************************************************************
!************************************************************
module hilbertcurve
!$$$ module documentation block
!           .      .    .                                       .
! module:   hilbertcurve
!   prgmmr: park             org: kma                 date: 2006-09-29
!
! abstract: contains subroutines to compute cross-validation datasets
!           using jim purser's hilbert curve approach
!
! program history log:
!   2006-09-29  park 
!   2008-11-04  pondeca - consolidate code by putting subroutines
!                         in a module. note: code is designed to handle
!                         each ob-type (eg. T, uv, q, etc) separately.
!                         The following restrictions apply: 
!                         (i) in convinfo, the number of cross-validating
!                         groups should be the same for all ob-subtypes
!                         of a given ob type such as T. In other words, it must
!                         be the same for subtypes 180, 181, etc. of the T-obs.
!                         If they are not the same, then the number of
!                         cross-validating subsets used by purser's hilbert curve 
!                         routine will be equal to the last number
!                         of groups that the code reads in for that ob type in
!                         read_prepbufr.f90; 
!                         (ii) the code will not work properly if there is more 
!                         than one processor handling the same ob type or if the
!                         same processor handles more than one ob-type. This
!                         poses no threat to the RTMA so far, but nonetheless it is
!                         something that will have to be dealt with in the future. 
!                        
!
! subroutines included:
!   sub init_hilbertcurve
!   sub accum_hilbertcurve
!   sub apply_hilbertcurve
!   sub destroy_hilbertcurve
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_hilbertcurve
  public :: accum_hilbertcurve
  public :: apply_hilbertcurve
  public :: destroy_hilbertcurve

  integer(i_kind) ncross
  real(r_kind),allocatable,dimension(:):: hil_dlon
  real(r_kind),allocatable,dimension(:):: hil_dlat
  integer(i_kind),allocatable,dimension(:):: hil_ikx
  integer(i_kind),allocatable,dimension(:):: hil_kx
  integer(i_kind),allocatable,dimension(:):: hil_i
  integer(i_kind),allocatable,dimension(:):: test_set

contains

subroutine init_hilbertcurve(maxobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_hilbertcurve
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!
!   input argument list:
!    maxobs
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: maxobs

  ncross=0

  allocate(hil_dlon(maxobs))
  allocate(hil_dlat(maxobs))
  allocate(hil_ikx(maxobs))
  allocate(hil_kx(maxobs))
  allocate(hil_i(maxobs))

end subroutine init_hilbertcurve

subroutine accum_hilbertcurve(usage,dlat,dlon,ikx,kx,ndata)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    accum_hilbertcurve
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!
!   input argument list:
!    usage,dlat,dlon
!    ikx,kx,ndata
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use convinfo, only: ncnumgrp,ncgroup
  use  gridmod, only: nlon,nlat

  implicit none

  real(r_kind)   ,intent(in   ) :: usage,dlat,dlon
  integer(i_kind),intent(in   ) :: ikx,kx,ndata

  if(ncnumgrp(ikx) > 0 .and. usage < 6._r_kind) then
     ncross=ncross+1
     hil_dlat(ncross)=dlat/nlat
     hil_dlon(ncross)=dlon/nlon
     hil_ikx(ncross)=ikx
     hil_kx(ncross)=kx
     hil_i(ncross)=ndata

!     write(6,*) 'CHECK0:',ndata,ncross,hil_ikx(ncross),hil_kx(ncross), & 
!                ncnumgrp(hil_ikx(ncross)),ncgroup(hil_ikx(ncross))
  endif

end subroutine accum_hilbertcurve

subroutine apply_hilbertcurve(maxobs,cdata,k1,k2,tob,ktob,uvob,kuvob,spdob,kspdob, & 
                              psob,kpsob,qob,kqob,pwob,kpwob,sstob,ksstob)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    apply_hilbertcurve
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!
!   input argument list:
!    tob,uvob,spdob,psob,qob,pwob,sstob
!    ktob,kuvob,kspdob,kpsob,kqob,kpwob,ksstob
!    maxobs,k1,k2
!    cdata
!
!   output argument list:
!    cdata
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use convinfo, only:  ncmiter,ncgroup,ncnumgrp

  implicit none


!Declare passed variables
  logical        ,intent(in   ) :: tob,uvob,spdob,psob,qob,pwob,sstob
  integer(i_kind),intent(in   ) :: ktob,kuvob,kspdob,kpsob,kqob,kpwob,ksstob
  integer(i_kind),intent(in   ) :: maxobs,k1,k2
  real(r_kind)   ,intent(inout) :: cdata(k1:k2,maxobs)

!Declare local variables
  integer(i_kind) i,ncnumgrp0,ncgroup0
  real(r_kind) usage

  if(ncross>0) then

     allocate(test_set(ncross))

     ncnumgrp0=ncnumgrp(hil_ikx(ncross)) ! number of cross-validating datasets is
                                         ! chosen to be the last "number of groups" 
                                         ! specified in convinfo for that ob type. 
                                         ! there is no particular reason to do so.

     call hilbert(ncnumgrp0,ncross,hil_dlon(1:ncross),hil_dlat(1:ncross),test_set)

     do i=1,ncross

        ncgroup0=ncgroup(hil_ikx(i))      !note: convinfo must be set up so that this 
                                          !is the same group element for all ob subtypes 
                                          !of a given ob type 

        if (test_set(i)==ncgroup0) then

           usage=ncmiter(hil_ikx(i))

           if(tob)   cdata(ktob,hil_i(i))=usage

           if(uvob)  cdata(kuvob,hil_i(i))=usage

           if(spdob) cdata(kspdob,hil_i(i))=usage

           if(psob)  cdata(kpsob,hil_i(i))=usage

           if(qob)   cdata(kqob,hil_i(i))=usage

           if(pwob)  cdata(kpwob,hil_i(i))=usage

           if(sstob) cdata(ksstob,hil_i(i))=usage

        endif

!        write(6,*) 'CHECK2:',i,test_set(i),ncgroup(hil_ikx(i)),ncross
     end do

  endif

end subroutine apply_hilbertcurve

subroutine destroy_hilbertcurve
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_hilbertcurve
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
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

  deallocate(hil_dlon)
  deallocate(hil_dlat)
  deallocate(hil_ikx)
  deallocate(hil_kx)
  deallocate(hil_i)
  if (ncross>0) deallocate(test_set)

end subroutine destroy_hilbertcurve

end module hilbertcurve
!************************************************************
subroutine mkheader_madis_and_time_rejects(cobtype,lun)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mkheader_madis_and_time_rejects
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-02  lueken - added subprogram doc block
!
!   input argument list:
!    cobtype
!    lun
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind
  implicit none

  character(10)  ,intent(in   ) :: cobtype
  integer(i_kind),intent(in   ) :: lun

  character(96) cheader

  cheader='stnname   obtype   lat(dg)  lon(dg E)   dtime      ob          qctype'

  if (trim(cobtype)=='t') then
     write(lun,*)   'RTMA TEMPERATURE OBS FLAGGED SOON AFTER THEY ARE READ IN FROM THE PREPBUFR FILE'
     write(lun,*)   'OB UNITS are K'
  endif

  if (trim(cobtype)=='q') then
     write(lun,*)   'RTMA SPECIFIC HUMIDITY OBS FLAGGED SOON AFTER THEY ARE READ IN FROM THE PREPBUFR FILE'
     write(lun,*)   'OB UNITS are g/Kg'
  endif

  if (trim(cobtype)=='ps') then
     write(lun,*)   'RTMA SURFACE PRESSURE OBS FLAGGED SOON AFTER THEY ARE READ IN FROM THE PREPBUFR FILE'
     write(lun,*)   'OB UNITS are Pa'
  endif

  if (trim(cobtype)=='uv') then
     write(lun,*)   'RTMA WIND OBS (sqrt(u**2+v**2)) FLAGGED SOON AFTER THEY ARE READ IN FROM THE PREPBUFR FILE'
     write(lun,*)   'OB UNITS are m/s'
  endif

  if (trim(cobtype)=='spd') then
     write(lun,*)   'RTMA WIND SPEED OBS FLAGGED SOON AFTER THEY ARE READ IN FROM THE PREPBUFR FILE'
     write(lun,*)   'OB UNITS are m/s'
  endif

  write(lun,*)   'dtime is the hour relative to the analysis time. For example, dtime=-0.1'
  write(lun,*)   '             means 0.1h (i.e. 6 minutes) before the analysis time'
  write(lun,*)   'qctype=1 ==> ob lies outside assimilation time window'
  write(lun,*)   'qctype=2 ==> MADIS QC related reject'
  write(lun,*)   'qctype=3 ==> MADIS QC related reject'
  write(lun,*)   'qctype=4 ==> MADIS QC related reject'
  write(lun,*)   '                              '
  write(lun,*)   cheader

end subroutine mkheader_madis_and_time_rejects
!------------------------------------------------------
!------------------------------------------------------
subroutine get_stndewpt(p,q,t,td,lboundtd)
!                .      .    .                                       .
! subprogram:    get_stndewpt
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-02  lueken - added subprogram doc block
!
!   input argument list:
!    q-specific humidity
!    p-pressure in Pa
!    t-
!    lboundtd
!
!   output argument list:
!    td-dewpoint in K
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind
  use constants, only: one,r100
  implicit none

! Declare passed variables
  real(r_kind),intent(in   ) :: p,q,t
  real(r_kind),intent(  out) :: td
  logical     ,intent(in   ) :: lboundtd

! Declare local parameters
  real(r_kind),parameter::eps=0.62197_r_kind    !=Rd/Rv
  real(r_kind),parameter::a=243.5_r_kind
  real(r_kind),parameter::b=440.8_r_kind
  real(r_kind),parameter::c=19.48_r_kind
  real(r_kind),parameter::c2k=273.15_r_kind

! Declare local variables
  real(r_kind)  e, qv, eln

  qv=q/(one-q)
  e=p/r100*qv/(eps+qv)
  eln=log(e)
  td = (a*eln-b)/(c-eln)+c2k
  if (lboundtd) td = min(t,td)

end subroutine get_stndewpt
!************************************************************
!------------------------------------------------------
      subroutine bilinear_2d0(rffcst,ix,jx,rfobs,xx,yy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bilinear_2d0
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-02  lueken - added subpogram doc block
!
!   input argument list:
!    rffcst               - model grid value
!    ix,jx
!    xx,yy                - define coordinates in grid units
!                         of point for which interpolation is
!                         performed
!
!   output argument list:
!    rfobs                - interpolated value
!
! notes:
!
!     i+1,j |          | i+1,j+1
!         --+----------+---
!           |          | dym
!           |    *     + -
!           |   x,y    | dy
!           |          |
!         --+----+-----+---
!        i,j|<dx>|<dxm>| i,j+1
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      use kinds, only: r_single,i_kind
      implicit none

!declare passed variables
      integer(i_kind),intent(in   ) :: ix,jx
      real(r_single) ,intent(in   ) :: rffcst(ix,jx)
      real(r_single) ,intent(in   ) :: xx,yy
      real(r_single) ,intent(  out) :: rfobs

!declare local variables
      integer(i_kind) i,j
      real(r_single) dx,dy,dxm,dym

      i  = ifix(yy)
      j  = ifix(xx)
      if((i>=1) .and. (i<=(ix-1)) .and. &
         (j>=1) .and. (j<=(jx-1)) ) then
         dx = xx - float(j)
         dy = yy - float(i)
         dxm= 1.0_r_single-dx
         dym= 1.0_r_single-dy
         rfobs=dxm*(dym*rffcst(i,j)+dy*rffcst(i+1,j)) &
                  + dx *(dym*rffcst(i,j+1)+dy*rffcst(i+1,j+1))
      else
	 rfobs=HUGE(rfobs)	! A note is left here for whoever knows what this is, since
         !rfobs=1.e+39_r_single	! this number (1.e+39) can not have a r_single kind.
      endif

      return
end subroutine bilinear_2d0
!------------------------------------------------------
