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
!   2011-02-09  zhu     - add gust,vis and pblh
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

  n_loop: do n=1,9

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
 
     read(in_unit)field2             !  GUST
     write(6,*)' convert_binary_2d: max,min GUST=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid GUST=',field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2

     read(in_unit)field2             !  VIS
     write(6,*)' convert_binary_2d: max,min VIS=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid VIS=',field2(nlon_regional/2,nlat_regional/2)
     write(lendian_out)field2

     read(in_unit)field2             !  PBLH
     write(6,*)' convert_binary_2d: max,min PBLH=',maxval(field2),minval(field2)
     write(6,*)' convert_binary_2d: mid PBLH=',field2(nlon_regional/2,nlat_regional/2)
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

  if(mype == 0)print*,'in read_2d_files: nhr_assimilation,nhr_half,time_offset=',nhr_assimilation,nhr_half,time_offset

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
!   2010-12-05  pondeca - change definition of land point from (landmask value)/=0.
!                         to (landmask value)>=0.5
!   2011-02-09  zhu     - add gust,vis and pblh
!   2011-05-01  todling - introduce met-guess (cwmr no longer in guess-grids)
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
  use guess_grids, only: ges_z,ges_ps,ges_tv,ges_q,ges_vor,&
       ges_div,ges_u,ges_v,ges_tvlat,ges_tvlon,ges_qlat,ges_qlon,&
       ges_gust,ges_vis,ges_pblh,&
       fact10,soil_type,veg_frac,veg_type,sfct,sno,soil_temp,soil_moi,&
       isli,nfldsig,ifilesig,ges_tsen
  use gridmod, only: lon1,lat1,nlat_regional,nlon_regional,&
       nsig,ijn_s,displs_s,itotsub
  use constants, only: zero,one,grav,fv,zero_single,one_tenth
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
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
  integer(i_kind) i_sm,i_xice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac,i_gust,i_vis,i_pblh
  integer(i_kind) isli_this
  real(r_kind) psfc_this,sm_this,xice_this
  real(r_kind),pointer,dimension(:,:,:)::ges_cwmr_it
  integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all,icwmr


!  RESTART FILE input grid dimensions in module gridmod
!      These are the following:
!          im -- number of x-points on C-grid
!          jm -- number of y-points on C-grid
!          lm -- number of vertical levels ( = nsig for now)


  num_doubtful_sfct=0
! if(mype==0) write(6,*)' at 0 in read_2d_guess'


! Big section of operations done only on first outer iteration

! if(mype==0) write(6,*)' at 0.1 in read_2d_guess'

  im=nlon_regional
  jm=nlat_regional
  lm=nsig

! Following is for convenient 2D input
  num_2d_fields=21! Adjust once exact content of RTMA restart file is known
  num_all_fields=num_2d_fields*nfldsig
  num_loc_groups=num_all_fields/npe
! if(mype==0) write(6,'(" at 1 in read_2d_guess, lm            =",i6)')lm
! if(mype==0) write(6,'(" at 1 in read_2d_guess, num_2d_fields=",i6)')num_2d_fields
! if(mype==0) write(6,'(" at 1 in read_2d_guess, nfldsig       =",i6)')nfldsig
! if(mype==0) write(6,'(" at 1 in read_2d_guess, num_all_fields=",i6)')num_all_fields
! if(mype==0) write(6,'(" at 1 in read_2d_guess, npe           =",i6)')npe
! if(mype==0) write(6,'(" at 1 in read_2d_guess, num_loc_groups=",i6)')num_loc_groups
  do
     num_all_pad=num_loc_groups*npe
     if(num_all_pad >= num_all_fields) exit
     num_loc_groups=num_loc_groups+1
  end do
! if(mype==0) write(6,'(" at 1 in read_2d_guess, num_all_pad   =",i6)')num_all_pad
! if(mype==0) write(6,'(" at 1 in read_2d_guess, num_loc_groups=",i6)')num_loc_groups

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
  i=i+1 ; i_gust=i                                            ! gust
  write(identity(i),'("record ",i3,"--gust")')i
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_vis=i                                             ! vis
  write(identity(i),'("record ",i3,"--vis")')i
  jsig_skip(i)=0 ; igtype(i)=1
  i=i+1 ; i_pblh=i                                            ! pblh
  write(identity(i),'("record ",i3,"--pblh")')i
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
     if(mype == 0)write(6,*)'READ_2d_GUESS:  open nfcst=',nfcst,' to file=',filename

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
              if(mype == 0)write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
              call fill_mass_grid2t(temp1,im,jm,tempa,1)
           end if
           if(igtype(ifld) < 0) then
              read(nfcst)((itemp1(i,j),i=1,im),j=1,jm)
              do j=1,jm
                 do i=1,im
                    temp1(i,j)=itemp1(i,j)
                 end do
              end do
              if(mype == 0)write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
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

!    if(mype==10) write(6,*)' in read_2d_guess, min,max(soil_moi)=', &
!       minval(soil_moi),maxval(soil_moi)
!    if(mype==10) write(6,*)' in read_2d_guess, min,max(soil_temp)=', &
!       minval(soil_temp),maxval(soil_temp)


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
     ges_tvlat=zero
     ges_tvlon=zero
     ges_qlat=zero
     ges_qlon=zero


!    Transfer surface fields
     do it=1,nfldsig

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,icwmr)
        if(icwmr==0) ges_cwmr_it=zero

        i_0=(it-1)*num_2d_fields
        do i=1,lon1+2
           do j=1,lat1+2
              fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
              veg_type(j,i,it)=all_loc(j,i,i_0+i_ivgtyp)
              veg_frac(j,i,it)=r0_01*all_loc(j,i,i_0+i_vegfrac)
              soil_type(j,i,it)=all_loc(j,i,i_0+i_isltyp)
              sm_this=zero
              if(all_loc(j,i,i_0+i_sm) >= 0.5_r_single) sm_this=one
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

              ges_gust(j,i,it)=all_loc(j,i,i_0+i_gust)

              ges_vis(j,i,it)=all_loc(j,i,i_0+i_vis)
              if (ges_vis(j,i,it)<=zero) ges_vis(j,i,it)=0.1_r_kind
              if (ges_vis(j,i,it)>20000.0_r_kind) ges_vis(j,i,it)=20000.0_r_kind

              ges_pblh(j,i,it)=all_loc(j,i,i_0+i_pblh)

           end do
        end do
     end do

     call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,1,mpi_integer,mpi_sum,&
          0,mpi_comm_world,ierror)
!    if(mype==0)     write(6,*)' in read_2d_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
!    if(mype==10) write(6,*)' in read_2d_guess, min,max(sfct)=', &
!         minval(sfct),maxval(sfct)
!    if(mype==10) write(6,*)' in read_2d_guess, min,max(veg_type)=', &
!         minval(veg_type),maxval(veg_type)
!    if(mype==10) write(6,*)' in read_2d_guess, min,max(veg_frac)=', &
!         minval(veg_frac),maxval(veg_frac)
!    if(mype==10) write(6,*)' in read_2d_guess, min,max(soil_type)=', &
!         minval(soil_type),maxval(soil_type)
!    if(mype==10) write(6,*)' in read_2d_guess, min,max(isli)=', &
!         minval(isli),maxval(isli)
!    if(mype==10) write(6,*)' in read_2d_guess, min,max(ges_gust)=', &
!         minval(ges_gust),maxval(ges_gust)

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
!   2011-02-09  zhu     - add gust,vis,pblh
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
       ges_q,ges_u,ges_v,ges_tsen,ges_gust,ges_vis,ges_pblh
  use mpimod, only: mpi_comm_world,ierror,mpi_real4
  use gridmod, only: lat2,iglobal,itotsub,update_regsfc,strip_single,&
       lon2,nsig,lon1,lat1,nlon_regional,nlat_regional,ijn,displs_g
  use mpeu_util, only: getindex
  use control_vectors, only: cvars2d
  use constants, only: zero_single,r10,r100
  use jfunc, only: qsatg,jiter,miter
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
  character(2) ch2
  integer(i_kind) iog,ioan,i,j,k,kt,kq,ku,kv,it,i_psfc,i_t,i_q,i_u,i_v
  integer(i_kind) i_sst,i_skt,i_gust,i_vis,i_pblh
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

  num_2d_fields=6+4*lm
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
  i_gust=i_skt+1
  i_vis=i_gust+1
  i_pblh=i_vis+1

  allocate(temp1(im*jm),temp1u((im+1)*jm),temp1v(im*(jm+1)))
  allocate(temp1_ps(im*jm))
  allocate(temp1_prh(im*jm))

! if(mype == 0) write(6,*)' at 2 in wr2d_binary'

  iog=15
  ioan=66
  if(mype == 0) then
     write(filename,'("sigf",i2.2)')ifilesig(ntguessig)
     open (iog,file=filename,form='unformatted')
     write(ch2,'(i2.2)') jiter+1
     if (jiter <  miter) open (ioan,file='sigfupdate'//ch2,form='unformatted')
     if (jiter == miter) open (ioan,file='siganl',form='unformatted')
     rewind iog ; rewind ioan
  end if

! Convert analysis variables to 2D variables
  it=ntguessig

! Create all_loc from ges_*
! if(mype == 0) write(6,*)' at 3 in wr2d_binary'
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
! if(mype == 0) write(6,*)' at 6 in wr2d_binary'

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
!    if (mype==0) write(6,*)' at 9.1 in wr2d_binary,max,min(temp1)=',maxval(temp1),minval(temp1)
     call strip_single(all_loc(1,1,i_sst),strp,1)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
!       if(mype == 0) write(6,*)' at 9.2 in wr2d_binary,max,min(tempa)=',maxval(tempa),minval(tempa)
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           if(tempb(i) < (r225)) then
              tempa(i)=zero_single
           else
              tempa(i)=tempa(i)-tempb(i)
           end if
        end do
!       if(mype == 0) write(6,*)' at 9.4 in wr2d_binary,max,min(tempa)=',maxval(tempa),minval(tempa)
        call unfill_mass_grid2t(tempa,im,jm,temp1)
!       write(6,*)' at 9.6 in wr2d_binary,max,min(temp1)=',maxval(temp1),minval(temp1)
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
!    if (mype==0) write(6,*)' at 10.0 in wr2d_binary,max,min(temp1)=',maxval(temp1),minval(temp1)
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

  if (getindex(cvars2d,'gust')>0) then
     do i=1,lon2
        do j=1,lat2
           all_loc(j,i,i_gust)=ges_gust(j,i,it)
        end do
     end do
     if(mype==0) read(iog)temp1
     call strip_single(all_loc(1,1,i_gust),strp,1)
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
  else
     if(mype==0) then
        read(iog)temp1
        write(ioan)temp1
     end if
  endif

  if (getindex(cvars2d,'vis')>0) then
     do i=1,lon2
        do j=1,lat2
           all_loc(j,i,i_vis)=ges_vis(j,i,it)
        end do
     end do
     if(mype == 0) read(iog)temp1
     call strip_single(all_loc(1,1,i_vis),strp,1)
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
  else
     if(mype==0) then
        read(iog)temp1
        write(ioan)temp1
     end if
  endif

  if (getindex(cvars2d,'pblh')>0) then
     do i=1,lon2
        do j=1,lat2
           all_loc(j,i,i_pblh)=ges_pblh(j,i,it)
        end do
     end do
     if(mype==0) read(iog)temp1
     call strip_single(all_loc(1,1,i_pblh),strp,1)
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
  else
     if(mype==0) then
        read(iog)temp1
        write(ioan)temp1
     end if
  endif

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
!   2010-12-05  pondeca - add the capability to shift slightly the obs
!               which are close to shorelines so that land (water) obs are not
!               mistaken for water(land) obs by the landmask.
!   2010-12-05  pondeca - add the capability to randomly pick one dataset
!               for cross-validation in the hilbert-curve procedure
!
!
! subroutines included:
!   sub init_ndfdgrid
!   sub ndfdgrid_info
!   sub latlon_to_grid0
!   sub grid_to_latlon0
!   sub terrain_slmask
!   sub relocsfcob
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
  public :: grid_to_latlon0
  public :: terrain_slmask
  public :: adjust_error
  public :: relocsfcob
  public :: destroy_ndfdgrid

  integer(i_kind),parameter::lunreloc=77

  character(60) cgrid
  integer(i_kind) nx,ny
  real(r_single),allocatable::slmask(:,:)
  real(r_single),allocatable::terrain(:,:)
  real(r_kind) da8,alat18,elon18,elonv8,alatan8,xx8,yy8

  real(r_kind) oberrinflfact,slmland
  integer(i_kind) ineighbour,jneighbour
  logical ladjusterr
  logical fexist
  logical lshoreline
  logical,save :: relocfile_opnd=.false.

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
  use mpimod, only: mype
  implicit none

  character(3) clun33

  namelist/parmcardreadprepb/cgrid,ladjusterr,oberrinflfact, & 
                             ineighbour,jneighbour,slmland,lshoreline

  cgrid='conus'
  ladjusterr=.false.
  lshoreline=.false.
  oberrinflfact=five
  ineighbour=3
  jneighbour=3
  slmland=0.51_r_kind

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
  print*,'in init_ndfdgrid: lshoreline=',lshoreline
  print*,'in init_ndfdgrid: slmland=',slmland

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

  if (.not.relocfile_opnd) then
     write(clun33,'(i3.3)') mype
     open (lunreloc,file='shoreline_obrelocation.dat_'//clun33, &
          form='formatted')
      write(lunreloc,*) '***********************************************************************'
      write(lunreloc,*) 'stn      itype     rlatin     rlonin       rlatout     rlonout  vartype'
      write(lunreloc,*) '***********************************************************************'
      relocfile_opnd=.true.
   endif


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

!    alat18=18.066780_r_kind      !before domain shift
!    elon18=198.374755_r_kind     !before domain shift

     alat18=18.072699_r_kind
     elon18=198.474999_r_kind
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

  elseif (trim(cgrid) == 'cohres') then
     nx=2145
     ny=1377
     alat18=20.192_r_kind
     elon18=238.446_r_kind
     da8=2539.703_r_kind
     elonv8=265.000_r_kind
     alatan8=25.000_r_kind

  elseif (trim(cgrid) == 'akhres') then
     nx=1649
     ny=1105
     alat18=40.530101_r_kind
     elon18=181.429000_r_kind
     da8=2976.563_r_kind
     elonv8=210.000000_r_kind
     alatan8=60.000000_r_kind

  elseif (trim(cgrid) == 'hrrr') then
     nx=1799
     ny=1059
     alat18=21.138_r_kind
     elon18=237.280_r_kind
     da8=3000.000_r_kind
     elonv8=262.500_r_kind
     alatan8=38.500_r_kind

  elseif (trim(cgrid) == 'cohresext') then
     nx=2145
     ny=1597
     alat18=20.192_r_kind
     elon18=238.446_r_kind
     da8=2539.703_r_kind
     elonv8=265.000_r_kind
     alatan8=25.000_r_kind

  elseif (trim(cgrid) == 'juneau') then
     nx=655
     ny=855
     alat18=51.500000_r_kind
     elon18=217.500000_r_kind
     da8=1448.281_r_kind
     elonv8=225.000000_r_kind
     alatan8=60.000000_r_kind

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

  logical lambconform
  logical polarstereo
  logical lmercator


  lambconform=trim(cgrid)=='conus'.or.trim(cgrid)=='cohres'.or.trim(cgrid)=='hrrr'.or.trim(cgrid)=='cohresext'
  polarstereo=trim(cgrid)=='alaska'.or.trim(cgrid)=='akhres'.or.trim(cgrid)=='juneau'
  lmercator=trim(cgrid)=='hawaii'.or.trim(cgrid)=='guam'.or.trim(cgrid)=='prico'
  
  if (lambconform) call w3fb11(rlat8,rlon8,alat18,elon18,da8,elonv8,alatan8,xx8,yy8)
  if (polarstereo) call w3fb06(rlat8,rlon8,alat18,elon18,da8,elonv8,xx8,yy8) 
  if (lmercator)   call w3fb08(rlat8,rlon8,alat18,elon18,alatan8,da8,xx8,yy8)

end subroutine latlon_to_grid0
!****************************************************************
subroutine grid_to_latlon0(xx8,yy8,rlat8,rlon8)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: given the (x,y) coordinates on the projected cartesian grid
! for a selected point of the ndfd grid, use w3 subroutines to
! compute the earth (lat,lon)
!
! program history log:
!   2010-03-18  pondeca
!
! input argument list:
!   xx8 - x coordinate on the plane projected grid
!   yy8 - y coordinate on the plane projected grid
!
! output argument list:
!   rlon8  - east longitude in degrees
!   rlat8  - latitude in degrees
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

  real(r_kind),intent(in   ) :: xx8,yy8
  real(r_kind),intent(  out) :: rlat8,rlon8

  integer(i_kind) ierr

  logical lambconform
  logical polarstereo
  logical lmercator


  lambconform=trim(cgrid)=='conus'.or.trim(cgrid)=='cohres'.or.trim(cgrid)=='hrrr'.or.trim(cgrid)=='cohresext'
  polarstereo=trim(cgrid)=='alaska'.or.trim(cgrid)=='akhres'.or.trim(cgrid)=='juneau'
  lmercator=trim(cgrid)=='hawaii'.or.trim(cgrid)=='guam'.or.trim(cgrid)=='prico'

  if (lambconform) then 
     call w3fb12(xx8,yy8,alat18,elon18,da8,elonv8,alatan8,rlat8,rlon8,ierr)
     if (ierr > 0) then
       print*,'in grid_to_latlon0: trouble,xx8,yy8,rlat8,rlon8,ierr=',&
                                           xx8,yy8,rlat8,rlon8,ierr
     endif

  endif

  if (polarstereo) call w3fb07(xx8,yy8,alat18,elon18,da8,elonv8,rlat8,rlon8)
  if (lmercator)   call w3fb09(xx8,yy8,alat18,elon18,alatan8,da8,rlat8,rlon8)

end subroutine grid_to_latlon0
!****************************************************************
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

  if (trim(cgrid)=='conus' .or. trim(cgrid)=='cohres' .or. trim(cgrid)=='cohresext') then
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
!****************************************************************
subroutine relocsfcob(rlon8,rlat8,cobtypein,cstationin,kxin)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2010-03-18
!
! abstract: relocate ob that land-sea mask says it's on water to 
! nearby land grid point
!
! program history log:
!   2010-03-18  pondeca  
!
! input argument list:
!   rlon8  - observation east longitude in radians
!   rlat8  - observation latitude in radians
!
! output argument list:
!   rlon8  - relocated observation east longitude in radians
!   rlat8  - relocated observation latitude in radians
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use constants, only: zero,zero_single,half,rad2deg,deg2rad

  implicit none

! Declare passed variables
  real (r_kind), intent(inout   ) :: rlon8,rlat8
  character(len=*) ,intent(in   ) :: cobtypein
  character(len=8) ,intent(in   ) :: cstationin
  integer(i_kind)  ,intent(in   ) :: kxin

! Declare local parameters
  integer(i_kind),parameter::npts=300
  real(r_single),parameter::dx=0.03125_r_single
  real(r_single),parameter::dy=0.03125_r_single
  real(r_single),parameter::del=0.125_r_single

! Declare local variables
  character(20) cvarname
  integer(i_kind) i,j,istart,jstart,is,ie,js,je
  integer(i_kind) imin,jmin
  real(r_kind) rlonin8,rlatin8,xxin8,yyin8,xx8,yy8
  real(r_single) xxin,yyin
  real(r_single) dist,distmin,ri,rj,delx,dely
  real(r_single) ris,rie,rjs,rje,rimin,rjmin
  real(r_single) idist,rlonin,rlatin,rlon,rlat
  real(r_single) slmin,slmout,slmask0
  logical lfound


  if (.not.lshoreline) return

  rlonin8=rlon8*rad2deg
  rlatin8=rlat8*rad2deg
  rlatin=real(rlatin8,r_single)
  rlonin=real(rlonin8,r_single)

  call latlon_to_grid0(rlatin8,rlonin8,xxin8,yyin8)

  xxin=real(xxin8,kind=r_single)
  yyin=real(yyin8,kind=r_single)

  istart=floor(xxin)
  jstart=floor(yyin)

  call bilinear_2d0(slmask,nx,ny,slmin,yyin,xxin)!Note the reverse order "yyin,xxin"

! print*,'in relocsfcob: cstationin,slmin,rlonin8,rlatin8=',trim(cstationin),slmin,rlonin8,rlatin8

  slmout=-9999.

  if (slmin < 0.75_r_single) then
  
     is=max(1,(istart-ineighbour))
     ie=min((istart+ineighbour),nx)
     js=max(1,(jstart-jneighbour))
     je=min((jstart+jneighbour),ny)

     ris=float(is)
     rie=float(ie)
     rjs=float(js)
     rje=float(je)

     distmin=1.e+20_r_single
     lfound=.false.

     do j=1,npts
      rj=rjs+float(j-1)*dy  
      if (rj > rje) cycle
      do i=1,npts
         ri=ris+float(i-1)*dx  
         if (ri > rie) cycle

         call bilinear_2d0(slmask,nx,ny,slmask0,rj,ri)

         if (slmask0>=slmland) then
            lfound=.true.
            dist=(ri-xxin)*(ri-xxin)+(rj-yyin)*(rj-yyin)                        
            if (dist > zero_single) dist=sqrt(dist)
            if (dist < distmin) then 
               rimin=ri
               rjmin=rj
               distmin=dist
               slmout=slmask0
            endif
         endif
      enddo 
     enddo 

     delx=zero_single
     dely=zero_single

     if (lfound) then
        if ((rimin-xxin) < zero_single) delx=-del
        if ((rimin-xxin) > zero_single) delx=+del
        if ((rjmin-yyin) < zero_single) dely=-del
        if ((rjmin-yyin) > zero_single) dely=+del

        call bilinear_2d0(slmask,nx,ny,slmask0,rjmin+dely,rimin+delx)

        if (slmask0>=slmland) then
           xx8=real(rimin+delx,r_kind)
           yy8=real(rjmin+dely,r_kind)
           slmout=slmask0
         else
           xx8=real(rimin,r_kind)
           yy8=real(rjmin,r_kind)
        endif

        call grid_to_latlon0(xx8,yy8,rlat8,rlon8)

        rlat=real(rlat8,r_single)
        rlon=real(rlon8,r_single)

        if (trim(cobtypein)=='t') cvarname='temperatureob'
        if (trim(cobtypein)=='q') cvarname='moistureob'
        if (trim(cobtypein)=='ps') cvarname='psfcob'
        if (trim(cobtypein)=='uv') cvarname='windob'
        if (trim(cobtypein)=='spd') cvarname='wspdob'
        write(lunreloc,'(a8,i6,4(f12.4),3x,a20)') cstationin,kxin,rlatin,rlonin,rlat,rlon,cvarname
     endif

!    print*,'in relocsfcob: cstationin,kxin,rlatin,rlonin,rlat,rlon,cvarname=', & 
!              trim(cstationin),kxin,rlatin,rlonin,rlat,rlon,trim(cvarname) 
!    print*,'in relocsfcob: rlatin8,rlonin8,xxin8,yyin8=',rlatin8,rlonin8,xxin8,yyin8
!    print*,'in relocsfcob: rlat8,rlon8,xx8,yy8=',rlat8,rlon8,xx8,yy8
!    print*,'in relocsfcob: istart,jstart,imin,jmin,distmin=',istart,jstart,imin,jmin,distmin
!    print*,'in relocsfcob: slmin,slmout=',slmin,slmout
!    print*,'in relocsfcob: ************************************************'

    rlat8=rlat8*deg2rad
    rlon8=rlon8*deg2rad

  endif

end subroutine relocsfcob
!****************************************************************
!****************************************************************
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
  if (relocfile_opnd) close (lunreloc)
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

  use kinds, only: i_kind,r_kind,r_single

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_hilbertcurve
  public :: accum_hilbertcurve
  public :: apply_hilbertcurve
  public :: destroy_hilbertcurve

  integer(i_kind) ncross,nxv

  character(8),allocatable,dimension(:):: hil_cstation
  character(8),allocatable,dimension(:):: hil_cprovider
  character(8),allocatable,dimension(:):: hil_csubprovider
  real(r_kind),allocatable,dimension(:):: hil_dlon
  real(r_kind),allocatable,dimension(:):: hil_dlat
  real(r_kind),allocatable,dimension(:):: hil_alon
  real(r_kind),allocatable,dimension(:):: hil_alat
  real(r_kind),allocatable,dimension(:):: hil_time
  integer(i_kind),allocatable,dimension(:):: hil_ikx
  integer(i_kind),allocatable,dimension(:):: hil_kx
  integer(i_kind),allocatable,dimension(:):: hil_i
  integer(i_kind),allocatable,dimension(:):: test_set
  
  integer(i_kind) ngrps_tob
  integer(i_kind) ngrps_uvob
  integer(i_kind) ngrps_spdob
  integer(i_kind) ngrps_psob
  integer(i_kind) ngrps_qob
  integer(i_kind) ngrps_pwob
  integer(i_kind) ngrps_sstob
  integer(i_kind) ngrps_gustob
  integer(i_kind) ngrps_visob

  logical random_cvgrp
  real(r_kind) usagecv

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
  use mpimod, only: mype

  implicit none

  integer(i_kind),intent(in   ) :: maxobs

  character(1),parameter::blank=' '
  integer(i_kind) i,k
  logical fexist

  namelist/parmcardhcurve/random_cvgrp,usagecv,ngrps_tob,ngrps_uvob, & 
                    ngrps_spdob,ngrps_psob,ngrps_qob, & 
                    ngrps_pwob,ngrps_sstob,ngrps_gustob,ngrps_visob

  random_cvgrp=.false.
  usagecv=3._r_kind
  ngrps_tob=5
  ngrps_uvob=8
  ngrps_spdob=0
  ngrps_psob=5
  ngrps_qob=5
  ngrps_pwob=0
  ngrps_sstob=0
  ngrps_gustob=8
  ngrps_visob=8

  inquire(file='parmcard_input',exist=fexist)
  if (fexist) then
     open(55,file='parmcard_input',form='formatted')
     read(55,parmcardhcurve)
     close(55)
  endif

  if(mype == 0)then
    print*,'in init_hilbertcurve: random_cvgrp=',random_cvgrp
    print*,'in init_hilbertcurve: usagecv=',usagecv
    print*,'in init_hilbertcurve: ngrps_tob=',ngrps_tob
    print*,'in init_hilbertcurve: ngrps_uvob=',ngrps_uvob
    print*,'in init_hilbertcurve: ngrps_spdob=',ngrps_spdob
    print*,'in init_hilbertcurve: ngrps_psob=',ngrps_psob
    print*,'in init_hilbertcurve: ngrps_qob=',ngrps_qob
    print*,'in init_hilbertcurve: ngrps_pwob=',ngrps_pwob
    print*,'in init_hilbertcurve: ngrps_sstob=',ngrps_sstob
  end if
  print*,'in init_hilbertcurve: ngrps_gustob=',ngrps_gustob
  print*,'in init_hilbertcurve: ngrps_visob=',ngrps_visob

  ncross=0

  allocate(hil_cstation(maxobs))
  allocate(hil_cprovider(maxobs))
  allocate(hil_csubprovider(maxobs))
  allocate(hil_dlon(maxobs))
  allocate(hil_dlat(maxobs))
  allocate(hil_alon(maxobs))
  allocate(hil_alat(maxobs))
  allocate(hil_time(maxobs))
  allocate(hil_ikx(maxobs))
  allocate(hil_kx(maxobs))
  allocate(hil_i(maxobs))

  do i=1,maxobs
     write(hil_cstation(i),'(8a1)') (blank,k=1,8)
     write(hil_cprovider(i),'(8a1)') (blank,k=1,8)
     write(hil_csubprovider(i),'(8a1)') (blank,k=1,8)
  enddo

end subroutine init_hilbertcurve

subroutine accum_hilbertcurve(usage,cstation,cprovider,csubprovider, &
                              alat,alon,dlat,dlon,time,toff,ikx,kx,ndata)
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
!    usage,time,toff
!    dlat,dlon       !grid-relative lat and lon
!    alat,alon       !earth lat and lon in radians on entry
!    ikx,kx,ndata
!    cstation,cprovider,csubprovider
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use constants, only: rad2deg
  use convinfo, only: ncnumgrp,ncgroup
  use  gridmod, only: nlon,nlat

  implicit none

  real(r_kind),intent(in)::usage,time,toff
  real(r_kind),intent(in)::dlat,dlon       !grid-relative lat and lon
  real(r_kind),intent(in)::alat,alon       !earth lat and lon in radians on entry
  integer(i_kind),intent(in)::ikx,kx,ndata
  character(8),intent(in)::cstation,cprovider,csubprovider
 
  logical goodkx
 
  goodkx= ( (kx>=180.and.kx<=188).or.(kx>=280.and.kx<=288).or.&
            (kx>=192.and.kx<=195).or.(kx>=292.and.kx<=295) ) .and.&
            usage<6._r_kind
    
!!!      if(ncnumgrp(ikx) > 0 .and. usage < 6._r_kind) then
!!!      if(usage < 6._r_kind) then
      if(goodkx) then
         ncross=ncross+1
         hil_cstation(ncross)=cstation
         hil_cprovider(ncross)=cprovider
         hil_csubprovider(ncross)=csubprovider
         hil_dlat(ncross)=dlat
         hil_dlon(ncross)=dlon
         hil_alat(ncross)=alat*rad2deg
         hil_alon(ncross)=alon*rad2deg
         hil_time(ncross)=time-toff
         hil_ikx(ncross)=ikx
         hil_kx(ncross)=kx
         hil_i(ncross)=ndata

!     write(6,*) 'CHECK0:',ndata,ncross,hil_ikx(ncross),hil_kx(ncross), & 
!                ncnumgrp(hil_ikx(ncross)),ncgroup(hil_ikx(ncross))
  endif

end subroutine accum_hilbertcurve

subroutine apply_hilbertcurve(maxobs,cdata,k1,k2,tob,ktob,uvob,kuvob,spdob,kspdob, & 
                              psob,kpsob,qob,kqob,pwob,kpwob,sstob,ksstob, & 
                              gustob,kgustob,visob,kvisob)
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
!    tob,uvob,spdob,psob,qob,pwob,sstob,gustob,visob
!    ktob,kuvob,kspdob,kpsob,kqob,kpwob,ksstob,kgustob,kvisob
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
  use  gridmod, only: nlon,nlat

  implicit none


!Declare passed variables
  logical        ,intent(in   ) :: tob,uvob,spdob,psob,qob,pwob,sstob,gustob,visob
  integer(i_kind),intent(in   ) :: ktob,kuvob,kspdob,kpsob,kqob,kpwob,ksstob,kgustob,kvisob
  integer(i_kind),intent(in   ) :: maxobs,k1,k2
  real(r_kind)   ,intent(inout) :: cdata(k1:k2,maxobs)

!Declare local parameter
  real(r_kind),parameter::usage_dup=8.

!Declare local variables
  real(r_kind),parameter:: epsilon=1.e-03_r_kind
  integer(i_kind) i,j,n,nt,ncnumgrp0,ncgroup0
  integer(i_kind),allocatable,dimension(:)::hilflag,hilikx,hili,ipoint
  real(r_kind),allocatable::hildlon(:),hildlat(:)
  real(r_kind) usage
  integer(i_kind) ncvcount(100)
  character(60) outfile
  character(30) cstring
  character(2) clun
  logical ldup


  allocate(hilflag(maxobs))
  allocate(hilikx(maxobs))
  allocate(hili(maxobs))
  allocate(ipoint(maxobs))
  allocate(hildlon(maxobs))
  allocate(hildlat(maxobs))

      print*,'in apply_hilbertcurve: tob,uvob,spdob,psob,qob,pwob,sstob,gustob,visob=',&
                                     tob,uvob,spdob,psob,qob,pwob,sstob,gustob,visob

      nt=ncross
      if(nt.gt.0) then

         !--deal with duplicate obs. use only the ob that is nearest
         !  to the valid assimilation time

         print*,'in apply_hilbertcurve: before duplicate removal: ncross=',nt

         hilflag=+1
         do j=1,nt
            do i=j+1,nt
!              ldup=hil_cstation(i)(1:8)==hil_cstation(j)(1:8)        .and. & 
!                   hil_cprovider(i)(1:8)==hil_cprovider(j)(1:8)      .and. &
!                   hil_csubprovider(i)(1:8)==hil_csubprovider(j)(1:8).and. &
!                   hil_kx(i)==hil_kx(j)                              .and. &
!                   abs(hil_dlon(i)-hil_dlon(j))<epsilon              .and. &
!                   abs(hil_dlat(i)-hil_dlat(j))<epsilon  
       
               ldup=abs(hil_dlon(i)-hil_dlon(j))<epsilon .and. &
                    abs(hil_dlat(i)-hil_dlat(j))<epsilon  

                if ( ldup ) then 
                  if ( abs(hil_time(i)) >= abs(hil_time(j)) ) then 
                       hilflag(i)=-1
                     else
                       hilflag(j)=-1
                  endif
                endif
            enddo
         enddo

         ncross=0
         do i=1,nt
            if (hilflag(i) > 0) then 
                ncross=ncross+1
                hildlon(ncross)=hil_dlon(i)
                hildlat(ncross)=hil_dlat(i)
                hilikx(ncross)=hil_ikx(i)
                hili(ncross)=hil_i(i)
                ipoint(ncross)=i
            endif
         enddo

         print*,'in apply_hilbertcurve: after duplicate removal: ncross=',ncross

        !--evoke the main code for the hilbert curve

        allocate(test_set(ncross))

        if (random_cvgrp) then 
            if(tob)   ncnumgrp0=ngrps_tob
            if(uvob)  ncnumgrp0=ngrps_uvob
            if(spdob) ncnumgrp0=ngrps_spdob
            if(psob)  ncnumgrp0=ngrps_psob
            if(qob)   ncnumgrp0=ngrps_qob
            if(pwob)  ncnumgrp0=ngrps_pwob
            if(sstob) ncnumgrp0=ngrps_sstob
            if(gustob)ncnumgrp0=ngrps_gustob
            if(visob) ncnumgrp0=ngrps_visob
          else
            ncnumgrp0=ncnumgrp(hilikx(ncross)) ! number of cross-validating datasets is
                                               ! chosen to be the last "number of groups" 
                                               ! specified in convinfo for that ob type. 
                                               ! there is no particular reason to do so.
        endif

        hildlon=hildlon/nlon
        hildlat=hildlat/nlat

        call hilbert(ncnumgrp0,ncross,hildlon(1:ncross),hildlat(1:ncross),test_set)

        if (random_cvgrp) call shuffle(ncnumgrp0,ncgroup0)

        do i=1,ncross

           if (.not. random_cvgrp)  ncgroup0=ncgroup(hilikx(i)) !note: convinfo must be set up so that this 
                                                                !is the same group element for all ob subtypes 
                                                                !of a given ob type 

           if (i==1) print*,'in apply_hilbertcurve: ncnumgrp0,ncgroup0=',ncnumgrp0,ncgroup0 

           if (test_set(i).eq.ncgroup0) then
              if (     random_cvgrp) usage=usagecv           !3.
              if (.not.random_cvgrp) usage=ncmiter(hilikx(i))

              if(tob)   cdata(ktob,hili(i))=usage
              if(uvob)  cdata(kuvob,hili(i))=usage
              if(spdob) cdata(kspdob,hili(i))=usage
              if(psob)  cdata(kpsob,hili(i))=usage
              if(qob)   cdata(kqob,hili(i))=usage
              if(pwob)  cdata(kpwob,hili(i))=usage
              if(sstob) cdata(ksstob,hili(i))=usage
              if(gustob)cdata(kgustob,hili(i))=usage
              if(visob) cdata(kvisob,hili(i))=usage

              j=ipoint(i)
              do n=1,nt
                 if (n==j) cycle
                 ldup=abs(hil_dlon(j)-hil_dlon(n))<epsilon .and. &
                      abs(hil_dlat(j)-hil_dlat(n))<epsilon  
                 if ( ldup ) then 
                    if(tob)   cdata(ktob,hil_i(n))=usage_dup
                    if(uvob)  cdata(kuvob,hil_i(n))=usage_dup
                    if(spdob) cdata(kspdob,hil_i(n))=usage_dup
                    if(psob)  cdata(kpsob,hil_i(n))=usage_dup
                    if(qob)   cdata(kqob,hil_i(n))=usage_dup
                    if(pwob)  cdata(kpwob,hil_i(n))=usage_dup
                    if(sstob) cdata(ksstob,hil_i(n))=usage_dup
                    if(gustob)cdata(kgustob,hil_i(n))=usage_dup
                    if(visob) cdata(kvisob,hil_i(n))=usage_dup
                 endif
              enddo
           endif

!         write(6,*) 'CHECK2:',i,test_set(i),ncgroup(hilikx(i)),ncross
        end do
      endif

      if(ncross.gt.0) then
        ! count number of obs in each cross-validation dataset
        print*,'ncnumgrp0,tob,uvob,spdob,psob,qob,pwob,sstob,gustob,visob=',&
                ncnumgrp0,tob,uvob,spdob,psob,qob,pwob,sstob,gustob,visob
        ncvcount=0
        do n=1,ncnumgrp0
           do i=1,ncross
              if (test_set(i).eq.n) then
                 ncvcount(n)=ncvcount(n)+1
              endif
           enddo
           print*,'n,ncvcount(n)=',n,ncvcount(n)
        enddo

        ! write all groups to a file
        if(tob)   outfile='tobs_allcv_groups'
        if(uvob)  outfile='uvobs_allcv_groups'
        if(spdob) outfile='spdobs_allcv_groups'
        if(psob)  outfile='psobs_allcv_groups'
        if(qob)   outfile='qobs_allcv_groups'
        if(gustob)outfile='gustobs_allcv_groups'
        if(visob) outfile='visobs_allcv_groups'

        open (92,file=trim(outfile),form='unformatted')

        write(92) ncross,ncnumgrp0,ncgroup0
        write(92) ncvcount

        do n=1,ncnumgrp0

           write(clun,'(i2.2)') n
           cstring='start group number '//clun
           write(92) cstring

           do i=1,ncross
              if (test_set(i).eq.n) then
                  j=ipoint(i)
                  write(92) hil_cstation(j)
                  write(92) hil_cprovider(j)
                  write(92) hil_csubprovider(j)
                  write(92) hil_kx(j), &
                            hil_alon(j),hil_alat(j), &
                            hil_dlon(j),hil_dlat(j)
              endif
           enddo
        enddo
        close(92)
      endif

  deallocate(hilflag)
  deallocate(hilikx)
  deallocate(hili)
  deallocate(ipoint)
  deallocate(hildlon)
  deallocate(hildlat)

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

  deallocate(hil_cstation)
  deallocate(hil_cprovider)
  deallocate(hil_csubprovider)
  deallocate(hil_dlon)
  deallocate(hil_dlat)
  deallocate(hil_alon)
  deallocate(hil_alat)
  deallocate(hil_time)
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

  if (trim(cobtype)=='gust') then
     write(lun,*)   'RTMA WIND GUST OBS FLAGGED SOON AFTER THEY ARE READ IN FROM THE PREPBUFR FILE'
     write(lun,*)   'OB UNITS are m/s'
  endif

  if (trim(cobtype)=='vis') then
     write(lun,*)   'RTMA VISIBILITY OBS FLAGGED SOON AFTER THEY ARE READ IN FROM THE PREPBUFR FILE'
     write(lun,*)   'OB UNITS are m'
  endif

  if (trim(cobtype)=='pblh') then
     write(lun,*)   'RTMA PBLH OBS FLAGGED SOON AFTER THEY ARE READ IN FROM THE PREPBUFR FILE'
     write(lun,*)   'OB UNITS are m'
  endif

  if (trim(cobtype)=='dist') then
     write(lun,*)   'RTMA CEELING HEIGHT OBS FLAGGED SOON AFTER THEY ARE READ IN FROM THE PREPBUFR FILE'
     write(lun,*)   'OB UNITS are m'
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
      integer(i_kind) i,j,ip,jp
      real(r_single) dx,dy,dxm,dym

      i  = ifix(yy)
      j  = ifix(xx)
      
      dx = xx - float(j)
      dy = yy - float(i)
      dxm= 1.0_r_single-dx
      dym= 1.0_r_single-dy
 
      i=min(max(1,i),ix) ; j=min(max(1,j),jx)
      ip=min(ix,i+1)     ; jp=min(jx,j+1) 

      rfobs=dxm*(dym*rffcst(i,j)+dy*rffcst(ip,j)) &
               + dx *(dym*rffcst(i,jp)+dy*rffcst(ip,jp))

      return
end subroutine bilinear_2d0
!------------------------------------------------------
!------------------------------------------------------
!------------------------------------------------------
subroutine getwdir(ue,ve,wdir)
!                .      .    .                                       .
! subprogram:   getwdir
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!    ue-
!    ve-
!
!   output argument list:
!    wdir
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind
  use constants, only: zero,deg2rad
  implicit none

! Declare passed variables
  real(r_kind),intent(in   ) :: ue,ve
  real(r_kind),intent(  out) :: wdir

! Declare local parameters
  real(r_kind),parameter::r90=90._r_kind
  real(r_kind),parameter::r180=180._r_kind
  real(r_kind),parameter::r270=270._r_kind
  real(r_kind),parameter::r360=360._r_kind

! Declare local variables
  real(r_kind) wspd2,angle

  wspd2=ue*ue+ve*ve
  if (wspd2.eq.zero) then
       wdir=zero 
       goto 100
  endif

  if (ve.eq.zero) then
     if (ue.gt.zero) wdir = r270
     if (ue.lt.zero) wdir = r90 
    else 
     angle = atan(ue/ve)/deg2rad
     if (ue.le.zero .and. ve.le.zero ) wdir = angle
     if (ue.le.zero .and. ve.ge.zero ) wdir = angle + r180
     if (ue.ge.zero .and. ve.ge.zero ) wdir = angle + r180
     if (ue.ge.zero .and. ve.le.zero ) wdir = angle + r360
  endif

100 continue
end subroutine getwdir
!************************************************************
!------------------------------------------------------
!------------------------------------------------------
subroutine windfactor(p0,wfact0)
! subprogram:   windfactor
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!    p0 
!
!   output argument list:
!    wfact
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use constants, only: one
  implicit none

! Declare passed variables
  real(r_kind),intent(in   ) :: p0
  real(r_kind),intent(  out) :: wfact0

! Declare local parameters
  integer(i_kind),parameter::nlevs=14

! Declare local variables
  real(r_kind) plevs(nlevs)
  real(r_kind) wfacts(nlevs)
  real(r_kind) alpha
  integer(i_kind) n

  !note:  wfacts represents the ratio 
  !       wind(n)/wind10m

  plevs(1)=1000._r_kind ; wfacts(1)=1.16
  plevs(2)=975._r_kind  ; wfacts(2)=1.23
  plevs(3)=950._r_kind  ; wfacts(3)=1.27
  plevs(4)=925._r_kind  ; wfacts(4)=1.31
  plevs(5)=900._r_kind  ; wfacts(5)=1.34
  plevs(6)=875._r_kind  ; wfacts(6)=1.33
  plevs(7)=850._r_kind  ; wfacts(7)=1.29
  plevs(8)=825._r_kind  ; wfacts(8)=1.25
  plevs(9)=800._r_kind  ; wfacts(9)=1.19
  plevs(10)=750._r_kind ; wfacts(10)=1.12
  plevs(11)=725._r_kind ; wfacts(11)=1.16
  plevs(12)=700._r_kind ; wfacts(12)=1.20
  plevs(13)=650._r_kind ; wfacts(13)=1.23
  plevs(14)=600._r_kind ; wfacts(14)=1.14

  do n=1,nlevs-1
     if (p0.ge.plevs(n+1) .and. p0.le.plevs(n)) then
        alpha=(wfacts(n+1)-wfacts(n))/(plevs(n+1)-plevs(n))
        wfact0=wfacts(n)+alpha*(p0-plevs(n))
     endif
  enddo

  if (p0 < plevs(nlevs))  wfact0=wfacts(nlevs)
  if (p0 > plevs(1))      wfact0=wfacts(1)

  wfact0=one/wfact0
end subroutine windfactor
!============================================================
subroutine shuffle(ngrps,ngrp0)

  use kinds, only: r_kind,i_kind
  use obsmod, only: iadate
  implicit none

!Declare passed variables
  integer(i_kind),intent(in)::  ngrps
  integer(i_kind),intent(out):: ngrp0

!Declare local variables
  integer(i_kind) iseed,n,nt
  real(r_kind) randx

  call w3fs21(iadate,iseed) !use # of minutes since 0000, 1 jan 1978
                            !as the seed

  nt=sum(iadate)            !arbitrarily select number of times to call
                            !random number generator
 
  print*,'in shuffle: iadate,iseed,nt=',iadate,iseed,nt

! do n=1,max(1,nt/5) !nt
!    call random8(iseed,randx) 
! enddo

! ngrp0=nint(real((ngrps-1),r_kind)*randx+0.001_r_kind)+1

! print*,'in shuffle: ngrps,randx,ngrp0=',ngrps,randx,ngrp0

! use this instead:  /20Jan2011
  ngrp0=mod(nt,ngrps)
  if (ngrp0==0) ngrp0=ngrps
  print*,'in shuffle:ngrps,ngrp0=',ngrps,ngrp0

end subroutine shuffle
!============================================================
subroutine random8(iseed,randx)

! this subroutine generates random numbers between 0.0 and 1.0
! using an integer seed

  use kinds, only: r_kind,i_kind
  implicit none

!Declare passed variables
  integer(i_kind),intent(inout):: iseed
  real(r_kind),intent(out):: randx

  iseed=2045*iseed+1
  iseed=iseed-(iseed/1048576)*1048576
  randx=real((iseed+1),r_kind)/1048577.0_r_kind

  return
end subroutine random8
!===========================================================================
