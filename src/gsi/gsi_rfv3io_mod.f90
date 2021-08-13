module gsi_rfv3io_mod
!$$$   module documentation block
!             .      .    .                                       .
! module:     gsi_rfv3io_mod
!   prgmmr:
!
! abstract: IO routines for regional FV3
!
! program history log:
!   2017-03-08  parrish - create new module gsi_rfv3io_mod, starting from
!                           gsi_nemsio_mod as a pattern.
!   2017-10-10  wu      - setup A grid and interpolation coeff in generate_anl_grid
!   2018-02-22  wu      - add subroutines for read/write fv3_ncdf
!   2019        ting    - modifications for use for ensemble IO and cold start files 
!   2020-11-19  Lu & Wang - add time label it for fgat. POC: xuguang.wang@ou.edu
!   2021-02-01  Lu & Wang - modify functions for hafs dual ens. POC:
!   xuguang.wang@ou.edu
! subroutines included:
!   sub gsi_rfv3io_get_grid_specs
!   sub read_fv3_files 
!   sub read_fv3_netcdf_guess
!   sub gsi_fv3ncdf2d_read
!   sub gsi_fv3ncdf_read
!   sub gsi_fv3ncdf_readuv
!   sub wrfv3_netcdf
!   sub gsi_fv3ncdf_writeuv
!   sub gsi_fv3ncdf_writeps
!   sub gsi_fv3ncdf_write
!   sub gsi_fv3ncdf_write_v1
!   sub check
!
! variable definitions:
!
! attributes:
!   langauge: f90
!    machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlon_regional,nlat_regional
  use mod_fv3_lolgrid, only: p_fv3sar2anlgrid 
  use mod_fv3_lolgrid, only: p_fv3sar2ensgrid 
  implicit none
  public type_fv3regfilenameg
  public bg_fv3regfilenameg
  public fv3sar_bg_opt

!    directory names (hardwired for now)
  type type_fv3regfilenameg
      character(len=:),allocatable :: grid_spec !='fv3_grid_spec'            
      character(len=:),allocatable :: ak_bk     !='fv3_akbk'
      character(len=:),allocatable :: dynvars   !='fv3_dynvars'
      character(len=:),allocatable :: tracers   !='fv3_tracer'
      character(len=:),allocatable :: sfcdata   !='fv3_sfcdata'
      character(len=:),allocatable :: couplerres!='coupler.res'
      contains
      procedure , pass(this):: init=>fv3regfilename_init
  end type type_fv3regfilenameg

  integer(i_kind):: fv3sar_bg_opt=0
  type(type_fv3regfilenameg):: bg_fv3regfilenameg
  integer(i_kind) nx,ny,nz,nxens,nyens
  real(r_kind),allocatable:: grid_lon(:,:),grid_lont(:,:),grid_lat(:,:),grid_latt(:,:)
  real(r_kind),allocatable:: ak(:),bk(:)
  integer(i_kind),allocatable:: ijns2d(:),displss2d(:),ijns(:),displss(:)
  integer(i_kind),allocatable:: ijnz(:),displsz_g(:)
  integer(i_kind),allocatable:: ijns2dens(:),displss2dens(:),ijnsens(:),displssens(:)
  integer(i_kind),allocatable:: ijnzens(:),displsz_gens(:)
! set default to private
  private
! set subroutines to public
  public :: gsi_rfv3io_get_grid_specs
  public :: gsi_rfv3io_get_ens_grid_specs
  public :: gsi_fv3ncdf_read
  public :: gsi_fv3ncdf_read_v1
  public :: gsi_fv3ncdf_readuv
  public :: gsi_fv3ncdf_readuv_v1
  public :: read_fv3_files 
  public :: read_fv3_netcdf_guess
  public :: wrfv3_netcdf
  public :: gsi_fv3ncdf2d_read_v1

  public :: mype_u,mype_v,mype_t,mype_q,mype_p,mype_oz,mype_ql
  public :: k_slmsk,k_tsea,k_vfrac,k_vtype,k_stype,k_zorl,k_smc,k_stc
  public :: k_snwdph,k_f10m,mype_2d,n2d,k_orog,k_psfc
  public :: ijns,ijns2d,displss,displss2d,ijnz,displsz_g
  public :: ijnsens,ijns2dens,displssens,displss2dens,ijnzens,displsz_gens

  integer(i_kind) mype_u,mype_v,mype_t,mype_q,mype_p,mype_delz,mype_oz,mype_ql
  integer(i_kind) k_slmsk,k_tsea,k_vfrac,k_vtype,k_stype,k_zorl,k_smc,k_stc
  integer(i_kind) k_snwdph,k_f10m,mype_2d,n2d,k_orog,k_psfc
  parameter(                   &  
    k_f10m =1,                  &   !fact10
    k_stype=2,                  &   !soil_type
    k_vfrac=3,                  &   !veg_frac
    k_vtype=4,                 &   !veg_type
    k_zorl =5,                &   !sfc_rough
    k_tsea =6,                  &   !sfct ?
    k_snwdph=7,                &   !sno ?
    k_stc  =8,                  &   !soil_temp
    k_smc  =9,                  &   !soil_moi
    k_slmsk=10,                 &   !isli
    k_orog =11,                 & !terrain
    n2d=11                   )

contains
  subroutine fv3regfilename_init(this,it,grid_spec_input,ak_bk_input,dynvars_input, &
                      tracers_input,sfcdata_input,couplerres_input)
  implicit None
  class(type_fv3regfilenameg),intent(inout):: this
  character(*),optional :: grid_spec_input,ak_bk_input,dynvars_input, &
                      tracers_input,sfcdata_input,couplerres_input
  integer,intent(in) :: it
  character(255):: filename
  if(present(grid_spec_input))then
    this%grid_spec=grid_spec_input
  else if (it.eq.6) then 
    this%grid_spec='fv3_grid_spec'
  else
    write(filename,"(A14,I2.2)") 'fv3_grid_spec_',(it)
    this%grid_spec=trim(filename)
  endif
  if(present(ak_bk_input))then
    this%ak_bk=ak_bk_input
  else if (it.eq.6) then
    this%ak_bk='fv3_ak_bk'
  else
    write(filename,"(A10,I2.2)") 'fv3_ak_bk_',(it)
    this%ak_bk=trim(filename)
  endif
  if(present(dynvars_input))then
    this%dynvars=dynvars_input
  else if (it.eq.6) then
    this%dynvars='fv3_dynvars'
  else
    write(filename,"(A12,I2.2)") 'fv3_dynvars_',(it)
    this%dynvars=trim(filename)
  endif
  if(present(tracers_input))then
    this%tracers=tracers_input
  else if (it.eq.6) then
    this%tracers='fv3_tracer'
  else
    write(filename,"(A11,I2.2)") 'fv3_tracer_',(it)
    this%tracers=trim(filename)
  endif
  if(present(sfcdata_input))then
    this%sfcdata=sfcdata_input
  else if (it.eq.6) then
    this%sfcdata='fv3_sfcdata'
  else
    write(filename,"(A12,I2.2)") 'fv3_sfcdata_',(it)
    this%sfcdata=trim(filename)
  endif

  if(present(couplerres_input))then
    this%couplerres=couplerres_input
  else if (it.eq.6) then
    this%couplerres='coupler.res'
  else
    write(filename,"(A12,I2.2)") 'coupler.res_',(it)
    this%couplerres=trim(filename)
  endif

  end subroutine fv3regfilename_init


subroutine gsi_rfv3io_get_grid_specs(fv3filenamegin,ierr)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_rfv3io_get_grid_specs
!   pgrmmr: parrish     org: np22                date: 2017-04-03
!
! abstract:  obtain grid dimensions nx,ny and grid definitions
!                grid_x,grid_xt,grid_y,grid_yt,grid_lon,grid_lont,grid_lat,grid_latt
!                nz,ak(nz),bk(nz)
!
! program history log:
!   2017-04-03  parrish - initial documentation
!   2017-10-10  wu - setup A grid and interpolation coeff with generate_anl_grid
!   2018-02-16  wu - read in time info from file coupler.res
!                    read in lat, lon at the center and corner of the grid cell
!                    from file fv3_grid_spec, and vertical grid infor from file fv3_akbk
!                    setup A grid and interpolation/rotation coeff
!   input argument list:
!    grid_spec
!    ak_bk
!    lendian_out
!
!   output argument list:
!    ierr
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
  use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
  use netcdf, only: nf90_inquire_variable
  use mpimod, only: mype
!cltorg  use mod_fv3_lola, only: generate_anl_grid
  use mod_fv3_lolgrid, only: generate_regular_grids
  use gridmod,  only:nsig,regional_time,regional_fhr,regional_fmin,aeta1_ll,aeta2_ll
  use gridmod,  only:nlon_regional,nlat_regional,eta1_ll,eta2_ll
  use gridmod,  only:region_lat,region_lon,nlat,nlon
  use gridmod,  only: region_dy,region_dx,region_dyi,region_dxi,coeffy,coeffx
  use kinds, only: i_kind,r_kind
  use constants, only: half,zero
  use mpimod, only: mpi_comm_world,mpi_itype,mpi_rtype

  implicit none
  integer(i_kind) gfile_grid_spec
  type (type_fv3regfilenameg) :: fv3filenamegin
  character(:),allocatable    :: grid_spec
  character(:),allocatable    :: ak_bk
  character(len=:),allocatable :: coupler_res_filenam 
  integer(i_kind),intent(  out) :: ierr
  integer(i_kind) i,k,ndimensions,iret,nvariables,nattributes,unlimiteddimid
  integer(i_kind) len,gfile_loc
  character(len=128) :: name
  integer(i_kind) myear,mmonth,mday,mhour,mminute,msecond
  real(r_kind),allocatable:: abk_fv3(:)

    coupler_res_filenam=fv3filenamegin%couplerres
    grid_spec=fv3filenamegin%grid_spec
    ak_bk=fv3filenamegin%ak_bk

!!!!! set regional_time
    open(24,file=trim(coupler_res_filenam),form='formatted')
    read(24,*)
    read(24,*)
    read(24,*)myear,mmonth,mday,mhour,mminute,msecond
    close(24)
    if(mype==0)  write(6,*)' myear,mmonth,mday,mhour,mminute,msecond=', myear,mmonth,mday,mhour,mminute,msecond
    regional_time(1)=myear
    regional_time(2)=mmonth
    regional_time(3)=mday
    regional_time(4)=mhour
    regional_time(5)=mminute
    regional_time(6)=msecond
    regional_fhr=zero          ! forecast hour set zero for now
    regional_fmin=zero          ! forecast min set zero for now

!!!!!!!!!!    grid_spec  !!!!!!!!!!!!!!!
    ierr=0
    iret=nf90_open(trim(grid_spec),nf90_nowrite,gfile_grid_spec)
    if(iret/=nf90_noerr) then
       write(6,*)' gsi_rfv3io_get_grid_specs: problem opening ',trim(grid_spec),', Status = ',iret
       ierr=1
       return
    endif

    iret=nf90_inquire(gfile_grid_spec,ndimensions,nvariables,nattributes,unlimiteddimid)
    gfile_loc=gfile_grid_spec
    do k=1,ndimensions
       iret=nf90_inquire_dimension(gfile_loc,k,name,len)
       if(trim(name)=='grid_xt') nx=len
       if(trim(name)=='grid_yt') ny=len
    enddo
    nlon_regional=nx
    nlat_regional=ny
    if(mype==0)write(6,*),'nx,ny=',nx,ny

!!!    get nx,ny,grid_lon,grid_lont,grid_lat,grid_latt,nz,ak,bk

    allocate(grid_lat(nx+1,ny+1))
    allocate(grid_lon(nx+1,ny+1))
    allocate(grid_latt(nx,ny))
    allocate(grid_lont(nx,ny))

    do k=ndimensions+1,nvariables
       iret=nf90_inquire_variable(gfile_loc,k,name,len)
       if(trim(name)=='grid_lat') then
          iret=nf90_get_var(gfile_loc,k,grid_lat)
       endif
       if(trim(name)=='grid_lon') then
          iret=nf90_get_var(gfile_loc,k,grid_lon)
       endif
       if(trim(name)=='grid_latt') then
          iret=nf90_get_var(gfile_loc,k,grid_latt)
       endif
       if(trim(name)=='grid_lont') then
          iret=nf90_get_var(gfile_loc,k,grid_lont)
       endif
    enddo

    iret=nf90_close(gfile_loc)

    iret=nf90_open(ak_bk,nf90_nowrite,gfile_loc)
    if(iret/=nf90_noerr) then
       write(6,*)'gsi_rfv3io_get_grid_specs: problem opening ',trim(ak_bk),', Status = ',iret
       ierr=1
       return
    endif
    iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
    do k=1,ndimensions
       iret=nf90_inquire_dimension(gfile_loc,k,name,len)
       if(trim(name)=='xaxis_1') nz=len
    enddo
    if(mype==0)write(6,'(" nz=",i5)') nz

    nsig=nz-1

!!!    get ak,bk

    allocate(aeta1_ll(nsig),aeta2_ll(nsig))
    allocate(eta1_ll(nsig+1),eta2_ll(nsig+1))
    allocate(ak(nz),bk(nz),abk_fv3(nz))

    do k=ndimensions+1,nvariables
       iret=nf90_inquire_variable(gfile_loc,k,name,len)
       if(trim(name)=='ak'.or.trim(name)=='AK') then
          iret=nf90_get_var(gfile_loc,k,abk_fv3)
          do i=1,nz
             ak(i)=abk_fv3(nz+1-i)
          enddo
       endif
       if(trim(name)=='bk'.or.trim(name)=='BK') then
          iret=nf90_get_var(gfile_loc,k,abk_fv3)
          do i=1,nz
             bk(i)=abk_fv3(nz+1-i)
          enddo
       endif
    enddo
    iret=nf90_close(gfile_loc)

!!!!! change unit of ak 
    do i=1,nsig+1
       eta1_ll(i)=ak(i)*0.001_r_kind
       eta2_ll(i)=bk(i)
    enddo
    do i=1,nsig
       aeta1_ll(i)=half*(ak(i)+ak(i+1))*0.001_r_kind
       aeta2_ll(i)=half*(bk(i)+bk(i+1))
    enddo
    if(mype==0)then
       do i=1,nz
          write(6,'(" ak,bk(",i3,") = ",2f17.6)') i,ak(i),bk(i)
       enddo
    endif

!!!!!!! setup A grid and interpolation/rotation coeff.
    call generate_regular_grids(nx,ny,grid_lon,grid_lont,grid_lat,grid_latt,p_fv3sar2anlgrid, &
                              nlat,nlon,region_lat,region_lon, &
                             region_dx,region_dy, &
                             region_dxi,region_dyi, &
                             coeffx,coeffy)


    deallocate (grid_lon,grid_lat,grid_lont,grid_latt)
    deallocate (ak,bk,abk_fv3)

    return
end subroutine gsi_rfv3io_get_grid_specs
subroutine gsi_rfv3io_get_ens_grid_specs(grid_spec,ierr)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_rfv3io_get_ens_grid_specs
! modified from     gsi_rfv3io_get_grid_specs
!   pgrmmr: parrish     org: np22                date: 2017-04-03
!
! abstract:  obtain grid dimensions nx,ny and grid definitions
!                grid_x,grid_xt,grid_y,grid_yt,grid_lon,grid_lont,grid_lat,grid_latt
!                nz,ak(nz),bk(nz)
!
! program history log:
!   2017-04-03  parrish - initial documentation
!   2017-10-10  wu - setup A grid and interpolation coeff with generate_anl_grid
!   2018-02-16  wu - read in time info from file coupler.res
!                    read in lat, lon at the center and corner of the grid cell
!                    from file fv3_grid_spec, and vertical grid infor from file fv3_akbk
!                    setup A grid and interpolation/rotation coeff
!   input argument list:
!    grid_spec
!    ak_bk
!    lendian_out
!
!   output argument list:
!    ierr
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
  use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
  use netcdf, only: nf90_inquire_variable
  use hybrid_ensemble_parameters, only: nlon_ens,nlat_ens
  use hybrid_ensemble_parameters, only: region_lat_ens,region_lon_ens
  use mpimod, only: mype
!cltorg  use mod_fv3_lola, only: generate_anl_grid
  use mod_fv3_lolgrid, only: definecoef_regular_grids,nxa_ens,nya_ens
  use gridmod,  only:region_lat,region_lon,nlat,nlon
  use gridmod,  only: region_dy,region_dx,region_dyi,region_dxi,coeffy,coeffx
  use kinds, only: i_kind,r_kind
  use constants, only: half,zero
  use mpimod, only: mpi_comm_world,ierror,mpi_itype,mpi_rtype
  use hybrid_ensemble_parameters, only: grd_ens
  use gridmod, only: ijn_sens,ijnens,displs_sens,itotsubens
  use general_commvars_mod, only: ltosi_sens,ltosj_sens

  implicit none

  character(:),allocatable,intent(in) :: grid_spec
  integer(i_kind) gfile_grid_spec,gfile_ak_bk,gfile_dynvars
  integer(i_kind) gfile_tracers,gfile_sfcdata
!  integer(i_kind) :: gfile
!  save gfile

  type (type_fv3regfilenameg) :: fv3filenamegin
  integer(i_kind),intent(  out) :: ierr
  integer(i_kind) i,k,ndimensions,iret,nvariables,nattributes,unlimiteddimid
  integer(i_kind) len,gfile_loc
  character(len=128) :: name
  


!!!!! set regional_time
!cltorg    open(24,file='coupler.res',form='formatted')

!!!!!!!!!!    grid_spec  !!!!!!!!!!!!!!!
    iret=nf90_open(trim(grid_spec),nf90_nowrite,gfile_grid_spec)
    if(iret/=nf90_noerr) then
       write(6,*)' problem opening1 ',trim(grid_spec),', Status = ',iret
       ierr=1
       return
    endif
    iret=nf90_inquire(gfile_grid_spec,ndimensions,nvariables,nattributes,unlimiteddimid)
    gfile_loc=gfile_grid_spec
    do k=1,ndimensions
       iret=nf90_inquire_dimension(gfile_loc,k,name,len)
       if(trim(name)=='grid_xt') nxens=len
       if(trim(name)=='grid_yt') nyens=len
    enddo
    if(mype==0)write(6,*),'nxens,nyens=',nxens,nyens

!!!    get nx,ny,grid_lon,grid_lont,grid_lat,grid_latt,nz,ak,bk

    allocate(grid_lat(nxens+1,nyens+1))
    allocate(grid_lon(nxens+1,nyens+1))
    allocate(grid_latt(nxens,nyens))
    allocate(grid_lont(nxens,nyens))

    do k=ndimensions+1,nvariables
       iret=nf90_inquire_variable(gfile_loc,k,name,len)
       if(trim(name)=='grid_lat') then
          iret=nf90_get_var(gfile_loc,k,grid_lat)
       endif
       if(trim(name)=='grid_lon') then
          iret=nf90_get_var(gfile_loc,k,grid_lon)
       endif
       if(trim(name)=='grid_latt') then
          iret=nf90_get_var(gfile_loc,k,grid_latt)
       endif
       if(trim(name)=='grid_lont') then
          iret=nf90_get_var(gfile_loc,k,grid_lont)
       endif
    enddo

    iret=nf90_close(gfile_loc)

!!!    get ak,bk


!!!!!!! setup A grid and interpolation/rotation coeff.
    ltosi_sens=grd_ens%ltosi_s
    ltosj_sens=grd_ens%ltosj_s
    ijn_sens=grd_ens%ijn_s
    ijnens=grd_ens%ijn
    itotsubens=grd_ens%itotsub
    displs_sens=grd_ens%displs_s
    call definecoef_regular_grids(nxens,nyens,nxa_ens,nya_ens,grid_lon,grid_lont,grid_lat,grid_latt,p_fv3sar2ensgrid, &
                              nlat_ens,nlon_ens,region_lat_ens,region_lon_ens)

    deallocate (grid_lon,grid_lat,grid_lont,grid_latt)

    return
end subroutine gsi_rfv3io_get_ens_grid_specs

subroutine read_fv3_files(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_fv3_files
!   prgmmr: wu               org: np22                date: 2017-10-10
!
! abstract: read in from fv3 files and figure out available time levels 
!           of background fields starting from read_files as a pattern
!           temporary setup for one first guess file
! program history log:
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
!$$$  end documentation block

    use kinds, only: r_kind,i_kind
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe
    use guess_grids, only: nfldsig,nfldsfc,ntguessig,ntguessfc,&
         ifilesig,ifilesfc,hrdifsig,hrdifsfc,create_gesfinfo
    use guess_grids, only: hrdifsig_all,hrdifsfc_all
    use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,nhr_assimilation
    use constants, only: zero,one,r60inv
    use obsmod, only: iadate,time_offset
    use gridmod, only:regional_time
    implicit none

! Declare passed variables
    integer(i_kind),intent(in   ) :: mype

! Declare local parameters
    real(r_kind),parameter:: r0_001=0.001_r_kind

! Declare local variables
    logical(4) fexist
    character(6) filename
    character(14) filenames
    integer(i_kind) in_unit
    integer(i_kind) i,j,iwan,npem1
    integer(i_kind) nhr_half
    integer(i_kind) nminanl,nmings,nming2,ndiff,isecond
    integer(i_kind),dimension(4):: idateg
    integer(i_kind),dimension(5):: idate5
    character*1 :: datein(4),dateout(4)
    integer(i_kind) datecc
    real(r_kind) hourg,temp,t4dv
    real(r_kind),dimension(202,2):: time_ges

!-----------------------------------------------------------------------------
! Start read_nems_nmmb_files here.
    nhr_half=nhr_assimilation/2
    if(nhr_half*2 < nhr_assimilation) nhr_half=nhr_half+1
    npem1=npe-1

    do i=1,202
       time_ges(i,1) = 999_r_kind
       time_ges(i,2) = 999_r_kind
    end do


! Let a single task query the guess files.
    if(mype==npem1) then

!    Convert analysis time to minutes relative to fixed date
       call w3fs21(iadate,nminanl)
       write(6,*)'READ_netcdf_fv3_FILES:  analysis date,minutes ',iadate,nminanl

!    Check for consistency of times from sigma guess files.
       in_unit=15
       iwan=0
!WWWWWW setup for one first guess file for now
       do i=0,99 !place holder for FGAT
          if (i.eq.6) then
            write(filenames,"(A11)") 'fv3_dynvars'
          else
            write(filenames,"(A12,I2.2)") 'fv3_dynvars_',i
          endif
          INQUIRE(FILE=filenames, EXIST=fexist)
          if(.not.fexist) cycle

!wwww read in from the external file directly, no internal files sigfxx for FV3
          datecc=regional_time(1)/100
          datein(1)=char(regional_time(1)-datecc*100)
          datein(2)=char(regional_time(2))
          datein(3)=char(regional_time(3))
          datein(4)=char(regional_time(4))
          call w3fs15(datein,(i-6),dateout)
          idate5(1)=ichar(dateout(1))+datecc*100
          idate5(2)=ichar(dateout(2))
          idate5(3)=ichar(dateout(3))
          idate5(4)=ichar(dateout(4))
          idate5(5)=  regional_time(5)
          isecond  =  regional_time(6)
          hourg    =  zero ! forcast hour

          call w3fs21(idate5,nmings)
          nming2=nmings+60*hourg
          write(6,*)'READ_netcdf_fv3_FILES:  sigma guess file, nming2 ',hourg,idate5,nming2
          t4dv=real((nming2-iwinbgn),r_kind)*r60inv
          if (l4dvar.or.l4densvar) then
             if (t4dv<zero .OR. t4dv>winlen) then
                write(6,*)'ges file not in time range, t4dv=',t4dv
                cycle ! place holder for FGAT
             endif
          else
             ndiff=nming2-nminanl
!for test with the 3 hr files with FGAT
             if(abs(ndiff) > 60*nhr_half ) then
                write(6,*)'ges file not in time range, ndiff=',ndiff
                cycle ! place holder for FGAT
             endif
          endif
          iwan=iwan+1
          time_ges(iwan,1) =real((nming2-iwinbgn),r_kind)*r60inv
          time_ges(iwan+100,1)=i+r0_001
       end do ! i !place holder for FGAT
       time_ges(201,1)=one
       time_ges(202,1)=one
       if(iwan > 1)then
          do i=1,iwan
             do j=i+1,iwan
                if(time_ges(j,1) < time_ges(i,1))then
                   temp=time_ges(i+100,1)
                   time_ges(i+100,1)=time_ges(j+100,1)
                   time_ges(j+100,1)=temp
                   temp=time_ges(i,1)
                   time_ges(i,1)=time_ges(j,1)
                   time_ges(j,1)=temp
                end if
             end do
             if(abs(time_ges(i,1)-time_offset) < r0_001)time_ges(202,1) = i
          end do
       end if
       time_ges(201,1) = iwan+r0_001

!    Check for consistency of times from surface guess files.
       iwan=0
       do i=0,99
          if (i.eq.6) then
           write(filenames,"(A11)") 'fv3_sfcdata'
          else
           write(filenames,"(A12,I2.2)") 'fv3_sfcdata_',i
          endif
          INQUIRE(FILE=filenames, EXIST=fexist)
          if(fexist)then
             datecc=iadate(1)/100
             datein(1)=char(iadate(1)-datecc*100)
             datein(2)=char(iadate(2))
             datein(3)=char(iadate(3))
             datein(4)=char(iadate(4))
             call w3fs15(datein,(i-6),dateout)
             idateg(1)=ichar(dateout(1))+datecc*100
             idateg(2)=ichar(dateout(2))
             idateg(3)=ichar(dateout(3))
             idateg(4)=ichar(dateout(4))
             hourg = zero
             idate5(1)=idateg(4); idate5(2)=idateg(2)
             idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=0
             call w3fs21(idate5,nmings)
             nming2=nmings+60*hourg
             write(6,*)'READ_netcdf_fv3_FILES:  surface guess file, nming2 ',hourg,idateg,nming2
             ndiff=nming2-nminanl
             if(abs(ndiff) > 60*nhr_half ) then
                write(6,*)'ges file not in time range, ndiff=',ndiff
                cycle ! place holder for FGAT
             endif
             iwan=iwan+1
             time_ges(iwan,2) =real((nming2-iwinbgn),r_kind)*r60inv
             time_ges(iwan+100,2)=i+r0_001
          end if
          if(iwan==1) exit
       end do
       time_ges(201,2)=one
       time_ges(202,2)=one
       if(iwan > 1)then
          do i=1,iwan
             do j=i+1,iwan
                if(time_ges(j,2) < time_ges(i,2))then
                   temp=time_ges(i+100,2)
                   time_ges(i+100,2)=time_ges(j+100,2)
                   time_ges(j+100,2)=temp
                   temp=time_ges(i,2)
                   time_ges(i,2)=time_ges(j,2)
                   time_ges(j,2)=temp
                end if
             end do
             if(abs(time_ges(i,2)-time_offset) < r0_001)time_ges(202,2) = i
          end do
       end if
       time_ges(201,2) = iwan+r0_001
    end if


! Broadcast guess file information to all tasks
    call mpi_bcast(time_ges,404,mpi_rtype,npem1,mpi_comm_world,ierror)

    nfldsig   = nint(time_ges(201,1))
!!nfldsfc   = nint(time_ges(201,2))
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
    ntguessig = nint(time_ges(202,1))
    do i=1,nfldsig
       hrdifsig(i) = time_ges(i,1)
       ifilesig(i) = nint(time_ges(i+100,1))
       hrdifsig_all(i) = hrdifsig(i)
    end do
    if(mype == 0) write(6,*)'READ_netcdf_fv3_FILES:  sigma fcst files used in analysis  :  ',&
         (ifilesig(i),i=1,nfldsig),(hrdifsig(i),i=1,nfldsig),ntguessig


! Load time information for surface guess field info into output arrays
    ntguessfc = nint(time_ges(202,2))
    do i=1,nfldsfc
       hrdifsfc(i) = time_ges(i,2)
       ifilesfc(i) = nint(time_ges(i+100,2))
       hrdifsfc_all(i) = hrdifsfc(i)
    end do

! Below is a temporary fix. The nems_nmmb regional mode does not have a
! surface
! file.  Instead the surface fields are passed through the atmospheric guess
! file.  Without a separate surface file the code above sets ntguessig and 
! nfldsig to zero.  This causes problems later in the code when arrays for
! the surface fields are allocated --> one of the array dimensions is nfldsfc
! and it will be zero.  This portion of the code should be rewritten, but
! until
! it is, the fix below gets around the above mentioned problem.

    ntguessfc = ntguessig
!!nfldsfc   = nfldsig
    do i=1,nfldsfc
       hrdifsfc(i) = hrdifsig(i)
       ifilesfc(i) = ifilesig(i)
       hrdifsfc_all(i) = hrdifsfc(i)
    end do
    if(mype == 0) write(6,*)'READ_nems_nmb_FILES:  surface fcst files used in analysis:  ',&
         (ifilesfc(i),i=1,nfldsfc),(hrdifsfc(i),i=1,nfldsfc),ntguessfc


! End of routine
    return
end subroutine read_fv3_files

subroutine read_fv3_netcdf_guess(fv3filenamegin,it)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_fv3_netcdf_guess            read fv3 interface file
!   prgmmr: wu               org: np22                date: 2017-07-06
!
! abstract:  read guess for FV3 regional model
! program history log:
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use kinds, only: r_kind,i_kind
    use mpimod, only: npe,mype
    use guess_grids, only: ges_tsen,ges_prsi
    use gridmod, only: lat2,lon2,nsig,ijn,eta1_ll,eta2_ll,ijn_s
    use gridmod, only: ijnens,ijn_sens
    use constants, only: one,fv
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die
    use guess_grids, only: ntguessig,nfldsig

    implicit none

    type (type_fv3regfilenameg),intent (in) :: fv3filenamegin
    integer(i_kind),intent (in) :: it
    character(len=24),parameter :: myname = 'read_fv3_netcdf_guess'
    integer(i_kind) k,i,j
    integer(i_kind) ier,istatus
    real(r_kind),dimension(:,:),pointer::ges_ps=>NULL()
    real(r_kind),dimension(:,:),pointer::ges_z=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_u=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_v=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_q=>NULL()
!   real(r_kind),dimension(:,:,:),pointer::ges_ql=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_oz=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
            
      character(len=:),allocatable :: dynvars   !='fv3_dynvars'
      character(len=:),allocatable :: tracers   !='fv3_tracer'
   


     dynvars= fv3filenamegin%dynvars
     tracers= fv3filenamegin%tracers

    if(npe< 8) then
       call die('read_fv3_netcdf_guess','not enough PEs to read in fv3 fields' )
    endif
    mype_u=0           
    mype_v=1
    mype_t=2
    mype_p=3
    mype_q=4
    mype_ql=5
    mype_oz=6
    mype_2d=7 
    mype_delz=8
      
!   allocate(ijns(npe),ijns2d(npe),ijnz(npe) )
!   allocate(displss(npe),displss2d(npe),displsz_g(npe) )
    if (.not.allocated(ijns)) allocate(ijns(npe))
    if (.not.allocated(ijns2d)) allocate(ijns2d(npe))
    if (.not.allocated(ijnz)) allocate(ijnz(npe))
    if (.not.allocated(displss)) allocate(displss(npe))
    if (.not.allocated(displss2d)) allocate(displss2d(npe))
    if (.not.allocated(displsz_g)) allocate(displsz_g(npe))
    if (.not.allocated(ijnsens)) allocate(ijnsens(npe))
    if (.not.allocated(ijns2dens)) allocate(ijns2dens(npe))
    if (.not.allocated(ijnzens)) allocate(ijnzens(npe))
    if (.not.allocated(displssens)) allocate(displssens(npe))
    if (.not.allocated(displss2dens)) allocate(displss2dens(npe))
    if (.not.allocated(displsz_gens)) allocate(displsz_gens(npe))
    do i=1,npe
       ijns(i)=ijn_s(i)*nsig
       ijnz(i)=ijn(i)*nsig
       ijns2d(i)=ijn_s(i)*n2d 
       ijnsens(i)=ijn_sens(i)*nsig
       ijnzens(i)=ijnens(i)*nsig
       ijns2dens(i)=ijn_sens(i)*n2d
    enddo
    displss(1)=0
    displsz_g(1)=0
    displss2d(1)=0
    do i=2,npe
       displss(i)=displss(i-1)+ ijns(i-1)
       displsz_g(i)=displsz_g(i-1)+ ijnz(i-1)
       displss2d(i)=displss2d(i-1)+ ijns2d(i-1)
    enddo
    displssens(1)=0
    displsz_gens(1)=0
    displss2dens(1)=0
    do i=2,npe
       displssens(i)=displssens(i-1)+ ijnsens(i-1)
       displsz_gens(i)=displsz_gens(i-1)+ ijnzens(i-1)
       displss2dens(i)=displss2dens(i-1)+ ijns2dens(i-1)
    enddo
!   do it=1,nfldsig
!   it=ntguessig


    ier=0
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps' ,ges_ps ,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z' , ges_z ,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u' , ges_u ,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v' , ges_v ,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv' ,ges_tv ,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q'  ,ges_q ,istatus );ier=ier+istatus
!   call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql'  ,ges_ql ,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'oz'  ,ges_oz ,istatus );ier=ier+istatus
    if (ier/=0) call die(trim(myname),'cannot get pointers for fv3 met-fields, ier =',ier)
     
    if( fv3sar_bg_opt == 0) then 
       call gsi_fv3ncdf_readuv(dynvars,ges_u,ges_v,lat2,lon2,.false.)
    else
       call gsi_fv3ncdf_readuv_v1(dynvars,ges_u,ges_v,lat2,lon2,.false.)
    endif
    if( fv3sar_bg_opt == 0) then 
       call gsi_fv3ncdf_read(dynvars,'T','t',ges_tsen(1,1,1,it),mype_t,lat2,lon2,.false.)
    else
       call gsi_fv3ncdf_read_v1(dynvars,'t','T',ges_tsen(1,1,1,it),mype_t,lat2,lon2,.false.)
    endif

    if( fv3sar_bg_opt == 0) then 
!      call gsi_fv3ncdf_read(dynvars,'DELP','delp',ges_prsi,mype_p)
       call gsi_fv3ncdf_read(dynvars,'DELP','delp',ges_prsi(:,:,:,it),mype_p,lat2,lon2,.false.)
       ges_prsi(:,:,nsig+1,it)=eta1_ll(nsig+1)
       do i=nsig,1,-1
          ges_prsi(:,:,i,it)=ges_prsi(:,:,i,it)*0.001_r_kind+ges_prsi(:,:,i+1,it)
       enddo
       ges_ps(:,:)=ges_prsi(:,:,1,it)
    else  
       call gsi_fv3ncdf2d_read_v1(dynvars,'ps','PS',ges_ps,mype_p,lat2,lon2,.false.)
       ges_ps=ges_ps*0.001_r_kind
       ges_prsi(:,:,nsig+1,it)=eta1_ll(nsig+1)
       do k=1,nsig
          ges_prsi(:,:,k,it)=eta1_ll(k)+eta2_ll(k)*ges_ps  
       enddo
    endif

    if( fv3sar_bg_opt == 0) then 
      call gsi_fv3ncdf_read(tracers,'SPHUM','sphum',ges_q,mype_q,lat2,lon2,.false.)
!     call gsi_fv3ncdf_read(tracers,'LIQ_WAT','liq_wat',ges_ql,mype_ql)
      call gsi_fv3ncdf_read(tracers,'O3MR','o3mr',ges_oz,mype_oz,lat2,lon2,.false.)
    else
      call gsi_fv3ncdf_read_v1(tracers,'sphum','SPHUM',ges_q,mype_q,lat2,lon2,.false.)
      call gsi_fv3ncdf_read_v1(tracers,'o3mr','O3MR',ges_oz,mype_oz,lat2,lon2,.false.)
    endif

!!  tsen2tv  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do k=1,nsig
       do j=1,lon2
          do i=1,lat2
             ges_tv(i,j,k)=ges_tsen(i,j,k,it)*(one+fv*ges_q(i,j,k))
          enddo
       enddo
    enddo

    call gsi_fv3ncdf2d_read(fv3filenamegin,it,ges_z)


end subroutine read_fv3_netcdf_guess

subroutine gsi_fv3ncdf2d_read(fv3filenamegin,it,ges_z)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv3ncdf2d_read       
!   prgmmr: wu w             org: np22                date: 2017-10-17
!
! abstract: read in 2d fields from fv3_sfcdata file in mype_2d 
!                Scatter the field to each PE 
! program history log:
!   input argument list:
!     it    - time index for 2d fields
!
!   output argument list:
!     ges_z - surface elevation
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_comm_world,npe,mpi_rtype,mype
    use guess_grids, only: fact10,soil_type,veg_frac,veg_type,sfc_rough, &
         sfct,sno,soil_temp,soil_moi,isli
    use gridmod, only: lat2,lon2,itotsub,ijn_s
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use mod_fv3_lolgrid, only: fv3_h_to_ll_regular_grids,nxa,nya,nxa_ens,nya_ens
    use constants, only: grav

    implicit none

    integer(i_kind),intent(in) :: it   
    real(r_kind),intent(in),dimension(:,:),pointer::ges_z
    type (type_fv3regfilenameg),intent(in) :: fv3filenamegin
    character(len=128) :: name
    integer(i_kind),allocatable,dimension(:):: dim_id,dim
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: a
    real(r_kind),allocatable,dimension(:,:,:):: sfcn2d
    real(r_kind),allocatable,dimension(:,:,:):: sfc
    real(r_kind),allocatable,dimension(:,:):: sfc1
    integer(i_kind) iret,gfile_loc,i,k,len,ndim
    integer(i_kind) ndimensions,nvariables,nattributes,unlimiteddimid
    integer(i_kind) kk,n,ns,j,ii,jj,mm1
      character(len=:),allocatable :: sfcdata   !='fv3_sfcdata'
      character(len=:),allocatable :: dynvars   !='fv3_dynvars'

    sfcdata= fv3filenamegin%sfcdata
    dynvars= fv3filenamegin%dynvars

    mm1=mype+1
    allocate(a(nya,nxa))
    allocate(work(itotsub*n2d))
    allocate( sfcn2d(lat2,lon2,n2d))

 if(mype==mype_2d ) then
    iret=nf90_open(sfcdata,nf90_nowrite,gfile_loc)
    if(iret/=nf90_noerr) then
       write(6,*)' problem opening3 ',trim(sfcdata),', Status = ',iret
       return
    endif
    iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
    allocate(dim(ndimensions))
    do k=1,ndimensions
       iret=nf90_inquire_dimension(gfile_loc,k,name,len)
       dim(k)=len
    enddo
!!!!!!!!!!!! read in 2d variables !!!!!!!!!!!!!!!!!!!!!!!!!!
    do i=ndimensions+1,nvariables
       iret=nf90_inquire_variable(gfile_loc,i,name,len)
       if( trim(name)=='f10m'.or.trim(name)=='F10M' ) then
          k=k_f10m
       else if( trim(name)=='stype'.or.trim(name)=='STYPE' ) then
          k=k_stype
       else if( trim(name)=='vfrac'.or.trim(name)=='VFRAC' ) then
          k=k_vfrac
       else if( trim(name)=='vtype'.or.trim(name)=='VTYPE' ) then
          k=k_vtype
       else if( trim(name)=='zorl'.or.trim(name)=='ZORL' ) then
          k=k_zorl
       else if( trim(name)=='tsea'.or.trim(name)=='TSEA' ) then
          k=k_tsea
       else if( trim(name)=='sheleg'.or.trim(name)=='SHELEG' ) then
          k=k_snwdph
       else if( trim(name)=='stc'.or.trim(name)=='STC' ) then
          k=k_stc 
       else if( trim(name)=='smc'.or.trim(name)=='SMC' ) then
          k=k_smc
       else if( trim(name)=='SLMSK'.or.trim(name)=='slmsk' ) then
          k=k_slmsk
       else
          cycle 
       endif
       iret=nf90_inquire_variable(gfile_loc,i,ndims=ndim)
       if(allocated(dim_id    )) deallocate(dim_id    )
       allocate(dim_id(ndim))
       iret=nf90_inquire_variable(gfile_loc,i,dimids=dim_id)
       if(allocated(sfc       )) deallocate(sfc       )
       allocate(sfc(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
       iret=nf90_get_var(gfile_loc,i,sfc)
       call fv3_h_to_ll_regular_grids(sfc(:,:,1),a,nx,ny,nxa,nya,p_fv3sar2anlgrid)

       kk=0
       do n=1,npe
          ns=displss2d(n)+(k-1)*ijn_s(n)
          do j=1,ijn_s(n)
             ns=ns+1
             kk=kk+1
             ii=ltosi_s(kk)
             jj=ltosj_s(kk)
             work(ns)=a(ii,jj)
          end do
       end do
    enddo ! i
    iret=nf90_close(gfile_loc)

!!!! read in orog from dynam !!!!!!!!!!!!
    iret=nf90_open(trim(dynvars ),nf90_nowrite,gfile_loc)
    if(iret/=nf90_noerr) then
       write(6,*)' problem opening4 ',trim(dynvars ),gfile_loc,', Status = ',iret
       return
    endif

    iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
    if(allocated(dim    )) deallocate(dim    )
    allocate(dim(ndimensions))

    do k=1,ndimensions
       iret=nf90_inquire_dimension(gfile_loc,k,name,len)
       dim(k)=len
    enddo


    do k=ndimensions+1,nvariables
       iret=nf90_inquire_variable(gfile_loc,k,name,len)
       if(trim(name)=='PHIS'   .or. trim(name)=='phis'  ) then
          iret=nf90_inquire_variable(gfile_loc,k,ndims=ndim)
          if(allocated(dim_id    )) deallocate(dim_id    )
          allocate(dim_id(ndim))
          iret=nf90_inquire_variable(gfile_loc,k,dimids=dim_id)
          allocate(sfc1(dim(dim_id(1)),dim(dim_id(2))) )
          iret=nf90_get_var(gfile_loc,k,sfc1)
          exit
       endif
    enddo     !   k
    iret=nf90_close(gfile_loc)

    k=k_orog
    call fv3_h_to_ll_regular_grids(sfc1,a,nx,ny,nxa,nya,p_fv3sar2anlgrid)

    kk=0
    do n=1,npe
       ns=displss2d(n)+(k-1)*ijn_s(n)
       do j=1,ijn_s(n)
          ns=ns+1
          kk=kk+1
          ii=ltosi_s(kk)
          jj=ltosj_s(kk)
          work(ns)=a(ii,jj)
       end do
    end do

    deallocate (dim_id,sfc,sfc1,dim)
 endif  ! mype


!!!!!!! scatter !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call mpi_scatterv(work,ijns2d,displss2d,mpi_rtype,&
      sfcn2d,ijns2d(mm1),mpi_rtype,mype_2d,mpi_comm_world,ierror)

    deallocate ( work )

    fact10(:,:,it)=sfcn2d(:,:,k_f10m)
    soil_type(:,:,it)=sfcn2d(:,:,k_stype)
    veg_frac(:,:,it)=sfcn2d(:,:,k_vfrac)
    veg_type(:,:,it)=sfcn2d(:,:,k_vtype)
    sfc_rough(:,:,it)=sfcn2d(:,:,k_zorl)
    sfct(:,:,it)=sfcn2d(:,:,k_tsea)
    sno(:,:,it)=sfcn2d(:,:,k_snwdph)
    soil_temp(:,:,it)=sfcn2d(:,:,k_stc)
    soil_moi(:,:,it)=sfcn2d(:,:,k_smc)
    ges_z(:,:)=sfcn2d(:,:,k_orog)/grav
    isli(:,:,it)=nint(sfcn2d(:,:,k_slmsk))
    deallocate (sfcn2d,a)
    return
end subroutine gsi_fv3ncdf2d_read
subroutine gsi_fv3ncdf2d_read_v1(filenamein,varname,varname2,work_sub,mype_io,lat2in,lon2in,ensgrid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv23ncdf2d_readv1       
!   prgmmr: T. Lei                               date: 2019-03-28
!           modified from gsi_fv3ncdf_read and gsi_fv3ncdf2d_read
!
! abstract: read in a 2d field from a netcdf FV3 file in mype_io
!          then scatter the field to each PE 
! program history log:
!
!   input argument list:
!     filename    - file name to read from       
!     varname     - variable name to read in
!     varname2    - variable name to read in
!     mype_io     - pe to read in the field
!
!   output argument list:
!     work_sub    - output sub domain field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block


    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_comm_world,npe,mpi_rtype,mype
    use gridmod, only: lat2,lon2,nlat,nlon,itotsub,ijn_s,displs_s
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inq_varid
    use netcdf, only: nf90_inquire_variable
    use mod_fv3_lolgrid, only: fv3_h_to_ll_regular_grids
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use gridmod, only: ijn_sens,ijnens,displs_sens,itotsubens
    use general_commvars_mod, only: ltosi_sens,ltosj_sens
    use hybrid_ensemble_parameters, only: nlon_ens,nlat_ens

    implicit none
    character(*)   ,intent(in   ) :: varname,varname2,filenamein
    integer(i_kind)   ,intent(in   ) :: lat2in,lon2in
    real(r_kind)   ,intent(out  ) :: work_sub(lat2in,lon2in)
    integer(i_kind)   ,intent(in   ) :: mype_io
    logical, intent(in ) :: ensgrid
    real(r_kind),allocatable,dimension(:,:,:):: uu
    integer(i_kind),allocatable,dimension(:):: dim_id,dim
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: a


    integer(i_kind) n,ns,k,len,ndim
    integer(i_kind) gfile_loc,var_id,iret
    integer(i_kind) nz,nzp1,kk,j,mm1,i,ir,ii,jj
    integer(i_kind) ndimensions,nvariables,nattributes,unlimiteddimid

    mm1=mype+1
    if (ensgrid) then
     allocate (work(itotsubens))
    else
     allocate (work(itotsub))
    endif

    if(mype==mype_io ) then
       iret=nf90_open(trim(filenamein),nf90_nowrite,gfile_loc)
       if(iret/=nf90_noerr) then
          write(6,*)' gsi_fv3ncdf2d_read_v1: problem opening ',trim(filenamein),gfile_loc,', Status = ',iret
          write(6,*)' gsi_fv3ncdf2d_read_v1: problem opening with varnam ',trim(varname)
          return
       endif

       iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
       allocate(dim(ndimensions))
       if (ensgrid) then
         allocate(a(nlat_ens,nlon_ens))
       else
         allocate(a(nlat,nlon))
       end if

       iret=nf90_inq_varid(gfile_loc,trim(adjustl(varname)),var_id)
       if(iret/=nf90_noerr) then
         iret=nf90_inq_varid(gfile_loc,trim(adjustl(varname2)),var_id)
         if(iret/=nf90_noerr) then
           write(6,*)' wrong to get var_id ',var_id
         endif
       endif

       iret=nf90_inquire_variable(gfile_loc,var_id,ndims=ndim)
       if(allocated(dim_id    )) deallocate(dim_id    )
       allocate(dim_id(ndim))
       iret=nf90_inquire_variable(gfile_loc,var_id,dimids=dim_id)
       if(allocated(uu       )) deallocate(uu       )
       if (ensgrid) then
          allocate(uu(nxens,nyens,1))
          iret=nf90_get_var(gfile_loc,var_id,uu)
          call fv3_h_to_ll_regular_grids(uu(:,:,1),a,nxens,nyens,nlon_ens,nlat_ens,p_fv3sar2ensgrid)
          kk=0
          do n=1,npe
             do j=1,ijn_sens(n)
                kk=kk+1
                ii=ltosi_sens(kk)
                jj=ltosj_sens(kk)
                work(kk)=a(ii,jj)
             end do
          end do
       else
          allocate(uu(nx,ny,1))
          iret=nf90_get_var(gfile_loc,var_id,uu)
          call fv3_h_to_ll_regular_grids(uu(:,:,1),a,nx,ny,nlon,nlat,p_fv3sar2anlgrid)
          kk=0
          do n=1,npe
             do j=1,ijn_s(n)
                kk=kk+1
                ii=ltosi_s(kk)
                jj=ltosj_s(kk)
                work(kk)=a(ii,jj)
             end do
          end do
       end if
       iret=nf90_close(gfile_loc)
       deallocate (uu,a,dim,dim_id)

    endif !mype

    if (ensgrid) then
      call mpi_scatterv(work,ijn_sens,displs_sens,mpi_rtype,&
         work_sub,ijn_sens(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
    else
      call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
         work_sub,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
    end if

    deallocate (work)
    return
end subroutine  gsi_fv3ncdf2d_read_v1 

subroutine gsi_fv3ncdf_read(filenamein,varname,varname2,work_sub,mype_io,lat2in,lon2in,ensgrid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv3ncdf_read       
!   prgmmr: wu               org: np22                date: 2017-10-10
!
! abstract: read in a field from a netcdf FV3 file in mype_io
!          then scatter the field to each PE 
! program history log:
!
!   input argument list:
!     filename    - file name to read from       
!     varname     - variable name to read in
!     varname2    - variable name to read in
!     mype_io     - pe to read in the field
!
!   output argument list:
!     work_sub    - output sub domain field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block


    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_comm_world,npe,mpi_rtype,mype
    use gridmod, only: lat2,lon2,nsig,nlat,nlon,itotsub,ijn_s
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use mod_fv3_lolgrid, only: fv3_h_to_ll_regular_grids
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use gridmod, only: ijn_sens,ijnens,itotsubens
    use general_commvars_mod, only: ltosi_sens,ltosj_sens
    use hybrid_ensemble_parameters, only: nlon_ens,nlat_ens

    implicit none
    character(*)   ,intent(in   ) :: varname,varname2,filenamein
    integer(i_kind)   ,intent(in   ) :: lat2in,lon2in
    real(r_kind)   ,intent(out  ) :: work_sub(lat2in,lon2in,nsig)
    integer(i_kind)   ,intent(in   ) :: mype_io
    logical, intent(in ) :: ensgrid
    character(len=128) :: name
    real(r_kind),allocatable,dimension(:,:,:):: uu
    integer(i_kind),allocatable,dimension(:):: dim_id,dim
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: a


    integer(i_kind) n,ns,k,len,ndim
    integer(i_kind) gfile_loc,iret
    integer(i_kind) nz,nzp1,kk,j,mm1,i,ir,ii,jj
    integer(i_kind) ndimensions,nvariables,nattributes,unlimiteddimid

    mm1=mype+1
    if (ensgrid) then
     allocate (work(itotsubens*nsig))
    else
     allocate (work(itotsub*nsig))
    endif

    if(mype==mype_io ) then
       iret=nf90_open(trim(filenamein),nf90_nowrite,gfile_loc)
       if(iret/=nf90_noerr) then
          write(6,*)' gsi_fv3ncdf_read: problem opening ',trim(filenamein),gfile_loc,', Status = ',iret
          write(6,*)' gsi_fv3ncdf_read:problem opening5 with varnam ',trim(varname)
          return
       endif
       iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
       allocate(dim(ndimensions))
       if (ensgrid) then
         allocate(a(nlat_ens,nlon_ens))
       else
         allocate(a(nlat,nlon))
       end if

       do k=1,ndimensions
          iret=nf90_inquire_dimension(gfile_loc,k,name,len)
          dim(k)=len
       enddo


!cltorg       do k=ndimensions+1,nvariables
       do k=1,nvariables
          iret=nf90_inquire_variable(gfile_loc,k,name,len)
          if(trim(name)==varname .or. trim(name)==varname2) then
             iret=nf90_inquire_variable(gfile_loc,k,ndims=ndim)
             if(allocated(dim_id    )) deallocate(dim_id    )
             allocate(dim_id(ndim))
             iret=nf90_inquire_variable(gfile_loc,k,dimids=dim_id)
             if(allocated(uu        )) deallocate(uu        )
             allocate(uu(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
             iret=nf90_get_var(gfile_loc,k,uu)
             exit
          endif
       enddo     !   k
       nz=nsig
       nzp1=nz+1
       do i=1,nz
          ir=nzp1-i
          if (ensgrid) then
            call fv3_h_to_ll_regular_grids(uu(:,:,i),a,dim(dim_id(1)),dim(dim_id(2)),nlon_ens,nlat_ens,p_fv3sar2ensgrid)
            kk=0
            do n=1,npe
              ns=displssens(n)+(ir-1)*ijn_sens(n)
              do j=1,ijn_sens(n)
                 ns=ns+1
                 kk=kk+1
                 ii=ltosi_sens(kk)
                 jj=ltosj_sens(kk)
                 work(ns)=a(ii,jj)
              end do
            end do
          else
            call fv3_h_to_ll_regular_grids(uu(:,:,i),a,dim(dim_id(1)),dim(dim_id(2)),nlon,nlat,p_fv3sar2anlgrid)

            kk=0
            do n=1,npe
             ns=displss(n)+(ir-1)*ijn_s(n)
             do j=1,ijn_s(n)
                ns=ns+1
                kk=kk+1
                ii=ltosi_s(kk)
                jj=ltosj_s(kk)
                work(ns)=a(ii,jj)
             end do
            end do
          end if
       enddo ! i
       iret=nf90_close(gfile_loc)
       deallocate (uu,a,dim,dim_id)

    endif !mype

    if (ensgrid) then
      call mpi_scatterv(work,ijnsens,displssens,mpi_rtype,&
         work_sub,ijnsens(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
    else
      call mpi_scatterv(work,ijns,displss,mpi_rtype,&
         work_sub,ijns(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
    end if

    deallocate (work)
    return
end subroutine gsi_fv3ncdf_read

subroutine gsi_fv3ncdf_read_v1(filenamein,varname,varname2,work_sub,mype_io,lat2in,lon2in,ensgrid)
  
!$$$  subprogram documentation block
!                 .      .    .                                       .
! subprogram:    gsi_fv3ncdf_read _v1      
!            Lei modified from gsi_fv3ncdf_read
!   prgmmr: wu               org: np22                date: 2017-10-10
!
! abstract: read in a field from a netcdf FV3 file in mype_io
!          then scatter the field to each PE 
! program history log:
!
!   input argument list:
!     filename    - file name to read from       
!     varname     - variable name to read in
!     varname2    - variable name to read in
!     mype_io     - pe to read in the field
!
!   output argument list:
!     work_sub    - output sub domain field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block


    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_comm_world,npe,mpi_rtype,mype
    use gridmod, only: lat2,lon2,nsig,nlat,nlon,itotsub,ijn_s
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use mod_fv3_lolgrid, only: fv3_h_to_ll_regular_grids
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use gridmod, only: lat2,lon2,nsig,nlat,nlon,itotsub,ijn_s,ijn_sens,itotsubens
    use general_commvars_mod, only: ltosi_sens,ltosj_sens
    use hybrid_ensemble_parameters, only: nlon_ens,nlat_ens
    implicit none
    character(*)   ,intent(in   ) :: varname,varname2,filenamein
    integer(i_kind)   ,intent(in   ) :: lon2in,lat2in
    real(r_kind)   ,intent(out  ) :: work_sub(lat2in,lon2in,nsig)
    integer(i_kind)   ,intent(in   ) :: mype_io
    logical, intent(in ) :: ensgrid
    character(len=128) :: name
    real(r_kind),allocatable,dimension(:,:,:):: uu
    real(r_kind),allocatable,dimension(:,:,:):: temp0 
    integer(i_kind),allocatable,dimension(:):: dim_id,dim
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: a


    integer(i_kind) n,ns,k,len,ndim,var_id
    integer(i_kind) gfile_loc,iret
    integer(i_kind) nztmp,nzp1,kk,j,mm1,i,ir,ii,jj
    integer(i_kind) ndimensions,nvariables,nattributes,unlimiteddimid

    mm1=mype+1
    if (ensgrid) then
     allocate (work(itotsubens*nsig))
    else
     allocate (work(itotsub*nsig))
    endif

    if(mype==mype_io ) then
       iret=nf90_open(trim(filenamein),nf90_nowrite,gfile_loc)
       if(iret/=nf90_noerr) then
          write(6,*)' gsi_fv3ncdf_read_v1: problem opening ',trim(filenamein),gfile_loc,', Status = ',iret
          write(6,*)' gsi_fv3ncdf_read_v1: problem opening5 with varnam ',trim(varname)
          return
       endif

       iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
       allocate(dim(ndimensions))
       if (ensgrid) then
         allocate(a(nlat_ens,nlon_ens))
       else
         allocate(a(nlat,nlon))
       end if

       do k=1,ndimensions
          iret=nf90_inquire_dimension(gfile_loc,k,name,len)
          dim(k)=len
       enddo

       if (ensgrid) then
             allocate(uu(nxens,nyens,nsig))
             allocate(temp0(nxens,nyens,nsig+1))
       else
             allocate(uu(nx,ny,nsig))
             allocate(temp0(nx,ny,nsig+1))
       end if

       iret=nf90_inq_varid(gfile_loc,trim(adjustl(varname)),var_id)
       if(iret/=nf90_noerr) then
         iret=nf90_inq_varid(gfile_loc,trim(adjustl(varname2)),var_id)
         if(iret/=nf90_noerr) then
           write(6,*)' wrong to get var_id ',var_id
         endif
       endif
       
       iret=nf90_get_var(gfile_loc,var_id,temp0)
       uu(:,:,:)=temp0(:,:,2:(nsig+1))

       nztmp=nsig
       nzp1=nztmp+1
       do i=1,nztmp
          ir=nzp1-i
          if (ensgrid) then
            call fv3_h_to_ll_regular_grids(uu(:,:,i),a,nxens,nyens,nlon_ens,nlat_ens,p_fv3sar2ensgrid)
            kk=0
            do n=1,npe
              ns=displssens(n)+(ir-1)*ijn_sens(n)
              do j=1,ijn_sens(n)
                 ns=ns+1
                 kk=kk+1
                 ii=ltosi_sens(kk)
                 jj=ltosj_sens(kk)
                 work(ns)=a(ii,jj)
              end do
            end do
          else
            call fv3_h_to_ll_regular_grids(uu(:,:,i),a,nx,ny,nlon,nlat,p_fv3sar2anlgrid)
            kk=0
            do n=1,npe
             ns=displss(n)+(ir-1)*ijn_s(n)
             do j=1,ijn_s(n)
                ns=ns+1
                kk=kk+1
                ii=ltosi_s(kk)
                jj=ltosj_s(kk)
                work(ns)=a(ii,jj)
             end do
            end do
          end if
       enddo ! i

       iret=nf90_close(gfile_loc)
       deallocate (uu,a,dim)
       deallocate (temp0)

    endif !mype

    if (ensgrid) then
      call mpi_scatterv(work,ijnsens,displssens,mpi_rtype,&
         work_sub,ijnsens(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
    else
      call mpi_scatterv(work,ijns,displss,mpi_rtype,&
         work_sub,ijns(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
    end if
    deallocate (work)
    return
end subroutine gsi_fv3ncdf_read_v1

subroutine gsi_fv3ncdf_readuv(dynvarsfile,ges_u,ges_v,lat2in,lon2in,ensgrid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv3ncdf_readuv
!   prgmmr: wu w             org: np22                date: 2017-11-22
!
! abstract: read in a field from a netcdf FV3 file in mype_u,mype_v
!           then scatter the field to each PE 
! program history log:
!
!   input argument list:
!
!   output argument list:
!     ges_u       - output sub domain u field
!     ges_v       - output sub domain v field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_comm_world,npe,mpi_rtype,mype
    use gridmod, only: lat2,lon2,nsig,itotsub,ijn_s,nlat,nlon
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use mod_fv3_lolgrid, only: fv3_h_to_ll_regular_grids,nya,nxa,fv3uv2earth_regular_grids
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use gridmod, only: ijn_sens,ijnens,itotsubens
    use general_commvars_mod, only: ltosi_sens,ltosj_sens
    use hybrid_ensemble_parameters, only: nlon_ens,nlat_ens

    implicit none
    character(*)   ,intent(in   ):: dynvarsfile
    integer(i_kind)   ,intent(in   ) :: lat2in,lon2in
    real(r_kind)   ,intent(out  ) :: ges_u(lat2in,lon2in,nsig)
    real(r_kind)   ,intent(out  ) :: ges_v(lat2in,lon2in,nsig)
    logical, intent(in ) :: ensgrid
    character(len=128) :: name
    real(r_kind),allocatable,dimension(:,:,:):: uu,temp1
    integer(i_kind),allocatable,dimension(:):: dim_id,dim
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: a
    real(r_kind),allocatable,dimension(:,:):: u,v

    integer(i_kind) n,ns,k,len,ndim
    integer(i_kind) gfile_loc,iret
    integer(i_kind) nz,nzp1,kk,j,mm1,i,ir,ii,jj
    integer(i_kind) ndimensions,nvariables,nattributes,unlimiteddimid

    if (ensgrid) then
     allocate (work(itotsubens*nsig))
    else
     allocate (work(itotsub*nsig))
    endif
    mm1=mype+1
    if(mype==mype_u .or. mype==mype_v) then
       iret=nf90_open(dynvarsfile,nf90_nowrite,gfile_loc)
       if(iret/=nf90_noerr) then
          write(6,*)' problem opening6 ',trim(dynvarsfile),', Status = ',iret
          return
       endif

       iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)

       allocate(dim(ndimensions))
       if (ensgrid) then
         allocate(a(nlat_ens,nlon_ens))
       else
         allocate(a(nlat,nlon))
       end if

       do k=1,ndimensions
          iret=nf90_inquire_dimension(gfile_loc,k,name,len)
          dim(k)=len
       enddo

       allocate(u(dim(1),dim(4)))
       allocate(v(dim(1),dim(4)))
       iret=nf90_inq_varid(gfile_loc,trim(adjustl("xaxis_1")),k) !thinkdeb
       iret=nf90_get_var(gfile_loc,k,u(:,1))

       do k=ndimensions+1,nvariables
          iret=nf90_inquire_variable(gfile_loc,k,name,len)
          if(trim(name)=='u'.or.trim(name)=='U' .or.  &
             trim(name)=='v'.or.trim(name)=='V' ) then 
             iret=nf90_inquire_variable(gfile_loc,k,ndims=ndim)
             if(allocated(dim_id    )) deallocate(dim_id    )
             allocate(dim_id(ndim))
             iret=nf90_inquire_variable(gfile_loc,k,dimids=dim_id)
!    NOTE:   dimension of variables on native fv3 grid.  
!            u and v have an extra row in one of the dimensions
             if(allocated(uu)) deallocate(uu)
             allocate(uu(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
             iret=nf90_get_var(gfile_loc,k,uu)
             if(trim(name)=='u'.or.trim(name)=='U') then
                allocate(temp1(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))
                temp1=uu
             else if(trim(name)=='v'.or.trim(name)=='V') then
                exit
             endif
          endif
       enddo     !   k
! transfor to earth u/v, interpolate to analysis grid, reverse vertical order
       nz=nsig
       nzp1=nz+1
       do i=1,nz
          ir=nzp1-i 
          if (ensgrid) then
            call fv3uv2earth_regular_grids(temp1(:,:,i),uu(:,:,i),nxens,nyens,u,v,p_fv3sar2ensgrid)
            if(mype==mype_u)then
               call fv3_h_to_ll_regular_grids(u,a,nxens,nyens,nlon_ens,nlat_ens,p_fv3sar2ensgrid)
            else
               call fv3_h_to_ll_regular_grids(v,a,nxens,nyens,nlon_ens,nlat_ens,p_fv3sar2ensgrid)
            endif
            kk=0
            do n=1,npe
              ns=displssens(n)+(ir-1)*ijn_sens(n)
              do j=1,ijn_sens(n)
                 ns=ns+1
                 kk=kk+1
                 ii=ltosi_sens(kk)
                 jj=ltosj_sens(kk)
                 work(ns)=a(ii,jj)
              end do
            end do
          else
            call fv3uv2earth_regular_grids(temp1(:,:,i),uu(:,:,i),nx,ny,u,v,p_fv3sar2anlgrid)
            if(mype==mype_u)then
               call fv3_h_to_ll_regular_grids(u,a,nx,ny,nlon,nlat,p_fv3sar2anlgrid)
            else
               call fv3_h_to_ll_regular_grids(v,a,nx,ny,nlon,nlat,p_fv3sar2anlgrid)
            endif
            kk=0
            do n=1,npe
             ns=displss(n)+(ir-1)*ijn_s(n)
             do j=1,ijn_s(n)
                ns=ns+1
                kk=kk+1
                ii=ltosi_s(kk)
                jj=ltosj_s(kk)
                work(ns)=a(ii,jj)
             end do
            end do
          end if
       enddo ! i
       deallocate(temp1,a)
       deallocate (dim,dim_id,uu,v,u)
       iret=nf90_close(gfile_loc)
    endif ! mype

!!  scatter to ges_u,ges_v !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (ensgrid) then
      call mpi_scatterv(work,ijnsens,displssens,mpi_rtype,&
         ges_u,ijnsens(mm1),mpi_rtype,mype_u,mpi_comm_world,ierror)
      call mpi_scatterv(work,ijnsens,displssens,mpi_rtype,&
         ges_v,ijnsens(mm1),mpi_rtype,mype_v,mpi_comm_world,ierror)
    else
      call mpi_scatterv(work,ijns,displss,mpi_rtype,&
         ges_u,ijns(mm1),mpi_rtype,mype_u,mpi_comm_world,ierror)
      call mpi_scatterv(work,ijns,displss,mpi_rtype,&
         ges_v,ijns(mm1),mpi_rtype,mype_v,mpi_comm_world,ierror)
    end if
    deallocate(work)
end subroutine gsi_fv3ncdf_readuv
subroutine gsi_fv3ncdf_readuv_v1(dynvarsfile,ges_u,ges_v,lat2in,lon2in,ensgrid)
!$$$  subprogram documentation block
! subprogram:    gsi_fv3ncdf_readuv_v1
!   prgmmr: wu w             org: np22                date: 2017-11-22
!
! program history log:
!   2019-04 lei  modified from  gsi_fv3ncdf_readuv to deal with cold start files      .    .                                       .
! abstract: read in a field from a "cold start" netcdf FV3 file in mype_u,mype_v
!           then scatter the field to each PE 
! program history log:
!
!   input argument list:
!
!   output argument list:
!     ges_u       - output sub domain u field
!     ges_v       - output sub domain v field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use constants, only:  half
    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_comm_world,npe,mpi_rtype,mype
    use gridmod, only: lat2,lon2,nsig,itotsub,ijn_s,nlat,nlon
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use mod_fv3_lolgrid, only: fv3_h_to_ll_regular_grids,nya,nxa,fv3uv2earth_regular_grids
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use gridmod, only: ijn_sens,ijnens,itotsubens
    use general_commvars_mod, only: ltosi_sens,ltosj_sens
    use hybrid_ensemble_parameters, only: nlon_ens,nlat_ens
    implicit none
    character(*)   ,intent(in   ):: dynvarsfile
    integer(i_kind)   ,intent(in   ) :: lat2in,lon2in
    real(r_kind)   ,intent(out  ) :: ges_u(lat2in,lon2in,nsig)
    real(r_kind)   ,intent(out  ) :: ges_v(lat2in,lon2in,nsig)
    logical, intent(in ) :: ensgrid
    character(len=128) :: name
    real(r_kind),allocatable,dimension(:,:,:):: uu,temp0
    integer(i_kind),allocatable,dimension(:):: dim_id,dim
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: a
    real(r_kind),allocatable,dimension(:,:):: uorv

    integer(i_kind) n,ns,k,len,ndim,var_id
    integer(i_kind) gfile_loc,iret
    integer(i_kind) nztmp,nzp1,kk,j,mm1,i,ir,ii,jj
    integer(i_kind) ndimensions,nvariables,nattributes,unlimiteddimid

    if (ensgrid) then
     allocate (work(itotsubens*nsig))
    else
     allocate (work(itotsub*nsig))
    endif
    mm1=mype+1
    if(mype==mype_u .or. mype==mype_v) then
       iret=nf90_open(dynvarsfile,nf90_nowrite,gfile_loc)
       if(iret/=nf90_noerr) then
          write(6,*)' gsi_fv3ncdf_readuv_v1: problem opening ',trim(dynvarsfile),', Status = ',iret
          return
       endif

       iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)

       allocate(dim(ndimensions))
       if (ensgrid) then
         allocate(a(nlat_ens,nlon_ens))
       else
         allocate(a(nlat,nlon))
       end if

       do k=1,ndimensions
          iret=nf90_inquire_dimension(gfile_loc,k,name,len)
          dim(k)=len
       enddo
       if (ensgrid) then
         if(mype == mype_u) then
           allocate(uorv(nxens,nyens))
           allocate(uu(nxens,nyens+1,nsig))
         else ! for mype_v
           allocate(uorv(nxens,nyens))
           allocate(uu(nxens+1,nyens,nsig))
         endif
       else
         if(mype == mype_u) then
           allocate(uorv(nx,ny))
           allocate(uu(nx,ny+1,nsig))
         else ! for mype_v
           allocate(uorv(nx,ny))
           allocate(uu(nx+1,ny,nsig))
         endif
       end if

! transfor to earth u/v, interpolate to analysis grid, reverse vertical order
       if(mype == mype_u) then 
         iret=nf90_inq_varid(gfile_loc,trim(adjustl("u_s")),var_id)
       
         iret=nf90_inquire_variable(gfile_loc,var_id,ndims=ndim)
         if (ensgrid) then
           allocate(temp0(nxens,nyens+1,nsig+1))
         else
           allocate(temp0(nx,ny+1,nsig+1))
         end if
         iret=nf90_get_var(gfile_loc,var_id,temp0)
         uu(:,:,:)=temp0(:,:,2:nsig+1)
         deallocate(temp0)
       endif
       if(mype == mype_v) then
         if (ensgrid) then
           allocate(temp0(nxens+1,nyens,nsig+1))
         else
           allocate(temp0(nx+1,ny,nsig+1))
         endif
         iret=nf90_inq_varid(gfile_loc,trim(adjustl("v_w")),var_id)
         iret=nf90_inquire_variable(gfile_loc,var_id,ndims=ndim)
         iret=nf90_get_var(gfile_loc,var_id,temp0)
         uu(:,:,:)=(temp0(:,:,2:nsig+1))
         deallocate (temp0)
       endif
       nztmp=nsig
       nzp1=nztmp+1
       do i=1,nztmp
          ir=nzp1-i 
          if (ensgrid) then
            if(mype == mype_u)then
             do j=1,nyens
              uorv(:,j)=half*(uu(:,j,i)+uu(:,j+1,i))
             enddo

             call fv3_h_to_ll_regular_grids(uorv(:,:),a,nxens,nyens,nlon_ens,nlat_ens,p_fv3sar2ensgrid)
            else
             do j=1,nxens
              uorv(j,:)=half*(uu(j,:,i)+uu(j+1,:,i))
             enddo
             call fv3_h_to_ll_regular_grids(uorv(:,:),a,nxens,nyens,nlon_ens,nlat_ens,p_fv3sar2ensgrid)
            endif
            kk=0
            do n=1,npe
             ns=displssens(n)+(ir-1)*ijn_sens(n)
             do j=1,ijn_sens(n)
                ns=ns+1
                kk=kk+1
                ii=ltosi_sens(kk)
                jj=ltosj_sens(kk)
                work(ns)=a(ii,jj)
             end do
            end do
          else
            if(mype == mype_u)then
             do j=1,ny
              uorv(:,j)=half*(uu(:,j,i)+uu(:,j+1,i))
             enddo
             
             call fv3_h_to_ll_regular_grids(uorv(:,:),a,nx,ny,nxa,nya,p_fv3sar2anlgrid)
            else
             do j=1,nx
              uorv(j,:)=half*(uu(j,:,i)+uu(j+1,:,i))
             enddo
             call fv3_h_to_ll_regular_grids(uorv(:,:),a,nx,ny,nxa,nya,p_fv3sar2anlgrid)
            endif
            kk=0
            do n=1,npe
             ns=displss(n)+(ir-1)*ijn_s(n)
             do j=1,ijn_s(n)
                ns=ns+1
                kk=kk+1
                ii=ltosi_s(kk)
                jj=ltosj_s(kk)
                work(ns)=a(ii,jj)
             end do
            end do
          end if
       enddo ! i
       deallocate(a)
       deallocate (uu,uorv)
       iret=nf90_close(gfile_loc)
    endif ! mype

!!  scatter to ges_u,ges_v !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (ensgrid) then
      call mpi_scatterv(work,ijnsens,displssens,mpi_rtype,&
         ges_u,ijnsens(mm1),mpi_rtype,mype_u,mpi_comm_world,ierror)
      call mpi_scatterv(work,ijnsens,displssens,mpi_rtype,&
         ges_v,ijnsens(mm1),mpi_rtype,mype_v,mpi_comm_world,ierror)
    else
      call mpi_scatterv(work,ijns,displss,mpi_rtype,&
         ges_u,ijns(mm1),mpi_rtype,mype_u,mpi_comm_world,ierror)
      call mpi_scatterv(work,ijns,displss,mpi_rtype,&
         ges_v,ijns(mm1),mpi_rtype,mype_v,mpi_comm_world,ierror)
    end if
    deallocate(work)
end subroutine gsi_fv3ncdf_readuv_v1

subroutine wrfv3_netcdf(fv3filenamegin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrfv3_netcdf           write out FV3 analysis increments
!   prgmmr: wu               org: np22                date: 2017-10-23
!
! abstract:  write FV3 analysis  in netcdf format
!
! program history log:
! 2021-01-05  x.zhang/lei  - add code for updating delz analysis in regional da
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
    use kinds, only: r_kind,i_kind
    use guess_grids, only: ntguessig,ges_tsen
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die

    use gridmod,only: l_reg_update_hydro_delz
    use gridmod, only: lat2,lon2,nsig
    use guess_grids, only:geom_hgti,geom_hgti_bg

    implicit none
    type (type_fv3regfilenameg),intent(in) :: fv3filenamegin

! Declare local constants
    logical add_saved
      character(len=:),allocatable :: grid_spec !='fv3_grid_spec'            
      character(len=:),allocatable :: ak_bk     !='fv3_akbk'
      character(len=:),allocatable :: dynvars   !='fv3_dynvars'
      character(len=:),allocatable :: tracers   !='fv3_tracer'
      character(len=:),allocatable :: sfcdata   !='fv3_sfcdata'
      character(len=:),allocatable :: couplerres!='coupler.res'
! variables for cloud info
    integer(i_kind) ier,istatus,it
    real(r_kind),pointer,dimension(:,:  ):: ges_ps  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q   =>NULL()
   
    real(r_kind),allocatable,dimension(:,:,:)::ges_delzinc
    integer(i_kind) k

    dynvars=fv3filenamegin%dynvars
    tracers=fv3filenamegin%tracers

    it=ntguessig
    ier=0
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps' ,ges_ps ,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u' , ges_u ,istatus);ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v' , ges_v ,istatus);ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q'  ,ges_q ,istatus);ier=ier+istatus
    if (ier/=0) call die('get ges','cannot get pointers for fv3 met-fields, ier =',ier)

    add_saved=.true.

!   write out
    if(fv3sar_bg_opt == 0) then
       call gsi_fv3ncdf_write(dynvars,'T',ges_tsen(1,1,1,it),mype_t,add_saved)
       call gsi_fv3ncdf_write(tracers,'sphum',ges_q   ,mype_q,add_saved)
       call gsi_fv3ncdf_writeuv(dynvars,ges_u,ges_v,mype_v,add_saved)
       call gsi_fv3ncdf_writeps(dynvars,'delp',ges_ps,mype_p,add_saved)
       if(l_reg_update_hydro_delz) then
          allocate(ges_delzinc(lat2,lon2,nsig))
          do k=1,nsig
             ges_delzinc(:,:,k)=geom_hgti(:,:,k+1,it)-geom_hgti_bg(:,:,k+1,it)-geom_hgti(:,:,k,it)+geom_hgti_bg(:,:,k,it)
          enddo
          call gsi_fv3ncdf_write_fv3_dz(dynvars,"DZ",ges_delzinc,mype_delz,add_saved)
          deallocate(ges_delzinc)
       endif

    else
       call gsi_fv3ncdf_write_v1(dynvars,'t',ges_tsen(1,1,1,it),mype_t,add_saved)
       call gsi_fv3ncdf_write_v1(tracers,'sphum',ges_q   ,mype_q,add_saved)
       call gsi_fv3ncdf_writeuv_v1(dynvars,ges_u,ges_v,mype_v,add_saved)
       call gsi_fv3ncdf_writeps_v1(dynvars,'ps',ges_ps,mype_p,add_saved)
    
    endif
    
end subroutine wrfv3_netcdf

subroutine gsi_fv3ncdf_writeuv(dynvars,varu,varv,mype_io,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_writeuv
!   pgrmmr: wu
!
! abstract: gather u/v fields to mype_io, put u/v in FV3 model defined directions & orders
!           then write out
!
! program history log:
!
!   input argument list:
!    varu,varv
!    add_saved - true: add analysis increments to readin guess then write out
!              - false: write out total analysis fields
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only:  mpi_rtype,mpi_comm_world,ierror,npe,mype
    use gridmod, only: lat2,lon2,nlon,nlat,lat1,lon1,nsig, &
                       ijn,displs_g,itotsub,iglobal, &
                       nlon_regional,nlat_regional
    use mod_fv3_lolgrid, only: fv3_ll_to_h_regular_grids,fv3_h_to_ll_regular_grids, &
                            fv3uv2earth_regular_grids,earthuv2fv3_regular_grids
    use general_commvars_mod, only: ltosi,ltosj
    use netcdf, only: nf90_open,nf90_close,nf90_noerr
    use netcdf, only: nf90_write,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var

    implicit none
    character(len=*),intent(in) :: dynvars   !='fv3_dynvars'

    real(r_kind)   ,intent(in   ) :: varu(lat2,lon2,nsig)
    real(r_kind)   ,intent(in   ) :: varv(lat2,lon2,nsig)
    integer(i_kind),intent(in   ) :: mype_io
    logical        ,intent(in   ) :: add_saved

    integer(i_kind) :: ugrd_VarId,gfile_loc,vgrd_VarId
    integer(i_kind) i,j,mm1,n,k,ns,kr,m
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:,:):: work_sub,work_au,work_av
    real(r_kind),allocatable,dimension(:,:,:):: work_bu,work_bv
    real(r_kind),allocatable,dimension(:,:):: u,v,workau2,workav2
    real(r_kind),allocatable,dimension(:,:):: workbu2,workbv2

    mm1=mype+1

    allocate(    work(max(iglobal,itotsub)*nsig),work_sub(lat1,lon1,nsig))
!!!!!! gather analysis u !! revers k !!!!!!!!!!!
    do k=1,nsig
       kr=nsig+1-k
       do i=1,lon1
          do j=1,lat1
             work_sub(j,i,kr)=varu(j+1,i+1,k)
          end do
       end do
    enddo
    call mpi_gatherv(work_sub,ijnz(mm1),mpi_rtype, &
          work,ijnz,displsz_g,mpi_rtype,mype_io,mpi_comm_world,ierror)

    if(mype==mype_io) then
       allocate( work_au(nlat,nlon,nsig),work_av(nlat,nlon,nsig))
       ns=0
       do m=1,npe
          do k=1,nsig
             do n=displs_g(m)+1,displs_g(m)+ijn(m) 
                ns=ns+1
                work_au(ltosi(n),ltosj(n),k)=work(ns)
             end do
          enddo
       enddo
    endif  ! mype

!!!!!! gather analysis v !! reverse k !!!!!!!!!!!!!!!!!!
    do k=1,nsig
       kr=nsig+1-k
       do i=1,lon1
          do j=1,lat1
             work_sub(j,i,kr)=varv(j+1,i+1,k)
          end do
       end do
    enddo
    call mpi_gatherv(work_sub,ijnz(mm1),mpi_rtype, &
          work,ijnz,displsz_g,mpi_rtype,mype_io,mpi_comm_world,ierror)

    if(mype==mype_io) then
       ns=0
       do m=1,npe
          do k=1,nsig
             do n=displs_g(m)+1,displs_g(m)+ijn(m) 
            ns=ns+1
             work_av(ltosi(n),ltosj(n),k)=work(ns)
             end do
          enddo
       enddo
       deallocate(work,work_sub)
       allocate( u(nlon_regional,nlat_regional+1))
       allocate( v(nlon_regional+1,nlat_regional))
       allocate( work_bu(nlon_regional,nlat_regional+1,nsig))
       allocate( work_bv(nlon_regional+1,nlat_regional,nsig))
       call check( nf90_open(trim(dynvars ),nf90_write,gfile_loc) )
       call check( nf90_inq_varid(gfile_loc,'u',ugrd_VarId) )
       call check( nf90_inq_varid(gfile_loc,'v',vgrd_VarId) )

       if(add_saved)then
          allocate( workau2(nlat,nlon),workav2(nlat,nlon))
          allocate( workbu2(nlon_regional,nlat_regional+1))
          allocate( workbv2(nlon_regional+1,nlat_regional))
!!!!!!!!  readin work_b !!!!!!!!!!!!!!!!
          call check( nf90_get_var(gfile_loc,ugrd_VarId,work_bu) )
          call check( nf90_get_var(gfile_loc,vgrd_VarId,work_bv) )
          do k=1,nsig
             call fv3uv2earth_regular_grids(work_bu(1,1,k),work_bv(1,1,k),nlon_regional,nlat_regional,u,v,p_fv3sar2anlgrid)
             call fv3_h_to_ll_regular_grids(u,workau2,nlon_regional,nlat_regional,nlon,nlat,p_fv3sar2anlgrid)
             call fv3_h_to_ll_regular_grids(v,workav2,nlon_regional,nlat_regional,nlon,nlat,p_fv3sar2anlgrid)
!!!!!!!! find analysis_inc:  work_a !!!!!!!!!!!!!!!!
             work_au(:,:,k)=work_au(:,:,k)-workau2(:,:)
             work_av(:,:,k)=work_av(:,:,k)-workav2(:,:)
             call fv3_ll_to_h_regular_grids(work_au(:,:,k),u,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
             call fv3_ll_to_h_regular_grids(work_av(:,:,k),v,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
             call earthuv2fv3_regular_grids(u,v,nlon_regional,nlat_regional,workbu2,workbv2,p_fv3sar2anlgrid)
!!!!!!!!  add analysis_inc to readin work_b !!!!!!!!!!!!!!!!
             work_bu(:,:,k)=work_bu(:,:,k)+workbu2(:,:)
             work_bv(:,:,k)=work_bv(:,:,k)+workbv2(:,:)
          enddo
          deallocate(workau2,workbu2,workav2,workbv2)
       else
          do k=1,nsig
             call fv3_ll_to_h_regular_grids(work_au(:,:,k),u,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
             call fv3_ll_to_h_regular_grids(work_av(:,:,k),v,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
             call earthuv2fv3_regular_grids(u,v,nlon_regional,nlat_regional,work_bu(:,:,k),work_bv(:,:,k),p_fv3sar2anlgrid)
          enddo
       endif

       deallocate(work_au,work_av,u,v)
       print *,'write out u/v to ',trim(dynvars )
       call check( nf90_put_var(gfile_loc,ugrd_VarId,work_bu) )
       call check( nf90_put_var(gfile_loc,vgrd_VarId,work_bv) )
       call check( nf90_close(gfile_loc) )
       deallocate(work_bu,work_bv)
    end if !mype_io

    if(allocated(work))deallocate(work)
    if(allocated(work_sub))deallocate(work_sub)

end subroutine gsi_fv3ncdf_writeuv

subroutine gsi_fv3ncdf_writeps(filename,varname,var,mype_io,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_writeps
!   pgrmmr: wu
!
! abstract: write out analyzed "delp" to fv_core.res.nest02.tile7.nc
!
! program history log:
!
!   input argument list:
!    varu,varv
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: mpi_rtype,mpi_comm_world,ierror,mype
    use gridmod, only: lat2,lon2,nlon,nlat,lat1,lon1,nsig
    use gridmod, only: ijn,displs_g,itotsub,iglobal
    use gridmod,  only: nlon_regional,nlat_regional,eta1_ll,eta2_ll
    use mod_fv3_lolgrid, only: fv3_ll_to_h_regular_grids,fv3_h_to_ll_regular_grids
    use general_commvars_mod, only: ltosi,ltosj
    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var
    implicit none

    real(r_kind)   ,intent(in   ) :: var(lat2,lon2)
    integer(i_kind),intent(in   ) :: mype_io
    logical        ,intent(in   ) :: add_saved
    character(*)   ,intent(in   ) :: varname,filename

    integer(i_kind) :: VarId,gfile_loc
    integer(i_kind) i,j,mm1,k,kr,kp
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: work_sub,work_a
    real(r_kind),allocatable,dimension(:,:,:):: work_b,work_bi
    real(r_kind),allocatable,dimension(:,:):: workb2,worka2

    mm1=mype+1
    allocate(    work(max(iglobal,itotsub)),work_sub(lat1,lon1) )
    do i=1,lon1
       do j=1,lat1
          work_sub(j,i)=var(j+1,i+1)
       end do
    end do
    call mpi_gatherv(work_sub,ijn(mm1),mpi_rtype, &
          work,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    if(mype==mype_io) then
       allocate( work_a(nlat,nlon))
       do i=1,iglobal
          work_a(ltosi(i),ltosj(i))=work(i)
       end do
       allocate( work_bi(nlon_regional,nlat_regional,nsig+1))
       allocate( work_b(nlon_regional,nlat_regional,nsig))
       call check( nf90_open(trim(filename),nf90_write,gfile_loc) )
       call check( nf90_inq_varid(gfile_loc,trim(varname),VarId) )
       allocate( workb2(nlon_regional,nlat_regional))

       if(add_saved)then
          allocate( worka2(nlat,nlon))
!!!!!!!! read in guess delp  !!!!!!!!!!!!!!
          call check( nf90_get_var(gfile_loc,VarId,work_b) )
          work_bi(:,:,1)=eta1_ll(nsig+1)
          do i=2,nsig+1
             work_bi(:,:,i)=work_b(:,:,i-1)*0.001_r_kind+work_bi(:,:,i-1)
          enddo
          call fv3_h_to_ll_regular_grids(work_bi(:,:,nsig+1),worka2,nlon_regional,nlat_regional,nlon,nlat,p_fv3sar2anlgrid)
!!!!!!! analysis_inc Psfc: work_a
          work_a(:,:)=work_a(:,:)-worka2(:,:)
          call fv3_ll_to_h_regular_grids(work_a,workb2,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
          do k=1,nsig+1
             kr=nsig+2-k
!!!!!!! ges_prsi+hydrostatic analysis_inc !!!!!!!!!!!!!!!!
             work_bi(:,:,k)=work_bi(:,:,k)+eta2_ll(kr)*workb2(:,:)
          enddo

       else
          call fv3_ll_to_h_regular_grids(work_a,workb2,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
          do k=1,nsig+1
             kr=nsig+2-k
!!!!!!! Psfc_ges+hydrostatic analysis_inc !!!!!!!!!!!!!!!!
             work_bi(:,:,k)=eta1_ll(kr)+eta2_ll(kr)*workb2(:,:)
          enddo
       endif
!          delp     
       do k=nsig,1,-1
          kp=k+1
          work_b(:,:,k)=(work_bi(:,:,kp)-work_bi(:,:,k))*1000._r_kind
       enddo
  
       call check( nf90_put_var(gfile_loc,VarId,work_b) )
       call check( nf90_close(gfile_loc) )
       if (allocated(worka2)) deallocate(worka2)
       if (allocated(workb2)) deallocate(workb2)
       deallocate(work_b,work_a,work_bi)

    end if !mype_io

    deallocate(work,work_sub)
end subroutine gsi_fv3ncdf_writeps
subroutine gsi_fv3ncdf_writeuv_v1(dynvars,varu,varv,mype_io,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_writeuv
!   pgrmmr: wu
!
! abstract: gather u/v fields to mype_io, put u/v in FV3 model defined directions & orders
!           then write out
!
! program history log:
! 2019-04-22  lei   modified from gsi_nemsio_writeuv_v1 for update
! u_w,v_w,u_s,v_s in the cold start files!
! 2020-03-06  lei   added ilev0 fix
!   input argument list:
!    varu,varv
!    add_saved - true: add analysis increments to readin guess then write out
!              - false: write out total analysis fields
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use constants, only: half,zero
    use mpimod, only:  mpi_rtype,mpi_comm_world,ierror,npe,mype
    use gridmod, only: lat2,lon2,nlon,nlat,lat1,lon1,nsig, &
                       ijn,displs_g,itotsub,iglobal, &
                       nlon_regional,nlat_regional
    use mod_fv3_lolgrid, only: fv3_ll_to_h_regular_grids,fv3_h_to_ll_regular_grids, &
                            fv3uv2earth_regular_grids,earthuv2fv3_regular_grids
    use general_commvars_mod, only: ltosi,ltosj
    use netcdf, only: nf90_open,nf90_close,nf90_noerr
    use netcdf, only: nf90_write,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var

    implicit none
    character(len=*),intent(in) :: dynvars   !='fv3_dynvars'

    real(r_kind)   ,intent(in   ) :: varu(lat2,lon2,nsig)
    real(r_kind)   ,intent(in   ) :: varv(lat2,lon2,nsig)
    integer(i_kind),intent(in   ) :: mype_io
    logical        ,intent(in   ) :: add_saved

    integer(i_kind) :: gfile_loc
    integer(i_kind) :: u_wgrd_VarId,v_wgrd_VarId
    integer(i_kind) :: u_sgrd_VarId,v_sgrd_VarId
    integer(i_kind) i,j,mm1,n,k,ns,kr,m
    integer(i_kind) ilev0
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:,:):: work_sub,work_au,work_av
    real(r_kind),allocatable,dimension(:,:,:):: work_bu_s,work_bv_s
    real(r_kind),allocatable,dimension(:,:,:):: work_bu_w,work_bv_w
    real(r_kind),allocatable,dimension(:,:):: u,v,workau2,workav2
    real(r_kind),allocatable,dimension(:,:):: workbu_s2,workbv_s2
    real(r_kind),allocatable,dimension(:,:):: workbu_w2,workbv_w2

    mm1=mype+1
    ilev0=1

    allocate(    work(max(iglobal,itotsub)*nsig),work_sub(lat1,lon1,nsig))
!!!!!! gather analysis u !! revers k !!!!!!!!!!!
    do k=1,nsig
       kr=nsig+1-k
       do i=1,lon1
          do j=1,lat1
             work_sub(j,i,kr)=varu(j+1,i+1,k)
          end do
       end do
    enddo
    call mpi_gatherv(work_sub,ijnz(mm1),mpi_rtype, &
          work,ijnz,displsz_g,mpi_rtype,mype_io,mpi_comm_world,ierror)

    if(mype==mype_io) then
       allocate( work_au(nlat,nlon,nsig),work_av(nlat,nlon,nsig))
       ns=0
       do m=1,npe
          do k=1,nsig
             do n=displs_g(m)+1,displs_g(m)+ijn(m) 
             ns=ns+1
             work_au(ltosi(n),ltosj(n),k)=work(ns)
             end do
          enddo
       enddo
    endif  ! mype

!!!!!! gather analysis v !! reverse k !!!!!!!!!!!!!!!!!!
    do k=1,nsig
       kr=nsig+1-k
       do i=1,lon1
          do j=1,lat1
             work_sub(j,i,kr)=varv(j+1,i+1,k)
          end do
       end do
    enddo
    call mpi_gatherv(work_sub,ijnz(mm1),mpi_rtype, &
          work,ijnz,displsz_g,mpi_rtype,mype_io,mpi_comm_world,ierror)

    if(mype==mype_io) then
       ns=0
       do m=1,npe
          do k=1,nsig
             do n=displs_g(m)+1,displs_g(m)+ijn(m) 
            ns=ns+1
             work_av(ltosi(n),ltosj(n),k)=work(ns)
             end do
          enddo
       enddo
       deallocate(work,work_sub)
! u and v would contain winds at either D-grid or A-grid
! do not diretly use them in between fv3uv2eath and fv3_h_to_ll unless paying
!attention to the actual storage layout
       call check( nf90_open(trim(dynvars ),nf90_write,gfile_loc) )

       allocate( u(nlon_regional,nlat_regional)) 
       allocate( v(nlon_regional,nlat_regional))

       allocate( work_bu_s(nlon_regional,nlat_regional+1,nsig+1))
       allocate( work_bv_s(nlon_regional,nlat_regional+1,nsig+1))
       allocate( work_bu_w(nlon_regional+1,nlat_regional,nsig+1))
       allocate( work_bv_w(nlon_regional+1,nlat_regional,nsig+1))



       call check( nf90_inq_varid(gfile_loc,'u_s',u_sgrd_VarId) )
       call check( nf90_inq_varid(gfile_loc,'u_w',u_wgrd_VarId) )
       call check( nf90_inq_varid(gfile_loc,'v_s',v_sgrd_VarId) )
       call check( nf90_inq_varid(gfile_loc,'v_w',v_wgrd_VarId) )

          allocate( workbu_w2(nlon_regional+1,nlat_regional))
          allocate( workbv_w2(nlon_regional+1,nlat_regional))
          allocate( workbu_s2(nlon_regional,nlat_regional+1))
          allocate( workbv_s2(nlon_regional,nlat_regional+1))
!!!!!!!!  readin work_b !!!!!!!!!!!!!!!!
          call check( nf90_get_var(gfile_loc,u_sgrd_VarId,work_bu_s) )
          call check( nf90_get_var(gfile_loc,u_wgrd_VarId,work_bu_w) )
          call check( nf90_get_var(gfile_loc,v_sgrd_VarId,work_bv_s) )
          call check( nf90_get_var(gfile_loc,v_wgrd_VarId,work_bv_w) )

       if(add_saved)then
          allocate( workau2(nlat,nlon),workav2(nlat,nlon))
          do k=1,nsig
             do j=1,nlat_regional
                u(:,j)=half * (work_bu_s(:,j,ilev0+k)+ work_bu_s(:,j+1,ilev0+k))
             enddo
             do i=1,nlon_regional
                v(i,:)=half*(work_bv_w(i,:,ilev0+k)+work_bv_w(i+1,:,ilev0+k))
             enddo
             call fv3_h_to_ll_regular_grids(u,workau2,nlon_regional,nlat_regional,nlon,nlat,p_fv3sar2anlgrid)
             call fv3_h_to_ll_regular_grids(v,workav2,nlon_regional,nlat_regional,nlon,nlat,p_fv3sar2anlgrid)
!!!!!!!! find analysis_inc:  work_a !!!!!!!!!!!!!!!!
             work_au(:,:,k)=work_au(:,:,k)-workau2(:,:)
             work_av(:,:,k)=work_av(:,:,k)-workav2(:,:)
             call fv3_ll_to_h_regular_grids(work_au(:,:,k),u,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
             call fv3_ll_to_h_regular_grids(work_av(:,:,k),v,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
!!!!!!!!  add analysis_inc to readin work_b !!!!!!!!!!!!!!!!
             do i=2,nlon_regional
               workbu_w2(i,:)=half*(u(i-1,:)+u(i,:))
               workbv_w2(i,:)=half*(v(i-1,:)+v(i,:))
             enddo
             workbu_w2(1,:)=u(1,:)
             workbv_w2(1,:)=v(1,:)
             workbu_w2(nlon_regional+1,:)=u(nlon_regional,:)
             workbv_w2(nlon_regional+1,:)=v(nlon_regional,:)

             do j=2,nlat_regional
               workbu_s2(:,j)=half*(u(:,j-1)+u(:,j))
               workbv_s2(:,j)=half*(v(:,j-1)+v(:,j))
             enddo
             workbu_s2(:,1)=u(:,1)
             workbv_s2(:,1)=v(:,1)
             workbu_s2(:,nlat_regional+1)=u(:,nlat_regional)
             workbv_s2(:,nlat_regional+1)=v(:,nlat_regional)



             work_bu_w(:,:,ilev0+k)=work_bu_w(:,:,ilev0+k)+workbu_w2(:,:)
             work_bu_s(:,:,ilev0+k)=work_bu_s(:,:,ilev0+k)+workbu_s2(:,:)
             work_bv_w(:,:,ilev0+k)=work_bv_w(:,:,ilev0+k)+workbv_w2(:,:)
             work_bv_s(:,:,ilev0+k)=work_bv_s(:,:,ilev0+k)+workbv_s2(:,:)
          enddo
          deallocate(workau2,workav2)
          deallocate(workbu_w2,workbv_w2)
          deallocate(workbu_s2,workbv_s2)
       else
          do k=1,nsig
             call fv3_ll_to_h_regular_grids(work_au(:,:,k),u,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
             call fv3_ll_to_h_regular_grids(work_av(:,:,k),v,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)

             do i=2,nlon_regional
               work_bu_w(i,:,k)=half*(u(i-1,:)+u(i,:))
               work_bv_w(i,:,k)=half*(v(i-1,:)+v(i,:))
             enddo
             work_bu_w(1,:,ilev0+k)=u(1,:)
             work_bv_w(1,:,ilev0+k)=v(1,:)
             work_bu_w(nlon_regional+1,:,ilev0+k)=u(nlon_regional,:)
             work_bv_w(nlon_regional+1,:,ilev0+k)=v(nlon_regional,:)

             do j=2,nlat_regional
               work_bu_s(:,j,ilev0+k)=half*(u(:,j-1)+u(:,j))
               work_bv_s(:,j,ilev0+k)=half*(v(:,j-1)+v(:,j))
             enddo
             work_bu_s(:,1,ilev0+k)=u(:,1)
             work_bv_s(:,1,ilev0+k)=v(:,1)
             work_bu_s(:,nlat_regional+1,ilev0+k)=u(:,nlat_regional)
             work_bv_s(:,nlat_regional+1,ilev0+k)=v(:,nlat_regional)


          enddo
       endif

       deallocate(work_au,work_av,u,v)
       print *,'write out u/v to ',trim(dynvars )
       call check( nf90_put_var(gfile_loc,u_wgrd_VarId,work_bu_w) )
       call check( nf90_put_var(gfile_loc,u_sgrd_VarId,work_bu_s) )
       call check( nf90_put_var(gfile_loc,v_wgrd_VarId,work_bv_w) )
       call check( nf90_put_var(gfile_loc,v_sgrd_VarId,work_bv_s) )
       call check( nf90_close(gfile_loc) )
       deallocate(work_bu_w,work_bv_w)
       deallocate(work_bu_s,work_bv_s)
    end if !mype_io

    if(allocated(work))deallocate(work)
    if(allocated(work_sub))deallocate(work_sub)

end subroutine gsi_fv3ncdf_writeuv_v1

subroutine gsi_fv3ncdf_writeps_v1(filename,varname,var,mype_io,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_writeps
!   pgrmmr: wu
!
! abstract: write out analyzed "delp" to fv_core.res.nest02.tile7.nc
!
! program history log:
! 2019-04  lei,  modified from gsi_nemsio_writeps to deal with cold start files
!
!   input argument list:
!    varu,varv
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: mpi_rtype,mpi_comm_world,ierror,mype
    use gridmod, only: lat2,lon2,nlon,nlat,lat1,lon1
    use gridmod, only: ijn,displs_g,itotsub,iglobal
    use gridmod,  only: nlon_regional,nlat_regional,eta1_ll,eta2_ll
    use mod_fv3_lolgrid, only: fv3_ll_to_h_regular_grids,fv3_h_to_ll_regular_grids
    use general_commvars_mod, only: ltosi,ltosj
    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var
    implicit none

    real(r_kind)   ,intent(in   ) :: var(lat2,lon2)
    integer(i_kind),intent(in   ) :: mype_io
    logical        ,intent(in   ) :: add_saved
    character(*)   ,intent(in   ) :: varname,filename

    integer(i_kind) :: VarId,gfile_loc
    integer(i_kind) i,j,mm1,k,kr,kp
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: work_sub,work_a
    real(r_kind),allocatable,dimension(:,:):: work_b,work_bi
    real(r_kind),allocatable,dimension(:,:):: workb2,worka2


    mm1=mype+1
    allocate(    work(max(iglobal,itotsub)),work_sub(lat1,lon1) )
    do i=1,lon1
       do j=1,lat1
          work_sub(j,i)=var(j+1,i+1)
       end do
    end do
    call mpi_gatherv(work_sub,ijn(mm1),mpi_rtype, &
          work,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)

    if(mype==mype_io) then
       allocate( work_a(nlat,nlon))
       do i=1,iglobal
          work_a(ltosi(i),ltosj(i))=work(i)
       end do
       allocate( work_bi(nlon_regional,nlat_regional))
       allocate( work_b(nlon_regional,nlat_regional))
       call check( nf90_open(trim(filename),nf90_write,gfile_loc) )
       call check( nf90_inq_varid(gfile_loc,trim(varname),VarId) )
       work_a=work_a*1000.0_r_kind
       if(add_saved)then
          allocate( workb2(nlon_regional,nlat_regional))
          allocate( worka2(nlat,nlon))
!!!!!!!! read in guess delp  !!!!!!!!!!!!!!
          call check( nf90_get_var(gfile_loc,VarId,work_b) )
          call fv3_h_to_ll_regular_grids(work_b,worka2,nlon_regional,nlat_regional,nlon,nlat,p_fv3sar2anlgrid)
!!!!!!! analysis_inc Psfc: work_a
          work_a(:,:)=work_a(:,:)-worka2(:,:)
          call fv3_ll_to_h_regular_grids(work_a,workb2,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
             work_b(:,:)=work_b(:,:)+workb2(:,:)
       else
          call fv3_ll_to_h_regular_grids(work_a,work_b,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
       endif

       call check( nf90_put_var(gfile_loc,VarId,work_b) )
       call check( nf90_close(gfile_loc) )
       if (allocated(worka2)) deallocate(worka2)
       if ( allocated(workb2)) deallocate(workb2)
       deallocate(work_b,work_a,work_bi)


    end if !mype_io

    deallocate(work,work_sub)
end subroutine gsi_fv3ncdf_writeps_v1

subroutine gsi_fv3ncdf_write(filename,varname,var,mype_io,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_write
!   pgrmmr: wu
!
! abstract:
!
! program history log:
!
!   input argument list:
!    varu,varv
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: mpi_rtype,mpi_comm_world,ierror,npe,mype
    use gridmod, only: lat2,lon2,nlon,nlat,lat1,lon1,nsig
    use gridmod, only: ijn,displs_g,itotsub,iglobal
    use mod_fv3_lolgrid, only: fv3_ll_to_h_regular_grids
    use mod_fv3_lolgrid, only: fv3_h_to_ll_regular_grids
    use general_commvars_mod, only: ltosi,ltosj
    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var
    implicit none

    real(r_kind)   ,intent(in   ) :: var(lat2,lon2,nsig)
    integer(i_kind),intent(in   ) :: mype_io
    logical        ,intent(in   ) :: add_saved
    character(*)   ,intent(in   ) :: varname,filename

    integer(i_kind) :: VarId,gfile_loc
    integer(i_kind) i,j,mm1,k,kr,ns,n,m
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:,:):: work_sub,work_a
    real(r_kind),allocatable,dimension(:,:,:):: work_b
    real(r_kind),allocatable,dimension(:,:):: workb2,worka2


    mm1=mype+1

    allocate(    work(max(iglobal,itotsub)*nsig),work_sub(lat1,lon1,nsig))
!!!!!!!! reverse z !!!!!!!!!!!!!!
    do k=1,nsig
       kr=nsig+1-k
       do i=1,lon1
          do j=1,lat1
             work_sub(j,i,kr)=var(j+1,i+1,k)
          end do
       end do
    enddo
    call mpi_gatherv(work_sub,ijnz(mm1),mpi_rtype, &
          work,ijnz,displsz_g,mpi_rtype,mype_io,mpi_comm_world,ierror)

    if(mype==mype_io) then
       allocate( work_a(nlat,nlon,nsig))
       ns=0
       do m=1,npe
          do k=1,nsig
             do n=displs_g(m)+1,displs_g(m)+ijn(m) 
                ns=ns+1
                work_a(ltosi(n),ltosj(n),k)=work(ns)
             end do
          enddo
       enddo

       allocate( work_b(nlon_regional,nlat_regional,nsig))

       call check( nf90_open(trim(filename),nf90_write,gfile_loc) )
       call check( nf90_inq_varid(gfile_loc,trim(varname),VarId) )


       if(add_saved)then
          allocate( workb2(nlon_regional,nlat_regional))
          allocate( worka2(nlat,nlon))
          call check( nf90_get_var(gfile_loc,VarId,work_b) )

          do k=1,nsig
             call fv3_h_to_ll_regular_grids(work_b(:,:,k),worka2,nlon_regional,nlat_regional,nlon,nlat,p_fv3sar2anlgrid)
!!!!!!!! analysis_inc:  work_a !!!!!!!!!!!!!!!!
             work_a(:,:,k)=work_a(:,:,k)-worka2(:,:)
             call fv3_ll_to_h_regular_grids(work_a(1,1,k),workb2,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
             work_b(:,:,k)=work_b(:,:,k)+workb2(:,:)
          enddo
          deallocate(worka2,workb2)
       else
          do k=1,nsig
             call fv3_ll_to_h_regular_grids(work_a(1,1,k),work_b(1,1,k),nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
          enddo
       endif

       print *,'write out ',trim(varname),' to ',trim(filename)
       call check( nf90_put_var(gfile_loc,VarId,work_b) )
       call check( nf90_close(gfile_loc) )
       deallocate(work_b,work_a)
    end if !mype_io

    deallocate(work,work_sub)

end subroutine gsi_fv3ncdf_write
subroutine gsi_fv3ncdf_write_fv3_dz(filename,varname,varinc,mype_io,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_write_fv3_dz from gsi_nemsio_write
!   pgrmmr: lei
!
! abstract:
!
! program history log:
!
!   input argument list:
!    varin
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: mpi_rtype,mpi_comm_world,ierror,npe,mype
    use gridmod, only: lat2,lon2,nlon,nlat,lat1,lon1,nsig
    use gridmod, only: ijn,displs_g,itotsub,iglobal
    use mod_fv3_lolgrid, only: fv3_ll_to_h_regular_grids,fv3_h_to_ll_regular_grids
    use general_commvars_mod, only: ltosi,ltosj
    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var
    implicit none

    real(r_kind)   ,intent(in   ) :: varinc(lat2,lon2,nsig)
    integer(i_kind),intent(in   ) :: mype_io
    logical        ,intent(in   ) :: add_saved
    character(*)   ,intent(in   ) :: varname,filename

    integer(i_kind) :: VarId,gfile_loc
    integer(i_kind) i,j,mm1,k,kr,ns,n,m
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:,:):: work_sub,work_ainc
    real(r_kind),allocatable,dimension(:,:,:):: work_b
    real(r_kind),allocatable,dimension(:,:):: workb2

    mm1=mype+1

    allocate(    work(max(iglobal,itotsub)*nsig),work_sub(lat1,lon1,nsig))
!!!!!!!! reverse z !!!!!!!!!!!!!!
    do k=1,nsig
       kr=nsig+1-k
       do i=1,lon1
          do j=1,lat1
             work_sub(j,i,kr)=varinc(j+1,i+1,k)
          end do
       end do
    enddo
    call mpi_gatherv(work_sub,ijnz(mm1),mpi_rtype, &
         work,ijnz,displsz_g,mpi_rtype,mype_io,mpi_comm_world,ierror)

    if(mype==mype_io) then
       allocate( work_ainc(nlat,nlon,nsig))
       ns=0
       do m=1,npe
          do k=1,nsig
             do n=displs_g(m)+1,displs_g(m)+ijn(m)
                ns=ns+1
                work_ainc(ltosi(n),ltosj(n),k)=work(ns)
             end do
          enddo
       enddo

       allocate( work_b(nlon_regional,nlat_regional,nsig))

       call check( nf90_open(trim(filename),nf90_write,gfile_loc) )
       call check( nf90_inq_varid(gfile_loc,trim(varname),VarId) )

       if(.not. add_saved)then
          write(6,*)'here the input is increments to be added to the read-in background, &
                     hence, add_saved has to be true'
       endif
       allocate( workb2(nlon_regional,nlat_regional))
       call check( nf90_get_var(gfile_loc,VarId,work_b) )

       do k=1,nsig
          call fv3_ll_to_h_regular_grids(work_ainc(1,1,k),workb2(:,:),nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
!!!!!!!! analysis_inc:  work_a !!!!!!!!!!!!!!!!
          work_b(:,:,k)=workb2(:,:)+work_b(:,:,k)
       enddo
       deallocate(workb2)

       print *,'write out ',trim(varname),' to ',trim(filename)
       call check( nf90_put_var(gfile_loc,VarId,work_b) )
       call check( nf90_close(gfile_loc) )
       deallocate(work_b,work_ainc)
    end if !mype_io

    deallocate(work,work_sub)

end subroutine gsi_fv3ncdf_write_fv3_dz
subroutine check(status)
    use kinds, only: i_kind
    use netcdf, only: nf90_noerr,nf90_strerror
    integer(i_kind), intent ( in) :: status

    if(status /= nf90_noerr) then
       print *,'ncdf error ', trim(nf90_strerror(status))
       stop  
    end if
end subroutine check
subroutine gsi_fv3ncdf_write_v1(filename,varname,var,mype_io,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_write
!   pgrmmr: wu
!
! abstract:
!
! program history log:
! 2020-03-05  lei  modified from gsi_fv3ncdf_write to gsi_fv3ncdf_write_v1  
!   input argument list:
!    varu,varv
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: mpi_rtype,mpi_comm_world,ierror,npe,mype
    use gridmod, only: lat2,lon2,nlon,nlat,lat1,lon1,nsig
    use gridmod, only: ijn,displs_g,itotsub,iglobal
    use mod_fv3_lolgrid, only: fv3_ll_to_h_regular_grids,fv3_h_to_ll_regular_grids
    use general_commvars_mod, only: ltosi,ltosj
    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var
    implicit none

    real(r_kind)   ,intent(in   ) :: var(lat2,lon2,nsig)
    integer(i_kind),intent(in   ) :: mype_io
    logical        ,intent(in   ) :: add_saved
    character(*)   ,intent(in   ) :: varname,filename

    integer(i_kind) :: VarId,gfile_loc
    integer(i_kind) :: ilev0
    integer(i_kind) i,j,mm1,k,kr,ns,n,m
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:,:):: work_sub,work_a
    real(r_kind),allocatable,dimension(:,:,:):: work_b
    real(r_kind),allocatable,dimension(:,:):: workb2,worka2


    mm1=mype+1

    allocate(    work(max(iglobal,itotsub)*nsig),work_sub(lat1,lon1,nsig))
!!!!!!!! reverse z !!!!!!!!!!!!!!
    do k=1,nsig
       kr=nsig+1-k
       do i=1,lon1
          do j=1,lat1
             work_sub(j,i,kr)=var(j+1,i+1,k)
          end do
       end do
    enddo
    call mpi_gatherv(work_sub,ijnz(mm1),mpi_rtype, &
          work,ijnz,displsz_g,mpi_rtype,mype_io,mpi_comm_world,ierror)

    if(mype==mype_io) then
       allocate( work_a(nlat,nlon,nsig))
       ns=0
       do m=1,npe
          do k=1,nsig
             do n=displs_g(m)+1,displs_g(m)+ijn(m) 
                ns=ns+1
                work_a(ltosi(n),ltosj(n),k)=work(ns)
             end do
          enddo
       enddo

       allocate( work_b(nlon_regional,nlat_regional,nsig+1))

       call check( nf90_open(trim(filename),nf90_write,gfile_loc) )
       call check( nf90_inq_varid(gfile_loc,trim(varname),VarId) )
       call check( nf90_get_var(gfile_loc,VarId,work_b) )
       ilev0=1

       if(add_saved)then
          allocate( workb2(nlon_regional,nlat_regional))
          allocate( worka2(nlat,nlon))
! for being now only lev between (including )  2 and nsig+1 of work_b (:,:,lev) 
! are updated
          do k=1,nsig
             call fv3_h_to_ll_regular_grids(work_b(:,:,ilev0+k),worka2,nlon_regional,nlat_regional,nlon,nlat,p_fv3sar2anlgrid)
!!!!!!!! analysis_inc:  work_a !!!!!!!!!!!!!!!!
             work_a(:,:,k)=work_a(:,:,k)-worka2(:,:)
             call fv3_ll_to_h_regular_grids(work_a(1,1,k),workb2,nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
             work_b(:,:,ilev0+k)=work_b(:,:,ilev0+k)+workb2(:,:)
          enddo
          deallocate(worka2,workb2)
       else
          do k=1,nsig
             call fv3_ll_to_h_regular_grids(work_a(1,1,k),work_b(1,1,ilev0+k),nlon,nlat,nlon_regional,nlat_regional,.true.,p_fv3sar2anlgrid)
          enddo
       endif

       print *,'write out ',trim(varname),' to ',trim(filename)
       call check( nf90_put_var(gfile_loc,VarId,work_b) )
       call check( nf90_close(gfile_loc) )
       deallocate(work_b,work_a)
    end if !mype_io

    deallocate(work,work_sub)

end subroutine gsi_fv3ncdf_write_v1


end module gsi_rfv3io_mod
