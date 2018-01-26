!    Copyright (C) 2015 Henry R. Winterbottom

!    Email: Henry.Winterbottom@noaa.gov

!    Snail-mail:

!    Henry R. Winterbottom
!    NOAA/OAR/PSD R/PSD1
!    325 Broadway
!    Boulder, CO 80303-3328

!    This file is part of global-model-py.

!    global-model-py is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.

!    global-model-py is distributed in the hope that it will be
!    useful, but WITHOUT ANY WARRANTY; without even the implied
!    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!    See the GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with global-model-py.  If not, see
!    <http://www.gnu.org/licenses/>.

module fv3_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants
  use kinds

  !-----------------------------------------------------------------------

  use gfs_nems_interface
  use namelist_def
  use netcdf
  use variable_interface

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type analysis_grid
     character(len=500)                          :: filename
     real(r_kind), dimension(:,:,:), allocatable :: dpres
     real(r_kind), dimension(:,:,:), allocatable :: delz
     real(r_kind), dimension(:,:,:), allocatable :: ugrd
     real(r_kind), dimension(:,:,:), allocatable :: vgrd
     real(r_kind), dimension(:,:,:), allocatable :: spfh
     real(r_kind), dimension(:,:,:), allocatable :: tmp
     real(r_kind), dimension(:,:,:), allocatable :: clwmr
     real(r_kind), dimension(:,:,:), allocatable :: o3mr
     real(r_kind), dimension(:,:,:), allocatable :: icmr
     real(r_kind), dimension(:,:),   allocatable :: psfc
     real(r_kind), dimension(:),     allocatable :: ak
     real(r_kind), dimension(:),     allocatable :: bk
     real(r_kind), dimension(:),     allocatable :: ck
  end type analysis_grid ! type analysis_grid

  type increment_grid
     real(r_kind), dimension(:,:,:), allocatable :: delp_inc
     real(r_kind), dimension(:,:,:), allocatable :: delz_inc
     real(r_kind), dimension(:,:,:), allocatable :: u_inc
     real(r_kind), dimension(:,:,:), allocatable :: v_inc
     real(r_kind), dimension(:,:,:), allocatable :: sphum_inc
     real(r_kind), dimension(:,:,:), allocatable :: temp_inc
     real(r_kind), dimension(:,:,:), allocatable :: clwmr_inc
     real(r_kind), dimension(:,:,:), allocatable :: o3mr_inc
     real(r_kind), dimension(:,:,:), allocatable :: icmr_inc
     real(r_kind), dimension(:),     allocatable :: lon
     real(r_kind), dimension(:),     allocatable :: lat
     real(r_kind), dimension(:),     allocatable :: lev
     real(r_kind), dimension(:),     allocatable :: ilev
     real(r_kind), dimension(:),     allocatable :: pfull
     real(r_kind), dimension(:),     allocatable :: hyai
     real(r_kind), dimension(:),     allocatable :: hybi
     integer                                     :: nx
     integer                                     :: ny
     integer                                     :: nz
     integer                                     :: nzp1
  end type increment_grid ! type increment_grid

  ! Define global variables

  type(nemsio_meta)   :: meta_nemsio
  type(analysis_grid) :: an_grid
  type(analysis_grid) :: fg_grid

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: fv3_calc_increment

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! fv3_calc_increment.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_calc_increment()

    ! Define variables computed within routine

    type(increment_grid) :: grid

    !=====================================================================

    ! Compute local variables

    call fv3_increment_compute(grid)

    ! Define local variables

    call fv3_increment_write(grid)

    ! Deallocate memory for local variables

    call fv3_increment_cleanup(grid)

    !=====================================================================

  end subroutine fv3_calc_increment

  !=======================================================================

  ! fv3_increment_write.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_write(grid)

    ! Define variables passed to routine

    type(increment_grid) :: grid

    ! Define variables computed within routine

    integer, dimension(3) :: dimid_3d
    integer, dimension(1) :: dimid_1d

    integer :: varid_lon
    integer :: varid_lat
    integer :: varid_lev
    integer :: varid_pfull
    integer :: varid_ilev
    integer :: varid_hyai
    integer :: varid_hybi
    integer :: varid_u_inc
    integer :: varid_v_inc
    integer :: varid_delp_inc
    integer :: varid_delz_inc
    integer :: varid_temp_inc
    integer :: varid_sphum_inc
    integer :: varid_clwmr_inc
    integer :: varid_o3mr_inc
    integer :: varid_icmr_inc
    integer :: dimid_lon
    integer :: dimid_lat
    integer :: dimid_lev
    integer :: dimid_ilev
    integer :: ncfileid
    integer :: ncvarid
    integer :: ncdimid

    !=====================================================================

    ! Define local variables

    print *,'writing to ',trim(increment_filename)
    call netcdf_check(nf90_create(trim(increment_filename), &
         cmode=ior(NF90_CLOBBER,NF90_64BIT_OFFSET),ncid=ncfileid), &
         & 'nf90_create')

    call netcdf_check(nf90_def_dim(ncfileid,'lon',grid%nx,dimid_lon), &
         & 'nf90_def_dim', context='lon')

    call netcdf_check(nf90_def_dim(ncfileid,'lat',grid%ny,dimid_lat), &
         & 'nf90_def_dim', context='lat')

    call netcdf_check(nf90_def_dim(ncfileid,'lev',grid%nz,dimid_lev), &
         & 'nf90_def_dim', context='lev')

    call netcdf_check(nf90_def_dim(ncfileid,'ilev',grid%nzp1,dimid_ilev), &
         & 'nf90_def_dim', context='ilev')

    if (debug) print *,'dims',grid%nx,grid%ny,grid%nz,grid%nzp1

    dimid_1d(1) = dimid_lon
    call netcdf_check(nf90_def_var(ncfileid,'lon',nf90_float,dimid_1d,varid_lon), &
         & 'nf90_def_var lon')
    call netcdf_check(nf90_put_att(ncfileid,varid_lon,'units','degrees_east'), &
         & 'nf90_put_att', context='lon units')

    dimid_1d(1) = dimid_lat
    call netcdf_check(nf90_def_var(ncfileid,'lat',nf90_float,dimid_1d,varid_lat), &
         & 'nf90_def_var', context='lat')
    call netcdf_check(nf90_put_att(ncfileid,varid_lat,'units','degrees_north'), &
         & 'nf90_put_att', context='lat units')

    dimid_1d(1) = dimid_lev
    call netcdf_check(nf90_def_var(ncfileid,'lev',nf90_float,dimid_1d,varid_lev), &
         & 'nf90_def_var', context='lev')

    call netcdf_check(nf90_def_var(ncfileid,'pfull',nf90_float,dimid_1d,varid_pfull), &
         & 'nf90_def_var', context='pfull')

    dimid_1d(1) = dimid_ilev
    call netcdf_check(nf90_def_var(ncfileid,'ilev',nf90_float,dimid_1d,varid_ilev), &
         & 'nf90_def_var', context='ilev')

    call netcdf_check(nf90_def_var(ncfileid,'hyai',nf90_float,dimid_1d,varid_hyai), &
         & 'nf90_def_var', context='hyai')

    call netcdf_check(nf90_def_var(ncfileid,'hybi',nf90_float,dimid_1d,varid_hybi), &
         & 'nf90_def_var', context='hybi')

    dimid_3d(1) = dimid_lon
    dimid_3d(2) = dimid_lat
    dimid_3d(3) = dimid_lev

    call netcdf_check(nf90_def_var(ncfileid,'u_inc',nf90_float,dimid_3d,varid_u_inc), &
         & 'nf90_def_var', context='u_inc')

    call netcdf_check(nf90_def_var(ncfileid,'v_inc',nf90_float,dimid_3d,varid_v_inc), &
         & 'nf90_def_var', context='v_inc')

    call netcdf_check(nf90_def_var(ncfileid,'delp_inc',nf90_float,dimid_3d,varid_delp_inc), &
         & 'nf90_def_var', context='delp_inc')

    call netcdf_check(nf90_def_var(ncfileid,'delz_inc',nf90_float,dimid_3d,varid_delz_inc), &
         & 'nf90_def_var', context='delz_inc')

    call netcdf_check(nf90_def_var(ncfileid,'T_inc',nf90_float,dimid_3d,varid_temp_inc), &
         & 'nf90_def_var', context='temp_inc')

    call netcdf_check(nf90_def_var(ncfileid,'sphum_inc',nf90_float,dimid_3d,varid_sphum_inc), &
         & 'nf90_def_var', context='sphum_inc')

    call netcdf_check(nf90_def_var(ncfileid,'liq_wat_inc',nf90_float,dimid_3d,varid_clwmr_inc), &
         & 'nf90_def_var', context='clwmr_inc')

    call netcdf_check(nf90_def_var(ncfileid,'o3mr_inc',nf90_float,dimid_3d,varid_o3mr_inc), &
         & 'nf90_def_var', context='o3mr_inc')

    if ( do_icmr ) then
      call netcdf_check(nf90_def_var(ncfileid,'icmr_inc',nf90_float,dimid_3d, varid_icmr_inc), &
           & 'nf90_def_var', context='icmr_inc')
    endif

    call netcdf_check(nf90_put_att(ncfileid,nf90_global,'source','GSI'), &
         & 'nf90_put_att', context='source')

    call netcdf_check(nf90_put_att(ncfileid,nf90_global,'comment','global analysis increment from calc_increment.x'), &
         & 'nf90_put_att', context='comment')

    call netcdf_check(nf90_enddef(ncfileid), &
         & 'nf90_enddef')

    call netcdf_check(nf90_put_var(ncfileid,varid_lon,grid%lon), &
         & 'nf90_put_var', context='lon')

    call netcdf_check(nf90_put_var(ncfileid,varid_lat,grid%lat), &
         & 'nf90_put_var', context='lat')

    call netcdf_check(nf90_put_var(ncfileid,varid_lev,grid%lev), &
         & 'nf90_put_var', context='lev')

    call netcdf_check(nf90_put_var(ncfileid,varid_ilev,grid%ilev), &
         & 'nf90_put_var', context='ilev')

    call netcdf_check(nf90_put_var(ncfileid,varid_pfull,grid%pfull), &
         & 'nf90_put_var', context='pfull')

    call netcdf_check(nf90_put_var(ncfileid,varid_hyai,grid%hyai), &
         & 'nf90_put_var', context='hyai')

    call netcdf_check(nf90_put_var(ncfileid,varid_hybi,grid%hybi), &
         & 'nf90_put_var', context='hybi')

    if (debug) print*, 'writing u_inc min/max =', minval(grid%u_inc),maxval(grid%u_inc)
    call netcdf_check(nf90_put_var(ncfileid,varid_u_inc,grid%u_inc), &
         & 'nf90_put_var', context='u_inc')

    if (debug) print*, 'writing v_inc min/max =', minval(grid%v_inc),maxval(grid%v_inc)
    call netcdf_check(nf90_put_var(ncfileid,varid_v_inc,grid%v_inc), &
         & 'nf90_put_var', context='v_inc')

    if (debug) print*, 'writing delp_inc min/max =', minval(grid%delp_inc),maxval(grid%delp_inc)
    call netcdf_check(nf90_put_var(ncfileid,varid_delp_inc,grid%delp_inc), &
         & 'nf90_put_var', context='delp_inc')

    if (debug) print*, 'writing delz_inc min/max =', minval(grid%delz_inc),maxval(grid%delz_inc)
    call netcdf_check(nf90_put_var(ncfileid,varid_delz_inc,grid%delz_inc), &
         & 'nf90_put_var', context='delz_inc')

    if (debug) print*, 'writing temp_inc min/max =', minval(grid%temp_inc),maxval(grid%temp_inc)
    call netcdf_check(nf90_put_var(ncfileid,varid_temp_inc,grid%temp_inc), &
         & 'nf90_put_var', context='temp_inc')

    if (debug) print*, 'writing sphum_inc min/max =', minval(grid%sphum_inc),maxval(grid%sphum_inc)
    call netcdf_check(nf90_put_var(ncfileid,varid_sphum_inc,grid%sphum_inc), &
         & 'nf90_put_var', context='sphum_inc')

    if (debug) print*, 'writing clwmr_inc min/max =', minval(grid%clwmr_inc),maxval(grid%clwmr_inc)
    call netcdf_check(nf90_put_var(ncfileid,varid_clwmr_inc,grid%clwmr_inc), &
         & 'nf90_put_var', context='clwmr_inc')

    if (debug) print*, 'writing o3mr_inc min/max =', minval(grid%o3mr_inc),maxval(grid%o3mr_inc)
    call netcdf_check(nf90_put_var(ncfileid,varid_o3mr_inc,grid%o3mr_inc), &
         & 'nf90_put_var', context='o3mr_inc')

    if ( do_icmr ) then
      if (debug) print*, 'writing icmr_inc min/max =', minval(grid%icmr_inc),maxval(grid%icmr_inc)
      call netcdf_check(nf90_put_var(ncfileid,varid_icmr_inc,grid%icmr_inc), &
           & 'nf90_put_var', context='icmr_inc')
    endif

    call netcdf_check(nf90_close(ncfileid), &
         & 'nf90_close')

    !=====================================================================

  end subroutine fv3_increment_write

  !=======================================================================

  !=======================================================================

  subroutine netcdf_check(ncstatus, nf90_call, context)

    implicit none

    integer, intent(in) :: ncstatus
    character(len=*), intent(in) :: nf90_call
    character(len=*), intent(in), optional :: context

    character(len=500) :: error_msg

    if (ncstatus /= nf90_noerr) then
      if ( present(context) ) then
        error_msg = 'error in: ' // trim(nf90_call) // ': ' // trim(context) // ': '//trim(nf90_strerror(ncstatus))
      else
        error_msg = 'error in: ' // trim(nf90_call) // ': ' // trim(nf90_strerror(ncstatus))
      endif
       print*, trim(error_msg)
       stop 1
    endif

  end subroutine netcdf_check

  !=======================================================================

  ! fv3_increment_compute.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_compute(incr_grid)

    ! Define variables passed to routine

    type(increment_grid) :: incr_grid

    ! Define variables computed within routine

    type(gfs_grid) :: grid

    ! Define counting variables

    integer :: i, j, k

    ! Define variable name string

    character(len=10) :: varname

    !=====================================================================

    ! Define local variables

    call init_constants_derived()
    call fv3_increment_initialize(incr_grid)
    an_grid%filename = analysis_filename
    fg_grid%filename = firstguess_filename
    call fv3_increment_define_analysis(an_grid)
    call fv3_increment_define_analysis(fg_grid)

    ! Compute local variables

    incr_grid%u_inc     = an_grid%ugrd  - fg_grid%ugrd
    incr_grid%v_inc     = an_grid%vgrd  - fg_grid%vgrd
    incr_grid%delp_inc  = an_grid%dpres - fg_grid%dpres
    incr_grid%delz_inc  = an_grid%delz  - fg_grid%delz
    incr_grid%temp_inc  = an_grid%tmp   - fg_grid%tmp
    incr_grid%sphum_inc = an_grid%spfh  - fg_grid%spfh
    incr_grid%clwmr_inc = an_grid%clwmr - fg_grid%clwmr
    incr_grid%o3mr_inc  = an_grid%o3mr  - fg_grid%o3mr
    if ( do_icmr ) incr_grid%icmr_inc = an_grid%icmr - fg_grid%icmr

    do i=1,max_vars
        varname = incvars_to_zero(i)
        if ( trim(varname) /= 'NONE' ) then
            if ( trim(varname) == 'u_inc'     ) incr_grid%u_inc     = zero
            if ( trim(varname) == 'v_inc'     ) incr_grid%v_inc     = zero
            if ( trim(varname) == 'delp_inc'  ) incr_grid%delp_inc  = zero
            if ( trim(varname) == 'delz_inc'  ) incr_grid%delz_inc  = zero
            if ( trim(varname) == 'temp_inc'  ) incr_grid%temp_inc  = zero
            if ( trim(varname) == 'sphum_inc' ) incr_grid%sphum_inc = zero
            if ( trim(varname) == 'clwmr_inc' ) incr_grid%clwmr_inc = zero
            if ( trim(varname) == 'o3mwr_inc' ) incr_grid%o3mr_inc  = zero
            if ( do_icmr .and. trim(varname) == 'icmr_inc' ) incr_grid%icmr_inc = zero
        else
            cycle
        endif
    enddo

    ! Define local variables

    grid%nlons   = meta_nemsio%dimx
    grid%nlats   = meta_nemsio%dimy
    call gfs_grid_initialize(grid, meta_nemsio)
    !incr_grid%lon = grid%rlon(:,1)*rad2deg
    !incr_grid%lat = grid%rlat(1,:)*rad2deg
    incr_grid%lon = grid%rlon(:,1)
    ! reverse latitudes (so they are in increasing order, S to N)
    if (grid%rlat(1,1) > grid%rlat(1,grid%nlats)) then
       do j=1,grid%nlats
          incr_grid%lat(j) = grid%rlat(1,grid%nlats-j+1)
       enddo
    else
       incr_grid%lat = grid%rlat(1,:)
    endif

    ! Loop through local variable

    do k = 1, incr_grid%nz

       ! Define local variables

       incr_grid%lev(k)   = real(k)
       incr_grid%pfull(k) = real(k)

    end do ! do k = 1, incr_grid%nz

    ! Loop through local variable

    do k = 1, incr_grid%nzp1

       ! Define local variables

       incr_grid%ilev(k) = real(k)
       incr_grid%hyai(k) = real(k)
       incr_grid%hybi(k) = real(k)

    end do ! do k = 1, incr_grid%nzp1

    ! Deallocate memory for local variables

    call gfs_grid_cleanup(grid)

    !=====================================================================

  end subroutine fv3_increment_compute

  !=======================================================================

  ! fv3_increment_define_analysis.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_define_analysis(grid)

    ! Define variables passed to routine

    type(analysis_grid) :: grid

    ! Define variables computed within routine

    type(varinfo) :: var_info

    real(r_kind), dimension(:,:,:), allocatable :: pressi
    real(r_kind), dimension(:,:,:), allocatable :: vcoord
    real(r_kind), dimension(:),     allocatable :: workgrid

    logical :: flip_lats
    logical :: ldpres = .false.

    ! Define counting variables

    integer :: i, j, k

    !=====================================================================

    ! Define local variables

    call gfs_nems_initialize(meta_nemsio,filename=grid%filename)
    ! Allocate memory for local variables

    if(.not. allocated(pressi))                                            &
         & allocate(pressi(meta_nemsio%dimx,meta_nemsio%dimy,              &
         & meta_nemsio%dimz + 1))
    if(.not. allocated(vcoord))                                            &
         & allocate(vcoord(meta_nemsio%dimz + 1,3,2))
    if(.not. allocated(workgrid))                                          &
         & allocate(workgrid(meta_nemsio%dimx*meta_nemsio%dimy))

    ! Define local variables

    if (debug) print *,'lats',meta_nemsio%lat(1), meta_nemsio%lat(meta_nemsio%dimx*meta_nemsio%dimy)
    if (meta_nemsio%lat(1) > meta_nemsio%lat(meta_nemsio%dimx*meta_nemsio%dimy)) then
      flip_lats = .true.
    else
      flip_lats = .false.
    endif
    if (debug) print *,'flip_lats',flip_lats

    ldpres = gfs_nems_variable_exist(meta_nemsio,'dpres')

    if ( .not. ldpres ) then

       call gfs_nems_vcoord(meta_nemsio,grid%filename,vcoord)
       grid%ak           = vcoord(:,1,1)
       grid%bk           = vcoord(:,2,1)
       var_info%var_name = 'psfc'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,var_info%nems_levtyp,   &
            & 1)
       grid%psfc(:,:)    = reshape(workgrid,(/meta_nemsio%dimx,               &
            & meta_nemsio%dimy/))

       do k = 1, meta_nemsio%dimz + 1
          pressi(:,:,k) = grid%ak(k) + grid%bk(k)*grid%psfc(:,:)
       end do ! do k = 1, meta_nemsio%dimz + 1

    endif

    do k = 1, meta_nemsio%dimz

       ! Define local variables

       if ( ldpres ) then
          var_info%var_name                        = 'dpres'
          call variable_lookup(var_info)
          call gfs_nems_read(workgrid,var_info%nems_name,                  &
               & var_info%nems_levtyp,k)
          grid%dpres(:,:,meta_nemsio%dimz - k + 1)  =                      &
               & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       else
          grid%dpres(:,:,meta_nemsio%dimz - k + 1) = pressi(:,:,k) -       &
               & pressi(:,:,k+1)
       endif
       !if (debug) print *,'dpres',k,minval(grid%dpres(:,:,meta_nemsio%dimz - k + 1)),&
       !maxval(grid%dpres(:,:,meta_nemsio%dimz - k + 1))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%dpres(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'delz'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%delz(:,:,meta_nemsio%dimz - k + 1)  =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%delz(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'ugrd'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%ugrd(:,:,meta_nemsio%dimz - k + 1)  =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%ugrd(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'vgrd'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%vgrd(:,:,meta_nemsio%dimz - k + 1)  =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%vgrd(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'spfh'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%spfh(:,:,meta_nemsio%dimz - k + 1)  =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%spfh(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'tmp'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%tmp(:,:,meta_nemsio%dimz - k + 1)   =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%tmp(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'clwmr'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%clwmr(:,:,meta_nemsio%dimz - k + 1) =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%clwmr(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'o3mr'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%o3mr(:,:,meta_nemsio%dimz - k + 1)  =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%o3mr(:,:,meta_nemsio%dimz - k + 1))
       if ( do_icmr ) then
         var_info%var_name                        = 'icmr'
         call variable_lookup(var_info)
         call gfs_nems_read(workgrid,var_info%nems_name,                     &
              & var_info%nems_levtyp,k)
         grid%icmr(:,:,meta_nemsio%dimz - k + 1)  =                          &
              & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
         if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
              & grid%icmr(:,:,meta_nemsio%dimz - k + 1))
       endif

    end do ! do k = 1, meta_nemsio%dimz

    ! Deallocate memory for local variables

    if(allocated(pressi))   deallocate(pressi)
    if(allocated(vcoord))   deallocate(vcoord)
    if(allocated(workgrid)) deallocate(workgrid)

    ! Define local variables

    call gfs_nems_finalize()

    !=====================================================================

  end subroutine fv3_increment_define_analysis

  !=======================================================================

  ! fv3_increment_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_initialize(grid)

    ! Define variables passed to routine

    type(increment_grid) :: grid

    !=====================================================================

    ! Define local variables

    call gfs_nems_initialize(meta_nemsio,filename=analysis_filename)
    grid%nx   = meta_nemsio%dimx
    grid%ny   = meta_nemsio%dimy
    grid%nz   = meta_nemsio%dimz
    grid%nzp1 = grid%nz + 1
    call gfs_nems_finalize()

    ! Allocate memory for local variables

    if(.not. allocated(grid%delp_inc))                                     &
         & allocate(grid%delp_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%delz_inc))                                     &
         & allocate(grid%delz_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%u_inc))                                        &
         & allocate(grid%u_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%v_inc))                                        &
         & allocate(grid%v_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%sphum_inc))                                    &
         & allocate(grid%sphum_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%temp_inc))                                     &
         & allocate(grid%temp_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%clwmr_inc))                                    &
         & allocate(grid%clwmr_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%o3mr_inc))                                     &
         & allocate(grid%o3mr_inc(grid%nx,grid%ny,grid%nz))
    if(do_icmr .and. .not. allocated(grid%icmr_inc))                       &
         & allocate(grid%icmr_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%nx))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ny))
    if(.not. allocated(grid%lev))                                          &
         & allocate(grid%lev(grid%nz))
    if(.not. allocated(grid%ilev))                                         &
         & allocate(grid%ilev(grid%nzp1))
    if(.not. allocated(grid%pfull))                                        &
         & allocate(grid%pfull(grid%nz))
    if(.not. allocated(grid%hyai))                                         &
         & allocate(grid%hyai(grid%nzp1))
    if(.not. allocated(grid%hybi))                                         &
         & allocate(grid%hybi(grid%nzp1))
    if(.not. allocated(an_grid%dpres))                                     &
         & allocate(an_grid%dpres(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%delz))                                      &
         & allocate(an_grid%delz(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%ugrd))                                      &
         & allocate(an_grid%ugrd(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%vgrd))                                      &
         & allocate(an_grid%vgrd(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%spfh))                                      &
         & allocate(an_grid%spfh(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%tmp))                                       &
         & allocate(an_grid%tmp(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%clwmr))                                     &
         & allocate(an_grid%clwmr(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%o3mr))                                      &
         & allocate(an_grid%o3mr(grid%nx,grid%ny,grid%nz))
    if(do_icmr .and. .not. allocated(an_grid%icmr))                        &
         & allocate(an_grid%icmr(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%psfc))                                      &
         & allocate(an_grid%psfc(grid%nx,grid%ny))
    if(.not. allocated(an_grid%ak))                                        &
         & allocate(an_grid%ak(grid%nz+1))
    if(.not. allocated(an_grid%bk))                                        &
         & allocate(an_grid%bk(grid%nz+1))
    if(.not. allocated(an_grid%ck))                                        &
         & allocate(an_grid%ck(grid%nz+1))
    if(.not. allocated(fg_grid%dpres))                                     &
         & allocate(fg_grid%dpres(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%delz))                                      &
         & allocate(fg_grid%delz(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%ugrd))                                      &
         & allocate(fg_grid%ugrd(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%vgrd))                                      &
         & allocate(fg_grid%vgrd(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%spfh))                                      &
         & allocate(fg_grid%spfh(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%tmp))                                       &
         & allocate(fg_grid%tmp(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%clwmr))                                     &
         & allocate(fg_grid%clwmr(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%o3mr))                                      &
         & allocate(fg_grid%o3mr(grid%nx,grid%ny,grid%nz))
    if(do_icmr .and. .not. allocated(fg_grid%icmr))                        &
         & allocate(fg_grid%icmr(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%psfc))                                      &
         & allocate(fg_grid%psfc(grid%nx,grid%ny))
    if(.not. allocated(fg_grid%ak))                                        &
         & allocate(fg_grid%ak(grid%nz+1))
    if(.not. allocated(fg_grid%bk))                                        &
         & allocate(fg_grid%bk(grid%nz+1))
    if(.not. allocated(fg_grid%ck))                                        &
         & allocate(fg_grid%ck(grid%nz+1))

    !=====================================================================

  end subroutine fv3_increment_initialize

  !=======================================================================

  ! fv3_increment_cleanup.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_cleanup(grid)

    ! Define variables passed to routine

    type(increment_grid) :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%delp_inc))  deallocate(grid%delp_inc)
    if(allocated(grid%delz_inc))  deallocate(grid%delz_inc)
    if(allocated(grid%u_inc))     deallocate(grid%u_inc)
    if(allocated(grid%v_inc))     deallocate(grid%v_inc)
    if(allocated(grid%sphum_inc)) deallocate(grid%sphum_inc)
    if(allocated(grid%temp_inc))  deallocate(grid%temp_inc)
    if(allocated(grid%clwmr_inc)) deallocate(grid%clwmr_inc)
    if(allocated(grid%o3mr_inc))  deallocate(grid%o3mr_inc)
    if(allocated(grid%icmr_inc))  deallocate(grid%icmr_inc)
    if(allocated(grid%lon))       deallocate(grid%lon)
    if(allocated(grid%lat))       deallocate(grid%lat)
    if(allocated(grid%lev))       deallocate(grid%lev)
    if(allocated(grid%ilev))      deallocate(grid%ilev)
    if(allocated(grid%pfull))     deallocate(grid%pfull)
    if(allocated(grid%hyai))      deallocate(grid%hyai)
    if(allocated(grid%hybi))      deallocate(grid%hybi)
    if(allocated(an_grid%dpres))  deallocate(an_grid%dpres)
    if(allocated(an_grid%delz))   deallocate(an_grid%delz)
    if(allocated(an_grid%ugrd))   deallocate(an_grid%ugrd)
    if(allocated(an_grid%vgrd))   deallocate(an_grid%vgrd)
    if(allocated(an_grid%spfh))   deallocate(an_grid%spfh)
    if(allocated(an_grid%tmp))    deallocate(an_grid%tmp)
    if(allocated(an_grid%clwmr))  deallocate(an_grid%clwmr)
    if(allocated(an_grid%o3mr))   deallocate(an_grid%o3mr)
    if(allocated(an_grid%icmr))   deallocate(an_grid%icmr)
    if(allocated(an_grid%psfc))   deallocate(an_grid%psfc)
    if(allocated(an_grid%ak))     deallocate(an_grid%ak)
    if(allocated(an_grid%bk))     deallocate(an_grid%bk)
    if(allocated(fg_grid%ck))     deallocate(an_grid%ck)
    if(allocated(fg_grid%dpres))  deallocate(fg_grid%dpres)
    if(allocated(fg_grid%delz))   deallocate(fg_grid%delz)
    if(allocated(fg_grid%ugrd))   deallocate(fg_grid%ugrd)
    if(allocated(fg_grid%vgrd))   deallocate(fg_grid%vgrd)
    if(allocated(fg_grid%spfh))   deallocate(fg_grid%spfh)
    if(allocated(fg_grid%tmp))    deallocate(fg_grid%tmp)
    if(allocated(fg_grid%clwmr))  deallocate(fg_grid%clwmr)
    if(allocated(fg_grid%o3mr))   deallocate(fg_grid%o3mr)
    if(allocated(fg_grid%icmr))   deallocate(fg_grid%icmr)
    if(allocated(fg_grid%psfc))   deallocate(fg_grid%psfc)
    if(allocated(fg_grid%ak))     deallocate(fg_grid%ak)
    if(allocated(fg_grid%bk))     deallocate(fg_grid%bk)
    if(allocated(fg_grid%ck))     deallocate(fg_grid%ck)

    !=====================================================================

  end subroutine fv3_increment_cleanup

  !=======================================================================

end module fv3_interface
