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

module gfs_ncio_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants
  use kinds

  !-----------------------------------------------------------------------

  use namelist_def
  use module_ncio

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type gfs_grid
     real(r_kind),                 dimension(:,:),     allocatable :: rlon
     real(r_kind),                 dimension(:,:),     allocatable :: rlat
     real(r_kind)                                                  :: rlon_min
     real(r_kind)                                                  :: rlon_max
     real(r_kind)                                                  :: rlat_min
     real(r_kind)                                                  :: rlat_max
     real(r_kind)                                                  :: dx
     real(r_kind)                                                  :: dy
     integer                                                       :: ntrunc
     integer                                                       :: ncoords
     integer                                                       :: nlons
     integer                                                       :: nlats
     integer                                                       :: nz
  end type gfs_grid ! type gfs_grid

  type ncio_meta
     real,        dimension(:,:),     allocatable :: vcoord
     real,        dimension(:),       allocatable :: lon
     real,        dimension(:),       allocatable :: lat
     real                                         :: rlon_min
     real                                         :: rlon_max
     real                                         :: rlat_min
     real                                         :: rlat_max
     integer                                       :: idate(6)
     integer                                       :: dimx
     integer                                       :: dimy
     integer                                       :: dimz
     integer                                       :: ntrac
     integer                                       :: ncldt
     integer                                       :: idvc
     integer                                       :: idsl
     integer                                       :: idvm
     integer                                       :: fhour
  end type ncio_meta ! type ncio_meta

  !-----------------------------------------------------------------------

  ! Define global variables

  type(Dataset)                                               :: gfile

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: gfs_grid_initialize
  public :: gfs_grid_cleanup
  public :: gfs_grid
  public :: gfs_ncio_initialize
  public :: gfs_ncio_finalize
  public :: gfs_ncio_read
  public :: gfs_ncio_vcoord
  public :: gfs_ncio_flip_xlat_axis
  public :: ncio_meta

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! gfs_ncio_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_ncio_initialize(meta_ncio,filename)

    ! Define variables passed to routine

    type(ncio_meta)                            :: meta_ncio
    character(len=500), optional, intent(inout) :: filename
    type(Dimension)                             :: ncdim
    real, allocatable, dimension(:)             :: tmp1d
    real, allocatable, dimension(:,:)           :: tmp2d

    !=====================================================================

    gfile = open_dataset(trim(adjustl(filename)))
    ncdim = get_dim(gfile,'grid_xt'); meta_ncio%dimx = ncdim%len
    ncdim = get_dim(gfile,'grid_yt'); meta_ncio%dimy = ncdim%len
    ncdim = get_dim(gfile,'pfull'); meta_ncio%dimz = ncdim%len
    if (.not. allocated(meta_ncio%lon)) &
        allocate(meta_ncio%lon(meta_ncio%dimx*meta_ncio%dimy))
    if (.not. allocated(meta_ncio%lat)) &
        allocate(meta_ncio%lat(meta_ncio%dimx*meta_ncio%dimy))
    if (.not. allocated(meta_ncio%vcoord)) &
        allocate(meta_ncio%vcoord(meta_ncio%dimz+1,2))
    call read_vardata(gfile,'lon', tmp2d)
    meta_ncio%lon = reshape(tmp2d, (/meta_ncio%dimx*meta_ncio%dimy/))
    call read_vardata(gfile,'lat', tmp2d)
    meta_ncio%lat = reshape(tmp2d, (/meta_ncio%dimx*meta_ncio%dimy/))
    meta_ncio%idate = get_idate_from_time_units(gfile)
    ! hard code these values that are the same for GFS
    meta_ncio%idvc=2
    meta_ncio%idsl=1
    meta_ncio%idvm=1
    meta_ncio%ntrac = 8
    meta_ncio%ncldt = 5
    call read_attribute(gfile,'ak',tmp1d)
    meta_ncio%vcoord(:,1) = tmp1d(:)
    call read_attribute(gfile,'bk',tmp1d)
    meta_ncio%vcoord(:,2) = tmp1d(:)

    call read_vardata(gfile,'time',tmp1d)
    meta_ncio%fhour = nint(tmp1d(1))

  end subroutine gfs_ncio_initialize

  !=======================================================================

  ! gfs_ncio_finalize.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_ncio_finalize()

    !=====================================================================

    call close_dataset(gfile)

    !=====================================================================

  end subroutine gfs_ncio_finalize

  !=======================================================================

  ! gfs_ncio_vcoord.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_ncio_vcoord(meta_ncio,filename,vcoord)

    ! Define variables passed to routine

    type(Dataset)                                                   :: lgfile
    type(ncio_meta)                                                 :: meta_ncio
    character(len=500)                                              :: filename
    real(r_kind),          dimension(meta_ncio%dimz+1,2)            :: vcoord
    real, allocatable, dimension(:)                                 :: tmp1d

    !=====================================================================

    ! Define local variables

    lgfile = open_dataset(trim(adjustl(filename)))
    call read_attribute(lgfile,'ak',tmp1d)
    vcoord(:,1) = tmp1d(:)
    call read_attribute(lgfile,'bk',tmp1d)
    vcoord(:,2) = tmp1d(:)
    call close_dataset(lgfile)

    !=====================================================================

  end subroutine gfs_ncio_vcoord

  !=======================================================================

  ! gfs_ncio_flip_xlat_axis.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_ncio_flip_xlat_axis(meta_ncio,grid)
    ! flip latitudes from N to S to S to N

    ! Define variables passed to routine

    type(ncio_meta)                                 :: meta_ncio
    real,  dimension(meta_ncio%dimx,meta_ncio%dimy) :: grid

    ! Define variables computed within routine

    real,  dimension(meta_ncio%dimx,meta_ncio%dimy) :: workgrid

    ! Define counting variables

    integer                                         :: i, j

    !=====================================================================

    ! Define local variables

    workgrid = grid

    ! Loop through local variable

    do j = 1, meta_ncio%dimy

       ! Loop through local variable

       do i = 1, meta_ncio%dimx

          ! Define local variables

          grid(i,meta_ncio%dimy - j + 1) = workgrid(i,j)

       end do ! do i = 1, meta_ncio%dimx

    end do ! do j = 1, meta_ncio%dimy

    !=====================================================================

  end subroutine gfs_ncio_flip_xlat_axis

  !=======================================================================

  ! gfs_ncio_read.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_ncio_read(ncio_data,ncio_varname)

    ! Define variables passed to routine

    character(10)                                           :: ncio_varname
    real,allocatable                                        :: ncio_data(:,:,:)

    call read_vardata(gfile,trim(ncio_varname),ncio_data)


    !=====================================================================

  end subroutine gfs_ncio_read


  !=======================================================================

  ! gfs_grid_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_grid_initialize(grid,meta_ncio)

    ! Define variables passed to routine

    type(gfs_grid)                                                       :: grid
    type(ncio_meta)                                                    :: meta_ncio

    ! Define variables computed within routine

    real(r_kind),               dimension(:),                allocatable :: slat
    real(r_kind),               dimension(:),                allocatable :: wlat
    real(r_kind),               dimension(:),                allocatable :: workgrid

    ! Define counting variables

    integer                                                              :: i, j, n

    !=====================================================================

    ! Define local variables

    call init_constants_derived()

    ! Allocate memory for local variables

    if(.not. allocated(grid%rlon))                                         &
         & allocate(grid%rlon(grid%nlons,grid%nlats))
    if(.not. allocated(grid%rlat))                                         &
         & allocate(grid%rlat(grid%nlons,grid%nlats))
    if(.not. allocated(workgrid))                                          &
         & allocate(workgrid(grid%nlats))

    ! Compute local variables

    grid%ncoords = grid%nlons*grid%nlats

    n = 0
    do j=1,grid%nlats
    do i=1,grid%nlons
       n = n + 1
       grid%rlon(i,j) = meta_ncio%lon(n)
       grid%rlat(i,j) = meta_ncio%lat(n)
    enddo
    enddo

    ! Deallocate memory for local variables

    if(allocated(slat))     deallocate(slat)
    if(allocated(wlat))     deallocate(wlat)
    if(allocated(workgrid)) deallocate(workgrid)

    !=====================================================================

  end subroutine gfs_grid_initialize

  !=======================================================================

  ! gfs_grid_cleanup.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_grid_cleanup(grid)

    ! Define variables passed to routine

    type(gfs_grid)                                                       :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%rlon)) deallocate(grid%rlon)
    if(allocated(grid%rlat)) deallocate(grid%rlat)

    !=====================================================================

  end subroutine gfs_grid_cleanup

  !=======================================================================

end module gfs_ncio_interface
