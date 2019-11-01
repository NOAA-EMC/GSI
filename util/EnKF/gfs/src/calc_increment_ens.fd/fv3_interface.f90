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
     real(r_kind), dimension(:,:,:), allocatable :: var3d
     real(r_kind), dimension(:,:),   allocatable :: psfc
     real(r_kind), dimension(:),     allocatable :: ak
     real(r_kind), dimension(:),     allocatable :: bk
     real(r_kind), dimension(:),     allocatable :: ck
     real(r_kind), dimension(:),     allocatable :: lon
     real(r_kind), dimension(:),     allocatable :: lat
     real(r_kind), dimension(:),     allocatable :: lev
     real(r_kind), dimension(:),     allocatable :: ilev
     real(r_kind), dimension(:),     allocatable :: pfull
     real(r_kind), dimension(:),     allocatable :: hyai
     real(r_kind), dimension(:),     allocatable :: hybi
     integer                                     :: nx = -1
     integer                                     :: ny = -1
     integer                                     :: nz = -1
     integer                                     :: nzp1 = 0
     logical                                     :: is_allocated = .false.
     logical                                     :: flip_lats = .true.
     logical                                     :: ldpres = .true.
  end type analysis_grid ! type analysis_grid

  type increment_netcdf
    integer :: dimid_lon = -1
    integer :: dimid_lat = -1
    integer :: dimid_lev = -1
    integer :: dimid_ilev = -1
    integer :: ncfileid = -1
  end type increment_netcdf

  integer, parameter :: n_inc_vars = 9  !! number of known variables

  ! Define global variables

  type(nemsio_meta)   :: meta_nemsio !! nemsio metadata for the current file
  type(analysis_grid) :: an_grid !! analysis grid data
  type(analysis_grid) :: fg_grid !! first guess grid data

  !! All known output variables.  These are the names in the output
  !! NetCDF file.  The input names are in input_vars.
  character(len=11), dimension(n_inc_vars) :: output_vars=(/       &
       'u_inc      ', 'v_inc      ', 'delp_inc   ', 'delz_inc   ', &
       'T_inc      ', 'sphum_inc  ', 'liq_wat_inc', 'o3mr_inc   ', &
       'icmr_inc   ' /)

  !! Synonyms for output_vars needed to be backward-compatible with
  !! bugs in the prior version of this script.  These are used to
  !! match to increments_to_zero.
  character(len=11), dimension(n_inc_vars) :: var_zero_synonyms=(/      &
       'u_inc      ', 'v_inc      ', 'delp_inc   ', 'delz_inc   ', &
       'temp_inc   ', 'sphum_inc  ', 'clwmr_inc  ', 'o3mwr_inc  ', &
       'icmr_inc   ' /)

  !! The input name from nemsio that matches each output filename from
  !! output_vars.
  character(len=11), dimension(n_inc_vars) :: input_vars=(/        &
       'ugrd       ', 'vgrd       ', 'dpres      ', 'delz       ', &
       'tmp        ', 'spfh       ', 'clwmr      ', 'o3mr       ', &
       'icmr       ' /)

  private
  public :: fv3_calc_increment

  !=======================================================================
  !=======================================================================

contains

  !=======================================================================
  !=======================================================================

  subroutine fv3_calc_increment(mype)

    integer,intent(in) :: mype

    type(gfs_grid) :: grid !! GFS analysis grid

    type(increment_netcdf) :: ncdat  !! cached info about NetCDF output file

    integer :: j, k ! loop indices within a variable
    integer :: ivar !! loop index over variables in input_vars & output_vars

    ! Formats for print statements:
100 format(A,': ',A)

    ! ------------------------------------------------------------------
    ! Initialize memory, read metadata, and read 1D arrays.

    ! Calculate constants
    call init_constants_derived()

    ! Allocate grids for analysis and first guess
    call fv3_grid_allocate(an_grid,fg_grid)

    ! Read the analysis and first guess non-increment vars and pressure:
    an_grid%filename = analysis_filename
    fg_grid%filename = firstguess_filename
    call fv3_analysis_read_non_inc_vars(an_grid)
    call fv3_analysis_read_non_inc_vars(fg_grid)

    ! ------------------------------------------------------------------
    ! Deal with everything that is NOT a 3D array:

    ! Copy horizontal dimensions from analysis grid
    grid%nlons   = an_grid%nx
    grid%nlats   = an_grid%ny

    ! Read the nemsio header
    call gfs_nems_initialize(meta_nemsio, firstguess_filename)
    call gfs_grid_initialize(grid, meta_nemsio)

    an_grid%lon = grid%rlon(:,1)

    ! reverse latitudes (so they are in increasing order, S to N)
    if (grid%rlat(1,1) > grid%rlat(1,grid%nlats)) then
       do j=1,grid%nlats
          an_grid%lat(j) = grid%rlat(1,grid%nlats-j+1)
       enddo
    else
       an_grid%lat = grid%rlat(1,:)
    endif

    ! Fill 1D vertical arrays with level numbers:

    nz_init: do k = 1, an_grid%nz
       an_grid%lev(k)   = real(k)
       an_grid%pfull(k) = real(k)
    end do nz_init

    nzp1_init: do k = 1, an_grid%nzp1
       an_grid%ilev(k) = real(k)
       an_grid%hyai(k) = real(k)
       an_grid%hybi(k) = real(k)
    end do nzp1_init

    ! Deallocate entire grid.
    call gfs_grid_cleanup(grid)
    call gfs_nems_finalize()

    ! ------------------------------------------------------------------
    ! Start the NetCDF file creation.  Define vars and write
    ! non-increment vars.

    call fv3_increment_def_start(an_grid,ncdat)

    var_def_loop: do ivar=1,n_inc_vars
       call fv3_increment_def_var(ncdat,output_vars(ivar))
       if(trim(input_vars(ivar)) == 'icmr' .and. .not. do_icmr) then
          if (mype==0) print 100, output_vars(ivar), 'do_icmr = F so var will not be in netcdf'
          cycle var_def_loop
       endif
    enddo var_def_loop

    call fv3_increment_def_end(ncdat)
    call fv3_increment_write_start(an_grid,ncdat)

    ! ------------------------------------------------------------------
    ! Deal with 3D arrays

    var_loop: do ivar=1,n_inc_vars
       ! Skip this var if it is icmr and we're told not to do_icmr:
       if(trim(input_vars(ivar)) == 'icmr' .and. .not. do_icmr) then
          if (mype==0) print 100, trim(output_vars(ivar)), &
               'do_icmr = F so will not diff this var'
          cycle var_loop
       endif

       ! Skip this var if it is to be zero.  No point in reading it...
       zero_or_read: if(should_zero_increments_for(var_zero_synonyms(ivar))) then
          if (mype==0) print 100, trim(output_vars(ivar)), &
               'is in incvars_to_zero; setting increments to zero'
          an_grid%var3d = 0
       else

          ! This var should not be skipped.  Let's get the analysis and
          ! first guess from the input files.
          if(trim(input_vars(ivar)) == 'dpres') then
             ! Special case.  We may have to calculate the 3D pressure
             ! from the 2D surface pressure and coordinate system using
             ! the hydrostatic approximation.
             call fv3_analysis_read_or_calc_dpres(an_grid,mype)
             call fv3_analysis_read_or_calc_dpres(fg_grid,mype)
          else
             ! Read the variable from the files directly.
             if (mype==0) print 100, trim(output_vars(ivar)), 'read variable'
             call fv3_analysis_read_var(an_grid,input_vars(ivar))
             call fv3_analysis_read_var(fg_grid,input_vars(ivar))
          endif
          
          ! Subtract and write
          an_grid%var3d = an_grid%var3d - fg_grid%var3d

          ! Flip increment if the delz background is positive (GFSv15)
          if(trim(input_vars(ivar)) == 'delz') then
             if (sum(fg_grid%var3d) > 0.0_r_kind) then
                an_grid%var3d = an_grid%var3d * -1.0_r_kind
             endif
          endif
       endif zero_or_read

       call fv3_netcdf_write_var3d(ncdat,output_vars(ivar),an_grid%var3d)
    enddo var_loop

    call fv3_increment_write_end(ncdat)

    call fv3_grid_deallocate(an_grid,fg_grid)

  end subroutine fv3_calc_increment  

  !=======================================================================

  !! Is this variable in incvars_to_zero?
  logical function should_zero_increments_for(check_var)

    character(len=*), intent(in) :: check_var !! Variable to search for

    ! Local variables

    character(len=10) :: varname ! temporary string for storing variable names
    integer :: i ! incvars_to_zero loop index

    should_zero_increments_for=.false.

    zeros_loop: do i=1,max_vars
       varname = incvars_to_zero(i)
       if ( trim(varname) == check_var ) then
          should_zero_increments_for=.true.
          return
       endif
    end do zeros_loop

  end function should_zero_increments_for

  !=======================================================================
  !== BASIC NETCDF UTILITIES =============================================
  !=======================================================================

  subroutine fv3_netcdf_def_var(ncdat,varname,ncdimid,att1_name,&
                                att1_values,att2_name,att2_values)

    ! Define variables passed to routine

    type(increment_netcdf) :: ncdat !! NetCDF file ids
    character(len=*) :: varname !! Name of the variable to define
    integer, dimension(:) :: ncdimid !! IDs of the file dimensions
    character(len=*), optional :: att1_name   !! name of the first attribute
    character(len=*), optional :: att1_values !! value of the first attribute
    character(len=*), optional :: att2_name   !! name of the second attribute
    character(len=*), optional :: att2_values !! value of the second attribute

    ! Local variable

    integer :: ncvarid ! NetCDF variable ID of the variable we create.

    ! Define the variable in the NetCDF file.
    call netcdf_check( &
         nf90_def_var(ncdat%ncfileid,varname,nf90_float,ncdimid,ncvarid), &
         'nf90_def_var',context=varname)

    ! If attributes were given, define those too.
    if(present(att1_name) .and. present(att1_values)) then
       call netcdf_check( &
            nf90_put_att(ncdat%ncfileid,ncvarid,att1_name,att1_values), &
            'nf90_def_var',context=varname // ' ' // att1_name)
    end if
    if(present(att2_name) .and. present(att2_values)) then
       call netcdf_check( &
            nf90_put_att(ncdat%ncfileid,ncvarid,att2_name,att2_values), &
            'nf90_def_var',context=varname // ' ' // att2_name)
    end if
  end subroutine fv3_netcdf_def_var

  !=======================================================================

  subroutine fv3_netcdf_write_var1d(ncdat,varname,values)

    ! Define variables passed to routine

    type(increment_netcdf) :: ncdat
    character(len=*) :: varname
    real(r_kind), intent(in), dimension(:) :: values

    ! Define variables computed within routine

    integer :: ncvarid

    call netcdf_check(nf90_inq_varid(ncdat%ncfileid,varname,ncvarid),&
                      'nf90_inq_varid',context=varname)
    call netcdf_check(nf90_put_var(ncdat%ncfileid,ncvarid,values),&
                      'nf90_put_var',context=varname)

  end subroutine fv3_netcdf_write_var1d

  !=======================================================================

  subroutine fv3_netcdf_write_var3d(ncdat,varname,values)

    ! Define variables passed to routine

    type(increment_netcdf) :: ncdat
    character(len=*),intent(in) :: varname
    real(r_kind), intent(in), dimension(:,:,:) :: values

    ! Define variables computed within routine

    integer :: ncvarid

    call netcdf_check(nf90_inq_varid(ncdat%ncfileid,varname,ncvarid),&
         'nf90_inq_varid',context=varname)
    call netcdf_check(nf90_put_var(ncdat%ncfileid,ncvarid,values),&
                      'nf90_put_var',context=varname)

  end subroutine fv3_netcdf_write_var3d

  !=======================================================================

  integer function fv3_netcdf_def_dim(ncdat,dimname,dimlen)
    ! Arguments to function
    type(increment_netcdf) :: ncdat !! storage areas for some netcdf ids
    character(len=*) :: dimname !! name of the new dimension
    integer :: dimlen !! length of the new dimension

    call netcdf_check(&
         nf90_def_dim(ncdat%ncfileid,dimname,dimlen,fv3_netcdf_def_dim),&
         'nf90_def_dim',context=dimname)

  end function fv3_netcdf_def_dim

  !=======================================================================

  subroutine netcdf_check(ncstatus, nf90_call, context)
    use mpi
    implicit none

    ! Arguments to subroutine
    integer, intent(in) :: ncstatus !! return status from nf90 function
    character(len=*), intent(in) :: nf90_call !! name of the called function
    character(len=*), intent(in), optional :: context !! contextual info

    integer :: ierr

    ! Formats for print statements
100 format('error in: ',A,': ',A,': ',A) ! context was supplied
200 format('error in: ',A,': ',A) ! context was not supplied

    ! If the nf90 function returned an error status then...
    if (ncstatus /= nf90_noerr) then

      ! send an informative message to stdout and stderr...
      if ( present(context) ) then
         write(0,100) trim(nf90_call), trim(context), trim(nf90_strerror(ncstatus))
         print 100,   trim(nf90_call), trim(context), trim(nf90_strerror(ncstatus))
      else
         write(0,200) trim(nf90_call), trim(nf90_strerror(ncstatus))
         print 200,   trim(nf90_call), trim(nf90_strerror(ncstatus))
      endif

      ! ...and abort the whole program.
      call MPI_Abort(MPI_COMM_WORLD,1,ierr)
    endif

  end subroutine netcdf_check

  !=======================================================================
  !== Increment File Output Utilities ====================================
  !=======================================================================

  subroutine fv3_increment_def_start(grid,ncdat)

    ! Define arguments to this subroutine

    type(analysis_grid) :: grid !! analysis grid data
    type(increment_netcdf) :: ncdat !! netcdf file ids

    print *,'writing to ',trim(increment_filename)

    ! Create the NetCDF file.
    
    call netcdf_check(nf90_create(trim(increment_filename), &
         cmode=ior(NF90_CLOBBER,NF90_64BIT_OFFSET),ncid=ncdat%ncfileid), &
         & 'nf90_create')

    ! Define the dimensions.

    ncdat%dimid_lon=fv3_netcdf_def_dim(ncdat,'lon',grid%nx)
    ncdat%dimid_lat=fv3_netcdf_def_dim(ncdat,'lat',grid%ny)
    ncdat%dimid_lev=fv3_netcdf_def_dim(ncdat,'lev',grid%nz)
    ncdat%dimid_ilev=fv3_netcdf_def_dim(ncdat,'ilev',grid%nzp1)

    if (debug) print *,'dims',grid%nx,grid%ny,grid%nz,grid%nzp1

    ! Define the variables that are NOT increments:

    call fv3_netcdf_def_var(ncdat,'lon',(/ncdat%dimid_lon/),'units','degrees_east')
    call fv3_netcdf_def_var(ncdat,'lat',(/ncdat%dimid_lat/),'units','degrees_north')
    call fv3_netcdf_def_var(ncdat,'lev',(/ncdat%dimid_lev/))
    call fv3_netcdf_def_var(ncdat,'pfull',(/ncdat%dimid_lev/))
    call fv3_netcdf_def_var(ncdat,'ilev',(/ncdat%dimid_ilev/))
    call fv3_netcdf_def_var(ncdat,'hyai',(/ncdat%dimid_ilev/))
    call fv3_netcdf_def_var(ncdat,'hybi',(/ncdat%dimid_ilev/))

  end subroutine fv3_increment_def_start

  !=======================================================================

  subroutine fv3_increment_def_var(ncdat,var)

    type(increment_netcdf) :: ncdat !! netcdf file ids
    character(len=*) :: var !! Name of the variable to define

    ! Locals
    integer, dimension(3) :: dimid_3d

    dimid_3d = (/ ncdat%dimid_lon, ncdat%dimid_lat, ncdat%dimid_lev /)

    call fv3_netcdf_def_var(ncdat,var,dimid_3d)
  end subroutine fv3_increment_def_var

  !=======================================================================

  subroutine fv3_increment_def_end(ncdat)
    !Arguments to routine
    type(increment_netcdf) :: ncdat

    ! Write the global variables: source of this data and comment:

    call netcdf_check(nf90_put_att(ncdat%ncfileid,nf90_global,'source','GSI'), &
         & 'nf90_put_att', context='source')

    call netcdf_check(nf90_put_att(ncdat%ncfileid,nf90_global, &
         'comment','global analysis increment from calc_increment.x'), &
         'nf90_put_att', context='comment')

    ! Terminate the definition phase of the NetCDF output:
    call netcdf_check(nf90_enddef(ncdat%ncfileid),'nf90_enddef')
  end subroutine fv3_increment_def_end

  !=======================================================================

  subroutine fv3_increment_write_start(grid,ncdat)
    !Arguments to routine
    type(analysis_grid) :: grid
    type(increment_netcdf) :: ncdat

    ! Write the variables that are NOT incremented:
    call fv3_netcdf_write_var1d(ncdat,'lon',grid%lon)
    call fv3_netcdf_write_var1d(ncdat,'lat',grid%lat)
    call fv3_netcdf_write_var1d(ncdat,'lev',grid%lev)
    call fv3_netcdf_write_var1d(ncdat,'ilev',grid%ilev)
    call fv3_netcdf_write_var1d(ncdat,'lon',grid%lon)
    call fv3_netcdf_write_var1d(ncdat,'pfull',grid%pfull)
    call fv3_netcdf_write_var1d(ncdat,'hyai',grid%hyai)
    call fv3_netcdf_write_var1d(ncdat,'hybi',grid%hybi)
  end subroutine fv3_increment_write_start

  !=======================================================================

  subroutine fv3_increment_write_end(ncdat)
    !Arguments to routine
    type(increment_netcdf) :: ncdat

    ! Close the NetCDF file.  This also flushes buffers.
    call netcdf_check(nf90_close(ncdat%ncfileid),'nf90_close',&
         context=trim(increment_filename))
  end subroutine fv3_increment_write_end

  !=======================================================================
  !== Analysis / First Guess Read Utilities ==============================
  !=======================================================================

  !! Read one variable that is NOT pressure
  subroutine fv3_analysis_read_var(grid,varname)
    ! Arguments to function

    type(analysis_grid) :: grid !! the analysis or first guess grid
    character(len=*) :: varname !! name of the variable to read

    ! local variables

    type(varinfo) :: var_info ! to request a variable from gfs_nems_read
    real(r_kind), allocatable :: workgrid(:) ! for reordering data
    integer :: k ! Vertical index loop when reading data level-by-level


    ! Read the nemsio file header
    call gfs_nems_initialize(meta_nemsio,filename=grid%filename)

    ! Allocate our local work array
    allocate(workgrid(grid%nx*grid%ny))

    ! Read in the variable, level-by-level:
    do k = 1, grid%nz
       var_info%var_name=varname
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name, &
                          var_info%nems_levtyp,k)

       grid%var3d(:,:,grid%nz-k+1)=reshape(workgrid,(/grid%nx,grid%ny/))
       if (grid%flip_lats) then
          call gfs_nems_flip_xlat_axis( &
               meta_nemsio,grid%var3d(:,:,grid%nz - k + 1))
       endif
    end do

    ! Close the nemsio file
    call gfs_nems_finalize()

    deallocate(workgrid)

  end subroutine fv3_analysis_read_var

  !=======================================================================

  !! Read or calculate 3D pressure
  subroutine fv3_analysis_read_or_calc_dpres(grid,mype)
    ! Arguments to function

    type(analysis_grid) :: grid !! the analysis or first guess grid
    integer,intent(in)  :: mype

    ! local variables

    type(varinfo) :: var_info ! to request a variable from gfs_nems_read
    real(r_kind), allocatable :: workgrid(:) ! for reordering data
    real(r_kind), allocatable :: pressi(:,:,:) ! interface pressure, 3D
    real(r_kind), allocatable :: vcoord(:,:,:) ! a & b for hydro. approx.
    integer :: k ! Vertical index loop when reading data level-by-level

100 format(A,': ',A)

    ! Read the nemsio file header
    call gfs_nems_initialize(meta_nemsio,filename=grid%filename)

    allocate(vcoord(meta_nemsio%dimz + 1,3,2))
    allocate(workgrid(meta_nemsio%dimx*meta_nemsio%dimy))

    ! Is the 3D pressure in the file?
    grid%ldpres = gfs_nems_variable_exist(meta_nemsio,'dpres')

    if ( .not. grid%ldpres ) then
       ! The 3D pressure is NOT in the file.  We need to calculate
       ! pressure from the hydrostatic approximation and surface
       ! pressure.

       if (mype==0) print 100,'dpres','calculate from 2D psfc and hydro. approx.'

       ! Allocate an array for the interface pressure.
       if(.not. allocated(pressi)) then
          allocate(pressi(meta_nemsio%dimx,meta_nemsio%dimy, &
                   meta_nemsio%dimz + 1))
       endif

       ! Read the A, B, and surface pressure
       call gfs_nems_vcoord(meta_nemsio,trim(grid%filename),vcoord)
       grid%ak           = vcoord(:,1,1)
       grid%bk           = vcoord(:,2,1)
       var_info%var_name = 'psfc'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,var_info%nems_levtyp,   &
            & 1)
       grid%psfc(:,:)    = reshape(workgrid,(/meta_nemsio%dimx,               &
            & meta_nemsio%dimy/))

       ! Apply the hydrostatic approximation to get 3D interface pressure:
       do k = 1, meta_nemsio%dimz + 1
          pressi(:,:,k) = grid%ak(k) + grid%bk(k)*grid%psfc(:,:)
       end do ! do k = 1, meta_nemsio%dimz + 1
    else
       if (mype==0) print 100,'dpres','read from file; do not calculate'
    endif

    ! Calculate or read the mid-level 3D pressure:
    do k = 1, meta_nemsio%dimz
       if ( grid%ldpres ) then
          ! Pressure is already in the file.  Read the 3D pressure array.
          var_info%var_name = 'dpres'
          call variable_lookup(var_info)
          call gfs_nems_read(workgrid,var_info%nems_name, &
                            var_info%nems_levtyp,k)
          grid%var3d(:,:,meta_nemsio%dimz - k + 1) = &
               reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       else
          ! Convert interface pressure to mass level pressure, using
          ! the 3D array generated in the prior loop:
          grid%var3d(:,:,meta_nemsio%dimz - k + 1) = &
               pressi(:,:,k) - pressi(:,:,k+1)
       endif

       ! Flip the pressure in the latitude direction if needed
       if (grid%flip_lats) then
          call gfs_nems_flip_xlat_axis( &
               meta_nemsio, grid%var3d(:,:,meta_nemsio%dimz - k + 1) )
       endif
    enddo


    ! Deallocate memory for work arrays

    if(allocated(pressi)) then
       deallocate(pressi) ! only allocated if hydro. pres. is used
    endif
    deallocate(vcoord)
    deallocate(workgrid)

  end subroutine fv3_analysis_read_or_calc_dpres

  !=======================================================================

  !! Read everything that is NOT incremented, plus the pressure
  subroutine fv3_analysis_read_non_inc_vars(grid)

    type(analysis_grid) :: grid !! analysis or first guess to read

    ! Local variables

    ! Read the nemsio file header
    call gfs_nems_initialize(meta_nemsio,filename=grid%filename)

    ! Allocate memory for work arrays
    grid%nx=meta_nemsio%dimx
    grid%ny=meta_nemsio%dimy
    grid%nz=meta_nemsio%dimz

    ! Determine ordering of latitudes:
    if (debug) then
       print *,'lats',meta_nemsio%lat(1), meta_nemsio%lat( &
                meta_nemsio%dimx*meta_nemsio%dimy)
    endif
    if (meta_nemsio%lat(1) > meta_nemsio%lat(meta_nemsio%dimx*meta_nemsio%dimy)) then
       grid%flip_lats = .true.
    else
       grid%flip_lats = .false.
    endif
    if (debug) print *,'flip_lats',grid%flip_lats

    ! Close this nemsio file.
    call gfs_nems_finalize()

  end subroutine fv3_analysis_read_non_inc_vars

  !=======================================================================
  !== Memory Management ==================================================
  !=======================================================================

  subroutine fv3_grid_allocate(an_grid,fg_grid)

    type(analysis_grid) :: an_grid !! analysis grid
    type(analysis_grid) :: fg_grid !! first guess grid

    ! Get the grid dimensions from the analysis file

    call gfs_nems_initialize(meta_nemsio,filename=analysis_filename)
    an_grid%nx   = meta_nemsio%dimx
    an_grid%ny   = meta_nemsio%dimy
    an_grid%nz   = meta_nemsio%dimz
    an_grid%nzp1 = an_grid%nz + 1
    call gfs_nems_finalize()

    ! Assume the first guess has the same dimensions.

    fg_grid%nx   = an_grid%nx
    fg_grid%ny   = an_grid%ny
    fg_grid%nz   = an_grid%nz
    fg_grid%nzp1 = an_grid%nzp1

    if(.not.an_grid%is_allocated) then
       allocate(an_grid%lon(an_grid%nx))
       allocate(an_grid%lat(an_grid%ny))
       allocate(an_grid%lev(an_grid%nz))
       allocate(an_grid%ilev(an_grid%nzp1))
       allocate(an_grid%pfull(an_grid%nz))
       allocate(an_grid%hyai(an_grid%nzp1))
       allocate(an_grid%hybi(an_grid%nzp1))

       allocate(an_grid%var3d(an_grid%nx,an_grid%ny,an_grid%nz))
       allocate(an_grid%psfc(an_grid%nx,an_grid%ny))
       allocate(an_grid%ak(an_grid%nz+1))
       allocate(an_grid%bk(an_grid%nz+1))
       allocate(an_grid%ck(an_grid%nz+1))
       an_grid%is_allocated=.true.
    endif

    if(.not.fg_grid%is_allocated) then
       allocate(fg_grid%var3d(fg_grid%nx,fg_grid%ny,fg_grid%nz))
       allocate(fg_grid%psfc(fg_grid%nx,fg_grid%ny))
       allocate(fg_grid%ak(fg_grid%nz+1))
       allocate(fg_grid%bk(fg_grid%nz+1))
       allocate(fg_grid%ck(fg_grid%nz+1))
       fg_grid%is_allocated=.true.
    endif

  end subroutine fv3_grid_allocate

  !=======================================================================

  subroutine fv3_grid_deallocate(an_grid,fg_grid)

    type(analysis_grid) :: an_grid !! analysis grid
    type(analysis_grid) :: fg_grid !! first guess grid

    if(an_grid%is_allocated) then
       deallocate(an_grid%lon)
       deallocate(an_grid%lat)
       deallocate(an_grid%lev)
       deallocate(an_grid%ilev)
       deallocate(an_grid%pfull)
       deallocate(an_grid%hyai)
       deallocate(an_grid%hybi)

       deallocate(an_grid%var3d)
       deallocate(an_grid%psfc)
       deallocate(an_grid%ak)
       deallocate(an_grid%bk)
       deallocate(an_grid%ck)
       an_grid%is_allocated=.false.
    endif

    if(fg_grid%is_allocated) then
       deallocate(fg_grid%var3d)
       deallocate(fg_grid%psfc)
       deallocate(fg_grid%ak)
       deallocate(fg_grid%bk)
       deallocate(fg_grid%ck)
       an_grid%is_allocated=.false.
    endif

  end subroutine fv3_grid_deallocate

  !=======================================================================

end module fv3_interface
