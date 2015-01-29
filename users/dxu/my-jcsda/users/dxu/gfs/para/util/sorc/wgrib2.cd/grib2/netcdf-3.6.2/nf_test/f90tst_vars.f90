!     This is part of the netCDF package.
!     Copyright 2006 University Corporation for Atmospheric Research/Unidata.
!     See COPYRIGHT file for conditions of use.

!     This program tests netCDF-4 variable functions from fortran.

!     $Id: f90tst_vars.f90,v 1.3 2006/11/01 11:06:11 ed Exp $

program f90tst_vars
  use typeSizes
  use netcdf
  implicit none
  
  ! This is the name of the data file we will create.
  character (len = *), parameter :: FILE_NAME = "f90tst_vars.nc"

  ! We are writing 2D data, a 6 x 12 grid. 
  integer, parameter :: NDIMS = 2
  integer, parameter :: NX = 6, NY = 12
  integer :: ncid, varid, dimids(NDIMS)
  integer :: x_dimid, y_dimid
  integer :: data_out(NY, NX), data_in(NY, NX)
  integer :: x, y, retval

  print *,'*** Testing definition of netCDF-4 vars from Fortran 90.'

  ! Create some pretend data.
  do x = 1, NX
     do y = 1, NY
        data_out(y, x) = (x - 1) * NY + (y - 1)
     end do
  end do

  ! Create the netCDF file. 
  retval = nf90_create(FILE_NAME, nf90_hdf5, ncid)
  if (retval /= nf90_noerr) call handle_err(retval)

  ! Define the dimensions.
  retval = nf90_def_dim(ncid, "x", NX, x_dimid)
  if (retval /= nf90_noerr) call handle_err(retval)
  retval = nf90_def_dim(ncid, "y", NY, y_dimid)
  if (retval /= nf90_noerr) call handle_err(retval)
  dimids =  (/ y_dimid, x_dimid /)

  ! Define the variable. 
  retval = nf90_def_var(ncid, "data", NF90_INT, dimids, varid)
  if (retval /= nf90_noerr) call handle_err(retval)

  retval = nf90_enddef(ncid)
  if (retval /= nf90_noerr) call handle_err(retval)

  ! Write the pretend data to the file.
  retval = nf90_put_var(ncid, varid, data_out)
  if (retval /= nf90_noerr) call handle_err(retval)

  ! Close the file. 
  retval = nf90_close(ncid)
  if (retval /= nf90_noerr) call handle_err(retval)
  
  print *,'*** SUCCESS!'

contains
!     This subroutine handles errors by printing an error message and
!     exiting with a non-zero status.
  subroutine handle_err(errcode)
    use netcdf
    implicit none
    integer, intent(in) :: errcode
    
    if(errcode /= nf90_noerr) then
       print *, 'Error: ', trim(nf90_strerror(errcode))
       stop "Stopped"
    endif
  end subroutine handle_err
end program f90tst_vars

