C     This is part of the netCDF package.
C     Copyright 2006 University Corporation for Atmospheric Research/Unidata.
C     See COPYRIGHT file for conditions of use.

C     This program tests netCDF-4 variable functions from fortran.

C     $Id: ftst_vars.F,v 1.5 2006/09/26 12:31:45 ed Exp $

      program ftst_vars
      implicit none
      include '../fortran/netcdf4.inc'

C     This is the name of the data file we will create.
      character*(*) FILE_NAME
      parameter (FILE_NAME='ftst_vars.nc')

C     We are writing 2D data, a 6 x 12 grid. 
      integer NDIMS
      parameter (NDIMS=2)
      integer NX, NY
      parameter (NX = 6, NY = 12)

C     NetCDF IDs.
      integer ncid, varid, dimids(NDIMS)
      integer x_dimid, y_dimid

C     This is the data array we will write, and a place to store it when
C     we read it back in.
      integer data_out(NY, NX), data_in(NY, NX)

C     For checking our data file to make sure it's correct.
      integer shuffle, deflate, deflate_level, fletcher32

C     Loop indexes, and error handling.
      integer x, y, retval

C     Create some pretend data.
      do x = 1, NX
         do y = 1, NY
            data_out(y, x) = (x - 1) * NY + (y - 1)
         end do
      end do

      print *,'*** Testing definition of netCDF-4 vars from Fortran 77.'
C      retval = nf_set_log_level(3)

C     Create the netCDF file.
      retval = nf_create(FILE_NAME, NF_NETCDF4, ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Define the dimensions.
      retval = nf_def_dim(ncid, "x", NX, x_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_dim(ncid, "y", NY, y_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Define the variable. 
      dimids(2) = x_dimid
      dimids(1) = y_dimid
      retval = nf_def_var(ncid, "data", NF_INT, NDIMS, dimids, varid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Turn on deflate, fletcher32.
      retval = nf_def_var_deflate(ncid, varid, 0, 1, 4)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_var_fletcher32(ncid, varid, 1)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Is everything set that is supposed to be?
      retval = nf_inq_var_deflate(ncid, varid, shuffle, deflate, 
     +     deflate_level)
      if (retval .ne. nf_noerr) call handle_err(retval)
      if (shuffle .ne. 0 .or. deflate .ne. 1 .or. 
     +     deflate_level .ne. 4) stop 2
      retval = nf_inq_var_fletcher32(ncid, varid, fletcher32)
      if (retval .ne. nf_noerr) call handle_err(retval)
      if (fletcher32 .ne. 1) stop 2

C     Write the pretend data to the file.
      retval = nf_put_var_int(ncid, varid, data_out)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Close the file. 
      retval = nf_close(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Reopen the file and check again.
      retval = nf_open(FILE_NAME, NF_NOWRITE, ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Find our variable.
      retval = nf_inq_varid(ncid, "data", varid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      if (varid .ne. 1) stop 2

C     Is the deflate set?
      retval = nf_inq_var_deflate(ncid, varid, shuffle, deflate, 
     +     deflate_level)
      if (retval .ne. nf_noerr) call handle_err(retval)
      if (shuffle .ne. 0 .or. deflate .ne. 1 .or. 
     +     deflate_level .ne. 4) stop 2
      retval = nf_inq_var_fletcher32(ncid, varid, fletcher32)
      if (retval .ne. nf_noerr) call handle_err(retval)
      if (fletcher32 .ne. 1) stop 2

C     Read the data and check it.
      retval = nf_get_var_int(ncid, varid, data_in)
      if (retval .ne. nf_noerr) call handle_err(retval)
      do x = 1, NX
         do y = 1, NY
            if (data_in(y, x) .ne. data_out(y, x)) stop 2
         end do
      end do

C     Close the file. 
      retval = nf_close(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

      print *,'*** SUCCESS!'
      end
