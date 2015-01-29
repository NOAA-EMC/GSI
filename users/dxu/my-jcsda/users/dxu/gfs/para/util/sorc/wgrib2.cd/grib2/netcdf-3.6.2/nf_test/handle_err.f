C     This is part of the netCDF package.
C     Copyright 2005 University Corporation for Atmospheric Research/Unidata.
C     See COPYRIGHT file for conditions of use.

C     This is the error handling function for all the F77 examples.

C     This is part of the netCDF tutorial, which can be found at:
C     http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial
      
C     $Id: handle_err.f,v 1.1 2006/09/21 10:50:39 ed Exp $

C     This subroutine handles errors by printing an error message and
C     exiting with a non-zero status.
      subroutine handle_err(errcode)
      implicit none
      include '../fortran/netcdf.inc'
      integer errcode

      print *, 'Error: ', nf_strerror(errcode)
      stop 2
      end

