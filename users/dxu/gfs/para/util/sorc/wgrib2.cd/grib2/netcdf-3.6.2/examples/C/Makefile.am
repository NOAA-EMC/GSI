# This is a automake file, part of Unidata's netCDF package.
# Copyright 2006, see the COPYRIGHT file for more information.

# This file builds the C examples.

# $Id: Makefile.am,v 1.6 2006/07/27 01:49:08 ed Exp $

LDADD = -lm
AM_CPPFLAGS = -I$(top_srcdir) 

# To build netcdf-4, or not to build netcdf-4, that is the question...
if USE_NETCDF4

# Link to the netCDF-4 and the HDF5 libraries.
LDADD += ../../libsrc4/libnetcdf.la -lhdf5_hl -lhdf5

# Point pre-processor and linker to netcdf-4 directory (libsrc4).
AM_CPPFLAGS += -I$(top_srcdir)/libsrc4
AM_LDFLAGS = -L../../libsrc4

# If the user specified a root for HDF5, use it.
if USE_HDF5_DIR
AM_CPPFLAGS += -I@HDF5DIR@/include
AM_LDFLAGS += -L@HDF5DIR@/lib
endif

else # not USE_NETCDF4

# Link to the netCDF-3 library.
LDADD += ../../libsrc/libnetcdf.la

# Point pre-processor and linker to netcdf-3 directory (libsrc).
AM_CPPFLAGS += -I$(top_srcdir)/libsrc
AM_LDFLAGS = -L../../libsrc

endif #USE_NETCDF4

# In this case, the test program is all there is in this directory!
TESTPROGRAMS = simple_xy_wr simple_xy_rd sfc_pres_temp_wr	\
sfc_pres_temp_rd pres_temp_4D_wr pres_temp_4D_rd
check_PROGRAMS = $(TESTPROGRAMS)
TESTS = $(TESTPROGRAMS)

# These files are created by the tests.
CLEANFILES = sfc_pres_temp.nc simple_xy.nc pres_temp_4D.nc


