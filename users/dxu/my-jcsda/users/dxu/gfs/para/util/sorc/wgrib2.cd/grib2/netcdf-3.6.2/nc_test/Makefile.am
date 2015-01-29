## This is a automake file, part of Unidata's netCDF package.
# Copyright 2005, see the COPYRIGHT file for more information.

# This file builds and runs the nc_test program, which tests the
# netCDF-3 API for all formats.

# $Id: Makefile.am,v 1.35 2007/03/01 19:46:08 ed Exp $

LDADD = -lm
AM_CPPFLAGS = -I$(top_srcdir) 

# These files are created by the tests.
CLEANFILES = nc_test_classic.nc nc_test_64bit.nc nc_test_netcdf4.nc	\
tst_small_classic.nc tst_small_64bit.nc tst_small_netcdf4_classic.nc	\
tst_small_netcdf4.nc

# These are the tests which are always run.
TESTPROGRAMS = tst_small nc_test 

# These are the source files for the main workhorse test program,
# nc_test. If you pass nc_test, you are doing well.
nc_test_SOURCES = nc_test.c error.c test_get.m4 test_put.m4 \
test_read.c test_write.c util.c error.h tests.h 

# This is an aptly named test program.
tst_small_SOURCES = tst_small.c nc_tests.h

# Does the user want to build netCDF-4?
if USE_NETCDF4

# Link to the netCDF-4 and the HDF5 libraries.
LDADD += ../libsrc4/libnetcdf.la -lhdf5_hl -lhdf5

# Point pre-processor and linker to netcdf-4 directory (libsrc4).
AM_CPPFLAGS += -I$(top_srcdir)/libsrc4
AM_LDFLAGS = -L../libsrc4

# If the user specified a root for HDF5, use it.
if USE_HDF5_DIR
AM_CPPFLAGS += -I@HDF5DIR@/include
AM_LDFLAGS += -L@HDF5DIR@/lib
endif

else # not USE_NETCDF4

# Link to the netCDF-3 library.
LDADD += ../libsrc/libnetcdf.la

# Point pre-processor and linker to netcdf-3 directory (libsrc).
AM_CPPFLAGS += -I$(top_srcdir)/libsrc
AM_LDFLAGS = -L../libsrc

endif #USE_NETCDF4

# If the user asked for large file tests, then add them.
if LARGE_FILE_TESTS
TESTPROGRAMS += quick_large_files large_files
quick_large_files_SOURCES = quick_large_files.c nc_tests.h
large_files_SOURCES = large_files.c nc_tests.h
CLEANFILES += large_files.nc quick_large_files.nc
endif # LARGE_FILE_TESTS

# Set up the tests.
check_PROGRAMS = $(TESTPROGRAMS)
TESTS = $(TESTPROGRAMS)

# Distribute the .c files so that m4 isn't required on the users
# machine.
EXTRA_DIST = test_get.c test_put.c

# Only clean these on mainatiner-clean, because they require m4 and
# automake to regenerate.
MAINTAINERCLEANFILES = test_get.c test_put.c Makefile.in

# This rule tells make how to turn our .m4 files into .c files.
.m4.c:
	m4 $(AM_M4FLAGS) $(M4FLAGS) $< >$@

test: check
