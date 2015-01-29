## This is a automake file, part of Unidata's netCDF package.
# Copyright 2005, see the COPYRIGHT file for more information.
# This file builds and runs the ncdump program.

# $Id: Makefile.am,v 1.44 2007/03/01 19:46:07 ed Exp $

LDADD = -lm

CLEANFILES = test0.nc tst_small.nc tst_small.cdl test1.cdl test1.nc	\
test2.cdl ctest1.cdl ctest.c ctest64.c ctest0.nc ctest0_64.nc c1.cdl	\
ctest1_64.cdl c0.nc tst_solar_1.cdl small.nc small2.nc c0tmp.nc

# These tests are run for both netCDF-4 and non-neCDF-4 builds.
TESTPROGRAMS = rewrite-scalar ctest ctest64 ncdump
check_PROGRAMS = $(TESTPROGRAMS)
TESTS = run_tests.sh tst_64bit.sh ctest ctest64 tst_output.sh	\
tst_lengths.sh 

# Is the user building netCDF-4?
if USE_NETCDF4

# Link to netCDF-4 and hdf libs.
LDADD += ../libsrc4/libnetcdf.la -lhdf5_hl -lhdf5

# Find netcdf.h in libsrc4.
AM_CPPFLAGS = -I../libsrc4 -I$(top_srcdir)/libsrc4

# If the user specified a root for HDF5, use it.
if USE_HDF5_DIR
AM_CPPFLAGS += -I@HDF5DIR@/include
AM_LDFLAGS = -L@HDF5DIR@/lib
endif

# NetCDF-4 has some extra tests.
check_PROGRAMS += tst_create_files 
TESTS += tst_create_files tst_netcdf4.sh 
CLEANFILES += tst_solar_1.nc tst_solar_2.nc

else # not USE_NETCDF4

# Find netcdf.h in libsrc.
AM_CPPFLAGS = -I../libsrc -I$(top_srcdir)/libsrc

# Link to netcdf-3 library.
LDADD +=  ../libsrc/libnetcdf.la

endif # USE_NETCDF4

# This is the program we're building, and it's sources.
bin_PROGRAMS = ncdump
ncdump_SOURCES = ncdump.c vardata.c dumplib.c ncdump.h vardata.h	\
dumplib.h

# This is the man page.
man_MANS = ncdump.1

# These files all have to be included with the distribution.
EXTRA_DIST = run_tests.sh tst_64bit.sh tst_output.sh test0.cdl		\
ref_ctest1_nc4.cdl ref_ctest1_nc4c.cdl ref_tst_solar_1.cdl		\
ref_tst_solar_2.cdl tst_netcdf4.sh ref_tst_small.cdl tst_lengths.sh	\
small.cdl small2.cdl $(man_MANS)

ctest.c:
	../ncgen/ncgen -c -o ctest0.nc $(srcdir)/../ncgen/c0.cdl > $(srcdir)/ctest.c

ctest64.c:
	../ncgen/ncgen -v2 -c -o ctest0_64.nc $(srcdir)/../ncgen/c0.cdl > $(srcdir)/ctest64.c

MAINTAINERCLEANFILES = Makefile.in 

test: check
