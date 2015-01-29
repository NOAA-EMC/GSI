## This is an automake file, part of Unidata's netCDF package.
# Copyright 2005, see the COPYRIGHT file for more information.

# THis automake file is in charge of building the libsrc directory.

# $Id: Makefile.am,v 2.47 2007/03/01 19:46:09 ed Exp $

# The C API man page.
man_MANS = netcdf.3 

# This rule generates the C manpage.
netcdf.3: $(top_srcdir)/man/netcdf.m4 
	m4 $(M4FLAGS) -DAPI=C $? >$@  || rm $@

# These files are part of the netCDF-3 library.
NC3_SOURCES = attr.m4 ncx.m4 putget.m4 dim.c error.c libvers.c nc.c	\
string.c v1hpg.c var.c fbits.h netcdf3.h nc.h ncio.h ncx.h netcdf.h	\
onstack.h rnd.h

# Does the user want to use ffio or posixio?
if USE_FFIO
NC3_SOURCES += ffio.c
else
NC3_SOURCES += posixio.c
endif

# We may have to add to these later.
libnetcdf_la_LIBADD = 
noinst_LTLIBRARIES = 
AM_CPPFLAGS =
libnetcdf_la_LDFLAGS =

# Does  the user want the V2 API?
if BUILD_V2

# This will create a convenience library for all the netcdf-2
# functions.
noinst_LTLIBRARIES += libnetcdf2.la 

# This is the v2 API source code.
libnetcdf2_la_SOURCES = v2i.c

endif # BUILD_V2

# If the user enabled netcdf-4 in the configure, then the netcdf-3
# library will only be built as a non-installing library.
if USE_NETCDF4

# If the user specified a root for HDF5, use it.
if USE_HDF5_DIR
AM_CPPFLAGS += -I@HDF5DIR@/include
AM_LDFLAGS = -L@HDF5DIR@/lib
endif

# This will create a convenience library for all the netcdf-3
# functions.
noinst_LTLIBRARIES += libnetcdf3.la

# This is a list of the code netcdf-3 library.
libnetcdf3_la_SOURCES = $(NC3_SOURCES) nc3convert.h

# Tell the test program to link to netcdf 3 library.
LDADD = libnetcdf3.la

else # not USE_NETCDF4

# This will cause the netcdf-3 header file to be installed.
include_HEADERS = netcdf.h

# This will cause the netcdf-3 library to be built.
lib_LTLIBRARIES = libnetcdf.la

# This is a list of the code netcdf-3 library.
libnetcdf_la_SOURCES = $(NC3_SOURCES)

# Include netcdf-2 convenience library.
if BUILD_V2
libnetcdf_la_LIBADD += libnetcdf2.la
endif

# If we are not building separate fortran libraries, then include
# their functions in the C library.
if !BUILD_SEPARATE_FORTRAN

# If F77 is built, include its convenience library. If F90 is built,
# it will already be part of the F77 convenience library.
if BUILD_F77
libnetcdf_la_LIBADD += ../fortran/libnetcdff.la
endif

endif # !BUILD_SEPARATE_FORTRAN

# Verson for the netcdf-3 library. Don't mess with this number
# lightly!! This is not the same as the netCDF version, it is a
# completely different versioning system for shared library binary
# files, which is used in a specific way by OSs which are using shared
# libraries. Do don't dork around with it or you will break netCDF for
# shared library users, and everyone will be very confused. We are
# starting with 4 because some package distributors (and other users)
# have already build netCDF with shared libraries, and we need to have
# a larger number than any they used.
libnetcdf_la_LDFLAGS += $(AM_LDFLAGS) -version-info 4:0:0 -no-undefined

# If building the DLL on mingw, do some extra stuff. For now, this
# works with netCDF-3 only. (Ed 12/3/6)
if BUILD_DLL

# We need extra arguments to produce the def file, which is needed by
# MS tools to create the VC++ import library. Add -avoid-version to
# avoid the version number in the .dll file name. 
libnetcdf_la_LDFLAGS += -Wl,--output-def,.libs/libnetcdf.def 
AM_CPPFLAGS += -DNC_DLL_EXPORT
endif # BUILD_DLL

# Compile in the library when building the test.
LDADD = ${lib_LTLIBRARIES}

endif # USE_NETCDF4

# Test the netCDF-3 library.
TESTPROGRAMS = t_nc
check_PROGRAMS = $(TESTPROGRAMS)
TESTS = $(TESTPROGRAMS)

# If building a DLL, the test program needs to define DLL_NETCDF.
if BUILD_DLL
t_nc_CPPFLAGS = -DDLL_NETCDF
endif

# These files are cleaned on developer workstations (and then rebuilt
# with m4), but they are included in the distribution so that the user
# does not have to have m4.
MAINTAINERCLEANFILES = attr.c ncx.c putget.c Makefile.in $(man_MANS)
EXTRA_DIST = attr.c ncx.c putget.c $(man_MANS)

# This file is created by the test.
CLEANFILES = test.nc

# This tells make how to turn .m4 files into .c files.
.m4.c:
	m4 $(AM_M4FLAGS) $(M4FLAGS) $< >$@

test: check
