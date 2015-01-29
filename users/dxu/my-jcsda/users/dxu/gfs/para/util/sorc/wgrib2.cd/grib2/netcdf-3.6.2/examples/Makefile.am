## This is a automake file, part of Unidata's netCDF package.
# Copyright 2005, see the COPYRIGHT file for more information.

# This is the main automake file for netCDF. It builds the different
# netcdf directories. Not all directories are built, depending on the
# options selected during configure.

# $Id: Makefile.am,v 1.8 2007/01/16 03:02:38 ed Exp $

# If the user is building the F77 API, build the F77 examples.
if BUILD_F77
F77_EXAMPLES = F77
endif

# If the user is building the F90 API, build the F90 examples.
if BUILD_F90
F90_EXAMPLES = F90
endif

# If the user is building the C++ API, build the CXX examples.
if BUILD_CXX
CXX_EXAMPLES = CXX
endif

if BUILD_UTILITIES
CDL_EXAMPLES = CDL
endif

# These are the subdirectories that will be built. C and CDL are
# always built, and the others depending on whether their API is
# built.
SUBDIRS = C $(CDL_EXAMPLES) $(F77_EXAMPLES) $(F90_EXAMPLES) $(CXX_EXAMPLES)