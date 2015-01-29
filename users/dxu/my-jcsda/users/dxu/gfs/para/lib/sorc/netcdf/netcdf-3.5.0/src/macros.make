# $Id: macros.make.in,v 1.32 2001/03/13 17:41:33 steve Exp $

# The purpose of this file is to contain common make(1) macros.
# It should be processed by every execution of that utility.


# POSIX shell.  Shouldn't be necessary -- but is under IRIX 5.3.
SHELL		= /bin/sh


# Installation Directories:
SRCDIR		= /global/save/wx23sm/GFS/f2010/trunk/para/lib/sorc/netcdf/netcdf-3.5.0/src
prefix		= /global/save/wx23sm/GFS/f2010/trunk/para/lib/sorc/netcdf/netcdf-3.5.0
exec_prefix	= $(prefix)
INCDIR		= $(exec_prefix)/include
LIBDIR		= $(exec_prefix)/lib
BINDIR		= $(exec_prefix)/bin
MANDIR		= $(prefix)/man


# Preprocessing:
M4		= m4
M4FLAGS		= -B10000
CPP		= /usr/bin/ncepxlc -E
CPPFLAGS	= $(INCLUDES) $(DEFINES) -DNDEBUG
FPP		= 
FPPFLAGS	= 
CXXCPPFLAGS	= $(CPPFLAGS)


# Compilation:
CC		= xlc_r
CXX		= /usr/bin/ncepxlC
FC		= xlf_r
F90		= xlf90_r
CFLAGS		= -O
CXXFLAGS	= -O
FFLAGS		= -O
F90FLAGS	= -qsuffix=f=f90
NETCDF.MOD	= netcdf.mod
CC_MAKEDEPEND	= false
COMPILE.c	= $(CC) -c $(CFLAGS) $(CPPFLAGS)
COMPILE.cxx	= $(CXX) -c $(CXXFLAGS) $(CXXCPPFLAGS)
COMPILE.f	= $(FC) -c $(FFLAGS)
COMPILE.F90	= $(F90) -c $(F90FLAGS)
# The following command isn't available on some systems; therefore, the
# `.F.o' rule is relatively complicated.
COMPILE.F	= $(COMPILE.f) $(FPPFLAGS)


# Linking:
MATHLIB		= -lm
FLIBS		= 
F90LIBS		= 
LIBS		= 
F90LDFLAGS	= $(LDFLAGS)
LINK.c		= $(CC) -o $@ $(CFLAGS) $(LDFLAGS)
LINK.cxx	= $(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS)
LINK.F		= $(FC) -o $@ $(FFLAGS) $(FLDFLAGS)
LINK.f		= $(FC) -o $@ $(FFLAGS) $(FLDFLAGS)
LINK.F90	= $(F90) -o $@ $(F90FLAGS) $(F90LDFLAGS)


# Manual pages:
WHATIS		= whatis
# The following macro should be empty on systems that don't
# allow users to create their own manual-page indexes.
MAKEWHATIS_CMD	= catman -w -M $(MANDIR)


# Misc. Utilities:
AR		= ar
ARFLAGS		= -X 64 cru
RANLIB		= ranlib
TARFLAGS	= -chf


# Dummy macros: used only as placeholders to silence GNU make.  They are
# redefined, as necessary, in subdirectory makefiles.
HEADER		= dummy_header
HEADER1		= dummy_header1
HEADER2		= dummy_header2
HEADER3		= dummy_header3
LIBRARY		= dummy_library.a
MANUAL		= dummy_manual
PROGRAM		= dummy_program


# Distribution macros:
FTPDIR		= /home/ftp/pub/$(PACKAGE)
FTPBINDIR	= /home/ftp/pub/binary/dummy_system
VERSION		= dummy_version
