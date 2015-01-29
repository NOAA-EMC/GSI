#                                               -*- Autoconf -*-
## Process this file with autoconf to produce a configure script.

# This is part of Unidata's netCDF package. Copyright 2005-2006, see
# the COPYRIGHT file for more information.

# Recall that ${VAR-exp} expands to $VAR if var is set (even to null),
# and to exp otherwise. 

## This puts the cvs ID tag in the output configure script.
AC_REVISION([$Id: configure.ac,v 1.177 2007/03/01 22:44:10 ed Exp $])

# Running autoconf on this file will trigger a warning if 
# autoconf is not at least the specified version.
AC_PREREQ([2.59])

# Initialize with name, version, and support email address. 
AC_INIT([netCDF], [3.6.2], [support@unidata.ucar.edu])

# Create the VERSION file, which contains the package version from
# AC_INIT.
echo -n AC_PACKAGE_VERSION>VERSION
AC_SUBST(PACKAGE_VERSION)

AC_MSG_NOTICE([netCDF AC_PACKAGE_VERSION])

# Find out about the host we're building on.
AC_CANONICAL_HOST

# We will output a config.h for the C library, and a fortran inc file,
# nfconfig.inc.
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_HEADERS([fortran/nfconfig1.inc:fortran/nfconfig.in], 
   [sed '1d;s%^/\* \(.*\) \*/%\1%' fortran/nfconfig1.inc >conftest.tmp && mv conftest.tmp fortran/nfconfig.inc])

# This call is required by automake.
AM_INIT_AUTOMAKE([foreign dist-bzip2 dist-zip dist-tarZ subdir-objects -Wall])

# Check for the existance of this file before proceeding.
AC_CONFIG_SRCDIR([libsrc/nc.c])

AC_MSG_NOTICE([checking user options])

# Turn off shared libraries by default
AM_DISABLE_SHARED

# For backward compatibility, check to see if the user has set CXX,
# FC, and/or F90 to null. If so, treat that as the equivilent
# --disable- option to configure. The autoconf manual suggests the
# following to set a var if it's not already set:
# test "${var+set}" = set || var={value}

# Did the user set cxx to null?
AC_MSG_CHECKING([whether CXX is set to ''])
nc_null_cxx=no
test "x${CXX+set}" = xset && test "x$CXX" = x && nc_null_cxx=yes
AC_MSG_RESULT([$nc_null_cxx])

# Did the user set fc to null?
AC_MSG_CHECKING([whether FC is set to ''])
nc_null_fc=no
test "x${FC+set}" = xset && test "x$FC" = x && nc_null_fc=yes
AC_MSG_RESULT([$nc_null_fc])

# Did the user set f90 to null?
AC_MSG_CHECKING([whether F90 is set to ''])
nc_null_f90=no
test "x${F90+set}" = xset && test "x$F90" = x && nc_null_f90=yes
AC_MSG_RESULT([$nc_null_f90])

# We are NOT building netCDF-4 in this version.
AM_CONDITIONAL(USE_NETCDF4, [test x$enable_netcdf_4 = xyes])
AM_CONDITIONAL(USE_HDF5_DIR, [test ! "x$HDF5DIR" = x])

# Does the user want to turn off install of documentation?
AC_MSG_CHECKING([whether documentation is to be installed])
AC_ARG_ENABLE([docs-install],
              [AS_HELP_STRING([--enable-docs-install],
                              [cause the netCDF documentation to be installed])])
test "x$enable_docs_install" = xyes || enable_docs_install=no
AC_MSG_RESULT($enable_docs_install)
AM_CONDITIONAL(INSTALL_DOCS, [test x$enable_docs_install = xyes])

# Does the user want to run failing tests?
dnl AC_MSG_CHECKING([whether failure modes should be tested])
dnl AC_ARG_ENABLE([failure-tests],
dnl               [AS_HELP_STRING([--enable-failure-tests],
dnl                               [cause the some extra tests to run, which are \
dnl 			      intended to fail (this confuses some old make \
dnl 			      versions, so is not done by default)])])
dnl test "x$enable_failure_tests" = xyes || enable_failure_tests=no
dnl AC_MSG_RESULT($enable_failure_tests)
dnl AM_CONDITIONAL(USE_FAILURE_TESTS, [test x$enable_failure_tests = xyes])

# Does the user want to test f90 compiler.
AC_MSG_CHECKING([whether Fortran compiler(s) should be tested during configure])
AC_ARG_ENABLE([fortran-compiler-check],
              [AS_HELP_STRING([--disable-fortran-compiler-check],
                              [disable check of F77/F90 compiler during configure])])
test "x$enable_fortran_compiler_check" = xno || enable_fortran_compiler_check=yes
AC_MSG_RESULT($enable_fortran_compiler_check)

# Does the user want configure to keep hands off any compiler or
# linker settings?
AC_MSG_CHECKING([whether configure should try to set compiler flags])
AC_ARG_ENABLE([flag-setting],
              [AS_HELP_STRING([--disable-flag-setting],
                              [turn off configure's attempts to correctly \
			      set compiler and linker flags])])
test "x$enable_flag_setting" = xno || enable_flag_setting=yes
AC_MSG_RESULT($enable_flag_setting)

# Does the user want to use the ffio module?
AC_MSG_CHECKING([whether FFIO will be used])
AC_ARG_ENABLE([ffio],
              [AS_HELP_STRING([--enable-ffio],
                              [use ffio instead of posixio (ex. on the Cray)])])
test "x$enable_ffio" = xyes || enable_ffio=no
AC_MSG_RESULT($enable_ffio)
AM_CONDITIONAL(USE_FFIO, [test x$enable_ffio = xyes])

# Do we want to recover from a bad C++, F77, or F90 compiler?
nc_compiler_recover=yes
AC_MSG_CHECKING([whether to skip C++, F77, or F90 APIs if compiler is broken])
AC_ARG_ENABLE([compiler-recover],
              [AS_HELP_STRING([--disable-compiler-recover],
                              [don't continue if a desired C++, F77, or F90 \
			      compiler is missing; this is useful for testing \
			      netCDF, but is not very useful for most netCDF \
			      users])])
test "x$enable_compiler_recover" = "xno" || enable_compiler_recover=yes
AC_MSG_RESULT([$enable_compiler_recover])

# If --enable-c-only is specified only the C lib will be built.
AC_MSG_CHECKING([whether only the C library is desired])
AC_ARG_ENABLE([c-only],
              [AS_HELP_STRING([--enable-c-only],
                              [build only the netCDF-3 C library; same as \
			      --disable-f77 --disable-cxx --disable-v2 --disable-examples \
			      --disable-utilities])])
test "x$enable_c_only" = xyes || enable_c_only=no
AC_MSG_RESULT([$enable_c_only])
if test "x$enable_c_only" = xyes; then
nc_build_f77=no
nc_build_f90=no
nc_build_cxx=no
nc_build_v2=no
nc_build_utilities=no
nc_build_examples=no
else
nc_build_f77=yes
nc_build_f90=yes
nc_build_cxx=yes
nc_build_v2=yes
nc_build_utilities=yes
nc_build_examples=yes
fi

# Does the user want to build examples?
AC_MSG_CHECKING([whether examples should be built])
AC_ARG_ENABLE([examples],
              [AS_HELP_STRING([--disable-examples],
                              [don't build the netCDF examples during make check \
			      (examples are treated as extra tests by netCDF)])])
test "x$enable_examples" = xno && nc_build_examples=no
AC_MSG_RESULT($nc_build_examples)
AM_CONDITIONAL(BUILD_EXAMPLES, [test x$nc_build_examples = xyes])

# Do we want to build the fortran 77 API? Check for --disable-f77.
AC_MSG_CHECKING([whether F77 API is desired])
AC_ARG_ENABLE([f77],
              [AS_HELP_STRING([--disable-f77],
                              [don't try to build the netCDF Fortran 77 (or \	
			      Fortran 90) API])])
test "x$enable_f77" = "xno" && nc_build_f77=no
test "x$nc_null_fc" = xyes && nc_build_f77=no
AC_MSG_RESULT([$nc_build_f77])

# Do we want to build the fortran 90 API?
nc_build_f90=yes
AC_MSG_CHECKING([whether F90 API is desired])
AC_ARG_ENABLE([f90],
              [AS_HELP_STRING([--disable-f90],
                              [don't try to build the netCDF Fortran 90 API])])
test "x$enable_f90" = xno && nc_build_f90=no
test "x$nc_null_fc" = xyes || test "x$nc_null_f90" = xyes || \
     test "x$nc_build_f77" = xno && nc_build_f90=no
AC_MSG_RESULT([$nc_build_f90])

# Does the user want to check into fortran type information?
AC_MSG_CHECKING([whether fortran type sizes should be checked])
AC_ARG_ENABLE([fortran-type-check],
              [AS_HELP_STRING([--disable-fortran-type-check],
                              [cause the Fortran type sizes checks to be skipped])])
test "x$enable_fortran_type_check" = xno || enable_fortran_type_check=yes
AC_MSG_RESULT($enable_fortran_type_check)

# Do we want to build the C API? If not, the location of an existing C
# library must be specified with --with-netcdf=
nc_build_c=yes
AC_MSG_CHECKING([whether C API is desired])
AC_ARG_ENABLE([c],
              [AS_HELP_STRING([--disable-c],
                              [don't try to build the netCDF C API (if this option \
			       is specified, the --with-netcdf-c-lib option must \
			       also be used)])])
test "x$enable_c" = xno && nc_build_c=no
AC_MSG_RESULT([$nc_build_c])

# If building without the C API (for example to build some Fortran API
# for a new Fortran compiler), the --with-netcdf-c-lib= option must be
# used to tell where to find the netCDF C API. This must point to a
# netCDF C library which does NOT contain the F77 or F90 functions.
AC_MSG_CHECKING([where to get netCDF C-only library for separate fortran libraries])
AC_ARG_WITH([netcdf-c-lib],
            [AS_HELP_STRING([--netcdf-c-lib=<directory>],
                            [Specify location of netCDF C-only library (only \
			    valid when --disable-c is also specified).])],
            [NETCDF_C_LIB=$with_netcdf_c_lib])
AM_CONDITIONAL(USE_NETCDF_C_LIB_DIR, [test ! "x$NETCDF_C_LIB" = x])
AC_MSG_RESULT($NETCDF_C_LIB)
AC_SUBST(NETCDF_C_LIB)

# If the C API is turned off, a C-only library must be specified.
if test "x$nc_build_c" = xno -a "x$NETCDF_C_LIB" = x; then
   AC_MSG_ERROR([If the C API is disabled, you must specify a location of \
   a netCDF C-only library with --with-netcdf-c-lib.])
fi

# Do we want to build the C++ API? Check for --disable-cxx as a
# configure argument.
AC_MSG_CHECKING([whether CXX API is desired])
AC_ARG_ENABLE([cxx],
              [AS_HELP_STRING([--disable-cxx],
                              [don't try to build C++ API])])
test "x$enable_cxx" = xno && nc_build_cxx=no
test "x$nc_null_cxx" = xyes && nc_build_cxx=no
AC_MSG_RESULT([$nc_build_cxx])

# Does the user want to disable the V2 API?
AC_MSG_CHECKING([whether v2 netCDF API should be built])
AC_ARG_ENABLE([v2],
              [AS_HELP_STRING([--disable-v2],
                              [turn off the netCDF version 2 API])])
test "x$enable_v2" = xno && nc_build_v2=no
AC_MSG_RESULT($nc_build_v2)
AM_CONDITIONAL(BUILD_V2, [test x$nc_build_v2 = xyes])
if test "x$nc_build_v2" = xno; then
   AC_DEFINE_UNQUOTED(NO_NETCDF_2, 1, [do not build the netCDF version 2 API])
fi

# Does the user want to disable ncgen/ncdump?
AC_MSG_CHECKING([whether the ncgen/ncdump should be built])
AC_ARG_ENABLE([utilities],
              [AS_HELP_STRING([--disable-utilities],
                              [don't build netCDF utilities ncgen and ncdump])])
test "x$enable_utilities" = xno && nc_build_utilities=no
AC_MSG_RESULT($nc_build_utilities)
AM_CONDITIONAL(BUILD_UTILITIES, [test x$nc_build_utilities = xyes])

# Does the user want to run tests for large files (> 2GiB)?
AC_MSG_CHECKING([whether large file (> 2GB) tests should be run])
AC_ARG_ENABLE([large-file-tests],
              [AS_HELP_STRING([--enable-large-file-tests],
                              [Run tests which create very large data files (~13 GB disk space
			      required, but it will be recovered when tests are complete). See
			      option --with-large-file to specify temporary directory])])
test "x$enable_large_file_tests" = xyes || enable_large_file_tests=no
AC_MSG_RESULT($enable_large_file_tests)
AM_CONDITIONAL(LARGE_FILE_TESTS, [test x$enable_large_file_tests = xyes])

# Does the user want to use extreme numbers in testing.
AC_MSG_CHECKING([whether extreme numbers should be used in tests])
AC_ARG_ENABLE([extreme-numbers],
              [AS_HELP_STRING([--disable-extreme-numbers],
                              [don't use extreme numbers during testing, such as MAX_INT - 1])])
case "$host_cpu $host_os" in
     *386*solaris*)
	test "x$enable_extreme_numbers" = xyes || enable_extreme_numbers=no
	;;
     *)
	test "x$enable_extreme_numbers" = xno || enable_extreme_numbers=yes
	;;
esac     
AC_MSG_RESULT($enable_extreme_numbers)

if test "x$enable_extreme_numbers" = xyes; then
   AC_DEFINE(USE_EXTREME_NUMBERS, 1, [set this to use extreme numbers in tests])
fi

# If the env. variable TEMP_LARGE is set, or if
# --with-temp-large=<directory>, use it as a place for the large
# (i.e. > 2 GiB) files created during the large file testing.
AC_MSG_CHECKING([where to put large temp files if large file tests are run])
AC_ARG_WITH([temp-large],
            [AS_HELP_STRING([--with-temp-large=<directory>],
                            [specify directory where large files (i.e. >2 GB) \
			    will be written, if large files tests are run with
			    --enable-large-file-tests])],
            [TEMP_LARGE=$with_temp_large])
TEMP_LARGE=${TEMP_LARGE-.}
AC_MSG_RESULT($TEMP_LARGE)
#AC_SUBST(TEMP_LARGE)
AC_DEFINE(TEMP_LARGE, $TEMP_LARGE, [Place to put very large netCDF test files.])

# If --enable-dll is specified the DLL will be built. This only works
# on mingw.
AC_MSG_CHECKING([whether a win32 DLL is desired])
AC_ARG_ENABLE([dll],
              [AS_HELP_STRING([--enable-dll],
                              [build a win32 DLL (only works on mingw)])])
test "x$enable_dll" = xyes || enable_dll=no
AC_MSG_RESULT([$enable_dll])
AM_CONDITIONAL(BUILD_DLL, [test x$enable_dll = xyes])
if test "x$enable_dll" = xyes; then
   AC_DEFINE(DLL_NETCDF, 1, [set this only when building a DLL under MinGW])
fi

# If --enable-separate-fortran is selected the F77 and F90 APIs will
# each get their own libraray instead of being crammed in with the C
# functions. If shared libraries are being built, this is turned
# on. (Otherwise C programmers using the shared netcdf library would
# not be able to run C/netCDF programs without the F90 shared
# libraries.)
AC_MSG_CHECKING([whether separate fortran libs are desired])
AC_ARG_ENABLE([separate-fortran],
              [AS_HELP_STRING([--enable-separate-fortran],
                              [build F77 and F90 libraries separate from C library \
			      (turned on automatically when shared libraries are built)])])
test "x$enable_separate_fortran" != xno && test "x$enable_shared" = xyes && enable_separate_fortran=yes
test "x$enable_separate_fortran" = xyes || enable_separate_fortran=no
AC_MSG_RESULT([$enable_separate_fortran])
AM_CONDITIONAL(BUILD_SEPARATE_FORTRAN, [test x$enable_separate_fortran = xyes])

# If we're building shared libraries set an automake conditional, so
# that the top level makefile.am can know what output files to expect
# for the extra checking of output, target check_install.
AM_CONDITIONAL(BUILD_SHARED, [test x$enable_shared = xyes])

# The make check_install target checks to ensure the install went
# OK. But shared libs have different extensions on some
# platforms. This is not important for the end user, but it allows
# netCDF nightly testing to check for correct output on all platforms.
sys_name=`uname`
AM_CONDITIONAL(SHAREEXT_DYLIB, [test x$sys_name = xDarwin])
AM_CONDITIONAL(SHAREEXT_SL, [test x$sys_name = "xHP-UX"])

# According to the autoconf mailing list gurus, we must test for
# compilers unconditionally. That is, we can't skip looking for the
# fortran compilers, just because the user doesn't want fortran. This
# is due to a limitation in autoconf.

# Find the C compiler.
AC_MSG_NOTICE([finding C compiler])

AC_PROG_CC(xlc cc c89 acc gcc)
AM_PROG_CC_C_O
AC_C_CONST

# If the user set environment var FC, but not F77, then set F77=FC
if test "x${FC+set}" = xset -a "x${F77+set}" != xset; then
   F77=${FC}
fi

# If F90 is set, and FC isn't, then set FC to whatever F90 was set to.
if test "${F90+set}" = set -a "${FC+set}" != set; then
   FC=$F90
fi

# If F77 is set, and FC (still) isn't, then set FC to whatever F77 was
# set to.
if test "${F77+set}" = set -a "${FC+set}" != set; then
   FC=$F77
fi

# Find fortran compiler. If FC isn't set after AC_PROG_FC, we couldn't
# find a fortran compiler. If the user requested the fortran API, and
# has told us not to recover from a missing compiler, then bail out
# right here.
AC_MSG_NOTICE([finding Fortran compiler (will not be used if Fortran API is not desired)])

# On solaris, f95 comes before gfortran.
case "$host" in
  *solaris*) 
	     F77S="xlf fort xlf95 ifort ifc efc pgf77 lf95 f95 f90 gfortran f77 frt pgf95 fort77 fl32 af77 f90 xlf90 pgf90 epcf90 g77"
	     F90S="xlf90 fort xlf95 ifort ifc efc pgf95 lf95 f95 f90 gfortran xlf90 pgf90 epcf90"
	     ;;
  *) 
	     F77S="xlf fort xlf95 ifort ifc efc pgf77 lf95 gfortran f77 frt pgf95 f95 f90 fort77 fl32 af77 f90 xlf90 pgf90 epcf90 g77"
	     F90S="xlf90 fort xlf95 ifort ifc efc pgf95 lf95 gfortran f95 f90 f90 xlf90 pgf90 epcf90"
	     ;;
esac

# Find a fortran 90 compiler, if possible.
# MingW has a wierd problem. If arguments are passed to AC_PROG_FC,
# the link of tests won't work (you get a multiply defined atexit
# function.) This is annoying, but there it is.
if test $host = "i686-pc-mingw32"; then
   AC_PROG_FC
else
   AC_PROG_FC($F90S)
fi

# If we couldn't find an F90 compiler, deal with it.
if test "${FC+set}" = set && test "x$FC" = x; then
   if test "x$enable_compiler_recover" = xno && test "x$nc_build_f90" = xyes; then
      AC_MSG_ERROR([Can't find F90 compiler, and compiler recovery disabled.])
   fi
   AC_MSG_WARN([No F90 compiler found. F90 API will not be built.])
   nc_build_f90=no
fi

# We must call AC_PROG_F77 here, otherwise the LIBTOOL macro below
# will call it, and reset F77 to g77.
AC_PROG_F77($F77S)
if test "${F77+set}" = set && test "x$F77" = x; then
   if test "x$enable_compiler_recover" = xno && test "x$nc_build_f77" = xyes; then
      AC_MSG_ERROR([Can't find F77 compiler, and compiler recovery disabled.])
   fi
   AC_MSG_WARN([No F77 compiler found. F77 API will not be built.])
   nc_build_f77=no
fi

if test "x$nc_build_f90" = xno; then
   F90=
   # If F77 is set, and FC isn't, then set FC to whatever F77 was set to.
   FC=$F77
fi

# See if the fortran 90 build is desired. If so, set some stuff
if test "x$nc_build_f90" = xyes; then
   AC_MSG_NOTICE([setting up Fortran 90])
   if ! test "${F90+set}" = set; then 
      F90=$FC 
   fi

   # Some f90 compilers change the case of the mod file names. Others
   # require special flags to be set to dea with .f90 files. Find out
   # about this compiler.
   AC_LANG_PUSH(Fortran)
   AC_FC_SRCEXT(f90)
   AC_LANG_POP(Fortran)
   AX_F90_MODULE_FLAG
   AC_SUBST(MOD_FLAG, ${ax_f90_modflag})

   # Set the FCLIBS flag to help with shared libraries.
#   AC_FC_LIBRARY_LDFLAGS
#   AC_F77_LIBRARY_LDFLAGS	

   # Check the fortran 90 compiler to make sure it can compile a
   # little test file.
   if test "x$enable_fortran_compiler_check" = xyes; then
      AC_MSG_CHECKING([if Fortran compiler can handle Fortran-90])
      cat <<EOF >conftest.f90
      program foo
      REAL, PARAMETER        :: A = 3.141592654
      end program
EOF
      doit='$F90 ${F90FLAGS}  ${F90FLAGS_64} ${FCFLAGS_f90} conftest.f90'
      if AC_TRY_EVAL(doit); then
         nc_ans=yes
      else
         nc_ans=no
         if test "x$nc_build_f90" = xyes && test "x$enable_compiler_recover" = xno; then
            AC_MSG_ERROR([Can't use F90 compiler, F90 API desired, and compiler recovery disabled.])
         fi
         unset F90
         nc_build_f90=no
      fi
      rm -f conftest.*
      AC_MSG_RESULT([$nc_ans])
      if test "x$nc_ans" = xno; then
	 AC_MSG_WARN([No F90 compiler found. F90 API will not be built.])
      fi
   fi
fi

AC_MSG_NOTICE([finding C++ compiler (will not be used if C++ API is not desired)])

# On cygwin, osf, and apple, go to g++ first.
case "$host" in
  *cygwin*|*osf*|*apple*) CCS="g++ cl KCC CC cxx cc++ xlC aCC c++ g++ egcs gcc";;
  *) CCS="cl KCC CC cxx cc++ xlC aCC c++ g++ egcs gcc"
esac

AC_PROG_CXX($CCS)

if test "${CXX+set}" = set && test "x$CXX" = x; then
   if test "x$enable_compiler_recover" = xno && test "x$nc_build_cxx" = xyes; then
      AC_MSG_ERROR([Can't find CXX compiler, and compiler recovery disabled.])
   fi
   AC_MSG_WARN([No CXX compiler found. The C++ API will not be built.])
   nc_build_cxx=no
fi

# Set these conditionals for automake. For some reason, it can't keep
# it's SUBDIR order straight if I use BUILD_F77 to attach the nf_test
# directory, so also define an automake conditional TEST_F77 every
# time BUILD_F77 is defined, and set it to the same value.
AM_CONDITIONAL(BUILD_C, [test "x$nc_build_c" = xyes])
AM_CONDITIONAL(BUILD_F77, [test "x$nc_build_f77" = xyes])
AM_CONDITIONAL(TEST_F77, [test "x$nc_build_f77" = xyes])
AM_CONDITIONAL(BUILD_F90, [test "x$nc_build_f90" = xyes])
AM_CONDITIONAL(BUILD_CXX, [test "x$nc_build_cxx" = xyes])

# Produce a windows DLL. This macro must be before AM_PROG_LIBTOOL.
AC_LIBTOOL_WIN32_DLL

# Use libtool.
AC_PROG_LIBTOOL

AC_MSG_NOTICE([finding other utilities])

AC_PROG_INSTALL

if test "x$nc_build_f77" = xyes; then
   AC_MSG_NOTICE([trying to set fortran flags for this platform])
   # Set fortran flag if the user has not already set it in CPPFLAGS.
   case "$CPPFLAGS" in
    *pgiFortran*|*NAGf90Fortran*|*f2cFortran*|*hpuxFortran*|*apolloFortran*|*sunFortran*|*IBMR2Fortran*|*CRAYFortran*|*PATHSCALE_COMPILER*|*gFortran*|*mipsFortran*|*DECFortran*|*vmsFortran*|*CONVEXFortran*|*PowerStationFortran*|*AbsoftUNIXFortran*|*AbsoftProFortran*|*SXFortran*)
       ;;
      *)
         case "$FC" in
            pgf95|pgf90|pgf77|ifort*|lf90|lf95)
               AC_DEFINE(pgiFortran, [1], [Turned on by netCDF configure.])
               ;;
             gfortran) 
                AC_DEFINE(gFortran, [1], [Turned on by netCDF configure.])
                ;;
             g77) 
                AC_DEFINE(f2cFortran, [1], [Turned on by netCDF configure.])
                ;;
             *)
	        # If it's a gnu compiler, guess f2c.
                if test "x$ac_cv_fc_compiler_gnu" = xyes; then
                   AC_DEFINE(f2cFortran, [1], [Turned on by netCDF configure.])
                fi
                ;;
         esac
   esac
fi 

if test "x$enable_flag_setting" = xyes; then

   AC_MSG_NOTICE([trying to set flags for this platform])
   # If you don't turn on HPUX_SOURCE on HPUX's, Lord Voldemort will
   # aquire a power to pass through the macigal protections of NCAR and
   # into Unidata, where he might damage our funding.
   AC_MSG_CHECKING([if _HPUX_SOURCE should be set])
   hpux_ans=no
   case "$host" in
      *hpux*)
         case "$CC" in
            *c89*) 
      	       	    AC_DEFINE(_HPUX_SOURCE, [1], [Needed by HPUX with c89 compiler.])
		    hpux_ans=yes
         esac
         ;;
   esac
   AC_MSG_RESULT($hpux_ans)

   # AC_SYS_LARGEFILES doesn't seem to work on solaris with the c89
   # compiler, unless the -Xa flag is used, so take care of that...
   AC_MSG_CHECKING(if Solaris -Xa option is needed)
   case "$host_os" in
     *solaris*)
        if test "x$CC" = xc89; then
           case "${CFLAGS}" in
              *-Xa*) ;;
              *) 
	         test "x$enable_cflags_setting" = xyes && CFLAGS="$CFLAGS -Xa" ;;
           esac
        fi
        ;;
   esac
   AC_MSG_RESULT([CFLAGS=$CFLAGS])


   if test "x$nc_build_f90" = xyes; then
      AC_MSG_NOTICE([trying to set fortran 90 flags for this platform])

      AC_MSG_CHECKING(if we need to add to FFLAGS for ifort compiler)
      case "${F90}" in
         *ifort*) 
	    test "x$enable_fflags_setting" = xyes && FFLAGS="$FFLAGS -O2 -mp -recursive"
	    F77=$F90
	    ;;
      esac
      AC_MSG_RESULT([FFLAGS=$FFLAGS])
   fi
fi

# Autoconf seems to like FCFLAGS, but netcdf has used FFLAGS. Let's
# just grab anything that's in FFLAGS and use FCFLAGS.

dnl UD_CONFIG_HEADER(fortran/nfconfig.inc:fortran/nfconfig.in)
dnl AC_DEFINE(_ANSI_C_SOURCE, [], [Defined by configure.ac])
dnl AC_DEFINE(_POSIX_SOURCE, [], [Defined by configure.ac])
dnl AC_DEFINE(_XOPEN_SOURCE, [], [Defined by configure.ac])

# AC_SYS_LARGEFILE has problems on AIX when OBJECT_MODE is 64. 
AC_MSG_CHECKING([if we need to set _LARGE_FILES despite results of previous tests])
if test "$enable_largefile" != no; then
case "$host_os" in
     *aix*)
        if test "$OBJECT_MODE" = 64; then
           AC_DEFINE(_LARGE_FILES, [1], [Turned on by netCDF configure.])
        fi
        case "$CFLAGS" in
             *q64*)
                AC_DEFINE(_LARGE_FILES, [1], [Turned on by netCDF configure.])
                ;;
        esac
        ;;
     *hpux*)
        AC_DEFINE(_FILE_OFFSET_BITS, [64], [Turned on by netCDF configure.])
        ;;
esac
fi
AC_MSG_RESULT(_LARGE_FILES=${_LARGE_FILES})

AC_SYS_LARGEFILE

AC_MSG_NOTICE([displaying some results])

## This next macro just prints some results for debugging
## support issues.
UD_DISPLAY_RESULTS

ac_cv_prog_f90_uppercase_mod=no

# This will test the f77 compiler, as well as check some fortran types.
if test "x$nc_build_f77" = xyes; then

   if test $enable_fortran_type_check = yes; then
      UD_FORTRAN_TYPES
      if test "x$ud_fc_failed" = xyes -a "x$enable_compiler_recover" = xno -a "x$nc_build_f77" = xyes; then
         AC_MSG_ERROR([F77 compiler doesn't work, and compiler recovery disabled.])
      fi
   else
      AC_DEFINE(NCBYTE_T, byte)
      AC_DEFINE(NCSHORT_T, integer*2)
      AC_DEFINE(NF_INT1_T, byte)
      AC_DEFINE(NF_INT2_T, integer*2)
      AC_DEFINE(NF_INT1_IS_C_SIGNED_CHAR, 1, [default])
      AC_DEFINE(NF_INT2_IS_C_SHORT, 1, [default])
      AC_DEFINE(NF_INT_IS_C_INT, 1, [default])
      AC_DEFINE(NF_REAL_IS_C_FLOAT, 1, [default])
      AC_DEFINE(NF_DOUBLEPRECISION_IS_C_DOUBLE, 1, [default])
   fi

   # Test whether F90 module names get capitalized.
   if test "x$nc_build_f90" = xyes; then
      AC_PROG_FC_UPPERCASE_MOD
   fi
fi
AM_CONDITIONAL(UPPER_CASE_MOD, [test "x$ac_cv_prog_f90_uppercase_mod" = xyes])

AC_MSG_NOTICE([checking types, headers, and functions])

AC_CHECK_HEADER(stdlib.h, ,AC_DEFINE([NO_STDLIB_H], [], [no stdlib.h]))
AC_CHECK_HEADER(sys/types.h, ,AC_DEFINE([NO_SYS_TYPES_H], [], [no sys_types.h]))

# Check for these functions...
AC_CHECK_FUNCS([strlcat strerror])
AC_FUNC_ALLOCA
#UD_CHECK_LIB_MATH

AC_STRUCT_ST_BLKSIZE
UD_CHECK_IEEE
AC_TYPE_SIZE_T
AC_TYPE_OFF_T
AC_CHECK_TYPES([ssize_t, ptrdiff_t, uchar])
#AC_CHECK_TYPE(ssize_t, int)
#AC_CHECK_TYPE(ptrdiff_t, int)
#AC_CHECK_TYPE(uchar, unsigned char)
AC_C_CHAR_UNSIGNED
AC_C_BIGENDIAN
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(off_t)
AC_CHECK_SIZEOF(size_t)
UD_MAKEWHATIS
UD_FTPBINDIR

# Tell the automake files whether or not parallel stuff is to be
# tested. (Always off in netCDF 3.6.2, will be turned on sometimes for
# netcdf 4.0).
AM_CONDITIONAL(TEST_PARALLEL, [test "x$enable_parallel" = xyes])

# Uncomment this to keep a copy of autoconf defines at this point, for
# debugging purposes.
# cp confdefs.h my_config.h


AC_MSG_NOTICE([generating header files and makefiles])

AC_CONFIG_FILES([Makefile
                 man/Makefile
                 fortran/Makefile
                 libsrc/Makefile
                 nctest/Makefile
                 nc_test/Makefile
 		 ncdump/Makefile
                 ncgen/Makefile
                 nf_test/Makefile
		 cxx/Makefile
		 f90/Makefile
		 examples/Makefile
		 examples/C/Makefile
		 examples/CDL/Makefile
		 examples/CXX/Makefile
		 examples/F90/Makefile
		 examples/F77/Makefile])

AC_OUTPUT()

