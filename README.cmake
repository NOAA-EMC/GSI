Quick start--

load desired compilers (intel recommended), desired mpi libraries/compilers, netcdf library (and hdf5 library for netcdf4)

skip the following section unless you are compiling on a different machine or wish to link in special libraries
----------------------------------
If not on WCOSS, Theia, or S4, ensure that the environment flags CC, CXX, and FC all point to to the C, C++, and Fortran compilers respectively

If building on a machine other than WCOSS, Theia, or S4, several additional environment variables need to be set--
  COREPATH should be set to the root path where the core libraries are located (i.e. /nwprod/lib )
  WRFPATH should be set to the root path where WRF is install (i.e. /nwprod/sorc/wrf_shared.f )
  If libraries are in these locations, they should all be found once these environment variables are set. Please note that this is all done 
  automatically on WCOSS, Theia, and S4.

  Alternatively, any (or every) core library can also be specified via environment variable. So, if a desired library is in an unusual location
  such as a user's home directory, it can be found by specifying the location like--
     export CRTM_LIB=/home/myname/MyCRTM/libcrtm.a (for csh and tcsh-- setenv CRTM_LIB /home/myname/MyCRTM/libcrtm.a )
     other library environment variable names are BACIO_LIB, BUFR_LIB, NEMSIO_LIB, SIGIO_LIB, SFCIO_LIB, SP_LIB, W3NCO_LIB W3EMC_LIB
---------------------------------

  
Create a build directory somewhere outside the source tree.
cd to the build directory
run--

	cmake (path-to-source-tree)
	make -j 8

The above should find all the dependent libraries (tested on wcoss, theia and s4) and build the base gsi executable which will be 
called gsi.x and located in the (build-dir)/bin directory.

----

Detailed instructions

CMake allows for various options that can be specified on the command line or from within the ccmake gui. The list of options
currently available is as follows--

BUILD_ENKF -- will build the enkf executable (default is ON)
BUILD_CORELIBS -- will attempt to find the source and build all the core libraries (nemsio, sigio, sfcio, sp, bacio, bufr, w3emc, w3nco crtm)
BUILD_GLOBAL -- will build GSI without WRF dependencies (default is OFF)
USE_WRF -- will build GSI with WRF dependencies (default is ON)
BUILD_REG_TESTING -- will build GSI with regression testing built in (default is ON)
BUILD_GFS -- will build ENKF using GFS (default is ON)
BUILD_WRF -- will build ENKF using WRF (default is OFF)
BUILD_NMMB -- will build ENKF using NMMB (default is OFF)

If CMake cannot find the source for a given library (should not be a problem on WCOSS, Theia, or S4), the location may be specified via
environment variable. The names of those variables is as follows--
   BACIO_SRC
   CRTM_SRC
   BUFR_SRC
   NEMSIO_SRC
   SIGIO_SRC
   SFCIO_SRC
   SP_SRC
   W3NCO_SRC 
   W3EMC_SRC
If specified, these variables should point to the the actual directory containing the source (i.e. export BACIO_SRC=/nwprod2/lib/bacio/v2.0.2/src )
If the modules for the libraries are loaded on WCOSS, they will automatically be detected by CMake

If BUILD_CORELIBS is turned on, individual libraries can be turned off using 
BUILD_CRTM
BUILD_SIGIO
BUILD_SFCIO
BUILD_NEMSIO
BUILD_SP
BUILD_BUFR
BUILD_BACIO
BUILD_NCO
BUILD_EMC

An example of configuring the GSI model to compile along with all the core libraries and the ENKF executable using WRF would be as follows

cmake -DBUILD_WRF=ON -DBUILD_CORELIBS=ON (path-to-source-tree)

Building only the GSI, ENKF and CRTM executables/library can be done as follows--

cmake -DBUILD_ENKF=ON -DBUILD_CORELIBS=ON -DBUILD_BACIO=OFF -DBUILD_SP=OFF -DBUILD_NEMSIO=OFF -DBUILD_NCO=OFF -DBUILD_EMC=OFF -DBUILD_SFCIO=OFF -DBUILD_SIGIO=OFF -DBUILD_BUFR=OFF  (path-to-source-tree)

If the core libraries desired are not located in the traditional paths, a hint can be provided to cmake via the environment variable $COREPATH. Similarly, 
the environment variable $WRFPATH can give cmake a hint to find the WRF libraries and associated files (pack_utils.o, module_machine.o)

-------------

Using the ccmake utility

ccmake is a text-based front end to cmake.  It can be launched with the same syntax as cmake, but will provide the user with feedback and requests for more
instructions.  A typical usage would be as follows--

from the build directory--

ccmake (path_to_source_tree)

(from the menu at the bottom--press c to configure)

Some stdout will show up on the screen and the menu will offer "Press [e] to exit help"
Press e

Press c to configure 

(the above steps might have to be repeated)

With no errors, the " Press [g] to generate and exit" option should show up in the menu section
Press g to generate a CMakeCache.txt file

This should exit ccmake and leave you in the build directory.
Type make -j n to commence the parallel build (where n is the number of cores to use) -- 4 may be around the maximum number that can be utilized currently

-----

Background on cmake and associated files----

cmake and ccmake create a file called "CMakeCache.txt" and a directory called CMakeFiles in the build directory.
The CMakeCache.txt file can be edited using a text editor or by re-running ccmake and changing some of the configurations there.
In some cases, cmake will not be able to "re-find" various libraries because of conflicts in the CMakeFiles directory. If this appears to be 
happening, remove CMakeCache.txt and CMakeFiles and re-run cmake/ccmake with a fresh set of requested options.

It is also important to note that when run from the build directory, cmake will not modify any source files and it is safe to delete the entire build
tree and start over at any time. The GSI cmake build system has also been set up not to allow for in-source builds.

Once cmake or ccmake have been run initially by pointing to the source tree, ccmake can be re-run with "ccmake ." 
When run in this fashion, it will simply read the CMakeCache.txt file and allow a user to edit the desired options

-----

Regression Testing on WCOSS and Theia

CMake will attempt to locate a control executable (gsi.x or global_gsi) somewhere in your source code treee. It is currently designed to look in
your source tree (up to two levels above the branch) as well as in Michael Lueken's directory. If it cannot find an executable, you can still
define the location inside the CMakeCache.txt file in your build directory. The name of the variable is GSICONTROL and it needs a full path to
the executable.  

Once a control has been found and the build is complete, regression testing can be launched by running "make test" or "ctest" from the build directory.
When run as simply "make test" or "ctest", the tests will run one at a time. However, the tests can be launched in parallel by adding the "-j N" argument 
as such--
    make test "ARGS= -j N"
    ctest -j N

where N is the number of parallel jobs desired. Similarly, individual regression tests may be run by specifying them via the -I parameter.
For example,

    make test "ARGS= -I 12" 

will run only the hwrf_nmm_d2 test. See "man ctest" for further details.
