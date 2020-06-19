Quick start--

Currently, the GSI build has module files for the following platforms--

WCOSS_C (Cray), WCOSS_D (Dell), Hera, S4 (U. Wisc), Orion (MSU), Cheyenne (NCAR), Jet, Gaea, and Discover (NASA)

If you are on one of these supported platforms, run the following command to build the GSI--

cd to GSI directory
module use $PWD/modulefiles
module load modulefile.ProdGSI.{platform_name}
./ush/build_all_cmake.sh 0 $PWD

where {platform_name} is replace by the name of your platform (look in the GSI/modulefiles directory to see the exact syntax)

The build_all_cmake.sh script will run cmake and build the GSI and EnKF executables along with several utilities which will
all reside in GSI/build/bin upon completion.

If you are on an unsupported platform, you will need to build (or load) a compiler (GNU or Intel), a version of MPI, NetCDF4,
HDF5, cmake (version 3.16+), and the following NCEPLIBS-- bacio, bufr, crtm (technically a fork of official repo), nemsio,
ip, sp, sigio, sfcio, w3emc, and w3nco. All of these libraries and components are now supplied by the NOAA-EMC repositories
containting NCEPLIBS-external (https://github.com/NOAA-EMC/NCEPLIBS-external) and NCEPLIBS (https://github.com/NOAA-EMC/NCEPLIBS).
There are detailed instructions for building both the external libraries and NCEPLIBS here--https://github.com/NOAA-EMC/NCEPLIBS-external/wiki

Once the externals and NCEPLIBS have been built, the GSI can be built using the following commands--

cd GSI
mkdir build
cd build
cmake -DCMAKE_PREFIX_INSTALL="path-to-NCEPLIBS-external-install-directory;path-to-NCEPLIBS-install-directory" ..
make -j N (where N is the number of cores dedicated to the build)

----

Detailed instructions

CMake allows for various options that can be specified on the command line or from within the ccmake gui. The list of options
currently available is as follows--

BUILD_ENKF -- will build the enkf executable (default is ON)
BUILD_GLOBAL -- will build GSI without WRF dependencies (default is OFF) -- this will also force USE_WRF=OFF
USE_WRF -- will build GSI with WRF dependencies (default is ON) 
BUILD_REG_TESTING -- will build GSI with regression testing built in (default is ON)
BUILD_GFS -- will build ENKF using GFS (default is ON)
BUILD_WRF -- will build ENKF using WRF (default is OFF)
BUILD_NMMB -- will build ENKF using NMMB (default is OFF)

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

Regression Testing on WCOSS and Hera 

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
