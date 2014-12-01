#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Config.c  just prints out the configuration
 *
 * 3/2009 public domain Wesley Ebisuzaki
 */

/*
 * HEADER:100:config:misc:0:shows the configuration
 */
int f_config(ARG0) {

    inv_out[0] = 0;
    strcat(inv_out, "wgrib2 " VERSION "\n\n");

    inv_out += strlen(inv_out);
    sprintf(inv_out,"Compiled on %s %s\n\n",__TIME__,__DATE__);

#ifdef USE_NETCDF3
    strcat(inv_out, "Netcdf3 package is installed\n");
#else
    strcat(inv_out, "Netcdf3 package is not installed\n");
#endif


#ifdef USE_NETCDF4
    strcat(inv_out, "Netcdf4 package is installed\n");
#else
    strcat(inv_out, "Netcdf4 package is not installed\n");
#endif

#ifdef USE_MYSQL
    strcat(inv_out, "mysql package is installed\n");
#else
    strcat(inv_out, "mysql package is not installed\n");
#endif


#ifdef USE_REGEX
    strcat(inv_out, "regex package is installed\n");
#else
    strcat(inv_out, "regex package is not installed\n");
#endif


#ifdef USE_TIGGE
    strcat(inv_out, "tigge package is installed\n");
#else
    strcat(inv_out, "tigge package is not installed\n");
#endif

#ifdef USE_IPOLATES
    strcat(inv_out, "interpolation package is installed\n");
#else
    strcat(inv_out, "interpolation package is not installed\n");
#endif


    inv_out += strlen(inv_out);
    sprintf(inv_out, "maximum number of arguments on command line: %d\n",
	N_ARGLIST);

    inv_out += strlen(inv_out);
    sprintf(inv_out, "stdout buffer length: %d\n", INV_BUFFER);

    strcat(inv_out, USE_G2CLIB ? 
       "g2clib is the default decoder\n" :
       "g2clib is not the default decoder\n");

    return 1;
}
