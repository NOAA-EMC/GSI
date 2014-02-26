/***********************************************************************
 *  Program Name      : set_sw.c
 *  Type              : Subroutine
 *  Function          : Program sets up HDF-EOS swath file 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : sw_def.c, sw_wrt.c 
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/10/2000      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/********************************************************/
long int sw_def ();
long int sw_wrt ();

/********************************************************/
void set_sw(char *hdf_filename, TOTNUM_OF_SCAN numscan)
{ 
   long int   status;

/*-----------------------------------*
 * Define HDF-EOS swath file
 *-----------------------------------*/
   printf ("set_sw.c/hdf_filename: %s\n", hdf_filename);
   status = sw_def(hdf_filename, numscan);
   printf ("set_sw.c/swath id return from sw_def %ld\n", status);

/*-----------------------------------*
 * Write the HDF-EOS swath file
 *-----------------------------------*/
   status = sw_wrt(hdf_filename, numscan);
   printf ("set_sw.c/swath id return from sw_wrt %ld\n", status);

}  /*  end of set_sw.c  */
