/***************************************************************************
 *  Program Name      : set_sw.c
 *  Type              : Subroutine
 *  Function          : Program sets up AMSU-A HDF-EOS swath file 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : sw_def.c, sw_wrt.c 
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/******************************************************************/
long int sw_def (char *hdf_filename, short int  num_of_scan);
long int sw_wrt (char *hdf_filename, short int  num_of_scan);

/******************************************************************/
void set_sw(char *hdf_filename, short int numscan)
{ 
   long int   status;

/*--------------------------------------------------------*
 * Define swath file
 *--------------------------------------------------------*/
   printf ("set_sw.c/hdf_filename: %s\n", hdf_filename);
   status = sw_def(hdf_filename, numscan);
   printf ("set_sw.c/swath id return from sw_def %ld\n", status);

/*--------------------------------------------------------*
 * Write swath file
 *--------------------------------------------------------*/
   status = sw_wrt(hdf_filename, numscan);
   printf ("set_sw.c/swath id return from sw_wrt %ld\n", status);

}/*  end of set_sw.c  */
