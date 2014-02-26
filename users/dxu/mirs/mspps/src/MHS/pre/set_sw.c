/***********************************************************************
 *  Program Name      : set_sw.c
 *  Type              : Subroutine
 *  Function          : Program sets up HDF-EOS swath file 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : gnrt_stime.c  sw_def.c  sw_wrt.c 
 *  Called by         : rmhs_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/*********************************************************/
void gnrt_stime(); 
long int sw_def();
long int sw_wrt();

/*********************************************************/
void set_sw(char *hdf_filename, TOTNUM_OF_SCAN numscan)
{ 
   long int   status;

/*--------------------------------------*
 * Generate scan time
 *--------------------------------------*/
   gnrt_stime(numscan);
   printf ("set_sw.c/finish generate the scantime \n");
  
/*--------------------------------------*
 * Define HDF-EOS swath file
 *--------------------------------------*/
   printf ("set_sw.c/hdf_filename: %s\n", hdf_filename);
   status = sw_def(hdf_filename, numscan);
   printf ("set_sw.c/swath id return from sw_def %ld\n", status);

/*--------------------------------------*
 * Write to the HDF-EOS swath file
 *--------------------------------------*/
   status = sw_wrt(hdf_filename, numscan);
   printf ("set_sw.c/swath id return from sw_wrt %ld\n", status);

} /* end of set_sw.c */
