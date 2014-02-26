/***********************************************************************
 *  Program Name      : sw_def.c
 *  Type              : Subroutine
 *  Function          : Program defines the MHS product fields for
 *                      the final MHS HDF-EOS swath file 
 *  Input Files       : None
 *  Output Files      : SWATH_MP1_MHS_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time) 
 *  Subroutine Called : def_prod.c 
 *  Called by         : set_sw.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/10/2000      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/**************************************************************/
long int   def_prod(); 

/**************************************************************/
long int sw_def(char *hdf_filename, TOTNUM_OF_SCAN numscan)
{ 
   long int   swhdf_id, sw_id;
   long int   status;

   printf ("\n**** Open HDF-EOS swath file ****\n");

/*-------------------------------------------*
 * Open HDF-EOS swath file for read and write
 *-------------------------------------------*/
   swhdf_id = SWopen(hdf_filename, DFACC_RDWR);
   printf ("sw_def/swath hdfeos id %ld\n", swhdf_id);
   if(swhdf_id  < 0 )
   {
     printf("sw_def.c/Open swath hdf file %s failed!\n", hdf_filename);
     exit(6);
   }


/*-------------------------------------------*
 * Attach to the swath 
 *-------------------------------------------*/
   printf ("sw_def/swath name %s\n", SWATH_NAME_M);
   sw_id = SWattach(swhdf_id, SWATH_NAME_M);
   printf ("sw_def/swath_id %ld\n", sw_id);
   if ( sw_id < 0 )
   {
     printf("sw_def/Attach swath  failed !\n");
     exit(7);
   }

/*-------------------------------------------*
 * Define product fields
 *-------------------------------------------*/
   status = def_prod(sw_id);
   printf ("sw_def/result return from def_prod  %ld\n", status);
   if ( status != 0 )
   {
     printf("sw_def/Define_prod   failed !\n");
     exit(10);
   }


/*-------------------------------------------*
 * Detach and close swath file
 *-------------------------------------------*/
   SWdetach(sw_id);
   SWclose(swhdf_id);

   return(sw_id);
 
} /* end of sw_def.c */
