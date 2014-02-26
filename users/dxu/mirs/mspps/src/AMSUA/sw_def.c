/**************************************************************************
 *  Program Name      : sw_def.c
 *  Type              : Subroutine
 *  Function          : Program defines geo and data fields for HDF-EOS 
 *                      swath file
 *  Input Files       : None
 *  Output Files      : SWATH_MP1_AMSUA_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Subroutine Called : None
 *  Called by         : sw_def.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
long int   def_dim  (long int  swath_id, short int  num_scan);
long int   def_geo  (long int  swath_id);
long int   def_data(long int  swath_id);
long int   def_prod(long int  swath_id);

/*****************************************************************/
long int sw_def(char *hdf_filename, short int numscan)
{ 

   long int   swhdf_id, sw_id;
   long int   status;

   printf ("\n**** open swath hdfeos file ****\n");

/*---------------------------------------------------------*
 * Create HDF-EOS swath file 
 *---------------------------------------------------------*/
   swhdf_id = SWopen(hdf_filename, DFACC_CREATE);
   printf ("sw_def/swath hdfeos id %ld\n", swhdf_id);
   if ( swhdf_id <0) {
     printf("sw_def/ Create hdf-eos file %s fail!\n", hdf_filename);
     exit(4);
   }


/*---------------------------------------------------------*
 * Create the first swath 
 *---------------------------------------------------------*/
   printf ("sw_def/swath name %s\n", SWATH_NAME_A);
   sw_id = SWcreate(swhdf_id, SWATH_NAME_A);
   printf ("sw_def/swath_id %ld\n", sw_id);
   if ( sw_id <0) {
     printf("sw_def/ Create hdf-eos swath %s fail!\n",SWATH_NAME_A );
     exit(5);
   }


/*---------------------------------------------------------*
 * Define the swath dimensions 
 *---------------------------------------------------------*/
   status = def_dim(sw_id, numscan);
   printf ("sw_def/result return from def_dim  %ld\n", status);

/*---------------------------------------------------------*
 * Set up the geolocation, data and product fields
 *---------------------------------------------------------*/
   status = def_geo (sw_id);
   printf ("sw_def/result return from def_geo  %ld\n", status);

   status = def_data(sw_id);
   printf ("sw_def/result return from def_data  %ld\n", status);

   status = def_prod(sw_id);
   printf ("sw_def/result return from def_prod  %ld\n", status);

/*---------------------------------------------------------*
 * Detach and close swath file
 *---------------------------------------------------------*/
   SWdetach(sw_id);
   SWclose(swhdf_id);

   return(sw_id);
 
}/*  end of sw_def.c  */
