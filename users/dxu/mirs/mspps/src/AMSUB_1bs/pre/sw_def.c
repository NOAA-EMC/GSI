/***********************************************************************
 *  Program Name      : sw_def.c
 *  Type              : Subroutine
 *  Function          : Program defines the geolocation and data fields 
 *			for HDF-EOS swath file
 *  Input Files       : None
 *  Output Files      : NPR.ABOP.NK.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : def_dim.c  def_geo.c  def_data.c
 *  Called by         : set_sw.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/*********************************************************/
long int   def_dim  (long int  swath_id, TOTNUM_OF_SCAN  num_scan);
long int   def_geo  (long int  swath_id);
long int   def_data(long int  swath_id);

/*********************************************************/
long int sw_def(char *hdf_filename, TOTNUM_OF_SCAN numscan)
{ 
   long int   swhdf_id, sw_id;
   long int   status;

   printf ("\n**** open swath hdfeos file ****\n");

/*--------------------------------------*
 * Open HDF-EOS swath file 
 *--------------------------------------*/
   swhdf_id = SWopen(hdf_filename, DFACC_CREATE);
   printf ("sw_def/swath hdfeos id %ld\n", swhdf_id);
   if ( swhdf_id <0) {
     printf("sw_def/ Create hdf-eos file %s fail!\n", hdf_filename);
     exit(4);
   }


/*--------------------------------------*
 * Create the first swath 
 *--------------------------------------*/
   printf ("sw_def/swath name %s\n", SWATH_NAME_B);
   sw_id = SWcreate(swhdf_id, SWATH_NAME_B);
   printf ("sw_def/swath_id %ld\n", sw_id);

   if ( sw_id <0) {
     printf("sw_def/ Create hdf-eos swath %s fail!\n",SWATH_NAME_B );
     exit(5);
   }


/*--------------------------------------*
 * Define swath dimensions 
 *--------------------------------------*/
   status = def_dim(sw_id, numscan);
   printf ("sw_def/result return from def_dim  %ld\n", status);

/*--------------------------------------*
 * Define geolocation fields
 *--------------------------------------*/
   status = def_geo (sw_id);
   printf ("sw_def/result return from def_geo  %ld\n", status);

/*--------------------------------------*
 * Define data fields
 *--------------------------------------*/
   status = def_data(sw_id);
   printf ("sw_def/result return from def_data  %ld\n", status);

/*--------------------------------------*
 * Detach and close swath file
 *--------------------------------------*/
   SWdetach(sw_id);
   SWclose(swhdf_id);

   return(sw_id);
 
} /* end of sw_def.c */
