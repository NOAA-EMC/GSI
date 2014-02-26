/**************************************************************************
 *  Program Name      : sw_wrt.c
 *  Type              : Subroutine
 *  Function          : Program writes geo, data, and product fields 
 *			to HDF-EOS swath file 
 *  Input Files       : None
 *  Output Files      : SWATH_MP1_AMSUA_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Subroutine Called : wrt_limit.c, wrt_scal.c, wrt_navig.c, wrt_geo.c,
 *			wrt_data.c, wrt_prod.c 
 *  Called by         : set_sw.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/ 
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************************/
long int   wrt_limit(long int swath_id);
long int   wrt_scal(long int swath_id);
long int   wrt_navig(long int swath_id);
long int   wrt_geo (long int swath_id, short int  numscan);
long int   wrt_data(long int swath_id, short int  numscan);
long int   wrt_prod(long int swath_id, short int  numscan);

/*******************************************************************/
long int sw_wrt(char *hdf_filename, short int numscan) 
{ 

   long int   swhdf_id, sw_id;
   long int   status;

/*---------------------------------------------------------*
 * Open HDF-EOS swath file
 *---------------------------------------------------------*/
   swhdf_id = SWopen(hdf_filename, DFACC_RDWR);
   printf ("sw_wrt/ swhdf_id %ld\n", swhdf_id);
   if ( swhdf_id <0) {
     printf("sw_wrt/ Open hdf-eos file %s fail!\n", hdf_filename);
     exit(6);
   }

   sw_id = SWattach(swhdf_id, SWATH_NAME_A);
   printf ("sw_wrt/ sw_id %ld\n", sw_id);
   if ( sw_id <0) {
     printf("sw_wrt/ Attach hdf-eos swath %s fail!\n",SWATH_NAME_A );
     exit(7);
   }

/*---------------------------------------------------------*
 * Write product limits, scaling factors, navigation 
 * parameters, geolocation, data and product fields
 *---------------------------------------------------------*/
   status = wrt_limit(sw_id);
   printf ("sw_wrt/result return from wrt_limit %ld\n", status);

   status = wrt_scal(sw_id);
   printf ("sw_wrt/result return from wrt_scal %ld\n", status);

   status = wrt_navig(sw_id);
   printf ("sw_wrt/result return from wrt_navig %ld\n", status);

   status = wrt_geo (sw_id, numscan);
   printf ("sw_wrt/result return from wrt_geo %ld\n", status);

   status = wrt_data(sw_id, numscan);
   printf ("sw_wrt/result return from wrt_data %ld\n", status);

   status = wrt_prod(sw_id, numscan);
   printf ("sw_wrt/result return from wrt_prod %ld\n", status);

/*---------------------------------------------------------*
 * Detach and close HDF-EOS swath file
 *---------------------------------------------------------*/
   status = SWdetach(sw_id);
   printf ("sw_wrt/value of detach swath file %ld\n", status);
   status = SWclose (swhdf_id);
   printf ("sw_wrt/value of close swath file %ld\n", status);

   return(sw_id);

}/*  end of sw_wrt.c */
