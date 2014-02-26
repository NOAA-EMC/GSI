/***********************************************************************
 *  Program Name      : wrt_prod.c
 *  Type              : Subroutine
 *  Function          : Program writes product fields to the final MHS
 *                      HDF-EOS swath file
 *  Input Files       : None
 *  Output Files      : SWATH_MP1_MHS_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Subroutine Called : None
 *  Called by         : sw_wrt.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/********************************************************/
long int wrt_prod(long int sw_id, TOTNUM_OF_SCAN numscan) 
{ 

   long int   status, result = 0;

   int32      start[2]  = {0, 0};
   int32      stride[2] = {1, 1};
   int32      edges[2];

/*-------------------------------------------*
 * Generate edges 
 *-------------------------------------------*/
   edges[0] = numscan;
   edges[1] = NUMSPOT_M;

/*-------------------------------------------*
 * Write product fields
 *-------------------------------------------*/
   status = SWwritefield(sw_id, "RR", start, stride, edges, rr);
   printf("wrt_prod/write field RR  %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "Snow", start, stride, edges, snow);
   printf("wrt_prod/write field Snow  %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "IWP", start, stride, edges, iwp);
   printf("wrt_prod/write field IWP %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "SWE", start, stride, edges, swe);
   printf("wrt_prod/write field SWE %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "SFR", start, stride, edges, snowr);
   printf("wrt_prod/write field SFR %ld\n", status);
   result = result + status;


   return(result);

} /* end of wrt_prod.c */
