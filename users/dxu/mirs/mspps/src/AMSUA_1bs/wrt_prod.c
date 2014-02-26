/**************************************************************************
 *  Program Name      : wrt_prod.c
 *  Type              : Subroutine
 *  Function          : Program writes product fields to HDF-EOS swath file
 *  Input Files       : None
 *  Output Files      : NPR.AAOP.NK.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : None
 *  Called by         : sw_wrt.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/****************************************************************/
long int wrt_prod(long int sw_id, short int numscan) 
{ 

   long int   status, result = 0;

   int32      start[2]  = {0, 0};
   int32      stride[2] = {1, 1};
   int32      edges[2];

/*-------------------------------------------------------*
 * Generate edges 
 *-------------------------------------------------------*/
   edges[0] = numscan;
   edges[1] = NUMSPOT_A;

/*-------------------------------------------------------*
 * Write product fields
 *-------------------------------------------------------*/
   status = SWwritefield(sw_id, "TPW", start, stride, edges,tpw);
   printf("wrt_prod/writefield TPW  %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "CLW", start, stride, edges,clw);
   printf("wrt_prod/writefield CLW  %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "SIce", start, stride, edges,sice);
   printf("wrt_prod/writefield SIce  %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "T_sfc", start, stride, edges,ts);
   printf("wrt_prod/writefield TS %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "Emis_23", start, stride, edges,em1);
   printf("wrt_prod/writefield EM23  %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "Emis_31", start, stride, edges,em2);
   printf("wrt_prod/writefield EM31  %ld\n", status);
   result = result + status;

   status = SWwritefield(sw_id, "Emis_50", start, stride, edges,em3);
   printf("wrt_prod/writefield EM50  %ld\n", status);
   result = result + status;

   return(result);

}/*  end of wrt_prod.c */
