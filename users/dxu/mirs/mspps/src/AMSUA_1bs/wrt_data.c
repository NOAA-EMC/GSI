/**************************************************************************
 *  Program Name      : wrt_data.c
 *  Type              : Subroutine
 *  Function          : Program writes data fields to HDF-EOS swath file
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

/*******************************************************************/
long int wrt_data(long int sw_id, short int numscan) 
{ 

   long int   status, result = 0, i, j, k;
   char       atname[10];
   short int  b_temp[MAXSCANLINE_A][NUMSPOT_A];

   int32        start[2]  = {0, 0};
   int32        stride[2] = {1, 1};
   int32        edges[2];

/*--------------------------------------------------------*
 * Generate edges
 *--------------------------------------------------------*/
   edges[0] = numscan;
   edges[1] = NUMSPOT_A;

/*--------------------------------------------------------*
 * Write surface type data field 
 *--------------------------------------------------------*/
   status = SWwritefield(sw_id, "Sfc_type", start, stride, edges, stype);
   printf("wrt_data/writefield SurfaceType %ld\n", status);
   result = result + status;

/*--------------------------------------------------------*
 * Write orbit mode data field 
 *--------------------------------------------------------*/
   status = SWwritefield(sw_id, "Orbit_mode", start, stride, edges, orb_mode);
   printf("wrt_data/writefield orbit_mode %ld\n", status);
   result = result + status;

/*--------------------------------------------------------*
 * Write angle data fields 
 *--------------------------------------------------------*/
   status = SWwritefield(sw_id, "LZ_angle", start, stride, edges, lza);
   printf("wrt_data/writefield lza %ld\n", status);
   result = result + status;
 
   status = SWwritefield(sw_id, "SZ_angle", start, stride, edges, sza);
   printf("wrt_data/writefield sza %ld\n", status);
   result = result + status;

/*--------------------------------------------------------*
 * Write antenna temperature fields
 *--------------------------------------------------------*/
   for(k = 0; k < NUMCHAN_A; k++)
   {
      for(i = 0; i < numscan; i++)
        for(j = 0; j < NUMSPOT_A; j++)
        {
          if(at[i][j][k] != -99.0)
             b_temp[i][j] = AT_SCAL*at[i][j][k];
          else
             b_temp[i][j] = -99;

        }
 
      sprintf(atname,"Chan%ld_AT",k+1);

      status = SWwritefield(sw_id, atname, start, stride, edges, b_temp);
      printf("wrt_data/writefield at%ld  %ld\n", k+1, status);
      result = result + status;
   }

   return(result);

}/*  end of wrt_data.c */
