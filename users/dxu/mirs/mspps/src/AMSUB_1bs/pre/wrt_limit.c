/***********************************************************************
 *  Program Name      : wrt_limit.c
 *  Type              : Subroutine
 *  Function          : Program writes limit data to HDF-EOS swath file
 *  Input Files       : None
 *  Output Files      : NPR.ABOP.NK.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : None
 *  Called by         : sw_wrt.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************/
long int wrt_limit(long int sw_id)
{
      long int   status, result=0;

      float     temp_limits[5][2];

      temp_limits[0][0] = limit_B.Temp_lower[0];
      temp_limits[0][1] = limit_B.Temp_upper[0];

      temp_limits[1][0] = limit_B.Temp_lower[1];
      temp_limits[1][1] = limit_B.Temp_upper[1];

      temp_limits[2][0] = limit_B.Temp_lower[2];
      temp_limits[2][1] = limit_B.Temp_upper[2];

      temp_limits[3][0] = limit_B.Temp_lower[3];
      temp_limits[3][1] = limit_B.Temp_upper[3];

      temp_limits[4][0] = limit_B.Temp_lower[4];
      temp_limits[4][1] = limit_B.Temp_upper[4];

      status = SWwriteattr(sw_id, "AT_Limits", DFNT_FLOAT32, 10, &temp_limits);
      printf("wrt_limits/writeattr AT_Limits %ld %f %f \n", status, temp_limits[0][0], temp_limits[0][1]);
      result = result + status;

      return(result);

} /* end of wrt_limit.c */
