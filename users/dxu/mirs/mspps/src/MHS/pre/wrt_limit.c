/***********************************************************************
 *  Program Name      : wrt_limit.c
 *  Type              : Subroutine
 *  Function          : Program writes limit data to HDF_EOS swath file
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
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/*************************************************************/
long int wrt_limit(long int sw_id)
{
      long int   status, result=0;

      float     temp_limits[5][2];

      temp_limits[0][0] = limit_M.Temp_lower[0];
      temp_limits[0][1] = limit_M.Temp_upper[0];

      temp_limits[1][0] = limit_M.Temp_lower[1];
      temp_limits[1][1] = limit_M.Temp_upper[1];

      temp_limits[2][0] = limit_M.Temp_lower[2];
      temp_limits[2][1] = limit_M.Temp_upper[2];

      temp_limits[3][0] = limit_M.Temp_lower[3];
      temp_limits[3][1] = limit_M.Temp_upper[3];

      temp_limits[4][0] = limit_M.Temp_lower[4];
      temp_limits[4][1] = limit_M.Temp_upper[4];

      status = SWwriteattr(sw_id, "AT_Limits", DFNT_FLOAT32, 10, &temp_limits);
      printf("wrt_limits/writeattr AT_Limits %ld %f %f \n", status, temp_limits[0][0], temp_limits[0][1]);
      result = result + status;

      return(result);

} /* end of wrt_limit.c */
