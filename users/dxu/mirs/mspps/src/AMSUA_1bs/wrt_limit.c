/**************************************************************************
 *  Program Name      : wrt_limit.c
 *  Type              : Subroutine
 *  Function          : Program writes limit data to HDF-EOS swath file
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
 *  12/11/2000      v2.1     Add limits for TS, EM23, EM31, and EM50
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*************************************************************/
long int wrt_limit(long int sw_id)
{
      long int   status, result=0;

      float     temp_limits[15][2];
      float     tpw_limits[2];
      float     clw_limits[2];
      float     sice_limits[2];
      float	ts_limits[2];
      float	em23_limits[2];
      float	em31_limits[2];
      float	em50_limits[2];
      float     rr_limits[2];
      float     snowc_limits[2];

      temp_limits[0][0] = limit_A.Temp_lower[0];
      temp_limits[0][1] = limit_A.Temp_upper[0];

      temp_limits[1][0] = limit_A.Temp_lower[1];
      temp_limits[1][1] = limit_A.Temp_upper[1];

      temp_limits[2][0] = limit_A.Temp_lower[2];
      temp_limits[2][1] = limit_A.Temp_upper[2];

      temp_limits[3][0] = limit_A.Temp_lower[3];
      temp_limits[3][1] = limit_A.Temp_upper[3];

      temp_limits[4][0] = limit_A.Temp_lower[4];
      temp_limits[4][1] = limit_A.Temp_upper[4];

      temp_limits[5][0] = limit_A.Temp_lower[5];
      temp_limits[5][1] = limit_A.Temp_upper[5];

      temp_limits[6][0] = limit_A.Temp_lower[6];
      temp_limits[6][1] = limit_A.Temp_upper[6];

      temp_limits[7][0] = limit_A.Temp_lower[7];
      temp_limits[7][1] = limit_A.Temp_upper[7];

      temp_limits[8][0] = limit_A.Temp_lower[8];
      temp_limits[8][1] = limit_A.Temp_upper[8];

      temp_limits[9][0] = limit_A.Temp_lower[9];
      temp_limits[9][1] = limit_A.Temp_upper[9];

      temp_limits[10][0] = limit_A.Temp_lower[10];
      temp_limits[10][1] = limit_A.Temp_upper[10];

      temp_limits[11][0] = limit_A.Temp_lower[11];
      temp_limits[11][1] = limit_A.Temp_upper[11];

      temp_limits[12][0] = limit_A.Temp_lower[12];
      temp_limits[12][1] = limit_A.Temp_upper[12];

      temp_limits[13][0] = limit_A.Temp_lower[13];
      temp_limits[13][1] = limit_A.Temp_upper[13];

      temp_limits[14][0] = limit_A.Temp_lower[14];
      temp_limits[14][1] = limit_A.Temp_upper[14];

      tpw_limits[0] = limit_A.TPW_lower;
      tpw_limits[1] = limit_A.TPW_upper;

      clw_limits[0] = limit_A.CLW_lower;
      clw_limits[1] = limit_A.CLW_upper;

      sice_limits[0] = limit_A.SIce_lower;
      sice_limits[1] = limit_A.SIce_upper;

      ts_limits[0] = limit_A.Tsfc_lower;
      ts_limits[1] = limit_A.Tsfc_upper;

      em23_limits[0] = limit_A.Em23_lower;
      em23_limits[1] = limit_A.Em23_upper;

      em31_limits[0] = limit_A.Em31_lower;
      em31_limits[1] = limit_A.Em31_upper;

      em50_limits[0] = limit_A.Em50_lower;
      em50_limits[1] = limit_A.Em50_upper;

      rr_limits[0] = limit_A.RR_lower;
      rr_limits[1] = limit_A.RR_upper;

      snowc_limits[0] = limit_A.SNowC_lower;
      snowc_limits[1] = limit_A.SNowC_upper;

      status = SWwriteattr(sw_id, "AT_Limits", DFNT_FLOAT32, 30, &temp_limits);
      printf("wrt_limits/writeattr AT_Limits %ld %f %f \n", status, temp_limits[0][0], temp_limits[0][1]);
      result = result + status;

      status = SWwriteattr(sw_id, "TPW_Limits", DFNT_FLOAT32, 2, &tpw_limits);
      printf("wrt_limits/writeattr TPW_Limits %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "CLW_Limits", DFNT_FLOAT32, 2, &clw_limits);
      printf("wrt_limits/writeattr CLW_Limits %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "SIce_Limits", DFNT_FLOAT32, 2, &sice_limits);
      printf("wrt_limits/writeattr SIce_Limits %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "TS_Limits", DFNT_FLOAT32, 2, &ts_limits);
      printf("wrt_limits/writeattr TS_Limits %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "EM23_Limits", DFNT_FLOAT32, 2, &em23_limits);
      printf("wrt_limits/writeattr EM23_Limits %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "EM31_Limits", DFNT_FLOAT32, 2, &em31_limits);
      printf("wrt_limits/writeattr EM31_Limits %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "EM50_Limits", DFNT_FLOAT32, 2, &em50_limits);
      printf("wrt_limits/writeattr EM50_Limits %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "Rain_Limits", DFNT_FLOAT32, 2, &rr_limits);
      printf("wrt_limits/writeattr Rain_Limits %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "SNowC_Limits",DFNT_FLOAT32,2,&snowc_limits);
      printf("wrt_limits/writeattr SNowC_Limits %ld\n",status);
      result = result + status;

      return(result);

} /* end of wrt_limit.c */
