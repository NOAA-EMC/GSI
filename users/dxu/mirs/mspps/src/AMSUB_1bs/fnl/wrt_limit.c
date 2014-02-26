/***********************************************************************
 *  Program Name      : wrt_limit.c
 *  Type              : Subroutine
 *  Function          : Program writes limit data to the final AMSU-B
 *                      HDF-EOS swath file
 *  Input Files       : None
 *  Output Files      : SWATH_N16_AMSUB_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Subroutine Called : None
 *  Called by         : sw_wrt.c
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/********************************************************/
long int wrt_limit(long int sw_id)
{
      long int   status, result=0;
    
      float	 rr_limits[2];
      float	 snow_limits[2];
      float	 iwp_limits[2];
      float	 swe_limits[2];
      float	 snowr_limits[2];

      rr_limits[0] = limit_Prod.RR_lower;
      rr_limits[1] = limit_Prod.RR_upper;
      snow_limits[0] = limit_Prod.SNowC_lower;
      snow_limits[1] = limit_Prod.SNowC_upper;
      iwp_limits[0] = limit_Prod.IWP_lower;
      iwp_limits[1] = limit_Prod.IWP_upper;
      swe_limits[0] = limit_Prod.SWE_lower;
      swe_limits[1] = limit_Prod.SWE_upper;
      snowr_limits[0] = limit_Prod.SFR_lower;
      snowr_limits[1] = limit_Prod.SFR_upper;

      status = SWwriteattr(sw_id, "RR_Limits", DFNT_FLOAT32, 2, &rr_limits);
      printf("wrt_limits/writeattr RR_Limits %ld\n", status);
      result = result + status;

      status = SWwriteattr(sw_id, "SNOW_Limits", DFNT_FLOAT32, 2, &snow_limits);
      printf("wrt_limits/writeattr SNOW_Limits %ld\n", status);
      result = result + status;

      status = SWwriteattr(sw_id, "IWP_Limits", DFNT_FLOAT32, 2, &iwp_limits);
      printf("wrt_limits/writeattr IWP_Limits %ld\n", status);
      result = result + status;

      status = SWwriteattr(sw_id, "SWE_Limits", DFNT_FLOAT32, 2, &swe_limits);
      printf("wrt_limits/writeattr SWE_Limits %ld\n", status);
      result = result + status;

      status = SWwriteattr(sw_id, "SFR_Limits", DFNT_FLOAT32, 2, &snowr_limits);
      printf("wrt_limits/writeattr SFR_Limits %ld\n", status);
      result = result + status;

      return(result);

} /* end of wrt_limit.c */
