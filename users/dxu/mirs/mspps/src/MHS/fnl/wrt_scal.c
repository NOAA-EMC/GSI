/**************************************************************************
 *  Program Name      : wrt_scal.c
 *  Type              : Subroutine
 *  Function          : Program writes scaling factors to the final MHS
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
 **************************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/*************************************************************/
long int wrt_scal(long int sw_id)
{
      long int   status, result=0;

      float     rr_scal, snow_scal, iwp_scal, swe_scal,snowr_scal;

      rr_scal = RR_SCAL;
      snow_scal = SNOW_SCAL;
      iwp_scal = IWP_SCAL;
      swe_scal = SWE_SCAL;
      snowr_scal = SFR_SCAL;


      status = SWwriteattr(sw_id, "RR_SCAL", DFNT_FLOAT32, 1, &rr_scal);
      printf("wrt_scal/writeattr RR_SCAL %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "SNOW_SCAL", DFNT_FLOAT32, 1, &snow_scal);
      printf("wrt_scal/writeattr SNOW_SCAL %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "IWP_SCAL", DFNT_FLOAT32, 1, &iwp_scal);
      printf("wrt_scal/writeattr IWP_SCAL %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "SWE_SCAL", DFNT_FLOAT32, 1, &swe_scal);
      printf("wrt_scal/writeattr SWE_SCAL %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "SFR_SCAL", DFNT_FLOAT32, 1, &snowr_scal);
      printf("wrt_scal/writeattr SFR_SCAL %ld\n",status);
      result = result + status;

      return(result);

} /* end of wrt_scal.c */
