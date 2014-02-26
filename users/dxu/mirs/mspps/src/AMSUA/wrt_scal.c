/**************************************************************************
 *  Program Name      : wrt_scal.c
 *  Type              : Subroutine
 *  Function          : Program writes scaling factors to HDF-EOS swath file
 *  Input Files       : None
 *  Output Files      : SWATH_MP1_AMSUA_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Subroutine Called : None
 *  Called by         : sw_wrt.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"

/*************************************************************/
long int wrt_scal(long int sw_id)
{
      long int   status, result=0;

      float     temp_scal;
      float     tpw_scal;
      float     clw_scal;
      float     sice_scal;
      float     ts_scal;
      float     em_scal;

      temp_scal = AT_SCAL;
      tpw_scal  = TPW_SCAL;
      clw_scal  = CLW_SCAL;
      sice_scal  = SICE_SCAL;
      ts_scal  = TS_SCAL;
      em_scal  = EM_SCAL;

      status = SWwriteattr(sw_id, "AT_SCAL", DFNT_FLOAT32, 1, &temp_scal);
      printf("wrt_scal/writeattr AT_SCAL %ld %f \n",status, temp_scal);
      result = result + status;

      status = SWwriteattr(sw_id, "TPW_SCAL", DFNT_FLOAT32, 1, &tpw_scal);
      printf("wrt_scal/writeattr TPW_SCAL %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "CLW_SCAL", DFNT_FLOAT32, 1, &clw_scal);
      printf("wrt_scal/writeattr CLW_SCAL %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "SICE_SCAL", DFNT_FLOAT32, 1, &sice_scal);
      printf("wrt_scal/writeattr SICE_SCAL %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "TS_SCAL", DFNT_FLOAT32, 1, &ts_scal);
      printf("wrt_scal/writeattr TS_SCAL %ld\n",status);
      result = result + status;

      status = SWwriteattr(sw_id, "EM_SCAL", DFNT_FLOAT32, 1, &em_scal);
      printf("wrt_scal/writeattr EM_SCAL %ld\n",status);
      result = result + status;

      return(result);

} /* end of wrt_scal.c */
