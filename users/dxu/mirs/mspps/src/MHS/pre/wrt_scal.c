/**************************************************************************
 *  Program Name      : wrt_scal.c
 *  Type              : Subroutine
 *  Function          : Program writes scaling factors to HDF_EOS swath file
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
 **************************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/*************************************************************/
long int wrt_scal(long int sw_id)
{
      long int   status, result=0;

      float     temp_scal;

      temp_scal = AT_SCAL;

      status = SWwriteattr(sw_id, "AT_SCAL", DFNT_FLOAT32, 1, &temp_scal);
      printf("wrt_scal/writeattr AT_SCAL %ld %f \n",status, temp_scal);
      result = result + status;

      return(result);

} /* end of wrt_scal.c */
