/**************************************************************************
 *  Program Name      : wrt_scal.c
 *  Type              : Subroutine
 *  Function          : Program writes scaling factors to HDF-EOS swath file
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
 **************************************************************************/
#include "AMB2HDF_INCLUDE.h"
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
