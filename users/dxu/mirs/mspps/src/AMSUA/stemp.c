/***************************************************************************
 *  Program Name      : stemp.c
 *  Type              : Subroutine
 *  Function          : Program calculates surface temperature (land only) 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : calprod.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *  11/06/2000      v2.1          Add non-linear terms in Tsrf retrieval
 *				  and change coefficients
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************************/
void stemp(short int iscan)
{
    char	 lstag;
    short int    ifov;

    float        tb23, tb31, tb50, fts, frr, dt;
    float        a0 = 2.9079e+2;
    float        a1 = -8.5059e-1;
    float        a2 = 1.9821e-3;
    float        a3 = 6.1433e-1;
    float        a4 = -2.3579e-3;
    float        a5 = -1.1493e-0;
    float        a6 = 5.4709e-3;

    for(ifov = 0; ifov < NUMSPOT_A; ifov++)
    {
      lstag = stype[iscan][ifov];

      fts = MISSING;

      if(lstag == 1 && at[iscan][ifov][0] > 0)  /* land */
      {
        tb23 = at[iscan][ifov][0];
        tb31 = at[iscan][ifov][1];
        tb50 = at[iscan][ifov][2];
        frr = rr[iscan][ifov];

/*        if(frr > 0.)  / Rain rate is not available at this stage /
	   fts = INDETERM;
*/
        dt = 15.0 * (cos(lza[iscan][ifov] * PI / 180.) - 0.54); 
        fts = a0 + a1 * tb23 + a2 * tb23 * tb23 + a3 * tb31 + a4 * tb31 * tb31 + a5 * tb50 + a6 * tb50 * tb50 - dt; 
      
	if(fts < limit_A.Tsfc_lower)
	   fts = BELOW_PROD;
        else if(fts > limit_A.Tsfc_upper)
           fts = limit_A.Tsfc_upper;

      }

      else  if((lstag == 0 || lstag == 2) && at[iscan][ifov][0] > 0)  /* ocean and coast */
	 fts = INDETERM;
  
      if(fts > 0.)
        ts[iscan][ifov] = fts * TS_SCAL; 
      else
        ts[iscan][ifov] = fts; 

    }

} /* end of stemp.c */ 
