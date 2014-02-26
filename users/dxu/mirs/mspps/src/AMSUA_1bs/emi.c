/***************************************************************************
 *  Program Name      : emi.c
 *  Type              : Subroutine
 *  Function          : Program calculates emissivity for the first three
 *			AMSU-A channels: 23.8, 31.4 and 50.3 GHz (land only)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : calprod.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *  11/06/2000      v2.1     Add non-linear terms to emissivity retrivals
 * 			     and change coefficients
 *  05/08/2007      V2.2     algorithm enhancement (ccr3135)
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************************/
void emi(short int iscan)
{
    char	 lstag;

    short int    ifov;

    float        tb23, tb31, tb50, fem1, fem2, fem3;

    float        b01 = -2.5404e-1; 
    float        b11 = 1.1326e-2; 
    float        b21 = -1.9479e-5;
    float        b31 = -4.5763e-3;
    float        b41 = 1.7833e-5;
    float        b51 = 3.2324e-3; 
    float        b61 = -1.9056e-5;

    float        b02 = -2.2606e-1;
    float        b12 = 3.4481e-3; 
    float        b22 = -9.7185e-6;
    float        b32 = 4.3299e-3; 
    float        b42 = 5.3281e-6; 
    float        b52 = 1.8668e-3; 
    float        b62 = -1.5369e-5;

    float        b03 = 8.9494e-2; 
    float        b13 = -3.6615e-3;
    float        b23 = 4.2390e-7; 
    float        b33 = 1.0636e-2;  
    float        b43 = -6.4559e-6;
    float        b53 = -4.2449e-4;
    float        b63 = -6.6878e-6; 

    for(ifov = 0; ifov < NUMSPOT_A; ifov++)
    {
      lstag = stype[iscan][ifov];
      
      if(lstag == 1 && at[iscan][ifov][0] > 0)    /* land */
      {
        tb23 = at[iscan][ifov][0];
        tb31 = at[iscan][ifov][1]; 
        tb50 = at[iscan][ifov][2];
/*        frr = rr[iscan][ifov];

	if(frr > 0.)
        {
	   fem1 = INDETERM;
	   fem2 = INDETERM;
	   fem3 = INDETERM;
        }
        else
*/
 	{
           fem1 = b01 + b11 * tb23 + b21 * tb23 * tb23 + b31 * tb31 + b41 * tb31 * tb31 + b51 * tb50 + b61 * tb50 * tb50;
           fem2 = b02 + b12 * tb23 + b22 * tb23 * tb23 + b32 * tb31 + b42 * tb31 * tb31 + b52 * tb50 + b62 * tb50 * tb50;
           fem3 = b03 + b13 * tb23 + b23 * tb23 * tb23 + b33 * tb31 + b43 * tb31 * tb31 + b53 * tb50 + b63 * tb50 * tb50;
	}

        if(fem1 < limit_A.Em23_lower)
           fem1 = BELOW_PROD;
        else if(fem1 > limit_A.Em23_upper)
	   fem1 = limit_A.Em23_upper;

        if(fem2 < limit_A.Em31_lower)
           fem2 = BELOW_PROD;
        else if(fem2 > limit_A.Em31_upper)
	   fem2 = limit_A.Em31_upper;

        if(fem3 < limit_A.Em50_lower)
           fem3 = BELOW_PROD;
        else if(fem3 > limit_A.Em50_upper)
	   fem3 = limit_A.Em50_upper;
      }

      else if(at[iscan][ifov][0] > 0)    /* ocean and coast */
      {
	fem1 = INDETERM;
	fem2 = INDETERM;
	fem3 = INDETERM;
      }

      if(fem1 > 0.) 
        em1[iscan][ifov] = fem1 * EM_SCAL; 
      else
	em1[iscan][ifov] = fem1;
 
      if(fem2 > 0.) 
        em2[iscan][ifov] = fem2 * EM_SCAL; 
      else
	em2[iscan][ifov] = fem2;
 
      if(fem3 > 0.) 
        em3[iscan][ifov] = fem3 * EM_SCAL; 
      else
	em3[iscan][ifov] = fem3;
 
    }

} /* end of emi.c */ 
