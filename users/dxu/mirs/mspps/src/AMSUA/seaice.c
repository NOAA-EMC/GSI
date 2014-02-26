/***************************************************************************
 *  Program Name      : seaice.c
 *  Type              : Subroutine
 *  Function          : Program calculates sea ice concentration  
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : corr.c 
 *  Called by         : calprod.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *   9/26/2000      v2.1        Computation of eps, eps_water, and eps_ice
 *   1/18/2001      v2.2        Change the cutoff value from 50% to 30%
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************************/
float  corr();

/*******************************************************************/
void seaice(short int iscan)
{
    char	 lstag;

    short int    ifov;

    float        tb23, tb31, tb50;
    float        alza, u, fice, alat;
    float        eps, eps_water, eps_ice;
    float        a, b, c, d;
    float	 difftb;

/*--------------------------------------------------------*
 * Sea ice computation for ocean FOVs 
 *--------------------------------------------------------*/
    for (ifov = 2; ifov < NUMSPOT_A - 2; ifov++) /* exclude orbital edge FOVs */
    {
      lstag = stype[iscan][ifov];

      if (lstag == 0 && at[iscan][ifov][0] > 0.)
      { 
          alat = lat[iscan][ifov]; 
          alza = lza[iscan][ifov];
          u = cos(alza * PI/180.0);
          eps_water = 0.1824 + 0.9048 * u - 0.6221 * u * u;

/*--------------------------------------------------------*
 * Perform AT test
 *--------------------------------------------------------*/

	  /* Asymmetry correction */
          tb23= at[iscan][ifov][0] + corr(iscan,ifov,0);
          tb31= at[iscan][ifov][1] + corr(iscan,ifov,1);
          tb50= at[iscan][ifov][2] + corr(iscan,ifov,2);


          a = 1.84 - 0.723 * u;
  	  b = -0.00088;
	  c = 0.0066 + 0.0029 * u;
	  d = -0.00926;
          eps = a + b*tb23 + c*tb31 + d*tb50;

          if(alat < 50.0 &&  alat > -50.0)  
             sice[iscan][ifov] = 0.0;  
          else
          {
             difftb = tb23 - tb31;
	     if(difftb < 5.)
	       eps_ice = 0.93;
	     else if(difftb >= 5. && difftb <= 10.)
               eps_ice = 0.87;
	     else 
	       eps_ice = 0.83;

             fice = 100 * (eps - eps_water)/(eps_ice - eps_water);

             if(fice < 30.0 ) /* cutoff value of 30% is applied */
	        fice = 0; 
             if(fice > limit_A.SIce_upper) 
                fice = limit_A.SIce_upper;
             if(fice < limit_A.SIce_lower) 
                fice = BELOW_PROD;

             if(fice > 0.)
               sice[iscan][ifov] = fice * SICE_SCAL;
             else
	       sice[iscan][ifov] = fice;

          }   /* end of eps check */

       } 

      else if(at[iscan][ifov][0] > 0.)   /* land and coast */
          sice[iscan][ifov] = INDETERM;

    }   /* end of ifov loop */

} /* end of seaice.c */    
