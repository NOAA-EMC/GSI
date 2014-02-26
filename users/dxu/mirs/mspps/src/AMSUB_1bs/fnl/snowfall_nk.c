/***********************************************************************
 *  Program Name      : snowfall.c
 *  Type              : Subroutine
 *  Function          : Program calculates snowfall and merge it with
 *			rain rate (100 * mm/hr) (global)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called :
 *  Called by         : calprod.c
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/****************************************************/
#define   A0a       3.37 
#define   A1a       0.595 
#define   A2a       1.582 

#define   A0b      -1.319 
#define   A1b      -1.555 
#define   A2b      -1.55 

/****************************************************/
void snowfall(short int iscan)
{
   char		lstag;
   short int    ifov;
   float 	A0, A1, A2;
   float	acoslza;
   float        a23, a52, a53, a53L, b89, b150, b176, b180, b182; 
   float	brr, threshold, depression;

   for (ifov = 0; ifov < NUMSPOT_B; ifov++)
   {
     a23 = at_AB[iscan][ifov][0];
     a52 = at_AB[iscan][ifov][3];
     a53 = at_AB[iscan][ifov][4];
     acoslza = cos(lza_AB[iscan][ifov] * PI / 180.);

     b89 = at[iscan][ifov][0];
     b150 = at[iscan][ifov][1];
     b182 = at[iscan][ifov][2];
     b180 = at[iscan][ifov][3];
     b176 = at[iscan][ifov][4];

     brr = rr[iscan][ifov];

     lstag = stype[iscan][ifov];

/*------------------------------------*
 * Limb correction for AMSU-A ch-5 AT 
 *------------------------------------*/
     if(a52 > 0 && a53 > 0)
     {
       A0 = A0a + A0b * acoslza;
       A1 = A1a + A1b * acoslza;
       A2 = A2a + A2b * acoslza;
       a53L = A0 - A1 * a53+ A2 * a52+ 4 * (1 - acoslza);
     }

/*------------------------------------*
 * Check for snowfall 
 *------------------------------------*/
     if(lstag == 1 && a23 > 0 && b89 > 0) /* over land and good data */
     {
       if(brr == INDETERM_SNOW || brr == INDETERM_FROZEN) /* snow cover, or frozen surface */
       {
         if(a53L >= COLD_SNOW_LO && a53L <= COLD_SNOW_HI) /* COLD_SNOW_LO=243, COLD_SNOW_HI=245 */
         {
           brr = 0.;
	   threshold = 242.5 + 5*acoslza; 
           depression = b180 - threshold;
           if(depression < 0)
             brr = SNOWFALL * RR_SCAL;
         }
         else if(a53L < COLD_SNOW_LO)  /* too cold for snowfall detection */
           brr = INDETERM_FROZEN;
         else
         {
           brr = 0.;
           if((b89 - b150) >= 4.)
           {
	     if((b176 < 255) && (b180 < 253) && (b182 < 250))
               brr = SNOWFALL * RR_SCAL;
             else
             {
	       if((b176 >= 255.) && (b180 <= 253.) && (a23 <= 262))
               {
	         if((b150 - b176) >= -16. && (b176 - b180) >= -3 && (b89 - b150) <= 10.) 
	           brr = SNOWFALL * RR_SCAL;
               }
             }

           }  /* end of (b89-b150) check */

         }  /* end of check for cold-snow */

         rr[iscan][ifov] = brr;

       }  /* end of rain flag check */

     }  /* end of land-sea tag check */

   }  /* end of ifov loop */

} /* end of snowfall.c */ 
