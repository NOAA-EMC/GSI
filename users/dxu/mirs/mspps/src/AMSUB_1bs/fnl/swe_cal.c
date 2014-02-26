/***********************************************************************
 *  Program Name      : swe_cal.c
 *  Type              : Subroutine
 *  Function          : Program determines SWE (land) 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called :
 *  Called by         : calprod.c
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/****************************************************/
#define   COEA  0.6
#define   COEB  1.7
#define   COEC 0.08
#define   COED  1.1
 
/****************************************************/
void swe_cal(short int iscan)
{
   short int    ifov;
   float	a23, a31, b89, SI31, SI89, SI89_1, snowc, swe1;
   float	blat, blon;

   for (ifov = 0; ifov < NUMSPOT_B; ifov++)
   {

     swe1 = MISSING;

     a23 = at_AB[iscan][ifov][0];
     a31 = at_AB[iscan][ifov][1];
     b89 = at[iscan][ifov][0];

     blat = lat[iscan][ifov];
     blon = lon[iscan][ifov];

     snowc = snow[iscan][ifov];

/*--------------------------------*
 * Compute SWE 
 *--------------------------------*/
     if(snowc <= 0)
       swe1 = snowc;
     else if(snowc > 0) 
     {
       SI31 = a23 - a31;
       SI89 = a31 - b89;
       SI89_1 = a23 - b89;
       swe1 = COEA*SI31 + COEB;
       if(SI89 >= (SI31*8.0) ) swe1 = COEC*SI89_1 + COED;
     }

/*--------------------------------*
 * Screening 
 *--------------------------------*/
     /* check Antarctic region for multi-year-ice */
     if(snowc > 0 && blat < SWE_LAT_LIMIT && blat > MISSING_GEO)
       swe1 = INDETERM_MULTI_YEAR_ICE;     

     /* check Himalayas region for multi-year-ice */
     else if(snowc > 0 && blat > 26.25 && blat < 41.25 && blon > 78.75 && blon < 105.75)   
       swe1 = INDETERM_MULTI_YEAR_ICE;     

     /* check Greenland region for multi-year-ice */
     else if(snowc > 0 && blat > 79. && blon > -68. && blon < -8.) 
       swe1 = INDETERM_MULTI_YEAR_ICE;     
     else if(snowc > 0 && blat > 72. && blat < 79. && blon > -73.5 && blon < -16.) 
       swe1 = INDETERM_MULTI_YEAR_ICE;     
     else if(snowc > 0 && blat > 67. && blat < 72. && blon > -56. && blon < -20.) 
       swe1 = INDETERM_MULTI_YEAR_ICE;     
     else if(snowc > 0 && blat > 60. && blat < 67. && blon > -56. && blon < -32.) 
       swe1 = INDETERM_MULTI_YEAR_ICE;     

     if(swe1 > limit_Prod.SWE_upper)
       swe1 = limit_Prod.SWE_upper;

     if(swe1 > 0)
       swe[iscan][ifov] = swe1 * SWE_SCAL;
     else
       swe[iscan][ifov] = swe1;

   }  /* end of ifov loop */

} /* end of swe_cal.c */ 
