/***************************************************************************
 *  Program Name      : snowcover.c
 *  Type              : Subroutine
 *  Function          : Program calculates snow cover (land only) 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : calprod.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *   1/18/2001      v2.1       Replace glacial ice by snow (50 -> 100)
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************************/
void snowcover(short int iscan)
{
    char	 lstag;
    short int    ifov;
    float        a23, a31, b89, a50;
    float        sc31, sc89, scat, tt, fsnow, df3;
    float	 alat, alon;

    for (ifov = 0; ifov < NUMSPOT_A; ifov++)
    {
      alat = lat[iscan][ifov];
      alon = lon[iscan][ifov];

      lstag = stype[iscan][ifov];

      if (lstag == 1) /* over land */
      { 
        fsnow = 0; 

/*---------------------------------------------------------*
 * Perform AT test, no asymmetry adjustment here 
 *---------------------------------------------------------*/
        a23 = at[iscan][ifov][0] ;
        a31 = at[iscan][ifov][1] ;
        a50 = at[iscan][ifov][2] ;
        b89 = at[iscan][ifov][14] ;

        df3 = 10.2+0.036*a23-0.074*a50;

        tt = 168.0 + 0.49*b89;
        sc89 = a23 - b89 - 3.;
        sc31 = a23 - a31 - 2.;
        scat = sc89;
        if(b89 < 255 && scat < sc31)
           scat = sc31;
            
/*---------------------------------------------------------*
 * Perform glacial ice check in Greenland and Antarctic
 *---------------------------------------------------------*/
        if(sc31 < 3 && a23 <= 215) 
	{
          if(alat < -62. || (alat >= 59.6 && alat <= 83.4 && alon >= -71.8 && alon <= -14.1)) 
             fsnow = 100*SNOW_SCAL;
        }
        else if(scat >= 1)  
        {
          fsnow = 100*SNOW_SCAL;

/*---------------------------------------------------------*
 * Remove confusions from certain sources 
 *---------------------------------------------------------*/
          if(a23 >= 262 || a23 >= tt)
             fsnow = 0;   /* precipitation */
          if(df3 <=0.4) 
             fsnow = 0;   /* cold desert */
        }
         
      } 

      else   /* over ocean and coast */
        fsnow = INDETERM;

      snow[iscan][ifov] = fsnow;
 
    }  /* end of ifov loop */

}  /* end of snowcover.c */    
