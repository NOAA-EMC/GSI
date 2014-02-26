/***********************************************************************
 *  Program Name      : snowcover.c
 *  Type              : Subroutine
 *  Function          : Program calculates snow cover (land)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : calprod.c
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/******************************************************/
#define  A0   145.068
#define  A1   0.240368
#define  A2   0.180854
#define  A3   11.51943

/******************************************************/
void snowcover(short int iscan)
{
    char	 lstag;
    short int    ifov;
    float        a23, a31, a89, b89, a50;
    float        sc31, sc89, scat, tt, fsnow, df3;
    float	 alat, alon;
    float	 acoslza;

    for (ifov = 0; ifov < NUMSPOT_B; ifov++)
    {
      alat = lat[iscan][ifov];
      alon = lon[iscan][ifov];
      lstag = stype_snow[iscan][ifov];
      acoslza = cos(lza_AB[iscan][ifov] * PI / 180.);
      
      a23 = at_AB[iscan][ifov][0];
      a31 = at_AB[iscan][ifov][1];
/*      a50 = at_AB[iscan][ifov][2];
      a89 = at_AB[iscan][ifov][14];
*/
      b89 = at[iscan][ifov][0];

/* AMSU-A Channel-3 BT is derived from AMSU-A Channel-1 and -2 BT through a linear regression equation, 11/18/03 */
/* AMSU-A Channel-15 BT is replaced by AMSU-B Channel-1 BT */

      a50 = A0 + A1 * a23 + A2 * a31 + A3/acoslza;
      a89 = b89;

/*------------------------------------*
 * Land FOV
 *------------------------------------*/
      if (lstag == 1) /* over land */
      { 
        fsnow = 0; 

/*------------------------------------*
 * Perform AT test; no asymmetry
 * adjustment over land
 *------------------------------------*/

        df3 = 10.2+0.036*a23-0.074*a50;
        tt = 168.0 + 0.49*b89;
        sc89 = a23 - b89 - 3.;
        sc31 = a23 - a31 - 2.;
        scat = sc89;

/*------------------------------------*
 * Glacial ice (taken as snow) check 
 * only in Greenland and Antarctic
 *------------------------------------*/
        if(sc31 < 3 && a23 <= 215) 
	{
          if(alat < -62. || (alat >= 59.6 && alat <= 83.4 && alon >= -71.8 && alon <= -14.1))  
             fsnow = 100*SNOW_SCAL;
        }
        else if(sc89 >= 1)
        {
          fsnow = 100*SNOW_SCAL;

/*------------------------------------*
 * Remove confusions from some sources
 *------------------------------------*/
          if(a23 >= 262 || a23 >= tt)
             fsnow = INDETERM_RAIN;
/*          if(df3 <=0.4) 
             fsnow = INDETERM_DESERT;
*/
        }
         
      } 

      else if (lstag == 2)  /* over coast */
      { 
        fsnow = 0; 

        df3 = 10.2+0.036*a23-0.074*a50;
        tt = 168.0 + 0.49*a89;
        sc89 = a23 - a89 - 3.;
        sc31 = a23 - a31 - 2.;
        scat = sc89;
        
/*------------------------------------*
 * Glacial ice (taken as snow) check 
 * only in Greenland and Antarctic
 *------------------------------------*/
        if(sc31 < 3 && a23 <= 215) 
	{
          if(alat < -62. || (alat >= 59.6 && alat <= 83.4 && alon >= -71.8 && alon <= -14.1))  
             fsnow = 100*SNOW_SCAL;
        }
        else if(scat >= 1)
        {
          fsnow = 100*SNOW_SCAL;

/*------------------------------------*
 * Remove confusions from some sources
 *------------------------------------*/
          if(a23 >= 262 || a23 >= tt)
             fsnow = INDETERM_RAIN;   
/*          if(df3 <=0.4) 
             fsnow = INDETERM_DESERT;   
*/

        }
         
      } 

      else 
        fsnow = INDETERM;

      snow[iscan][ifov] = fsnow;
 
    }  /* end of ifov loop */

}  /* end of snowcover.c */    
