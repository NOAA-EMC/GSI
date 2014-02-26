/***********************************************************************
 *  Program Name      : snowcover.c
 *  Type              : Subroutine
 *  Function          : Program calculates snow cover (land)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : calprod.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 *   12/20/2000      v2.1     1. Replace glacial ice with snow (50->100)
 *			      2. Add explicit flags for rain and desert
 *   11/28/2001               3. i) Use AMSU-A 89 GHz for snow detection
 *                               on coast and MHS 89 GHz for snow
 *                               detection on land;
 *                               ii) Use only the difference between
 *                               channels 23 GHz and 89 GHz for snow
 *                               detection except for glacial snow for
 *                               which the difference between 23 GHz
 *                               and 31 GHz is used.
 *                               iii) Use a new land/sea mask which
 *                               includes very few inland waterbodies
 *                               to minimize the coastal effect on snow
 *                               computation.
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/******************************************************/
#define   A0a       3.37
#define   A1a       0.595
#define   A2a       1.582

#define   A0b      -1.319
#define   A1b      -1.555
#define   A2b      -1.55

/******************************************************/
void snowcover(short int iscan)
{
    char	 lstag;
    short int    ifov;
    float        a23, a31, a89, a50, a52, a53, a53L;
    float	 b89, b150, b180;
    float        sc31, sc89, sc150, sc180, scat, tt, fsnow, df3;
    float	 blat, blon, acoslza;
    float        A0, A1, A2;

    for (ifov = 0; ifov < NUMSPOT_M; ifov++)
    {
      blat = lat[iscan][ifov];
      blon = lon[iscan][ifov];
      lstag = stype_snow[iscan][ifov];
      
      a23 = at_AB[iscan][ifov][0];
      a31 = at_AB[iscan][ifov][1];
      a50 = at_AB[iscan][ifov][2];
      a52 = at_AB[iscan][ifov][3];
      a53 = at_AB[iscan][ifov][4];
      acoslza = cos(lza_AB[iscan][ifov] * PI / 180.);

      a89 = at_AB[iscan][ifov][14];
      b89 = at[iscan][ifov][0];
      b150 = at150[iscan][ifov];
      b180 = at[iscan][ifov][3];

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
          if(blat < -62. || (blat >= 59.6 && blat <= 83.4 && blon >= -71.8 && blon <= -14.1))  
             fsnow = 100*SNOW_SCAL;
        }
        else if(sc89 >= 1)
        {
          fsnow = 100*SNOW_SCAL;

/*------------------------------------*
 * Remove confusions from some sources
 *------------------------------------*/
          if(a23 >= 262 || a23 >= tt)
          {
/* Perform limb correction for AMSU-A ch-5 AT */
            if(a52 > 0 && a53 > 0)
     	    {
              A0 = A0a + A0b * acoslza;
              A1 = A1a + A1b * acoslza;
              A2 = A2a + A2b * acoslza;
              a53L = A0 - A1 * a53+ A2 * a52+ 4 * (1 - acoslza);
            }
    
            sc150 = b89 - b150;
            sc180 = a53L - b180;

            if((a23 < 268) && (sc150 > 3) && (sc180 < -7.0) && (a53L < 250.))
              fsnow = 100*SNOW_SCAL;
            else
              fsnow = INDETERM_RAIN;

          }

          if(df3 <=0.4) 
             fsnow = INDETERM_DESERT;
        }
         
      } 

/*------------------------------------*
 * Coast FOV
 *------------------------------------*/
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
          if(blat < -62. || (blat >= 59.6 && blat <= 83.4 && blon >= -71.8 && blon <= -14.1))  
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
          if(df3 <=0.4) 
             fsnow = INDETERM_DESERT;   

        }
         
      } 

      else 
        fsnow = INDETERM;

      snow[iscan][ifov] = fsnow;
 
    }  /* end of ifov loop */

}  /* end of snowcover.c */    
