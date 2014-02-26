/***********************************************************************
 *  Program Name      : icewp.c
 *  Type              : Subroutine
 *  Function          : Program calculates ice water path (100 * kg/m^2) 
 *			(global)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : cal_pred_ocean.c, cal_pred_land.c
 *  Called by         : calprod.c
 *  05/09/2007        : algorithm enhancement (ccr3135)
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/*************************************************/
#define  LAT_LIMIT_NH  80
#define  LAT_LIMIT_SH  -75

/*************************************************/
void cal_pred_ocean();
void cal_pred_land();

/*************************************************/
void icewp(short int iscan)
{
    char	lstag, lstag_A;
    short int   ifov, ilat, ilon; 
    short int   hr, min, i, ii, indx;
    float 	b89, b150, b182, b180, b176;
    float	omega, omega89, omega150; 
    float	omegan, ratio, mu;
    float	coef_a, coef_b, coef_c, coef_d;
    float	coef_iwp_a, coef_iwp_b, coef_iwp_c;
    float 	blza;	
    float	biwp, bde;
    float       alat, alon;
    float       ts, bsnow, aice, aclw;
    float       lza0,lza1, b89_0, b89_1, b150_0, b150_1;
    float       interpt;

    if(num_avn < 0) 
      return;

/*----------------------------*
 * Define parameters 
 *----------------------------*/
    for (ifov = 0; ifov < NUMSPOT_B; ifov++)
    {
      pred89 = MISSING;
      pred150 = MISSING;    
      omega89 = MISSING;
      omega150 = MISSING;
      omegan = MISSING;
      biwp = MISSING;
      bde = MISSING;  /* de = effective particle size */

      iwp_flag[iscan][ifov] = 0;  // Set to 1 when new IWP is calculated

      lstag_A = stype_AB[iscan][ifov]; // new variable addad for coastal check over AMSU-A FOV

      lstag = stype[iscan][ifov];

      b89 = at[iscan][ifov][0];
      b150 = at[iscan][ifov][1];
      b182 = at[iscan][ifov][2]; 
      b180 = at[iscan][ifov][3]; 
      b176 = at[iscan][ifov][4]; 
      blza = lza[iscan][ifov]; 

      bsnow = snow[iscan][ifov];
      aice = sice_AB[iscan][ifov];

      if(clw_AB[iscan][ifov] > 0)
        aclw = clw_AB[iscan][ifov]/CLW_SCAL;
      else
        aclw = clw_AB[iscan][ifov];

      alat = lat[iscan][ifov];
      alon = lon[iscan][ifov];

      ilat = 90.0 - lat[iscan][ifov];
      ilon = 180.0 + lon[iscan][ifov];
      if(ilat < 0)
         ilat = 0;
      if(ilat > NUMROW_AVN - 1)
         ilat = NUMROW_AVN - 1;
      if(ilon < 0)
         ilon = 0;
      if(ilon > NUMCOL_AVN - 1)
         ilon = NUMCOL_AVN - 1;

/*----------------------------*
 * Find the corresponding AVN
 * data to interpulate based on
 * the time of the scan line 
 *----------------------------*/
      hr = hour[iscan];
      min = minute[iscan];
      
      indx = -1;
      if(num_avn == 2)
      {
        ii = 0;
        indx = 0;
      }
      else if(num_avn == 3)
      {
        if(avnhr[1] == 24 && avnhr[2] == 3 && hr < 3)
        {
          ii = 1;
          indx = 0;
        }
        else
        {
          for(i = 0; i < 2; i++)
          {
            if(hr >= avnhr[i] && hr < avnhr[i+1])
            {
              ii = i;
              indx = 0;
            }
          }
        }
      }

      if(indx == -1)
      {
        printf("icewp/Point %d %d doesn't have corresponding AVN data\n",iscan,ifov);
        printf("%d %d %d %d %d\n",hr,min,avnhr[0],avnhr[1],avnhr[2]);
        return;
      }

      if(avnhr[ii] == 24 && hr < 3)
        interpt = (hr + min/60.)/3.;
      else
        interpt = (hr - avnhr[ii] + min/60.)/3.;

      ts = ts_avn[ii][ilat][ilon] + (ts_avn[ii+1][ilat][ilon]-ts_avn[ii][ilat][ilon])*interpt;

      if(lstag == 0)
         cal_pred_ocean(iscan, ifov, ii);
 
      else if(lstag == 1 || lstag == 2)
         cal_pred_land(iscan, ifov); 
  
      if(pred89 != MISSING && pred150 != MISSING)
      {
         if(abs(blza) > 0 && abs(blza) < 10)
         {
            lza0=0.0;
            lza1=10.0;
            b89_0=183.073-0.649864*b89+0.00355388*b89*b89;
            b150_0=89.4492+0.133525*b150+0.00193974*b150*b150;
            b89_1=168.617-0.526129*b89+0.00329590*b89*b89;
            b150_1=85.7358+0.169666*b150+0.00185847*b150*b150;
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0;
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0;
         }
         else if(abs(blza) >= 10 && abs(blza) < 20)
         {
            lza0=10.0;
            lza1=20.0;
            b89_0=168.617-0.526129*b89+0.00329590*b89*b89;
            b150_0=85.7358+0.169666*b150+0.00185847*b150*b150;
            b89_1=135.886-0.239320*b89+0.00268872*b89*b89;
            b150_1=72.1034+0.300571*b150+0.00156526*b150*b150;
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0;
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0;
         }
         else if(abs(blza) >= 20 && abs(blza) < 30)
         {
            lza0=20.0;
            lza1=30.0;
            b89_0=135.886-0.239320*b89+0.00268872*b89*b89;
            b150_0=72.1034+0.300571*b150+0.00156526*b150*b150;
            b89_1=99.8433+0.0911668*b89+0.00196905*b89*b89;
            b150_1=51.6176+0.501623*b150+0.00110930*b150*b150;
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0;
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0;
         }
         else if(abs(blza) >= 30 && abs(blza) < 40)
         {
            lza0=30.0;
            lza1=40.0;
            b89_0=99.8433+0.0911668*b89+0.00196905*b89*b89;
            b150_0=51.6176+0.501623*b150+0.00110930*b150*b150;
            b89_1=52.4938+0.535288*b89+0.000986296*b89*b89;
            b150_1=26.8442+0.753185*b150+0.000528123*b150*b150;
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0;
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0;
         }
         else if(abs(blza) >= 40 && abs(blza) < 50)
         {
            lza0=40.0;
            lza1=50.0;
            b89_0=52.4938+0.535288*b89+0.000986296*b89*b89;
            b150_0=26.8442+0.753185*b150+0.000528123*b150*b150;
            b89_1=7.92203+0.981133*b89-0.0000394*b89*b89;
            b150_1=-2.74337+1.06524*b150-0.000209793*b150*b150;
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0;
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0;
         }
         else if(abs(blza) >= 50)
         {
            b89=7.92203+0.981133*b89-0.0000394*b89*b89;
            b150=-2.74337+1.06524*b150-0.000209793*b150*b150;
         }

         omega89 = (pred89 - b89)/b89;
   	 omega150 = (pred150 - b150)/b150;
	 ratio = omega89/omega150;

         iwp[iscan][ifov] =0.0;
         de[iscan][ifov] = 0.0; 
         if(omega89 > 0.0 && omega150 > 0.0 && b176 < 265) 
         /* conditions for the existance of detectable clouds */
	 {
            mu = cos(blza * PI / 180.);    
	    bde = 0.0;
	    biwp = 0.0;
	    if((ratio > 0.0) && (ratio <= 0.8))  
            /* Calculate the ice particle effective diameters */
	    {
/*----------------------------*
 * Calculate ice particle 
 * effective diameters 
 *----------------------------*/
                coef_a = -0.300323;
                coef_b = 4.30881;
                coef_c = -3.98255;
                coef_d = 2.78323;
                bde = coef_a + coef_b * ratio + coef_c * pow(ratio,2)
                             + coef_d * pow(ratio,3);

                /* Calculate the ice water path   */
                omega=omega89;
                coef_iwp_a = -1.19301;
                coef_iwp_b = 2.08831;
                coef_iwp_c = -0.857469;
                if(bde <= 1.0)
                {
                   omega=omega150;
                   coef_iwp_a = -0.294459;
                   coef_iwp_b = 1.38838;
                   coef_iwp_c = -0.753624;
                }
                if (bde > 0.0)
                {
                    omegan = exp(coef_iwp_a + coef_iwp_b * log(bde)
                              +  coef_iwp_c * pow((log(bde)),2));
                    if(omegan > 0.0)
                     biwp = (omega * bde * 1.e-3 * mu * ICE_DEN / omegan);
                 }
             }  /* end of ratio loop  */
         } /* end of checking omega89 & omega150  */

/* The new coastal extension to the existing algorithm starts here
 Apply the new algorithm over coast AMSU-A FOV or over coast AMSU-B FOV
 and for iwp or de values less than necessary conditions for the existance
 of rain */

         if( (biwp < 0.05 || bde < 0.3 || aclw <= 0.2 ) && (lstag_A == 2 || lstag == 2) )
         {
           if ( ((b182-b176) > -15) &&  ((b182-b180) > -10.0) && ((b89-b150) > 3.0)  )
           {
             mu = cos(blza * PI / 180.);
             biwp = 0.01*(292.49289 - 1.83559*b176 + 0.002554378 *b176*b176 + 23.45537*mu);


/* remove confusion of light or warm rain with clouds */
             if( b176 > 255.0 || b180 > 250.0) biwp = 0.0;
             if (biwp > 0.0) iwp_flag[iscan][ifov] = 1;

            }
         } 

         if(biwp > limit_Prod.IWP_upper)
            biwp = limit_Prod.IWP_upper;
         if(biwp < limit_Prod.IWP_lower)
            biwp = limit_Prod.IWP_lower;

         if(bde > limit_Prod.De_upper)
            bde = limit_Prod.De_upper;
         if(bde < limit_Prod.De_lower)
            bde = limit_Prod.De_lower;

         iwp[iscan][ifov] = biwp * IWP_SCAL + 0.5;
         de[iscan][ifov] =  bde*IWP_SCAL + 0.5;

/*----------------------------*
 * Screening 
 *----------------------------*/
         if(bsnow > 0 && (lstag == 1 || lstag == 2))    
           /*  no retrieval over surface covered with snow */
         {
            iwp[iscan][ifov] = INDETERM_SNOW;
            de[iscan][ifov] = INDETERM_SNOW;
         }
         else if(aice > 0 && lstag == 0)  
            /* no retrieval over sea ice  */
         {
            iwp[iscan][ifov] = INDETERM_SICE;
            de[iscan][ifov] = INDETERM_SICE;
         }
         else if(alat > LAT_LIMIT_NH)
         {
            iwp[iscan][ifov] = INDETERM;
            de[iscan][ifov] = INDETERM;
         }
         else if(alat < LAT_LIMIT_SH)
         {
            iwp[iscan][ifov] = INDETERM;
            de[iscan][ifov] = INDETERM;
         }
         else if(ts < 269)  
           /* high latitude frozen surface undetected by snow and sea ice  */  
         {
            iwp[iscan][ifov] = INDETERM_FROZEN;
            de[iscan][ifov] = INDETERM_FROZEN;
         }
         else if(lstag == 1 && b89 < b150 && bde > 0)
         {
            iwp[iscan][ifov] = INDETERM;
            de[iscan][ifov] = INDETERM;
         }
         else if(lstag == 0 && b89 < b150  && bde > 0)
         {
            iwp[iscan][ifov] = INDETERM;
            de[iscan][ifov] = INDETERM;
         }
         else if((ts - b176) < 10 && lstag > 0 && iwp[iscan][ifov] > 0.0) 
           /* possibly over desert */
         {                 
            iwp[iscan][ifov] = INDETERM_DESERT;
            de[iscan][ifov] = INDETERM_DESERT;
         }
         else if((b89-b150) < 0 && alat > 22. && alat < 47.
                 && alon > 65. && alon < 112 && iwp[iscan][ifov] > 0.0) 
           /* special check over the Himalayas Mountains */
         {               
            iwp[iscan][ifov] = INDETERM_ELEV;
            de[iscan][ifov] = INDETERM_ELEV;
         }

      }/* end of checking pred89 & pred150  */	

    } /* end of loop ifov  */

} /* end of icewp.c */
