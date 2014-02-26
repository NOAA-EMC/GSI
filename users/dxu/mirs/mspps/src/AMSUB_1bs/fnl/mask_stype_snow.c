/***********************************************************************
 *  Program Name      : mask_stype_snow.c
 *  Type              : Subroutine
 *  Function          : Program generates surface type field from a 1/6
 *                      degree land/sea mask from University of Bristol
 *                      which has been filtered for waterbodies occupying 
 *			less or equal to 1000 connecting pixels
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

#define   MAX_LONCORR   200

/********************************************************/
void mask_stype_snow(TOTNUM_OF_SCAN numscan)
{
   const       float lon0 = 180.; 
   const       float lat0 = 90.;

   short int   iscan, ifov, i, j, indx;
   short int   latbot_mask, lattop_mask;
   short int   lonleft_mask, lonright_mask;
   float       ang[NUMSPOT_B+1], angle = 0, angle1 = 0;
   float       fovsize[NUMSPOT_B], fov_size, loncorr, sum;
   float       alat, alon;

/*-------------------------------------------------*
 * Calculate size of FOV
 *-------------------------------------------------*/
   for (i = 0; i < NUMSPOT_B+1; i++)
   {
      angle = PI * SCAN_ANG_B * (i - NUMSPOT_B/2) / 180;
      angle1 = (REARTH + RSAT) * sin(angle) / REARTH;
      angle1 = atan(angle1 / sqrt(1 - angle1 * angle1));
      ang[i] = (angle1 * 180 / PI) - SCAN_ANG_B * (i - NUMSPOT_B/2);
      if (i > 0) fovsize[i - 1] = fabs(ang[i] - ang[i-1]);
   }

/*-------------------------------------------------*
 * Check if the scanline is good
 *-------------------------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
          for(ifov = 0; ifov < NUMSPOT_B; ifov++)
          {

	     fov_size = fovsize[ifov];
             alat = lat[iscan][ifov]; 
             alon = lon[iscan][ifov]; 

/*-------------------------------------------------*
 * Determine the coordinates of FOV boundaries
 * in the mask data coordinate system
 *-------------------------------------------------*/
	     loncorr = fabs(1/cos(PI * alat / 180));
             if(loncorr > MAX_LONCORR)
               loncorr = MAX_LONCORR;

	     lonleft_mask = (alon + lon0 - fov_size * loncorr) * MASK_RESOLUTION_SNOW;
	     lonright_mask = (alon + lon0 + fov_size * loncorr) * MASK_RESOLUTION_SNOW;
             
             lattop_mask = MAP_ROWS_SNOW - 1 - (alat + lat0) * MASK_RESOLUTION_SNOW + latbox_up[ifov];
	     latbot_mask = lattop_mask - latbox_down[ifov];

             if(lattop_mask > MAP_ROWS_SNOW - 1)
               lattop_mask = MAP_ROWS_SNOW - 1;
	     if(latbot_mask > MAP_ROWS_SNOW - 1)
	       latbot_mask = MAP_ROWS_SNOW - 1;
	     if(lattop_mask < 0)
	       lattop_mask = 0;
	     if(latbot_mask < 0)
	       latbot_mask = 0;
                
/*-------------------------------------------------*
 * Determine surface type of an FOV
 *-------------------------------------------------*/
             sum = 0.;
             indx = 0;

   	     if(lonleft_mask < 0 && lonright_mask > MAP_COLS_SNOW - 1)
             {
                for(i = latbot_mask; i < lattop_mask + 1; i++)
       		{
         	   for(j = 0; j < MAP_COLS_SNOW; j++)
         	   {
                      if(mask[i][j] == 1)
		         sum = sum + 1.;
           	      indx = indx + 1;
                   }
                }
             }

   	     else if(lonleft_mask < 0)
             {
       	        for(i = latbot_mask; i < lattop_mask + 1; i++)
       	     	{
         	   for(j = 0; j < lonright_mask + 1; j++)
         	   {
                      if(mask[i][j] == 1)
                         sum = sum + 1.;
                      indx = indx + 1;
                   }

         	   for(j = MAP_COLS_SNOW + lonleft_mask; j < MAP_COLS_SNOW; j++)
                   {
                      if(mask[i][j] == 1)
                         sum = sum + 1.;
                      indx = indx + 1;
                   }
                }
             }

             else if(lonright_mask > MAP_COLS_SNOW - 1)
             {
                for(i = latbot_mask; i < lattop_mask + 1; i++)
                {
                   for(j = lonleft_mask; j < MAP_COLS_SNOW; j++)
                   {
                      if(mask[i][j] == 1)
                         sum = sum + 1.;
                      indx = indx + 1;
                   }

                   for(j = 0; j < lonright_mask - MAP_COLS_SNOW + 1; j++)
         	   {
                      if(mask[i][j] == 1)
                         sum = sum + 1.;
                      indx = indx + 1;
                   }
                }
             }

            else
            {
               for(i = latbot_mask; i < lattop_mask + 1; i++)
               {
                  for(j = lonleft_mask; j < lonright_mask + 1; j++)
                  {
                      if(mask[i][j] == 1)
                         sum = sum + 1.;
                      indx = indx + 1;
                   }
               }
            }  

            sum = sum/indx;
            
            if(sum >= 0.99) 		  /* land */ 
               stype_snow[iscan][ifov] = 1;
            else if(sum <= 0.01)  	  /* ocean */
               stype_snow[iscan][ifov] = 0;
	    else  			  /* coast */
               stype_snow[iscan][ifov] = 2;

         }  /* end of ifov */

    }     /* end of iscan loop */

}  /* end of mask_stype_snow.c */
