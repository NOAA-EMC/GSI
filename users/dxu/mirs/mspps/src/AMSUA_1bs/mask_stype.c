/***********************************************************************
 *  Program Name      : mask_stype.c
 *  Type              : Subroutine
 *  Function          : Program generates surface type field from CLAVR 
 *			8 km (1/16 degree) land/sea mask.
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 *   02/13/2006      v2.1      Change from using 1/6 deg mask to 1/16 deg mask
 ***********************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*************************************************************/
void mask_stype(short int numscan)
{
   const       float lon0 = 180.; 
   const       float lat0 = 90.;

   short int   iscan, ifov, i, j;
   short int   lat_pts, latbot_mask, lattop_mask;
   short int   lonleft_mask, lonright_mask;
   long int    indx;
   float       ang[NUMSPOT_A+1], angle = 0, angle1 = 0;
   float       fovsize[NUMSPOT_A], fov_size, loncorr, sum;
   float       alat, alon;

/*-------------------------------------------------*
 * Calculate size of FOV
 *-------------------------------------------------*/
   for (i = 0; i < NUMSPOT_A+1; i++)
   {
      angle = PI * SCAN_ANG_A * (i - NUMSPOT_A/2) / 180;
      angle1 = (REARTH + RSAT) * sin(angle) / REARTH;
      angle1 = atan(angle1 / sqrt(1 - angle1 * angle1));
      ang[i] = (angle1 * 180 / PI) - SCAN_ANG_A * (i - NUMSPOT_A/2);
      if (i > 0) fovsize[i - 1] = fabs(ang[i] - ang[i-1]);
   }

/*-------------------------------------------------*
 * Check if the scanline is good   
 *-------------------------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
          for(ifov = 0; ifov < NUMSPOT_A; ifov++)
          {

	     fov_size = fovsize[ifov];
             alat = lat[iscan][ifov]; 
             alon = lon[iscan][ifov]; 

/*-------------------------------------------------*
 * Determine the coordinates of FOV boundaries 
 * in the mask data coordinate system 
 *-------------------------------------------------*/
	     loncorr = fabs(1/cos(PI * alat / 180));
	     lonleft_mask = (alon + lon0 - fov_size * loncorr) * MASK_RESOLUTION;
	     lonright_mask = (alon + lon0 + fov_size * loncorr) * MASK_RESOLUTION;
             
             if(lonleft_mask < 0)
               lonleft_mask = 0;
             if(lonright_mask > MAP_COLS - 1)
               lonright_mask = MAP_COLS - 1;

             lat_pts = MAP_ROWS  - (alat + lat0) * MASK_RESOLUTION;
             latbot_mask = lat_pts - latbox_up[ifov];
	     lattop_mask = lat_pts + latbox_down[ifov];

             if(lattop_mask > MAP_ROWS - 1)
               lattop_mask = MAP_ROWS - 1;
	     if(latbot_mask > MAP_ROWS - 1)
	       latbot_mask = MAP_ROWS - 1;
	     if(lattop_mask < 0)
	       lattop_mask = 0;
	     if(latbot_mask < 0)
	       latbot_mask = 0;
                
/*-------------------------------------------------*
 * Determine surface type of an FOV 
 *-------------------------------------------------*/
             sum = 0.;
             indx = 0;

   	     if(lonleft_mask < 0 && lonright_mask > MAP_COLS - 1)
             {
                for(i = latbot_mask; i < lattop_mask + 1; i++)
       		{
         	   for(j = 0; j < MAP_COLS; j++)
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

         	   for(j = MAP_COLS + lonleft_mask; j < MAP_COLS; j++)
                   {
                      if(mask[i][j] == 1)
                         sum = sum + 1.;
                      indx = indx + 1;
                   }
                }
             }

             else if(lonright_mask > MAP_COLS - 1)
             {
                for(i = latbot_mask; i < lattop_mask + 1; i++)
                {
                   for(j = lonleft_mask; j < MAP_COLS; j++)
                   {
                      if(mask[i][j] == 1)
                         sum = sum + 1.;
                      indx = indx + 1;
                   }

                   for(j = 0; j < lonright_mask - MAP_COLS + 1; j++)
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
               stype[iscan][ifov] = 1;
            else if(sum <= 0.01)  	  /* ocean */
               stype[iscan][ifov] = 0;
	    else  			  /* coast */
               stype[iscan][ifov] = 2;

         }  /* end of ifov   */

    }     /* end of iscan loop      */

}  /* end of mask_stype.c  */
