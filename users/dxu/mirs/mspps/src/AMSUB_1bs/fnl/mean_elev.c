/***********************************************************************
 *  Program Name      : mean_elev.c
 *  Type              : Subroutine
 *  Function          : Program computes mean of elevation in an FOV
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : gnrt_elev.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************/
void mean_elev(short int iscan, short int ifov)
{
   short int   i, j, indx; 

   elev_mean[iscan][ifov] = 0.;

   if(lonleft < 0 && lonright > NUMX_ELEV - 1)
   {
       for(i = latbot; i < lattop + 1; i++)
       {
         for(j = 0; j < NUMX_ELEV; j++)
         {
           elev_mean[iscan][ifov] = elev_mean[iscan][ifov] + elev_large[i][j];
           indx = indx + 1;
         }
       }
    }

   else if(lonleft < 0)
   {
       for(i = latbot; i < lattop + 1; i++)
       {
	 for(j = 0; j < lonright + 1; j++)
	 {
	   elev_mean[iscan][ifov] = elev_mean[iscan][ifov] + elev_large[i][j];
	   indx = indx + 1;
	 }

	 for(j = NUMX_ELEV + lonleft; j < NUMX_ELEV; j++)
	 {
	   elev_mean[iscan][ifov] = elev_mean[iscan][ifov] + elev_large[i][j];
	   indx = indx + 1;
	 }
       }

   }
              
   else if(lonright > NUMX_ELEV - 1)
   {
       for(i = latbot; i < lattop + 1; i++)
       {
         for(j = lonleft; j < NUMX_ELEV; j++)
         {
	   elev_mean[iscan][ifov] = elev_mean[iscan][ifov] + elev_large[i][j];
	   indx = indx + 1;
	 }

	 for(j = 0; j < lonright - NUMX_ELEV + 1; j++)
	 {
	   elev_mean[iscan][ifov] = elev_mean[iscan][ifov] + elev_large[i][j];
	   indx = indx + 1;
	 }

       }
   }

   else
   {
       for(i = latbot; i < lattop + 1; i++)
       {
         for(j = lonleft; j < lonright + 1; j++)
         {
           elev_mean[iscan][ifov] = elev_mean[iscan][ifov] + elev_large[i][j];
           indx = indx + 1;
         }
       }
   }

   elev_mean[iscan][ifov] = elev_mean[iscan][ifov]/indx;

}  /* end of mean_elev.c */
