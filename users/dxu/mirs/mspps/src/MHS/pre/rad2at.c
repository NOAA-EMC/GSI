/***********************************************************************
 *  Program Name      : rad2at.c
 *  Type              : Subroutine
 *  Function          : Program converts radiance to antenna temperature
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : getcal.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/***********************************************************/
void rad2at(long int chan)
{

   short int i;

   float c1v3, c2v, pl;
   float plcoef1 = 1.1910659E-05;
   float plcoef2 = 1.438833;

   double temp;

   pl = t_r_central_wave_number[chan];

   c1v3 = plcoef1*pl*pl*pl;
   c2v  = plcoef2*pl;
   
   for(i=0; i<NUMSPOT_M; i++)
   {
       rat[i] = MISSING;
       if(rad[i] != 0) 
       {
          temp =(double)c1v3/rad[i];
          if(temp>0)
          {
             temp = log(temp + 1.0);
             if(temp != 0) rat[i] = c2v/(float)temp;
          }
       }
    }
}  /* end of rad2at.c */
 
