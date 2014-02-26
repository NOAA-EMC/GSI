/**************************************************************************
 *  Program Name      : locate_ssmi.c
 *  Type              : Subroutine
 *  Function          : Program locates the closest point in the SSMI 
 * 			Rain file to a given spot (lat, lon) for using 
 *			climate data
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : rainrate.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/****************************************************************/
void locate_ssmi(float alon, float alat)
{
   int i = 0, j = 0, ii = 0;

/*-----------------------------------------------------------*
 * Locate i_ssmi, longitude of SSMI rain from 0E to 0W 
 *-----------------------------------------------------------*/
   if (alon<-0.5)
   {
     while(fabs(alon-ii) >  0.5)
      {
       ii=ii-1;
        if(ii < -180)
          {
           printf (" Something went wrong when locating SSMI Rain.");
           exit(111);          
	  }
      }    /* end while */

     i_ssmi=360+ii;

   }   /* alon <-0.5 */
   else
   {
     while(fabs(alon-i) > 0.5)
       {
        i=i+1;
        if(i > 180)
	  {
           printf (" Something went wrong when locating SSMI rain.");
           exit(222); 
	  }
       }

     i_ssmi=i;

   } /* end else */

/*-----------------------------------------------------------*
 * Locate j_ssmi, latitude of SSMI rain from 90S to 90N 
 *-----------------------------------------------------------*/
    while(fabs(alat - j + 90) >0.5)
      {
        j=j+1;
        if (j >180)
         {
           printf (" Something went wrong when locating SSMI rain.");
           exit(333);
	 }
      }  /* end while */

    j_ssmi=j;

}  /* end of locate_ssmi.c */
