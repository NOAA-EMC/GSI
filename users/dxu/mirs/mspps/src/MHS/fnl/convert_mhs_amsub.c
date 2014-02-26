/***********************************************************************
 *  Program Name      : convert_mhs_amsub.c
 *  Type              : Subroutine
 *  Function          : Program converts MHS channel-2 (157 GHz) and 
 *			channel-5 (190.311 GHz) to AMSU-B channel-2 
 *			(150 GHz) and channel-5 (183.31 +/- 7.00 GHz)
 *			using empirical (regression) equations.
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : rmhs_whdfeos.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   03/04/2005      v1.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/****************************************************/
#define   A20_LAND        -0.691781
#define   A21_LAND        1.00302 
#define   A20_OCEAN       0.096
#define   A21_OCEAN       0.992386

#define   A50_LAND        0.03 
#define   A51_LAND        1.0005 
#define   A50_OCEAN       0.052 
#define   A51_OCEAN       1.0040

/****************************************************/
void convert_mhs_amsub(short int numscan)
{
   char		lstag;
   short int    iscan, ifov; 

   for (iscan = 0; iscan < numscan; iscan++)
     for (ifov = 0; ifov < NUMSPOT_M; ifov++)
     {
       lstag = stype[iscan][ifov];

       if(lstag == 0)  /* ocean */
       {
         at150[iscan][ifov] = A20_OCEAN + A21_OCEAN * at[iscan][ifov][1];
         at176[iscan][ifov] = A50_OCEAN + A51_OCEAN * at[iscan][ifov][4];
       }
       else   /* land and coast */
       {
         at150[iscan][ifov] = A20_LAND + A21_LAND * at[iscan][ifov][1];
         at176[iscan][ifov] = A50_LAND + A51_LAND * at[iscan][ifov][4];
       }

     }

} /* end of convert_mhs_amsub.c */ 
