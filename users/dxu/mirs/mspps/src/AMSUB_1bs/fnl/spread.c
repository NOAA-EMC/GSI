/***********************************************************************
 *  Program Name      : spread.c
 *  Type              : Subroutine
 *  Function          : Program spreads data from AMSU-A swath size
 *                      to AMSU-B swath size
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/***************************************************************/ 
void spread(TOTNUM_OF_SCAN numscan_A, TOTNUM_OF_SCAN numscan_B)
{

       short int   ifov, iscan, ich; 
       short int   indx_iscan = -3, indx_ifov;
       short int   i,j;

/*---------------------------------------------*
 * Each AMSU-A FOV corresponds to 3-FOV by
 * 3-scanline of AMSU-B, i.e. 9 AMSU-B FOVs
 *---------------------------------------------*/
       for(iscan = 0; iscan < numscan_A; iscan++)
       {
           indx_iscan += 3;
           indx_ifov = -3;

           for(ifov = 0; ifov < NUMSPOT_A; ifov++)
           {
               indx_ifov += 3;
               for(i = indx_iscan; i < indx_iscan + 3; i++)
                  for(j = indx_ifov; j < indx_ifov + 3; j++)
		  {
                     clw_AB[i][j] = clw_A[iscan][ifov]; 
                     tpw_AB[i][j] = tpw_A[iscan][ifov]; 
                     sice_AB[i][j] = sice_A[iscan][ifov]; 
                     lza_AB[i][j] = lza_A[iscan][ifov]; 
                     stype_AB[i][j] = stype_A[iscan][ifov];

                     for(ich = 0; ich < NUMCHAN_A; ich++)
                        at_AB[i][j][ich] = at_A[iscan][ifov][ich]; 
 
                  }

           } 
       }

/*---------------------------------------------*
 * If AMSU-B swath is longer than 3 times of 
 * AMSU-A swath, set the values at the gap as 
 * missing (-99)
 *---------------------------------------------*/
       if(numscan_A * 3 < numscan_B)
       {
         for(iscan = numscan_A*3; iscan < numscan_B; iscan++)
         {
           for(ifov = 0; ifov < NUMSPOT_B; ifov++)
           {
              clw_AB[iscan][ifov] = MISSING; 
              tpw_AB[iscan][ifov] = MISSING;
              sice_AB[iscan][ifov] = MISSING; 
              lza_AB[iscan][ifov] = MISSING; 

              for(ich = 0; ich < NUMCHAN_A; ich++)
                 at_AB[iscan][ifov][ich] = MISSING; 

           }
	 }
       }

} /* end of spread.c */ 
