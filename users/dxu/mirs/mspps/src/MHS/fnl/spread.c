/***********************************************************************
 *  Program Name      : spread.c
 *  Type              : Subroutine
 *  Function          : Program spreads data from AMSU-A swath size
 *                      to MHS swath size
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 *   11/05/07        Added switch the mismatched AMSUA/MHS scans 
 *                   when spread AMSUA scans 
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/***************************************************************/ 
void spread(TOTNUM_OF_SCAN numscan_A, TOTNUM_OF_SCAN numscan_M)
{

       short int   ifov, iscan, ich; 
       short int   indx_iscan = -3, indx_ifov;
       short int   i,j,k;


       /***************************************************
         First added the switched scan numbers to index and init
         the spead_scans corresponding to the mismatched MHS scan
         with the MISSING  
        *****************************************************/

       if (switch_scnnum >0){

	 indx_iscan = indx_iscan + switch_scnnum;

	 for (k=0;k<switch_scnnum;k++){
	   for(j=0;j<NUMSPOT_M;j++){
	     lza_AB[k][j]=-999;
	     stype_AB[k][j]=MISSING_CHAR;
	     for(ich = 0; ich < NUMCHAN_A; ich++){
	       at_AB[k][j][ich] = MISSING;
	     }
	   }
	 } 
       }  /*if switch_scnnum >0  */

/*---------------------------------------------*
 * Each AMSU-A FOV corresponds to 3-FOV by
 * 3-scanline of MHS, i.e. 9 MHS FOVs
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
 * If MHS swath is longer than 3 times of 
 * AMSU-A swath, set the values at the gap as 
 * missing (-99)
 *---------------------------------------------*/
       if(numscan_A * 3 + switch_scnnum < numscan_M)
       {
         for(iscan = numscan_A*3+ switch_scnnum; iscan < numscan_M; iscan++)
         {
           for(ifov = 0; ifov < NUMSPOT_M; ifov++)
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
