/***************************************************************************
 *  Program Name      : read_corr_table.c
 *  Type              : Subroutine
 *  Function          : Program reads in the antenna temperature correction
 *			table 
 *  Input Files       : corr_table.dat 
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : AMA2HDF.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
void read_corr_table(FILE *fparm)
{
 
   float         coef[NUMCHAN_A];
   short int     i, j, i1, ip=0, file_num=0;
  
   while (ip !=30)
   {
      fscanf( fparm,"%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f", 
                 &coef[0], &coef[1], &coef[2], &coef[3], &coef[4],
                 &coef[5], &coef[6], &coef[7], &coef[8], &coef[9],
                 &coef[10], &coef[11], &coef[12], &coef[13], &coef[14]);

      for(i=0; i<NUMCHAN_A; i++)
         corr[ip][i] = coef[i];

      ip++;
    }

   fclose(fparm);

}  /* end of read_corr_table.c */
