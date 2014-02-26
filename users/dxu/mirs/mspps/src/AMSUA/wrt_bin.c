/***************************************************************************
 *  Program Name      : wrt_bin.c 
 *  Type              : Subroutine
 *  Function          : Program writes to the binary swath file 
 *  Input Files       : None
 *  Output Files      : NPR.AAOP.NK.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.bin
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : None
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   1/25/2001      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"             
#include "ESWATH.h"
#include "EBINARY.h"

/*****************************************************************/
void wrt_bin(char *output_file,TOTNUM_OF_SCAN  numscan, short int inx_orb)
{
  short int i, j;
  long int counter;
  FILE  *fp1;

  if ((fp1=fopen(output_file,"wb"))==NULL)
  {
     fprintf(stderr,"Error opening file %s.", output_file);
     exit(8);
  }

  header[0] = numscan;
  header[1] = NUMSPOT_A;
  header[2] = orbit_start_time;
  header[3] = orbit_end_time;
  for (i=4;i<NUM_HEADER;i++){
    header[i] = 0.0;
  }

  if((fwrite((void *)header, sizeof(long int), NUM_HEADER, fp1)) != NUM_HEADER){
  printf("Error writing to file %s\n", output_file);
  exit(9);
  }

  counter=NUMSPOT_A*numscan;

  for(i = 0; i < numscan; i++)
      for(j = 0; j < NUMSPOT_A; j++)
      {
         if(tpw[i][j] >= 0)
            b_tpw[i][j] = (float) tpw[i][j]/TPW_SCAL;
         else
            b_tpw[i][j] = -99.0;

         if(clw[i][j] >= 0)
            b_clw[i][j] = (float) clw[i][j]/CLW_SCAL;
         else
            b_clw[i][j] = -99.0;

         if(sice[i][j] >= 0)
            b_sice[i][j] = (float) sice[i][j]/SICE_SCAL;
         else
            b_sice[i][j] = -99.0;

         if(ts[i][j] >= 0)
            b_ts[i][j] = (float) ts[i][j]/TS_SCAL;
         else
            b_ts[i][j] = -99.0;

         if(em1[i][j] >= 0)
            b_em1[i][j] = (float) em1[i][j]/EM_SCAL;
         else
            b_em1[i][j] = -99.0;

         if(em2[i][j] >= 0)
            b_em2[i][j] = (float) em2[i][j]/EM_SCAL;
         else
            b_em2[i][j] = -99.0;

         if(em3[i][j] >= 0)
            b_em3[i][j] = (float) em3[i][j]/EM_SCAL;
         else
            b_em3[i][j] = -99.0;

         out_array[0] = b_time[i][j];
         out_array[1] = lat[i][j];
         out_array[2] = lon[i][j];
         out_array[3] = b_tpw[i][j];
         out_array[4] = b_clw[i][j];
         out_array[5] = b_sice[i][j];
         out_array[6] = b_ts[i][j];
         out_array[7] = b_em1[i][j];
         out_array[8] = b_em2[i][j];
         out_array[9] = b_em3[i][j];
	 out_array[10] = lza[i][j];
/*         out_array[10] = b_rr[i][j];
         out_array[11] = b_snow[i][j];
*/

         if((fwrite((void *)out_array, sizeof(float), NUM_PROD, fp1)) != NUM_PROD ){
         printf("Error writing to file %s\n", output_file);
	 exit(9);
	 }

      }

  fclose(fp1);
  
} /* End of wrt_bin.c */
