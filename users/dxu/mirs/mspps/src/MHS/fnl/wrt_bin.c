/***************************************************************************
 *  Program Name      : wrt_bin.c 
 *  Type              : Subroutine
 *  Function          : Program writes to the binary swath file 
 *  Input Files       : None
 *  Output Files      : NPR.MHOP.NN.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   1/26/2001      v2.0
 *   10/11/06   Modified to add exit() and output more fields  jzhao(QSS)
 *************************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"
#include "EBINARY.h"

/*****************************************************************/
void wrt_bin(char *output_file, TOTNUM_OF_SCAN  numscan)
{
  short int i, j;
  long int counter;
  FILE  *fp1;

  if ((fp1=fopen(output_file,"wb"))==NULL)
   {
     printf("Error opening bin file %s.\n", output_file);
     exit(8);
   }

  header[0] = numscan;
  header[1] = NUMSPOT_M;
  header[2] = orbit_start_time;
  header[3] = orbit_end_time;
  header[4] = 0;
  header[5] = 0;
  header[6] = 0;

  if((fwrite((void *)header, sizeof(long int), NUM_HEADER, fp1)) != NUM_HEADER)
  printf("Error writing to file %s\n", output_file);

  counter=NUMSPOT_M*numscan;

  for(i = 0; i < numscan; i++)
      for(j = 0; j < NUMSPOT_M; j++)
      {
         if(rr[i][j] >= 0)  
            b_rr[i][j] = (float) rr[i][j]/RR_SCAL;
         else
            b_rr[i][j] = -99.0;

         if(snow[i][j] >= 0)
            b_snow[i][j] = (float) snow[i][j]/SNOW_SCAL;
         else
            b_snow[i][j] = -99.0;

         if(iwp[i][j] >= 0)
            b_iwp[i][j] = (float) iwp[i][j]/IWP_SCAL;
         else
            b_iwp[i][j] = -99.0;

         out_array[0] = b_time[i][j];
         out_array[1] = lat[i][j];
         out_array[2] = lon[i][j];
         out_array[3] = b_rr[i][j];
         out_array[4] = b_snow[i][j];
         out_array[5] = b_iwp[i][j];
	 out_array[6] = lza[i][j];

         if((fwrite((void *)out_array, sizeof(float), NUM_PROD, fp1)) != NUM_PROD ){
         printf("Error writing to file %s\n", output_file);
	 exit(9);

	 }
      }

  fclose(fp1);

} /* End of wrt_bin.c */
