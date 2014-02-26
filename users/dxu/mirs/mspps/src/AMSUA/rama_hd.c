/***************************************************************************
 *  Program Name      : rama_hd.c
 *  Type              : Subroutine
 *  Function          : Program reads the header record of AMSU-A 1B file 
 *  Input Files       : AMAX.NL.Dyyddd.Ssttt.Eettt
 *			(yy: year, ddd: julian day, sttt: starting time,
 *			 ettt: ending time) 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   08/30/2000      v2.0
 *   05/09/2006                   Change to reading from 1b instead of 1b*
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"
 
/**************************************************************/
void rama_hd(FILE *ama1b)
{

/*---------------------------------------------------*
 * Read the header block
 *---------------------------------------------------*/
   fread(&hblock, L_HBLOCK, 1, ama1b);

/*---------------------------------------------------*
 * Put central wave numbers in an array 
 *---------------------------------------------------*/
   t_r_central_wave_number[0] = hblock.t_r_central_wave_number_1*1.0E-6;
   t_r_central_wave_number[1] = hblock.t_r_central_wave_number_2*1.0E-6;
   t_r_central_wave_number[2] = hblock.t_r_central_wave_number_3*1.0E-6;
   t_r_central_wave_number[3] = hblock.t_r_central_wave_number_4*1.0E-6;
   t_r_central_wave_number[4] = hblock.t_r_central_wave_number_5*1.0E-6;
   t_r_central_wave_number[5] = hblock.t_r_central_wave_number_6*1.0E-6;
   t_r_central_wave_number[6] = hblock.t_r_central_wave_number_7*1.0E-6;
   t_r_central_wave_number[7] = hblock.t_r_central_wave_number_8*1.0E-6;
   t_r_central_wave_number[8] = hblock.t_r_central_wave_number_9*1.0E-6;
   t_r_central_wave_number[9] = hblock.t_r_central_wave_number_10*1.0E-6;
   t_r_central_wave_number[10] = hblock.t_r_central_wave_number_11*1.0E-6;
   t_r_central_wave_number[11] = hblock.t_r_central_wave_number_12*1.0E-6;
   t_r_central_wave_number[12] = hblock.t_r_central_wave_number_13*1.0E-6;
   t_r_central_wave_number[13] = hblock.t_r_central_wave_number_14*1.0E-6;
   t_r_central_wave_number[14] = hblock.t_r_central_wave_number_15*1.0E-6;

/*---------------------------------------------------*
 * Print out header block information
 *---------------------------------------------------*/
   printf("\n-----------------------------------------------------\n");
   printf(" Start Year             :  %d\n", hblock.start_year);
   printf(" Start Day of Year      :  %d\n", hblock.start_day_of_year);
   printf(" End Year               :  %d\n", hblock.end_year);
   printf(" End Day of Year        :  %d\n", hblock.end_day_of_year);
   printf(" Central Wave Number 1  :  %ld\n", hblock.t_r_central_wave_number_1);
   printf(" Central Wave Number 2  :  %ld\n", hblock.t_r_central_wave_number_2);
   printf(" Central Wave Number 3  :  %ld\n", hblock.t_r_central_wave_number_3);
   printf(" Central Wave Number 4  :  %ld\n", hblock.t_r_central_wave_number_4);
   printf(" Central Wave Number 5  :  %ld\n", hblock.t_r_central_wave_number_5);
   printf(" Central Wave Number 6  :  %ld\n", hblock.t_r_central_wave_number_6);
   printf(" Central Wave Number 7  :  %ld\n", hblock.t_r_central_wave_number_7);
   printf(" Central Wave Number 8  :  %ld\n", hblock.t_r_central_wave_number_8);
   printf(" Central Wave Number 9  :  %ld\n", hblock.t_r_central_wave_number_9);
   printf(" Central Wave Number 10 :  %ld\n", hblock.t_r_central_wave_number_10);
   printf(" Central Wave Number 11 :  %ld\n", hblock.t_r_central_wave_number_11);
   printf(" Central Wave Number 12 :  %ld\n", hblock.t_r_central_wave_number_12);
   printf(" Central Wave Number 13 :  %ld\n", hblock.t_r_central_wave_number_13);
   printf(" Central Wave Number 14 :  %ld\n", hblock.t_r_central_wave_number_14);
   printf(" Central Wave Number 15 :  %ld\n", hblock.t_r_central_wave_number_15);

   printf("-----------------------------------------------------\n");

} /* end of rama_hd.c */
