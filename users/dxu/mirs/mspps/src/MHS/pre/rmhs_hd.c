/*************************************************************************
 *  Program Name      : rmhs_hd.c
 *  Type              : Subroutine
 *  Function          : Program reads the header record of AMSU-B 1B* file
 *  Input Files       : MHSX.NL.Dyyddd.Ssttt.Eettt
 *                      (yy: year, ddd: julian day, sttt: starting time,
 *                       ettt: ending time)
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : rmhs_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 *************************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"
 
/**************************************************************/
void rmhs_hd(FILE *mhs1bs)
{

/*---------------------------------------------------*
 * Read the header block 
 *---------------------------------------------------*/
   fread(&hblock, L_HBLOCK, 1, mhs1bs);

/*---------------------------------------------------*
 * Put central wave numbers in an array
 *---------------------------------------------------*/
   t_r_central_wave_number[0] = hblock.t_r_central_wave_number_1*1.0E-6;
   t_r_central_wave_number[1] = hblock.t_r_central_wave_number_2*1.0E-6;
   t_r_central_wave_number[2] = hblock.t_r_central_wave_number_3*1.0E-6;
   t_r_central_wave_number[3] = hblock.t_r_central_wave_number_4*1.0E-6;
   t_r_central_wave_number[4] = hblock.t_r_central_wave_number_5*1.0E-6;

/*---------------------------------------------------*
 * Print out header block1 information
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
   printf("-----------------------------------------------------\n");

} /* end of rmhs_hd.c */
