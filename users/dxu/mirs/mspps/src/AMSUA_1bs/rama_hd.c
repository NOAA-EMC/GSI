/***************************************************************************
 *  Program Name      : rama_hd.c
 *  Type              : Subroutine
 *  Function          : Program reads the header record of AMSU-A 1B* file 
 *  Input Files       : AMAX.NK.Dyyddd.Ssttt.Eettt
 *			(yy: year, ddd: julian day, sttt: starting time,
 *			 ettt: ending time) 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"
 
/**************************************************************/
void rama_hd(FILE *ama1bs)
{
   int       left_rec, i;

/*---------------------------------------------------*
 * Read the first block of header
 *---------------------------------------------------*/
   fread(&hblock1, L_HBLOCK1, 1, ama1bs);

/*---------------------------------------------------*
 * Skip the second block of header
 *---------------------------------------------------*/

   fseek(ama1bs, L_HBLOCK2, SEEK_CUR);

/*---------------------------------------------------*
 * Read the third block of header
 *---------------------------------------------------*/

   fread(&hblock3, L_HBLOCK3, 1, ama1bs);

/*---------------------------------------------------*
 * Read the fourth block of header
 *---------------------------------------------------*/

   fread(&hblock4, L_HBLOCK4, 1, ama1bs);

/*---------------------------------------------------*
 * Skip the rest bytes in the header record
 *---------------------------------------------------*/
   left_rec = REC_LENGTH_A - L_HBLOCK1 - L_HBLOCK2 - 
                             L_HBLOCK3 - L_HBLOCK4;

   fseek(ama1bs, left_rec, SEEK_CUR);

/*---------------------------------------------------*
 * Print out header block1 information
 *---------------------------------------------------*/
   printf("\n-----------------------------------------------------\n");
   printf(" Component id        :  %s\n", hblock1.component_id);
   printf(" Version number      :  %d\n", hblock1.version_number);
   printf(" Create yr/day       :  %d/%d\n", hblock1.creation_year_day[0], 
                              hblock1.creation_year_day[1]);
   printf(" Letter q            :  %c\n", hblock1.letter_q);
   printf(" Real number PI      :  %f\n", hblock1.pi);
   printf(" Hexa indicator      :  %ld\n", hblock1.hex_afffffff);
   printf(" Local dsname        :  ");
   for(i=0; i<30; i++)
     printf("%c", hblock1.local_data_set_name[i]);
     printf("\n");
   printf(" Origi dsname        :  ");
   for(i=0; i<50; i++)
     printf("%c", hblock1.original_data_set_name[i]);
     printf("\n");
   printf(" Proc block id       :  %s\n", hblock1.processing_block_id);
   printf(" Spacecraft id       :  %d\n", hblock1.spacecraft_id);
   printf(" Date type code      :  %d\n", hblock1.data_type_code);
   printf(" Start jd/yr/doy/msec:  %3ld/%5d/%3d/%11ld\n",
             hblock1.start_julian_day,
             hblock1.start_year, hblock1.start_day_of_year, 
             hblock1.start_milliseconds_of_day);
   printf(" End   jd/yr/doy/msec:  %3ld/%5d/%3d/%11ld\n", 
             hblock1.end_julian_day,
             hblock1.end_year, hblock1.end_day_of_year, 
             hblock1.end_milliseconds_of_day);
   printf(" Fst/Lst scan rec    : %4d/%4d\n", hblock1.first_scan_record, 
             hblock1.last_scan_record);
   printf("-----------------------------------------------------\n");

} /* end of rama_hd.c */
