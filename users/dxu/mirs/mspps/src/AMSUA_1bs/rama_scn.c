/***************************************************************************
 *  Program Name      : rama_scn.c
 *  Type              : Subroutine
 *  Function          : Program reads scanline data from AMSU-A 1B* file
 *  Input Files       : AMAX.NK.Dyyddd.Ssttt.Eettt
 *                      (yy: year, ddd: julian day, sttt: starting time,
 *                       ettt: ending time) 
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

/******************************************************************/
void rama_scn(FILE *ama1bs, short int numscan)
{
   short int       iscan, left_rec;

/*---------------------------------------------------*
 * Read AMSU-A 1B* scanline data records
 *---------------------------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
     fread(&scanline1[iscan], L_SCANLINE1, 1, ama1bs);
     fread(&scanline2[iscan], L_SCANLINE2, 1, ama1bs);
     fread(&scanline3[iscan], L_SCANLINE3, 1, ama1bs);

/*---------------------------------------------------*
 * Skip the rest bytes in each scanline
 *---------------------------------------------------*/
     left_rec = REC_LENGTH_A - L_SCANLINE1 - L_SCANLINE2 - L_SCANLINE3;
     fseek(ama1bs, left_rec, SEEK_CUR);

/*---------------------------------------------------*
 * Print out some scanline information
 *---------------------------------------------------*/
/*
     printf(" scan num    :  %d\n",scanline1[iscan].scan_line_number);
     printf(" year-scan   :  %d\n",scanline1[iscan].year_of_scan);
     printf(" d/y of scan :  %d\n",scanline1[iscan].day_of_year_of_scan);
     printf(" t/d of scan :  %d\n",scanline1[iscan].time_of_day_of_scan);
     printf(" nu scan indi:  %d\n",scanline1[iscan].do_not_use_scan);
     printf("---------------------------------\n");
*/
   }
} /* end of rama_scn.c */
