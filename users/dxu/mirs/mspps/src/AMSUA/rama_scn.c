/***************************************************************************
 *  Program Name      : rama_scn.c
 *  Type              : Subroutine
 *  Function          : Program reads scanline data from AMSU-A 1B* file
 *  Input Files       : AMAX.NL.Dyyddd.Ssttt.Eettt
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
void rama_scn(FILE *ama1b, short int numscan)
{
   short int       iscan;

/*---------------------------------------------------*
 * Read AMSU-A 1B* scanline data records
 *---------------------------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
     fread(&scanline[iscan], L_SCANLINE, 1, ama1b);

/*---------------------------------------------------*
 * Print out some scanline information
 *---------------------------------------------------*/
/*
     printf(" scan num    :  %d\n",scanline[iscan].scan_line_number);
     printf(" year-scan   :  %d\n",scanline[iscan].year_of_scan);
     printf(" d/y of scan :  %d\n",scanline[iscan].day_of_year_of_scan);
     printf(" t/d of scan :  %d\n",scanline[iscan].time_of_day_of_scan);
     printf("---------------------------------\n");
*/
   }
} /* end of rama_scn.c */
