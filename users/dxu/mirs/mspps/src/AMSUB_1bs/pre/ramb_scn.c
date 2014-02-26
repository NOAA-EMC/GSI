/***********************************************************************
 *  Program Name      : ramb_scn.c
 *  Type              : Subroutine
 *  Function          : Program reads AMSU-B scanline data
 *  Input Files       : AMBX.NK.Dyyddd.Ssttt.Eettt
 *                      (yy: year, ddd: julian day, sttt: starting time,
 *                       ettt: ending time)
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/******************************************************/
void ramb_scn(FILE *amb1bs, TOTNUM_OF_SCAN numscan)
{
   short int       iscan, left_rec;

/*---------------------------------------------------*
 * Read AMSUB 1B* scanline data records
 *---------------------------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
   {
     fread(&scanline1[iscan], L_SCANLINE1, 1, amb1bs);
     fread(&scanline2[iscan], L_SCANLINE2, 1, amb1bs);
     fread(&scanline3[iscan], L_SCANLINE3, 1, amb1bs);
     fseek(amb1bs, L_SCANLINE4, SEEK_CUR);
     fread(&scanline5[iscan], L_SCANLINE5, 1, amb1bs);

/*---------------------------------------------------*
 * Skip the rest bytes in each scanline
 *---------------------------------------------------*/
     left_rec = REC_LENGTH_B - L_SCANLINE1 - L_SCANLINE2 - L_SCANLINE3
       - L_SCANLINE4 - L_SCANLINE5;
     fseek(amb1bs, left_rec, SEEK_CUR);

   }

} /* end of ramb_scn.c */
