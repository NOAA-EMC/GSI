/***********************************************************************
 *  Program Name      : rmhs_scn.c
 *  Type              : Subroutine
 *  Function          : Program reads MHS scanline data 
 *  Input Files       : MHSX.NN.Dyyddd.Ssttt.Eettt
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
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/*********************************************************/
void rmhs_scn(FILE *mhs1bs, TOTNUM_OF_SCAN numscan)
{
   short int       iscan;

/*---------------------------------------------------*
 * Read MHS 1B* scanline data records
 *---------------------------------------------------*/
   for(iscan = 0; iscan < numscan; iscan++)
     fread(&scanline[iscan], L_SCANLINE, 1, mhs1bs);

} /* end of rmhs_scn.c */
