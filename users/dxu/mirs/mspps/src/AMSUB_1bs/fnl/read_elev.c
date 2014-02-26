/***********************************************************************
 *  Program Name      : read_elev.c
 *  Type              : Subroutine
 *  Function          : Program reads in elevation data
 *  Input Files       : tbase_big.bin
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : BSWATH.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/***************************************************/
void read_elev(FILE *fparm)
{
   fread(elev_large, sizeof(elev_large), 1, fparm);

   fclose(fparm);

}  /* end of read_elev.c */
