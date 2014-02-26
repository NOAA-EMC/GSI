/***********************************************************************
 *  Program Name      : read_mask.c
 *  Type              : Subroutine
 *  Function          : Program reads in the 1/6 degree land/sea
 *			mask data from University of Bristol 
 *  Input Files       : mask.bin 
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : MHS2HDF.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/************************************************************/
void read_mask(FILE *fparm)
{
   fread(mask, sizeof(mask), 1, fparm);

   fclose(fparm);

   printf(" Successfully read the 1/6 degree mask data in !\n");

} /* end of read_mask.c */
