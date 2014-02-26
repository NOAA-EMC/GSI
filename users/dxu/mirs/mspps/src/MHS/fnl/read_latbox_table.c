/***********************************************************************
 *  Program Name      : read_latbox_table.c
 *  Type              : Subroutine
 *  Function          : Program reads in the latitude box limit table
 *			for the determination of surface type
 *  Input Files       : latbox_table.dat
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : BSWATH.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   11/28/2001      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************/
void read_latbox_table(FILE *fparm)
{
   short int     i; 
   int		intdum;
  
   for(i = 0; i < NUMSPOT_M; i++)
     fscanf(fparm, "%d %d %d", &latbox_up[i], &latbox_down[i], &intdum);

   fclose(fparm);

} /* end of read_latbox_table.c */
