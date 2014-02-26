/***************************************************************************
 *  Program Name      : read_latbox_table.c
 *  Type              : Subroutine
 *  Function          : Program reads in the lat_box_limit table which is
 *			used to determine surface type from mask data
 *  Input Files       : latbox_table.dat 
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : AMA2HDF.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/****************************************************************/
void read_latbox_table(FILE *fparm)
{
   short int     i; 
   int	 intdum;
  
   for(i = 0; i < NUMSPOT_A; i++)
   {
     fscanf(fparm, "%d %d %d", &latbox_up[i], &latbox_down[i],&intdum);
   }

   fclose(fparm);

}  /* end of read_latbox_table.c */
