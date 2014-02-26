/***************************************************************************
 *  Program Name      : read_ssmir.c
 *  Type              : Subroutine
 *  Function          : Program reads SSMI climate rain data for rain rate
 *			calculation
 *  Input Files       : jan.dat, feb.dat, mar.dat, apr.dat,
 *                      may.dat, jun.dat, jul.dat, aug.dat,
 *                      sep.dat, oct.dat, nov.dat, dec.dat
 *			(only use one input file for each swath)
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : gnrt_prod.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/**************************************************************/
void read_ssmir(FILE *filep)
{
    fread(ssmi_r,sizeof(float),SSMI_LON*SSMI_LAT,filep);
    printf("Finish reading ssmi_r data. \n");

} /* read_ssmir.c */
