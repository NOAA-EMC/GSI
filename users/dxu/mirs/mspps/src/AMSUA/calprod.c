/***************************************************************************
 *  Program Name      : calprod.c
 *  Type              : Subroutine
 *  Function          : Program calculates AMSU-A products 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : seaice.c, tpwclw.c, snowcover.c, rainrate.c,
 *			stemp.c, emi.c
 *  Called by         : gnrt_prod.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/***************************************************************/
void seaice();
void tpwclw();
void stemp();
void emi();

/***************************************************************/
void calprod(short int iscan)
{

/*-------------------------------------------------------*
 * Call subroutines to compute the products
 *-------------------------------------------------------*/

     seaice(iscan);
     tpwclw(iscan);
     stemp(iscan);
     emi(iscan);

}  /* end of calprod.c */ 
