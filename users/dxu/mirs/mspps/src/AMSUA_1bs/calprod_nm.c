/***************************************************************************
 *  Program Name      : calprod.c
 *  Type              : Subroutine
 *  Function          : Program calculates AMSU-A products 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : seaice.c, tpwclw.c
 *  Called by         : gnrt_prod.c 
 *
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
/*   stemp(iscan);
     emi(iscan);
*/

}  /* end of calprod.c */ 
