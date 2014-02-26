/***********************************************************************
 *  Program Name      : calprod.c
 *  Type              : Subroutine
 *  Function          : Program calculates MHS products
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : snowcover.c, icewp.c, rainrate.c
 *  Called by         : gnrt_prod.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************/
void snowcover();
void get_conv_index();
void rainrate_correct();
void icewp();
void rainrate();
void snowfall();
void snowfall_rate();
void swe_cal();

/*******************************************************/
void calprod(short int iscan)
{

/*------------------------------------------*
 * Call subroutines to compute the products
 *------------------------------------------*/

     snowcover(iscan);
     swe_cal(iscan);
     get_conv_index(iscan);
     icewp(iscan);
     rainrate(iscan);
     snowfall(iscan);
     rainrate_correct(iscan);
     snowfall_rate(iscan);

}  /* end of calprod.c */ 
