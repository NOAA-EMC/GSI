/***********************************************************************
 *  Program Name      : calprod.c
 *  Type              : Subroutine
 *  Function          : Program calculates AMSU-B products
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : snowcover.c, swe_cal.c, get_conv_index.c,
 *                      icewp.c, rainrate.c, rainrate_correct.c, snowfall.c
 *  Called by         : gnrt_prod.c
 *  05/09/2007        : algorithm enhancement (ccr3135)
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************/
void snowcover();
void get_conv_index();
void icewp();
void rainrate();
void rainrate_correct();
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
