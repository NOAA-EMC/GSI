/***********************************************************************
 *  Program Name      : cal_pred_land.c
 *  Type              : Subroutine
 *  Function          : Program calculates cloud base brightness
 *                      temperature over land
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : icewp.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 *   04/24/2001      v2.1           Change Coefficients
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/*********************************************************/
void cal_pred_land(short int iscan, short int ifov)
{
    float 	a23, a31;
    
    pred89 = MISSING;
    pred150 = MISSING;

    a23 = at_AB[iscan][ifov][0];
    a31 = at_AB[iscan][ifov][1];

    pred89 = 17.88 + 1.61* a23 - 0.67 * a31;
    pred150 = 33.78 + 1.69* a23 - 0.80* a31;

} /* end of cal_pred_land.c */
