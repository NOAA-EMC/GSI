/***********************************************************************
 *  Program Name      : leapyear.c
 *  Type              : Subroutine
 *  Function          : Program checks if a given year is leap year 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : getcal.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/************************************************************/
int leapyear(unsigned short int iyear)
{
    short int    div4, div100, div400;

/*---------------------------------------*
 *  Check for leap year
 *---------------------------------------*/
    div4   = iyear%4;
    div100 = iyear%100;
    div400 = iyear%400;

    if( (div4==0 && !div100==0) || div400==0)
      return(1);
    else
      return(0);

}  /* end of leapyear.c */ 
