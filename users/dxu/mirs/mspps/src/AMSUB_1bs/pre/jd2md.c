/***********************************************************************
 *  Program Name      : jd2md.c
 *  Type              : Subroutine
 *  Function          : Program converts julian day to day of month
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : leapyear.c
 *  Called by         : gnrt_stime.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

int leapyear(short int);

/***********************************************************/
void jd2md(short int jday, short int *dom, short int *mon)
{
    short int   i, md;
    long  int   is_leap, iyear;
    short int   listmon[]={31,28,31,30,31,30,31,31,30,31,30,31};

/*----------------------------------------*
 * Check for leap year
 *----------------------------------------*/
    iyear = hblock1.start_year;

    is_leap = leapyear(iyear);

    if( is_leap )
        listmon[1] = 29;

/*----------------------------------------*
 * Convert the julian day to day of month
 *----------------------------------------*/
    i = 0;
    do
    {
       md = jday;
       jday = md - listmon[i];
       i++;
    }while(jday > 0);

    *mon = i;
    *dom = md;

}  /* end of jd2md.c */ 
