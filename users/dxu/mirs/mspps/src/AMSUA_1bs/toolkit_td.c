
/**************************************************************************
 *  Subroutine name   : toolkit_td.c
 *  Type              : Subroutine
 *  Function          : Program uses the time/metadata stand alone toolkit
 *            to convert scan time to HDF-EOS standard, TAI93
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : wrt_geo.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *
 * Gte this one from Wanchun     09/05

*************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/************************************************************/
double  toolkit_td (char *UTC_time)
{

        double     Con_time;
    int year, month, day, hour, minute, second;

    char yr[5], mon[3], dy[3], hr[3], min[3], sec[3];

    int i;

    time_t TAI93;
    struct tm T93, thisTime;

      T93.tm_sec=0;           /* Seconds.     [0-60] (1 leap second) */
        T93.tm_min=0;            /* Minutes.     [0-59] */
        T93.tm_hour=0;            /* Hours.       [0-23] */
        T93.tm_mday=1;            /* Day.         [1-31] */
        T93.tm_mon=0;            /* Month.       [0-11] */
        T93.tm_year=93;            /* Year - 1900.  */
        T93.tm_wday=4;            /* Day of week. [0-6] */
        T93.tm_yday=0;            /* Days in year.[0-365] */
      T93.tm_isdst=0;        /* DST.         [-1/0/1]*/

    TAI93 = mktime(&T93);

    for ( i=0; i<4;   i++ )  yr[i]     = UTC_time[i]; yr[4]='\0';
    for ( i=5; i<7;   i++ )  mon[i-5]  = UTC_time[i]; mon[2]='\0';
    for ( i=8; i<10;  i++ )  dy[i-8]   = UTC_time[i]; dy[2]='\0';
    for ( i=11; i<13; i++ ) hr[i-11]   = UTC_time[i]; hr[2]='\0';
    for ( i=14; i<16; i++ ) min[i-14]  = UTC_time[i]; min[2]='\0';
    for ( i=17; i<19; i++ ) sec[i-17]  = UTC_time[i]; sec[2]='\0';

    year     = atoi(yr);
    month     = atoi(mon);
    day     = atoi(dy);
    hour     = atoi(hr);
    minute     = atoi(min);
    second     = atoi(sec);
    /*
    printf("year=%d\n", year);
    printf("month=%d\n", month);
    printf("day=%d\n", day);
    printf("hour=%d\n", hour);
    printf("moinute=%d\n", minute);
    printf("second=%d\n", second);
    */
      thisTime.tm_sec=second;               /* Seconds.     [0-60] (1 leap second) */
        thisTime.tm_min=minute;                /* Minutes.     [0-59] */
        thisTime.tm_hour=hour;                /* Hours.       [0-23] */
        thisTime.tm_mday=day;                /* Day.         [1-31] */
        thisTime.tm_mon=month-1;            /* Month.       [0-11] */
        thisTime.tm_year=year-1900;            /* Year - 1900.  */
        thisTime.tm_wday=2;                /* Day of week. [0-6] */
        thisTime.tm_yday=334;                /* Days in year.[0-365] */

    if ( month >= 11 || month < 4 )
          thisTime.tm_isdst=0;              /* DST.          [-1/0/1]*/
    else
          thisTime.tm_isdst=1;

    Con_time=difftime( mktime(&thisTime), TAI93 );

    //return(Con_time);
    return (Con_time+10);

}
