/***********************************************************************
 *  Program Name      : read_systim.c
 *  Type              : Subroutine
 *  Function          : Program reads system date and time 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : MHS2HDF.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/***********************************************************/
void read_systim(string1, string2) 
    char    *string1, *string2;
{
    time_t   time_pointer;
    struct   tm *tm_pointer;
    char     *local, *gmt;    

    time(&time_pointer);
    tm_pointer = localtime(&time_pointer);

/*------------------------------------------------*
 * System date ( julian day: 0-365; year: year 
 *	       since 1900; mon: 0-11; day: 1-31 )
 *------------------------------------------------*/

    Sys_times.sys_jday = tm_pointer->tm_yday;
    Sys_times.sys_year = tm_pointer->tm_year;
    Sys_times.sys_mon  = tm_pointer->tm_mon;
    Sys_times.sys_mday = tm_pointer->tm_mday;

/*------------------------------------------------*
 * System time 
 *------------------------------------------------*/

    Sys_times.sys_hour = tm_pointer->tm_hour;
    Sys_times.sys_min  = tm_pointer->tm_min;
    Sys_times.sys_sec  = tm_pointer->tm_sec;

/*------------------------------------------------*
 * Sample print outs
 *------------------------------------------------*/

    printf("\n\n*****************************************************************\n");
    printf("* AMSUA %s PROCESSING %s ON : %2d/%2d/%4d (DOY: %3d) *\n",
             string2, string1,
             Sys_times.sys_mon+1, Sys_times.sys_mday,
             Sys_times.sys_year+1900,  Sys_times.sys_jday+1); 
    printf("*----------------------------------------------------------*\n");

/*------------------------------------------------*
 * LOCAL time
 *------------------------------------------------*/

    tm_pointer = localtime(&time_pointer);
    local = asctime(tm_pointer);

    printf("*  AT LOCAL : %s*\n",local);

/*------------------------------------------------*
 * GMT time
 *------------------------------------------------*/

    tm_pointer = gmtime(&time_pointer);
    gmt = asctime(tm_pointer);

    printf("*  AT GMT   : %s*\n",gmt);

    printf("*****************************************************************\n");

} /* end of read_systim.c */
