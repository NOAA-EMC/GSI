/**************************************************************************
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
 *   8/30/2000      v2.0
 * Made some changes here, if jday as a unsigned and used in jday>0
 *      it may never stop.           jzhao
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/******************************************************************/
void jd2md(unsigned short int jday, short int *dom, short int *mon)
{
    long int leapyear(unsigned short int iyear);
    short int   i, md,nday;
    short int   listmon[]={31,28,31,30,31,30,31,31,30,31,30,31};
    long  int   is_leap;
    unsigned short iyear;

/*------------------------------------------------------*
 *  Check if the year is leap year
 *------------------------------------------------------*/
    iyear = hblock.start_year;

    is_leap = leapyear(iyear);

    if( is_leap )
        listmon[1] = 29;

    //printf("jd to md:  %hi\n", jday);
    
/* -----------------------------------------------------*
 *  Convert the julian day to day of month
 * -----------------------------------------------------*/
    i = 0;
    /*  do
    {
       md = jday;
       jday = md - listmon[i];
       i++;
       // printf("jd to md in loop: %hi %hi\n", i-1, jday);
       }while(jday > 0);  */

       nday = jday;
    while( nday > 0 && i<12){
      md = nday;
       nday = md - listmon[i];
       i++;
       /*      printf("jd to md in loop: %hi %hi\n", i, md); */
    }

    /*printf("jd to md: %hi %hi\n", i, md); */
    *mon = i;
    *dom = md;

}  /* end of jd2md.c */ 
