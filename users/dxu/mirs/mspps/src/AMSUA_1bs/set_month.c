/***************************************************************************
 *  Program Name      : set_month.c
 *  Type              : Subroutine
 *  Function          : Program gets the month info from julian day 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : leapyear.c
 *  Called by         : gnrt_prod.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/****************************************************************/
int leapyear(int );

/****************************************************************/
int set_month()
{
  short int   listmon[]={31,28,31,30,31,30,31,31,30,31,30,31};
  short int   jday ,i , md;
  long  int   iyear,is_leap;
 
  jday=hblock1.start_day_of_year;
  iyear=hblock1.start_year;
   
  is_leap = leapyear(iyear);
 
  if( is_leap )
     listmon[1] = 29;

  i = 0;
  do
  {
     md = jday;
     jday = md - listmon[i];
     i++;

  }while(jday > 0);
    
  return i;
}  /* end of set_month.c */
