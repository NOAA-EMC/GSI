/***********************************************************************
 *  Program Name      : gnrt_stime.c
 *  Type              : Subroutine
 *  Function          : Program sets up the scan time for geo table 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : jd2md.c 
 *  Called by         : set_sw.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/*************************************************************/
long int  leapyear();
void      jd2md();

/*************************************************************/
void gnrt_stime(TOTNUM_OF_SCAN numscan)
{
   unsigned short       jday, sday, eday;
   unsigned long        itotsec, stotsec;
   unsigned short       stime, syear;
   long  int            lyflag, hour, min, sec;
   long  int   		totsec, is_day_cross=0;
   short int   		iscan, mon, domth;
  
   short int  		lzyear;
   short int  		lzdoy;
   char       		lzmonth, lzdom;
   char       		lzhour, lzminute, lzsecond;

/*------------------------------------------------------*
 * Extract the hour, minute and second of the scan line.  
 * Set up the scanline julian day by looking up the day 
 * cross flag.
 *------------------------------------------------------*/
   sday  = hblock.start_day_of_year;
   eday  = hblock.end_day_of_year;
   if(sday != eday) 
     is_day_cross = 1;
   jday = sday;

   stime = hblock.start_year;

   syear = stime;

   lyflag = leapyear(syear);

   stotsec = hblock.start_milliseconds_of_day;

   for (iscan = 0; iscan < numscan; iscan++)
   {
      itotsec = scanline[iscan].time_of_day_of_scan;
      totsec = itotsec / 1000;
      hour  = totsec / 3600;
      min = (totsec - hour*3600) / 60;
      sec = totsec - hour * 3600 - min * 60;

/*------------------------------------------------------*
 * Decide the day of month/year and month of year 
 *------------------------------------------------------*/
      if(is_day_cross == 1)
         if(itotsec < stotsec)
         {
            jday = sday + 1;

         /* Change of the year  */
            if( (lyflag && jday == 367) || (!lyflag && jday == 366) )
            {
                jday = 1;
                syear = syear + 1;

         /* Change of the century  */
                if(syear == 100)
                   syear = 0;
            }
         }

     /* Conver julian day to day of month */
     jd2md(jday, &domth, &mon);   

     lzyear = syear;
     lzmonth = mon;
     lzdom   = domth;  
     lzhour  = hour;
     lzminute = min;
     lzsecond = sec;
     lzdoy    = jday; 

     //      printf("gnrt_stime: %hi %hi %hi %hi %hi %hi\n", lzmonth,lzdom,lzhour,lzminute,lzsecond,lzdoy);
/*------------------------------------------------------*
 * Set up scan time for geo table 
 *------------------------------------------------------*/
     geo_scantime[iscan].year   = lzyear;
     geo_scantime[iscan].month  = lzmonth;
     geo_scantime[iscan].dom    = lzdom;
     geo_scantime[iscan].hour   = lzhour;
     geo_scantime[iscan].minute = lzminute;
     geo_scantime[iscan].second = lzsecond;
     geo_scantime[iscan].doy    = lzdoy;
      
   }  /* iscan for loop */

} /* end of gnrt_stime.c */
