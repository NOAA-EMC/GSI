/***************************************************************************
 *  Program Name      : gnrt_bin_fname.c
 *  Type              : Subroutine
 *  Function          : Program generates binary swath file name
 *  Input Files       : None
 *  Output Files      : SWATH_MPA_AMSUA_Syyddd_sttttt_Eyyddd_ettttt.bin
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Subroutine Called : None
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   1/25/2001      v2.0
 *   09/18/2006               Changed for OPUS   jzhao(QSS)
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
void gnrt_bin_fname(char* bin_filename)
{

   char     temp[80], data_info[31];


   int i;
   unsigned short int sday, eday;
   unsigned long totsec, stime, etime, ihour, imin, isec;

   sday = (hblock.start_year - (hblock.start_year/100)*100)*1000
          + hblock.start_day_of_year;
   eday = (hblock.end_year - (hblock.end_year/100)*100)*1000
          + hblock.end_day_of_year;
   totsec = hblock.start_milliseconds_of_day;
   totsec = totsec / 1000;
   ihour = totsec / 3600;
   imin = (totsec - ihour*3600) / 60;
   isec = totsec - ihour * 3600 - imin * 60;
   /*stime= ihour*10000 + imin*100 + isec;*/
   stime= ihour*100+imin;

   totsec = hblock.end_milliseconds_of_day;
   totsec = totsec / 1000;
   ihour = totsec / 3600;
   imin = (totsec - ihour*3600) / 60;
   isec = totsec - ihour * 3600 - imin * 60;
   /*etime= ihour*10000 + imin*100 + isec;*/
   etime=ihour*100+imin;


   for (i=0;i<30;i++){
     data_info[i]=hblock.original_data_set_name[i+9];
   }
   data_info[30]='\0';

   sprintf(temp,"NPR.AAOP.%s.BIN", data_info);

   strcpy(bin_filename,output_dir);
   strcat(bin_filename,"biny/");
   strcat(bin_filename,temp);

   printf("gnrt_bin_fname/filename: %s\n",bin_filename);

} /* end of gnrt_bin_fname.c */                             
