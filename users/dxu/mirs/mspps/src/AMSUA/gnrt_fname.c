/***************************************************************************
 *  Program Name      : gnrt_fname.c
 *  Type              : Subroutine
 *  Function          : Program generates AMSU-A swath file name 
 *  Input Files       : None 
 *  Output Files      : SWATH_MPA_AMSUA_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *			(yy: year, ddd: julian day, sttttt: starting time,
 *			 ettttt: ending time)
 *  Subroutine Called : None 
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 * Changed 09/19/06             For OPUS use
 * Modified to use 1b's filename information  jz    12/03/2007
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
void gnrt_fname(char* hdf_filename)
{

   char     temp[80], data_info[31];

   int i;
   unsigned short int sday, eday;
   unsigned long int totsec, stime, etime, ihour, imin, isec;

/*----------------------------------------------------------*
 * Generate the HDF-EOS swath file name
 *----------------------------------------------------------*/
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

   sprintf(temp,"NPR.AAOP.%s.NS", data_info);


   strcpy(hdf_filename,output_dir);
   strcat(hdf_filename,"swath/");
   strcat(hdf_filename,temp);

   printf("gnrt_fname/filename: %s\n",hdf_filename);

} /* end of gnrt_fname.c */                             
