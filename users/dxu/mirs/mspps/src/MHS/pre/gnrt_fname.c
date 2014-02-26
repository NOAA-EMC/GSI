/***********************************************************************
 *  Program Name      : gnrt_fname.c
 *  Type              : Subroutine
 *  Function          : Program generates MHS swath file name 
 *			MHS Swath
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : rmhs_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/*************************************************************/
char *gnrt_fname()
{

   char	    *hdf_filename;
   char     temp[80],data_info[31];
   

   int i;
   unsigned short int sday, eday;
   unsigned long int totsec, stime, etime, ihour, imin, isec;
   
   hdf_filename = malloc(120);

/*------------------------------------------------*
 *  Generate HDF-EOS swath file name
 *------------------------------------------------*/
   sday = (hblock.start_year - (hblock.start_year/100)*100)*1000
          + hblock.start_day_of_year;
   eday = (hblock.end_year - (hblock.end_year/100)*100)*1000
          + hblock.end_day_of_year;

   totsec = hblock.start_milliseconds_of_day;
   totsec = totsec / 1000;
   ihour = totsec / 3600;
   imin = (totsec - ihour*3600) / 60;
   isec = totsec - ihour * 3600 - imin * 60;
   /* stime= ihour*10000 + imin*100 + isec; */
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

   sprintf(temp,"NPR.MHOP.%s.NS", data_info);

   strcpy(hdf_filename,temp_dir);
   strcat(hdf_filename,temp);

   printf("gnrt_fname/filename: %s\n",hdf_filename);

   return(hdf_filename);

}
