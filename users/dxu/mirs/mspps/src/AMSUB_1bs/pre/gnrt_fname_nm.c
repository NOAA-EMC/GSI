/***********************************************************************
 *  Program Name      : gnrt_fname.c
 *  Type              : Subroutine
 *  Function          : Program generates AMSU-B swath file name 
 *  Input Files       : None
 *  Output Files      : NPR.ABOP.NM.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/********************************************************/
void  gnrt_fname(char *file_1bs, char* hdf_filename)
{
   char     	tmp[64], sensor[4];
   char     	direct[]="../data/NOAA-M/tmp/";
   char 	*s;
   size_t 	length;
   
   
   s=strrchr( file_1bs,  '/');
   if ( s != NULL ) {
     s++;
     strcpy(tmp,s);
   }
   else {
     strcpy(tmp,file_1bs);
   }
   
   //printf("tmp=%s\n", tmp); 

   sensor[0]=tmp[4];
   sensor[1]=tmp[5];
   sensor[2]=tmp[6];
   sensor[3]=tmp[7];
   sensor[4]='\0';
      
   //printf("sensor=%s\n", sensor); 
   
   if( strcmp(sensor, "AMAX") == 0 ) {
     strncpy(tmp, "NPR.AAOP", 8);
   }
   else if( strcmp(sensor, "AMBX") == 0 ) {
     strncpy(tmp, "NPR.ABOP", 8);
   }

   //printf("tmp=%s\n", tmp); 
   //printf("strlen(tmp)=%i\n", strlen(tmp)); 
   
   length=strlen(tmp);
   
   tmp[length-2]='N';
   tmp[length-1]='S';
   
   //printf("tmp=%s\n", tmp); 
   
   strcpy(hdf_filename,direct);
   strcat(hdf_filename,tmp);

   printf("gnrt_fname/filename: %s\n",hdf_filename);

}
