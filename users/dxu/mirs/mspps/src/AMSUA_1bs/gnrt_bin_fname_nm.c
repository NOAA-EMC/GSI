/***************************************************************************
 *  Program Name      : gnrt_bin_fname.c
 *  Type              : Subroutine
 *  Function          : Program generates binary swath file name
 *  Input Files       : None
 *  Output Files      : NPR.AAOP.NL.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.BIN
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : None
 *  Called by         : rama_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   1/25/2001      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
void gnrt_bin_fname(char *file_1bs, char* bin_filename)
{


   char     	tmp[64],sensor[4];
   char     	direct[]="../data/NOAA-M/output/biny/";
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
   
   tmp[length-2]='B';
   tmp[length-1]='I';
   tmp[length]='N';
   tmp[length+1]='\0';
   
   //printf("tmp=%s\n", tmp); 

   strcpy(bin_filename,direct);
   strcat(bin_filename,tmp);

   printf("gnrt_bin_fname/filename: %s\n",bin_filename);


}
