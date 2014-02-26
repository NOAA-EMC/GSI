/***************************************************************************
 *  Program Name      : gnrt_nc_fname_npp.c
 *  Type              : Subroutine
 *  Function          : Program generates MIRS netcdf file name for NPP products
 *  Input Files       : 
 *  Output Files      : NPR-MIRS-IMG_vX_NPP_syyyymmddhhmmss_eyyyymmddhhmmss_cyyyymmddhhmmss.nc
 *			(vX: version number, s: starting year, month, day, hour, minute, second;  
 *			 e: ending year, month, day, hour, minute, second; 
 *			 c: creation year, month, day, hour, minute, second)
 *  Subroutine Called : None 
 *  Called by         : rmirs_wnetcdf.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   Added DEP file or IMG file name
 *    5/05/2011      2             Changed to NDE file naming convention
 *************************************************************************/
#include "defaults.h"
#include <time.h>

void gnrt_nc_fname_npp(char* in_edrn, char* outdir, char* nc_edrn, char* nc_depn)
{
   char *poscnt1  = NULL;
   char *poscnt2  = NULL; 
   char *poscnt3  = NULL;
   char *pos1    = NULL;
   char *pos2    = NULL;
   char *pos3    = NULL;
   char *pos4    = NULL;
   char *pos5    = NULL;
   char *pos6    = NULL;

   char yyyymmdd[9];
   char sts[6];
   char ets[6];

   int lendate  = 0;
   int lenst    = 0;
   int lenet    = 0;

   int cyeari=0;
   int cmonthi=0;
   char cyearstr[4];
   char cmonthstr[2];
   char cmdaystr[2];
   char chourstr[2];
   char cminutestr[2];
   char csecondstr[2];

   time_t rawtime;
   struct tm *timecreated;

   //get current time in UTC and convert to strings
   rawtime=time(NULL);
   timecreated = gmtime(&rawtime);   
   cyeari = timecreated->tm_year + 1900;
   cmonthi = timecreated->tm_mon + 1;
   snprintf(cyearstr,5,"%d",cyeari);
   snprintf(cmonthstr,3,"%d",cmonthi);
   snprintf(cmdaystr,3,"%d",timecreated->tm_mday);
   snprintf(chourstr,3,"%d",timecreated->tm_hour);
   snprintf(cminutestr,3,"%d",timecreated->tm_min);
   snprintf(csecondstr,3,"%d",timecreated->tm_sec);

   //append date of data to edr (snd) and dep (img) filenames
   pos1 = strchr(in_edrn,'d');
   poscnt1  = pos1;
   pos2 = strstr(in_edrn,"_t");
   
   while( pos1 != pos2 )
   { 
     pos1++;  
     lendate++;
     
   }

   strncpy(yyyymmdd,poscnt1+1,lendate-1);

   strcpy(nc_edrn,outdir);
   strcat(nc_edrn,"NPR-MIRS-SND_v9_NPP_s");
   strcat(nc_edrn,yyyymmdd);

   strcpy(nc_depn,outdir);
   strcat(nc_depn,"NPR-MIRS-IMG_v9_NPP_s");
   strcat(nc_depn,yyyymmdd);

   //append start time to filename
   pos3   = strchr(in_edrn,'t');
   poscnt2 = pos3;
   pos4   = strstr(in_edrn,"_e");

   while( pos3 != pos4 )
   { 
     pos3++;  
     lenst++;     
   }

   strncpy(sts,poscnt2+1,lenst-2);
   strcat(nc_edrn,sts);
   strcat(nc_edrn,"_e");
   strcat(nc_depn,sts);
   strcat(nc_depn,"_e");

   //append date and end time to filename
   pos5   = strchr(in_edrn,'e');
   poscnt3 = pos5;
   pos6   = strstr(in_edrn,"_b");

   while( pos5 != pos6 )
   { 
     pos5++;  
     lenet++;
   }

   strncpy(ets,poscnt3+1,lenet-2);

   strcat(nc_edrn,yyyymmdd);
   strcat(nc_edrn,ets);
   strcat(nc_edrn,"_c");
   strcat(nc_depn,yyyymmdd);
   strcat(nc_depn,ets);
   strcat(nc_depn,"_c");

   //append time the file was created by this program
   strcat(nc_edrn,cyearstr);
   strcat(nc_depn,cyearstr);
   if (cmonthi < 10) {
     strcat(nc_edrn,"0");
     strcat(nc_depn,"0");
   }
   strcat(nc_edrn,cmonthstr);
   strcat(nc_depn,cmonthstr);

   if (timecreated->tm_mday < 10) {
     strcat(nc_edrn,"0");
     strcat(nc_depn,"0");
   }
   strcat(nc_edrn,cmdaystr);
   strcat(nc_depn,cmdaystr);

   if (timecreated->tm_hour < 10) {
     strcat(nc_edrn,"0");
     strcat(nc_depn,"0");
   }
   strcat(nc_edrn,chourstr);
   strcat(nc_depn,chourstr);
   
   if (timecreated->tm_min < 10) {
     strcat(nc_edrn,"0");
     strcat(nc_depn,"0");
   }
   strcat(nc_edrn,cminutestr);
   strcat(nc_depn,cminutestr);

   if (timecreated->tm_sec < 10) {
     strcat(nc_edrn,"0");
     strcat(nc_depn,"0");
   }
   strcat(nc_edrn,csecondstr);
   strcat(nc_depn,csecondstr);

   //append nc extension
   strcat(nc_edrn,".nc");
   strcat(nc_depn,".nc");
 
   //printf("Output Sounding file=%s\n",nc_edrn);
   //printf("Output Imaging file=%s\n",nc_depn);

} /* end of gnrt_nc_fname.c */
