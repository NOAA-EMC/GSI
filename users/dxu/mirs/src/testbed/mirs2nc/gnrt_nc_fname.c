/***************************************************************************
 *  Program Name      : gnrt_nc_fname.c
 *  Type              : Subroutine
 *  Function          : Program generates MIRS netcdf file name 
 *  Input Files       : 
 *  Output Files      : NPR.AMOP.MIRS.NN.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS.nc
 *			(yy: year, ddd: julian day, Shhmm: starting hour 
 *			 and minute, Ehhmm: ending hour and minute, 
 *			 nnnnnnn: orbit ID)
 *  Subroutine Called : None 
 *  Called by         : rmirs_wnetcdf.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   Added DEP file or IMG file name
 *************************************************************************/
#include "defaults.h"

/*****************************************************************/
void gnrt_nc_fname(char* in_edrn, char* outdir, char* nc_edrn, char* nc_depn)
{
   char     temp[50],satid[6],*ttt,instid[5];
   char     orb_num[9];
   char     stime[6], etime[6], sday[7];
   int      ic;

   // printf("gnrt_nc_fname: Input name is %s\n",in_edrn);
   
   /*****************************************************
    Figure out day, stime,etime and orb_num from in_name
   ******************************************************/
   if ( strstr(in_edrn,"NN") > 0) {
     strcpy(satid,"NN");
     strcpy(instid,"AAMH");
   }else if( strstr(in_edrn,"NP") > 0) {
     strcpy(satid,"NP");
     strcpy(instid,"AAMH");
   }else if( strstr(in_edrn,"M2") > 0) {
     strcpy(satid,"M2");
     strcpy(instid,"AAMH");
   }else if( strstr(in_edrn,"SA") > 0) {
     strcpy(satid,"SA");
     strcpy(instid,"SSMIS");
   }else{
     printf("EDR filename does not have right satellite  id?\n");
     exit(3);
   }
     

   ttt = strstr(in_edrn,".D");
   if(ttt != NULL){
     for (ic=0;ic<6;ic++){
       sday[ic]=ttt[ic+1];
     }
     sday[6]='\0';
   }else
     {
       printf("EDR filename does not have time info?\n");
       exit(3);
     }

   ttt = strstr(in_edrn,".S");
   if(ttt != NULL ){
     if( strcmp(satid,"SA") !=0){//Non SSMI
       for (ic=0;ic<5;ic++){
	 stime[ic]=ttt[ic+1];
       }
       stime[5]='\0';
     }else{//SSMI
       for (ic=0;ic<5;ic++){
	 stime[ic]=ttt[ic+11];
       }
       stime[5]='\0';
     }
   }else{
       printf("EDR filename does not have Start time info?\n");
       exit(3);
   }


   ttt = strstr(in_edrn,".E");
   if(ttt != NULL ){
     for (ic=0;ic<5;ic++){
       etime[ic]=ttt[ic+1];
     }
     etime[5]='\0';
   }else{
       printf("EDR filename does not have End time info?\n");
       exit(3);
   }

   ttt = strstr(in_edrn,".B");
   if(ttt !=NULL ){
     for (ic=0;ic<8;ic++){
       orb_num[ic]=ttt[ic+1];
     }
     orb_num[8]='\0';
   }else{
       printf("EDR filename does not have orb_num info?\n");
       exit(3);
   }

   sprintf(temp,"NPR.MIRS.V4.SND.%s.%s.%s.%s.%s.%s", instid,satid, sday, stime, etime, orb_num);
   strcpy(nc_edrn,outdir);
   strcat(nc_edrn,temp);
   strcat(nc_edrn,".NS.nc");

   sprintf(temp,"NPR.MIRS.V4.IMG.%s.%s.%s.%s.%s.%s", instid,satid, sday, stime, etime, orb_num);
   strcpy(nc_depn,outdir);
   strcat(nc_depn,temp);
   strcat(nc_depn,".NS.nc");
  
   //printf("gnrt_nc_fname/filenames: %s\n %s\n",nc_edrn,nc_depn);

} /* end of gnrt_nc_fname.c */                             
