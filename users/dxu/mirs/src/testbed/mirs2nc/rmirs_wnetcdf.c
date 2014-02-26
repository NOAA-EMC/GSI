/***************************************************************************
 *  Program Name      : rmirs_wnetcdf.c
 *  Type              : Subroutine
 *  Function          : Program reads MIRS  generated products
 *                      and creates netcdf file 
 *  Input Files       : EDR binary data and the output directory
 *  Output Files      : Netcdf files
 *  Subroutine Called : gnrt_nc_fname.c,  openfs_sw.c, rmirs_edr.c, rmirs_dep.c,
 *                      set_nc_snd.c, set_nc_img.c
 *  Called by         : mirs2nc.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *  
 *  12/05/2008      v              JZhao (PSGS)
 *************************************************************************/
#include "defaults.h"
#include "constants.h"
#include "swath.h"

/*****************************************************************/
void gnrt_nc_fname(char*, char *,char* ,char *);
FILE *openfs_sw();
void rmirs_edr();
void rmirs_dep();
void init_sw();
void set_nc_snd();
void set_nc_img();

/*****************************************************************/
void rmirs_wnetcdf(char *input_edr, char *input_dep, char *output_dir, char *output_img, char *output_snd)
{

   FILE        *fp_edr, * fp_dep;

   short int   numscan, numspot,numlayer,numchan;
   char        nc_name_snd[300], nc_name_img[300], *fname_edr, *fname_dep ;
   char	       satid[4]="";
   int         intsatid=-99;
   char	       *pch;

/*-----------------------------------------------------*
 * Initialize the AT and product arrays
 *-----------------------------------------------------*/

     init_sw();

/*  We comment this out, not necessary

// check the consistency of EDR and DEP files : OSDPD version
     fname_dep = strstr(input_dep,".D");
     temp = strstr(input_edr,".D");

     if(strcmp(fname_dep,temp)!= 0){
       printf("The input edr & dep name are not consistent!\n %s\n %s\n",fname_dep,temp);
       exit(3);
     }

// check the consistency of EDR and DEP files : NDE NPP ATMS version

     fname_dep = strstr(input_dep,"_d");
     temp   = strstr(input_edr,"_d");

     if( strcmp(fname_dep,temp) != 0 ){
       printf("The input edr & dep name are not consistent!\n %s\n %s\n",fname_dep,temp);
       exit(3);
     }

*/


/*********************added here to get the info of satid  ************/

   if ( strstr(input_dep,".NN") > 0) {
     strcpy(satid,"NN");
     intsatid=1;
    }else if( strstr(input_dep,".NP") > 0) {
     strcpy(satid,"NP");    
     intsatid=4;
   }else if( strstr(input_dep,".M2") > 0) {
     strcpy(satid,"M2");
     intsatid=2; 
   }else if( strstr(input_dep,".SA") > 0) {
     strcpy(satid,"SA");
     intsatid=3;
   }else if( strstr(input_dep,".SB") > 0) {
     strcpy(satid,"SB");
     intsatid=18;
   }else if( strstr(input_dep,".SC") > 0) {
     strcpy(satid,"SC");
     intsatid=5;
   }else if( strstr(input_dep,"npp_d") > 0) {
     strcpy(satid,"NPP");
     intsatid=6;
   }else if( strstr(input_dep,"SR_E") > 0) {
     strcpy(satid,"AE");
     intsatid=7;
   }else if( strstr(input_dep,"_MT1SA") > 0) {
     strcpy(satid,"MTSA");
     intsatid=13;
   }else{
     //printf("DEP filename does not have right satellite id?\n");
     //exit(3);
     strcpy(satid,"XX");
   }
   
   /*   printf("intsatid:\n%i\n", intsatid); 
	printf("satid:\n%s\n", satid); */


/******************************************     
 *  Then try to read the input Mirs data  
 ************************************************/
     printf("Input edr & dep files:\n%s\n%s\n", input_edr, input_dep);
     
     fp_edr = openfs_sw(input_edr);
     rmirs_edr(fp_edr, &numscan, &numspot, &numlayer, &numchan, satid);
     fclose(fp_edr);

     fp_dep = openfs_sw(input_dep);
     rmirs_dep(fp_dep, &numscan, &numspot, satid);
     fclose(fp_dep);

/*-----------------------------------------------------*
 * If an orbit has scan number larger than an allowed
 * limit (MAXSCAN = 1200 ), it'll be cut to the
 * limit to prevent the program from crashing.
 *-----------------------------------------------------*/


     printf("Data dimension of orbit: nscan=%d, npos=%d, nchan=%d, nlay=%d\n", numscan,numspot,numchan,numlayer);

     if( numscan > MAXSCAN )     
     {
	//numscan = MAXSCAN;
	printf("numscan > MAXSCAN, numscan=%d, MAXSCAN=%d\n", numscan, MAXSCAN );
	exit(13);
     }
    
     if( numchan > MAXCH ) {
	printf("numchan > MAXCH, numchan=%d, MAXCH=%d\n", numchan, MAXCH);
	exit(13);
     }
     
     if( numspot > MAXFOV  ) {
	printf("numspot > MAXFOV, numspot=%d, MAXFOV=%d\n", numspot, MAXFOV);
	exit(13);
     }

     if( numlayer > MAXLAY ) {
	printf("numlayer > MAXLAY, numlayer=%d, MAXLAY=%d\n", numlayer, MAXLAY);
	exit(13);
     }
     
     

 /*******************************************************
 *Figure out the EDR/DEP file nude name from input files
 ********************************************************/

     fname_edr = strstr(input_edr,"EDR_");
     fname_dep = strstr(input_dep,"DEP_");
     //printf("The input edr & dep file:\n%s\n%s\n",fname_edr, fname_dep);
     
     strcpy(EDR_name,fname_edr);
     strcpy(DEP_name,fname_dep);
     
     if( intsatid == 6 ) {
       pch = strstr(fname_edr,"_npp_d");
       strcpy(RDR_name,"SATMS");
       strncat(RDR_name,pch,strlen(pch)-7); // strip trailing ".LR.ORB" or ".HR.ORB"
     }
     else if ( intsatid == 1 ) {
       pch = strstr(fname_edr,".NN.D");
       strcpy(RDR_name,"NSS.AMAX");
       strncat(RDR_name,pch,strlen(pch)-7); // strip trailing ".LR.ORB" or ".HR.ORB"
     }
     else if ( intsatid == 2 ) {
       pch = strstr(fname_edr,".M2.D");
       strcpy(RDR_name,"NSS.AMAX");
       strncat(RDR_name,pch,strlen(pch)-7); // strip trailing ".LR.ORB" or ".HR.ORB"
     }
     else if ( intsatid == 4 ) {
       pch = strstr(fname_edr,".NP.D");
       strcpy(RDR_name,"NSS.AMAX");
       strncat(RDR_name,pch,strlen(pch)-7); // strip trailing ".LR.ORB" or ".HR.ORB"
     }
     else if ( intsatid == 3 ) {
       pch = strstr(fname_edr,".SA.D");
       strcpy(RDR_name,"NPR.TDRN");
       strncat(RDR_name,pch,strlen(pch)-14); // strip trailing ".CALIB.UAS.ORB"
     }
     else if ( intsatid == 18 ) {
       pch = strstr(fname_edr,".SB.D");
       strcpy(RDR_name,"NPR.TDRN");
       strncat(RDR_name,pch,strlen(pch)-8); // strip trailing ".UAS.ORB" or ".IMG.ORB"
     }
     else if ( intsatid == 5 ) {
       pch = strstr(fname_edr,".SC.D");
       strcpy(RDR_name,"NPR.TDRN");
       strncat(RDR_name,pch,strlen(pch)-8); // strip trailing ".UAS.ORB"
     }
     else if ( intsatid == 13 ) {
       pch = strstr(fname_edr,"_1.05_");
       strcpy(RDR_name,"MTSAPSL1A2");
       strncat(RDR_name,pch,strlen(pch)-7); // strip trailing ".CR.ORB"
     }


/*-----------------------------------------------------*
 * Generate netcdf file name
 *-----------------------------------------------------*/
/*
     if( strcmp(satid,"NA") == 0 )
	gnrt_nc_fname_npp(fname_edr, output_dir, nc_name_snd, nc_name_img);
     else
     	gnrt_nc_fname(fname_edr, output_dir, nc_name_snd, nc_name_img);
     printf("The output sounding & imaging netcdf files:\n%s\n%s\n\n", nc_name_snd, nc_name_img);
*/

    strcpy(nc_name_img,output_dir);
    strcat(nc_name_img,output_img);

    strcpy(nc_name_snd,output_dir);
    strcat(nc_name_snd,output_snd);

    printf("Output sounding & imaging netcdf4 files:\n%s\n%s\n\n", nc_name_snd, nc_name_img);

/*-----------------------------------------------------*
 * Define and write to the swath file
 *-----------------------------------------------------*/
     set_nc_snd(nc_name_snd, numscan,numspot,numlayer,numchan,satid);
     set_nc_img(nc_name_img, numscan,numspot,numchan,satid);

     //printf("rmirs_wnetcdf/Finish one orbit netcdf file !\n");
     //printf("*********************************************\n");

/*--------------------------------------------------------
 * Write out data in binary format
 *--------------------------------------------------------*/

}  /* end of rmirs_wnetcdf.c */
