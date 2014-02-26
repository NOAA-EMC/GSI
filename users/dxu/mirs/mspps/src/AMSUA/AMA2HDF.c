/**************************************************************************
 *  Program Name      : AMA2HDF.c 
 *  Type              : Main program     
 *  Function          : Program reads AMSU-A 1B* data, generates products
 *                      and creates AMSU-A SWATH file (HDF-EOS format)
 *  Input Files       : None 
 *  Output Files      : None 
 *  Subroutine Called : openfs.c, read_systim.c, read_parm.c, 
 *			read_corr_para.c, read_latbox_table.c,
 * 			read_mask.c, rama_whdfeos.c
 *  Called by	      : None
 * 
 *  Modification History:
 *      Date	   Version	   Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0     
 *   09/18/2006                 Changed for OPUS use          
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "SWATH.h"
#include "BINARY.h"
 
/*************************************************************/
FILE *openfs();
void read_systim();
void read_parm();
void read_corr_para();
void read_latbox_table();
void read_mask();
void rama_whdfeos();

/*************************************************************/
int main(int argc,char *argv[])
{
   FILE        *fp_PARAM, *fp_FNAME;
   char        *string1[] = { "BEGIN_AMSUA", "END_AMSUA"};
   char        *string2[] = { "ORBITAL", "DAILY",
                             "PENTAD",  "MONTHLY" };

   char        input_f[] = "input_A.dat";
   char        corr_f[] = "corr_para_A.dat"; 
   char        latbox_f[] = "latbox_table_A.dat"; 


  if (argc !=2 ){
    printf("Usage: ama2hdf $satname (Metop/NOAA-N/NOAA-P)...) \n");
    exit(9);
  }
  if (!strcmp(argv[1], "Metop")){
    strcpy(input_dir,data_dir);
    strcat(input_dir, "Metop/input/");

    strcpy(misc_dir, data_dir);
    strcat(misc_dir,"Metop/misc/");
    
    strcpy(output_dir,data_dir);
    strcat(output_dir, "Metop/output/");

  }else if (!strcmp(argv[1], "NOAA-P")){
    strcpy(input_dir,data_dir);
    strcat(input_dir, "NOAA-P/input/");
    
    strcpy(misc_dir, data_dir);
    strcat(misc_dir,"NOAA-P/misc/");

    strcpy(output_dir,data_dir);
    strcat(output_dir, "NOAA-P/output/");

  }
  else if (!strcmp(argv[1], "NOAA-N")){
    strcpy(input_dir,data_dir);
    strcat(input_dir, "NOAA-N/input/");
    
    strcpy(misc_dir, data_dir);
    strcat(misc_dir,"NOAA-N/misc/");

    strcpy(output_dir,data_dir);
    strcat(output_dir, "NOAA-N/output/");

  }else{
      printf("Not a right satellite name! Use Metop/NOAA-N/NOAA-P... \n");
      exit(18);
  }

  //  printf("Dirnames: %s\n %s\n %s\n",input_dir,misc_dir,output_dir);

  /************************************************
   * Get file names
   ************************************************/
  strcpy(mask_fname,data_dir);
  strcat(mask_fname,"control/mask.bin");

  strcpy(input_fname, input_dir);
  strcat(input_fname,input_f);

  strcpy(corr_fname, input_dir);
  strcat(corr_fname,corr_f);

  strcpy(latbox_fname, input_dir);
  strcat(latbox_fname,latbox_f);
  
  

/*---------------------------------------------------*
 * Read in system time
 *---------------------------------------------------*/
   read_systim(string1[0],string2[0]);
   printf("\n AMA2HDF/ Successfully read system time !\n"); 
   printf("***************************************************\n");

/*---------------------------------------------------*
 * Open and read parameter file 
 *---------------------------------------------------*/
   fp_PARAM = openfs(input_fname);

   read_parm(fp_PARAM);

/*---------------------------------------------------*
 * Open and read AT correction parameter 
 *---------------------------------------------------*/
   fp_PARAM = openfs(corr_fname);

   read_corr_para(fp_PARAM);

/*---------------------------------------------------*
 * Open and read lat_box table
 *---------------------------------------------------*/
   fp_PARAM = openfs(latbox_fname);

   read_latbox_table(fp_PARAM);

/*---------------------------------------------------*
 * Open and read mask data 
 *---------------------------------------------------*/
   fp_PARAM = openfs(mask_fname);

   read_mask(fp_PARAM);

/*---------------------------------------------------*
 * Open AMA1B* file name file
 *---------------------------------------------------*/


   strcpy(ama_list,misc_dir);
   strcat(ama_list,"amsuafile");

   if((fp_FNAME=fopen(ama_list,"r")) == NULL)
   {
     printf("AMA2HDF/Can't open AMA1B filename file: amsuafile !\n");
     exit(1);
   }
   printf(" Successfully open AMA1B filename file: amsuafile !\n");

/*---------------------------------------------------*
 * Read AMA1B* files and generate products to store 
 * in HDF-EOS swath file 
 *---------------------------------------------------*/

   rama_whdfeos(fp_FNAME);

/*---------------------------------------------------*
 * Read in system time
 *---------------------------------------------------*/

   read_systim(string1[1],string2[0]);

   exit(0);
}  /* end of AMA2HDF.c */
