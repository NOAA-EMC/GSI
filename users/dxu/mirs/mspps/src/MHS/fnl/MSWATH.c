/**************************************************************************
 *  Program Name      : MSWATH.c
 *  Type              : Main program
 *  Function          : Program reads from AMSU-A and preliminary MHS 
 *                      swath files, generates MHS products and appends
 *                      the products to the preliminary MHS swath to
 *                      create the final MHS HDF-EOS swath 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : openfs.c, read_parm.c, fov.c,read_latbox_table.c,
 *                      read_lza_parameter.c,read_mask_snow.c, rmhs_whdfeos.c
 *  Called by         : None
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 *   11/28/2001      v2.1      Add code reading a land/sea mask for snow 
 *			       computation and code reading a latbox table
 *   09/28/06     Modified for OPUS ready    jzhao 
 *   04/12/06     Add read LZA functions (ORA update)     jz
 *    02/08       ADD snow fall rate    jz
 **************************************************************************/
#include "MSWATH_INCLUDE.h"
#include "SWATH.h"
#include "BINARY.h"
 
/******************************************************/
FILE *openfs();
void read_parm();
void read_mask_snow();
void read_latbox_table();
void read_elev();
void fov();
void read_lza_parameter();
void read_iwp_s_table();

void rmhs_whdfeos();

/******************************************************/
int main(int argc, char * argv[])
{
   FILE         *fp_PARAM, *fp_FNAME_A, *fp_FNAME_M;

   char         input_f[] = "input_prod_M.dat";
   char         mask_f[] = "mask1000.bin";
   char         latbox_f[] = "latbox_table_M.dat";
   char         lza_f[] = "lza_parameters.dat";
   char         iwp_s_f[] = "iwp_s_table.dat";

   char     	file_A[]="swath_A.list";
   char     	file_M[]="swath_M.list";



  if (argc !=2 ){
    printf("Usage: runmhs_fnl_m2/runmhs_fnl_nn/runmhs_fnl_np $satname (Metop/NOAA-N/NOAA-P) \n");
    exit(9);
  }
  if (!strcmp(argv[1], "Metop")){
    strcpy(input_dir,data_dir);
    strcat(input_dir, "Metop/input/");

    strcpy(misc_dir, data_dir);
    strcat(misc_dir,"Metop/misc/");

    strcpy(temp_dir, data_dir);
    strcat(temp_dir,"Metop/tmp/");

    strcpy(output_dir,data_dir);
    strcat(output_dir, "Metop/output/");

  }else if (!strcmp(argv[1], "NOAA-N")){
    strcpy(input_dir,data_dir);
    strcat(input_dir, "NOAA-N/input/");
    
    strcpy(misc_dir, data_dir);
    strcat(misc_dir,"NOAA-N/misc/");

    strcpy(temp_dir, data_dir);
    strcat(temp_dir,"NOAA-N/tmp/");

    strcpy(output_dir,data_dir);
    strcat(output_dir, "NOAA-N/output/");

  }else if (!strcmp(argv[1], "NOAA-P")){
    strcpy(input_dir,data_dir);
    strcat(input_dir, "NOAA-P/input/");
    
    strcpy(misc_dir, data_dir);
    strcat(misc_dir,"NOAA-P/misc/");

    strcpy(temp_dir, data_dir);
    strcat(temp_dir,"NOAA-P/tmp/");

    strcpy(output_dir,data_dir);
    strcat(output_dir, "NOAA-P/output/");

   }else{
      printf("Not a right satellite name! Use Metop/NOAA-N/NOAA-P \n");
      exit(18);
  }

 /************************************************
   * Get file names
   ************************************************/
  strcpy(mask_fname,data_dir);
  strcat(mask_fname,"control/");
  strcat(mask_fname,mask_f);

  strcpy(iwp_s_fname,data_dir);
  strcat(iwp_s_fname,"control/");
  strcat(iwp_s_fname,iwp_s_f);

  strcpy(input_fname, input_dir);
  strcat(input_fname,input_f);

 
  strcpy(lza_fname, data_dir);
  strcat(lza_fname,"control/");
  strcat(lza_fname,lza_f);

  strcpy(latbox_fname, input_dir);
  strcat(latbox_fname,latbox_f);


/*---------------------------------------*
 * Open and read the parameter file 
 *---------------------------------------*/
   fp_PARAM = openfs(input_fname);

   read_parm(fp_PARAM);

/*----------------------------------------------------*
 * Open and read the latbox table
 *----------------------------------------------------*/
   fp_PARAM = openfs(latbox_fname);

   read_latbox_table(fp_PARAM);

/*---------------------------------------*
 * Open and read the mask file
 *---------------------------------------*/
   fp_PARAM = openfs(mask_fname);

   read_mask_snow(fp_PARAM);

/*---------------------------------------*
 * Open and read the elevation data
 *---------------------------------------*/


/****************************************
   * Open and read the LZA parameter file
*---------------------------------------*/
   fp_PARAM = openfs(lza_fname);

   read_lza_parameter(fp_PARAM);
 
/*---------------------------------------*
 * Open and read the IWP-S lookup table
 *---------------------------------------*/
   fp_PARAM = openfs(iwp_s_fname);

   read_iwp_s_table(fp_PARAM);


/*---------------------------------------*
 * Calculate FOV sizes
 *---------------------------------------*/

   fov();

/*---------------------------------------*
 * Open the file containing the names of
 * AMSU-A swath files 
 *---------------------------------------*/

   strcpy(agrnl_list,misc_dir);
   strcat(agrnl_list,file_A);

   if((fp_FNAME_A=fopen(agrnl_list, "r")) == NULL)
   {
     printf("MSWATH/Can't open orbital swath filename file : %s!\n",agrnl_list);     
     exit(1);
   }
   printf("MSWATH/Successfully open AMSU-A orbital swath filename file !\n");

/*---------------------------------------*
 * Open the file containing the names of
 * MHS swath files 
 *---------------------------------------*/

   strcpy(mgrnl_list,misc_dir);
   strcat(mgrnl_list,file_M);


   printf("***************************************************\n");
   if((fp_FNAME_M=fopen( mgrnl_list , "r")) == NULL)
   {
     printf("MSWATH/Can't open orbital swath filename file : %s!\n",mgrnl_list);     
     exit(1);
   }
   printf("MSWATH/Successfully open MHS orbital swath filename file !\n");

/*---------------------------------------*
 * Read swath files (A & MHS) and generate 
 * MHS products for the final MHS 
 * HDF-EOS swath file 
 *---------------------------------------*/

   rmhs_whdfeos(fp_FNAME_A, fp_FNAME_M);

   exit(0);
}  /* end of MSWATH.c   */
