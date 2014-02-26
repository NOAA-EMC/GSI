/**************************************************************************
 *  Program Name      : MHS2HDF.c
 *  Type              : Main program
 *  Function          : Program reads NOAA-15 MHS 1B* data, computes 
 *			antenna temperatures and creates preliminary 
 *			MHS HDF-EOS swath file 
 *			(HDF-EOS format)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : openfs.c, read_systim.c, read_parm.c,
 *                      read_biastable.c, read_latbox_table.c,
 *                      read_mask.c, rmhs_whdfeos.c
 *  Called by         : None
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 *   09/21/2006                 Changed for OPUS ready    jzhao      
 *************************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "SWATH.h"
 
/***************************************************************/
FILE *openfs();
void read_systim();
void read_parm();
void read_biastable();
void read_latbox_table();
void read_mask();
void rmhs_whdfeos();

/***************************************************************/
int main(int argc, char * argv[])
{
   FILE        *fp_PARAM, *fp_FNAME;
   char        *string1[] = { "BEGIN_MHS", "END_MHS"};
   char        *string2[] = { "ORBITAL", "DAILY",
                             "PENTAD",  "MONTHLY" };
   char         input_f[] = "input_M.dat";

   char		latbox_f[] = "latbox_table_M.dat";
   

  if (argc !=2 ){
    printf("Usage: runmhs_pre_m2/runmhs_pre_nn/runmhs_pre_np $satname (Metop/NOAA-N/NOAA-P) \n");
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

  }
  else if (!strcmp(argv[1], "NOAA-N")){
    strcpy(input_dir,data_dir);
    strcat(input_dir, "NOAA-N/input/");
    
    strcpy(misc_dir, data_dir);
    strcat(misc_dir,"NOAA-N/misc/");

    strcpy(temp_dir, data_dir);
    strcat(temp_dir,"NOAA-N/tmp/");

    strcpy(output_dir,data_dir);
    strcat(output_dir, "NOAA-N/output/");

  }
  else if (!strcmp(argv[1], "NOAA-P")){
    strcpy(input_dir,data_dir);
    strcat(input_dir, "NOAA-P/input/");
    
    strcpy(misc_dir, data_dir);
    strcat(misc_dir,"NOAA-P/misc/");

    strcpy(temp_dir, data_dir);
    strcat(temp_dir,"NOAA-P/tmp/");

    strcpy(output_dir,data_dir);
    strcat(output_dir, "NOAA-P/output/");

  }
  else{
      printf("Not a right satellite name! Use Metop/NOAA-N/NOAA-P... \n");
      exit(18);
  }



/*----------------------------------------------------*
 * Read in system time
 *----------------------------------------------------*/
   read_systim(string1[0],string2[0]);
   printf("\n MHS2HDF/ Successfully read system time !\n"); 
   printf("***************************************************\n");


  /************************************************
   * Get file names
   ************************************************/
  strcpy(mask_fname,data_dir);
  strcat(mask_fname,"control/mask.bin");

  strcpy(input_fname, input_dir);
  strcat(input_fname,input_f);

  /***********************************
  strcpy(corr_fname, input_dir);
  strcat(corr_fname,corr_f);
  ****************************************/

  strcpy(latbox_fname, input_dir);
  strcat(latbox_fname,latbox_f);


/*----------------------------------------------------*
 * Open and read the parameter file 
 *----------------------------------------------------*/
   fp_PARAM = openfs(input_fname);

   read_parm(fp_PARAM);

/*----------------------------------------------------*
 * Open and read the RFI correction tables
 *----------------------------------------------------*/


/*----------------------------------------------------*
 * Open and read the latbox table 
 *----------------------------------------------------*/
   fp_PARAM = openfs(latbox_fname);

   read_latbox_table(fp_PARAM);

/*----------------------------------------------------*
 * Open and read the mask data 
 *----------------------------------------------------*/
   fp_PARAM = openfs(mask_fname);

   read_mask(fp_PARAM);

/*----------------------------------------------------*
 * Open MHS 1B* file name file
 *----------------------------------------------------*/
   strcpy(mhs_list,misc_dir);
   strcat(mhs_list,"mhsfile");

   if((fp_FNAME=fopen(mhs_list,"r")) == NULL)
   {
     printf("MHS2HDF/Can't open MHS1B filename file: mhsfile !\n");
     exit(1);
   }
   printf(" Successfully open MHS1B filename file: mhsfile !\n");

/*----------------------------------------------------*
 * Read MHS1B* files and compute ATs to be stored in
 * the preliminary swath file (in HDF-EOS format) 
 *----------------------------------------------------*/

   rmhs_whdfeos(fp_FNAME);

/*----------------------------------------------------*
 * Read in system time
 *----------------------------------------------------*/

   read_systim(string1[1],string2[0]);
   
   exit(0);
}  /* end of MHS2HDF.c */
