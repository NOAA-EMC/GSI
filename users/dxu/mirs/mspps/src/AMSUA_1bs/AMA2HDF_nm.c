/**************************************************************************
 *  Program Name      : AMA2HDF.c 
 *  Type              : Main program     
 *  Function          : Program reads NOAA-15 AMSU-A 1B* data, computes
 *			antenna temperatures, generates products and 
 *			creates AMSU-A HDF-EOS swath file and binary 
 *			swath file
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
 *   11/08/2005     v2.1          changed the pathes for the input files
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
int main()
{
   FILE        *fp_PARAM, *fp_FNAME;
   char        *string1[] = { "BEGIN_AMSUA", "END_AMSUA"};
   char        *string2[] = { "ORBITAL", "DAILY",
                             "PENTAD",  "MONTHLY" };
   char        input_fname[] = "../data/NOAA-M/input/input_A.dat";
   char        corr_fname[] = "../data/NOAA-M/input/corr_para_A.dat"; 
   char        latbox_fname[] = "../data/NOAA-M/input/latbox_table_A.dat"; 
   char        mask_fname[]="../data/control/mask.bin";

/*---------------------------------------------------*
 * Read in system time
 *---------------------------------------------------*/
   read_systim(string1[0],string2[0]);
   printf("\n AMA2HDF/ Successfully read system time!\n"); 
   printf("***************************************************\n");

/*---------------------------------------------------*
 * Open and read parameter file 
 *---------------------------------------------------*/
   fp_PARAM = openfs(input_fname);

   read_parm(fp_PARAM);

/*---------------------------------------------------*
 * Open and read AT correction parameters 
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
   if((fp_FNAME=fopen("../data/NOAA-M/misc/amsuafile_nm","r")) == NULL)
   {
     printf("AMA2HDF/Can't open AMA1BS filename file: amsuafile_nm !\n");
     exit(1);
   }
   printf("AMA2HDF/Successfully open AMA1BS filename file: amsuafile_nm !\n");

/*---------------------------------------------------*
 * Read AMA1B* files and generate products to store 
 * in HDF-EOS and binary swath file 
 *---------------------------------------------------*/

   rama_whdfeos(fp_FNAME);

/*---------------------------------------------------*
 * Read in system time
 *---------------------------------------------------*/

   read_systim(string1[1],string2[0]);
   
   return 0;
}  /* end of AMA2HDF.c */
