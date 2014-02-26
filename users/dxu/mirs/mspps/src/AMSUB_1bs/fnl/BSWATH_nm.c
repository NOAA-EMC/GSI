/**************************************************************************
 *  Program Name      : BSWATH.c
 *  Type              : Main program
 *  Function          : Program reads from AMSU-A and preliminary AMSU-B
 *                      swath files, generates AMSU-B products and appends
 *                      the products to the preliminary AMSU-B swath to
 *                      create the final AMSU-B HDF-EOS swath 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : openfs.c, read_parm.c, read_elev.c, fov.c,
 *                      read_mask_snow.c, read_latbox_table.c, ramb_whdfeos.c,
 *                      read_lza_parameter.c
 *  Called by         : None
 *
 **************************************************************************/
#include "BSWATH_INCLUDE.h"
#include "SWATH.h"
#include "BINARY.h"
 
/******************************************************/
FILE *openfs();
void read_parm();
void read_mask_snow();
void read_latbox_table();
void read_lza_parameter();
void read_elev();
void fov();
void ramb_whdfeos();

int main()
{
   FILE         *fp_PARAM, *fp_FNAME_A, *fp_FNAME_B;

   char         input_fname[] = "../data/NOAA-M/input/input_prod_B.dat";
   char         mask_fname[] = "../data/control/mask1000.bin";
   char         latbox_fname[] = "../data/control/latbox_table_B.dat";
   char         lza_fname[] = "../data/control/lza_parameters.dat";

   char     	snamefile_A[50]="../data/NOAA-M/misc/swath_A_nm.list";
   char     	snamefile_B[50]="../data/NOAA-M/misc/swath_B_nm.list";

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
 * Open and read the LZA parameter file
 *---------------------------------------*/
   fp_PARAM = openfs(lza_fname);

   read_lza_parameter(fp_PARAM);

/*---------------------------------------*
 * Open and read the elevation data
 *---------------------------------------*/
/*   if((fp_PARAM = fopen(elev_fname, "rb")) == NULL)
   {
      printf("Fail to open elevation data \n");
      exit(1);
   }

   read_elev(fp_PARAM);
   printf("Finish reading elevation data! \n");
*/

/*---------------------------------------*
 * Calculate FOV sizes
 *---------------------------------------*/

   fov();

/*---------------------------------------*
 * Open the file containing the names of
 * AMSU-A swath files 
 *---------------------------------------*/
   printf("***************************************************\n");
   if((fp_FNAME_A=fopen(snamefile_A, "r")) == NULL)
   {
     printf("BSWATH/Can't open orbital swath filename file : %s!\n",snamefile_A);     
     exit(1);
   }
   printf("BSWATH/Successfully open AMSU-A orbital swath filename file !\n");

/*---------------------------------------*
 * Open the file containing the names of
 * AMSU-B swath files 
 *---------------------------------------*/
   printf("***************************************************\n");
   if((fp_FNAME_B=fopen(snamefile_B, "r")) == NULL)
   {
     printf("BSWATH/Can't open orbital swath filename file : %s!\n",snamefile_B);     
     exit(1);
   }
   printf("BSWATH/Successfully open AMSU-B orbital swath filename file !\n");

/*---------------------------------------*
 * Read swath files (A & B) and generate 
 * AMSU-B products for the final AMSU-B 
 * HDF-EOS swath file 
 *---------------------------------------*/

   ramb_whdfeos(fp_FNAME_A, fp_FNAME_B);
   
   return 0;

}  /* end of BSWATH.c   */
