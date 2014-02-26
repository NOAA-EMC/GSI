/***********************************************************************
 *  Program Name      : rmhs_whdfeos.c
 *  Type              : Subroutine
 *  Function          : Program reads AMSU-B 1B* data, computes antenna
 *			temperature, and creates AMSU-B swath file 
 *			(HDF-EOS format)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : openfs.c rmhs_hd.c rmhs_scn.c scn_data.c 
 *			gnrt_fname.c init.c mask_stype.c gnrt_at.c 
 *			set_sw.c 
 *  Called by         : MHS2HDF.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/**********************************************************/
FILE *openfs();
void rmhs_hd();
void rmhs_scn(); 
void scn_data();
void initall();
void mask_stype();
void gnrt_at();
void set_sw();
char *gnrt_fname();

/**********************************************************/
void rmhs_whdfeos(FILE *fp_FNAME)
{

   FILE             *fp_MHS1BS;
   TOTNUM_OF_SCAN   numscan;
   short int        inx_orb = 0,i;

   /* char        mhs_direct[]="../../mhs2hdf_input_data/"; */

   char        *hdf_name, filename[120]= "", fname[120]="";

/*---------------------------------------------*
 * Open AMSU-B 1B* orbital files 
 *---------------------------------------------*/
   while(fgets(fname,120,fp_FNAME) != NULL)
   {
     i=strlen(fname);
     
     printf("\nrmhs_whdfeos/MHS file name: %s.\n",fname);

     strncpy(filename, fname,i-1);

     fp_MHS1BS = openfs(filename);

     rmhs_hd(fp_MHS1BS);

/*-----------------------------------------------------*
 * If an orbit has scan number larger than an allowed
 * limit (MAXSCANLINE_M = 3000), it'll be cut to the
 * limit to prevent the program from crushing.
 *-----------------------------------------------------*/
     numscan = hblock.last_scan_record;
     printf("Scanline number in this orbit: %d\n", numscan);

     if( numscan > MAXSCANLINE_M)     
     {
         numscan = MAXSCANLINE_M;
         printf("Warning: The number of scanlines is larger than the \n");
         printf("maximum number of scanlines allowed. The program is \n");
         printf("running by setting the parameter numscan = MAXSCANLINE_M. \n"); 
         printf("You might need to change MAXSCANLINE_M to a larger number \n");
         printf("so the real numscan can be used.\n");
     }

     rmhs_scn(fp_MHS1BS, numscan);

     scn_data(numscan);

/*---------------------------------------------*
 * Generate HDF-EOS swath file name
 *---------------------------------------------*/
     /* strncpy(hdf_name, gnrt_fname(), HDF_FNL); */
     hdf_name = gnrt_fname();
     printf("HDF file name %s\n",hdf_name);

/*---------------------------------------------*
 * Initialize the AT and product arrays
 *---------------------------------------------*/

     initall();
 
/*---------------------------------------------*
 * Determine surface type from mask data 
 *---------------------------------------------*/

     mask_stype(numscan);

/*---------------------------------------------*
 * Compute ATs from counts
 *---------------------------------------------*/

     gnrt_at(numscan);
     printf("Finish generating all products \n");
 
/*---------------------------------------------*
 * Write data to the swath file
 *---------------------------------------------*/
     printf("rmhs_whdfeos/hdf_filename: %s\n", hdf_name);
     set_sw(hdf_name, numscan);

     printf("rmhs_whdfeos/Finish one swath HDF_EOS file !\n");
     printf("*********************************************\n");

     inx_orb += 1;
   }

   printf("\n-------------------------------------------------------------\n");
   printf("rmhs_whdfeos/    TOTAL OF %d ORBITS ARE PROCESSED!\n",inx_orb);
   printf("-------------------------------------------------------------\n");

}  /* rmhs_whdfeos.c  */
