/***********************************************************************
 *  Program Name      : ramb_whdfeos.c
 *  Type              : Subroutine
 *  Function          : Program reads AMSU-B 1B* data, computes antenna
 *                      temperature, and creates preliminary AMSU-B 
 *			HDF-EOS swath file
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : openfs.c ramb_hd.c ramb_scn.c scn_data.c
 *                      gnrt_fname.c init.c mask_stype.c gnrt_at.c
 *                      set_sw.c
 *  Called by         : AMB2HDF.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 *   01/10/2006                   Modified for OPUS  jzhao (QSS)
 *   04/19/2006                   Modified init() as initat()  jzhao
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/******************************************************/
FILE *openfs();
void gnrt_fname(char*, char*);
void ramb_hd();
void ramb_scn(); 
void scn_data();
void initat();
void mask_stype();
void gnrt_at();
void set_sw();

/******************************************************/
void ramb_whdfeos(FILE *fp_FNAME)
{

   FILE             *fp_AMB1BS;
   TOTNUM_OF_SCAN   numscan;
   short int        inx_orb = 0, i=0;

   char        hdf_name[120]="", filename[120]="", fname[120]="";

/*--------------------------------------------*
 * Open AMSU-B 1B* orbital files 
 *--------------------------------------------*/
  while(fgets(fname,120,fp_FNAME) != NULL)
  {
     /*chomp the last '\n'   */
     i=strlen(fname);

     strncat(filename, fname,i-1);
     printf("\nramb_whdfeos/ The whole AMSUB file name: %s.\n",filename);

     fp_AMB1BS = openfs(filename);

     ramb_hd(fp_AMB1BS);

/*-----------------------------------------------------*
 * If an orbit has scan number larger than an allowed
 * limit (MAXSCANLINE_B = 3000), it'll be cut to the
 * limit to prevent the program from crushing.
 *-----------------------------------------------------*/
     numscan = hblock1.last_scan_record - hblock1.first_scan_record + 1;
     printf("Scanline number in this orbit: %d\n", numscan);

     if( numscan > MAXSCANLINE_B)     
     {
         numscan = MAXSCANLINE_B;
         printf("Warning: The number of scanline is larger than the \n");
         printf("maximum number of scanlines allowed. The program is \n");
         printf("running by setting the parameter numscan = MAXSCANLINE_B. \n");
         printf("You might need to change MAXSCANLINE_B to a larger number \n");
         printf("so the real numscan can be used. \n");
     }

     ramb_scn(fp_AMB1BS, numscan);
 
     scn_data(numscan);

/*----------------------------------------------*
 * Generate HDF-EOS swath file name
 *----------------------------------------------*/
     gnrt_fname(filename, hdf_name);
     printf("HDF file name %s\n",hdf_name);

/*----------------------------------------------*
 * Initialize AT and product arrays
 *----------------------------------------------*/

     initat();
 
/*----------------------------------------------*
 * Determine surface type from mask data 
 *----------------------------------------------*/

     mask_stype(numscan);

/*----------------------------------------------*
 * Generate ATs from counts
 *----------------------------------------------*/

     gnrt_at(numscan);
     printf("Finish generating all products \n");
 
/*----------------------------------------------*
 * Write data fields to the swath file 
 *----------------------------------------------*/
     printf("ramb_whdfeos/hdf_filename: %s\n", hdf_name);
     set_sw(hdf_name, numscan);

     printf("ramb_whdfeos/Finish one swath HDF_EOS file !\n");
     printf("*********************************************\n");

     inx_orb += 1;
   }

   printf("\n-------------------------------------------------------------\n");
   printf("ramb_whdfeos/    TOTAL NUMBER OF ORBITS PROCESSED IS %d!\n",inx_orb);
   printf("-------------------------------------------------------------\n");

} /* ramb_whdfeos.c */
