/***********************************************************************
 *  Program Name      : rmhs_whdfeos.c
 *  Type              : Subroutine
 *  Function          : Program reads from AMSU-A and MHS swaths,
 *                      generates MHS products and creates the final
 *                      MHS HDF-EOS and binary swaths
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : read_swath_A.c, read_swath_M.c, spread.c, 
 *			init.c, mask_stype_snow.c, avn.c, gnrt_prod.c, 
 *			set_sw.c, bin_time.c, wrt_bin.c
 *  Called by         : BSWATH.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 *   01/26/2001      v2.1    Add the generation of binary swath file
 *   03/08/2001      v2.2    Change from using GDAS data to using AVN data
 *   11/28/2001      v2.3    Add a special land/sea mask for snow computation
 *   09/28/2006      Modified for OPUS readiness   jzhao(QSS)
 *   11/05/07        Compare time_ to get switch_scnnum   
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"
#include "EBINARY.h"

/**************************************************/
void spread();
void convert_mhs_amsub();
void initprod();
void avn();
void mask_stype_snow();
void gnrt_prod();
void set_sw();
void bin_time();
void wrt_bin();
long int read_swath_A(); 
long int read_swath_M();

/**************************************************/
void rmhs_whdfeos(FILE *fp_FNAME_A, FILE *fp_FNAME_M)
{
   TOTNUM_OF_SCAN  numscan_A, numscan_M, numscan;
   short int       i, inx_orb = 0,k;
   long int	   status;

   int		   hrint;

   short int	   hs, he; 
   short int       avn_time[9] = { 0, 3, 6, 9, 12, 15, 18, 21, 24 };
   char        	   hr[3];
   char            hdf_name[120], fname_A[80], fname_M[80]; 
   char		   bin_name[120]; 
   char	           swathfile_A[120], swathfile_M[120];
   char		   bin[]=".BIN";
   
/*----------------------------------------------------*
 * Read swath file names (A and B) and open these files
 *----------------------------------------------------*/
   while(fgets(fname_A,80,fp_FNAME_A) != NULL)
   {
     i=strlen(fname_A);
     printf("rmhs_whdfeos/SWATH file name:%s.\n",fname_A);
     printf("length is %i\n",i);
     strcpy(swathfile_A, output_dir);
     strcat(swathfile_A,"swath/");
     strncat(swathfile_A, fname_A,i-1);
     printf("rmhs_whdfeos/SWATH file name:%s.\n",swathfile_A);

     status = read_swath_A(swathfile_A,SWATH_NAME_A, &numscan_A);
     printf("rmhs_whdfeos/from read_swath_A  %ld\n", status);


     fgets(fname_M,80,fp_FNAME_M); 
     i=strlen(fname_M);
     printf("rmhs_whdfeos/SWATH file name: %s.\n",fname_M);

     strcpy(swathfile_M, temp_dir);
     strncat(swathfile_M, fname_M,i-1);

     status = read_swath_M(swathfile_M,SWATH_NAME_M, &numscan_M);
     printf("rmhs_whdfeos/from read_swath_M  %ld\n", status);

     numscan = numscan_M;

/* Convert MHS channel-2 and channel-5 to AMSU-B channel-2 and channel-5 */
     convert_mhs_amsub(numscan);

/********************************************
 *Now compare the time_A and time_M
 *to see whether there are time mismatch,
 *Here only consider mhs first 1~2 is switched
 *in from the previous granule
*********************************************/
     switch_scnnum=0;
     for (k=0;k<2;k++){
       if (time_A[0] - time_tai93[k] >1) switch_scnnum++;       
     }
     printf("The 1st scan time of AMSUA&MHS and switch_scnnum are %f %f %hi \n",time_A[0],time_tai93[0],switch_scnnum);
 
/*-----------------------------------*
 * Spread AMSU-A fields from AMSU-A 
 * swath size to MHS swath size
 *-----------------------------------*/
  
     spread(numscan_A, numscan_M);

/*-----------------------------------*
 * Initialize product arrays
 *-----------------------------------*/

     initprod();
 
/*----------------------------------------------*
 * Determine surface type from mask data
 *----------------------------------------------*/

     mask_stype_snow(numscan);

/*-----------------------------------*
 * Determine which AVN files to read 
 * and read the data
 *-----------------------------------*/
/*     hs = hour[0];
     he = hour[numscan - 1]; 
*/
     hr[2] = '\0';
     hr[0] = fname_M[20];
     hr[1] = fname_M[21];
     hrint = atoi(hr);
     hs = hrint;

     hr[0] = fname_M[26];
     hr[1] = fname_M[27];
     hrint = atoi(hr);
     he = hrint;

     printf("The starting and ending hours of this orbit are %d and %d \n",hs,he);

     for(i = 0; i < 8; i++)
     {
        if(hs >= avn_time[i] && hs < avn_time[i+1])
        {
          avnset[0] = i;
          avnhr[0] = avn_time[i];
          avnhr[1] = avn_time[i+1];
          if(avn_time[i+1] == 24)
            avnset[1] = 0;
          else
            avnset[1] = i+1;
        }

        if(he >= avn_time[i] && he < avn_time[i+1])
        {
          avnhr[2] = avn_time[i+1];
          if(avn_time[i+1] == 24)
            avnset[2] = 0;
          else
            avnset[2] = i+1;
        }
     }

     if(avnset[1] == avnset[2])
       num_avn = 2;
     else if(avnset[1] == avnset[2] - 1)
       num_avn = 3;
     else if(avnset[1] == 7 && avnset[2] == 0)
       num_avn = 3;
     else
       num_avn = -1;

     if(num_avn == 2)
       printf("This orbit needs 2 AVN files. The times are %d %d \n",avnhr[0],avnhr[1]);
     else if(num_avn == 3)
       printf("This orbit needs 3 AVN files. The times are %d %d %d \n",avnhr[0],avnhr[1],avnhr[2]);

     avn();

/*-----------------------------------*
 * Generate products 
 *-----------------------------------*/
     printf("Numscan is %d\n",numscan);
     gnrt_prod(numscan);
     printf("Finish generating all products \n");

/*-----------------------------------*
 * Generate the output HDF-EOS swath 
 * file name
 *-----------------------------------*/
     /*i=strlen(fname_M);
     strcpy(hdf_name, direct_swath_out);
     strncat(hdf_name, fname_M,i-1);
     */

     strcpy(hdf_name, swathfile_M);

     printf("HDF file name %s\n",hdf_name);

/*-----------------------------------*
 * Set up the HDF-EOS swath file
 *-----------------------------------*/
     printf("rmhs_whdfeos/hdf_filename: %s\n", hdf_name);
     set_sw(hdf_name, numscan);

     printf("rmhs_whdfeos/Finish one swath HDF-EOS file !\n");
     printf("*********************************************\n");

/*-----------------------------------*
 * Generate binary swath file name 
 *-----------------------------------*/
     strcpy(bin_name, output_dir);
     strcat(bin_name,"biny/");
     strncat(bin_name, fname_M, BUFR_FNL);
     strcat(bin_name, bin);
     printf("BUFR file name %s\n",bin_name);

/*-----------------------------------*
 * Calculate parameters for the binary 
 * file header record 
 *-----------------------------------*/
     orbit_start_time = hour[0] * 3600 + minute[0] * 60 + second[0];
     orbit_end_time = hour[numscan-1] * 3600 + minute[numscan-1] * 60 + second[numscan-1]; 

/*-----------------------------------*
 * Calculate output time field for 
 * the binary file 
 *-----------------------------------*/

     bin_time(numscan); 

/*-----------------------------------*
 * Write out data in binary format 
 *-----------------------------------*/
     wrt_bin(bin_name, numscan);
     printf("rama_whdfeos/Finish one binary swath file ! \n");

     inx_orb += 1;
   }

   printf("\n-------------------------------------------------------------\n");
   printf("rmhs_whdfeos/    TOTAL NUMBER OF ORBIT(S) PROCESSED IS %d!\n",inx_orb);
   printf("-------------------------------------------------------------\n");

}  /* End of rmhs_whdfeos.c */
