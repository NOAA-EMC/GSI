/***************************************************************************
 *  Program Name      : rama_whdfeos.c
 *  Type              : Subroutine
 *  Function          : Program reads AMSU-A 1B* data, generates products
 *                      and creates AMSU-A HDF-EOS swath file and binary 
 *			swath file 
 *  Input Files       : amafile 
 *  Output Files      : None
 *  Subroutine Called : gnrt_fname.c, gnrt_bin_fname.c, openfs.c, rama_hd.c, 
 *			rama_scn.c, scn_data.c, init.c, mask_stype.c, avn.c
 *			gnrt_stime.c, gnrt_prod.c, set_sw.c, wrt_bin.c 
 *  Called by         : AMA2HDF.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *  08/30/2000      v2.0
 *  01/26/2001      v2.1     Add the generation of binary swath file
 *  03/08/2001      v2.2     Change from using GDAS data to using AVN data
 *  09/18/2006               Changed for OPUS   jzhao(QSS)
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"
#include "EBINARY.h"
#include <string.h>


/*****************************************************************/
void gnrt_fname(char *);
void gnrt_bin_fname(char *);
FILE *openfs();
void rama_hd();
void rama_scn();
void scn_data();
void initall();
void mask_stype();
void avn();
void gnrt_stime();
void gnrt_prod();
void set_sw();
void wrt_bin();

/*****************************************************************/
void rama_whdfeos(FILE *fp_FNAME)
{

   FILE        *fp_AMA1B;

   short int   numscan, i;
   short int   inx_orb = 0;
   short int   hs, he;
   int	       hrint;
   short int   avn_time[9] = { 0, 3, 6, 9, 12, 15, 18, 21, 24 };

   /* char        ama_direct[]="../ama2hdf_input_data/"; */ 
   char        hdf_name[180], bin_name[180], fname[120],rfname[80];
   char	       hr[3];
   char   filename[120]="";
   char *pst;

/*-----------------------------------------------------*
 * Open AMSU-A 1B* orbital files 
 *-----------------------------------------------------*/
   while(fgets(fname,120,fp_FNAME) != NULL)
   {
     strcpy(filename,"");
     i=strlen(fname);
     printf("\nrama_whdfeos/AMA file name: %s.\n",fname);

     strncat(filename, fname,i-1);

     fp_AMA1B = openfs(filename);


     rama_hd(fp_AMA1B);

/*-----------------------------------------------------*
 * If an orbit has scan number larger than an allowed
 * limit (MAXSCANLINE_A = 950), it'll be cut to the
 * limit to prevent the program from crushing.
 *-----------------------------------------------------*/
     numscan = hblock.last_scan_record;
     printf("Scanline number in this orbit: %d\n", numscan);

     if( numscan > MAXSCANLINE_A)     
     {
         numscan = MAXSCANLINE_A;

         printf("Warning: The number of scanline is larger than the \n");
         printf("maximum scanline number allowed. The program is running\n");
         printf("by setting numscan = MAXSCANLINE_A, but you might need\n");
         printf("to change MAXSCANLINE_A to a larger number to use the \n");
         printf("real numscan. \n");
     }
     rama_scn(fp_AMA1B, numscan);
  
     scn_data(numscan);

/*-----------------------------------------------------*
 * Generate HDF-EOS swath file name
 *-----------------------------------------------------*/
     /*strncpy(hdf_name, gnrt_fname(), HDF_FNL); */
     gnrt_fname(hdf_name);
     printf("HDF file name %s\n",hdf_name);

/*------------------------------------------------------------------
 * Generate binary swath file name
 *------------------------------------------------------------------*/
     /*strncpy(bin_name, gnrt_bin_fname(), BUFR_FNL); */
     gnrt_bin_fname(bin_name);
     printf("BUFR file name %s\n",bin_name);

/*-----------------------------------------------------*
 * Initialize the AT and product arrays
 *-----------------------------------------------------*/

     initall();
 
/*-----------------------------------------------------*
 * Determine surface type 
 *-----------------------------------------------------*/

     mask_stype(numscan);

/*-----------------------------------------------------*
 * Determine which AVN files to read and read the data
 *-----------------------------------------------------*/
/*jz cahnged for the new file name */ 
     pst = strstr(fname,"NSS.");
     strcpy(rfname,pst);
     printf("The real fname is %s \n ",rfname);


     hr[2] = '\0';
     hr[0] = rfname[20];
     hr[1] = rfname[21];
     hrint = atoi(hr);
     hs = hrint;

     hr[0] = rfname[26];
     hr[1] = rfname[27];
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

/*--------------------------------------------------------*
 * Generate scan times
 *--------------------------------------------------------*/
     gnrt_stime(numscan);
     printf ("Finish generating the scan times \n");

/*-----------------------------------------------------*
 * Calculate ATs from counts and generate products 
 *-----------------------------------------------------*/

     gnrt_prod(numscan);
     printf("Finish generating all products \n");
 
/*-----------------------------------------------------*
 * Define and write to the swath file
 *-----------------------------------------------------*/
     printf("rama_whdfeos/hdf_filename: %s\n", hdf_name);
     set_sw(hdf_name, numscan);

     printf("rama_whdfeos/Finish one swath HDF-EOS file !\n");
     printf("*********************************************\n");

/*--------------------------------------------------------
 * Write out data in binary format
 *--------------------------------------------------------*/
     orbit_start_time = hblock.start_milliseconds_of_day/1000;
     orbit_end_time = hblock.end_milliseconds_of_day/1000;
     printf("Check the start/end time%d %d %ld %ld\n", inx_orb, numscan, orbit_start_time, orbit_end_time);
     wrt_bin(bin_name, numscan, inx_orb);
     printf("rama_whdfeos/Finish one binary swath file ! \n");

     inx_orb += 1;
   }

   printf("\n-------------------------------------------------------------\n");
   printf("rama_whdfeos/    TOTAL OF %d ORBITS ARE PROCESSED!\n",inx_orb);
   printf("-------------------------------------------------------------\n");

}  /* end of rama_whdfeos.c */
