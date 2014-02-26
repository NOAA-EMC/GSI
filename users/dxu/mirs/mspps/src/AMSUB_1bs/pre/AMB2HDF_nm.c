/**************************************************************************
 *  Program Name      : AMB2HDF.c
 *  Type              : Main program
 *  Function          : Program reads AMSU-B 1B* data, computes ATs 
 *                      and creates preliminary AMSU-B SWATH file 
 *			(HDF-EOS format)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : openfs.c, read_systim.c, read_parm.c,
 *                      read_biastable.c, read_latbox_table.c, 
 *			read_mask.c, ramb_whdfeos.c
 *  Called by         : None
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   07/10/2002     v2.0
 *   01/12/2006    v2.1  Modified for OPUS   jzhao(QSS)
 *************************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "SWATH.h"
 
/*************************************************************/
FILE *openfs();
void read_systim();
void read_parm();
void read_biastable();
void read_latbox_table();
void read_mask();
void ramb_whdfeos();

/*************************************************************/
int main()
{
   FILE        *fp_PARAM, *fp_FNAME;
   char        *string1[] = { "BEGINE_AMSUB", "END_AMSUB"};
   char        *string2[] = { "ORBITAL", "DAILY",
                             "PENTAD",  "MONTHLY" };
   char         input_fname[] = "../data/NOAA-M/input/input_B.dat";
   char         org_fname[] = "../data/NOAA-M/input/bias_org_B.dat";
   char		latbox_fname[] = "../data/control/latbox_table_B.dat";
   char 	mask_fname[]="../data/control/mask.bin";
   
/*---------------------------------------------------*
 * Read in system time
 *---------------------------------------------------*/
   read_systim(string1[0],string2[0]);
   printf("\n AMB2HDF/ Successfully read system time !\n"); 
   printf("***************************************************\n");

/*---------------------------------------------------*
 * Open and read the parameter file 
 *---------------------------------------------------*/
   fp_PARAM = openfs(input_fname);

   read_parm(fp_PARAM);

/*----------------------------------------------------*
 * Open and read the RFI correction tables
 *----------------------------------------------------*/
   fp_PARAM = openfs(org_fname);

   read_biastable(fp_PARAM);

/*---------------------------------------------------*
 * Open and read the latbox table
 *---------------------------------------------------*/
   fp_PARAM = openfs(latbox_fname);

   read_latbox_table(fp_PARAM);

/*---------------------------------------------------*
 * Open and read the mask data 
 *---------------------------------------------------*/
   fp_PARAM = openfs(mask_fname);

   read_mask(fp_PARAM);

/*---------------------------------------------------*
 * Open AMB1B* file name file
 *---------------------------------------------------*/
   if((fp_FNAME=fopen("../data/NOAA-M/misc/amsubfile_nm","r")) == NULL)
   {
     printf("AMB2HDF/Can't open AMB1BS filename file: amsubfile_nm !\n");
     exit(1);
   }
   printf(" Successfully open AMB1BS filename file: amsubfile_nm !\n");

/*---------------------------------------------------*
 * Read AMB1B* files and compute ATs to be stored in 
 * the preliminary swath file (in HDF-EOS format) 
 *---------------------------------------------------*/

   ramb_whdfeos(fp_FNAME);

/*----------------------------------------------------*
 * Read in system time
 *----------------------------------------------------*/

   read_systim(string1[1],string2[0]);
   return 0;

}  /* end of AMB2HDF.c */
