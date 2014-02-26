/**************************************************************************
 *  Program Name      : mirs2nc.c 
 *  Type              : Main program     
 *  Function          : Program reads MIRS  generated products and 
 *			creates netcdf  files 
 *  Input Files       : Binary sounding/dep files
 *  Output Files      : netcdf sounding/dep files 
 *  Subroutine Called : rama_wnetcdf.c
 *  Called by	      : None
 * 
 *  Modification History:
 *      Date	   Version	   Description of Changes
 *     ------     ---------       ------------------------
 *           
 *   12/05/2008     v   Based on MIRS2HDF.c        jzhao(PSGS)
 *   04/21/2008     Modified for NPP ATMS cases    wchen(DELL)
 *   05/17/2011     Let script level to create output filenames and output files
 *                  names are passed in as arguments.  wchen(DELL)
 *
 *************************************************************************/
#include "defaults.h"
 
void rmirs_wnetcdf();

/*************************************************************/
int main(int argc, char* argv[])
{
    
   char        	*inputfile_edr ;
   char		*inputfile_dep ;
   char        	*output_dir ;
   char        	*outputfile_img ;
   char		*outputfile_snd ;
   
   if(argc != 6 ){
     printf("Usage:mirs2nc inputfile_edr inputfile_dep outputdir outputfile_img outputfile_snd\n");
     exit(1);
   }

/*---------------------------------------------------*
 * get arguments from command line
 *---------------------------------------------------*/

   inputfile_edr  = malloc((strlen(argv[1])+1)*sizeof(char));
   inputfile_dep  = malloc((strlen(argv[2])+1)*sizeof(char));
   output_dir     = malloc((strlen(argv[3])+1)*sizeof(char));
   outputfile_img = malloc((strlen(argv[4])+1)*sizeof(char));
   outputfile_snd = malloc((strlen(argv[5])+1)*sizeof(char));
   
   strcpy(inputfile_edr, argv[1]);
   strcpy(inputfile_dep, argv[2]);
   strcpy(output_dir,    argv[3]);
   strcpy(outputfile_img,argv[4]);
   strcpy(outputfile_snd,argv[5]);
   
   
/*---------------------------------------------------*
 * Read MIRS EDR & DEP files and save into netcdf4 files
 *---------------------------------------------------*/

   rmirs_wnetcdf(inputfile_edr,inputfile_dep,output_dir,outputfile_img,outputfile_snd);


/*---------------------------------------------------*
 * free up
 *---------------------------------------------------*/
   
   free(inputfile_edr);
   free(inputfile_dep);
   free(output_dir);
   free(outputfile_img);
   free(outputfile_snd);
   
   return 0;
   
}  /* end of mirs2nc.c */
