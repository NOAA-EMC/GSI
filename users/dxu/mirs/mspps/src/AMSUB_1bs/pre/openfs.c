/***********************************************************************
 *  Program Name      : openfs.c
 *  Type              : Subroutine
 *  Function          : Program opens file with given file name
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : AMB2HDF.c ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/********************************************************/
FILE *openfs(char *fname)
{
   FILE     *fp;

/*-------------------------*
 * Open input file 
 *-------------------------*/
   fp=fopen(fname, "r");

   if(fp==NULL)
   {
     printf("openfs/Cannot open input file %s\n", fname); 
     exit(0);
   }
   printf("\nopenfs/Successfully open input file %s\n", fname); 

   return fp;
         
} /* end of openfs.c */                             
