/***************************************************************************
 *  Program Name      : openfs.c
 *  Type              : Subroutine
 *  Function          : Program opens file with the given file name
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : BSWATH.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/10/2000      v2.0
 *************************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/********************************************************/
FILE *openfs(char *fname)
{
   FILE     *fp;

   fp=fopen(fname, "r");

   if(fp==NULL)
   {
     printf("openfs/Cannot open file %s\n", fname); 
     exit(2);
   }
   printf("\nopenfs/Successfully open file %s\n", fname); 

   return fp;
         
} /* end of openfs.c */                             
