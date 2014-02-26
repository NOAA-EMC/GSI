/***********************************************************************
 *  Program Name      : read_parm.c
 *  Type              : Subroutine
 *  Function          : Program reads in AT parameters 
 *  Input Files       : input.dat 
 *  Output Files      : None
 *  Subroutine Called : None
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

void read_parm(FILE *fparm)
{
   char          ch[80];
   float         lwat, upat;
   FILE_LENGTH   fnamel;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);  
   limit_M.Temp_lower[0] = lwat;
   limit_M.Temp_upper[0] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_M.Temp_lower[1] = lwat;
   limit_M.Temp_upper[1] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_M.Temp_lower[2] = lwat;
   limit_M.Temp_upper[2] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_M.Temp_lower[3] = lwat;
   limit_M.Temp_upper[3] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_M.Temp_lower[4] = lwat;
   limit_M.Temp_upper[4] = upat;

/* file name length for MHS 1b */
   fgets(ch,80,fparm);
   sscanf(ch, "%hd", &fnamel);  
   FN_length_MHS = fnamel;

   printf("\n---------------------------------------\n");
   printf(" Lower and upper limits for: \n");
   printf("---------------------------------------\n"); 
   printf("Temp  ==> %5.1f/%5.1f\n", limit_M.Temp_lower[0], limit_M.Temp_upper[0]);
   printf("FLMHS ==> %d\n", FN_length_MHS);

   fclose(fparm);

}  /* end of read_parm.c */
