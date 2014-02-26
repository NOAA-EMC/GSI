/**************************************************************************
 *  Program Name      : read_parm.c
 *  Type              : Subroutine 
 *  Function          : Program reads in AT and product limit parameters 
 *  Input Files       : input.dat 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : AMA2HDF.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/****************************************************************/
void read_parm(FILE *fparm)
{
   char          ch[80];
   float         lwat, upat;
   short int	 fnamel;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);  
   limit_A.Temp_lower[0] = lwat;
   limit_A.Temp_upper[0] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[1] = lwat;
   limit_A.Temp_upper[1] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[2] = lwat;
   limit_A.Temp_upper[2] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[3] = lwat;
   limit_A.Temp_upper[3] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[4] = lwat;
   limit_A.Temp_upper[4] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[5] = lwat;
   limit_A.Temp_upper[5] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[6] = lwat;
   limit_A.Temp_upper[6] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[7] = lwat;
   limit_A.Temp_upper[7] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[8] = lwat;
   limit_A.Temp_upper[8] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[9] = lwat;
   limit_A.Temp_upper[9] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[10] = lwat;
   limit_A.Temp_upper[10] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[11] = lwat;
   limit_A.Temp_upper[11] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[12] = lwat;
   limit_A.Temp_upper[12] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[13] = lwat;
   limit_A.Temp_upper[13] = upat;

/* lower/upper limits for temperatures */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Temp_lower[14] = lwat;
   limit_A.Temp_upper[14] = upat;

/* lower/upper limits for Rain Rate */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);  
   limit_A.RR_lower = lwat;
   limit_A.RR_upper = upat;

/* lower/upper limits for TPW */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);  
   limit_A.TPW_lower = lwat;
   limit_A.TPW_upper = upat;

/* lower/upper limits for CLW */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);  
   limit_A.CLW_lower = lwat;
   limit_A.CLW_upper = upat;

/* lower/upper limits for SIce */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);  
   limit_A.SIce_lower = lwat;
   limit_A.SIce_upper = upat;

/* lower/upper limits for OWS */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);  
   limit_A.OWS_lower = lwat;
   limit_A.OWS_upper = upat;

/* lower/upper limits for STemp */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);  
   limit_A.STemp_lower = lwat;
   limit_A.STemp_upper = upat;

/* lower/upper limits for SWet */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);  
   limit_A.SWet_lower = lwat;
   limit_A.SWet_upper = upat;

/* lower/upper limits for SNowC */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.SNowC_lower = lwat;
   limit_A.SNowC_upper = upat;

/* lower/upper limits for Emi23*/
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Em23_lower = lwat;
   limit_A.Em23_upper = upat;

/* lower/upper limits for Emi31*/
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Em31_lower = lwat;
   limit_A.Em31_upper = upat;

/* lower/upper limits for Emi50*/
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Em50_lower = lwat;
   limit_A.Em50_upper = upat;

/* lower/upper limits for Tsfc */
   fgets(ch,80,fparm);
   sscanf(ch, "%f %f", &lwat, &upat);
   limit_A.Tsfc_lower = lwat;
   limit_A.Tsfc_upper = upat;

/* file name length for amsua 1bs */
   fgets(ch,80,fparm);
   sscanf(ch, "%hd", &fnamel);  
   FN_length_AMA = fnamel;

   printf("\n---------------------------------------\n");
   printf(" Lower and upper limits for: \n");
   printf("---------------------------------------\n"); 
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[0], limit_A.Temp_upper[0]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[1], limit_A.Temp_upper[1]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[2], limit_A.Temp_upper[2]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[3], limit_A.Temp_upper[3]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[4], limit_A.Temp_upper[4]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[5], limit_A.Temp_upper[5]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[6], limit_A.Temp_upper[6]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[7], limit_A.Temp_upper[7]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[8], limit_A.Temp_upper[8]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[9], limit_A.Temp_upper[9]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[10], limit_A.Temp_upper[10]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[11], limit_A.Temp_upper[11]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[12], limit_A.Temp_upper[12]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[13], limit_A.Temp_upper[13]);
   printf("Temp  ==> %5.1f/%5.1f\n", limit_A.Temp_lower[14], limit_A.Temp_upper[14]);
   printf("RR    ==> %5.1f/%5.1f\n", limit_A.RR_lower, limit_A.RR_upper);
   printf("TPW   ==> %5.1f/%5.1f\n", limit_A.TPW_lower, limit_A.TPW_upper);
   printf("CLW   ==> %5.1f/%5.1f\n", limit_A.CLW_lower, limit_A.CLW_upper);
   printf("OWS   ==> %5.1f/%5.1f\n", limit_A.OWS_lower, limit_A.OWS_upper);
   printf("STemp ==> %5.1f/%5.1f\n", limit_A.STemp_lower, limit_A.STemp_upper);
   printf("SIce  ==> %5.1f/%5.1f\n", limit_A.SIce_lower, limit_A.SIce_upper);
   printf("SWet  ==> %5.1f/%5.1f\n", limit_A.SWet_lower, limit_A.SWet_upper);
   printf("SNowC  ==> %5.1f/%5.1f\n", limit_A.SNowC_lower, limit_A.SNowC_upper);
   printf("FLAMA ==> %d\n", FN_length_AMA);

   fclose(fparm);

}  /* end of read_parm.c */
