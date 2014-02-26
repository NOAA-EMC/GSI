/***********************************************************************
 *  Program Name      : read_lza_parameter.c 
 *  Type              : Subroutine
 *  Function          : Program reads in the parameters used for RR correction. 
 *  Input Files       : lza_parameters.dat
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : MSWATH.c
 *
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/*******************************************************/
void read_lza_parameter(FILE *fparm)
{
   short int     i; 
   double      	 db0,db1,db2,db3,db4,db5;
  
   for(i = 0; i < 5; i++)
   {
     fscanf(fparm, "%lg %lg %lg %lg %lg %lg", &db0,&db1,&db2,&db3,&db4,&db5);
     coe_rr[i][0] = db0;
     coe_rr[i][1] = db1;
     coe_rr[i][2] = db2;
     coe_rr[i][3] = db3;
     coe_rr[i][4] = db4;
     coe_rr[i][5] = db5;

     printf("LZA parameters: %d %lg %lg %lg %lg %lg %lg\n",i,coe_rr[i][0],coe_rr[i][1],coe_rr[i][2],coe_rr[i][3],coe_rr[i][4],coe_rr[i][5]);
   }

   fclose(fparm);

} /* end of read_lza_parameter.c */
