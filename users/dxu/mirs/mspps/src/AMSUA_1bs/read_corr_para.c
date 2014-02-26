/***************************************************************************
 *  Program Name      : read_corr_para.c
 *  Type              : Subroutine
 *  Function          : Program reads in the parameters used to determine
 *			antenna temperature asymmetry correction coefficients
 *  Input Files       : corr_para.dat 
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

/*****************************************************************/
void read_corr_para(FILE *fparm)
{
 
   short int    ich;
   float	dum0,dum1,dum2,dum3,dum4,dum5;
  
   for(ich = 0; ich < 4; ich++)
   {
     fscanf(fparm,"%f %f %f %f %f %f",&dum0,&dum1,&dum2,&dum3,&dum4,&dum5);
     A0[ich] = dum0;
     A1[ich] = dum1;
     A2[ich] = dum2;
     A3[ich] = dum3;
     A4[ich] = dum4;
     A5[ich] = dum5;
   }
   printf("Asymmetry Correction Coe.");
   for(ich = 0; ich < 4; ich++)
     printf("%f %f %f %f %f %f\n",A0[ich],A1[ich],A2[ich],A3[ich],A4[ich],A5[ich]);

   for(ich = 0; ich < 4; ich++)
   {
     fscanf(fparm,"%f %f %f %f %f %f",&dum0,&dum1,&dum2,&dum3,&dum4,&dum5);
     D0[ich] = dum0;
     D1[ich] = dum1;
     D2[ich] = dum2;
     D3[ich] = dum3;
     D4[ich] = dum4;
     D5[ich] = dum5;
   }
   for(ich = 0; ich < 4; ich++)
     printf("%f %f %f %f %f %f\n",D0[ich],D1[ich],D2[ich],D3[ich],D4[ich],D5[ich]);

   fclose(fparm);

}  /* end of read_corr_para.c */
