/***********************************************************************
 *  Program Name      : rainrate.c
 *  Type              : Subroutine
 *  Function          : Program calculates rain rate (100 * mm/hr) (global)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called :
 *  Called by         : calprod.c
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/****************************************************/
#define   A0    0.321717 
#define   A1    16.5043 
#define   A2    -3.3419

#define	  B0	0.08925
#define   B1	20.8194
#define	  B2	-2.9117
/****************************************************/
void rainrate(short int iscan)
{
   char		lstag, lstag_A;
   short int    ifov, flag, rr_flag;
   short int    conv_index;
   float        b89, b150, b176; 
   float        aclw, biwp, bde, brr; 
   float        alat;
   float        coef0, coef1, coef2;

   coef0 = A0;
   coef1 = A1;
   coef2 = A2;

   if(num_avn < 0)
      return;

   for (ifov = 0; ifov < NUMSPOT_B; ifov++)
   {
       alat = lat[iscan][ifov];
       conv_index=ctype[iscan][ifov];

       flag = iwp[iscan][ifov];

       if (flag <= 0)
           brr = flag;  
       else
       {
           brr = 0.;
           rr[iscan][ifov] = 0;

           if(conv_index >= 3)
           {
              coef0 = 0.08925;
              coef1 = 20.8194;
              coef2 = -2.9117;
           }

           b89 = at[iscan][ifov][0]; 
           b150 = at[iscan][ifov][1];
           b176 = at[iscan][ifov][4];

           biwp = iwp[iscan][ifov]/IWP_SCAL;
           bde = de[iscan][ifov]/IWP_SCAL;
           rr_flag = iwp_flag[iscan][ifov]; 


           if(clw_AB[iscan][ifov] > 0)
              aclw = clw_AB[iscan][ifov]/CLW_SCAL;
           else
	      aclw = clw_AB[iscan][ifov];

           lstag = stype[iscan][ifov];
           lstag_A = stype_AB[iscan][ifov];  


/*------------------------------------*
 * Ocean FOV 
 *------------------------------------*/
           if(lstag == 0) /* ocean */
           {
             if(aclw > 0.2 && biwp >= 0.05 && bde >= 0.3)
               brr = coef0 + coef1 * biwp + coef2 * pow(biwp, 2); 
           }
  
/*------------------------------------*
 * Land or coastal FOV 
 *------------------------------------*/
           else if(lstag == 1 || lstag == 2) /* land and coast */
           {
             if(biwp >= 0.05 && bde >= 0.3 && (b89-b150) >3)
               brr = coef0 + coef1 * biwp + coef2 * pow(biwp, 2);
           }

/* New coastal algorithm starts here: coast FOV (AMSU-A and B  and when RR from 
   current algorithm is zero */

           if( brr <= 0.0 && (lstag_A == 2 ||  lstag == 2) ) 
           {
             if (rr_flag == 1 && biwp > 0.05) 
               brr = coef0 + coef1 * biwp + coef2 * pow(biwp, 2);
           } 
       
           
           if(lstag == 2 && fabs(alat) > 50)  
              /* remove the noises near coast at high latitue */ 
                brr = INDETERM_COAST;

        }  /* end of if clause of IWP flag checking */ 

        if(brr > limit_Prod.RR_upper)
           brr = limit_Prod.RR_upper;

        if(brr > 0.) 
           rr[iscan][ifov] = brr * RR_SCAL + 0.5;
        else
           rr[iscan][ifov] = brr;
  }

} /* end of rainrate.c */ 
