/***********************************************************************
 *  Program Name      : rainrate_correct.c
 *  Type              : Subroutine
 *  Function          : Program corrects rain rate based on D. Vila's 
 *                      algorithm (100 * mm/hr) (global)
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : calprod.c
 *  05/09/2007        : algorithm enhancemant (ccr3135)
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

#define		SSMIArea	182
#define         RR_MAX          400

/****************************************************/
void rainrate_correct(short int iscan)
{
   float	b0[4]={1.50155, 1.93515, 4.17168, 7.81001};
   float	b1[4]={0.0591993, 0.566360, 2.03765, 4.91456};

   short int    lstag, ind, ci, ifov;
   double       ltd, lza1, mu, sigma, suma, rr_temp; 
   double       exponente, factor, slope, ordinate, AMSUArea;
   double       delta1, delta2, delta3, aclw, alfa; 
   float        b182, b180, b176; 
   float        fix = 0.5;

   if(num_avn < 0)
      return;

   for (ifov = 0; ifov < NUMSPOT_B; ifov++)
   {

// Snowfall
       if(rr[iscan][ifov] == 1)
         continue;

       lstag = stype[iscan][ifov];
      
// LZA normalization process (see AGU, 2006)

       if(rr[iscan][ifov] > 0)
       {
         if(lstag == 0) ind = 0;
         if(lstag >= 1) ind = 2;

         ltd = fabs(lat[iscan][ifov]); 
         lza1 = fabs(lza[iscan][ifov]); 
         rr_temp = rr[iscan][ifov] / RR_SCAL;

         mu = coe_rr[ind][0]+coe_rr[ind][1]*lza1+coe_rr[ind][2]*lza1*lza1+
              coe_rr[ind][3]*ltd+coe_rr[ind][4]*ltd*lza1+coe_rr[ind][5]*ltd*ltd;
 
         sigma = coe_rr[ind+1][0]+coe_rr[ind+1][1]*lza1+coe_rr[ind+1][2]*lza1*lza1+
                 coe_rr[ind+1][3]*ltd+coe_rr[ind+1][4]*ltd*lza1+coe_rr[ind+1][5]*ltd*ltd;
       
         suma = mu+2*sigma;

         alfa = 0.3+lza1*0.0051;

         AMSUArea = coe_rr[4][4]*pow(lza1,4)+coe_rr[4][3]*pow(lza1,3)
                    +coe_rr[4][2]*pow(lza1,2)+coe_rr[4][1]*lza1+coe_rr[4][0];

         if(lstag == 1 || lstag == 2) 
           slope = 1.3*sqrt(AMSUArea/SSMIArea);
         if(lstag == 0) 
           slope = 1.5*sqrt(AMSUArea/SSMIArea);

         exponente = -0.5*pow(((suma-mu)/sigma),2);
         factor = 1 - (1-alfa)*exp(exponente);
         ordinate = factor*suma - slope*suma;

         if(rr_temp < suma)
         {
           exponente = -0.5 * pow(((rr_temp - mu)/sigma),2);

           if(rr_temp < mu) 
             exponente = -0.5 * pow(((rr_temp - mu)/(sigma*2)),2);

           factor = 1 - (1 - alfa)*exp(exponente);

           rr[iscan][ifov] = factor * rr_temp * RR_SCAL + fix;
         }
         else
           rr[iscan][ifov] = (slope * rr_temp + ordinate) * RR_SCAL + fix; 

       } // end of if(rr > 0)

// Perform convective index (ci) and fill up the gaps (indeterm = -10)

       else if (rr[iscan][ifov] == -10 || rr[iscan][ifov] == 0) 
       {
// Evaluate CI
         ci = 0;
 
         b182 = at[iscan][ifov][2];
         b180 = at[iscan][ifov][3];
         b176 = at[iscan][ifov][4];

         delta1 = at[iscan][ifov][2] - at[iscan][ifov][4];
         delta2 = at[iscan][ifov][3] - at[iscan][ifov][4];
         delta3 = at[iscan][ifov][2] - at[iscan][ifov][3];

         if(delta2 > 0 && delta2 > delta1 && delta2 > delta3)
           ci = 1;
         if(delta1 > 0 && delta2 > 0 && delta3 > 0 
           && delta1 > delta2 && delta1 > delta3 && delta2 > delta3) 
           ci = 2;
         if(delta1 > 0 && delta2 > 0 && delta3 > 0 
           && delta1 > delta2 && delta1 > delta3 && delta2 <= delta3)
           ci = 3;
       
         if(lstag >= 1 && rr[iscan][ifov] == -10)
         {
           if(ci == 0) 
             rr[iscan][ifov] = 0;
           if(ci == 1) 
             rr[iscan][ifov] = 1.97 * RR_SCAL + fix;
           if(ci == 2) 
             rr[iscan][ifov] = 5.95 * RR_SCAL + fix;
           if(ci == 3) 
             rr[iscan][ifov] = 10.95 * RR_SCAL + fix;
         }
         else if(lstag == 0)
         {
           aclw = clw_AB[iscan][ifov]/CLW_SCAL;
  
           if(aclw >= 0.4)
           {
             if(ci == 0) 
               rr[iscan][ifov] = (b0[0]+b1[0]*aclw)*RR_SCAL + fix;
             if(ci == 1) 
               rr[iscan][ifov] = (b0[1]+b1[1]*aclw)*RR_SCAL + fix;
             if(ci == 2) 
               rr[iscan][ifov] = (b0[2]+b1[2]*aclw)*RR_SCAL + fix;
             if(ci == 3) 
               rr[iscan][ifov] = (b0[3]+b1[3]*aclw)*RR_SCAL + fix;
           }
           else if(aclw < 0.4 && aclw >= 0)
           {
             if(ci == 0 || ci == 1 || ci == 2 || ci == 3)
               rr[iscan][ifov] = 0;
           }

         } // end of if(lstag = 0)
 
       }  // end of if rr=-10 or rr=0 check 
       
// Modify rr value if it's 1 (0.1 without scaling) to avoid confusion with snowfall.

       if(rr[iscan][ifov] == 1)
         rr[iscan][ifov] = 2;

// Set upper RR limit (40 mm/hr).
       else if(rr[iscan][ifov] > RR_MAX)
         rr[iscan][ifov] = RR_MAX;

   } // end of for(ifov) loop 

} /* end of rainrate_correct.c */ 
