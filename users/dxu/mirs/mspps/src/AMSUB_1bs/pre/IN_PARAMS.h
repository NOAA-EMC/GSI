/***********************************************************************        
 *  Header file name :  IN_PARAM.h     
 *  Programer        :  Bing-Zhang (SMSRC)
 *  Date             :  4/22/96              
 *  Function         :  
 *  Function         :  This header file contants the structures 
 *                      for input parameters.
 ***********************************************************************/ 

/* Lower and upper limits for antenna temperatures. */

typedef struct
{
  float      Temp_lower[5], Temp_upper[5];
} Limit_B;

/* Lower and upper limits for products */ 

typedef struct  
{
  float      RR_lower, RR_upper;
  float      SNowC_lower, SNowC_upper;
  float      IWP_lower, IWP_upper;
  float      De_lower, De_upper;
  float      SWE_lower, SWE_upper;
  float      SFR_lower, SFR_upper;
  float      TPW_lower, TPW_upper;
  float      CLW_lower, CLW_upper;
  float      SIce_lower, SIce_upper;
  float      OWS_lower, OWS_upper;
  float      STemp_lower, STemp_upper;
  float      SWet_lower, SWet_upper;
} Limit_Prod;
