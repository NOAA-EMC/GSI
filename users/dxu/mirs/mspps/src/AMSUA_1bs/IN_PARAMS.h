/***********************************************************************        
 *  Header file name :  IN_PARAM.h     
 *  Programer        :  Bing-Zhang (SMSRC)
 *  Date             :  4/22/96              
 *  Function         :  
 *  Function         :  This header file contants the structures 
 *                      for input parameters.
 ***********************************************************************/ 

/* Lower and upper limits for brightness temperatures. */

typedef struct  
{
  float      Temp_lower[15], Temp_upper[15];
  float      RR_lower, RR_upper;
  float      TPW_lower, TPW_upper;
  float      CLW_lower, CLW_upper;
  float      SIce_lower, SIce_upper;
  float      OWS_lower, OWS_upper;
  float      STemp_lower, STemp_upper;
  float      SWet_lower, SWet_upper;
  float      SNowC_lower, SNowC_upper;
  float      Em23_lower, Em23_upper;
  float      Em31_lower, Em31_upper;
  float      Em50_lower, Em50_upper;
  float      Tsfc_lower, Tsfc_upper;
} Limit_A;

