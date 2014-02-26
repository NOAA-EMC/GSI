/***************************************************************************
 *  Program Name      : rmirs_dep.c
 *  Type              : Subroutine
 *  Function          : Program reads the MIRS DEP output 
 *  Input Files       : 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : rmirs_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   06/14/2007    v1.0 
 *   06/06/2008    V2.0               Jiang Zhao
 *   08/24/2009    V3.0               Wanchun Chen     modified for DEP
 *************************************************************************/

#include "defaults.h"
#include "constants.h"
#include "swath.h"


extern int intSwap(char *value) ;
extern float floatSwap(char *value);


void rmirs_dep(FILE *mirsd, short int *nscan_old, short int *nspot, char *satid)
{
   short int i,k;

   // header identifiers
   char dummy[4];
   char char_itype[4], char_alg[4], char_nprf[4], char_npos[4], char_nscan[4];
   int itype, alg, nprf, npos, nscan;;


   // content identifiers
   char char_profIndx[4];
   int profIndx;
   
   char char_iatm[4], char_tpw[4], char_clw[4], char_rwp[4], char_swp[4], char_iwp[4], char_gwp[4], char_rr[4],char_sfr[4],
        char_cldtop[4], char_cldbase[4], char_cldthick[4], char_preciptype[4], char_rainflag[4], char_lwp[4]; 
   int value_iatm; 
   float value_tpw, value_clw, value_rwp, value_swp, value_iwp, value_gwp, value_rr, value_sfr, 
         value_cldtop, value_cldbase, value_cldthick, value_preciptype, value_rainflag, value_lwp; 
  
   char char_isfc[4], char_swe[4], char_snow[4], char_sm[4], char_sice[4], char_windsp[4], char_winddir[4], char_u[4], 
        char_v[4], char_snowgs[4], char_sicefy[4], char_sicemy[4];
   int value_isfc;
   float value_swe, value_snow, value_sm, value_sice, value_windsp, value_winddir, 
         value_u, value_v, value_snowgs, value_sicefy, value_sicemy;
   
   char char_qc[4];
   int value_qc[4];

   char char_lat[4], char_lon[4], char_node[4], char_utc[4], char_year[4], char_day[4], char_ipos[4], char_iscan[4], 
        char_angle[4], char_realAziAngle[4], char_solZenAngle[4];
   //float lat, lon, utc, angle, realAziAngle, solZenAngle; 
   //int value_node,value_year, value_day, ipos, iscan; 
   int ipos, iscan; 

   char char_nIter[4], char_chisq[4];
   //int nIter, chisq;
   
   int iprf;
   int qc0,qc1,qc2;
   
   qc0 = 0;
   qc1 = 0;
   qc2 = 0;
   

   // reader header part
   fread(&dummy, 4, 1, mirsd); 
   fread(&char_itype, 4, 1, mirsd); 
   fread(&char_alg, 4, 1, mirsd);  
   fread(&dummy, 4, 1, mirsd);
   
   fread(&dummy, 4, 1, mirsd); fread(&char_nprf,     4, 1, mirsd); fread(&dummy, 4, 1, mirsd);
   fread(&dummy, 4, 1, mirsd); fread(&char_npos,     4, 1, mirsd); fread(&dummy, 4, 1, mirsd);
   fread(&dummy, 4, 1, mirsd); fread(&char_nscan,    4, 1, mirsd); fread(&dummy, 4, 1, mirsd);

   itype    = intSwap(char_itype);
   alg      = intSwap(char_alg);
   nprf     = intSwap(char_nprf);
   npos     = intSwap(char_npos);
   nscan    = intSwap(char_nscan);
   
/*
   printf("itype=%i\n", itype);
   printf("alg=%i\n", alg);
   printf("nprf=%i\n", nprf); 
   printf("npos=%i\n", npos);
   printf("nscan=%i\n", nscan);
*/

   alg_sn = alg;
   
  // read content
  for(iprf=0; iprf<nprf; iprf++) {
    
    fread(&dummy, 4, 1, mirsd); 
    fread(&char_profIndx,  4, 1, mirsd);
    fread(&dummy, 4, 1, mirsd);
    profIndx = intSwap(char_profIndx); 
    
    fread(&dummy, 4, 1, mirsd);
    fread(&char_iatm,  4, 1, mirsd);
    fread(&char_tpw,  4, 1, mirsd);
    fread(&char_clw,  4, 1, mirsd);
    fread(&char_rwp,  4, 1, mirsd);
    fread(&char_swp,  4, 1, mirsd);
    fread(&char_iwp,  4, 1, mirsd);
    fread(&char_gwp,  4, 1, mirsd);
    fread(&char_rr,  4, 1, mirsd);
    fread(&char_sfr,  4, 1, mirsd);
    fread(&char_cldtop,  4, 1, mirsd);
    fread(&char_cldbase,  4, 1, mirsd);
    fread(&char_cldthick,  4, 1, mirsd);
    fread(&char_preciptype,  4, 1, mirsd);
    fread(&char_rainflag,  4, 1, mirsd);
    fread(&char_lwp,  4, 1, mirsd);
    fread(&dummy, 4, 1, mirsd);
    
    value_iatm       = intSwap(char_iatm);
    value_tpw        = floatSwap(char_tpw);
    value_clw        = floatSwap(char_clw);
    value_rwp        = floatSwap(char_rwp);
    value_swp        = floatSwap(char_swp);
    value_iwp        = floatSwap(char_iwp);
    value_gwp        = floatSwap(char_gwp);
    value_rr         = floatSwap(char_rr);
    value_sfr        = floatSwap(char_sfr);
    value_cldtop     = floatSwap(char_cldtop);
    value_cldbase    = floatSwap(char_cldbase);
    value_cldthick   = floatSwap(char_cldthick);
    value_preciptype = floatSwap(char_preciptype);
    value_rainflag   = floatSwap(char_rainflag);
    value_lwp        = floatSwap(char_lwp);
    
    fread(&dummy, 4, 1, mirsd);
    fread(&char_isfc,  4, 1, mirsd);
    fread(&char_swe,  4, 1, mirsd);
    fread(&char_snow,  4, 1, mirsd);
    fread(&char_sm,  4, 1, mirsd);
    fread(&char_sice,  4, 1, mirsd);
    fread(&char_windsp,  4, 1, mirsd);
    fread(&char_winddir,  4, 1, mirsd);
    fread(&char_u,  4, 1, mirsd);
    fread(&char_v,  4, 1, mirsd);
    fread(&char_snowgs,  4, 1, mirsd);
    fread(&char_sicefy,  4, 1, mirsd);
    fread(&char_sicemy,  4, 1, mirsd);
    fread(&dummy, 4, 1, mirsd);
 
    fread(&dummy, 4, 1, mirsd);
    for(i=0; i<4; i++) {
      fread(&char_qc,  4, 1, mirsd);
      value_qc[i] = intSwap(char_qc);
    }    
    if( value_qc[0] == 0 ) qc0++;
    if( value_qc[0] == 1 ) qc1++;
    if( value_qc[0] == 2 ) qc2++;
    fread(&dummy, 4, 1, mirsd);
 
    value_isfc      = intSwap(char_isfc);
    value_swe       = floatSwap(char_swe);
    value_snow      = floatSwap(char_snow);
    value_sm        = floatSwap(char_sm);
    value_sice      = floatSwap(char_sice);
    value_windsp    = floatSwap(char_windsp);
    value_winddir   = floatSwap(char_winddir);
    value_u         = floatSwap(char_u);
    value_v         = floatSwap(char_v);
    value_snowgs    = floatSwap(char_snowgs);
    value_sicefy    = floatSwap(char_sicefy);
    value_sicemy    = floatSwap(char_sicemy);
    
    fread(&dummy, 4, 1, mirsd);
    fread(&char_lat, 4, 1, mirsd);
    fread(&char_lon, 4, 1, mirsd);
    fread(&char_node, 4, 1, mirsd);
    fread(&char_utc, 4, 1, mirsd);
    fread(&char_year, 4, 1, mirsd);
    fread(&char_day, 4, 1, mirsd);
    fread(&char_ipos, 4, 1, mirsd);
    fread(&char_iscan, 4, 1, mirsd);
    fread(&char_angle, 4, 1, mirsd);
    fread(&char_realAziAngle, 4, 1, mirsd);
    fread(&char_solZenAngle, 4, 1, mirsd);
    fread(&dummy, 4, 1, mirsd);
    
    // !!! note Fortran starts from 1, while C starts from 0
    ipos  = intSwap(char_ipos) - 1;
    iscan = intSwap(char_iscan) - 1 ;
    
    // !!! dep qc are the same as edr qc, so no need to update qc array
    for(k=0;k<4;k++)
      qc_dep[iscan][ipos][k] = value_qc[k];
    
    iTypAtm[iscan][ipos] = value_iatm;
    iTypSfc[iscan][ipos] = value_isfc;
    
    if( value_tpw > 0 )
      tpw[iscan][ipos] = value_tpw * TPW_SCAL;
    else
      tpw[iscan][ipos] = value_tpw;
    
    if( value_clw > 0 )
      clw[iscan][ipos] = value_clw * CLW_SCAL;
    else
      clw[iscan][ipos] = value_clw;
      
    if( value_rwp > 0 )   
      rwp[iscan][ipos] = value_rwp * RWP_SCAL;
    else
      rwp[iscan][ipos] = value_rwp;
      
    if( value_iwp > 0 )   
      iwp[iscan][ipos] = value_iwp * IWP_SCAL;
    else
      iwp[iscan][ipos] = value_iwp;
      
    if( value_lwp > 0 )   
      lwp[iscan][ipos] = value_lwp * LWP_SCAL;
    else
      lwp[iscan][ipos] = value_lwp;
      
    if( value_swp > 0 )  
      swp[iscan][ipos] = value_swp * SWP_SCAL;
    else
      swp[iscan][ipos] = value_swp;
    
    if( value_gwp > 0 )   
      gwp[iscan][ipos] = value_gwp * GWP_SCAL;
    else
      gwp[iscan][ipos] = value_gwp;

    if( value_rr > 0 )   
      rr[iscan][ipos] = value_rr * RR_SCAL;
    else
      rr[iscan][ipos] = value_rr;
      
    if( value_swe > 0 )   
      swe[iscan][ipos]  = value_swe * SWE_SCAL;
    else
      swe[iscan][ipos]  = value_swe;
      
    if( value_snow > 0 )   
      snow[iscan][ipos] = value_snow * SNOW_SCAL;
    else
      snow[iscan][ipos] = value_snow;
     
    if( value_sice > 0 )   
      sice[iscan][ipos] = value_sice * SICE_SCAL;
    else
      sice[iscan][ipos] = value_sice * SICE_SCAL;
    
    if( value_sicemy > 0 )   
      sice_my[iscan][ipos] = value_sicemy * SICE_SCAL;
    else
      sice_my[iscan][ipos] = value_sicemy;
    
    if( value_sicefy > 0 )   
      sice_fy[iscan][ipos] = value_sicefy * SICE_SCAL; 
    else
      sice_fy[iscan][ipos] = value_sicefy; 
    
    if( value_snowgs > 0 )
      snowgs[iscan][ipos]  = value_snowgs * SNOWGS_SCAL;
    else
      snowgs[iscan][ipos]  = value_snowgs;
      
    /*
    if( value_cldtop> 0 )   
      cldtop[iscan][ipos] = value_cldtop * CLDTOP_SCAL; 
    else
      cldtop[iscan][ipos] = value_cldtop; 
    
    if( value_cldbase > 0 )   
      cldbase[iscan][ipos] = value_cldbase * CLDBASE_SCAL;
    else
      cldbase[iscan][ipos] = value_cldbase;
    
    if( value_cldthick > 0 )   
      cldthick[iscan][ipos] = value_cldthick * CLDTHICK_SCAL;
    else
      cldthick[iscan][ipos] = value_cldthick;
      
    if( value_sm > 0 )   
      surfm[iscan][ipos]   = value_sm * SURFM_SCAL; 
    else
      surfm[iscan][ipos]   = value_sm; 
    
    if( value_windsp > 0 )   
      windsp[iscan][ipos]  = value_windsp * WINDSP_SCAL;
    else
      windsp[iscan][ipos]  = value_windsp;
    
    winddir[iscan][ipos] = value_winddir * WINDDIR_SCAL; 
    windu[iscan][ipos]   = value_u * WINDU_SCAL; 
    windv[iscan][ipos]   = value_v * WINDV_SCAL;  
    */
    
    
    /* Those variables are not retrieved in DEP, */
      
    sfr[iscan][ipos]        = NAVA; 
    cldtop[iscan][ipos]     = NAVA; 
    cldbase[iscan][ipos]    = NAVA;
    cldthick[iscan][ipos]   = NAVA;
    preciptype[iscan][ipos] = NAVA;
    rflag[iscan][ipos]      = NAVA;

    surfm[iscan][ipos]      = NAVA;
    windsp[iscan][ipos]     = NAVA;
    winddir[iscan][ipos]    = NAVA;
    windu[iscan][ipos]      = NAVA;
    windv[iscan][ipos]      = NAVA;
    
   
    if( itype == 1 ) {
      fread(&dummy, 4, 1, mirsd);
      fread(&char_nIter, 4, 1, mirsd);
      fread(&char_chisq, 4, 1, mirsd);
      fread(&dummy, 4, 1, mirsd);
      //printf("iprf=%i, nIter=%i, chisq=%g\n", iprf, intSwap(char_nIter), floatSwap(char_chisq) ); 
    } 
  
  }
  
  // compute qc percentage rate
  if( nprf > 0 ) {
    percentage_optimal_retrievals[0] = (float)qc0 / (float)nprf;
    percentage_suboptimal_retrievals[0] = (float)qc1 / (float)nprf;
    percentage_bad_retrievals[0] = (float)qc2 / (float)nprf;
  }
  
  return;

} /* end of rmirs_dep.c */
