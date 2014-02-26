/***************************************************************************
 *  Program Name      : init_sw.c
 *  Type              : Subroutine
 *  Function          : Program initializes AT and product fields 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : rmirs_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   11/30/2006      v2.0
 *************************************************************************/
#include "defaults.h"
#include "constants.h"
#include "swath.h"

void init_sw()
{
    short int ifov, iscan, ich, ilay, k, i;
    
    for(i=0;i<20;i++)
        time_coverage_start[i]='X';
    time_coverage_start[20]='\0';
    
    for(i=0;i<20;i++)
        time_coverage_end[i]='X';
    time_coverage_end[20]='\0';
    
    
    for(iscan = 0; iscan < MAXSCAN; iscan++){
       for(ifov = 0; ifov < MAXFOV; ifov++)
       {

          iTypAtm[iscan][ifov] = MISSING;
          iTypSfc[iscan][ifov] = MISSING;
	  nattempt[iscan][ifov] = MISSING;
	  niter[iscan][ifov] = MISSING;
	  chisq[iscan][ifov] = MISSING; 
	  lat[iscan][ifov] = MISSING; 
	  lon[iscan][ifov] = MISSING;
	  angle[iscan][ifov] = MISSING;
	  rel_azi[iscan][ifov] = MISSING;
	  sza[iscan][ifov] = MISSING;
	  for(k=0;k<4;k++){
	    qc[iscan][ifov][k] = MISSING;
	    qc_dep[iscan][ifov][k] = MISSING;
	  }



	  for(ilay=0;ilay< MAXLAY; ilay++){
	  
	    ptemp[iscan][ifov][ilay] = MISSING;
	    pvapor[iscan][ifov][ilay] = MISSING;
	    pozone[iscan][ifov][ilay] = MISSING;
	    pclw[iscan][ifov][ilay] = MISSING;
	    prain[iscan][ifov][ilay] = MISSING;
	    pgraupel[iscan][ifov][ilay] = MISSING;
	    psnow[iscan][ifov][ilay] = MISSING;
	    pice[iscan][ifov][ilay] = MISSING;	    
       
	  }
	    /*** DEP outputs ****************************/
	     clw[iscan][ifov] = MISSING;
             tpw[iscan][ifov] = MISSING;
	     lwp[iscan][ifov] = MISSING;
             rwp[iscan][ifov] = MISSING;
             swp[iscan][ifov] = MISSING;
             iwp[iscan][ifov] = MISSING;
             gwp[iscan][ifov] = MISSING;
             sfr[iscan][ifov] = MISSING;
             cldtop[iscan][ifov] = MISSING;
             cldbase[iscan][ifov] = MISSING;
             cldthick[iscan][ifov] = MISSING;
             preciptype[iscan][ifov] = MISSING;
             rr[iscan][ifov] = MISSING;
             rflag[iscan][ifov] = MISSING;

             swe[iscan][ifov] = MISSING;
	     snowgs[iscan][ifov] = MISSING;
             snow[iscan][ifov] = MISSING;
             surfm[iscan][ifov] = MISSING;
             sice[iscan][ifov] = MISSING;
	     sice_my[iscan][ifov] = MISSING;
	     sice_fy[iscan][ifov] = MISSING;
             windsp[iscan][ifov] = MISSING;
             winddir[iscan][ifov] = MISSING;
             windu[iscan][ifov] = MISSING;
             windv[iscan][ifov] = MISSING;
	     tskin[iscan][ifov] = MISSING;
	     surfp[iscan][ifov] = MISSING;
	     
	     //Multi-channel parameters	    
	     for(ich = 0; ich < MAXCH; ich++){
	       bt[iscan][ifov][ich] = MISSING;
	       ym[iscan][ifov][ich] = MISSING; 
	       yfwd[iscan][ifov][ich] = MISSING; 
	       chanSel[iscan][ifov][ich] = MISSING; 
	       emis[iscan][ifov][ich] = MISSING;
	     }

	  }//ifov

       orb_mode[iscan] = MISSING;
       year[iscan]     = MISSING;
       month[iscan]    = MISSING;
       dom[iscan]      = MISSING;
       hour[iscan]     = MISSING;
       minute[iscan]   = MISSING;
       second[iscan]   = MISSING;
       doy[iscan]      = MISSING;
       time_utc[iscan] = MISSING;

       }//iscan

} /* end of init.c */
