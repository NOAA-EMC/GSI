/**************************************************************************
 *  Program Name      : bias_corr.c
 *  Type              : Subroutine
 *  Function          : Program reads AMSU-B 1b* bias correction tables 
 *			and corrects the original bias as well as the 
 *			additionl bias
 *  Input Files       : input.dat
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : getcal.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 *************************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/**************************************************************/
void bias_corr(short int iscan) 
{
  short int scan_mod;
  short int nscan, lineno;
  short int ichan ,ibeam;
  float ra1,ra2,ra3,ra4,ra5,ra45;

/*--------------------------------------*
 * Correct the original bias 
 *--------------------------------------*/

  /* Get the ratio of relative power */
  ra1=scanline5[iscan].stx_1_power/hblock4.tx_power[0];
  ra2=scanline5[iscan].stx_2_power/hblock4.tx_power[1];
  ra3=scanline5[iscan].stx_3_power/hblock4.tx_power[2];

  /* Temporarily not using power_flags */

  ra4=scanline5[iscan].sarr_a_power/hblock4.tx_power[3];
  ra5=scanline5[iscan].sarr_b_power/hblock4.tx_power[3];
  ra45=0.0;
  if(ra4 > 0.0) 
    ra45 = ra4;
  if(ra5 > ra45) 
    ra45 = ra5;

  for (ichan=0;ichan<NUMCHAN_B;ichan++)
    for (ibeam=0;ibeam<NUMSPOT_B;ibeam++)
      count[ichan][ibeam]=count[ichan][ibeam]+ra1*corr_org[ichan][ibeam][0]
                    +ra2*corr_org[ichan][ibeam][1]
                    +ra3*corr_org[ichan][ibeam][2]
                    +(ra45)*corr_org[ichan][ibeam][3];

/*--------------------------------------*
 * Correct the additional bias 
 *--------------------------------------*/
  if(scanline1[iscan].tx_newbias_flag==1) /* 0 or 1 */
  {
    nscan=scanline1[iscan].scan_line_number;

  /* Note iscan & nscan start at 0 */
    scan_mod = nscan/3;
    lineno = nscan - scan_mod*3;

    for ( ichan=0;ichan<NUMCHAN_B;ichan++)
      for ( ibeam=0;ibeam<NUMSPOT_B;ibeam++)
        count[ichan][ibeam]=count[ichan][ibeam]+corr_mys[ichan][ibeam][lineno];
  }
  else if (scanline1[iscan].tx_newbias_flag==0)
  { }
  
  else 
   printf("New bias flag is wrong/unknown for scanline: %d\n", nscan);

} /* end of bias_corr.c */
