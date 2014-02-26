/***********************************************************************
 *  Program Name      : read_biastable.c
 *  Type              : Subroutine
 *  Function          : Program reads the AMSU-B 1B* bias correction
 *                      table
 *  Input Files       : bias_org.dat
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : AMB2HDF.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/26/2000      v2.0
 ***********************************************************************/
#include "AMB2HDF_INCLUDE.h"
#include "ESWATH.h"

/******************************************************/
void read_biastable(FILE *orgfname) 
{ 

  int temp[6];
  short int istx,ibeam,ichan,i,j,k;

/*---------------------------------*
 * Initialize the arrays
 *---------------------------------*/
  for (i=0;i<NUMCHAN_B;i++)
    for (j=0;j<NUMSPOT_B;j++)
    {
      for(k=0;k<4;k++)
        corr_org[i][j][k]=0;

    }

/*---------------------------------*
 * Read the original bias table
 *---------------------------------*/
  /* Read for STX-1, STX-2, STX-3 and SARR */

  for(istx = 0; istx < 4; istx++)
  {
    for(ibeam = 0; ibeam < NUMSPOT_B; ibeam++)
    {
      for(i = 0; i < 6; i++)
        fscanf(orgfname,"%d",&temp[i]);

      for(ichan = 0; ichan < NUMCHAN_B; ichan++)
        corr_org[ichan][ibeam][istx] = temp[ichan+1];

    }

  }

  fclose(orgfname);

} /* end of read_biastable.c */
