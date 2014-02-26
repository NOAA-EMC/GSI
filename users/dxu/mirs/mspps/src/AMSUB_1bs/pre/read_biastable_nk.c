/***********************************************************************
 *  Program Name      : read_biastable.c
 *  Type              : Subroutine
 *  Function          : Program reads the AMSU-B 1B* bias correction
 *                      tables
 *  Input Files       : bias_org.dat, bias_mys.dat
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
void read_biastable(FILE *orgfname, FILE *mysfname) 
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

      for( k=0;k<3;k++)
        corr_mys[i][j][k]=0;
    }

/*---------------------------------*
 * Read the original bias table
 *---------------------------------*/
  /* First part for STX-1 */

  istx=0;

  for( ibeam=0;ibeam<NUMSPOT_B;ibeam++)
  {
    for( i=0;i<6;i++)
      fscanf(orgfname,"%d",&temp[i]);

    for( ichan=0;ichan<NUMCHAN_B;ichan++)
      corr_org[ichan][ibeam][istx]=temp[ichan+1];
  }

  /* Skip STX-2, next part for STX-3 and SARR */

  for( istx=1;istx<4;istx++)
    for( ibeam=0;ibeam<NUMSPOT_B;ibeam++)
    {
      for( i=0;i<6;i++)
        fscanf(orgfname,"%d",&temp[i]);

      for( ichan=0;ichan<NUMCHAN_B;ichan++)
        corr_org[ichan][ibeam][istx]=temp[ichan+1];
    }

/*---------------------------------*
 * Read the additional bias table
 *---------------------------------*/
  for(istx=0;istx<3;istx++)
    for(ibeam=0;ibeam<NUMSPOT_B;ibeam++)
    {
      for( i=0;i<6;i++)
        fscanf(mysfname,"%d",&temp[i]);

      for(ichan=0;ichan<NUMCHAN_B;ichan++)
        corr_mys[ichan][ibeam][istx]=temp[ichan+1];

    }

  fclose(orgfname);
  fclose(mysfname);

} /* end of read_biastable.c */
