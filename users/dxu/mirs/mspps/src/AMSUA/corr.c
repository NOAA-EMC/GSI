/***************************************************************************
 *  Program Name      : corr.c
 *  Type              : Subroutine
 *  Function          : Program determines the asymmetry correction 
 *			coefficient for a given channel at a given pixel 
 *  Input Files       : None 
 *  Output Files      : None
 *  Subroutine Called : None 
 *  Called by         : seaice.c, tpwclw.c, rainrate.c 
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   3/15/2001      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/********************************************************************/
float corr(short int iscan, short int ifov, short int ich)
{
    float  coe, theta;

    theta = lza[iscan][ifov];  
 
    if(orb_mode[iscan] == 1) 
      coe = A0[ich]*exp(-pow((theta-A1[ich])/A2[ich],2.)/2.) + A3[ich] + A4[ich]*theta + A5[ich] * pow(theta,2.);
    else if(orb_mode[iscan] == 2) 
      coe = D0[ich]*exp(-pow((theta-D1[ich])/D2[ich],2.)/2.) + D3[ich] + D4[ich]*theta + D5[ich] * pow(theta,2.);

    return(coe);

} /* end of corr.c */
