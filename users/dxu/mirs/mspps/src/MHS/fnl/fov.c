/***********************************************************************
 *  Program Name      : fov.c
 *  Type              : Subroutine
 *  Function          : Program calculates the size of each FOV in the
 *  			longitude direction 
 *  Input Files       : None
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/10/2000      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/*************************************************************/

void fov(void)
{
  short int	i;
  float 	ang[NUMSPOT_M+1], angle, angle1;

  for (i = 0; i < NUMSPOT_M+1; i++)
  {
     angle = PI * SCAN_ANG_M * (i - NUMSPOT_M/2) / 180;
     angle1 = (REARTH + RSAT) * sin(angle) / REARTH;
     angle1 = atan(angle1 / sqrt(1 - angle1 * angle1));
     ang[i] = (angle1 * 180 / PI) - SCAN_ANG_M * (i - NUMSPOT_M/2);
     if (i > 0) fovsize[i - 1] = fabs(ang[i] - ang[i-1]);
  }

} /* end of fov.c */
