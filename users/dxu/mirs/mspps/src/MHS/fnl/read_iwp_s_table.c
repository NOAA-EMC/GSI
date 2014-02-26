/***********************************************************************
 *  Program name       : read_iwp_s_table.c
 *  Function           : Program reads an IWP-Snowfall Rate lookup table
 *  Input Files        : iwp_s_table.dat
 *  Output Files       : None
 *  Subroutines called : None
 *  Called by          : BSWATH.c
 *
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

void read_iwp_s_table(FILE *fparm)
{
   short int    itb;

/*------------------------------------------------------------------
 * Open and read IWP-Snowfall rate lookup table
 *------------------------------------------------------------------*/
   for(itb = 0; itb < MAX_TB; itb++)
     fscanf(fparm, "%f %f", &iwp_tb[itb],&snowr_tb[itb]);

   fclose(fparm);

}
