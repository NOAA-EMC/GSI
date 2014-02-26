/***********************************************************************
 *  Program name      :  read_iwp_s_table.c
 *  DATE              :  12/12/06
 *  Function          :  Program reads an IWP-Snowfall rate lookup table 
 *
 *  Subroutines called:  None
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
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
