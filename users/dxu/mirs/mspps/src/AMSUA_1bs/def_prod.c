/**************************************************************************
 *  Program Name      : def_prod.c
 *  Type              : Subroutine
 *  Function          : Program defines the product fields for HDF-EOS 
 *                      swath file
 *  Input Files       : None
 *  Output Files      : NPR.AAOP.NK.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Subroutine Called : None
 *  Called by         : sw_def.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
long int def_prod(long int sw_id)
{ 
   
   long int   status, result = 0;
        int   compparm = 5;

/*---------------------------------------------------------*
 * Set field compression for all subsequent field definitions
 *---------------------------------------------------------*/
   status = SWdefcomp(sw_id, HDFE_COMP_DEFLATE, &compparm);
   printf ("def_prod/swdefcomp %ld\n", status);
   result = result + status;

/*---------------------------------------------------------*
 * Define product fields
 *---------------------------------------------------------*/
   status = SWdefdatafield(sw_id, "TPW", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define prodfield TPW %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "CLW", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define prodfield CLW %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "SIce", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define prodfield SIce %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "T_sfc", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define prodfield TS %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "Emis_23", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define prodfield EM23 %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "Emis_31", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define prodfield EM31 %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "Emis_50", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define prodfield EM50 %ld\n", status);
   result = result + status;

   status = SWdefcomp(sw_id, HDFE_COMP_NONE, NULL);
   printf ("def_data/swdefcomp %ld\n", status);
   result = result + status;

   return(result);
 
}/*  end of def_prod.c  */
