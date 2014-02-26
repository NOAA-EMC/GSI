/***********************************************************************
 *  Program Name      : def_prod.c
 *  Type              : Subroutine
 *  Function          : Program defines the MHS product fields for
 *                      HDF-EOS swath file
 *  Input Files       : None
 *  Output Files      : SWATH_MP1_MHS_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Subroutine Called : None
 *  Called by         : sw_def.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 ***********************************************************************/
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/*********************************************************/
long int def_prod(long int sw_id)
{ 
   
   long int   status, result = 0;
        int   compparm = 5;

/*---------------------------------*
 * Set field compression for all 
 * subsequent field definitions
 *---------------------------------*/
   status = SWdefcomp(sw_id, HDFE_COMP_DEFLATE, &compparm);
   printf ("def_prod/swdefcomp %ld\n", status);
   result = result + status;

/*---------------------------------*
 * Define product fields 
 *---------------------------------*/
   status = SWdefdatafield(sw_id, "RR", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define product field RR %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "Snow", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define product field Snow %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "SFR", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define product field SFR %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "IWP", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define product field IWP %ld\n", status);
   result = result + status;

   status = SWdefdatafield(sw_id, "SWE", "Scanline,Field_of_view",
            DFNT_INT16, HDFE_NOMERGE);
   printf ("def_prod/define product field SWE %ld\n", status);
   result = result + status;

   status = SWdefcomp(sw_id, HDFE_COMP_NONE, NULL);
   printf ("def_data/swdefcomp %ld\n", status);
   result = result + status;

   return(result);
 
} /* end of def_prod.c */
