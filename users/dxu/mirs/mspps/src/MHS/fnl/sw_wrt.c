/***********************************************************************
 *  Program Name      : sw_wrt.c
 *  Type              : Subroutine
 *  Function          : Program writes ancillary data and product fields 
 *			to the final MHS swath HDF-EOS file 
 *  Input Files       : None
 *  Output Files      : SWATH_MP1_MHS_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Subroutine Called : wrt_limit.c, wrt_scal.c, wrt_prod.c 
 *  Called by         : set_sw.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/10/2000      v2.0
 ***********************************************************************/ 
#include "MSWATH_INCLUDE.h"
#include "ESWATH.h"

/**********************************************************/
long int   wrt_limit(long int swid);
long int   wrt_scal(long int swid); 
long int   wrt_prod(long int swid, TOTNUM_OF_SCAN  num_of_scan);

/**********************************************************/
long int sw_wrt(char *hdf_filename, TOTNUM_OF_SCAN numscan) 
{ 
   long int   swhdf_id, sw_id;
   long int   status, swrt=0;

/*-------------------------------------*
 * Open HDF-EOS swath file
 *-------------------------------------*/
   swhdf_id = SWopen(hdf_filename, DFACC_RDWR);
   printf ("sw_wrt/ swhdf_id %ld\n", swhdf_id);
   if(swhdf_id  < 0 )
   {
     printf("sw_wrt.c/Open swath hdf file failed !\n");
     exit(6);
   }

   sw_id = SWattach(swhdf_id, SWATH_NAME_M);
   printf ("sw_wrt/ sw_id %ld\n", sw_id);
   if ( sw_id < 0 )
   {
     printf("sw_wrt/Attach swath  failed !\n");
     exit(7);
   }

/*-------------------------------------*
 * Write limits 
 *-------------------------------------*/
   status = wrt_limit(sw_id);
   printf ("sw_wrt/wrt_limit %ld\n", status);
   swrt += status;

/*-------------------------------------*
 * Write scales 
 *-------------------------------------*/
   status = wrt_scal(sw_id);
   printf ("sw_wrt/wrt_scal %ld\n", status);
   swrt += status;
/*-------------------------------------*
 * Write product fields
 *-------------------------------------*/
   status = wrt_prod(sw_id, numscan);
   printf ("sw_wrt/wrt_prod %ld\n", status);
   swrt += status;

 /*-------------------------------------------------*
 * check the hdf write
 *-------------------------------------------------*/   
   if(swrt != 0){
     printf ("sw_wrt/SW write result %ld, failed.\n",swrt);
     exit(11);
   }

/*-------------------------------------*
 * Detach and close Swath HDFEOS file
 *-------------------------------------*/
   status = SWdetach(sw_id);
   printf ("sw_wrt/detach swath file %ld\n", status);
   status = SWclose (swhdf_id);
   printf ("sw_wrt/close swath file %ld\n", status);

   return(sw_id);

} /* end of sw_wrt.c */
