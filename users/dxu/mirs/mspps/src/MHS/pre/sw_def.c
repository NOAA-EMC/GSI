/***********************************************************************
 *  Program Name      : sw_def.c
 *  Type              : Subroutine
 *  Function          : Program defines the geolocation and data fields 
 *			for HDF-EOS swath file 
 *  Input Files       : None 
 *  Output Files      : SWATH_MP1_MHS_Syyddd_sttttt_Eyyddd_ettttt.hdf 
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time) 
 *  Subroutine Called : def_dim.c  def_geo.c  def_data.c 
 *  Called by         : set_sw.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/02/2000      v2.0
 *   09/22/06    Added exit() for OPUS err handling  jzhao
 ***********************************************************************/
#include "MHS2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
long int   def_dim  (long int  swath_id, TOTNUM_OF_SCAN  num_scan);
long int   def_geo  (long int  swath_id);
long int   def_data(long int  swath_id);

/*****************************************************************/
long int sw_def(char *hdf_filename, TOTNUM_OF_SCAN numscan)
{ 
   long int   swhdf_id, sw_id;
   long int   status, sdef;

   printf ("\n**** open swath hdfeos file ****\n");

/*-------------------------------------------------*
 * Open HDF-EOS swath file 
 *-------------------------------------------------*/
   swhdf_id = SWopen(hdf_filename, DFACC_CREATE);
   printf ("sw_def/swath hdfeos id %ld\n", swhdf_id);
  if ( swhdf_id <0) {
     printf("sw_def/ Create hdf-eos file %s fail!\n", hdf_filename);
     exit(4);
   }

/*-------------------------------------------------*
 * Create the first swath 
 *-------------------------------------------------*/
   printf ("sw_def/swath name %s\n", SWATH_NAME_M);
   sw_id = SWcreate(swhdf_id, SWATH_NAME_M);
   printf ("sw_def/swath_id %ld\n", sw_id);
  if ( sw_id <0) {
     printf("sw_def/Create hdf-eos swath %s fail!\n",SWATH_NAME_M );
     exit(5);
   }

  sdef=0;
/*-------------------------------------------------*
 * Define swath dimensions 
 *-------------------------------------------------*/
   status = def_dim(sw_id, numscan);
   printf ("sw_def/result return from def_dim  %ld\n", status);

   sdef += status;
/*-------------------------------------------------*
 * Define geolocation fields
 *-------------------------------------------------*/
   status = def_geo (sw_id);
   printf ("sw_def/result return from def_geo  %ld\n", status);
   sdef += status;

/*-------------------------------------------------*
 * Define data fields
 *-------------------------------------------------*/
   status = def_data(sw_id);
   printf ("sw_def/result return from def_data  %ld\n", status);
   sdef += status;
   
/*-------------------------------------------------*
 * check the hdf define
 *-------------------------------------------------*/   
   if(sdef !=0){
     printf ("sw_def/Definision result %ld, failed.\n",sdef);
     exit(10);
   }

/*-------------------------------------------------*
 * Detach and close swath file
 *-------------------------------------------------*/
   SWdetach(sw_id);
   SWclose(swhdf_id);

   return(sw_id);
 
} /* end of sw_def.c */
