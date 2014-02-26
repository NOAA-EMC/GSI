/***********************************************************************
 *  Program Name      : read_swath_B.c
 *  Type              : Subroutine
 *  Function          : Program reads preliminary AMSU-B orbital fields
 *  Input Files       : swath_fname_B.list,
 *			NPR.ABOP.NK.Dyyddd.Shhmm.Ehhmm.Bnnnnnnn.NS
 *                      (yy: year, ddd: julian day, Shhmm: starting hour
 *                       and minute, Ehhmm: ending hour and minute,
 *                       nnnnnnn: orbit ID)
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   10/31/2000      v2.0
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/************************************************************/
long int read_swath_B(char *filename, char *swathname, 
                    TOTNUM_OF_SCAN  *nscan)
{
   char       atname[10];

   short int  a_temp[MAXSCANLINE_B][NUMSPOT_B];
   long	      sfid, sid, status, i, j, k, result = 0;
   long       dimsize[40];

/*-------------------------------*
 * Read number of scan lines
 * and relevant data fields
 *-------------------------------*/
   printf("\n\n****************************************\n");
   printf("   Information from AMSU-B swath fields\n");
   printf("****************************************\n\n");

   printf("swath filename %s\n", filename);
   if((sfid = SWopen(filename, DFACC_RDONLY)) == -1)
   {
     printf("read_swath_B/Open swath hdf file failed !\n");
     exit(0);
   }
   if( (sid = SWattach(sfid, swathname)) == -1)
   {
     printf("read_swath_B/Open swath file failed !\n");
     exit(1);
   }
   printf("read_swath_B/HDF file id  %ld,  SWATH id %ld\n", sfid, sid);


   dimsize[0] = SWdiminfo(sid, "Scanline");
   printf("read_swath_B/Scanline %6ld  \n", dimsize[0]);
   *nscan = dimsize[0];

   status = SWreadfield(sid, "ScanTime_hour", NULL, NULL, NULL, hour);
   result += status;
   status = SWreadfield(sid, "ScanTime_minute", NULL, NULL, NULL, minute);
   result += status;
   status = SWreadfield(sid, "ScanTime_second", NULL, NULL, NULL, second);
   result += status;

   status = SWreadfield(sid, "Latitude", NULL, NULL, NULL, lat);
   result += status;
   status = SWreadfield(sid, "Longitude", NULL, NULL, NULL, lon);
   result += status;

   status = SWreadfield(sid, "LZ_angle", NULL, NULL, NULL, lza);
   result += status;

   status = SWreadfield(sid, "Sfc_type", NULL, NULL, NULL, stype);
   result += status;

   printf("read_swath_B/scan line and geo fields %ld \n", result); 

/*-------------------------------*
 * Read AT fields
 *-------------------------------*/
   for(k = 0; k < NUMCHAN_B; k++)
   {
      sprintf(atname,"Chan%ld_AT",k+1);

      status = SWreadfield(sid, atname, NULL, NULL, NULL, a_temp);
      printf("\n read_swath_B/read Chan%ld_AT  %ld\n",k+1,status);
      result = result + status;

      for(i = 0; i < *nscan; i++)
        for(j = 0; j < NUMSPOT_B; j++)
        {
          if(a_temp[i][j] > 0)
             at[i][j][k] = a_temp[i][j]/AT_SCAL;
	  else
             at[i][j][k] = a_temp[i][j];
        }

    }
   printf("read_swath_B/AT fields %ld \n", result); 

   status = SWdetach(sid);
   result += status;
   status = SWclose (sfid);
   result += status;
   printf("read_swath_B = %ld\n", result);

   return(result);

} /* end of read_swath_B.c */
