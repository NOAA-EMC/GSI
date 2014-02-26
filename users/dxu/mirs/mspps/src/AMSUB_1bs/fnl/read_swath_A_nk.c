/***********************************************************************
 *  Program Name      : read_swath_A.c
 *  Type              : Subroutine
 *  Function          : Program reads AMSU-A orbital fields
 *  Input Files       : swath_fname_A.list,
 *                      SWATH_N15_AMSUA_Syyddd_sttttt_Eyyddd_ettttt.hdf
 *                      (yy: year, ddd: julian day, sttttt: starting time,
 *                       ettttt: ending time)
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : ramb_whdfeos.c
 *
 ***********************************************************************/
#include "BSWATH_INCLUDE.h"
#include "ESWATH.h"

/******************************************************/
long int read_swath_A(char *filename, char *swathname, 
                    TOTNUM_OF_SCAN  *nscan)
{
   char       atname[10];

   short int  a_temp[MAXSCANLINE_A][NUMSPOT_A];
   long       sfid, sid, status, i, j, k, result = 0;
   long       dimsize[40];

/*--------------------------------*
 * Read number of scan lines
 *--------------------------------*/
   printf("\n\n****************************************\n");
   printf("   Information from AMSU-A swath fields\n");
   printf("****************************************\n\n");

   printf("swath filename %s\n", filename);
   if((sfid = SWopen(filename, DFACC_RDONLY)) == -1)
   {
     printf("read_swath_A/Open swath hdf file failed !\n");
     exit(4);
   }
   if( (sid = SWattach(sfid, swathname)) == -1)
   {
     printf("read_swath_A/Open swath file failed !\n");
     exit(5);
   }
   printf("read_swath_A/HDF file id  %ld,  SWATH id %ld\n", sfid, sid);


   dimsize[0] = SWdiminfo(sid, "Scanline");
   printf("read_swath_A/Scanline %6ld  \n", dimsize[0]);
   *nscan = dimsize[0];
   
/*--------------------------------*
 * Read AT fields
 *--------------------------------*/
   for(k = 0; k < 5; k++)
   {
      sprintf(atname,"Chan%ld_AT",k+1);

      status = SWreadfield(sid, atname, NULL, NULL, NULL, a_temp);
      printf("\n read_swath_A/read Chan%ld_AT  %ld\n",k+1,status);
      result = result + status;

      for(i = 0; i < *nscan; i++)
        for(j = 0; j < NUMSPOT_A; j++)
        {
           if(a_temp[i][j] > 0)
             at_A[i][j][k] = a_temp[i][j]/AT_SCAL;
	   else
             at_A[i][j][k] = a_temp[i][j];
        }

    }

    k = 14;
    sprintf(atname,"Chan%ld_AT",k+1);

    status = SWreadfield(sid, atname, NULL, NULL, NULL, a_temp);
    printf("\n read_swath_A/read Chan%ld_AT  %ld\n",k+1,status);
    result = result + status;

    for(i = 0; i < *nscan; i++)
      for(j = 0; j < NUMSPOT_A; j++)
      {
        if(a_temp[i][j] > 0)
          at_A[i][j][k] = a_temp[i][j]/AT_SCAL;
        else
          at_A[i][j][k] = a_temp[i][j];
      }

/*--------------------------------*
 * Read product fields
 *--------------------------------*/
   status = SWreadfield(sid, "CLW", NULL, NULL, NULL, clw_A);
   result = result + status;
   status = SWreadfield(sid, "TPW", NULL, NULL, NULL, tpw_A);
   result = result + status;
   status = SWreadfield(sid, "SIce", NULL, NULL, NULL, sice_A);
   result = result + status;
   status = SWreadfield(sid, "LZ_angle", NULL, NULL, NULL, lza_A);
   result = result + status;
   status = SWreadfield(sid, "Sfc_type", NULL, NULL, NULL, stype_A);
   result = result + status;

   status = SWdetach(sid);
   result += status;
   status = SWclose (sfid);
   result += status;
   printf("read_swath_A = %ld\n", result);

   return(result);

} /* end of read_swath_A.c */
