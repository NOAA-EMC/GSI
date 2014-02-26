/***************************************************************************
 *  Program Name      : read_gdas.c
 *  Type              : Subroutine
 *  Function          : Program reads in GDAS data 
 *  Input Files       : gdas_u.bin, gdas_v.bin, gdas_ts.bin, 
 *			gdas_ts2m.bin, gdas_tpw.bin
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : AMA2HDF.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *   8/30/2000      v2.0
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/***************************************************************/
void read_gdas()
{
   FILE     *fp_FNAME;

   char     dir[]="../../../gdas/";
   char     *fileu="gdas_u.bin";
   char     *filev="gdas_v.bin";
   char     *filets="gdas_ts.bin";
   char     *filets2m="gdas_ts2m.bin";
   char     *filetpw="gdas_tpw.bin";
   char     file_u[80], file_v[80], file_ts[80], file_ts2m[80], file_tpw[80];

   short int  i, j;

   float    utmp[NUMROW_GDAS][NUMCOL_GDAS];
   float    vtmp[NUMROW_GDAS][NUMCOL_GDAS];
   float    tstmp[NUMROW_GDAS][NUMCOL_GDAS];
   float    ts2mtmp[NUMROW_GDAS][NUMCOL_GDAS];
   float    tpwtmp[NUMROW_GDAS][NUMCOL_GDAS];

/*---------------------------------------------------*
 * Read GDAS data 
 *---------------------------------------------------*/
   strcpy(file_u, dir);
   strcat(file_u, fileu); 
   if((fp_FNAME=fopen(file_u,"rb")) == NULL)
   {
     printf("AMA2HDF/Can't open GDAS_U file !\n");
     exit(1);
   }
   printf("Surftype / Successfully open GDAS_U file!\n");
   fread(utmp, sizeof(utmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read GDAS-U data in !\n");

   strcpy(file_v, dir);
   strcat(file_v, filev); 
   if((fp_FNAME=fopen(file_v,"rb")) == NULL)
   {
     printf("AMA2HDF/Can't open GDAS_V file !\n");
     exit(1);
   }
   printf("Surftype / Successfully open GDAS_V file!\n");
   fread(vtmp, sizeof(vtmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read GDAS-V data in !\n");

   strcpy(file_ts, dir);
   strcat(file_ts, filets); 
   if((fp_FNAME=fopen(file_ts,"rb")) == NULL)
   {
     printf("AMA2HDF/Can't open GDAS_TS file !\n");
     exit(1);
   }
   printf("Surftype / Successfully open GDAS_TS file!\n");
   fread(tstmp, sizeof(tstmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read GDAS-TS data in !\n");

   strcpy(file_ts2m, dir);
   strcat(file_ts2m, filets2m); 
   if((fp_FNAME=fopen(file_ts2m,"rb")) == NULL)
   {
     printf("AMA2HDF/Can't open GDAS_TS2m file !\n");
     exit(1);
   }
   printf("Surftype / Successfully open GDAS_TS2m file!\n");
   fread(ts2mtmp, sizeof(ts2mtmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read GDAS-TS2m data in !\n");

   strcpy(file_tpw, dir);
   strcat(file_tpw, filetpw); 
   if((fp_FNAME=fopen(file_tpw,"rb")) == NULL)
   {
     printf("AMA2HDF/Can't open GDAS_TPW file !\n");
     exit(1);
   }
   printf("Surftype / Successfully open GDAS_TPW file!\n");
   fread(tpwtmp, sizeof(tpwtmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read GDAS-TPW data in !\n");

/*-----------------------------------------------------*
 * The longitude of the original data ranges from
 * 0E to 0W. Change it to ranging from 180W to 180E. 
 *-----------------------------------------------------*/
   for(i = 0; i < NUMROW_GDAS; i++)
   {
     for(j = 0; j < NUMCOL_GDAS/2; j++)
     {
       windu_gdas[i][j+NUMCOL_GDAS/2] = utmp[i][j];
       windv_gdas[i][j+NUMCOL_GDAS/2] = vtmp[i][j];
       ts_gdas[i][j+NUMCOL_GDAS/2] = tstmp[i][j];
       ts2m_gdas[i][j+NUMCOL_GDAS/2] = ts2mtmp[i][j]; 
       tpw_gdas[i][j+NUMCOL_GDAS/2] = tpwtmp[i][j]; 
     }

     for(j = NUMCOL_GDAS/2; j < NUMCOL_GDAS; j++)
     {
       windu_gdas[i][j-NUMCOL_GDAS/2] = utmp[i][j];
       windv_gdas[i][j-NUMCOL_GDAS/2] = vtmp[i][j];
       ts_gdas[i][j-NUMCOL_GDAS/2] = tstmp[i][j];
       ts2m_gdas[i][j-NUMCOL_GDAS/2] = ts2mtmp[i][j];
       tpw_gdas[i][j-NUMCOL_GDAS/2] = tpwtmp[i][j];
     }

   }

}   /* end of read_gdas.c */
