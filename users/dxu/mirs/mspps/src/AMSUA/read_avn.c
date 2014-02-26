/***************************************************************************
 *  Program Name      : read_avn.c
 *  Type              : Subroutine
 *  Function          : Program reads AVN data  
 *  Input Files       : AVN binary data files
 *  Output Files      : None
 *  Subroutine Called : None
 *  Called by         : avn.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *    3/2/2001      v2.0
 *    6/10/2004                   1. If the program can't open an AVN file, 
 *				     variable 'num_avn' is set to be -1 which
 *				     will prevent any products from being 
 *				     generated. (In the same situation, the
 *				     older version of the program will simply
 *				     stop running.) 
 *				  2. If AVN wind or Tsfc data are zero at five 
 *				     grid points along longitude=180 degree, then
 *				     num_avn=-1. 
 *
 *   09/19/06   change dir for OPUS dir structure    jzhao
 *   04/10/07   modified for ORA update, comment windv=0 as a criteria
 *              because that can be true             jzhao
 *************************************************************************/
#include "AMA2HDF_INCLUDE.h"
#include "ESWATH.h"

/*****************************************************************/
void read_avn(short int nset)
{
   FILE     *fp_FNAME;

   char     dir[100];
   char     *fileu[8]={"avn_u00.bin","avn_u03.bin","avn_u06.bin","avn_u09.bin","avn_u12.bin","avn_u15.bin","avn_u18.bin","avn_u21.bin"};
   char     *filev[8]={"avn_v00.bin","avn_v03.bin","avn_v06.bin","avn_v09.bin","avn_v12.bin","avn_v15.bin","avn_v18.bin","avn_v21.bin"};
   char     *filets[8]={"avn_ts00.bin","avn_ts03.bin","avn_ts06.bin","avn_ts09.bin","avn_ts12.bin","avn_ts15.bin","avn_ts18.bin","avn_ts21.bin"};
   char     *filets2m[8]={"avn_ts2m00.bin","avn_ts2m03.bin","avn_ts2m06.bin","avn_ts2m09.bin","avn_ts2m12.bin","avn_ts2m15.bin","avn_ts2m18.bin","avn_ts2m21.bin"};
   char     *filetpw[8]={"avn_tpw00.bin","avn_tpw03.bin","avn_tpw06.bin","avn_tpw09.bin","avn_tpw12.bin","avn_tpw15.bin","avn_tpw18.bin","avn_tpw21.bin"};
   char     file_u[80], file_v[80], file_ts[80], file_ts2m[80], file_tpw[80];

   short int  i, j;

   float    utmp[NUMROW_AVN][NUMCOL_AVN];
   float    vtmp[NUMROW_AVN][NUMCOL_AVN];
   float    tstmp[NUMROW_AVN][NUMCOL_AVN];
   float    ts2mtmp[NUMROW_AVN][NUMCOL_AVN];
   float    tpwtmp[NUMROW_AVN][NUMCOL_AVN];

   strcpy(dir, data_dir);
   strcat(dir,"GFS/current/");

   strcpy(file_u, dir);
   strcat(file_u, fileu[avnset[nset]]); 
   if((fp_FNAME=fopen(file_u,"rb")) == NULL)
   {
     printf("read_avn/Can't open AVN_U file !\n");
/*     exit(1); */
     num_avn = -1;
   }
   printf("read_avn/Successfully open AVN_U file!\n");
   fread(utmp, sizeof(utmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read AVN_U data in !\n");

   strcpy(file_v, dir);
   strcat(file_v, filev[avnset[nset]]); 
   if((fp_FNAME=fopen(file_v,"rb")) == NULL)
   {
     printf("read_avn/Can't open AVN_V file !\n");
/*     exit(1); */
     num_avn = -1;
   }
   printf("read_avn/Successfully open AVN_V file!\n");
   fread(vtmp, sizeof(vtmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read AVN_V data in !\n");

   strcpy(file_ts, dir);
   strcat(file_ts, filets[avnset[nset]]); 
   if((fp_FNAME=fopen(file_ts,"rb")) == NULL)
   {
     printf("read_avn/Can't open AVN_TS file !\n");
/*     exit(1); */
     num_avn = -1;
   }
   printf("read_avn/Successfully open AVN_TS file!\n");
   fread(tstmp, sizeof(tstmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read AVN_TS data in !\n");

   strcpy(file_ts2m, dir);
   strcat(file_ts2m, filets2m[avnset[nset]]); 
   if((fp_FNAME=fopen(file_ts2m,"rb")) == NULL)
   {
     printf("read_avn/Can't open AVN_TS2m file !\n");
/*     exit(1); */
     num_avn = -1;
   }
   printf("read_avn/Successfully open AVN_TS2m file!\n");
   fread(ts2mtmp, sizeof(ts2mtmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read AVN_TS2m data in !\n");

   strcpy(file_tpw, dir);
   strcat(file_tpw, filetpw[avnset[nset]]); 
   if((fp_FNAME=fopen(file_tpw,"rb")) == NULL)
   {
     printf("read_avn/Can't open AVN_TPW file !\n");
/*     exit(1); */
     num_avn = -1;
   }
   printf("read_avn/Successfully open AVN_TPW file!\n");
   fread(tpwtmp, sizeof(tpwtmp), 1, fp_FNAME);
   fclose(fp_FNAME);
   printf(" Successfully read AVN_TPW data in !\n");

/*-----------------------------------------------------------*
   AVN data starts from longitude = 0. It needs to be changed
   to start from -180. 
 *-----------------------------------------------------------*/

   for(i = 0; i < NUMROW_AVN; i++)
   {
     for(j = 0; j < NUMCOL_AVN/2; j++)
     {
       windu_avn[nset][i][j+NUMCOL_AVN/2] = utmp[i][j];
       windv_avn[nset][i][j+NUMCOL_AVN/2] = vtmp[i][j];
       ts_avn[nset][i][j+NUMCOL_AVN/2] = tstmp[i][j];
       ts2m_avn[nset][i][j+NUMCOL_AVN/2] = ts2mtmp[i][j];
       tpw_avn[nset][i][j+NUMCOL_AVN/2] = tpwtmp[i][j];
     }

     for(j = NUMCOL_AVN/2; j < NUMCOL_AVN; j++)
     {
       windu_avn[nset][i][j-NUMCOL_AVN/2] = utmp[i][j];
       windv_avn[nset][i][j-NUMCOL_AVN/2] = vtmp[i][j];
       ts_avn[nset][i][j-NUMCOL_AVN/2] = tstmp[i][j];
       ts2m_avn[nset][i][j-NUMCOL_AVN/2] = ts2mtmp[i][j];
       tpw_avn[nset][i][j-NUMCOL_AVN/2] = tpwtmp[i][j];
     }

   }

  if(ts_avn[nset][0][0] == 0 && ts_avn[nset][10][10] == 0 && ts_avn[nset][20][20] == 0 && ts_avn[nset][30][30] ==0 && ts_avn[nset][40][40] == 0 && ts_avn[nset][50][50] == 0)
    num_avn = -1;
  if(tpw_avn[nset][0][0] == 0 && tpw_avn[nset][10][10] == 0 && tpw_avn[nset][20][20] == 0 && tpw_avn[nset][30][30] ==0 && tpw_avn[nset][40][40] == 0 && tpw_avn[nset][50][50] == 0)
    num_avn = -1;
  if(windu_avn[nset][0][0] == 0 && windu_avn[nset][10][10] == 0 && windu_avn[nset][20][20] == 0 && windu_avn[nset][30][30] ==0 && windu_avn[nset][40][40] == 0 && windu_avn[nset][50][50] == 0)
    num_avn = -1;
  /************************************
       if(windv_avn[nset][0][0] == 0 && windv_avn[nset][10][10] == 0 && windv_avn[nset][20][20] == 0 && windv_avn[nset][30][30] ==0 && windv_avn[nset][40][40] == 0 && windv_avn[nset][50][50] == 0)
    num_avn = -1;
  *****************************************/

} /* end of read_avn.c */
