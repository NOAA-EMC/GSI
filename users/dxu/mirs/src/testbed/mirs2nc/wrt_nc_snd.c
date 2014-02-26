/***************************************************************************
 *  Program Name      : wrt_nc_snd.c
 *  Type              : Subroutine
 *  Function          : Program creates netcdf sounding file 
 *  Input Files       : 
 *  Output Files      : Netcdf files
 *  Subroutine Called : 
 *  Called by         : set_nc_snd.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *  
 *  12/2008      v0              JZhao (PSGS)
 *  05/2011      v1              Wanchun Chen  updated for netcdf4
 *  11/2011      v2              Kevin Garrett commented out writing O3 profile
 **************************************************************************/
 
#include "defaults.h"
#include "constants.h"
#include "swath.h"
#include <netcdf.h>

void check_err(const int stat, const int line, const char *file ,int err_id );

int wrt_nc_snd(char * fname_nc_snd, short int nscan, short int nspot, short int  nlay){
   int  stat;			/* return status */
   int  ncid;			/* netCDF id */
   int  errid = 16;
   int  varid;
   float arr3D[nscan * nspot * nlay];
   short int arr2D[nscan * nspot];
   int i,j,k,n;

   //size_t start1[]={0};
   //size_t count1[]={nscan};

   size_t start2[]={0,0};
   size_t count2[]={nscan,nspot};

   size_t start3[]={0,0,0};
   size_t count3[]={nscan,nspot,nlay};

   //printf("In wrt_nc_snd.c ....\n");
   stat = nc_open(fname_nc_snd, NC_WRITE, &ncid);
   check_err(stat,__LINE__,__FILE__,errid);

   //Layer of the products
   if ((stat = nc_inq_varid(ncid, "Player", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_var_float(ncid, varid, player)))
   check_err(stat,__LINE__,__FILE__,errid);

   if ((stat = nc_inq_varid(ncid, "Plevel", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_var_float(ncid, varid, plevel)))
   check_err(stat,__LINE__,__FILE__,errid);


   //PROFILE products

   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nlay;k++){
     n = i*nspot*nlay + j*nlay + k;
     arr3D[n] = ptemp[i][j][k];
   }
   }
   }
   if ((stat = nc_inq_varid(ncid, "PTemp", &varid)))
   check_err(stat,__LINE__,__FILE__,errid);
   if ((stat = nc_put_vara_float(ncid, varid, start3, count3, arr3D)))
   check_err(stat,__LINE__,__FILE__,errid);


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nlay;k++){
     n = i*nspot*nlay + j*nlay + k;
     arr3D[n] = pvapor[i][j][k];
   }
   }
   }
   if ((stat = nc_inq_varid(ncid, "PVapor", &varid)))
   check_err(stat,__LINE__,__FILE__,errid);
   if ((stat = nc_put_vara_float(ncid, varid, start3, count3, arr3D)))
   check_err(stat,__LINE__,__FILE__,errid);


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nlay;k++){
     n = i*nspot*nlay + j*nlay + k;
     arr3D[n] = pclw[i][j][k];
   }
   }
   }
   if ((stat = nc_inq_varid(ncid, "PClw", &varid)))
   check_err(stat,__LINE__,__FILE__,errid);
   if ((stat = nc_put_vara_float(ncid, varid, start3,count3, arr3D)))
   check_err(stat,__LINE__,__FILE__,errid);


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nlay;k++){
     n = i*nspot*nlay + j*nlay + k;
     arr3D[n] = prain[i][j][k];
   }
   }
   }
   if ((stat = nc_inq_varid(ncid, "PRain", &varid)))
   check_err(stat,__LINE__,__FILE__,errid);
   if ((stat = nc_put_vara_float(ncid, varid, start3, count3, arr3D)))
   check_err(stat,__LINE__,__FILE__,errid);


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nlay;k++){
     n = i*nspot*nlay + j*nlay + k;
     arr3D[n] = pgraupel[i][j][k];
   }
   }
   }
   if ((stat = nc_inq_varid(ncid, "PGraupel", &varid)))
   check_err(stat,__LINE__,__FILE__,errid);
   if ((stat = nc_put_vara_float(ncid, varid, start3,count3, arr3D)))
   check_err(stat,__LINE__,__FILE__,errid);


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nlay;k++){
     n = i*nspot*nlay + j*nlay + k;
     arr3D[n] = psnow[i][j][k];
   }
   }
   }
   if ((stat = nc_inq_varid(ncid, "PSnow", &varid)))
   check_err(stat,__LINE__,__FILE__,errid);
   if ((stat = nc_put_vara_float(ncid, varid, start3, count3, arr3D)))
   check_err(stat,__LINE__,__FILE__,errid);


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nlay;k++){
     n = i*nspot*nlay + j*nlay + k;
     arr3D[n] = pice[i][j][k];
   }
   }
   }
   if ((stat = nc_inq_varid(ncid, "PIce", &varid)))
   check_err(stat,__LINE__,__FILE__,errid);
   if ((stat = nc_put_vara_float(ncid, varid, start3, count3, arr3D)))
   check_err(stat,__LINE__,__FILE__,errid);
   
   
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = surfp[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "SurfP", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid); 


   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__,errid);

   return 0;
}

