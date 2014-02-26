/***************************************************************************
 *  Program Name      : wrt_nc_img.c
 *  Type              : Subroutine
 *  Function          : Program creates netcdf image file 
 *  Input Files       : 
 *  Output Files      : Netcdf files
 *  Subroutine Called : 
 *  Called by         : set_nc_img.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *  
 *  12/2008      v0              JZhao (PSGS)
 *  05/2011      v1              Wanchun Chen  updated for netcdf4
 *************************************************************************/

#include "defaults.h"
#include "constants.h"
#include "swath.h"
#include <netcdf.h>

extern void check_err(const int stat, const int line, const char *file, int err_id);

int wrt_nc_img(char * fname_nc_img, short int nscan, short int nspot, short int  nchan){
   int  stat;			/* return status */
   int  ncid;			/* netCDF id */
   int  errid = 18;

   int varid;

   int i=0,j=0,k=0,n=0;

   //short int temp[nscan][nspot][nchan];
   
   short int arr2D[nscan * nspot];
   short int arr3D[nscan * nspot * nchan];
   
   //size_t start1[]={0};
   //size_t count1[]={nscan};

   size_t start2[]={0,0};
   size_t count2[]={nscan,nspot};

   size_t start3[]={0,0,0};
   size_t count3[]={nscan,nspot,nchan};


   if ((stat = nc_open(fname_nc_img, NC_WRITE, &ncid)))
   check_err(stat,__LINE__,__FILE__,errid);


   // PROFILE products

   if ((stat = nc_inq_varid(ncid, "BT", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nchan;k++){
     n = i*nspot*nchan + j*nchan + k;
     arr3D[n] = bt[i][j][k];
   }
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start3, count3, arr3D)))
       check_err(stat,__LINE__,__FILE__,errid); 


   if ((stat = nc_inq_varid(ncid, "YM", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nchan;k++){
     n = i*nspot*nchan + j*nchan + k;
     arr3D[n] = ym[i][j][k];
   }
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start3, count3, arr3D)))
       check_err(stat,__LINE__,__FILE__,errid); 

/*
   if ((stat = nc_inq_varid(ncid, "YFWD", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nchan;k++){
     n = i*nspot*nchan + j*nchan + k;
     arr3D[n] = yfwd[i][j][k];
   }
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start3, count3, arr3D)))
       check_err(stat,__LINE__,__FILE__,errid); 
*/


   if ((stat = nc_inq_varid(ncid, "ChanSel", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nchan;k++){
     n = i*nspot*nchan + j*nchan + k;
     arr3D[n] = chanSel[i][j][k];
   }
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start3, count3, arr3D)))
       check_err(stat,__LINE__,__FILE__,errid); 


   if ((stat = nc_inq_varid(ncid, "TPW", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = tpw[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
       check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "CLW", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = clw[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
       check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "RWP", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = rwp[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
       check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "LWP", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = lwp[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
       check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "SWP", &varid)))
       check_err(stat,__LINE__,__FILE__,errid);
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = swp[i][j];
   }
   }   
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
       check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "IWP", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = iwp[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D))) 
   check_err(stat,__LINE__,__FILE__,errid);

   if ((stat = nc_inq_varid(ncid, "GWP", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = gwp[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "RR", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = rr[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D))) 
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "Snow", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = snow[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D))) 
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "SWE", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = swe[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "SnowGS", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = snowgs[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "SIce", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = sice[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "SIce_MY", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = sice_my[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2,arr2D))) 
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "SIce_FY", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = sice_fy[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "TSkin", &varid)))
   check_err(stat,__LINE__,__FILE__,errid);
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = tskin[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);

   if ((stat = nc_inq_varid(ncid, "SurfP", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = surfp[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid); 



   // Emissivity need trim as BT
   if ((stat = nc_inq_varid(ncid, "Emis", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nchan;k++){
     n = i*nspot*nchan + j*nchan + k;
     arr3D[n] = emis[i][j][k];
   }
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start3, count3, arr3D)))
   check_err(stat,__LINE__,__FILE__,errid);

   
   if ((stat = nc_inq_varid(ncid, "SFR", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = sfr[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "CldTop", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = cldtop[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "CldBase", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = cldbase[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "CldThick", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = cldthick[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "PrecipType", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = preciptype[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "RFlag", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = rflag[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   
   if ((stat = nc_inq_varid(ncid, "SurfM", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = surfm[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "WindSp", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = windsp[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "WindDir", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = winddir[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "WindU", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = windu[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);


   if ((stat = nc_inq_varid(ncid, "WindV", &varid)))
   check_err(stat,__LINE__,__FILE__,errid); 
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr2D[k] = windv[i][j];
   }
   }
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr2D)))
   check_err(stat,__LINE__,__FILE__,errid);

   
   
   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__,errid);

   return 0;
}

