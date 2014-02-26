/***************************************************************************
 *  Program Name      : wrt_nc_gdata.c
 *  Type              : Subroutine
 *  Function          : Program creates netcdf sounding file 
 *  Input Files       : 
 *  Output Files      : Netcdf files
 *  Subroutine Called : 
 *  Called by         : set_nc_snd.c,set_nc_img.c
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

int wrt_nc_gdata(char *fname_nc_snd, short int nscan, short int nspot, short int nchan){
   int  stat;			/* return status */
   int  ncid;			/* netCDF id */
   int  errid = 18;
   int  varid;
   int  nqc = 4;
   
   
   size_t start3[]={0,0,0};
   size_t count3[]={nscan,nspot,nqc};

   size_t start1[]={0};
   size_t count1[]={nscan};

   size_t start2[]={0,0};
   size_t count2[]={nscan,nspot};


   size_t start0[]={0};
   size_t count0[]={nchan};


   float arr_float_2D[nscan * nspot];
   short int arr_short_2D[nscan * nspot];
   short int arr_short_3D[nscan * nspot * nqc];
   int i,j,k,n;
   

   stat = nc_open(fname_nc_snd, NC_WRITE, &ncid);
   check_err(stat,__LINE__,__FILE__,errid);
   
   
   // channel infor
   if ( (stat = nc_inq_varid(ncid, "Freq", &varid)) )
       check_err(stat,__LINE__,__FILE__,errid); 
   if ( (stat = nc_put_vara_float(ncid, varid, start0, count0, freq)))
       check_err(stat,__LINE__,__FILE__,errid); 

   if ( (stat = nc_inq_varid(ncid, "Polo", &varid)) )
       check_err(stat,__LINE__,__FILE__,errid); 
   if ( (stat = nc_put_vara_short(ncid, varid, start0, count0, polo)))
       check_err(stat,__LINE__,__FILE__,errid); 
   
   
   
   //Geo data
   if ((stat = nc_inq_varid(ncid, "ScanTime_year", &varid)))
     check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start1, count1, year)))
     check_err(stat,__LINE__,__FILE__,errid); 

   if ((stat = nc_inq_varid(ncid, "ScanTime_doy", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start1, count1, doy)))
       check_err(stat,__LINE__,__FILE__,errid); 

   if ((stat = nc_inq_varid(ncid, "ScanTime_month", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start1, count1, month)))
       check_err(stat,__LINE__,__FILE__,errid); 

   if ((stat = nc_inq_varid(ncid, "ScanTime_dom", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start1, count1, dom)))
       check_err(stat,__LINE__,__FILE__,errid); 
 
   if ((stat = nc_inq_varid(ncid, "ScanTime_hour", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start1, count1, hour)))
       check_err(stat,__LINE__,__FILE__,errid); 

   if ((stat = nc_inq_varid(ncid, "ScanTime_minute", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start1, count1, minute)))
       check_err(stat,__LINE__,__FILE__,errid); 

   if ((stat = nc_inq_varid(ncid, "ScanTime_second", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start1, count1, second)))
       check_err(stat,__LINE__,__FILE__,errid); 

  
   if ((stat = nc_inq_varid(ncid, "ScanTime_UTC", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_double(ncid, varid, start1, count1, time_utc)))
       check_err(stat,__LINE__,__FILE__,errid); 

   if ((stat = nc_inq_varid(ncid, "Orb_mode", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start1, count1, orb_mode)))
       check_err(stat,__LINE__,__FILE__,errid); 


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_float_2D[k] = lat[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "Latitude", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_float(ncid, varid, start2, count2, arr_float_2D )))
       check_err(stat,__LINE__,__FILE__,errid); 


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_float_2D[k] = lon[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "Longitude", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_float(ncid, varid, start2, count2, arr_float_2D )))
       check_err(stat,__LINE__,__FILE__,errid); 


   //Ancillary data
   
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_short_2D[k] = iTypSfc[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "Sfc_type", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr_short_2D )))
       check_err(stat,__LINE__,__FILE__,errid); 


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_short_2D[k] = iTypAtm[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "Atm_type", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr_short_2D ))) 
       check_err(stat,__LINE__,__FILE__,errid); 


   //QC data
   
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
   for(k=0;k<nqc;k++){
     n = i*nspot*nqc + j*nqc + k;
     arr_short_3D[n] = qc[i][j][k];
   }
   }
   }
   if ((stat = nc_inq_varid(ncid, "Qc", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start3, count3, arr_short_3D )))
       check_err(stat,__LINE__,__FILE__,errid); 


/*
   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_short_2D[k] = nattempt[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "NAttempt", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr_short_2D )))
       check_err(stat,__LINE__,__FILE__,errid); 


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_short_2D[k] = niter[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "NIter", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_short(ncid, varid, start2, count2, arr_short_2D )))
       check_err(stat,__LINE__,__FILE__,errid); 
*/


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_float_2D[k] = chisq[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "ChiSqr", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_float(ncid, varid, start2, count2, arr_float_2D )))
       check_err(stat,__LINE__,__FILE__,errid); 


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_float_2D[k] = angle[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "LZ_angle", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_float(ncid, varid, start2, count2, arr_float_2D )))
       check_err(stat,__LINE__,__FILE__,errid);


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_float_2D[k] = rel_azi[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "RAzi_angle", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_float(ncid, varid, start2, count2, arr_float_2D )))
       check_err(stat,__LINE__,__FILE__,errid);


   for(i=0;i<nscan;i++){
   for(j=0;j<nspot;j++){
     k=i*nspot+j;
     arr_float_2D[k] = sza[i][j];
   }
   }
   if ((stat = nc_inq_varid(ncid, "SZ_angle", &varid)))
       check_err(stat,__LINE__,__FILE__,errid); 
   if ((stat = nc_put_vara_float(ncid, varid, start2, count2, arr_float_2D )))
       check_err(stat,__LINE__,__FILE__,errid);


   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__,errid);

   return 0;
}

