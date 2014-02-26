/***************************************************************************
 *  Program Name      : set_nc_snd.c
 *  Type              : Subroutine
 *  Function          : Program creates netcdf sounding file 
 *  Input Files       : 
 *  Output Files      : Netcdf files
 *  Subroutine Called : gnrt_nc_snd, wrt_nc_snd
 *  Called by         : rmirs_wnetcdf.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 *  
 *  12/05/2008      v              JZhao (PSGS)
 *************************************************************************/
#include "defaults.h"
#include "constants.h"
#include "swath.h"


extern int gnrt_nc_snd(char*, short int , short int, short int, short int, char* );
extern int wrt_nc_gdata(char*, short int , short int, short int );
extern int wrt_nc_snd(char*, short int , short int ,short int );


void set_nc_snd(char *fname_nc_snd, short int nscan, short int nspot, short int nlay, short int nchan, char *satid){

  int status=0;
  status=gnrt_nc_snd(fname_nc_snd, nscan, nspot, nlay, nchan, satid);
  if(status != 0) {
    printf("Error in generate the sounding netcdf file %s\n",fname_nc_snd);
    exit(15);
  }
  
  status=wrt_nc_gdata(fname_nc_snd, nscan, nspot, nchan);
  if(status != 0) {
    printf("Error in writing geo_data to the image netcdf file %s\n",fname_nc_snd);
    exit(18);
  }
  
  status=wrt_nc_snd(fname_nc_snd, nscan, nspot, nlay);
  if(status != 0) {
    printf("Error in writing data to the sounding netcdf file %s\n",fname_nc_snd);
    exit(16);
  }

}
