/**************************************************************************
 *  Program Name      : utc2hms.c
 *  Type              : Subroutine
 *  Function          : Program converts utc seconds to hour, minute and second 
 *  Input Files       : None
 *  Output Files      : None 
 *  Subroutine Called : 
 *  Called by         : rmirs_edr.c
 *
 *  Modification History:
 *      Date       Version         Description of Changes
 *     ------     ---------       ------------------------
 * 
 *************************************************************************/
/*
void utc2hms(double utc, short int * hr, short int *min, short int *sec){
  int day;
  short int thr, tmin,tsec;
  double today_sec;
  
  day=utc/(3600*24);
  today_sec = utc - day*(3600*24);

  thr=today_sec/3600;
  tmin = (today_sec - thr*3600) / 60;
  tsec = today_sec - thr * 3600 - tmin * 60; 
  
  *hr = thr;
  *min = tmin;
  *sec = tsec;
}
*/

void utc2hms(float today_sec, short int *hr, short int *min, short int *sec){
  
  short int thr, tmin, tsec;

  thr  = today_sec/3600;
  tmin = (today_sec - thr * 3600) / 60;
  tsec =  today_sec - thr * 3600 - tmin * 60; 
  
  *hr  = thr;
  *min = tmin;
  *sec = tsec;
}
