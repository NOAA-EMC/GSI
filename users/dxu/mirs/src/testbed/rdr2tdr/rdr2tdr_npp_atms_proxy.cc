#include <iostream>
#include <string>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <ctime>
#include "hdf5.h"

#include "../../lib_cpp/MeasurementType.h"

using namespace std;

//#define file_GATMO "GATMO_npp_d20030125_t084657_e084833_b00015_c20071214193913_den_OPS_SEG.h5"
//#define file_TATMS "TATMS_npp_d20030125_t084657_e084833_b00015_c20071214193913_den_OPS_SEG.h5"

/*  struct tm 
int    tm_sec   seconds [0,61]  tm_sec is generally 0-59. Extra range to accommodate for leap seconds in certain systems.
int    tm_min   minutes [0,59]
int    tm_hour  hour [0,23]
int    tm_mday  day of month [1,31]
int    tm_mon   month of year [0,11]
int    tm_year  years since 1900
int    tm_wday  day of week [0,6] (Sunday = 0)
int    tm_yday  day of year [0,365]
int    tm_isdst daylight savings flag 
       is greater than zero if Daylight Saving Time is in effect, 
       zero if Daylight Saving Time is not in effect, 
       and less than zero if the information is not available.
*/


/**
 * return 1 if little endian machine,
 * return 0 if big endian machine.
 */
int getEndian()
{
    union {
        int theInteger;
        char theByte;
    } endianUnion;

    endianUnion.theInteger = 1 ;
    return endianUnion.theByte;
}

int intSwap(char *value)
{
  char buffer[ 4 ];

  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 3 ];
    buffer[ 1 ] = value[ 2 ];
    buffer[ 2 ] = value[ 1 ];
    buffer[ 3 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
    buffer[ 2 ] = value[ 2 ];
    buffer[ 3 ] = value[ 3 ];
  }

  return *( (int *) &buffer );
}


short int shortSwap(char *value)
{
  char buffer[ 2 ];
  
  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 1 ];
    buffer[ 1 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
  }
  return *( (short int *) &buffer );
}


float floatSwap(char *value)
{
  char buffer[ 4 ];

  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 3 ];
    buffer[ 1 ] = value[ 2 ];
    buffer[ 2 ] = value[ 1 ];
    buffer[ 3 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
    buffer[ 2 ] = value[ 2 ];   
    buffer[ 3 ] = value[ 3 ];
  }

  return *( (float *) &buffer );
}


long long longlongSwap(char *value)
{
  char buffer[ 8 ];
  
  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 7 ];
    buffer[ 1 ] = value[ 6 ];
    buffer[ 2 ] = value[ 5 ];
    buffer[ 3 ] = value[ 4 ];
    buffer[ 4 ] = value[ 3 ];
    buffer[ 5 ] = value[ 2 ];
    buffer[ 6 ] = value[ 1 ];
    buffer[ 7 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
    buffer[ 2 ] = value[ 2 ];
    buffer[ 3 ] = value[ 3 ];
    buffer[ 4 ] = value[ 4 ];
    buffer[ 5 ] = value[ 5 ];
    buffer[ 6 ] = value[ 6 ];
    buffer[ 7 ] = value[ 7 ];
  }
  return *( (long long *) &buffer );
}


double doubleSwap(char *value)
{
  char buffer[ 8 ];

  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 7 ];
    buffer[ 1 ] = value[ 6 ];
    buffer[ 2 ] = value[ 5 ];
    buffer[ 3 ] = value[ 4 ];
    buffer[ 4 ] = value[ 3 ];
    buffer[ 5 ] = value[ 2 ];
    buffer[ 6 ] = value[ 1 ];
    buffer[ 7 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
    buffer[ 2 ] = value[ 2 ];
    buffer[ 3 ] = value[ 3 ];
    buffer[ 4 ] = value[ 4 ];
    buffer[ 5 ] = value[ 5 ];
    buffer[ 6 ] = value[ 6 ];
    buffer[ 7 ] = value[ 7 ];
  }
  return *( (double *) &buffer );
}


int BSWAP32(int x)  {
  
  int y =  ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) | 
      	   ( ((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))  ;
  return y;

}


int cal2jday(int year, int month, int day )
{
  if ( day > 31 || day < 1 || month > 12 || month < 1 )  return -1;
  
  int JulianDate1[12] = { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 } ;
  int JulianDate2[12] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 } ;

  int leap = 0; 
  if ( ( year % 4 == 0 && year % 100 != 0 ) || year % 400 == 0 ) leap = 1;
  
  int jday;  
  if ( leap == 1 ) jday = JulianDate1[month-1] + day ;
  else 		   jday = JulianDate2[month-1] + day ;
  
  return jday;
}

/*--------------------------------------------------------------------------*/
/* This routine converts a date given in seconds into years, months, days,  */
/* hours, minutes and seconds which are fields in tm struct.  The zero      */
/* point of the date is set to 1900 Jan 1, at 00:00. This routine will work */
/* only between the years 1900 to 2040. The algorithm is from the book:     */
/* Jean Meeus, Astronomical Algorithms, p 59-66.                            */
/*--------------------------------------------------------------------------*/
void seconds2tm(unsigned long Secs, struct tm *tp) {

  long Seconds;
  long alfa,B,C,D,E;
  long DayNbr;
  
  Seconds = ((unsigned long) Secs) % ((unsigned long) 86400);
  DayNbr  = ((unsigned long) Secs) / ((unsigned long) 86400);

  tp->tm_hour = (int)(Seconds/3600);
  tp->tm_min  = (int)((Seconds % 3600)/60);
  tp->tm_sec  = (int)(Seconds % 60);

  DayNbr += 2415021;  /* 1900, Jan 1, 00:00 is Julian day 2415020.5 */
  alfa = (long)((DayNbr-1867216.25)/36524.25);
  B = DayNbr+alfa-alfa/4+1525;
  C = (long)((B-122.1)/365.25);
  D = (long)(365.25*C);
  E = (long)((B-D)/30.6001);
  tp->tm_mday = B-D-(long)(30.6001*E);
  tp->tm_mon  = (E<14) ? E-2 : E-14;
  tp->tm_year = (tp->tm_mon > 1) ? C-6616 : C-6615;
  
  tp->tm_yday = cal2jday(tp->tm_year, tp->tm_mon+1, tp->tm_mday) - 1 ;
  
  tp->tm_isdst = 0;

  /*
  cout << "year="  << tp->tm_year << endl;
  cout << "month=" << tp->tm_mon  << endl;
  cout << "day="   << tp->tm_mday << endl;
  cout << "hour="  << tp->tm_hour << endl;
  cout << "min="   << tp->tm_min  << endl;
  cout << "sec="   << tp->tm_sec  << endl;
  cout << "jday="  << tp->tm_yday << endl;
  */
}

int main(int argc, char *argv[] )
{
  hid_t dsetr_id;
  hid_t space_id;
  hid_t type_id;
  H5T_class_t class_id;
  hsize_t storage_size;
  int status; 

  float freq_npp[22] = { 23.800, 31.400, 50.300, 51.760, 52.800, 53.596, 54.400, 54.940, 55.500, 57.290,
	57.290, 57.290, 57.290, 57.290, 57.290, 88.200, 165.500, 183.310, 183.310, 183.310,
	183.310, 183.310 } ;
  int polar_npp[22] = { 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3 };
   	 
  float freq_n18[20] = { 23.800, 31.400, 50.300, 52.799, 53.595, 54.400, 54.941, 55.499, 57.290, 57.290,
	57.290, 57.290, 57.290, 57.290, 89.000, 89.000, 157.000, 183.311, 183.311, 190.311 } ;
  int polar_n18[20] = { 2, 2, 2, 2, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 3, 3, 2 } ;


  const int NQC   = 4;
  
  const int FILESIZE = 512;
  const int FILENUM  = 32;

  int index;
  int index2;
  
  char file_TATMS[FILESIZE];
  //char file_GATMO[FILESIZE];
  char file_OUT[FILESIZE];
  
  string files_tatms[FILENUM];
  //string files_gatmo[FILENUM];
  string files_out[FILENUM];
  
  char rdrFileList[FILESIZE];
  char pathTDR[FILESIZE];
  char nedtFile[FILESIZE];
  char instrConfigFile[FILESIZE];
  char norbits2process[FILESIZE];
  char logFile[FILESIZE];
  
  ////////////////////////////////////////////////////////////////////////////////////////////
  //
  // pre-define some time stuffs
  //
  ////////////////////////////////////////////////////////////////////////////////////////////

  struct tm time1900;		// 1900
  time1900.tm_sec   = 0;
  time1900.tm_min   = 0;
  time1900.tm_hour  = 0;
  time1900.tm_mday  = 1;
  time1900.tm_mon   = 0;
  time1900.tm_year  = 0;
  time1900.tm_wday  = 1;
  time1900.tm_yday  = 0;
  time1900.tm_isdst = 0;
  //time_t t1900 = mktime(&time1900);
  //printf("%s", asctime(&time1900));
 
  struct tm time1958;  		// 1958
  time1958.tm_sec   = 0;
  time1958.tm_min   = 0;
  time1958.tm_hour  = 0;
  time1958.tm_mday  = 1;
  time1958.tm_mon   = 0;
  time1958.tm_year  = 58;
  time1958.tm_yday  = 0;
  time1958.tm_isdst = 0;
  //time_t t1958 = mktime(&time1958);
  //printf("%s", asctime(&time1958));
  
  /*
  struct tm time1970;  		// 1970
  time1970.tm_sec   = 0;
  time1970.tm_min   = 0;
  time1970.tm_hour  = 0;
  time1970.tm_mday  = 1;
  time1970.tm_mon   = 0;
  time1970.tm_year  = 70;
  time1970.tm_yday  = 0;
  time1970.tm_isdst = 0;
  time_t t1970 = mktime(&time1970);
  printf("%s", asctime(&time1970));
  */
  
  //cout << long( difftime( t1958, t1900 ) ) << endl; // 1830297600
  //cout << long( difftime( t1970, t1900 ) ) << endl; // 2208988800 or 2208985200(wrong)  
  //cout << long( difftime( t1970, t1958 ) ) << endl; // 378691200  or 378687600(wrong)  
  
  // seconds of 1958 since 1900
  //unsigned long secsBefore1958 = (unsigned long)(difftime( t1958, t1900 ));
  //cout << "secsBefore1958=" << secsBefore1958 << endl;  // 1830297600 
  
  ////////////////////////////////////////////////////////////////////////////////////////////
  //
  // to get file list and control parameters
  //
  ////////////////////////////////////////////////////////////////////////////////////////////
  string line;
  getline(cin,line);   // ignore first line
  
  // sdrFileList
  getline(cin, line);
  int start_index = line.find_first_of("=") + 1;
  int len = line.length();
  for(int i=0; i<len-start_index;i++)
      rdrFileList[i] = line[i+start_index];
  rdrFileList[len-start_index] = '\0';
  //cout << rdrFileList << endl;

  // pathTDR
  getline(cin, line);
  start_index = line.find_first_of("=") + 1;
  len = line.length();
  for(int i=0; i<len-start_index;i++)
      pathTDR[i] = line[i+start_index];
  pathTDR[len-start_index] = '\0';
  //cout << pathTDR << endl;
  
  // nedtFile
  getline(cin, line);
  start_index = line.find_first_of("=") + 1;
  len = line.length();
  for(int i=0; i<len-start_index;i++)
      nedtFile[i] = line[i+start_index];
  nedtFile[len-start_index] = '\0';
  //cout << nedtFile << endl;
  
  // instrConfigFile
  getline(cin, line);
  start_index = line.find_first_of("=") + 1;
  len = line.length();
  for(int i=0; i<len-start_index;i++)
      instrConfigFile[i] = line[i+start_index];
  instrConfigFile[len-start_index] = '\0';
  //cout << instrConfigFile << endl;
  
  // norbits2process
  getline(cin, line);
  start_index = line.find_first_of("=") + 1;
  len = line.length();
  for(int i=0; i<len-start_index;i++)
      norbits2process[i] = line[i+start_index];
  norbits2process[len-start_index] = '\0';
  //cout << norbits2process << endl;
  
  // logFile
  getline(cin, line);
  start_index = line.find_first_of("=") + 1;
  len = line.length();
  for(int i=0; i<len-start_index;i++)
      logFile[i] = line[i+start_index];
  logFile[len-start_index] = '\0';
  //cout << logFile << endl;

  ////////////////////////////////////////////////////////////////////////////////////////////
  //
  // fill in 3 file name arrays
  //
  ////////////////////////////////////////////////////////////////////////////////////////////
  ifstream inFile(rdrFileList, ios::in);
  if ( !inFile ) {
      cerr << "File could not be opened: " << rdrFileList << endl;
      exit( 1 );
  }
  int nfile=0;
  while ( getline(inFile,line) ) {
      files_tatms[nfile] = line;
      int pos = line.find("SATMS_");
      //files_gatmo[nfile] = line.replace(pos,6,"GATMO_");
      //pos = line.find("GATMO_");
      //line.replace(pos,6,"TDR_");
      files_out[nfile] = string(pathTDR) + "TDR_" + line.substr(pos,FILESIZE);
      nfile++;
  }
  inFile.close();

  ////////////////////////////////////////////////////////////////////////////////////////////
  //
  // start loop around files
  //
  ////////////////////////////////////////////////////////////////////////////////////////////
  
  for(int ifile=0; ifile<nfile; ifile++ ) {
    
    cout << files_tatms[ifile] << endl;
    //cout << files_gatmo[ifile] << endl;
    cout << files_out[ifile]   << endl;
    
    strcpy( file_TATMS,files_tatms[ifile].c_str() );
    //strcpy( file_GATMO,files_gatmo[ifile].c_str() );
    strcpy( file_OUT,  files_out[ifile].c_str()   );


   
    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // TATMS file ( contains antenna temperature )
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    hid_t file_id	= H5Fopen(file_TATMS, H5F_ACC_RDONLY, H5P_DEFAULT);
    hid_t root_id	= H5Gopen1(file_id, "/");
    hid_t all_data_id   = H5Gopen1(root_id, "ATMSproxy");


    ////////////////////////////////////////////////////////////////////////////////////////////
    // Latitude 		32-bit floating-point,    2307 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "BeamLatitude_MIT");


    // get space id for latitude
    space_id = H5Dget_space( dsetr_id );
    // data type id
    type_id = H5Dget_type( dsetr_id );
    // number of dimensions
    int ndims = H5Sget_simple_extent_ndims(space_id);
    // each dimension size
    hsize_t *dims = new hsize_t[ndims];
    hsize_t *maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int NSCAN = dims[0];
    int NFOV  = dims[1];

    //int NSCAN = 36;
    //int NCHAN = 22;
    //int NFOV  = 96;
    
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Latitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Latitude);

    float lats[NFOV][NSCAN];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
	index = iscan*NFOV + ifov;
	lats[ifov][iscan] = out_Latitude[index];
      }
    }
    
    delete out_Latitude;

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Longitude 		32-bit floating-point,    2307 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "BeamLongitude_MIT");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Longitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Longitude);

    float lons[NFOV][NSCAN];
    
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan*NFOV + ifov;
    	  lons[ifov][iscan] = out_Longitude[index];
      }
    }

    delete out_Longitude;

    ////////////////////////////////////////////////////////////////////////////////////////////////
    //MHS QC Flags 		64-bit floating-point,    2307 x 8
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "MHS_scan_qual_flag_CalProblemCode");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_mhs_flag_CalProblem = new double[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_mhs_flag_CalProblem);

    float mhs_flag[NSCAN][8];
    
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for ( int iflag=0; iflag<8; iflag++ ) {
	index=iscan*8 + iflag;
	mhs_flag[iscan][iflag] = out_mhs_flag_CalProblem[index];
	//cout << out_amsu_flag_CalProblem[iscan] << endl;
      }
    }

    delete out_mhs_flag_CalProblem;

    ////////////////////////////////////////////////////////////////////////////////////////////////
    //AMSU QC Flags 		64-bit floating-point,    770 x 8
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "AMSUA_scan_qual_flag_CalProblemCode");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_amsu_flag_CalProblem = new double[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_amsu_flag_CalProblem);

    float amsu_flag[NSCAN][8];

    for( int iscan=0; iscan<NSCAN/3; iscan++ ) {
      for ( int iflag=0; iflag<8; iflag++ ) {
	//amsu_flag[iscan] = doubleSwap( (char*  ) (&out_amsu_flag_CalProblem[iscan]) );
	index=iscan*8 + iflag;
	for ( int i=0;i<3; i++ ) {
	  index2=iscan*3 + i;
	  amsu_flag[index2][iflag] = out_amsu_flag_CalProblem[index];
	}
      }
    }

    delete out_amsu_flag_CalProblem;
    
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // BrightnessTemperatureFactors		32-bit floating-point,    2307
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "BrightnessTemperatureFactors_MIT");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_BrightnessTemperatureFactors = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_BrightnessTemperatureFactors);

    float BrightnessTemperatureFactors[NSCAN][2];   

    for( int ityp=0; ityp<2; ityp++ ) {
      for( int iscan=0; iscan<NSCAN; iscan++ ) {
	index = ityp*NSCAN + iscan;
	BrightnessTemperatureFactors[iscan][ityp] = out_BrightnessTemperatureFactors[index];
      }
    }

    delete out_BrightnessTemperatureFactors;

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // read antenna temperature: 32 float 2307 x 22 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////

    // Open and get the id for the antenna temperature
    dsetr_id = H5Dopen1(all_data_id, "BrightnessTemperature_MIT");
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    // data type id
    type_id = H5Dget_type( dsetr_id );

    class_id = H5Tget_class( type_id );
    // get storage size
    storage_size = H5Dget_storage_size( dsetr_id );

    unsigned short int* out_BrightnessTemperature_MIT = new unsigned short int[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_UINT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_BrightnessTemperature_MIT);
    int NCHAN = 22;

    float temp[NCHAN][NFOV][NSCAN];
    //float temp[NFOV][NCHAN][NSCAN];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ichan=0; ichan<NCHAN; ichan++ ) {
	for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan*NCHAN*NFOV + ichan*NFOV + ifov;
    	  temp[ichan][ifov][iscan] = out_BrightnessTemperature_MIT[index];
    	}
      }
    }


    delete out_BrightnessTemperature_MIT;
    
   ////////////////////////////////////////////////////////////////////////////////////////////////
    // read antenna temperature: 16-bit unsigned integer 2307 x 22 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    /*
    // Open and get the id for the antenna temperature
    dsetr_id = H5Dopen1(all_data_id, "BrightnessTemperature_MIT");
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    // data type id
    type_id = H5Dget_type( dsetr_id );

    class_id = H5Tget_class( type_id );

    // get storage size
    storage_size = H5Dget_storage_size( dsetr_id );

    unsigned short int* out_BrightnessTemperature_MIT = new unsigned short int[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_UINT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_BrightnessTemperature_MIT);

    int NCHAN = 22;

    float temp[NCHAN][NFOV][NSCAN];
    //float temp[NFOV][NCHAN][NSCAN];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ichan=0; ichan<NCHAN; ichan++ ) {
	for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan*NCHAN*NFOV + ichan*NFOV + ifov;
    	  temp[ichan][ifov][iscan] = out_BrightnessTemperature_MIT[index];
    	}
      }
    }


    delete out_BrightnessTemperature_MIT;
    */

    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	Scan_line_UTC_msec		64-bit float,    2307	
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(all_data_id, "Scan_line_UTC_msec");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_scan_UTC = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_scan_UTC);

    float scan_UTC[NSCAN];
    //int sec_diff = 378691200 ; // time difference in seconds between 1970 and 1958 UTC
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      scan_UTC[iscan] =  out_scan_UTC[iscan];
    }

    delete out_scan_UTC;

    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	Scan_line_doy		64-bit float,    2307	
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(all_data_id, "Scan_line_doy");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_scan_day_of_year = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_scan_day_of_year);

    float scan_day_of_year[NSCAN];
    //int sec_diff = 378691200 ; // time difference in seconds between 1970 and 1958 UTC
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
	scan_day_of_year[iscan] =  out_scan_day_of_year[iscan] ;
    }

    delete out_scan_day_of_year;

    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	Scan_line_year		64-bit float,    2307	
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(all_data_id, "Scan_line_year");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_scan_year = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_scan_year);

    float scan_year[NSCAN];
    //int sec_diff = 378691200 ; // time difference in seconds between 1970 and 1958 UTC
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
	scan_year[iscan] =  out_scan_year[iscan] ;
    	//cout << out_BeamTime[index] << beamTime[ifov][iscan] << endl; 
    }

    delete out_scan_year;

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SatelliteZenithAngle 		32-bit floating-point,    2307
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "SatelliteZenithAngle_MIT");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SatelliteZenithAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SatelliteZenithAngle);

    float satelliteZenithAngle[NSCAN];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
	satelliteZenithAngle[iscan] = out_SatelliteZenithAngle[iscan];
    }

    delete out_SatelliteZenithAngle;

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SolarZenithAngle 		32-bit floating-point,    2307
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "SolarZenithAngle_MIT");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SolarZenithAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SolarZenithAngle);

    float solarZenithAngle[NSCAN];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      solarZenithAngle[iscan] = out_SolarZenithAngle[iscan];
    }

    delete out_SolarZenithAngle;

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // ScanAngle 		64-bit floating-point,    2307
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "ATMS_scan_angles");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_ScanAngle = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_ScanAngle);

    float ScanAngle[NFOV][NSCAN];

    for (int iscan=0; iscan<NSCAN; iscan++) {
      for( int ifov=0; ifov<NFOV; ifov++ ) {
	ScanAngle[ifov][iscan] = out_ScanAngle[ifov];
      }
    }

    delete out_ScanAngle;


    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Output Section
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    int nscan = NSCAN;
    MeasurementTypeHeader  header(nscan,NCHAN,NFOV,NQC);
    
    if( NCHAN == 22 ) { 
	for(int ichan=0; ichan<NCHAN; ichan++) {
	    header.freq[ichan]  = freq_npp[ichan];
	    header.polar[ichan] = polar_npp[ichan];
	}
    }
    else if ( NCHAN == 20 ) {
	for(int ichan=0; ichan<NCHAN; ichan++) {
	    header.freq[ichan]  = freq_n18[ichan];
	    header.polar[ichan] = polar_n18[ichan];
	}
    }
    
    // open file 
    ofstream myFile(file_OUT, ios::out);
    
    
    // header part, so that can be read out by fortran subroutine ReadRadHdrScanLMode_ascii 

    writeMeasurementHdr(myFile, header);
    
    
    // Content part, can be read by fortran subroutine ReadRadMeasScanLMode_ascii
    int qc[NQC];
    for(int iqc=0;iqc<NQC;iqc++) qc[iqc]=1;

    int node=0;
    if ( lats[46][1]  >= lats[46][0] ) 	node = 0;
    else 				node = 1;

    MeasurementType ms(NCHAN,NFOV,NQC);
    
    for(int iscan=0; iscan<nscan; iscan++) {
    	
	if ( iscan >= 1 ) {
	    if ( lats[46][iscan] > lats[46][iscan-1] ) 	node = 0;
	    else 					node = 1;
	}

	ms.node = node;
	
	// date/time information
	//unsigned long secsAfter1958 = (unsigned long)( beamTime[iscan] * 0.000001 ); // micro seconds into seconds
	//unsigned long seconds = secsBefore1958 + secsAfter1958 ;
	
  	//struct tm time_str;
  	//struct tm *tp = &time_str;
	//seconds2tm( seconds, tp );
	
	//ms.year = tp->tm_year + 1900;
	//ms.jday = tp->tm_yday + 1;
	//ms.secs = tp->tm_hour * 3600 + tp->tm_min * 60 + tp->tm_sec; // time in seconds since the start of day

	ms.year = int(scan_year[iscan]);
        ms.jday = int(scan_day_of_year[iscan]);
	ms.secs = int(scan_UTC[iscan]);
	
	//cout << "tp->tm_hour=" << tp->tm_hour << endl;
	//cout << "secsAfter1958=" << secsAfter1958 << endl;
	
	// lat/lon/angle/relAziAngle/solZenAngle
	for(int ifov=0;ifov<NFOV;ifov++) {
	    ms.lat[ifov]         = lats[ifov][iscan];
	    ms.lon[ifov]         = lons[ifov][iscan];
	    ms.angle[ifov]       = ScanAngle[ifov][iscan];
	    ms.relAziAngle[ifov] = -999;
	    ms.solZenAngle[ifov] = solarZenithAngle[iscan];
	}
    
	// tb
	for(int ichan=0;ichan<NCHAN;ichan++) {
	  for(int ifov=0;ifov<NFOV;ifov++) {
	    ms.tb[ifov][ichan] = temp[ichan][ifov][iscan]*BrightnessTemperatureFactors[iscan][0];
	  }
	}

        	

	// qc
	for(int iqc=0; iqc<NQC; iqc++ )
	  ms.qc[iqc] = int(mhs_flag[iscan][2])+int(mhs_flag[iscan][5])+int(mhs_flag[iscan][7]);
	for(int iqc=0; iqc<NQC; iqc++ )
	  ms.qc[iqc] = ms.qc[iqc]+int(amsu_flag[iscan][2])+int(amsu_flag[iscan][5])+int(amsu_flag[iscan][7]);
	// write measurement content
	writeMeasurement( myFile, ms );	  
	
    }

    myFile.close();
    
  } // end loop around files
  
  return 0;
  
}
