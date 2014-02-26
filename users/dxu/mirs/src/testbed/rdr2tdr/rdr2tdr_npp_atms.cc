/***************************************************************************************************
 * 
 * This is the C++ decoder for NPP ATMS data. 4 subrotines to handle different input data types.
 *
 * Author: Wanchun Chen <Wanchun.Chen@noaa.gov>
 *
 * Date: 03/11/2010 
 *
 ***************************************************************************************************/

#include <iostream>
#include <fstream>
#include <memory>
#include <new>
#include <iomanip>
#include <cmath>
#include <ctime>
#include <string>
#include <cstring>
#include <stdlib.h>

#include "hdf5.h"

using namespace std;



////////////////////////////////////////////////////////////////////////////////////////////
//
// some global variables
// 
////////////////////////////////////////////////////////////////////////////////////////////

float freq_npp[22] = { 23.800, 31.400, 50.300, 51.760, 52.800, 53.596, 54.400, 54.940, 55.500, 57.290,
      57.290, 57.290, 57.290, 57.290, 57.290, 88.200, 165.500, 183.310, 183.310, 183.310,183.310, 183.310 } ;

int polar_npp[22] = { 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3 };
       
float freq_n18[20] = { 23.800, 31.400, 50.300, 52.799, 53.595, 54.400, 54.941, 55.499, 57.290, 57.290,
      57.290, 57.290, 57.290, 57.290, 89.000, 89.000, 157.000, 183.311, 183.311, 190.311 } ;

int polar_n18[20] = { 2, 2, 2, 2, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 3, 3, 2 } ;

const int NQC = 4;

const int FILESIZE = 256;
const int FILENUM  = 8192;


/**
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
  time_t t1900 = mktime(&time1900);
 
  struct tm time1958;  		// 1958
  time1958.tm_sec   = 0;
  time1958.tm_min   = 0;
  time1958.tm_hour  = 0;
  time1958.tm_mday  = 1;
  time1958.tm_mon   = 0;
  time1958.tm_year  = 58;
  time1958.tm_yday  = 0;
  time1958.tm_isdst = 0;
  time_t t1958 = mktime(&time1958);

  // seconds of 1958 since 1900
  unsigned long secsBefore1958 = (unsigned long)(difftime( t1958, t1900 ));
*/

unsigned long secsBefore1958 = 1830297600;

// before 2012-06-30	23:59:60, LEAP_SECONDS=34
// after  2012-06-30	23:59:60, LEAP_SECONDS=35
const int LEAP_SECONDS = 35;

class MeasurementTypeHeader {
public: 
	int nscan;
	int nchan;
	int nfov;
	int nqc;
	int nPosScan;
	float* freq; 
        int*  polar;
	
	// constructor
	MeasurementTypeHeader(int nscan_arg, int nchan_arg, int nfov_arg, int nqc_arg ) {
		nscan = nscan_arg;
		nchan = nchan_arg;
		nfov  = nfov_arg;
		nqc   = nqc_arg;
		freq  = new float[nchan];
		polar = new int[nchan];
	};
	
	// copy constructor
	MeasurementTypeHeader(const MeasurementTypeHeader& mh) {
		
		nscan = mh.nscan;
		nchan = mh.nchan;
		nfov  = mh.nfov;
		nqc   = mh.nqc;
		freq  = new float[nchan];
		polar = new int[nchan];
	
	}

	// destructor
	~MeasurementTypeHeader() {
		delete [] freq;
		delete [] polar;
		freq  = NULL;
		polar = NULL;
	};
	
};



class MeasurementType {
public: 
	int 		nchan	     ;
	int 		nfov         ;
	int 		nqc          ;
	
     	int		node	     ;
     	int		jday         ;
     	int		year	     ;
     	int		secs	     ;

     	float*		lat	     ;
     	float*		lon	     ;
     	float* 		angle        ;
     	float*		relAziAngle  ;
     	float*		solZenAngle  ;
     	float**		tb           ;
     	int* 		qc           ;
        
	// constructor
     	MeasurementType(int nch, int nfv, int nc) {
		
		nchan = nch;
		nfov  = nfv;
		nqc   = nc;
		
		lat = new float[nfov];     	
		lon = new float[nfov];     	
		angle = new float[nfov];   
		relAziAngle = new float[nfov];   
		solZenAngle = new float[nfov];
     		qc = new int[nqc];
		
		tb =  new float *[nfov];
		for(int i = 0; i<nfov; i++ ) tb[i] = new float[nchan];
     	};
	
	// copy constructor
     	MeasurementType(const MeasurementType& ms) {
		
		nchan = ms.nchan;
		nfov  = ms.nfov;
		nqc   = ms.nqc;
		
		lat = new float[nfov];     	
		lon = new float[nfov];     	
		angle = new float[nfov];   
		relAziAngle = new float[nfov];   
		solZenAngle = new float[nfov];
     		qc = new int[nqc];
		
		tb = new float *[nfov];
		for(int i = 0; i<nfov; i++ ) tb[i] = new float[nchan];
		
     	};
	
	// destructor
	~MeasurementType() {
		
		delete [] lat;
		delete [] lon;
		delete [] angle;
		delete [] relAziAngle;
		delete [] solZenAngle;
		delete [] qc;
		
		lat         = NULL;
		lon         = NULL;
		angle       = NULL;
		relAziAngle = NULL;
		solZenAngle = NULL;
		qc          = NULL;
		
		for(int i = 0; i<nfov; i++ ) 
		{ 
		  delete [] tb[i]; 
		  tb[i] = NULL ; 
		}
		  
		delete [] tb;
		tb = NULL;
	
	};
     
};


int writeMeasurementHdr(ofstream &myFile, MeasurementTypeHeader& header) {

    myFile.width(4); myFile << header.nscan ;
    myFile.width(4); myFile << header.nfov;
    myFile.width(4); myFile << header.nqc ;
    myFile.width(4); myFile << header.nchan << '\n';
    
    for( int ichan=0; ichan<header.nchan; ichan++ )
    {
       myFile << setprecision(5) << setw(10) << right << fixed << header.freq[ichan] ;
       if ( (((ichan+1) % 10) == 0) && ((ichan+1) < header.nchan) ) myFile << endl;
    }
    myFile << endl;
    
    for( int ichan=0; ichan<header.nchan; ichan++ )
    {
       myFile << setw(3) << header.polar[ichan] ;
       if ( (((ichan+1) % 20) == 0) && ((ichan+1) < header.nchan) ) myFile << endl;
    }
    myFile << endl;
    
    return 0;

}



int writeMeasurement(ofstream &myFile, MeasurementType& measurement) {
    
    int NFOV  = measurement.nfov;
    int NCHAN = measurement.nchan;
    int NQC   = measurement.nqc;

    myFile.width(4);  myFile << measurement.node ;
    myFile.width(10); myFile << measurement.jday;
    myFile.width(10); myFile << measurement.year;
    myFile.width(10); myFile << measurement.secs << '\n';

    // lat
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.lat[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;

    // lon
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.lon[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;

    // angle - Sensor Zenith Angle
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.angle[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;
    
    // relAziAngle
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.relAziAngle[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;
   
    // solZenAngle
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.solZenAngle[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;

    
    // !!!!  C/C++ is row major, while fortran/IDL is column major !!!!
    // !!!!  We write in Fortran/IDL order : row(ifov) changes faster than column(ichan)
    
    // tb[nfov][nchan]
    for(int ichan=0; ichan<NCHAN; ichan++) {
    for(int ifov=0;  ifov<NFOV; ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.tb[ifov][ichan];
	
	// C/C++ order in memory
        // int index = ichan + ifov * NCHAN ;   
	
	// Fortran/IDL order in memory
	int index = ichan * NFOV + ifov;

    	if ( (((index+1) % 10) == 0) && ( (index+1) < (NCHAN*NFOV) ) ) myFile << endl;
    }
    }
    myFile << endl;

    // qc 
    if ( NQC > 0 ) {
      for(int iqc=0; iqc<NQC; iqc++ ) {
    	myFile.width(4); myFile << measurement.qc[iqc];
    	if ( (((iqc+1) % 10) == 0) && ((iqc+1) < NQC) ) myFile << endl;
      }
    }
    myFile << endl;

    return 0;
}



/**  struct tm 
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

}





////////////////////////////////////////////////////////////////////////////////////////////
//
// 4 subroutines to read raw data
// 
////////////////////////////////////////////////////////////////////////////////////////////



/***************************************************************************************************
 *
 * The subroutine to read TDR data
 *
 ***************************************************************************************************/

int rdr2tdr_npp_atms_TDR(char *file_RDR,char* file_GEO,char *file_OUT) {

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    int index;
  
    ////////////////////////////////////////////////////////////////////////////////////////////
    //
    // GATMO file ( contains meta data, lat/lon/zenith angle/time, etc )
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////
    hid_t file_id     = H5Fopen(file_GEO, H5F_ACC_RDONLY,  H5P_DEFAULT);
    hid_t root_id     = H5Gopen1(file_id, "/");
    hid_t all_data_id = H5Gopen1(root_id, "All_Data");
    hid_t gran_id     = H5Gopen1(all_data_id, "ATMS-SDR-GEO_All");
    

    ////////////////////////////////////////////////////////////////////////////////////////////
    // Latitude 		32-bit floating-point,    12 row-scan x 96 column-nfov
    ////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "Latitude");
   
    // get space id for antenna temperature
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

    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Latitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Latitude);

    // row major array,
    // offset = row*NUMCOLS + column
    float lats[NSCAN][NFOV];
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
	index = iscan * NFOV + ifov;
	lats[iscan][ifov] = out_Latitude[index];
      }
    }

    delete [] out_Latitude;
    out_Latitude = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Longitude 		32-bit floating-point,    12(scan) x 96(fov)
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "Longitude");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Longitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Longitude);

    float lons[NSCAN][NFOV];
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
	index = iscan * NFOV + ifov;
	lons[iscan][ifov] = out_Longitude[index];
      }
    }

    delete [] out_Longitude;
    out_Longitude = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    //	ScanMidTime	64-bit integer,    12 scan
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "MidTime");
    storage_size = H5Dget_storage_size( dsetr_id );
    long long *out_ScanMidTime = new long long[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_ScanMidTime);
    
    long long scanMidTime[NSCAN];
    for( int iscan=0; iscan<NSCAN; iscan++ )
	scanMidTime[iscan] = out_ScanMidTime[iscan];
    
    delete [] out_ScanMidTime;
    out_ScanMidTime = NULL;
    H5Dclose(dsetr_id);
    
	
    ////////////////////////////////////////////////////////////////////////////////////////////////
    //	ScanStartTime	64-bit integer,    12 scan
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "StartTime");
    storage_size = H5Dget_storage_size( dsetr_id );
    long long *out_ScanStartTime = new long long[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_ScanStartTime);

    long long scanStartTime[NSCAN];
    for( int iscan=0; iscan<NSCAN; iscan++ )
	scanStartTime[iscan] = out_ScanStartTime[iscan] ;
    
    delete [] out_ScanStartTime;
    out_ScanStartTime = NULL;
    H5Dclose(dsetr_id);

	
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SensorAzimuthAngle 		32-bit floating-point,    12 scan x 96 fov
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SatelliteAzimuthAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SensorAzimuthAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SensorAzimuthAngle);

    float sensorAzimuthAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  sensorAzimuthAngle[iscan][ifov] = out_SensorAzimuthAngle[index];
      }
    }

    delete [] out_SensorAzimuthAngle;
    out_SensorAzimuthAngle = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SensorZenithAngle 		32-bit floating-point,    12 scan x 96 fov
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SatelliteZenithAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SensorZenithAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SensorZenithAngle);

    float sensorZenithAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  sensorZenithAngle[iscan][ifov] = out_SensorZenithAngle[index];
      }
    }

    delete [] out_SensorZenithAngle;
    out_SensorZenithAngle = NULL;
    H5Dclose(dsetr_id);
    

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SolarAzimuthAngle 		32-bit floating-point,    36 scan x 96 fov
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SolarAzimuthAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SolarAzimuthAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SolarAzimuthAngle);
    
    float solarAzimuthAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  solarAzimuthAngle[iscan][ifov] = out_SolarAzimuthAngle[index];
      }
    }

    delete [] out_SolarAzimuthAngle;
    out_SolarAzimuthAngle = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SolarZenithAngle 		32-bit floating-point,    36 scan x 96 fov
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SolarZenithAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SolarZenithAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SolarZenithAngle);

    float solarZenithAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  solarZenithAngle[iscan][ifov] = out_SolarZenithAngle[index];
      }
    }

    delete [] out_SolarZenithAngle;
    out_SolarZenithAngle = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    //	QF1_ATMSSDRGEO	8-bit unsigned character,    12 scan
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "QF1_ATMSSDRGEO");
    storage_size = H5Dget_storage_size( dsetr_id );
    unsigned char *out_QF1_ATMSSDRGEO = new unsigned char[storage_size];
    status = H5Dread(dsetr_id, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_QF1_ATMSSDRGEO);

    unsigned short QF1_ATMSSDRGEO[NSCAN];
    for( int iscan=0; iscan<NSCAN; iscan++ )
	QF1_ATMSSDRGEO[iscan] = out_QF1_ATMSSDRGEO[iscan] ;
    
    delete [] out_QF1_ATMSSDRGEO;
    out_QF1_ATMSSDRGEO = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Close GEO group and file
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    H5Gclose( gran_id ) ;
    H5Gclose( all_data_id ) ;
    H5Gclose( root_id ) ;
    H5Fclose( file_id ) ;
    

   
    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Open TATMS file ( contains antenna temperature )
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    file_id	= H5Fopen(file_RDR, H5F_ACC_RDONLY, H5P_DEFAULT);
    root_id	= H5Gopen1(file_id, "/");
    all_data_id = H5Gopen1(root_id, "All_Data");
    gran_id	= H5Gopen1(all_data_id, "ATMS-TDR_All");

    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	AntennaTemperature	16-bit unsigned integer,    12 scan x 96 fov x 22 channel
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    // Open and get the id for the antenna temperature
    dsetr_id = H5Dopen1(gran_id, "AntennaTemperature");
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    // data type id
    type_id = H5Dget_type( dsetr_id );
    
    // number of dimensions
    ndims = H5Sget_simple_extent_ndims(space_id);
    hsize_t *dims_tb = new hsize_t[ndims];
    hsize_t *maxdims_tb = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims_tb, maxdims_tb );
    
    //for( int i=0; i<ndims; i++ ) cout << dims_tb[i] << endl;
    //NSCAN = dims_tb[0];
    //NFOV = dims_tb[1]; 
    int NCHAN = dims_tb[2]; 

    /*
    Valid class identifiers, as defined in H5Tpublic.h, are:

    * H5T_INTEGER
    * H5T_FLOAT
    * H5T_TIME
    * H5T_STRING
    * H5T_BITFIELD
    * H5T_OPAQUE
    * H5T_COMPOUND
    * H5T_REFERENCE
    * H5T_ENUM
    * H5T_VLEN
    * H5T_ARRAY
    *
    */    

    class_id = H5Tget_class( type_id );

    // get storage size
    storage_size = H5Dget_storage_size( dsetr_id );

    // read antenna temperature: 16-bit unsigned integer
    unsigned short int* out_AntennaTemperature = new unsigned short int[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_UINT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_AntennaTemperature);

    float scale = 330.0/( pow(2.0,16) - 9 );
    float temp[NSCAN][NFOV][NCHAN];
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
    for(int ifov=0;  ifov<NFOV;   ifov++  ) {
    for(int ichan=0; ichan<NCHAN; ichan++ ) {
        temp[iscan][ifov][ichan] = -999.0;
    }
    }
    }

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
    for(int ifov=0;  ifov<NFOV;   ifov++  ) {
    for(int ichan=0; ichan<NCHAN; ichan++ ) {
        index = iscan * NFOV * NCHAN + ifov * NCHAN + ichan;
        if (out_AntennaTemperature[index] < 65528)
          temp[iscan][ifov][ichan] = out_AntennaTemperature[index] * scale;
    }
    }
    }

    delete [] out_AntennaTemperature;
    out_AntennaTemperature = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	BeamTime		64-bit integer,    12 scan x 96	fov
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(gran_id, "BeamTime");
    storage_size = H5Dget_storage_size( dsetr_id );
    long long* out_BeamTime = new long long[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_BeamTime);

    long long beamTime[NSCAN][NFOV];
    //int sec_diff = 378691200 ; // time difference in seconds between 1970 and 1958 UTC

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
        index = iscan * NFOV + ifov;
	beamTime[iscan][ifov] =  out_BeamTime[index] ;
      }
    }

    delete [] out_BeamTime;
    out_BeamTime = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	QF19_SCAN_ATMSSDR		8-bit  unsigned character,    12 scan
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(gran_id, "QF19_SCAN_ATMSSDR");
    storage_size = H5Dget_storage_size( dsetr_id );
    unsigned char* out_QF19 = new unsigned char[storage_size];
    status = H5Dread(dsetr_id, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_QF19);
    int QF19[NSCAN];

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      QF19[iscan] = out_QF19[iscan];
    }

    delete [] out_QF19;
    out_QF19 = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // free up resource to avoid memory leak
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////////
    
    delete [] dims;
    delete [] maxdims;
    delete [] dims_tb;
    delete [] maxdims_tb;
    
    dims = NULL;
    maxdims = NULL;
    dims_tb = NULL;
    maxdims_tb = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Close TATMS group and file
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    H5Gclose( gran_id ) ;
    H5Gclose( all_data_id ) ;
    H5Gclose( root_id ) ;
    H5Fclose( file_id ) ;
    

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
    if( !myFile ) {
    	cerr << "ERROR: " << file_OUT << endl;
	exit(1);
    }
    
    // header part, so that can be read out by fortran subroutine ReadRadHdrScanLMode_ascii 
    writeMeasurementHdr(myFile, header);

    
    // Content part, can be read by fortran subroutine ReadRadMeasScanLMode_ascii

    int node=0;
    if ( lats[1][0]  >= lats[0][0] ) 	node = 0;
    else 				node = 1;
    
    MeasurementType ms(NCHAN,NFOV,NQC);

    for(int iscan=0; iscan<nscan; iscan++) {
    	
        // Default QC flags to zero
        for(int iqc=0; iqc<NQC; iqc++) {
	  ms.qc[iqc]=0;
	}

	// Define the node (ascending/descending)
	if ( iscan >= 1 ) {
	    if ( lats[iscan][0] > lats[iscan-1][0] ) 	node = 0;
	    else 					node = 1;
	}
    	
	ms.node = node;
	
	// date/time information
	unsigned long secsAfter1958 = (unsigned long)( scanStartTime[iscan] * 0.000001 ); // micro seconds into seconds
	//unsigned long seconds = secsBefore1958 + secsAfter1958 - LEAP_SECONDS;
	unsigned long seconds = 0L;
	if( secsAfter1958 < 1719792035 ) { // 1719792035 ---> 2012/07/01 00:00:00
	    seconds = secsBefore1958 + secsAfter1958 - 34;  
	}
	else {
	    seconds = secsBefore1958 + secsAfter1958 - 35;  
	}
	// more branches are needed in the future when leap second gets adjusted again.
	
  	struct tm time_str;
  	struct tm *tp = &time_str;
	seconds2tm( seconds, tp );
	
	ms.year = tp->tm_year + 1900;
	ms.jday = tp->tm_yday + 1;
	ms.secs = ( tp->tm_hour * 3600 + tp->tm_min * 60 + tp->tm_sec ) * 1000 ; // time in milli-seconds since the start of day
	
	// lat/lon/angle/relAziAngle/solZenAngle
	for(int ifov=0; ifov<NFOV; ifov++) {
	    ms.lat[ifov]         = lats[iscan][ifov];
	    ms.lon[ifov]         = lons[iscan][ifov];
	    ms.angle[ifov]       = sensorZenithAngle[iscan][ifov];
            if( ifov <= 47 && ms.angle[ifov] > 0 ) ms.angle[ifov] = -1.0 * ms.angle[ifov];  // 1-48 to negative values
	    ms.relAziAngle[ifov] = sensorAzimuthAngle[iscan][ifov];
	    ms.solZenAngle[ifov] = solarZenithAngle[iscan][ifov];
	}
	
	// tb
	for(int ifov=0;ifov<NFOV;ifov++) {
	  for(int ichan=0;ichan<NCHAN;ichan++) {
	    ms.tb[ifov][ichan] = temp[iscan][ifov][ichan];
	  }
	}
	
	// qc
	if( QF19[iscan] > 0 ) ms.qc[0] = 1;
        
	if( QF1_ATMSSDRGEO[iscan] != 0 ) ms.qc[0] = 1;
	
	
	// write measurement content of this scan
	writeMeasurement( myFile, ms );	  
	
    }

    myFile.close();
  
    return 0;
}




/***************************************************************************************************
 *
 * The subroutine to read MIT SDR proxy data
 *
 ***************************************************************************************************/

int rdr2tdr_npp_atms_SDR_proxy(char *file_RDR, char *file_OUT) {

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    int index;
    int index2;

    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // TATMS file ( contains antenna temperature )
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    hid_t file_id	= H5Fopen(file_RDR, H5F_ACC_RDONLY, H5P_DEFAULT);
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

    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Latitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Latitude);

    float lats[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
	index = iscan * NFOV + ifov;
	lats[iscan][ifov] = out_Latitude[index];
      }
    }
    
    delete [] out_Latitude;
    out_Latitude = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Longitude 		32-bit floating-point,    2307 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "BeamLongitude_MIT");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Longitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Longitude);

    float lons[NSCAN][NFOV];
    
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  lons[iscan][ifov] = out_Longitude[index];
      }
    }

    delete [] out_Longitude;
    out_Longitude = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////
    //MHS QC Flags 		64-bit floating-point,    2307 x 8
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "MHS_scan_qual_flag_CalProblemCode");
    
    space_id = H5Dget_space( dsetr_id );
    
    // we already knew it is 2-D
    // ndims = H5Sget_simple_extent_ndims(space_id);
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int NFLAG = dims[1];

//cout << "NFLAG=" << NFLAG << endl;
    
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_mhs_flag_CalProblem = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_mhs_flag_CalProblem);

    float mhs_flag[NSCAN][NFLAG];
    
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for ( int iflag=0; iflag<NFLAG; iflag++ ) {
	index = iscan * NFLAG + iflag;
	mhs_flag[iscan][iflag] = out_mhs_flag_CalProblem[index];
	//cout << out_mhs_flag_CalProblem[iscan] << endl;
      }
    }

    delete [] out_mhs_flag_CalProblem;
    out_mhs_flag_CalProblem = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////
    //AMSU QC Flags 		64-bit floating-point,    770 x 8
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "AMSUA_scan_qual_flag_CalProblemCode");
    space_id = H5Dget_space( dsetr_id );
    
    // we already knew it is 2-D
    // ndims = H5Sget_simple_extent_ndims(space_id);
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    // int NSCAN_AMSUA = dims[0];

//cout <<  "NSCAN_AMSUA=" << NSCAN_AMSUA << endl;
    
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_amsu_flag_CalProblem = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_amsu_flag_CalProblem);

/*
    float amsu_flag[NSCAN_AMSUA][NFLAG];

    for( int iscan=0; iscan<NSCAN_AMSUA; iscan++ ) {
      for( int iflag=0; iflag<NFLAG; iflag++ ) {
	 index = iscan * NFLAG + iflag;
	 amsu_flag[iscan][iflag] = out_amsu_flag_CalProblem[index];
      }
    }
*/


    float amsu_flag[NSCAN][NFLAG];

    for( int iscan=0; iscan<NSCAN/3; iscan++ ) {
      for ( int iflag=0; iflag<NFLAG; iflag++ ) {
	index=iscan * NFLAG + iflag;
	for ( int i=0;i<3; i++ ) {
	  index2=iscan*3 + i;
	  amsu_flag[index2][iflag] = out_amsu_flag_CalProblem[index];
	}
      }
    }


    delete [] out_amsu_flag_CalProblem;
    out_amsu_flag_CalProblem = NULL;

    
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // BrightnessTemperatureFactors		32-bit floating-point,   2 X   2307 scans
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "BrightnessTemperatureFactors_MIT");
    space_id = H5Dget_space( dsetr_id );
    
    // we already knew it is 2-D
    // ndims = H5Sget_simple_extent_ndims(space_id);
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int NTYP = dims[0];

//cout << "NTYP=" << NTYP << endl;
    
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_BrightnessTemperatureFactors = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_BrightnessTemperatureFactors);

    float BrightnessTemperatureFactors[NTYP][NSCAN];   

    for( int ityp=0; ityp<NTYP; ityp++ ) {
      for( int iscan=0; iscan<NSCAN; iscan++ ) {
	index = ityp * NSCAN + iscan;
	BrightnessTemperatureFactors[ityp][iscan] = out_BrightnessTemperatureFactors[index];
      }
    }

    delete [] out_BrightnessTemperatureFactors;
    out_BrightnessTemperatureFactors = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // read antenna temperature: 16-bit unsigned integer, 2307 Scan x 22 Chan x 96 FOV
    ////////////////////////////////////////////////////////////////////////////////////////////////

    // Open and get the id for the antenna temperature
    dsetr_id = H5Dopen1(all_data_id, "BrightnessTemperature_MIT");
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    // number of dimensions
    ndims = H5Sget_simple_extent_ndims(space_id);
    //cout << "ndims=" << ndims << endl;
   
    // each dimension size
    hsize_t *dims_tb = new hsize_t[ndims];
    hsize_t *maxdims_tb = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims_tb, maxdims_tb );
    
    // for( int i=0; i<ndims; i++ ) cout << dims_tb[i] << endl;
    // NSCAN = dims_tb[0];
    int NCHAN = dims_tb[1]; 
    // NFOV = dims_tb[2]; 
    
    // data type id
    type_id = H5Dget_type( dsetr_id );

    class_id = H5Tget_class( type_id );
    
    // get storage size
    storage_size = H5Dget_storage_size( dsetr_id );
    //cout << "storage_size=" << storage_size << endl;
    
    unsigned short int* out_BrightnessTemperature_MIT = new unsigned short int[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_UINT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_BrightnessTemperature_MIT);

    float temp[NSCAN][NCHAN][NFOV];

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
    for(int ichan=0; ichan<NCHAN; ichan++ ) {
    for(int ifov=0;  ifov<NFOV;   ifov++  ) {
        index = iscan * NCHAN * NFOV + ichan * NFOV + ifov;
        temp[iscan][ichan][ifov] = out_BrightnessTemperature_MIT[index];
    }
    }
    }

/*  
    for(int ifov=0; ifov<NFOV; ifov++ )     {
    for(int ichan=0; ichan<NCHAN; ichan++ ) {
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
        index = ifov * NCHAN * NSCAN + ichan * NSCAN + iscan;
        temp[iscan][ichan][ifov] = out_BrightnessTemperature_MIT[index];
    }
    }
    }
*/

    delete [] out_BrightnessTemperature_MIT;
    out_BrightnessTemperature_MIT = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	Scan_line_UTC_msec		64-bit float,    2307  scan
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(all_data_id, "Scan_line_UTC_msec");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_scan_UTC = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_scan_UTC);

    float scan_UTC[NSCAN];
    //int sec_diff = 378691200 ; // time difference in seconds between 1970 and 1958 UTC
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      scan_UTC[iscan] = out_scan_UTC[iscan];
    }

    delete [] out_scan_UTC;
    out_scan_UTC = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	Scan_line_doy		64-bit float,    2307  scan	
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(all_data_id, "Scan_line_doy");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_scan_day_of_year = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_scan_day_of_year);

    float scan_day_of_year[NSCAN];
    //int sec_diff = 378691200 ; // time difference in seconds between 1970 and 1958 UTC
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
	scan_day_of_year[iscan] = out_scan_day_of_year[iscan] ;
    }

    delete [] out_scan_day_of_year;
    out_scan_day_of_year = NULL;
    

    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	Scan_line_year		64-bit float,    2307 scan	
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(all_data_id, "Scan_line_year");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_scan_year = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_scan_year);

    float scan_year[NSCAN];
    //int sec_diff = 378691200 ; // time difference in seconds between 1970 and 1958 UTC
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
	scan_year[iscan] = out_scan_year[iscan] ;
    	//cout << out_BeamTime[index] << beamTime[iscan][ifov] << endl; 
    }

    delete [] out_scan_year;
    out_scan_year = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SatelliteZenithAngle 		64-bit floating-point,    2307 Scan X 96 FOV
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "SatelliteZenithAngle_MIT");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_SatelliteZenithAngle = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SatelliteZenithAngle);

    float satelliteZenithAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
        index = iscan * NFOV + ifov;
	satelliteZenithAngle[iscan][ifov] = out_SatelliteZenithAngle[index];
      }
    }

    delete [] out_SatelliteZenithAngle;
    out_SatelliteZenithAngle = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SolarZenithAngle 		32-bit floating-point,    2307 Scan X 96 FOV
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "SolarZenithAngle_MIT");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_SolarZenithAngle = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SolarZenithAngle);

    float solarZenithAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
        index = iscan * NFOV + ifov;
        solarZenithAngle[iscan][ifov] = out_SolarZenithAngle[index];
      }
    }
    delete [] out_SolarZenithAngle;
    out_SolarZenithAngle = NULL;
    

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // ScanAngle 		64-bit floating-point,   96 FOV
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(all_data_id, "ATMS_scan_angles");
    storage_size = H5Dget_storage_size( dsetr_id );
    double *out_ScanAngle = new double[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_ScanAngle);

    float ScanAngle[NSCAN][NFOV];

    for (int iscan=0; iscan<NSCAN; iscan++) {
      for( int ifov=0; ifov<NFOV; ifov++ ) {
	ScanAngle[iscan][ifov] = out_ScanAngle[ifov];
      }
    }

    delete [] out_ScanAngle;
    out_ScanAngle = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // free up resource to avoid memory leak 
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////////
    
    delete [] dims;
    delete [] maxdims;
    delete [] dims_tb;
    delete [] maxdims_tb;
    
    dims = NULL;
    maxdims = NULL;
    dims_tb = NULL;
    maxdims_tb = NULL;



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
    if ( lats[1][46]  >= lats[0][46] ) 	node = 0;
    else 				node = 1;
    
    MeasurementType ms(NCHAN,NFOV,NQC);
    
    for(int iscan=0; iscan<nscan; iscan++) {
    	
	if ( iscan >= 1 ) {
	    if ( lats[iscan][46] > lats[iscan-1][46] ) 	node = 0;
	    else 					node = 1;
	}
    	
	ms.node = node;
	
	ms.year = int(scan_year[iscan]);
        ms.jday = int(scan_day_of_year[iscan]);
	ms.secs = int(scan_UTC[iscan]);
	
	// lat/lon/angle/relAziAngle/solZenAngle
	for(int ifov=0;ifov<NFOV;ifov++) {
	    ms.lat[ifov]         = lats[iscan][ifov];
	    ms.lon[ifov]         = lons[iscan][ifov];
	    ms.angle[ifov]       = ScanAngle[iscan][ifov];
	    ms.relAziAngle[ifov] = -999;
	    ms.solZenAngle[ifov] = solarZenithAngle[iscan][ifov];
	}
    
	// tb
	for(int ichan=0;ichan<NCHAN;ichan++) {
	  for(int ifov=0;ifov<NFOV;ifov++) {
	    ms.tb[ifov][ichan] = temp[iscan][ichan][ifov] * BrightnessTemperatureFactors[0][iscan];
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
 
    return 0;
}    




/***************************************************************************************************
 *
 * The subroutine to read SDR data
 *
 ***************************************************************************************************/

int rdr2tdr_npp_atms_SDR(char *file_RDR,char* file_GEO,char *file_OUT,char *file_NEDT,char *instrFile) {

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    int index;

   
    ////////////////////////////////////////////////////////////////////////////////////////////
    //
    // Open GEO file ( contains meta data, lat/lon/zenith angle/time, etc )
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////
    hid_t file_id     = H5Fopen(file_GEO, H5F_ACC_RDONLY,  H5P_DEFAULT);
    hid_t root_id     = H5Gopen1(file_id, "/");
    hid_t all_data_id = H5Gopen1(root_id, "All_Data");
    hid_t gran_id     = H5Gopen1(all_data_id, "ATMS-SDR-GEO_All");

    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // Latitude 		32-bit floating-point,    12 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "Latitude");
   
    // get space id for antenna temperature
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
    
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Latitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Latitude);

    float lats[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
	index = iscan * NFOV + ifov;
	lats[iscan][ifov] = out_Latitude[index];
      }
    }

    delete [] out_Latitude;
    out_Latitude = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Longitude 		32-bit floating-point,    12 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "Longitude");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Longitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Longitude);

    float lons[NSCAN][NFOV];
    
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  lons[iscan][ifov] = out_Longitude[index];
      }
    }

    delete [] out_Longitude;
    out_Longitude = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    //	ScanMidTime	64-bit integer,    12
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "MidTime");
    storage_size = H5Dget_storage_size( dsetr_id );
    long long *out_ScanMidTime = new long long[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_ScanMidTime);
    
    long long scanMidTime[NSCAN];
    for( int iscan=0; iscan<NSCAN; iscan++ )
	scanMidTime[iscan] = out_ScanMidTime[iscan];
    
    delete [] out_ScanMidTime;
    out_ScanMidTime = NULL;
    H5Dclose(dsetr_id);

	
    ////////////////////////////////////////////////////////////////////////////////////////////////
    //	ScanStartTime	64-bit integer,    12
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "StartTime");
    storage_size = H5Dget_storage_size( dsetr_id );
    long long *out_ScanStartTime = new long long[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_ScanStartTime);

    long long scanStartTime[NSCAN];
    for( int iscan=0; iscan<NSCAN; iscan++ )
	scanStartTime[iscan] = out_ScanStartTime[iscan] ;
    
    delete [] out_ScanStartTime;
    out_ScanStartTime = NULL;
    H5Dclose(dsetr_id);
    
	
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SensorAzimuthAngle 		32-bit floating-point,    12 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SatelliteAzimuthAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SensorAzimuthAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SensorAzimuthAngle);

    float sensorAzimuthAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  sensorAzimuthAngle[iscan][ifov] = out_SensorAzimuthAngle[index];
      }
    }

    delete [] out_SensorAzimuthAngle;
    out_SensorAzimuthAngle = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SensorZenithAngle 		32-bit floating-point,    12 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SatelliteZenithAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SensorZenithAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SensorZenithAngle);

    float sensorZenithAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  sensorZenithAngle[iscan][ifov] = out_SensorZenithAngle[index];
      }
    }

    delete [] out_SensorZenithAngle;
    out_SensorZenithAngle = NULL;
    H5Dclose(dsetr_id);

    
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SolarAzimuthAngle 		32-bit floating-point,    12 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SolarAzimuthAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SolarAzimuthAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SolarAzimuthAngle);
    
    float solarAzimuthAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  solarAzimuthAngle[iscan][ifov] = out_SolarAzimuthAngle[index];
      }
    }

    delete [] out_SolarAzimuthAngle;
    out_SolarAzimuthAngle = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SolarZenithAngle 		32-bit floating-point,    36 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SolarZenithAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SolarZenithAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SolarZenithAngle);

    float solarZenithAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  solarZenithAngle[iscan][ifov] = out_SolarZenithAngle[index];
      }
    }

    delete [] out_SolarZenithAngle;
    out_SolarZenithAngle = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    //	QF1_ATMSSDRGEO	8-bit unsigned character,    12 scan
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "QF1_ATMSSDRGEO");
    storage_size = H5Dget_storage_size( dsetr_id );
    unsigned char *out_QF1_ATMSSDRGEO = new unsigned char[storage_size];
    status = H5Dread(dsetr_id, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_QF1_ATMSSDRGEO);

    unsigned short QF1_ATMSSDRGEO[NSCAN];
    for( int iscan=0; iscan<NSCAN; iscan++ )
	QF1_ATMSSDRGEO[iscan] = out_QF1_ATMSSDRGEO[iscan] ;
    
    delete [] out_QF1_ATMSSDRGEO;
    out_QF1_ATMSSDRGEO = NULL;
    H5Dclose(dsetr_id);



    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Close GEO group and file
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    H5Gclose( gran_id ) ;
    H5Gclose( all_data_id ) ;
    H5Gclose( root_id ) ;
    H5Fclose( file_id ) ;
    


    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Open SDR file ( contains antenna temperature )
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    file_id	= H5Fopen(file_RDR, H5F_ACC_RDONLY, H5P_DEFAULT);
    root_id	= H5Gopen1(file_id, "/");
    all_data_id = H5Gopen1(root_id, "All_Data");
    gran_id	= H5Gopen1(all_data_id, "ATMS-SDR_All");

    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	AntennaTemperature	16-bit unsigned integer,    12 x 96 x 22
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    // Open and get the id for the antenna temperature
    dsetr_id = H5Dopen1(gran_id, "BrightnessTemperature");
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    // data type id
    type_id = H5Dget_type( dsetr_id );

    int ndims_tb = H5Sget_simple_extent_ndims(space_id);
    hsize_t *dims_tb = new hsize_t[ndims_tb];
    hsize_t *maxdims_tb = new hsize_t[ndims_tb];
    status = H5Sget_simple_extent_dims(space_id, dims_tb, maxdims_tb );
    
    //for(int i = 0 ; i < ndims_tb ; i++ ) cout << dims_tb[i] << endl;
    
    //int NSCAN = dims_tb[0];
    //int NFOV  = dims_tb[1];
    int NCHAN = dims_tb[2];


    /*
     * Valid class identifiers, as defined in H5Tpublic.h, are:
     *
     * H5T_INTEGER
     * H5T_FLOAT
     * H5T_TIME
     * H5T_STRING
     * H5T_BITFIELD
     * H5T_OPAQUE
     * H5T_COMPOUND
     * H5T_REFERENCE
     * H5T_ENUM
     * H5T_VLEN
     * H5T_ARRAY
     *
     */    
    class_id = H5Tget_class( type_id );

    // get storage size
    storage_size = H5Dget_storage_size( dsetr_id );

    // read antenna temperature: 16-bit unsigned integer
    unsigned short int* out_AntennaTemperature = new unsigned short int[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_UINT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_AntennaTemperature);

    float scale = 330.0/( pow(2.0,16) - 9 );
    float temp[NSCAN][NFOV][NCHAN];
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
        for(int ichan=0; ichan<NCHAN; ichan++ ) {
    	  temp[iscan][ifov][ichan] = -999.0;
    	}
      }
    }
    
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
        for(int ichan=0; ichan<NCHAN; ichan++ ) {
    	  index = iscan * NFOV * NCHAN + ifov * NCHAN + ichan;
    	  if (out_AntennaTemperature[index] < 65528)
            temp[iscan][ifov][ichan] = out_AntennaTemperature[index] * scale;
    	}
      }
    }

    delete [] out_AntennaTemperature;
    out_AntennaTemperature = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	BeamTime		64-bit integer,    12 x 96	
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(gran_id, "BeamTime");
    storage_size = H5Dget_storage_size( dsetr_id );
    long long* out_BeamTime = new long long[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_BeamTime);

    long long beamTime[NSCAN][NFOV];
    //int sec_diff = 378691200 ; // time difference in seconds between 1970 and 1958 UTC
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
        index = iscan * NFOV + ifov;
	beamTime[iscan][ifov] = out_BeamTime[index] ;
      }
    }

    delete [] out_BeamTime;
    out_BeamTime = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	NEdTWarm		32-bit  float,    12 scan x 22 chan
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(gran_id, "NEdTWarm");
    storage_size = H5Dget_storage_size( dsetr_id );
    float* out_NEdTWarm = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_NEdTWarm);
    float NEdTWarm[NSCAN][NCHAN];

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ichan=0; ichan<NCHAN; ichan++ ) {
        index = iscan * NCHAN + ichan;
        NEdTWarm[iscan][ichan] = out_NEdTWarm[index] ;
      }
    }

    delete [] out_NEdTWarm;
    out_NEdTWarm = NULL;    
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	NEdTCold		32-bit  float,    12 scan x 22 chan
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(gran_id, "NEdTCold");
    storage_size = H5Dget_storage_size( dsetr_id );
    float* out_NEdTCold = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_NEdTCold);
    float NEdTCold[NSCAN][NCHAN];

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ichan=0; ichan<NCHAN; ichan++ ) {
        index = iscan * NCHAN + ichan;
        NEdTCold[iscan][ichan] = out_NEdTCold[index] ;
      }
    }

    delete [] out_NEdTCold;
    out_NEdTCold = NULL;    
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	QF19_SCAN_ATMSSDR		8-bit  unsigned character,    12 scan
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    dsetr_id = H5Dopen1(gran_id, "QF19_SCAN_ATMSSDR");
    storage_size = H5Dget_storage_size( dsetr_id );
    unsigned char* out_QF19 = new unsigned char[storage_size];
    status = H5Dread(dsetr_id, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_QF19);
    int QF19[NSCAN];

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      QF19[iscan] = out_QF19[iscan];
    }

    delete [] out_QF19;
    out_QF19 = NULL;    
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // free up resource to avoid memory leak 
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////////
    
    delete [] dims;
    delete [] maxdims;
    delete [] dims_tb;
    delete [] maxdims_tb;
    
    dims = NULL;
    maxdims = NULL;
    dims_tb = NULL;
    maxdims_tb = NULL;

    
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // Close SDR group and file
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////////
    
    H5Gclose( gran_id ) ;
    H5Gclose( all_data_id ) ;
    H5Gclose( root_id ) ;
    H5Fclose( file_id ) ;
    
    
    
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

    int node=0;
    if ( lats[1][0]  >= lats[0][0] ) 	node = 0;
    else 				node = 1;
    
    MeasurementType ms(NCHAN,NFOV,NQC);
    
    for(int iscan=0; iscan<nscan; iscan++) {
    	
        // Default QC flags to zero
        for(int iqc=0; iqc<NQC; iqc++) {
	  ms.qc[iqc]=0;
	}

	if ( iscan >= 1 ) {
	    if ( lats[iscan][0] > lats[iscan-1][0] ) 	node = 0;
	    else 					node = 1;
	}
    	
	ms.node = node;
	
	// date/time information
	unsigned long secsAfter1958 = (unsigned long)( scanStartTime[iscan] * 0.000001 ); // micro seconds into seconds
	//unsigned long seconds = secsBefore1958 + secsAfter1958 - LEAP_SECONDS;
	unsigned long seconds = 0L;
	if( secsAfter1958 < 1719792035 ) { // 1719792035 ---> 2012/07/01 00:00:00 
	    seconds = secsBefore1958 + secsAfter1958 - 34;  
	}
	else {
	    seconds = secsBefore1958 + secsAfter1958 - 35;  
	}
	// more branches are needed in the future when leap second gets adjusted again.
	
  	struct tm time_str;
  	struct tm *tp = &time_str;
	seconds2tm( seconds, tp );
	
	ms.year = tp->tm_year + 1900;
	ms.jday = tp->tm_yday + 1;
	ms.secs = ( tp->tm_hour * 3600 + tp->tm_min * 60 + tp->tm_sec ) * 1000 ; // time in milli-seconds since the start of day
	
	// lat/lon/angle/relAziAngle/solZenAngle
	for(int ifov=0;ifov<NFOV;ifov++) {
	    ms.lat[ifov]         = lats[iscan][ifov];
	    ms.lon[ifov]         = lons[iscan][ifov];
	    ms.angle[ifov]       = sensorZenithAngle[iscan][ifov];
            if( ifov <= 47 && ms.angle[ifov] > 0 ) ms.angle[ifov] = -1.0 * ms.angle[ifov];  // 1-48 to negative values
	    ms.relAziAngle[ifov] = sensorAzimuthAngle[iscan][ifov];
	    ms.solZenAngle[ifov] = solarZenithAngle[iscan][ifov];
	}
    
	// tb
	for(int ifov=0;ifov<NFOV;ifov++) {
	  for(int ichan=0;ichan<NCHAN;ichan++) {
	    ms.tb[ifov][ichan] = temp[iscan][ifov][ichan];
	  }
	}
	
	// qc
	if( QF19[iscan] > 0 ) ms.qc[0] = 1;

	if( QF1_ATMSSDRGEO[iscan] != 0 ) ms.qc[0] = 1;
	
	// write measurement content
	writeMeasurement( myFile, ms );	  
	
    }

    myFile.close();

  
    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Output NEDT file
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           
    
    //float NEdTWarm[NSCAN][NCHAN];
    float nedts[NCHAN];
    
    for(int ichan=0; ichan<NCHAN; ichan++) nedts[ichan]=0.0;
    
    for(int ichan=0; ichan<NCHAN; ichan++) {
      for(int iscan=0; iscan<NSCAN; iscan++) {
        if( NEdTWarm[iscan][ichan] > 0 )  nedts[ichan] += NEdTWarm[iscan][ichan] ;
      }
    }
    
    for(int ichan=0; ichan<NCHAN; ichan++) nedts[ichan] = nedts[ichan] / NCHAN ;
    
    ofstream nedtFile(file_NEDT, ios::out);
    
    nedtFile << "nChan                   =";
    nedtFile.width(6); nedtFile << NCHAN << '\n';
    
    nedtFile << "CentrFreq               =\n";
    for( int ichan=0; ichan<NCHAN; ichan++ )
    {
       nedtFile << setprecision(3) << setw(10) << right << fixed << header.freq[ichan] ;
       if ( (((ichan+1) % 10) == 0) && ((ichan+1) < NCHAN) ) nedtFile << endl;
    }
    nedtFile << endl;
    
    nedtFile << "RMSnoise                =\n";
    for( int ichan=0; ichan<NCHAN; ichan++ )
    {
       nedtFile << setprecision(3) << setw(10) << right << fixed << nedts[ichan] ;
       if ( (((ichan+1) % 10) == 0) && ((ichan+1) < NCHAN) ) nedtFile << endl;
    }
    nedtFile << endl;
    
    nedtFile << "NEDTnoise               =\n";
    for( int ichan=0; ichan<NCHAN; ichan++ )
    {
       nedtFile << setprecision(3) << setw(10) << right << fixed << nedts[ichan] ;
       if ( (((ichan+1) % 10) == 0) && ((ichan+1) < NCHAN) ) nedtFile << endl;
    }
    nedtFile << endl;
    
    nedtFile.close();
    
    return 0;

}




/***************************************************************************************************
 *
 * The subroutine to read remapped SDR data
 *
 ***************************************************************************************************/

int rdr2tdr_npp_atms_SDR_remap(char *file_RDR,char* file_GEO,char *file_OUT) {

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    int index;

    ////////////////////////////////////////////////////////////////////////////////////////////
    //
    // GATMO file ( contains meta data, lat/lon/zenith angle/time, etc )
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////
    hid_t file_id     = H5Fopen(file_GEO, H5F_ACC_RDONLY,  H5P_DEFAULT);
    hid_t root_id     = H5Gopen1(file_id, "/");
    hid_t all_data_id = H5Gopen1(root_id, "All_Data");
    hid_t gran_id     = H5Gopen1(all_data_id, "ATMS-REMAP-SDR-GEO_All");
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // Latitude 		32-bit floating-point,    36 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "Latitude");
   
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    // data type id
    type_id = H5Dget_type( dsetr_id );
   
    int ndims = H5Sget_simple_extent_ndims(space_id);
    hsize_t *dims = new hsize_t[ndims];
    hsize_t *maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int NSCAN = dims[0];
    int NFOV  = dims[1];
    
    cout << dims[0] << endl;
    cout << dims[1] << endl;
    
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Latitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Latitude);

    float lats[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
	index = iscan * NFOV + ifov;
	lats[iscan][ifov] = out_Latitude[index];
      }
    }
    
    delete [] out_Latitude;
    out_Latitude = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Longitude 		32-bit floating-point,    36 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "Longitude");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_Longitude = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_Longitude);

    float lons[NSCAN][NFOV];
    
    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  lons[iscan][ifov] = out_Longitude[index];
      }
    }

    delete [] out_Longitude;
    out_Longitude = NULL;
    H5Dclose(dsetr_id);
    

    ////////////////////////////////////////////////////////////////////////////////////////////////
    //	ScanMidTime	64-bit integer,    36
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "MidTime");
    storage_size = H5Dget_storage_size( dsetr_id );
    long long *out_ScanMidTime = new long long[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_ScanMidTime);
    
    long long scanMidTime[NSCAN];
    for( int iscan=0; iscan<NSCAN; iscan++ )
	scanMidTime[iscan] = out_ScanMidTime[iscan];
    
    delete [] out_ScanMidTime;
    out_ScanMidTime = NULL;
    H5Dclose(dsetr_id);
    
	
    ////////////////////////////////////////////////////////////////////////////////////////////////
    //	ScanStartTime	64-bit integer,    36
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "StartTime");
    storage_size = H5Dget_storage_size( dsetr_id );
    long long *out_ScanStartTime = new long long[storage_size/8];
    status = H5Dread(dsetr_id, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_ScanStartTime);

    long long scanStartTime[NSCAN];
    for( int iscan=0; iscan<NSCAN; iscan++ )
	scanStartTime[iscan] = out_ScanStartTime[iscan] ;
    
    delete [] out_ScanStartTime;
    out_ScanStartTime = NULL;
    H5Dclose(dsetr_id);
    
	
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SensorAzimuthAngle 		32-bit floating-point,    36 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SatelliteAzimuthAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SensorAzimuthAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SensorAzimuthAngle);

    float sensorAzimuthAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  sensorAzimuthAngle[iscan][ifov] = out_SensorAzimuthAngle[index];
      }
    }

    delete [] out_SensorAzimuthAngle;
    out_SensorAzimuthAngle = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SensorZenithAngle 		32-bit floating-point,    36 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SatelliteZenithAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SensorZenithAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SensorZenithAngle);

    float sensorZenithAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  sensorZenithAngle[iscan][ifov] = out_SensorZenithAngle[index];
      }
    }

    delete [] out_SensorZenithAngle;
    out_SensorZenithAngle = NULL;
    H5Dclose(dsetr_id);
    

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SolarAzimuthAngle 		32-bit floating-point,    36 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SolarAzimuthAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SolarAzimuthAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SolarAzimuthAngle);
    
    float solarAzimuthAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  solarAzimuthAngle[iscan][ifov] = out_SolarAzimuthAngle[index];
      }
    }

    delete [] out_SolarAzimuthAngle;
    out_SolarAzimuthAngle = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // SolarZenithAngle 		32-bit floating-point,    36 x 96
    ////////////////////////////////////////////////////////////////////////////////////////////////
    dsetr_id = H5Dopen1(gran_id, "SolarZenithAngle");
    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_SolarZenithAngle = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_SolarZenithAngle);

    float solarZenithAngle[NSCAN][NFOV];

    for( int iscan=0; iscan<NSCAN; iscan++ ) {
      for(int ifov=0; ifov<NFOV; ifov++ ) {
    	  index = iscan * NFOV + ifov;
    	  solarZenithAngle[iscan][ifov] = out_SolarZenithAngle[index];
      }
    }

    delete [] out_SolarZenithAngle;
    out_SolarZenithAngle = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // Close GEO group and file
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////////
    
    H5Gclose( gran_id ) ;
    H5Gclose( all_data_id ) ;
    H5Gclose( root_id ) ;
    H5Fclose( file_id ) ;
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Open TATMS file ( contains antenna temperature )
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    file_id	= H5Fopen(file_RDR, H5F_ACC_RDONLY, H5P_DEFAULT);
    root_id	= H5Gopen1(file_id, "/");
    all_data_id = H5Gopen1(root_id, "All_Data");
    gran_id	= H5Gopen1(all_data_id, "ATMS-REMAP-SDR_All");


    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    //	AntennaTemperature	16-bit unsigned integer,    36 x 96 x 22
    ////////////////////////////////////////////////////////////////////////////////////////////////////           
    // Open and get the id for the antenna temperature
    dsetr_id = H5Dopen1(gran_id, "BrightnessTemperature");
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    // data type id
    type_id = H5Dget_type( dsetr_id );

    int ndims_tb = H5Sget_simple_extent_ndims(space_id);
    hsize_t *dims_tb = new hsize_t[ndims_tb];
    hsize_t *maxdims_tb = new hsize_t[ndims_tb];
    status = H5Sget_simple_extent_dims(space_id, dims_tb, maxdims_tb );
    for( int i = 0 ; i < ndims_tb ; i++ ) cout <<  dims_tb[i] << endl;
    
    //int NSCAN = dims_tb[0];
    //int NFOV  = dims_tb[1];
    int NCHAN = dims_tb[2];

    /*
    Valid class identifiers, as defined in H5Tpublic.h, are:

    * H5T_INTEGER
    * H5T_FLOAT
    * H5T_TIME
    * H5T_STRING
    * H5T_BITFIELD
    * H5T_OPAQUE
    * H5T_COMPOUND
    * H5T_REFERENCE
    * H5T_ENUM
    * H5T_VLEN
    * H5T_ARRAY
    *
    */    
    class_id = H5Tget_class( type_id );

    // get storage size
    storage_size = H5Dget_storage_size( dsetr_id );

    // read antenna temperature: 16-bit unsigned integer
    unsigned short int* out_AntennaTemperature = new unsigned short int[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_UINT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_AntennaTemperature);

    float scale = 330.0/( pow(2.0,16) - 9 );
    float temp[NSCAN][NFOV][NCHAN];
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
    for(int ifov=0;  ifov<NFOV;   ifov++  ) {
    for(int ichan=0; ichan<NCHAN; ichan++ ) {
        index = iscan * NFOV * NCHAN + ifov * NCHAN + ichan;
        temp[iscan][ifov][ichan] = out_AntennaTemperature[index] * scale;
    }
    }
    }

    delete [] out_AntennaTemperature;
    out_AntennaTemperature = NULL;
    H5Dclose(dsetr_id);


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // free up resource to avoid memory leak 
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////////
    
    delete [] dims;
    delete [] maxdims;
    delete [] dims_tb;
    delete [] maxdims_tb;
    
    dims = NULL;
    maxdims = NULL;
    dims_tb = NULL;
    maxdims_tb = NULL;


    ////////////////////////////////////////////////////////////////////////////////////////////////
    // 
    // Close group and file
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////////
    
    H5Gclose( gran_id ) ;
    H5Gclose( all_data_id ) ;
    H5Gclose( root_id ) ;
    H5Fclose( file_id ) ;


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
    if ( lats[1][0]  >= lats[0][0] ) 	node = 0;
    else 				node = 1;
    
    MeasurementType ms(NCHAN,NFOV,NQC);
    
    for(int iscan=0; iscan<nscan; iscan++) {
    	
	if ( iscan >= 1 ) {
	    if ( lats[iscan][0] > lats[iscan-1][0] ) 	node = 0;
	    else 					node = 1;
	}
    	
	ms.node = node;
	
	// date/time information
	unsigned long secsAfter1958 = (unsigned long)( scanStartTime[iscan] * 0.000001 ); // micro seconds into seconds
	//unsigned long seconds = secsBefore1958 + secsAfter1958 - LEAP_SECONDS ;
	unsigned long seconds = 0L;
	if( secsAfter1958 < 1719792035 ) { // 1719792035 ---> 2012/07/01 00:00:00
	    seconds = secsBefore1958 + secsAfter1958 - 34;  
	}
	else {
	    seconds = secsBefore1958 + secsAfter1958 - 35;  
	}
	// more branches are needed in the future when leap second gets adjusted again.
	
  	struct tm time_str;
  	struct tm *tp = &time_str;
	seconds2tm( seconds, tp );
	
	ms.year = tp->tm_year + 1900;
	ms.jday = tp->tm_yday + 1;
	ms.secs = ( tp->tm_hour * 3600 + tp->tm_min * 60 + tp->tm_sec ) * 1000 ; // time in milli-seconds since the start of day
	
	// lat/lon/angle/relAziAngle/solZenAngle
	for(int ifov=0;ifov<NFOV;ifov++) {
	    ms.lat[ifov]         = lats[iscan][ifov];
	    ms.lon[ifov]         = lons[iscan][ifov];
	    ms.angle[ifov]       = sensorZenithAngle[iscan][ifov];
	    ms.relAziAngle[ifov] = sensorAzimuthAngle[iscan][ifov];
	    ms.solZenAngle[ifov] = solarZenithAngle[iscan][ifov];
	}
    
	// tb
	for(int ifov=0;ifov<NFOV;ifov++) {
	  for(int ichan=0;ichan<NCHAN;ichan++) {
	    ms.tb[ifov][ichan] = temp[iscan][ifov][ichan];
	  }
	}
	
	// qc
	for(int iqc=0; iqc<NQC; iqc++ )
	    ms.qc[iqc] = qc[iqc];
	    
	// write measurement content
	writeMeasurement( myFile, ms );	  
	
    }

    myFile.close();
    
    return 0;  

}





////////////////////////////////////////////////////////////////////////////////////////////
//
// This is the main program to call different subroutine according to different data types
//
////////////////////////////////////////////////////////////////////////////////////////////

int main( int argc, char *argv[] )
{
  char file_RDR[FILESIZE];
  char file_GEO[FILESIZE];
  char file_OUT[FILESIZE];
  char file_NEDT[FILESIZE];

  string files_rdr[FILENUM];
  string files_geo[FILENUM];
  string files_out[FILENUM];
  string files_nedt[FILENUM];

  char rdrFileList[FILESIZE];
  char geoFileList[FILESIZE];
  char pathTDR[FILESIZE];
  char pathNEDT[FILESIZE];
  char instrConfigFile[FILESIZE];
  
  ////////////////////////////////////////////////////////////////////////////////////////////
  //
  // to get file list and control parameters from command line
  //
  ////////////////////////////////////////////////////////////////////////////////////////////

  char lineBuffer[FILESIZE] = "";
  cin.getline(lineBuffer,FILESIZE);
  char tdr_type = lineBuffer[0];
  
  cin.getline(rdrFileList,FILESIZE);
  cin.getline(pathTDR,FILESIZE);
  
  cin.getline(pathNEDT,FILESIZE);
  cin.getline(instrConfigFile,FILESIZE);
  
  
  if( tdr_type == '1' || tdr_type == '3' || tdr_type == '4' ) {
    cin.getline(geoFileList,FILESIZE);
  }

  // The following is only for debugging purpose
  /*
  char tdr_type = '3' ;
  strcpy( rdrFileList,"/disk1/pub/wchen/mirs_gfortran_linux_x64/data/InputsData/npp_atms_rdrFiles_2010-09-06.list");
  strcpy( pathTDR,"/disk1/pub/wchen/mirs_gfortran_linux_x64/data/TestbedData/DynamicData/tdr/npp_atms/2010-09-06/");	
  strcpy( pathNEDT,"/disk1/pub/wchen/mirs_gfortran_linux_x64/data/TestbedData/nedt/npp_atms/");  	
  strcpy( instrConfigFile,"/disk1/pub/wchen/mirs_gfortran_linux_x64/data/StaticData/InstrConfigInfo/InstrConfig_npp_atms.dat");
  strcpy( geoFileList,"/disk1/pub/wchen/mirs_gfortran_linux_x64/data/InputsData/npp_atms_geoFiles_2010-09-06.list");
  */

  char token[6] = "";
  if(      tdr_type == '1' ) strcpy(token,"TATMS_");
  else if( tdr_type == '2' ) strcpy(token,"SATMS_");
  else if( tdr_type == '3' ) strcpy(token,"SATMS_");
  else if( tdr_type == '4' ) strcpy(token,"SATMR_");
   
  
  ////////////////////////////////////////////////////////////////////////////////////////////
  //
  // fill in 3 file name arrays
  //
  ////////////////////////////////////////////////////////////////////////////////////////////
  ifstream inFile(rdrFileList, ios::in);
  if ( !inFile ) {
      cerr << "RDR file could not be opened: " << rdrFileList << endl;
      exit( 1 );
  }

  int nfile=0;
  char filename[FILESIZE] = "";
  
  string line;
  while ( inFile.getline(filename,FILESIZE) ) {
      //cout << filename << endl;
      files_rdr[nfile] = filename;
      string line2(filename);
      int pos = line2.find(token);
      int pos_end = line2.find(".h5");
      int length = pos_end - pos;
      files_out[nfile]  = string(pathTDR)  + "TDR_"  + line2.substr(pos,FILESIZE);
      files_nedt[nfile] = string(pathNEDT) + "NEDT_" + line2.substr(pos,length) + "_befFM.dat";
      nfile++;
  }

  inFile.close();
  
  
  if( tdr_type == '1' || tdr_type == '3' || tdr_type == '4' )
  {
  
    ifstream inFile2(geoFileList, ifstream::in);
    if ( !inFile2 ) {
	cerr << "GEO file could not be opened: " << geoFileList << endl;
	exit( 1 );
    }
    
    int nfile2=0;
    char filename2[FILESIZE] = "";
    while ( inFile2.getline(filename2,FILESIZE) ) {
	files_geo[nfile2] = filename2;
	nfile2++;
    }
    
    inFile2.close();
    
    if( nfile2 != nfile ) {
      cerr << "The file numbers of TDR and GEO are not mathched." << endl;
      exit(1); 
    }
    
  }
  
  cout << "nfile=" << nfile << endl;
  
  /* comment out
  cin.getline(nedtFile,FILESIZE);
  cin.getline(instrConfigFile,FILESIZE);
  cin.getline(norbits2process,FILESIZE);
  cin.getline(logFile,FILESIZE);
  */
  
  ////////////////////////////////////////////////////////////////////////////////////////////
  //
  // start loop around files
  //
  ////////////////////////////////////////////////////////////////////////////////////////////
  
  if(tdr_type == '1' ) 
  {
    for(int ifile=0; ifile<nfile; ifile++ ) {
      strcpy( file_RDR,files_rdr[ifile].c_str() );
      strcpy( file_GEO,files_geo[ifile].c_str() );
      strcpy( file_OUT,files_out[ifile].c_str() );
      rdr2tdr_npp_atms_TDR(file_RDR,file_GEO,file_OUT);
    }
  }
  else if(tdr_type == '2' ) 
  {
    for(int ifile=0; ifile<nfile; ifile++ ) {
      strcpy( file_RDR,files_rdr[ifile].c_str() );
      strcpy( file_OUT,files_out[ifile].c_str() );
      cout << file_RDR << endl;
      cout << file_OUT << endl;
      
      rdr2tdr_npp_atms_SDR_proxy(file_RDR,file_OUT);
    }
  }
  else if(tdr_type == '3' ) 
  {
    for(int ifile=0; ifile<nfile; ifile++ ) {
      strcpy( file_RDR,files_rdr[ifile].c_str() );
      strcpy( file_GEO,files_geo[ifile].c_str() );
      strcpy( file_OUT,files_out[ifile].c_str() );
      strcpy( file_NEDT,files_nedt[ifile].c_str() );

      //cout << file_RDR << endl;
      //cout << file_GEO << endl;
      //cout << file_OUT << endl;
      //cout << file_NEDT << endl;
      
      // to check if both files exist or not before reading them
      ifstream file1(file_RDR);
      ifstream file2(file_GEO);
      if( file1.good() && file2.good() ) {
        file1.close();
	file2.close();
        rdr2tdr_npp_atms_SDR(file_RDR,file_GEO,file_OUT,file_NEDT,instrConfigFile);
      }
    }
  }
  else if(tdr_type == '4' ) 
  {
    for(int ifile=0; ifile<nfile; ifile++ ) {
      strcpy( file_RDR,files_rdr[ifile].c_str() );
      strcpy( file_GEO,files_geo[ifile].c_str() );
      strcpy( file_OUT,files_out[ifile].c_str() );

      cout << file_RDR << endl;
      cout << file_GEO << endl;
      cout << file_OUT << endl;
      
      rdr2tdr_npp_atms_SDR_remap(file_RDR,file_GEO,file_OUT);
    }
  }

  return 0;
  
}
