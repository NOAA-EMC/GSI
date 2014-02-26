/*******************************************************************************
 * 
 * This is the C++ decoder for GCOM-W1 AMSR2 hdf5 
 *
 * Author: Wanchun Chen <Wanchun.Chen@noaa.gov>
 * export LD_LIBRARY_PATH=/data/data006/pub/zlib/lib:/data/data006/pub/szip/lib:/data/data006/pub/hdf5/lib
 * ulimit -s unlimited
 * 
 * http://www.timeanddate.com/date/timeduration.html
 * 2,934,835,200 seconds without leap seconds adjustment
 * 
 * leap second adjustment, falling into 2 values, -27 and -28
 * as of 1993-01-01 00:00:00, still -27 seconds
 *
 * http://www.timeanddate.com/time/leap-seconds-future.html
 * 
 * 1992-06-30	23:59:60	-27 seconds
 *   Time counted since 1993 00:00:00
 * 1993-06-30	23:59:60	-28 seconds    ( 1 )  
 * 1994-06-30	23:59:60	-29 seconds    ( 2 )  
 * 1995-12-31	23:59:60	-30 seconds    ( 3 )  
 * 1997-06-30	23:59:60	-31 seconds    ( 4 )  
 * 1998-12-31	23:59:60	-32 seconds    ( 5 )  
 * 2005-12-31	23:59:60	-33 seconds    ( 6 )  
 * 2008-12-31	23:59:60	-34 seconds    ( 7 )  
 * 2012-06-30	23:59:60	-35 seconds    ( 8 )  
 *
 * !!!!! Be careful here, we need only adjust leap second ever since
 * 1993/01/01 00:00:00 to current processing time, sort of relative
 * leap seconds.
 *
 * Since GCOM-W1 was launched at 1:39 18 May 2012 (Japan Standard Time),
 * so leap second adjustment at launching time is -34 seconds.
 * The first adjustment comes when date changes on 2012-06-30 23:59:60
 * SEC_19930101_TO_20120630=615254400
 * 
 * When >= SEC_19930101_TO_20120630, we use -35   ( 8 )
 * When <  SEC_19930101_TO_20120630, we use -34   ( 7 )
 *
 *
 * L1B:
 * The product that contains the converted data from level 1A data, 
 * converting observation data into brightness temperature using 
 * radiometric correction, and supplementary information same as L1A.
 *
 * 
 * L1R(Resampling):
 * The product that contains the converted data from level 1B product, 
 * adapting the resolution of each frequency and processing altitude correction.
 * 
 * Date: 11/09/2012   Wanchun Chen      Original Coder
 *
 *******************************************************************************/

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


////////////////////////////////////////////////////////////////////////////////
//
// some global constants
// 
////////////////////////////////////////////////////////////////////////////////

const int MAX_FILE_LEN = 512;

unsigned long secsBefore1993 = 2934835200;

// as of 2012-11-09, this is correct number, 
// need adjustment according to different years
// http://www.timeanddate.com/time/leap-seconds-future.html

//int LEAP_SECOND_1 = 34;
//int LEAP_SECOND_2 = 35;

int LEAP_SECOND_1 = 7;
int LEAP_SECOND_2 = 8;


//int LEAP_SECOND_3 = 36;
//int LEAP_SECOND_4 = 37;

// passing seconds from 1993/01/01 00:00:00 to 2012/06/30 23:59:60
// use as leap seconds adjustment parameter
unsigned long SEC_19930101_TO_20120630 = 615254400;


int   RFI = 1;  // do RFI mitigation 0:NOT, 1-DO

int   DEBUG = 0;
int   NQC = 4;

int   NCHAN = 14;
int   NCHAN_LR = 12;
int   NCHAN_HR = 2;

int   NFOV_LR = 243;
int   NFOV_HR = 486;

float FREQS[] = { 6.925, 6.925, 7.30,   7.30, 10.65, 10.65, 18.70, 18.70,
                  23.80, 23.80, 36.50, 36.50, 89.00, 89.00 } ;
int   POLS[]  = {4,5,4,5,4,5,4,5,4,5,4,5,4,5};

const char *TB_L1B_TOKENS[] = { 
	"Brightness Temperature (6.9GHz,V)",
        "Brightness Temperature (6.9GHz,H)",			
        "Brightness Temperature (7.3GHz,V)",			
        "Brightness Temperature (7.3GHz,H)",			
        "Brightness Temperature (10.7GHz,V)",			
        "Brightness Temperature (10.7GHz,H)",			
        "Brightness Temperature (18.7GHz,V)",			
        "Brightness Temperature (18.7GHz,H)",			
        "Brightness Temperature (23.8GHz,V)",			
        "Brightness Temperature (23.8GHz,H)",			
        "Brightness Temperature (36.5GHz,V)",			
        "Brightness Temperature (36.5GHz,H)",			
        "Brightness Temperature (89.0GHz-A,V)", 		
        "Brightness Temperature (89.0GHz-A,H)" };	


const char *TB_L1R_TOKENS[] = { 
	"Brightness Temperature (res06,6.9GHz,V)",
        "Brightness Temperature (res06,6.9GHz,H)",			
        "Brightness Temperature (res06,7.3GHz,V)",		       
        "Brightness Temperature (res06,7.3GHz,H)",		       
        "Brightness Temperature (res10,10.7GHz,V)",		      
        "Brightness Temperature (res10,10.7GHz,H)",		      
        "Brightness Temperature (res10,18.7GHz,V)",		      
        "Brightness Temperature (res10,18.7GHz,H)",		      
        "Brightness Temperature (res10,23.8GHz,V)",		      
        "Brightness Temperature (res10,23.8GHz,H)",		      
        "Brightness Temperature (res23,36.5GHz,V)",		      
        "Brightness Temperature (res23,36.5GHz,H)",		      
        "Brightness Temperature (original,89GHz-A,V)",  		
        "Brightness Temperature (original,89GHz-A,H)" };	


char *ANGLE_TOKENS[] = {"Earth Incidence",
                        "Earth Azimuth",
                        "Sun Elevation"};

float TB_SCALE = 0.01;
float ANGLE_SCALE = 0.01;


/**
  This is consitent with computation result from: 
  http://www.timeanddate.com/date/durationresult.html?
  m1=01&d1=01&y1=1900&h1=00&i1=00&s1=00&m2=01&d2=01&y2=1993&h2=00&i2=00&s2=00
  
  before leap seconds adjustment 

int main( int argc, char* argv[] ) {

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
 
  struct tm time1993;  		// 1993
  time1993.tm_sec   = 0;
  time1993.tm_min   = 0;
  time1993.tm_hour  = 0;
  time1993.tm_mday  = 1;
  time1993.tm_mon   = 0;
  time1993.tm_year  = 93;
  time1993.tm_yday  = 0;
  time1993.tm_isdst = 0;
  time_t t1993 = mktime(&time1993);

  // seconds of 1993 since 1900
  unsigned long secsBefore1993 = (unsigned long)(difftime( t1993, t1900 ));

  cout << "secsBefore1993=" << secsBefore1993 << endl;
  // secsBefore1993=2934835200

}

*/


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



/*************************************************************** 
 * input argument:  fileRDR  - Raw HDF5 RDR input file
 *
 * output arguments: 
 *                  fileTDR_LR  - Low Res. TDR
 *                  fileTDR_HR  - High Res TDR
 *
 **************************************************************/
int rdr2tdr_amsr2( char *fileRDR, char *fileTDR_LR, char *fileTDR_HR ) {

  hid_t file_id = H5Fopen(fileRDR, H5F_ACC_RDWR,  H5P_DEFAULT);
  if( DEBUG == 1 ) cout << "file_id=" << file_id << endl;
  
  hid_t root_id = H5Gopen1(file_id, "/");
  if( DEBUG == 1 ) cout << "root_id=" << root_id << endl;
  
  
  //
  //  To determine whether it is L1B or L1R 
  //  L1B: Group size = 44
  //  L1B in 2012: Group size = 45
  //  L1R: Group size = 58
  //
  
  hsize_t num_obj;
  H5Gget_num_objs(root_id, &num_obj);
  cout << "num_obj=" << num_obj << endl;
  
  
  char *startTimeString[1];
  hid_t attr_id = H5Aopen_name( root_id, "ObservationStartDateTime" );
  hid_t attr_type_id = H5Aget_type( attr_id );
  hid_t native_attr_type_id = H5Tget_native_type(attr_type_id, H5T_DIR_ASCEND);
  startTimeString[0] = new char[64];
  herr_t herr = H5Aread(attr_id, native_attr_type_id, &startTimeString);
  //cout << "herr=" << herr << endl;
  cout << "startTimeString=" << startTimeString[0] << endl;
  herr = H5Aclose( attr_id ); 
    
  
  char *TB_TOKENS[NCHAN];
  
  if( num_obj == 44 || num_obj == 45 ) {
    for( int ichan = 0; ichan < NCHAN; ichan++ ) {
      int len = strlen(TB_L1B_TOKENS[ichan]);
      TB_TOKENS[ichan] = new char[len+1];
      strcpy(TB_TOKENS[ichan],TB_L1B_TOKENS[ichan]); 
      //cout << TB_TOKENS[ichan] << endl;
    }
  }
  else {
    for( int ichan = 0; ichan < NCHAN; ichan++ ) {
      int len = strlen(TB_L1R_TOKENS[ichan]);
      TB_TOKENS[ichan] = new char[len+1];
      strcpy(TB_TOKENS[ichan],TB_L1R_TOKENS[ichan]); 
      //cout << TB_TOKENS[ichan] << endl;
    }
  }

  
  // some shared variable for all parameters
  hid_t dsetr_id;
  hid_t space_id;
  hid_t type_id;
  int   ndims;
  int   index;
  hsize_t status; 
  hsize_t storage_size;
  
  
  //////////////////////////////////////////////////////////////////////////////
  // Latitude of Observation Point for 89A, 32-bit floating-point, 3961 x 486
  //////////////////////////////////////////////////////////////////////////////
  if( DEBUG == 1) cout << "Latitude of Observation Point for 89A" << endl;
  
  dsetr_id = H5Dopen1(root_id, "Latitude of Observation Point for 89A");
  if( DEBUG == 1 ) cout << "dsetr_id=" << dsetr_id << endl;
  
  space_id = H5Dget_space( dsetr_id );
  if( DEBUG == 1 ) cout << "space_id=" << space_id << endl;
  
  type_id = H5Dget_type( dsetr_id );
  if( DEBUG == 1 ) cout << "type_id=" << type_id << endl;
  
  // number of dimensions
  ndims = H5Sget_simple_extent_ndims(space_id);
  if( DEBUG == 1 ) cout << "ndims=" << ndims << endl;
  
  // each dimension size
  hsize_t *dims = new hsize_t[ndims];
  hsize_t *maxdims = new hsize_t[ndims];
  status = H5Sget_simple_extent_dims( space_id, dims, maxdims );
  int nscan = dims[0];
  int nfov  = dims[1];
  
  int NSCAN = nscan;
  int NFOV  = nfov;
  
  if( DEBUG == 1 ) cout << "nscan=" << nscan << endl;
  if( DEBUG == 1 ) cout << "nfov=" << nfov << endl;
  
  storage_size = H5Dget_storage_size( dsetr_id );
  float *out_lats = new float[storage_size/4];
  status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_lats);

  // row major array, offset = row*NUMCOLS + column
  float lats[NSCAN][NFOV];
  for( int iscan=0; iscan<NSCAN; iscan++ ) {
    for(int ifov=0; ifov<NFOV; ifov++ ) {
      index = iscan * NFOV + ifov;
      lats[iscan][ifov] = out_lats[index];
    }
  }
  
  if( DEBUG == 1) cout << lats[0][0] << "," << lats[0][1] << "," << lats[1][0] << endl;

  delete [] out_lats;
  delete [] dims;
  delete [] maxdims;
  out_lats = NULL;
  H5Dclose(dsetr_id);
 

  //////////////////////////////////////////////////////////////////////////////
  // Longitude of Observation Point for 89A, 32-bit floating-point, 3961 x 486
  //////////////////////////////////////////////////////////////////////////////
  if( DEBUG == 1) cout << "Longitude of Observation Point for 89A" << endl;
  
  dsetr_id = H5Dopen1(root_id, "Longitude of Observation Point for 89A");
  if( DEBUG == 1 ) cout << "dsetr_id=" << dsetr_id << endl;
  
  space_id = H5Dget_space( dsetr_id );
  if( DEBUG == 1 ) cout << "space_id=" << space_id << endl;
  
  type_id = H5Dget_type( dsetr_id );
  if( DEBUG == 1 ) cout << "type_id=" << type_id << endl;
  
  // number of dimensions
  ndims = H5Sget_simple_extent_ndims(space_id);
  if( DEBUG == 1 ) cout << "ndims=" << ndims << endl;
  
  // each dimension size
  dims = new hsize_t[ndims];
  maxdims = new hsize_t[ndims];
  status = H5Sget_simple_extent_dims( space_id, dims, maxdims );
  nscan = dims[0];
  nfov  = dims[1];
  
  NSCAN = nscan;
  NFOV  = nfov;
  
  if( DEBUG == 1 ) cout << "nscan=" << nscan << endl;
  if( DEBUG == 1 ) cout << "nfov=" << nfov << endl;
  
  storage_size = H5Dget_storage_size( dsetr_id );
  float *out_lons = new float[storage_size/4];
  status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_lons);

  // row major array, offset = row*NUMCOLS + column
  float lons[NSCAN][NFOV];
  for( int iscan=0; iscan<NSCAN; iscan++ ) {
    for(int ifov=0; ifov<NFOV; ifov++ ) {
      index = iscan * NFOV + ifov;
      lons[iscan][ifov] = out_lons[index];
    }
  }
  
  if( DEBUG == 1) cout << lons[0][0] << "," << lons[0][1] << "," << lons[1][0] << endl;

  delete [] out_lons;
  delete [] dims;
  delete [] maxdims;
  out_lons = NULL;
  H5Dclose(dsetr_id);



  //////////////////////////////////////////////////////////////////////////////
  // Scan Time,64-bit floating-point,3961 SCALE FACTOR = 1.0 UNIT = sec
  // Time is in seconds since 0000 UTC on 1/1/1993 !!!!!
  //////////////////////////////////////////////////////////////////////////////
  if( DEBUG == 1) cout << "Scan Time" << endl;
  
  dsetr_id = H5Dopen1(root_id, "Scan Time");
  if( DEBUG == 1 ) cout << "dsetr_id=" << dsetr_id << endl;
  
  space_id = H5Dget_space( dsetr_id );
  if( DEBUG == 1 ) cout << "space_id=" << space_id << endl;
  
  type_id = H5Dget_type( dsetr_id );
  if( DEBUG == 1 ) cout << "type_id=" << type_id << endl;
  
  // number of dimensions
  ndims = H5Sget_simple_extent_ndims(space_id);
  if( DEBUG == 1 ) cout << "ndims=" << ndims << endl;
  
  // each dimension size
  dims = new hsize_t[ndims];
  maxdims = new hsize_t[ndims];
  status = H5Sget_simple_extent_dims( space_id, dims, maxdims );
  nscan = dims[0];
  nfov  = dims[1];
  
  NSCAN = nscan;
  NFOV  = nfov;
  
  if( DEBUG == 1 ) cout << "nscan=" << nscan << endl;
  if( DEBUG == 1 ) cout << "nfov=" << nfov << endl;
  
  storage_size = H5Dget_storage_size( dsetr_id );
  double *out_times = new double[storage_size/8];
  status = H5Dread(dsetr_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_times);

  // row major array, offset = row*NUMCOLS + column
  double times[NSCAN];
  for( int iscan=0; iscan<NSCAN; iscan++ ) {
    times[iscan] = out_times[iscan];
  }
  
  if( DEBUG == 1) cout << times[0] << "," << times[1] << endl;

  delete [] out_times;
  delete [] dims;
  delete [] maxdims;
  out_times = NULL;
  H5Dclose(dsetr_id);


  int years[NSCAN];
  int jdays[NSCAN];
  int msecs[NSCAN];  // NOTE: !!!!!!!!! as this step, MIRS saved in milli-seconds

  for( int iscan=0; iscan<NSCAN; iscan++ ) {
  
    unsigned long secsAfter1993 = (unsigned long) times[iscan];
    if( DEBUG == 1 ) cout << "diff=" << times[iscan] - secsAfter1993 << endl;
    
    unsigned long secs = 0;
    if( secsAfter1993 <= SEC_19930101_TO_20120630 ) {
      secs = secsBefore1993 + secsAfter1993 - LEAP_SECOND_1;
    }
    else {
      secs = secsBefore1993 + secsAfter1993 - LEAP_SECOND_2;
      //secs = secsBefore1993 + secsAfter1993 - 8;
    }
    // .... NOTE, we might need add more branches when TAI and UTC difference gets changed again.
    
    
    struct tm time_str;
    struct tm *tp = &time_str;
    seconds2tm( secs, tp );
	
    years[iscan] = tp->tm_year + 1900;
    jdays[iscan] = tp->tm_yday + 1;
    msecs[iscan] = ( tp->tm_hour * 3600 + tp->tm_min * 60 + tp->tm_sec ) * 1000 ; 
    
    //if( DEBUG == 1 ) cout  << times[iscan] << "," << years[iscan] <<","
    if( iscan == 0 ) {
      cout  << "times[iscan]="  << times[iscan]  << endl;
      cout  << "secsAfter1993=" << secsAfter1993 << endl;
      cout  << "years[iscan]="  << years[iscan]  << endl;
      cout  << "jdays[iscan]="  << jdays[iscan]  << endl;
      cout  << "month=" << tp->tm_mon+1 << endl;
      cout  << "day="   << tp->tm_mday  << endl;
      cout  << "hour="  << tp->tm_hour  << endl;
      cout  << "min="   << tp->tm_min   << endl;
      cout  << "sec="   << tp->tm_sec   << endl;
    }
  }


  //////////////////////////////////////////////////////////////////////////////
  // Earth Incidence,Earth Azimuth,Sun Elevation
  // 16-bit integer, 3961 x 243 SCALE FACTOR = 0.01 UNIT = deg
  //////////////////////////////////////////////////////////////////////////////
  
  float satZenAngs[NSCAN][NFOV_LR];
  float satAziAngs[NSCAN][NFOV_LR];
  float sunZenAngs[NSCAN][NFOV_LR];
  
  for( int iang=0; iang<3; iang++ ) {
  
    if( DEBUG == 1) cout << ANGLE_TOKENS[iang] << endl;
    dsetr_id = H5Dopen1(root_id, ANGLE_TOKENS[iang]);
    if( DEBUG == 1 ) cout << "dsetr_id=" << dsetr_id << endl;

    space_id = H5Dget_space( dsetr_id );
    if( DEBUG == 1 ) cout << "space_id=" << space_id << endl;

    type_id = H5Dget_type( dsetr_id );
    if( DEBUG == 1 ) cout << "type_id=" << type_id << endl;

    // number of dimensions
    ndims = H5Sget_simple_extent_ndims(space_id);
    if( DEBUG == 1 ) cout << "ndims=" << ndims << endl;

    // each dimension size
    dims = new hsize_t[ndims];
    maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims( space_id, dims, maxdims );
    nscan = dims[0];
    nfov  = dims[1];

    NSCAN = nscan;
    NFOV  = nfov;

    if( DEBUG == 1 ) cout << "nscan=" << nscan << endl;
    if( DEBUG == 1 ) cout << "nfov=" << nfov << endl;

    storage_size = H5Dget_storage_size( dsetr_id );
    short *out_angs = new short[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_angs);

    // row major array, offset = row*NUMCOLS + column
    if( iang == 0 ) {
      for( int iscan=0; iscan<NSCAN; iscan++ ) {
	for( int ifov=0; ifov<NFOV; ifov++ ) {
          index = iscan * NFOV + ifov;
          satZenAngs[iscan][ifov] = out_angs[index] * ANGLE_SCALE;
	}
      }
      if( DEBUG == 1) cout << satZenAngs[0][0] << "," <<
      satZenAngs[0][1] << "," << satZenAngs[1][0] << endl;
    }
    else if( iang == 1 ) {
      for( int iscan=0; iscan<NSCAN; iscan++ ) {
	for( int ifov=0; ifov<NFOV; ifov++ ) {
          index = iscan * NFOV + ifov;
          satAziAngs[iscan][ifov] = out_angs[index] * ANGLE_SCALE;
	}
      }
      if( DEBUG == 1) cout << satAziAngs[0][0] << "," <<
      satAziAngs[0][1] << "," << satAziAngs[1][0] << endl;
    }
    else if( iang == 2 ) {
      for( int iscan=0; iscan<NSCAN; iscan++ ) {
	for( int ifov=0; ifov<NFOV; ifov++ ) {
          index = iscan * NFOV + ifov;
          sunZenAngs[iscan][ifov] = out_angs[index] * ANGLE_SCALE;
	}
      }
      if( DEBUG == 1) cout << sunZenAngs[0][0] << "," <<
      sunZenAngs[0][1] << "," << sunZenAngs[1][0] << endl;
    }

    delete [] out_angs;
    delete [] dims;
    delete [] maxdims;
    out_angs = NULL;
    H5Dclose(dsetr_id);
  }
  
  
  for( int iscan=0; iscan<NSCAN; iscan++ ) {
    for( int ifov=0; ifov<NFOV_LR; ifov++ ) {
      sunZenAngs[iscan][ifov] = sunZenAngs[iscan][ifov] + satZenAngs[iscan][ifov] ;
    }
  }



  //////////////////////////////////////////////////////////////////////////////
  // Now loop thru low res. channel TB
  //////////////////////////////////////////////////////////////////////////////
  
  float tb_lrs[NSCAN][NFOV_LR][NCHAN_LR];
  
  for( int ichan=0; ichan<NCHAN_LR; ichan++ ) {
    
    if( DEBUG == 1 ) cout << TB_TOKENS[ichan] << endl;
    dsetr_id = H5Dopen1(root_id, TB_TOKENS[ichan]);
    
    if( DEBUG == 1 ) cout << "dsetr_id=" << dsetr_id << endl;
    space_id = H5Dget_space( dsetr_id );
    if( DEBUG == 1 ) cout << "space_id=" << space_id << endl;

    type_id = H5Dget_type( dsetr_id );
    if( DEBUG == 1 ) cout << "type_id=" << type_id << endl;

    ndims = H5Sget_simple_extent_ndims(space_id);
    if( DEBUG == 1 ) cout << "ndims=" << ndims << endl;

    // each dimension size
    dims = new hsize_t[ndims];
    maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    nscan = dims[0];
    nfov  = dims[1];

    if( DEBUG == 1 ) cout << "nscan=" << nscan << endl;
    if( DEBUG == 1 ) cout << "nfov=" << nfov << endl;

    storage_size = H5Dget_storage_size( dsetr_id );
    if( DEBUG == 1 ) cout << "storage_size=" << storage_size << endl;

    short *out_tbs = new short[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_tbs);

    if( DEBUG == 1 ) cout << "status=" << status << endl; 

    for( int iscan=0; iscan<nscan; iscan++ ) {
      for(int ifov=0; ifov<nfov; ifov++ ) {
	index = iscan * nfov + ifov;
	tb_lrs[iscan][ifov][ichan] = out_tbs[index] * TB_SCALE;
      }
    }
    
    if( DEBUG == 1) cout << tb_lrs[0][0][ichan] << endl;
    
    delete [] out_tbs;
    delete [] dims;
    delete [] maxdims;
    out_lats = NULL;
    H5Dclose(dsetr_id);
 
  }


  //////////////////////////////////////////////////////////////////////////////
  // Now loop thru high res. channel TB
  //////////////////////////////////////////////////////////////////////////////
  
  float tb_hrs[NSCAN][NFOV_HR][NCHAN_HR];
  
  for( int ichan=0; ichan<NCHAN_HR; ichan++ ) {
    
    if( DEBUG == 1 ) cout << TB_TOKENS[ichan+NCHAN_LR] << endl;
    dsetr_id = H5Dopen1(root_id, TB_TOKENS[ichan+NCHAN_LR]);
    
    if( DEBUG == 1 ) cout << "dsetr_id=" << dsetr_id << endl;
    space_id = H5Dget_space( dsetr_id );
    if( DEBUG == 1 ) cout << "space_id=" << space_id << endl;

    type_id = H5Dget_type( dsetr_id );
    if( DEBUG == 1 ) cout << "type_id=" << type_id << endl;

    ndims = H5Sget_simple_extent_ndims(space_id);
    if( DEBUG == 1 ) cout << "ndims=" << ndims << endl;

    // each dimension size
    dims = new hsize_t[ndims];
    maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    nscan = dims[0];
    nfov  = dims[1];

    if( DEBUG == 1 ) cout << "nscan=" << nscan << endl;
    if( DEBUG == 1 ) cout << "nfov=" << nfov << endl;

    storage_size = H5Dget_storage_size( dsetr_id );
    if( DEBUG == 1 ) cout << "storage_size=" << storage_size << endl;

    short *out_tbs = new short[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_tbs);

    if( DEBUG == 1 ) cout << "status=" << status << endl; 

    for( int iscan=0; iscan<nscan; iscan++ ) {
      for(int ifov=0; ifov<nfov; ifov++ ) {
	index = iscan * nfov + ifov;
	tb_hrs[iscan][ifov][ichan] = out_tbs[index] * TB_SCALE;
      }
    }
    
    if( DEBUG == 1) cout << tb_hrs[0][0][ichan] << endl;
    
    delete [] out_tbs;
    delete [] dims;
    delete [] maxdims;
    out_lats = NULL;
    H5Dclose(dsetr_id);
 
  }


  
  // close group and close the file

  H5Gclose( root_id ) ;
  H5Fclose( file_id ) ;
  

  
  ////////////////////////////////////////////////////////////////////////////           
  //
  // RFI Section Start
  //
  ////////////////////////////////////////////////////////////////////////////           

  if( RFI == 1 ) {  // start branch if( RFI == 1 )

    //threshold for FRI identification
    //float rfimin = -5.0 ;
    float rfimax =  5.0 ;

    float diff10_6h = 6.0 ;
    float diff10_6v = 4.0 ;
    float diff6_18h = 10.0 ;
    float diff6_18v = 5.0 ;

    // coefficients:
    // use TB10.65 to predict TB6.925, stdev_h = 1.66425, stdev_v = 1.53857
    float a0 = -9.68610 ;
    float a1 = 1.10629 ;
    float a2 = -0.0718768 ;
    float a3 = 0.0 ;
    float a4 = 0.0 ;
    float b0 = -8.99197 ;
    float b1 = 0.951212 ;
    float b2 = 0.0752778 ;
    float b3 = 0.0 ;
    float b4 = 0.0 ;

    //use TB18.7 to predict TB10.65, stdev_h = 1.29012, stdev_v = 1.48666
    float c0 = 10.6396 ;
    float c1 = 1.04417 ;
    float c2 = -0.0904358 ;
    float c3 = 0.0 ;
    float c4 = 0.0 ;
    float d0 = 9.31105 ;
    float d1 = 0.913560 ;
    float d2 = 0.0403827 ;
    float d3 = 0.0 ;
    float d4 = 0.0 ;

    //use TB18.7 to predict TB6.925, stdev_h = 2.21522, stdev_v = 1.97212
    float e0 = 15.3020 ;
    float e1 = 1.11368 ;
    float e2 = -0.175613 ;
    float e3 = 0 ;
    float e4 = 0 ;
    float f0 = 17.4359 ;
    float f1 = 0.903253 ;
    float f2 = 0.0175537 ;
    float f3 = 0.0 ;
    float f4 = 0.0 ;

    for( int iscan = 0; iscan<NSCAN;  iscan++ ) {  // iscan loop start
    for( int ifov  = 0; ifov<NFOV_LR; ifov++  ) {  // ifov  loop start

	// detect and mitigate RFI-related contaminations V-pol start
	if( tb_lrs[iscan][ifov][2]-tb_lrs[iscan][ifov][4] >= rfimax ) {
	  if( tb_lrs[iscan][ifov][2]-tb_lrs[iscan][ifov][0] >=  diff10_6v ) {
            // TB10 is contaminated by RFI
            if( tb_lrs[iscan][ifov][0]-tb_lrs[iscan][ifov][4] >= diff6_18v ) {
              // TB6 is also  contaminated by RFI, use TB18 to predict TB6 and TB10
              tb_lrs[iscan][ifov][0]=e0+e1*tb_lrs[iscan][ifov][4]+e2*tb_lrs[iscan][ifov][5]+
        	  e3*pow(tb_lrs[iscan][ifov][4],2)+e4*pow(tb_lrs[iscan][ifov][5],2);
              tb_lrs[iscan][ifov][2]=c0+c1*tb_lrs[iscan][ifov][4]+c2*tb_lrs[iscan][ifov][5]+
        	  c3*pow(tb_lrs[iscan][ifov][4],2)+c4*pow(tb_lrs[iscan][ifov][5],2);
            }
            else {
              // TB10 is contaminated by RFI, but not for TB6, use TB18 to predict TB10
              tb_lrs[iscan][ifov][2]=c0+c1*tb_lrs[iscan][ifov][4]+c2*tb_lrs[iscan][ifov][5]+
        	  c3*pow(tb_lrs[iscan][ifov][4],2)+c4*pow(tb_lrs[iscan][ifov][5],2);
            }
	  }
	} else {
	  if( tb_lrs[iscan][ifov][0]-tb_lrs[iscan][ifov][2] >= rfimax  ) {
            // TB6 is contaminated by RFI, but not for TB10, use TB10 to predict TB6
            tb_lrs[iscan][ifov][0]=a0+a1*tb_lrs[iscan][ifov][2]+a2*tb_lrs[iscan][ifov][3]+
        	a3*pow(tb_lrs[iscan][ifov][2],2)+a4*pow(tb_lrs[iscan][ifov][3],2);
	  }
	}
	// detect and mitigate RFI-related contaminations V-pol end

	// detect and mitigate RFI-related contaminations H-pol start
	if( tb_lrs[iscan][ifov][3]-tb_lrs[iscan][ifov][5] >= rfimax  ) {
	  if( tb_lrs[iscan][ifov][3]-tb_lrs[iscan][ifov][1] >=  diff10_6h ) {
            // TB10 is contaminated by RFI
            if( tb_lrs[iscan][ifov][1]-tb_lrs[iscan][ifov][5] >= diff6_18h ) {
              // TB6 is also  contaminated by RFI, use TB18 to predict TB6 and TB10
              tb_lrs[iscan][ifov][1]=f0+f1*tb_lrs[iscan][ifov][5]+f2*tb_lrs[iscan][ifov][4]+
        	  f3*pow(tb_lrs[iscan][ifov][5],2)+f4*pow(tb_lrs[iscan][ifov][4],2);
              tb_lrs[iscan][ifov][3]=d0+d1*tb_lrs[iscan][ifov][5]+d2*tb_lrs[iscan][ifov][4]+
        	  d3*pow(tb_lrs[iscan][ifov][5],2)+d4*pow(tb_lrs[iscan][ifov][4],2);
            }
            else {
              // TB10 is contaminated by RFI, but not for TB6, use TB18 to predict TB10
              tb_lrs[iscan][ifov][3]=d0+d1*tb_lrs[iscan][ifov][5]+d2*tb_lrs[iscan][ifov][4]+
        	  d3*pow(tb_lrs[iscan][ifov][5],2)+d4*pow(tb_lrs[iscan][ifov][4],2);
            }
	  }  
	} else {
	    if( tb_lrs[iscan][ifov][1]-tb_lrs[iscan][ifov][3] >= rfimax  ) {
              // TB6 is contaminated by RFI, but not for TB10, use TB10 to predict TB6
              tb_lrs[iscan][ifov][1]=b0+b1*tb_lrs[iscan][ifov][3]+b2*tb_lrs[iscan][ifov][2]+
        	  b3*pow(tb_lrs[iscan][ifov][3],2)+b4*pow(tb_lrs[iscan][ifov][2],2);
	    }
        }
	//detect and mitigate RFI-related contaminations H-pol end

    } // ifov loop end
    } // iscan loop end

  } // end branch if( RFI == 1 )

  ////////////////////////////////////////////////////////////////////////////           
  //
  // RFI Section End
  //
  ////////////////////////////////////////////////////////////////////////////           



  ////////////////////////////////////////////////////////////////////////////           
  //
  // Output section 1: LR 
  //
  ////////////////////////////////////////////////////////////////////////////           
  nscan = NSCAN;

  MeasurementTypeHeader  header_LR(nscan,NCHAN_LR,NFOV_LR,NQC);
  for( int ichan=0; ichan<NCHAN_LR; ichan++) {
      header_LR.freq[ichan]  = FREQS[ichan];
      header_LR.polar[ichan] = POLS[ichan];
  } 

  // open TDR file for output 
  ofstream outFile_LR(fileTDR_LR, ios::out);

  // header part, so that it can be read out by fortran subroutine ReadRadHdrScanLMode_ascii 
  writeMeasurementHdr(outFile_LR, header_LR);

  int node=0;
  if ( lats[1][0]  >= lats[0][0] )
    node = 0;
  else
    node = 1;

  MeasurementType ms_LR(NCHAN_LR,NFOV_LR,NQC);

  for(int iscan=0; iscan<nscan; iscan++) {

      // Default QC flags to zero
      for(int iqc=0; iqc<NQC; iqc++) {
          ms_LR.qc[iqc]=0;
      }

      if ( iscan >= 1 ) {
          if ( lats[iscan][0] > lats[iscan-1][0] )
	    node = 0;
          else
	    node = 1;
      }

      ms_LR.node = node;
      ms_LR.year = years[iscan];
      ms_LR.jday = jdays[iscan];
      ms_LR.secs = msecs[iscan];

      // lat/lon/angle/relAziAngle/solZenAngle
      for(int ifov=0;ifov<NFOV_LR;ifov++) {
        ms_LR.lat[ifov]         = lats[iscan][2*ifov];
        ms_LR.lon[ifov]         = lons[iscan][2*ifov];
        ms_LR.angle[ifov]       = satZenAngs[iscan][ifov];
        ms_LR.relAziAngle[ifov] = satAziAngs[iscan][ifov];
        ms_LR.solZenAngle[ifov] = sunZenAngs[iscan][ifov];
      }

      // tb
      for(int ifov=0;ifov<NFOV_LR;ifov++) {
        for(int ichan=0;ichan<NCHAN_LR;ichan++) {
          ms_LR.tb[ifov][ichan] = tb_lrs[iscan][ifov][ichan];
        }
      }

      // write measurement content
      writeMeasurement( outFile_LR, ms_LR );	  

  }

  outFile_LR.close();

  if( DEBUG == 1 ) cout << "Finish output LR, now output HR" << endl;


  //////////////////////////////////////////////////////////////////////////////
  //
  // Output section 2: HR
  //
  //////////////////////////////////////////////////////////////////////////////

  MeasurementTypeHeader  header_HR(nscan,NCHAN_HR,NFOV_HR,NQC);
  for( int ichan=0; ichan<NCHAN_HR; ichan++) {
      header_HR.freq[ichan]  = FREQS[ichan+NCHAN_LR];
      header_HR.polar[ichan] = POLS[ichan+NCHAN_LR];
  } 

  // open TDR file for output 
  ofstream outFile_HR(fileTDR_HR, ios::out);

  // header part, so that it can be read out by fortran subroutine ReadRadHdrScanLMode_ascii 
  writeMeasurementHdr(outFile_HR, header_HR);

  node=0;
  if ( lats[1][0]  >= lats[0][0] )
    node = 0;
  else
    node = 1;

  MeasurementType ms_HR(NCHAN_HR,NFOV_HR,NQC);

  for(int iscan=0; iscan<nscan; iscan++) {

      // Default QC flags to zero
      for(int iqc=0; iqc<NQC; iqc++) {
          ms_HR.qc[iqc]=0;
      }

      if ( iscan >= 1 ) {
          if ( lats[iscan][0] > lats[iscan-1][0] )
	    node = 0;
          else
	    node = 1;
      }

      ms_HR.node = node;
      ms_HR.year = years[iscan];
      ms_HR.jday = jdays[iscan];
      ms_HR.secs = msecs[iscan];

      // lat/lon/angle/relAziAngle/solZenAngle
      for(int ifov=0;ifov<NFOV_HR;ifov++) {
        ms_HR.lat[ifov]         = lats[iscan][ifov];
        ms_HR.lon[ifov]         = lons[iscan][ifov];
        ms_HR.angle[ifov]       = satZenAngs[iscan][ifov/2];
        ms_HR.relAziAngle[ifov] = satAziAngs[iscan][ifov/2];
        ms_HR.solZenAngle[ifov] = sunZenAngs[iscan][ifov/2];
      }

      // tb
      for(int ifov=0;ifov<NFOV_HR;ifov++) {
        for(int ichan=0;ichan<NCHAN_HR;ichan++) {
          ms_HR.tb[ifov][ichan] = tb_hrs[iscan][ifov][ichan];
        }
      }

      // write measurement content
      writeMeasurement( outFile_HR, ms_HR );          

  }

  outFile_HR.close();
  
  
  // release memory allocated  
  for(int ichan = 0; ichan < NCHAN; ichan++ )
      delete [] TB_TOKENS[ichan];  
  
  return 0;

} 



/**
 *  main program read two lines from stand input.
 *  line 1: list of RDR files
 *  line 2: output path of TDR, with an ending slash
 *
 */
int main( int argc, char *argv[] ) {

  // read in the 2 arguments
  string listRDR;
  getline(cin,listRDR);
  if( DEBUG == 1) cout << "listRDR" << listRDR << "\n";
  
  string pathTDR;
  getline(cin,pathTDR);
  if( DEBUG == 1) cout << "pathTDR" << pathTDR << "\n";

  // convert the 2 arguments into C char type
  char *listRDR_cstr = new char[listRDR.size()+1];
  strcpy(listRDR_cstr, listRDR.c_str());
  
  char *pathTDR_cstr = new char[pathTDR.size()+1];
  strcpy(pathTDR_cstr, pathTDR.c_str());
  
  
  ifstream inFile(listRDR_cstr, ios::in);
  if ( !inFile ) {
      cerr << "RDR file list could not be opened: " << listRDR_cstr << endl;
      exit( 1 );
  }
  
  // loop thru the list
  char fileRDR[MAX_FILE_LEN];
  char fileTDR_LR[MAX_FILE_LEN];
  char fileTDR_HR[MAX_FILE_LEN];
  
  int ifile=1;
  while( inFile.getline(fileRDR,MAX_FILE_LEN) ) {
    
    cout << "ifile=" << ifile << endl;
    //if( DEBUG == 1) cout << "RDR=" << fileRDR << endl;
    cout << "RDR=" << fileRDR << endl;
    
    strcpy(fileTDR_LR, pathTDR_cstr);
    strcat(fileTDR_LR, "TDR_LR_");
    
    strcpy(fileTDR_HR, pathTDR_cstr);
    strcat(fileTDR_HR, "TDR_HR_");
    
    // get file base name part of fileRDR
    char *ptr = strrchr(fileRDR,'/');
    if( ptr != NULL ) {
      if( DEBUG == 1) cout << "RDR basename=" << ptr+1 << endl;
      strcat(fileTDR_LR, ptr+1);
      strcat(fileTDR_HR, ptr+1);
    }
    else {
      if( DEBUG == 1) cout << "RDR basename=" << fileRDR << endl;
      strcat(fileTDR_LR, fileRDR);
      strcat(fileTDR_HR, fileRDR);
    }
    
    // strip trailing .h5
    int len = strlen(fileTDR_LR);
    fileTDR_LR[len-3] = '\0';
    fileTDR_HR[len-3] = '\0';
    
    cout << "TDR_LR=" << fileTDR_LR << endl ;
    cout << "TDR_HR=" << fileTDR_HR << endl ;
    
    // call sub to decode input RDR and generate output TDR
    rdr2tdr_amsr2(fileRDR,fileTDR_LR,fileTDR_HR);
    cout << endl;
    ifile++;
  }
  
  delete [] listRDR_cstr;
  delete [] pathTDR_cstr;


  return 0;
}
