/***************************************************************************************************
 * 
 * This is the C++ decoder for MT/SAPHIR L1A2.
 *
 * The raw HDF5 file name is like:
 * MT1SAPSL1A2_1.05_000_9_14_I_2013_02_21_16_45_19_2013_02_21_18_01_21_07043_07044_073_60_61_KRU_00.h5 
 *
 * Author: Wanchun Chen <Wanchun.Chen@noaa.gov>
 *
 * Date: 03/04/2013 
 *
 ***************************************************************************************************/

#include "mt.h"

// those functions are implemented in mt.cc

unsigned short *get_data_ushort_1d_from_group(hid_t, char *, int & );
float **get_data_ushort_2d_from_group(hid_t, char *, int &, int & );
float **get_data_short_2d_from_group(hid_t, char *, int &, int & );
float **get_data_float_2d_from_group(hid_t, char *, int &, int & );
char **get_data_string_2d_from_group(hid_t, char *, int &, int & );
int getJday( int year, int month, int day );

int writeMeasurementHdr(ofstream &myFile, MeasurementTypeHeader& header);
int writeMeasurement(ofstream &myFile, MeasurementType& measurement);


////////////////////////////////////////////////////////////////////////////////////////////
//
// some global variables
// 
////////////////////////////////////////////////////////////////////////////////////////////

const int NCHAN = 6;
const int NQC = 4;
const int NFOV = 130;

int NSCAN = 0;

float freqs[NCHAN] = { 183.310,183.310,183.310,183.310,183.310,183.310 };
int polars[NCHAN] = {3,3,3,3,3,3};


int rdr2tdr_saphir_TDR( char *file_RDR, char *file_TDR ) {
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    //
    // Open file, root, group
    // 
    ////////////////////////////////////////////////////////////////////////////////////////////
    hid_t file_id     = H5Fopen(file_RDR, H5F_ACC_RDWR,  H5P_DEFAULT);
    hid_t root_id     = H5Gopen1(file_id, "/");
    hid_t group_id    = H5Gopen1(root_id, "ScienceData");
    

    ////////////////////////////////////////////////////////////////////////////////////////////
    // Latitude                 16-bit unsigned integer,    4263 x 130
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **lats;
    char lat_name [] = "Latitude_pixels";
    
    int nscan_return = 0;
    int nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << lat_name << endl;
    lats = get_data_ushort_2d_from_group( group_id, lat_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "lats[0][0]=" << lats[0][0] << endl;
      cout << "lats[0][1]=" << lats[0][1] << endl;
      cout << endl;
    }
    
    // only assign global NSCAN once
    NSCAN = nscan_return;
    cout << "NSCAN=" << NSCAN << endl;
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // Longitude                 16-bit unsigned integer,    4263 x 130
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **lons;
    char lon_name [] = "Longitude_pixels";
    
    nscan_return = 0;
    nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << lon_name << endl;
    lons = get_data_ushort_2d_from_group( group_id, lon_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "lons[0][0]=" << lons[0][0] << endl;
      cout << "lons[0][1]=" << lons[0][1] << endl;
      cout << endl;
    }
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // Scan_FirstPixelAcqTime         String,length=25, 1 x 4263
    ////////////////////////////////////////////////////////////////////////////////////////////
    char **times;
    char time_name [] = "Scan_FirstPixelAcqTime";
    
    nscan_return = 0;
    nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << time_name << endl;
    times = get_data_string_2d_from_group( group_id, time_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "times[0]=" << times[0] << endl;      
      cout << "times[1]=" << times[1] << endl;
      cout << endl;
    }
    
    int years[NSCAN];
    int jdays[NSCAN];
    int secs[NSCAN];
    
    for(int iscan=0; iscan<nscan_return; iscan++ ) {
      char year_cstr[5];
      char month_cstr[3];
      char day_cstr[3];
      char hh_cstr[3];
      char mm_cstr[3];
      char ss_cstr[3];
      char us_cstr[7];  // micro second
      
      string time_string = string(times[iscan]);
      string year_string = time_string.substr(0,4);
      string month_string = time_string.substr(4,2);
      string day_string = time_string.substr(6,2);
      string hh_string = time_string.substr(9,2);
      string mm_string = time_string.substr(11,2);
      string ss_string = time_string.substr(13,2);
      string us_string = time_string.substr(15,6);
      
      strcpy(year_cstr,year_string.c_str());
      strcpy(month_cstr,month_string.c_str());
      strcpy(day_cstr,day_string.c_str());
      strcpy(hh_cstr,hh_string.c_str());
      strcpy(mm_cstr,mm_string.c_str());
      strcpy(ss_cstr,ss_string.c_str());
      strcpy(us_cstr,us_string.c_str());
      
      int year = atoi(year_cstr);
      int month = atoi(month_cstr);
      int day = atoi(day_cstr);
      int hh = atoi(hh_cstr);
      int mm = atoi(mm_cstr);
      int ss = atoi(ss_cstr);
      int us = atoi(us_cstr);
      
      int jday = getJday(year,month,day);
      // MIRS time in milli-seconds since the start of day
      int ms = ( hh * 3600 + mm * 60 + ss + us / 1000000 ) * 1000 ; 
      
      years[iscan] = year;
      jdays[iscan] = jday;
      secs[iscan] = ms;
      
      
      if( DEBUG_FLAG == 1 ) {
        cout << "time_string=" << time_string << endl;
        cout << "year_string=" << year_string << endl;
        cout << "month_string=" << month_string << endl;
        cout << "day_string=" << day_string << endl;
        cout << "hh_string=" << hh_string << endl;
        cout << "mm_string=" << mm_string << endl;
        cout << "ss_string=" << ss_string << endl;
        cout << "us_string=" << us_string << endl;
        
        cout << "year=" << year << endl;
        cout << "jday=" << jday << endl;
        cout << "ms=" << ms << endl;
        cout << endl;
        
      }
      
    }


    ////////////////////////////////////////////////////////////////////////////////////////////
    // Incidence_Angle           16-bit integer,    4263 x 130
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **angles;
    char angle_name [] = "Incidence_Angle";
    
    nscan_return = 0;
    nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << angle_name << endl;
    angles = get_data_short_2d_from_group( group_id, angle_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "angles[0][0]=" << angles[0][0] << endl;
      cout << "angles[0][1]=" << angles[0][1] << endl;
      cout << endl;
    }

    
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_S1           16-bit unsigned integer,    4263 x 130
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb1s;
    char tb1_name [] = "TB_Pixels_S1";
    
    nscan_return = 0;
    nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << tb1_name << endl;
    tb1s = get_data_ushort_2d_from_group( group_id, tb1_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb1s[0][0]=" << tb1s[0][0] << endl;
      cout << "tb1s[0][1]=" << tb1s[0][1] << endl;
      cout << endl;
    }
    

    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_S2           16-bit unsigned integer,    4263 x 130
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb2s;
    char tb2_name [] = "TB_Pixels_S2";
    
    nscan_return = 0;
    nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << tb2_name << endl;
    tb2s = get_data_ushort_2d_from_group( group_id, tb2_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb2s[0][0]=" << tb2s[0][0] << endl;
      cout << "tb2s[0][1]=" << tb2s[0][1] << endl;
      cout << endl;
    }
    

    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_S3           16-bit unsigned integer,    4263 x 130
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb3s;
    char tb3_name [] = "TB_Pixels_S3";
    
    nscan_return = 0;
    nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << tb3_name << endl;
    tb3s = get_data_ushort_2d_from_group( group_id, tb3_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb3s[0][0]=" << tb3s[0][0] << endl;
      cout << "tb3s[0][1]=" << tb3s[0][1] << endl;
      cout << endl;
    }


    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_S4           16-bit unsigned integer,    4263 x 130
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb4s;
    char tb4_name [] = "TB_Pixels_S4";
    
    nscan_return = 0;
    nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << tb4_name << endl;
    tb4s = get_data_ushort_2d_from_group( group_id, tb4_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb4s[0][0]=" << tb4s[0][0] << endl;
      cout << "tb4s[0][1]=" << tb4s[0][1] << endl;
      cout << endl;
    }
    

    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_S5           16-bit unsigned integer,    4263 x 130
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb5s;
    char tb5_name [] = "TB_Pixels_S5";
    
    nscan_return = 0;
    nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << tb5_name << endl;
    tb5s = get_data_ushort_2d_from_group( group_id, tb5_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb5s[0][0]=" << tb5s[0][0] << endl;
      cout << "tb5s[0][1]=" << tb5s[0][1] << endl;
      cout << endl;
    }


    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_S6           16-bit unsigned integer,    4263 x 130
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb6s;
    char tb6_name [] = "TB_Pixels_S6";
    
    nscan_return = 0;
    nfov_return = 0;

    if( DEBUG_FLAG == 1 ) cout << tb6_name << endl;
    tb6s = get_data_ushort_2d_from_group( group_id, tb6_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb6s[0][0]=" << tb6s[0][0] << endl;
      cout << "tb6s[0][1]=" << tb6s[0][1] << endl;
      cout << endl;
    }
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // Scan_Gain                 32-bit floating-point,    4263 x 6
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **scan_gains;
    char scan_gain_name [] = "Scan_Gain";
    
    nscan_return = 0;
    nfov_return = 0;
    
    if( DEBUG_FLAG == 1 ) cout << scan_gain_name << endl;
    scan_gains = get_data_float_2d_from_group( group_id, scan_gain_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "scan_gains[0][0]=" << scan_gains[0][0] << endl;
      cout << "scan_gains[0][1]=" << scan_gains[0][1] << endl;      
      cout << endl;
    }
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // Scan_Offset                 32-bit floating-point,    4263 x 6
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **scan_offsets;
    char scan_offset_name [] = "Scan_Offset";
    
    nscan_return = 0;
    nfov_return = 0;
    
    if( DEBUG_FLAG == 1 ) cout << scan_offset_name << endl;
    scan_offsets = get_data_float_2d_from_group( group_id, scan_offset_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "scan_offsets[0][0]=" << scan_offsets[0][0] << endl;
      cout << "scan_offsets[0][1]=" << scan_offsets[0][1] << endl;
      cout << endl;
    }
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // SAPHIR_QF_scan                   16-bit unsigned integer,    3394
    ////////////////////////////////////////////////////////////////////////////////////////////
    unsigned short *qscans;
    char qscan_name [] = "SAPHIR_QF_scan";
    nscan_return = 0;
    if( DEBUG_FLAG == 1 ) cout << qscan_name << endl;
    qscans = get_data_ushort_1d_from_group( group_id, qscan_name, nscan_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "qscans[0]=" << qscans[0] << endl;
      cout << "qscans[nscan_return-1]=" << qscans[nscan_return-1] << endl;
      cout << endl;
    }
    
    // retrive passing mode information ( 0-ascending, 1-descending )
    int nodes[nscan_return];
    for(int iscan=0; iscan<nscan_return; iscan++ ) {
      unsigned short qscan = qscans[iscan];
      
      //int mode = get_bit(qscan,14);
      int mode = ( 0x4000 & qscan ) >> 14;
      nodes[iscan] = mode;
      
      //if( mode == 1 ) cout << "mode=" << mode << endl;
      //if( mode == 0 ) cout << "mode=" << mode << endl;
      //cout << "mode=" << mode << endl;
    }


    

    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Close group and file
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           

    H5Gclose( group_id ) ;
    H5Gclose( root_id ) ;
    H5Fclose( file_id ) ;
    



    
    

    ////////////////////////////////////////////////////////////////////////////////////////////////           
    //
    // Output section
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////           
    int nscan = NSCAN;
    
    MeasurementTypeHeader  header(nscan,NCHAN,NFOV,NQC);
    for( int ichan=0; ichan<NCHAN; ichan++) {
        header.freq[ichan]  = freqs[ichan];
        header.polar[ichan] = polars[ichan];
    } 

    // open TDR file for output 
    ofstream outFile(file_TDR, ios::out);

    // header part, so that it can be read out by fortran subroutine ReadRadHdrScanLMode_ascii 
    writeMeasurementHdr(outFile, header);
    

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
    	
        
        if( node != nodes[iscan] && DEBUG_FLAG == 1 ) cout << "node=" << node << ",  nodes[iscan]=" << nodes[iscan] << endl;
        
	//ms.node = node;
	ms.node = nodes[iscan];
	
	ms.year = years[iscan];
	ms.jday = jdays[iscan];
	ms.secs = secs[iscan];
	
	// lat/lon/angle/relAziAngle/solZenAngle
	for(int ifov=0;ifov<NFOV;ifov++) {
	    ms.lat[ifov]         = lats[iscan][ifov];
	    ms.lon[ifov]         = lons[iscan][ifov];
	    ms.angle[ifov]       = angles[iscan][ifov];
            if( ifov <= NFOV/2  &&  ms.angle[ifov] > 0 ) ms.angle[ifov] = -1.0 * ms.angle[ifov];  // 1-65 to negative values
	    ms.relAziAngle[ifov] = MISSING;
	    ms.solZenAngle[ifov] = MISSING;
	}
    
	// tb
	for(int ifov=0;ifov<NFOV;ifov++) {
          ms.tb[ifov][0] = tb1s[iscan][ifov];
          ms.tb[ifov][1] = tb2s[iscan][ifov];
          ms.tb[ifov][2] = tb3s[iscan][ifov];
          ms.tb[ifov][3] = tb4s[iscan][ifov];
          ms.tb[ifov][4] = tb5s[iscan][ifov];
          ms.tb[ifov][5] = tb6s[iscan][ifov];
	}
	
	// write measurement content
	writeMeasurement( outFile, ms );	  
	
    }

    outFile.close();




    ////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // free up all memory to prevent memory leak
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete lats[iscan] ;
      lats[iscan] = NULL;
    }
    delete [] lats ;
    lats = NULL;


    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete lons[iscan] ;
      lons[iscan] = NULL;
    }
    delete [] lons ;
    lons = NULL;
    
    // NOTE, times newed differently
    delete times[0];
    times[0] = NULL;
    delete [] times ;
    times = NULL;

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete angles[iscan] ;
      angles[iscan] = NULL;
    }
    delete [] angles ;
    angles = NULL;

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete tb1s[iscan] ;
      tb1s[iscan] = NULL;
    }
    delete [] tb1s ;
    tb1s = NULL;
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete tb2s[iscan] ;
      tb2s[iscan] = NULL;
    }
    delete [] tb2s ;
    tb2s = NULL;
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete tb3s[iscan] ;
      tb3s[iscan] = NULL;
    }
    delete [] tb3s ;
    tb3s = NULL;
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete tb4s[iscan] ;
      tb4s[iscan] = NULL;
    }
    delete [] tb4s ;
    tb4s = NULL;
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete tb5s[iscan] ;
      tb5s[iscan] = NULL;
    }
    delete [] tb5s ;
    tb5s = NULL;
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete tb6s[iscan] ;
      tb6s[iscan] = NULL;
    }
    delete [] tb6s ;
    tb6s = NULL;


    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete scan_gains[iscan] ;
      scan_gains[iscan] = NULL;
    }
    delete [] scan_gains ;
    scan_gains = NULL;
    

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete scan_offsets[iscan] ;
      scan_offsets[iscan] = NULL;
    }
    delete [] scan_offsets ;
    scan_offsets = NULL;

    delete [] qscans; 
    qscans = NULL;
    
    
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
  if( DEBUG_FLAG == 1) cout << "listRDR" << listRDR << "\n";
  
  string pathTDR;
  getline(cin,pathTDR);
  if( DEBUG_FLAG == 1) cout << "pathTDR" << pathTDR << "\n";
    
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
  char fileTDR[MAX_FILE_LEN];
  while( inFile.getline(fileRDR,MAX_FILE_LEN) ) {
    
    //if( DEBUG_FLAG == 1) cout << "RDR=" << fileRDR << endl;
    cout << "RDR=" << fileRDR << endl;
    
    strcpy(fileTDR, pathTDR_cstr);
    strcat(fileTDR, "TDR_");
    
    // get file base name part of fileRDR
    char *ptr = strrchr(fileRDR,'/');
    if( ptr != NULL ) {
      if( DEBUG_FLAG == 1) cout << "RDR basename=" << ptr+1 << endl;
      strcat(fileTDR, ptr+1);
    }
    else {
      if( DEBUG_FLAG == 1) cout << "RDR basename=" << fileRDR << endl;
      strcat(fileTDR, fileRDR);
    }
    
    //if( DEBUG_FLAG == 1) cout << "TDR=" << fileTDR << endl << endl;
    cout << "TDR=" << fileTDR << endl;
    
    
    // call sub to decode input RDR and generate output TDR
    rdr2tdr_saphir_TDR( fileRDR, fileTDR );
    
    cout << endl;
    
  }


  delete [] listRDR_cstr;
  delete [] pathTDR_cstr;
  
  return 0;
}





