/*******************************************************************************
 * 
 * This is the C++ decoder for Megha-Tropiques sensor MADRAS L1A2
 *
 * Author: Wanchun Chen <Wanchun.Chen@noaa.gov>
 * 
 *  For too large file, default stack size might be too small. Do this
 *         
 *              ulimit -s unlimited 
 * or
 *              limit stacksize unlimited 
 *
 * Date: 11/29/2012 
 *
 ******************************************************************************/

#include "mt.h"

// those functions are implemented in mt.cc

unsigned short *get_data_ushort_1d_from_group(hid_t, char *, int & );
float **get_data_ushort_2d_from_group(hid_t, char *, int &, int & );
float **get_data_float_2d_from_group(hid_t, char *, int &, int & );
char **get_data_string_2d_from_group(hid_t, char *, int &, int & );
float **get_data_uchar_2d_from_group(hid_t, char *, int &, int & );
int getJday( int year, int month, int day );
int writeMeasurementHdr(ofstream &myFile, MeasurementTypeHeader& header);
int writeMeasurement(ofstream &myFile, MeasurementType& measurement);


////////////////////////////////////////////////////////////////////////////////
// global variables
////////////////////////////////////////////////////////////////////////////////

const int NCHAN = 9;
const int NFOV = 214;
const int NQC = 4;

int NSCAN = 0;

float freqs[NCHAN] = { 18.70,18.70,23.80,36.50,36.50,89.0,89.0,157.0,157.0 };
int polars[NCHAN] = {4,5,4,4,5,4,5,4,5};


/*******************************************************************************
 * input argument:  file_RDR  - Raw HDF5 RDR input file
 * output arguments: file_TDR  -  output TDR file in text format
 ******************************************************************************/
int rdr2tdr_madras_TDR( char *file_RDR, char *file_TDR ) {
    
    ////////////////////////////////////////////////////////////////////////////
    //
    // Open file, root, group
    // 
    ////////////////////////////////////////////////////////////////////////////
    hid_t file_id     = H5Fopen(file_RDR, H5F_ACC_RDWR,  H5P_DEFAULT);
    hid_t root_id     = H5Gopen1(file_id, "/");
    hid_t group_id    = H5Gopen1(root_id, "ScienceData");
    

    /**
    IncidenceAngle_Pixels_LF (2750361)
    8-bit character,    2709 x 214
    Number of attributes = 13
        CLASS = IMAGE
        IMAGE_MINMAXRANGE = 10,49
        IMAGE_SUBCLASS = IMAGE_GRAYSCALE
        _FillValue = 127
        add_offset = 53.0
        comment = Angle between Zenith and line of sight
        dimension_label = Number of Scans, Number of Samples
        geolocation_label = Latitude_Pixel, Longitude_Pixel
        long_name = Incidence Angle of Low resolution samples
        scale_factor = 0.01
        standard_name = Incidence Angle for Sample Acquired
        units = degree
        valid_range = [51.72,54.27]
    */
    float **HFangles;
    char HFangle_name [] = "IncidenceAngle_Pixels_HF";
    int nscan_return = 0;
    int nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << HFangle_name << endl;
    HFangles = get_data_uchar_2d_from_group( group_id, HFangle_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "HFangles[0][0]=" << HFangles[0][0] << endl;
      cout << "HFangles[0][1]=" << HFangles[0][1] << endl;
      cout << endl;
    }



    /**
    Latitude_pixels (130126)
    16-bit unsigned integer,    2709 x 214
    Number of attributes = 12
        CLASS = IMAGE
        IMAGE_MINMAXRANGE = 1229,6775
        IMAGE_SUBCLASS = IMAGE_GRAYSCALE
        _FillValue = 65535
        add_offset = -40.0
        comment = Accuracy is better then 5km
        dimension_label = Number of Scans, Number of LF Samples
        long_name = Latitude of pixel
        scale_factor = 0.01
        standard_name = Latitude pixel
        units = degree
        valid_range = [-40.0, 40.0]
    */
    float **HFlats;
    char HFlat_name [] = "Latitude_pixels";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << HFlat_name << endl;
    HFlats = get_data_ushort_2d_from_group( group_id, HFlat_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "HFlats[0][0]=" << HFlats[0][0] << endl;
      cout << "HFlats[0][1]=" << HFlats[0][1] << endl;
      cout << endl;
    }


    ////////////////////////////////////////////////////////////////////////////////////////////
    // only assign global NSCAN once
    ////////////////////////////////////////////////////////////////////////////////////////////
    NSCAN = nscan_return;
    cout << "NSCAN=" << NSCAN << endl;



    /**
    Longitude_pixels (130398)
    16-bit unsigned integer,    2709 x 214
    Number of attributes = 12
        CLASS = IMAGE
        IMAGE_MINMAXRANGE = 0,35999
        IMAGE_SUBCLASS = IMAGE_GRAYSCALE
        _FillValue = 65535
        add_offset = 0.0
        comment = Accuracy is better then 5km
        dimension_label = Number of Scans, Number of LF Samples
        long_name = Longitude of Low resolution samples
        scale_factor = 0.01
        standard_name = Longitude Sample Acquired
        units = degree
        valid_range = [-40.0, 40.0]
    */
    float **HFlons;
    char HFlon_name [] = "Longitude_pixels";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << HFlon_name << endl;
    HFlons = get_data_ushort_2d_from_group( group_id, HFlon_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "HFlons[0][0]=" << HFlons[0][0] << endl;
      cout << "HFlons[0][1]=" << HFlons[0][1] << endl;
      cout << endl;
    }


    /**
    Scan_FirstSampleAcqTime_HF (129854)
    String, length = 25,    1 x 2709
    Number of attributes = 6
        _FillValue = yyyymmdd hhmmssuuuuu
        comment = Format: yyyymmdd hhmmssuuuuu
        dimension_label = Number of Scans
        long_name = LF :Date and time of first footprint acquried
        standard_name = time
        units = UTC time upto micorsecond level    
    */
    char **HFtimes;
    char HFtime_name [] = "Scan_FirstSampleAcqTime_HF";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << HFtime_name << endl;
    HFtimes = get_data_string_2d_from_group( group_id, HFtime_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "HFtimes[0]=" << HFtimes[0] << endl;      
      cout << "HFtimes[1]=" << HFtimes[1] << endl;
      cout << endl;
    }
    
    int HFyears[NSCAN];
    int HFjdays[NSCAN];
    int HFsecs[NSCAN];
    
    for(int iscan=0; iscan<nscan_return; iscan++ ) {
      char year_cstr[5];
      char month_cstr[3];
      char day_cstr[3];
      char hh_cstr[3];
      char mm_cstr[3];
      char ss_cstr[3];
      char us_cstr[7];  // micro second
      
      string time_string = string(HFtimes[iscan]);
	  
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
      
      HFyears[iscan] = year;
      HFjdays[iscan] = jday;
      HFsecs[iscan] = ms;
      
      
      if( DEBUG_FLAG == 1 ) {
        cout << "HFtime_string=" << time_string << endl;
        cout << "HFyear_string=" << year_string << endl;
        cout << "HFmonth_string=" << month_string << endl;
        cout << "HFday_string=" << day_string << endl;
        cout << "HFhh_string=" << hh_string << endl;
        cout << "HFmm_string=" << mm_string << endl;
        cout << "HFss_string=" << ss_string << endl;
        cout << "HFus_string=" << us_string << endl;
        
        cout << "HFyear=" << year << endl;
        cout << "HFjday=" << jday << endl;
        cout << "HFms=" << ms << endl;
        cout << endl;
        
      }
      
    }




    /**
    TB_Pixels_18.7_V               16-bit unsigned integer,  2709 x 214
    Number of attributes = 14
        CLASS = IMAGE
        IMAGE_MINMAXRANGE = 106,65465
        IMAGE_SUBCLASS = IMAGE_GRAYSCALE
        Quality_Flag = QF_Pixels_18.7V
        _FillValue = 65535
        add_offset = 0.0
        comment = TB estimated from raw insturment count
        dimension_label = Number of Scans, Number of LF Samples
        geolocation_label = Latitude_Pixel, Longitude_Pixel
        long_name = Sample Brightness Temperature of 18.7V channel
        scale_factor = 0.01
        standard_name = Brightness Temperature of Sample Acquired
        units = Kelvin
        valid_range = [0.0, 400.0]
    */
    float **tb1s;
    char tb1_name [] = "TB_Pixels_18.7_V";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << tb1_name << endl;
    tb1s = get_data_ushort_2d_from_group( group_id, tb1_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb_18.7_V[0][0]=" << tb1s[0][0] << endl;
      cout << "tb_18.7_V[0][1]=" << tb1s[0][1] << endl;
      cout << endl;
    }
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_18.7_H               16-bit unsigned integer,  2709 x 214  
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb2s;
    char tb2_name [] = "TB_Pixels_18.7_H";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << tb2_name << endl;
    tb2s = get_data_ushort_2d_from_group( group_id, tb2_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb_18.7_H[0][0]=" << tb2s[0][0] << endl;
      cout << "tb_18.7_H[0][1]=" << tb2s[0][1] << endl;
      cout << endl;
    }
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_23.8_V               16-bit unsigned integer,     2709 x 214
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb3s;
    char tb3_name [] = "TB_Pixels_23.8_V";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << tb3_name << endl;
    tb3s = get_data_ushort_2d_from_group( group_id, tb3_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb_23.8_V[0][0]=" << tb3s[0][0] << endl;
      cout << "tb_23.8_V[0][1]=" << tb3s[0][1] << endl;
      cout << endl;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_36.5_V               16-bit unsigned integer,     2709 x 214
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb4s;
    char tb4_name [] = "TB_Pixels_36.5_V";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << tb4_name << endl;
    tb4s = get_data_ushort_2d_from_group( group_id, tb4_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb_36.5_V[0][0]=" << tb4s[0][0] << endl;
      cout << "tb_36.5_V[0][1]=" << tb4s[0][1] << endl;
      cout << endl;
    }
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_36.5_H               16-bit unsigned integer,     2709 x 214
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb5s;
    char tb5_name [] = "TB_Pixels_36.5_H";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << tb5_name << endl;
    tb5s = get_data_ushort_2d_from_group( group_id, tb5_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb_36.5_H[0][0]=" << tb5s[0][0] << endl;
      cout << "tb_36.5_H[0][1]=" << tb5s[0][1] << endl;
      cout << endl;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_89.0_V               16-bit unsigned integer,     2709 x 214
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb6s;
    char tb6_name [] = "TB_Pixels_89.0_V";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << tb6_name << endl;
    tb6s = get_data_ushort_2d_from_group( group_id, tb6_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb_89.0_V[0][0]=" << tb6s[0][0] << endl;
      cout << "tb_89.0_V[0][1]=" << tb6s[0][1] << endl;
      cout << endl;
    }
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_89.0_H               16-bit unsigned integer,     2709 x 214
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb7s;
    char tb7_name [] = "TB_Pixels_89.0_H";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << tb7_name << endl;
    tb7s = get_data_ushort_2d_from_group( group_id, tb7_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb_89.0_H[0][0]=" << tb7s[0][0] << endl;
      cout << "tb_89.0_H[0][1]=" << tb7s[0][1] << endl;
      cout << endl;
    }
	
    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_157.0_V               16-bit unsigned integer,     2709 x 214
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb8s;
    char tb8_name [] = "TB_Pixels_157.0_V";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << tb8_name << endl;
    tb8s = get_data_ushort_2d_from_group( group_id, tb8_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb_157.0_V[0][0]=" << tb8s[0][0] << endl;
      cout << "tb_157.0_V[0][1]=" << tb8s[0][1] << endl;
      cout << endl;
    }
    
    ////////////////////////////////////////////////////////////////////////////////////////////
    // TB_Pixels_157.0_H               16-bit unsigned integer,     2709 x 214
    ////////////////////////////////////////////////////////////////////////////////////////////
    float **tb9s;
    char tb9_name [] = "TB_Pixels_157.0_H";
    nscan_return = 0;
    nfov_return = 0;
    if( DEBUG_FLAG == 1 ) cout << tb9_name << endl;
    tb9s = get_data_ushort_2d_from_group( group_id, tb9_name, nscan_return, nfov_return );
    if( DEBUG_FLAG == 1 ) {
      cout << "nscan_return=" << nscan_return << endl;
      cout << "nfov_return=" << nfov_return << endl;
      cout << "tb_157.0_H[0][0]=" << tb9s[0][0] << endl;
      cout << "tb_157.0_H[0][1]=" << tb9s[0][1] << endl;
      cout << endl;
    }


    /**
    MADRAS_QF_scan (1952)
    16-bit unsigned integer,    2709
    Number of attributes = 4
        comment = 16-bits_array(=0:good/=1:bad): #15:Scan_validity_flag #14:pass_type #13:scanning_type 
	#12:scan/row_error #11:datation_error #10:PRT_error #9:encoder_error #8:MADRAS_correction_flag 
	#7-6:blank #5-3:Payload_mode #2-0:Satellite_mode
        dimension_label = Number of Scans
        geolocation_label = Scan First Pixel Acqired time
        long_name = Quality Flag Applicable to the Scan Line
    */
    unsigned short *qscans;
    char qscan_name [] = "MADRAS_QF_scan";
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
      //int mode = ( qscan & ( 1 << 14 ) ) >> 14;
      int mode = ( 0x4000 & qscan ) >> 14;
      
      nodes[iscan] = mode;
      
      //if( mode == 1 ) cout << "mode=" << mode << endl;
      //if( mode == 0 ) cout << "mode=" << mode << endl;
      //cout << "mode=" << mode << endl;
    }




    ////////////////////////////////////////////////////////////////////////////           
    //
    // Close group and file
    //
    ////////////////////////////////////////////////////////////////////////////           

    H5Gclose( group_id ) ;
    H5Gclose( root_id ) ;
    H5Fclose( file_id ) ;



    if( DEBUG_FLAG == 1 ) cout << "Finish reading, now output" << endl;

    ////////////////////////////////////////////////////////////////////////////           
    //
    // Output section
    //
    ////////////////////////////////////////////////////////////////////////////           
    int nscan = NSCAN;
    
    MeasurementTypeHeader header(nscan,NCHAN,NFOV,NQC);
    for( int ichan=0; ichan<NCHAN; ichan++) {
        header.freq[ichan]  = freqs[ichan];
        header.polar[ichan] = polars[ichan];
    } 

    // open TDR file for output 
    ofstream outFile(file_TDR, ios::out);

    // header part, so that it can be read out by fortran subroutine ReadRadHdrScanLMode_ascii 
    writeMeasurementHdr(outFile, header);
    

    int node=0;
    if ( HFlats[1][0]  >= HFlats[0][0] ) node = 0;
    else                                 node = 1;
    
    MeasurementType ms(NCHAN,NFOV,NQC);
    
    for( int iscan=0; iscan<nscan; iscan++ ) {
            
        // Default QC flags to zero
        for(int iqc=0; iqc<NQC; iqc++) {
            ms.qc[iqc]=0;
        }

        if ( iscan >= 1 ) {
            if ( HFlats[iscan][0] > HFlats[iscan-1][0] )  node = 0;
            else                                          node = 1;
        }
            
	ms.node = nodes[iscan];
        ms.year = HFyears[iscan];
        ms.jday = HFjdays[iscan];
        ms.secs = HFsecs[iscan];
        
        // lat/lon/angle/relAziAngle/solZenAngle
        for( int ifov=0; ifov<NFOV; ifov++ ) {
          ms.lat[ifov]         = HFlats[iscan][ifov];
          ms.lon[ifov]         = HFlons[iscan][ifov];
          ms.angle[ifov]       = HFangles[iscan][ifov];
          ms.relAziAngle[ifov] = MISSING;
          ms.solZenAngle[ifov] = MISSING;
        }
    
        // tb
        for( int ifov=0; ifov<NFOV; ifov++ ) {
	  ms.tb[ifov][0] = tb1s[iscan][ifov];
          ms.tb[ifov][1] = tb2s[iscan][ifov];
	  ms.tb[ifov][2] = tb3s[iscan][ifov];
          ms.tb[ifov][3] = tb4s[iscan][ifov];
	  ms.tb[ifov][4] = tb5s[iscan][ifov];
          ms.tb[ifov][5] = tb6s[iscan][ifov];
	  ms.tb[ifov][6] = tb7s[iscan][ifov];
          ms.tb[ifov][7] = tb8s[iscan][ifov];
          ms.tb[ifov][8] = tb9s[iscan][ifov];
        }
        
        // write measurement content
        writeMeasurement( outFile, ms );          
        
    }

    outFile.close();

    if( DEBUG_FLAG == 1 ) cout << "Finish output, free up memory" << endl;


    //////////////////////////////////////////////////////////////////////////// 
    // 
    // free up all memory to prevent memory leak 
    // 
    //////////////////////////////////////////////////////////////////////////// 
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete HFangles[iscan] ;
      HFangles[iscan] = NULL;
    }
    delete [] HFangles ;
    HFangles = NULL;
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete HFlats[iscan] ;
      HFlats[iscan] = NULL;
    }
    delete [] HFlats ;
    HFlats = NULL;
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
      delete HFlons[iscan] ;
      HFlons[iscan] = NULL;
    }
    delete [] HFlons ;
    HFlons = NULL;
    
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
        delete tb7s[iscan] ; 
        tb7s[iscan] = NULL; 
    } 
    delete [] tb7s ;
    tb7s = NULL; 

    for(int iscan=0; iscan<NSCAN; iscan++ ) {
        delete tb8s[iscan] ; 
        tb8s[iscan] = NULL; 
    } 
    delete [] tb8s ;
    tb8s = NULL; 
    
    for(int iscan=0; iscan<NSCAN; iscan++ ) {
        delete tb9s[iscan] ; 
        tb9s[iscan] = NULL; 
    } 
    delete [] tb9s ;
    tb9s = NULL; 

    delete [] qscans; 
    qscans = NULL;
    
    // NOTE, times newed differently, so need delete differently
    delete HFtimes[0];
    HFtimes[0] = NULL;
    delete [] HFtimes ;
    HFtimes = NULL;
  
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
    
    cout << "RDR=" << fileRDR << endl;
    
    strcpy(fileTDR, pathTDR_cstr);
    strcat(fileTDR, "TDR_");
    
    // get file base name part of fileRDR
    char *ptr = strrchr(fileRDR,'/');
    if( ptr != NULL ) {
      if( DEBUG_FLAG == 1) 
          cout << "RDR basename=" << ptr+1 << endl;
      strcat(fileTDR, ptr+1);
    }
    else {
      if( DEBUG_FLAG == 1)
          cout << "RDR basename=" << fileRDR << endl;
      strcat(fileTDR, fileRDR);
    }
    
    cout << "TDR=" << fileTDR << endl ;
    
    // call sub to decode input RDR and generate output TDR
    rdr2tdr_madras_TDR( fileRDR, fileTDR );
    
    cout << endl;

  }
  
  delete [] listRDR_cstr;
  delete [] pathTDR_cstr;
  
  return 0;
}
