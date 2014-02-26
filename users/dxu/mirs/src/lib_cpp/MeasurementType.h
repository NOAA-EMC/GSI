#include <iostream>
#include <fstream>
#include <iomanip>

using namespace std;

class MeasurementTypeHeader {
public: 
	int nscan;
	int nchan;
	int nfov;
	int nqc;
	int nPosScan;
	float* freq; 
        int*  polar;
	
	MeasurementTypeHeader(int nscan_arg, int nchan_arg, int nfov_arg, int nqc_arg ) {
		nscan = nscan_arg;
		nchan = nchan_arg;
		nfov  = nfov_arg;
		nqc   = nqc_arg;
		freq  = new float[nchan];
		polar = new int[nchan];
	};
	/*
	~MeasurementTypeHeader() {
		delete [] freq;
		delete [] polar;
		freq  = 0;
		polar = 0;
	};
	*/
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
		
		tb =  new float*[nfov];
		for(int i = 0; i<nfov; i++ ) tb[i] = new float[nchan];
     	};
	/*
	~MeasurementType() {
		
		delete [] lat;
		delete [] lon;
		delete [] angle;
		delete [] relAziAngle;
		delete [] solZenAngle;
		delete [] qc;
		
		for(int i = 0; i<nfov; i++ ) delete [] tb[i];
		
		delete [] tb;
		
		lat         = 0;
		lon         = 0;
		angle       = 0;
		relAziAngle = 0;
		solZenAngle = 0;
		qc          = 0;
		tb          = 0;
	
	};
	*/
     
};

int writeMeasurementHdr(ofstream &myFile, MeasurementTypeHeader header) {

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



int writeMeasurement(ofstream &myFile, MeasurementType measurement) {
    
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
    
	// tb
	for(int ichan=0;ichan<NCHAN;ichan++) {
	for(int ifov=0;ifov<NFOV;ifov++) {
	    myFile << setprecision(2) << setw(8) << fixed << measurement.tb[ifov][ichan];
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
