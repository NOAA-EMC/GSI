#include <iostream>
#include <fstream>
#include <memory>
#include <new>
#include <iomanip>
#include <cmath>
#include <ctime>
#include <cstring>
#include <stdlib.h>
#include "hdf5.h"

using namespace std;

// any non-1 value would distable print out of information
#define DEBUG_FLAG 0

#define MISSING -999.0

#define MAX_FILE_NUM 8192
#define MAX_FILE_LEN 512


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
