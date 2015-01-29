#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

#ifdef USE_IPOLATES

/* mk_kgds
 *
 * this routine takes the *sec[] and makes a grib-1 kgds(*) array for ipolates library
 *
 * the kgds(*) is defined by ncep w3lib (w3fi63)
 *
 * problems: 
 *           kgds() only stores angles to 1e-3 which is less that grib2 (default 1e-6)
 *           ipolates uses fixed earth radius except for polar stereographic routine
 * solution: 
 *           minimize use of ipolates
 *           try to keep everything in double precision
 *           scale radius to radius defined by grib2 metadata
 *
 * INPUT:
 *
 * unsigned char **sec = grib2 message
 *     only use sec[3][*]
 *
 * OUTPUT:
 *
 * int kgds[200]
 *     grib1 grid descriptor as defined by w3fi73.f (NCEP's w3lib library)
 *
 *
 * public domain 6/2010 Wesley Ebisuzaki
 *
 * support input lat-lon, gaussian, lambert, mercator
 */


/* RADIUS_EARTH_IPOLATES is the radius of the earth as used by the IPOLATES library */
/* to make ipolates work with other radius .. scale distances */

#define RADIUS_EARTH_IPOLATES	6371200.0
#define IP_FACTOR (RADIUS_EARTH_IPOLATES/radius)


int mk_kgds(unsigned char **sec, int *kgds) {

    int i, gdt, basic_ang, sub_ang;
    unsigned char *gds;
    double radius, lat1, lat2, dlat, lon1, lon2, dlon, units;

    int nx, ny, res, scan;
    unsigned int npnts;


    get_nxny(sec, &nx, &ny, &npnts, &res, &scan);
    if (scan & 16) fatal_error_i("mk_kgds: unsupported scan mode %d", scan);
    radius = radius_earth(sec);
    gds = sec[3];

    for (i = 0; i < 200; i++) kgds[i] = 0;

    // encode the input grid with grib1 kgds
    gdt = code_table_3_1(sec);

    if (gdt == 0 && nx > 0 && ny > 0) {		// lat-lon grid, not thinned
	basic_ang  = GDS_LatLon_basic_ang(gds);
        sub_ang = GDS_LatLon_sub_ang(gds);
        if (basic_ang != 0) {
            units = (double) basic_ang / (double) sub_ang;
	}
	else {
            units = 0.000001;
        }
	dlat = GDS_LatLon_dlat(gds) * units;
	dlon = GDS_LatLon_dlon(gds) * units;
	lat1 = GDS_LatLon_lat1(gds) * units;
	lat2 = GDS_LatLon_lat2(gds) * units;
	lon1 = GDS_LatLon_lon1(gds) * units;
	lon2 = GDS_LatLon_lon2(gds) * units;

	kgds[0]= 0;
	kgds[1]= nx;
	kgds[2]= ny;
	kgds[3]= floor(lat1*1000.0+0.5);
	kgds[4]= floor(lon1*1000.0+0.5);
	kgds[5]= 128;			// resolution flag - winds N/S
	kgds[6]= floor(lat2*1000.0+0.5);
	kgds[7]= floor(lon2*1000.0+0.5);
	kgds[8]= floor(dlon*1000.0+0.5);
	kgds[9]= floor(dlat*1000.0+0.5);
	kgds[10]= scan;
	kgds[18] = 0;			// number of vert par
	kgds[19] = 255;			// number of vert par
    }
    else if (gdt == 10 && nx > 0 && ny > 0) {		// mercator, not thinned
	dlat     = GDS_Mercator_dy(gds);
	dlon     = GDS_Mercator_dx(gds);
	lat1 = GDS_Mercator_lat1(gds);
	lat2 = GDS_Mercator_lat2(gds);
	lon1 = GDS_Mercator_lon1(gds);
	lon2 = GDS_Mercator_lon2(gds);
	kgds[0]= 1;
	kgds[1]= nx;
	kgds[2]= ny;
	kgds[3]= floor(lat1*1000.0+0.5);
	kgds[4]= floor(lon1*1000.0+0.5);
	kgds[5]= 128;			// resolution flag - winds N/S
	kgds[6]= floor(lat2*1000.0+0.5);
	kgds[7]= floor(lon2*1000.0+0.5);
	kgds[8] = floor(GDS_Mercator_latD(gds)*1000+0.5);
	kgds[9]= 0;
	kgds[10]= scan;
	kgds[11]=  floor(dlon + 0.5);
	kgds[12]=  floor(dlat + 0.5);
    }
    else if (gdt == 30 && nx > 0 && ny > 0) {	// lambert conformal
        lat1   = GDS_Lambert_La1(gds);
        lon1   = GDS_Lambert_Lo1(gds);
	kgds[0]= 3;				// lambert conformal
	kgds[1]= nx;
	kgds[2]= ny;
	kgds[3]= floor(lat1*1000.0+0.5);
	kgds[4]= floor(lon1*1000.0+0.5);
	kgds[5]= 128 + (flag_table_3_3(sec) & 8);		// resolution flag - winds same as in sec
	kgds[6]= floor(GDS_Lambert_Lov(gds)*1000+0.5);		// LOV
	kgds[7]= floor(GDS_Lambert_dx(gds)*IP_FACTOR + 0.5);
	kgds[8]= floor(GDS_Lambert_dy(gds)*IP_FACTOR + 0.5);
	kgds[9]= GDS_Lambert_NP(gds) ? 0 : 128;
	kgds[10]= scan;
	kgds[11]= floor(GDS_Lambert_Latin1(gds)*1000.0+0.5);
	kgds[12]= floor(GDS_Lambert_Latin2(gds)*1000.0+0.5);
    }
    else if (gdt == 40 && nx > 0 && ny > 0) {			// gaussian, not thinned
	basic_ang = GDS_Gaussian_basic_ang(gds);
        sub_ang = GDS_Gaussian_sub_ang(gds);
        units = basic_ang == 0 ? 0.000001 : (double) basic_ang / (double) sub_ang;
        lat1 = GDS_Gaussian_lat1(gds) * units;
        lat2 = GDS_Gaussian_lat2(gds) * units;
        lon1 = GDS_Gaussian_lon1(gds) * units;
        lon2 = GDS_Gaussian_lon2(gds) * units;

	kgds[0]= 4;				// gaussian grid
	kgds[1]= nx;
	kgds[2]= ny;
	kgds[3]= floor(lat1*1000.0+0.5);
	kgds[4]= floor(lon1*1000.0+0.5);
	kgds[5]= 128;					// resolution flag - winds N/S
	kgds[6]= floor(lat2*1000.0+0.5);
	kgds[7]= floor(lon2*1000.0+0.5);
	if (lon2 < lon1) lon2 += 360.0;
	kgds[8]= floor((lon2-lon1)/(nx-1)*1000.0+0.5);
	kgds[9]= ny/2;
	kgds[10]= scan;
	kgds[11]= 0;
	kgds[12]= 255;
    }
    else {
	fatal_error_i("mk_kgds: unsupported input grid code table 3.1=%d", gdt);
    }
    return 0;
}

#endif
