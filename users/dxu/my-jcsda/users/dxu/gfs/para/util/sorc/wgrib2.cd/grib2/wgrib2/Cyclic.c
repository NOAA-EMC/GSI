#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

#define ERROR 0.0001

/*
 * HEADER:-1:cyclic:inv:0:is grid cyclic? (not for gaussian)
 */

int f_cyclic(ARG0) {
    if (mode >= 0) {
	sprintf(inv_out,cyclic(sec) ? "cyclic" : "not cyclic");
    }
    return 0;
}

int cyclic(unsigned char **sec) {
    int grid_template, nx, ny, res, scan, flag_3_3, no_dx, basic_ang, sub_ang;
    unsigned int npnts;
    unsigned char *gds;
    double dlon, units;

    get_nxny(sec, &nx, &ny, &npnts, &res, &scan);
    if ((unsigned) (nx * ny) != npnts) return 0;
    if (nx <= 0 || ny <= 0) return 0;

    grid_template = code_table_3_1(sec);
    gds = sec[3];

    flag_3_3 = flag_table_3_3(sec);
    no_dx =  0;
    if (flag_3_3 != -1) {
        if ((flag_3_3 & 0x20) == 0) no_dx = 1;
    }

    if (grid_template == 0) {

        basic_ang = GDS_LatLon_basic_ang(gds);
        sub_ang = GDS_LatLon_sub_ang(gds);
        units = basic_ang == 0 ?  0.000001 : (double) basic_ang / (double) sub_ang;

        dlon = units * GDS_LatLon_dlon(gds);
        if (no_dx) dlon = 0.0;

	dlon = nx * dlon;
	return (fabs(dlon-360.0) < ERROR);
    }

    if (grid_template == 10) {
        dlon = GDS_Mercator_dx(gds);
        if (no_dx) dlon = 0.0;
	dlon = nx * dlon;
	return (fabs(dlon-360.0) < ERROR);
    }

// need to add gaussian grid - a bit more complicated

    return 0;
}
