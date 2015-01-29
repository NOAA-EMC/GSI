#include <stdio.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/* function to return the size of the earth */

double radius_earth(unsigned char **sec) {
    int table_3_2;
    double radius;
    unsigned char *p;
    int factor, value;

    p = code_table_3_2_location(sec);
    table_3_2 = (int) *p;

    /* set a default value .. not sure what to do with most values */
    radius = 6367.47 *1000.0;
    if (table_3_2 == 0) radius = 6367.47 * 1000.0;
    else if (table_3_2 == 1)  {
	factor = p[1];
	value = uint4(p+2);
	radius = scaled2dbl(factor, value);
	if (radius < 6300000.0 || radius > 6400000.0) 
	   fatal_error_i("radius of earth is %d m", (int) radius);
    }
    else if (table_3_2 == 2)  radius = (6378.160 + 6356.775)*0.5 * 1000.0;
    else if (table_3_2 == 3) {
	/* get major axis */
	factor = p[6];
	value = uint4(p+7);
	radius = scaled2dbl(factor, value);
	/* get minor axis */
	factor = p[11];
	value = uint4(p+12);
	radius = (radius + scaled2dbl(factor, value)) * 0.5;
        if (radius < 6300000.0 || radius > 6400000.0) 
	   fatal_error_i("radius of earth is %d m", (int) radius);
    }
    else if (table_3_2 == 4)  radius = (6378.137 + 6356.772)*0.5 * 1000.0;
    else if (table_3_2 == 6)  radius = 6371.2290 * 1000.0;
    else if (table_3_2 == 8)  radius = 6371.200 * 1000.0;
    return radius;
}

