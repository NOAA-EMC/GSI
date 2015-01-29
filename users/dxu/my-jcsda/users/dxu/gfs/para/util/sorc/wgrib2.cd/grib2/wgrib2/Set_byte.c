#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * set_byte
 *
 * routine to modify grib fields
 *
 * 3/2008 Public Domain by Wesley Ebisuzaki
 *
 */

/*
 * HEADER:100:set_byte:misc:3:set bytes in Section X, location Y (1..N), bytes Z (a|a:b:c)
 */

int f_set_byte(ARG3) {

    int i, j, k, m, val, seclen;
    if (mode < 0) {
        i = atoi(arg1);
	if (i < 0 || i > 8) fatal_error("set_byte - bad section number %s", arg1);
	return 0;
    }

    i = atoi(arg1);
    j = atoi(arg2);

    if (i == 0) seclen = GB2_Sec0_size;
    else if (i == 8)  seclen = GB2_Sec8_size;
    else seclen = uint4(sec[i]);

    k = sscanf(arg3, "%d%n", &val, &m);
    while (k == 1) {
	if (j > seclen) fatal_error("set_byte out of bounds section %s",arg1);
	sec[i][j++ -1] = val;
	arg3 += m;
        k = sscanf(arg3, ":%d%n", &val, &m);
    }
    return 0;
}

/*
 * get_byte
 *
 * routine to view bytes
 *
 * 5/2009 Public Domain by Wesley Ebisuzaki
 *
 */


/*
 * HEADER:100:get_byte:inv:3:get bytes in Section X, location Y (1..N), number of bytes Z
 */

int f_get_byte(ARG3) {

    int i, j, k, m, seclen;
    double tot;

    if (mode < 0) {
        i = atoi(arg1);
        if (i < 0 || i > 8) fatal_error("get_byte - bad section number %s", arg1);
        return 0;
    }

    i = atoi(arg1);
    j = atoi(arg2);
    k = atoi(arg3);

    if (i == 0) seclen = GB2_Sec0_size;
    else if (i == 8)  seclen = GB2_Sec8_size;
    else seclen = uint4(sec[i]);

    if (k+j-1 > seclen) fatal_error("get_byte - query out of range","");

    // check output size first
    tot = 0;
    for (m = 1; m < k; m++) {
	if (sec[i][j+m-1] > 99) tot +=4.0;
	else if (sec[i][j+m-1] > 9) tot +=3.0;
	else tot += 2.0;
    }
    if (tot > INV_BUFFER - 1000) {
	sprintf(inv_out,"%d-%d=too many numbers",i,j);
	return 0;
    }

    sprintf(inv_out,"%d-%d=%d",i,j,sec[i][j-1]);
    for (m = 1; m < k; m++) {
	inv_out += strlen(inv_out);
	sprintf(inv_out,",%d",sec[i][j+m-1]);
    }
    return 0;
}

/*
 * HEADER:100:get_int:inv:3:get ints in Section X, location Y (byte), number of ints Z
 */

int f_get_int(ARG3) {
    int i, j, k, m, seclen, len;
    double tot;

    if (mode < 0) {
        i = atoi(arg1);
        if (i < 0 || i > 8) fatal_error("get_byte - bad section number %s", arg1);
        return 0;
    }

    i = atoi(arg1);
    j = atoi(arg2);
    k = atoi(arg3);

    if (i == 0) seclen = GB2_Sec0_size;
    else if (i == 8)  seclen = GB2_Sec8_size;
    else seclen = uint4(sec[i]);

    if (4*k+j-1 > seclen) fatal_error("get_int - query out of range","");
    sprintf(inv_out,"%d-%d=%d",i,j,int4(sec[i]+j-1));
    tot = (len = strlen(inv_out));
    inv_out += len;

    for (m = 1; m < k; m++) {
        sprintf(inv_out,",%d", int4(sec[i]+j-1+4*m));
        tot += (len = strlen(inv_out));
        inv_out += len;
	if (tot > INV_BUFFER - 1000) {
            sprintf(inv_out,"... too many numbers");
 	    return 0;
	}
    }
    return 0;
}
