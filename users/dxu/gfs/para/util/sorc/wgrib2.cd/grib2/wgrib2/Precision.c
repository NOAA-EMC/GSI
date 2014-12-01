#include <stdio.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * HEADER:510:scaling:inv:0:scaling for packing (old format)
 */

int f_scaling(ARG0) {

    int dec, bin, nbits;
    double base;
    if (mode < 0) return 0;
    if (scaling(sec, &base, &dec, &bin, &nbits) == 0) {
       sprintf(inv_out,"scaling ref=%g dec_scale=%d bin_scale=%d nbits=%d", base, dec, bin, nbits);
    }
    return 0;
}

/*
 * HEADER:510:scale:inv:0:scale for packing
 */
int f_scale(ARG0) {

    int dec, bin, nbits;
    double base;
    if (mode < 0) return 0;
    if (scaling(sec, &base, &dec, &bin, &nbits) == 0) {
       sprintf(inv_out,"scale=%d,%d", dec, bin);
    }
    return 0;
}


int scaling(unsigned char **sec, double *base, int *decimal, int *binary, int *nbits) {
    int pack;
    unsigned char *p;

    pack = (int) code_table_5_0(sec);
    p = sec[5];
    if (pack == 0 || pack == 1 || pack == 2 || pack == 3 || pack == 40 || pack == 40000 || 
		pack == 50 || pack == 40010) {
       *base = ieee2flt(p+11);
       *binary = int2(p+15);
       *decimal = -int2(p+17);
       *nbits = p[19];
    }
    else {
      return 1;
    }
    return 0;
}

