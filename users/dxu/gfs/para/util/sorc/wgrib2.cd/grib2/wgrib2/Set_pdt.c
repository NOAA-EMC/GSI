#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * the set options
 *
 * routines make a generic PDT
 *
 * 9/2008 Public Domain by Wesley Ebisuzaki
 *
 */

/*
 * HEADER:100:set_pdt:misc:1:makes new (clean) pdt, X=PDT_number or X=PDT_number:size of PDT in octets
 */


int f_set_pdt(ARG1) {

    int pdt, i, len;
    
    static unsigned char new_sec4[SET_PDT_SIZE];

    if (mode < 0) return 0;

    i = sscanf(arg1,"%d:%d", &pdt, &len);

    if (i == 0) fatal_error("set_pdt: X=PDT_number[:PDT_SIZE]","");

    if (i == 1) {	// use default PDT size

        switch(pdt) {

        case 0: len = 34; break;
        case 1: len = 37; break;
        case 2: len = 36; break;
        case 3: len = 68; break;
        case 4: len = 64; break;
        case 5: len = 48; break;
        case 6: len = 35; break;
        case 7: len = 34; break;
        case 8: len = 54; break;
        case 9: len = 67; break;
        case 10: len = 55; break;
        case 11: len = 57; break;
        case 12: len = 56; break;
        case 13: len = 88; break;
        case 14: len = 84; break;
        case 15: len = 37; break;
        case 20: len = 43; break;
        case 30: len = 14; break;
        case 31: len = 14; break;
        default: fatal_error_i("set_pdt: unsupported pdt=%d",pdt); break;
        }
    }

    if (len > SET_PDT_SIZE) fatal_error_i("set_pdt: maximum pdt len is %d", SET_PDT_SIZE);


    uint_char(len, new_sec4);		    // size of sec[4];
    new_sec4[4] = 4;                        // section number
    uint2_char(0, new_sec4+5);              // no extra coordinates
    uint2_char(pdt, new_sec4+7);            // pdt
    for (i = 9; i < len; i++) new_sec4[i] = 255;

    sec[4] = &(new_sec4[0]);
    return 0;
}
