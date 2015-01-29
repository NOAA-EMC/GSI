#include <stdio.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * length of various sections
 * public domain 2007: Wesley Ebisuzaki
 */

/*
 * HEADER:700:Sec_len:inv:0:length of various grib sections
 */


int f_Sec_len(ARG0) {
    if (mode >= 0) {
        sprintf(inv_out,"Sec size msg=%lu", (unsigned long int) GB2_MsgLen(sec));
        inv_out += strlen(inv_out);

        sprintf(inv_out," id(1)=%u", uint4(sec[1]));
        inv_out += strlen(inv_out);

	if (sec[2] != NULL) 
           sprintf(inv_out," local(2)=%u", uint4(sec[2]));
	else
           sprintf(inv_out," local(2)=0");
        inv_out += strlen(inv_out);
	
        sprintf(inv_out," grid(3)=%u", uint4(sec[3]));
        inv_out += strlen(inv_out);

        sprintf(inv_out," product(4)=%u", uint4(sec[4]));
        inv_out += strlen(inv_out);

        sprintf(inv_out," data-rep(5)=%u", uint4(sec[5]));
        inv_out += strlen(inv_out);

	if (sec[5] != NULL) 
           sprintf(inv_out," bitmap(6)=%u", uint4(sec[6]));
	else
           sprintf(inv_out," bitmap(6)=0");
        inv_out += strlen(inv_out);

        sprintf(inv_out," data(7)=%u", uint4(sec[7]));
        inv_out += strlen(inv_out);
    }
    return 0;
}
