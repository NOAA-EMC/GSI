#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Public Domain 2010: Wesley Ebisuzaki
 */

/*
 * HEADER:400:spatial_proc:inv:0:show spacial processing, pdt=4.15
 */
int f_spatial_proc(ARG0) {
    int val;
    char *string;
    if (mode >= 0 && GB2_ProdDefTemplateNo(sec) == 15) {
        val = code_table_4_10(sec);
        string = NULL;
        switch(val) {
#include "CodeTable_4.10.dat"
        }
        if (string) {
	   if (mode == 0) {
		sprintf(inv_out, "spatial %s", string);
	   }
	   else {
               sprintf(inv_out,"spatial filter=%s interpolate (table 4.15)=%d points=%d", string, 
		code_table_4_15(sec), sec[4][36]);
	   }
        } 
	else {
	   if (mode == 0) {
		sprintf(inv_out, "unspecified spatial filter");
	   }
	   else {
               sprintf(inv_out,"unspecified spatial filter interpolate (table 4.15)=%d points=%d", 
		code_table_4_15(sec), sec[4][36]);
	   }
	}
	inv_out += strlen(inv_out);
    }
    return 0;
}
