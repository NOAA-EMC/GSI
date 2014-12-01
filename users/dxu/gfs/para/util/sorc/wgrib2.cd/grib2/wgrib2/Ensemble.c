#include <stdio.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * 2/2007 Public Domain: Wesley Ebisuzaki
 */

/*
 * HEADER:200:ens:inv:0:ensemble information
 */

int f_ens(ARG0) {
    int type, typefcst;
    if (mode >= 0) {
	typefcst = code_table_4_7(sec);
	type = code_table_4_6(sec);
	if (type >= 0) {
	    switch(type) {
	        case 0: sprintf(inv_out,"ENS=hi-res ctl"); break;
	        case 1: sprintf(inv_out,"ENS=low-res ctl"); break;
	        case 2: 
			sprintf(inv_out,"ENS=-%d", perturbation_number(sec)); break;
	        case 3: 
			sprintf(inv_out,"ENS=+%d", perturbation_number(sec)); break;
	        case 4: 
			sprintf(inv_out,"MM-ENS=%d", perturbation_number(sec)); break;
	        default:
			sprintf(inv_out,"ENS=? table4.6=%d pert=%d",type,perturbation_number(sec)); break;
	    }
	    inv_out += strlen(inv_out);
	    if (typefcst >= 0) {
		*inv_out++=' ';
		*inv_out=0;
	    }
	}
	if (typefcst >= 0) {
	    switch(typefcst) {
	        case 0: sprintf(inv_out,"ens-mean"); break;
	        case 1: sprintf(inv_out,"wt ens-mean"); break;
	        case 2: sprintf(inv_out,"std dev"); break;
	        case 3: sprintf(inv_out,"normalized std dev"); break;
	        case 4: sprintf(inv_out,"spread"); break;
	        case 5: sprintf(inv_out,"large anom index"); break;
	        case 6: sprintf(inv_out,"cluster mean"); break;
		default: sprintf(inv_out,"unknown derived fcst"); break;
	    }
	    inv_out += strlen(inv_out);
	}
    }
    return 0;
}

/*
 * HEADER:200:N_ens:inv:0:number of ensemble members
 */
int f_N_ens(ARG0) {
    unsigned int n;

    if (mode >= 0) {
	n = number_of_forecasts_in_the_ensemble(sec);
	if (n > 0) sprintf(inv_out,"%d ens members", n); 
    }
    return 0;
}
