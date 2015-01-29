#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * this file contains nice to know values 
 */

/*
 * HEADER:-1:pds_fcst_time:inv:0:fcst_time(1) in units given by pds
 */
int f_pds_fcst_time(ARG0) {
    unsigned int p;
    if (mode >= 0) {
        p = forecast_time_in_units(sec);
	if (p != 0xffffffff) sprintf(inv_out,"pds_fcst_time1=%u", p);
    }
    return 0;
}


int number_of_forecasts_in_the_ensemble(unsigned char **sec) {
    int pdt, n;

    pdt = code_table_4_0(sec);
    switch(pdt) {
	case 1:
	case 11:
		n = sec[4][36]; break;
	case 2:
	case 3:
	case 4:
	case 12:
	case 13:
		n = sec[4][35]; break;
	case 41:
	case 43:
		n = sec[4][38]; break;
	default: n=-1; break;
    }
    return n;
}

int perturbation_number(unsigned char **sec) {
    int pdt, n;

    pdt = code_table_4_0(sec);
    switch(pdt) {
	case 1:
	case 11:
		n = sec[4][35]; break;
	case 41:
	case 43:
		n = sec[4][37]; break;
	default: n = -1; break;
    }
    return n;
}

unsigned int forecast_time_in_units(unsigned char **sec) {

    int p;
    p = code_table_4_0(sec);
    if (p <= 15 || p == 1000 || p == 1001 || p == 1002 || p == 1100 || p == 1101)  {
        return uint4(sec[4]+18);
    }
    if (p >= 40 && p <= 43)  {
        return uint4(sec[4]+20);
    }
    return 0xffffffff;
}


void fixed_surfaces(unsigned char **sec, int *type1, float *surface1, 
	int *undef_val1, int *type2, float *surface2, int *undef_val2) {

    int pdt;
    unsigned char *p1, *p2;
    pdt = code_table_4_0(sec);
    *undef_val1 = *undef_val2 = 1;
    *surface1 = *surface2 = UNDEFINED;
    *type1 = *type2 = 255;

    switch (pdt) {
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 1100:
    case 1101:
	p1 = sec[4]+22; p2 = sec[4]+28; break;
    case 40:
    case 41:
    case 42:
    case 43:
	p1 = sec[4]+24; p2 = sec[4]+30; break;
    case 20:
    case 30:
    case 31:
    case 1000:
    case 1001:
    case 1002:
    case 254:
	return; break;
    default: 
	fprintf(stderr,"levels: product definition template #%d not supported\n", pdt);
	return;
	break;
    }

    if (*p1 != 255) {
	*type1 = *p1;
        if (p1[1] != 255) {
	    if (p1[2] != 255 || p1[3] != 255 || p1[4] != 255 || p1[5] != 255) {
		*undef_val1 = 0;
                *surface1 = scaled2flt(INT1(p1[1]), int4(p1+2));
	    }
	}
    }
    if (*p2 != 255) {
	*type2 = *p2;
        if (p2[1] != 255) {
	    if (p2[2] != 255 || p2[3] != 255 || p2[4] != 255 || p2[5] != 255) {
		*undef_val2 = 0;
                *surface2 = scaled2flt(INT1(p2[1]), int4(p2+2));
	    }
	}
    }
    return ;
}

int background_generating_process_identifier(unsigned char **sec) {
    unsigned char *p;
    p = background_generating_process_identifier_location(sec);
    if (p) return (int) *p;
    return -1;
}
unsigned char *background_generating_process_identifier_location(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p <= 15 || p == 1000 || p == 1001 || p == 1002 || p == 1100 || p == 1101)
        return sec[4]+12;
    if ( (p >= 40 && p <= 43) )
        return sec[4]+14;
    return NULL;
}


int analysis_or_forecast_generating_process_identifier(unsigned char **sec) {
    unsigned char *p;
    p = analysis_or_forecast_generating_process_identifier_location(sec);
    if (p) return (int) *p;
    return -1;
}
unsigned char *analysis_or_forecast_generating_process_identifier_location(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p <= 15 || p == 1000 || p == 1001 || p == 1002 || p == 1100 || p == 1101)
        return sec[4]+13;
    if ( (p >= 40 && p <= 43) )
        return sec[4]+15;
    return NULL;
}

int hours_of_observational_data_cutoff_after_reference_time(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p <= 15 || p == 1000 || p == 1001 || p == 1002 || p == 1100 || p == 1101)
        return int2(sec[4]+14);
    return -1;
}

int minutes_of_observational_data_cutoff_after_reference_time(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p <= 15 || p == 1000 || p == 1001 || p == 1002 || p == 1100 || p == 1101)
        return int1(sec[4]+16);
    return -1;
}


int observation_generating_process_identifier(unsigned char **sec) {
    int p;
    p = GB2_ProdDefTemplateNo(sec);
    if (p == 30 || p == 31)
        return (int) sec[4][12];
    return -1;
}


/*
 * get substitute umissing value
 *
 * returns number of missing values
 */

int sub_missing_values(unsigned char **sec, float *missing1, float *missing2) {
    int i, j;
    unsigned char *p;

    i = code_table_5_5(sec);
    if (i < 1 || i > 2) return 0;
    j = code_table_5_1(sec);
    p = sec[5];
    if (j == 0) {		// ieee
	if (p[23] == 255 && p[24] == 255 && p[25] == 255 && p[26] == 255) *missing1 = UNDEFINED;
	else *missing1 = ieee2flt(p+23);
	if (i == 2) {
	    if (p[27] == 255 && p[28] == 255 && p[29] == 255 && p[30] == 255) *missing1 = UNDEFINED;
	    else *missing2 = ieee2flt(p+27);
	}
    }
    else if (j == 1) {		// integer
	if (p[23] == 255 && p[24] == 255 && p[25] == 255 && p[26] == 255) *missing1 = UNDEFINED;
	else *missing1 = (float) int4(p+23);
	if (i == 2) {
	    if (p[27] == 255 && p[28] == 255 && p[29] == 255 && p[30] == 255) *missing1 = UNDEFINED;
	    else *missing2 = (float) int4(p+27);
	}
    }
    return i;
}


int stat_proc_verf_time(unsigned char **sec, int *year, int *month, int *day, int *hour, int *minute, int *second)
{
    int i, j;
    i = code_table_4_0(sec);
    j = 0;
    if (i == 8) j = 34;
    else if (i == 9) j = 47;
    else if (i == 10) j = 35;
    else if (i == 11) j = 37;
    else if (i == 12) j = 36;
    else if (i == 13) j = 68;
    else if (i == 14) j = 64;

    if (j) {
	get_time(sec[4]+j, year, month, day, hour, minute, second);
	return 0;
    }
    else {
	*year = *month = *day = *hour = *minute = *second = 0;
    }
    return 1;
}
	
	

