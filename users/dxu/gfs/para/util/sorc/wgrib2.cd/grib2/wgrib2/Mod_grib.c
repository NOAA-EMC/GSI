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
 * routines to modify grib fields
 *
 * 3/2008 Public Domain by Wesley Ebisuzaki
 *
 */

extern int header, decode, free_sec4;
extern const char *level_table[192];
extern int use_scale, dec_scale, bin_scale;

/*
 * HEADER:100:set_lev:misc:1:changes level code .. not complete
 */

int f_set_lev(ARG1) {
    unsigned char *p;
    const char *t;
    float val1, val2;
    double dval1;
    int pdt, i, n, n_percent, len_arg1, ival, center;
    char string[100], *s;

    if (mode < 0) return 0;
    pdt = code_table_4_0(sec);
    len_arg1 = strlen(arg1);
    p = sec[4];

if (mode == 99) fprintf(stderr,"set_lev: pdt=%d arg=%s\n", pdt, arg1);

    if ( (pdt >= 0 && pdt <= 14) ) {
	for (i = 22; i <= 33; i++) p[i] = 255;

	for (i = 1; i < 192; i++) {
	    if (strcmp("reserved", level_table[i]) == 0) continue;

	    // count the number of % characters in the level table
            n_percent = 0;
	    t = level_table[i];
            while (*t) { if (*t++ == '%') n_percent++; }

	    if (n_percent == 0) {
	        if (strcmp(arg1, level_table[i]) == 0) {
 	            p[22] = i;
 	            return 0;
	        }
	    }

	    else if (n_percent == 1) {
		// add %n to the format given by level_table[i]
		strcpy(string,level_table[i]);
		strcat(string,"%n");
		if (n = -1, sscanf(arg1,string,&val1,&n), n == len_arg1) {
		    dval1 = (double) val1;
		    p[22] = i;
		    if (i == 100 || i == 108) dval1 = dval1 * 100; // convert mb to Pa

		    best_scaled_value(dval1, &n, &ival);
		    p[23] = INT1(n);
		    int_char(ival, p+24);
		    return 0;
		}
	    }
//	    else if (n_percent == 2) {
//	    }
        }

//	check for NCEP special levels
	ival = -1;
	s = arg1;
	center = GB2_Center(sec);
#include "NCEP_local_levels_test.h"
	if (ival >= 0) {
 	    p[22] = ival;
	    return 0;
	}


if (mode == 99) fprintf(stderr,"in set_lev arg1=%s\n", arg1);
         
	if (n=-1, sscanf(arg1,"%g-%g mb above ground%n",&val1,&val2,&n), n == len_arg1) {
	    val1 *= 100.0;
	    val2 *= 100.0;
	    p[22] = p[28] = 108;
	    p[23] = 0;
	    int_char((int) val1, p+24);
	    p[29] = 0;
	    int_char((int) val2, p+30);
            return 0;
	}
	if (n=-1, sscanf(arg1,"%g-%g mb%n",&val1,&val2,&n), n == len_arg1) {
	    val1 *= 100.0;
	    val2 *= 100.0;
	    p[22] = p[28] = 100;
	    p[23] = 0;
	    int_char((int) val1, p+24);
	    p[29] = 0;
	    int_char((int) val2, p+30);
            return 0;
	}
	if (n=-1, sscanf(arg1,"%g-%g m below ground%n",&val1,&val2,&n), n == len_arg1) {
	    p[22] = p[28] = 106;

	    best_scaled_value(val1, &n, &ival);
	    p[23] = INT1(n);
	    int_char(ival, p+24);

	    best_scaled_value(val2, &n, &ival);
	    p[29] = INT1(n);
	    int_char(ival, p+30);

            return 0;
	}
        if (n=-1, sscanf(arg1,"%g-%g m above ground%n",&val1,&val2,&n), n == len_arg1) {
            p[22] = p[28] = 103;

            best_scaled_value(val1, &n, &ival);
            p[23] = INT1(n);
            int_char(ival, p+24);

            best_scaled_value(val2, &n, &ival);
            p[29] = INT1(n);
            int_char(ival, p+30);

            return 0;
        }
        if (n=-1, sscanf(arg1,"%g-%g sigma layer%n",&val1,&val2,&n), n == len_arg1) {
            p[22] = p[28] = 104;

            best_scaled_value(val1, &n, &ival);
            p[23] = INT1(n);
            int_char(ival, p+24);

            best_scaled_value(val2, &n, &ival);
            p[29] = INT1(n);
            int_char(ival, p+30);

            return 0;
	}
        if (n=-1, sscanf(arg1,"%g-%g m below sea level%n",&val1,&val2,&n), n == len_arg1) {

            p[22] = p[28] = 160;
            best_scaled_value(val1, &n, &ival);
            p[23] = INT1(n);
            int_char(ival, p+24);

            best_scaled_value(val2, &n, &ival);
            p[29] = INT1(n);
            int_char(ival, p+30);
            return 0;
        }
	if (center == 7) {
            if (n=-1, sscanf(arg1,"%gC ocean isotherm level%n",&val1,&n), n == len_arg1) {
                p[22] = 235;
	        val1 *= 10.0;
                best_scaled_value(val1, &n, &ival);
                p[23] = INT1(n);
                int_char(ival, p+24);
                return 0;
	     }
	}

        if (strcmp("atmos col", arg1) == 0 ||	// wgrib2 compatible
            strcmp("Entire atmosphere (considered as a single layer)",
		arg1) == 0) {
            p[22] = 1;
            p[28] = 8;
            return 0;
        }

    }
    fatal_error("need to modify set_lev for %s", arg1);
    return 0;
}


/*
 * HEADER:100:set_date:misc:1:changes date code .. not complete .. only analysis/fcst
 */

int f_set_date(ARG1) {

    char *p;
    int year, month, day, hour, minute, second, i, j, units, n;
    int pdt;

    unsigned int dtime;

    if (mode == -1) {
	return 0;
    }
    if (mode < 0) return 0;

        p = arg1;
        day = 1;
        second = minute = hour = 0;

        if (sscanf(p,"%4d",&year) == 0) fatal_error("set_date: year %s", arg1);
	p += 4;
        if (sscanf(p,"%2d",&month) == 0) fatal_error("set_date: month %s", arg1);
	p += 2;
        if (sscanf(p,"%2d",&day) == 1) {
	    p += 2;
            if (sscanf(p,"%2d",&hour) == 1) {
	        p += 2;
                if (sscanf(p,"%2d",&minute) == 1) {
	            p += 2;
                    sscanf(p,"%2d",&second);
		}
	    }
	}

    // set reference time
    save_time(year,month,day,hour,minute,second, sec[1]+12);

    pdt = code_table_4_0(sec);
    j = 0;
    if (pdt == 8) j = 34;
    else if (pdt == 9) j = 47;
    else if (pdt == 10) j = 35;
    else if (pdt == 11) j = 37;
    else if (pdt == 12) j = 36;
    else if (pdt == 13) j = 68;
    else if (pdt == 14) j = 64;

    if (j == 0) return 0;

    n = sec[4][j+7];		// number of stat proc elements

    // add forecast time to time

    units = code_table_4_4(sec);
    dtime = forecast_time_in_units(sec);
    add_time(&year, &month, &day, &hour, &minute, &second, dtime, units);

    for (i = 0; i < n; i++) {
        // add statistical processing time to time
        units = (int) sec[4][48-34+j+i*12];
        dtime = uint4(sec[4]+49-34+j+i*12);
       add_time(&year, &month, &day, &hour, &minute, &second, dtime, units);
    }

    save_time(year,month,day,hour,minute,second, sec[4]+j);
    return 0;
}	

extern struct gribtab_s gribtab[];

/*
 * HEADER:100:set_var:misc:1:changes variable name
 */

int f_set_var(ARG1) {
    struct gribtab_s *p;
    if (mode == -1) {
	p = gribtab;
	while (p->disc != -1) {
	    if (strcmp(arg1,p->name) == 0) {
		*local = (void *) p;
		return 0;
	    }
	    p++;
	}
	fatal_error("set_var: could not find %s", arg1);
	*local = p;
    }
    else if (mode >= 0) {
	p = *local;
	sec[0][6] = p->disc;
        sec[1][9] = p->mtab;
        sec[1][10] = p->ltab;
        sec[4][9] = p->pcat;
        sec[4][10] = p->pnum;
    }
    return 0;
}

int set_var(ARG1) {
    struct gribtab_s *p;
    p = gribtab;
    while (p->disc != -1) {
        if (strcmp(arg1,p->name) == 0) {
            break;
        }
        p++;
    }
    if (p->disc == -1) fatal_error("set_var: could not find %s", arg1);
    sec[0][6] = p->disc;
    sec[1][9] = p->mtab;
    sec[1][10] = p->ltab;
    sec[4][9] = p->pcat;
    sec[4][10] = p->pnum;
    return 0;
}

/*
 * HEADER:100:set_center:misc:1:changes center X = C or C:S     C and S are center/subcenter numbers
 */

int f_set_center(ARG1) {
   int i, center, subcenter;

   if (mode >= 0) {
       i = sscanf(arg1,"%d:%d", &center, &subcenter);
       if (i == 0) fatal_error("set_center: bad arg %s", arg1);
       int2_char(center, sec[1]+5);
       if (i == 2) int2_char(subcenter, sec[1]+7);
    }
    return 0;
}

/*
 * HEADER:100:set:misc:2:set X = Y, X=local_table,etc (help: -set help help)
 */

static char *set_options="discipline, center, subcenter, master_table,"
	" local_table, table_1.2, table_1.3, table_1.4, table_3.0, table_3.1/GDT" 
	", table_3.3, table_3.4, table_4.0/PDT, table_4.1, table_4.2, table_4.6, table_5.0/DRT, table_6.0";


int f_set(ARG2) {
    int i;
    unsigned char *p;

    if (mode == -1) {
	if (strcmp(arg1,"help") == 0) {
	    sprintf(inv_out,"-set: change values of %s\n", set_options);
	    return -1;
	}
	return 0;
    }

    if (mode >= 0) {

	i = atoi(arg2);
	if (strcmp(arg1,"discipline") == 0) {
	    sec[1][10] = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"local_table") == 0) {
	    sec[1][10] = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"master_table") == 0) {
	    sec[1][9] = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"center") == 0) {
            int2_char(i, sec[1]+5);
	    return 0;
	}
	if (strcmp(arg1,"subcenter") == 0) {
            int2_char(i, sec[1]+7);
	    return 0;
	}
	if (strcmp(arg1,"table_1.2") == 0) {
	    sec[1][11] = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"table_1.3") == 0) {
	    sec[1][19] = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"table_1.4") == 0) {
	    sec[1][20] = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"table_3.0") == 0) {
	    sec[3][5] = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"table_3.1") == 0 || strcmp(arg1,"GDT") == 0) {
            uint2_char(i, sec[3]+12);
	    return 0;
	}
	if (strcmp(arg1,"table_3.3") == 0 || strcmp(arg1,"flag_table_3.3") == 0) {
	    return set_flag_table_3_3(sec, i);
	}
	if (strcmp(arg1,"table_3.4") == 0 || strcmp(arg1,"flag_table_3.4") == 0) {
	    return set_flag_table_3_4(sec, i);
	}
	if (strcmp(arg1,"table_4.0") == 0 || strcmp(arg1,"PDT") == 0) {
            uint2_char(i, sec[4]+7);
	    return 0;
	}
	if (strcmp(arg1,"table_4.1") == 0) {
	    sec[4][9] = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"table_4.2") == 0) {
	    sec[4][10] = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"table_4.6") == 0) {
	    p = code_table_4_6_location(sec);
	    *p = (unsigned char) i;
	    return 0;
	}
	if (strcmp(arg1,"table_5.0") == 0 || strcmp(arg1,"DRT") == 0) {
            uint2_char(i, sec[5]+9);
	    return 0;
	}
	if (strcmp(arg1,"table_6.0") == 0) {
	    sec[6][5] = (unsigned char) i;
	    return 0;
	}
	fatal_error("set: allowed values: %s", set_options);

	return 1;
    }
    return 0;
}

/*
 * HEADER:100:set_ftime:misc:1:set ftime .. only on pdt=4.0 only anl/fcst
 */

int f_set_ftime(ARG1) {
    int pdt, n, unit, len;
    char string[STRING_SIZE];
    char string2[STRING_SIZE];

    if (mode < 0) return 0;

    pdt = GB2_ProdDefTemplateNo(sec);
    n = 0;
    unit = -1;
    if (strcmp(arg1 ,"anl") == 0) {
	n = 0;
	unit = 1;
    }
    else {
        if (sscanf(arg1,"%d %s %s%n", &n , string, string2, &len) == 3 &&  len == strlen(arg1)) {
    	    if (strcmp(string2,"forecast") == 0 || strcmp(string2,"fcst") == 0) {
		unit = a2time_range(string);
	    }
	}
    }
    if (unit == -1) fatal_error("set_ftime: unknown option %s", arg1);
    if (pdt == 0 || pdt == 1 || pdt == 2) {
        sec[4][17] = (unsigned char) unit;
        int_char(n,sec[4]+18);
	// significance of reference time
	// type of grib data
	if (n == 0) {
	    sec[1][11] = 0;		// analysis time
	    sec[1][20] = 0;		// analysis data
	}
	else {
	    sec[1][11] = 1;		// start of forecast
	    sec[1][21] = 0;		// analysis forecast
	}
        return 0;
    }
    fatal_error("set_ftime: only works on pdt 1..2","");
    return -1;
}

/*
 * HEADER:100:set_ave:misc:1:set ave/acc .. only on pdt=4.0 only anl/fcst
 */

/* not finished */

int f_set_ave(ARG1) {
    int i, tr, tr2, len, len_arg1;
    char string[STRING_SIZE];
    char string2[STRING_SIZE];
    char string3[STRING_SIZE];
    char *string4;
    static unsigned char new_sec4[58+12+12];		// now use n == 3 forms

    int year, month, day, hour, minute, second;
    int j,k,j2,k2, m, m2, missing;

    if (mode < 0) return 0;

    len_arg1 = strlen(arg1);

//  6 hour ave anl

    i = sscanf(arg1,"%d %s %s %s%n",&j,string,string2,string3,&len);
    if (len != len_arg1) i = 0;
    if (i == 4 && ((tr = a2time_range(string)) >= 0)) {

	for (i = 0; i < 34; i++) new_sec4[i] = sec[4][i];
	get_time(sec[1]+12, &year, &month, &day, &hour, &minute, &second);
        add_time(&year, &month, &day, &hour, &minute, &second, j, tr);
        save_time(year, month, day, hour, minute, second, new_sec4+34);

        uint_char(58, new_sec4);
        uint2_char(8, new_sec4+7);              // pdt = 8
        new_sec4[17] = tr;		    	// hour, etc
        uint_char(0, new_sec4+18);              // start time
        new_sec4[41] = 1;                       // number of time ranges
        uint_char(0, new_sec4+42);              // missing

        if (strcmp(string2,"ave") == 0) new_sec4[46] = 0;
        else if (strcmp(string2,"acc") == 0) new_sec4[46] = 1;
        else if (strcmp(string2,"max") == 0) new_sec4[46] = 2;
        else if (strcmp(string2,"min") == 0) new_sec4[46] = 3;
        else if (strcmp(string2,"last-first") == 0) new_sec4[46] = 4;
        else if (strcmp(string2,"RMS") == 0) new_sec4[46] = 5;
        else if (strcmp(string2,"StdDev") == 0) new_sec4[46] = 6;
        else if (strcmp(string2,"covar") == 0) new_sec4[46] = 7;
        else if (strcmp(string2,"first-last") == 0) new_sec4[46] = 8;
	else fatal_error("set_ave: unknown statistical operator %s",string2);

        new_sec4[47] = 1;

        new_sec4[48] = tr;                       // hour
        uint_char(j, new_sec4+49);
        new_sec4[53] = tr;
        uint_char(0, new_sec4+54);
        sec[4] = &(new_sec4[0]);
        return 0;
    }

    // 1-5 hour ave fcst

    i = sscanf(arg1,"%d-%d %s %s %s%n",&j,&k,string3,string,string2,&len);
    if (len != len_arg1) i = 0;
    if (i == 5 && (tr = a2time_range(string3)) >= 0) {

	for (i = 0; i < 34; i++) new_sec4[i] = sec[4][i];

	get_time(sec[1]+12, &year, &month, &day, &hour, &minute, &second);
        add_time(&year, &month, &day, &hour, &minute, &second, k, tr);
        save_time(year, month, day, hour, minute, second, new_sec4+34);

        uint_char(58, new_sec4);
        uint2_char(8, new_sec4+7);              // pdt = 8
        new_sec4[17] = tr;			// hour
        uint_char(j, new_sec4+18);		// start time
        new_sec4[41] = 1;			// number of time ranges
        uint_char(0, new_sec4+42);		// missing

        if (strcmp(string,"ave") == 0) new_sec4[46] = 0;
        else if (strcmp(string,"acc") == 0) new_sec4[46] = 1;
        else if (strcmp(string,"max") == 0) new_sec4[46] = 2;
        else if (strcmp(string,"min") == 0) new_sec4[46] = 3;
        else if (strcmp(string,"last-first") == 0) new_sec4[46] = 4;
        else if (strcmp(string,"RMS") == 0) new_sec4[46] = 5;
        else if (strcmp(string,"StdDev") == 0) new_sec4[46] = 6;
        else if (strcmp(string,"covar") == 0) new_sec4[46] = 7;
        else if (strcmp(string,"first-last") == 0) new_sec4[46] = 8;
	else fatal_error("set_ave: unknown statistical operator %s", string);

        if (strcmp(string2,"anl") == 0) new_sec4[47] = 1;
        else if (strcmp(string2,"fcst") == 0) new_sec4[47] = 2;
	else fatal_error("set_ave: expecting anl/fcst got %s", string);

        new_sec4[48] = tr;			// hour
	uint_char(k-j, new_sec4+49);
        new_sec4[53] = tr;
        uint_char(0, new_sec4+54);
	sec[4] = &(new_sec4[0]);
        return 0;
    }

    // obsolete form: 1@6 hour ave anl,missing=0

    i = sscanf(arg1,"%d@%d %s ave anl,missing=%d%n",&j,&k,string,&missing,&len);
    if (len != len_arg1) i = 0;

    // new form 1@6 hour ave(anl),missing=0

    if (i != 4) {
        i = sscanf(arg1,"%d@%d %s ave(anl),missing=%d%n",&j,&k,string,&missing,&len);
        if (len != len_arg1) i = 0;
    }

    if (i == 4) {
	tr = a2time_range(string);
	if (tr == -1) fatal_error("set_ave: bad time range %s", string);

        for (i = 0; i < 34; i++) new_sec4[i] = sec[4][i];

        get_time(sec[1]+12, &year, &month, &day, &hour, &minute, &second);
        add_time(&year, &month, &day, &hour, &minute, &second, (j-1)*k, tr);
        save_time(year, month, day, hour, minute, second, new_sec4+34);

        uint_char(58, new_sec4);		// length of section
        uint2_char(8, new_sec4+7);              // pdt = 8

        new_sec4[17] = tr;                      // hour
        uint_char(0, new_sec4+18);              // start time 0 = analysis

        new_sec4[41] = 1;                       // number of time ranges
        uint_char(missing, new_sec4+42);        // missing

        new_sec4[46] = 0;                       // 0 = ave 1 = acc
        new_sec4[47] = 1;                       // 1 = start of forecast increased
	new_sec4[53] = new_sec4[48] = tr;
	uint_char((j-1)*k, new_sec4+49);
	uint_char(k, new_sec4+54);
	sec[4] = &(new_sec4[0]);
	return 0;
    }

// 1@6 hour ave(6 hour fcst),missing=0


    i = sscanf(arg1,"%d@%d %s ave(%d %s fcst),missing=%d%n",&j,&k,string, &m, string2, &missing,&len);
    if (len != len_arg1) i = 0;
    if (i == 6) {
        tr = a2time_range(string);
	if (tr == -1) fatal_error("set_ave: bad time range %s", string);
        tr2 = a2time_range(string2);
	if (tr2 == -1) fatal_error("set_ave: bad time range %s", string2);

        for (i = 0; i < 34; i++) new_sec4[i] = sec[4][i];

        get_time(sec[1]+12, &year, &month, &day, &hour, &minute, &second);
        add_time(&year, &month, &day, &hour, &minute, &second, (j-1)*k, tr);
        add_time(&year, &month, &day, &hour, &minute, &second, m, tr2);
        save_time(year, month, day, hour, minute, second, new_sec4+34);

        uint_char(58, new_sec4);                // length of section
        uint2_char(8, new_sec4+7);              // pdt = 8

        new_sec4[17] = tr2;                     // forecast time range
        uint_char(m, new_sec4+18);              // start time 0 = analysis

        new_sec4[41] = 1;                       // number of time ranges
        uint_char(missing, new_sec4+42);        // missing

        new_sec4[46] = 0;                       // 0 = ave 1 = acc
        new_sec4[47] = 1;                       // 1 = start of forecast increased
        new_sec4[53] = new_sec4[48] = tr;
        uint_char((j-1)*k, new_sec4+49);
        uint_char(k, new_sec4+54);
        sec[4] = &(new_sec4[0]);
        return 0;
    }

// 1@6 hour ave(0-6 hour ave fcst),missing=0

    i = sscanf(arg1,"%d@%d %s ave(%d-%d %s %s fcst),missing=%d%n",&j,&k,string, &m, &m2,string2, string3, &missing,&len);
    string4 = "ave";
    if (i != 8) {
        i = sscanf(arg1,"%d@%d %s acc(%d-%d %s %s fcst),missing=%d%n",&j,&k,string, &m, &m2,string2, string3, &missing,&len);
        string4 = "acc";
    }
    if (i != 8) {
        i = sscanf(arg1,"%d@%d %s min(%d-%d %s %s fcst),missing=%d%n",&j,&k,string, &m, &m2,string2, string3, &missing,&len);
        string4 = "min";
    }
    if (i != 8) {
        i = sscanf(arg1,"%d@%d %s max(%d-%d %s %s fcst),missing=%d%n",&j,&k,string, &m, &m2,string2, string3, &missing,&len);
        string4 = "max";
    }

    if (len != len_arg1) i = 0;
    if (i == 8) {
        tr = a2time_range(string);
        if (tr == -1) fatal_error("set_ave: bad time range %s", string);
        tr2 = a2time_range(string2);
        if (tr2 == -1) fatal_error("set_ave: bad time range %s", string2);

        for (i = 0; i < 34; i++) new_sec4[i] = sec[4][i];

        get_time(sec[1]+12, &year, &month, &day, &hour, &minute, &second);
        add_time(&year, &month, &day, &hour, &minute, &second, (j-1)*k, tr);
        add_time(&year, &month, &day, &hour, &minute, &second, m2, tr2);
        save_time(year, month, day, hour, minute, second, new_sec4+34);

        uint_char(70, new_sec4);                // length of section
        uint2_char(8, new_sec4+7);              // pdt = 8

        new_sec4[17] = tr2;                     // forecast time range
        uint_char(m, new_sec4+18);              // start time 0 = analysis

        new_sec4[41] = 2;                       // number of time ranges
        uint_char(missing, new_sec4+42);        // missing

	// string4
        if (strcmp(string3,"ave") == 0) new_sec4[46] = 0;
        else if (strcmp(string3,"acc") == 0) new_sec4[46] = 1;
        else if (strcmp(string3,"max") == 0) new_sec4[46] = 2;
        else if (strcmp(string3,"min") == 0) new_sec4[46] = 3;
	else fatal_error("set_ave: unknown statistical operator %s",string4);

        new_sec4[47] = 1;                       // 1 = start of forecast increased
        new_sec4[53] = new_sec4[48] = tr;
        uint_char((j-1)*k, new_sec4+49);
        uint_char(k, new_sec4+54);

        if (strcmp(string3,"ave") == 0) new_sec4[46+12] = 0;
        else if (strcmp(string3,"acc") == 0) new_sec4[46+12] = 1;
        else if (strcmp(string3,"max") == 0) new_sec4[46+12] = 2;
        else if (strcmp(string3,"min") == 0) new_sec4[46+12] = 3;
	else fatal_error("set_ave: unknown statistical operator %s",string3);
        // new_sec4[46+12] = 0;                       // 0 = ave 1 = acc
        new_sec4[47+12] = 2;                       // same forecast
        new_sec4[53+12] = new_sec4[48+12] = tr2;
        uint_char(m2-m , new_sec4+49+12);
        uint_char(0, new_sec4+54+12);

        sec[4] = &(new_sec4[0]);
        return 0;
    }

// 10@30 year ave(124@6 hour ave anl)

    i = sscanf(arg1,"%d@%d %s ave(%d@%d %s ave anl),missing=%d%n",&j,&k,string, &j2,&k2,string2, &missing,&len);

    if (len != len_arg1) i = 0;
    if (i == 7) {
fprintf(stderr,">>> need to check out climo1\n");
        tr = a2time_range(string);
        if (tr == -1) fatal_error("set_ave: bad time range %s", string);
        tr2 = a2time_range(string2);
        if (tr2 == -1) fatal_error("set_ave: bad time range %s", string2);

        for (i = 0; i < 34; i++) new_sec4[i] = sec[4][i];

        get_time(sec[1]+12, &year, &month, &day, &hour, &minute, &second);
        add_time(&year, &month, &day, &hour, &minute, &second, (j-1)*k, tr);
        add_time(&year, &month, &day, &hour, &minute, &second, (j2-1)*k2, tr2);
        save_time(year, month, day, hour, minute, second, new_sec4+34);
        

        uint_char(58, new_sec4);                // length of section
        uint2_char(8, new_sec4+7);              // pdt = 8

        new_sec4[17] = tr;                      // hour
        uint_char(0, new_sec4+18);              // start time 0 = analysis

        new_sec4[41] = 2;                       // number of time ranges
        uint_char(missing, new_sec4+42);        // missing

        new_sec4[46] = 0;			// ave
        new_sec4[47] = 1;                       // 1 = start of forecast increased
        new_sec4[53] = new_sec4[48] = tr;
        uint_char((j-1)*k, new_sec4+49);
        uint_char(k, new_sec4+54);


        new_sec4[46+12] = 0;			// ave
        new_sec4[47+12] = 1;                    // 1 = start of forecast increased
        new_sec4[53+12] = new_sec4[48+12] = tr2;
        uint_char((j2-1)*k2, new_sec4+49+12);
        uint_char(k2, new_sec4+54+12);

        sec[4] = &(new_sec4[0]);
	return 0 ;
    }


// 10@30 year ave(124@6 hour ave 6 hour fcst)
// 10@30 year ave(124@6 hour ave(0-6 hour ave fcst)



    fatal_error("set_ave: not implemented %s", arg1);
    return 0;
}

/*
 * HEADER:100:set_metadata:misc:1:read meta-data for grib writing from file X
 */

int f_set_metadata(ARG1) {
    char line[STRING_SIZE];
    char date[STRING_SIZE];
    char var[STRING_SIZE];
    char lev[STRING_SIZE];
    char string[STRING_SIZE];
    char ftime[STRING_SIZE];
    char *arg2;
    int i, len, len_arg1, n, j, i0, i1;
    /* to alter field dimension .. need to alter sscanf format too */
    char field[5][100], str1[100],str2[100];

    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1,"r")) == NULL) {
            fatal_error("Could not open %s", arg1);
        }
    }
    else if (mode >= 0) {
	if (fgets(line, STRING_SIZE, (FILE *)*local) == NULL) {
	    return 1;
	}

	i = sizeof(field[0]);
	n = sizeof(field) / i;
	for (j = 0; j < n; j++) field[j][i-1] = 0;

        i = sscanf(line, "%*[^:]:%*[^:]:d=%99[^:]:%[^:]:%[^:]:%[^:]:%99[^:]:%99[^:]:%99[^:]:%99[^:]", 
		date, var, lev, ftime,field[0],field[1],field[2],field[3]);
	if (i < 4) fatal_error("set_metadata: bad input %s",line);
	n = i - 4;

	if (mode == 99) fprintf(stderr,"set_metadata: ftime %s, f0=%s f1=%s\n",ftime,field[0],field[1]);

	if (strlen(date)) {
	arg1 = date;
	if (f_set_date(CALL_ARG1) != 0) return 1;
	}

	if (strlen(var)) {
	arg1 = var;
	if (set_var(CALL_ARG1) != 0) return 1;
	}

	if (strlen(lev)) {
	arg1 = lev;
	if (f_set_lev(CALL_ARG1) != 0) return 1;
	}

	if (strlen(ftime)) {
	arg1 = ftime;
	len_arg1 = strlen(arg1);
	if (strcmp(arg1,"anl") == 0) f_set_ftime(CALL_ARG1);
	else if ((i = sscanf(arg1,"%*d %*s %s%n", string, &len)) == 1 &&  len == len_arg1) f_set_ftime(CALL_ARG1);
        else f_set_ave(CALL_ARG1);
	}


	// process arguments that can be in any order
	for (i = 0; i < n; i++) {
	    arg1 = field[i];
	    if (mode == 99) fprintf(stderr,"set_metadata arg1=%s\n",arg1);
	     // fprintf(stderr,"set_metadata arg1=%s\n",arg1);
	    // get rid of training newline
	    j = strlen(arg1);
	    if (j == 0) continue;
	    if (arg1[j-1] == '\n') arg1[j-1] = 0;
	    if (arg1[0] == 0) continue;

	    j = sscanf(arg1,"scale=%d,%d", &i0, &i1);
	    if (j == 2) {
		dec_scale = i0;
		bin_scale = i1;
		use_scale = 1;
		continue;
	    }

	    j = sscanf(arg1,"packing=%s", string);
	    if (j == 1) {
                arg1=string;
                f_set_grib_type(CALL_ARG1);
		continue;
	    }

	    // see if -set X T will work
	    j = sscanf(arg1,"%[^=]=%s",str1, str2);
	    if (j == 2) {
	        arg1 = str1;
	        arg2 = str2;
	        if (f_set(CALL_ARG2) == 0) continue;
	    }

	    /* do better
	    j = sscanf(arg1,"background generating process=%d forecast generating process=%d", &i0, &i1);
	    if (j == 2) {
		p = background_generating_process_identifier_location(sec);
		if (p) *p = i0;
		p = analysis_or_forecast_generating_process_identifier_location(sec);
		if (p) *p = i1;
		continue;
	    }
	    */

	    fatal_error("set_metadata: field not understood = %s", arg1);
	}
    }
    return 0;
}

/*
 * HEADER:100:set_flag_table_3.3:misc:1:flag table 3.3 = X
 */

int f_set_flag_table_3_3(ARG1) {
    if (mode >= 0) return set_flag_table_3_3(sec, atoi(arg1));
    return 0;
}

/*
 * HEADER:100:set_flag_table_3.4:misc:1:flag table 3.4 = X
 */

int f_set_flag_table_3_4(ARG1) {
    if (mode >= 0) return set_flag_table_3_4(sec, atoi(arg1));
    return 0;
}
