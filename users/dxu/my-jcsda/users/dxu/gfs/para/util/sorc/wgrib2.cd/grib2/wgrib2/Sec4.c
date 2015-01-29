#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * some routines that involve Sec4
 *
 * Public Domain 2006: Wesley Ebisuzaki
 * 1/2007  cleanup M Schwarb
 * 7/2009 bug fix (buffer overflow) Reinoud Bokhorst
 */

/*
 * HEADER:400:Sec4:inv:0:Sec 4 values (Product definition section)
 */
int f_Sec4(ARG0) {
    int pdtsize;
    if (mode >= 0) {
	pdtsize =  prod_def_temp_size(sec);
	sprintf(inv_out,"Sec4 len=%u #vert coordinate=%u Product Defn Template=4.%d size=%d free=%d", 
          uint4(&(sec[4][0])), uint2(&(sec[4][5])),GB2_ProdDefTemplateNo(sec),
	  pdtsize, uint4(&(sec[4][0])) - pdtsize - 8*uint2(&(sec[4][5])));
	inv_out += strlen(inv_out);

        if (GB2_ProdDefTemplateNo(sec) == 0) {
	   sprintf(inv_out," lvl1=%d scale=%d val=%d", sec[4][22], INT1(sec[4][23]), 
                 int4(sec[4] + 24));
	   inv_out += strlen(inv_out);
        }
    }
    return 0;
}

/*
 * HEADER:400:processid:inv:0:process id (locally defined)
 */
int f_processid(ARG0) {
    int i;
    char *space;


    if (mode >= 0) {
        space = "";
	i = observation_generating_process_identifier(sec);
	if (i >= 0) {
	    sprintf(inv_out,"%sobservation generating process=%d", space, i);
	    inv_out += strlen(inv_out);
	    space = " ";
	}

        i = background_generating_process_identifier(sec);
	if (i >= 0) {
	    sprintf(inv_out,"%sbackground generating process=%d", space, i);
	    inv_out += strlen(inv_out);
	    space = " ";
	}

        i = analysis_or_forecast_generating_process_identifier(sec);
	if (i >= 0) {
	    sprintf(inv_out,"%sforecast generating process=%d", space, i);
	    inv_out += strlen(inv_out);
	    space = " ";
	}
    }
    return 0;
}


/*
 * HEADER:400:0xSec:inv:1:Hex dump of section X (0..8)
 */
int f_0xSec(ARG1) {
    int i;
    unsigned int j, len;
    double tot;
    unsigned char *s;

    if (mode >= 0) {
        i = atoi(arg1);
        if (i < 0  || i > 8) return 1;
        s = sec[i];
        if (s == NULL) {
            sprintf(inv_out,"sec%d is missing", i);
            return 0;
        }

        if (i == 0) { 
	    len = GB2_Sec0_size;
	}
	else if (i == 8){
	    len = GB2_Sec8_size;
        }
        else { 
            len = uint4(&(sec[i][0]));
        }

	/* Calculate number of bytes to print */

	if (mode == 0) tot = 2.0 * len;
	else if (mode == 1) tot = 3.0 * len;
	else {		// mode >= 2
	    j = len;
	    i = 0;
	    while (j) {
		j /= 10;
		i++;
	    }
	    // i = maximum number of digits in address
	    tot = len*i			// maximum of characters for address
		  + 4*len		// blank : 2 digits
		  + 4*(len % 15);	// new line (2 for windows) + two blanks
        }

	if (tot >= INV_BUFFER - 1000) {	// 1000 is precaution
	    sprintf(inv_out,"Sec%d=too long to print",i);
	    return 0;
	}

        if (mode == 0) sprintf(inv_out,"Sec%d(1..%u)=0x",i,len);
	else sprintf(inv_out,"Sec%d(1..%u)=",i,len);
	inv_out += strlen(inv_out);

	if (mode == 0) {
            while (len--) {
         	sprintf(inv_out,"%.2x", *s++);
		inv_out += strlen(inv_out);
            }
	}
	else if (mode == 1) {
            for (j = 1; j <= len; j++) {
		sprintf(inv_out," %.2x", *s++);
		inv_out += strlen(inv_out);
	    }
	}
	else if (mode >= 2) {
            for (j = 1; j <= len; j++) {
		sprintf(inv_out,"%u:%.2x", j, *s++);
		inv_out += strlen(inv_out);
		sprintf(inv_out,j % 15 == 0 ? "\n  " : " ");
		inv_out += strlen(inv_out);
	    }
	}
    }
    return 0;
}

/*
 * HEADER:400:var:inv:0:short variable name
 */

int f_var(ARG0) {
    if (mode >= 0) {
        getName(sec, mode, inv_out, NULL, NULL, NULL);
	inv_out += strlen(inv_out);
    }
    return 0;
}

/*
 * HEADER:400:varX:inv:0:raw variable name - discipline mastertab localtab center parmcat parmnum
 */

int f_varX(ARG0) {

    int discipline, center,mastertab,localtab,parmcat,parmnum;

    if (mode >= 0) {
        discipline = GB2_Discipline(sec);
        center = GB2_Center(sec);
        mastertab = GB2_MasterTable(sec);
        localtab = GB2_LocalTable(sec);
        parmcat = GB2_ParmCat(sec);
        parmnum = GB2_ParmNum(sec);
	if (mode == 0) {
            if (parmnum == 255) {
                sprintf(inv_out,"missing");
                return 0;
	    }
	    sprintf(inv_out,"var%d_%d_%d_%d_%d_%d", discipline, mastertab, localtab, center, parmcat, parmnum);
        }
	else if (mode > 0) {
	    if (parmnum == 255) {
                sprintf(inv_out,"missing definition [?]");
                return 0;
            }
            if (parmnum < 192) {
                sprintf(inv_out,"var discipline=%d master_table=%d parmcat=%d parm=%d", 
                      discipline, mastertab, parmcat, parmnum); }
            else {
                sprintf(inv_out,"var discipline=%d local_table=%d center=%d parmcat=%d parm=%d",
                discipline, localtab, center, parmcat, parmnum);
            }
        }
    }
    return 0;
}

/*
 * HEADER:440:ftime:inv:0:forecast time
 */

/*
Code Table 4.4: Indicator of unit of time range
Code figure   Meaning
  0	Minute
  1	Hour
  2	Day
  3	Month
  4	Year
  5	Decade (10 years)
  6	Normal (30 years)
  7	Century (100 years)
  8-9	Reserved
 10	3 hours
 11	6 hours
 12	12 hours
 13	Second
 14-191	Reserved
192-254	Reserved for local use
255	Missing
*/


int wrt_time(int unit, int value, char *inv_out) {

   const char *string;

   normalize_time_range(&unit, &value);

   if (unit == 13) {	// second
	if (value == 0 || value % 60 != 0) {
	    sprintf(inv_out,"%d sec", value);
	    return 0;
	}
	value = value / 60;
	unit = 0;
   }
   if (unit == 0) {	// minute
	if (value == 0 || value % 60 != 0) {
	    sprintf(inv_out,"%d min", value);
	    return 0;
	}
	value = value / 60;
	unit = 1;
   }

   if (unit == 1) {     // hours
        if (value == 0 || value % 24 != 0) {
            sprintf(inv_out,"%d hour", value);
            return 0;
        }
        value = value / 24;
        unit = 2;
   }

   string = time_range2a(unit);
   if (string != NULL) {
	sprintf(inv_out,"%d %s", value, string);
	return 0;
   }
   sprintf(inv_out,"????");
   return 1;
}

static void print_ftime (int unit1, int value1, int unit2, int value2, int format, char *inv_out) {

    normalize_time_range(&unit1, &value1);
    if (format != 1) normalize_time_range(&unit2, &value2);

    if (format == 1) sprintf(inv_out,"%d %s",value1,time_range2a(unit1));
    else if (format == 2) {
	if (unit1 == unit2) {
	    if (unit1 ==  1 && (value1 % 24 == 0) && (value2 % 24 == 0)) {
		value1 /= 24;
		value2 /= 24;
		unit1=2;
	    }
	    sprintf(inv_out,"%d-%d %s",value1,value1+value2,time_range2a(unit1));
	}
	else {
	    sprintf(inv_out,"%d %s-(%d %s+%d %s",value1,time_range2a(unit1),
		value1,time_range2a(unit1),value2,time_range2a(unit2));
	}
    }
}

int prt_stat_tr(int mode, unsigned char **sec, char *inv_out, unsigned char *p, int inner) {
    char *string;
    int unit, value, unit2, value2, unit3, value3;

    unit2 = p[2];
    value2 = uint4(p+3);

    unit3 = p[7];
    value3 = uint4(p+8);

    if (mode == 99) fprintf(stderr,"prt_stat_tr: code_4.10=p[0]=%d inner=%d\n",
		(int) p[0], inner);
    if (mode == 99) fprintf(stderr,"prt_stat_tr: code_4.11=p[1]=%d\n",(int) p[1]);
    if (mode == 99) fprintf(stderr,"prt_stat_tr: unit3=%d value3=%d\n", unit3, value3);

    if (p[1] == 0) {
	sprintf(inv_out,"reserved");
        inv_out += strlen(inv_out);
	return 0;
    }
    if (p[1] == 255) {
	sprintf(inv_out,"missing");
        inv_out += strlen(inv_out);
	return 0;
    }
    if (p[1] > 5) {
	sprintf(inv_out,"CodeTable 4.11=%d",p[1]);
        inv_out += strlen(inv_out);
	return 0;
    }

    if (inner) {
	if ((unit = code_table_4_4(sec)) < 0) return -1;
        // value = GB2_ForecastTime(sec);
	value = forecast_time_in_units(sec);

        if (mode == 99) fprintf(stderr,"prt_stat_tr: inner time_unit=%d fcst time=%d\n",unit,value);

	if (p[1] == 1) {

	    // initial time incremented
	    // forecast length is the same

	    if (p[0] == 51) {
	        print_ftime(unit,value,unit2,value2,2, inv_out);
		inv_out += strlen(inv_out);
		sprintf(inv_out," climo");
		inv_out += strlen(inv_out);
		return 0;
	    }

	    if (mode == 99) fprintf(stderr,"loc:1234 p[0]=%d, p[1]=%d\n", p[0],p[1]);

	    

            /* average or accumulation or */
            string = "";
            switch(*p) {
#include           "CodeTable_4.10.dat"
            }
	    if (strcmp(string,"") == 0) string="???";


//old          ave valid 12-16 hours
//new       12-15 hour ave anl
	    if (unit3 == 255  || value3 == 0) {
//	        sprintf(inv_out,"%s valid ",string);
//       		inv_out += strlen(inv_out);
//	        print_ftime(unit,value,unit2,value2,2, inv_out);
//	        inv_out += strlen(inv_out);

	        print_ftime(unit,value,unit2,value2,2, inv_out);
	        inv_out += strlen(inv_out);
		sprintf(inv_out," %s anl",string);
	        inv_out += strlen(inv_out);
		return 0;
	    }

//	    5@1 day ave
            if (unit2 == unit3 && value3 != 0 && value2 % value3 == 0) {
	        sprintf(inv_out,"%d@",value2/value3+1);
       		inv_out += strlen(inv_out);

//	        sprintf(inv_out,"[%d@]",value2);
       		inv_out += strlen(inv_out);

	        wrt_time(unit3,value3,inv_out);
       		inv_out += strlen(inv_out);
	        sprintf(inv_out," %s",string);
        	inv_out += strlen(inv_out);
	    }
//	    (time) @ (dt) ave
	    else {
                wrt_time(unit2,value2,inv_out);
	        inv_out += strlen(inv_out);

                sprintf(inv_out," %s",string);
                inv_out += strlen(inv_out);
		if (value3 != 0 && unit3 != 255) {
                    sprintf(inv_out," @");
                    inv_out += strlen(inv_out);

                    wrt_time(unit3,value3,inv_out);
                    inv_out += strlen(inv_out);
		}
	    }

            // value = GB2_ForecastTime(sec);
	    value = forecast_time_in_units(sec);
	    if ((unit = code_table_4_4(sec)) < 0) return -1;

	    if (value == 0) {
		// sprintf(inv_out," anl");
		sprintf(inv_out,"(anl)");
                inv_out += strlen(inv_out);
	    }
	    else {
		// wne sprintf(inv_out," (");
		sprintf(inv_out,"(");
                inv_out += strlen(inv_out);
                wrt_time(unit,value,inv_out);
                inv_out += strlen(inv_out);
		sprintf(inv_out," fcst)");
                inv_out += strlen(inv_out);
	    }

	    return 0;
	}

	if (p[1] == 2) {

	    if (mode == 99) fprintf(stderr,"loc:1235 p[0]=%d, p[1]=%d\n", p[0],p[1]);
	    if (mode == 99) fprintf(stderr,"unit=%d value=%d unit2=%d value2=%d\n",unit,value,unit2,value2);
	    print_ftime(unit,value,unit2,value2,2, inv_out);
	    inv_out += strlen(inv_out);
            /* average or accumulation or */
            string = "";
            switch(*p) {
#include           "CodeTable_4.10.dat"
            }
	    if (strcmp(string,"") == 0) string="???";
	    if (mode == 99) fprintf(stderr,"codetable_4.10 is (%s)\n", string);

	    // NCEP TMIN and TMAX set codetable 4.10 to missing
	    // if TMIN/TMAX, do not print stat processing operator if missing

	    if (GB2_Discipline(sec) == 0 && GB2_Center(sec) == 7 && GB2_ParmCat(sec) == 0
		&& (GB2_MasterTable(sec) <= 5) && (GB2_ParmNum(sec) == 4 || GB2_ParmNum(sec) == 5)
		&& strcmp(string,"missing") == 0) {
		string="";
	    }
	    else {
                sprintf(inv_out," %s",string);
	    }
            inv_out += strlen(inv_out);

	    if (value3 != 0 && unit3 != 255) {
                sprintf(inv_out,"@");
                inv_out += strlen(inv_out);
                wrt_time(unit3,value3,inv_out);
                inv_out += strlen(inv_out);
	    }
	    sprintf(inv_out," fcst");
            inv_out += strlen(inv_out);
	    return 0;
	}

	if (p[1] == 3 || p[1] == 4) {
	    if (mode == 99) fprintf(stderr,"adfadf p[1]=2 value2=%d unit2=%d\n", value2,unit2);
           /* average or accumulation or */
            string = "";
            switch(*p) {
#include           "CodeTable_4.10.dat"
            }
            if (strcmp(string,"") == 0) string="???";


//          ensemble ave valid 12 hours
            sprintf(inv_out,"ensemble %s valid ",string);
            inv_out += strlen(inv_out);
            print_ftime(unit,value,unit2,value2,1, inv_out);
            inv_out += strlen(inv_out);

	    if (mode > 0) {
	        sprintf(inv_out," code4.10=%d unit2=%d value2=%d unit3=%d value3=%d",
		   p[1], unit2, value2, unit3, value3);
	    }
	    return 0;
	}
	wrt_time(unit,value,inv_out);
	inv_out += strlen(inv_out);
	if (p[1] == 3) {
	    sprintf(inv_out," t0+ ft- ");
	}
	else if (p[1] == 4) {
	    sprintf(inv_out," t0- ft+ ");
	}
	else if (p[1] == 5) {
	    sprintf(inv_out," dt ");
	}
	else {
	    sprintf(inv_out," (%d) ", p[1]);
	}
	inv_out += strlen(inv_out);

        wrt_time(unit2,value2,inv_out);
	inv_out += strlen(inv_out);
    }

//  outer loop

    /* average or accumulation or */
    string = "";
    switch(*p) {
#include           "CodeTable_4.10.dat"
    }
    if (strcmp(string,"") == 0) string="???";


//   10@3 hour ave

if (mode == 99) fprintf(stderr,"ftime: value2 %d value3 %d\n", value2, value3);
if (mode == 99) fprintf(stderr,"ftime: unit2 %d unit3 %d\n", unit2, unit3);
    if (unit2 == unit3 && value3 != 0 && value2 % value3 == 0 && p[0] != 51 ) {
        sprintf(inv_out,"%d@",value2/value3+1);
        inv_out += strlen(inv_out);
	wrt_time(unit3,value3,inv_out);
	inv_out += strlen(inv_out);
        sprintf(inv_out," %s",string);
        inv_out += strlen(inv_out);
	return 0;
    }

if (mode == 99) fprintf(stderr,"ftime: more code");

    wrt_time(unit2,value2,inv_out);
    inv_out += strlen(inv_out);

    sprintf(inv_out," %s",string);
    inv_out += strlen(inv_out);

    if (value3 != 0 && p[0] != 51) {	// not if climo or value == 0 
        sprintf(inv_out,"@");
        inv_out += strlen(inv_out);
	wrt_time(unit3,value3,inv_out);
	inv_out += strlen(inv_out);
    }

    return 0;
}

int f_ftime(ARG0) {
    int unit, value, n,i,code_4_11, code_4_10;
    int loc_n;

    if (mode < 0) return 0;

    if (mode == 99) fprintf(stdout,"ProdDefTemplateNo=%d\n", GB2_ProdDefTemplateNo(sec));

    // templates with statistical processing start with a n value
    // loc_n is the start of the stat processing data
    // loc_n == -2 implies no forecast time
    // loc_n == -1 implies forecast time and no stat processing

    switch (GB2_ProdDefTemplateNo(sec)) {
	case 8: loc_n = 41; break;
	case 9: loc_n = 54; break;
	case 10: loc_n = 42; break;
	case 11: loc_n = 44; break;
	case 12: loc_n = 43; break;
	case 13: loc_n = 75; break;
	case 14: loc_n = 71; break;
	case 20: loc_n = -2; break;
	case 30: loc_n = -2; break;
	case 31: loc_n = -2; break;
	case 42: loc_n = 43; break;
	case 43: loc_n = 46; break;
	default: loc_n = -1; break;
    }

    if (loc_n == -2) {
        sprintf(inv_out,"anl");
	return 0;
    }

    if (loc_n == -1) {
	if ((unit = code_table_4_4(sec)) < 0) return -1;
        // value = GB2_ForecastTime(sec);
	value = forecast_time_in_units(sec);
        if (value == 0) {
            sprintf(inv_out,"anl");
        }
        else {
    	    print_ftime(unit, value, 0, 0, 1, inv_out);
	    inv_out += strlen(inv_out);
            sprintf(inv_out," fcst");
	    inv_out += strlen(inv_out);
        }
	return 0;
    }
    else {
	if ((unit = code_table_4_4(sec)) < 0) return -1;
        n = (int) sec[4][loc_n];
	if (mode == 99) fprintf(stdout,"f_ftime:  n=sec[4][loc_n]=%d\n", n);
	code_4_10 = sec[4][5+loc_n];
	code_4_11 = sec[4][6+loc_n];
	if (mode == 99) printf("f_ftime: stat_proc code_4.10=%d increment code_4.11=%d\n", code_4_10, code_4_11);
	if (mode == 99) printf("f_ftime: code_4.4=%d ntime=%d\n", sec[4][7+loc_n],uint4(sec[4]+8+loc_n));

	for (i = 1; i <= n; i++) {
	    if (i > 1) {
		sprintf(inv_out,"(");
	        inv_out += strlen(inv_out);
	    }
	    prt_stat_tr(mode, sec, inv_out, sec[4]+5+loc_n+i*12-12, n == i);
	    inv_out += strlen(inv_out);
	}
	for (i = 1; i < n; i++) {
	    sprintf(inv_out,")");
	    inv_out += strlen(inv_out);
        }



	if (uint4(sec[4]+13+loc_n) != 0) {	// not a continous process -- print no. missing
	    sprintf(inv_out,",missing=%d",uint4(sec[4]+1+loc_n));
	    inv_out += strlen(inv_out);
	}
	return 0;
    }
    return 0;
}
