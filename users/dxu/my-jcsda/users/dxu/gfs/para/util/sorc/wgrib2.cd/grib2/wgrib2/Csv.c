/******************************************************************************************
 Copyright (C) 2008 Niklas Sondell, Storm Weather Center
 This file is part of wgrib2 and could be distributed under terms of the GNU General Public License

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

extern int decode, flush_mode;
extern int file_append;

extern double *lat, *lon;
extern int decode, latlon;

/*
 * HEADER:100:csv:output:1:make comma separated file, X=file
 */

int f_csv(ARG1) {

    char text[STRING_SIZE];
    char *save_inv_out, new_inv_out[STRING_SIZE];
    char name[100], desc[100], unit[100];
    FILE *out;

    unsigned int j;
    double longitude;
    char vt[20],rt[20];
//  unsigned char *p;
    int year, month, day, hour, minute, second;
	
    /* initialization phase */

    if (mode == -1) {
        decode = latlon = 1;
        if ((*local = (void *) ffopen(arg1,file_append ? "a" : "w")) == NULL)
		fatal_error("csv could not open file %s", arg1);  
	return 0;
    }

    /* cleanup phase */

    if (mode == -2) return 0;

    /* processing phase */

    if (lat == NULL || lon == NULL) {
	fprintf(stderr,"csv: latitude/longitude not defined, record skipped\n");
	return 0;
    }

    out = *local;

    /*Collect runtime and validtime into vt and rt*/

//    p = sec[1];
//    sprintf(rt, "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d", (p[12]<<8)+p[13], p[14],p[15],p[16],p[17],p[18]);
    reftime(sec, &year, &month, &day, &hour, &minute, &second);
    sprintf(rt, "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d", year,month,day,hour,minute,second);

    if (verftime(sec, &year, &month, &day, &hour, &minute, &second) == 0) {
        sprintf(vt,"%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d", year,month,day,hour,minute,second);
    }

    /*Get parameter name into text*/
    getName(sec, mode, text, NULL, NULL, NULL);
	
    /*Get levels, parameter name, description and unit*/

    save_inv_out = inv_out;
    inv_out = new_inv_out;

    f_lev(CALL_ARG0);
    inv_out = save_inv_out;

    if (strcmp(new_inv_out, "reserved")==0) return 0;
    getName(sec, mode, NULL, name, desc, unit);
//	fprintf(stderr,"Start processing of %s at %s\n", name, new_inv_out);
//	fprintf(stderr,"Gridpoints in data: %d\n", ndata);
//	fprintf(stderr,"Description: %s, Unit %s\n", desc,unit);

     /* Lage if-setning rundt hele som sjekker om alt eller deler skal ut*/

//		fprintf(stderr,"Writing all gridpoints to file %s \n", arg3);

    for (j = 0; j < ndata; j++) {
        if (!UNDEFINED_VAL(data[j])) {
	    longitude = lon[j];
	    if (longitude > 180) longitude-=360;
	    fprintf(out,"\"%s\",\"%s\",\"%s\",\"%s\",%g,%g,%lg\n",rt,vt,name,
				new_inv_out,longitude,lat[j],data[j]);
	}
    }
    if (flush_mode) fflush(out);
    return 0;
}
