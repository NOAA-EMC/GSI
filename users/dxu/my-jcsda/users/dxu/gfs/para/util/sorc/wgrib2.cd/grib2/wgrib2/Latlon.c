#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * 12/2006 Public Domain Wesley Ebisuzaki
 * 1/2007 Cleanup M. Schwarb
 * 1/2008 lat and lon changed from float to double 
 */
   
extern int decode;
extern int need_output_file;
extern enum output_order_type output_order;

extern double *lat, *lon;
extern int latlon;

extern int nx, ny, new_GDS;
extern unsigned int npnts;

void free_lat_lon(void);

/*
 * HEADER:100:ij:inv:2:value of field at grid(X,Y) X=1,..,nx Y=1,..,ny
 */

int f_ij(ARG2) {

    struct local_struct {
        int ix, iy, iptr;
    };
    struct local_struct *save;

    if (mode == -1) {
        decode = 1;
        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation f_ij","");
        save->ix = atoi(arg1);
        save->iy = atoi(arg2);
        save->iptr = -1;
    }
    if (mode < 0) return 0;
    save = *local;

    if (new_GDS) {
        if (output_order != wesn) {
            fatal_error("ij only works in we:sn order","");
        }
        if (save->ix <= 0 || save->ix > nx || save->iy <= 0 || save->iy > ny) {
            fatal_error("invalid i, j values","");
        }
        save->iptr = (save->ix-1) + (save->iy-1) *nx;
    }

    if (mode > 0) sprintf(inv_out,"(%d,%d),val=%lg",save->ix,save->iy,data[save->iptr]);
    else sprintf(inv_out,"val=%lg",data[save->iptr]);

    return 0;
}

/*
 * HEADER:100:ijlat:inv:2:lat,lon and grid value at grid(X,Y) X=1,..,nx Y=1,..,ny
 */

int f_ijlat(ARG2) {

    struct local_struct {
        int ix, iy, iptr;
    };
    struct local_struct *save;

    if (mode == -1) {
        decode = latlon = 1;
        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation f_ijlat","");
        save->ix = atoi(arg1);
        save->iy = atoi(arg2);
        save->iptr = -1;
    }
    if (mode < 0) return 0;
    save = *local;

    if (new_GDS) {
        if (output_order != wesn) {
            fatal_error("ijlat only works in we:sn order","");
        }
        if (lat == NULL || lon == NULL || data == NULL) {
            fatal_error("no lat/lon in ijlat","");
        }

        if (save->ix <= 0 || save->ix > nx || save->iy <= 0 || save->iy > ny) {
            /* fprintf(stderr," nx=%d ny=%d ix=%d iy=%d\n",nx,ny,save->ix,save->iy); */
            fatal_error("ijlat: invalid i, j values","");
        }
        save->iptr = (save->ix-1) + (save->iy-1) * nx;
    }
//vsm_fmt    sprintf(inv_out,"(%d,%d),lon=%g,lat=%g,val=%lg",save->ix,save->iy,
    sprintf(inv_out,"(%d,%d),lon=%lf,lat=%lf,val=%lg",save->ix,save->iy,
        lon[save->iptr],lat[save->iptr],data[save->iptr]);
    return 0;
}

/*
 * HEADER:100:ilat:inv:1:lat,lon and grid value at Xth grid point, X=1,..,npnts
 */

int f_ilat(ARG1) {

    struct local_struct {
        int ix;
    };
    struct local_struct *save;
    int i;

    if (mode == -1) {
        decode = latlon = 1;
        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation f_ilat","");
        save->ix = atoi(arg1);
    }
    if (mode < 0) return 0;

    save = *local;
    i = save->ix;

    if (new_GDS) {
        if (output_order != wesn) {
            fatal_error("ilat only works in we:sn order","");
        }
        if (lat == NULL || lon == NULL || data == NULL) {
            fatal_error("no lat/lon in ilat","");
        }
        if (i < 1 || i > (int) npnts) {
            fatal_error_i("ilat: invalid i = %d", i);
        }
    }
//vsm_fmt    sprintf(inv_out,"grid pt %d,lon=%g,lat=%g,val=%lg",i,
    sprintf(inv_out,"grid pt %d,lon=%lf,lat=%lf,val=%lg",i,
        lon[i-1],lat[i-1],data[i-1]);
    return 0;
}

/*
 * HEADER:100:lon:inv:2:value at grid point nearest lon=X lat=Y
 */

int f_lon(ARG2) {

    struct local_struct {
        double plat, plon;
        int iptr;
    };
    struct local_struct *save;

    if (mode == -1) {
        decode = latlon = 1;
        *local = save = (struct local_struct *)malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation f_lon","");
        save->plon = atof(arg1);
        if (save->plon < 0.0) save->plon += 360.0;
        save->plat = atof(arg2);
        save->iptr = -1;
    }
    if (mode < 0) return 0;
    save = *local;

    if (new_GDS) {
//        if (output_order != wesn) {
//            fatal_error("lon only works in we:sn order","");
//        }
        if (lat == NULL || lon == NULL || data == NULL) {
            fatal_error("no val","");
        }
        closest_init(sec);
        save->iptr = closest(sec, save->plat, save->plon);
    }
//vsm_frm    sprintf(inv_out,"lon=%g,lat=%g,val=%lg",lon[save->iptr],lat[save->iptr],data[save->iptr]);
    if (save->iptr < 0)  {
	sprintf(inv_out,"lon=999,lat=999,val=%lg", UNDEFINED);
	fprintf(stderr,"-lon: grid outside of domain of data\n");
	return 0;
    }

    if (mode == 0) {
        sprintf(inv_out,"lon=%lf,lat=%lf,val=%lg",lon[save->iptr],lat[save->iptr],data[save->iptr]);
    }
    else {
	sprintf(inv_out,"lon=%lf,lat=%lf,i=%d,", lon[save->iptr],lat[save->iptr],(save->iptr)+1);
	inv_out += strlen(inv_out);
	if (nx > 0 && ny > 0) {
	    sprintf(inv_out,"ix=%d,iy=%d,", (save->iptr)%nx+1, (save->iptr)/nx+1);
	    inv_out += strlen(inv_out);
	}
	sprintf(inv_out,"val=%lg", data[save->iptr]);
    }
    return 0;
}


int get_latlon(unsigned char **sec) {
    int grid_template;

    free_lat_lon();
    grid_template = code_table_3_1(sec);
    if (grid_template == 0) {
        regular2ll(sec, &lat, &lon);
    }
    else if (grid_template == 10) {
        mercator2ll(sec, &lat, &lon);
    }
    else if (grid_template == 20) {
        polar2ll(sec, &lat, &lon);
    }
    else if (grid_template == 30) {
        lambert2ll(sec, &lat, &lon);
    }
    else if (grid_template == 40) {
        gauss2ll(sec, &lat, &lon);
    }

    return 0; 
}

void free_lat_lon(void) {
    if (lat != NULL) {
        free(lat);
        lat = NULL;
        free(lon);
        lon = NULL;
    }
}

