#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * New_grid
 *
 *  v 0.9 experimental .. bilinear interpolation using ipolates library
 *          input = winds are N/S or grid
 *          output = winds are N/S
 *
 * to add new grids
 *
 *    input grids: add to mk_kdgs.c
 *    output grids: add to sec3_grids
 *                  add code to mode == -1 to parse grid specifications
 *
 * to add types to vector fields definition
 *             modify source code: vectors[]
 *
 * 6/2010: Public Domain Wesley Ebisuzaki
 *
 */

#ifdef USE_IPOLATES

#ifdef G95
#define IPOLATES ipolates_
#define IPOLATEV ipolatev_
void g95_runtime_start(int ,char **);
void g95_runtime_stop(void);
static int g95_runstop = 0;
#endif

#ifdef GFORTRAN
#define IPOLATES ipolates_
#define IPOLATEV ipolatev_
#endif

#ifdef XLF
#define IPOLATES ipolates
#define IPOLATEV ipolatev
#endif

void IPOLATES(int *interpol, int *ipopt, int *kgds, int *kgds_out, int *npnts, int *n_out0, 
		int *km, int *ibi, unsigned char *bitmap, float *data_in, int *n_out, 
		float *rlat, float *rlon, int *ibo, unsigned char *bitmap_out, 
		float *data_out, int *iret);

void IPOLATEV(int *interpol, int *ipopt, int *kgds, int *kgds_out, int *npnts, int *n_out0, 
		int *km, int *ibi, unsigned char *bitmap, float *u_in, float *v_in, 
		int *n_out, float *rlat, float *rlon, float *crot, float *srot, int *ibo,
		unsigned char *bitmap_out, float *u_out, float *v_out, int *iret);


extern unsigned int npnts,nx,ny;
extern double *lat, *lon;
extern int decode, latlon;
extern int decode, flush_mode;
extern int file_append, flush_mode;
extern int use_scale, dec_scale, bin_scale, wanted_bits, max_bits;
extern enum output_grib_type grib_type;
extern enum output_order_type output_order;
extern int save_translation;
extern enum output_order_type output_order_wanted, output_order;

static int interpol_type = 0;
static int ipopt[20] = {-1,0,0, 0,0,0, 0,0,0, 0};

/*
 * HEADER:111:new_grid_interpolation:misc:1:new_grid interpolation X=bilinear,bicubic,neighbor,budget
 */

int f_new_grid_interpolation(ARG1) {

   if (strcmp(arg1,"bilinear") == 0) { interpol_type = 0; ipopt[0] = -1; }
   else if (strcmp(arg1,"bicubic") == 0) { interpol_type = 1; ipopt[0] = 0; }
   else if (strcmp(arg1,"neighbor") == 0) { interpol_type = 2; ipopt[0] = 1; }
   else if (strcmp(arg1,"budget") == 0) { interpol_type = 3; ipopt[0] = -1; }
//  turned off spectral -- new library rarely used interpolation option
//   else if (strcmp(arg1,"spectral") == 0) { interpol_type = 4; ipopt[0] = 0; ipopt[1] = 36; }
//  turned off neighbor-budget - save space for rarely used interpolation option
//   else if (strcmp(arg1,"neighbor-budget") == 0) { interpol_type = 6; ipopt[0] = -1; }
   else fatal_error("new_grid_interpolation: unknown type %s", arg1);

   return 0;
}

/*
 * HEADER:111:new_grid_ipopt:misc:1:new_grid ipopt values X=i1:i2..:iN N <= 20
 */
int f_new_grid_ipopt(ARG1) {
    int i, k, val, m;

    i = 0;
    k = sscanf(arg1, "%d%n", &val, &m);
    while (k == 1) {
        if (i > 19) fatal_error("new_grid_ipopt: too many ipopt values, 20 max","");
        ipopt[i++] = val;
        arg1 += m;
        k = sscanf(arg1, ":%d%n", &val, &m);
    }
    return 0;
}


/*
 * HEADER:111:new_grid_winds:misc:1:new_grid wind rotation: X = grid, N/S (default) -- alpha
 */
static enum {grid, earth} wind_rotation  = earth;

int f_new_grid_winds(ARG1) {
    if (strcmp(arg1,"grid") == 0) wind_rotation = grid;
    else if (strcmp(arg1,"earth") == 0) wind_rotation = earth;
    else fatal_error("new_grid_winds: bad arg %s", arg1);
    return 0;
}


/*
 * HEADER:111:new_grid:output:4:bilinear interpolate: X=projection Y=x0:nx:dx Z=y0:ny:dy A=grib_file alpha
 */

struct local_struct {
	// U data
        float *u_val;
        int has_u, nx, ny;
        unsigned char *clone_sec[9];

	// interpolation
        int npnts_out;
        float *rlat, *rlon, *crot, *srot;
        unsigned char *sec3;
	int kgds_out[200];

	// output file
        FILE *out;
};

static const char *vectors[] = {"UGRD", "VGRD", "VUCSH", "VVCSH","UFLX", "VFLX",
	"UGUST","VGUST","USTM","VSTM","VDFUA", "VDFVA", 
	"UOGRD","VOGRD"
};


int f_new_grid(ARG4) {
    struct local_struct *save;

    unsigned int i;
    int is_u, is_v;
    int kgds[200], km;
    float *data_in, *data_out;
    double x0, y0, dx, dy;
    double lov, lad, latin1, latin2;
    int proj;					// projection: for LC 0 = NP, 128 = SP
    char name[NAMELEN];
    int j, ibi, ibo, iret, nnx, nny, n_out;
    unsigned char *new_sec[8], *gds, *s, *bitmap, *bitmap_out, *p;

    if (mode == -1) {			// initialization
        decode = 1;
        output_order_wanted = raw;	// in raw order


	// initialize g95 runtime library
#ifdef G95
	if (g95_runstop == 0) { g95_runtime_start(0,NULL); g95_runstop = 1; }
#endif

        if ( (sizeof(vectors) / sizeof (vectors[0])) % 2 == 1) fatal_error("new_grid: program error in vectors[]","");

	// allocate static variables

        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation -wind_speed","");

        if ((save->out = (void *) ffopen(arg4, file_append ? "ab" : "wb")) == NULL) {
	    fatal_error("-new_grid: could not open file %s", arg1);
	}
	save->has_u = -1;
	init_sec(save->clone_sec);
	s = NULL;

	// parse NCEP grids */
	ncep_grids(&arg1, &arg2, &arg3);

	// for each output grid
        if (strcmp(arg1,"latlon") == 0) {
            if (sscanf(arg2,"%lf:%d:%lf", &x0, &nnx, &dx) != 3)
                fatal_error("new_grid: XDEF wrong:%s",arg2);
            if (sscanf(arg3,"%lf:%d:%lf", &y0, &nny, &dy) != 3)
                fatal_error("new_grid: YDEF wrong:%s",arg3);

            save->nx = nnx;
            save->ny = nny;
            save->npnts_out = n_out = nnx*nny;
            if (n_out <= 0) fatal_error("new_grid: bad latlon","");

            // make a new section 3
            s = sec3_lola(nnx, x0, dx, nny, y0, dy, sec, NULL, NULL);
	}
        else if (strcmp(arg1,"gaussian") == 0) {
            if (sscanf(arg2,"%lf:%d:%lf", &x0, &nnx, &dx) != 3)
                fatal_error("new_grid: XDEF wrong:%s",arg2);
            if (sscanf(arg3,"%lf:%d", &y0, &nny) != 2)
                fatal_error("new_grid: YDEF wrong:%s",arg3);
            save->nx = nnx;
            save->ny = nny;
            save->npnts_out = n_out = nnx*nny;
            if (n_out <= 0) fatal_error("new_grid: bad gauss","");
            // make a new section 3
            s = sec3_gaussian(nnx, x0, dx, nny, y0, sec, NULL, NULL);
	}
        else if (strncmp(arg1,"lambert:",8) == 0) {
            i = sscanf(arg1,"lambert:%lf:%lf:%lf:%lf", &lov, &latin1, &latin2, &lad);
            if (i < 2) fatal_error("new_grid: arg1 wrong:%s",arg1);
            if (lov < 0.0)  lov += 360.0;
            if (i < 3) latin2 = latin1;
            if (i < 4) lad = latin2;
            proj = 0;
            if (latin2 < 0.0) proj = 128;

            if (sscanf(arg2,"%lf:%d:%lf", &x0, &nnx, &dx) != 3)
                fatal_error("new_grid: XDEF wrong:%s",arg2);
            if (sscanf(arg3,"%lf:%d:%lf", &y0, &nny, &dy) != 3)
                fatal_error("new_grid: YDEF wrong:%s",arg3);

            save->nx = nnx;
            save->ny = nny;
            save->npnts_out = n_out = nnx*nny;
            if (n_out <= 0) fatal_error("new_grid: bad lambert conformal","");

            // make a new section 3
            s = sec3_lc(lov, lad, latin1, latin2, proj, nnx, x0, dx, nny, y0, dy, sec, NULL, NULL);
        }
        else if (strncmp(arg1,"nps:",4) == 0 || strncmp(arg1,"sps:",4) == 0)  {

            if (sscanf(arg1,"%*[ns]ps:%lf:%lf", &lov, &lad) != 2) fatal_error("new_grid: arg1 wrong:%s",arg1);
            proj = 0;
	    if (arg1[0] == 's') proj = 128;
            if (sscanf(arg2,"%lf:%d:%lf", &x0, &nnx, &dx) != 3)
                fatal_error("new_grid: XDEF wrong:%s",arg2);
            if (sscanf(arg3,"%lf:%d:%lf", &y0, &nny, &dy) != 3)
                fatal_error("new_grid: YDEF wrong:%s",arg3);
	    if (lov < 0.0)  lov += 360.0;

            save->nx = nnx;
            save->ny = nny;
            save->npnts_out = n_out = nnx*nny;
            if (n_out <= 0) fatal_error("new_grid: bad polar sterographic","");

            // make a new section 3
	    s = sec3_polar_stereo(lov, lad, proj, nnx, x0, dx, nny, y0, dy, sec, NULL, NULL);
	}
        else fatal_error("new_grid: unsupported output grid %s", arg1);

	// save new section 3
        i = (int) uint4(s);         // size of section 3
        new_sec[3] = save->sec3 = (unsigned char *) malloc(i * sizeof(unsigned char));
        for (j = 0; j < i; j++) save->sec3[j] = s[j];

	// apply wind rotation .. change flag 3.3
	if (wind_rotation == grid && (p = flag_table_3_3_location(new_sec))) *p = *p | 8;

        if (mk_kgds(new_sec, save->kgds_out)) fatal_error("new_grid: encoding output kgds","");

	/* some vectors need by interpolation routines */
        if ((save->rlat = (float *) malloc(n_out * sizeof(float))) == NULL)
                fatal_error("new_grid memory allocation","");
        if ((save->rlon = (float *) malloc(n_out * sizeof(float))) == NULL)
                fatal_error("new_grid memory allocation","");
        if ((save->crot = (float *) malloc(n_out * sizeof(float))) == NULL)
                fatal_error("new_grid memory allocation","");
        if ((save->srot = (float *) malloc(n_out * sizeof(float))) == NULL)
                fatal_error("new_grid memory allocation","");

	return 0;
    }

    save = *local;
    if (mode == -2) {			// cleanup
#ifdef G95
	if (g95_runstop == 1) { g95_runtime_stop(); g95_runstop = 0; }
#endif
	if (save->has_u > 0) {
	    free(save->u_val);
	    free_sec(save->clone_sec);
	}
	free(save->rlon);
	free(save->rlat);
	free(save->crot);
	free(save->srot);
	free(save->sec3);
	free(save);

	return 0;
    }

    if (mode >= 0) {			// processing
	if (output_order != raw) fatal_error("new_grid: must be in raw output order","");
	gds = sec[3];
        i = getName(sec, mode, NULL, name, NULL, NULL);
	is_u = is_v = 0;
	for (j = 0 ; j < sizeof(vectors) / sizeof(vectors[0]); j++) {\
	    if (strcmp(name,vectors[j]) == 0) {
		if (j % 2 == 0) is_u = 1;
		else is_v = 1;
		break;
	    }
	}
// printf(" %s isu %d isv %d ", name, is_u, is_v);
// for (i = 0; i < 12; i++) { printf("kgds_out[%d] = %d ",i,save->kgds_out[i]); }

	// check if V matches expectation

	if (is_v && (same_sec0(sec,save->clone_sec) != 1 ||
            same_sec1(sec,save->clone_sec) != 1 ||
            same_sec3(sec,save->clone_sec) != 1 ||
            same_sec4(sec,save->clone_sec) != 1) ) {
	    fprintf(stderr,"new_grid: %s doesn't pair with previous field, field ignored", name);
	    return 0;
	}

	// if U field - save

        if (is_u) {
            if (save->has_u > 0) {
                fprintf(stderr,"new_grid: discarding U\n");
                free(save->u_val);
                free_sec(save->clone_sec);
            }
            copy_sec(sec, save->clone_sec);
            copy_data(data,ndata,&(save->u_val));
            GB2_ParmNum(save->clone_sec) = GB2_ParmNum(sec) + 1;
            save->has_u = is_u;
            return 0;
        }

	// at this point will call polates with either a scalar or vector

	n_out = save->npnts_out;
	nnx = save->nx;
	nny = save->ny;
	km = 1;			// only one field

	if (mk_kgds(sec, kgds)) fatal_error("new_grid: encoding input kgds","");

	data_in = (float *) malloc(npnts * (1 + (is_v != 0)) * sizeof(float));
        bitmap = (unsigned char *) malloc(npnts * sizeof(unsigned char));
        bitmap_out = (unsigned char *) malloc(n_out * sizeof(unsigned char));
	data_out = (float *) malloc(n_out * (1 + (is_v != 0)) * sizeof(float));

	if (data_in == NULL || data_out == NULL || bitmap == NULL || bitmap_out == NULL) 
	    fatal_error("new_grid: memory allocation problem","");

	ibi = 0;                        // input bitmap is not used
	if (is_v) {
	    for (i = 0; i < npnts; i++) {
                if (DEFINED_VAL(data[i]) && DEFINED_VAL(save->u_val[i])) {
                    data_in[i] = save->u_val[i];
                    data_in[i+npnts] = data[i];
                    bitmap[i] = 1;
		}
		else {
                    data_in[i] = data_in[i + npnts] = 0.0;
                    bitmap[i] = 0;
                    ibi = 1;                // input bitmap is used
		}
	    }
	}
	else {
	    for (i = 0; i < npnts; i++) {
                if (DEFINED_VAL(data[i])) {
                    data_in[i] = data[i];
                    bitmap[i] = 1;
		}
		else {
                    data_in[i] = 0.0;
                    bitmap[i] = 0;
                    ibi = 1;                // input bitmap is used
		}
	    }
	}

	// interpolate

// for (i = 0; i < 12; i++) { printf("\nkgds_in[%d] = %d  out=%d ",i,kgds[i],save->kgds_out[i]); }
	i = n_out;
	if (is_v) {
	    IPOLATEV(&interpol_type, ipopt,kgds,save->kgds_out, 
		&npnts, &n_out, &km, &ibi, bitmap, data_in, data_in+npnts, 
		&i,save->rlat,save->rlon, save->crot, save->srot,
                &ibo, bitmap_out, data_out, data_out + n_out, &iret);
	}
	else {
	    IPOLATES(&interpol_type, ipopt,kgds,save->kgds_out, 
		&npnts, &n_out, &km, &ibi, bitmap, data_in, &i,
		save->rlat,save->rlon, &ibo, bitmap_out, data_out, &iret);
	}
	if (iret != 0) fatal_error_i("new_grid: polates0 iret=%d",iret);

        /* use bitmap to set UNDEFINED values */
        if (ibo == 1) {         // has a bitmap
	    if (is_v) {
                for (i = 0; i < n_out; i++) {
		    if (bitmap_out[i] == 0) data_out[i] = data_out[i+n_out] = UNDEFINED;
		}
	    }
	    else {
                for (i = 0; i < n_out; i++) {
		    if (bitmap_out[i] == 0) data_out[i] = UNDEFINED;
		}
            }
	} 

	// now to write out the grib file

	for (i = 0; i < 8; i++) new_sec[i] = sec[i];
	new_sec[3] = save->sec3;

	if (is_v != 0) {
            GB2_ParmNum(new_sec) = GB2_ParmNum(new_sec) - 1;
            grib_wrt(new_sec, data_out, n_out, nnx, nny, use_scale, dec_scale, bin_scale,
                wanted_bits, max_bits, grib_type, save->out);
            GB2_ParmNum(new_sec) = GB2_ParmNum(new_sec) + 1;
            grib_wrt(new_sec, data_out+n_out, n_out, nnx, nny, use_scale, dec_scale, bin_scale,
                wanted_bits, max_bits, grib_type, save->out);
	}
	else {
            grib_wrt(new_sec, data_out, n_out, nnx, nny, use_scale, dec_scale, bin_scale,
                wanted_bits, max_bits, grib_type, save->out);
	}
        if (flush_mode) fflush(save->out);
        free(data_in);
        free(bitmap);
        free(bitmap_out);
	free(data_out);
	if (is_v != 0) {
	    save->has_u = 0;
            free(save->u_val);
            free_sec(save->clone_sec);
	}
    }
    return 0;
}

#else
int f_new_grid_interpolation(ARG1) {
    fprintf(stderr,"IPOLATES package is not installed\n");
    return 1;
}
int f_new_grid_ipopt(ARG1) {
    fprintf(stderr,"IPOLATES package is not installed\n");
    return 1;
}
int f_new_grid(ARG4) {
    fprintf(stderr,"IPOLATES package is not installed\n");
    return 1;
}
int f_new_grid_winds(ARG1) {
    fprintf(stderr,"IPOLATES package is not installed\n");
    return 1;
}
#endif

