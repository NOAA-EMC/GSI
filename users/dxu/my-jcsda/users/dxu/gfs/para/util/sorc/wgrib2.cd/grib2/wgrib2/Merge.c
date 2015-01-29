#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * merge
 *
 *  v 0.1 experimental
 *
 * 8/2009: Public Domain: Wesley Ebisuzaki
 *
 */

extern int decode, file_append, nx, ny, save_translation;
extern int flush_mode;
extern int use_scale, dec_scale, bin_scale, wanted_bits, max_bits;
extern enum output_grib_type grib_type;

/*
 * HEADER:100:merge_fcst:output:2:normalize NCEP-type ave/acc X=number Y=output grib file
 */

enum processing_type {ave, acc, max, min};

int f_merge_fcst(ARG2) {

    struct local_struct {
        float *val;				// grid point value accumulator
        int has_val;				// 1 = val is valid
        int n;					// number of grids accumulated
	int nx, ny, use_scale, dec_scale;
	int bin_scale, wanted_bits, max_bits;
	enum output_grib_type grib_type;
	int ndata;				// size of grid
	int last_fhour;				// last fhour
	int is_ave;
	enum processing_type processing;	// ave, acc, max, min
        unsigned char *clone_sec[9];		// copy of original sec
	unsigned char last_end_time[9];		// copy of the last end_of_overal_period
	int num_to_merge;			// check that have right number
        FILE *output;				// output file
    };
    struct local_struct *save;

    int i, pdt, new_type, is_ave;
    float *d, *data_tmp, factor;

    if (mode == -1) {			// initialization
        save_translation = decode = 1;

	// allocate static variables

        save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation f_merge","");

	if ((save->num_to_merge = atoi(arg1)) < 1) fatal_error("merge_fcst: bad number","");
        if ((save->output = (void *) ffopen(arg2, file_append ? "ab" : "wb")) == NULL) 
	    fatal_error("f_merge: could not open file %s", arg1);
	
	save->has_val = 0;
	init_sec(save->clone_sec);
        *local = save;
	return 0;
    }

    save = *local;

    if (mode == -2) {			// cleanup
	if (mode == 99) fprintf(stderr,"merge_fcst: cleanup - has_val=%d num_to_merge=%d n=%d\n",
		save->has_val, save->num_to_merge, save->n);
	if (save->has_val == 1 && save->num_to_merge == save->n) {
	    d = save->val;
/*
	    if (save->is_ave == 1) {
	        factor = 1.0 / (double) save->n;
	        for (i = 0; i < save->ndata; i++) {
		    if (DEFINED_VAL(d[i])) d[i] *= factor;
	        }
	    }
*/
	    if (mode == 99) fprintf(stderr,"merge_fcst: cleanup - write field\n");
	    if (save->processing == ave) {
	        factor = 1.0 / (double) save->n;
	        for (i = 0; i < save->ndata; i++) {
		    if (DEFINED_VAL(d[i])) d[i] *= factor;
	        }
	    }

	    // change time codes
            uint_char(uint4(save->clone_sec[4]+49) * save->n, save->clone_sec[4]+49);
	    memcpy(save->clone_sec[4]+34,save->last_end_time,9);

            grib_wrt(save->clone_sec, d, save->ndata, save->nx, save->ny, save->use_scale, 
		save->dec_scale, save->bin_scale, save->wanted_bits, save->max_bits, 
		save->grib_type, save->output);
            if (flush_mode) fflush(save->output);
	}
	if (save->has_val) {
	    free(save->val);
	    free_sec(save->clone_sec);
	}
	free(save);
	return 0;
    }

    if (mode >= 0) {			// processing

	// only process averages or accumulations, PDT = 8 
	pdt = GB2_ProdDefTemplateNo(sec);
	if (pdt != 8) return 0;

	i = code_table_4_10(sec);
/*
	if (i == 0) is_ave = 1;			// average
	else if (i == 1) is_ave = 0;		// accumulation
*/
	if (i == 0) save->processing = ave;
	else if (i == 1) save->processing = acc;
	else if (i == 2) save->processing = max;
	else if (i == 3) save->processing = min;
	else return 0;				// only process ave,acc,min,max

if (mode == 99)  fprintf(stderr,"merge_fcst: code_table 4.10=%d  ",i);

	// translate the data into raw mode now because the translation table
	// will be different for a new grid or missing at mode == -2

        if ((data_tmp = (float *) malloc(ndata * sizeof(float))) == NULL)
                fatal_error("memory allocation - data_tmp","");
        undo_output_order(data, data_tmp, ndata);

	// if (empty) { save_data ; return }
	// if (old field) { add to accumulator , n++ ; return }
	// if (new field) { process ; write out; save_data; return }

	if (save->has_val == 0) {			// new data: write and save
	    if (mode == 99) fprintf(stderr,"\nmerge: first field");
            copy_sec(sec, save->clone_sec);
            copy_data(data_tmp,ndata,&(save->val));
	    memcpy(save->last_end_time,sec[4]+34,9);
            save->has_val = 1;
            save->n = 1;
	    save->last_fhour = int4(sec[4]+18);
	    save->is_ave = is_ave;
	    save->nx = nx;
	    save->ny = ny;
	    save->use_scale = use_scale;
 	    save->dec_scale = dec_scale;
	    save->bin_scale = bin_scale;
	    save->wanted_bits = wanted_bits;
	    save->grib_type = grib_type;
	    save->max_bits = max_bits;
	    save->ndata = ndata;
	    free(data_tmp);
	    return 0;
	}

        new_type = 0;
//        fprintf(stderr,"test sec0=%d\n",same_sec0(sec,save->clone_sec));
//        fprintf(stderr,"test sec1=%d\n",same_sec1(sec,save->clone_sec));
//        fprintf(stderr,"test sec3=%d\n",same_sec3(sec,save->clone_sec));
//        fprintf(stderr,"test sec4=%d\n",same_sec4_for_merge(sec,save->clone_sec));

	if (save->last_fhour >= int4(sec[4]+18)) new_type = 1;		// fhour should be >= old fhour
									// do better test here
        if (new_type == 0) {
            if (same_sec0(sec,save->clone_sec) == 0 ||
            same_sec1(sec,save->clone_sec) == 0 ||
            same_sec3(sec,save->clone_sec) == 0 ||
            same_sec4_for_merge(sec,save->clone_sec) == 0) {
                new_type = 1;
            }
        }

//	fprintf(stderr,"new type %d\n",new_type);
	if (mode == 99) fprintf(stderr,"new type %d\n",new_type);

	if (new_type == 0) {		
	    d = save->val;
/*
	    for (i = 0; i < ndata; i++) {
		if (UNDEFINED_VAL(d[i]) || UNDEFINED_VAL(data_tmp[i])) d[i] = UNDEFINED;
		else d[i] += data_tmp[i];
	    }
*/
	    if (save->processing == acc || save->processing == ave) {
	        for (i = 0; i < ndata; i++) {
		    if (UNDEFINED_VAL(d[i]) || UNDEFINED_VAL(data_tmp[i])) d[i] = UNDEFINED;
		    else d[i] += data_tmp[i];
	        }
	    }
	    else if (save->processing == max) {
	        for (i = 0; i < ndata; i++) {
		    if (UNDEFINED_VAL(d[i])) d[i] = data_tmp[i];
		    else {
 			if (DEFINED_VAL(data_tmp[i]) && (d[i] < data_tmp[i])) d[i] = data_tmp[i];
		    }
	        }
	    }
	    else if (save->processing == min) {
	        for (i = 0; i < ndata; i++) {
		    if (UNDEFINED_VAL(d[i])) d[i] = data_tmp[i];
		    else {
 			if (DEFINED_VAL(data_tmp[i]) && (d[i] > data_tmp[i])) d[i] = data_tmp[i];
		    }
	        }
	    }

            save->n = save->n + 1;
	    save->last_fhour = int4(sec[4]+18);
	    memcpy(save->last_end_time,sec[4]+34,9);
	    free(data_tmp);
	    return 0;
	}

	// write the average/accumulation

	if (save->n == save->num_to_merge) {
	    d = save->val;
/*
	    if (save->is_ave == 1) {
	        factor = 1.0 / (double) save->n;
	        for (i = 0; i < save->ndata; i++) {
		    if (DEFINED_VAL(d[i])) d[i] *= factor;
	        }
	    }
*/
	    if (save->processing == ave) {
	        factor = 1.0 / (double) save->n;
	        for (i = 0; i < save->ndata; i++) {
		    if (DEFINED_VAL(d[i])) d[i] *= factor;
	        }
	    }

	    // change time codes
            uint_char(uint4(save->clone_sec[4]+49) * save->n, save->clone_sec[4]+49);
	    memcpy(save->clone_sec[4]+34,save->last_end_time,9);

            grib_wrt(save->clone_sec, d, save->ndata, save->nx, save->ny, save->use_scale, 
		save->dec_scale, save->bin_scale, save->wanted_bits, save->max_bits, 
		save->grib_type, save->output);
            if (flush_mode) fflush(save->output);
	}

	// save data
        free(save->val);                        // save = new field
        free_sec(save->clone_sec);
        copy_sec(sec, save->clone_sec);
        copy_data(data_tmp,ndata,&(save->val));
	memcpy(save->last_end_time,sec[4]+34,9);
	save->last_fhour = int4(sec[4]+18);
        save->has_val = 1;
        save->n = 1;
	save->is_ave = is_ave;
	save->nx = nx;
	save->ny = ny;
	save->use_scale = use_scale;
	save->dec_scale = dec_scale;
	save->bin_scale = bin_scale;
	save->wanted_bits = wanted_bits;
	save->max_bits = max_bits;
	save->grib_type = grib_type;
	save->ndata = ndata;
	if (mode == 99) fprintf(stderr," ncep_merge: saved as new type/sequence\n");
	free(data_tmp);
	return 0;
    }
    return 0;
}
