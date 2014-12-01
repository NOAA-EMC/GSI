#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * ncep_norm
 *
 *  v 0.1 experimental
 *
 * 3/2009: Public Domain: Wesley Ebisuzaki
 *
 */

extern int decode, file_append, nx, ny, save_translation;
extern int flush_mode;
extern int use_scale, dec_scale, bin_scale, wanted_bits, max_bits;

extern enum output_grib_type grib_type;

/*
 * HEADER:100:ncep_norm:output:1:normalize NCEP-type ave/acc X=output grib file
 */



int f_ncep_norm(ARG1) {

    struct local_struct {
        float *val;
        int has_val;
        unsigned char *clone_sec[9];
        FILE *output;
    };
    struct local_struct *save;

    int j, pdt, fhr_1, fhr_2,  dt1, dt2, new_type, is_ave;
    unsigned int i;
    float *d1, *data_tmp;

    if (mode == -1) {			// initialization
        save_translation = decode = 1;

	// allocate static variables

        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation f_normalize","");

        if ((save->output = (void *) ffopen(arg1, file_append ? "ab" : "wb")) == NULL) {
	    fatal_error("f_ncep_norm: could not open file %s", arg1);
	}
	save->has_val = 0;
	init_sec(save->clone_sec);
	return 0;
    }

    save = *local;

    if (mode == -2) {			// cleanup
	if (save->has_val == 1) {
	    free(save->val);
	    free_sec(save->clone_sec);
	    free(save);
	}
	return 0;
    }

    if (mode >= 0) {			// processing

	// only hande PDT = 8 
	pdt = GB2_ProdDefTemplateNo(sec);
	if (pdt != 8) return 0;

	// only process averages or accumulations

	// check for code table 4.8

	j = code_table_4_10(sec);
	if (mode == 99) fprintf(stderr,"\nncep_norm: code table 4.10 (ave/acc/etc)=%d\n",j);
	if (j == 0) is_ave = 1;			// average
	else if (j == 1) is_ave = 0;		// accumulation
	else return 0;				// only process average or accumulations

	
	// only process when fcst time units == ave/acc time units
        if (sec[4][17] != sec[4][48]) {
            if (int4(sec[4]+18) != 0) {
		return 0;
	    }
	}

        fhr_2 = int4(sec[4]+18);			// start time
        dt2 = int4(sec[4]+49);				// delta-time
	if (dt2 == 0) return 0;	 			// dt == 0

	if (save->has_val == 0) {			// new data: write and save
            if ((data_tmp = (float *) malloc(ndata * sizeof(float))) == NULL)
                fatal_error("memory allocation - data_tmp","");
            undo_output_order(data, data_tmp, ndata);

            grib_wrt(sec, data_tmp, ndata, nx, ny, use_scale, dec_scale, bin_scale,
                wanted_bits, max_bits, grib_type, save->output);
/*
            if (grib_type == simple) grib_out(sec, data_tmp, ndata, save->output);
            else if (grib_type == jpeg) jpeg_grib_out(sec, data_tmp, ndata, nx,
                     ny, use_scale, dec_scale, bin_scale, save->output);
            else if (grib_type == ieee) ieee_grib_out(sec, data_tmp, ndata,
                save->output);
	    else fatal_error("NCEP_norm: unsupported grib type output","");
*/

            if (flush_mode) fflush(save->output);
            free(data_tmp);

            if (save->has_val  == 1) {			// copy data to save
                free(save->val);			// save = new field
                free_sec(save->clone_sec);
	    }
            copy_sec(sec, save->clone_sec);
            copy_data(data,ndata,&(save->val));
            save->has_val = 1;
	    if (mode == 99) fprintf(stderr," ncep_norm: saved as new field\n");
            return 0;
        }


        new_type = 0;

        fhr_1 = int4(save->clone_sec[4]+18);             // start time of save message
        dt1 = int4(save->clone_sec[4]+49);               // delta time

	if (mode == 99) fprintf(stderr,"ncep_norm: is_ave = %d\n fhr_1 %d dt1 %d fhr_2 %d dt2 %d\n",is_ave,
		fhr_1, dt1, fhr_2, dt2);

	if (fhr_1 != fhr_2) new_type = 1;

	if (new_type == 0) {
	    if (same_sec0(sec,save->clone_sec) == 0 ||
            same_sec1(sec,save->clone_sec) == 0 ||
            same_sec3(sec,save->clone_sec) == 0 ||
            same_sec4_diff_ave_period(sec,save->clone_sec) == 0) {
                new_type = 1;
	    }
        }

        if (new_type == 1) {                    // fields dont match: write and save
            if ((data_tmp = (float *) malloc(ndata * sizeof(float))) == NULL)
                fatal_error("memory allocation - data_tmp","");
            undo_output_order(data, data_tmp, ndata);

            grib_wrt(sec, data_tmp, ndata, nx, ny, use_scale, dec_scale, bin_scale,
                wanted_bits, max_bits, grib_type, save->output);
/*
            if (grib_type == simple) grib_out(sec, data_tmp, ndata, save->output);
            else if (grib_type == jpeg) jpeg_grib_out(sec, data_tmp, ndata, nx,
                     ny, use_scale, dec_scale, bin_scale, save->output);
            else if (grib_type == ieee) ieee_grib_out(sec, data_tmp, ndata,
                save->output);
*/

            if (flush_mode) fflush(save->output);
            free(data_tmp);

            if (save->has_val  == 1) {                  // copy data to save
                free(save->val);                        // save = new field
                free_sec(save->clone_sec);
            }
            copy_sec(sec, save->clone_sec);
            copy_data(data,ndata,&(save->val));
            save->has_val = 1;
	    if (mode == 99) fprintf(stderr," ncep_norm: saved as new type/sequence\n");
            return 0;
        }

        /* now do stuff */

	if (dt1 == dt2) return 0;			// same ending time

        // change metadata

        int_char(dt2-dt1, save->clone_sec[4]+49);		//  dt = dt2 - dt1
        save->clone_sec[4][17] = sec[4][48];                    // new forecast time unit
        int_char(dt1+fhr_1, save->clone_sec[4]+18);             // fhr = fhr + dt1

        for (i = 34; i < 41; i++) {                             // ending time from pds2
            save->clone_sec[4][i] = sec[4][i];
        }

	if (mode == 99) {
	    if (is_ave)
	        fprintf(stderr," process: factor: NEW*%g - OLD*%g\n", dt2/ (double) (dt2 - dt1),
			dt1/ (double) (dt2-dt1));
	    else fprintf(stderr," process: current-last\n");
	}

        // change floating point data

        d1= save->val;
        for (i = 0; i < ndata; i++) {
            if (!UNDEFINED_VAL(data[i]) && !UNDEFINED_VAL(*d1) ) {
		if (is_ave) {
                    *d1 = (data[i]*dt2 - *d1*dt1) / (double) (dt2 - dt1);
		}
		else {		// accumulation
                    *d1 = data[i] - *d1;
		}
	    }
            else *d1 = UNDEFINED;
            d1++;
	}

        // write grib output

        if ((data_tmp = (float *) malloc(ndata * sizeof(float))) == NULL)
                fatal_error("memory allocation - data_tmp","");
        undo_output_order(save->val, data_tmp, ndata);

        grib_wrt(save->clone_sec, data_tmp, ndata, nx, ny, use_scale, dec_scale, bin_scale,
            wanted_bits, max_bits, grib_type, save->output);
/*
        if (grib_type == simple) grib_out(save->clone_sec, data_tmp, ndata, save->output);
        else if (grib_type == jpeg) jpeg_grib_out(save->clone_sec, data_tmp, ndata, nx,
                     ny, use_scale, dec_scale, bin_scale, save->output);
        else if (grib_type == ieee) ieee_grib_out(save->clone_sec, data_tmp, ndata,
                save->output);
        else if (grib_type == complex1) complex_grib_out(save->clone_sec, data_tmp, ndata, 
		use_scale, dec_scale, bin_scale, wanted_bits, max_bits, 1, save->output);
        else if (grib_type == complex2) complex_grib_out(save->clone_sec, data_tmp, ndata, 
		use_scale, dec_scale, bin_scale, wanted_bits, max_bits, 2, save->output);
        else if (grib_type == complex3) complex_grib_out(save->clone_sec, data_tmp, ndata, 
		use_scale, dec_scale, bin_scale, wanted_bits, max_bits, 3, save->output);
	else fatal_error("NCEP_norm: unsupported grib output type","");
*/

        if (flush_mode) fflush(save->output);
        free(data_tmp);

	// save data
        free(save->val);                    // save = new field
        free_sec(save->clone_sec);
        copy_sec(sec, save->clone_sec);
        copy_data(data,ndata,&(save->val));
        return 0;
    }
    return 0;
}
