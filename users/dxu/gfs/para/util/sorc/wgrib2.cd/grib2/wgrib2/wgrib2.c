/* wgrib2 main module:  w. ebisuzaki
 *
 * CHECK code is now duplicated
 *  if (decode) -- check before decoding
 *  if (!decode) -- check after processing one record so you can use wgrib2 to look at the field
 *
 * 1/2007 mods M. Schwarb: unsigned int ndata
 * 2/2008 WNE add -if support
 * 2/2008 WNE fixed bug in processing of submessages
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>

#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"
#include "grib2.h"

/* #define DEBUG */
#define CHECK

/* global variables .. can be modified by funtions */

int mode=0;             /* -2=finalize, -1=initialize,  0 .. N is verbosity mode */
int header=1;           /* file header flag */
int flush_mode = 0;	/* flush of output 1 = yes */
int save_translation = 0;
int use_g2clib = USE_G2CLIB;	/* use g2clib code for decoding */
int fix_ncep_2_flag = 0;	
int fix_ncep_3_flag = 0;	
int fix_ncep_4_flag = 0;

int for_mode = 0, for_start, for_end, for_step;
int for_n_mode = 0, for_n_start, for_n_end, for_n_step;

#ifdef USE_REGEX
int match = 0;
int match_flag = 0;
#endif
int last_message = 0;	/* last message to process if set */

FILE *inv_file;


enum input_type input = all_mode;
enum output_order_type output_order = wesn, output_order_wanted = wesn;

char inv_out[INV_BUFFER]; 		/* inv functions write to this buffer */

int only_submsg = 0;    /* if only_submsg > 0 .. only process submsg number (only_submsg) */



/* output file variables */

int file_append = 0;
int dump_msg = 0, dump_submsg = 0;
int ieee_little_endian = 0;

int decode = 0;		/* decode grib file flag */

char *item_deliminator = ":";
char *nl = "\n\t";

/* current grib location */
long int pos;
unsigned long int len;
int submsg, msg_no, inv_no;

/* lat lon array .. used by Latlon.c to store location information */
double *lat = NULL, *lon = NULL;
int latlon = 0;
int new_GDS = 0;
int old_GDS_size = 0;
int GDS_max_size = 100;
unsigned char *old_gds;
int nx, ny, res, scan;
unsigned int npnts;
int use_scale = 0, dec_scale, bin_scale,  max_bits = 16, wanted_bits = 12;
enum output_grib_type grib_type = simple;


/* may disappear in future versions  grib2 decoder variables */
gribfield *grib_data;
int free_gribfield = 0;			// flag for allocated gribfield
int free_sec4 = 0;			// flag for allocated sec4

/*
 * wgrib2
 *
 * simple wgrib for GRIB2 files
 *
 */


int main(int argc, char **argv) {
    FILE *in;
    unsigned char *msg, *sec[9];
    long int last_pos;

    int file_arg, i, j, num_submsgs;
    int n_arg;
    unsigned int k, ndata;
    float *data, missing_c_val_1, missing_c_val_2;
    double *ddata, ref;
    g2int *bitmap, has_bitmap;
    g2float *g2_data;

    struct ARGLIST arglist[N_ARGLIST];
    int narglist = 0;
    char *new_argv[N_ARGLIST];
    void *local[N_ARGLIST];
    int has_inv_option, last_submsg;
    int err;

    inv_file = stdout;
//    jas_init();

    /* no arguments .. help screen */
    if (argc == 1) {
	f_help(-1,NULL,NULL,0,inv_out,local,"most");
	fprintf(inv_file, "%s\n", inv_out);
	exit(8);
    }

    /* copy argv */

    for (i = 0; i < argc; i++) {
	new_argv[i] = argv[i];
    }

    /* scan for "inv" and input file */
    has_inv_option = 0;
    file_arg = 0;
    for (i = 1; i < argc; i++) {
	if (new_argv[i][0] != '-') {
	    /* must be filename */
            file_arg = i;
            continue;
        }
	/* must be an option */
	for (j = 0; j < nfunctions; j++) {
	    if (strcmp(&(new_argv[i][1]),functions[j].name) == 0) {
	        if (functions[j].type == inv) has_inv_option = 1;
		i += functions[j].nargs;
                break;
            }
        }
    }

    /* if no inv option, use default inventory .. put it at end */

    if (has_inv_option == 0) {
	for (i = 0; i < argc; i++) {
	    new_argv[i] = new_argv[i];
	}
	new_argv[argc++] = "-s";
    } 


    /* parse parameters */
    file_arg = 0;
    for (i = 1; i < argc; i++) {

	if (new_argv[i][0] != '-') {
	    /* must be filename */
	    if (file_arg == 0) {
		file_arg = i;
		continue;
	    } else {
		fatal_error("too many grib files .. 2nd=%s", new_argv[i]);
	    }
	}

	/* must be an option */

	for (j = 0; j < nfunctions; j++) {
	    if (strcmp(&(new_argv[i][1]),functions[j].name) == 0) {
#ifdef DEBUG
		fprintf(stderr,"match .. -%s %d args\n",  functions[j].name, functions[j].nargs);
#endif
                /* add to function argument list */
		arglist[narglist].fn = j;
		arglist[narglist].i_argc = i+1;

	        if (functions[j].type == inv) has_inv_option = 1;

		i += functions[j].nargs;
		if (i >= argc) fatal_error("missing arguments option=%s",functions[j].name);
		narglist++;
		break;
	    }
	}

	if (j == nfunctions) {
	    fatal_error("unknown option %s", new_argv[i]);
	}
    }

    if (has_inv_option == 0) {
        fatal_error("missing arguments on last option","");
    }

    /* initialize options mode = -1 */

#ifdef DEBUG
    fprintf(stderr,"init options narglist %d\n",narglist);
#endif

    for (j = 0; j < narglist; j++) {
	inv_out[0] = 0;
	n_arg = functions[arglist[j].fn].nargs;
        err = 0;
        if (n_arg == 0) err = functions[arglist[j].fn].fn(-1,NULL,NULL,0, inv_out,local+j);
	else if (n_arg == 1) err = functions[arglist[j].fn].fn(-1,NULL,NULL,0, inv_out,local+j,
		new_argv[arglist[j].i_argc]);
	else if (n_arg == 2) err = functions[arglist[j].fn].fn(-1,NULL,NULL,0, inv_out,local+j,
		new_argv[arglist[j].i_argc],new_argv[arglist[j].i_argc+1]);
	else if (n_arg == 3) err = functions[arglist[j].fn].fn(-1,NULL,NULL,0, inv_out,local+j,
		new_argv[arglist[j].i_argc],new_argv[arglist[j].i_argc+1],new_argv[arglist[j].i_argc+2]);
	else if (n_arg == 4) err = functions[arglist[j].fn].fn(-1,NULL,NULL,0, inv_out,local+j,
		new_argv[arglist[j].i_argc],new_argv[arglist[j].i_argc+1],
		new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3]);
	else if (n_arg == 5) err = functions[arglist[j].fn].fn(-1,NULL,NULL,0, inv_out,local+j,
		new_argv[arglist[j].i_argc],new_argv[arglist[j].i_argc+1],
		new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
		new_argv[arglist[j].i_argc+4]);
	else if (n_arg == 6) err = functions[arglist[j].fn].fn(-1,NULL,NULL,0, inv_out,local+j,
		new_argv[arglist[j].i_argc],new_argv[arglist[j].i_argc+1],
		new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
		new_argv[arglist[j].i_argc+4], new_argv[arglist[j].i_argc+5]);
	else if (n_arg == 7) err = functions[arglist[j].fn].fn(-1,NULL,NULL,0, inv_out,local+j,
		new_argv[arglist[j].i_argc],new_argv[arglist[j].i_argc+1],
		new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
		new_argv[arglist[j].i_argc+4], new_argv[arglist[j].i_argc+5],
		new_argv[arglist[j].i_argc+6]);
	else if (n_arg == 8) err = functions[arglist[j].fn].fn(-1,NULL,NULL,0, inv_out,local+j,
		new_argv[arglist[j].i_argc],new_argv[arglist[j].i_argc+1],
		new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
		new_argv[arglist[j].i_argc+4], new_argv[arglist[j].i_argc+5],
		new_argv[arglist[j].i_argc+6], new_argv[arglist[j].i_argc+7]);

        if(inv_out[0] != 0)  fprintf(inv_file, "%s", inv_out);
        if (err) exit(8);
    }

    if (file_arg == 0 && argc > 1) fatal_error("no input file", "");
    if (latlon == 1 && output_order_wanted != wesn) 
           fatal_error("latitude-longitude information is only available with -order we:sn","");

    /* open input file */
    if ((in = fopen(new_argv[file_arg],"rb")) == NULL) {
        fatal_error("could not open file: %s", new_argv[file_arg]);
    }

    ndata = 0;
    data = NULL;
    ddata = NULL;
    msg_no = 1;
    inv_no = 0;
    len = pos = 0;
    submsg = 0;
    msg = NULL;

    if ((old_gds = (unsigned char *) malloc(GDS_max_size * sizeof(char)) ) == NULL) {
	fatal_error("memory allocation problem old_gds in wgrib2.main","");
    }
    
    last_pos = -1;
    last_submsg = -1;

    /* if dump mode .. position io stream */

    if (input == dump_mode) {
        while (msg_no < dump_msg) {
            if ((msg = rd_grib2_msg(in, &pos, &len,&num_submsgs)) == NULL) {
                fatal_error_i("record %d not found", dump_msg);
            }
            last_pos = pos;
            pos += len;
            msg_no++;
        }
#ifdef DEBUG
        printf("dump mode msg=%d\n", msg_no);
#endif
    }

    /* 
     * submsg = 0 .. beginning of unread record
     * submsg = i .. start at ith submsg
     * num_submsgs = number of submessages in grib message
     */

    /* inventory loop */ 

    for (;last_message == 0;) {

        /* need position and submessage number of message */
        if (input == inv_mode || input == dump_mode) {
            if (input == inv_mode) {
                if (rd_inventory(&msg_no,&submsg, &pos)) break;
            }
            else if (input == dump_mode) {
                if (dump_msg == -1) break;
                submsg = dump_submsg;
                dump_msg = -1;
	    }

            if (pos != last_pos) {
	        if ((msg = rd_grib2_msg(in, &pos, &len, &num_submsgs)) == NULL) {
                    fatal_error_i("grib message #%d not found", msg_no);
                    break;
                }
                last_pos = pos;
		last_submsg = -1;
            }

            if (pos == last_pos && submsg == last_submsg + 1) {
                /* read previous submessage */
		if (parse_next_msg(sec) != 0) {
                    fprintf(stderr,"\n*** grib message #%d.%d not found ***\n\n", msg_no, submsg);
                    break;
		}
            }
            else {
                /* need to get desired submessage into sec */
		if (parse_1st_msg(sec) != 0) {
                    fprintf(stderr,"\n*** grib message #%d.1 not found ***\n\n", msg_no);
                    break;
		}
                for (i = 2; i <= submsg; i++) {
		    if (parse_next_msg(sec) != 0) {
                        fprintf(stderr,"\n*** grib message #%d.%d not found ***\n\n", msg_no, i);
                        break;
                    }
		}
	    }
            last_submsg = submsg;
	}
        else if (input == all_mode) {
	    if (submsg == 0) {
	        if ((msg = rd_grib2_msg(in, &pos, &len, &num_submsgs)) == NULL) break;
                submsg = 1;
	    }
	    else if (submsg > num_submsgs) {
		pos += len;
                msg_no++;
	        if ((msg = rd_grib2_msg(in, &pos, &len, &num_submsgs)) == NULL) break;
                submsg = 1;
	    }
            if (submsg == 1) {
		if (parse_1st_msg(sec) != 0) {
		    fprintf(stderr,"illegal format: parsing 1st submessage\n");
		}
            }
            else {
		if (parse_next_msg(sec) != 0) {
                    fprintf(stderr,"illegal format: parsing submessages\n");
                }
	    }
	}
        if (only_submsg > 0 && only_submsg != submsg) {
	    submsg++;
	    continue;
	}

	if (for_mode) {
	    if (msg_no < for_start || msg_no > for_end || ((msg_no - for_start) % for_step) != 0) {
	        if (msg_no > for_end && input != inv_mode) last_message = 1;
		submsg++;
		continue;
	    }
	}

#ifdef USE_REGEX
        if (match) {
	   inv_out[0] = 0;
	   if (num_submsgs > 1) {
	       sprintf(inv_out,"%d.%d:", msg_no, submsg);
	   }
           else {
	       sprintf(inv_out,"%d:", msg_no);
	   }

           f_match_inv(0, sec, NULL, 0, inv_out+strlen(inv_out), NULL);
           if (is_match(inv_out) != 0) {
              submsg++;
              continue;
           }
        }
	match_flag = 0;
#endif

	inv_no++;

        if (for_n_mode) {
            if (inv_no < for_n_start || inv_no > for_n_end || ((inv_no - for_n_start) % for_n_step) != 0) {
                if (inv_no > for_n_end) last_message = 1;
                submsg++;
                continue;
            }
        }

        /* see if new GDS */

	if ((i = GB2_Sec3_size(sec)) != old_GDS_size) {
	    new_GDS = 1;
	}
	else {
	    new_GDS = 0;
	    for (j = 0; j < i; j++) {
		if (old_gds[j] != sec[3][j]) new_GDS = 1;
	    }
	}
	if (new_GDS) {
	    if (i > GDS_max_size) {
		free(old_gds);
		GDS_max_size = i;
    		if ((old_gds = (unsigned char *) malloc(GDS_max_size) ) == NULL) {
			fatal_error("memory allocation problem old_gds in wgrib2.main","");
		}
	    }
	    for (j = 0; j < i; j++) {
		old_gds[j] = sec[3][j];
            }
	    old_GDS_size = i;
	    /* update grid information */
            get_nxny(sec, &nx, &ny, &npnts, &res, &scan);	 /* get nx, ny, and scan mode of grid */
	    output_order = (nx == -1 || ny == -1) ? raw : output_order_wanted;
            if (latlon) get_latlon(sec);			 /* get lat lon of grid points */
	}

	// any fixes to raw grib message before decode need to be placed here
	if (fix_ncep_2_flag) fix_ncep_2(sec);
	if (fix_ncep_3_flag) fix_ncep_3(sec);
	if (fix_ncep_4_flag) fix_ncep_4(sec);


	if (decode) {

#ifdef CHECK
            if (code_table_6_0(sec) == 0) {                         // has bitmap
                k = GB2_Sec3_npts(sec) -  GB2_Sec5_nval(sec);
                if (k != missing_points(sec[6]+6, GB2_Sec3_npts(sec)))
                    fatal_error_ii("inconsistent number of bitmap points sec3-sec5: %d sec6: %d",
			k, missing_points(sec[6]+6, GB2_Sec3_npts(sec)));
            }
            else if (code_table_6_0(sec) == 255) {                  // no bitmap
                if (GB2_Sec3_npts(sec) != GB2_Sec5_nval(sec))
                    fatal_error_ii("inconsistent number of data points sec3: %d sec5: %d",
                        GB2_Sec3_npts(sec), GB2_Sec5_nval(sec));
            }
#endif

            /* allocate data */
            if (GB2_Sec3_npts(sec) != ndata) {
                if (ndata) free(data);
                ndata = GB2_Sec3_npts(sec);
                if (ndata) {
                    data = (float *) malloc(ndata * sizeof(float));
                    if (data == NULL) fatal_error("main: memory allocation failed data","");
                }
                else { data = NULL; }
            }

	    j = code_table_5_0(sec);		// type of compression

	    if ((j == 4 || j == 200) || ( (use_g2clib == 0) && (j == 0 || 
			j == 4 || j == 2 || j == 3 ||
			j == 40 || j == 41 || j == 40000))) {
		err = unpk_grib(sec, data);
                if (err != 0) {
                    fprintf(stderr,"Fatal decode packing type %d\n",j);
                    exit(8);
		}
	    }
            else {
  	        /* unpack grib field grib2clib */

//	        if (use_g2clib == 0 && (j == 2 || j == 3) &&
//			uint4(sec[5]+31) == 0 && int2(sec[5]+17) != 0) {
		    // change reference value so NCEP routines handle constant field correctly
		    // reference = reference * 10**-dec_scale
		    // dec_scale = 0
//		    flt2ieee(
//			ieee2flt(sec[5]+11)*Int_Power(10.0,-int2(sec[5]+17)),
//			sec[5]+11);
//		    sec[5][17] = sec[5][18] = 0;
//		}

	        err = g2_getfld(msg,submsg,1,1,&grib_data);
                if (err != 0) {
                    fprintf(stderr,"Fatal decode err=%d msg %d.%d\n",err, msg_no, submsg);
                    exit(8);
                }
		free_gribfield = 1;

	        has_bitmap = grib_data->ibmap;
	        g2_data = &(grib_data->fld[0]);
	        if (has_bitmap == 0 || has_bitmap == 254) {
		    bitmap = grib_data->bmap;
		    for (k = 0; k < ndata; k++) {
		         data[k] = (bitmap[k] == 0) ? UNDEFINED : g2_data[k];
		    }
	        }
	        else {
		    for (k = 0; k < ndata; k++) {
		        data[k] = *g2_data++;
		    }
	        }

	        /* complex packing uses special values for undefined */
		i = sub_missing_values(sec, &missing_c_val_1, &missing_c_val_2);
		if (i == 1) {
		    for (k = 0; k < ndata; k++) {
		        if (data[k] == missing_c_val_1) data[k] = UNDEFINED;
		    }
		}
  	        else if (i == 2) {
		    for (k = 0; k < ndata; k++) {
		        if (data[k] == missing_c_val_1) data[k] = UNDEFINED;
		        if (data[k] == missing_c_val_2) data[k] = UNDEFINED;
		    }
	        }

            }

	    /* convert to standard output order we:sn */

	    if (output_order_wanted == wesn) to_we_sn_scan(data);
	    else if (output_order_wanted == wens) to_we_ns_scan(data);
	}
        else {
	    if (ndata) free(data);
            ndata = 0;
            data = NULL;
        }

	/* get scaling parameters */

	use_scale = scaling(sec, &ref, &dec_scale, &bin_scale, &i) == 0;


	if (num_submsgs > 1) {
	    fprintf(inv_file, "%d.%d%s%ld", msg_no, submsg, ":", pos);
	}
        else {
	    fprintf(inv_file, "%d%s%ld", msg_no, ":", pos);
	}

	for (j = 0; j < narglist; j++) {

	    /* skip execution if match_flag == 1 */
	    /* an output option acts as endif for match_flag */
	    if (match_flag == 1) {
                if (functions[arglist[j].fn].type == output)  match_flag = 0;
		continue;
	    }


            if (functions[arglist[j].fn].type == inv) fprintf(inv_file, item_deliminator);
            if (functions[arglist[j].fn].type != setup) {
		inv_out[0] = 0;
	        n_arg = functions[arglist[j].fn].nargs;
		if (n_arg == 0) 
                    functions[arglist[j].fn].fn(mode, sec, data, ndata, inv_out, local+j);
		else if (n_arg == 1)
		    functions[arglist[j].fn].fn(mode, sec, data, ndata, inv_out, local+j,
			 new_argv[arglist[j].i_argc]);
		else if (n_arg == 2)
		    functions[arglist[j].fn].fn(mode, sec, data, ndata, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1]);
		else if (n_arg == 3)
		    functions[arglist[j].fn].fn(mode, sec, data, ndata, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2]);
		else if (n_arg == 4)
		    functions[arglist[j].fn].fn(mode, sec, data, ndata, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3]);
		else if (n_arg == 5)
		    functions[arglist[j].fn].fn(mode, sec, data, ndata, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
			new_argv[arglist[j].i_argc+4]);
		else if (n_arg == 6)
		    functions[arglist[j].fn].fn(mode, sec, data, ndata, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
			new_argv[arglist[j].i_argc+4], new_argv[arglist[j].i_argc+5]);
		else if (n_arg == 7)
		    functions[arglist[j].fn].fn(mode, sec, data, ndata, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
			new_argv[arglist[j].i_argc+4], new_argv[arglist[j].i_argc+5],
			new_argv[arglist[j].i_argc+6]);
		else if (n_arg == 8)
		    functions[arglist[j].fn].fn(mode, sec, data, ndata, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
			new_argv[arglist[j].i_argc+4], new_argv[arglist[j].i_argc+5],
			new_argv[arglist[j].i_argc+6], new_argv[arglist[j].i_argc+7]);

                // if (functions[arglist[j].fn].type == inv) fprintf(inv_file, "%s", inv_out);
        	if(inv_out[0] != 0)  fprintf(inv_file, "%s", inv_out);
           }
	}

#ifdef CHECK
	if (!decode) {
            if (code_table_6_0(sec) == 0) {                         // has bitmap
                k = GB2_Sec3_npts(sec) -  GB2_Sec5_nval(sec);
                if (k != missing_points(sec[6]+6, GB2_Sec3_npts(sec)))
                    fatal_error_ii("inconsistent number of bitmap points sec3-sec5: %d sec6: %d",
			k, missing_points(sec[6]+6, GB2_Sec3_npts(sec)));
            }
            else if (code_table_6_0(sec) == 255) {                  // no bitmap
                if (GB2_Sec3_npts(sec) != GB2_Sec5_nval(sec))
                    fatal_error_ii("inconsistent number of data points sec3: %d sec5: %d",
                        GB2_Sec3_npts(sec), GB2_Sec5_nval(sec));
            }
	}
#endif


	submsg++;

	if (free_sec4) { free(sec[4]); free_sec4 = 0;}
	if (free_gribfield) { g2_free(grib_data); free_gribfield = 0;}
	

	fprintf(inv_file, "\n");
	if (flush_mode) fflush(inv_file);
	if (dump_msg > 0) break;
    }

    /* finalize all functions, call with mode = -2 */

    for (j = 0; j < narglist; j++) {
        if (functions[arglist[j].fn].type != setup) {
	    n_arg = functions[arglist[j].fn].nargs;
	    inv_out[0] = 0;
	    if (n_arg == 0) 
                functions[arglist[j].fn].fn(-2, NULL, NULL, 0, inv_out, local+j);
	    else if (n_arg == 1)
		functions[arglist[j].fn].fn(-2, NULL, NULL, 0, inv_out, local+j,
			new_argv[arglist[j].i_argc]);
	    else if (n_arg == 2)
		functions[arglist[j].fn].fn(-2, NULL, NULL, 0, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1]);
	    else if (n_arg == 3)
		functions[arglist[j].fn].fn(-2, NULL, NULL, 0, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2]);
	    else if (n_arg == 4)
		functions[arglist[j].fn].fn(-2, NULL, NULL, 0, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3]);
	    else if (n_arg == 5)
		functions[arglist[j].fn].fn(-2, NULL, NULL, 0, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
			new_argv[arglist[j].i_argc+4]);
	    else if (n_arg == 6)
		functions[arglist[j].fn].fn(-2, NULL, NULL, 0, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
			new_argv[arglist[j].i_argc+4], new_argv[arglist[j].i_argc+5]);
	    else if (n_arg == 7)
		functions[arglist[j].fn].fn(-2, NULL, NULL, 0, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
			new_argv[arglist[j].i_argc+4], new_argv[arglist[j].i_argc+5],
			new_argv[arglist[j].i_argc+6]);
	    else if (n_arg == 8)
		functions[arglist[j].fn].fn(-2, NULL, NULL, 0, inv_out, local+j,
			new_argv[arglist[j].i_argc], new_argv[arglist[j].i_argc+1],
			new_argv[arglist[j].i_argc+2], new_argv[arglist[j].i_argc+3],
			new_argv[arglist[j].i_argc+4], new_argv[arglist[j].i_argc+5],
			new_argv[arglist[j].i_argc+6], new_argv[arglist[j].i_argc+7]);
            if (inv_out[0]) fprintf(stderr, "%s\n", inv_out);
        }
    }
    exit(0);
}

/*
 * reads inventory and pulls out the address of the record
 * and submessage number
 */

int rd_inventory(int *rec_num, int *submsg, long int *pos) {

    long int tmp;
    int c, i;

    /* format: [digits].[submessage]:[pos]: or digits]:[pos]: */

    i = 0;

    c=getchar();
    while (c == ' ') c = getchar();
    if (c == EOF) return 1;
    if (!isdigit(c)) {
	fatal_error_i("bad inventory on line: %d",*rec_num);
    }

    /* record number */
    while (isdigit(c) ) {
	i = 10*i + c - '0';
        c=getchar();
    }

    *rec_num = i;

    if (c == '.') {
        i = 0;
	while (isdigit(c = getchar()) ) {
	    i = 10*i + c - '0';
	}
	*submsg = i;
    }
    else {
        *submsg = 1;
    }

    if (c != ':') fatal_error_i("bad inventory on line: %d",*rec_num);

    tmp = 0;
    while (isdigit(c = getchar()) ) {
        tmp = 10*tmp + c - '0';
    }

    /* if (c != ':') fatal_error_i("bad inventory on line: %d",*rec_num); */

    /* read the rest of the line */
    while (c != EOF && c != '\n') {
	c = getchar();
    }
    *pos = tmp;
    return 0;
}

void set_mode(int new_mode) {
	mode = new_mode;
}
