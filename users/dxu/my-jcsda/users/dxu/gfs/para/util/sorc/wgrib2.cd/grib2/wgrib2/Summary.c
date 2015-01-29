#include <stdio.h>
#include <stdlib.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Summary.c
 *
 * options that produce output at the end of the job (mode == 2)
 * they can provide a summary of the operations
 *
 * 10/2008 Public Domain Wesley Ebisuzaki
 *
 * count: writes to stdout the number of records processed
 * grid_changes: checks to see that only 1 grid type was processed
 */



extern enum input_type input;
extern int header, dump_rec, dump_submsg;

extern int file_append;
extern char *item_deliminator;
extern FILE *inv_file;
extern char *nl;

extern int new_GDS;

/*
 * HEADER:100:count:misc:0:prints number of fields 
 */
int f_count(ARG0) {
    struct local_struct {
        int count;
    };
    struct local_struct *save;

    if (mode == -1) {
        *local = save = (struct local_struct *)malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("f_count memory allocation ","");
	save->count = 0;
    }
    else if (mode >= 0) {
        save = *local;
	save->count += 1;
    }
    else if (mode == -2) {
        save = *local;
	sprintf(inv_out,"number of records: %d", save->count);
	free(save);
    }
    return 0;
}

/*
 * HEADER:100:grid_changes:misc:0:prints number of grid changes
 */
int f_grid_changes(ARG0) {
    struct local_struct {
        int count;
    };
    struct local_struct *save;

    if (mode == -1) {
        *local = save = (struct local_struct *)malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("f_count memory allocation ","");
	save->count = 0;
    }
    else if (mode >= 0) {
	if (new_GDS) {
            save = *local;
	    save->count += 1;
	}
    }
    else if (mode == -2) {
        save = *local;
	switch(save->count) {
	case 0: sprintf(inv_out,"Warning: no grib2 records");
		break;
	case 1: sprintf(inv_out,"Good: only one grid");
		break;
	default: sprintf(inv_out,"Warning: multiple grids");
		break;
	}
	free(save);
    }
    return 0;
}
