#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Inv_number.c inventory_number routines
 *
 * inv_number is the line number of the inventory
 *
 * to multitask, you can split up the work by the inv number
 *  note: inv number is not the message number because
 *    1) a grib message can have multiple submessages
 *    2) a match command can select out records
 *       for example, -match :HGT: will only give you the heght fields
 *         and you will want to split the processing over the various height fields
 *    3) the -i command will read an inventory
 *
 * the -n and -for_n are preliminary
 *
 * 5/2009 in public domain Wesley Ebisuzaki
 *
 */

/*
 * HEADER:100:n:inv:0:prints out inventory number
 */

extern int inv_no;
extern int match_flag;

int f_n(ARG0)  {
    if (mode >= 0) {
	sprintf(inv_out,"n=%d",inv_no);
    }
    return 0;
}

/*
 * HEADER:100:for_n:setup:1:process inv numbers in range,  X=(start:end:step), only one -for allowed
 */

extern int for_n_mode, for_n_start, for_n_end, for_n_step;

int f_for_n(ARG1)  {
    int i;    
    if (mode == -1) {
        if (for_n_mode == 1) fatal_error("for_n: only one for_n allowed","");
        for_n_mode = 1;
        i = sscanf(arg1,"%d:%d:%d", &for_n_start, &for_n_end, &for_n_step);
	if (i == 2) {
	    for_n_step = 1;
	}
	else if (i == 1) {
            i = sscanf(arg1,"%d::%d", &for_n_start, &for_n_step);
	    if (i == 2) {
		for_n_end = INT_MAX;
	    }
	    else if (i == 1) {
		for_n_end = INT_MAX;
	        for_n_step = 1;
	    }
	}
	else if (i != 3) fatal_error("for_n: bad arg %s", arg1);

	if (for_n_step <= 0 || for_n_end < for_n_start)
		fatal_error("for_n: bad arguments %s", arg1);
    }
    return 0;
}

/*
 * HEADER:100:if_n:misc:1:if (inv numbers in range),  X=(start:end:step)
 */


int f_if_n(ARG1)  {
    int i;

    struct local_struct {
        int start, end, step;
    };
    struct local_struct *save;


    if (mode == -1) {

        save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation f_if_n","");

        i = sscanf(arg1,"%d:%d:%d", &(save->start), &(save->end), &(save->step));
        if (i == 2) {
            save->step = 1;
        }
        else if (i == 1) {
            i = sscanf(arg1,"%d::%d", &(save->start), &(save->step));
            if (i == 2) {
                save->end = INT_MAX;
            }
            else if (i == 1) {
                save->end = INT_MAX;
                save->step = 1;
            }
        }
        else if (i != 3) fatal_error("if_n: bad arg %s", arg1);

        if (save->step <= 0 || save->end < save->start)
                fatal_error("if_n: bad arguments %s", arg1);
	*local = save;
	return 0;
    }
    if (mode == -2) {
	free(*local);
	return 0;
    }
    if (mode >= 0) {
	save = *local;
	if (inv_no >= save->start && inv_no <= save->end && ((inv_no - save->start) % save->step) 
		== 0) match_flag=0;
	else match_flag = 1;
        return 0;
    }
    return 0;
}

