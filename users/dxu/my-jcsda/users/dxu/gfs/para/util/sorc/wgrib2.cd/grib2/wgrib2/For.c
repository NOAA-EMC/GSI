#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * for: allows you set up a for-loop for records to process
 *
 * for example only want to do the first 100 records
 *
 * -for 1:100
 *
 * 10/2008 in public domain Wesley Ebisuzaki
 *
 */

/*
 * HEADER:100:for:setup:1:process record numbers in range,  X=(start:end:step), only one -for allowed
 */

extern int for_mode, for_start, for_end, for_step;

int f_for(ARG1)  {
    int i;    
    if (mode == -1) {
	if (for_mode == 1) fatal_error("for: only one for allow","");
        for_mode = 1;
        i = sscanf(arg1,"%d:%d:%d", &for_start, &for_end, &for_step);
        if (i == 2) {
            for_step = 1;
        }
        else if (i == 1) {
            i = sscanf(arg1,"%d::%d", &for_start, &for_step);
            if (i == 2) {
                for_end = INT_MAX;
            }
            else if (i == 1) {
                for_end = INT_MAX;
                for_step = 1;
            }
	    else fatal_error("for: bad arg %s", arg1);
        }
	else if (i != 3) fatal_error("for: bad arg %s", arg1);

	if (for_step <= 0 || for_end < for_start)
		fatal_error("for: bad arguments %s", arg1);
    }
    return 0;
}
