#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Set_ijval
 *
 *  Routine to set a grid point value
 *
 * 2/2009: Public Domain: Wesley Ebisuzaki
 *
 */


extern int decode, latlon;
extern double *lat, *lon;
extern int nx, ny;

/*
 * HEADER:100:set_ijval:misc:3:sets grid point value X=ix Y=iy Z=val
 */

int f_set_ijval(ARG3) {

    struct local_struct {
        int ix, iy;
	float val;
    };
    struct local_struct *save;

    int x,y;

    if (mode == -1) {
        decode = 1;
        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("memory allocation f_ijval","");

        save->ix = atoi(arg1) - 1;
        save->iy = atoi(arg2) - 1;
        save->val = atof(arg3);

	if (save->ix < 0) fatal_error_i("ijval: ix value (%d) should be >= 1", save->ix + 1);
	if (save->iy < 0) fatal_error_i("ijval: iy value (%d) should be >= 1", save->iy + 1);
    }
    if (mode < 0) return 0;
    save = *local;

    x = save->ix;
    y = save->iy;

    if (x < nx && y < ny) data[x + y*nx] = save->val;
    else fatal_error_ii("ijval: failed with problem with grid dimensions (%dx%d)",nx,ny);
    return 0;
}
