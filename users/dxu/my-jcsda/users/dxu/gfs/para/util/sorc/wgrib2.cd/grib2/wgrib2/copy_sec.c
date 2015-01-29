/* simple utility routine to copy sec[], and free it later
 *
 * 8/2008 public domain Wesley Ebisuzaki
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#include "grb2.h"
#include "wgrib2.h"



int copy_sec(unsigned char **sec, unsigned char **clone_sec) {

    int size[9], i;

//  get size of each secion

    size[0] = GB2_Sec0_size;
    size[1] = GB2_Sec1_size(sec);
    size[2] = 0;
    if (sec[2]) size[2] = GB2_Sec2_size(sec);
    size[3] = GB2_Sec3_size(sec);
    size[4] = GB2_Sec4_size(sec);
    size[5] = GB2_Sec5_size(sec);
    size[6] = GB2_Sec6_size(sec);
    size[7] = GB2_Sec7_size(sec);
    size[8] = GB2_Sec8_size;

//  allocate memory

    for (i = 0; i < 9; i++) {
	if (size[i] > 0) {
	    if ((clone_sec[i] = (unsigned char *) malloc(size[i])) == NULL) 
	        fatal_error_i("memory allocation failed copy_sec %d",i);
	}
	else {
	    clone_sec[i] = NULL;
	}
    }	

//  copy memory
    for (i = 0; i < 9; i++) {
	if (size[i] > 0) {
	    memcpy(clone_sec[i], sec[i], size[i]);
	}
    }
    return 0;
}

int free_sec(unsigned char **clone_sec) {
    int i;
    for (i = 0; i < 9; i++) {
	if (clone_sec[i]) free(clone_sec[i]);
    }
    return 0;
}
