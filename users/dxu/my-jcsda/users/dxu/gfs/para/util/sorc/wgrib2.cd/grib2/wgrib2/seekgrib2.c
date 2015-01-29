/*
 *
 * file = what do you think?
 * pos = initial position to start looking at  ( = 0 for 1st call)
 *       returns with position of next grib header (units=bytes)
 * len_grib = length of the grib record (bytes)
 * buffer[buf_len] = buffer for reading/writing
 *
 * returns (char *) to start of GRIB header+PDS
 *         NULL if not found
 *
 * adapted from SKGB (Mark Iredell) and later seekgrib.c (Wesley Ebisuzaki)
 *
 * v1.0 modifications for grib2
 * v1.1 1/2007 cleanup by M Schwarb, fread == 0 test
 *
 */
#include <stdio.h>
#include "grb2.h"
#include "wgrib2.h"

#define G	71
#define R	82
#define I	73
#define B	66

#define NTRY 100


unsigned char *seek_grib2(FILE *file, long int *pos, unsigned long int *len_grib, 
        unsigned char *buffer, unsigned int buf_len, long int *n_bytes) {

    long int i, len, current_pos;
    int j;

    for (j = 0; j < NTRY; j++) {

        if (fseek(file, *pos, SEEK_SET) == -1) break;
        if ((*n_bytes = i = fread(buffer, sizeof(unsigned char), buf_len, file)) == 0) break;

        len = i - GB2_Sec0_size;
     
        for (i = 0; i < len; i++) {
            if (buffer[i] == G && buffer[i+1] == R && buffer[i+2] == I
                && buffer[i+3] == B) {
		if (buffer[i+7] == 2) {
                    *len_grib = uint8(buffer+i+8);
                    *pos += i;
                    return (buffer+i);
		}
		if (buffer[i+7] == 1) {
		    fprintf(stderr,"grib1 message ignored (use wgrib)\n");
		}
            }
        }
	*pos = *pos + (buf_len - GB2_Sec0_size);
    }

    *len_grib = 0;
    return (unsigned char *) NULL;
}
