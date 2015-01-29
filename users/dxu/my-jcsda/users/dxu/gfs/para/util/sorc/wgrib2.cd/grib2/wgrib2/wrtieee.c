#include <stdio.h>
#include <stddef.h>
#include "wgrib2.h"

/* wesley ebisuzaki v1.3
 *
 * write ieee file -- big endian format
 *
 * input float *array		data to be written
 *	 int n			size of array
 *	 int header		1 for f77 style header 0 for none
 *				(header is 4 byte header
 *	 FILE *output		output file
 *
 * v1.2 7/97 buffered, faster
 * v1.3 2/99 fixed (typo) error in wrtieee_header found by
 *     Bob Farquhar
 * v1.4 3/2008 w. ebisuzaki added little-endian output
 */

/* BSIZ MUST be a multiple of 4 */

#define BSIZ (1024*4)

extern int ieee_little_endian;

int wrtieee(float *array, unsigned int n, int header, FILE *output) {

	unsigned int l;
	unsigned int i, nbuf;
	unsigned char buff[BSIZ];
	unsigned char h4[4];

	nbuf = 0;
	if (header) {
		l = n * 4;
		for (i = 0; i < 4; i++) {
			h4[i] = l & 255;
			l >>= 8;
		}
		buff[nbuf++] = h4[3];
		buff[nbuf++] = h4[2];
		buff[nbuf++] = h4[1];
		buff[nbuf++] = h4[0];
	}
	for (i = 0; i < n; i++) {
		if (nbuf >= BSIZ) {
		    if (ieee_little_endian) swap_buffer(buff, BSIZ);
		    fwrite(buff, 1, BSIZ, output);
		    nbuf = 0;
		}
		flt2ieee(array[i], buff + nbuf);
		nbuf += 4;
	}
	if (header) {
		if (nbuf == BSIZ) {
		    if (ieee_little_endian) swap_buffer(buff, BSIZ);
		    fwrite(buff, 1, BSIZ, output);
		    nbuf = 0;
		}
		buff[nbuf++] = h4[3];
		buff[nbuf++] = h4[2];
		buff[nbuf++] = h4[1];
		buff[nbuf++] = h4[0];
	}
	if (nbuf) {
	    if (ieee_little_endian) swap_buffer(buff, nbuf);
	    fwrite(buff, 1, nbuf, output);
	}
	return 0;
}
