#include <stdio.h>

#include "grb2.h"
#include "wgrib2.h"


/*
 * returns the size of the product definition template
 *   i.e. sizeof (sec4) - size of vertical coordinates
 *
 *
 * 3/2008 public domain Wesley Ebisuzaki
 */

int prod_def_temp_size(unsigned char **sec) {
   int pdt, n, nc;

   pdt = GB2_ProdDefTemplateNo(sec);

   switch(pdt) {
	case 0: return 34;
	case 1: return 37;
	case 2: return 36;
	case 3: nc = sec[4][57];
		return (68+nc);
	case 4: nc = sec[4][53];
		return 64+nc;
	case 5: return 47;
	case 6: return 35;
	case 7: return 34;
	case 8: n = sec[4][41];
		if (n <= 1) return 58;
		return 46+12*n;
	case 9: n = sec[4][54];
		if (n <= 1) return 71;
		return 59+12*n;
	case 10: n = sec[4][42];
		if (n <= 1) return 59;
		return 47+12*n;
	case 11: n = sec[4][44];
		if (n <= 1) return 61;
		return 49+12*n;
	case 12: n = sec[4][43];
		if (n <= 1) return 60;
		return 48+12*n;
	case 13: n = sec[4][75];
		if (n <= 1) return 92;
		return 80+12*n;
	case 14: n = sec[4][71];
		if (n <= 1) return 88;
		return 76+12*n;
	case 20: return 43;
	case 30: n = sec[4][13];
		return 14+n*10;
	case 31: n = sec[4][13];
		return 14+n*11;
	case 254: return 15;
	case 1000: return 22;
	case 1001: return 38;
	case 1002: return 35;
	case 1100: return 34;
	case 1101: return 46;
	default: return -1;
    }
    return -1;
}

