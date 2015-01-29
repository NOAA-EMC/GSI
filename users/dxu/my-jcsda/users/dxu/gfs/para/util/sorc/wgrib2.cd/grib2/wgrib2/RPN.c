#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * RPN reversed polish notation
 *
 * 4/2009 Public Domain by Wesley Ebisuzaki
 *
 * operations:
 *
 *     + - * /
 *    == != < <= > >=
 *
 *    sqrt, sq, abs, 1/x, floor, ceil, pow (x^y), exp, ln
 *    min, max, merge, mask
 *    sin, cos, tan, asin, acos, atan, atan2
 *
 *    pi
 *
 *    registers: sto_N, rcl_N, clr_N
 *               rcl_lat, rcl_lon
 *               rcl (data)
 *    stack: exc, pop, dup, clr
 *
 *    print_(X):  max, min, rms, corr
 *
 * at the end of rpn, the top of the stack is saved to data unless clr done first
 */
#define N_REGS 10
#define STACK_SIZE 10

extern int decode, latlon;
extern double *lat, *lon;
extern int match_flag;
extern char *item_deliminator;
extern int use_scale;

unsigned int rpn_n[N_REGS];
float *rpn_data[N_REGS];
static float *stack[STACK_SIZE];

#define SCALAR 0
#define VECTOR 1
#define DBL_VEC 2

#ifndef M_PI
#define M_PI           3.14159265358979323846  /* pi */
#endif

int push(int top, unsigned int ndata, int type, float f, float *ff, double *d);


/*
 * HEADER:100:rpn:misc:1:reverse polish notation calculator (beta)
 */

int f_rpn(ARG1) {
    char string[100], *p;
    int j, k, n;
    unsigned int i;
    float f;
    float tmp;
    int top, flag;
    double cos_lat, last_lat;
    double sum1, sum2, wt, sq1, sq2, sq12;
    int nx, ny, res, scan;
    unsigned int npnts;
    float *p1, *p2;

    static int state=0;

    if (mode == -1) {
	decode = latlon = 1;
	if (state == 0) {
            for (i = 0; i < N_REGS; i++) {
	        rpn_n[i] = 0;
	        rpn_data[i] = NULL;
	    }
	    state = 1;
	}
	return 0;
    }
    if (mode == -2) {
	if (state == 1) {
            for (i = 0; i < N_REGS; i++) {
	        if (rpn_data[i]) {
		    free (rpn_data[i]);
		    rpn_data[i] = NULL;
	            rpn_n[i] = 0;
	        }
	    }
	}
	state = 0;
	return 0;
    }

    // initialize stack

    if (data == NULL) fatal_error("-rpn: decode failed","");

    for (i = 0; i < STACK_SIZE; i++) stack[i] = NULL;
    top = push(-1, ndata, VECTOR, 0.0, data, NULL);
    use_scale = 0;

    if (mode == 98) fprintf(stderr,"RPN: arg=%s\n",arg1);

    // scan parameters

    p = arg1;
    while (sscanf(p,"%[^:]%n", string, &n) == 1) {
	if (mode == 98) { 
	    fprintf(stderr, "RPN: top=%d (%s)", top, string);
	}
	p = p + n;
	if (*p == ':') p++;

	// binary operators + - * /

	if (strcmp(string,"+") == 0) {
	    if (mode == 98) fprintf(stderr," plus");
	    if (top <= 0) fatal_error("-rpn: bad + expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) { stack[j][i] = stack[j][i] + stack[top][i];
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}
	else if (strcmp(string,"-") == 0) {
	    if (mode == 98) fprintf(stderr," minus");
	    if (top <= 0) fatal_error("-rpn: bad - expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    stack[j][i] = stack[j][i] - stack[top][i];
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}
	else if (strcmp(string,"*") == 0) {
	    if (mode == 98) fprintf(stderr," times");
	    if (top <= 0) fatal_error("-rpn: bad * expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    stack[j][i] = stack[j][i] * stack[top][i];
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}
	else if (strcmp(string,"/") == 0) {
	    if (mode == 98) fprintf(stderr," div");
	    if (top <= 0) fatal_error("-rpn: bad / expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i]) && (stack[top][i] != 0.0)) {
		    stack[j][i] = stack[j][i] / stack[top][i];
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}

	// merge:  stack(top-1) = stack(top) (if defined) ; top--;

	else if (strcmp(string,"merge") == 0) {
	    if (mode == 98) fprintf(stderr," merge");
	    if (top <= 0) fatal_error("-rpn: bad merge expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    stack[j][i] = stack[top][i];
		}
	    }
	    top--;
	}


	// exc (swap top and top-1 stack entries)

	else if (strcmp(string,"exc") == 0) {
	    if (mode == 98) fprintf(stderr," exchange");
	    if (top <= 0) fatal_error("-rpn: bad exc expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		f = stack[j][i];
		stack[j][i] = stack[top][i];
		stack[top][i] = j;
	    }
	    top--;
	}

	// pop:  top--;

	else if (strcmp(string,"pop") == 0) {
	    if (mode == 98) fprintf(stderr," pop");
	    if (top < 0) fatal_error("-rpn: bad pop","");
	    top--;
	}

	// dup: top++; stack(top) = stack(top-1)

	else if (strcmp(string,"dup") == 0) {
	    if (mode == 98) fprintf(stderr," dup");
	    top = push(top,ndata,VECTOR,0.0,stack[top],NULL);
	}

	//  sqrt: stack(top) = sqrt(stack(top))

	else if (strcmp(string,"sqrt") == 0) {
	    if (mode == 98) fprintf(stderr," sqrt");
	    if (top < 0) fatal_error("-rpn: bad sqrt expression","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && stack[top][i] >= 0.0) {
		    stack[top][i] = sqrtf(stack[top][i]);
		}
		else stack[top][i] = UNDEFINED;
	    }
	}
	// sq: x*x
	else if (strcmp(string,"sq") == 0) {
	    if (mode == 98) fprintf(stderr," sq");
	    if (top < 0) fatal_error("-rpn: bad sq expression","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    stack[top][i] *= stack[top][i];
		}
	    }
	}
	// pow: x^y
	else if (strcmp(string,"pow") == 0) {
	    if (top <= 0) fatal_error("-rpn: bad pow expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    stack[j][i] = powf(stack[j][i], stack[top][i]);
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}
        // ln - natural log
        else if (strcmp(string,"ln") == 0) {
            if (top < 0) fatal_error("-rpn: bad log expression","");
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i]) && stack[top][i] > 0.0) {
                    stack[top][i] = logf(stack[top][i]);
                }
                else stack[top][i] = UNDEFINED;
            }
            top--;
        }

	// exp
	else if (strcmp(string,"exp") == 0) {
	    if (top < 0) fatal_error("-rpn: bad exp expression","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    stack[top][i] = expf(stack[top][i]);
		}
	    }
	}

	// abs
	else if (strcmp(string,"abs") == 0) {
	    if (top < 0) fatal_error("-rpn: bad abs expression","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    if (stack[top][i] < 0.0) stack[top][i] = -stack[top][i];
		}
	    }
	}

	// 1/x

	else if (strcmp(string,"1/x") == 0) {
	    if (mode == 98) fprintf(stderr," 1/x");
	    if (top < 0) fatal_error("-rpn: bad 1/x","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && stack[top][i] != 0.0) {
		    stack[top][i] = 1.0 / stack[top][i];
		}
		else stack[top][i] = UNDEFINED;
	    }
	}

	// floor

	else if (strcmp(string,"floor") == 0) {
	    if (top < 0) fatal_error("-rpn: bad floor","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    stack[top][i] = floorf(stack[top][i]);
		}
	    }
	}

	// ceil

	else if (strcmp(string,"ceil") == 0) {
	    if (top < 0) fatal_error("-rpn: bad ceil","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    stack[top][i] = ceilf(stack[top][i]);
		}
	    }
	}

	// sin cos tan asin acos atan
	else if (strcmp(string,"sin") == 0) {
	    if (top < 0) fatal_error("-rpn: bad sin","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    stack[top][i] = sinf(stack[top][i]);
		}
	    }
	}
	else if (strcmp(string,"cos") == 0) {
	    if (top < 0) fatal_error("-rpn: bad cos","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    stack[top][i] = cosf(stack[top][i]);
		}
	    }
	}
	else if (strcmp(string,"tan") == 0) {
	    if (top < 0) fatal_error("-rpn: bad tan","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    stack[top][i] = tanf(stack[top][i]);
		}
	    }
	}
	else if (strcmp(string,"asin") == 0) {
	    if (top < 0) fatal_error("-rpn: bad asin","");
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    if (fabsf(stack[top][i]) > 1.0) stack[top][i] = UNDEFINED;
		    else stack[top][i] = asinf(stack[top][i]);
		}
	    }
	}
        else if (strcmp(string,"acos") == 0) {
            if (top < 0) fatal_error("-rpn: bad acos","");
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i])) {
                    if (fabsf(stack[top][i]) > 1.0) stack[top][i] = UNDEFINED;
                    else stack[top][i] = acosf(stack[top][i]);
                }
            }
        }
	else if (strcmp(string,"atan") == 0) {
            if (top < 0) fatal_error("-rpn: bad atan","");
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i])) {
                    stack[top][i] = atanf(stack[top][i]);
                }
            }
        }
        else if (strcmp(string,"atan2") == 0) {
            if (top <= 0) fatal_error("-rpn: bad atan2 expression","");
            j = top-1;
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
                    stack[j][i] = atan2f(stack[j][i], stack[top][i]);
                }
                else stack[j][i] = UNDEFINED;
            }
            top--;
        }

	// sto_e
	else if (string[0] == 's' && string[1] == 't' && string[2] == 'o' && string[3] == '_' 
		&& isdigit(string[4]) && string[5] == 0) {
	    if (top < 0) fatal_error("-rpn: sto","");
	    j = atoi(string+4);
	    if (j >= N_REGS || j < 0) fatal_error("-rpn: bad register number in %s", string);
	    if (ndata != rpn_n[j]) {
		if (rpn_data[j]) free(rpn_data[j]);
		rpn_n[j] = ndata;
		rpn_data[j] = (float *) malloc(ndata* sizeof(float));
		if (rpn_data[j] == NULL) fatal_error("-rpn: memory allocation failed in %s",string);
	    }
	    for (i=0; i < ndata; i++) {
		rpn_data[j][i] = stack[top][i];
	    }
        }

	// rcl_N
	else if (string[0] == 'r' && string[1] == 'c' && string[2] == 'l' && string[3] == '_'
		&& isdigit(string[4]) && string[5] == 0) {
	    j = atoi(string+4);
	    if (j >= N_REGS || j < 0) fatal_error("-rpn: bad register number in %s", string);

	    if (rpn_n[j] != 0 && rpn_n[j] != ndata) fatal_error("-rpn: rcl size mismatch","");

	    if (rpn_n[j] == 0) {	// unused register are zero
	        top = push(top,ndata,SCALAR,0.0,rpn_data[j],NULL);
	    }
	    else {
	        top = push(top,ndata,VECTOR,0.0,rpn_data[j],NULL);
	    }
	}

	// clr_N
	else if (string[0] == 'c' && string[1] == 'l' && string[2] == 'r' && string[3] == '_'
		&& isdigit(string[4]) && string[5] == 0) {
	    j = atoi(string+4);
	    if (j >= N_REGS || j < 0) fatal_error("-rpn: bad register number in %s", string);
	    if (rpn_data[j]) {
		free(rpn_data[j]);
		rpn_data[j] = NULL;
	    }
	    rpn_n[j] = 0;
	}

	// rcl_lat
	else if (strcmp(string,"rcl_lat") == 0) {
	    if (lat == NULL) fatal_error("-rpn: rcl_lat: lat not defined","");
	    top = push(top,ndata,DBL_VEC,0.0,NULL,lat);
	}
	// rcl_lon
	else if (strcmp(string,"rcl_lon") == 0) {
	    if (lon == NULL) fatal_error("-rpn: rcl_lon: lon not defined","");
	    top = push(top,ndata,DBL_VEC,0.0,NULL,lon);
	}

	// max and min

	else if (strcmp(string,"max") == 0) {
	    if (top <= 0) fatal_error("-rpn: bad max expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    if (stack[j][i] < stack[top][i]) stack[j][i] = stack[top][i];
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}
	else if (strcmp(string,"min") == 0) {
	    if (top <= 0) fatal_error("-rpn: bad min expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    if (stack[j][i] > stack[top][i]) stack[j][i] = stack[top][i];
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}

	else if (strcmp(string,">") == 0) {
	    if (top <= 0) fatal_error("-rpn: bad > expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    stack[j][i] =  (stack[j][i] > stack[top][i]);
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}
        else if (strcmp(string,">=") == 0) {
            if (top <= 0) fatal_error("-rpn: bad >= expression","");
            j = top-1;
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
                    stack[j][i] =  (stack[j][i] >= stack[top][i]);
                }
                else stack[j][i] = UNDEFINED;
            }
            top--;
        }
        else if (strcmp(string,"!=") == 0) {
            if (top <= 0) fatal_error("-rpn: bad != expression","");
            j = top-1;
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
                    stack[j][i] =  (stack[j][i] != stack[top][i]);
                }
                else stack[j][i] = UNDEFINED;
            }
            top--;
        }
	else if (strcmp(string,"==") == 0) {
	    if (top <= 0) fatal_error("-rpn: bad == expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    stack[j][i] =  (stack[j][i] == stack[top][i]);
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}
        else if (strcmp(string,"<") == 0) {
            if (top <= 0) fatal_error("-rpn: bad < expression","");
            j = top-1;
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
                    stack[j][i] =  (stack[j][i] < stack[top][i]);
                }
                else stack[j][i] = UNDEFINED;
            }
            top--;
        }
        else if (strcmp(string,"<=") == 0) {
            if (top <= 0) fatal_error("-rpn: bad <= expression","");
            j = top-1;
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
                    stack[j][i] =  (stack[j][i] <= stack[top][i]);
                }
                else stack[j][i] = UNDEFINED;
            }
            top--;
        }

	else if (strcmp(string,"mask") == 0) {
	    if (top <= 0) fatal_error("-rpn: bad mask expression","");
	    j = top-1;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    if (stack[top][i] == 0.0) stack[j][i] = UNDEFINED;
	        }
		else stack[j][i] = UNDEFINED;
	    }
	    top--;
	}
	else if (strcmp(string,"yrev") == 0) {
	    if (top < 0) fatal_error("-rpn: yrev needs field","");
            get_nxny(sec, &nx, &ny, &npnts, &res, &scan);
	    if (nx <= 0 || ny <= 0) fatal_error("-rpn: yrev only on nx x ny grids","");
 	    if ((scan >> 4) != 0 && (scan >> 4) != 4) 
		fatal_error("-rpn: yrev only appropriate for we:ns and we:sn grids","");
	    for (k = 0; k < ny/2; k++) {
		p1 = stack[top] + nx*k;
		p2 = stack[top] + nx*(ny-k-1);
		for (j = 0; j < nx; j++) {
		    tmp = p1[j];
		    p1[j] = p2[j];
		    p2[j] = tmp;
		}
	    }
	}

	// change to rcl-data, rcl-lat, rcl-lon

	// rcl:  stack(++top) = data
	else if (strcmp(string,"rcl") == 0) {
	    top = push(top,ndata,VECTOR,0.0,data,NULL);
	}

	// sto:  data = stack(top)
	else if (strcmp(string,"sto") == 0) {
	    if (top < 0) fatal_error("-rpn: bad sto","");
	    for (i=0; i < ndata; i++) {
		data[i] = stack[top][i];
	    }
        }

	// clr: emtpy stack
	else if (strcmp(string,"clr") == 0) {
	    top = -1;
	}

	// pi: stack(++top) = pi
	else if (strcmp(string,"pi") == 0) {
	    top = push(top,ndata,SCALAR,(float) M_PI,NULL,NULL);
	}

	// print operations .. doesnt affect the stack

	else if (strcmp(string,"print_max") == 0) {
	    if (top < 0) fatal_error("-rpn: bad print_max expression","");
            flag = 0;
	    tmp = 0.0;
	    for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i])) {
		    if (flag) tmp = (tmp < stack[top][i]) ? stack[top][i] : tmp;
		    else {
		        flag = 1;
		        tmp = stack[top][i];
		    }
		}
	    }
	    sprintf(inv_out,"%srpn_max=%g",item_deliminator,tmp);
	    inv_out += strlen(inv_out);
	}
        else if (strcmp(string,"print_min") == 0) {
            if (top < 0) fatal_error("-rpn: bad print_min expression","");
            flag = 0;
            tmp = 0.0;
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i])) {
                    if (flag) tmp = (tmp > stack[top][i]) ? stack[top][i] : tmp;
                    else {
                        flag = 1;
                        tmp = stack[top][i];
                    }
                }
            }
	    sprintf(inv_out,"%srpn_min=%g",item_deliminator,tmp);
            inv_out += strlen(inv_out);
        }

	// print_rms: prints out cosine weighted RMS

        else if (strcmp(string,"print_rms") == 0) {
            if (top <= 0) fatal_error("-rpn: print_rms needs two fields","");
	    if (lat == NULL) fatal_error("-rpn: print_rms .. no latitudes defined","");
            j = top - 1;
	    last_lat = 0;
	    cos_lat = 1.0;
            sum1 = wt = 0.0;
            for (i = 0; i < ndata; i++) {
		if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    if (lat && last_lat != lat[i]) cos_lat = cos(lat[i]*M_PI/180.0);
		    sum1 +=  (stack[top][i] - stack[j][i]) * (stack[top][i] - stack[j][i]) *cos_lat;
		    wt += cos_lat;
		}
            }
	    if (wt != 0.0)  sprintf(inv_out,"%srpn_rms=%g",item_deliminator,sqrt(sum1/wt));
	    else sprintf(inv_out,"%srpn_rms=undefined",item_deliminator);
            inv_out += strlen(inv_out);
	}

	// print_corr: prints cosine(lat) weighted spatial correlation

        else if (strcmp(string,"print_corr") == 0) {
            if (top <= 0) fatal_error("-rpn: print_corr needs two fields","");
	    if (lat == NULL) fatal_error("-rpn: print_corr .. no latitudes defined","");
            j = top - 1;
	    sum1 = sum2 = wt = 0.0;
	    last_lat = 0;
	    cos_lat = 1.0;
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    if (lat && last_lat != lat[i]) cos_lat = cos(lat[i]*M_PI/180.0);
		    sum1 += stack[top][i] * cos_lat;
		    sum2 += stack[j][i] * cos_lat;
		    wt += cos_lat;
                }
	    }
	    sum1 = sum1 / wt;
	    sum2 = sum2 / wt;
	    sq1 = sq2 = sq12 = 0.0;
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(stack[top][i]) && DEFINED_VAL(stack[j][i])) {
		    if (last_lat != lat[i]) cos_lat = cos(lat[i]*M_PI/180.0);
		    sq1 += (stack[top][i]-sum1)*(stack[top][i]-sum1)*cos_lat;
		    sq2 += (stack[j][i]-sum2)*(stack[j][i]-sum2)*cos_lat;
		    sq12 += (stack[top][i]-sum1)*(stack[j][i]-sum2)*cos_lat;
		}
	    }
	    sq1 = sq1 / wt;
	    sq2 = sq2 / wt;
	    sq12 = sq12 / wt;

	    if (sq1*sq2 == 0.0) sprintf(inv_out,"%srpn_corr=%g",item_deliminator,1.0);
	    else sprintf(inv_out,"%srpn_corr=%g",item_deliminator, sq12/sqrt(sq1*sq2));
            inv_out += strlen(inv_out);
        }

	// number:  stack(++top) = number

	else if (string[0] == '+' || string[0] == '-' || isdigit(string[0])) {
	    f = atof(string);
	    top = push(top,ndata,SCALAR,f,NULL,NULL);
	    if (mode == 98) fprintf(stderr," constant=%f", f);
	}
	else fatal_error("-rpn: unidentified symbol %s", string);
	if (mode == 98) fprintf(stderr," top=%d\n", top);
    }	

    if (top >= 0) {
	for (i = 0; i < ndata; i++) {
	    data[i] = stack[top][i];
	}
    }
    else fatal_error("-rpn: stack empty","");

    // free stack
    for (i = 0; i < STACK_SIZE; i++) free(stack[i]);

    return 0;
}

int push(int top, unsigned int ndata, int type, float f, float *ff, double *d) {

    unsigned int i;

    if (++top == STACK_SIZE) fatal_error_i("-rpn: push: stack overflow %d",top);
    if (stack[top] == NULL) {
	 stack[top] = (float *) malloc(ndata * sizeof(float));
	if (stack[top] == NULL) fatal_error("-rpn: push: memory allocation","");
    }
    if (type == SCALAR) {
	for (i = 0; i < ndata; i++) stack[top][i] = f;
    }
    else if (type == VECTOR) {
	for (i = 0; i < ndata; i++) stack[top][i] = ff[i];
    }
    else if (type == DBL_VEC) {
	for (i = 0; i < ndata; i++) stack[top][i] = (float) d[i];
    }

    return top;
}

/*
 * HEADER:100:if_reg:misc:1:if rpn registers defined, X = A, A:B, A:B:C, etc A = register number
 */
int f_if_reg(ARG1) {
    int i, j, *list;
    char *p;

    if (mode == -1) {
	// figure out the number of arguments
	i = 1;
	p = arg1;
	while (*p) {
	   if (*p++ == ':') i++;
	}
	*local = list = (int *) calloc(i+1, sizeof (int));
	if (list == NULL) fatal_error("if_reg: memory allocation failed","");
	list[0] = i;
	p = arg1;
	for (j = 1; j <= i; j++) {
	    list[j] = atoi(p);
	    if (list[j] >= N_REGS || list[j] < 0) fatal_error_i("if_reg: bad register %d", list[j]);
	    while (isdigit(*p)) p++;
	    if (*p == ':') p++;
	}
    }
    else if (mode == -2) {
	list = (int *) *local;
	free(list);
    }
    else if (mode >= 0) {
	list = (int *) *local;
	i = list[0];
	match_flag = 0;
	for (j=1; j <= i; j++) {
	    if (rpn_n[list[j]] == 0) match_flag = 1;
	}
    }
    return 0;
}
