#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"


/* some routines to check whether two fields are the same */


int same_sec0(unsigned char **sec_a, unsigned char **sec_b) {

    unsigned char *a, *b;
    int i;
    a = sec_a[0];
    b = sec_b[0];
    for (i = 0; i < 8; i++) {
	if (*a++ != *b++) return 0;
    }
    return 1;
}

int same_sec1(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i;
    i = GB2_Sec1_size(sec_a);
    if (GB2_Sec1_size(sec_b) != i) return 0;
    a = sec_a[1];
    b = sec_b[1];
    while (i--) {
	if (*a++ != *b++) return 0;
    }
    return 1;
}

// test to see if same sec1 but don't do time stamp

int same_sec1_not_time(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i, j;
    i = GB2_Sec1_size(sec_a);
    if (GB2_Sec1_size(sec_b) != i) return 0;
    a = sec_a[1];
    b = sec_b[1];
    for (j = 0; j < 12; j++) {
	if (a[j] != b[j]) return 0;
    }
    for (j = 19; j < i; j++) {
	if (a[j] != b[j]) return 0;
    }
    return 1;
}


int same_sec3(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i;
    i = GB2_Sec3_size(sec_a);
    if (GB2_Sec3_size(sec_b) != i) return 0;
    a = sec_a[3];
    b = sec_b[3];
    while (i--) {
        if (*a++ != *b++) return 0;
    }
    return 1;
}

int same_sec4(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i;

    i = GB2_Sec4_size(sec_a);
    if (GB2_Sec4_size(sec_b) != i) return 0;
    a = sec_a[4];
    b = sec_b[4];
    while (i--) {
        if (*a++ != *b++) return 0;
    }
    return 1;
}


int same_sec4_not_time(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i, j;
    int pdt;
    static int warning = 0; 

    i = GB2_Sec4_size(sec_a);
    if (GB2_Sec4_size(sec_b) != i) return 0;
    pdt = GB2_ProdDefTemplateNo(sec_a);
    if (GB2_ProdDefTemplateNo(sec_b) != pdt) return 0;

    a = sec_a[4];
    b = sec_b[4];

    if (pdt == 0) {
	for (j = 0; j < 16; j++) {
	    if (a[j] != b[j]) return 0;
	}
	for (j = 22; j < i; j++) {
	    if (a[j] != b[j]) return 0;
	}
	return 1;
    }
    if (pdt == 8) {
	for (j = 0; j < 16; j++) {
	    if (a[j] != b[j]) return 0;
	}
	for (j = 22; j < 34; j++) {
	    if (a[j] != b[j]) return 0;
	}
	for (j = 41; j < i; j++) {
	    if (a[j] != b[j]) return 0;
	}
	return 1;
    }
    if (warning == 0) {
	warning = 1;
	fprintf(stderr,"same_sec4_not_time does not handle pdt=%d",pdt);
    }
    return 0;
}

// same sec4 but ok if 0-1 hour ave/acc and 0-2 hour ave/acc


int same_sec4_diff_ave_period(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i, j;

    if (GB2_ProdDefTemplateNo(sec_a) != 8) return 0;
    i = GB2_Sec4_size(sec_a);
    if (GB2_Sec4_size(sec_b) != i) return 0;

    a = sec_a[4];
    b = sec_b[4];

    for (j = 0; j < 34; j++) {
	if (a[j] != b[j]) return 0;
    }

    for (j = 41; j < 50;  j++) {
	if (a[j] != b[j]) return 0;
    }

    for (j = 53; j < i; j++) {
	if (a[j] != b[j]) return 0;
    }
    return 1;
}

// test for same sec4 merge

int same_sec4_for_merge(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i, j;

    if (GB2_ProdDefTemplateNo(sec_a) != 8) return 0;
    i = GB2_Sec4_size(sec_a);
    if (GB2_Sec4_size(sec_b) != i) return 0;
    if (sec_a[4][41] != 1 || sec_b[4][41] != 1) return 0;	// only simple PDT = 8 forms

    a = sec_a[4];
    b = sec_b[4];

    for (j = 0; j < 18; j++) {
	if (a[j] != b[j]) return 0;
    }
    for (j = 22; j < 34; j++) {
	if (a[j] != b[j]) return 0;
    }
    for (j = 41; j < 53; j++) {
	if (a[j] != b[j]) return 0;
    }
    return 1;
}
