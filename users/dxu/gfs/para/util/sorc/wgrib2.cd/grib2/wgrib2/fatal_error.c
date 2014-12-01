#include <stdio.h>
#include <stdlib.h>
#include "wgrib2.h"

/*
 * write fatal error message .. so to have common format 
 */

void fatal_error(const char *fmt, const char *string)
{
    fprintf(stderr, "\n*** FATAL ERROR: ");
    fprintf(stderr, fmt, string);
    fprintf(stderr," ***\n\n");
    exit(8);
    return;
}

void fatal_error_i(const char *fmt, const int i)
{
    fprintf(stderr, "\n*** FATAL ERROR: ");
    fprintf(stderr, fmt, i);
    fprintf(stderr," ***\n\n");
    exit(8);
    return;
}

void fatal_error_ii(const char *fmt, const int i, const int j)
{
    fprintf(stderr, "\n*** FATAL ERROR: ");
    fprintf(stderr, fmt, i, j);
    fprintf(stderr," ***\n\n");
    exit(8);
    return;
}
