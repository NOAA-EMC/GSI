#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wgrib2.h"

/*
 * a simple extension to fopen
 *
 * if file is already open, just return the pointer to the file handler
 *
 * public domain 2/2008 Wesley Ebisuzaki
 *
 */

FILE *ffopen(const char *filename, const char *mode) {

    struct opened_file { char *name; FILE *handle; struct opened_file *next; };
    static struct opened_file *opened_file_start = NULL;
    struct opened_file *ptr;

    if (strcmp(filename,"-") == 0) return stdout;

    ptr = opened_file_start;
    while (ptr != NULL) {
	if (strcmp(filename,ptr->name) == 0) return ptr->handle;
	ptr = ptr->next;
    }

    ptr = malloc( sizeof(struct opened_file) );
    ptr->handle = fopen(filename,mode);
    ptr->name = malloc(strlen(filename) + 1);
    strcpy(ptr->name, filename);
    ptr->next = opened_file_start;
    opened_file_start = ptr;

    return ptr->handle;
}
