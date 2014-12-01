/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/ncdump/ncdump.h,v 1.6 2005/07/22 23:01:39 russ Exp $
 *********************************************************************/


/* error checking macro */
#define NC_CHECK(status)    {\
	    int nc_status = status;\
	    if(nc_status != NC_NOERR)\
	        error(nc_strerror(nc_status));\
	}

#define  Printf  (void) printf

typedef int boolean;
enum {false=0, true=1};

typedef struct {			/* dimension */
    char name[NC_MAX_NAME];
    size_t size;
} ncdim_t;

typedef struct {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int *dims;
    int natts;
    boolean has_fillval;
    double fillval;
} ncvar_t;

typedef struct {			/* attribute */
    int var;
    char name[NC_MAX_NAME];
    nc_type type;
    size_t len;
    char *string;		/* for text attributes (type = NC_CHAR) */
    double *vals;		/* for numeric attributes of all types */
} ncatt_t;

typedef
enum {LANG_C, LANG_F} Nclang; 

typedef struct {			/* specification for how to format dump */

    char *name;			/* name specified with -n or derived from
				 * file name */

    boolean header_only;	/* if true, don't print any variable data */

    boolean coord_vals;		/* if true, print header and coordinate
				 * dimension values (values of variables
				 * that are also dimensions), but no other
				 * variable data */

    boolean brief_data_cmnts;	/* if true, put // comments in data section
				 * identifying variable and indices, useful
				 * for navigating through large
				 * multi-dimensional data lists.  */

    boolean full_data_cmnts;	/* if true, put // comments in data section
				 * identifying every value, useful for
				 * navigating through large
				 * multi-dimensional data lists.  */

    Nclang data_lang;		/* Specifies index conventions used in data
				 * comments, either LANG_C (C, 0-based,
				 * column major) or LANG_F (Fortran,
				 * 1-based, row major) */

    int nlvars;			/* Number of variables specified with -v
				 * option on command line */

    char** lvars;		/* list of variable names specified with -v
				 * option on command line */
} fspec_t;
