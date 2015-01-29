/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/README file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/ncdump/dumplib.c,v 1.11 2007/02/16 13:39:09 ed Exp $
 *********************************************************************/

/*
 * We potentially include <stdarg.h> before <stdio.h> in order to obtain a
 * definition for va_list from the GNU C compiler.
 */
#include <config.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "netcdf.h"
#include "dumplib.h"
#include "ncdump.h"

static char* has_c_format_att(int ncid, int varid);
static vnode* newvnode(void);

int float_precision_specified = 0; /* -p option specified float precision */
int double_precision_specified = 0; /* -p option specified double precision */
char float_var_fmt[] = "%.NNg";
char double_var_fmt[] = "%.NNg";
char float_att_fmt[] = "%#.NNgf";
char float_attx_fmt[] = "%#.NNg";
char double_att_fmt[] = "%#.NNg";

/*
 * Print error message to stderr and exit
 */
void
error(const char *fmt, ...)
{
    va_list args ;

    (void) fprintf(stderr,"%s: ", progname);
    va_start(args, fmt) ;
    (void) vfprintf(stderr,fmt,args) ;
    va_end(args) ;

    (void) fprintf(stderr, "\n") ;
    (void) fflush(stderr);	/* to ensure log files are current */
    exit(EXIT_FAILURE);
}

void *
emalloc (			/* check return from malloc */
	size_t size)
{
    void   *p;

    p = (void *) malloc (size);
    if (p == 0) {
	error ("out of memory\n");
    }
    return p;
}

#define LINEPIND	"    "	/* indent of continued lines */

static int linep;
static int max_line_len;

void
set_indent(int in)
{
    linep = in;
}


void
set_max_len(int len)
{
    max_line_len = len-2;
}


void
lput(const char *cp)
{
    size_t nn = strlen(cp);

    if (nn+linep > max_line_len && nn > 2) {
	(void) fputs("\n", stdout);
	(void) fputs(LINEPIND, stdout);
	linep = (int)strlen(LINEPIND);
    }
    (void) fputs(cp,stdout);
    linep += nn;
}

/* In case different formats specified with -d option, set them here. */
void
set_formats(int float_digits, int double_digits)
{
    (void) sprintf(float_var_fmt, "%%.%dg", float_digits);
    (void) sprintf(double_var_fmt, "%%.%dg", double_digits);
    (void) sprintf(float_att_fmt, "%%#.%dgf", float_digits);
    (void) sprintf(float_attx_fmt, "%%#.%dg", float_digits);
    (void) sprintf(double_att_fmt, "%%#.%dg", double_digits);
}


static char *
has_c_format_att(
    int ncid,			/* netcdf id */
    int varid			/* variable id */
    )
{
    nc_type cfmt_type;
    size_t cfmt_len;
#define C_FMT_NAME	"C_format" /* name of C format attribute */
#define	MAX_CFMT_LEN	100	/* max length of C format attribute */
    static char cfmt[MAX_CFMT_LEN];
    
    /* we expect nc_inq_att to fail if there is no "C_format" attribute */
    int nc_stat = nc_inq_att(ncid, varid, "C_format", &cfmt_type, &cfmt_len);

    switch(nc_stat) {
    case NC_NOERR:
	if (cfmt_type == NC_CHAR && cfmt_len != 0 && cfmt_len < MAX_CFMT_LEN) {
	    int nc_stat = nc_get_att_text(ncid, varid, "C_format", cfmt);
	    if(nc_stat != NC_NOERR) {
		fprintf(stderr, "Getting 'C_format' attribute %s\n", 
			nc_strerror(nc_stat));
		(void) fflush(stderr);
	    }
	    return &cfmt[0];
	}
	break;
    case NC_ENOTATT:
	break;
    default:
	fprintf(stderr, "Inquiring about 'C_format' attribute %s\n", 
		nc_strerror(nc_stat));
	(void) fflush(stderr);
	break;
    }
    return 0;
}


/*
 * Determine print format to use for each value for this variable.  Use value
 * of attribute C_format if it exists, otherwise a sensible default.
 */
const char *
get_fmt(
     int ncid,			/* netcdf id */
     int varid,			/* variable id */
     nc_type type		/* netCDF data type */
     )
{
    char *c_format_att;

    /* float or double precision specified with -p option overrides any
       C_format attribute value, so check for that first. */

    if (float_precision_specified && type == NC_FLOAT)
	return float_var_fmt;

    if (double_precision_specified && type == NC_DOUBLE)
	return double_var_fmt;

    /* If C_format attribute exists, return it */
    c_format_att = has_c_format_att(ncid, varid);
    if (c_format_att)
      return c_format_att;    

    /* Otherwise return sensible default. */
    switch (type) {
      case NC_BYTE:
	return "%d";
      case NC_CHAR:
	return "%s";
      case NC_SHORT:
	return "%d";
      case NC_INT:
 	return "%d";
      case NC_FLOAT:
	return float_var_fmt;
      case NC_DOUBLE:
	return double_var_fmt;
#ifdef USE_NETCDF4
       case NC_UBYTE:
	  return "%d";
       case NC_USHORT:
	  return "%d";
       case NC_UINT:
	  return "%d";
       case NC_INT64:
       case NC_UINT64:
	  return "%lld";
       case NC_STRING:
	  return "%s";
#endif	/* USE_NETCDF4 */
      default:
	error("pr_vals: bad type");
    }

    return 0;
}


static vnode*
newvnode(void)
{
    vnode *newvp = (vnode*) emalloc(sizeof(vnode));
    return newvp;
}


/*
 * Get a new, empty variable list.
 */
vnode*
newvlist(void)
{
    vnode *vp = newvnode();

    vp -> next = 0;
    vp -> id = -1;		/* bad id */

    return vp;
}


void
varadd(vnode* vlist, int varid)
{
    vnode *newvp = newvnode();
    
    newvp -> next = vlist -> next;
    newvp -> id = varid;
    vlist -> next = newvp;
}


/* 
 * return 1 if variable identified by varid is member of variable
 * list vlist points to.
 */
int
varmember(const vnode* vlist, int varid)
{
    vnode *vp = vlist -> next;

    for (; vp ; vp = vp->next)
      if (vp->id == varid)
	return 1;
    return 0;    
}


/*
 * return 1 if varid identifies a coordinate variable
 * else return 0
 */
int
iscoordvar(int ncid, int varid)
{
    int ndims;
    int dimid;
    ncdim_t *dims;
    int is_coord = 0;		/* true if variable is a coordinate variable */
    char varname[NC_MAX_NAME];
    int varndims;

    NC_CHECK( nc_inq_ndims(ncid, &ndims) );
    dims = (ncdim_t *) emalloc((ndims + 1) * sizeof(ncdim_t));
    for (dimid = 0; dimid < ndims; dimid++) {
	NC_CHECK( nc_inq_dimname(ncid, dimid, dims[dimid].name) );
    }
    NC_CHECK( nc_inq_varname(ncid, varid, varname) );
    NC_CHECK( nc_inq_varndims(ncid, varid, &varndims) );
   
    for (dimid = 0; dimid < ndims; dimid++) {
	if (strcmp(dims[dimid].name, varname) == 0 && varndims == 1) {
	    is_coord = 1;
	    break;
	}
    }
    free(dims);
    return is_coord;
}


/*
 * return 1 if varid identifies a record variable
 * else return 0
 */
int
isrecvar(int ncid, int varid)
{
    int recdimid;
    int ndims;
    int is_recvar = 0;
    int *dimids;

    NC_CHECK( nc_inq_unlimdim(ncid, &recdimid) );
    NC_CHECK( nc_inq_varndims(ncid, varid, &ndims) );
    if (ndims > 0) {
	dimids = (int *) emalloc((ndims + 1) * sizeof(int));
	NC_CHECK( nc_inq_vardimid(ncid, varid, dimids) );
	/* TODO assumes only one unlimited dimension, but netcdf4 alllows multiple */
	if(dimids[0] == recdimid)
	    is_recvar = 1;
	free(dimids);
    }
    return is_recvar;
}
