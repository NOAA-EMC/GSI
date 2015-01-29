#include "netcdf.h"
#include "ncfortran.h"


/*
 * Return a string that identifies the version of the netCDF library.
 */
FCALLSCFUN0(STRING, (char*)nc_inq_libvers, NF_INQ_LIBVERS, nf_inq_libvers)


/*
 * Return the string associated with an error code.
 */
FCALLSCFUN1(STRING, (char*)nc_strerror, NF_STRERROR, nf_strerror, 
	    FINT2CINT)


static int
nc_issyserr(int errcode)
{
    return errcode > 0;
}


/*
 * Indicate whether or not an error-code refers to a system error
 * rather than a netCDF error
 */
FCALLSCFUN1(LOGICAL, nc_issyserr, NF_ISSYSERR, nf_issyserr,
	    FINT2CINT)
