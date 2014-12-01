#include <stddef.h>	/* for NULL */
#include <errno.h>

#include "netcdf.h"
#include "ncfortran.h"
#include "fort-lib.h"


/*
 * Convert a C dimension-ID vector into a FORTRAN dimension-ID vector.
 */
NF_INTEGER*
c2f_dimids(int ncid, int varid, const int* cdimids, NF_INTEGER* fdimids)
{
    int	i;
    int	ndims;

    if (nc_inq_varndims(ncid, varid, &ndims) != 0)
	return NULL;

    for (i = 0; i < ndims; ++i)
	fdimids[ndims - 1 - i] = cdimids[i] + 1;

    return fdimids;
}


/*
 * Convert a FORTRAN dimension-ID vector into a C dimension-ID vector.
 */
int*
f2c_dimids(int ndims, const NF_INTEGER* fdimids, int* cdimids)
{
    int	i;

    for (i = 0; i < ndims; ++i)
	cdimids[i] = fdimids[ndims - 1 - i] - 1;

    return cdimids;
}


/*
 * Convert FORTRAN co-ordinates into C co-ordinates.
 */
size_t*
f2c_coords(int ncid, int varid, const NF_INTEGER* fcoords,
    size_t* ccoords)
{
    int	i;
    int	ndims;

    if (nc_inq_varndims(ncid, varid, &ndims) != 0)
	return NULL;

    for (i = 0; i < ndims; ++i)
	ccoords[i] = fcoords[ndims - 1 - i] - 1;

    return ccoords;
}


/*
 * Convert FORTRAN counts into C counts.
 */
size_t*
f2c_counts(int ncid, int varid, const NF_INTEGER* fcounts,
    size_t* ccounts)
{
    int	i;
    int	ndims;

    if (nc_inq_varndims(ncid, varid, &ndims) != 0)
	return NULL;

    for (i = 0; i < ndims; ++i)
	ccounts[i] = fcounts[ndims - 1 - i];

    return ccounts;
}


/*
 * Convert FORTRAN strides into C strides.
 *
 * Helper function.
 */
ptrdiff_t*
f2c_strides(int ncid, int varid, const NF_INTEGER* fstrides,
    ptrdiff_t* cstrides)
{
    int	i;
    int	ndims;

    if (nc_inq_varndims(ncid, varid, &ndims) != 0)
	return NULL;

    for (i = 0; i < ndims; ++i)
	cstrides[i] = fstrides[ndims - 1 - i];

    return cstrides;
}


/*
 * Convert a FORTRAN mapping vector into a C mapping vector.
 */
ptrdiff_t*
f2c_maps(int ncid, int varid, const NF_INTEGER* fmaps, ptrdiff_t* cmaps)
{
    return f2c_strides(ncid, varid, fmaps, cmaps);
}
