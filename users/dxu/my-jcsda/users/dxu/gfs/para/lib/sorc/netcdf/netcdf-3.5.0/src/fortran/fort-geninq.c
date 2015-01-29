#include "netcdf.h"
#include "ncfortran.h"


/*
 * Generally inquire about a netCDF dataset.
 */
FCALLSCFUN5(NF_INT, nc_inq, NF_INQ, nf_inq,
	    NCID, PNDIMS, PNVARS, PNATTS, PDIMID)


/*
 * Inquire about the number of dimensions in a netCDF dataset.
 */
FCALLSCFUN2(NF_INT, nc_inq_ndims, NF_INQ_NDIMS, nf_inq_ndims,
	    NCID, PNDIMS)


/*
 * Inquire about the number of variables in a netCDF dataset.
 */
FCALLSCFUN2(NF_INT, nc_inq_nvars, NF_INQ_NVARS, nf_inq_nvars,
	    NCID, PNVARS)


/*
 * Inquire about the number of attributes in a netCDF dataset.
 */
FCALLSCFUN2(NF_INT, nc_inq_natts, NF_INQ_NATTS, nf_inq_natts,
	    NCID, PNATTS)


/*
 * Inquire about the index of the unlimited dimension in a netCDF dataset.
 */
FCALLSCFUN2(NF_INT, nc_inq_unlimdim, NF_INQ_UNLIMDIM, nf_inq_unlimdim,
	    NCID, PDIMID)
