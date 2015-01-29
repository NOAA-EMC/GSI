/*
This file is part of netcdf-4, a netCDF-like interface for HDF5, or a
HDF5 backend for netCDF, depending on your point of view.

This file provides the fortran functions for the new functions added
to the API as part of netCDF-4. This file is only compiled for
netCDF-4 builds.

Copyright 2005, University Corporation for Atmospheric Research. See
COPYRIGHT file for copying and redistribution conditions.

$Id: fort-nc4.c,v 1.10 2006/10/02 21:27:43 ed Exp $
*/

#include <config.h>
#include <netcdf.h>
#include "ncfortran.h"

#ifdef LOGGING
FCALLSCFUN1(NF_INT, nc_set_log_level, NF_SET_LOG_LEVEL, nf_set_log_level,
	    FINT2CINT)
#endif /* LOGGING */

FCALLSCFUN5(NF_INT, nc_create_par, NF_CREATE_PAR, nf_create_par,
	    STRING, FINT2CINT, FINT2CINT, FINT2CINT, PCINT2FINT)

FCALLSCFUN5(NF_INT, nc_open_par, NF_OPEN_PAR, nf_open_par,
	    STRING, FINT2CINT, FINT2CINT, FINT2CINT, PCINT2FINT)

FCALLSCFUN3(NF_INT, nc_var_par_access, NF_VAR_PAR_ACCESS, nf_var_par_access,
	    NCID, VARID, FINT2CINT)

FCALLSCFUN3(NF_INT, nc_inq_ncid, NF_INQ_NCID, nf_inq_ncid,
	    NCID, STRING, PCINT2FINT)

FCALLSCFUN3(NF_INT, nc_inq_grps, NF_INQ_GRPS, nf_inq_grps,
	    NCID, PCINT2FINT, PCINT2FINT)

FCALLSCFUN2(NF_INT, nc_inq_grpname, NF_INQ_GRPNAME, nf_inq_grpname,
	    NCID, STRING)

FCALLSCFUN3(NF_INT, nc_inq_varids, NF_INQ_VARIDS, nf_inq_varids,
	    NCID, PCINT2FINT, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_inq_dimids, NF_INQ_DIMIDS, nf_inq_dimids,
	    NCID, PCINT2FINT, PCINT2FINT, FINT2CINT)

FCALLSCFUN3(NF_INT, nc_inq_typeids, NF_INQ_TYPEIDS, nf_inq_typeids,
	    NCID, PCINT2FINT, PCINT2FINT)

FCALLSCFUN3(NF_INT, nc_def_grp, NF_DEF_GRP, nf_def_grp,
	    NCID, STRING, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_def_compound, NF_DEF_COMPOUND, nf_def_compound,
	    NCID, FINT2CSIZET, STRING, PCINT2FINT)

FCALLSCFUN5(NF_INT, nc_insert_compound, NF_INSERT_COMPOUND, nf_insert_compound,
	    NCID, FINT2CINT, STRING, FINT2CSIZET, FINT2CINT)

FCALLSCFUN7(NF_INT, nc_insert_array_compound, NF_INSERT_ARRAY_COMPOUND, 
	    nf_insert_array_compound,
	    NCID, FINT2CINT, STRING, FINT2CSIZET, 
	    FINT2CINT, FINT2CINT, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_inq_type, NF_INQ_TYPE, nf_inq_type,
	    NCID, FINT2CINT, STRING, PSIZET)

FCALLSCFUN5(NF_INT, nc_inq_compound, NF_INQ_COMPOUND, nf_inq_compound,
	    NCID, FINT2CINT, STRING, PSIZET, PSIZET)

FCALLSCFUN3(NF_INT, nc_inq_compound_name, NF_COMPOUND_NAME, nf_compound_name,
	    NCID, FINT2CINT, STRING)

FCALLSCFUN3(NF_INT, nc_inq_compound_size, NF_COMPOUND_SIZE, nf_compound_size,
	    NCID, FINT2CINT, PSIZET)

FCALLSCFUN3(NF_INT, nc_inq_compound_nfields, NF_COMPOUND_NFIELDS, 
	    nf_compound_nfields,
	    NCID, FINT2CINT, PSIZET)

FCALLSCFUN8(NF_INT, nc_inq_compound_field, NF_INQ_COMPOUND_FIELD, 
	    nf_inq_compound_field,
	    NCID, FINT2CINT, FINT2CINT, STRING, PSIZET, PCINT2FINT, 
	    PCINT2FINT, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_inq_compound_fieldname, NF_COMPOUND_FIELDNAME, 
	    nf_compound_fieldname,
	    NCID, FINT2CINT, FINT2CINT, STRING)

FCALLSCFUN4(NF_INT, nc_inq_compound_fieldindex, NF_COMPOUND_FIELDINDEX, 
	    nf_compound_fieldindex,
	    NCID, FINT2CINT, STRING, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_inq_compound_fieldtype, NF_COMPOUND_FIELDTYPE, 
	    nf_compound_fieldtype,
	    NCID, FINT2CINT, FINT2CINT, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_inq_compound_fieldndims, NF_COMPOUND_FIELDNDIMS, 
	    nf_compound_fieldndims,
	    NCID, FINT2CINT, FINT2CINT, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_inq_compound_fielddim_sizes, NF_COMPOUND_FIELDDIM_SIZES, 
	    nf_compound_fielddim_sizes,
	    NCID, FINT2CINT, FINT2CINT, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_def_vlen, NF_DEF_VLEN, nf_def_vlen,
	    NCID, STRING, FINT2CINT, PCINT2FINT)

FCALLSCFUN5(NF_INT, nc_inq_vlen, NF_INQ_VLEN, nf_inq_vlen,
	    NCID, FINT2CINT, STRING, PSIZET, PCINT2FINT)

FCALLSCFUN7(NF_INT, nc_inq_user_type, NF_INQ_USER_TYPE, nf_inq_user_type,
	    NCID, FINT2CINT, STRING, PSIZET, PCINT2FINT, PSIZET, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_def_enum, NF_DEF_ENUM, nf_def_enum,
	    NCID, FINT2CINT, STRING, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_insert_enum, NF_INSERT_ENUM, nf_insert_enum,
	    NCID, FINT2CINT, STRING, PVOID)

FCALLSCFUN6(NF_INT, nc_inq_enum, NF_INQ_ENUM, nf_inq_enum,
	    NCID, FINT2CINT, STRING, PCINT2FINT, PSIZET, PSIZET)

FCALLSCFUN5(NF_INT, nc_inq_enum_member, NF_INQ_ENUM_MEMBER, nf_inq_enum_member,
	    NCID, FINT2CINT, FINT2CINT, STRING, PVOID)

FCALLSCFUN4(NF_INT, nc_def_opaque, NF_DEF_OPAQUE, nf_def_opaque,
	    NCID, FINT2CSIZET, STRING, PCINT2FINT)

FCALLSCFUN4(NF_INT, nc_inq_opaque, NF_INQ_OPAQUE, nf_inq_opaque,
	    NCID, FINT2CSIZET, STRING, PSIZET)

FCALLSCFUN5(NF_INT, nc_def_var_chunking, NF_DEF_VAR_CHUNKING, nf_def_var_chunking,
	    NCID, VARID, PCINT2FINT, PCINT2FINT, PCINT2FINT)

FCALLSCFUN5(NF_INT, nc_def_var_deflate, NF_DEF_VAR_DEFLATE, nf_def_var_deflate,
	    NCID, VARID, FINT2CINT, FINT2CINT, FINT2CINT)

FCALLSCFUN3(NF_INT, nc_def_var_fletcher32, NF_DEF_VAR_FLETCHER32, nf_def_var_fletcher32,
	    NCID, VARID, FINT2CINT)

FCALLSCFUN5(NF_INT, nc_inq_var_chunking, NF_INQ_VAR_CHUNKING, nf_inq_var_chunking,
	    NCID, VARID, PCINT2FINT, PCINT2FINT, PCINT2FINT)

FCALLSCFUN5(NF_INT, nc_inq_var_deflate, NF_INQ_VAR_DEFLATE, nf_inq_var_deflate,
	    NCID, VARID, PCINT2FINT, PCINT2FINT, PCINT2FINT)

FCALLSCFUN3(NF_INT, nc_inq_var_fletcher32, NF_INQ_VAR_FLETCHER32, nf_inq_var_fletcher32,
	    NCID, VARID, PCINT2FINT)

FCALLSCFUN3(NF_INT, nc_def_var_endian, NF_DEF_VAR_ENDIAN, nf_def_var_endian,
	    NCID, VARID, FINT2CINT)

FCALLSCFUN3(NF_INT, nc_inq_var_endian, NF_INQ_VAR_ENDIAN, nf_inq_var_endian,
	    NCID, VARID, PCINT2FINT)




