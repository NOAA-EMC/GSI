! This is part of the netCDF-4 fortran 90 API.
! Copyright 2006, UCAR
! $Id: netcdf4_externals.f90,v 1.2 2006/10/02 21:27:43 ed Exp $

  ! Extra netCDF-4 functions 
  integer, external :: nf_create_par, nf_open_par, nf_var_par_access,     &
       nf_inq_ncid, nf_inq_grps, nf_inq_grpname, nf_inq_varids, &
       nf_inq_dimids, nf_inq_typeids, nf_def_grp, nf_def_compound, &
       nf_insert_compound, nf_insert_array_compound, nf_inq_type, &
       nf_inq_compound, nf_inq_compound_name, nf_inq_compound_size, &
       nf_inq_compound_nfields, nf_inq_compound_field, &
       nf_inq_compound_fieldname, nf_inq_compound_fieldindex, &
       nf_inq_compound_fieldtype, nf_inq_compound_fieldndims, &
       nf_inq_compound_fielddim_sizes, nf_def_vlen, nf_inq_vlen, &
       nf_inq_user_type, nf_def_enum, nf_insert_enum, nf_inq_enum, nf_inq_enum_member, &
       nf_def_opaque, nf_inq_opaque, nf_def_var_chunking, nf_def_var_deflate, &
       nf_def_var_fletcher32, nf_inq_var_chunking, nf_inq_var_deflate, &
       nf_inq_var_fletcher32, nf_inq_var_endian, nf_def_var_endian
  

