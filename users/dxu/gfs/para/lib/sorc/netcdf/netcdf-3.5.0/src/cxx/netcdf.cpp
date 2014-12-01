/*********************************************************************
 *   Copyright 1992, University Corporation for Atmospheric Research
 *   See netcdf/README file for copying and redistribution conditions.
 *
 *   Purpose:	Implements class interface for netCDF over C interface
 *
 *   $Header: /upc/share/CVS/netcdf-3/cxx/netcdf.cpp,v 1.4 2001/02/05 20:29:12 russ Exp $
 *********************************************************************/

#include <string.h>
#include <stdlib.h>
#include "netcdfcpp.h"

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

static const int ncGlobal = NC_GLOBAL; // psuedo-variable for global attributes

static const int ncBad = -1;	// failure return for netCDF C interface 

NcFile::~NcFile( void )
{
    (void) close();
}

NcBool NcFile::is_valid( void ) const
{
    return the_id != ncBad;
}

int NcFile::num_dims( void ) const
{
    int num = 0;
    if (is_valid())
      ncinquire(the_id, &num, 0, 0, 0);
    return num;
}

int NcFile::num_vars( void ) const
{
    int num = 0;
    if (is_valid())
      ncinquire(the_id, 0, &num, 0, 0);
    return num;
}

int NcFile::num_atts( void ) const
{
    int num = 0;
    if (is_valid())
      ncinquire(the_id, 0, 0, &num, 0);
    return num;
}

NcDim* NcFile::get_dim( NcToken name ) const
{
    int dimid = ncdimid(the_id, name);
    return get_dim(dimid);
}

NcVar* NcFile::get_var( NcToken name ) const
{
    int varid = ncvarid(the_id, name);
    return get_var(varid);
}

NcAtt* NcFile::get_att( NcToken aname ) const
{
    return is_valid() ? globalv->get_att(aname) : 0;
}

NcDim* NcFile::get_dim( int i ) const
{
    if (! is_valid() || i < 0 || i >= num_dims())
      return 0;
    return dimensions[i];
}

NcVar* NcFile::get_var( int i ) const
{
    if (! is_valid() || i < 0 || i >= num_vars())
      return 0;
    return variables[i];
}

NcAtt* NcFile::get_att( int n ) const
{
    return is_valid() ? globalv->get_att(n) : 0;
}

NcDim* NcFile::rec_dim( ) const
{
    if (! is_valid())
      return 0;
    int recdim;
    ncinquire(the_id, 0, 0, 0, &recdim);
    if (recdim == -1)
      return 0;
    return get_dim(recdim);
}

NcDim* NcFile::add_dim(NcToken name, long size)
{
    if (!is_valid() || !define_mode())
      return 0;
    int n = num_dims();
    NcDim* dimp = new NcDim(this, name, size);
    dimensions[n] = dimp;	// for garbage collection on close()
    return dimp;
}

NcDim* NcFile::add_dim(NcToken name)
{
    return add_dim(name, NC_UNLIMITED);
}

// To create scalar, 1-dimensional, ..., 5-dimensional variables, just supply
// as many dimension arguments as necessary

NcVar* NcFile::add_var(NcToken name, NcType type, // scalar to 5D var
			    const NcDim* dim0,
			    const NcDim* dim1,
			    const NcDim* dim2,
			    const NcDim* dim3,
			    const NcDim* dim4)
{
    if (!is_valid() || !define_mode())
      return 0;
    int dims[5];
    int ndims = 0;
    if (dim0) {
	ndims++;
	dims[0] = dim0->id();
	if (dim1) {
	    ndims++;
	    dims[1] = dim1->id();
	    if (dim2) {
		ndims++;
		dims[2] = dim2->id();
		if (dim3) {
		    ndims++;
		    dims[3] = dim3->id();
		    if (dim4) {
			ndims++;
			dims[4] = dim4->id();
		    }
		}
	    }
	}
    }
    int n = num_vars();
    NcVar* varp =
      new NcVar(this, ncvardef(the_id, name, (nc_type) type, ndims, dims));
    variables[n] = varp;
    return varp;
}

// For variables with more than 5 dimensions, use n-dimensional interface
// with vector of dimensions.

NcVar* NcFile::add_var(NcToken name, NcType type, int ndims, const NcDim** dims)
{
    if (!is_valid() || !define_mode())
      return 0;
    int* dimids = new int[ndims];
    for (int i=0; i < ndims; i++)
      dimids[i] = dims[i]->id();
    int n = num_vars();
    NcVar* varp =
      new NcVar(this, ncvardef(the_id, name, (nc_type) type, ndims, dimids));
    variables[n] = varp;
    delete [] dimids;
    return varp;
}

#define NcFile_add_scalar_att(TYPE)					      \
NcBool NcFile::add_att(NcToken aname, TYPE val)				      \
{									      \
    return globalv->add_att(aname, val);				      \
}

NcFile_add_scalar_att(char)
NcFile_add_scalar_att(ncbyte)
NcFile_add_scalar_att(short)
NcFile_add_scalar_att(int)
NcFile_add_scalar_att(long)
NcFile_add_scalar_att(float)
NcFile_add_scalar_att(double)
NcFile_add_scalar_att(const char*)

#define NcFile_add_vector_att(TYPE)					      \
NcBool NcFile::add_att(NcToken aname, int n, const TYPE* val)		      \
{									      \
    return globalv->add_att(aname, n, val);				      \
}

NcFile_add_vector_att(char)
NcFile_add_vector_att(ncbyte)
NcFile_add_vector_att(short)
NcFile_add_vector_att(int)
NcFile_add_vector_att(long)
NcFile_add_vector_att(float)
NcFile_add_vector_att(double)

NcBool NcFile::set_fill( FillMode a_mode )
{
  if (ncsetfill(the_id, a_mode) != ncBad) {
    the_fill_mode = a_mode;
    return TRUE;
  }
  return FALSE;
}

NcFile::FillMode NcFile::get_fill( void ) const
{
    return the_fill_mode;
}

NcBool NcFile::sync( void )
{
    if (!data_mode())
      return 0;
    if (ncsync(the_id) == ncBad)
      return 0;
    int i;
    for (i = 0; i < num_dims(); i++) {
	if (dimensions[i]->is_valid()) {
	    dimensions[i]->sync();
	} else {		// someone else added a new dimension
	    dimensions[i] = new NcDim(this,i);
	}
    }
    for (i = 0; i < num_vars(); i++) {
	if (variables[i]->is_valid()) {
	    variables[i]->sync();
	} else {		// someone else added a new variable
	    variables[i] = new NcVar(this,i);
	}
    }
    return 1;
}

NcBool NcFile::close( void )
{
    int i;
    
    if (the_id == ncBad)
      return 0;
    for (i = 0; i < num_dims(); i++)
      delete dimensions[i];
    for (i = 0; i < num_vars(); i++)
      delete variables[i];
    delete [] dimensions;
    delete [] variables;
    delete globalv;
    int old_id = the_id;
    the_id = ncBad;
    return ncclose(old_id) != ncBad;
}

NcBool NcFile::abort( void )
{
    return ncabort(the_id) != ncBad;
}

NcBool NcFile::define_mode( void )
{
    if (! is_valid())
      return FALSE;
    if (in_define_mode)
      return TRUE;
    if (ncredef(the_id) == ncBad)
      return FALSE;
    in_define_mode = 1;
    return TRUE;
}

NcBool NcFile::data_mode( void )
{
    if (! is_valid())
      return FALSE;
    if (! in_define_mode)
      return TRUE;
    if (ncendef(the_id) == ncBad)
      return FALSE;
    in_define_mode = 0;
    return TRUE;
}

int NcFile::id( void ) const
{
    return the_id;
}

NcFile::NcFile( const char* path, FileMode fmode, 
		size_t* chunksizeptr, size_t initialsize  )
{
    NcError err(NcError::silent_nonfatal); // constructor must not fail

    int mode = NC_NOWRITE;
    the_fill_mode = Fill;
    int status;

    switch (fmode) {
    case Write:
	mode = NC_WRITE;
	/*FALLTHRU*/
    case ReadOnly:
	// use netcdf-3 interface to permit specifying tuning parameter
	status = nc__open(path, mode, chunksizeptr, &the_id);
	if(status != NC_NOERR)
	{
		nc_advise("ncopen", status, "filename \"%s\"", path);
		the_id =  -1;
	}
	in_define_mode = 0;
	break;
    case New:
	mode = NC_NOCLOBBER;
	/*FALLTHRU*/
    case Replace:
	// use netcdf-3 interface to permit specifying tuning parameters
	status = nc__create(path, mode, initialsize,
				      chunksizeptr, &the_id);
	if(status != NC_NOERR)
	{
		nc_advise("nccreate", status, "filename \"%s\"", path);
		the_id =  -1;
	}
	in_define_mode = 1;
	break;
    default:
	the_id = ncBad;
	in_define_mode = 0;
	break;
    }
    if (is_valid()) {
	dimensions = new NcDim*[MAX_NC_DIMS];
	variables = new NcVar*[MAX_NC_VARS];
	int i;
	for (i = 0; i < num_dims(); i++)
	    dimensions[i] = new NcDim(this, i);
	for (i = 0; i < num_vars(); i++)
	    variables[i] = new NcVar(this, i);
	globalv = new NcVar(this, ncGlobal);
    } else {
	dimensions = 0;
	variables = 0;
	globalv = 0;
    }
}

NcToken NcDim::name( void ) const
{
    return the_name;
}

long NcDim::size( void ) const
{
    long sz = 0;
    if (the_file)
      ncdiminq(the_file->id(), the_id, (char*)0, &sz);
    return sz;
}

NcBool NcDim::is_valid( void ) const
{
    return the_file->is_valid() && the_id != ncBad;
}

NcBool NcDim::is_unlimited( void ) const
{
    if (!the_file)
      return FALSE;
    int recdim;
    ncinquire(the_file->id(), 0, 0, 0, &recdim);
    return the_id == recdim;
}

NcBool NcDim::rename(NcToken newname)
{
    if (strlen(newname) > strlen(the_name)) {
	if (! the_file->define_mode())
	    return FALSE;
    }
    NcBool ret = ncdimrename(the_file->id(), the_id, newname) != ncBad;
    if (ret) {
	delete [] the_name;
	the_name = new char[1 + strlen(newname)];
	strcpy(the_name, newname);
    }
    return ret;
}

int NcDim::id( void ) const
{
    return the_id;
}

NcBool NcDim::sync(void) 
{    
    char nam[MAX_NC_NAME];
    if (the_name) {
	delete [] the_name;
    }
    if (the_file && ncdiminq(the_file->id(), the_id, nam, 0) != ncBad) {
	the_name = new char[strlen(nam) + 1]; 
	strcpy(the_name, nam);
	return TRUE;
    }
    the_name = 0;
    return FALSE;
}

NcDim::NcDim(NcFile* nc, int id)
	: the_file(nc), the_id(id)
{
    char nam[MAX_NC_NAME];
    if (the_file && ncdiminq(the_file->id(), the_id, nam, 0) != ncBad) {
	the_name = new char[strlen(nam) + 1]; 
	strcpy(the_name, nam);
    } else {
	the_name = 0;
    }
}

NcDim::NcDim(NcFile* nc, NcToken name, long sz)
	: the_file(nc)
{
    the_id = ncdimdef(the_file->id(), name, sz);
    if (the_id != ncBad) {
	the_name = new char[strlen(name) + 1];
	strcpy(the_name, name);
    } else {
	the_name = 0;
    }
}

NcDim::~NcDim( void )
{
    delete [] the_name;
}

#define Nc_as(TYPE) name2(as_,TYPE)
#define NcTypedComponent_as(TYPE)				          \
TYPE NcTypedComponent::Nc_as(TYPE)( long n ) const		          \
{								          \
  NcValues* tmp = values();                                               \
  TYPE rval = tmp->Nc_as(TYPE)(n);                                        \
  delete tmp;                                                             \
  return rval;                                                            \
}
NcTypedComponent_as(ncbyte)
NcTypedComponent_as(char)
NcTypedComponent_as(short)
NcTypedComponent_as(int)
NcTypedComponent_as(nclong)
NcTypedComponent_as(long)
NcTypedComponent_as(float)
NcTypedComponent_as(double)

char* NcTypedComponent::as_string( long n ) const
{
    NcValues* tmp = values();
    char* rval = tmp->as_string(n);
    delete tmp;
    return rval;
}

NcTypedComponent::NcTypedComponent ( NcFile* nc )
	: the_file(nc)
{}

NcValues* NcTypedComponent::get_space( long numVals ) const
{
    NcValues* valp;
    if (numVals < 1)
	numVals = num_vals();
    switch (type()) {
      case ncFloat:
	valp = new NcValues_float(numVals);
	break;
      case ncDouble:
	valp = new NcValues_double(numVals);
	break;
      case ncInt:
	valp = new NcValues_int(numVals);
	break;
      case ncShort:
	valp = new NcValues_short(numVals);
	break;
      case ncByte:
      case ncChar:
	valp = new NcValues_char(numVals);
	break;
      case ncNoType:
      default:
	valp = 0;
    }
    return valp;
}

NcVar::~NcVar( void )
{
    delete[] the_cur;
    delete[] the_name;
}

NcToken NcVar::name( void ) const
{
    return the_name;
}

NcType NcVar::type( void ) const
{
    nc_type typ;
    ncvarinq(the_file->id(), the_id, 0, &typ, 0, 0, 0);
    return (NcType) typ;
}

NcBool NcVar::is_valid( void ) const
{
    return the_file->is_valid() && the_id != ncBad;
}

int NcVar::num_dims( void ) const
{
    int ndim;
    ncvarinq(the_file->id(), the_id, 0, 0, &ndim, 0, 0);
    return ndim;
}

// The i-th dimension for this variable
NcDim* NcVar::get_dim( int i ) const
{
    int ndim;
    int dims[MAX_NC_DIMS];
    if(ncvarinq(the_file->id(), the_id, 0, 0, &ndim, dims, 0) == ncBad ||
       i < 0 || i >= ndim)
      return 0;
    return the_file->get_dim(dims[i]);
}

long* NcVar::edges( void ) const	// edge lengths (dimension sizes)
{
    long* evec = new long[num_dims()];
    for(int i=0; i < num_dims(); i++)
      evec[i] = get_dim(i)->size();
    return evec;
}

int NcVar::num_atts( void ) const // handles variable and global atts
{
    int natt = 0;
    if (the_file->is_valid())
      if (the_id == ncGlobal)
	natt = the_file->num_atts();
      else
	ncvarinq(the_file->id(), the_id, 0, 0, 0, 0, &natt);
    return natt;
}

NcAtt* NcVar::get_att( NcToken aname ) const
{
    NcAtt* att = new NcAtt(the_file, this, aname);
    if (! att->is_valid()) {
	delete att;
	return 0;
    }
    return att;
}

NcAtt* NcVar::get_att( int n ) const
{
    if (n < 0 || n >= num_atts())
      return 0;
    NcToken aname = attname(n);
    NcAtt* ap = get_att(aname);
    delete [] (char*)aname;
    return ap;
}

long NcVar::num_vals( void ) const
{
    long prod = 1;
    for (int d = 0; d < num_dims(); d++)
	prod *= get_dim(d)->size();
    return  prod;
}

NcValues* NcVar::values( void ) const
{
    int ndims = num_dims();
    long crnr[MAX_NC_DIMS];
    long edgs[MAX_NC_DIMS];
    for (int i = 0; i < ndims; i++) {
	crnr[i] = 0;
	edgs[i] = get_dim(i)->size();
    }
    NcValues* valp = get_space();
    if (ncvarget(the_file->id(), the_id, crnr, edgs, valp->base()) == ncBad)
	return 0;
    return valp;
}

void NcVar::set_rec(long rec)
{
  cur_rec = rec;
  return;
} 

NcValues* NcVar::get_rec(void)
{
    return get_rec(cur_rec);
}

NcValues* NcVar::get_rec(long rec)
{
    long size = num_dims();
    long* start = new long[size];
    for (int i=1; i < size ; i++) start[i] = 0;
    start[0] = rec;
    NcBool result = set_cur(start);
    if (! result ) {
	delete [] start;
	return 0;
    }

    long* edge = edges();
    edge[0] = 1;
    NcValues* valp = get_space(rec_size());
    if (ncvarget(the_file->id(), the_id, start, edge, valp->base()) == ncBad) {
	delete [] start;
	delete [] edge;
	delete valp;
	return 0;
    }
    delete [] start;
    delete [] edge;
    return valp;
} 


#define NcVar_put_rec(TYPE)                                                   \
NcBool NcVar::put_rec( const TYPE* vals)                                      \
{                                                                             \
    return put_rec(vals, cur_rec);                                            \
}                                                                             \
                                                                              \
NcBool NcVar::put_rec( const TYPE* vals,                                      \
                     long rec)                                                \
{                                                                             \
    long size = num_dims();                                                   \
    long* start = new long[size];                                             \
    for (int i=1; i < size ; i++) start[i] = 0;                               \
    start[0] = rec;                                                           \
    NcBool result = set_cur(start);                                           \
    delete [] start;                                                          \
    if (! result )                                                            \
      return FALSE;                                                           \
                                                                              \
    long* edge = edges();                                                     \
    edge[0] = 1;                                                              \
    result = put(vals, edge);                                                 \
    delete [] edge;                                                           \
    return result;                                                            \
}

NcVar_put_rec(ncbyte)
NcVar_put_rec(char)
NcVar_put_rec(short)
NcVar_put_rec(int)
NcVar_put_rec(long)
NcVar_put_rec(float)
NcVar_put_rec(double)

long NcVar::rec_size(void) {
    long size = 1;
    long* edge = edges();
    for( int i = num_dims()-1 ; i>0 ; i--)
	size *= edge[i];
    delete [] edge;
    return size;
}

#define NcVar_get_index(TYPE)                                                 \
long NcVar::get_index(const TYPE* key)                                        \
{                                                                             \
if (type() != NcTypeEnum(TYPE))                                               \
    return -1;                                                                \
if (! the_file->data_mode())                                                  \
    return -1;                                                                \
long maxrec = get_dim(0)->size();                                             \
long maxvals = rec_size();                                                    \
NcValues* val;                                                                \
int validx;                                                                  \
for (long j=0; j<maxrec; j++) {                                               \
    val = get_rec(j);                                                         \
    if (val == NULL) return -1;                                               \
    for (validx = 0; validx < maxvals; validx++) {                            \
        if (key[validx] != val->as_ ## TYPE(validx)) break;                   \
        }                                                                     \
    delete val;                                                               \
    if (validx == maxvals) return j;                                          \
    }                                                                         \
return -1;                                                                    \
}


NcVar_get_index(ncbyte)
NcVar_get_index(char)
NcVar_get_index(short)
NcVar_get_index(nclong)
NcVar_get_index(long)
NcVar_get_index(float)
NcVar_get_index(double)
    

#define NcVar_put_array(TYPE)						      \
NcBool NcVar::put( const TYPE* vals,					      \
		     long edge0,					      \
		     long edge1,					      \
		     long edge2,					      \
		     long edge3,					      \
		     long edge4)					      \
{									      \
    if (type() != NcTypeEnum(TYPE))					      \
      return FALSE;							      \
    if (! the_file->data_mode())					      \
      return FALSE;							      \
    long count[5];							      \
    count[0] = edge0;							      \
    count[1] = edge1;							      \
    count[2] = edge2;							      \
    count[3] = edge3;							      \
    count[4] = edge4;							      \
    for (int i = 0; i < 5; i++) {					      \
	if (count[i]) {							      \
	    if (num_dims() < i)						      \
	      return FALSE;						      \
	} else								      \
	  break;							      \
    }									      \
    static long start[5] = {0, 0, 0, 0, 0};				      \
    for (int j = 0; j < 5; j++) {					      \
     start[j] = the_cur[j];						      \
    }									      \
    return ncvarput(the_file->id(), the_id, start, count, vals) != ncBad;     \
}

NcVar_put_array(ncbyte)
NcVar_put_array(char)
NcVar_put_array(short)
NcVar_put_array(int)
NcVar_put_array(long)
NcVar_put_array(float)
NcVar_put_array(double)

#define NcVar_put_nd_array(TYPE)					      \
NcBool NcVar::put( const TYPE* vals, const long* count )		      \
{									      \
    if (type() != NcTypeEnum(TYPE))					      \
      return FALSE;							      \
    if (! the_file->data_mode())					      \
      return FALSE;							      \
    long start[MAX_NC_DIMS];						      \
    for (int i = 0; i < num_dims(); i++)				      \
      start[i] = the_cur[i];						      \
    return ncvarput(the_file->id(), the_id, start, count, vals) != ncBad;     \
}

NcVar_put_nd_array(ncbyte)
NcVar_put_nd_array(char)
NcVar_put_nd_array(short)
NcVar_put_nd_array(int)
NcVar_put_nd_array(long)
NcVar_put_nd_array(float)
NcVar_put_nd_array(double)

#define NcVar_get_array(TYPE)						      \
NcBool NcVar::get( TYPE* vals,						      \
		     long edge0,					      \
		     long edge1,					      \
		     long edge2,					      \
		     long edge3,					      \
		     long edge4) const					      \
{									      \
    if (type() != NcTypeEnum(TYPE))					      \
      return FALSE;							      \
    if (! the_file->data_mode())					      \
      return FALSE;							      \
    long count[5];							      \
    count[0] = edge0;							      \
    count[1] = edge1;							      \
    count[2] = edge2;							      \
    count[3] = edge3;							      \
    count[4] = edge4;							      \
    for (int i = 0; i < 5; i++) {					      \
	if (count[i]) {							      \
	    if (num_dims() < i)						      \
	      return FALSE;						      \
	} else								      \
	  break;							      \
    }									      \
    static long start[5] = {0, 0, 0, 0, 0};				      \
    for (int j = 0; j < 5; j++) {					      \
     start[j] = the_cur[j];						      \
    }									      \
    return ncvarget(the_file->id(), the_id, start, count, vals) != ncBad;     \
}

NcVar_get_array(ncbyte)
NcVar_get_array(char)
NcVar_get_array(short)
NcVar_get_array(int)
NcVar_get_array(long)
NcVar_get_array(float)
NcVar_get_array(double)

#define NcVar_get_nd_array(TYPE)					      \
NcBool NcVar::get( TYPE* vals, const long* count ) const		      \
{									      \
    if (type() != NcTypeEnum(TYPE))					      \
      return FALSE;							      \
    if (! the_file->data_mode())					      \
      return FALSE;							      \
    long start[MAX_NC_DIMS];						      \
    for (int i = 0; i < num_dims(); i++)				      \
	start[i] = the_cur[i];						      \
    return ncvarget(the_file->id(), the_id, start, count, vals) != ncBad;     \
}

NcVar_get_nd_array(ncbyte)
NcVar_get_nd_array(char)
NcVar_get_nd_array(short)
NcVar_get_nd_array(int)
NcVar_get_nd_array(long)
NcVar_get_nd_array(float)
NcVar_get_nd_array(double)

// If no args, set cursor to all zeros.	 Else set initial elements of cursor
// to args provided, rest to zeros.
NcBool NcVar::set_cur(long c0, long c1, long c2, long c3, long c4)
{
    long t[6];
    t[0] = c0;
    t[1] = c1;
    t[2] = c2;
    t[3] = c3;
    t[4] = c4;
    t[5] = -1;
    for(int j = 0; j < 6; j++) { // find how many parameters were used
	int i;
	if (t[j] == -1) {
	    if (num_dims() < j)
	      return FALSE;	// too many for variable's dimensionality
	    for (i = 0; i < j; i++) {
		if (t[i] >= get_dim(i)->size() && ! get_dim(i)->is_unlimited())
		  return FALSE;	// too big for dimension
		the_cur[i] = t[i];
	    }
	    for(i = j; i < num_dims(); i++)
	      the_cur[i] = 0;
	    return TRUE;
	}
    }
    return TRUE;
}

NcBool NcVar::set_cur(long* cur)
{
    for(int i = 0; i < num_dims(); i++) {
	if (cur[i] >= get_dim(i)->size() && ! get_dim(i)->is_unlimited())
	  return FALSE;
	the_cur[i] = cur[i];
    }
    return TRUE;
}

#define NcVar_add_scalar_att(TYPE)					      \
NcBool NcVar::add_att(NcToken aname, TYPE val)				      \
{									      \
    if (! the_file->define_mode())					      \
      return FALSE;							      \
    if (ncattput(the_file->id(), the_id, aname, (nc_type) NcTypeEnum(TYPE),   \
		 1, &val) == ncBad)					      \
      return FALSE;							      \
    return TRUE;							      \
}									      \

NcVar_add_scalar_att(char)
NcVar_add_scalar_att(ncbyte)
NcVar_add_scalar_att(short)
NcVar_add_scalar_att(int)
NcVar_add_scalar_att(long)
NcVar_add_scalar_att(double)

NcBool NcVar::add_att(NcToken aname, float val)
{
    if (! the_file->define_mode())
      return FALSE;
    float fval = (float) val;	// workaround for bug, val passed as double??
    if (ncattput(the_file->id(), the_id, aname, (nc_type) ncFloat,
		 1, &fval) == ncBad)
      return FALSE;
    return TRUE;
}

NcBool NcVar::add_att(NcToken aname, const char* val)
{
    if (! the_file->define_mode())
      return FALSE;
    if (ncattput(the_file->id(), the_id, aname, (nc_type) ncChar,
		 strlen(val), val) == ncBad)
      return FALSE;
    return TRUE;
}

#define NcVar_add_vector_att(TYPE)					      \
NcBool NcVar::add_att(NcToken aname, int len, const TYPE* vals)		      \
{									      \
    if (! the_file->define_mode())					      \
      return FALSE;							      \
    if (ncattput(the_file->id(), the_id, aname, (nc_type) NcTypeEnum(TYPE),   \
		 len, vals) == ncBad)					      \
      return FALSE;							      \
    return TRUE;							      \
}
NcVar_add_vector_att(char)
NcVar_add_vector_att(ncbyte)
NcVar_add_vector_att(short)
NcVar_add_vector_att(int)
NcVar_add_vector_att(long)
NcVar_add_vector_att(float)
NcVar_add_vector_att(double)

NcBool NcVar::rename(NcToken newname)
{
    if (strlen(newname) > strlen(the_name)) {
	if (! the_file->define_mode())
	    return FALSE;
    }
    NcBool ret = ncvarrename(the_file->id(), the_id, newname) != ncBad;
    if (ret) {
	delete [] the_name;
	the_name = new char [1 + strlen(newname)];
	strcpy(the_name, newname);
    }
    return ret;
}

int NcVar::id( void ) const
{
    return the_id;
}

NcBool NcVar::sync(void)
{
    if (the_name) {
	delete [] the_name;
    }
    if (the_cur) {
	delete [] the_cur;
    }
    char nam[MAX_NC_NAME];
    if (the_file 
	&& ncvarinq(the_file->id(), the_id, nam, 0, 0, 0, 0) != ncBad) {
	the_name = new char[1 + strlen(nam)];
	strcpy(the_name, nam);
    } else {
	the_name = 0;
	return FALSE;
    }
    cur_rec = 0;
    init_cur(); 
    return TRUE;
}


NcVar::NcVar(NcFile* nc, int id)
   : NcTypedComponent(nc), the_id(id)
{
    char nam[MAX_NC_NAME];
    if (the_file 
	&& ncvarinq(the_file->id(), the_id, nam, 0, 0, 0, 0) != ncBad) {
	the_name = new char[1 + strlen(nam)];
	strcpy(the_name, nam);
    } else {
	the_name = 0;
    }
    cur_rec = 0;
    init_cur();
}

int NcVar::attnum( NcToken attrname ) const
{
    int num;
    for(num=0; num < num_atts(); num++) {
	char aname[MAX_NC_NAME];
	ncattname(the_file->id(), the_id, num, aname);
	if (strcmp(aname, attrname) == 0)
	  break;
    }
    return num;			// num_atts() if no such attribute
}

NcToken NcVar::attname( int attnum ) const // caller must delete[]
{
    if (attnum < 0 || attnum >= num_atts())
      return 0;
    char aname[MAX_NC_NAME];
    if (ncattname(the_file->id(), the_id, attnum, aname) == ncBad)
      return 0;
    char* rname = new char[1 + strlen(aname)];
    strcpy(rname, aname);
    return rname;
}

void NcVar::init_cur( void )
{
    the_cur = new long[MAX_NC_DIMS]; // *** don't know num_dims() yet?
    for(int i = 0; i < MAX_NC_DIMS; i++)
      the_cur[i] = 0;
}

NcAtt::NcAtt(NcFile* nc, const NcVar* var, NcToken name)
   : NcTypedComponent(nc), the_variable(var)
{
    the_name = new char[1 + strlen(name)];
    strcpy(the_name, name);
}

NcAtt::NcAtt(NcFile* nc, NcToken name)
   : NcTypedComponent(nc), the_variable(NULL)
{
    the_name = new char[1 + strlen(name)];
    strcpy(the_name, name);
}

NcAtt::~NcAtt( void )
{
    delete [] the_name;
}

NcToken NcAtt::name( void ) const
{
    return the_name;
}

NcType NcAtt::type( void ) const
{
    nc_type typ;
    ncattinq(the_file->id(), the_variable->id(), the_name, &typ, 0);
    return (NcType) typ;
}

long NcAtt::num_vals( void ) const
{
    int len;
    ncattinq(the_file->id(), the_variable->id(), the_name, 0, &len);
    return len;
}

NcBool NcAtt::is_valid( void ) const
{
    return the_file->is_valid() &&
      (the_variable->id() == NC_GLOBAL || the_variable->is_valid()) &&
	ncattinq(the_file->id(), the_variable->id(), the_name, 0, 0) != ncBad;
}

NcValues* NcAtt::values( void ) const
{
    NcValues* valp = get_space();
    if (ncattget(the_file->id(),
		 the_variable->id(),
		 the_name,
		 valp->base()) == ncBad) {
	delete valp;
	return 0;
    }
	
    return valp;
}

NcBool NcAtt::rename(NcToken newname)
{
    if (strlen(newname) > strlen(the_name)) {
	if (! the_file->define_mode())
	    return FALSE;
    }
    return ncattrename(the_file->id(), the_variable->id(),
		       the_name, newname) != ncBad;
}

NcBool NcAtt::remove( void )
{
    if (! the_file->define_mode())
	return FALSE;
    return ncattdel(the_file->id(), the_variable->id(), the_name) != ncBad;
}

NcError::NcError( Behavior b )
{
    the_old_state = ncopts;	// global variable in C interface
    the_old_err = ncerr;	// global variable in C interface
    ncopts = (int) b;
}

NcError::~NcError( void )
{
    ncopts = the_old_state;
    ncerr = the_old_err;
}

int NcError::get_err( void )	// returns most recent error
{
    return ncerr;
}
