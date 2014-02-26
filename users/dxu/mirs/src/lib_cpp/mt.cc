/*
 * implementation for mt.h to match fortran type MeasurData_type ( IO_MeasurData.f90 )
 * 
 * 05/18/2012       Wanchun Chen
 *
 */

#include "mt.h"


unsigned short *get_data_ushort_1d_from_group(hid_t group_id, char *dsetname, int &nscan_return ) {

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    //H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    //int index;
    
    dsetr_id = H5Dopen1(group_id, dsetname);
   
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    
    // data type id
    type_id = H5Dget_type( dsetr_id );
    if( DEBUG_FLAG == 1 ) cout << "type_id=" << type_id << endl;

/*    
    // get scale factor
    float scale = 1.0;
    hid_t attr_id = H5Aopen_name( dsetr_id, "scale_factor" );
    if( attr_id >= 0 ) {
      char scale_factor[32];
      if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
      hid_t type_scale = H5Aget_type(attr_id);
      if( DEBUG_FLAG == 1 ) cout << "type_scale=" << type_scale << endl;
      herr_t  ret = H5Aread( attr_id, type_scale, scale_factor );
      if( DEBUG_FLAG == 1 ) cout << "scale_factor=" << scale_factor << endl;    
      H5Aclose(attr_id);   
      scale = atof(scale_factor);
    }
    if( DEBUG_FLAG == 1 ) cout << "scale=" << scale << endl;
    
    // get offset
    float offset = 0.0;
    attr_id = H5Aopen_name( dsetr_id, "add_offset" );
    if( attr_id >= 0 ) { 
      char add_offset[32];
      if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
      hid_t type_offset = H5Aget_type(attr_id);
      if( DEBUG_FLAG == 1 ) cout << "type_offset=" << type_offset << endl;
      herr_t ret = H5Aread( attr_id, type_offset, add_offset );
      if( DEBUG_FLAG == 1 ) cout << "add_offset=" << add_offset << endl;    
      H5Aclose(attr_id);   
      offset = atof(add_offset);
    }
    if( DEBUG_FLAG == 1 ) cout << "offset=" << offset << endl;
*/
    
    // number of dimensions
    int ndims = H5Sget_simple_extent_ndims(space_id);
    if( DEBUG_FLAG == 1 ) cout << "ndims=" << ndims << endl;
    // each dimension size
    hsize_t *dims = new hsize_t[ndims];
    hsize_t *maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int nscan = dims[0];

    storage_size = H5Dget_storage_size( dsetr_id );
    unsigned short *out_values = new unsigned short[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_USHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_values);

    if( DEBUG_FLAG == 1 ) cout << "out_values[0]=" << out_values[0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "out_values[1]=" << out_values[1] << endl;
    
    nscan_return = nscan;
    
    delete [] dims;
    delete [] maxdims;
    
    dims = NULL;
    maxdims = NULL;
    
    H5Dclose(dsetr_id);

    return out_values;
}


float* float_get_data_ushort_1d_from_group(hid_t group_id, char *dsetname, int &nscan_return ) {

    float *values;

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    //H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    //int index;
    
    dsetr_id = H5Dopen1(group_id, dsetname);
   
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    
    // data type id
    type_id = H5Dget_type( dsetr_id );
    if( DEBUG_FLAG == 1 ) cout << "type_id=" << type_id << endl;
    
    // get scale factor
    float scale = 1.0;
    hid_t attr_id = H5Aopen_name( dsetr_id, "scale_factor" );
    if( attr_id >= 0 ) {
      char scale_factor[32];
      if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
      hid_t type_scale = H5Aget_type(attr_id);
      if( DEBUG_FLAG == 1 ) cout << "type_scale=" << type_scale << endl;
      //herr_t ret = H5Aread( attr_id, type_scale, scale_factor );
      H5Aread( attr_id, type_scale, scale_factor );
      if( DEBUG_FLAG == 1 ) cout << "scale_factor=" << scale_factor << endl;    
      H5Aclose(attr_id);   
      scale = atof(scale_factor);
    }
    if( DEBUG_FLAG == 1 ) cout << "scale=" << scale << endl;
    
    // get offset
    float offset = 0.0;
    attr_id = H5Aopen_name( dsetr_id, "add_offset" );
    if( attr_id >= 0 ) { 
      char add_offset[32];
      if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
      hid_t type_offset = H5Aget_type(attr_id);
      if( DEBUG_FLAG == 1 ) cout << "type_offset=" << type_offset << endl;
      //hid_t ret = H5Aread( attr_id, type_offset, add_offset );
      H5Aread( attr_id, type_offset, add_offset );
      if( DEBUG_FLAG == 1 ) cout << "add_offset=" << add_offset << endl;    
      H5Aclose(attr_id);   
      offset = atof(add_offset);
    }
    if( DEBUG_FLAG == 1 ) cout << "offset=" << offset << endl;
    
    // number of dimensions
    int ndims = H5Sget_simple_extent_ndims(space_id);
    if( DEBUG_FLAG == 1 ) cout << "ndims=" << ndims << endl;
    // each dimension size
    hsize_t *dims = new hsize_t[ndims];
    hsize_t *maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int nscan = dims[0];

    storage_size = H5Dget_storage_size( dsetr_id );
    unsigned short *out_values = new unsigned short[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_USHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_values);

    if( DEBUG_FLAG == 1 ) cout << "out_values[0]=" << out_values[0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "out_values[1]=" << out_values[1] << endl;
    
    // convert unsigned short into signed float
    values = new float[nscan];
    for( int iscan=0; iscan<nscan; iscan++ ) {
      values[iscan] = out_values[iscan] * scale + offset;
    }
    
    nscan_return = nscan;
    
    if( DEBUG_FLAG == 1 ) cout << "values[0]=" << values[0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "values[1]=" << values[1] << endl;
    
    delete [] out_values;
    delete [] dims;
    delete [] maxdims;
    
    out_values = NULL;
    dims = NULL;
    maxdims = NULL;
    
    H5Dclose(dsetr_id);

    return values;
}



float** get_data_ushort_2d_from_group(hid_t group_id, char *dsetname, int &nscan_return, int &nfov_return ) {

    float **values;

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    //H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    int index;
    
    dsetr_id = H5Dopen1(group_id, dsetname);
   
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    
    // data type id
    type_id = H5Dget_type( dsetr_id );
    if( DEBUG_FLAG == 1 ) cout << "type_id=" << type_id << endl;
    
    // get scale factor
    char scale_factor[32];
    hid_t attr_id = H5Aopen_name( dsetr_id, "scale_factor" );
    if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
    hid_t type_scale = H5Aget_type(attr_id);
    if( DEBUG_FLAG == 1 ) cout << "type_scale=" << type_scale << endl;
    herr_t  ret = H5Aread( attr_id, type_scale, scale_factor );
    if( DEBUG_FLAG == 1 ) cout << "scale_factor=" << scale_factor << endl;    
    H5Aclose(attr_id);   
    float scale = atof(scale_factor);
    if( DEBUG_FLAG == 1 ) cout << "scale=" << scale << endl;
    
    // get offset
    char add_offset[32];
    attr_id = H5Aopen_name( dsetr_id, "add_offset" );
    if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
    hid_t type_offset = H5Aget_type(attr_id);
    if( DEBUG_FLAG == 1 ) cout << "type_offset=" << type_offset << endl;
    ret = H5Aread( attr_id, type_offset, add_offset );
    if( DEBUG_FLAG == 1 ) cout << "add_offset=" << add_offset << endl;    
    H5Aclose(attr_id);   
    float offset = atof(add_offset);
    if( DEBUG_FLAG == 1 ) cout << "offset=" << offset << endl;
    
    // number of dimensions
    int ndims = H5Sget_simple_extent_ndims(space_id);
    // each dimension size
    hsize_t *dims = new hsize_t[ndims];
    hsize_t *maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int nscan = dims[0];
    int nfov  = dims[1];

    storage_size = H5Dget_storage_size( dsetr_id );
    unsigned short *out_value = new unsigned short[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_USHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_value);

    // row major array,
    // offset = row*NUMCOLS + column
    unsigned short values_ushort[nscan][nfov];
    for( int iscan=0; iscan<nscan; iscan++ ) {
      for(int ifov=0; ifov<nfov; ifov++ ) {
        index = iscan * nfov + ifov;
        values_ushort[iscan][ifov] = out_value[index];
      }
    }
    
    delete [] out_value;
    delete [] dims;
    delete [] maxdims;
    
    out_value = NULL;
    dims = NULL;
    maxdims = NULL;
    
    H5Dclose(dsetr_id);

    if( DEBUG_FLAG == 1 ) cout << "values_ushort[0][0]=" << values_ushort[0][0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "values_ushort[0][1]=" << values_ushort[0][1] << endl;
    
    // convert unsigned short into signed float
    values = new float *[nscan];
    for( int iscan=0; iscan<nscan; iscan++ ) {
      values[iscan] = new float[nfov];
      for(int ifov=0; ifov<nfov; ifov++ ) {
        values[iscan][ifov] = values_ushort[iscan][ifov] * scale + offset;
      }
    }
    
    nscan_return = nscan;
    nfov_return = nfov;
    
    if( DEBUG_FLAG == 1 ) cout << "values[0][0]=" << values[0][0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "values[0][1]=" << values[0][1] << endl;
    
    return values;
}


float** get_data_short_2d_from_group(hid_t group_id, char *dsetname, int &nscan_return, int &nfov_return ) {

    float **values;

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    //H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    int index;
    
    dsetr_id = H5Dopen1(group_id, dsetname);
   
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    
    // data type id
    type_id = H5Dget_type( dsetr_id );
    if( DEBUG_FLAG == 1 ) cout << "type_id=" << type_id << endl;
    
    // get scale factor
    char scale_factor[32];
    hid_t attr_id = H5Aopen_name( dsetr_id, "scale_factor" );
    if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
    hid_t type_scale = H5Aget_type(attr_id);
    if( DEBUG_FLAG == 1 ) cout << "type_scale=" << type_scale << endl;
    //herr_t  ret = H5Aread( attr_id, type_scale, scale_factor );
    H5Aread( attr_id, type_scale, scale_factor );
    if( DEBUG_FLAG == 1 ) cout << "scale_factor=" << scale_factor << endl;    
    H5Aclose(attr_id);   
    float scale = atof(scale_factor);
    if( DEBUG_FLAG == 1 ) cout << "scale=" << scale << endl;
    
    /** 
    // get offset
    char add_offset[32];
    attr_id = H5Aopen_name( dsetr_id, "add_offset" );
    if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
    hid_t type_offset = H5Aget_type(attr_id);
    if( DEBUG_FLAG == 1 ) cout << "type_offset=" << type_offset << endl;
    ret = H5Aread( attr_id, type_offset, add_offset );
    if( DEBUG_FLAG == 1 ) cout << "add_offset=" << add_offset << endl;    
    H5Aclose(attr_id);   
    float offset = atof(add_offset);
    if( DEBUG_FLAG == 1 ) cout << "offset=" << offset << endl;
    */
    
    float offset = 0.0;
    
    // number of dimensions
    int ndims = H5Sget_simple_extent_ndims(space_id);
    // each dimension size
    hsize_t *dims = new hsize_t[ndims];
    hsize_t *maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int nscan = dims[0];
    int nfov  = dims[1];

    storage_size = H5Dget_storage_size( dsetr_id );
    short *out_value = new short[storage_size/2];
    status = H5Dread(dsetr_id, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_value);

    // row major array,
    // offset = row*NUMCOLS + column
    short values_short[nscan][nfov];
    for( int iscan=0; iscan<nscan; iscan++ ) {
      for(int ifov=0; ifov<nfov; ifov++ ) {
        index = iscan * nfov + ifov;
        values_short[iscan][ifov] = out_value[index];
      }
    }
    
    delete [] out_value;
    delete [] dims;
    delete [] maxdims;
    
    out_value = NULL;
    dims = NULL;
    maxdims = NULL;
    
    H5Dclose(dsetr_id);

    if( DEBUG_FLAG == 1 ) cout << "values_short[0][0]=" << values_short[0][0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "values_short[0][1]=" << values_short[0][1] << endl;
    
    // convert unsigned short into signed float
    values = new float *[nscan];
    for( int iscan=0; iscan<nscan; iscan++ ) {
      values[iscan] = new float[nfov];
      for(int ifov=0; ifov<nfov; ifov++ ) {
        values[iscan][ifov] = values_short[iscan][ifov] * scale + offset;
      }
    }
    
    nscan_return = nscan;
    nfov_return = nfov;
    
    if( DEBUG_FLAG == 1 ) cout << "values[0][0]=" << values[0][0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "values[0][1]=" << values[0][1] << endl;
    
    return values;
}



float** get_data_float_2d_from_group(hid_t group_id, char *dsetname, int &nscan_return, int &nfov_return ) {

    float **values;

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    //H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    int index;
    
    dsetr_id = H5Dopen1(group_id, dsetname);
   
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    
    // data type id
    type_id = H5Dget_type( dsetr_id );
    if( DEBUG_FLAG == 1 ) cout << "type_id=" << type_id << endl;
    
    // get scale factor
    char scale_factor[32];
    hid_t attr_id = H5Aopen_name( dsetr_id, "scale_factor" );
    if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
    hid_t type_scale = H5Aget_type(attr_id);
    if( DEBUG_FLAG == 1 ) cout << "type_scale=" << type_scale << endl;
    herr_t  ret = H5Aread( attr_id, type_scale, scale_factor );
    if( DEBUG_FLAG == 1 ) cout << "scale_factor=" << scale_factor << endl;    
    H5Aclose(attr_id);   
    float scale = atof(scale_factor);
    if( DEBUG_FLAG == 1 ) cout << "scale=" << scale << endl;
    
    // get offset
    char add_offset[32];
    attr_id = H5Aopen_name( dsetr_id, "add_offset" );
    if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
    hid_t type_offset = H5Aget_type(attr_id);
    if( DEBUG_FLAG == 1 ) cout << "type_offset=" << type_offset << endl;
    ret = H5Aread( attr_id, type_offset, add_offset );
    if( DEBUG_FLAG == 1 ) cout << "add_offset=" << add_offset << endl;    
    H5Aclose(attr_id);   
    float offset = atof(add_offset);
    if( DEBUG_FLAG == 1 ) cout << "offset=" << offset << endl;
    
    // number of dimensions
    int ndims = H5Sget_simple_extent_ndims(space_id);
    // each dimension size
    hsize_t *dims = new hsize_t[ndims];
    hsize_t *maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int nscan = dims[0];
    int nfov  = dims[1];

    storage_size = H5Dget_storage_size( dsetr_id );
    float *out_value = new float[storage_size/4];
    status = H5Dread(dsetr_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_value);

    // row major array,
    // offset = row*NUMCOLS + column
    float values_float[nscan][nfov];
    for( int iscan=0; iscan<nscan; iscan++ ) {
      for(int ifov=0; ifov<nfov; ifov++ ) {
        index = iscan * nfov + ifov;
        values_float[iscan][ifov] = out_value[index];
      }
    }
    
    delete [] out_value;
    delete [] dims;
    delete [] maxdims;
    
    out_value = NULL;
    dims = NULL;
    maxdims = NULL;
    
    H5Dclose(dsetr_id);

    if( DEBUG_FLAG == 1 ) cout << "values_float[0][0]=" << values_float[0][0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "values_float[0][1]=" << values_float[0][1] << endl;
    
    // apply scale factor and offset
    values = new float *[nscan];
    for( int iscan=0; iscan<nscan; iscan++ ) {
      values[iscan] = new float[nfov];
      for(int ifov=0; ifov<nfov; ifov++ ) {
        values[iscan][ifov] = values_float[iscan][ifov] * scale + offset;
      }
    }
    
    nscan_return = nscan;
    nfov_return = nfov;
    
    if( DEBUG_FLAG == 1 ) cout << "values[0][0]=" << values[0][0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "values[0][1]=" << values[0][1] << endl;
    
    return values;
}


// nfov_return is each string element length here, NOT normal sense of NFOV
char ** get_data_string_2d_from_group(hid_t group_id, char *dsetname, int &nscan_return, int &nfov_return ) {

    char **values;

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    //H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    //int index;
    
    dsetr_id = H5Dopen1(group_id, dsetname);
   
    // get space id
    space_id = H5Dget_space( dsetr_id );
    
    // data type id
    type_id = H5Dget_type( dsetr_id );
    if( DEBUG_FLAG == 1 ) cout << "type_id=" << type_id << endl;
    
    // get data type size
    size_t data_size = H5Tget_size(type_id);
    if( DEBUG_FLAG == 1 ) cout << "data_size=" << data_size << endl;
    data_size++ ;  // make room for null terminator
    
    // number of dimensions
    int ndims = H5Sget_simple_extent_ndims(space_id);
    if( DEBUG_FLAG == 1 ) cout << "ndims=" << ndims << endl;
    // each dimension size
    hsize_t *dims = new hsize_t[ndims];
    hsize_t *maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int nscan = dims[1];
    //int nfov  = dims[0];
    if( DEBUG_FLAG == 1 ) cout << "dims[0]=" << dims[0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "dims[1]=" << dims[1] << endl;
    
    storage_size = H5Dget_storage_size( dsetr_id );
    if( DEBUG_FLAG == 1 ) cout << "storage_size=" << storage_size << endl;
    
    
    // This is so weird to read string array in HDF5 !!!!
    hid_t memtype = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(memtype,data_size);
    
    // the 2 are C way
    //values = (char **) malloc(nscan * sizeof(char *));
    //values[0] = (char *) malloc(nscan * data_size * sizeof(char));
    
    // the 2 are C++ Way, C++ way is preferred over C way of memory allocation
    values = new char *[nscan];
    values[0] = new char[nscan * data_size];
    
    // set the rest of pointers to rows of the correct addresses
    for( int iscan=1; iscan < nscan; iscan++ )
      values[iscan] = values[0] + iscan * data_size;
      
    // read the data
    status = H5Dread(dsetr_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, values[0]);
    if( DEBUG_FLAG == 1 ) {
      cout << "status=" << status << endl;
      cout << "values[0]=" << values[0] << endl;
      cout << "values[1]=" << values[1] << endl;
    }
    
    nscan_return = nscan;
    nfov_return = data_size-1;
    
    delete [] dims;
    delete [] maxdims;
    
    dims = NULL;
    maxdims = NULL;
    
    H5Dclose(dsetr_id);

    return values;
}



float** get_data_uchar_2d_from_group(hid_t group_id, char *dsetname, int &nscan_return, int &nfov_return ) {

    float **values;

    hid_t dsetr_id;
    hid_t space_id;
    hid_t type_id;
    //H5T_class_t class_id;
    hsize_t storage_size;
    int status; 
    int index;
    
    dsetr_id = H5Dopen1(group_id, dsetname);
   
    // get space id for antenna temperature
    space_id = H5Dget_space( dsetr_id );
    
    // data type id
    type_id = H5Dget_type( dsetr_id );
    if( DEBUG_FLAG == 1 ) cout << "type_id=" << type_id << endl;
    
    // get scale factor
    char scale_factor[32];
    hid_t attr_id = H5Aopen_name( dsetr_id, "scale_factor" );
    if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
    hid_t type_scale = H5Aget_type(attr_id);
    if( DEBUG_FLAG == 1 ) cout << "type_scale=" << type_scale << endl;
    herr_t  ret = H5Aread( attr_id, type_scale, scale_factor );
    if( DEBUG_FLAG == 1 ) cout << "scale_factor=" << scale_factor << endl;    
    H5Aclose(attr_id);   
    float scale = atof(scale_factor);
    if( DEBUG_FLAG == 1 ) cout << "scale=" << scale << endl;
    
    // get offset
    char add_offset[32];
    attr_id = H5Aopen_name( dsetr_id, "add_offset" );
    if( DEBUG_FLAG == 1 ) cout << "attr_id=" << attr_id << endl;
    hid_t type_offset = H5Aget_type(attr_id);
    if( DEBUG_FLAG == 1 ) cout << "type_offset=" << type_offset << endl;
    ret = H5Aread( attr_id, type_offset, add_offset );
    if( DEBUG_FLAG == 1 ) cout << "add_offset=" << add_offset << endl;    
    H5Aclose(attr_id);   
    float offset = atof(add_offset);
    if( DEBUG_FLAG == 1 ) cout << "offset=" << offset << endl;
	
    // number of dimensions
    int ndims = H5Sget_simple_extent_ndims(space_id);
    // each dimension size
    hsize_t *dims = new hsize_t[ndims];
    hsize_t *maxdims = new hsize_t[ndims];
    status = H5Sget_simple_extent_dims(space_id, dims, maxdims );
    int nscan = dims[0];
    int nfov  = dims[1];

    storage_size = H5Dget_storage_size( dsetr_id );
    char *out_value = new char[storage_size];
    status = H5Dread(dsetr_id, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, out_value);

    // row major array,
    // offset = row*NUMCOLS + column
    float values_uchar[nscan][nfov];
    for( int iscan=0; iscan<nscan; iscan++ ) {
      for(int ifov=0; ifov<nfov; ifov++ ) {
        index = iscan * nfov + ifov;
        values_uchar[iscan][ifov] = out_value[index];
      }
    }
    
    delete [] out_value;
    delete [] dims;
    delete [] maxdims;
    
    out_value = NULL;
    dims = NULL;
    maxdims = NULL;
    
    H5Dclose(dsetr_id);

    if( DEBUG_FLAG == 1 ) cout << "values_uchar[0][0]=" << values_uchar[0][0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "values_uchar[0][1]=" << values_uchar[0][1] << endl;
    
    // convert unsigned short into signed char
    values = new float *[nscan];
    for( int iscan=0; iscan<nscan; iscan++ ) {
	  values[iscan] = new float[nfov];
	  for(int ifov=0; ifov<nfov; ifov++ ) {
        values[iscan][ifov] = values_uchar[iscan][ifov] * scale + offset;
      }
    }
    
    nscan_return = nscan;
    nfov_return = nfov;
	
    if( DEBUG_FLAG == 1 ) cout << "values[0][0]=" << values[0][0] << endl;
    if( DEBUG_FLAG == 1 ) cout << "values[0][1]=" << values[0][1] << endl;
	
    return values;
}


/***************************************************************************************************
 *
 * Help function to get day of the year 1-365/366, given year, month and day of month
 * 
 *  input arguments:
 *        - year  ( 4 digit year )
 *        - month ( 1 - 12 )
 *        - day ( 1-31 )
 *
 *  normal return : a int in the range of 1-366 for leap year, 1-365 for non-leap year
 *
 *  error return: -1
 *
 ***************************************************************************************************/
int getJday( int year, int month, int day ) {
  if( year < 0 || month < 1 || month > 12 || day < 1 || day > 31 ) return -1;
  int JulianDate1[12] = {0,31,60,91,121,152,182,213,244,274,305,335};
  int JulianDate2[12] = {0,31,59,90,120,151,181,212,243,273,304,334};
  int leap = 0;
  if( ( year % 4 == 0 && year % 100 != 0 ) || ( year % 400 == 0 ) ) leap = 1;
  int jday = -1;
  if( leap == 1 )
    jday = JulianDate1[month-1]+day;
  else
    jday = JulianDate2[month-1]+day;  
  
  return jday;
}
 


/**
 * return 1 if little endian machine,
 * return 0 if big endian machine.
 */
int getEndian()
{
    union {
        int theInteger;
        char theByte;
    } endianUnion;

    endianUnion.theInteger = 1 ;
    return endianUnion.theByte;
}


int intSwap(char *value)
{
  char buffer[ 4 ];

  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 3 ];
    buffer[ 1 ] = value[ 2 ];
    buffer[ 2 ] = value[ 1 ];
    buffer[ 3 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
    buffer[ 2 ] = value[ 2 ];
    buffer[ 3 ] = value[ 3 ];
  }

  return *( (int *) &buffer );
}


short int shortSwap(char *value)
{
  char buffer[ 2 ];
  
  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 1 ];
    buffer[ 1 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
  }
  return *( (short int *) &buffer );
}


float floatSwap(char *value)
{
  char buffer[ 4 ];

  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 3 ];
    buffer[ 1 ] = value[ 2 ];
    buffer[ 2 ] = value[ 1 ];
    buffer[ 3 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
    buffer[ 2 ] = value[ 2 ];   
    buffer[ 3 ] = value[ 3 ];
  }

  return *( (float *) &buffer );
}


long long longlongSwap(char *value)
{
  char buffer[ 8 ];
  
  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 7 ];
    buffer[ 1 ] = value[ 6 ];
    buffer[ 2 ] = value[ 5 ];
    buffer[ 3 ] = value[ 4 ];
    buffer[ 4 ] = value[ 3 ];
    buffer[ 5 ] = value[ 2 ];
    buffer[ 6 ] = value[ 1 ];
    buffer[ 7 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
    buffer[ 2 ] = value[ 2 ];
    buffer[ 3 ] = value[ 3 ];
    buffer[ 4 ] = value[ 4 ];
    buffer[ 5 ] = value[ 5 ];
    buffer[ 6 ] = value[ 6 ];
    buffer[ 7 ] = value[ 7 ];
  }
  return *( (long long *) &buffer );
}


double doubleSwap(char *value)
{
  char buffer[ 8 ];

  if( getEndian() == 1 ) {
    buffer[ 0 ] = value[ 7 ];
    buffer[ 1 ] = value[ 6 ];
    buffer[ 2 ] = value[ 5 ];
    buffer[ 3 ] = value[ 4 ];
    buffer[ 4 ] = value[ 3 ];
    buffer[ 5 ] = value[ 2 ];
    buffer[ 6 ] = value[ 1 ];
    buffer[ 7 ] = value[ 0 ];
  }
  else {
    buffer[ 0 ] = value[ 0 ];
    buffer[ 1 ] = value[ 1 ];
    buffer[ 2 ] = value[ 2 ];
    buffer[ 3 ] = value[ 3 ];
    buffer[ 4 ] = value[ 4 ];
    buffer[ 5 ] = value[ 5 ];
    buffer[ 6 ] = value[ 6 ];
    buffer[ 7 ] = value[ 7 ];
  }
  return *( (double *) &buffer );
}


int BSWAP32(int x)  {
  
  int y =  ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) | 
      	   ( ((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))  ;
  return y;

}


/**
 * Function takes the decimal number
 * Function takes the Nth bit (1 to 31)
 * Return the value of Nth bit from decimal
 */
int get_bit(unsigned short decimal, int N){
 
  // Shifting the 1 for N-1 bits
  int constant = 1 << (N-1);
 
  // if the bit is set, return 1
  if( decimal & constant ){
   return 1;
  }
 
  // If the bit is not set, return 0
  return 0;
}


int writeMeasurementHdr(ofstream &myFile, MeasurementTypeHeader& header) {

    myFile.width(4); myFile << header.nscan ;
    myFile.width(4); myFile << header.nfov;
    myFile.width(4); myFile << header.nqc ;
    myFile.width(4); myFile << header.nchan << '\n';
    
    for( int ichan=0; ichan<header.nchan; ichan++ )
    {
       myFile << setprecision(5) << setw(10) << right << fixed << header.freq[ichan] ;
       if ( (((ichan+1) % 10) == 0) && ((ichan+1) < header.nchan) ) myFile << endl;
    }
    myFile << endl;
    
    for( int ichan=0; ichan<header.nchan; ichan++ )
    {
       myFile << setw(3) << header.polar[ichan] ;
       if ( (((ichan+1) % 20) == 0) && ((ichan+1) < header.nchan) ) myFile << endl;
    }
    myFile << endl;
    
    return 0;

}



int writeMeasurement(ofstream &myFile, MeasurementType& measurement) {
    
    int NFOV  = measurement.nfov;
    int NCHAN = measurement.nchan;
    int NQC   = measurement.nqc;

    myFile.width(4);  myFile << measurement.node ;
    myFile.width(10); myFile << measurement.jday;
    myFile.width(10); myFile << measurement.year;
    myFile.width(10); myFile << measurement.secs << '\n';

    // lat
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.lat[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;

    // lon
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.lon[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;

    // angle - Sensor Zenith Angle
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.angle[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;
    
    // relAziAngle
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.relAziAngle[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;
   
    // solZenAngle
    for(int ifov=0;ifov<NFOV;ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.solZenAngle[ifov] ;
    	if ( (((ifov+1) % 10) == 0) && ((ifov+1) < NFOV) ) myFile << endl;
    }
    myFile << endl;

    
    // !!!!  C/C++ is row major, while fortran/IDL is column major !!!!
    // !!!!  We write in Fortran/IDL order : row(ifov) changes faster than column(ichan)
    
    // tb[nfov][nchan]
    for(int ichan=0; ichan<NCHAN; ichan++) {
    for(int ifov=0;  ifov<NFOV; ifov++) {
    	myFile << setprecision(2) << setw(8) << fixed << measurement.tb[ifov][ichan];
	
	// C/C++ order in memory
        // int index = ichan + ifov * NCHAN ;   
	
	// Fortran/IDL order in memory
	int index = ichan * NFOV + ifov;

    	if ( (((index+1) % 10) == 0) && ( (index+1) < (NCHAN*NFOV) ) ) myFile << endl;
    }
    }
    myFile << endl;

    // qc 
    if ( NQC > 0 ) {
      for(int iqc=0; iqc<NQC; iqc++ ) {
    	myFile.width(4); myFile << measurement.qc[iqc];
    	if ( (((iqc+1) % 10) == 0) && ((iqc+1) < NQC) ) myFile << endl;
      }
    }
    myFile << endl;

    return 0;
}


