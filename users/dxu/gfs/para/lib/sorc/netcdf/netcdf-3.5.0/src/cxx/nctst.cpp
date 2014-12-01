#include <iostream.h>
#include <string.h>
#include "netcdfcpp.h"

void gen(const char* path)		// Generate a netCDF file
{
    NcFile nc(path, NcFile::Replace); // Create, leave in define mode

    // Check if the file was opened successfully
    if (! nc.is_valid()) {
	cerr << "can't create netCDF file " << path << "\n";
	return;
    }

    // Create dimensions
    const int NLATS = 4;
    const int NLONS = 3;
    const int NFRTIMES = 2;
    const int TIMESTRINGLEN = 20;
    NcDim* latd = nc.add_dim("lat", NLATS);
    NcDim* lond = nc.add_dim("lon", NLONS);
    NcDim* frtimed = nc.add_dim("frtime"); // unlimited dimension
    NcDim* timelend = nc.add_dim("timelen", TIMESTRINGLEN); 

    // Create variables and their attributes
    NcVar* P = nc.add_var("P", ncFloat, frtimed, latd, lond);
    P->add_att("long_name", "pressure at maximum wind");
    P->add_att("units", "hectopascals");
    static float range[] = {0., 1500.};
    P->add_att("valid_range", 2, range);
    P->add_att("_FillValue", -9999.0f);

    NcVar* lat = nc.add_var("lat", ncFloat, latd);
    lat->add_att("long_name", "latitude");
    lat->add_att("units", "degrees_north");

    NcVar* lon = nc.add_var("lon", ncFloat, lond);
    lon->add_att("long_name", "longitude");
    lon->add_att("units", "degrees_east");

    NcVar* frtime = nc.add_var("frtime", ncLong, frtimed);
    frtime->add_att("long_name", "forecast time");
    frtime->add_att("units", "hours");

    NcVar* reftime = nc.add_var("reftime",ncChar,timelend);
    reftime->add_att("long_name", "reference time");
    reftime->add_att("units", "text_time");

    NcVar* scalar = nc.add_var("scalarv", ncInt);
    scalar->add_att("scalar_att", 1);

    // Global attributes
    nc.add_att("history", "created by Unidata LDM from NPS broadcast");
    nc.add_att("title", "NMC Global Product Set: Pressure at Maximum Wind");

    // Start writing data, implictly leaves define mode

    static float lats[NLATS] = {-90, -87.5, -85, -82.5};
    lat->put(lats, NLATS);

    static float lons[NLONS] = {-180, -175, -170};
    lon->put(lons, NLONS);

    static int frtimes[NFRTIMES] = {12, 18};
    frtime->put(frtimes, NFRTIMES);

    static const char* s = "1992-3-21 12:00" ;
    reftime->put(s, strlen(s));

    static float P_data[2][4][3] = {
	{{950, 951, 952}, {953, 954, 955}, {956, 957, 958}, {959, 960, 961}},
	{{962, 963, 964}, {965, 966, 967}, {968, 969, 970}, {971, 972, 973}}
      };
    // We could write all P data at once with P->put(&P_data[0][0][0], P->edges()),
    // but instead we write one record at a time, to show use of setcur().
    long rec = 0;                                      // start at zero-th
    const long nrecs = 1;		               // # records to write
    P->put(&P_data[0][0][0], nrecs, NLATS, NLONS);           // write zero-th record
    P->set_cur(++rec);		                       // set to next record
    P->put(&P_data[1][0][0], nrecs, NLATS, NLONS); // write next record

    // close of nc takes place in destructor
}


/*
 * Convert pathname of netcdf file into name for CDL, by taking last component
 * of path and stripping off any extension.  The returned string is in static
 * storage, so copy it if you need to keep it.
 */
static char* 
cdl_name(const char* path)
{
    const char* cp = path + strlen(path);
    while (*(cp-1) != '/' && cp != path) // assumes UNIX path separator
	cp--;

    static char np[MAX_NC_NAME];
    strncpy(&np[0], cp, MAX_NC_NAME);

    char* ep = np + strlen(np);
    while (*ep != '.' && ep != np)
	ep--;
    if (*ep == '.')
      *ep = '\0';
    return np;
}

// A derived class, just like NcFile except knows how to "dump" its
// dimensions, variables, global attributes, and data in ASCII form.
class DumpableNcFile : public NcFile
{
  public:
    DumpableNcFile(const char* path, NcFile::FileMode mode = ReadOnly)
	: NcFile(path, mode) {} ;
    void dumpdims( void );
    void dumpvars( void );
    void dumpgatts( void );
    void dumpdata( void );
};

void DumpableNcFile::dumpdims( void )
{

    for (int n=0; n < num_dims(); n++) {
	NcDim* dim = get_dim(n);
	cout << "\t" << dim->name() << " = " ;
	if (dim->is_unlimited())
	  cout << "UNLIMITED" << " ;\t " << "// " << dim->size() <<
	    " currently\n";
	else
	  cout << dim->size() << " ;\n";
    }
}

void dumpatts(NcVar& var)
{
    NcToken vname = var.name();
    NcAtt* ap;
    for(int n = 0; ap = var.get_att(n); n++) {
	cout << "\t\t" << vname << ":" << ap->name() << " = " ;
	NcValues* vals = ap->values();
	cout << *vals << " ;" << endl ;
	delete ap;
	delete vals;
    }
}

void DumpableNcFile::dumpvars( void )
{
    int n;
    static const char* types[] =
      {"","byte","char","short","long","float","double"};
    NcVar* vp;

    for(n = 0; vp = get_var(n); n++) {
	cout << "\t" << types[vp->type()] << " " << vp->name() ;

	if (vp->num_dims() > 0) {
	    cout << "(";
	    for (int d = 0; d < vp->num_dims(); d++) {
		NcDim* dim = vp->get_dim(d);
		cout << dim->name();
		if (d < vp->num_dims()-1)
		  cout << ", ";		  
	    }
	    cout << ")";
	}
	cout << " ;\n";
	// now dump each of this variable's attributes
	dumpatts(*vp);
    }
}

void DumpableNcFile::dumpgatts( void )
{
    NcAtt* ap;
    for(int n = 0; ap = get_att(n); n++) {
	cout << "\t\t" << ":" << ap->name() << " = " ;
	NcValues* vals = ap->values();
	cout << *vals << " ;" << endl ;
	delete vals;
	delete ap;
    }
}

void DumpableNcFile::dumpdata( )
{
    NcVar* vp;
    for (int n = 0; vp = get_var(n); n++) {
	cout << " " << vp->name() << " = ";
	NcValues* vals = vp->values();
	cout << *vals << " ;" << endl ;
	delete vals;
    }
}

void dump(const char* path)
{
    DumpableNcFile nc(path);	// default is open in read-only mode

    cout << "netcdf " << cdl_name(path) << " {" << endl <<
	    "dimensions:" << endl ;

    nc.dumpdims();

    cout << "variables:" << endl;

    nc.dumpvars();

    if (nc.num_atts() > 0)
      cout << "// global attributes" << endl ;

    nc.dumpgatts();

    cout << "data:" << endl;

    nc.dumpdata();

    cout << "}" << endl;
}


int
main( void )	// test new netCDF interface
{
    const char* path = "example.nc";

    gen(path);		// create a netCDF file
    dump(path);		// print what's in it

    return 0;
}
