#---------------------------------------------------------------------------------
# Description:
#    Compile f90 code that generates grib extreme locations.
#    
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Version: June 27, 2014, DXu, Initial coding
#
#----------------------------------------------
# Compile option
FC=/opt/intel/composer_xe_2011_sp1.10.319/bin/intel64/ifort
CFLAGS = -O -g -traceback -C -integer-size 64 -real-size 64 -assume byterecl -free

# Link option
LD=$(FC)

# Clean tool
RM = rm

EXE = gribExtreme.x
FSRC = gribdifs.f90 grib_dist.f90 read_grib.f90 rdgrib.f90  
FOBJ = ${FSRC:.f90=.o}

# Clear out all suffixes
.SUFFIXES:
# List only those we use
.SUFFIXES: .o .f90

# Define a suffix rule for .f90 -> .o
.f90.o:
	$(FC) $(CFLAGS) -c $<

#
LIBS= -L/usr/local/jcsda/nwprod_v2012/lib  -lbacio_8  -L/usr/local/jcsda/nwprod_gdas_2014/lib  -lw3emc_8 -lw3nco_8

#
$(EXE): $(FOBJ)
	$(LD) -z muldefs -o $@ $(CFLAGS) $(FOBJ) $(LIBS)

clean:
	$(RM) -f $(EXE)  *.o *.mod


