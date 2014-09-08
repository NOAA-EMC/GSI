
#-------------------------------------------------------------------------
# Makefile for the nhcver program.
# NHC, Jim Gross  05/07/13
#
#-------------------------------------------------------------------------

OBJS      = nhcver.o dataio.o upcase.o dtgutils.o

FC        = $(FCMP)
LIBS	  =

CMD       = nhcver.x

$(CMD): $(OBJS)
	$(FC) $(LIBS) $(FFLAGS) -o $(CMD) $(OBJS)

nhcver.o: nhcver.f
	$(FC) -c $(FFLAGS) nhcver.f

dataio.o: dataformats.inc dataioparms.inc dataio.f
	$(FC) -c $(FFLAGS) dataio.f

upcase.o: upcase.f
	$(FC) -c $(FFLAGS) upcase.f

dtgutils.o: dtgutils.f
	$(FC) -c $(FFLAGS) dtgutils.f

