SHELL=          /bin/sh
LIBS=           -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbufr_4_64 -lbacio_4
###LIBS=           -L/u/Jack.Woollen/lib -lw3_4 -lbufr_sc -lbacio_4
FC=             ifort
FFLAGS=         -heap-arrays -i4 
LDFLAGS=       
SRCS=        grid3d.f surfce.f gridef.f observ.f backgv.f guesfc.f guess.f itotal.f obstrs.f weights.f debug.f counts.f vdates.f vrtfac.f guser.f adddate.f calcape.f calpbl.f calpw.f  getbak.f getfct.f getprof_upr.f htindex.f ireadpb.f \
             obscape.f obspbl.f  raddate.f setterp.f ufbxy3.f w3fb14.f w3fi71.f windchill.f prepfits.f
OBJS=        grid3d.o surfce.o gridef.o observ.o backgv.o guesfc.o guess.o itotal.o obstrs.o  weights.o debug.o counts.o vdates.o vrtfac.o guser.o adddate.o calcape.o calpbl.o calpw.o  getbak.o getfct.o getprof_upr.o htindex.o ireadpb.o \
             obscape.o obspbl.o  raddate.o setterp.o ufbxy3.o w3fb14.o w3fi71.o windchill.o prepfits.o
CMD=            verf_gridtobs_prepfits

all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS)$(LDFLAGS) -g  -o $(@) $(OBJS) $(LIBS)
clean:
	-rm -rf *.o *.lst
