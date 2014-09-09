#!/bin/ksh

FOPTS=	 $(FFLAGS)
SRCS=	grid3d.f surfce.f gridef.f observ.f backgv.f guesfc.f guess.f itotal.f obstrs.f weights.f \
	debug.f counts.f vdates.f vrtfac.f guser.f adddate.f calcape.f calpbl.f calpw.f  getbak.f \
	getfct.f getprof_upr.f htindex.f ireadpb.f obscape.f obspbl.f  raddate.f setterp.f ufbxy3.f \
	w3fb14.f w3fi71.f windchill.f prepfits.f


CMD=    ../../exec/verf_gridtobs_prepfits

$(CMD):	  $(SRCS)
	  $(FCMP) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
