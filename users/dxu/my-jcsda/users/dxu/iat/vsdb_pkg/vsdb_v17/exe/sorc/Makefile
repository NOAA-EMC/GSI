#!/bin/sh

CMD=    ../grid2grid.x

FOPTS=  $(FFLAGS)
SRCS=  grid2grid.f gtgdef.f readcntl.f setarea.f \
	setfcst.f setlevel.f setmodel.f setobtyp.f \
	stclst.f stldsp.f stlstr.f stnumb.f stread.f strmbl.f \
	strxbl.f getregion.f getnumber.f getGRIBdata.f \
	read_area.f sl1l2.f vl1l2.f fho.f getfho.f get_hasdata.f \
	getHGTsfc.f getTndGRIBdata.f getMeanClimData.f sal1l2.f \
	getAnomlyLevMrk.f val1l2.f afho.f FITWAV_2D.f FITWAV_1D.f \
	esl1l2.f evl1l2.f rhnt.f rhet.f  
$(CMD): $(SRCS)
	$(FCMP) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
########################################################
