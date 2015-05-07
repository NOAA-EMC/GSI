#!/bin/ksh

FOPTS=	   $(FFLAGS)
SRCS=	gridtobs.f  inarea.f   nevn2.f     setfcst.f   setobtyp.f  stldsp.f  stread.f     strmbl.f   w3fa09a.f \
	gtgdef.f    inlayer.f  readcntl.f  setlevel.f  status.f    stlstr.f  streadfho.f  strxbl.f   w3fa09.f \
	igotdata.f  isitob.f   setarea.f   setmodel.f  stclst.f    stnumb.f  streal.f     ufbin3a.f  w3fi71.f

CMD=	 ../../exec/verf_gridtobs_gridtobs

$(CMD):	 $(SRCS)
	 $(FCMP) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
