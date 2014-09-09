#!/bin/sh

FOPTS=	  $(FFLAGS)
SRCS=	  gtgdef.f  raddate.f  w3fi71.f  editbufr.f
CMD=	  ../../exec/verf_gridtobs_editbufr

$(CMD):	  $(SRCS)
	  $(FCMP) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
