SHELL=  /bin/sh
CMD=	../exec/precip_score.x
SRCS=	precip_score.f   
$(CMD): $(SRCS)
	$(FCMP) $(FFLAGS) $(SRCS) $(LIBS) -o $(CMD)
