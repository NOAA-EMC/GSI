SHELL=/bin/sh
## setenv OBJECT_MODE 64
ISIZE = 4
RSIZE = 8
COMP=   $(FCMP)
LIBS=   -L$(LIBDIR) -lw3_d -lbacio_4
LDFLAGS= -bloadmap:loadmap -bnoquiet
FFLAGS= -O -qcheck -qextchk -qarch=auto -qflttrap=ov:zero:inv:enable -qintsize=$(ISIZE) -qrealsize=$(RSIZE) 

gettrk_gen:      gettrk_main.gen.f gettrk_modules_gen.o
	@echo " "
	@echo "  Compiling the tracking program....."
	$(COMP) $(FFLAGS) $(LDFLAGS) gettrk_modules_gen.o gettrk_main.gen.f $(LIBS) -o gettrk_gen
	@echo " "

gettrk_modules_gen.o:    gettrk_modules.gen.f
	@echo " "
	@echo "  Compiling the modules....."
	$(COMP) $(FFLAGS) -c gettrk_modules.gen.f -o gettrk_modules_gen.o
	@echo " "
