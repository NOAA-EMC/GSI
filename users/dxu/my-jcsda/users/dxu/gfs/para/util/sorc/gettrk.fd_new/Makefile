SHELL=  /bin/sh
ISIZE = 4
RSIZE = 8
COMP=   $(FCMP)
LIBS=   -L$(LIBDIR) -lw3emc_d -lw3nco_d -lbacio_4
LDFLAGS= $(LDFLAGSM)
FFLAGS= $(FFLAGSM)

gettrk:      gettrk_main.f gettrk_modules.o
	@echo " "
	@echo "  Compiling the tracking program....."
	$(COMP) $(FFLAGS) $(LDFLAGS) gettrk_modules.o gettrk_main.f $(LIBS) -o gettrk
	@echo " "

gettrk_modules.o:    gettrk_modules.f
	@echo " "
	@echo "  Compiling the modules....."
	$(COMP) $(FFLAGS) -c gettrk_modules.f -o gettrk_modules.o
	@echo " "

clean:
	@echo " "
	@echo " Remove objects, modules, executable, and loadmap"
	- $(RM) gettrk *.o *.mod loadmap.txt

