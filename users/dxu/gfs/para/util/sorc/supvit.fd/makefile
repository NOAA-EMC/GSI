SHELL=  /bin/sh
ISIZE = 4
RSIZE = 8
COMP=   xlf_r
LIBS=   -L../../../lib -lw3_d
#FFLAGS= -O  
#FFLAGS= -O -qcheck -qextchk -qwarn64   -qintsize=$(ISIZE) -qrealsize=$(RSIZE)
FFLAGS= -O   -qflttrap=ov:und:zero:inv:enable -qintsize=$(ISIZE) -qrealsize=$(RSIZE)
#LDFLAGS= -bnoquiet -bloadmap:loadmap

supvit:     supvit_main.f supvit_modules.o
	@echo " "
	@echo "  Compiling program that sorts and updates vitals records...."
	$(COMP) $(FFLAGS) $(LDFLAGS) supvit_modules.o supvit_main.f $(LIBS) -o supvit
	@echo " "

supvit_modules.o:   supvit_modules.f
	@echo " "
	@echo "  Compiling the modules....."
	$(COMP) -c supvit_modules.f -o supvit_modules.o
	@echo " "

