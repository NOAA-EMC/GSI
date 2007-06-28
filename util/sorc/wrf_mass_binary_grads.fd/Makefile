#==============================================================================
#
# Makefile for the program to create GrADS station data files
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------


# -------------
# This makefile
# -------------

MAKE_FILE = Makefile


# ----------------
# Executable files
# ----------------

EXE_FILE = wrf_mass_binary_grads


# -----------------
# Install directory
# -----------------

  INSTALL_DIR = ./


# -----------------
# Log file for make
# -----------------

  LOG_FILE = log.$(EXE_FILE)


# ---------------
# Call child make
# ---------------

"" :
	@$(MAKE) -f $(MAKE_FILE) all		# default make with optimizing options
#	@$(MAKE) -f $(MAKE_FILE) debug		# default make with debug options 


# ------------
# Make install
# ------------

install:
	@echo
	@echo '==== INSTALL ================================================='
	@if [ -e $(INSTALL_DIR) ]; then \
	  if [ ! -d $(INSTALL_DIR) ]; then \
	    echo '### Fail to create installing directory ###' ;\
	    echo '### Stop the installation               ###' ;\
	    exit ;\
	  fi ;\
	else \
	  echo "	mkdir -p $(INSTALL_DIR)" ;\
	  mkdir -p $(INSTALL_DIR) ;\
	fi
	cp $(EXE_FILE) $(INSTALL_DIR)
	@cd $(INSTALL_DIR) ; ls -l `pwd`/$(EXE_FILE)


# ----------
# Make clean
# ----------

clean:
	@echo
	@echo '==== CLEAN ==================================================='
	- rm -f $(EXE_FILE) *.o *.mod *.MOD *.lst



#-----------------------------------------------------------------------------
#                          -- Child make --
#-----------------------------------------------------------------------------

# ------------
# Source files
# ------------

SRC_C   =

SRC_F90 = \
		wrf_mass_binary_grads.f90

SRC = $(SRC_C) $(SRC_F) $(SRC_F90)


# ------------
# Object files
# ------------

OBJ_FILES = ${SRC_C:.c=.o} ${SRC_F:.f=.o} ${SRC_F90:.f90=.o}


# ---------------
# Dependency list
# ---------------


# ---------------------
# Default compile rules
# ---------------------

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS) -c $<

.f90.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS) -c $<

.f.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGFIX) -c $<

.c.o  :
	@echo
	@echo '---> Compiling $<'
	$(CC) $(CFLAGS) -c $<


# ----
# Link
# ----

$(EXE_FILE) : $(OBJ_FILES)
	$(LD) $(LDFLAGS) -o $@ $(OBJ_FILES) $(LIBS)


# ----------------------------
# Fortran compiler and options
# ----------------------------

  CF		= xlf90

#--- Normal mode options

  FFLAGS_F90	= -qfree=f90 -qsuffix=f=f90:cpp=F90 \
		  -WF,-C -WF,-P -WF,-DNEWPRED=$(NEWPRED)
  FFLAGS_FIX	= -qfixed

  FFLAGS_COM_N	= -NS32704 -qmaxmem=-1 -O2 -Q

  FFLAGS_N	= $(FFLAGS_F90) $(FFLAGS_COM_N) $(INCLUDES)
  FFLAGFIX_N	= $(FFLAGS_FIX) $(FFLAGS_COM_N) $(INCLUDES)

#--- Debug mode options

  FFLAGS_COM_D	= -qmaxmem=-1 \
			-qdbg -qfullpath \
			-qlanglvl=90pure \
			-qsigtrap=xl__trcedump \
			-qcheck -qextchk -qwarn64 \
			-qflag=i:i \
			-qfloat=nans \
			-qflttrap=overflow:zero:invalid:enable \
			-qattr=full -qxref=full -qnolm \
			-d

#			-qhalt=w \
#			-qinitauto=7FF7FFFF \
#			-qlist -qflag=i:i \

  FFLAGS_D	= $(FFLAGS_F90) $(FFLAGS_COM_D) $(INCLUDES)
  FFLAGFIX_D	= $(FFLAGS_FIX) $(FFLAGS_COM_D) $(INCLUDES)

#--- Compile options for each source file

  FFLAGS	= $(FFLAGS_D)
  FFLAGFIX	= $(FFLAGFIX_D)


# ----------------------
# C compiler and options
# ----------------------

  CC		= cc

#--- Normal mode options

  CFLAGS_N	= -I ./ -O2

#--- Debug mode options

  CFLAGS_D	= -I ./ -g

#--- Compile options for each source

  CFLAGS	= $(CFLAGS_D)


# ------------------
# Linker and options
# ------------------

  LD		= $(CF)

#--- Normal mode options

  WRFPATH=/nwprod/sorc/nam_nmm_real_fcst.fd

  LIBwrfio_int    = $(WRFPATH)/external/io_int/libwrfio_int.a
  LIBwrfio_netcdf = $(WRFPATH)/external/io_netcdf/libwrfio_nf.a
  OBJwrf_frame_pk = $(WRFPATH)/frame/pack_utils.o

  WRFLIB = $(LIBwrfio_int) $(LIBwrfio_netcdf) $(OBJwrf_frame_pk)

  LIBS_N	= -lessl -lmass $(WRFLIB)

  LDFLAGS_N	= -bloadmap:loadmap.txt

#--- Debug mode options

  LIBS_D	= $(LIBS_N)

  LDFLAGS_D	= $(LDFLAGS_N)


# ------------------------
# Call compiler and linker
# ------------------------

all :
	@$(MAKE) -f $(MAKE_FILE) "COMP_MODE=$@" check_mode
	@echo
	@echo '==== COMPILE ================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"FFLAGS=$(FFLAGS_N)" "FFLAGFIX=$(FFLAGFIX_N)" \
		"CFLAGS=$(CFLAGS_N)" \
		$(OBJ_FILES)
	@echo
	@echo '==== LINK ===================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"LIBS=$(LIBS_N)" "LDFLAGS=$(LDFLAGS_N)" \
		$(EXE_FILE)

debug :
	@$(MAKE) -f $(MAKE_FILE) "COMP_MODE=$@" check_mode
	@echo
	@echo '==== COMPILE ================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"FFLAGS=$(FFLAGS_D)" "FFLAGFIX=$(FFLAGFIX_D)" \
		"CFLAGS=$(CFLAGS_D)" \
		$(OBJ_FILES)
	@echo
	@echo '==== LINK ===================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"LIBS=$(LIBS_D)" "LDFLAGS=$(LDFLAGS_D)" \
		$(EXE_FILE)

check_mode :
	@if [ -e $(LOG_FILE) ]; then \
	  if [ '$(COMP_MODE)' != `head -n 1 $(LOG_FILE)` ]; then \
	    echo ;\
	    echo "### COMPILE MODE WAS CHANGED ###" ;\
	    make clean ;\
	  fi ;\
	else \
	  echo ;\
	  echo "### NO LOG FILE ###" ;\
	  make clean ;\
	fi
	@echo $(COMP_MODE) > $(LOG_FILE)
