SHELL=/bin/sh

#==============================================================================
#
# GLOBAL_ANGUPDATE Makefile
#
# <Usage>
#   0) Export this makefile name to a variable 'MAKE_FILE' as 
#       export MAKE_FILE = makefile
#      If this file is named neither 'makefile' nor 'Makefile' but 
#      'makeairs' for instance, then call this makefile by typing
#      'make -f makeairs' instead of 'make'.
#
#   1) To make a GLOBAL_ANGUPDATE executable file, type
#         > make  or  > make all
#
#   2) To make a GLOBAL_ANGUPDATE executable file with debug options, type
#         > make debug
#
#   3) To copy the GLOBAL_ANGUPDATE load module to installing directory, type
#         > make install
#      . Specify the directory to a variable 'INSTALL_DIR' below.
#
#   4) To crean up files created by make, type
#         > make clean
#
#
#
#==============================================================================


#-----------------------------------------------------------------------------
#                          -- Parent make (calls child make) --
#-----------------------------------------------------------------------------

# -------------
# This makefile
# -------------

  MAKE_FILE = Makefile


# -----------
# Load module
# -----------

  EXE_FILE = global_angupdate


# --------------------
# Installing directory
# --------------------

  INSTALL_DIR = ../bin


# --------
# Log file
# --------

  LOG_FILE = log.make.$(EXE_FILE)


# ---------------
# Call child make
# ---------------

"" :
	@$(MAKE) -f $(MAKE_FILE) all


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
	- rm -f *.o *.mod *.MOD *.lst


#-----------------------------------------------------------------------------
#                          -- Child make --
#-----------------------------------------------------------------------------

# ------------
# Source files
# ------------

  SRCSF90C = main.f90 read_diag.f90 determine_time_levels.f90

  SRCSF77 =

  SRCSC =

  SRCS = $(SRCSF90C) $(SRCF77) $(SRCSC)


# ------------
# Object files
# ------------

  SRCSF90	= ${SRCSF90C:.F90=.f90}

  OBJS 		= ${SRCSF90:.f90=.o} ${SRCSF77:.f=.o} ${SRCSC:.c=.o}


# -----------------------
# Default compiling rules
# -----------------------

.SUFFIXES : .F90 .f90 .f .c

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


# -----------------
# Exceptional rules
# -----------------

	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGR) -c $(*).f90


# ------------
# Dependencies
# ------------
main.o :  read_diag.o

# ----
# Link
# ----

$(EXE_FILE) : $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)


# ----------------------------
# Fortran compiler and options
# ----------------------------

 CF	= ncepmpxlf95

#--- Normal mode options

  FFLAGS_F90	= -qsuffix=f=f90
  FFLAGS_COM_N	= -I ./ -qarch=auto

  FFLAGS_N	= -O3 -qstrict $(FFLAGS_F90) $(FFLAGS_COM_N)

#--- Debug mode options

  FFLAGS_COM_D	= $(FFLAGS_COM_N) \
		  -O0 -qdbg -qfullpath \
		  -qsigtrap=xl__trcedump \
		  -qinitauto=7FF7FFFF \
		  -qflttrap=overflow:zero:enable \
		  -qcheck \
		  -qwarn64 \
		  -qflag=i:i

  FFLAGS_D	= $(FFLAGS_F90) $(FFLAGS_COM_D)


# ----------------------
# C Compiler and options
# ----------------------

  CC		= ncepcc

#--- Normal mode options

  CFLAGS_N	= -I ./ -O3

#--- Debug mode options

  CFLAGS_D	= -I ./ -g


# ------------------
# Linker and options
# ------------------

  LD		= $(CF)

#--- Normal mode options

  LIBS_N= -L/nwprod/lib -lw3_4

  LDFLAGS_N	= -lessl -lmass -bloadmap:loadmap.txt 

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
		"FFLAGS=$(FFLAGS_N)" "FFLAGR=$(FFLAGR_N)" "FFLAGFIX=$(FFLAGFIX_N)" \
		"CFLAGS=$(CFLAGS_N)" \
		$(OBJS)
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
		"FFLAGS=$(FFLAGS_D)" "FFLAGR=$(FFLAGR_D)" "FFLAGFIX=$(FFLAGFIX_D)" \
		"CFLAGS=$(CFLAGS_D)" \
		$(OBJS)
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

