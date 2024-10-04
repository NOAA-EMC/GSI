#==============================================================================
#
# Makefile for the program to create GrADS station data files
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

#------------
# Include machine dependent compile & load options
#------------
MAKE_CONF = Makefile.conf
include $(MAKE_CONF)


# -------------
# This makefile
# -------------

MAKE_FILE = Makefile


# ----------------
# Executable files
# ----------------

EXE_FILE = diag2grads


# -----------------
# Install directory
# -----------------

  INSTALL_DIR = ./


# -----------------
# Log file for make
# -----------------

  LOG_FILE = log.make.$(EXE_FILE)


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
	- rm -f $(EXE_FILE) *.o *.mod *.MOD *.lst loadmap.txt log.make.$(EXE_FILE)



#-----------------------------------------------------------------------------
#                          -- Child make --
#-----------------------------------------------------------------------------

# ------------
# Source files
# ------------

SRC_C   =

SRC_F90 = \
		kinds.F90		\
		utilities_time.f90	\
		read_diag.f90		\
		grads_hdr.f90		\
		write_station.f90       \
		diag2grads.f90

SRC = $(SRC_C) $(SRC_F) $(SRC_F90)


# ------------
# Object files
# ------------

SRCSF90       = ${SRC_F90:.F90=.f90}

OBJ_FILES = ${SRC_C:.c=.o} ${SRC_F:.f=.o} ${SRCSF90:.f90=.o}


# ---------------
# Dependency list
# ---------------

diag2grads.o : diag2grads.f90 write_station.o grads_hdr.o read_diag.o utilities_time.o
grads_hdr.o : grads_hdr.f90
kinds.o : kinds.F90
read_diag.o : read_diag.f90 kinds.o
utilities_time.o : utilities_time.f90
write_station.o : write_station.f90 read_diag.o grads_hdr.o



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
