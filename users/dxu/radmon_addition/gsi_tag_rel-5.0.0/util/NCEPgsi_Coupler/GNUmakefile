#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 3mar2004  Zaslavsky  Initial imlementation.
# 20Oct2004  da Silva  Standardization
# 16Mar2007  Kokron    Remove default optimization; Add LOOP_VECT 
# 19Aug2009 Jing Guo   Add m_rhs.F90 and m_dtime.F90 to support multi-pass observer
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------


THIS = $(shell basename `pwd`)
LIBO = lib$(THIS).a 
LIBX = libNCEPgsi_Util.a
LIB  = $(LIBO) $(LIBX)
BIN  =

test:
	: test  = "$@"
	: SHELL = $(SHELL)
	: PWD   = `pwd`
	: THIS  = $(THIS)
	: LIB   = $(LIB)
	: MODD	= $(MOD_DIRS)

#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

ALLDIRS = mksi

SUBDIRS = $(wildcard $(ALLDIRS))

TARGETS = esma_install esma_clean esma_distclean esma_doc \
	  install clean distclean doc 

.PHONY: install local_install install_lib install_inc install_bin install_etc

export ESMADIR BASEDIR ARCH SITE

$(TARGETS): 
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do                    \
	    ( cd $$d                            ;\
	      echo ""; echo Making $$t in `pwd`          ;\
	      $(MAKE) -e $$t ) \
	  done
	$(MAKE) local_$@

#                  ----------------------
#                   User Defined Targets
#                  ----------------------

RSRC =	

local_esma_install local_install:
	$(MAKE) install_lib
	$(MAKE) install_inc
	$(MAKE) install_bin
	$(MAKE) install_etc

install_lib: $(ESMALIB) $(LIB)
	@ echo "-- $@: $(LIB) --> $(ESMALIB)/ --"
	$(CP) $(LIB)     $(ESMALIB)/

install_inc: $(ESMAINC)/$(THIS)
	@ echo "-- $@: *.mod --> $(ESMAINC)/ --"
	$(CP) *.mod     $(ESMAINC)/$(THIS)

install_bin: $(ESMABIN) $(BIN)

install_etc: $(ESMAETC) $(RSRC)
	@ echo "-- $@: $(RSRC) --> $(ESMAETC)/ --"
	@ for f in $(RSRC); do \
	    ( case $$f in \
	      *.sample)		F=`basename $$f .sample` ;;\
	      *.txt)		F=`basename $$f .txt`.rc ;;\
	      *)		F=$$f			 ;;\
	      esac ;\
	      echo "$(CP) $$f     $(ESMAETC)/$$F" ;\
	      $(CP) $$f $(ESMAETC)/$$F )\
	  done

$(ESMALIB) $(ESMABIN) $(ESMAINC)/$(THIS) $(ESMAETC):
	@ echo "$@: making directory $@ ..."
	$(MKDIR) $@

local_esma_clean local_clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

local_esma_distclean local_distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

local_esma_doc local_doc:
	@echo "Target $@ not implemented yet in `pwd`"


esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% make esma_install    (builds and install under ESMADIR)"
	@echo "% make esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% make esma_distclean  (leaves in the same state as cvs co)"
	@echo "% make esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% make esma_help       (this message)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"
	@echo "        FREAL = $(FREAL)"

show_fflags:
	@echo "FFLAGS          = $(FFLAGS)"
	@echo "F90FLAGS        = $(F90FLAGS)"
	@echo "FFLAGS_OPENBUFR = $(FFLAGS_OPENBUFR)"
	@echo "FFLAGS_OPENBIG  = $(FFLAGS_OPENBIG)"
	@echo "USER_FFLAGS     = $(USER_FFLAGS)"
	@echo "_D              = $(_D)"

#                  --------------------
#                  User Defined Targets
#                  --------------------

SRCS =	$(wildcard \
          calctends_model_ad.f90 \
          calctends_model.f90 \
          calctends_model_tl.f90 \
          cplr_pertmod.F90 \
          dynamics_adams_bashforth.f90 \
          funtend.f90 \
          getphi.f90 \
          getprs_bck.f90 \
          gsi_model_ad.f90 \
          gsi_model_tl.f90 \
          gsi_4dmodel_ad.f90 \
          gsi_4dmodel_init_nl.f90 \
          gsi_4dmodel_tl.f90 \
          gsi_model_adjtest.f90 \
          gsi_model_tlmtest.f90 \
          hdiff.f90 \
          mp_compact_diffs_mod2.f90 \
          ncep_pertmod.f90 \
          nonlinmod.f90 \
          pbl_ad.f90 \
          pbl.f90 \
          pblmod.f90 \
          pbl_tl.f90 \
          read_nonlinear.f90 \
          sfcdrag.f90 \
          smooth_tends.f90 \
          tends4pertmod.f90 \
          write_nonlinear.f90 \
          write_bkgvars_grid_mod.f90 )

ALLSRCS = $(SRCS)

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(ALLSRCS)))

_D =

#ifeq ("$(FOPT)","-O3")
#   FOPT += $(LOOP_VECT)
#endif
FREAL      = $(FREAL4) 
FOPT       = $(FOPT2)
FPE        =

THIS_GFSIO = NCEP_gfsio
INC_GFSIO  = $(ESMADIR)/$(ARCH)/include/$(THIS_GFSIO)   # move to proper place

MOD_DIRS = . $(INC_GSI) 
USER_FDEFS = $(_D) $(HAVE_ESMF)
USER_FFLAGS = $(BIG_ENDIAN) $(BYTERECLEN)
USER_FFLAGS =
USER_CFLAGS = -I . -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -O3
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 

vpath % $(MOD_DIRS)

$(LIBX) : $(LIB_GSI_UTIL)
	$(CP) $(LIB_GSI_UTIL) $(LIBX)
	$(AR) -d $(LIBX) stub_pertmod.o

$(LIBO) lib : $(OBJS)
	$(RM) $(LIBO)
	$(AR) $(AR_FLAGS) $(LIBO) $(OBJS)

lib : $(LIBO) $(LIBX)

%.x : $(LIB) %.o 
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB) $(LIB_SYS)

FFLAGS_OPENBIG = $(BIG_ENDIAN) $(f90FLAGS)

#                  --------------------
#                     Documentation
#                  --------------------


# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros
#.
