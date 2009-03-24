#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 07Feb2008  Jing Guo   Based on GNUmakefile of GSI_GridComp
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../../../../..
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
BIN  = make_satinfo.x
DATA =	satinfo-merged.tmpl	\
	satinfo-v1.tmpl		\
	satinfo-v2.tmpl		\
	sidb

DIRS = $(ESMAETC) $(ESMABIN)

	# My default make-target is not "install" for local convenience.
my-default: test

esma_install install: install_bin install_etc

$(ESMABIN) $(ESMAETC) $(ESMAETC)/gmao_global_satinfo.db:
	$(MKDIR) $@

install_bin: $(ESMABIN) $(BIN)
	  $(CP) -p $(BIN) $(ESMABIN)/

install_etc: $(ESMAETC) $(ESMAETC)/gmao_global_satinfo.db $(DATA)
	  $(CP) sidb/*.tbl	\
	  	$(ESMAETC)/gmao_global_satinfo.db/
	  $(CP) -p satinfo-merged.tmpl	\
	  	$(ESMAETC)/gmao_global_satinfo-merged.tmpl
	  $(CP) -p satinfo-v1.tmpl		\
	  	$(ESMAETC)/gmao_global_satinfo-v1.tmpl
	  $(CP) -p satinfo-v2.tmpl		\
	  	$(ESMAETC)/gmao_global_satinfo-v2.tmpl

esma_clean clean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
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

#                  --------------------
#                  User Defined Targets
#                  --------------------


SRCS =  main.F90		\
	m_actvchan.F90		\
	m_satinst.F90		\
	m_satops.F90		\
	m_sitmpl.F90		\
	satinfo_util.F90

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

make_satinfo.x: $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS)

list:
	@ echo $(SRCS)

test: make_satinfo.x
	./make_satinfo.x <setup.nml

_D =

FOPT       = $(FOPT3)
FREAL      = $(FREAL4) 
FPE        =

USER_FDEFS	= $(_D)
USER_FFLAGS	= $(BIG_ENDIAN) $(BYTERECLEN)
USER_FFLAGS	=
USER_CFLAGS	=

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

#.
