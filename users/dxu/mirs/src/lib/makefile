#==============================================================================
#
# Makefile for library
#
# S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 2005-2006.
#==============================================================================

#---Paths are read from a common file (used across the MIRS package)
paths_path = ../../setup
include $(paths_path)/paths

#---Default macros and rules (from CRTM)
include $(CRTM_path)/make.macros
include $(CRTM_path)/make.rules

# -------------
# This makefile
# -------------
MAKEFILE = makefile


# ------------
# Object files
# ------------
OBJ_FILES_SUB = \
	$(lib_path)/misc/misc.o                  $(lib_path)/utilities/utils.o          \
	$(lib_path)/misc/CntrlParams.o           $(lib_path)/misc/Consts.o              \
	$(lib_path)/misc/TuningParams.o          $(lib_path)/io/IO_Scene.o              \
	$(lib_path)/InversProcess/GeophCovBkg.o  $(lib_path)/InversProcess/SeFeErrCov.o \
	$(lib_path)/io/IO_MeasurData.o           $(lib_path)/qc/QCchecking.o            \
	$(lib_path)/PrePostProcess/PreClassif.o  $(lib_path)/noise/Noise.o              \
	$(lib_path)/FwdOperProcess/FwdOperator.o $(lib_path)/math/mathFcts.o            \
	$(lib_path)/InversProcess/VarOprs.o      $(lib_path)/io/IO_InstrConfig.o        \
	$(lib_path)/io/IO_Monitor.o              $(lib_path)/io/IO_DropSonde.o          \
	$(lib_path)/io/IO_Misc_LE.o                                                     \
	$(lib_path)/io/IO_Noise.o                $(lib_path)/io/IO_Regress.o            \
	$(lib_path)/io/IO_Misc.o                 $(lib_path)/qc/ErrorHandling.o         \
        $(lib_path)/io/IO_Colocate.o             $(lib_path)/io/IO_DEP.o		\
	$(lib_path)/io/io_ssmis.o

OBJ_FILES = $(OBJ_FILES_SUB)


INCLUDES  = \
	-I$(lib_path)/misc          -I$(lib_path)/io              -I$(lib_path)/math           \
	-I$(lib_path)/InversProcess -I$(lib_path)/FwdOperProcess  -I$(lib_path)/PrePostProcess \
	-I$(lib_path)/qc            -I$(lib_path)/noise           -I$(CRTM_path)/include       \
	-I$(lib_path)/utilities

LIBRARIES = \
	-L$(CRTM_path)/lib -lCRTM

# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(convEndOption)



#----------------
# Execution
#----------------
all:
	@case `uname -s` in \
		"SunOS"  ) make -f $(MAKEFILE) the_library $(SUNOS_FLAGS)  ;; \
		"AIX"    ) make -f $(MAKEFILE) the_library $(AIX_FLAGS)    ;; \
		"IRIX64" ) make -f $(MAKEFILE) the_library $(IRIX64_FLAGS) ;; \
		"Linux"  ) make -f $(MAKEFILE) the_library $(LINUX_FLAGS)  ;; \
		*        ) echo "This system is not supported"             ;; \
	esac

the_library: lib


#----------------------------------
# Library files compilation
#----------------------------------

#----Self-dependent modules
$(lib_path)/misc/Consts.o     : $(lib_path)/misc/Consts.f90
	cd $(lib_path)/misc ; $(FC) $(EXTRA_FC_FLAGS) $(FC_FLAGS) Consts.f90
$(lib_path)/misc/misc.o          : $(lib_path)/misc/misc.f90 $(lib_path)/misc/Consts.o
	cd $(lib_path)/misc ; $(FC) $(EXTRA_FC_FLAGS) $(FC_FLAGS) misc.f90
#----Modules dependent on others
$(lib_path)/qc/ErrorHandling.o       : $(lib_path)/qc/ErrorHandling.f90 $(lib_path)/misc/misc.o 
	cd $(lib_path)/qc ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) ErrorHandling.f90
$(lib_path)/math/mathFcts.o      : $(lib_path)/math/mathFcts.f90 $(lib_path)/qc/ErrorHandling.o
	cd $(lib_path)/math ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) mathFcts.f90
$(lib_path)/misc/TuningParams.o     : $(lib_path)/misc/TuningParams.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/Consts.o
	cd $(lib_path)/misc ; $(FC) $(INCLUDES)  $(EXTRA_FC_FLAGS) $(FC_FLAGS) TuningParams.f90
$(lib_path)/misc/CntrlParams.o   : $(lib_path)/misc/CntrlParams.f90 $(lib_path)/misc/Consts.o
	cd $(lib_path)/misc ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) CntrlParams.f90
$(lib_path)/io/IO_Monitor.o      : $(lib_path)/io/IO_Monitor.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/Consts.o $(lib_path)/misc/TuningParams.o
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Monitor.f90
$(lib_path)/io/IO_Regress.o      : $(lib_path)/io/IO_Regress.f90 $(lib_path)/misc/misc.o 
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Regress.f90
$(lib_path)/io/IO_Noise.o      : $(lib_path)/io/IO_Noise.f90 $(lib_path)/misc/misc.o 
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Noise.f90
$(lib_path)/noise/Noise.o        : $(lib_path)/noise/Noise.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/CntrlParams.o $(lib_path)/io/IO_Noise.o
	cd $(lib_path)/noise ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) Noise.f90
$(lib_path)/utilities/utils.o: $(lib_path)/utilities/utils.f90 $(lib_path)/misc/Consts.o \
	$(lib_path)/qc/ErrorHandling.o $(lib_path)/noise/Noise.o $(lib_path)/math/mathFcts.o
	cd $(lib_path)/utilities ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) utils.f90 
$(lib_path)/io/IO_Misc.o        : $(lib_path)/io/IO_Misc.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/Consts.o $(lib_path)/utilities/utils.o $(lib_path)/qc/ErrorHandling.o 
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Misc.f90
$(lib_path)/io/IO_Misc_LE.o      : $(lib_path)/io/IO_Misc_LE.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/Consts.o $(lib_path)/utilities/utils.o $(lib_path)/qc/ErrorHandling.o 
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Misc_LE.f90
$(lib_path)/io/IO_Scene.o        : $(lib_path)/io/IO_Scene.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/Consts.o $(lib_path)/utilities/utils.o $(lib_path)/qc/ErrorHandling.o
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Scene.f90
$(lib_path)/io/IO_DropSonde.o   : $(lib_path)/io/IO_DropSonde.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/utilities/utils.o 
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_DropSonde.f90
$(lib_path)/io/IO_DEP.o        : $(lib_path)/io/IO_DEP.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/Consts.o $(lib_path)/utilities/utils.o $(lib_path)/qc/ErrorHandling.o
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_DEP.f90
$(lib_path)/io/IO_Colocate.o        : $(lib_path)/io/IO_Colocate.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/Consts.o $(lib_path)/utilities/utils.o
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_Colocate.f90
$(lib_path)/io/IO_InstrConfig.o    : $(lib_path)/io/IO_InstrConfig.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/Consts.o
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) IO_InstrConfig.f90
$(lib_path)/io/io_ssmis.o    : $(lib_path)/io/io_ssmis.f90
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) io_ssmis.f90
$(lib_path)/InversProcess/GeophCovBkg.o  : $(lib_path)/InversProcess/GeophCovBkg.f90 \
	$(lib_path)/misc/misc.o $(lib_path)/misc/CntrlParams.o $(lib_path)/io/IO_Scene.o \
	$(lib_path)/misc/TuningParams.o $(lib_path)/utilities/utils.o $(lib_path)/qc/ErrorHandling.o
	cd $(lib_path)/InversProcess ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) GeophCovBkg.f90
$(lib_path)/InversProcess/SeFeErrCov.o : $(lib_path)/InversProcess/SeFeErrCov.f90 \
	$(lib_path)/misc/misc.o $(lib_path)/misc/CntrlParams.o $(lib_path)/qc/ErrorHandling.o
	cd $(lib_path)/InversProcess ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) SeFeErrCov.f90
$(lib_path)/io/IO_MeasurData.o       : $(lib_path)/io/IO_MeasurData.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/Consts.o $(lib_path)/qc/ErrorHandling.o
	cd $(lib_path)/io ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS)  $(FC_FLAGS) IO_MeasurData.f90
$(lib_path)/qc/QCchecking.o          : $(lib_path)/qc/QCchecking.f90 $(lib_path)/misc/misc.o \
	$(lib_path)/misc/CntrlParams.o $(lib_path)/misc/TuningParams.o $(lib_path)/InversProcess/SeFeErrCov.o \
	$(lib_path)/InversProcess/GeophCovBkg.o $(lib_path)/noise/Noise.o $(lib_path)/io/IO_Scene.o  \
	$(lib_path)/utilities/utils.o $(lib_path)/qc/ErrorHandling.o $(lib_path)/misc/Consts.o
	cd $(lib_path)/qc ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) QCchecking.f90
$(lib_path)/PrePostProcess/PreClassif.o  : $(lib_path)/PrePostProcess/PreClassif.f90 \
	$(lib_path)/misc/misc.o $(lib_path)/misc/Consts.o
	cd $(lib_path)/PrePostProcess ; $(FC)  $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) PreClassif.f90
$(lib_path)/FwdOperProcess/FwdOperator.o     : $(lib_path)/FwdOperProcess/FwdOperator.f90 \
	$(lib_path)/misc/CntrlParams.o $(lib_path)/io/IO_Scene.o $(lib_path)/misc/Consts.o \
	$(lib_path)/utilities/utils.o $(lib_path)/qc/ErrorHandling.o
	cd $(lib_path)/FwdOperProcess ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) FwdOperator.f90
$(lib_path)/InversProcess/VarOprs.o : $(lib_path)/InversProcess/VarOprs.f90 \
	$(lib_path)/math/mathFcts.o
	cd $(lib_path)/InversProcess ; $(FC) $(INCLUDES) $(EXTRA_FC_FLAGS) $(FC_FLAGS) VarOprs.f90
#---Dependence 
lib : $(OBJ_FILES_SUB)



# --------
# Clean up
# --------
clean:
	$(REMOVE)  \
	$(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb *~            $(lib_path)/misc/*.mod  $(lib_path)/misc/*~             \
	$(lib_path)/misc/*.MOD            $(lib_path)/io/*.mod   $(lib_path)/io/*~       $(lib_path)/io/*.MOD            \
	$(lib_path)/math/*.mod            $(lib_path)/math/*~    $(lib_path)/math/*.MOD  $(lib_path)/InversProcess/*.mod \
	$(lib_path)/InversProcess/*~      $(lib_path)/InversProcess/*.MOD   $(lib_path)/FwdOperProcess/*.mod             \
	$(lib_path)/FwdOperProcess/*~     $(lib_path)/FwdOperProcess/*.MOD  $(lib_path)/PrePostProcess/*.mod             \
	$(lib_path)/PrePostProcess/*~     $(lib_path)/PrePostProcess/*.MOD  $(lib_path)/qc/*.mod   $(lib_path)/qc/*~     \
	$(lib_path)/qc/*.MOD              $(lib_path)/noise/*.mod           $(lib_path)/noise/*~                         \
	$(lib_path)/noise/*.MOD           $(lib_path)/utilities/*.mod       $(lib_path)/utilities/*~                     \
	$(lib_path)/utilities/*.MOD 



