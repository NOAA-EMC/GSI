#==============================================================================
#
# Makefile for JCSDA CRTM Build
#
# $Id: makefile 22454 2012-11-16 23:39:18Z paul.vandelst@noaa.gov $
#
#==============================================================================

include ../make.macros
include make.filelist


# Library build
all library: $(LIBRARY)

$(LIBRARY): $(OBJ_FILES)
	$(ARCHIVER) $(ARCHIVER_FLAGS) $@ $(OBJ_FILES)


# Library cleanup
clean:
	$(REMOVE) $(OBJ_FILES) *.$(EXT_MOD) $(LIBRARY)


include make.dependencies
include ../make.rules
