#	Top level Makefile for wrf system

LN      =       ln -s
MAKE    =       make -i -r
MV	=	/bin/mv
RM      =       /bin/rm -f

deflt :
		@ echo Please compile the code using ./compile

include ./configure.gen_be

#### 3.d.   add macros to specify the modules for this core

ALL_MODULES =                           \
               $(INCLUDE_MODULES)

configcheck:
	@if [ "$(A2DCASE)" -a "$(DMPARALLEL)" ] ; then \
	 echo "------------------------------------------------------------------------------" ; \
	 echo "WRF CONFIGURATION ERROR                                                       " ; \
	 echo "The $(A2DCASE) case cannot be used on distributed memory parallel systems." ; \
	 echo "Only 3D WRF cases will run on these systems." ; \
	 echo "Please chose a different case or rerun configure and chose a different option."  ; \
	 echo "------------------------------------------------------------------------------" ; \
         exit 2 ; \
	fi


gen_be :
	@ echo '--------------------------------------'
	( $(MAKE) $(J) externals  )
	( cd src ; $(MAKE) $(J) MODULE_DIRS="$(ALL_MODULES)" be )
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`

clean :
		@ echo 'Use the clean script'

# DO NOT DELETE
