SHELL=	/bin/sh
LIBS    =  -L./landsfcutil_d -llandsfcutil_d  \
           -L/nwprod/lib -lbacio_4 -lw3_d -lip_d -lsp_d -lessl
UTIL_LIB_DIR = ./landsfcutil_d
FCOMP = mpxlf95_r
FFLAGS =  -q64 -qrealsize=8 -I${UTIL_LIB_DIR} \
          -qessl -O3 -qstrict -qmaxmem=-1 
#FFLAGS =  -C -g -q64 -qrealsize=8 -I${UTIL_LIB_DIR}  
EXE = gridgen_sfc.exe

OBJS = calc_latlons.o \
       calc_tiles.o \
       glacier.o \
       green.o \
       grib_latlons.o \
       gribit.o \
       gridgen_sfc.o \
       interp_tiles.o \
       interp_to_mdl.o \
       leaf_area_index.o \
       lsmask_orog.o  \
       max_snow_albedo.o \
       program_setup.o \
       putgb.o \
       roughness.o \
       seaice.o \
       slope_type.o \
       smooth.o \
       snow_climo.o \
       snow_free_albedo.o \
       soil_substrate.o \
       soil_vegtype_tile.o \
       soilm.o \
       soiltype_notile.o \
       sort.o \
       sst_climo.o \
       vegtype_notile.o 

${EXE}:	${OBJS} 
	$(FCOMP) $(FFLAGS) -qsmp=noauto -o ${EXE} ${OBJS} $(LIBS) ${UTIL_LIB}

calc_latlons.o: program_setup.o
	${FCOMP} ${FFLAGS} -c calc_latlons.f

calc_tiles.o:
	${FCOMP} ${FFLAGS} -c calc_tiles.f

green.o: program_setup.o
	${FCOMP} ${FFLAGS} -c green.f

gridgen_sfc.o: program_setup.o gribit.o calc_latlons.o \
               lsmask_orog.o grib_latlons.o  \
               vegtype_notile.o soiltype_notile.o
	${FCOMP} ${FFLAGS} -c gridgen_sfc.f

glacier.o: program_setup.o
	${FCOMP} ${FFLAGS} -c glacier.f

gribit.o: program_setup.o calc_latlons.o
	${FCOMP} ${FFLAGS} -c gribit.f

grib_latlons.o: gribit.o calc_latlons.o program_setup.o
	${FCOMP} ${FFLAGS} -c grib_latlons.f

interp_tiles.o: gribit.o program_setup.o 
	${FCOMP} ${FFLAGS} -c interp_tiles.f

interp_to_mdl.o: program_setup.o lsmask_orog.o calc_latlons.o gribit.o
	${FCOMP} ${FFLAGS} -c interp_to_mdl.f

leaf_area_index.o: calc_latlons.o lsmask_orog.o program_setup.o gribit.o  
	${FCOMP} ${FFLAGS} -c leaf_area_index.f

lsmask_orog.o: program_setup.o gribit.o calc_latlons.o 
	${FCOMP} ${FFLAGS} -c lsmask_orog.f

max_snow_albedo.o: program_setup.o
	${FCOMP} ${FFLAGS} -c max_snow_albedo.f

program_setup.o:
	${FCOMP} ${FFLAGS} -c program_setup.f

putgb.o:
	xlf_r ${FFLAGS} -c putgb.f

roughness.o: program_setup.o lsmask_orog.o gribit.o
	${FCOMP} ${FFLAGS} -c roughness.f

seaice.o: program_setup.o
	${FCOMP} ${FFLAGS} -c seaice.f

slope_type.o: program_setup.o
	${FCOMP} ${FFLAGS} -c slope_type.f

smooth.o: 
	${FCOMP} ${FFLAGS} -c smooth.f

snow_climo.o: program_setup.o
	${FCOMP} ${FFLAGS} -c snow_climo.f

snow_free_albedo.o: program_setup.o
	${FCOMP} ${FFLAGS} -c snow_free_albedo.f

soil_substrate.o: gribit.o program_setup.o calc_latlons.o lsmask_orog.o
	${FCOMP} ${FFLAGS} -c soil_substrate.f

soil_vegtype_tile.o: gribit.o program_setup.o lsmask_orog.o interp_tiles.o
	${FCOMP} ${FFLAGS} -c soil_vegtype_tile.f

soilm.o: program_setup.o
	${FCOMP} ${FFLAGS} -c soilm.f

soiltype_notile.o: program_setup.o
	${FCOMP} ${FFLAGS} -c soiltype_notile.f

sort.o:
	${FCOMP} ${FFLAGS} -c sort.f

sst_climo.o: program_setup.o
	${FCOMP} ${FFLAGS} -c sst_climo.f

vegtype_notile.o: program_setup.o
	${FCOMP} ${FFLAGS} -c vegtype_notile.f

clean:
	rm -f *.o *.mod ${EXE}
