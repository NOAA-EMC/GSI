SHELL=/bin/bash
include ../../../include/Makefile_include

OBJS =  MSWATH.o openfs.o read_parm.o read_latbox_table.o read_mask_snow.o \
        fov.o init.o rmhs_whdfeos.o avn.o read_avn.o gnrt_prod.o \
        mask_stype_snow.o convert_mhs_amsub.o read_swath_A.o read_swath_M.o \
        calprod.o spread.o snowcover.o rainrate.o snowfall.o swe_cal.o \
        get_conv_index.o icewp.o cal_pred_ocean.o cal_pred_land.o \
        set_sw.o sw_def.o def_prod.o sw_wrt.o bin_time.o \
	wrt_limit.o wrt_scal.o wrt_prod.o wrt_bin.o \
	rainrate_correct.o  read_lza_parameter.o read_iwp_s_table.o snowfall_rate.o

OBJS2=  CLOUD_OPT_CAL.o   GET_EMISS_1DALG.o   onelyr.o


INCLUDE =  -I$(HDFEOS_INC) -I$(HDF_INC)
LIBS	=  -L$(HDFEOS_LIB) -L$(HDF_LIB) -L${SZIP_LIB} -lhdfeos -lGctp -lmfhdf -ldf -ljpeg -lz -lm -lc -lsz $(LDFLAGS_FC)


CMD1 = ${BIN_DIR}/runmhs_fnl_m2
CMD2 = ${BIN_DIR}/runmhs_fnl_nn
CMD3 = ${BIN_DIR}/runmhs_fnl_np

all: ${CMD1} ${CMD2} ${CMD3}


$(CMD1): $(OBJS) $(OBJS2)
	$(CC) $(INCLUDE) -o $(@) $(OBJS) $(OBJS2) $(LIBS)

$(CMD2): $(OBJS) $(OBJS2)
	$(CC) $(INCLUDE) -o $(@) $(OBJS) $(OBJS2) $(LIBS)

$(CMD3): $(OBJS) $(OBJS2)
	$(CC) $(INCLUDE) -o $(@) $(OBJS) $(OBJS2) $(LIBS)

.c.o:
	${CC} $(INCLUDE) -c $<

.f90.o:
	${FC} $(INCLUDE) $(FC_FLAGS) -c $<

clean:
	-rm -f $(OBJS) $(OBJS2) $(CMD1) $(CMD2) $(CMD3) *.mod *.MOD
