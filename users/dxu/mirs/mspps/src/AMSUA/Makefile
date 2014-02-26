SHELL=/bin/bash
include ../../include/Makefile_include

OBJS=   AMA2HDF.o openfs.o read_parm.o read_corr_para.o read_systim.o \
        read_mask.o read_latbox_table.o init.o rama_whdfeos.o \
	rama_hd.o rama_scn.o scn_data.o gnrt_fname.o avn.o read_avn.o \
        gnrt_prod.o getcal.o cnt2rd.o rad2at.o \
        qc_at.o calprod.o mask_stype.o corr.o \
        tpwclw.o seaice.o stemp.o emi.o \
        set_sw.o sw_def.o def_dim.o def_geo.o def_data.o def_prod.o \
        gnrt_stime.o jd2md.o leapyear.o sw_wrt.o    \
        wrt_limit.o wrt_scal.o wrt_navig.o wrt_geo.o wrt_data.o \
        wrt_prod.o toolkit_td.o gnrt_bin_fname.o wrt_bin.o 

INCLUDE =  -I$(HDFEOS_INC) -I$(HDF_INC)
LIBS	=  -L$(HDFEOS_LIB) -L$(HDF_LIB) -L${SZIP_LIB} -lhdfeos -lGctp -lmfhdf -ldf -ljpeg -lz -lm -lsz

CMD1 = ${BIN_DIR}/runamsua_m2
CMD2 = ${BIN_DIR}/runamsua_nn
CMD3 = ${BIN_DIR}/runamsua_np

all: $(CMD1) $(CMD2) $(CMD3)

$(CMD1): $(OBJS)
	$(CC) $(INCLUDE) -o $(@) $(OBJS) $(LIBS)

$(CMD2): $(OBJS)
	$(CC) $(INCLUDE) -o $(@) $(OBJS) $(LIBS)

$(CMD3): $(OBJS)
	$(CC) $(INCLUDE) -o $(@) $(OBJS) $(LIBS)

.c.o:
	${CC} $(INCLUDE) -c $<

clean:
	-rm -f $(OBJS) $(CMD1) $(CMD2) $(CMD3)
