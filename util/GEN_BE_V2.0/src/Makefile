# gen_be GD

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

include ../configure.gen_be

GEN_BE_OBJS = \
	../external/blas/da_blas.o \
	../external/lapack/da_lapack.o

be : \
        gen_be_stage0.exe \
        gen_be_stage1.exe \
        gen_be_stage2.exe \
        gen_be_stage3.exe \
        gen_be_stage4.exe \
        gen_be_diags.exe \
	da_advance_time.exe \
	gen_be_nc2gsi.exe \

GEN_BE_LIB = $(GEN_BE_SRC_ROOT_DIR)/external/tools/libtools.a -L$(GEN_BE_SRC_ROOT_DIR)/external/fftpack/fftpack5 -lfftpack $(GEN_BE_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a $(LIB_EXTERNAL)

da_advance_time.exe : da_advance_time.o
	$(RM) $@
	x=`echo "$(SFC)" | awk '{print $$1}'` ; export x ; \
        if [ $$x = "gfortran" ] ; then \
           echo removing external declaration of iargc for gfortran ; \
           $(CPP) $(CPPFLAGS) da_advance_time.F | sed '/integer *, *external.*iargc/d' > da_advance_time.f90 ;\
        else \
           $(CPP) $(CPPFLAGS) da_advance_time.F > da_advance_time.f90 ; \
        fi
	$(SFC) -c $(FCFLAGS) da_advance_time.f90
	$(SFC) $(LDFLAGS) -o $@ da_advance_time.o

gen_be_stage0.exe : gen_be_stage0.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0.o $(GEN_BE_LIB)

gen_be_stage1.exe : gen_be_stage1.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o $(GEN_BE_LIB)

gen_be_stage2.exe : gen_be_stage2.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o $(GEN_BE_LIB)

gen_be_stage3.exe : gen_be_stage3.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o $(GEN_BE_LIB)

gen_be_stage4.exe : gen_be_stage4.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4.o $(GEN_BE_LIB)

gen_be_diags.exe : gen_be_diags.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o $(GEN_BE_LIB)

gen_be_nc2gsi.exe : gen_be_nc2gsi.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_nc2gsi.o $(GEN_BE_LIB)

basicclean:
	$(RM) *.f90 *.o

superclean:
	$(RM) *.f *.o *.mod *.exe

