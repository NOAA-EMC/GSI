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
LIB  = lib$(THIS).a
LIBgsi_util  = libGSI_Util.a
LIBgsi_solver  = libGSI_Solver.a
BIN  = prepbykx.x

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

MAPL_MOD	= $(wildcard $(LIB_MAPL_BASE))
ifeq ("$(MAPL_MOD)", "")
   HAVE_ESMF  =
   INC_ESMF   =
   INC_GEOS   =
   GSIGC_SRCS =
   SRCSX      = crtm_cloud.F90 stub_set_crtm_cloud.F90
   BIN       += #gsi.x
   LIB_GMAO   =
else
   HAVE_ESMF = -DHAVE_ESMF
   INC_GEOS  = $(INC_MAPL_BASE) $(INC_TRANSF) $(INC_GEOS_SHARED) $(INC_MPEU)
   GSIGC_SRCS = GSI_GridCompMod.F90
   SRCSX      = 
   LIB_GMAO =  $(LIB_GFIO) $(LIB_MPEU)
endif

# To deactivate GEOS_PERT-related routines
# ----------------------------------------
GEOS_PERT = -DGEOS_PERT
GEOS_PERT =
INC_FVGCM =
LIB_FVGCM =

RSRC =	gmao_airs_bufr.tbl		\
        gmao_global_aeroinfo.txt        \
        gmao_global_anavinfo.txt        \
	gmao_global_blacklist.txt	\
	gmao_global_coinfo.txt		\
	gmao_global_convinfo.txt	\
	gmao_global_ozinfo.txt		\
	gmao_global_pcpinfo.txt		\
	gmao_global_satinfo.txt		\
        gmao_global_scaninfo.txt        \
	gsi_fdda_1.rc.tmpl 		\
	gsi_fdda_2.rc.tmpl 		\
	gsi_fgat_1.rc.tmpl 		\
	gsi_fgat_2.rc.tmpl 		\
	gsi.rc.tmpl			\
	gsi_sens.rc.tmpl		\
	obs.rc.tmpl

local_esma_install local_install:
	$(MAKE) install_lib
	$(MAKE) install_inc
	$(MAKE) install_bin
	$(MAKE) install_etc

install_lib: $(ESMALIB) $(LIB)
	@ echo "-- $@: $(LIB) --> $(ESMALIB)/ --"
	$(CP) $(LIBgsi_util)   $(ESMALIB)/
	$(CP) $(LIBgsi_solver) $(ESMALIB)/
	$(CP) $(LIB)           $(ESMALIB)/

install_inc: $(ESMAINC)/$(THIS)
	@ echo "-- $@: *.mod --> $(ESMAINC)/ --"
	$(CP) *.mod     $(ESMAINC)/$(THIS)

install_bin: $(ESMABIN) $(BIN) analyzer gsidiags
	@ echo "-- $@: $(BIN) --> $(ESMABIN)/ --"
	$(CP) $(BIN)     $(ESMABIN)/
	$(SED) -e "s^@DASPERL^$(PERL)^" < analyzer > $(ESMABIN)/analyzer
	$(SED) -e "s^@DASPERL^$(PERL)^" < gsidiags > $(ESMABIN)/gsidiags
	chmod 755 $(ESMABIN)/analyzer
	chmod 755 $(ESMABIN)/gsidiags

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


#                  ----------------------------------------
#                  Define What Participates in GSI Util Lib
#                  ----------------------------------------
#                  Requirement: things depending on gridmod
#                               and below gridmod level with
#                               some reaon (obviously)

SRCS_UTIL =	$(wildcard \
	aniso_ens_util.f90 \
	antcorr_application.f90 \
	compact_diffs.f90 \
	compute_fact10.f90 \
	constants.f90 \
        control_vectors.f90 \
	egrid2agrid_mod.f90 \
	fgrid2agrid_mod.f90 \
	fill_mass_grid2.f90 \
	fill_nmm_grid2.f90 \
	fpvsx_ad.f90 \
        general_read_gfsatm.f90 \
        general_specmod.f90 \
        general_spectral_transforms.f90 \
        general_sub2grid_mod.f90 \
        general_transform.f90 \
	gengrid_vars.f90 \
	get_derivatives.f90 \
	get_derivatives2.f90 \
        get_gefs_ensperts_dualres.f90 \
        get_wrf_mass_ensperts_netcdf.F90 \
	get_semimp_mats.f90 \
	getuv.f90 \
        gsi_bundlemod.F90 \
        gsi_chemguess_mod.F90 \
        gsi_metguess_mod.F90 \
	grdcrd.f90 \
	grid2sub.f90 \
	gridmod.F90 \
	gscond_ad.f90 \
        gsi_4dcouplermod.F90 \
	gsi_io.f90 \
	gsi_nemsio_mod.f90 \
	half_nmm_grid2.f90 \
	hilbert_curve.f90 \
	hybrid_ensemble_isotropic.F90 \
	hybrid_ensemble_parameters.f90 \
	init_commvars.f90 \
        insitu_info.f90 \
	kinds.f90 \
        looplimits.f90 \
	m_dgeevx.F90 \
	m_rerank.F90 \
	m_stats.F90 \
	m_tick.F90 \
	mpeu_mpif.F90 \
	mpeu_util.F90 \
	missing_routines.f90 \
	mod_nmmb_to_a.f90 \
	mod_strong.f90 \
	mp_compact_diffs_mod1.f90 \
	mp_compact_diffs_support.f90 \
	mpimod.F90 \
	mpl_allreduce.F90 \
	mpl_bcast.f90 \
        ncepgfs_ghg.f90 \
	ncepnems_io.f90 \
	nstio_module.f90 \
	nlmsas_ad.f90 \
	omegas_ad.f90 \
	phil.f90 \
	phil1.f90 \
	plib8.f90 \
	polcarf.f90 \
	prad_bias.f90 \
	precond.f90 \
	precpd_ad.f90 \
	psichi2uv_reg.f90 \
	psichi2uvt_reg.f90 \
	raflib.f90 \
        rapidrefresh_cldsurf_mod.f90 \
	rdgrbsst.f90 \
	rfdpar.f90 \
	rsearch.F90 \
	rtlnmc_version3.f90 \
	sfc_model.f90 \
	simpin1.f90 \
	simpin1_init.f90 \
	smoothwwrf.f90 \
	state_vectors.f90 \
        stop1.f90 \
        stub_set_crtm_aerosol.F90 \
        stub_pertmod.F90 \
        stub_timermod.F90 \
	sub2grid.f90 \
	tendsmod.f90 \
	timermod.F90 \
	tintrp2a.f90 \
	tintrp3.f90 \
	turbl.f90 \
	turbl_ad.f90 \
	turbl_tl.f90 \
	turblmod.f90 \
	unfill_mass_grid2.f90 \
	unfill_nmm_grid2.f90 \
	unhalf_nmm_grid2.f90 \
	wind_fft.f90 \
	blockIO.c ${SRCSX} )

#                  --------------------
#                  User Defined Targets
#                  --------------------

SRCS_SOLVER =	$(wildcard \
	adjtest.f90 \
        aeroinfo.f90 \
	anberror.f90 \
	anbkerror.f90 \
	anisofilter.f90 \
	anisofilter_glb.f90 \
	antest_maps0.f90 \
	antest_maps0_glb.f90 \
	balmod.f90 \
	berror.f90 \
	bias_predictors.f90 \
	bkerror.f90 \
	bkgcov.f90 \
	bkgvar.f90 \
	bkgvar_rewgt.f90 \
        bicg.f90 \
        bicglanczos.F90 \
	blacklist.f90 \
	cal_tztr.f90 \
	calc_fov_conical.f90 \
	calc_fov_crosstrk.f90 \
	calctends.f90 \
	calctends_ad.f90 \
	calctends_tl.f90 \
	calctends_no_ad.f90 \
	calctends_no_tl.f90 \
	chemmod.f90 \
        clw_mod.f90 \
	cmaq_routines.f90 \
        co_mop_ak.f90 \
        coinfo.f90 \
	combine_radobs.f90 \
	compute_derived.f90 \
        compute_qvar3d.f90 \
	control2model.f90 \
	control2state.f90 \
	converr.f90 \
	convinfo.f90 \
	convthin.f90 \
        crtm_interface.f90 \
	cvsection.f90 \
	dtast.f90 \
	deter_nst.f90 \
        enorm_state.F90 \
        evaljgrad.f90 \
        evaljcdfi.F90 \
        evaljo.f90 \
        evalqlim.f90 \
	genqsat.f90 \
	genstats_gps.f90 \
	gesinfo.F90 \
	getprs.f90 \
        getsiga.f90 \
	getvvel.f90 \
	glbsoi.F90 \
	grtest.f90 \
        gsd_terrain_match_surfTobs.f90 \
        gsdcloudanalysis.F90 \
	gsi_4dvar.f90 \
	gsimod.F90 \
	gsisub.F90 \
	guess_grids.F90 \
	inc2guess.f90 \
	init_jcdfi.F90 \
	int3dvar.f90 \
	intall.f90 \
	intco.f90 \
	intdw.f90 \
	intgps.f90 \
	intjcpdry.f90 \
	intjo.f90 \
	intlag.F90 \
	intlimq.f90 \
	intoz.f90 \
	intpcp.f90 \
	intpm2_5.f90 \
	intps.f90 \
	intpw.f90 \
	intq.f90 \
	intrad.f90 \
	intrp2a.f90 \
	intrp3oz.f90 \
	intrw.f90 \
	intspd.f90 \
	intsrw.f90 \
	intsst.f90 \
	intt.f90 \
	inttcp.f90 \
	intw.f90 \
	jcmod.f90 \
	jfunc.f90 \
        jgrad.f90 \
	lag_fields.f90 \
	lag_interp.F90 \
	lag_traj.F90 \
	lagmod.f90 \
        lanczos.F90 \
	m_berror_stats.F90 \
        m_berror_stats_reg.f90 \
	m_gsiBiases.F90 \
	m_dtime.F90 \
	m_gpsrhs.F90 \
	m_obdiag.F90 \
	m_rhs.F90 \
	mod_vtrans.f90 \
	model_ad.F90 \
	model_tl.F90 \
	model2control.f90 \
	ncepgfs_io.f90 \
	normal_rh_to_q.f90 \
	obs_ferrscale.F90 \
	obs_para.f90 \
	obs_sensitivity.f90 \
	observer.F90 \
	obsmod.F90 \
	oneobmod.F90 \
	ozinfo.f90 \
	patch2grid_mod.f90 \
	pcgsoi.f90 \
	pcgsqrt.f90 \
	pcp_k.f90 \
	pcpinfo.f90 \
	penal.f90 \
	prewgt.f90 \
	prewgt_reg.f90 \
	projmethod_support.f90 \
	prt_guess.f90 \
	q_diag.f90 \
	qcmod.f90 \
	qnewton.f90 \
	qnewton3.f90 \
	radinfo.f90 \
        read_aerosol.F90 \
	read_airs.f90 \
	read_amsre.f90 \
	read_anowbufr.f90 \
	read_avhrr.f90 \
	read_avhrr_navy.f90 \
	read_bufrtovs.f90 \
	read_co.F90 \
	read_diag.f90 \
	read_files.f90 \
        read_gfs_ozone_for_regional.f90 \
	read_goesimg.f90 \
	read_goesndr.f90 \
	read_gps.f90 \
	read_guess.F90 \
	read_iasi.f90 \
        read_cris.f90 \
	read_l2bufr_mod.f90 \
	read_lag.F90 \
	read_lidar.f90 \
        read_Lightning.f90 \
	read_modsbufr.f90 \
        read_NASA_LaRC.f90 \
	read_obs.F90 \
	read_obsdiags.F90 \
	read_ozone.F90 \
	read_pcp.f90 \
	read_prepbufr.f90 \
	read_radar.f90 \
        read_RadarRef_mosaic.f90 \
        read_seviri.f90 \
	read_ssmi.f90 \
	read_ssmis.f90 \
	read_superwinds.f90 \
	read_tcps.f90 \
	read_wrf_mass_files.f90 \
	read_wrf_mass_guess.F90 \
	read_wrf_nmm_files.f90 \
	read_wrf_nmm_guess.F90 \
	regional_io.f90 \
        reorg_metar_cloud.f90 \
	satthin.F90 \
	setupbend.f90 \
	setupco.f90 \
	setupdw.f90 \
	setuplag.F90 \
	setupoz.f90 \
	setuppcp.f90 \
        setuppm2_5.f90 \
	setupps.f90 \
	setuppw.f90 \
	setupq.f90 \
	setuprad.f90 \
	setupref.f90 \
	setuprhsall.f90 \
	setuprw.f90 \
	setupspd.f90 \
	setupsrw.f90 \
	setupsst.f90 \
	setupt.f90 \
	setuptcp.f90 \
	setupw.f90 \
	setupyobs.f90 \
	sfcobsqc.f90 \
	skindepth.f90 \
	smooth_polcarf.f90 \
	smoothrf.f90 \
	smoothzrf.f90 \
        sqrtmin.f90 \
	sst_retrieval.f90 \
	state2control.f90 \
	statsco.f90 \
	statsconv.f90 \
	statsoz.f90 \
	statspcp.f90 \
	statsrad.f90 \
	stp3dvar.f90 \
	stpcalc.f90 \
	stpco.f90 \
	stpdw.f90 \
	stpgps.f90 \
	stpjo.f90 \
	stpjcpdry.f90 \
	stplimq.f90 \
	stpoz.f90 \
	stppcp.f90 \
	stppm2_5.f90 \
	stpps.f90 \
	stppw.f90 \
	stpq.f90 \
	stprad.f90 \
	stprw.f90 \
	stpspd.f90 \
	stpsrw.f90 \
	stpsst.f90 \
	stpt.f90 \
	stptcp.f90 \
	stpw.f90 \
	strong_bal_correction.f90 \
	strong_baldiag_inc.f90 \
	strong_fast_global_mod.f90 \
	strong_slow_global_mod.f90 \
	sub2fslab_mod.f90 \
	support_2dvar.f90 \
	test_obsens.f90 \
	tcv_mod.f90 \
	tpause.f90 \
	tpause_t.F90 \
	tv_to_tsen.f90 \
	update_guess.f90 \
	update_geswtend.f90 \
	wrf_binary_interface.F90 \
	wrf_mass_guess_mod.F90 \
	wrf_netcdf_interface.F90 \
	write_all.F90 \
	write_bkgvars_grid.f90 \
	write_obsdiags.F90 \
	wrwrfmassa.F90 \
	wrwrfnmma.F90 \
	xhat_vordivmod.f90 \
	zrnmi_mod.f90 )


SRCS = $(SRCS_UTIL) $(SRCS_SOLVER) $(GSIGC_SRCS)
ALLSRCS = $(SRCS) gsimain.F90 prepbykx.f

OBJS_UTIL   := $(addsuffix .o, $(basename $(SRCS_UTIL)))
OBJS_SOLVER := $(addsuffix .o, $(basename $(SRCS_SOLVER)))
OBJS_GC     := $(addsuffix .o, $(basename $(GSIGC_SRCS)))
DEPS := $(addsuffix .d, $(basename $(ALLSRCS)))

_D = -D_GMAO_FVGSI_ -D_IGNORE_GRIDVERIFY_ $(GEOS_PERT)
_D =                                      $(GEOS_PERT)

ifeq ("$(FOPT)","-O3")
   FOPT += $(LOOP_VECT)
endif
FREAL      = $(FREAL4) 
FOPT_nobig = $(FOPT) $(BYTERECLEN) $(_D)
FPE        =

THIS_SP    = NCEP_sp_r8i4
THIS_W3    = NCEP_w3_r8i4
THIS_BACIO = NCEP_bacio_r4i4
THIS_BUFR  = NCEP_bufr_r8i4
THIS_GFSIO = NCEP_gfsio
THIS_NEMSIO= NCEP_nemsio
LIB_GFSIO  = $(ESMADIR)/$(ARCH)/lib/lib$(THIS_GFSIO).a   # move to proper place
INC_GFSIO  = $(ESMADIR)/$(ARCH)/include/$(THIS_GFSIO)   # move to proper place
INC_BACIO  = # $(ESMADIR)/$(ARCH)/include/$(THIS_BACIO)   # move to proper place

MOD_DIRS = . $(INC_ESMF) $(INC_CRTM)              \
	     $(INC_SIGIO) $(INC_GFSIO) $(INC_BACIO) $(INC_NEMSIO) \
	     $(INC_SFCIO) $(INC_GEOS) $(INC_MPI)
USER_FINCS = $(foreach dir,$(MOD_DIRS), $(I)$(dir))
USER_FDEFS = $(_D) $(HAVE_ESMF)
USER_FFLAGS = -CB $(BIG_ENDIAN) $(BYTERECLEN)
USER_FFLAGS = $(BYTERECLEN)
USER_FFLAGS =
USER_FFLAGS = $(BIG_ENDIAN) $(BYTERECLEN)
USER_CFLAGS = -I . -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -O3
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 

vpath % $(MOD_DIRS)

$(LIBgsi_util) : $(OBJS_UTIL)
	$(RM) $(LIBgsi_util)
	$(AR) $(AR_FLAGS) $(LIBgsi_util) $(OBJS_UTIL)

$(LIBgsi_solver) : $(OBJS_SOLVER)
	$(RM) $(LIBgsi_solver)
	$(AR) $(AR_FLAGS) $(LIBgsi_solver) $(OBJS_SOLVER)

$(LIB) lib : $(LIBgsi_util) $(LIBgsi_solver) $(OBJS_GC)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS_GC)

%.x : $(LIB) %.o 
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB) $(LIB_SYS)

gsi.x:  $(LIBgsi_util)  $(LIBgsi_solver) gsimain.o
	$(FC) $(LDFLAGS) -o gsi.x gsimain.o $(LIBgsi_solver) $(LIBgsi_util) $(LIB_CRTM)  \
	     $(LIB_SFCIO)  $(LIB_BUFR) $(LIB_NEMSIO) $(LIB_BACIO) $(LIB_GFSIO) $(LIB_SIGIO) \
	     $(LIB_SP) $(LIB_W3) \
	     $(LIB_MPI) $(LIB_SCI) $(LIB_SYS)

ut_gsibundle.x:  $(OBJS) $(LIB) ut_gsibundle.o
	$(FC) $(LDFLAGS) -o ut_gsibundle.x ut_gsibundle.o $(LIBgsi_util) \
	     $(LIB_MPI) $(LIB_SCI) $(LIB_SYS)

fFLAGSx = $(patsubst -convert big_endian,,$(fFLAGS))
prepbykx.o : prepbykx.f
	$(ESMA_TIMER) $(FC) -c $(fFLAGSx) $<

prepbykx.x: prepbykx.o
	$(LD) $(LDFLAGS) -o prepbykx.x prepbykx.o $(LIB_BUFR)

blockIO.o : blockIO.c
	@echo '---> Special handling of C code $<'
	$(CC) $(USER_CFLAGS) -c $<

blockIO.d : blockIO.c
	@ touch $@

# OBJS_OPENBUFR lists all i/o (OPEN) objects opened the same way as 
# NCEP_bufr files (native).

OBJS_OPENBUFR	= read_airs.o		\
		  read_amsre.o		\
		  read_anowbufr.o       \
		  read_avhrr.o		\
		  read_avhrr_navy.o	\
		  read_bufrtovs.o	\
		  read_cris.o		\
		  read_goesimg.o	\
		  read_goesndr.o	\
		  read_gps.o		\
		  read_iasi.o		\
		  read_l2bufr_mod.o	\
		  read_lidar.o		\
		  read_modsbufr.o	\
		  read_ozone.o		\
		  read_pcp.o		\
		  read_prepbufr.o	\
		  read_radar.o		\
                  read_seviri.f90       \
		  read_ssmi.o		\
		  read_ssmis.o		\
		  read_superwinds.o	\
		  oneobmod.o

$(OBJS_OPENBUFR) :
	@echo '---> Special handling of Fortran "native" BUFR-OPEN $<'
	$(FC) -c $(patsubst $(BIG_ENDIAN),,$(f90FLAGS)) $<

ifeq ($(BOPT),g)
FOPTx = $(patsubst -O0,-O1,$(FOPT))
f90FLAGSx  = $(FDEFS) $(FINCS) $(FMODS) $(FOPTx) $(FREAL) $(FINT) $(XFLAGS) $(USER_FFLAGS)
nlmsas_ad.o: nlmsas_ad.f90
	$(ESMA_TIMER) $(FC) -c $(f90FLAGSx) $<
endif

FFLAGS_OPENBUFR = $(patsubst $(BIG_ENDIAN),,$(f90FLAGS))

#
OBJS_OPENBIG	= m_berror_stats.o	\
		  balmod.o		\
		  prewgt.o

$(OBJS_OPENBIG) :
	@echo '---> Special handling of Fortran "big_endian" OPEN $<'
	$(FC) -c $(BIG_ENDIAN) $(f90FLAGS) $<

FFLAGS_OPENBIG = $(BIG_ENDIAN) $(f90FLAGS)

#                  --------------------
#                     Documentation
#                  --------------------

#PDF_DOCS = GSI_BundleUserGuide.pdf GSI_ChemGuess_Mod.pdf
PDF_DOCS = GSI_ChemGuess_Mod.pdf GSI_MetGuess_Mod.pdf
PDF_DOCS = GSI_MetGuess_Mod.pdf

esma_doc doc: $(PDF_DOCS)

UG_SRCS = gsi_bundlemod.F90

GSI_BundleUserGuide.tex: $(UG_SRCS)
	$(PROTEX) -g ut_gsibundle.F90 > ut_gsibundle.tex
	$(PROTEX) -g -b -f $(UG_SRCS) > GSI_BundleUserGuide___.tex
	$(CAT) ut_gsibundle.tex GSI_BundleUserGuide___.tex > GSI_BundleUserGuide.tex

GSI_ChemGuess_Mod.tex: gsi_chemguess_mod.F90
	$(PROTEX) -g gsi_chemguess_mod.F90  > GSI_ChemGuess_Mod.tex

GSI_MetGuess_Mod.tex: gsi_metguess_mod.F90
	$(PROTEX) -g gsi_metguess_mod.F90  > GSI_MetGuess_Mod.tex

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros
#.
