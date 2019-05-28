
cat <<EOF > gsiparm.anl

 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=1,
   factqmin=0.0,factqmax=0.0,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,verbose=.true.,
   offtime_data=.true.,diag_aero=.false.,
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,
   use_edges=.false.,diag_precon=.false.,
 /
 &GRIDOPTS
   JCAP=62,JCAP_B=62,NLAT=60,NLON=60,nsig=60,regional=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=${bk_core_arw},
   nems_nmmb_regional=.false.,nmmb_reference_grid='H',diagnostic_reg=.false.,
   cmaq_regional=${bk_core_cmaq},
   filled_grid=.false.,half_grid=.true.,netcdf=${bk_if_netcdf},
 /
 &BKGERR
   vs=1.0,
   hzscl=.373,.746,1.5,
   bw=0.,fstat=.true.,
 /
 &ANBKGERR
 /
 &JCOPTS
 /
 &STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=30,time_window_max=240.0,ext_sonde=.true.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                 dval    dthin dsfcalc
   modisbufr      modis_aod   terra     v.modis_terra        1.0     2     0
   modisbufr      modis_aod   aqua      v.modis_aqua         1.0     2     0
   pm25bufr       pm2_5       null      TEOM                 1.0     0     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=.false.,
 /
 &RAPIDREFRESH_CLDSURF
 /
 &CHEM
  laeroana_gocart=${bk_laeroana_gocart},
  l_aoderr_table = .false.,
  aod_qa_limit = 3,
  luse_deepblue = .false.,
  aero_ratios = .false.,
  tunable_error=0.5,
  berror_chem=.true.,
  wrf_pm2_5=${bk_wrf_pm2_5},
  diag_incr=.true.,
  in_fname="cmaq_in.bin",
  out_fname="cmaq_out.bin",
  incr_fname="cmaq_increment.bin",
 /
 &NST
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${ANAL_TIME},
   obhourset=0.,
 /
EOF
