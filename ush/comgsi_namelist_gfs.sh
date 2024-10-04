
cat <<EOF > gsiparm.anl

 &SETUP
   miter=${nummiter},niter(1)=50,niter(2)=50,
   niter_no_qc(1)=50,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=82,qoption=2,cwoption=3,
   factqmin=5.0,factqmax=5.0,deltim=1200,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,verbose=.true., 
   use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
   use_gfs_nemsio=.false.,lrun_subdirs=.false.,
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,diag_precon=.true.,step_start=1.0e-3,emiss_bc=.true.,cwoption=3,
   deltim=$DELTIM,
   lread_obs_save=${if_read_obs_save},lread_obs_skip=${if_read_obs_skip},
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
 /
 &BKGERR
   vs=${vs_op}
   hzscl=${hzscl_op}
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
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
   dmesh(1)=1450.0,dmesh(2)=1500.0,time_window_max=0.5,ext_sonde=.true.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                 dval    dthin dsfcalc
   prepbufr       ps          null      ps                   0.0     0     0
   prepbufr       t           null      t                    0.0     0     0
   prepbufr       q           null      q                    0.0     0     0
   prepbufr       pw          null      pw                   0.0     0     0
   satwndbufr     uv          null      uv                   0.0     0     0
   prepbufr       uv          null      uv                   0.0     0     0
   prepbufr       spd         null      spd                  0.0     0     0
   prepbufr       dw          null      dw                   0.0     0     0
   radarbufr      rw          null      l3rw                 0.0     0     0
   l2rwbufr       rw          null      l2rw                 0.0     0     0
   prepbufr       sst         null      sst                  0.0     0     0
   gpsrobufr      gps_bnd  null      gps                  0.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp      pcp_ssmi             0.0    -1     0
   tmirrbufr      pcp_tmi     trmm      pcp_tmi              0.0    -1     0
   sbuvbufr       sbuv2       n16       sbuv8_n16            0.0     0     0
   sbuvbufr       sbuv2       n17       sbuv8_n17            0.0     0     0
   sbuvbufr       sbuv2       n18       sbuv8_n18            0.0     0     0
   hirs3bufr      hirs3       n17       hirs3_n17            0.0     1     1
   hirs4bufr      hirs4       metop-a   hirs4_metop-a        0.0     1     1
   gimgrbufr      goes_img    g11       imgr_g11             0.0     1     0
   gimgrbufr      goes_img    g12       imgr_g12             0.0     1     0
   airsbufr       airs        aqua      airs_aqua            0.0     1     1
   amsuabufr      amsua       n15       amsua_n15            0.0     1     1
   amsuabufr      amsua       n18       amsua_n18            0.0     1     1
   amsuabufr      amsua       metop-a   amsua_metop-a        0.0     1     1
   airsbufr       amsua       aqua      amsua_aqua           0.0     1     1
   amsubbufr      amsub       n17       amsub_n17            0.0     1     1
   mhsbufr        mhs         n18       mhs_n18              0.0     1     1
   mhsbufr        mhs         metop-a   mhs_metop-a          0.0     1     1
   ssmitbufr      ssmi        f15       ssmi_f15             0.0     1     0
   amsrebufr      amsre_low   aqua      amsre_aqua           0.0     1     0
   amsrebufr      amsre_mid   aqua      amsre_aqua           0.0     1     0
   amsrebufr      amsre_hig   aqua      amsre_aqua           0.0     1     0
   ssmisbufr      ssmis_las   f16       ssmis_f16            0.0     1     0
   ssmisbufr      ssmis_uas   f16       ssmis_f16            0.0     1     0
   ssmisbufr      ssmis_img   f16       ssmis_f16            0.0     1     0
   ssmisbufr      ssmis_env   f16       ssmis_f16            0.0     1     0
   gsnd1bufr      sndrd1      g12       sndrD1_g12           0.0     1     0
   gsnd1bufr      sndrd2      g12       sndrD2_g12           0.0     1     0
   gsnd1bufr      sndrd3      g12       sndrD3_g12           0.0     1     0
   gsnd1bufr      sndrd4      g12       sndrD4_g12           0.0     1     0
   gsnd1bufr      sndrd1      g11       sndrD1_g11           0.0     1     0
   gsnd1bufr      sndrd2      g11       sndrD2_g11           0.0     1     0
   gsnd1bufr      sndrd3      g11       sndrD3_g11           0.0     1     0
   gsnd1bufr      sndrd4      g11       sndrD4_g11           0.0     1     0
   gsnd1bufr      sndrd1      g13       sndrD1_g13           0.0     1     0
   gsnd1bufr      sndrd2      g13       sndrD2_g13           0.0     1     0
   gsnd1bufr      sndrd3      g13       sndrD3_g13           0.0     1     0
   gsnd1bufr      sndrd4      g13       sndrD4_g13           0.0     1     0
   iasibufr       iasi        metop-a   iasi_metop-a         0.0     1     1
   gomebufr       gome        metop-a   gome_metop-a         0.0     2     0
   omibufr        omi         aura      omi_aura             0.0     2     0
   sbuvbufr       sbuv2       n19       sbuv8_n19            0.0     0     0
   hirs4bufr      hirs4       n19       hirs4_n19            0.0     1     1
   amsuabufr      amsua       n19       amsua_n19            0.0     1     1
   mhsbufr        mhs         n19       mhs_n19              0.0     1     1
   tcvitl         tcp         null      tcp                  0.0     0     0
   seviribufr     seviri      m08       seviri_m08           0.0     1     0
   seviribufr     seviri      m09       seviri_m09           0.0     1     0
   seviribufr     seviri      m10       seviri_m10           0.0     1     0
   hirs4bufr      hirs4       metop-b   hirs4_metop-b        0.0     1     0
   amsuabufr      amsua       metop-b   amsua_metop-b        0.0     1     0
   mhsbufr        mhs         metop-b   mhs_metop-b          0.0     1     0
   iasibufr       iasi        metop-b   iasi_metop-b         0.0     1     0
   gomebufr       gome        metop-b   gome_metop-b         0.0     2     0
   atmsbufr       atms        npp       atms_npp             0.0     1     0
   atmsbufr       atms        n20       atms_n20             0.0     1     0
   crisbufr       cris        npp       cris_npp             0.0     1     0
   crisfsbufr     cris-fsr    npp       cris-fsr_npp         0.0     1     0
   crisfsbufr     cris-fsr    n20       cris-fsr_n20         0.0     1     0
   mlsbufr        mls30       aura      mls30_aura           0.0     0     0
   oscatbufr      uv          null      uv                   0.0     0     0
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
 /
 &NST
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${ANAL_TIME},
   obhourset=0.,
 /
EOF
