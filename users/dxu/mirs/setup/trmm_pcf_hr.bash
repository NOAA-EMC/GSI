
####################################################################################################
#
#
#   S E C T I O N   O F   D A T A   A N D   P A T H S
#
#
####################################################################################################

#-------------------------------------------------------------------------------
#        Satellite, sensor and default date used in GUI
#-------------------------------------------------------------------------------
satId=trmm
sensor1=tmi
sensor2=dummy
date=2010-07-20

#-------------------------------------------------------------------------------
#        Major root paths
#-------------------------------------------------------------------------------
rootPath=/home/pub/mirs_operational
dataPath=${rootPath}/data
binPath=${rootPath}/bin
logPath=${rootPath}/logs
IDL=/usr/local/bin/idl
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/intel/fc/lib/ia32	#OS dependent(HP is SHLIB_PATH; AIX is LIBPATH; Linux is ID_LIBRARAY_PATH)

#-------------------------------------------------------------------------------
#        Research data & Paths
#-------------------------------------------------------------------------------
researchDataPath=/net/orbit006L/home/sidb/ResearchData
fwdPath=${researchDataPath}/FwdSimulOutputs
out1dvarPath=${researchDataPath}/1dvarOutputs
monitorFile=${researchDataPath}/IterProcessMonitor/Monitoring.dat
modelNonErrPath=${researchDataPath}/ModelErrStats/amsua_mhs

#-------------------------------------------------------------------------------
#        External data & Paths
#-------------------------------------------------------------------------------
externalDataPath=/home/pub/external_data/1b
rdrSensor1Path=${externalDataPath}/rdr/${satId}_${sensor1}
rdrOrbitPath=${externalDataPath}/rdr/OrbitalMode
nwpGdasGridPath=${externalDataPath}/gridNWP_analys
nwpEcmwfGridPath=${externalDataPath}/gridNWP_analys
nwpGfsGridPath=${externalDataPath}/gridNWP_analys

#-------------------------------------------------------------------------------
#        Static data & Paths
#-------------------------------------------------------------------------------
staticDataPath=${dataPath}/StaticData
instrumentPath=${staticDataPath}/InstrConfigInfo
instrumentSensor1File=${instrumentPath}/InstrConfig_${satId}_${sensor1}.dat
topographyFile=${staticDataPath}/Topography/topography.bin_sgi
antennaPath=${staticDataPath}/AntennaPatterns
antennaSensor1File=${antennaPath}/${satId}_${sensor1}_antennaPattern.dat
tune1File=${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}.in
tune2File=${staticDataPath}/TuningData/TunParams_${satId}_${sensor1}_2.in
nedtNominalFile=${staticDataPath}/NominalNedts/${satId}_NoiseFile.dat
modelErrNominalFile=${staticDataPath}/NominalModelErrs/${satId}_ModelErrFile.dat
covBkgAtm1File=${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat
covBkgAtm2File=${staticDataPath}/CovBkgStats/CovBkgMatrxTotAtm_all.dat
covBkgSfc1File=${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat
covBkgSfc2File=${staticDataPath}/CovBkgStats/CovBkgMatrxTotSfc_all_${satId}_${sensor1}.dat
spcCoeffFile=${staticDataPath}/CRTMFiles/${satId}_${sensor1}_SpcCoeff.bin
tauCoeffFile=${staticDataPath}/CRTMFiles/${satId}_${sensor1}_TauCoeff.bin
siceEmissCatalogFile=${staticDataPath}/EmissCatalog/SeaIceEmissCatalog_${satId}_${sensor1}.dat
snowEmissCatalogFile=${staticDataPath}/EmissCatalog/SnowEmissCatalog_${satId}_${sensor1}.dat
cldOptPropFile=${staticDataPath}/CRTMFiles/mw_cloud_opt.dat

#-------------------------------------------------------------------------------
#        Semi-Static data & Paths
#-------------------------------------------------------------------------------
semiStaticDataPath=${dataPath}/SemiStaticData
biasPath=${semiStaticDataPath}/biasCorrec
regressPath=${semiStaticDataPath}/regressAlgors
regressCoeffOceanClwFile=${regressPath}/Oc_regressCoeffs_${satId}_clw.dat
regressCoeffSeaIceClwFile=${regressPath}/SeaIce_regressCoeffs_${satId}_clw.dat
regressCoeffLandClwFile=${regressPath}/Land_regressCoeffs_${satId}_clw.dat
regressCoeffSnowClwFile=${regressPath}/Snow_regressCoeffs_${satId}_clw.dat
regressCoeffOceanTskinFile=${regressPath}/Oc_regressCoeffs_${satId}_tskin.dat
regressCoeffSeaIceTskinFile=${regressPath}/SeaIce_regressCoeffs_${satId}_tskin.dat
regressCoeffLandTskinFile=${regressPath}/Land_regressCoeffs_${satId}_tskin.dat
regressCoeffSnowTskinFile=${regressPath}/Snow_regressCoeffs_${satId}_tskin.dat
regressCoeffOceanTpwFile=${regressPath}/Oc_regressCoeffs_${satId}_tpw.dat
regressCoeffSeaIceTpwFile=${regressPath}/SeaIce_regressCoeffs_${satId}_tpw.dat
regressCoeffLandTpwFile=${regressPath}/Land_regressCoeffs_${satId}_tpw.dat
regressCoeffSnowTpwFile=${regressPath}/Snow_regressCoeffs_${satId}_tpw.dat
regressCoeffOceanEmFile=${regressPath}/Oc_regressCoeffs_${satId}_em.dat
regressCoeffSeaIceEmFile=${regressPath}/SeaIce_regressCoeffs_${satId}_em.dat
regressCoeffLandEmFile=${regressPath}/Land_regressCoeffs_${satId}_em.dat
regressCoeffSnowEmFile=${regressPath}/Snow_regressCoeffs_${satId}_em.dat
regressCoeffOceanWvFile=${regressPath}/Oc_regressCoeffs_${satId}_wv.dat
regressCoeffSeaIceWvFile=${regressPath}/SeaIce_regressCoeffs_${satId}_wv.dat
regressCoeffLandWvFile=${regressPath}/Land_regressCoeffs_${satId}_wv.dat
regressCoeffSnowWvFile=${regressPath}/Snow_regressCoeffs_${satId}_wv.dat
regressCoeffOceanTempFile=${regressPath}/Oc_regressCoeffs_${satId}_temp.dat
regressCoeffSeaIceTempFile=${regressPath}/SeaIce_regressCoeffs_${satId}_temp.dat
regressCoeffLandTempFile=${regressPath}/Land_regressCoeffs_${satId}_temp.dat
regressCoeffSnowTempFile=${regressPath}/Snow_regressCoeffs_${satId}_temp.dat
regressCoeffOceanGwpFile=${regressPath}/Oc_regressCoeffs_${satId}_gwp.dat
regressCoeffSeaIceGwpFile=${regressPath}/SeaIce_regressCoeffs_${satId}_gwp.dat
regressCoeffLandGwpFile=${regressPath}/Land_regressCoeffs_${satId}_gwp.dat
regressCoeffSnowGwpFile=${regressPath}/Snow_regressCoeffs_${satId}_gwp.dat
regressCoeffDesertFile=${regressPath}/Desert_regressCoeffs_${satId}.dat
biasFileToUse=${biasPath}/biasCorrec_${satId}.dat
calibBiasFitFile=${biasPath}/calibBiasFit_${satId}.dat
calibDTRlutFile=${biasPath}/calibDTRlut_${satId}.dat

#-------------------------------------------------------------------------------
#        Testbed data & Paths
#-------------------------------------------------------------------------------
testbedDataPath=${dataPath}/TestbedData
nedtPath=${testbedDataPath}/nedt
nedtSensor1Path=${nedtPath}/${satId}_${sensor1}
edrPath=${testbedDataPath}/Outputs/edr/${satId}_${sensor1}
depPath=${testbedDataPath}/Outputs/dep/${satId}_${sensor1}
gridPath=${testbedDataPath}/Outputs/grid/${satId}_${sensor1}
figsPath=${testbedDataPath}/Outputs/figs/${satId}_${sensor1}
perfsMonitorPath=${testbedDataPath}/PerfsMonitoring/${satId}_${sensor1}
logFile=${logPath}/${satId}_logFile

#-------------------------------------------------------------------------------
#        Dynamic data & Paths
#-------------------------------------------------------------------------------
dynamicDataPath=${testbedDataPath}/DynamicData
tdrPath=${dynamicDataPath}/tdr
tdrSensor1Path=${tdrPath}/${satId}_${sensor1}
sdrPath=${dynamicDataPath}/sdr
sdrSensor1Path=${sdrPath}/${satId}_${sensor1}
fmsdrPath=${dynamicDataPath}/fmsdr/${satId}_${sensor1}
choppPath=${dynamicDataPath}/fmsdrchopp/${satId}_${sensor1}
nwpAnalysPath=${dynamicDataPath}/nwp_analys/${satId}_${sensor1}
fwdAnalysPath=${dynamicDataPath}/fwd_analys/${satId}_${sensor1}
regressRetrPath=${dynamicDataPath}/regress_retr/${satId}_${sensor1}

#-------------------------------------------------------------------------------
#        Control Files
#-------------------------------------------------------------------------------
controlDataPath=${dataPath}/ControlData
rdr2tdrSensor1ControlFile=${controlDataPath}/${satId}_${sensor1}_rdr2tdr
mergeNedtControlFile=${controlDataPath}/${satId}_mergeNEDT
tdr2sdrSensor1ControlFile=${controlDataPath}/${satId}_${sensor1}_tdr2sdr
fmControlFile=${controlDataPath}/${satId}_${sensor1}_fm
fmsdr2edrControlFile=${controlDataPath}/${satId}_CntrlConfig_1dvar
grid2nwpControlFile=${controlDataPath}/${satId}_${sensor1}_colocNWPwRAD
fwdControlFile=${controlDataPath}/${satId}_cntrl_fwd
regressControlFile=${controlDataPath}/${satId}_ApplyRegress
choppControlFile=${controlDataPath}/${satId}_Chopp
mergeEdrControlFile=${controlDataPath}/${satId}_MergeEDR
vippControlFile=${controlDataPath}/${satId}_Vipp
gridControlFile=${controlDataPath}/${satId}_Grid
nwpGridControlFile=${controlDataPath}/${satId}_NWPGrid
fwdGridControlFile=${controlDataPath}/${satId}_FWDGrid
biasGridControlFile=${controlDataPath}/${satId}_BiasGrid
biasCompuControlFile=${controlDataPath}/${satId}_Inputs4BiasComputation
biasVerifControlFile=${controlDataPath}/${satId}_Inputs4BiasVerification
regressGenControlFile=${controlDataPath}/${satId}_Inputs4RegressGen
modifyNedtControlFile=${controlDataPath}/${satId}_modifyNedt
figsGenControlFile=${controlDataPath}/${satId}_Inputs4FigsGener

#-------------------------------------------------------------------------------
#        File List
#-------------------------------------------------------------------------------
inputDataPath=${dataPath}/InputsData
rdrSensor1List=${inputDataPath}/${satId}_${sensor1}_rdrFiles
tdrSensor1List=${inputDataPath}/${satId}_${sensor1}_tdrFiles
sdrSensor1List=${inputDataPath}/${satId}_${sensor1}_sdrFiles_lr
sdrSensor2List=${inputDataPath}/${satId}_${sensor1}_sdrFiles_hr
fmsdrList=${inputDataPath}/${satId}_fmsdrFiles
fmsdr4BiasList=${inputDataPath}/${satId}_fmsdrFiles_4Bias
fmsdr4ChoppList=${inputDataPath}/${satId}_fmsdrFiles_4Chopping
fmsdr4NwpList=${inputDataPath}/${satId}_fmsdrFiles_4nwp
fmsdr4BiasList=${inputDataPath}/${satId}_fmsdrFiles_4Bias
fmsdr4RegressList=${inputDataPath}/${satId}_fmsdrFiles_4regress
fmsdr4ApplyRegressList=${inputDataPath}/${satId}_fmsdrFiles_4ApplyRegress
edrList=${inputDataPath}/${satId}_edrFiles
edr4BiasList=${inputDataPath}/${satId}_edrFiles_4Bias
dep4BiasList=${inputDataPath}/${satId}_depFiles_4Bias
edr4MergeList=${inputDataPath}/${satId}_FullOrbitEDR_4Merging
depList=${inputDataPath}/${satId}_depFiles
nedtList=${inputDataPath}/${satId}_nedtDirs_${sensor1}
nedtSensor1List=${inputDataPath}/${satId}_nedtDirs_${sensor1}
gridSfcNwpAnalysList=${inputDataPath}/${satId}_sfcNWPanalys
gridAtmNwpAnalysList=${inputDataPath}/${satId}_atmNWPanalys
nwpAnalysList=${inputDataPath}/${satId}_NWPanalysFiles
nwpAnalysRetrList=${inputDataPath}/${satId}_NWPanalysFiles_4retr
nwpAnalys4BiasList=${inputDataPath}/${satId}_NWPanalysFiles_4Bias
nwpAnalys4RegressList=${inputDataPath}/${satId}_NWPanalysFiles_4Regress
fwdAnalys4BiasList=${inputDataPath}/${satId}_FWDanalysSimulFiles_4Bias


####################################################################################################
#
#
#   S E C T I O N   O F   A P P L I C A T I O N S   A N D   P R O C E S S E S
#
#
####################################################################################################

#-------------------------------------------------------------------------------
#        Source Directories
#-------------------------------------------------------------------------------
rdr2tdrSensor1Src=${rootPath}/src/testbed/rdr2tdr
mergeNedtSrc=${rootPath}/src/testbed/mergeNEDTofDiffInstr
tdr2sdrSrc=${rootPath}/src/testbed/tdr2sdr
fmSrc=${rootPath}/src/testbed/fm
choppSrc=${rootPath}/src/testbed/chopp
fmsdr2edrSrc=${rootPath}/src/1dvar
mergeEdrSrc=${rootPath}/src/testbed/mergeEDR
vippSrc=${rootPath}/src/testbed/vipp
gridSrc=${rootPath}/src/testbed/grid
nedtMonitorSrc=${rootPath}/src/testbed/nedtMonitoring
nwpGenAnalysSrc=${rootPath}/src/testbed/nwp
fwdSrc=${rootPath}/src/fwd
determineBiasSrc=${rootPath}/src/testbed/biasGenerAndMonit
regressAlgSrc=${rootPath}/src/testbed/regressAlgors
applyRegressAlgSrc=${rootPath}/src/testbed/retrRegress


####################################################################################################
#
#
#   S E C T I O N   O F   S W I T C H E S (W H I C H   A P P L I C A T I O N   T O   R U N)
#
#
####################################################################################################

step_rdr2tdrSensor1=1	#RDR->TDR (Sensor1)
step_mergeNedt=1	#MERGE NEDTs (Sensor1 and Sensor2)
step_tdr2sdrSensor1=1	#TDR->SDR (Sensor1)
step_fm=1	#FOOTPRINT MATCHING
step_nwp=0	#CREATE NWP SCENE (GDAS)
step_fwd=0	#USE FWD OPERATOR ON NWP SCENE
step_biasGen=0	#GENERATE A NEW TB EC
step_choppRadFiles=0	#CHOPPING RADIANCE FILES FOR MULTIPLE PROCESS SUBMISSION
step_externalDataFromRegress=1	#USE OF REGRESSION ALGORIHMS TO GENERATE EXTERN DATA
step_fmsdr2edr=1	#FMSDR->EDR
step_mergeEdr=0	#MERGE THE MINI EDR FILES INTO A FULL ORBITAL EDR FILE 
step_vipp=1	#VERTICAL INTEGRATION AND POST PROCESSING
step_grid=0	#Gridded LEVEL III DATA GENERATION
step_figsGen=1	#FIGS GENERATION
step_biasFigsGen=0	#BIAS FIGS GENERATION
step_dataMonitor=0	#MONITORING OF DATA QUALITY
step_clean=0	#DISK CLEANING/PURGING


####################################################################################################
#
#
#   S E C T I O N  OF  C O N T R O L L I N G  F L A G S
#
#
####################################################################################################

processMode=1	#0:Orbit processing  1:Daily processing
sensorId=9	#1:N18,2:MetopA,3:F16,4:N19,5:F18,6:NPP,7:AMSRE,8:FY3-MWRI,9:TRMM/TMI
outFMAccuracy=0	#Flag to output of the FM accuracy metric (DeltaTB @89)
prefixFMAccuracy=QCcheck	#Prefix of file(s) w FM-acuracy metric (only if outFMaccur=1)
nProfs2Retr=1000000	#Maximum number of profiles to process in retrieval
nProfs2Fwd=1000000	#Maximum number of profiles to simulate using the fwd operator (over analyses)
nAttempts=2	#Number of retrieval attempts in case of non-convergence 
fmType=1	#POES:0(low),1(high); DMSP:0(UAS-lowest),1(LAS),2(ENV),3(IMG-highest); TRMM:-1(coarse),0(low),1(high)
addDeviceNoise=0	#=1 Flag to add noise to the fwd simulations (over analyses), =0->no Noise added 
monitorIterative=0	#=1->Yes, monitor iterative process, 	     =0-> Do not
monitorRetrieval=0	#=1->Yes, on-screen-monitoring of retrieval ,  =0-> Do not.
monitorFwd=0	#=1->Yes, on-screen-monitoring of fwd-simul ,  =0-> Do not.
externalDataAvailable=1	#=1->Ext data available, use ExtDataUse =0-> No Ext data available,
externalDataSrc=2	#Source of external Data (only if externDataAvailable=1). =1-> ANALYS, =2->REGRESS
nwpGdasUse=1	#=1->To Use GDAS in NWP Collocation,  =0-> Not Use GDAS
nwpEcmwfUse=0	#=1->To Use ECMWF in NWP Collocation, =0-> Not Use ECMWF
nwpGfsUse=0	#=1->To Use GFS in NWP Collocation,  =0-> Not Use GFS
geoLimit=1	#=0-> Process all data. =1-> only in the lat/lon box. 2->Only ocean, 3->Only land
minLat=20.	#Min latitude of data to be processed (only if GeogrLimits=1)
maxLat=40.	#Max latitude of data to be processed (only if GeogrLimits=1)
minLon=-110.	#Min longitude of data to be processed (only if GeogrLimits=1)
maxLon=-70.	#Max longitude of data to be processed (only if GeogrLimits=1)
cend=2	#Orbit(s) 2 process: 0:ascending, 1:descending, 2:both
nDaysBack=2	#TB-Bias assessed w. data from NdayBack, due to reference data delay. NdayBack between 0 & mxDaysAllowed-1
maxDaysArchived=0	#Max number of days allowed for archiving (purged regularily)
dayUsed4Bias=2010_07_20	#Extension used to determine which bias to use in the ec process.
dayUsed4Alg=2010_07_20	#Extension used to determine which algorithms to use in the externDataFromRegress step
nOrbits2Process=1000000	#Number of orbits/suborbits to process: overwrites the existing number of orbits
tdrFormat=1	#Format of TDR (depends on the RDR decoder at hand):0->ascii, 1->binary
gifDensity=100	#density used when converting PS files into Gif images
gridFactor=4	#grid factor used when gridding level III data
nScanLineSensor1Skip=-99	#Number of sensor1 scan lines to skip upfront (to accomodate geolocation issues) 
nScanLineSensor2Skip=-99	#Number of sensor1 scan lines to skip upfront (to accomodate geolocation issues) 
scanLineIndexSensor2TimeColloc=-99	#Sensor2 ScanLine index (1,2 or 3) that corresponds in time to sensor1
fwdCloudOffOrOn=0	#Set Hydrometeors to Zero or retain values (FWD step only).  0->Set to Zero, 1->Retain
biasComputeMethod=1	#Method for computing the bias. 0->Simple bias, 1->Histogram adjustment
regressionBiasApplyDomain=-2	#Domain of application of bias(regression).-2 nowhere, -1->everywhere
nChoppedFilesPerOrbit=10	#Number of chopped sub-orbits per orbit. If 1 no chopping is done
retrOnOrbitOrSubOrbit=0	#Switches between performing retr on full-orbits (0) or chopped ones (1)
retrOnWhichSDR=1	#Switches between retr on uncorrected TBs (1) or NWP-based simuls(2)
fwdMatrix2Use=0	#Switches fwd model error matrix (0:dynamically generated by compar with simul, 1:Non-error)
makeOrNot=0	#Switches between making (1) the executables on the fly or not (0)
useCPU=1	#0->Use one CPU; 1->Use all CPUs; 2->Use QSUB
makeClean=0	#During cleaning step, do we want make-clean as well (=1) or not (=0)
