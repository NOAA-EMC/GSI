#!/bin/bash

#------Store arguments & set starting date
script=$0
oldargs=("$@")
nargs=$#
cd /disk1/pub/mirs_oper/scripts
sdate=`date`
MIRS_ROOT=`grep ^MIRS_ROOT ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`
HDF4LIB=`grep HDF4LIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`
HDF5LIB=`grep HDF5LIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
HDFEOSLIB=`grep HDFEOSLIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`
SZIPLIB=`grep SZIPLIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`
ZLIBLIB=`grep ZLIBLIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`
NETCDF4LIB=`grep NETCDF4LIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ \t]*//'`


#-----Include libraries and setup Info
. ../scripts/script_functions.bash
. ../setup/npp_pcf_daily.bash
# Add more colon-separated shared libraries here:
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB
# set stack size to unlimited
ulimit -s unlimited

extResol=`determineSpatialResol ${satId} ${fmType}`

version='9999'
if [[ -s ${rootPath}/version.txt ]] ; then
    version=`cat ${rootPath}/version.txt`
fi

os=`uname -s`
#Linux g95 and gfortran is the same as AIX xlf90/95(stream,unformatted)
#Linux ifort is (sequential,binary)
accessStr='sequential'
formStr='binary'
if [[ ${os} == 'Linux' ]] ; then
  accessStr='sequential'
  formStr='binary'
elif [[ ${os} == 'AIX' ]] ; then
  accessStr='stream'
  formStr='unformatted'
fi

gdasData=1
ecmwfData=2
gfsData=3

#----Construct date extension(s) for building directories
if [[ ${nargs} -eq 0 ]] ; then
   extensions=`DetermineYesterdExtAndAlanysExt ${nDaysBack}`
   date=$(echo ${extensions}|cut -c1-10)
   rdrSensor1Dir=${rdrSensor1Path}/${date}
   rdrSensor2Dir=${rdrSensor2Path}/${date}
elif [[ ${nargs} -eq 1 ]] ; then
   #----Check 3 different paths (absolute/relative/a nude dir name)
   indx_slash=`echo ${oldargs} | awk -v slash="/" '{printf index($1,slash)}'`
   if [[ ${indx_slash} -eq 1 ]] ; then # =1: absolute path
     inputPath=${oldargs}
   elif [[ ${indx_slash} -gt 1 ]] ; then # >1: relative path
     inputPath=$(readlink -f ${oldargs}) # to get absolute full path
   else # <1: a nude dir name with no slash in it
     inputPath=${rdrSensor1Path}/${oldargs}
   fi
   date=`getDateFromFile ${satId} ${inputPath}`
   if [[ ${date} == 'xxxx-xx-xx' ]] ; then # something wrong
     op_msg "Error: date $date is incorrect"
     exit 1
   fi
   rdrSensor1Dir=${inputPath}
   rdrSensor2Dir=${inputPath}
   extensions=`DetermineExtAndAlanysExtFromArgument ${date}`
else
   ErrMessDue2UsageInDailyMode
fi
rdirExt=$(echo ${extensions}|cut -c1-10)
rdirAnalysExt=$(echo ${extensions}|cut -c12-21)
rdirNextAnalysExt=$(echo ${extensions}|cut -c23-32)
#----set the generic extension
fileExt=`determineFileExt ${rdirExt}`
fileAnalysExt=`determineFileExt ${rdirAnalysExt}`
orbitInfo=Dummy

#----Names of directories for both orbital and Daily processing
tdrSensor1Dir=${tdrSensor1Path}/${rdirExt}
tdrSensor2Dir=${tdrSensor2Path}/${rdirExt}
sdrSensor1Dir=${sdrSensor1Path}/${rdirExt}
sdrSensor2Dir=${sdrSensor2Path}/${rdirExt}
fmsdrDir=${fmsdrPath}/${rdirExt}
fmsdrChoppDir=${choppPath}/${rdirExt}
edrDir=${edrPath}/${rdirExt}
depDir=${depPath}/${rdirExt}
gridDir=${gridPath}/${rdirExt}
ncDir=${ncPath}/${rdirExt}
figsDir=${figsPath}/${rdirExt}
regressRetrDir=${regressRetrPath}/${rdirExt}
orbitMonPath=${perfsMonitorPath}/orbitmon/
#----control file and input file identifiers depending on processing mode
if [[ ${processMode} -eq 0 ]] ; then
  identifier=${orbitInfo}
else
  identifier=${rdirExt}
fi
#----Control file
rdr2tdrSensor1ControlFile=${rdr2tdrSensor1ControlFile}_${identifier}.in
rdr2tdrSensor2ControlFile=${rdr2tdrSensor2ControlFile}_${identifier}.in
mergeNedtControlFile=${mergeNedtControlFile}_${identifier}.in
tdr2sdrSensor1ControlFile=${tdr2sdrSensor1ControlFile}_${identifier}.in
tdr2sdrSensor2ControlFile=${tdr2sdrSensor2ControlFile}_${identifier}.in
fmControlFile=${fmControlFile}_${identifier}.in
modifyNedtControlFile=${modifyNedtControlFile}_${identifier}.in
fmsdr2edrControlFile=${fmsdr2edrControlFile}_${identifier}.in
grid2nwpControlFile=${grid2nwpControlFile}_${identifier}.in
fwdControlFile=${fwdControlFile}_${identifier}.in
regressControlFile=${regressControlFile}_${identifier}.in
choppControlFile=${choppControlFile}_${identifier}.in
mergeEdrControlFile=${mergeEdrControlFile}_${identifier}.in
vippControlFile=${vippControlFile}_${identifier}.in
gridControlFile=${gridControlFile}_${identifier}.in
nwpGridControlFile=${nwpGridControlFile}_${identifier}.in
fwdGridControlFile=${fwdGridControlFile}_${identifier}.in
biasGridControlFile=${biasGridControlFile}_${identifier}.in
biasCompuControlFile=${biasCompuControlFile}_${identifier}.in
biasVerifControlFile=${biasVerifControlFile}_${identifier}.in
regressGenControlFile=${regressGenControlFile}_${identifier}.in
figsGenControlFile=${figsGenControlFile}_${identifier}.in
#---- Input file list
rdrSensor1List=${rdrSensor1List}_${identifier}.list
rdrSensor2List=${rdrSensor2List}_${identifier}.list
tdrSensor1List=${tdrSensor1List}_${identifier}.list
tdrSensor2List=${tdrSensor2List}_${identifier}.list
sdrSensor1List=${sdrSensor1List}_${identifier}.list
sdrSensor2List=${sdrSensor2List}_${identifier}.list
fmsdrList=${fmsdrList}_${identifier}.list
fmsdr4BiasList=${fmsdr4BiasList}_${identifier}.list
fmsdr4ChoppList=${fmsdr4ChoppList}_${identifier}.list
fmsdr4NwpList=${fmsdr4NwpList}_${identifier}.list
fmsdr4BiasList=${fmsdr4BiasList}_${identifier}.list
fmsdr4RegressList=${fmsdr4RegressList}_${identifier}.list
fmsdr4ApplyRegressList=${fmsdr4ApplyRegressList}_${identifier}.list
edrList=${edrList}_${identifier}.list
edr4BiasList=${edr4BiasList}_${identifier}.list
dep4BiasList=${dep4BiasList}_${identifier}.list
edr4MergeList=${edr4MergeList}_${identifier}.list
depList=${depList}_${identifier}.list
nedtList=${nedtList}_${identifier}.list
nedtSensor1List=${nedtSensor1List}_${identifier}.list
nedtSensor2List=${nedtSensor2List}_${identifier}.list
gridSfcNwpAnalysList=${gridSfcNwpAnalysList}_${identifier}.list
gridAtmNwpAnalysList=${gridAtmNwpAnalysList}_${identifier}.list
nwpAnalysList=${nwpAnalysList}_${identifier}.list
nwpAnalysRetrList=${nwpAnalysRetrList}_${identifier}.list
nwpAnalys4BiasList=${nwpAnalys4BiasList}_${identifier}.list
nwpAnalys4RegressList=${nwpAnalys4RegressList}_${identifier}.list
fwdAnalys4BiasList=${fwdAnalys4BiasList}_${identifier}.list

#----Names of directories exclusively for Daily processing
figs4BiasDir=${perfsMonitorPath}/${rdirAnalysExt}
qcCheckDir=${perfsMonitorPath}/${rdirExt}
figs4RegressDir=${perfsMonitorPath}/${rdirAnalysExt}
nwpAnalysDir=${nwpAnalysPath}/${rdirAnalysExt}
fwdAnalysDir=${fwdAnalysPath}/${rdirAnalysExt}
fwd2hdf5Dir=${externalDataPath}/rdr/npp_atms/${rdirAnalysExt}
fmsdr4BiasDir=${fmsdrPath}/${rdirAnalysExt}
edr4BiasDir=${edrPath}/${rdirAnalysExt}
dep4BiasDir=${depPath}/${rdirAnalysExt}
grid4BiasDir=${gridPath}/${rdirAnalysExt}
#----Names of the files dynamically generated
sensor1Nedt=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${fileExt}.dat
sensor2Nedt=${nedtSensor2Path}/${satId}_${sensor2}_nedt_${fileExt}.dat
nedtExt=${fileExt}
if [[ ${satId} == "f16" || ${satId} == "aqua" || ${satId} == "f18" || ${satId} == "fy3ri" || ${satId} == "trmm" || ${satId} == "gpm" ]] ; then
  nedtBefFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${fileExt}_befFM.dat
  nedtAftFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${fileExt}_aftFM.dat
  nedtDir=${nedtSensor1Path}/${rdirExt}
elif [[ ${satId} == "npp" ]]; then
  nedtExt=`determineNedtExt ${satId} ${rdrSensor1Dir} ${rdrType}`
  nedtBefFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${nedtExt}_befFM.dat
  nedtAftFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${nedtExt}_aftFM.dat
  nedtDir=${nedtSensor1Path}/${rdirExt}
else
  nedtBefFMFile=${nedtSensor1Sensor2Path}/${satId}_${sensor1}_${sensor2}_nedt_${fileExt}_befFM.dat
  nedtAftFMFile=${nedtSensor1Sensor2Path}/${satId}_${sensor1}_${sensor2}_nedt_${fileExt}_aftFM.dat
  nedtDir=${nedtSensor1Sensor2Path}/${rdirExt}
fi
sensor1Wt=${nedtSensor1Path}/${satId}_${sensor1}_wt_${fileExt}.dat
sensor2Wt=${nedtSensor2Path}/${satId}_${sensor2}_wt_${fileExt}.dat
sensor1Sensor2Wt=${nedtSensor1Sensor2Path}/${satId}_${sensor1}_${sensor2}_wt_${fileExt}.dat
modelErrFile=${biasPath}/ModelErrFile_${satId}_${fileExt}.dat	      #Model err file that will be generated
if [[ ${satId} == "npp" ]] ; then
  modelErrFile=${biasPath}/ModelErrFile_${satId}_${nedtExt}.dat
fi
logFile=${logFile}_${fileExt}.dat
#----Names of files dynamically generated only for daily processing 
biasFile=${biasPath}/biasCorrec_${satId}_${fileAnalysExt}.dat	      #bias file that will be generated
if [[ ${satId} == "npp" ]] ; then
  biasFile=${biasPath}/biasCorrec_${satId}_${nedtExt}.dat
fi
biasCheckFile=${biasPath}/biasAfterCorr_${satId}_${fileAnalysExt}.dat #bias residual file
#----EDR/DEP/NWP grid/p2p products list
edrGridStr="angle,chisq,em,nattempt,niter,psfc,qc,scanday,scanpos,sfc,tbf,tbu,tbc,tbl,temp,tskin,wv,clwp,rainp,graupelp,pressure"
edrP2PStr="em,scanpos,sfc,tbu,tskin,wspd,temp,wv,psfc"
depGridStr="clw,gs,iwp,lwp,rr,rwp,sfc2,sice,sicefy,sicemy,snow,swe,tpw"
depP2PStr="clw,iwp,lat,lwp,rr,rwp,sfc2,swe,tpw"
nwpGridStr="angle,chisq,clw,em,iwp,nattempt,niter,psfc,rr,scanday,scanpos,sfc,swe,tbf,tbu,tbc,tbl,temp,tpw,tskin,wv"
nwpP2PStr="clw,em,iwp,lwp,rwp,sfc,swe,tbu,tpw,tskin,temp,wv,psfc"
if [[ ${satId} == "f16" || ${satId} == "f18" || ${satId} == "trmm" || ${satId} == "gpm" ]] ; then
  depGridStr="clw,gs,iwp,lwp,rr,rwp,sfc2,sice,sicefy,sicemy,snow,swe,tpw,wspd"
  depP2PStr="clw,iwp,lat,lwp,rr,rwp,sfc2,swe,tpw,wspd"
  nwpGridStr="angle,chisq,clw,em,iwp,nattempt,niter,psfc,rr,scanday,scanpos,sfc,swe,tbf,tbu,tbc,tbl,temp,tpw,tskin,wspd,wv"
  nwpP2PStr="clw,em,iwp,lwp,rwp,sfc,swe,tbu,tpw,tskin,temp,wspd,wv,psfc"
fi

#---- fwd model error matrix
modelErrFile1ToUse=${biasPath}/ModelErrFile_${satId}.dat
modelErrFile2ToUse=${biasPath}/ModelErrFile_${satId}.dat

#---Check existence of the directories. If negative, make them
DirGen ${tdrSensor1Dir} "TDR-${sensor1}"
if [[ $sensor2 != "dummy" ]] ; then 
  DirGen ${tdrSensor2Dir} "TDR-${sensor2}"
fi
DirGen ${sdrSensor1Dir} "SDR-${sensor1}"
if [[ $sensor2 != "dummy" ]] ; then 
  DirGen ${sdrSensor2Dir} "SDR-${sensor2}"
fi
if [[ ${satId} == "npp" ]] ; then
  DirGen ${nedtDir} "NEDT"
fi
DirGen ${fmsdrDir} "FM-SDR"
DirGen ${fmsdrChoppDir} "CHOPPED FMSDRs"
DirGen ${edrDir} "EDR"
DirGen ${depDir} "DEP"
DirGen ${gridDir} "GRID"
DirGen ${ncDir} "NETCDF4"
DirGen ${figsDir} "FIGS"
DirGen ${figs4BiasDir} "FIGS"
DirGen ${grid4BiasDir} "GRID-BIAS"
DirGen ${qcCheckDir} "QCCheck"
DirGen ${figs4RegressDir} "regr-FIGS"
DirGen ${nwpAnalysDir} "NWP-ANALYSIS"
DirGen ${fwdAnalysDir} "FWD SIMUL on Analyses"
DirGen ${orbitMonPath} "QC Monitor"
DirGen ${regressRetrDir} "Regression-Based Retrievals"


#--------------------------------------------------------------------------------
#      NPP/ATMS special case: to generate from SDR files 
#--------------------------------------------------------------------------------
if [[ "${step_rdr2tdrSensor1}" -eq 1 && ${satId} == "npp" && ${rdrType} -eq 1 ]] ; then
  rdrType2=3
  nfile_gatmo=`ls -1 ${rdrSensor1Dir}/GATMO_npp* | wc -l`
  nfile_satms=`ls -1 ${rdrSensor1Dir}/SATMS_npp* | wc -l`
  #---- only do this when SDR files exist and have equal numer of GATMO files
  if [[ ${nfile_satms} -ge 1 && ${nfile_gatmo} -eq ${nfile_satms} ]] ; then
  rdr2tdr ${rdrSensor1Dir} ${rdrSensor1List} ${sensor1} ${tdrSensor1Dir} ${sensor1Nedt}\
  ${instrumentSensor1File} ${sensor1Wt} ${nOrbits2Process} ${logFile}\
  ${rdr2tdrSensor1ControlFile} ${rdr2tdrSensor1Src} ${makeOrNot} ${binPath}\
  ${processMode} ${orbitInfo} ${satId} "${rdirExt}" ${controlDataPath}\
  ${calibBiasFitFile} ${calibDTRlutFile} ${accessStr} ${formStr} ${rdrType2}
  #---- clean up ( only need remove TDR to avoid conflict )
  rm -f ${tdrSensor1Dir}/TDR*
  fi
fi

#--------------------------------------------------------------------------------
#      step: RDR to TDR conversion (sensor1)
#--------------------------------------------------------------------------------
if [[ "${step_rdr2tdrSensor1}" -eq 1 ]] ; then
  rdr2tdr ${rdrSensor1Dir} ${rdrSensor1List} ${sensor1} ${tdrSensor1Dir} ${sensor1Nedt}\
  ${instrumentSensor1File} ${sensor1Wt} ${nOrbits2Process} ${logFile}\
  ${rdr2tdrSensor1ControlFile} ${rdr2tdrSensor1Src} ${makeOrNot} ${binPath}\
  ${processMode} ${orbitInfo} ${satId} "${rdirExt}" ${controlDataPath}\
  ${calibBiasFitFile} ${calibDTRlutFile} ${accessStr} ${formStr} ${rdrType}
fi

#--------------------------------------------------------------------------------
#      step: Merge AMSUA & MHS NEDT files into a single file
#--------------------------------------------------------------------------------
if [[ "${step_mergeNedt}" -eq 1 ]] ; then
  mergeNedt ${sensor1Nedt} ${sensor2Nedt} ${nedtBefFMFile} ${logFile}\
  ${mergeNedtControlFile} ${mergeNedtSrc} ${makeOrNot} ${binPath} ${satId}\
  "${nedtNominalFile}" "${nedtDir}"
fi

#--------------------------------------------------------------------------------
#      step: TDR to SDR conversion (sensor1)
#--------------------------------------------------------------------------------
if [[ "${step_tdr2sdrSensor1}" -eq 1 ]] ; then
  tdr2sdr ${tdrSensor1Dir} ${tdrSensor1List} ${sensor1} ${tdrFormat} ${sdrSensor1Dir}\
  ${antennaSensor1File} ${nOrbits2Process} ${logFile} ${tdr2sdrSensor1ControlFile}\
  ${makeOrNot} ${binPath} ${processMode} ${orbitInfo} ${tdr2sdrSrc}
fi

#--------------------------------------------------------------------------------
#      step: Footprint Matching (FM) sensor1/sensor2
#--------------------------------------------------------------------------------
if [[ "${step_fm}" -eq 1 ]] ; then
  fm ${sdrSensor1Dir} ${sdrSensor1List} ${sdrSensor2Dir} ${sdrSensor2List}\
  ${sensor1} ${sensor2} ${fmsdrDir} ${outFMAccuracy} ${perfsMonitorPath}\
  ${prefixFMAccuracy} ${fmType} ${nScanLineSensor1Skip} ${nScanLineSensor2Skip}\
  ${scanLineIndexSensor2TimeColloc} ${nOrbits2Process} ${logFile} ${fmControlFile}\
  ${fmSrc} ${makeOrNot} ${binPath} ${processMode} ${orbitInfo} ${satId}\
  ${nedtBefFMFile} ${nedtAftFMFile} ${modifyNedtControlFile} ${figs4BiasDir}\
  ${identifier} ${outFMAccuracy} ${inputDataPath} ${controlDataPath}\
  ${geoLimit} ${minLat} ${maxLat} ${minLon} ${maxLon}
fi

#--------------------------------------------------------------------------------
#      step: Apply regression-based algorithms on FM-SDR radiances
#--------------------------------------------------------------------------------
if [[ "${step_externalDataFromRegress}" -eq 1 ]] ; then
  applyRegress ${fmsdrDir} "${extResol}" ${fmsdr4ApplyRegressList} ${regressRetrDir}\
  ${topographyFile} ${covBkgAtm1File} ${covBkgSfc1File} ${logFile} ${regressControlFile}\
  ${applyRegressAlgSrc} ${makeOrNot} ${binPath} ${processMode} ${orbitInfo}\
  ${regressCoeffOceanClwFile}   ${regressCoeffSeaIceClwFile}   ${regressCoeffLandClwFile}   ${regressCoeffSnowClwFile}\
  ${regressCoeffOceanTskinFile} ${regressCoeffSeaIceTskinFile} ${regressCoeffLandTskinFile} ${regressCoeffSnowTskinFile}\
  ${regressCoeffOceanTpwFile}   ${regressCoeffSeaIceTpwFile}   ${regressCoeffLandTpwFile}   ${regressCoeffSnowTpwFile}\
  ${regressCoeffOceanEmFile}    ${regressCoeffSeaIceEmFile}    ${regressCoeffLandEmFile}    ${regressCoeffSnowEmFile}\
  ${regressCoeffOceanWvFile}    ${regressCoeffSeaIceWvFile}    ${regressCoeffLandWvFile}    ${regressCoeffSnowWvFile}\
  ${regressCoeffOceanTempFile}  ${regressCoeffSeaIceTempFile}  ${regressCoeffLandTempFile}  ${regressCoeffSnowTempFile}\
  ${regressCoeffOceanGwpFile}   ${regressCoeffSeaIceGwpFile}   ${regressCoeffLandGwpFile}   ${regressCoeffSnowGwpFile}\
  ${retrOnOrbitOrSubOrbit} ${fmsdrChoppDir} ${sensorId} ${nOrbits2Process} ${biasFileToUse} ${tune1File} ${version}
fi

#--------------------------------------------------------------------------------
#      step: EDRs retrieval from the FM-SDRs
#--------------------------------------------------------------------------------
if [[ "${step_fmsdr2edr}" -eq 1 ]] ; then
  fmsdr2edr ${fmsdr2edrSrc} ${makeOrNot} ${retrOnWhichSDR} ${fmsdrDir}\
  ${fwdAnalysDir} ${retrOnOrbitOrSubOrbit} ${fmsdrChoppDir} ${fmsdrList}\
  ${nOrbits2Process} ${externalDataAvailable} ${externalDataSrc} ${nwpAnalysDir}\
  ${nwpAnalysRetrList} ${regressRetrDir} ${edrDir} ${nProfs2Retr}\
  ${monitorIterative} ${nAttempts} ${monitorRetrieval} ${geoLimit}\
  ${minLat} ${maxLat} ${minLon} ${maxLon} ${cend} ${sensorId}\
  ${tune1File} ${tune2File} ${covBkgAtm1File} ${covBkgAtm2File}\
  ${covBkgSfc1File} ${covBkgSfc2File} ${modelErrFile1ToUse} ${modelErrFile2ToUse} \
  ${nedtAftFMFile} ${monitorFile} ${topographyFile}\
  ${spcCoeffFile} ${tauCoeffFile} ${cldOptPropFile} ${logFile}\
  ${fmsdr2edrControlFile} ${useCPU} ${processMode} ${orbitInfo} ${binPath}\
  ${biasFileToUse} ${version} "${extResol}" "${rdirExt}"
fi

#--------------------------------------------------------------------------------
#      step: To generate vertical integrated products
#--------------------------------------------------------------------------------
if [[ "${step_vipp}" -eq 1 ]] ; then
  vipp ${edrList} ${edrDir} ${depDir}  ${logFile} ${nOrbits2Process} ${nProfs2Retr}\
  ${sensorId} ${processMode} ${orbitInfo} "${extResol}" ${vippControlFile}\
  ${vippSrc} ${binPath} ${siceEmissCatalogFile} ${snowEmissCatalogFile}
fi

#--------------------------------------------------------------------------------
#      step: To generate gridded products
#--------------------------------------------------------------------------------
if [[ "${step_grid}" -eq 1 ]] ; then
  gridGen_parallel ${processMode} ${edrDir} ${depDir} ${orbitInfo} ${edrList} ${depList}\
  "${extResol}" ${satId} ${fileExt} ${gridSrc} ${gridDir} ${makeOrNot} ${binPath}\
  ${gridControlFile} ${gridFactor} ${minLat} ${maxLat} ${minLon} ${maxLon}\
  ${fmType} "${edrGridStr}" "${edrP2PStr}" "${depGridStr}" "${depP2PStr}"
fi


#--------------------------------------------------------------------------------
#      step: To convert EDR & DEP into netcdf4 format
#--------------------------------------------------------------------------------
if [[ "${step_nc}" -eq 1 ]] ; then
  mirs2nc_parallel ${edrDir} ${depDir} ${ncDir} ${binPath} ${satId}
fi

#--------------------------------------------------------------------------------
#      step: EDRs Figures Generation
#--------------------------------------------------------------------------------
if [[ "${step_figsGen}" -eq 1 ]] ; then
  figsGen_parallel ${satId} ${gridFactor} ${gridDir} ${gridSrc} ${figsDir} ${IDL}\
  ${identifier} ${figsGenControlFile} ${processMode} ${controlDataPath} ${version}\
  ${minLat} ${maxLat} ${minLon} ${maxLon}
fi

qcRetrieval ${depDir} ${depList} ${orbitMonPath} ${IDL} ${gridSrc} \
  ${controlDataPath}/qcRetrieval_abnormal_${satId}_${fileExt} \
  ${controlDataPath}/qcRetrieval_namelist_${satId}_${fileExt} \
  "${email}" "${website}" ${controlDataPath} ${satId} "${rdirExt}" ${binPath}

qcAvg ${satId} ${rdirExt} ${orbitMonPath} ${controlDataPath}

dataQualityMonitor ${nedtSensor1Path} ${nedtList} ${nedtMonitorSrc} ${IDL} \
  ${orbitMonPath} ${figsDir} ${processMode} ${fileExt} ${satId} ${controlDataPath} ${perfsMonitorPath}

cp ${figsDir}/nedtLife*png  /net/www/www/smcd/mirs/images/${satId}/nedt/
cp ${figsDir}/nedtMonth*png /net/www/www/smcd/mirs/images/${satId}/nedt/
cp ${perfsMonitorPath}/*orbit_monitoring*.png /net/www/www/smcd/mirs/images/${satId}/dataquality/


mkdir -p /net/www/www/smcd/mirs/images/${satId}/${identifier}
find ${figsDir} -name "*.png"  -exec cp {} /net/www/www/smcd/mirs/images/${satId}/${identifier} \;

emspectrum ${satId} ${rdirExt} ${figsDir} ${controlDataPath} ${figsGenControlFile} \
  ${edrList} ${gridSrc} ${version} ${IDL} ${gridDir} ${binPath}
cp ${figsDir}/*_emspectrum_*png /net/www/www/smcd/mirs/images/${satId}/${identifier}


#copy edr/dep/grid/fmsdr to archive
archivePath=/disk1/pub/archive
mkdir -p ${archivePath}/edr/${satId}_atms/${rdirExt}
mkdir -p ${archivePath}/dep/${satId}_atms/${rdirExt}
mkdir -p ${archivePath}/grid/${satId}_atms/${rdirExt}
cp ${edrDir}/* ${archivePath}/edr/${satId}_atms/${rdirExt}
cp ${depDir}/* ${archivePath}/dep/${satId}_atms/${rdirExt}
cp ${gridDir}/GRID*  ${archivePath}/grid/${satId}_atms/${rdirExt}

#mkdir -p ${archivePath}/fmsdr/${satId}_atms/${rdirExt}
#cp ${fmsdrDir}/FMSDR_*  ${archivePath}/fmsdr/${satId}_atms/${rdirExt}

# remove *orbit_monitoring*.png older than 7 days
find ${perfsMonitorPath} -maxdepth 1 -name "*.png" -mtime +7 -exec rm {} \;

# copy netCDF4 data to ftp site
ftpPath=/net/www/aftp/pub/corp/scsb/mirs/data/nc/npp_atms/${rdirExt}
mkdir -p ${ftpPath}
cp ${ncDir}/* ${ftpPath}
PathDel=/net/www/aftp/pub/corp/scsb/mirs/data/nc/npp_atms
#find ${PathDel} -maxdepth 1 -name "${rdirExt}" -mtime +7 -exec rm -rf {} \;

#find ${PathDel} -maxdepth 1 -mtime +7 -exec rm -rf {} \;
# temporary fix for /net/www/aftp/pub/corp/scsb/mirs/data/nc/npp_atms/2012-02-01/, which is owned by kgarrett
find ${PathDel} -maxdepth 1 -mtime +7  -user wchen -exec rm -rf {} \;


GO_WEB=1

if [[ ${GO_WEB} -eq 1 ]] ; then
  mkdir -p /net/www/aftp/pub/corp/scsb/mirs/data/dep/npp_atms/${rdirExt}
  echo "copy DEP to /net/www/aftp/pub/corp/scsb/mirs/data/dep/npp_atms/${rdirExt}"
  cp ${depDir}/* /net/www/aftp/pub/corp/scsb/mirs/data/dep/npp_atms/${rdirExt}
fi
