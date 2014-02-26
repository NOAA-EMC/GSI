#!/bin/bash

#################################################################################################################
#
# Description:
#        This is the GUI generated bash script used to run the MIRS testbed.
#
# Record of revisions:
#           Date        Ver.        Programmer               Description of change
#       ============   =====   =======================   ============================================
#        09/03/2005     v0      Sid-Ahmed Boukabara       Original script created
#                               (NOAA/NESDIS/ORA/IMSG)
#
#        02/20/2006     v1      Ninghai Sun               Modify script for operational testbed
#                               (NOAA/NESDIS/ORA/IMSG)
#
#        03/20/2006     v2      Ninghai Sun               Modify script to compromise to SSM/IS
#                               (NOAA/NESDIS/ORA/IMSG)    Change the way to find GDAS data to standard
#
#        03/31/2006     v3      Sid Ahmed Boukabara       Changes related to :
#                                                         (1) new footprint-matching, 
#                                                         (2) addition of new covariance matrix
#                                                         (3) sensor ID (to distinguish sensor-dependent classifiers),
#                                                         (4) flexible handling of scanlines shift (mhs vs amsu)
#                                                         (5) bias correction method (bias removal or slope/intercept)
#                                                         (6) Added threshold checking of relative humidity
#        05/31/2006     v4      Sid Ahmed Boukabara       Changes related to directory structure more in line with oper.
#
#
#        12/19/2006     v5      Sid Ahmed Boukabara       Added the capability to run the scripts in
#                              (IMSG@NOAA/NESDIS/STAR)    orbital mode in addition to daily mode.
#
#        12/22/2006     v6      Sid Ahmed Boukabara       Major changes to make the script as simple as possible to 
#                                                         maintain and ultimately to be generated through the JAVA GUI.
#
#        12/28/2006     v7      Sid Ahmed Boukabara       Extensive revision to make all functions general.
#                              (IMSG@NOAA/NESDIS/STAR)    Scripts to be generated automatically through a Java GUI.
#                                                         This removes the need to maintain as many scripts as sensors.
#        03/10/2010     v8      Wanchun Chen              Many functions previously in script are moved into library.
#                              (QSS/PSGS/DELL)
#
#
#################################################################################################################

#------Store arguments & set starting date
script=$0
oldargs=("$@")
nargs=$#
sdate=`date`
MIRS_ROOT=`grep ^MIRS_ROOT ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
HDF4LIB=`grep HDF4LIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
HDF5LIB=`grep HDF5LIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
HDFEOSLIB=`grep HDFEOSLIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
SZIPLIB=`grep SZIPLIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
ZLIBLIB=`grep ZLIBLIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
NETCDF4LIB=`grep NETCDF4LIB ../setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`


#-----Include libraries and setup Info
. ../scripts/script_functions.bash
. ../setup/f18_pcf_benchmark_lr.bash
# Add more colon-separated shared libraries here:
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB:$HDF5LIB:$NETCDF4LIB
# set stack size to unlimited
ulimit -s unlimited

rdrSensor1Path=${rdrOrbitPath}
rdrSensor2Path=${rdrOrbitPath}

displayVerif ${script} ${logFile} ${processMode} ${sensorId}\
	${outFMAccuracy} ${prefixFMAccuracy} ${nProfs2Retr} ${nProfs2Fwd}\
	${addDeviceNoise} ${monitorIterative} ${nAttempts}\
	${externalDataAvailable} ${monitorRetrieval} ${monitorFwd} ${geoLimit}\
	${minLat} ${maxLat} ${minLon} ${maxLon} ${maxDaysArchived} ${nDaysBack}\
	${tdrFormat} ${cend} ${dayUsed4Bias} ${dayUsed4Alg} ${nOrbits2Process}\
	${gifDensity} ${externalDataSrc} ${fmType} ${biasComputeMethod} \
	${nChoppedFilesPerOrbit} ${retrOnOrbitOrSubOrbit} ${retrOnWhichSDR}\
	${fwdMatrix2Use} ${makeOrNot} ${useCPU}

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

if [[ ${nargs} -ne 1 ]] ; then
   ErrMessDue2UsageInOrbitalMode $0 "Example: $0 NSS.AMBX.NN.D04260.S1110.E1256.B2054344.GC"
elif [[ ${satId} == "f16" || ${satId} == "f18" || ${satId} == "fy3ri" ]] ; then
   SanityChecksOrbitProcess1 $1 ${rdrSensor1Path}
   orbitInfo=`ExtractRdrFileNamesfromOrbit1 $1 ${rdrSensor1Path}`
elif [[ ${satId} == "npp" ]] ; then
   SanityChecksOrbitProcess1 $1 ${rdrSensor1Path}
   orbitInfo=`ExtractRdrFileNamesfromOrbitNpp $1 ${rdrSensor1Path}`
elif [[ ${satId} == "trmm" ]] ; then
   SanityChecksOrbitProcess1 $1 ${rdrSensor1Path}
   orbitInfo=`ExtractRdrFileNamesfromOrbitTrmm $1 ${rdrSensor1Path}`
elif [[ ${satId} == "mtma" ||  ${satId} == "mtsa" ]] ; then
   SanityChecksOrbitProcess1 $1 ${rdrSensor1Path}
   orbitInfo=`ExtractRdrFileNamesfromOrbitMT $1`
else
   SanityChecksOrbitProcess2 $1 ${rdrSensor1Path} ${rdrSensor2Path}
   orbitInfo=`ExtractRdrFileNamesfromOrbit2 $1 ${rdrSensor1Path} ${rdrSensor2Path}`
fi
#----Names of directories (specific to orbital processing)
rdrSensor1Dir=${rdrSensor1Path}
rdrSensor2Dir=${rdrSensor2Path}
#----set the generic extension (of file and directory)
fileExt=${orbitInfo}
rdirExt=''
rdirAnalysExt=''
rdirNextAnalysExt=''

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
if [[ ${satId} == "f16" || ${satId} == "aqua" || ${satId} == "f18" || ${satId} == "fy3ri" || ${satId} == "trmm" || ${satId} == "gpm" || ${satId} == "mtma" || ${satId} == "mtsa" ]] ; then
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
edrP2PStr="em,scanpos,sfc,tbu,tskin,wspd,temp,wv"
depGridStr="clw,gs,iwp,lwp,rr,rwp,sfc2,sice,sicefy,sicemy,snow,swe,tpw"
depP2PStr="clw,iwp,lat,lwp,rr,rwp,sfc2,swe,tpw"
nwpGridStr="angle,chisq,clw,em,iwp,nattempt,niter,psfc,rr,scanday,scanpos,sfc,swe,tbf,tbu,tbc,tbl,temp,tpw,tskin,wv"
nwpP2PStr="clw,em,iwp,lwp,rwp,sfc,swe,tbu,tpw,tskin,temp,wv"
if [[ ${satId} == "f16" || ${satId} == "f18" || ${satId} == "trmm" || ${satId} == "gpm" || ${satId} == "mtma" || ${satId} == "mtsa" ]] ; then
  depGridStr="clw,gs,iwp,lwp,rr,rwp,sfc2,sice,sicefy,sicemy,snow,swe,tpw,wspd"
  depP2PStr="clw,iwp,lat,lwp,rr,rwp,sfc2,swe,tpw,wspd"
  nwpGridStr="angle,chisq,clw,em,iwp,nattempt,niter,psfc,rr,scanday,scanpos,sfc,swe,tbf,tbu,tbc,tbl,temp,tpw,tskin,wspd,wv"
  nwpP2PStr="clw,em,iwp,lwp,rwp,sfc,swe,tbu,tpw,tskin,temp,wspd,wv"
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
#      NPP/ATMS special case: to generate NEDT from SDR files
#--------------------------------------------------------------------------------
if [[ ${satId} == "npp" && ${rdrType} -eq 1 ]] ; then
  rdrType2=3
  nfile_gatmo=`ls -1 ${rdrSensor1Dir}/GATMO_npp* 2> /dev/null | wc -l`
  nfile_satms=`ls -1 ${rdrSensor1Dir}/SATMS_npp* 2> /dev/null | wc -l`
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
#      step: Merge Different Sensor NEDT files into a single file
#--------------------------------------------------------------------------------
if [[ "${step_mergeNedt}" -eq 1 ]] ; then
  mergeNedt ${sensor1Nedt} ${sensor2Nedt} ${nedtBefFMFile} ${logFile}\
  ${mergeNedtControlFile} ${mergeNedtSrc} ${makeOrNot} ${binPath} ${satId}\
  "${nedtNominalFile}"  "${nedtDir}" 
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
#      step: Chopping the FMSDR file  into pieces for faster processing
#--------------------------------------------------------------------------------
if [[ "${step_choppRadFiles}" -eq 1 ]] ; then
  chopp ${fmsdrDir} ${fmsdr4ChoppList} ${sensor1} ${fmsdrChoppDir}\
  ${nChoppedFilesPerOrbit} ${logFile} ${choppControlFile} ${choppSrc} ${makeOrNot}\
  ${binPath} ${processMode} ${orbitInfo} "${extResol}"
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
  ${retrOnOrbitOrSubOrbit} ${fmsdrChoppDir} ${sensorId}\
  ${nOrbits2Process} ${biasFileToUse} ${tune1File} ${version}
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


