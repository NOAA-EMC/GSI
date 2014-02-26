#!/bin/bash

#------Store arguments & set starting date
script=$0
oldargs=("$@")
nargs=$#
sdate=`date`


#-----Include libraries and setup Info
. ${oldargs}/npp_pcf_qc.bash
. ${rootPath}/scripts/script_functions.bash

MIRS_ROOT=`grep ^MIRS_ROOT ${rootPath}/setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
HDF4LIB=`grep HDF4LIB ${rootPath}/setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
HDF5LIB=`grep HDF5LIB ${rootPath}/setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
HDFEOSLIB=`grep HDFEOSLIB ${rootPath}/setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
SZIPLIB=`grep SZIPLIB ${rootPath}/setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
ZLIBLIB=`grep ZLIBLIB ${rootPath}/setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`
NETCDF4LIB=`grep NETCDF4LIB ${rootPath}/setup/paths | cut -f2 -d '=' | sed -e 's/^[ 	]*//'`

# Add more colon-separated shared libraries here:
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH
if [[ ${satId} == "trmm" ]] ; then
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF4LIB:$HDFEOSLIB:$SZIPLIB:$ZLIBLIB
fi

if [[ ${satId} == "npp" ]] ; then
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF5LIB:$SZIPLIB:$ZLIBLIB
fi

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
fmsdrDir=${fmsdrPath}
fmsdrChoppDir=${choppPath}/${rdirExt}
edrDir=${edrPath}
depDir=${depPath}
gridDir=${gridPath}
figsDir=${figsPath}
regressRetrDir=${regressRetrPath}
orbitMonPath=${perfsMonitorPath}/
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
fmsdr4BiasDir=${fmsdrPath}
edr4BiasDir=${edrPath}
dep4BiasDir=${depPath}
grid4BiasDir=${gridPath}
#----Names of the files dynamically generated
sensor1Nedt=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${fileExt}.dat
sensor2Nedt=${nedtSensor2Path}/${satId}_${sensor2}_nedt_${fileExt}.dat
nedtExt=${fileExt}
if [[ ${satId} == "f16" || ${satId} == "aqua" || ${satId} == "f18" || ${satId} == "fy3ri" || ${satId} == "trmm" || ${satId} == "gpm" ]] ; then
  nedtBefFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${fileExt}_befFM.dat
  nedtAftFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${fileExt}_aftFM.dat
  nedtDir=${nedtSensor1Path}/${rdirExt}
elif [[ ${satId} == "npp" ]] ; then
  tdrType=3
  nedtDir=${nedtSensor1Path}
  nedtExt=`determineNedtExt ${satId} ${rdrSensor1Dir} ${tdrType}`
  nedtBefFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${nedtExt}_befFM.dat
  nedtAftFMFile=${nedtSensor1Path}/${satId}_${sensor1}_nedt_${nedtExt}_aftFM.dat
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
logFile=${logFile}.dat
#----Names of files dynamically generated only for daily processing 
biasFile=${biasPath}/biasCorrec_${satId}_${fileAnalysExt}.dat	      #bias file that will be generated
if [[ ${satId} == "npp" ]] ; then
  biasFile=${biasPath}/biasCorrec_${satId}_${nedtExt}.dat
fi
biasCheckFile=${biasPath}/biasAfterCorr_${satId}_${fileAnalysExt}.dat #bias residual file
#----EDR/DEP/NWP grid/p2p products list
edrGridStr="angle,chisq,em,nattempt,niter,psfc,qc,scanday,scanpos,sfc,tbf,tbu,tbc,tbl,temp,tskin,wv,clwp,rainp,graupelp,pressure"
edrP2PStr="em,scanpos,sfc,tbu,tskin,temp,wv"
depGridStr="clw,gs,iwp,lwp,rr,rwp,sfc2,sice,sicefy,sicemy,snow,swe,tpw"
depP2PStr="clw,iwp,lat,lwp,rr,rwp,sfc2,swe,tpw"
nwpGridStr="angle,chisq,clw,em,iwp,nattempt,niter,psfc,rr,scanday,scanpos,sfc,swe,tbf,tbu,tbc,tbl,temp,tpw,tskin,wv"
nwpP2PStr="clw,em,iwp,lwp,rwp,sfc,swe,tbu,tpw,tskin,temp,wv"

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
DirGen ${fmsdrDir} "FM-SDR"
DirGen ${fmsdrChoppDir} "CHOPPED FMSDRs"
DirGen ${edrDir} "EDR"
DirGen ${depDir} "DEP"
DirGen ${gridDir} "GRID"
DirGen ${figsDir} "FIGS"
DirGen ${figs4BiasDir} "FIGS"
DirGen ${grid4BiasDir} "GRID-BIAS"
DirGen ${qcCheckDir} "QCCheck"
DirGen ${figs4RegressDir} "regr-FIGS"
DirGen ${nwpAnalysDir} "NWP-ANALYSIS"
DirGen ${fwdAnalysDir} "FWD SIMUL on Analyses"
DirGen ${orbitMonPath} "QC Monitor"
DirGen ${regressRetrDir} "Regression-Based Retrievals"


####################################################################################################
#
# Start user definition section here
#
####################################################################################################

# NWP Step reference data set used, select only one and comment the other 2
#refData=${gdasData}
#refData=${ecmwfData}
refData=${gfsData}

# NWP Step reference data path, select only one and comment the other 2
#refDataPath=${nwpGdasGridPath}
#refDataPath=${nwpEcmwfGridPath}
refDataPath=${nwpGfsGridPath}

# to run retreival quality step or not ( 0-no, 1-yes )
step_qcRetrieval=1

# to run NEDT monitor step or not ( 0-no, 1-yes )
# this step is only for POES, not for DMSP
step_qcNedt=1

# to run radiometric bias step or not ( 0-no, 1-yes ) 
step_qcRadioBias=1

# to run radiometric/geophysical/p2p step or not ( 0-no, 1-yes )
# This step is heavy and requires lots of resource.
step_qcRadioGeo=0

# websites of those quality stats
web_nedt=""
web_retrieval=""
web_radiobias=""
web_radiogeo=""

#---- more controlling options for qcRadioGeo step ----

# integer region code ( 0-glbal, 1-USA, 2-Europe, 3-Gulf of Mexico, 5-East China Sea )
region=0 

# to plot MIRS EDR&DEP gridded products or not ( 0-no, 1-yes )
plot_edrdep=0

# to plot NWP gridded products or not ( 0-no, 1-yes )
plot_nwp=0

# to plot FWD(radiometric) gridded products or not ( 0-no, 1-yes )
plot_fwd=0

# to plot gridded Bias/Asymmetry products or not ( 0-no, 1-yes )
plot_bias=0

# to do P2P comparison or not ( 0-no, 1-yes )
plot_p2p=0
  
   
####################################################################################################
#
# End user definition section
#
####################################################################################################

#  copy NEDT files into rdirExt where they are expected to be
cp ${nedtDir}/NEDT*.dat ${nedtDir}/${rdirExt}
nedtBefFMFile=`ls ${nedtDir}/${rdirExt}/NEDT*befFM.dat`
nedtAftFMFile=`ls ${nedtDir}/${rdirExt}/NEDT*aftFM.dat`

#--------------------------------------------------------------------------------
#      step: colocation of NWP GFS analyses with radiances (for bias)
#	note: In this step, if you get data allocate problem, you need do this:
#		ulimit -d unlimited  ( on IBM or Linux )
#	Or	limit datasize unlimited ( on other platforms )
#--------------------------------------------------------------------------------
if [[ "${step_nwp}" -eq 1 ]] ; then
  
  checkNWP=`prepNWP ${fmsdrPath} ${rdirAnalysExt} "${extResol}" ${satId} ${refData} \
  ${nwpGenAnalysSrc} ${makeOrNot} ${binPath} ${controlDataPath} ${refDataPath} \
  ${gridSfcNwpAnalysList} ${gridAtmNwpAnalysList}`
  
  if [[ ${checkNWP} == "1" ]] ; then
    echo "NWP files needed are complete"
  else
    echo "NWP files needed are NOT complete"
    exit 1
  fi

  nwp ${refDataPath} ${rdirAnalysExt} ${gridSfcNwpAnalysList} ${gridAtmNwpAnalysList}\
  ${fmsdrPath} "${extResol}" ${fmsdr4NwpList} ${nwpAnalysDir} ${topographyFile}\
  ${covBkgAtm1File} ${nOrbits2Process} ${logFile} ${nProfs2Fwd} ${grid2nwpControlFile}\
  ${nwpGenAnalysSrc} ${makeOrNot} ${binPath} ${sensorId} ${rdirNextAnalysExt}\
  ${spcCoeffFile} ${tauCoeffFile} ${cldOptPropFile} ${refData}
fi

#--------------------------------------------------------------------------------
#      step: Application of forward operator on NWP GFS analyses
#--------------------------------------------------------------------------------
if [[ "${step_fwd}" -eq 1 ]] ; then
  fwd ${fwdSrc} ${makeOrNot} ${nwpAnalysDir} ${nwpAnalysList} ${nOrbits2Process}\
  ${fwdAnalysDir} ${spcCoeffFile} ${tauCoeffFile} ${cldOptPropFile}\
  ${instrumentSensor1File} ${nProfs2Fwd} ${addDeviceNoise} ${nedtAftFMFile}\
  ${monitorFwd} ${logFile} ${fwdControlFile} ${binPath} ${satId} ${refData} ${fwdCloudOffOrOn} "${rdirExt}"
fi

#--------------------------------------------------------------------------------
#      step: Determination of the bias (by comparing FWD simul and meas)
#--------------------------------------------------------------------------------
if [[ "${step_biasGen}" -eq 1 ]] ; then
  biasGen ${determineBiasSrc} ${nwpAnalysDir} "${extResol}" ${nwpAnalys4BiasList}\
  ${fwdAnalysDir} ${fwdAnalys4BiasList} ${fmsdr4BiasDir} ${fmsdr4BiasList}\
  ${biasCompuControlFile} ${biasComputeMethod} ${biasFile} ${modelErrFile}\
  ${figs4BiasDir} ${nOrbits2Process} ${satId} ${IDL} ${sensorId}\
  ${topographyFile} ${binPath} "${modelErrNominalFile}" ${controlDataPath} ${refData}\
  ${minLat} ${maxLat} ${minLon} ${maxLon} 
fi



####################################################################################################
#
#       The following qc steps generate qc data
#
####################################################################################################


#--------------------------------------------------------------------------------
#      step: Retrieval quality monitoring
#--------------------------------------------------------------------------------
if [[ ${step_qcRetrieval} -eq 1 ]] ; then
  checkFile=${controlDataPath}/qcRetrieval_Alert_${satId}_${nedtExt}.txt
  namelist=${controlDataPath}/qcRetrieval_namelist
  qcRetrieval_IDL ${depDir} ${depList} ${orbitMonPath} ${IDL} ${gridSrc} ${checkFile} \
  ${namelist} ${email} "${web_retrieval}" ${controlDataPath}
fi


#--------------------------------------------------------------------------------
#      step: NEDT monitoring
#--------------------------------------------------------------------------------
if [[ ${step_qcNedt} -eq 1 ]] ; then
  checkResult=${controlDataPath}/qcNedt_Alert_${satId}_${nedtExt}.txt
  qcNedt ${satId} ${nedtBefFMFile} ${checkResult} ${tune1File} \
  ${nedtMonitorSrc} ${IDL} ${controlDataPath} ${email} "${web_nedt}"
fi


#--------------------------------------------------------------------------------
#      step: Radiometric monitoring
#--------------------------------------------------------------------------------
if [[ ${step_qcRadioBias} -eq 1 ]] ; then
  checkResult=${controlDataPath}/qcRadioBias_Alert_${satId}_${nedtExt}.txt
  qcRadioBias ${satId} ${nedtBefFMFile} ${tune1File} ${biasFile} ${biasFileToUse} ${modelErrFile} ${checkResult} \
  ${determineBiasSrc} ${IDL} ${controlDataPath} ${refData} ${email} "${web_radiobias}"
fi

#--------------------------------------------------------------------------------
#      step: Level II and III radiometric/geophysocal/p2p/bias/bias asymmetry
#--------------------------------------------------------------------------------
if [[ ${step_qcRadioGeo} -eq 1 ]] ; then
  qcRadioGeo ${satId} ${refData} ${rdirAnalysExt} ${edr4BiasDir} ${dep4BiasDir} \
  ${nwpAnalysDir} ${fwdAnalysDir} ${edr4BiasList} ${dep4BiasList} ${nwpAnalys4BiasList} \
  ${fwdAnalys4BiasList} ${gridSrc} ${grid4BiasDir} ${binPath} ${biasGridControlFile} \
  ${gridFactor} ${minLat} ${maxLat} ${minLon} ${maxLon} \
  ${orbitMonPath} ${figs4BiasDir} ${figsGenControlFile} ${IDL} ${region} ${version} \
  ${plot_edrdep} ${plot_nwp} ${plot_fwd} ${plot_bias} ${plot_p2p} ${fmType} \
  "${edrGridStr}" "${edrP2PStr}" "${depGridStr}" "${depP2PStr}" "${nwpGridStr}" "${nwpP2PStr}" "${nedtExt}"
fi

#--------------------------------------------------------------------------------
#      step: nedt monitoring, convergence and qc monitoring timeseries
#--------------------------------------------------------------------------------
#if [[ ${step_dataMonitor} -eq 1 ]] ; then
#  dataQualityMonitor ${nedtSensor1Path} ${nedtList} ${nedtMonitorSrc} ${IDL} \
#  ${orbitMonPath} ${figsDir} ${processMode} ${fileExt} ${satId} ${controlDataPath} \
#  ${perfsMonitorPath}
#fi

#--------------------------------------------------------------------------------
#      step: nedt monitoring, convergence and qc monitoring timeseries
#--------------------------------------------------------------------------------
if [[ ${step_dataMonitor} -eq 1 ]] ; then
  dataQualityMonitorRecord ${nedtSensor1Path} ${nedtBefFMFile} ${nedtMonitorSrc} ${IDL} \
  ${orbitMonPath} ${figsDir} ${nedtExt} ${satId} ${controlDataPath}
fi

#--------------------------------------------------------------------------------
#      step: radiometric bias monitoring timeseries
#--------------------------------------------------------------------------------
if [[ ${step_dataMonitor} -eq 1 ]] ; then
  biasMonitorRecord ${biasFile} ${biasPath} ${figsDir} ${satId} ${determineBiasSrc} ${IDL} ${controlDataPath} \
      ${refData} ${version} ${fmType}
fi

#--------------------------------------------------------------------------------
#     step: write list of outputs for QC
#--------------------------------------------------------------------------------
OutputList='npp_qc.PSF'
WriteListOfOutputs_QC ${nedtSensor1Path} ${dataPath} ${nedtExt} ${OutputList} ${satId}
