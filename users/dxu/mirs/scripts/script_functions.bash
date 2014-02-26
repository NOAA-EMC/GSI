#!/bin/bash
####################################################################################################
# This is the library of the bash scripts used to run 
# the MIRS testbed on N18 AMSU/MHS, F16 SSMI/S data, etc
#
# Sid -Ahmed Boukabara. IMSG @ NOAA/NESDIS/ORA, Oct 05.
#
#      Purpose:
#        This is the bash script functions used to run the MIRS testbed.
#
#
#      Functions:
#        DirGen
#        CreatNamList
#        PurgeDir
#        DirExistEstaa
#        DataExistEst
#        LeapYearEst
#        date2jday
#        jday2date
#
#      Record of revisions:
#           Date              Programmer                      Description of change
#       ============   =======================   ==============================================
#        09/03/2005     Sid-Ahmed Boukabara        Original script function created
#                      (IMSG@NOAA/NESDIS/STAR)
#
#        02/22/2006     Ninghai Sun                Add more functions.
#                      (NOAA/NESDIS/ORA/IMSG)
#
#
#        12/19/2006     Sid Boukabara              Added the capability to run the scripts in 
#                      (IMSG@NOAA/NESDIS/STAR)     orbital mode in addition to daily mode.
#
#        12/28/2006     Sid Boukabara              Extensive revision to make all functions general.
#                      (IMSG@NOAA/NESDIS/STAR)     Scripts to be generated automatically through
#						   a GUI-based JAVA script. This removes the 
#						   need to have to maintain as many scripts as sensors.
#						   Many functions previously in script moved into this
#						   library.
#
#	04/18/2007	Wanchun Chen		   Extend to include SSMIS/AMSRE/ATMS. 
#						   Removed all global variables.
#
####################################################################################################



#===============================================================
# Name:		    rdr2tdr
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	RDR to TDR conversion
#
#
# Input Variables:
# 	- rdrDir: Raw TDR data directory
# 	- rdrList: Raw TDR file list
# 	- sensor: Sensor (1:N18,  2:METOPA)
# 	- tdrDir: TDR data directory
# 	- nedtFile: nedt file
# 	- instrFile: instrument file
# 	- wtFile: WT file
# 	- norbits2process: Number of orbits to process
# 	- logFile: Log file
# 	- rdr2tdr_cntrl: control file
# 	- rdr2tdrSrc: Src dir of rdr2tdr
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- binDir: Bin directory where all executables reside
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- orbitInfo: Orbit information
# 	- satId: Satellite short name(n18,metopA,f16,etc)
# 	- processingDate: processing date(for orbit mode)
#
#
# Outputs:
#      - None
#
#===============================================================
function rdr2tdr() {

    if [[ $# -ne 23 ]]
    then
	op_msg "Error: function rdr2tdr: check number of arguments. Found $#"
	exit 1      
    fi
    local rdrDir=$1
    local rdrList=$2
    local sensor=$3
    local tdrDir=$4
    local nedtFile=$5
    local instrFile=$6
    local wtFile=$7
    local norbits2process=$8
    local logFile=$9
    local rdr2tdr_cntrl=${10}
    local rdr2tdrSrc=${11}
    local makeOrNot=${12}
    local binDir=${13}
    local processingMode=${14}
    local orbitInfo=${15}
    local satId=${16}
    local processingDate=${17}	# ex: 2007-04-18
    local controlDataPath=${18}
    local calibBiasFile=${19}
    local calibDTRlutFile=${20}
    local accessStr=${21}
    local formStr=${22}
    local rdrType=${23}

    local suffix
    local ext1
    local ext2
    
    if [[ "$sensor" == "amsua" ]] ; then
   	suffix="AMSUA"
	ext1="NSS.AMAX*"
	ext2="*"
    elif [[ "$sensor" == "mhs" ]] ; then
   	suffix="MHSX"
	ext1="NSS.MHSX*"
	ext2="*"
    elif [[ "$sensor" == "ssmis" ]] ; then
   	suffix="SSMIS"
	ext1="NPR.TDRN*"
	ext2="*"
    elif [[ "$sensor" == "atms" ]] ; then
   	suffix="npp"
	ext1="*ATMS*"
	ext2="*"
    elif [[ "$sensor" == "amsre" ]] ; then
   	suffix="aqua"
	ext1="AMSR*"
	ext2="hdf"
    elif [[ "$sensor" == "amsr2" ]] ; then
   	suffix="gcomw1"
	ext1="GW1AM2*"
	ext2="h5"
    elif [[ "$sensor" == "mwri" ]] ; then
   	suffix="fy3ri"
	ext1="FY3A_MWRID*"
	ext2="hdf"
    elif [[ "$sensor" == "tmi" ]] ; then
   	suffix="1B11"
	ext1="*"
	ext2="*HDF"
    elif [[ "$sensor" == "madras" ]] ; then
   	suffix="mtma"
	ext1="MT1MADSL1A*"
	ext2="*h5"
    elif [[ "$sensor" == "saphir" ]] ; then
   	suffix="mtsa"
	ext1="MT1SAPSL1A*"
	ext2="*h5"
    fi    
    
    echo "=============== RUNNING RDR->TDR(${sensor}) STEP =======================" 
    rm -f ${tdrDir}/TDR* #purge TDR files that may already exist
    if [[ "${satId}" == "n18" || "${satId}" == "metopA" || "${satId}" == "n19" || "${satId}" == "metopB" ]] ; then 
    	DirExistEst "RDR-${suffix}" ${rdrDir}     #---Test existence of RDR directory
    	ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "${ext1}" "${ext2}"
    	#---Test existence of RDR data files  
    	DataExistEst "${suffix}" "RDR" ${rdrList}
	
    	set 	"rdrfileList='${rdrList}'" 		\
		"pathTDR='${tdrDir}/'" 			\
		"NEDTfile='${nedtFile}'" 		\
    	    	"InstrConfigFile='${instrFile}'" 	\
	    	"WarmTargetFile='${wtFile}'" 		\
	    	"norbits2process=${norbits2process}" 	\
    	    	"LogFile='${logFile}'"
		
	CreatNamList ${rdr2tdr_cntrl} '&ContrlRDR2TDR' "$@"
	DoMake ${rdr2tdrSrc} ${makeOrNot}
    	${binDir}/rdr2tdr_${satId}_${sensor} < ${rdr2tdr_cntrl}
	checkStatus $? "rdr2tdr"
		
    elif [[ "${satId}" == "f16" ]] ; then
    
     	DirExistEst "RDR-${suffix}" ${rdrDir}     #---Test existence of RDR directory
    	ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "${ext1}" "${ext2}"
    	DataExistEst "${suffix}" "RDR" ${rdrList} #---Test existence of SSMIS RDR data files  
	
    	set 	"tdr_path_input='${rdrDir}/'"  		\
        	"tdr_list_input='${rdrList}'"  		\
		"tdr_path_output='${tdrDir}/'"  	\
        	"norbits2process=${norbits2process}" 	\
		"fname_DTBCOE_Table='${calibBiasFile}'" \
		"fname_DTR_Table='${calibDTRlutFile}'"  \
		"accessStr='${accessStr}'"		\
		"formStr='${formStr}'"
    	
	CreatNamList ${rdr2tdr_cntrl}_calib '&ContrlCalibTDR' "$@"
    	#---Calibration correction of TDRs
    	DoMake ${rdr2tdrSrc} ${makeOrNot}
    	${binDir}/calib_${satId}_ssmis < ${rdr2tdr_cntrl}_calib
    	checkStatus $? "rdr2tdr: calib_${satId}_ssmis"
	
	#create the list of RDR SSMIS (calibrated) files
	if [[ ${processingMode} -eq 0 ]] ; then
	    ls ${tdrDir}NPR*${orbitInfo}*CALIB* > ${rdrList}
    	else
            jjj=`yyyymmdd2jjj ${processingDate}`
            yy=`echo ${processingDate} | cut -c3-4`
            yyjjj=${yy}${jjj}
	    ls ${tdrDir}/NPR*CALIB*  > ${rdrList}
	fi    
	
	#---Conversion to MIRS-compatible format
	set 	"rdrfileList='${rdrList}'"	\
		"pathTDR='${tdrDir}/'"		\
		"accessStr='${accessStr}'"	\
		"formStr='${formStr}'"
    	CreatNamList ${rdr2tdr_cntrl}_grid '&ContrlRDR2TDR' "$@"
    	DoMake ${rdr2tdrSrc} ${makeOrNot}
    	${binDir}/rdr2tdr_${satId}_ssmis < ${rdr2tdr_cntrl}_grid
	checkStatus $? "rdr2tdr: rdr2tdr_${satId}_ssmis"
	
    elif [[ "${satId}" == "f18" || "${satId}" == "f17" ]] ; then
    
     	DirExistEst "RDR-${suffix}" ${rdrDir}     #---Test existence of RDR directory
    	ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "${ext1}" "${ext2}"
    	DataExistEst "${suffix}" "RDR" ${rdrList} #---Test existence of SSMIS RDR data files  
	
	#---Conversion to MIRS-compatible format
	set 	"rdrfileList='${rdrList}'"	\
		"pathTDR='${tdrDir}/'"		\
		"accessStr='${accessStr}'"	\
		"formStr='${formStr}'"
    	CreatNamList ${rdr2tdr_cntrl} '&ContrlRDR2TDR' "$@"
    	DoMake ${rdr2tdrSrc} ${makeOrNot}
    	${binDir}/rdr2tdr_${satId}_ssmis < ${rdr2tdr_cntrl}
	checkStatus $? "rdr2tdr: rdr2tdr_${satId}_ssmis"
	
    elif [[ "${satId}" == "npp" ]] ; then
     	
	DirExistEst "RDR-${suffix}" ${rdrDir}     #---Test existence of RDR directory
    	
        # rdrType ( 1:TDR;    2:SDR-proxy from MIT;    3:SDR;    4:SDR-remap )
	# assume RDR and GEO files are in the same directory, otherwise, need a geoDir
	if [[ ${rdrType} -eq 1 ]] ; then
    	  ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "TATMS*" "${ext2}"
	  geoList=${rdrList/npp_atms_rdrFiles_/npp_atms_geoFiles_}
	  ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${geoList} "GATMO*" "${ext2}"
	elif [[ ${rdrType} -eq 2 ]] ; then  
    	  ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "SATMS*" "${ext2}"
	elif [[ ${rdrType} -eq 3 ]] ; then  
    	  ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "SATMS*" "${ext2}"
	  geoList=${rdrList/npp_atms_rdrFiles_/npp_atms_geoFiles_}
	  ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${geoList} "GATMO*" "${ext2}"
	elif [[ ${rdrType} -eq 4 ]] ; then  
    	  ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "SATMR*" "${ext2}"
	  geoList=${rdrList/npp_atms_rdrFiles_/npp_atms_geoFiles_}
	  ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${geoList} "GATRO*" "${ext2}"
        fi

	DataExistEst "${suffix}" "RDR" ${rdrList} #---Test existence of RDR files
	  
	if [[ ${rdrType} -eq 1 || ${rdrType} -eq 3 || ${rdrType} -eq 4 ]] ; then
 	  DataExistEst "${suffix}" "GEO" ${geoList} #---Test existence of GEO files
	  # also need to check equal number of rdr and geo files
	  local rdr_cont=`wc -l ${rdrList} | awk '{print $1}'`
	  local geo_cont=`wc -l ${geoList} | awk '{print $1}'`
	  if [[ ${rdr_cont} -ne ${geo_cont} ]] ; then
	    op_msg "Error: RDR and GEO file numbers are not equal."
	    exit 1
	  fi
	fi  
	
	local nedtPath=`dirname ${nedtFile}`
	
	echo ${rdrType}      >  ${rdr2tdr_cntrl}
	echo "${rdrList}"    >> ${rdr2tdr_cntrl} 
	echo "${tdrDir}/"    >> ${rdr2tdr_cntrl}
	echo "${nedtPath}/"  >> ${rdr2tdr_cntrl}
	echo "${instrFile}"  >> ${rdr2tdr_cntrl}
	
	if [[ ${rdrType} -eq 1 || ${rdrType} -eq 3 || ${rdrType} -eq 4 ]] ; then
	  echo "${geoList}" >> ${rdr2tdr_cntrl}
	fi
	
	#echo "${norbits2process}" >> ${rdr2tdr_cntrl}
	#echo "${logFile}"         >> ${rdr2tdr_cntrl}

    	DoMake ${rdr2tdrSrc} ${makeOrNot}
	${binDir}/rdr2tdr_npp_atms < ${rdr2tdr_cntrl}

    elif [[ "${satId}" == "aqua" ]] ; then
     	DirExistEst "RDR-${suffix}" ${rdrDir}     #---Test existence of RDR directory
    	ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList}_as "${ext1}" "*_A.${ext2}"
    	ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList}_ds "${ext1}" "*_D.${ext2}"
    	DataExistEst "${suffix}" "RDR" ${rdrList}_as #---Test existence of AMSRE files
    	DataExistEst "${suffix}" "RDR" ${rdrList}_ds #---Test existence of AMSRE files
    	
    	cd  ${rdr2tdrSrc}
    	echo ${satId}		>  ${rdr2tdr_cntrl}
    	echo ${rdrList}_as 	>> ${rdr2tdr_cntrl}
    	echo ${rdrList}_ds 	>> ${rdr2tdr_cntrl}
    	echo ${tdrDir}/		>> ${rdr2tdr_cntrl}
	
    	cd  ${rdr2tdrSrc}
  	echo "!QUIET=1"						>  ${controlDataPath}/batchRDR2TDR_AQUA_AMSRE.pro
  	echo ".r rdr2tdr_aqua_amsre.pro" 			>> ${controlDataPath}/batchRDR2TDR_AQUA_AMSRE.pro 
  	echo "RDR2TDR_AQUA_AMSRE, namelist='${rdr2tdr_cntrl}'"  >> ${controlDataPath}/batchRDR2TDR_AQUA_AMSRE.pro 
  	echo "exit"						>> ${controlDataPath}/batchRDR2TDR_AQUA_AMSRE.pro 

  	${IDL} ${controlDataPath}/batchRDR2TDR_AQUA_AMSRE.pro 
	checkStatus $? "rdr2tdr: rdr2tdr_aqua_amsre.pro"
	rm -f ${controlDataPath}/batchRDR2TDR_AQUA_AMSRE.pro
   
    elif [[ "${satId}" == "fy3ri" ]] ; then
    
    	DirExistEst "RDR-${suffix}" ${rdrDir}     #---Test existence of RDR directory
    	ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "${ext1}" "${ext2}"
    	#---Test existence of RDR data files  
    	DataExistEst "${suffix}" "RDR" ${rdrList}
	
    	set 	"rdrfileList='${rdrList}'" \
		"pathTDR='${tdrDir}/'" 
	CreatNamList ${rdr2tdr_cntrl} '&ContrlRDR2TDR' "$@"
	DoMake ${rdr2tdrSrc} ${makeOrNot}
    	${binDir}/rdr2tdr_${satId}_${sensor} < ${rdr2tdr_cntrl}
	checkStatus $? "rdr2tdr: rdr2tdr_fy3ri_mwri"

    elif [[ "${satId}" == "trmm" ]] ; then
    
    	DirExistEst "RDR-${suffix}" ${rdrDir}     #---Test existence of RDR directory
    	ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "${ext1}" "${ext2}"
    	#---Test existence of RDR data files  
    	DataExistEst "${suffix}" "RDR" ${rdrList}
    	set 	"rdrfileList='${rdrList}'" 		\
		"pathTDR='${tdrDir}/'" 			\
	    	"norbits2process=${norbits2process}" 	\
    	    	"logFile='${logFile}'"
	CreatNamList ${rdr2tdr_cntrl} '&ContrlRDR2TDR' "$@"
	DoMake ${rdr2tdrSrc} ${makeOrNot}
    	${binDir}/rdr2tdr_${satId}_${sensor} < ${rdr2tdr_cntrl}
	checkStatus $? "rdr2tdr: rdr2tdr_trmm_tmi"

    elif [[ "${satId}" == "mtma" || "${satId}" == "mtsa" ]] ; then
        
    	DirExistEst "RDR-${suffix}" ${rdrDir}     #---Test existence of RDR directory
    	ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "${ext1}" "${ext2}"
    	#---Test existence of RDR data files  
    	DataExistEst "${suffix}" "RDR" ${rdrList}
	
        echo ${rdrList} > ${rdr2tdr_cntrl}
        local slash_end=`echo ${tdrDir} | grep '/$'`
        if [[ ${slash_end} == "" ]] ; then
          echo ${tdrDir}/ >> ${rdr2tdr_cntrl}
        else
          echo ${tdrDir} >> ${rdr2tdr_cntrl}
        fi
        
        DoMake ${rdr2tdrSrc} ${makeOrNot}
    	${binDir}/rdr2tdr_${satId}_${sensor} < ${rdr2tdr_cntrl}
	checkStatus $? "rdr2tdr: rdr2tdr_${satId}_${sensor}"

    elif [[ "${satId}" == "gcomw1" ]] ; then
      
      if [[ ${processingMode} -eq 0 ]] ; then
        ls -1 ${rdrDir}/*${orbitInfo}.h5 > ${rdrList}
      else  
    	DirExistEst "RDR-${suffix}" ${rdrDir}
    	ConstructList ${processingMode} ${rdrDir} ${orbitInfo} ${rdrList} "${ext1}" "${ext2}"
    	DataExistEst "${suffix}" "RDR" ${rdrList}
      fi
        echo ${rdrList} > ${rdr2tdr_cntrl}
        local slash_end=`echo ${tdrDir} | grep '/$'`
        if [[ ${slash_end} == "" ]] ; then
          echo ${tdrDir}/ >> ${rdr2tdr_cntrl}
        else
          echo ${tdrDir} >> ${rdr2tdr_cntrl}
        fi
        
        DoMake ${rdr2tdrSrc} ${makeOrNot}
    	${binDir}/rdr2tdr_${satId}_${sensor} < ${rdr2tdr_cntrl}
	checkStatus $? "rdr2tdr: rdr2tdr_${satId}_${sensor}"

    fi
    
    echo "End of step rdr2tdr"    
}



#===============================================================
# Name:		    mergeNedt
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Merge NeDTs
#
#
# Input Variables:
# 	- nedtFile1: Sensor 1 nedt file 
# 	- nedtFile2: Sensor 2 nedt file 1 
# 	- nedtFile1File2: combined sensor1 and sensor 2 file
# 	- logFile: Log file
# 	- mergeNedt_cntrl: control file
# 	- mergeNedtSrc: Src dir for nedt merge
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- binDir: Bin directory where all executables reside
# 	- satId: Satellite short name(n18,metopA,f16,etc)
# 	- nedtNominalFile: Nedt nominal file(only for F16)
#
#
# Outputs:
#      - None
#
#===============================================================
function mergeNedt() {
    if [[ $# -ne 11 ]]
    then
	op_msg "Error: function mergeNEdt: check number of arguments: $#"
	exit 1      
    fi
    local nedtFile1=$1
    local nedtFile2=$2
    local nedtBefFMFile=$3
    local logFile=$4
    local mergeNedt_cntrl=$5
    local mergeNedtSrc=$6
    local makeOrNot=$7
    local binDir=$8
    local satId=$9
    local nedtNominalFile=${10}
    local nedtGranuleDir=${11}
    
    echo "=============== RUNNING MRG-NEDT STEP ======================="
    if [[ "${satId}" == "n18" || "${satId}" == "metopA" || "${satId}" == "n19" || "${satId}" == "metopB" ]] ; then 
    	set 	"NEDTfile1='${nedtFile1}'" 	       \
		"NEDTfile2='${nedtFile2}'" 	       \
		"NEDTfile='${nedtBefFMFile}'"  	       \
	        "NominalNEDTfile='${nedtNominalFile}'" \
		"LogFile='${logFile}'"
    	CreatNamList ${mergeNedt_cntrl} '&MergeNEDTCntrl' "$@"
    	DoMake ${mergeNedtSrc} ${makeOrNot}
    	${binDir}/mergeNEDT < ${mergeNedt_cntrl}
    elif [[ "${satId}" == "f16" || "${satId}" == "f17" || "${satId}" == "f18" ]] ; then
    	cp ${nedtNominalFile} ${nedtBefFMFile}
    elif [[ "${satId}" == "aqua" || "${satId}" == "fy3ri" || "${satId}" == "trmm" || "${satId}" == "gpm" || "${satId}" == "mtma" || "${satId}" == "mtsa" ]] ; then
    	cp ${nedtNominalFile} ${nedtBefFMFile}
    elif [[ "${satId}" == "mtma" || "${satId}" == "mtsa" ]] ; then
    	cp ${nedtNominalFile} ${nedtBefFMFile}
    elif [[ "${satId}" == "gcomw1" ]] ; then
    	cp ${nedtNominalFile} ${nedtBefFMFile}
    elif [[ "${satId}" == "npp" ]] ; then

	local nedtPath=`dirname ${nedtBefFMFile}`
	local controlPath=`dirname ${mergeNedt_cntrl}`
        local fileList=${controlPath}/NEDT_npp_atms_list
	ls -1 ${nedtPath}/NEDT_*dat > ${fileList} 2> /dev/null
	local nfile=`wc -l ${fileList} | awk '{print $1}' 2> /dev/null`
	if [[ ${nfile} -eq 0 ]] ; then
	    echo "NO NEDT_* files under ${nedtPath}, use nominal nedt file"
	    cp ${nedtNominalFile} ${nedtBefFMFile}
	else
            set  "listFile='${fileList}'"     \
                 "NEDTfile='${nedtBefFMFile}'" 
            CreatNamList ${mergeNedt_cntrl} '&MergeNEDTCntrl' "$@"
            DoMake ${mergeNedtSrc} ${makeOrNot}
	    ${binDir}/mergeNedt_npp_atms < ${mergeNedt_cntrl}
	    if [[ "${nedtPath}" != "${nedtGranuleDir}" && "${nedtPath}/" != "${nedtGranuleDir}" ]] ; then
                mv -f ${nedtPath}/NEDT_*dat ${nedtGranuleDir}
            fi
	fi

    fi
    
    checkStatus $? "mergeNedt"
    echo "End of step mergeNedt"    
}


#===============================================================
# Name:		    tdr2sdr
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	TDR to SDR conversion
#
#
# Input Variables:
# 	- tdrDir: TDR data directory
# 	- tdrList: TDR data list
# 	- sensor: Sensor (1:N18,  2:METOPA)
# 	- tdrFormat: Format of TDR (depends on the RDR decoder at hand):0->ascii, 1->binary
# 	- sdrDir: SDR data directory
# 	- antennaFile: Antenna file 
# 	- norbits2process: Number of orbits to process
# 	- logFile: Log file
# 	- tdr2sdr_cntrl: TDR2SDR control file
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- binDir: Bin directory where all executables reside
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- orbitInfo: Orbit information
# 	- tdr2sdrSrc: TDR2SDR source code directory
#
#
# Outputs:
#      - None
#
#===============================================================
function tdr2sdr() {
    local nargs=$#
    local args="$@"
    if [ $nargs -ne 14 ] 
    then
	  op_msg "Error: function tdr2sdr check number of arguments: $nargs "
	  exit 1
    fi	
    local tdrDir=$1
    local tdrList=$2
    local sensor=$3
    local tdrFormat=$4
    local sdrDir=$5
    local antennaFile=$6
    local norbits2process=$7
    local logFile=$8
    local tdr2sdr_cntrl=$9
    local makeOrNot=${10}
    local binDir=${11}
    local processingMode=${12}
    local orbitInfo=${13}
    local tdr2sdrSrc=${14}
    
    local ext1
    local ext2
    
    if [[ "${sensor}" == "amsua" ]] ; then
        ext1="TDR_AMAX*"
	ext2="*"
    elif [[ "${sensor}" == "mhs" ]] ; then	
        ext1="TDR_MHSX*"
	ext2="*"
    else
        ext1="TDR_*"
	ext2="*"
    fi
    
    echo "=============== RUNNING TDR->SDR("${sensor}") STEP ======================="
    rm -f ${sdrDir}/SDR* #purge SDR files that may already exist
    ConstructList ${processingMode} ${tdrDir} ${orbitInfo} ${tdrList} "${ext1}" "${ext2}"
    #---Test existence of TDR data files  
    DataExistEst ${sensor} "TDR" ${tdrList}
    set "tdrfileList='${tdrList}'" 		\
    	"tdrFormat=${tdrFormat}" 		\
	"pathSDR='${sdrDir}/'" 			\
        "AntennaPattFile='${antennaFile}'"  	\
	"norbits2process=${norbits2process}" 	\
	"LogFile='${logFile}'"
    CreatNamList ${tdr2sdr_cntrl} '&ContrlTDR2SDR' "$@"
    DoMake ${tdr2sdrSrc} ${makeOrNot}
    ${binDir}/tdr2sdr < ${tdr2sdr_cntrl}
    checkStatus $? "tdr2sdr"

    echo "End of step tdr2sdr" 
}
  

#===============================================================
# Name:		    fm
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Footprint matching
#
#
# Input Variables:
# 	- sdrDir_amsua: Sensor 1 SDR data directory
# 	- sdrList_amsua: Sensor 1 SDR data file list
# 	- sdrDir_mhs: Sensor 2 SDR data directory
# 	- sdrList_mhs: Sensor 2 SDR data file list
# 	- Sensor1: Sensor 1 ( ex. amsua )
# 	- Sensor2: Sensor 2 ( ex. mhs )
# 	- fmsdrDir: FMSDR data directory
# 	- outFMaccur: Flag to output of the FM accuracy metric (DeltaTB @89)
# 	- PerfsMonitorPath: Performance monitoring path
# 	- prefixFMaccur: Prefix of file(s) w FM-acuracy metric (only if outFMaccur=1)
# 	- iFMtype: Footprint matching type .0->to lowest resolution (AMSUA), 1->to highest resolution (MHS)
# 	- nScanL_amsua_2skip: umber of AMSUA scan lines to skip upfront (to accomodate geolocation issues)
# 	- nScanL_mhs_2skip: Number of MHS scan lines to skip upfront (to accomodate geolocation issues)
# 	- ScanLindx_mhs_TimeColloc: MHS ScanLine index (1,2 or 3) that corresponds in time to AMSUA
# 	- norbits2process: Number of orbits to process
# 	- Logfile: Log file
# 	- fm_cntrl: Footprint matching control file
# 	- fmSrc: Footprint matching src dir
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- binDir: Bin directory where all executables reside
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
# 	- satId: Satellite short name(n18,metopA,f16,etc)
# 	- nedtFileIn: Input nedt file 
# 	- nedtFileOut: Output nedt file
# 	- modifyNedt_cntrl: control file
#
#
# Outputs:
#      - None
#
#===============================================================
function fm() {
    if [[ $# -ne 36 ]]
    then
	op_msg "Error: function fm: check number of arguments: $#"
	exit 1      
    fi
    local sdrDir_amsua=$1
    local sdrList_amsua=$2
    local sdrDir_mhs=$3
    local sdrList_mhs=$4
    local Sensor1=$5
    local Sensor2=$6
    local fmsdrDir=$7
    local outFMaccur=$8
    local PerfsMonitorPath=$9
    local prefixFMaccur=${10}
    local iFMtype=${11}
    local nScanL_amsua_2skip=${12}
    local nScanL_mhs_2skip=${13}
    local ScanLindx_mhs_TimeColloc=${14}
    local norbits2process=${15}
    local Logfile=${16}
    local fm_cntrl=${17}
    local fmSrc=${18}
    local makeOrNot=${19}
    local binDir=${20}
    local processingMode=${21}
    local OrbitInfo=${22}
    local satId=${23}
    local nedtFileIn=${24}
    local nedtFileOut=${25}
    local modifyNedt_cntrl=${26}
    local figs4BiasDir=${27}
    local rdirExt=${28}
    local outFMAccuracy=${29}
    local inputDataPath=${30}
    local controlDataPath=${31}
    local geoLimit=${32}
    local minLat=${33}
    local maxLat=${34}
    local minLon=${35}
    local maxLon=${36}

    echo "=============== RUNNING FM STEP ======================="
    rm -f ${fmsdrDir}/FM* #purge FMSDR files that may already exist
    if [[ "${satId}" == "n18" || "${satId}" == "metopA" || "${satId}" == "n19" || "${satId}" == "metopB" ]] ; then
	rm -f ${PerfsMonitorPath}/QCcheck*
    	ConstructListFM ${processingMode} ${OrbitInfo} ${sdrDir_amsua} ${sdrDir_mhs} \
			${sdrList_amsua} ${sdrList_mhs} "SDR_AMAX." "SDR_MHSX." "*" "*"
	#---Test existence of AMSU-A & MHS SDR data files  
    	DataExistEst ${Sensor1} "SDR" ${sdrList_amsua}
    	DataExistEst ${Sensor2} "SDR" ${sdrList_mhs}
    	set 	"sdrfileList_amsua='${sdrList_amsua}'" 			\
		"sdrfileList_mhs='${sdrList_mhs}'" 			\
		"pathFMSDR='${fmsdrDir}/'"  				\
	    	"iOutpFM_accur=${outFMaccur}" 				\
		"prefCheck='${PerfsMonitorPath}/${prefixFMaccur}'" 	\
		"iFMtype=${iFMtype}" 					\
	    	"nScanL_amsua_2skip=${nScanL_amsua_2skip}" 		\
		"nScanL_mhs_2skip=${nScanL_mhs_2skip}"			\
    	    	"ScanLindx_mhs_TimeColloc=${ScanLindx_mhs_TimeColloc}" 	\
		"norbits2process=${norbits2process}" 	  		\
	    	"LogFile='${Logfile}'"					\
		"GeogrLimits=${geoLimit}"		 		\
		"minLat=${minLat}"			 		\
		"maxLat=${maxLat}"			 		\
		"minLon=${minLon}"			 		\
		"maxLon=${maxLon}"
		
    	CreatNamList ${fm_cntrl} '&ContrlFM' "$@"
    	DoMake ${fmSrc} ${makeOrNot}
    	${binDir}/fm_${satId} < ${fm_cntrl}
	checkStatus $? "fm"
	
	#---modify nedt values after the footprint matching process (noise amplification/reduction)
    	cp ${nedtFileIn} ${nedtFileOut}

        # Plot footprint matching monitoring
	if [[ ${outFMAccuracy} -eq 1 ]] ; then
	    cd  ${fmSrc}
	    echo "!QUIET=1"                                                      			 	>  ${controlDataPath}/batchFM.pro
	    echo ".r checkFMaccur_${satId}.pro"									>> ${controlDataPath}/batchFM.pro
	    echo "checkFMaccur_${satId}, '${PerfsMonitorPath}', '${satId}', '${rdirExt}', ${processingMode}"	>> ${controlDataPath}/batchFM.pro
	    echo "exit"												>> ${controlDataPath}/batchFM.pro
	    ${IDL} ${controlDataPath}/batchFM.pro
	    rm -f ${controlDataPath}/batchFM.pro
	fi
	    
    elif [[ "${satId}" == "f16" || "${satId}" == "f17" || "${satId}" == "f18" ]] ; then
    	
	sdrList_ssmis_img=${inputDataPath}/sdrFiles_ssmis_img.${OrbitInfo}.list
	sdrList_ssmis_env=${inputDataPath}/sdrFiles_ssmis_env.${OrbitInfo}.list
	sdrList_ssmis_las=${inputDataPath}/sdrFiles_ssmis_las.${OrbitInfo}.list
	sdrList_ssmis_uas=${inputDataPath}/sdrFiles_ssmis_uas.${OrbitInfo}.list
	# need the four lists 
	if [[ ${processingMode} -eq 0 ]] ; then  # orbit mode, we need orbit information
    	  ls -1 ${sdrDir_amsua}/SDR_IMGN.${OrbitInfo}* > ${sdrList_ssmis_img}  #create the list of SDR SSMIS files (IMG)
    	  ls -1 ${sdrDir_amsua}/SDR_ENVN.${OrbitInfo}* > ${sdrList_ssmis_env}  #create the list of SDR SSMIS files (ENV)
    	  ls -1 ${sdrDir_amsua}/SDR_LASN.${OrbitInfo}* > ${sdrList_ssmis_las}  #create the list of SDR SSMIS files (LAS)
    	  ls -1 ${sdrDir_amsua}/SDR_UASN.${OrbitInfo}* > ${sdrList_ssmis_uas}  #create the list of SDR SSMIS files (UAS)
    	else
    	  ls -1 ${sdrDir_amsua}/SDR_IMGN.* > ${sdrList_ssmis_img}  #create the list of SDR SSMIS files (IMG)
    	  ls -1 ${sdrDir_amsua}/SDR_ENVN.* > ${sdrList_ssmis_env}  #create the list of SDR SSMIS files (ENV)
    	  ls -1 ${sdrDir_amsua}/SDR_LASN.* > ${sdrList_ssmis_las}  #create the list of SDR SSMIS files (LAS)
    	  ls -1 ${sdrDir_amsua}/SDR_UASN.* > ${sdrList_ssmis_uas}  #create the list of SDR SSMIS files (UAS)
	fi
	DataExistEst "SSM/IS-IMG" "SDR" ${sdrList_ssmis_img}
    	DataExistEst "SSM/IS-ENV" "SDR" ${sdrList_ssmis_env}
    	DataExistEst "SSM/IS-LAS" "SDR" ${sdrList_ssmis_las}
    	DataExistEst "SSM/IS-UAS" "SDR" ${sdrList_ssmis_uas}
    
    	set 	"sdrfileList_img='${sdrList_ssmis_img}'" \
		"sdrfileList_env='${sdrList_ssmis_env}'" \
    	    	"sdrfileList_las='${sdrList_ssmis_las}'" \
		"sdrfileList_uas='${sdrList_ssmis_uas}'" \
    	    	"pathFMSDR='${fmsdrDir}/'" 		 \
		"FMresol=${iFMtype}"			 \
		"GeogrLimits=${geoLimit}"		 \
		"minLat=${minLat}"			 \
		"maxLat=${maxLat}"			 \
		"minLon=${minLon}"			 \
		"maxLon=${maxLon}"                       \
		"LogFile='${Logfile}'"		
    	CreatNamList ${fm_cntrl} '&ContrlFM' "$@"
    
    	DoMake ${fmSrc} ${makeOrNot}
    	${binDir}/fm_${satId} < ${fm_cntrl}    
	checkStatus $? "fm_${satId}"
    
	#---modify nedt values after the footprint matching process (noise amplification/reduction)
    	set 	"NedtFile_in='${nedtFileIn}'" 	\
		"NedtFile_out='${nedtFileOut}'" \
		"FMresol=${iFMtype}"
    	CreatNamList ${modifyNedt_cntrl} '&NedtModif' "$@"
    	${binDir}/modifyNedt_ssmis < ${modifyNedt_cntrl}
	checkStatus $? "modifyNedt_ssmis"
    
    elif [[ "${satId}" == "npp" ]] ; then
    	ConstructList ${processingMode} ${sdrDir_amsua} ${OrbitInfo} ${sdrList_amsua} "SDR_*" "*"
	DataExistEst "NPP" "ATMS" ${sdrList_amsua}
    	set 	"sdrfileList='${sdrList_amsua}'"	\
		"pathFMSDR='${fmsdrDir}/'"		\
	        "FMresol=${iFMtype}"                    \
		"norbits2process=${norbits2process}"	\
	    	"LogFile='${Logfile}'"			\
		"GeogrLimits=${geoLimit}"		\
		"minLat=${minLat}"			\
		"maxLat=${maxLat}"			\
		"minLon=${minLon}"			\
		"maxLon=${maxLon}"
    	CreatNamList ${fm_cntrl} '&ContrlFM' "$@"
    	DoMake ${fmSrc} ${makeOrNot}
    	${binDir}/fm_${satId} < ${fm_cntrl}
	checkStatus $? "fm_${satId}"
	
	#---modify nedt values after the footprint matching process (noise amplification/reduction)
	# Daily averaged file
    	cp ${nedtFileIn} ${nedtFileOut}
	# Granule level files
	local nedtPath=`dirname ${nedtFileIn}`/${rdirExt}
	local controlPath=`dirname ${modifyNedt_cntrl}`
	local fileList=${controlPath}/npp_atms_nedt_list

	ls -1 ${nedtPath}/NEDT_*_befFM.dat > ${fileList} 2> /dev/null
	local nfile=`wc -l ${fileList} | awk '{print $1}' 2> /dev/null`
	local files=($fileList)
	for line in `cat ${fileList}`
	  do
	  file=`basename ${line}`
	  prefix=`echo ${file} | cut -f1-10 -d "_"`
	  cp ${line} ${nedtPath}/${prefix}_aftFM.dat
	done  

    elif [[ "${satId}" == "aqua" ||  "${satId}" == "fy3ri" ]] ; then
    	ConstructList ${processingMode} ${sdrDir_amsua} ${OrbitInfo} ${sdrList_amsua} "SDR_*" "*"
	DataExistEst "AQUA" "AMSR-E" ${sdrList_amsua}
    	set 	"sdrfileList='${sdrList_amsua}'"	\
		"pathFMSDR='${fmsdrDir}/'"		\
	        "FMresol=${iFMtype}"                    \
		"norbits2process=${norbits2process}"	\
	    	"LogFile='${Logfile}'"			\
		"GeogrLimits=${geoLimit}"		\
		"minLat=${minLat}"			\
		"maxLat=${maxLat}"			\
		"minLon=${minLon}"			\
		"maxLon=${maxLon}"
    	CreatNamList ${fm_cntrl} '&ContrlFM' "$@"
    	DoMake ${fmSrc} ${makeOrNot}
    	${binDir}/fm_${satId} < ${fm_cntrl}
	checkStatus $? "fm_${satId}"
	
	#---modify nedt values after the footprint matching process (noise amplification/reduction)
    	cp ${nedtFileIn} ${nedtFileOut}

    elif [[ "${satId}" == "trmm" ]] ; then
	
	if [[ ${processingMode} -eq 1 ]] ; then
	  ls -1 ${sdrDir_amsua}/SDR_LR.* > ${sdrList_amsua}
	  ls -1 ${sdrDir_amsua}/SDR_HR.* > ${sdrList_mhs}
	else
	  ls -1 ${sdrDir_amsua}/SDR_LR.${OrbitInfo}* > ${sdrList_amsua}
	  ls -1 ${sdrDir_amsua}/SDR_HR.${OrbitInfo}* > ${sdrList_mhs}
	fi
	
	local nfile1=`wc -l ${sdrList_amsua} | awk '{print $1}'`
	local nfile2=`wc -l ${sdrList_mhs}   | awk '{print $1}'`
	
	if [[ ${nfile1} -eq 0 || ${nfile2} -eq 0 || ${nfile1} -ne ${nfile2} ]] ; then
	  op_msg "Error: Incorrect file number"
	  exit 1 
	fi 
	
    	set 	"sdrfileList_lr='${sdrList_amsua}'" 			\
		"sdrfileList_hr='${sdrList_mhs}'" 			\
		"pathFMSDR='${fmsdrDir}/'"  				\
	    	"iOutpFM_accur=${outFMaccur}" 				\
		"prefCheck='${PerfsMonitorPath}/${prefixFMaccur}'" 	\
		"iFMtype=${iFMtype}" 					\
	    	"nScanL_lr_2skip=${nScanL_amsua_2skip}" 		\
		"nScanL_hr_2skip=${nScanL_mhs_2skip}"			\
    	    	"ScanLindx_hr_TimeColloc=${ScanLindx_mhs_TimeColloc}" 	\
		"norbits2process=${norbits2process}" 	  		\
	    	"logFile='${Logfile}'"					\
		"GeogrLimits=${geoLimit}"		 		\
		"minLat=${minLat}"			 		\
		"maxLat=${maxLat}"			 		\
		"minLon=${minLon}"			 		\
		"maxLon=${maxLon}"
		
    	CreatNamList ${fm_cntrl} '&ContrlFM' "$@"
    	DoMake ${fmSrc} ${makeOrNot}
    	${binDir}/fm_${satId} < ${fm_cntrl}
	checkStatus $? "fm_${satId}"
	
	#---modify nedt values after the footprint matching process (noise amplification/reduction)
    	cp ${nedtFileIn} ${nedtFileOut}

    elif [[ "${satId}" == "gcomw1" ]] ; then
	
	if [[ ${processingMode} -eq 1 ]] ; then
	  sdrList_lr=${inputDataPath}/gcomw1_amsr2_sdrFiles_LR_${rdirExt}.list
	  sdrList_hr=${inputDataPath}/gcomw1_amsr2_sdrFiles_HR_${rdirExt}.list
	  ls -1 ${sdrDir_amsua}/SDR_LR_* > ${sdrList_lr}
	  ls -1 ${sdrDir_amsua}/SDR_HR_* > ${sdrList_hr}
	else
	  sdrList_lr=${inputDataPath}/gcomw1_amsr2_sdrFiles_LR_${OrbitInfo}.list
	  sdrList_hr=${inputDataPath}/gcomw1_amsr2_sdrFiles_HR_${OrbitInfo}.list
	  ls -1 ${sdrDir_amsua}/SDR_LR_${OrbitInfo}* > ${sdrList_lr}
	  ls -1 ${sdrDir_amsua}/SDR_HR_${OrbitInfo}* > ${sdrList_hr}
	fi
	
	local nfile1=`wc -l ${sdrList_lr} | awk '{print $1}'`
	local nfile2=`wc -l ${sdrList_hr} | awk '{print $1}'`
	
	if [[ ${nfile1} -eq 0 || ${nfile2} -eq 0 || ${nfile1} -ne ${nfile2} ]] ; then
	  op_msg "Error: fm_gcomw1, Incorrect file number"
	  exit 1 
	fi 
	
    	set 	"sdrfileList_lr='${sdrList_lr}'" 			\
		"sdrfileList_hr='${sdrList_hr}'" 			\
		"pathFMSDR='${fmsdrDir}/'"  				\
	    	"iOutpFM_accur=${outFMaccur}" 				\
		"prefCheck='${PerfsMonitorPath}/${prefixFMaccur}'" 	\
		"iFMtype=${iFMtype}" 					\
	    	"nScanL_lr_2skip=${nScanL_amsua_2skip}" 		\
		"nScanL_hr_2skip=${nScanL_mhs_2skip}"			\
    	    	"ScanLindx_hr_TimeColloc=${ScanLindx_mhs_TimeColloc}" 	\
		"norbits2process=${norbits2process}" 	  		\
	    	"logFile='${Logfile}'"					\
		"GeogrLimits=${geoLimit}"		 		\
		"minLat=${minLat}"			 		\
		"maxLat=${maxLat}"			 		\
		"minLon=${minLon}"			 		\
		"maxLon=${maxLon}"
		
    	CreatNamList ${fm_cntrl} '&ContrlFM' "$@"
    	DoMake ${fmSrc} ${makeOrNot}
    	${binDir}/fm_${satId} < ${fm_cntrl}
	checkStatus $? "fm_${satId}"
	
	#---modify nedt values after the footprint matching process (noise amplification/reduction)
    	cp ${nedtFileIn} ${nedtFileOut}

    elif [[ "${satId}" == "mtma" ]] ; then
	
	sdrList_madras=${inputDataPath}/mtma_madras_sdrFiles_${rdirExt}.list

	if [[ ${processingMode} -eq 1 ]] ; then
	  ls -1 ${sdrDir_amsua}/SDR_* > ${sdrList_madras}
	else
	  ls -1 ${sdrDir_amsua}/SDR_.${OrbitInfo}* > ${sdrList_madras}
	fi
	
	local nfile1=`wc -l ${sdrList_madras}   | awk '{print $1}'`
	
	if [[ ${nfile1} -eq 0  ]] ; then
	  op_msg "Error: Incorrect file number"
	  exit 1 
	fi 
	
    	set 	"sdrfileList='${sdrList_madras}'" 		        \
		"pathFMSDR='${fmsdrDir}/'"  				\
		"FMresol=${iFMtype}" 					\
		"GeogrLimits=${geoLimit}"		 		\
		"minLat=${minLat}"			 		\
		"maxLat=${maxLat}"			 		\
		"minLon=${minLon}"			 		\
		"maxLon=${maxLon}"                                      \
	    	"LogFile='${Logfile}'"					
		
    	CreatNamList ${fm_cntrl} '&ContrlFM' "$@"
    	DoMake ${fmSrc} ${makeOrNot}
    	${binDir}/fm_${satId} < ${fm_cntrl}
	checkStatus $? "fm_${satId}"
	
	#---modify nedt values after the footprint matching process (noise amplification/reduction)
    	cp ${nedtFileIn} ${nedtFileOut}

    elif [[ "${satId}" == "mtsa" ]] ; then
	
	echo "FM for mtsa"
	sdrList_saphir=${inputDataPath}/mtsa_saphir_sdrFiles_${rdirExt}.list

	if [[ ${processingMode} -eq 1 ]] ; then
 	  ls -1 ${sdrDir_amsua}/SDR_* > ${sdrList_saphir}
	else
 	  ls -1 ${sdrDir_amsua}/SDR_.${OrbitInfo}* > ${sdrList_saphir}
	fi
	
	local nfile1=`wc -l ${sdrList_saphir} | awk '{print $1}'`
	
	if [[ ${nfile1} -eq 0 ]] ; then
	  op_msg "Error: Incorrect file number"
	  exit 1 
	fi 
	
    	set 	"sdrfileList='${sdrList_saphir}'" 		\
		"pathFMSDR='${fmsdrDir}/'"  				\
		"FMresol=${iFMtype}" 					\
		"GeogrLimits=${geoLimit}"		 		\
		"minLat=${minLat}"			 		\
		"maxLat=${maxLat}"			 		\
		"minLon=${minLon}"			 		\
		"maxLon=${maxLon}"                                      \
	    	"LogFile='${Logfile}'"					
		
    	CreatNamList ${fm_cntrl} '&ContrlFM' "$@"
    	DoMake ${fmSrc} ${makeOrNot}
    	${binDir}/fm_${satId} < ${fm_cntrl}
	checkStatus $? "fm_${satId}"
	
	#---modify nedt values after the footprint matching process (noise amplification/reduction)
    	cp ${nedtFileIn} ${nedtFileOut}

    fi
    
    echo "End of step fm" 
}


#===============================================================
# Name:		    prepNWP
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Prepare NWP step, pick out nwp binary needed for a set of FMSDR files
#
#
# Input Variables:
# 	- fmsdrPath: FMSDR data path
# 	- rdirAnalysExt: NWP data extension 
# 	- extResol: Extension resolution
# 	- satId: Satellite ID ( n18/n19/f16/npp/trmm, etc )
# 	- nwpDataSrc: 1-gdas, 2-ecmwf, 3-gfs
# 	- srcPrepNWP : src directory of locating prepNWP.f90
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- binDir: Bin directory where all executables reside
# 	- controlDataPath: control data directory for those namelist, etc
#       - nwpGridPath: nwp gridded data path
#
#  
# Usg: prepNWP ${fmsdrPath} ${rdirAnalysExt} "${extResol}" ${satId} ${gfsData} \
#      ${nwpGenAnalysSrc} ${makeOrNot} ${binPath} ${controlDataPath} ${nwpGfsGridPath} \
#      ${gridSfcNwpAnalysList} ${gridAtmNwpAnalysList}
#  
# 
# Outputs:
#      - 1         	means nwp files needed are complete
#      - 0  		means nwp files needed are NOT complete 
#      - 2	        NO FMSDR files
#      - 3	        prepNWP executable get a problem
#
#===============================================================
function prepNWP() {
    if [[ $# -ne 12 ]]
    then
	op_msg "Error: function prepNWP: check number of arguments: $#"
	exit 1      
    fi

    local fmsdrPath=$1
    local rdirAnalysExt=$2
    local extResol=$3
    local satId=$4
    local nwpDataSrc=$5
    local srcPrepNWP=$6
    local makeOrNot=$7
    local binDir=$8
    local controlDataPath=$9
    local nwpGridPath=${10}
    local nwpSfcFileList=${11}
    local nwpAtmFileList=${12}
    
    local refId=gdas
    if [[ ${nwpDataSrc} -eq 1 ]] ; then
      refId=gdas
    elif [[ ${nwpDataSrc} -eq 2 ]] ; then
      refId=ecmwf
    elif [[ ${nwpDataSrc} -eq 3 ]] ; then
      refId=gfs
    fi 
    
    local fileComplete=1
    
    #echo "=============== RUNNING prepNWP( ${refId} ) STEP ======================="
    local RadFileList=${controlDataPath}/${satId}_fmFiles_prepNWP_${refId}_${rdirAnalysExt}_${extResol}
    local out_prepNWP=${controlDataPath}/${satId}_prepNWPOut_${refId}_${rdirAnalysExt}_${extResol}
    local namelist=${controlDataPath}/${satId}_namelist_prepNWP_${refId}_${rdirAnalysExt}_${extResol}
    
    if [[ ${fmsdrPath} == ${nwpGridPath} ]] ; then # data in the same location
      ls -1 ${fmsdrPath}/FMSDR*${extResol}* > ${RadFileList}  2> /dev/null
    else  
      ls -1 ${fmsdrPath}/${rdirAnalysExt}/FMSDR*${extResol}* > ${RadFileList} 2> /dev/null
    fi
    
    local nline=`wc -l ${RadFileList} | awk '{print $1}'`
    if [[ ${nline} -lt 1 ]] ; then
      #op_msg "Error: NO FMSDR file exist"
      #exit 1
      fileComplete=2
      echo $fileComplete
      return
    fi

    set "RadFileList='${RadFileList}'" \
        "satId='${satId}'"             \
        "fileOut='${out_prepNWP}'" 

    CreatNamList ${namelist} '&prepNWPNameList' "$@"
    DoMake ${src} ${makeOrNot}
    ${binDir}/prepNWP < ${namelist}
    checkStatus $? "prepNWP: ${satId} ${refId}"
    #if [[ $? -eq 0 ]] ; then
    #  echo 3
    #  return
    #fi
    
    #  now picked out starting and ending of nwp files
    
    local ymd0=${rdirAnalysExt}
    ymd_next=`date -d "${ymd0} + 1 day" +%Y-%m-%d`
    nline=`wc -l ${out_prepNWP} | awk '{print $1}'`

    declare -i line_min
    declare -i line_max

    declare -i sec1_min
    declare -i sec2_max
    declare -i sec

    declare -i year_min
    declare -i jday_min
    declare -i sec_min

    declare -i year_max
    declare -i jday_max
    declare -i sec_max

    let sec1_min=5000000000
    let sec2_max=0

    for (( iline=1; iline<=${nline}; iline++ )) ; do

      line=`sed -n "${iline},${iline} p" ${out_prepNWP}`

      year1=`echo $line | awk '{print $2}'`
      jday1=`echo $line | awk '{print $3}'`
      sec1=`echo $line  | awk '{print $4}'`

      year2=`echo $line | awk '{print $5}'`
      jday2=`echo $line | awk '{print $6}'`
      sec2=`echo $line  | awk '{print $7}'`

      ymd1=`jday2caldate ${year1} ${jday1}`
      ymd2=`jday2caldate ${year2} ${jday2}`

      sec=`date -d ${ymd1} +%s`
      let sec=sec+sec1
      if [[ ${sec} -lt ${sec1_min} ]] ; then
	let sec1_min=sec
	let line_min=iline
	let year_min=year1
	let jday_min=jday1
	let sec_min=sec1
      fi

      sec=`date -d ${ymd2} +%s`
      let sec=sec+sec2
      if [[ ${sec} -gt ${sec2_max} ]] ; then
	let sec2_max=sec
	let line_max=iline
	let year_max=year2
	let jday_max=jday2
	let sec_max=sec2
      fi

    done

    ymd_min=`jday2caldate ${year_min} ${jday_min}`
    hour_min=`echo ${sec_min} | awk '{printf "%i", $1/3600}'`

    ymd_max=`jday2caldate ${year_max} ${jday_max}`
    hour_max=`echo ${sec_max} | awk '{printf "%i", $1/3600}'`

    if [[ ${hour_min} -lt 10 ]] ; then
      hh_min=0${hour_min}
    else
      hh_min=${hour_min}
    fi

    if [[ ${hour_max} -lt 10 ]] ; then
      hh_max=0${hour_max}
    else
      hh_max=${hour_max}
    fi

    hhs=(0 6 12 18 24 30)

    # floor
    let index_start=hour_min/6

    # ceiling 
    let index_end=hour_max/6+1

    if [[ ${hour_min} -lt 24 ]] ; then
      ymd1=${ymd_min}
      hh1=${hhs[$index_start]}
    else
      ymd1=${ymd_next}
      hh1=0
    fi  

    if [[ ${hour_max} -lt 18 ]] ; then
      ymd2=${ymd_max}
      hh2=${hhs[$index_end]}
    elif [[ ${hour_max} -ge 18 && ${hour_max} -lt 24 ]] ; then
      ymd2=${ymd_next}
      hh2=0
    else
      ymd2=${ymd_next}
      hh2=6
    fi  

    if [[ ${hh1} -lt 10 ]] ; then
      hh1=0${hh1}
    fi

    if [[ ${hh2} -lt 10 ]] ; then
      hh2=0${hh2}
    fi

    # now list all files in between, inclusively, namely, including 2 ends

    ymdh_s=${ymd1}-${hh1}
    ymdh_e=${ymd2}-${hh2}
    
    fileRefList=${controlDataPath}/${satId}_${refId}_prepNWP_${rdirAnalysExt}_${extResol}
    
    getRefList ${ymdh_s} ${ymdh_e} > ${fileRefList}
    
    #local fileComplete=1
    
    local nfile=`wc -l ${fileRefList} | awk '{print $1}'`
    
    > ${nwpSfcFileList}_${refId}
    > ${nwpAtmFileList}_${refId}
    
    for (( iline=1; iline<=${nfile}; iline++ )) ; do
  
      line=`sed -n "${iline},${iline} p" ${fileRefList}`
      
      fileSfc=${nwpGridPath}/${refId}_sfc${line}
      fileAtm=${nwpGridPath}/${refId}_atm${line}
      
      echo ${fileSfc} >> ${nwpSfcFileList}_${refId}
      echo ${fileAtm} >> ${nwpAtmFileList}_${refId}
      
      if [[ ! -s ${fileSfc} || ! -s ${fileAtm} ]] ; then
         fileComplete=0
      fi
      
    done
    
    echo ${fileComplete}
    #echo "End of step prepNWP" 
}


#===============================================================
# Name:		    nwp
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	NWP collocation
#
#
# Input Variables:
# 	- nwpPath_GridAnalys: NWP grid analysis data path
# 	- rdirAnalysExt: RDR analysis extension
# 	- GridSfcNWPanalysList: NWP gridded surface analysis data list
# 	- GridAtmNWPanalysList: NWP gridded atmospheirc analysis data list
# 	- fmsdrPath: FMSDR data path
# 	- extResol: Extension resolution
# 	- fmsdrList4nwp: FMSDR data file list for NWP collocation
# 	- nwpDir_analys: NWP analysis data directory
# 	- Topogr: Topography file
# 	- CovBkgFileAtm: Atmospheric covariance background file
# 	- norbits2process: Number of orbits to process
# 	- Logfile: Log file
# 	- nprofs2fwd: Maximum number of profiles to simulate using the fwd operator (over analyses)
# 	- grid2nwp_cntrl: Control file of gridded NWP data
# 	- nwpGener_analys: NWP analysis data generation souce code directory
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- binDir: Bin directory where all executables reside
# 	- sensor_id: Sensor (1:N18,  2:METOPA)
# 	- rdirNextAnalysExt: next day's NWP data extension 
#
#
# Outputs:
#      - None
#
#===============================================================
function nwp() {
    if [[ $# -ne 21 ]]
    then
	op_msg "Error: function nwp: check number of arguments: $#"
	exit 1      
    fi
    local nwpPath_GridAnalys=$1
    local rdirAnalysExt=$2
    local GridSfcNWPanalysList=$3
    local GridAtmNWPanalysList=$4
    
    local fmsdrPath=$5
    local extResol=$6
    local fmsdrList4nwp=$7
    local nwpDir_analys=$8
    local Topogr=$9
    
    local CovBkgFileAtm=${10}
    local norbits2process=${11}
    local Logfile=${12}
    local nprofs2fwd=${13}
    local grid2nwp_cntrl=${14}
    
    local nwpGener_analys=${15}
    local makeOrNot=${16}
    local binDir=${17}
    local sensor_id=${18}
    local rdirNextAnalysExt=${19}

    local CRTM_Coeff_Path=${20}
    local nwpDataSrc=${21}
    
    local nwpstr='gdas'
    local NWPSTR='GDAS'
    if [[ ${nwpDataSrc} -eq 2 ]] ; then
      nwpstr='ecmwf'
      NWPSTR='ECMW'
    fi
    if [[ ${nwpDataSrc} -eq 3 ]] ; then
      nwpstr='gfs'
      NWPSTR='GFS'
    fi
    
    echo "=============== RUNNING NWP(${NWPSTR}) STEP ======================="
    
    if [[ ${fmsdrPath} == ${nwpPath_GridAnalys} ]] ; then
      ls -1 ${fmsdrPath}/FMSDR*${extResol}*  > ${fmsdrList4nwp}
    else
      ls -1 ${fmsdrPath}/${rdirAnalysExt}/FMSDR*${extResol}*  > ${fmsdrList4nwp}
    fi

    rm -f ${nwpDir_analys}/NWP_${NWPSTR}*.* 

    local nfile=`wc -l ${fmsdrList4nwp} | awk '{print $1}'`
    local ifile=1
    
    # way too many granules for NPP/ATMS
    local CPUuse=1
    if [[ ${sensor_id} -eq 6 ]] ; then
      CPUuse=0
    fi
    
    if [[ ${CPUuse} -eq 0 ]] ; then  # use only one CPU

      set "RadFileList='${fmsdrList4nwp}'"                        \
          "atmNWPFilesList='${GridAtmNWPanalysList}_${nwpstr}'"   \
          "sfcNWPFilesList='${GridSfcNWPanalysList}_${nwpstr}'"   \
          "pathNWPout='${nwpDir_analys}/'"                        \
          "Topogr='${Topogr}'"                                    \
          "CovBkgFileAtm='${CovBkgFileAtm}'"                      \
          "norbits2process=${norbits2process}"                    \
          "LogFile='${Logfile}'"                                  \
          "nprofs2process=${nprofs2fwd}"                          \
          "sensor_id=${sensor_id}"                                \
          "nwp_source=${nwpDataSrc}"                              \
	  "Coeff_Path='${CRTM_Coeff_Path}'"                       \
          "BiasFile='${biasFileToUse}'"                           \
          "TuningFile='${tune1File}'"
      CreatNamList ${grid2nwp_cntrl}_${nwpstr} '&ControlNWP' "$@"
      ${binDir}/colocNWPwRad < ${grid2nwp_cntrl}_${nwpstr}
      
      checkStatus $? "nwp: ${nwpstr}"

    else    
    
      #----Loop over the number of available files
      while (( $ifile <= $nfile ))
      do
	sed -n "$ifile,$ifile p" ${fmsdrList4nwp} > ${fmsdrList4nwp}_${ifile}
	set "RadFileList='${fmsdrList4nwp}_${ifile}'"               \
            "atmNWPFilesList='${GridAtmNWPanalysList}_${nwpstr}'"   \
            "sfcNWPFilesList='${GridSfcNWPanalysList}_${nwpstr}'"   \
            "pathNWPout='${nwpDir_analys}/'"                        \
            "Topogr='${Topogr}'"                                    \
            "CovBkgFileAtm='${CovBkgFileAtm}'"                      \
            "norbits2process=${norbits2process}"                    \
            "LogFile='${Logfile}'"                                  \
            "nprofs2process=${nprofs2fwd}"                          \
            "sensor_id=${sensor_id}"                                \
            "nwp_source=${nwpDataSrc}"                              \
            "Coeff_Path='${CRTM_Coeff_Path}'"                       \
            "BiasFile='${biasFileToUse}'"                           \
            "TuningFile='${tune1File}'"
	CreatNamList ${grid2nwp_cntrl}_${nwpstr}_${ifile} '&ControlNWP' "$@"
	#DoMake ${nwpGener_analys} ${makeOrNot}
	${binDir}/colocNWPwRad < ${grid2nwp_cntrl}_${nwpstr}_${ifile} &
	#checkStatus $? "nwp: ${nwpstr}"
	let ifile=ifile+1
      done
      wait
    
    fi
    
    echo "End of step nwp" 
}


#===============================================================
# Name:		    fwd
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Forward simulation
#
#
# Input Variables:
# 	- fwdSrc: Forward simulation source code directory
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- nwpDir_analys: NWP analysis data directory
# 	- NWPanalysList: NWP analysis file list
# 	- norbits2process: Number of orbits to process
# 	- fwdDir_analys: Forward simulation analysis data directory
# 	- SpcCoeffFile: Spatial coefficient file
# 	- TauCoeffFile: Tau coefficient file
# 	- CldOptPropFile: Cold Option Prop file
# 	- instr: instrument file
# 	- nprofs2fwd: Maximum number of profiles to simulate using the fwd operator (over analyses)
# 	- iAddDeviceNoise: 1-> Flag to add noise to the fwd simulations (over analyses), 0->no Noise added 
# 	- nedtFile: Nedt file
# 	- iPrintMonitor_fwd: 1->Yes, on-screen-monitoring of fwd-simul ,  =0-> Do not
# 	- Logfile: Log file
# 	- fwd_cntrl: Forward simulation control file
# 	- binDir: Bin directory where all executables reside
# 	- satId: Satellite short name(n18,metopA,f16,etc)
#
#
# Outputs:
#      - None
#
#===============================================================
function fwd() {
    if [[ $# -ne 20 ]]
    then
	op_msg "Error: function fwd: check number of arguments: $#"
	exit 1      
    fi
    local fwdSrc=$1
    local makeOrNot=$2
    local nwpDir_analys=$3
    local NWPanalysList=$4
    local norbits2process=$5
    local fwdDir_analys=$6
    local CRTM_Coeff_Path=$7
    local instr=${8}
    local nprofs2fwd=${9}
    local iAddDeviceNoise=${10}
    local nedtFile=${11}
    local iPrintMonitor_fwd=${12}
    local Logfile=${13}
    local fwd_cntrl=${14}
    local binDir=${15}
    local satId=${16}
    local sensorId=${17}
    local nwpDataSrc=${18}
    local ClrOrCloudy=${19}
    local rdirExt=${20}

    local nwpstr='gdas'
    local NWPSTR='GDAS'
    if [[ ${nwpDataSrc} -eq 2 ]] ; then
      nwpstr='ecmw'
      NWPSTR='ECMW'
    fi
    if [[ ${nwpDataSrc} -eq 3 ]] ; then
      nwpstr='gfs'
      NWPSTR='GFS'
    fi
    
    if [[ ${satId} == 'npp' ]] ; then
	local aveNedtFile=${nedtFile}
	local nedtPath=`dirname ${aveNedtFile}`
	local nedtGranPath=${nedtPath}/${rdirExt}
	local controlPath=`dirname ${fwd_cntrl}`
    fi

    echo "=============== RUNNING FWD(${NWPSTR}) STEP ======================="
    
    ls -1 ${nwpDir_analys}/NWP_${NWPSTR}* > ${NWPanalysList}_${nwpstr}
    rm -f ${fwdDir_analys}/FWD_${NWPSTR}*
    xarr=(`cat ${NWPanalysList}_${nwpstr}`)
    #---Test existence of NWP data files  
    nfiles=`wc -l ${NWPanalysList}_${nwpstr} | awk '{print $1}'`
    if [[ ${nfiles} -eq 0 ]] ; then
      op_msg "Error: NO NWP_${NWPSTR}* files exist under ${nwpDir_analys}"
      exit 1
    fi
      
    if [ ${nfiles} -gt ${norbits2process} ]
    then
        nfiles=${norbits2process}
    fi
    ifile=0
    #----Loop over the number of available files
    while (( $ifile < $nfiles ))
    do
      nwpfile=`basename ${xarr[$ifile]}`
      fwdfile=`echo ${nwpfile} |sed -e 's/NWP/FWD/g'`
      nwpfile=${nwpDir_analys}/$nwpfile
      fwdfile=${fwdDir_analys}/$fwdfile

      #---construct the namelist for the retrieval process
      if [[ ${satId} == "n18" || ${satId} == "metopA" || ${satId} == "n19" || ${satId} == "metopB" ]] ; then
        set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"       \
            "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
            "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
            "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
            "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
            "CntrlConfig_fwd%ChanSel(7)=1"  "CntrlConfig_fwd%ChanSel(8)=1"  "CntrlConfig_fwd%ChanSel(9)=1"        \
            "CntrlConfig_fwd%ChanSel(10)=1" "CntrlConfig_fwd%ChanSel(11)=1" "CntrlConfig_fwd%ChanSel(12)=1"       \
            "CntrlConfig_fwd%ChanSel(13)=1" "CntrlConfig_fwd%ChanSel(14)=1" "CntrlConfig_fwd%ChanSel(15)=1"       \
            "CntrlConfig_fwd%ChanSel(16)=1" "CntrlConfig_fwd%ChanSel(17)=1" "CntrlConfig_fwd%ChanSel(18)=1"       \
            "CntrlConfig_fwd%ChanSel(19)=1" "CntrlConfig_fwd%ChanSel(20)=1"				          \
    	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      elif [[ ${satId} == "f16" || "${satId}" == "f17" || ${satId} == "f18" ]] ; then
        set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"       \
    	    "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
    	    "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
    	    "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
    	    "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
    	    "CntrlConfig_fwd%ChanSel(7)=1"  "CntrlConfig_fwd%ChanSel(8)=1"  "CntrlConfig_fwd%ChanSel(9)=1"        \
    	    "CntrlConfig_fwd%ChanSel(10)=1" "CntrlConfig_fwd%ChanSel(11)=1" "CntrlConfig_fwd%ChanSel(12)=1"       \
    	    "CntrlConfig_fwd%ChanSel(13)=1" "CntrlConfig_fwd%ChanSel(14)=1" "CntrlConfig_fwd%ChanSel(15)=1"       \
    	    "CntrlConfig_fwd%ChanSel(16)=1" "CntrlConfig_fwd%ChanSel(17)=1" "CntrlConfig_fwd%ChanSel(18)=1"       \
    	    "CntrlConfig_fwd%ChanSel(19)=1" "CntrlConfig_fwd%ChanSel(20)=1" "CntrlConfig_fwd%ChanSel(21)=1"       \
    	    "CntrlConfig_fwd%ChanSel(22)=1" "CntrlConfig_fwd%ChanSel(23)=1" "CntrlConfig_fwd%ChanSel(24)=1"       \
    	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      elif [[ ${satId} == "aqua" ]] ; then
        set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"       \
    	    "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
    	    "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
    	    "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
    	    "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
    	    "CntrlConfig_fwd%ChanSel(7)=1"  "CntrlConfig_fwd%ChanSel(8)=1"  "CntrlConfig_fwd%ChanSel(9)=1"        \
    	    "CntrlConfig_fwd%ChanSel(10)=1" "CntrlConfig_fwd%ChanSel(11)=1" "CntrlConfig_fwd%ChanSel(12)=1"       \
    	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      elif [[ ${satId} == "gcomw1" ]] ; then
        set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"       \
    	    "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
    	    "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
    	    "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
    	    "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
    	    "CntrlConfig_fwd%ChanSel(7)=1"  "CntrlConfig_fwd%ChanSel(8)=1"  "CntrlConfig_fwd%ChanSel(9)=1"        \
    	    "CntrlConfig_fwd%ChanSel(10)=1" "CntrlConfig_fwd%ChanSel(11)=1" "CntrlConfig_fwd%ChanSel(12)=1"       \
    	    "CntrlConfig_fwd%ChanSel(13)=1" "CntrlConfig_fwd%ChanSel(14)=1"                                       \
    	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      elif [[ ${satId} == "trmm" ]] ; then
        set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"       \
    	    "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
    	    "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
    	    "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
    	    "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
    	    "CntrlConfig_fwd%ChanSel(7)=1"  "CntrlConfig_fwd%ChanSel(8)=1"  "CntrlConfig_fwd%ChanSel(9)=1"        \
    	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      elif [[ ${satId} == "gpm" ]] ; then
        set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"       \
    	    "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
    	    "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
    	    "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
    	    "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
    	    "CntrlConfig_fwd%ChanSel(7)=1"  "CntrlConfig_fwd%ChanSel(8)=1"  "CntrlConfig_fwd%ChanSel(9)=1"        \
    	    "CntrlConfig_fwd%ChanSel(10)=1" "CntrlConfig_fwd%ChanSel(11)=1" "CntrlConfig_fwd%ChanSel(12)=1"       \
    	    "CntrlConfig_fwd%ChanSel(13)=1"                                                                       \
	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      elif [[ ${satId} == "mtma" ]] ; then
        set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"       \
    	    "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
    	    "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
    	    "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
    	    "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
    	    "CntrlConfig_fwd%ChanSel(7)=1"  "CntrlConfig_fwd%ChanSel(8)=1"  "CntrlConfig_fwd%ChanSel(9)=1"        \
    	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      elif [[ ${satId} == "mtsa" ]] ; then
        set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"       \
    	    "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
    	    "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
    	    "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
    	    "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
    	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      elif [[ ${satId} == "fy3ri" ]] ; then
        set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"       \
    	    "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
    	    "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
    	    "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
    	    "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
    	    "CntrlConfig_fwd%ChanSel(7)=1"  "CntrlConfig_fwd%ChanSel(8)=1"  "CntrlConfig_fwd%ChanSel(9)=1"        \
    	    "CntrlConfig_fwd%ChanSel(10)=1"                                                                       \
    	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      elif [[ ${satId} == "npp" ]] ; then
	 fileName=`basename "${nwpfile}"`
	 timeInfo=`echo ${fileName} | cut -f3-9 -d "_"`
	 nedtGranFile=`ls ${nedtGranPath}/NEDT_*_${timeInfo}_ops_aftFM.dat 2> /dev/null`
	 #nedtGranFile=${nedtGranPath}/NEDT_SATMS_${timeInfo}_ops_aftFM.dat
	 if [ -f ${nedtGranFile} ] ; then  
	     nedtFile=${nedtGranFile}
	 else
	     nedtFile=${aveNedtFile}
	 fi

	 set "CntrlConfig_fwd%GeophInputFile='${nwpfile}'" "CntrlConfig_fwd%Coeff_Path='${CRTM_Coeff_Path}'"      \
    	    "CntrlConfig_fwd%InstrConfigFile='${instr}'"  "CntrlConfig_fwd%OutputFile='${fwdfile}'"	          \
    	    "CntrlConfig_fwd%nprofs2process=${nprofs2fwd}" "CntrlConfig_fwd%SensorID=${sensorId}"	          \
    	    "CntrlConfig_fwd%ChanSel(1)=1"  "CntrlConfig_fwd%ChanSel(2)=1"  "CntrlConfig_fwd%ChanSel(3)=1"        \
    	    "CntrlConfig_fwd%ChanSel(4)=1"  "CntrlConfig_fwd%ChanSel(5)=1"  "CntrlConfig_fwd%ChanSel(6)=1"        \
    	    "CntrlConfig_fwd%ChanSel(7)=1"  "CntrlConfig_fwd%ChanSel(8)=1"  "CntrlConfig_fwd%ChanSel(9)=1"        \
    	    "CntrlConfig_fwd%ChanSel(10)=1" "CntrlConfig_fwd%ChanSel(11)=1" "CntrlConfig_fwd%ChanSel(12)=1"       \
    	    "CntrlConfig_fwd%ChanSel(13)=1" "CntrlConfig_fwd%ChanSel(14)=1" "CntrlConfig_fwd%ChanSel(15)=1"       \
    	    "CntrlConfig_fwd%ChanSel(16)=1" "CntrlConfig_fwd%ChanSel(17)=1" "CntrlConfig_fwd%ChanSel(18)=1"       \
    	    "CntrlConfig_fwd%ChanSel(19)=1" "CntrlConfig_fwd%ChanSel(20)=1" "CntrlConfig_fwd%ChanSel(21)=1"       \
    	    "CntrlConfig_fwd%ChanSel(22)=1"                                                                       \
    	    "CntrlConfig_fwd%iAddDeviceNoise=${iAddDeviceNoise}" "CntrlConfig_fwd%iCldOffOrOn=${ClrOrCloudy}"     \
            "CntrlConfig_fwd%NoiseFile='${nedtFile}'" "CntrlConfig_fwd%iPrintMonitor=${iPrintMonitor_fwd}"        \
            "CntrlConfig_fwd%LogFile='${Logfile}'"
      fi	

      CreatNamList ${fwd_cntrl}_${ifile}_${nwpstr} '&ContrlP_fwd' "$@"
      ${binDir}/fwd < ${fwd_cntrl}_${ifile}_${nwpstr} &
      #checkStatus $? "fwd-${nwpstr}"
      let ifile=ifile+1
      echo
      echo "ifile=${ifile}"
      echo "Input NWP file=${nwpfile}"
      echo "Output FWD file=${fwdfile}"
    done
    
    wait
    
    echo "End of step fwd"
}


#===============================================================
# Name:		    biasGen
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Bias generation
#
#
# Input Variables:
# 	- determinBias: Determine bias source code directory
# 	- nwpDir_analys: NWP analysis data directory
# 	- extResol: Extension resolution
# 	- NWPanalysList_4Bias: NWP analysis data list for bias generation
# 	- fwdDir_analys: Forward simulation analysis data directory
# 	- FWDanalysList_4Bias: Forward simulation analysis data list for bias generation
# 	- fmsdrDir4Bias: FMSDR directory for bias verification
# 	- fmsdrList4Bias: FMSDR data file list for bias verification
# 	- BiasComputation_cntrl: Bias computation control file
# 	- iBiasComputMethod: Method for computing the bias. 0->Simple bias, 1->Histogram adjustment
# 	- biasFile: Bias file
# 	- ModelErrFile: Model error file
# 	- figsDir4Bias: Generated bias image directory
# 	- norbits2process: Number of orbits to process
# 	- Sensor: Sensor (1:N18,  2:METOPA)
# 	- IDL: IDL path
# 	- iSensor_ID: Sensor (1:N18,  2:METOPA)
# 	- Topogr: Topography file
# 	- binDir: Bin directory where all executables reside
# 	- modelErrNominalFile: 
#
#
# Outputs:
#      - None
#
#===============================================================
function biasGen() {
    if [[ $# -ne 26 ]]
    then
	op_msg "Error: function biasGen: check number of arguments: $#"
	exit 1      
    fi
    local determinBias=$1    
    local nwpDir_analys=$2    
    local extResol=$3    
    local NWPanalysList_4Bias=$4    
    local fwdDir_analys=$5    
    local FWDanalysList_4Bias=$6    
    local fmsdrDir4Bias=$7    
    local fmsdrList4Bias=$8    
    local biasGen_cntrl=$9    
    local iBiasComputMethod=${10}    
    local biasFile=${11}    
    local ModelErrFile=${12}    
    local figsDir4Bias=${13}    
    local norbits2process=${14}    
    local satId=${15}    
    local IDL=${16}
    local iSensor_ID=${17}
    local Topogr=${18}
    local binDir=${19}
    local modelErrNominalFile=${20}
    local controlDataPath=${21}
    local nwpDataSrc=${22}
    local minLat=${23}
    local maxLat=${24}
    local minLon=${25}
    local maxLon=${26}

    local nwpstr='gdas'
    local NWPSTR='GDAS'
    if [[ ${nwpDataSrc} -eq 2 ]] ; then
      nwpstr='ecmw'
      NWPSTR='ECMW'
    fi
    if [[ ${nwpDataSrc} -eq 3 ]] ; then
      nwpstr='gfs'
      NWPSTR='GFS'
    fi
    
    # from extension resolution --> fmType
    local fmType=0
    if [[ ${extResol} == "CR" ]] ; then
      fmType=-1
    elif [[ ${extResol} == "LR" ]] ; then 
      fmType=0
    elif [[ ${extResol} == "HR" ]] ; then 
      fmType=1
    elif [[ ${extResol} == "UAS" ]] ; then 
      fmType=0
    elif [[ ${extResol} == "LAS" ]] ; then 
      fmType=1
    elif [[ ${extResol} == "ENV" ]] ; then 
      fmType=2
    elif [[ ${extResol} == "IMG" ]] ; then 
      fmType=3
    fi
    
    echo "=============== RUNNING BIAS GENERATION STEP (${NWPSTR}) ======================="
    local biasFile_${nwpstr}=`echo ${biasFile} | sed -e "s/_${satId}_/_${satId}_${nwpstr}_/g"`
    local ModelErrFile_${nwpstr}=`echo ${ModelErrFile} | sed -e "s/_${satId}_/_${satId}_${nwpstr}_/g"`

    ls -1 ${nwpDir_analys}/NWP_${NWPSTR}*${extResol}* > ${NWPanalysList_4Bias}_${nwpstr}
    ls -1 ${fwdDir_analys}/FWD_${NWPSTR}*${extResol}* > ${FWDanalysList_4Bias}_${nwpstr}
    ls -1 ${fmsdrDir4Bias}/FMSDR*${extResol}*         > ${fmsdrList4Bias}
    
    echo '&Namelist_Calib_generic_rad'                    >  ${biasGen_cntrl}_${nwpstr}
    echo "fileListNWP='${NWPanalysList_4Bias}_${nwpstr}'" >> ${biasGen_cntrl}_${nwpstr}
    echo "fileListFWD='${FWDanalysList_4Bias}_${nwpstr}'" >> ${biasGen_cntrl}_${nwpstr}
    echo "fileListFM='${fmsdrList4Bias}'"		  >> ${biasGen_cntrl}_${nwpstr}
    echo "iBiasComputMethod=${iBiasComputMethod}"	  >> ${biasGen_cntrl}_${nwpstr}
    echo "fileBias='${biasFile}_${nwpstr}'"  		  >> ${biasGen_cntrl}_${nwpstr}
    echo "fileErr='${ModelErrFile}_${nwpstr}'"		  >> ${biasGen_cntrl}_${nwpstr}
    echo "sensorId=${iSensor_ID}"			  >> ${biasGen_cntrl}_${nwpstr}
    echo "fmType=${fmType}"			          >> ${biasGen_cntrl}_${nwpstr}
    echo "minlat=${minLat}"			          >> ${biasGen_cntrl}_${nwpstr}
    echo "maxlat=${maxLat}"			          >> ${biasGen_cntrl}_${nwpstr}
    echo "minlon=${minLon}"			          >> ${biasGen_cntrl}_${nwpstr}
    echo "maxlon=${maxLon}"			          >> ${biasGen_cntrl}_${nwpstr}
    echo "/"                                              >> ${biasGen_cntrl}_${nwpstr}
    
    ${binDir}/Calib_generic_rad  < ${biasGen_cntrl}_${nwpstr}
    
    echo "End of step biasGen" 
}


#===============================================================
# Name:		    chopp
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Chopp a full orbit into some sub-orbits
#
#
# Input Variables:
# 	- fmsdrDir: FMSDR data directory
# 	- fmsdrList4Chopping: FMSDR date file list for chopping
# 	- sensor: Sensor (1:N18,  2:METOPA)
# 	- fmsdrDir_Chopp: Chopped FMSDR data directory
# 	- nChoppedFilesPerOrbit: Number of chopped sub-orbits per orbit. If 1 no chopping is done
# 	- Logfile: Log file
# 	- chopp_cntrl: Chopp control file
# 	- choppSrc: Chopp src dir
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- binDir: Bin directory where all executables reside
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
#
#
# Outputs:
#      - None
#
#===============================================================
function chopp() {
    if [[ $# -ne 13 ]]
    then
	op_msg "Error: function chopp: check number of arguments: $#"
	exit 1      
    fi
    local fmsdrDir=$1
    local fmsdrList4Chopping=$2
    local sensor=$3
    local fmsdrDir_Chopp=$4
    local nChoppedFilesPerOrbit=$5
    local Logfile=$6
    local chopp_cntrl=$7
    local choppSrc=$8
    local makeOrNot=$9
    local binDir=${10}
    local processingMode=${11}
    local OrbitInfo=${12}
    local extResol=${13}

    echo "=============== RUNNING CHOPPING FMSDR STEP ======================="
    rm -f ${fmsdrDir_Chopp}/CHP* #Purge chopped FMSDRs that may already exist
    ConstructList ${processingMode} ${fmsdrDir}  ${OrbitInfo} ${fmsdrList4Chopping} "FM*" "*${extResol}"
    DataExistEst ${sensor} "FM-SDR" ${fmsdrList4Chopping}
    set "fmsdrfileList='${fmsdrList4Chopping}'" \
    	"pathFMSDR_Chopp='${fmsdrDir_Chopp}/'"  \
	"nChoppedFiles=${nChoppedFilesPerOrbit}"\
	"LogFile='${Logfile}'"
    CreatNamList ${chopp_cntrl} '&ContrlChopping' "$@"
    DoMake ${choppSrc} ${makeOrNot}
    ${binDir}/chopp < ${chopp_cntrl}
    checkStatus $? "chopp"
    echo "End of step chopp" 
}



#===============================================================
# Name:		    applyRegress
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Apply regression algorithm
#
#
# Input Variables:
# 	- fmsdrDir: FMSDR data directory
# 	- extResol: Extension resolution
# 	- fmsdrList4Applyregress: FMSDR data file ist for applying regression
# 	- regrRetrdir: Regression retrieval directory
# 	- Topogr: Topography file
# 	- CovBkgFileAtm: Atmospheric covariance background file
# 	- CovBkgFileSfc: Surface covariance background file
# 	- Logfile: Log file
# 	- regress_cntrl: Regression control file
# 	- applyRegressAlgSrc: 
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- binDir: Bin directory where all executables reside
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
# 	- regressCoeffFile_oc: Ocean regression coefficient file
# 	- regressCoeffFile_seaice: Sea ice regression coefficient file
# 	- regressCoeffFile_land: Land regression coefficient file
# 	- regressCoeffFile_snow: Snow regression coefficient file
# 	- retrOnOrbitOrSubOrbit: Switch between performing retr on full-orbits (0) or chopped ones (1)
# 	- fmsdrDir_Chopp: Chopped FMSDR data directory
# 	- iSensor_ID: Sensor (1:N18,  2:METOPA)
# 	- nOrbits2Process: Number of orbits to process
#
#
# Outputs:
#      - None
#
#===============================================================
function applyRegress() {
    if [[ $# -ne 49 ]]
    then
	op_msg "Error: function applyRegress: check number of arguments: $#"
	exit 1      
    fi
    local fmsdrDir=$1
    local extResol=$2
    local fmsdrList4Applyregress=$3
    local regrRetrdir=$4
    
    local Topogr=$5
    local CovBkgFileAtm=$6
    local CovBkgFileSfc=$7
    local Logfile=${8}
    local regress_cntrl=${9}
    
    local applyRegressAlgSrc=${10}
    local makeOrNot=${11}
    local binDir=${12}
    local processingMode=${13}
    local OrbitInfo=${14}
    
    local regressCoeffFile_oc_clw=${15} 
    local regressCoeffFile_seaice_clw=${16}
    local regressCoeffFile_land_clw=${17}
    local regressCoeffFile_snow_clw=${18}

    local regressCoeffFile_oc_tskin=${19} 
    local regressCoeffFile_seaice_tskin=${20}
    local regressCoeffFile_land_tskin=${21}
    local regressCoeffFile_snow_tskin=${22}

    local regressCoeffFile_oc_tpw=${23} 
    local regressCoeffFile_seaice_tpw=${24}
    local regressCoeffFile_land_tpw=${25}
    local regressCoeffFile_snow_tpw=${26}

    local regressCoeffFile_oc_em=${27} 
    local regressCoeffFile_seaice_em=${28}
    local regressCoeffFile_land_em=${29}
    local regressCoeffFile_snow_em=${30}

    local regressCoeffFile_oc_wv=${31} 
    local regressCoeffFile_seaice_wv=${32}
    local regressCoeffFile_land_wv=${33}
    local regressCoeffFile_snow_wv=${34}

    local regressCoeffFile_oc_temp=${35} 
    local regressCoeffFile_seaice_temp=${36}
    local regressCoeffFile_land_temp=${37}
    local regressCoeffFile_snow_temp=${38}

    local regressCoeffFile_oc_gwp=${39} 
    local regressCoeffFile_seaice_gwp=${40}
    local regressCoeffFile_land_gwp=${41}
    local regressCoeffFile_snow_gwp=${42}

    local retrOnOrbitOrSubOrbit=${43}
    local fmsdrDir_Chopp=${44}
    local iSensor_ID=${45}
    
    local nOrbits2Process=${46}
    local biasFile=${47}
    
    local tuneFile=${48}
    local algSN=${49}

    local ext1="FM*"
    local ext2="*"

    #-----Switch between full-orbit and chopped orbit depending on retrOnOrbitOrSubOrbit
    if [ ${retrOnOrbitOrSubOrbit} == 0 ]
    then
	fmsdrDir0=${fmsdrDir}
	pref=${ext1}
    elif [ ${retrOnOrbitOrSubOrbit} == 1 ]
    then
	fmsdrDir0=${fmsdrDir_Chopp}
	pref="CHP*"
    fi
    if [ ${processingMode} == 0 ]
	then
	rm -f ${regrRetrdir}REGRESS*${OrbitInfo}* #purge files to avoid confusion in post-processing stage
    else
	rm -f ${regrRetrdir}/REGRESS* #purge files to avoid confusion in post-processing stage
    fi
    echo "=============== APPLICATION OF REGRESSION ALGORITHMS ======================="
    #-----Number of algorithms
    nAlgsSensor2Use=6
    if [ ${iSensor_ID} == 9 ]
    then
	nAlgsSensor2Use=7
    fi
    ConstructList ${processingMode} ${fmsdrDir0} ${OrbitInfo} ${fmsdrList4Applyregress} "${pref}" "*${extResol}*"
    DataExistEst "FM-SDR" "for regression" ${fmsdrList4Applyregress}
    set "nAlgors=${nAlgsSensor2Use}" 						\
    	"algorsfiles(1,1)='${regressCoeffFile_oc_clw}'" 	\
	"algorsfiles(1,2)='${regressCoeffFile_seaice_clw}'" 	\
	"algorsfiles(1,3)='${regressCoeffFile_land_clw}'" 	\
	"algorsfiles(1,4)='${regressCoeffFile_snow_clw}'" 	\
    	"algorsfiles(2,1)='${regressCoeffFile_oc_tskin}'" 	\
	"algorsfiles(2,2)='${regressCoeffFile_seaice_tskin}'" 	\
	"algorsfiles(2,3)='${regressCoeffFile_land_tskin}'" 	\
	"algorsfiles(2,4)='${regressCoeffFile_snow_tskin}'" 	\
    	"algorsfiles(3,1)='${regressCoeffFile_oc_tpw}'" 	\
	"algorsfiles(3,2)='${regressCoeffFile_seaice_tpw}'" 	\
	"algorsfiles(3,3)='${regressCoeffFile_land_tpw}'" 	\
	"algorsfiles(3,4)='${regressCoeffFile_snow_tpw}'" 	\
    	"algorsfiles(4,1)='${regressCoeffFile_oc_em}'" 	        \
	"algorsfiles(4,2)='${regressCoeffFile_seaice_em}'" 	\
	"algorsfiles(4,3)='${regressCoeffFile_land_em}'" 	\
	"algorsfiles(4,4)='${regressCoeffFile_snow_em}'" 	\
    	"algorsfiles(5,1)='${regressCoeffFile_oc_wv}'" 	        \
	"algorsfiles(5,2)='${regressCoeffFile_seaice_wv}'" 	\
	"algorsfiles(5,3)='${regressCoeffFile_land_wv}'" 	\
	"algorsfiles(5,4)='${regressCoeffFile_snow_wv}'" 	\
    	"algorsfiles(6,1)='${regressCoeffFile_oc_temp}'" 	\
	"algorsfiles(6,2)='${regressCoeffFile_seaice_temp}'" 	\
	"algorsfiles(6,3)='${regressCoeffFile_land_temp}'" 	\
	"algorsfiles(6,4)='${regressCoeffFile_snow_temp}'" 	\
    	"algorsfiles(7,1)='${regressCoeffFile_oc_gwp}'" 	\
	"algorsfiles(7,2)='${regressCoeffFile_seaice_gwp}'" 	\
	"algorsfiles(7,3)='${regressCoeffFile_land_gwp}'" 	\
	"algorsfiles(7,4)='${regressCoeffFile_snow_gwp}'" 	\
	"ListRadMeasFiles='${fmsdrList4Applyregress}'" 		\
	"pathRegress='${regrRetrdir}/'" 			\
	"Topogr='${Topogr}'"     				\
        "CovBkgFileAtm='${CovBkgFileAtm}'" 			\
	"CovBkgFileSfc='${CovBkgFileSfc}'" 			\
	"LogFile='${Logfile}'" 					\
	"BiasFile='${biasFile}'"				\
        "sensor_id=${iSensor_ID}" 				\
	"nOrbits2Process=${nOrbits2Process}"			\
	"TuningFile='${tuneFile}'"				\
	"AlgSN=${algSN}"
    CreatNamList ${regress_cntrl} '&ContrlRegress' "$@"
    DoMake ${applyRegressAlgSrc} ${makeOrNot}
    ${binDir}/ApplyRegress < ${regress_cntrl}
    checkStatus $? "applyRegress"
    echo "End of step applyRegress" 
}



#===============================================================
# Name:		    fmsdr2edr
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	FMSDR2EDR
#
#
# Input Variables:
# 	- fmsdr2edr: FMSDR2EDR source code directory
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- retrOnWhichSDR: Switch between retr on empirically-corrected TBs(0), uncorrected(1) or NWP-based simuls(2)
# 	- fmsdrDir: FMSDR data directory
# 	- fwdDir_analys: Forward simulation analysis data directory
# 	- retrOnOrbitOrSubOrbit: Switch between performing retr on full-orbits (0) or chopped ones (1)
# 	- fmsdrDir_Chopp: Chopped FMSDR data directory
# 	- fmsdrList: FMSDR data file List
# 	- norbits2process: Number of orbits to process
# 	- ExternDataAvailab: 1->External data available, use ExtDataUse; 0 -> No Ext data available,
# 	- iSrcExtData: 
# 	- nwpDir_analys: NWP analysis data directory
# 	- NWPanalysList_retr: Retrived NWP analysis data file list
# 	- regrRetrdir: Regression retrieval directory
# 	- edrDir: EDR data directory
# 	- nprofs2retr: Maximum number of profiles to retrieve
# 	- RetrErrCharac: Retrieval error character
# 	- MonitorIterat: =1->Yes, monitor iterative process; 0-> Do not
# 	- nAttempts: Number of retrieval attempts in case of non-convergence
# 	- PrintMonitor: =1->Yes, on-screen-monitoring of retrieval;  =0-> Do not.
# 	- GeogrLimits: 0-> Process all data, 1-> only in the lat/lon box, 2->Only ocean, 3->Only land
# 	- minLat: Min latitude of data to be processed (only if GeogrLimits=1)
# 	- maxLat: Max latitude of data to be processed (only if GeogrLimits=1)
# 	- minLon: Min longitude of data to be processed (only if GeogrLimits=1)
# 	- maxLon: Max longitude of data to be processed (only if GeogrLimits=1)
# 	- Pass: Orbit(s) 2 process: 0:ascending, 1:descending, 2:both
# 	- iSensor_ID: Sensor (1:N18,  2:METOPA)
# 	- tunFile1: Tune file 1
# 	- tunFile2: Tune file 2
# 	- CovBkgFileAtm: Atmospheric covariance background file
# 	- CovBkgFileSfc: Surface covariance background file
# 	- ModelErrFile2Use: Model error file to use
# 	- extBkgAtmFile: Spatially/temporally varying background file (means, no covariances)
# 	- extBkgAtmUse: 1->Use Spatially/spatially varying background file, 0-> Do not use.
# 	- nedtFileAfterFM: 
# 	- MonitorFile: Monitoring file
# 	- Topogr: Topography file
# 	- SpcCoeffFile: Spatial coefficient file
# 	- TauCoeffFile: Tau coefficient file
# 	- CldOptPropFile: Cold Option Prop file
# 	- Logfile: Log file
# 	- fmsdr2edr_cntrl: The control file of FMSDR2EDR
# 	- CPUuse: 0->Use one CPU at a time, 1->Use all available CPUs at once to speed up
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
# 	- binDir: Bin directory where all executables reside
#
#
# Outputs:
#      - None
#
#===============================================================
function fmsdr2edr() {
    if [[ $# -ne 50 ]]
          then
          op_msg "Error: function fmsdr2edr: check number of arguments: $#"
          exit 1
    fi
    
    local fmsdr2edr=$1 ; shift
    local makeOrNot=$1 ; shift
    local retrOnWhichSDR=$1 ; shift  
    local fmsdrDir=$1 ; shift
    
    local fwdDir_analys=$1 ; shift
    local retrOnOrbitOrSubOrbit=$1 ; shift
    local fmsdrDir_Chopp=$1 ; shift
    local fmsdrList=$1 ; shift
    
    local norbits2process=$1 ; shift
    local ExternDataAvailab=$1 ; shift
    local iSrcExtData=$1 ; shift
    local nwpDir_analys=$1 ; shift
    
    local NWPanalysList_retr=$1 ; shift
    local regrRetrdir=$1 ; shift
    local edrDir=$1 ; shift
    local nprofs2retr=$1 ; shift

    local MonitorIterat=$1 ; shift
    local nAttempts=$1 ; shift
    local PrintMonitor=$1 ; shift
    local GeogrLimits=$1 ; shift
    
    local minLat=$1 ; shift
    local maxLat=$1 ; shift
    local minLon=$1 ; shift
    local maxLon=$1 ; shift
    local Pass=$1 ; shift
    local iSensor_ID=$1 ; shift
    
    local tunFile1=$1 ; shift
    local tunFile2=$1 ; shift
    local CovBkgFile1Atm=$1 ; shift
    local CovBkgFile2Atm=$1 ; shift
    
    local CovBkgFile1Sfc=$1 ; shift
    local CovBkgFile2Sfc=$1 ; shift

    local ModelErrFile1ToUse=$1 ; shift
    local ModelErrFile2ToUse=$1 ; shift
    
    local extBkgAtmFile=$1 ; shift
    local extBkgAtmUse=$1 ; shift

    local nedtFileAfterFM=$1 ; shift
    local MonitorFile=$1 ; shift
    local Topogr=$1 ; shift

    local CRTM_Coeff_Path=$1 ; shift
    local Logfile=$1 ; shift
    
    local fmsdr2edr_cntrl=$1 ; shift
    local CPUuse=$1 ; shift
    local processingMode=$1 ; shift
    local OrbitInfo=$1 ; shift
    local binDir=$1 ; shift
    
    local biasFile=$1 ; shift
    local algSN=$1; shift
    local extResol=$1; shift
    local rdirExt=$1
  
    echo "=============== RUNNING FMSDR->EDR STEP ======================="
    DoMake ${fmsdr2edr} ${makeOrNot}

    if [[ ${iSensor_ID} == 6 ]] ; then
	local aveNedtFile=${nedtFileAfterFM}
	local nedtPath=`dirname ${aveNedtFile}`
	local nedtGranPath=${nedtPath}/${rdirExt}
	local controlPath=`dirname ${fmsdr2edr_cntrl}`
    fi

    #-----Switch between retr non-EC TBs (depend on retrOnWhichSDR)
    if [ ${retrOnWhichSDR} == 1 ] #Use non-empirically-corrected TBs
    then
	fmsdrDir0=${fmsdrDir}
	pref0='FM*'
    elif [ ${retrOnWhichSDR} == 2 ] #Use NWP-based simulated TBs
    then
	fmsdrDir0=${fwdDir_analys}
	pref0='FWD*'
    fi
    
    #-----Switch between full-orbit and chopped orbit depending on retrOnOrbitOrSubOrbit
    if [ ${retrOnOrbitOrSubOrbit} == 0 ]
    then
	fmsdrDir0=${fmsdrDir0}
	pref=${pref0}
    elif [ ${retrOnOrbitOrSubOrbit} == 1 ]
    then
	fmsdrDir0=${fmsdrDir_Chopp}
	pref='CHP*'
	extResol="${extResol}_*"
    fi
    
    ConstructList ${processingMode} ${fmsdrDir0} ${OrbitInfo} ${fmsdrList} "${pref}" "*${extResol}"
    nfiles=`wc -l ${fmsdrList} | awk '{print $1}'`
    if [[ ${nfiles} -eq 0 ]] ; then
      op_msg "Error: No FMSDR files"
      exit 1
    fi

    echo "nfiles=${nfiles}"
    xarr=(`cat ${fmsdrList}`)
    if [ ${nfiles} -gt ${norbits2process} ]
    then
	nfiles=${norbits2process}
    fi
    
    #----list of external data if requested
    if [ ${ExternDataAvailab} == 1 ] 
    then 
	if [ ${iSrcExtData} == 1 ] 
        then 
	    ConstructList ${processingMode} ${nwpDir_analys} ${OrbitInfo} ${NWPanalysList_retr} "*" ""
	elif [ ${iSrcExtData} == 2 ] 
	then 
	    ConstructList ${processingMode} ${regrRetrdir} ${OrbitInfo} ${NWPanalysList_retr} "REGRESS*" "*${extResol}"
	fi
	xarrExt=(`cat ${NWPanalysList_retr}`)
        DataExistEst "external" "for retrieval" ${NWPanalysList_retr}
        #nfilesExt=$?
	local nfilesExt=`wc -l ${NWPanalysList_retr} | awk '{print $1}'`
        #----Consistency checks
	if [ ${nfiles} -ne ${nfilesExt} ]
	then
	    op_msg "Warning: Number of files inconsistent between Radiances and External data: ${nfiles} ${nfilesExt}"
	    exit 1
	fi
    fi
    
    #----Loop over the number of available FM-SDR files
    #rm -f ${edrDir}/* #purge files to avoid confusion in post-processing stage
    if [ ${processingMode} == 0 ]
	then
	rm -f ${edrDir}*${OrbitInfo}* #purge files to avoid confusion in post-processing stage
    else
	rm -f ${edrDir}/EDR* #purge files to avoid confusion in post-processing stage
    fi
    local ifile=0
    local jfile=1
 
    local ncpu=16  # how many CPUs will be used here, check /proc/cpuinfo to get number of cpus
    if [[ ${nfiles} -lt 32 ]] ; then

      # start outer while loop ifile_out
      local ifile_out=0
      while (( ${ifile_out} < ${nfiles} ))
      do

	let bot=ifile_out
	let top=ifile_out+ncpu
	if [[ ${top} -gt ${nfiles} ]] ; then
          let top=nfiles
	fi
	let ind_end=top-1
	
	# inner loop
	for (( ifile=${bot}; ifile<${top}; ifile++ )); do  # start ifile inner loop

	  let jfile=ifile+1
	  radfile=${xarr[${ifile}]}
	  radfile=`basename ${radfile}`
	  if [ ${retrOnOrbitOrSubOrbit} == 0 ]
	  then
	      if [ ${retrOnWhichSDR} == 1 ] #Use non-empirically-corrected TBs
	      then
		  edrfile=`echo ${radfile} |sed -e 's/FMSDR/EDR/g'`    
	      elif [ ${retrOnWhichSDR} == 2 ] #Use NWP-based simulated TBs
	      then
		  edrfile=`echo ${radfile} |sed -e 's/FWD/EDR/g'`    
	      fi
	      edrfile=${edrfile}'.ORB'
	  fi
	  if [ ${retrOnOrbitOrSubOrbit} == 1 ]
	  then
	      edrfile=`echo ${radfile} |sed -e 's/CHPDR/EDR/g'`    
	  fi
	  radfile=${fmsdrDir0}/${radfile}
	  #edrfile=${edrDir}/${edrfile}
	  if [ ${processingMode} == 0 ]
	  then
            edrfile=${edrDir}${edrfile}
	  else
	   edrfile=${edrDir}/${edrfile} 
	  fi

	  if [ ${ExternDataAvailab} == 1 ] 
	  then
	      nwpfile=${xarrExt[${ifile}]}
	  fi
	  #get NEDT file at ATMS granule level
	  if [[ ${iSensor_ID} == 6 ]] ; then
	      fileName=`basename "${radfile}"`
	      timeInfo=`echo ${fileName} | cut -f3-7 -d "_"`
	      nedtFile=`ls ${nedtGranPath}/NEDT_*_${timeInfo}_*ops_aftFM.dat 2> /dev/null`
	      #nedtFile=${nedtGranPath}/NEDT_SATMS_${timeInfo}_ops_aftFM.dat
	      if [[ -z ${nedtFile} ]] ; then  
		  nedtFileAfterFM=${aveNedtFile}
	      else
		  nedtFileAfterFM=${nedtFile}
	      fi
	  fi
	  echo "Processing File# ${jfile} ${radfile}"
	  #---construct the namelist for the retrieval process
	  set "CntrlConfig%AlgSN=${algSN}" "CntrlConfig%SensorID=${iSensor_ID}"                                 \
              "CntrlConfig%nprofs2process=${nprofs2retr}"				                        \
	      "CntrlConfig%MonitorIterat=${MonitorIterat}" "CntrlConfig%nAttempts=${nAttempts}"              	\
	      "CntrlConfig%ExternDataAvailab=${ExternDataAvailab}" "CntrlConfig%PrintMonitor=${PrintMonitor}"	\
	      "CntrlConfig%GeogrLimits=${GeogrLimits}" "CntrlConfig%pass=${Pass}"                    		\
	      "CntrlConfig%minLat=${minLat}" "CntrlConfig%maxLat=${maxLat}"                              	\
	      "CntrlConfig%minLon=${minLon}" "CntrlConfig%maxLon=${maxLon}"             			\
	      "CntrlConfig%MeasurmtFile='${radfile}'" "CntrlConfig%BiasFile='${biasFile}'"			\
	      "CntrlConfig%TuningFile(1)='${tunFile1}'" "CntrlConfig%TuningFile(2)='${tunFile2}'"        	\
	      "CntrlConfig%OutputFile='${edrfile}'"                                                      	\
              "CntrlConfig%CovBkgFileAtm(1)='${CovBkgFile1Atm}'"                                           	\
              "CntrlConfig%CovBkgFileAtm(2)='${CovBkgFile2Atm}'"                                        	\
	      "CntrlConfig%CovBkgFileSfc(1)='${CovBkgFile1Sfc}'"                                            	\
	      "CntrlConfig%CovBkgFileSfc(2)='${CovBkgFile2Sfc}'"                                             	\
	      "CntrlConfig%BkgAtmFile_Ext='${extBkgAtmFile}'"                                             	\
	      "CntrlConfig%useBkgExt=${extBkgAtmUse}"                                             	        \
              "CntrlConfig%FileModelErr(1)='${ModelErrFile1ToUse}'"                                        	\
              "CntrlConfig%FileModelErr(2)='${ModelErrFile2ToUse}'"                                          	\
	      "CntrlConfig%ExternDataFile='${nwpfile}'" "CntrlConfig%NoiseFile='${nedtFileAfterFM}'"        	\
	      "CntrlConfig%MonitorFile='${MonitorFile}'" "CntrlConfig%Topogr='${Topogr}'"                   	\
	      "CntrlConfig%Coeff_Path='${CRTM_Coeff_Path}'"  "CntrlConfig%LogFile='${Logfile}'"		      
	  CreatNamList ${fmsdr2edr_cntrl}_${jfile} '&ContrlP' "$@"

	  ${binDir}/1dvar < ${fmsdr2edr_cntrl}_${jfile} &
	  checkStatus $? "fmsdr2edr"

	done # end ifile inner loop
	
	# wait those inner files finish
	wait
	
	echo "Finish one bunch: ${bot} -> ${ind_end} ( 0-based index )"
	let ifile_out=ifile_out+ncpu

      done  # end outer while loop ifile_out

    else

      while (( ${ifile} < ${nfiles} ))
      do
	let jfile=ifile+1
	radfile=${xarr[${ifile}]}
	radfile=`basename ${radfile}`
	if [ ${retrOnOrbitOrSubOrbit} == 0 ]
	then
	    if [ ${retrOnWhichSDR} == 1 ] #Use non-empirically-corrected TBs
	    then
		edrfile=`echo ${radfile} |sed -e 's/FMSDR/EDR/g'`    
	    elif [ ${retrOnWhichSDR} == 2 ] #Use NWP-based simulated TBs
	    then
		edrfile=`echo ${radfile} |sed -e 's/FWD/EDR/g'`    
	    fi
	    edrfile=${edrfile}'.ORB'
	fi
	if [ ${retrOnOrbitOrSubOrbit} == 1 ]
	then
	    edrfile=`echo ${radfile} |sed -e 's/CHPDR/EDR/g'`    
	fi
	radfile=${fmsdrDir0}/${radfile}
	#edrfile=${edrDir}/${edrfile}
	if [ ${processingMode} == 0 ]
	then
          edrfile=${edrDir}${edrfile}
	else
	 edrfile=${edrDir}/${edrfile} 
	fi

	if [ ${ExternDataAvailab} == 1 ] 
	then
	    nwpfile=${xarrExt[${ifile}]}
	fi
	#get NEDT file at ATMS granule level
	if [[ ${iSensor_ID} == 6 ]] ; then
	    fileName=`basename "${radfile}"`
	    timeInfo=`echo ${fileName} | cut -f3-7 -d "_"`
	    nedtFile=`ls ${nedtGranPath}/NEDT_*_${timeInfo}_*ops_aftFM.dat 2> /dev/null`
	    #nedtFile=${nedtGranPath}/NEDT_SATMS_${timeInfo}_ops_aftFM.dat
	    if [[ -z ${nedtFile} ]] ; then  
	        nedtFileAfterFM=${aveNedtFile}
	    else
	        nedtFileAfterFM=${nedtFile}
	    fi
	fi
	echo "Processing File# ${jfile} ${radfile}"
	#---construct the namelist for the retrieval process
	set "CntrlConfig%AlgSN=${algSN}" "CntrlConfig%SensorID=${iSensor_ID}"                                   \                  "CntrlConfig%nprofs2process=${nprofs2retr}"				                               \
	    "CntrlConfig%MonitorIterat=${MonitorIterat}" "CntrlConfig%nAttempts=${nAttempts}"                	\
	    "CntrlConfig%ExternDataAvailab=${ExternDataAvailab}" "CntrlConfig%PrintMonitor=${PrintMonitor}"	\
	    "CntrlConfig%GeogrLimits=${GeogrLimits}" "CntrlConfig%pass=${Pass}"                    		\
	    "CntrlConfig%minLat=${minLat}" "CntrlConfig%maxLat=${maxLat}"                                   	\
	    "CntrlConfig%minLon=${minLon}" "CntrlConfig%maxLon=${maxLon}"             				\
	    "CntrlConfig%MeasurmtFile='${radfile}'" "CntrlConfig%BiasFile='${biasFile}'"			\
	    "CntrlConfig%TuningFile(1)='${tunFile1}'" "CntrlConfig%TuningFile(2)='${tunFile2}'"              	\
	    "CntrlConfig%OutputFile='${edrfile}'"                                                            	\
            "CntrlConfig%CovBkgFileAtm(1)='${CovBkgFile1Atm}'"                                                  \
            "CntrlConfig%CovBkgFileAtm(2)='${CovBkgFile2Atm}'"                                                  \
	    "CntrlConfig%CovBkgFileSfc(1)='${CovBkgFile1Sfc}'"                                                  \
	    "CntrlConfig%CovBkgFileSfc(2)='${CovBkgFile2Sfc}'"                                                  \
            "CntrlConfig%BkgAtmFile_Ext='${extBkgAtmFile}'"                                             	\
	    "CntrlConfig%useBkgExt=${extBkgAtmUse}"                                             	        \
            "CntrlConfig%FileModelErr(1)='${ModelErrFile1ToUse}'"                                               \
            "CntrlConfig%FileModelErr(2)='${ModelErrFile2ToUse}'"                                               \
	    "CntrlConfig%ExternDataFile='${nwpfile}'" "CntrlConfig%NoiseFile='${nedtFileAfterFM}'"              \
	    "CntrlConfig%MonitorFile='${MonitorFile}'" "CntrlConfig%Topogr='${Topogr}'"                         \
	    "CntrlConfig%Coeff_Path='${CRTM_Coeff_Path}'" "CntrlConfig%LogFile='${Logfile}'"		      
	CreatNamList ${fmsdr2edr_cntrl}_${jfile} '&ContrlP' "$@"
	if [ ${CPUuse} == 0 ]  # use only one CPU
	then
	    ${binDir}/1dvar < ${fmsdr2edr_cntrl}_${jfile} 
            checkStatus $? "fmsdr2edr"
	elif [ ${CPUuse} == 1 ]  # use multiple CPUs
	then
	    ${binDir}/1dvar < ${fmsdr2edr_cntrl}_${jfile} &
            checkStatus $? "fmsdr2edr"
	elif [ ${CPUuse} == 2 ]  # this option only for cluster machines where qsub is available
	then
	    echo "#!/bin/sh" 					  > ${fmsdr2edr_cntrl}_${ifile}_qsub
	    echo "#PBS -N ${satId}_1dvar_${jfile}"	         >> ${fmsdr2edr_cntrl}_${jfile}_qsub
	    echo "#PBS -o  ${Logfile}_${jfile}_qsub"	         >> ${fmsdr2edr_cntrl}_${jfile}_qsub
	    echo "#PBS -e  ${Logfile}_${jfile}_qsub"	         >> ${fmsdr2edr_cntrl}_${jfile}_qsub
	    echo "${binDir}/1dvar < ${fmsdr2edr_cntrl}_${jfile}" >> ${fmsdr2edr_cntrl}_${jfile}_qsub
	    /bin/chmod 755 ${fmsdr2edr_cntrl}_${jfile}_qsub
	    /usr/local/bin/qsub ${fmsdr2edr_cntrl}_${jfile}_qsub 
	    checkStatus $? "fmsdr2edr"
       fi
	let ifile=ifile+1
      done 

    fi
    
    # this option only for our cluster machine where jobs are submitted in qsub
    if [[ ${CPUuse} -eq 2 ]] ; then
      wait1dvar ${satId}
    else 
      wait
    fi

    echo "End of step fmsdr2edr" 
}


#===============================================================
# Name:		    wait1dvar
#
# Type:		    Bash Shell Script
#
# Description:
#	To check if all 1dvar jobs finish in qsub
#
#
# Input Variables:
# 	- satId: satellite ID ( n18, metopA, f16 )
#
# Outputs:
#      - None
#
#===============================================================
function wait1dvar() {
  local satId=$1
  echo "waiting 1dvar qsub jobs..."
  while [[ 1 -eq 1 ]] ; do
    qsubStatus=`/usr/local/bin/qstat | grep ${satId}_1dvar_`
    if [[ ${qsubStatus} == "" ]] ; then
      echo "All 1dvar qsub jobs are done."
      break
    else
      sleep 30
    fi
  done
}


#===============================================================
# Name:		    mergeEdr
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Merge chopped EDRs
#
#
# Input Variables:
# 	- edrDir: EDR data directory
# 	- extResol: Extension resolution
# 	- edrList4Merging: EDR data list for merging
# 	- Sensor: Sensor (1:N18,  2:METOPA)
# 	- nChoppedFilesPerOrbit: Number of chopped sub-orbits per orbit. If 1 no chopping is done
# 	- Logfile: Log file
# 	- mergeEDR_cntrl: The control file to merge chopped EDR files
# 	- mergeEDR: Merge EDR source code directory
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- binDir: Bin directory where all executables reside
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
#
#
# Outputs:
#      - None
#
#===============================================================
function mergeEdr() {
    if [[ $# -ne 12 ]]
    then
	op_msg "Error: function mergeEdr: check number of arguments"
	exit 1      
    fi
    local edrDir=$1
    local extResol=$2
    local edrList4Merging=$3
    local Sensor=$4
    local nChoppedFilesPerOrbit=$5
    local Logfile=$6
    local mergeEDR_cntrl=$7
    local mergeEDR=$8
    local makeOrNot=$9
    local binDir=${10}
    local processingMode=${11}
    local OrbitInfo=${12}

    echo "=============== RUNNING MERGING EDR STEP ======================="
    # Generate the list for output full orbit EDR files based on the mini files for each orbit
    if [ ${processingMode} -eq 0 ] #orbit processing
	then
	ls -1 ${edrDir}*${OrbitInfo}*${extResol}_??? | sed 's/_[0-9][0-9][0-9]$//'| uniq > ${edrList4Merging}

    fi
    if [ ${processingMode} -eq 1 ] #Daily processing
	then
	ls -1 ${edrDir}/EDR*${extResol}_??? | sed 's/_[0-9][0-9][0-9]$//' | uniq > ${edrList4Merging}
    fi
    xarr=(`cat ${edrList4Merging}`)
    DataExistEst ${Sensor} "MINI-EDR" ${edrList4Merging}
    nfiles=$?
    #----Loop over the full orbit files in the list to merge available correspondent mini files 
    ifile=0
    while (( ${ifile} < ${nfiles} ))
    do
      edrOrbFile=${xarr[$ifile]}
      nMiniEDRperOrbit=`ls ${edrOrbFile}_??? |wc -l|awk '{print $1}'`
      #----Consistency checks
      if [ ${nMiniEDRperOrbit} -ne ${nChoppedFilesPerOrbit} ]
	  then
	  op_msg "Warning: Number of files inconsistent between chopping and merging steps"
	  op_msg "nMiniEDRperOrbit=${nMiniEDRperOrbit}"
	  op_msg "nChoppedFilesPerOrbit=${nChoppedFilesPerOrbit}"
	  op_msg "EXIT Now!"
	  exit 1
      fi
      set 	"FullOrbFileName='${edrOrbFile}'" 	\
      		"pathEDR='${edrDir}/'" 			\
	  	"nMiniEDRfiles=${nMiniEDRperOrbit}" 	\
		"LogFile='${Logfile}'" 
      CreatNamList ${mergeEDR_cntrl}_${ifile} '&ContrlMergeEDR' "$@"
      DoMake ${mergeEDR} ${makeOrNot}
      ${binDir}/mergeEDR < ${mergeEDR_cntrl}_${ifile}
      checkStatus $? "mergeEdr"
      let ifile=ifile+1
      echo "Processed file:${ifile}"
    done

    echo "End of step mergeEdr"
}



#===============================================================
# Name:		    vipp
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	???
#
#
# Input Variables:
# 	- edrList4VIPP: EDR list for vipp
# 	- edrDir4VIPP: EDR Dir for Vipp data
# 	- pathDEPOut: Output path for DEP
# 	- LogFile: Log file
# 	- norbits2process: Number of orbits to process
# 	- nprofs2process: Number of profiles to process
# 	- sensor_id: Sensor (1:N18,  2:METOPA)
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
# 	- extResol: Extension resolution
# 	- vipp_cntrl: control file
# 	- vipp_src: VIPP src dir
# 	- binDir: Bin directory where all executables reside
#
#
# Outputs:
#      - None
#
#===============================================================
function vipp() {
    if [[ $# -ne 15 ]]
	then
	op_msg "Error: function vipp : check number of arguments"
	exit 1
    fi
    local edrList4VIPP=$1
    local edrDir4VIPP=$2
    local pathDEPOut=$3
    local LogFile=$4
    local norbits2process=$5
    local nprofs2process=$6
    local sensor_id=$7
    local processingMode=$8
    local orbitInfo=$9
    local extResol=${10}
    local vipp_cntrl=${11}
    local vipp_src=${12}
    local binDir=${13}
    local seaIceCatalog=${14}
    local snowCatalog=${15}

    echo "=============== RUNNING VIPP STEP ======================="
    rm -f ${pathDEPOut}/DEP*  #purge DEP files that may already exist
    ConstructList ${processingMode} ${edrDir4VIPP}   ${orbitInfo} ${edrList4VIPP} "EDR*" "*"${extResol}".ORB"
    set "edrFilesList='${edrList4VIPP}'"	\
        "SeaiceFile='${seaIceCatalog}'"		\
        "SnowCoverFile='${snowCatalog}'"	\
        "pathDEPOut='${pathDEPOut}/'"		\
	"LogFile='${LogFile}'"			\
        "norbits2process=${norbits2process}"   	\
	"nprofs2process=${nprofs2process}"     	\
	"sensor_id=${sensor_id}"
    CreatNamList ${vipp_cntrl} '&VIPPNMLST' "$@"
    DoMake ${vipp_src} ${makeOrNot}
    ${binDir}/vipp < ${vipp_cntrl}
    checkStatus $? "vipp"
    echo "End of step vipp"
}


#===============================================================
# Name:		    verifBias
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Bias verification
#
#
# Input Variables:
# 	- determinBias: Determine bias source code directory
# 	- nwpDir_analys: NWP analysis data directory
# 	- extResol: Extension resolution
# 	- NWPanalysList_4Bias: NWP analysis data list for bias generation
# 	- fwdDir_analys: Forward simulation analysis data directory
# 	- FWDanalysList_4Bias: Forward simulation analysis data list for bias generation
# 	- fmsdrDir4Bias: FMSDR data directory for bias generation
# 	- fmsdrList4Bias: FMSDR data list for bias generation
# 	- edrDir4Bias: EDR data directory for bias generation
# 	- edrList4Bias: EDR data list for bias generation
# 	- BiasVerification_cntrl: The control file for bias verification
# 	- biasFileCheck: The file to used to check bias
# 	- figsDir4Bias: Generated bias image directory
# 	- norbits2process: Number of orbits to process
# 	- sensor_id: Sensor (1:N18,  2:METOPA)
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
#
#
# Outputs:
#      - None
#
#===============================================================
function verifBias() {
    if [[ $# -ne 18 ]]
	then
	op_msg "Error: function VerifBias: check number of arguments"
	exit 1
    fi
    local determinBias=$1
    local nwpDir_analys=$2
    local extResol=$3
    local NWPanalysList_4Bias=$4
    local fwdDir_analys=$5
    local FWDanalysList_4Bias=$6
    local fmsdrDir4Bias=$7
    local fmsdrList4Bias=$8
    local edrDir4Bias=$9
    local edrList4Bias=${10}
    local BiasVerification_cntrl=${11}
    local biasFileCheck=${12}
    local figsDir4Bias=${13}
    local norbits2process=${14}
    local sensor_id=${15}
    local processingMode=${16}
    local OrbitInfo=${17}
    local controlDataPath=${18}
    
    echo "=============== RUNNING VERIF-BIAS STEP ======================="
    cd ${determinBias}
    ConstructList ${processingMode} ${nwpDir_analys}   ${OrbitInfo} ${NWPanalysList_4Bias} "NWP*" "*"${extResol}"*"
    ConstructList ${processingMode} ${fwdDir_analys}   ${OrbitInfo} ${FWDanalysList_4Bias} "FWD*" "*"${extResol}"*"
    ConstructList ${processingMode} ${fmsdrDir4Bias}   ${OrbitInfo} ${fmsdrList4Bias}      "FM*"    ${extResol}"*"
    ConstructList ${processingMode} ${edrDir4Bias}     ${OrbitInfo} ${edrList4Bias}        "EDR*"    ${extResol}"*"
    echo ${NWPanalysList_4Bias}                            >  ${BiasVerification_cntrl}
    echo ${FWDanalysList_4Bias}                            >> ${BiasVerification_cntrl}
    echo ${fmsdrList4Bias}                                 >> ${BiasVerification_cntrl}
    echo ${edrList4Bias}                                   >> ${BiasVerification_cntrl}
    echo ${biasFileCheck}                                  >> ${BiasVerification_cntrl}
    echo ${figsDir4Bias}'/MonitorRadnGeophBias.ps'         >> ${BiasVerification_cntrl}
    echo ${norbits2process}                                >> ${BiasVerification_cntrl}
    echo "!QUIET=1"                                                      			> ${controlDataPath}/batchCalibRadAndGeoph.pro
    echo ".r Calib_generic_rad_geoph.pro"                                                      >> ${controlDataPath}/batchCalibRadAndGeoph.pro
    echo "Calib_generic_rad_geoph,Namelist='${BiasVerification_cntrl}',sensor_id=${sensor_id}" >> ${controlDataPath}/batchCalibRadAndGeoph.pro
    echo "exit"                                               	                               >> ${controlDataPath}/batchCalibRadAndGeoph.pro
    ${IDL} ${controlDataPath}/batchCalibRadAndGeoph.pro
    checkStatus $? "verifBias"
    rm -f ${controlDataPath}/batchCalibRadAndGeoph.pro
    echo "End of step verifBias"
}


#===============================================================
# Name:		    gridGen
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Level III data(Gridded Data) generation
#	Apply to both EDRs and NWP Collocated Gdas data.
#
#
# Input Variables:
# 	- edrDir: EDR data directory
# 	- extResol: Extension resolution
# 	- edrList: EDR file list
# 	- gridSrc: Grid generation source code directory
# 	- gridDir: Generated image directory
# 	- satId: satellite ID(n18,metopA,f16,etc)
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
# 	- fileExt: File extension ( yyyy-mm-dd, etc)
#	- makeOrNot: on-the-fly make or not if executable not there
#	- binDir: Executable location
# 	- grid_cntrl: The control file to generate gridded data
#
# Outputs:
#      	- None
#
#===============================================================
function gridGen() {
    if [[ $# -ne 24 ]]
        then
        op_msg "Error: function gridGen: check number of arguments: $#"
        exit 1
    fi
    local processingMode=${1}
    local edrDir=$2
    local depDir=$3
    local OrbitInfo=$4
    local edrList=$5
    local depList=$6
    local extResol=$7
    local satId=$8
    local fileExt=$9
    local gridSrc=${10}
    local gridDir=${11}
    local makeOrNot=${12}
    local binDir=${13}
    local grid_cntrl=${14}
    local gridFactor=${15}
    local latmin=${16}
    local latmax=${17}
    local lonmin=${18}
    local lonmax=${19}
    local fmType=${20}
    local edrGridStr=${21}
    local edrP2PStr=${22}
    local depGridStr=${23}
    local depP2PStr=${24}
    
    echo "=============== GRID & P2P DATA GENERATION STEP ======================="
    ConstructList ${processingMode} ${edrDir} ${OrbitInfo} ${edrList} "*" "*"${extResol}"*"
    ConstructList ${processingMode} ${depDir} ${OrbitInfo} ${depList} "*" "*"${extResol}"*"
    
    if [[ ${processingMode} -eq 1 ]] ; then
        local yyyymmdd=`echo $fileExt | cut -c1-4,6-7,9-10`
    else
        local yyyymmdd=${OrbitInfo}
    fi
    
    #EDR grid
    #echo "EDR grid"
    set "filesList='${edrList}'"	     	\
	"satId='${satId}'"		     	\
	"yyyymmdd='${yyyymmdd}'" 	     	\
	"gridfactor=${gridFactor}"	     	\
	"gridPath='${gridDir}/'" 	     	\
	"latmin=${latmin}"		     	\
	"latmax=${latmax}"		     	\
	"lonmin=${lonmin}"  		     	\
	"lonmax=${lonmax}"		     	\
	"isMirs=0"			     	\
	"processMode=${processingMode}"         \
	"fmType=${fmType}"                      \
	"prodStr='${edrGridStr}'"
    CreatNamList ${grid_cntrl}_gridEdr '&gridEdrNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/gridEdr < ${grid_cntrl}_gridEdr
    checkStatus $? "gridGen:gridEdr"
    
    #EDR p2p
    #echo "EDR p2p"
    set "filesList='${edrList}'"	     	\
	"satId='${satId}'"		     	\
	"yyyymmdd='${yyyymmdd}'" 	     	\
	"p2pPath='${gridDir}/'" 	     	\
	"latmin=${latmin}"		     	\
	"latmax=${latmax}"		     	\
	"lonmin=${lonmin}"  		     	\
	"lonmax=${lonmax}"		     	\
	"isMirs=0"			     	\
	"processMode=${processingMode}"         \
	"fmType=${fmType}"                      \
	"prodStr='${edrP2PStr}'"
    CreatNamList ${grid_cntrl}_p2pEdr '&p2pEdrNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/p2pEdr < ${grid_cntrl}_p2pEdr
    checkStatus $? "gridGen:p2pEdr"

    #DEP grid
    #echo "DEP grid"
    set "filesList='${depList}'" 		\
    	"satId='${satId}'" 			\
	"yyyymmdd='${yyyymmdd}'" 		\
	"gridfactor=${gridFactor}"		\
        "gridPath='${gridDir}/'"  		\
       	"latmin=${latmin}"		     	\
       	"latmax=${latmax}"		     	\
       	"lonmin=${lonmin}"  		     	\
       	"lonmax=${lonmax}"		     	\
	"processMode=${processingMode}"         \
	"fmType=${fmType}"                      \
	"prodStr='${depGridStr}'"
    CreatNamList ${grid_cntrl}_gridDep '&gridDepNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/gridDep < ${grid_cntrl}_gridDep
    checkStatus $? "gridGen:gridDep"

    #DEP p2p
    #echo "DEP p2p"
    set "filesList='${depList}'" 		\
    	"satId='${satId}'" 			\
	"yyyymmdd='${yyyymmdd}'" 		\
        "p2pPath='${gridDir}/'"  		\
       	"latmin=${latmin}"		     	\
       	"latmax=${latmax}"		     	\
       	"lonmin=${lonmin}"  		     	\
       	"lonmax=${lonmax}"		     	\
	"processMode=${processingMode}"         \
	"fmType=${fmType}"                      \
	"prodStr='${depP2PStr}'"
    CreatNamList ${grid_cntrl}_p2pDep '&p2pDepNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/p2pDep < ${grid_cntrl}_p2pDep
    checkStatus $? "gridGen:p2pDep"

    echo "End of step gridGen"
}


#===============================================================
# Name:		    gridClimate
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	To generate climate gridded products(1.0 and 2.5 degree)
#
#
# Input Variables:
# 	- yyyymmdd_start: 2009-02-10
# 	- yyyymmdd_end:  2009-02-14
# 	- satId: satellite ID(n18,metopA,f16,etc)
# 	- edrDir: EDR directory
# 	- depDir: DEP directory
#	- edrList: EDR files list
#	- depList: DEP files list
#	- girdPath: generated bin data location
#	- climateType: 0-pentad,1-weekly,2-monthly
#	- binDir: Executable location
# 	- grid_cntrl: The control file to generate gridded data
#
# Outputs:
#      	- None
#
#===============================================================
function gridClimate() {
    if [[ $# -ne 11 ]]
        then
        op_msg "Error: function gridClimate: check number of arguments: $#"
        exit 1
    fi
    local yyyymmdd_start=$1
    local yyyymmdd_end=$2
    local satId=$3
    local edrDir=$4
    local depDir=$5
    local edrList=$6
    local depList=$7
    local gridDir=${8}
    local climateType=${9}
    local binDir=${10}
    local grid_cntrl=${11}
    
    echo "=============== RUNNING GRID CLIMATE STEP ====================="
    > ${edrList}
    > ${depList}
    
    local yyyymmdd=$yyyymmdd_start
    ls -1 ${edrDir}/${yyyymmdd}/EDR* >> ${edrList}
    ls -1 ${depDir}/${yyyymmdd}/DEP* >> ${depList}
    while [[ $yyyymmdd != $yyyymmdd_end ]] ; do
        yyyymmdd=`date -d "$yyyymmdd +1 day" "+%Y-%m-%d"`
        ls -1 ${edrDir}/${yyyymmdd}/EDR* >> ${edrList}
        ls -1 ${depDir}/${yyyymmdd}/DEP* >> ${depList}
    done

    #EDR 1.0 degree gridding
    set "filesList='${edrList}'"	     	\
	"satId='${satId}'"		     	\
	"yyyymmdd_start='${yyyymmdd_start}'"	\
	"yyyymmdd_end='${yyyymmdd_end}'"	\
	"gridfactor=1.0"	     		\
	"gridPath='${gridDir}/'"		\
	"climateType=${climateType}"
	
    CreatNamList ${grid_cntrl}_edr_climate_1.0 '&GridNameList' "$@"
    ${binDir}/gridEdrClimate < ${grid_cntrl}_edr_climate_1.0
 
    #EDR 2.5 degree gridding
    set "filesList='${edrList}'"	     	\
	"satId='${satId}'"		     	\
	"yyyymmdd_start='${yyyymmdd_start}'"	\
	"yyyymmdd_end='${yyyymmdd_end}'"	\
	"gridfactor=0.4"	     		\
	"gridPath='${gridDir}/'"		\
	"climateType=${climateType}"
    CreatNamList ${grid_cntrl}_edr_climate_2.5 '&GridNameList' "$@"
    ${binDir}/gridEdrClimate < ${grid_cntrl}_edr_climate_2.5
 
    #DEP 1.0 degree gridding
    set "filesList='${depList}'"	     	\
	"satId='${satId}'"		     	\
	"yyyymmdd_start='${yyyymmdd_start}'" 	\
	"yyyymmdd_end='${yyyymmdd_end}'" 	\
	"gridfactor=1.0"	     		\
	"gridPath='${gridDir}/'"		\
	"climateType=${climateType}"
    CreatNamList ${grid_cntrl}_dep_climate_1.0 '&GridNameList' "$@"
    ${binDir}/gridDepClimate < ${grid_cntrl}_dep_climate_1.0
 
    #DEP 2.5 degree gridding
    set "filesList='${depList}'"	     	\
	"satId='${satId}'"		     	\
	"yyyymmdd_start='${yyyymmdd_start}'"	\
	"yyyymmdd_end='${yyyymmdd_end}'"	\
	"gridfactor=0.4"	     		\
	"gridPath='${gridDir}/'"		\
	"climateType=${climateType}"
    CreatNamList ${grid_cntrl}_dep_climate_2.5 '&GridNameList' "$@"
    ${binDir}/gridDepClimate < ${grid_cntrl}_dep_climate_2.5
 
    echo "End of step gridClimate"
}


#===============================================================
# Name:		    dynamicBackground
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	To generate 2.5 degree dynamic back ground data.
#
#
# Input Variables:
# 	- yyyymmdd_start: 2009-02-10
# 	- yyyymmdd_end:  2009-02-14
# 	- satId: satellite ID(n18,metopA,f16,etc)
# 	- edrDir: EDR directory
# 	- depDir: DEP directory
#	- edrList: EDR files list
#	- depList: DEP files list
#	- girdPath: generated bin data location
#	- climateType: 0-pentad,1-weekly,2-monthly
#	- binDir: Executable location
# 	- grid_cntrl: The control file to generate gridded data
#
# Outputs:
#      	- None
#
#===============================================================
function dynamicBackground() {
    if [[ $# -ne 8 ]]
        then
        op_msg "Error: function dynamicBackground: check number of arguments: $#"
        exit 1
    fi
    local yyyymmdd_start=$1
    local yyyymmdd_end=$2
    local satId=$3
    local edrDir=$4
    local edrList=$5
    local gridDir=${6}
    local binDir=${7}
    local grid_cntrl=${8}
    
    echo "=============== RUNNING DYNAMIC BACKGROUND STEP ====================="
    > ${edrList}
    
    local yyyymmdd=$yyyymmdd_start
    ls -1 ${edrDir}/${yyyymmdd}/EDR* >> ${edrList}
    while [[ $yyyymmdd != $yyyymmdd_end ]] ; do
        yyyymmdd=`date -d "$yyyymmdd +1 day" "+%Y-%m-%d"`
        ls -1 ${edrDir}/${yyyymmdd}/EDR* >> ${edrList}
    done

    #2.5 degree gridding
    set "edrList='${edrList}'"	\
	"satId='${satId}'"	\
	"gridstep=2.5"		\
	"gridPath='${gridDir}/'"
    CreatNamList ${grid_cntrl}_dynamicBackground '&GridNameList' "$@"
    ${binDir}/dynamicBackground < ${grid_cntrl}_dynamicBackground
 
    echo "End of step dynamicBackground"
}


#===============================================================
# Name:		    gridRain
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Level III data(Gridded Data) generation
#	Apply to both EDRs and NWP Collocated Gdas data.
#
#
# Input Variables:
# 	- edrDir: EDR data directory
# 	- extResol: Extension resolution
# 	- edrList: EDR file list
# 	- gridSrc: Grid generation source code directory
# 	- gridDir: Generated image directory
# 	- satId: satellite ID(n18,metopA,f16,etc)
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
# 	- fileExt: File extension ( yyyy-mm-dd, etc)
#	- makeOrNot: on-the-fly make or not if executable not there
#	- binDir: Executable location
# 	- grid_cntrl: The control file to generate gridded data
#
# Outputs:
#      	- None
#
# Usg:
#	gridRain ${testbedDataPath} ${inputDataPath} ${controlDataPath} ${orbitInfo}\
#	${gridSrc} ${makeOrNot} ${binPath} ${gridFactor} ${minLat} ${maxLat} ${minLon} ${maxLon} 
#
#
#===============================================================
function gridRain() {
    if [[ $# -ne 12 ]]
        then
        op_msg "Error: function gridRain: check number of arguments: $#"
        exit 1
    fi
    local testbedDataPath=$1
    local inputDataPath=$2
    local controlDataPath=$3
    local fileExt=$4
    local gridSrc=$5
    local makeOrNot=$6
    local binDir=$7
    local gridFactor=$8
    local latmin=${9}
    local latmax=${10}
    local lonmin=${11}
    local lonmax=${12}
    
    local allDepPath=${testbedDataPath}/Outputs/dep
    local allGridPath=${testbedDataPath}/Outputs/grid
    
    local depList1=${inputDataPath}/n18_dep_list_rain_${fileExt}
    local depList2=${inputDataPath}/metopA_dep_list_rain_${fileExt}
    local depList3=${inputDataPath}/f16_dep_list_rain_${fileExt}
    local grid_cntl=${controlDataPath}/grid_rain_${fileExt}
    
    ls -1 ${allDepPath}/n18_amsua_mhs/${fileExt}/DEP* > ${depList1}
    ls -1 ${allDepPath}/metopA_amsua_mhs/${fileExt}/DEP* > ${depList2}
    ls -1 ${allDepPath}/f16_ssmis/${fileExt}/DEP* > ${depList3}
    
    local yyyymmdd=`echo $fileExt | cut -c1-4,6-7,9-10`

    #Rain gridding
    set "filesList(1)='${depList1}'" 		\
    	"filesList(2)='${depList2}'" 		\
    	"filesList(3)='${depList3}'" 		\
	"yyyymmdd='${yyyymmdd}'" 		\
	"gridfactor=${gridFactor}"		\
        "gridPath='${allGridPath}/'"  		\
       	"latmin=${latmin}"		     	\
       	"latmax=${latmax}"		     	\
       	"lonmin=${lonmin}"  		     	\
       	"lonmax=${lonmax}"		     	\

    CreatNamList ${grid_cntrl} '&GridRainNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/gridRain < ${grid_cntrl}_rain
    checkStatus $? "gridRain:gridRain"
    
    echo "End of step gridRain"
}


#===============================================================
# Name:		    figsGen
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Gridded data figs generation
#
#
# Input Variables:
# 	- satId: satellite ID(n18,metopA,f16,etc)
#	- gridfactor: grid factor(1,2,4,etc)
# 	- gridDir: EDR gridded data directory
# 	- figsGener: Image generation source code directory
# 	- figsDir: Generated image directory
# 	- IDL: IDL path
# 	- fileExt: File extension
# 	- figsGener_cntrl: The control file to generate images.
#
#
# Outputs:
#      	- None
#
#===============================================================
function figsGen() {
    if [[ $# -ne 15 ]] ; then
        op_msg "Error: function figsGen: check number of arguments: $#"
        exit 1
    fi
    local satId=$1
    local gridfactor=$2
    local gridDir=$3
    local figsGener=$4
    local figsDir=$5
    local IDL=$6
    local fileExt=$7
    local figsGener_cntrl=$8
    local processMode=$9
    local controlDataPath=${10}
    local version=${11}
    local latmin=${12}
    local latmax=${13}
    local lonmin=${14}
    local lonmax=${15}

    echo "=============== RUNNING GRIDDED MAP GENERATION STEP ======================="

    cd ${figsGener}
    
    local iregions=(0 1 2 3 5) 
    local regions=(glb us eu gulf china)
    local nregions=2

    if [[ ${satId} == "mtma" || ${satId} == "mtsa" ]] ; then
	local iregions=( 0 ) 
	local regions=( glb )
	local nregions=1
    fi

    for (( i=0; i<${nregions}; i++ )) ; do
      
      echo ${satId}             >  ${figsGener_cntrl}_${regions[$i]}
      echo ${gridfactor}        >> ${figsGener_cntrl}_${regions[$i]}
      echo ${gridDir}/          >> ${figsGener_cntrl}_${regions[$i]}
      echo ${figsDir}/          >> ${figsGener_cntrl}_${regions[$i]}
      echo ${fileExt}           >> ${figsGener_cntrl}_${regions[$i]}
      echo ${processMode}       >> ${figsGener_cntrl}_${regions[$i]}
      echo ${version}           >> ${figsGener_cntrl}_${regions[$i]}
      echo ${latmin}            >> ${figsGener_cntrl}_${regions[$i]}
      echo ${latmax}            >> ${figsGener_cntrl}_${regions[$i]}
      echo ${lonmin}            >> ${figsGener_cntrl}_${regions[$i]}
      echo ${lonmax}            >> ${figsGener_cntrl}_${regions[$i]}
      echo ${iregions[$i]}      >> ${figsGener_cntrl}_${regions[$i]}
      
      # horizontal plot 
      echo "!QUIET=1"                                               >  ${controlDataPath}/batchGridMirs.pro
      echo ".r gridMirs.pro"                                	    >> ${controlDataPath}/batchGridMirs.pro
      echo "GridMirs, namelist='${figsGener_cntrl}_${regions[$i]}'" >> ${controlDataPath}/batchGridMirs.pro
      echo "exit"                                                   >> ${controlDataPath}/batchGridMirs.pro
      ${IDL} ${controlDataPath}/batchGridMirs.pro
      checkStatus $? "figsGen: gridMirs.pro"    
      rm -f ${controlDataPath}/batchGridMirs.pro
      
      # vertical cross section ( has global/mexico gulf/china east sea in IDL code )
      if [[ $i -eq 0 ]] ; then
	echo "!QUIET=1"					                 >  ${controlDataPath}/batchVertical.pro
	echo ".r vertical.pro"				                 >> ${controlDataPath}/batchVertical.pro
	echo "Vertical, nameList='${figsGener_cntrl}_${regions[$i]}'"    >> ${controlDataPath}/batchVertical.pro
	echo "exit" 					                 >> ${controlDataPath}/batchVertical.pro
	${IDL} ${controlDataPath}/batchVertical.pro
	checkStatus $? "figsGen: vertical.pro"    
	rm -f ${controlDataPath}/batchVertical.pro
      fi
    
    done
    
    echo "End of step figsGen"
}


#===============================================================================
# Name:		    figsGenHr
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Generate images directly from EDR and DEP HR, bypass grid step
#
#
# Input Variables:
# 	- satId: satellite ID(n18,metopA,f16,etc)
#	- gridfactor: grid factor(1,2,4,etc)
# 	- figsGener: Image generation source code directory
# 	- figsDir: Generated image directory
# 	- IDL: IDL path
# 	- fileExt: File extension
# 	- figsGener_cntrl: The control file to generate images.
#
#
# Outputs:
#      	- None
#
# Usg: 
#
#  	figsGenHr ${satId} ${gridFactor} ${gridSrc} ${figsDir} ${IDL} \
#  	${identifier} ${extResol} ${figsGenControlFile} ${processMode} ${controlDataPath} ${version} \
#  	${minLat} ${maxLat} ${minLon} ${maxLon} ${edrDir} ${depDir} ${edrList} ${depList} 
#
#
#===============================================================================
function figsGenHr() {
    if [[ $# -ne 19 ]]
        then
        op_msg "Error: function figsGenHr: check number of arguments: $#"
        exit 1
    fi
    local satId=$1
    local gridFactor=$2
    local figsSrc=$3
    local figsDir=$4
    local IDL=$5
    local fileExt=$6
    local extResol=$7
    local figsGener_cntrl=$8
    local processMode=$9
    local controlDataPath=${10}
    local version=${11}
    local latmin=${12}
    local latmax=${13}
    local lonmin=${14}
    local lonmax=${15}
    local edrDir=${16}
    local depDir=${17}
    local edrList=${18}
    local depList=${19}
    
    local resolution=1
    local region=3
    
    if [[ ${satId} == "n18" ]] ; then
      region=3
    elif [[ ${satId} == "metopA" ]] ; then
      region=4
    elif [[ ${satId} == "f16" || "${satId}" == "f17" || ${satId} == "f18" ]] ; then
      region=5
    elif [[ ${satId} == "aqua" ]] ; then
      region=6
    elif [[ ${satId} == "trmm" ]] ; then
      region=7
    elif [[ ${satId} == "gpm" ]] ; then
      region=7
    fi
    
    echo "=============== EDR & DEP IMAGE GENERATION STEP ======================="
    
    if [[ ${processMode} -eq 0 ]] ; then
      ls -1 ${edrDir}EDR*${fileExt}*${extResol}.*  > ${edrList}
      ls -1 ${depDir}DEP*${fileExt}*${extResol}.*  > ${depList}
    else
      ls -1 ${edrDir}/EDR*${extResol}.* > ${edrList}
      ls -1 ${depDir}/DEP*${extResol}.* > ${depList}
    fi 
    	
    cd ${figsSrc}
    echo ${satId}                                   		>  ${figsGener_cntrl}_edr_hr
    echo ${figsDir}/                                    	>> ${figsGener_cntrl}_edr_hr
    echo ${fileExt}                                     	>> ${figsGener_cntrl}_edr_hr
    echo ${version}                                     	>> ${figsGener_cntrl}_edr_hr
    echo ${latmin}                                     		>> ${figsGener_cntrl}_edr_hr
    echo ${latmax}                                     		>> ${figsGener_cntrl}_edr_hr
    echo ${lonmin}                                     		>> ${figsGener_cntrl}_edr_hr
    echo ${lonmax}                                     		>> ${figsGener_cntrl}_edr_hr
    echo ${edrList}						>> ${figsGener_cntrl}_edr_hr
    echo ${region}						>> ${figsGener_cntrl}_edr_hr
    echo ${resolution}						>> ${figsGener_cntrl}_edr_hr
    
    # EDR plot 
    local lineEdr=`wc -l ${edrList} | awk '{print $1}'`
    if [[ $lineEdr -gt 0 ]] ; then
      echo "!QUIET=1"                                             >  ${controlDataPath}/batchGenEdrImgHr.pro
      echo ".r nongridMirs_edr.pro"                               >> ${controlDataPath}/batchGenEdrImgHr.pro
      echo "NONGRIDMIRS_EDR,namelist='${figsGener_cntrl}_edr_hr'" >> ${controlDataPath}/batchGenEdrImgHr.pro
      echo "exit"                                                 >> ${controlDataPath}/batchGenEdrImgHr.pro
      ${IDL} ${controlDataPath}/batchGenEdrImgHr.pro
      checkStatus $? "figsGenHr: nongridMirs_edr"
      rm -f ${controlDataPath}/batchGenEdrImgHr.pro
    fi
    
    cd ${figsSrc}
    echo ${satId}                                   		>  ${figsGener_cntrl}_dep_hr
    echo ${figsDir}/                                    	>> ${figsGener_cntrl}_dep_hr
    echo ${fileExt}                                     	>> ${figsGener_cntrl}_dep_hr
    echo ${version}                                     	>> ${figsGener_cntrl}_dep_hr
    echo ${latmin}                                     		>> ${figsGener_cntrl}_dep_hr
    echo ${latmax}                                     		>> ${figsGener_cntrl}_dep_hr
    echo ${lonmin}                                     		>> ${figsGener_cntrl}_dep_hr
    echo ${lonmax}                                     		>> ${figsGener_cntrl}_dep_hr
    echo ${depList}						>> ${figsGener_cntrl}_dep_hr
    echo ${region}						>> ${figsGener_cntrl}_dep_hr
    echo ${resolution}						>> ${figsGener_cntrl}_dep_hr
    
    # DEP plot
    local lineDep=`wc -l ${depList} | awk '{print $1}'`
    if [[ $lineDep -gt 0 ]] ; then
      echo "!QUIET=1"                                             >  ${controlDataPath}/batchGenDepImgHr.pro
      echo ".r nongridMirs_dep.pro"                               >> ${controlDataPath}/batchGenDepImgHr.pro
      echo "NONGRIDMIRS_DEP,namelist='${figsGener_cntrl}_dep_hr'" >> ${controlDataPath}/batchGenDepImgHr.pro
      echo "exit"                                                 >> ${controlDataPath}/batchGenDepImgHr.pro
      ${IDL} ${controlDataPath}/batchGenDepImgHr.pro
      checkStatus $? "figsGenHr: "
      rm -f ${controlDataPath}/batchGenDepImgHr.pro
    fi
    echo "End of step figsGenHr"
}


#===============================================================================
# Name:		    imgHr
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Generate high resolution images directly from EDR and DEP HR, bypass grid step
#
#
# Input Variables:
# 	- satId: satellite ID(n18,metopA,f16,etc)
#	- gridfactor: grid factor(1,2,4,etc)
# 	- figsGener: Image generation source code directory
# 	- figsDir: Generated image directory
# 	- IDL: IDL path
# 	- fileExt: File extension
# 	- figsGener_cntrl: The control file to generate images.
#
#
# Outputs:
#      	- None
#
# Usg: 
#
#  	figsGenHr ${satId} ${gridFactor} ${gridSrc} ${figsDir} ${IDL} \
#  	${identifier} ${extResol} ${figsGenControlFile} ${processMode} ${controlDataPath} ${version} \
#  	${minLat} ${maxLat} ${minLon} ${maxLon} ${edrDir} ${depDir} ${edrList} ${depList} 
#
#
#===============================================================================
function imgHr() {
    if [[ $# -ne 19 ]]
        then
        op_msg "Error: function imgHr: check number of arguments: $#"
        exit 1
    fi
    local satId=$1
    local gridFactor=$2
    local figsSrc=$3
    local figsDir=$4
    local IDL=$5
    local fileExt=$6
    local extResol=$7
    local figsGener_cntrl=$8
    local processMode=$9
    local controlDataPath=${10}
    local version=${11}
    local latmin=${12}
    local latmax=${13}
    local lonmin=${14}
    local lonmax=${15}
    local edrDir=${16}
    local depDir=${17}
    local edrList=${18}
    local depList=${19}
    
    local resolution=1
    local region=-1
    
    echo "============= EDR & DEP HIGH RESOLUTION IMAGE GENERATION STEP ================"
    
    if [[ ${processMode} -eq 0 ]] ; then
      ls -1 ${edrDir}EDR*${fileExt}*${extResol}.*  > ${edrList}
      ls -1 ${depDir}DEP*${fileExt}*${extResol}.*  > ${depList}
    else
      ls -1 ${edrDir}/EDR*${extResol}.* > ${edrList}
      ls -1 ${depDir}/DEP*${extResol}.* > ${depList}
    fi 
    	
    cd ${figsSrc}
    echo ${satId}                                   		>  ${figsGener_cntrl}_edr_hr
    echo ${figsDir}/                                    	>> ${figsGener_cntrl}_edr_hr
    echo ${fileExt}                                     	>> ${figsGener_cntrl}_edr_hr
    echo ${version}                                     	>> ${figsGener_cntrl}_edr_hr
    echo ${latmin}                                     		>> ${figsGener_cntrl}_edr_hr
    echo ${latmax}                                     		>> ${figsGener_cntrl}_edr_hr
    echo ${lonmin}                                     		>> ${figsGener_cntrl}_edr_hr
    echo ${lonmax}                                     		>> ${figsGener_cntrl}_edr_hr
    echo ${edrList}						>> ${figsGener_cntrl}_edr_hr
    echo ${region}						>> ${figsGener_cntrl}_edr_hr
    echo ${resolution}						>> ${figsGener_cntrl}_edr_hr
    
    # EDR plot 
    local lineEdr=`wc -l ${edrList} | awk '{print $1}'`
    if [[ $lineEdr -gt 0 ]] ; then
      echo "!QUIET=1"                                             >  ${controlDataPath}/batchGenEdrImgHr.pro
      echo ".r nongridMirs_edr.pro"                               >> ${controlDataPath}/batchGenEdrImgHr.pro
      echo "NONGRIDMIRS_EDR,namelist='${figsGener_cntrl}_edr_hr'" >> ${controlDataPath}/batchGenEdrImgHr.pro
      echo "exit"                                                 >> ${controlDataPath}/batchGenEdrImgHr.pro
      ${IDL} ${controlDataPath}/batchGenEdrImgHr.pro
      checkStatus $? "figsGenHr: nongridMirs_edr"
      rm -f ${controlDataPath}/batchGenEdrImgHr.pro
    fi
    
    cd ${figsSrc}
    echo ${satId}                                   		>  ${figsGener_cntrl}_dep_hr
    echo ${figsDir}/                                    	>> ${figsGener_cntrl}_dep_hr
    echo ${fileExt}                                     	>> ${figsGener_cntrl}_dep_hr
    echo ${version}                                     	>> ${figsGener_cntrl}_dep_hr
    echo ${latmin}                                     		>> ${figsGener_cntrl}_dep_hr
    echo ${latmax}                                     		>> ${figsGener_cntrl}_dep_hr
    echo ${lonmin}                                     		>> ${figsGener_cntrl}_dep_hr
    echo ${lonmax}                                     		>> ${figsGener_cntrl}_dep_hr
    echo ${depList}						>> ${figsGener_cntrl}_dep_hr
    echo ${region}						>> ${figsGener_cntrl}_dep_hr
    echo ${resolution}						>> ${figsGener_cntrl}_dep_hr
    
    # DEP plot
    local lineDep=`wc -l ${depList} | awk '{print $1}'`
    if [[ $lineDep -gt 0 ]] ; then
      echo "!QUIET=1"                                             >  ${controlDataPath}/batchGenDepImgHr.pro
      echo ".r nongridMirs_dep.pro"                               >> ${controlDataPath}/batchGenDepImgHr.pro
      echo "NONGRIDMIRS_DEP,namelist='${figsGener_cntrl}_dep_hr'" >> ${controlDataPath}/batchGenDepImgHr.pro
      echo "exit"                                                 >> ${controlDataPath}/batchGenDepImgHr.pro
      ${IDL} ${controlDataPath}/batchGenDepImgHr.pro
      checkStatus $? "figsGenHr: "
      rm -f ${controlDataPath}/batchGenDepImgHr.pro
    fi
    echo "End of step imgHr"
}


#===============================================================================
# Name:		    figsGen2
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Generate images directly from EDR and DEP HR, bypass grid step
#
#
# Input Variables:
# 	- satId: satellite ID(n18,metopA,f16,etc)
#	- gridfactor: grid factor(1,2,4,etc)
# 	- figsGener: Image generation source code directory
# 	- figsDir: Generated image directory
# 	- IDL: IDL path
# 	- fileExt: File extension
# 	- figsGener_cntrl: The control file to generate images.
#
#
# Outputs:
#      	- None
#
# Usg: 
#
#  	figsGen2 ${satId} ${gridFactor} ${gridSrc} ${figsDir} ${IDL} \
#  	${identifier} ${extResol} ${figsGenControlFile} ${processMode} ${controlDataPath} ${version} \
#  	${minLat} ${maxLat} ${minLon} ${maxLon} ${edrDir} ${depDir} ${edrList} ${depList} 
#
#
#===============================================================================
function figsGen2() {
    if [[ $# -ne 19 ]]
        then
        op_msg "Error: function figsGen2: check number of arguments: $#"
        exit 1
    fi
    local satId=$1
    local gridfactor=$2
    local figsGener=$3
    local figsDir=$4
    local IDL=$5
    local fileExt=$6
    local extResol=$7
    local cntrlFile=$8
    local processMode=$9
    local controlDataPath=${10}
    local version=${11}
    local latmin=${12}
    local latmax=${13}
    local lonmin=${14}
    local lonmax=${15}
    local edrDir=${16}
    local depDir=${17}
    local edrList=${18}
    local depList=${19}

    echo "=============== EDR & DEP IMAGE GENERATION STEP ======================="
    
    if [[ ${processMode} -eq 0 ]] ; then
      ls -1 ${edrDir}EDR*${fileExt}*${extResol}.*  > ${edrList}
      ls -1 ${depDir}DEP*${fileExt}*${extResol}.*  > ${depList}
    else
      ls -1 ${edrDir}/EDR*${extResol}.* > ${edrList}
      ls -1 ${depDir}/DEP*${extResol}.* > ${depList}
    fi 
    
    # EDR plot 
    cd ${figsGener}
    echo ${satId}		>  ${cntrlFile}_edr_${extResol}
    echo ${gridfactor}       	>> ${cntrlFile}_edr_${extResol}
    echo ${figsDir}/         	>> ${cntrlFile}_edr_${extResol}
    echo ${fileExt}          	>> ${cntrlFile}_edr_${extResol}
    echo ${processMode}      	>> ${cntrlFile}_edr_${extResol}
    echo ${version}          	>> ${cntrlFile}_edr_${extResol}
    echo ${latmin}           	>> ${cntrlFile}_edr_${extResol}
    echo ${latmax}           	>> ${cntrlFile}_edr_${extResol}
    echo ${lonmin}           	>> ${cntrlFile}_edr_${extResol}
    echo ${lonmax}           	>> ${cntrlFile}_edr_${extResol}
    echo ${edrList}	     	>> ${cntrlFile}_edr_${extResol}
    echo ${extResol}	     	>> ${cntrlFile}_edr_${extResol}
    echo "!QUIET=1"                               		>  ${controlDataPath}/batchGridEdr.pro
    echo ".r gridEdr.pro"                       		>> ${controlDataPath}/batchGridEdr.pro
    echo "gridEdr, namelist='${cntrlFile}_edr_${extResol}'"	>> ${controlDataPath}/batchGridEdr.pro
    echo "exit"                                      		>> ${controlDataPath}/batchGridEdr.pro
    ${IDL} ${controlDataPath}/batchGridEdr.pro
    checkStatus $? "figsGen2: gridEdr.pro"
    rm -f ${controlDataPath}/batchGridEdr.pro

    # DEP plot
    cd ${figsGener}
    echo ${satId}		>  ${cntrlFile}_dep_${extResol}
    echo ${gridfactor}          >> ${cntrlFile}_dep_${extResol}
    echo ${figsDir}/            >> ${cntrlFile}_dep_${extResol}
    echo ${fileExt}             >> ${cntrlFile}_dep_${extResol}
    echo ${processMode}         >> ${cntrlFile}_dep_${extResol}
    echo ${version}             >> ${cntrlFile}_dep_${extResol}
    echo ${latmin}              >> ${cntrlFile}_dep_${extResol}
    echo ${latmax}              >> ${cntrlFile}_dep_${extResol}
    echo ${lonmin}              >> ${cntrlFile}_dep_${extResol}
    echo ${lonmax}              >> ${cntrlFile}_dep_${extResol}
    echo ${depList}		>> ${cntrlFile}_dep_${extResol}
    echo ${extResol}		>> ${cntrlFile}_dep_${extResol}
    echo "!QUIET=1"                                   		>  ${controlDataPath}/batchGridDep.pro
    echo ".r gridDep.pro"                             		>> ${controlDataPath}/batchGridDep.pro
    echo "gridDep, namelist='${cntrlFile}_dep_${extResol}'"  	>> ${controlDataPath}/batchGridDep.pro
    echo "exit"                                  		>> ${controlDataPath}/batchGridDep.pro
    ${IDL} ${controlDataPath}/batchGridDep.pro
    checkStatus $? "figsGen2: gridDep.pro"
    rm -f ${controlDataPath}/batchGridDep.pro

    echo "End of step figsGen2"
}


#===============================================================
# Name:		    biasFigsGen
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Level III bias data generation
#	First, it will grid NWP and Radiometric
#	Second, it will compute NWP and Radiometric Bias
#	Third, it will compute Geophysical and Radiometric Bias Asymmetry
#
#
# Input Variables:
# 	- edrDir: EDR data directory
# 	- extResol: Extension resolution
# 	- edrList: EDR file list
# 	- gridSrc: Grid generation source code directory
# 	- gridDir: Generated image directory
# 	- satId: satellite ID(n18,metopA,f16,etc)
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- OrbitInfo: Orbit information
# 	- fileExt: File extension ( yyyy-mm-dd, etc)
#	- makeOrNot: on-the-fly make or not if executable not there
#	- binDir: Executable location
# 	- grid_cntrl: The control file to generate gridded data
#
#	- gridfactor: grid factor(1,2,4,etc)
# 	- figsGener: Image generation source code directory
# 	- figsDir: Generated image directory
# 	- IDL: IDL path
# 	- fileExt: File extension
# 	- figsGener_cntrl: The control file to generate images.
# Outputs:
#      	- None
#
#===============================================================
function biasFigsGen() {
    if [[ $# -ne 38 ]]
        then
        op_msg "Error: function biasFigsGen: check number of arguments: $#"
        exit 1
    fi
    local edr4BiasDir=$1
    local nwp4BiasDir=$2
    local fwd4BiasDir=$3
    local edr4BiasList=$4
    
    local nwp4BiasList=$5
    local fwd4BiasList=$6
    local processingMode=${7}
    local OrbitInfo=$8
    
    local extResol=$9
    local satId=${10}
    local fileExt=${11}
    local gridSrc=${12}
    local grid4BiasDir=${13}
    local makeOrNot=${14}
    
    local binDir=${15}
    local gridNwp_cntrl=${16}
    local gridFwd_cntrl=${17}
    local gridBias_cntrl=${18}
    
    local gridfactor=${19}
    local figsGener=${20}
    local figsDir=${21}
    local IDL=${22}
    local fileExt=${23}
    
    local figsGener_cntrl=${24}
    local controlDataPath=${25}
    local dep4BiasDir=${26}
    local dep4BiasList=${27}
    
    local version=${28}
    local biasPath=${29}
    local latmin=${30}
    local latmax=${31}
    local lonmin=${32}
    local lonmax=${33}
    local nwpDataSrc=${34}
    local fmType=${35}
    local nwpGridStr=${36}
    local nwpP2PStr=${37}
    local nedtExt=${38}
    
    local nwpstr='gdas'
    local NWPSTR='GDAS'
    if [[ ${nwpDataSrc} -eq 2 ]] ; then
      nwpstr='ecmw'
      NWPSTR='ECMW'
    fi
    if [[ ${nwpDataSrc} -eq 3 ]] ; then
      nwpstr='gfs'
      NWPSTR='GFSX'
      if [[ ${satId} == 'n18' ||  ${satId} == 'metopA' ||  ${satId} == 'n19' || ${satId} == 'metopB' ]] ; then
        NWPSTR='GFSX'
      elif [[ ${satId} == 'f16' || ${satId} == 'f17' || ${satId} == 'f18' ]] ; then
        NWPSTR='GFSN'
      elif [[ ${satId} == 'trmm' ]] ; then
        NWPSTR='GFS'
      fi
    fi
    local tempNWPList=${nwp4BiasList}_temp
    local tempFWDList=${fwd4BiasList}_temp
    local tempDEPList=${dep4BiasList}_temp

    echo "============ RUNNING BIAS DATA/IMG (${NWPSTR}) GENERATION STEP ==================="
    ConstructList ${processingMode} ${edr4BiasDir} ${OrbitInfo} ${edr4BiasList} "*" "*"${extResol}".*"
    ConstructList ${processingMode} ${dep4BiasDir} ${OrbitInfo} ${dep4BiasList} "*" "*"${extResol}".*"

    local yyyymmdd=`echo $fileExt | cut -c1-4,6-7,9-10`
    
    local nchan=20
    local isatId=1
    
    if [[ ${satId}   == 'n18'    ]] ; then
      isatId=1
      nchan=20
    elif [[ ${satId} == 'metopA' ]] ; then
      isatId=2
      nchan=20
    elif [[ ${satId} == 'metopB' ]] ; then
      isatId=2
      nchan=20
    elif [[ ${satId} == 'f16'    ]] ; then
      isatId=3
      nchan=24
    elif [[ ${satId} == 'n19'    ]] ; then
      isatId=4
      nchan=20
    elif [[ ${satId} == 'f18'    ]] ; then
      isatId=5
      nchan=24
    elif [[ ${satId} == 'npp'    ]] ; then
      isatId=6
      nchan=22
    elif [[ ${satId} == 'aqua'   ]] ; then
      isatId=7
      nchan=12
    elif [[ ${satId} == 'fy3ri'  ]] ; then
      isatId=8
      nchan=10      
    elif [[ ${satId} == 'trmm'   ]] ; then
      isatId=9
      nchan=9      
    elif [[ ${satId} == 'gpm'    ]] ; then
      isatId=10
      nchan=13   
    elif [[ ${satId} == 'f17'    ]] ; then
      isatId=18
      nchan=24 
    elif [[ ${satId} == 'mtma'   ]] ; then
      isatId=12
      nchan=9      
    elif [[ ${satId} == 'mtsa'   ]] ; then
      isatId=13
      nchan=6  
    elif [[ ${satId} == 'gcomw1'   ]] ; then
      isatId=15
      nchan=14     
    fi

    
    if [[ ${isatId} -eq 1 || ${isatId} -eq 2  || ${isatId} -eq 4 ]] ; then

    > ${tempNWPList}
    ls ${edr4BiasDir}/EDR* > ${nwp4BiasList}_${nwpstr}
    sed ' 
s/EDR_SX/'NWP_${NWPSTR}'/g 
s/Outputs/DynamicData/g 
s/edr/nwp_analys/g
s/.ORB//g' ${nwp4BiasList}_${nwpstr} > ${tempNWPList}
    mv ${tempNWPList} ${nwp4BiasList}_${nwpstr}
    
    > ${tempFWDList}
    ls ${edr4BiasDir}/EDR* > ${fwd4BiasList}_${nwpstr}
    sed ' 
s/EDR_SX/'FWD_${NWPSTR}'/g 
s/Outputs/DynamicData/g 
s/edr/fwd_analys/g
s/.ORB//g' ${fwd4BiasList}_${nwpstr} > ${tempFWDList}
    mv ${tempFWDList} ${fwd4BiasList}_${nwpstr}

    > ${tempDEPList}
    ls ${edr4BiasDir}/EDR* > ${dep4BiasList}
    sed ' 
s/EDR/DEP/g 
s/edr/dep/g' ${dep4BiasList} > ${tempDEPList}
mv ${tempDEPList} ${dep4BiasList}

    
    elif [[ ${isatId} -eq 3 || ${isatId} -eq 5 || ${isatId} -eq 11 ]] ; then
	> ${tempNWPList}
	ls ${edr4BiasDir}/EDR* > ${nwp4BiasList}_${nwpstr}
	sed ' 
s/EDR_SN/'NWP_${NWPSTR}'/g 
s/Outputs/DynamicData/g 
s/edr/nwp_analys/g
s/.ORB//g' ${nwp4BiasList}_${nwpstr} > ${tempNWPList}
	mv ${tempNWPList} ${nwp4BiasList}_${nwpstr}
	
	> ${tempFWDList}
	ls ${edr4BiasDir}/EDR* > ${fwd4BiasList}_${nwpstr}
	sed ' 
s/EDR_SN/'FWD_${NWPSTR}'/g 
s/Outputs/DynamicData/g 
s/edr/fwd_analys/g
s/.ORB//g' ${fwd4BiasList}_${nwpstr} > ${tempFWDList}
	mv ${tempFWDList} ${fwd4BiasList}_${nwpstr}

	> ${tempDEPList}
	ls ${edr4BiasDir}/EDR* > ${dep4BiasList}
	sed ' 
s/EDR/DEP/g 
s/edr/dep/g' ${dep4BiasList} > ${tempDEPList}
	mv ${tempDEPList} ${dep4BiasList}
    
    elif [[ ${isatId} -eq 6 ]] ; then
	> ${tempNWPList}
	ls ${edr4BiasDir}/EDR* > ${nwp4BiasList}_${nwpstr}
	sed ' 
s/EDR_TMS/'NWP_${NWPSTR}S'/g 
s/Outputs/DynamicData/g 
s/edr/nwp_analys/g
s/.ORB//g' ${nwp4BiasList}_${nwpstr} > ${tempNWPList}
	mv ${tempNWPList} ${nwp4BiasList}_${nwpstr}
	
	> ${tempFWDList}
	ls ${edr4BiasDir}/EDR* > ${fwd4BiasList}_${nwpstr}
	sed ' 
s/EDR_TMS/'FWD_${NWPSTR}S'/g 
s/Outputs/DynamicData/g 
s/edr/fwd_analys/g
s/.ORB//g' ${fwd4BiasList}_${nwpstr} > ${tempFWDList}
	mv ${tempFWDList} ${fwd4BiasList}_${nwpstr}

	> ${tempDEPList}
	ls ${edr4BiasDir}/EDR* > ${dep4BiasList}
	sed ' 
s/EDR/DEP/g 
s/edr/dep/g' ${dep4BiasList} > ${tempDEPList}
	mv ${tempDEPList} ${dep4BiasList}

    elif [[ ${isatId} -eq 7 || ${isatId} -eq 8 || ${isatId} -eq 9 || ${isatId} -eq 10 || ${isatId} -eq 15 ]] ; then
	ls -1 ${nwp4BiasDir}/NWP_${NWPSTR}* > ${nwp4BiasList}_${nwpstr}
	ls -1 ${fwd4BiasDir}/FWD_${NWPSTR}* > ${fwd4BiasList}_${nwpstr}
	ls -1 ${dep4BiasDir}/DEP* > ${dep4BiasList}
    fi

	
    # NWP grid
    set "filesList='${nwp4BiasList}_${nwpstr}'"	\
        "satId='${satId}'"			\
        "yyyymmdd='${yyyymmdd}'"	     	\
        "gridfactor=${gridfactor}"	      	\
    	"gridPath='${grid4BiasDir}/'"	      	\
        "latmin=${latmin}"  		      	\
        "latmax=${latmax}"			\
        "lonmin=${lonmin}" 		      	\
        "lonmax=${lonmax}"  		      	\
        "isMirs=${nwpDataSrc}"		      	\
        "processMode=${processingMode}"		\
	"fmType=${fmType}"                      \
	"prodStr='${nwpGridStr}'"
    CreatNamList ${gridNwp_cntrl}_${nwpstr}_gridEdr '&gridEdrNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/gridEdr < ${gridNwp_cntrl}_${nwpstr}_gridEdr
    checkStatus $? "biasFigsGen: NWP ${NWPSTR} gridEdr"
    
    # NWP p2p
    set "filesList='${nwp4BiasList}_${nwpstr}'"	\
        "satId='${satId}'"			\
        "yyyymmdd='${yyyymmdd}'"	     	\
    	"p2pPath='${grid4BiasDir}/'"	      	\
        "latmin=${latmin}"  		      	\
        "latmax=${latmax}"			\
        "lonmin=${lonmin}" 		      	\
        "lonmax=${lonmax}"  		      	\
        "isMirs=${nwpDataSrc}"		      	\
        "processMode=${processingMode}"		\
	"fmType=${fmType}"                      \
	"prodStr='${nwpP2PStr}'"
    CreatNamList ${gridNwp_cntrl}_${nwpstr}_p2pEdr '&p2pEdrNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/p2pEdr < ${gridNwp_cntrl}_${nwpstr}_p2pEdr
    checkStatus $? "biasFigsGen: NWP ${NWPSTR} p2pEdr"

    # Rad(FWD) grid
    set "filesList='${fwd4BiasList}_${nwpstr}'"	\
        "satId='${satId}'"		      	\
        "yyyymmdd='${yyyymmdd}'"	      	\
        "gridfactor=${gridfactor}"	      	\
    	"gridPath='${grid4BiasDir}/'"	      	\
        "latmin=${latmin}"  		      	\
        "latmax=${latmax}"			\
        "lonmin=${lonmin}" 		      	\
        "lonmax=${lonmax}"  		      	\
        "isMirs=${nwpDataSrc}"                  \
	"fmType=${fmType}"
    CreatNamList ${gridFwd_cntrl}_${nwpstr}_gridRad '&gridRadNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/gridRad < ${gridFwd_cntrl}_${nwpstr}_gridRad
    checkStatus $? "biasFigsGen: Rad ${NWPSTR} gridRad"

    # Rad(FWD) p2p
    set "filesList='${fwd4BiasList}_${nwpstr}'"	\
        "satId='${satId}'"		      	\
        "yyyymmdd='${yyyymmdd}'"	      	\
    	"p2pPath='${grid4BiasDir}/'"	      	\
        "latmin=${latmin}"  		      	\
        "latmax=${latmax}"			\
        "lonmin=${lonmin}" 		      	\
        "lonmax=${lonmax}"  		      	\
        "isMirs=${nwpDataSrc}"
    CreatNamList ${gridFwd_cntrl}_${nwpstr}_p2pRad '&p2pRadNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/p2pRad < ${gridFwd_cntrl}_${nwpstr}_p2pRad
    checkStatus $? "biasFigsGen: Rad ${NWPSTR} p2pRad"

    # Compute bias and bias asymmetry
    set "satId='${satId}'"		      	\
        "yyyymmdd='${yyyymmdd}'"	      	\
        "gridfactor=${gridfactor}"	      	\
    	"gridPath='${grid4BiasDir}/'"	      	\
        "latmin=${latmin}"  		      	\
        "latmax=${latmax}"			\
        "lonmin=${lonmin}" 		      	\
        "lonmax=${lonmax}"  		      	\
        "NLAY=100"			      	\
        "NCHAN=${nchan}"		      	\
        "nwpData=${nwpDataSrc}"		      	\
	"isatId=${isatId}"
    CreatNamList ${gridBias_cntrl}_${nwpstr} '&GridBiasNameList' "$@"
    DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/gridBias < ${gridBias_cntrl}_${nwpstr}
    checkStatus $? "biasFigsGen: gridBias: ${nwpstr}"

  
    cd ${figsGener}
    echo ${satId}             	>  ${figsGener_cntrl}_${nwpstr}
    echo ${gridfactor}        	>> ${figsGener_cntrl}_${nwpstr}
    echo ${grid4BiasDir}/     	>> ${figsGener_cntrl}_${nwpstr}
    echo ${figsDir}/          	>> ${figsGener_cntrl}_${nwpstr}
    echo ${fileExt}           	>> ${figsGener_cntrl}_${nwpstr}
    echo ${version}           	>> ${figsGener_cntrl}_${nwpstr}
    echo ${edr4BiasList}      	>> ${figsGener_cntrl}_${nwpstr}
    echo ${dep4BiasList}      	>> ${figsGener_cntrl}_${nwpstr}
    echo ${biasPath}          	>> ${figsGener_cntrl}_${nwpstr}
    echo ${latmin}            	>> ${figsGener_cntrl}_${nwpstr}
    echo ${latmax}            	>> ${figsGener_cntrl}_${nwpstr}
    echo ${lonmin}              >> ${figsGener_cntrl}_${nwpstr}
    echo ${lonmax}              >> ${figsGener_cntrl}_${nwpstr}
    echo ${nwpDataSrc}		>> ${figsGener_cntrl}_${nwpstr}
    echo ${nedtExt}		>> ${figsGener_cntrl}_${nwpstr}
    echo ${fmType}		>> ${figsGener_cntrl}_${nwpstr}

    # NWP Gridded Map generation	
    echo "!QUIET=1"                                           	>  ${controlDataPath}/batchGridNwp.pro
    echo ".r gridNwp.pro"                                     	>> ${controlDataPath}/batchGridNwp.pro
    echo "GridNwp, nameList='${figsGener_cntrl}_${nwpstr}'"   	>> ${controlDataPath}/batchGridNwp.pro
    echo "exit"                                               	>> ${controlDataPath}/batchGridNwp.pro
    ${IDL} ${controlDataPath}/batchGridNwp.pro
    checkStatus $? "biasFigsGen(${NWPSTR}): gridNwp.pro"
    rm -f ${controlDataPath}/batchGridNwp.pro

    # NWP Bias Map generation
    echo "!QUIET=1"                                             >  ${controlDataPath}/batchNwpGridBias.pro
    echo ".r gridNwpBias.pro"                                	>> ${controlDataPath}/batchNwpGridBias.pro
    echo "GridNwpBias, nameList='${figsGener_cntrl}_${nwpstr}'"	>> ${controlDataPath}/batchNwpGridBias.pro
    echo "exit"                                                 >> ${controlDataPath}/batchNwpGridBias.pro
    ${IDL} ${controlDataPath}/batchNwpGridBias.pro 
    checkStatus $? "biasFigsGen(${NWPSTR}): gridNwpBias.pro"
    rm -f ${controlDataPath}/batchNwpGridBias.pro

    # NWP Bias Asymmetry Map generation
    echo "!QUIET=1"                                             >  ${controlDataPath}/batchNwpGridAsym.pro
    echo ".r gridNwpAsym.pro"                            	>> ${controlDataPath}/batchNwpGridAsym.pro
    echo "GridNwpAsym, nameList='${figsGener_cntrl}_${nwpstr}'" >> ${controlDataPath}/batchNwpGridAsym.pro
    echo "exit"                                             	>> ${controlDataPath}/batchNwpGridAsym.pro
    ${IDL} ${controlDataPath}/batchNwpGridAsym.pro
    checkStatus $? "biasFigsGen(${NWPSTR}): gridNwpAsym.pro"
    rm -f ${controlDataPath}/batchNwpGridAsym.pro

    # Radiometric Map, Bias/Bias Asymmetry, Residual/Residual Asymmetry generation
    echo "!QUIET=1"                                           	>  ${controlDataPath}/batchGridRad.pro
    echo ".r gridRad.pro"                                	>> ${controlDataPath}/batchGridRad.pro
    echo "GridRad, nameList='${figsGener_cntrl}_${nwpstr}'"	>> ${controlDataPath}/batchGridRad.pro
    echo "exit"                                      		>> ${controlDataPath}/batchGridRad.pro
    ${IDL} ${controlDataPath}/batchGridRad.pro
    checkStatus $? "biasFigsGen(${NWPSTR}): gridRad.pro"
    rm -f ${controlDataPath}/batchGridRad.pro

    # P2P of MIRS vs NWP
    echo "!QUIET=1"                                   		>  ${controlDataPath}/batchMirsNwp.pro
    echo ".r p2p_mirs_nwp.pro"                           	>> ${controlDataPath}/batchMirsNwp.pro
    echo "P2P_MIRS_NWP, nameList='${figsGener_cntrl}_${nwpstr}'">> ${controlDataPath}/batchMirsNwp.pro
    echo "exit"                                               	>> ${controlDataPath}/batchMirsNwp.pro
    ${IDL} ${controlDataPath}/batchMirsNwp.pro
    checkStatus $? "biasFigsGen(${NWPSTR}): p2p_mirs_nwp.pro"    
    rm -f ${controlDataPath}/batchMirsNwp.pro

     # NWP Vertical images -- to call the same IDL code vertical.pro	
    cd ${figsGener}
    echo ${satId}             	>  ${figsGener_cntrl}_${nwpstr}
    echo ${gridfactor}        	>> ${figsGener_cntrl}_${nwpstr}
    echo ${grid4BiasDir}/     	>> ${figsGener_cntrl}_${nwpstr}
    echo ${figsDir}/          	>> ${figsGener_cntrl}_${nwpstr}
    echo ${fileExt}           	>> ${figsGener_cntrl}_${nwpstr}
    echo ${processingMode}      >> ${figsGener_cntrl}_${nwpstr}
    echo ${version}           	>> ${figsGener_cntrl}_${nwpstr}
    echo ${latmin}            	>> ${figsGener_cntrl}_${nwpstr}
    echo ${latmax}            	>> ${figsGener_cntrl}_${nwpstr}
    echo ${lonmin}              >> ${figsGener_cntrl}_${nwpstr}
    echo ${lonmax}              >> ${figsGener_cntrl}_${nwpstr}
    echo ${nwpDataSrc}		>> ${figsGener_cntrl}_${nwpstr}

    echo "!QUIET=1"                                           	>  ${controlDataPath}/batchVertical.pro
    echo ".r vertical.pro"                            	        >> ${controlDataPath}/batchVertical.pro
    echo "Vertical, nameList='${figsGener_cntrl}_${nwpstr}'"    >> ${controlDataPath}/batchVertical.pro
    echo "exit"                                               	>> ${controlDataPath}/batchVertical.pro
    ${IDL} ${controlDataPath}/batchVertical.pro
    checkStatus $? "biasFigsGen(${NWPSTR}): vertical.pro"
    rm -f ${controlDataPath}/batchVertical.pro
    
    #---- mirs vs mspps ( p2p, bias, asymmetry, only for n18/metopA/n19 ), only for gdas
    if [[ ${nwpDataSrc} -eq 1  && ( ${satId} == 'n18' || ${satId} == 'metopA' || ${satId} == 'n19' ) ]] ; then
      cd ${figsGener}
      echo ${satId}				>  ${figsGener_cntrl}_mspps
      echo ${gridfactor}  			>> ${figsGener_cntrl}_mspps
      echo ${grid4BiasDir}/			>> ${figsGener_cntrl}_mspps
      echo ${figsDir}/				>> ${figsGener_cntrl}_mspps
      echo ${fileExt}				>> ${figsGener_cntrl}_mspps
      echo ${version}				>> ${figsGener_cntrl}_mspps
      echo ${edr4BiasList}			>> ${figsGener_cntrl}_mspps
      echo ${dep4BiasList}			>> ${figsGener_cntrl}_mspps
      echo ${nwp4BiasList}_${nwpstr}		>> ${figsGener_cntrl}_mspps
      echo ${biasPath}				>> ${figsGener_cntrl}_mspps
      echo ${latmin}				>> ${figsGener_cntrl}_mspps
      echo ${latmax}				>> ${figsGener_cntrl}_mspps
      echo ${lonmin}				>> ${figsGener_cntrl}_mspps
      echo ${lonmax}				>> ${figsGener_cntrl}_mspps
      echo "!QUIET=1"                                              >  ${controlDataPath}/batchMirsMspps.pro
      echo ".r p2p_mirs_mspps.pro"                                 >> ${controlDataPath}/batchMirsMspps.pro
      echo "P2P_MIRS_MSPPS, nameList='${figsGener_cntrl}_mspps'"   >> ${controlDataPath}/batchMirsMspps.pro
      echo "exit"                                                  >> ${controlDataPath}/batchMirsMspps.pro
      ${IDL} ${controlDataPath}/batchMirsMspps.pro
      checkStatus $? "biasFigsGen: p2p_mirs_mspps.pro"    
      rm -f ${controlDataPath}/batchMirsMspps.pro
    fi
    
    echo "End of step biasFigsGen"
}


#===============================================================
# Name:		    dataQualityMonitor
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Data quality monitoring
#        - nedt monitoring
#        - convergence and qc monitoring
#
#
# Input Variables:
# 	- nedtPath: Nedt path
# 	- nedtList: NeDT file list
# 	- nedtMonitorSrc: nedt src dir
# 	- IDL: IDL path
# 	- gifDensity: Density used when converting PS files into Gif images
# 	- figsDir: Generated image directory
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- fileExt: File extension
# 	- satId: Satellite short name(n18,metopA,n19)
#       - controlDataPath: namelist directory
#       - MonitorPath: directoring for monitoring I/O
#
#
# Outputs:
#      - None
#
#===============================================================
function dataQualityMonitor() {
    if [[ $# -ne 11 ]] ; then
      op_msg "Error: function dataQualityMonitor: check number of arguments: $#"
      exit 1
    fi

    local nedtPath=$1
    local nedtList=$2
    local nedtMonitorSrc=$3
    local IDL=$4
    local orbitMonPath=$5
    local figsDir=$6
    local processingMode=$7
    local fileExt=$8
    local satId=$9
    local controlDataPath=${10}
    local MonitorPath=${11}
    
    echo "=============== RUNNING DATA QUALITY MONITORING STEP ======================="
    find ${nedtPath} -maxdepth 1 -name "*_nedt*befFM.dat" -print | sort > ${nedtList} #create the list of NEDT files
    if [[ ${satId} == "npp" ]] ; then
      find ${nedtPath} -maxdepth 1 -name "npp_atms_nedt_d2*befFM.dat" -print | sort > ${nedtList}
    fi
    if [[ ${satId} == "f16" || ${satId} == "f17" || ${satId} == "f18" ]] ; then
      ls ${nedtPath}/${satId}*nedt_????_??_??_aftFM.dat > ${nedtList} #create the list of NEDT files
    fi
    
    cntrlNEDTMonitor=${controlDataPath}/${satId}_cntrlNEDTMonitor.in
    echo ${satId}	 > ${cntrlNEDTMonitor}
    echo ${nedtList}	>> ${cntrlNEDTMonitor}
    echo ${figsDir}/	>> ${cntrlNEDTMonitor}
    
    cd ${nedtMonitorSrc}
    if [[ ${satId} == "n18" || ${satId} == "n19" || ${satId} == "metopA" || ${satId} == "npp" || ${satId} == "metopB" ]] ; then
      echo "!QUIET=1"					 > ${controlDataPath}/batchMonitoringNEDT.pro
      echo ".r MonitorNEDT.pro"				>> ${controlDataPath}/batchMonitoringNEDT.pro
      echo "MonitorNEDT,namelist='${cntrlNEDTMonitor}'" >> ${controlDataPath}/batchMonitoringNEDT.pro
      echo "exit" 					>> ${controlDataPath}/batchMonitoringNEDT.pro
      ${IDL} ${controlDataPath}/batchMonitoringNEDT.pro
      checkStatus $? "nedtMonitor"
      rm -f ${controlDataPath}/batchMonitoringNEDT.pro
    fi
    echo "MonitorNEDT.pro done"
    
    cntrlQCMonitor=${controlDataPath}/${satId}_cntrlQCMonitor.in
    echo ${satId}          > ${cntrlQCMonitor}
    echo ${MonitorPath}/  >> ${cntrlQCMonitor}
    echo ${orbitMonPath}  >> ${cntrlQCMonitor}

    cd ${nedtMonitorSrc}
    echo "!QUIET=1"                                > ${controlDataPath}/batchQCMonitor.pro
    echo ".r MonitorQC.pro"                       >> ${controlDataPath}/batchQCMonitor.pro
    echo "MonitorQC,namelist='${cntrlQCMonitor}'" >> ${controlDataPath}/batchQCMonitor.pro
    echo "exit"                                   >> ${controlDataPath}/batchQCMonitor.pro

    ${IDL} ${controlDataPath}/batchQCMonitor.pro
    checkStatus $? "QCMonitor"
    rm -f ${controlDataPath}/batchQCMonitor.pro
    echo "MonitorQC.pro done"
    
    # this is for NPP/ATMS daily granule NEDT monitoring
    if [[ ${satId} == "npp" ]] ; then
  
      local yyyy=`echo ${fileExt} | cut -c1-4`
      local mm=`echo ${fileExt} | cut -c6-7`
      local dd=`echo ${fileExt} | cut -c9-10`
      local ymd=${yyyy}-${mm}-${dd}
  
      ls ${nedtPath}/${ymd}/NEDT_*_befFM.dat > ${nedtList}_granule 2> /dev/null
      nfile=`wc -l ${nedtList}_granule | awk '{print $1}'`
      if [[ ${nfile} -ge 2 ]] ; then
    
        contrlNedtGranule=${controlDataPath}/${satId}_cntrlNedtGranule_${ymd}
        echo ${satId}            >  ${contrlNedtGranule}
        echo ${ymd}              >> ${contrlNedtGranule}
        echo ${nedtList}_granule >> ${contrlNedtGranule}
        echo ${figsDir}/         >> ${contrlNedtGranule}
    
        cd ${nedtMonitorSrc}
        echo "!QUIET=1"                                             >  ${controlDataPath}/batch_MonitorNEDT_granule.pro
        echo ".r MonitorNEDT_granule.pro"                           >> ${controlDataPath}/batch_MonitorNEDT_granule.pro
        echo "MonitorNEDT_granule, namelist='${contrlNedtGranule}'" >> ${controlDataPath}/batch_MonitorNEDT_granule.pro
        echo "exit"                                                 >> ${controlDataPath}/batch_MonitorNEDT_granule.pro
    
        ${IDL} ${controlDataPath}/batch_MonitorNEDT_granule.pro
        checkStatus $? "MonitorNEDT_granule.pro"
        rm -f ${controlDataPath}/batch_MonitorNEDT_granule.pro
  
      fi
    fi

    # this is for NPP/ATMS daily granule QC monitoring
    if [[ ${satId} == "npp" ]] ; then
    
      local yyyy=`echo ${fileExt} | cut -c1-4`
      local mm=`echo ${fileExt} | cut -c6-7`
      local dd=`echo ${fileExt} | cut -c9-10`
      local ymd=${yyyy}-${mm}-${dd}
      
      ls -1 ${orbitMonPath}/${ymd}/QC_MONS* > ${controlDataPath}/${satId}_qcList_${ymd} 2> /dev/null
      local nfile=`wc -l ${controlDataPath}/${satId}_qcList_${ymd} | awk '{print $1}'`
      if [[ ${nfile} -ge 2 ]] ; then
        
	contrlQcGranule=${controlDataPath}/${satId}_cntrlQcGranule_${ymd}
	
        echo ${satId}                                  >  ${contrlQcGranule}
        echo ${ymd}                                    >> ${contrlQcGranule}
        echo ${controlDataPath}/${satId}_qcList_${ymd} >> ${contrlQcGranule}
        echo ${MonitorPath}/                           >> ${contrlQcGranule}
    
        cd ${nedtMonitorSrc}
        echo "!QUIET=1"                                         >  ${controlDataPath}/batch_MonitorQC_granule.pro
        echo ".r MonitorQC_granule.pro"                         >> ${controlDataPath}/batch_MonitorQC_granule.pro
        echo "MonitorQC_granule, namelist='${contrlQcGranule}'" >> ${controlDataPath}/batch_MonitorQC_granule.pro
        echo "exit"                                             >> ${controlDataPath}/batch_MonitorQC_granule.pro
    
        ${IDL} -quiet ${controlDataPath}/batch_MonitorQC_granule.pro
        checkStatus $? "MonitorQC_granule.pro"
        rm -f ${controlDataPath}/batch_MonitorQC_granule.pro
	    
      fi
      
    fi

    echo "End of step dataQualityMonitor"
}


#===============================================================
# Name:		    dataQualityMonitorRecord
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Data quality monitoring using dynamically appended record
#       files to store NEDT, convergence and QC data
#        - nedt monitoring
#        - convergence and qc monitoring
#
#
# Input Variables:
# 	- nedtPath: Nedt path
# 	- nedtList: NeDT file list
# 	- nedtMonitorSrc: nedt src dir
# 	- IDL: IDL path
# 	- gifDensity: Density used when converting PS files into Gif images
# 	- figsDir: Generated image directory
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- fileExt: File extension
# 	- satId: Satellite short name(n18,metopA,n19)
#       - controlDataPath: namelist directory
#       - MonitorPath: directoring for monitoring I/O
#
#
# Outputs:
#      - None
#
#===============================================================
function dataQualityMonitorRecord() {
    if [[ $# -ne 9 ]] ; then
      op_msg "Error: function dataQualityMonitorRecord: check number of arguments: $#"
      exit 1
    fi

    local nedtPath=$1
    local nedtFile=$2
    local nedtMonitorSrc=$3
    local IDL=$4
    local orbitMonPath=$5
    local figsDir=$6
    local fileExt=$7
    local satId=$8
    local controlDataPath=${9}
    
    echo "=============== RUNNING DATA QUALITY MONITORING STEP ======================="
   
    #Determine input NEDT record file, if one exists
    local nedtRecordFile=`ls ${nedtPath}/nedtRecord_${satId}*.dat | tail -1`
 
    local time=`date +%Y%m%d%H%S%M`

    cntrlNEDTMonitor=${controlDataPath}/${satId}_cntrlNEDTMonitor.in
    echo ${satId}	         > ${cntrlNEDTMonitor}
    echo ${nedtFile}	        >> ${cntrlNEDTMonitor}
    echo ${nedtRecordFile}	>> ${cntrlNEDTMonitor}
    echo ${nedtPath}            >> ${cntrlNEDTMonitor}
    echo ${time}                >> ${cntrlNEDTMonitor}
    echo ${figsDir}/	        >> ${cntrlNEDTMonitor}
    
    cd ${nedtMonitorSrc}
    if [[ ${satId} == "n18" || ${satId} == "n19" || ${satId} == "metopA" || ${satId} == "npp" || ${satId} == "metopB" ]] ; then
      echo "!QUIET=1"					         > ${controlDataPath}/batchMonitoringNEDT.pro
      echo ".r MonitorNEDT_record.pro"				>> ${controlDataPath}/batchMonitoringNEDT.pro
      echo "MonitorNEDT_record,namelist='${cntrlNEDTMonitor}'"  >> ${controlDataPath}/batchMonitoringNEDT.pro
      echo "exit" 					        >> ${controlDataPath}/batchMonitoringNEDT.pro
      ${IDL} ${controlDataPath}/batchMonitoringNEDT.pro
      checkStatus $? "nedtMonitor_record"
    fi
    echo "MonitorNEDT_record.pro done"

    #Determine input QC record file, if one exists
    local qcRecordFile=`ls ${nedtPath}/qcRecord_${satId}*.dat | tail -1`

    qcFile=`ls ${orbitMonPath}/QC_MON*`

    cntrlQCMonitor=${controlDataPath}/${satId}_cntrlQCMonitor.in
    echo ${satId}          > ${cntrlQCMonitor}
    echo ${qcFile}        >> ${cntrlQCMonitor}
    echo ${qcRecordFile}  >> ${cntrlQCMonitor}
    echo ${orbitMonPath}  >> ${cntrlQCMonitor}
    echo ${time}          >> ${cntrlQCMonitor}
    echo ${figsDir}/      >> ${cntrlQCMonitor}

    cd ${nedtMonitorSrc}
    echo "!QUIET=1"                                       > ${controlDataPath}/batchQCMonitor.pro
    echo ".r MonitorQC_record.pro"                       >> ${controlDataPath}/batchQCMonitor.pro
    echo "MonitorQC_record,namelist='${cntrlQCMonitor}'" >> ${controlDataPath}/batchQCMonitor.pro
    echo "exit"                                          >> ${controlDataPath}/batchQCMonitor.pro

    ${IDL} ${controlDataPath}/batchQCMonitor.pro
    checkStatus $? "MonitorQC_record"
    rm -f ${controlDataPath}/batchQCMonitor.pro
    echo "MonitorQC_record.pro done"   

    echo "End of step dataQualityMonitor"
}



#===============================================================
# Name:		    biasMonitor
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Bias Monitoring
#
#
# Input Variables:
# 	- biasPath: bias file path
# 	- figsBiasDir: generated bias image location
# 	- satId: Satellite short name(n18,metopA,f16,etc)
# 	- biasSrc: IDL src directory
# 	- IDL: IDL path
# 	- biasMonitorCntrl: control file of bias generation
# 	- controlDataPath: control data path
#
#
# Outputs:
#      - None
# 
# Usg:
#	biasMonitor ${biasPath} ${figs4BiasDir} ${satId} ${determineBiasSrc}\
#	 	${IDL} ${biasVerifControlFile} ${controlDataPath} ${ecmwfData} ${version} ${fmType}
#===============================================================
function biasMonitor() {
    if [[ $# -ne 10 ]]
          then	
          echo "Error: function biasMonitor: check number of arguments: $#"
          exit 1
    fi
    
    local biasDir=$1
    local figsBiasDir=$2
    local satId=$3
    local biasSrc=$4
    local IDL=$5
    local biasMonitorCntrl=$6
    local controlDataPath=$7
    local nwpId=$8
    local version=$9
    local fmType=${10}
    
    echo "=============== RUNNING BIAS-MONITOR STEP======================="
    echo $biasDir      > ${biasMonitorCntrl}
    echo $figsBiasDir >> ${biasMonitorCntrl}
    echo $satId       >> ${biasMonitorCntrl}
    echo $nwpId       >> ${biasMonitorCntrl}
    echo $version     >> ${biasMonitorCntrl}
    echo $fmType      >> ${biasMonitorCntrl}

    cd ${biasSrc}
    echo "!QUIET=1"                                     >  ${controlDataPath}/batchBiasMonitor.pro
    echo ".r biasMonitor.pro"                       	>> ${controlDataPath}/batchBiasMonitor.pro
    echo "biasMonitor,nameList='${biasMonitorCntrl}'"  	>> ${controlDataPath}/batchBiasMonitor.pro
    echo "exit"                                         >> ${controlDataPath}/batchBiasMonitor.pro
     ${IDL} ${controlDataPath}/batchBiasMonitor.pro
    rm -f ${controlDataPath}/batchBiasMonitor.pro
    
    # performance time series
    echo "!QUIET=1"                                     >  ${controlDataPath}/batchBiasMeanStdv.pro
    echo ".r biasMeanStdv.pro"                       	>> ${controlDataPath}/batchBiasMeanStdv.pro
    echo "biasMeanStdv,nameList='${biasMonitorCntrl}'"  >> ${controlDataPath}/batchBiasMeanStdv.pro
    echo "exit"                                         >> ${controlDataPath}/batchBiasMeanStdv.pro
    ${IDL} ${controlDataPath}/batchBiasMeanStdv.pro
    rm -f ${controlDataPath}/batchBiasMeanStdv.pro
    
    echo "End of step biasMonitor"
}



#===============================================================
# Name:		    biasMonitorRecord
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Bias Monitoring using a single file containing the master
#       bias record timeseries
#
#
# Input Variables:
# 	- biasPath: bias file path
# 	- figsBiasDir: generated bias image location
# 	- satId: Satellite short name(n18,metopA,f16,etc)
# 	- biasSrc: IDL src directory
# 	- IDL: IDL path
# 	- biasMonitorCntrl: control file of bias generation
# 	- controlDataPath: control data path
#
#
# Outputs:
#      - None
# 
# Usg:
#	biasMonitorRecord ${biasPath} ${figs4BiasDir} ${satId} ${determineBiasSrc}\
#	 	${IDL} ${biasVerifControlFile} ${controlDataPath} ${ecmwfData} \
#               ${version} ${fmType}
#===============================================================
function biasMonitorRecord() {
    if [[ $# -ne 10 ]]
          then	
          echo "Error: function biasMonitorRecord: check number of arguments: $#"
          exit 1
    fi
    
    local biasFile=$1
    local biasDir=$2
    local figsBiasDir=$3
    local satId=$4
    local biasSrc=$5
    local IDL=$6
    local controlDataPath=$7
    local nwpId=$8
    local version=$9
    local fmType=${10}
    
    local nwpStr='gdas'

    if   [[ ${nwpId} -eq 1 ]] ; then
	nwpStr='_gdas'
    elif [[ ${nwpId} -eq 2 ]] ; then
	nwpStr='_ecmw'
    elif [[ ${nwpId} -eq 3 ]] ; then
	nwpStr='_gfs'
    fi

    biasMonitorCntrl=${controlDataPath}/biasMonitorCntrl.in

    local biasRecordFileIn=`ls ${biasDir}/biasRecord_${satId}*${nwpStr} | tail -1`

    local time=`date +%Y%m%d%H%S%M`

    echo "=============== RUNNING BIAS-MONITOR STEP======================="
    echo $biasFile$nwpStr     > ${biasMonitorCntrl}
    echo $biasRecordFileIn   >> ${biasMonitorCntrl}
    echo $biasDir            >> ${biasMonitorCntrl}
    echo $figsBiasDir        >> ${biasMonitorCntrl}
    echo $time               >> ${biasMonitorCntrl}
    echo $satId              >> ${biasMonitorCntrl}
    echo $nwpId              >> ${biasMonitorCntrl}
    echo $version            >> ${biasMonitorCntrl}
    echo $fmType             >> ${biasMonitorCntrl}

    cd ${biasSrc}
    echo "!QUIET=1"                                              > ${controlDataPath}/batchBiasMonitor.pro
    echo ".r biasMonitor_record.pro"                  	        >> ${controlDataPath}/batchBiasMonitor.pro
    echo "biasMonitor_record,nameList='${biasMonitorCntrl}'"  	>> ${controlDataPath}/batchBiasMonitor.pro
    echo "exit"                                                 >> ${controlDataPath}/batchBiasMonitor.pro
     ${IDL} ${controlDataPath}/batchBiasMonitor.pro

    echo "End of step biasMonitor"
}


#===============================================================
# Name:		    checkNEDT
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Check NEDT abnormal
#
#
# Input Variables:
# 	- satId: Satellite short name(n18,metopA,f16,etc)
# 	- nedtPath: Nedt path
# 	- fileExt: File extension
# 	- nedtMonitorSrc: nedt src dir
# 	- IDL: IDL path
# 	- controlDataPath: control data path
#
#
# Outputs:
#      - Email if abnormal happens
# 
# Usg: 	checkNEDT ${satId} ${nedtSensor1Sensor2Path} ${fileExt} \
#	${nedtMonitorSrc} ${IDL} ${controlDataPath} $email
#
#===============================================================
function checkNEDT() {
    if [[ $# -ne 8 ]]
          then	
          echo "Error: function checkNEDT: check number of arguments: $#"
          exit 1
    fi
    
    local satId=$1
    local nedtPath=$2
    local fileExt=$3
    local tunFile=$4
    local nedtMonitorSrc=$5
    local IDL=$6
    local controlDataPath=$7
    local email=$8
    
    local yyyy=`echo $fileExt | cut -c1-4`
    local mm=`echo $fileExt | cut -c6-7`
    local dd=`echo $fileExt | cut -c9-10`
    
    op_msg "=============== RUNNING checkNEDT STEP======================="
    
    nedtFile=${nedtPath}/${satId}_amsua_mhs_nedt_${yyyy}_${mm}_${dd}_befFM.dat
    if [[ ${satId} == "f16" ]] ; then
	nedtFile=${nedtPath}/${satId}_ssmis_nedt_${yyyy}_${mm}_${dd}_aftFM.dat
    fi
#    if [[ ${satId} == "npp" ]] ; then
#	nedtFile=${nedtPath}/${satId}_atms_nedt_${yyyy}_${mm}_${dd}_befFM.dat
#    fi

    checkResult=${controlDataPath}/checkNEDT.txt
    
    if [[ -e ${nedtFile} ]] ; then

	cd ${nedtMonitorSrc}
  	echo "!QUIET=1"                           			    >  ${controlDataPath}/batchCheckNEDT.pro
  	echo ".r ${nedtMonitorSrc}/checkNEDT.pro" 			    >> ${controlDataPath}/batchCheckNEDT.pro 
  	echo "checkNEDT, nedtFile='${nedtFile}', nedtTxt='${checkResult}', tunFile='${tunFile}'"  >> ${controlDataPath}/batchCheckNEDT.pro 
  	echo "exit"                               			    >> ${controlDataPath}/batchCheckNEDT.pro 

  	${IDL} ${controlDataPath}/batchCheckNEDT.pro

  	if [[ -s ${checkResult} ]] ; then
    		echo " " >> ${checkResult}
    		echo "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/nedt.php" >> ${checkResult}
    		if [[ ! -z "${email}" ]] ; then
		  mail -s "${satId} NEDT Alert for ${yyyy}-${mm}-${dd}" ${email} < ${checkResult}
    		fi
		rm -f ${checkResult} ${controlDataPath}/batchCheckNEDT.pro   
    	fi
    fi

    echo "End of step checkNEDT"
}

#===============================================================
# Name:		    checkQC
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Email alert for QC problems
#
#
# Input Variables:
# 	- MonitorPath: Directory containing checkQC file if exists
# 	- satId: Satellite short name(n18,metopA,f16,etc)
#       - email: Addresses to email alert to
#
#
# Outputs:
#      - None
#
#===============================================================
function checkQC() {
    if [[ $# -ne 3 ]]
          then	
          op_msg "Error: function checkQC: check number of arguments: $#"
          exit 1
    fi
 
    local MonitorPath=$1
    local satId=$2
    local email=$3

    checkResult=${MonitorPath}/checkQC.txt

    if [[ -s ${checkResult} ]] ; then
	echo " " >> ${checkResult}
	echo "http://www.star.nesdis.noaa.gov/corp/scsb/mirs/dataquality.php" >> ${checkResult}
	if [[ ! -z "${email}" ]] ; then
	  mail -s "${satId} QC abnormal in past 30 orbits ${yyyy}-${mm}-${dd}" ${email} < ${checkResult}
	fi
	rm -f ${checkResult} 
    fi

}


#===============================================================
# Name:		    checkBias
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Check bias abnormal
#
#
# Input Variables:
# 	- satId: Satellite short name(n18,metopA,f16,etc)
# 	- biasPath: bias path
# 	- rdirAnalysExt: File extension
# 	- determineBiasSrc: bias src dir
# 	- IDL: IDL path
# 	- controlDataPath: control data path
#
#
# Outputs:
#      - Email if abnormal happens
# 
# Usg: 	checkBias ${satId} ${biasPath} ${nedtBefFMFil} ${rdirAnalysExt} \
#	${determineBiasSrc} ${IDL} ${controlDataPath} $email
#
#===============================================================
function checkBias() {
    if [[ $# -ne 10 ]]
          then	
          echo "Error: function checkBias: check number of arguments: $#"
          exit 1
    fi
    
    local satId=$1
    local biasPath=$2
    local nedtFile=$3
    local tunFile=$4
    local rdirAnalysExt=$5
    local determineBiasSrc=$6
    local IDL=$7
    local controlDataPath=$8
    local nwpId=$9
    local email=${10}
    
    local yyyy=`echo $rdirAnalysExt | cut -c1-4`
    local mm=`echo $rdirAnalysExt | cut -c6-7`
    local dd=`echo $rdirAnalysExt | cut -c9-10`
    local nwpStr='gdas'

    echo "=============== RUNNING checkBias STEP ======================="

    if [[ ${nwpId} -eq 2 ]]
	then
	nwpStr='ecmw'
    fi
    if [[ ${nwpId} -eq 3 ]]
	then
	nwpStr='gfs'
    fi

    local biasFile=${biasPath}/biasCorrec_${satId}_${yyyy}_${mm}_${dd}.dat_${nwpStr}
    local modelErrFile=${biasPath}/ModelErrFile_${satId}_${yyyy}_${mm}_${dd}.dat_${nwpStr}
    local checkResult=${controlDataPath}/checkBias.txt
    local biasFileStatic=${biasPath}/biasCorrec_${satId}.dat
    
    if [[ -s ${biasFile} ]] ; then

    	cd ${determineBiasSrc}
  	echo "!QUIET=1"                           			    							   >  ${controlDataPath}/batchCheckBias.pro
  	echo ".r checkBias.pro" 			    									   >> ${controlDataPath}/batchCheckBias.pro 
  	echo "checkBias, satId='${satId}', biasFileDynamic='${biasFile}', biasFileStatic='${biasFileStatic}', modelErrFile='${modelErrFile}', nedtFile='${nedtFile}', tunFile='${tunFile}', biasTxt='${checkResult}'" >> ${controlDataPath}/batchCheckBias.pro 
  	echo "exit"                               			    							   >> ${controlDataPath}/batchCheckBias.pro 

  	${IDL} ${controlDataPath}/batchCheckBias.pro

  	if [[ -s ${checkResult} ]] ; then
    		echo " " >> ${checkResult}
    		echo "http://www.orbit2.nesdis.noaa.gov/corp/scsb/mirs/radiobiasecmwf.php" >> ${checkResult}
    		if [[ ! -z "${email}" ]] ; then
		  mail -s "${satId} Bias abnormal for ${yyyy}-${mm}-${dd}" ${email} < ${checkResult}
    		fi
		rm -f ${checkResult} ${controlDataPath}/batchCheckBias.pro   
    	fi
    fi

    echo "End of step checkBias"
}


#===============================================================
# Name:		    checkGeophysical
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Check geophysical performance monitoring bias abnormal
#
#
# Input Variables:
# 	- satId: Satellite short name(n18,metopA,f16,etc)
# 	- biasPath: bias path
#	- nwpId: gdas(1)/ecmwf(2)/gfs(3)
# 	- rdirAnalysExt: File extension
# 	- determineBiasSrc: bias src dir
# 	- IDL: IDL path
# 	- controlDataPath: control data path
# 	- email: email adresse
#
#
# Outputs:
#      - Email if abnormal happens
# 
# Usg: 
#	checkGeophysical ${satId} ${biasPath} ${gdasData} ${rdirAnalysExt} ${determineBiasSrc} ${IDL} ${controlDataPath} ${email}
#	checkGeophysical ${satId} ${biasPath} ${ecmwfData} ${rdirAnalysExt} ${determineBiasSrc} ${IDL} ${controlDataPath} ${email}
#	checkGeophysical ${satId} ${biasPath} ${gfsData} ${rdirAnalysExt} ${determineBiasSrc} ${IDL} ${controlDataPath} ${email}
#
#	
#===============================================================
function checkGeophysical() {
    if [[ $# -ne 8 ]]
          then	
          echo "Error: function checkGeophysical: check number of arguments: $#"
          exit 1
    fi
    
    local satId=$1
    local biasPath=$2
    local nwpId=$3
    local rdirAnalysExt=$4
    local determineBiasSrc=$5
    local IDL=$6
    local controlDataPath=$7
    local email=$8
    
    local yyyy=`echo $rdirAnalysExt | cut -c1-4`
    local mm=`echo $rdirAnalysExt | cut -c6-7`
    local dd=`echo $rdirAnalysExt | cut -c9-10`
    local yyyymmdd=$yyyy$mm$dd
    
    echo "=============== Running checkGeophysical Step ======================="

    local nwpStr='gdas'
    if   [[ ${nwpId} -eq 1 ]] ; then
	nwpStr='gdas'
    elif   [[ ${nwpId} -eq 2 ]] ; then
	nwpStr='ecmw'
    elif [[ ${nwpId} -eq 3 ]] ; then
	nwpStr='gfs'
    fi
    
    local webId=${nwpStr}
    if [[ ${nwpId} -eq 2 ]] ; then
	webId='ecmwf'
    fi
     
    local checkResult=${controlDataPath}/${satId}_geophysicalTxt_${nwpStr}_${yyyymmdd}
    cd ${determineBiasSrc}
    echo "!QUIET=1"										>  ${controlDataPath}/batchCheckGeophysical.pro
    echo ".r checkGeophysical.pro"								>> ${controlDataPath}/batchCheckGeophysical.pro 
    echo "checkGeophysical,'${satId}','${biasPath}','${nwpStr}','${yyyymmdd}','${checkResult}'" >> ${controlDataPath}/batchCheckGeophysical.pro 
    echo "exit" 										>> ${controlDataPath}/batchCheckGeophysical.pro 
    ${IDL} ${controlDataPath}/batchCheckGeophysical.pro

    if [[ -s ${checkResult} ]] ; then
      echo " " >> ${checkResult}
      echo http://www.orbit2.nesdis.noaa.gov/corp/scsb/mirs/geo${webId}.php >> ${checkResult}
      sed "1i\
      ${satId} ${rdirAnalysExt} ${nwpStr} geophysical monitoring is abnormal:\n\n" ${checkResult} > ${checkResult}_tmp
      mv ${checkResult}_tmp ${checkResult}
      if [[ ! -z "${email}" ]] ; then
        mail -s "${satId} ${webId} geophysical abnormal ${yyyy}-${mm}-${dd}" ${email} < ${checkResult}
      fi
      rm -f ${checkResult} ${controlDataPath}/batchCheckGeophysical.pro  
    fi
    echo "End of step checkGeophysical"
}


#===============================================================
# Name:		    clean
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Clean data
#
#
# Input Variables:
# 	- mxDaysAllowed: Maximum days of data to be keeped.
# 	- tdrPath_amsua: TDR Sensor 1 data path
# 	- tdrPath_mhs: TDR Sensor 2 data path
# 	- sdrPath_amsua: SDR Sensor 1 data path
# 	- sdrPath_mhs: SDR Sensor 2 data path
# 	- fmsdrPath: FMSDR data path
# 	- choppPath: Chopped data path
# 	- edrPath: EDR data path
# 	- depPath: Dep data path
# 	- figsPath: Image path
# 	- nwpPath_analys: NWP analysis data path
# 	- fwdPath_analys: Forward analysis data path
# 	- regrRetrPath: Regression retrieved data path
# 	- PerfsMonitorPath: Performance monitoring path
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- rdr2tdr_amsua: RDR2TDR sensor 1 source code directory
# 	- rdr2tdr_mhs: RDR2TDR sensor 2 source code directory
# 	- mergeNedts: Merge NeDTs source code directory
# 	- tdr2sdr: TDR2SDR source code directory
# 	- fm: Footprint matching source code directory
# 	- nwpGener_analys: NWP analysis data generation souce code directory
# 	- fwd: Forward simulation source code directory
# 	- ApplyregressAlgors: Apply regress algorithm source code directory
# 	- fmsdr2edr: FMSDR2EDR source code directory
# 	- cntrldir: Control data directory
# 	- inputdir: Input data directory
# 	- binDir: Bin directory where all executables reside
# 	- logsdir: Logs directory
# 	- prefixFMaccur: Prefix of file(s) w FM-acuracy metric (only if outFMaccur=1)
# 	- determinBias: Determine bias source code directory
# 	- regressAlgors: Regress algorithm source code directory
# 	- ApplyregressAlgors: Apply regress algorithm source code directory
# 	- rootPath: MIRS root path
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- MakeCleaning: During cleaning step, do we want make-clean as well (=1) or not (=0)
# 	- nedtSensor1Path: NeDT sensor 1 path
# 	- nedtSensor2Path: NeDT sensor 2 path
# 	- nedtSensor1Sensor2Path: NeDT sensor 1 and 2 path
# 	- regressPath: Regression Path
#
#
# Outputs:
#      - None
#
#===============================================================
function clean() {
    local nargs=$#
    local args="$@"
    if [ $# -ne 42 ]
          then
          op_msg "Error: function clean: check number of arguments. Found $#"
          exit 1
    fi
    local satId=$1 			; shift   			
    local mxDaysAllowed=$1   		; shift
    local tdrPath_amsua=$1   		; shift
    local tdrPath_mhs=$1   		; shift
    local sdrPath_amsua=$1   		; shift
    local sdrPath_mhs=$1   		; shift
    local fmsdrPath=$1   		; shift
    local choppPath=$1   		; shift
    local edrPath=$1   			; shift
    local depPath=$1   			; shift
    local figsPath=$1   		; shift
    local nwpPath_analys=$1   		; shift
    local fwdPath_analys=$1   		; shift
    local regrRetrPath=$1   		; shift
    local PerfsMonitorPath=$1   	; shift 
    local makeOrNot=$1   		; shift    
    local rdr2tdr_amsua=$1   		; shift
    local rdr2tdr_mhs=$1   		; shift
    local mergeNedts=$1   		; shift
    local tdr2sdr=$1   			; shift
    local fm=$1   			; shift
    local nwpGener_analys=$1   		; shift
    local fwd=$1   			; shift
    local ApplyregressAlgors=$1   	; shift
    local fmsdr2edr=$1   		; shift
    local cntrldir=$1   		; shift
    local inputdir=$1   		; shift
    local binDir=$1   			; shift
    local logsdir=$1   			; shift    
    local prefixFMaccur=$1   		; shift
    local determinBias=$1   		; shift
    local regressAlgors=$1   		; shift
    local ApplyregressAlgors=$1   	; shift
    local rootPath=$1                   ; shift
    local processingMode=$1		; shift
    local MakeCleaning=$1               ; shift
    local nedtSensor1Path=$1            ; shift
    local nedtSensor2Path=$1            ; shift
    local nedtSensor1Sensor2Path=$1     ; shift
    local regressPath=$1    		; shift
    local biasPath=$1    		; shift
    local gridPath=$1

    echo "=============== RUNNING CLEANING STEP ======================="
    #-----Removals of control files/Namelists, executables, logs and List files
    rm -f ${cntrldir}/*.in   ${cntrldir}/*.in_* ${cntrldir}/*~ 
    rm -f ${inputdir}/*.list* ${inputdir}/*~ ${binDir}/* ${logsdir}/*    
    #----removals of misc items
    rm -f ${fm}/*.ps ${PerfsMonitorPath}/${prefixFMaccur}* ${determinBias}/*.ps
    rm -f ${regressAlgors}/*.ps 
    rm -f ${regressAlgors}/batch*
    rm -f ${rootPath}/scripts/*~ ${rootPath}/setup/*~
    rm -f ${nedtSensor1Path}/*nedt*  ${nedtSensor1Path}/*_wt_*
    
    if [[ ${satId} == 'n18' || ${satId} == 'metopA' || ${satId} == 'n19' ]] ; then
    	rm -f ${nedtSensor2Path}/*nedt*  ${nedtSensor2Path}/*_wt_*
    	rm -f ${nedtSensor1Sensor2Path}/*nedt*
    fi
    rm -f ${biasPath}/biasAfterCorr_${satId}_*  ${biasPath}/ModelErrFile_${satId}_*  ${biasPath}/biasCorrec_${satId}_*
    
    #----Purge directories
    if [[ ${processingMode} -eq 1 ]] #daily processing mode: clean directories
    then
        #-----Remove directories/files after several days (limited space)
	PurgeDir ${tdrPath_amsua}    ${mxDaysAllowed}
	PurgeDir ${sdrPath_amsua}    ${mxDaysAllowed}
	PurgeDir ${fmsdrPath}        ${mxDaysAllowed}
	PurgeDir ${choppPath}        ${mxDaysAllowed}
	PurgeDir ${edrPath}          ${mxDaysAllowed}
	PurgeDir ${depPath}          ${mxDaysAllowed}
	PurgeDir ${gridPath} 	     ${mxDaysAllowed}
	PurgeDir ${figsPath}         ${mxDaysAllowed}
	PurgeDir ${nwpPath_analys}   ${mxDaysAllowed}
	PurgeDir ${fwdPath_analys}   ${mxDaysAllowed}
	PurgeDir ${regrRetrPath}     ${mxDaysAllowed}
	PurgeDir ${PerfsMonitorPath} ${mxDaysAllowed}
	if [[ ${satId} == 'n18' || ${satId} == 'metopA' || ${satId} == 'n19' ]] ; then
		PurgeDir ${tdrPath_mhs}      ${mxDaysAllowed}
		PurgeDir ${sdrPath_mhs}      ${mxDaysAllowed}
	fi
    fi
    
    if [[ ${processingMode} -eq 0 ]] #orbital processing mode: clean files
    then
	rm -f ${tdrPath_amsua}/*
	rm -f ${sdrPath_amsua}/*
	rm -f ${fmsdrPath}/*
	rm -f ${choppPath}/*
	rm -f ${edrPath}/*
	rm -f ${depPath}/*
	rm -f ${gridPath}/*
	rm -f ${figsPath}/*
	rm -f ${nwpPath_analys}/*
	rm -f ${fwdPath_analys}/*
	rm -f ${regrRetrPath}/*
	rm -f ${PerfsMonitorPath}/*
	if [[ ${satId} == 'n18' || ${satId} == 'metopA' || ${satId} == 'n19' ]] ; then
		rm -f ${tdrPath_mhs}/*
		rm -f ${sdrPath_mhs}/*
	fi
    fi
    #-----Clean-up source directory
    if [[ ${MakeCleaning} -eq 1 ]]
    then 
	makeClean ${rdr2tdr_amsua}      
	makeClean ${mergeNedts}         
	makeClean ${tdr2sdr}            
	makeClean ${fm}                 
	makeClean ${nwpGener_analys}    
	makeClean ${fwd}                
	makeClean ${ApplyregressAlgors} 
	makeClean ${fmsdr2edr}        
	if [[ ${satId} == 'n18' || ${satId} == 'metopA' || ${satId} == 'n19' ]] ; then
		makeClean ${rdr2tdr_mhs}        
	fi
    fi
    checkStatus $? "clean"
    
    rm -f ${rootPath}/scripts/${satId}_scs.bash ${rootPath}/setup/${satId}_pcf.bash 
    
    echo "End of step clean"

}


#===============================================================
# Name:		    displayVerif
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Verify input
#
#
# Input Variables:
# 	- script: Script currently running
# 	- Logfile: Log file
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- iSensor_ID: Sensor (1:N18,  2:METOPA)
# 	- outFMaccur: Flag to output of the FM accuracy metric (DeltaTB @89)
# 	- prefixFMaccur: Prefix of file(s) w FM-acuracy metric (only if outFMaccur=1)
# 	- nprofs2retr: Maximum number of profiles to retrieve
# 	- nprofs2fwd: Maximum number of profiles to simulate using the fwd operator (over analyses)
# 	- iAddDeviceNoise: 1-> Flag to add noise to the fwd simulations (over analyses), 0->no Noise added 
# 	- RetrErrCharac: Retrieval error character
# 	- MonitorIterat: =1->Yes, monitor iterative process; 0-> Do not
# 	- nAttempts: Number of retrieval attempts in case of non-convergence
# 	- extBkgAtmUse: 1->Use Spatially/temporally varying background file, 2-> Do not use.
# 	- ExternDataAvailab: 1->External data available, use ExtDataUse; 0 -> No Ext data available,
# 	- PrintMonitor: =1->Yes, on-screen-monitoring of retrieval;  =0-> Do not.
# 	- iPrintMonitor_fwd: 1->Yes, on-screen-monitoring of fwd-simul ,  =0-> Do not
# 	- GeogrLimits: 0-> Process all data, 1-> only in the lat/lon box, 2->Only ocean, 3->Only land
# 	- minLat: Min latitude of data to be processed (only if GeogrLimits=1)
# 	- maxLat: Max latitude of data to be processed (only if GeogrLimits=1)
# 	- minLon: Min longitude of data to be processed (only if GeogrLimits=1)
# 	- maxLon: Max longitude of data to be processed (only if GeogrLimits=1)
# 	- mxDaysAllowed: Maximum days of data to be keeped.
# 	- NdayBack: TB-Bias assessed w. data from NdayBack, due to GDAS delay. NdayBack between 0 & mxDaysAllowed-1
# 	- tdrFormat: Format of TDR (depends on the RDR decoder at hand):0->ascii, 1->binary
# 	- Pass: Orbit(s) 2 process: 0:ascending, 1:descending, 2:both
# 	- DayUsed4Bias: Extension used to determine which bias to use in the ec process.
# 	- DayUsed4Algs: Extension used to determine which algorithms to use in the externDataFromRegress step
# 	- norbits2process: Number of orbits to process
# 	- GifDensity: Density used when converting PS files into Gif images
# 	- iSrcExtData: External data src available or not
# 	- iFMtype: Footprint matching type .0->to lowest resolution (AMSUA), 1->to highest resolution (MHS)
# 	- nScanL_amsua_2skip: umber of AMSUA scan lines to skip upfront (to accomodate geolocation issues)
# 	- nScanL_mhs_2skip: Number of MHS scan lines to skip upfront (to accomodate geolocation issues)
# 	- ScanLindx_mhs_TimeColloc: MHS ScanLine index (1,2 or 3) that corresponds in time to AMSUA
# 	- iBiasComputMethod: Method for computing the bias. 0->Simple bias, 1->Histogram adjustment
# 	- iCorrMethod: Method for correcting TBs.0->Bias removal, 1-> Slope/Intercept removals
# 	- rhMaxAllowed: Maximum relative humidity allowed (check for supersaturation)
# 	- nChoppedFilesPerOrbit: Number of chopped sub-orbits per orbit. If 1 no chopping is done
# 	- retrOnOrbitOrSubOrbit: Switch between performing retr on full-orbits (0) or chopped ones (1)
# 	- retrOnWhichSDR: Switch between retr on empirically-corrected TBs(0), uncorrected(1) or NWP-based simuls(2)
# 	- Fmatrx2Use: Switch fwd model error matrix (0:dynamically generated by compar with simul, 1:Non-error)
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
# 	- CPUuse: 0->Use one CPU at a time, 1->Use all available CPUs at once to speed up
#
#
# Outputs:
#      - None
#
#===============================================================

function displayVerif() {
    local nargs=$#
    local args="$@"
    
    local script=$1 ; shift
    local Logfile=$1 ; shift 
    local processingMode=$1 ; shift 
    local iSensor_ID=$1 ; shift
       
    local outFMaccur=$1 ; shift
    local prefixFMaccur=$1 ; shift 
    local nprofs2retr=$1 ; shift 
    local nprofs2fwd=$1 ; shift
     
    local iAddDeviceNoise=$1 ; shift 
    local MonitorIterat=$1 ; shift 
    local nAttempts=$1 ; shift
    local extBkgAtmUse=$1 ; shift
     
    local ExternDataAvailab=$1 ; shift 
    local PrintMonitor=$1 ; shift 
    local iPrintMonitor_fwd=$1 ; shift 
    local GeogrLimits=$1 ; shift
    
    local minLat=$1 ; shift 
    local maxLat=$1 ; shift 
    local minLon=$1 ; shift 
    local maxLon=$1 ; shift
    local mxDaysAllowed=$1 ; shift 
    local NdayBack=$1 ; shift
     
    local tdrFormat=$1 ; shift 
    local Pass=$1 ; shift 
    local DayUsed4Bias=$1 ; shift 
    local DayUsed4Algs=$1 ; shift 
    local norbits2process=$1 ; shift 

    local GifDensity=$1 ; shift 
    local iSrcExtData=$1 ; shift 
    local iFMtype=$1 ; shift
    local iBiasComputMethod=$1 ; shift
    
    local nChoppedFilesPerOrbit=$1 ; shift
    local retrOnOrbitOrSubOrbit=$1 ; shift
    local retrOnWhichSDR=$1 ; shift
    
    local Fmatrx2Use=$1 ; shift
    local makeOrNot=$1 ; shift
    local CPUuse=$1
    
    if [ $nargs -ne 37 ] 
	 then
	 op_msg "Error in DisplayVerif: check number of arguments. Found: $nargs"
	 exit 1
    fi 

    echo '|----------------------------------------|'
    echo '|                                        |' 
    echo '|      M I R S    T E S T B E D          |'
    echo '|                                        |'
    echo '|----------------------------------------|'
    echo '|      General Information:              |'
    echo '|----------------------------------------|'

    echo "Script currently running: ${script}"
    echo "LogFile		   =${Logfile}"
    echo "processingMode           =${processingMode}"
    echo "iSensor_ID               =${iSensor_ID}" 
                
    echo "outFMaccur               =${outFMaccur}"
    echo "prefixFMaccur            =${prefixFMaccur}"
    echo "nprofs2retr              =${nprofs2retr}"
    echo "nprofs2fwd               =${nprofs2fwd}"
    
    echo "iAddDeviceNoise          =${iAddDeviceNoise}"
    echo "MonitorIterat            =${MonitorIterat}"
    echo "nAttempts                =${nAttempts}"
    echo "extBkgAtmUse             =${extBkgAtmUse}"
    
    echo "ExternDataAvailab        =${ExternDataAvailab}"
    echo "PrintMonitor             =${PrintMonitor}"
    echo "iPrintMonitor_fwd        =${iPrintMonitor_fwd}"
    echo "GeogrLimits              =${GeogrLimits}"
    
    echo "minLat                   =${minLat}"
    echo "maxLat                   =${maxLat}"
    echo "minLon                   =${minLon}"
    echo "maxLon                   =${maxLon}"
    echo "mxDaysAllowed            =${mxDaysAllowed}"
    echo "NdayBack                 =${NdayBack}"
    
    echo "tdrFormat                =${tdrFormat}"
    echo "Pass                     =${Pass}"
    echo "DayUsed4Bias             =${DayUsed4Bias}"
    echo "DayUsed4Algs             =${DayUsed4Algs}"
    echo "norbits2process          =${norbits2process}"
    
    echo "GifDensity               =${GifDensity}"
    echo "iSrcExtData              =${iSrcExtData}"
    echo "iFMtype                  =${iFMtype}"
    echo "iBiasComputMethod        =${iBiasComputMethod}"
    
    echo "nChoppedFilesPerOrbit    =${nChoppedFilesPerOrbit}"
    echo "retrOnOrbitOrSubOrbit    =${retrOnOrbitOrSubOrbit}"
    echo "retrOnWhichSDR           =${retrOnWhichSDR}"
    
    echo "Fmatrx2Use               =${Fmatrx2Use}"
    echo "makeOrNot                =${makeOrNot}"
    echo "CPUuse                   =${CPUuse}"
}



#===============================================================
# Name:		    wtMonitor
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	???
#
#
# Input Variables:
# 	- NeDTPath_amsua: NeDT Sensor 1 path
# 	- nedtList_amsua: Sensor 1 NeDT file list
# 	- NeDTPath_mhs: NeDT Sensor 2 path
# 	- nedtList_mhs: Sensor 2 NeDT file list
# 	- nedtMonitor: NeDT monitoring source code directory
# 	- Sensor: Sensor (1:N18,  2:METOPA)
# 	- IDL: IDL path
# 	- GifDensity: Density used when converting PS files into Gif images
# 	- figsDir: Generated image directory
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- FileExt: File extension
#
#
# Outputs:
#      - None
#
#===============================================================
function wtMonitor() {
    if [[ $# -ne 11 ]]
          then
          op_msg "Error: function wtMonit: check number of arguments"
          exit 1
    fi
    local NeDTPath_amsua=$1
    local nedtList_amsua=$2
    local NeDTPath_mhs=$3
    local nedtList_mhs=$4
    local nedtMonitor=$5
    local Sensor=$6
    local IDL=$7
    local GifDensity=$8
    local figsDir=$9
    local processingMode=${10}
    local FileExt=${11}
    echo "=============== RUNNING WT-MONITOR STEP ======================="
    ls -1 -d ${NeDTPath_amsua}/* > ${nedtList_amsua} #create the list of NEDT directories
    ls -1 -d ${NeDTPath_mhs}/* > ${nedtList_mhs}     #create the list of NEDT directories
    cd ${nedtMonitor}/n18
    echo "!QUIET=1"						> "batchMonitoringWT_n18.pro"
    echo ".r MonitorWT.pro"                                    >> "batchMonitoringWT_n18.pro"
    echo "MonitorWT,wtList='${nedtList_amsua}',ext='AMSUA'"    >> "batchMonitoringWT_n18.pro"
    echo "MonitorWT,wtList='${nedtList_mhs}',ext='MHS'"        >> "batchMonitoringWT_n18.pro"
    echo "exit"                                                >> "batchMonitoringWT_n18.pro"
    ${IDL} batchMonitoringWT_n18.pro
    #----convert WT plots
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan01.ps  ${figsDir}/monitoring_wt_AMSUA_Chan01.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan02.ps  ${figsDir}/monitoring_wt_AMSUA_Chan02.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan03.ps  ${figsDir}/monitoring_wt_AMSUA_Chan03.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan04.ps  ${figsDir}/monitoring_wt_AMSUA_Chan04.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan05.ps  ${figsDir}/monitoring_wt_AMSUA_Chan05.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan06.ps  ${figsDir}/monitoring_wt_AMSUA_Chan06.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan07.ps  ${figsDir}/monitoring_wt_AMSUA_Chan07.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan08.ps  ${figsDir}/monitoring_wt_AMSUA_Chan08.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan09.ps  ${figsDir}/monitoring_wt_AMSUA_Chan09.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan10.ps  ${figsDir}/monitoring_wt_AMSUA_Chan10.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan11.ps  ${figsDir}/monitoring_wt_AMSUA_Chan11.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan12.ps  ${figsDir}/monitoring_wt_AMSUA_Chan12.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan13.ps  ${figsDir}/monitoring_wt_AMSUA_Chan13.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan14.ps  ${figsDir}/monitoring_wt_AMSUA_Chan14.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_AMSUA_Chan15.ps  ${figsDir}/monitoring_wt_AMSUA_Chan15.gif
    convert -density ${GifDensity} -rotate -90 visu_wt_MHS_Chan01.ps  ${figsDir}/monitoring_wt_MHS_Chan01.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_MHS_Chan02.ps  ${figsDir}/monitoring_wt_MHS_Chan02.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_MHS_Chan03.ps  ${figsDir}/monitoring_wt_MHS_Chan03.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_MHS_Chan04.ps  ${figsDir}/monitoring_wt_MHS_Chan04.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_MHS_Chan05.ps  ${figsDir}/monitoring_wt_MHS_Chan05.gif
    #----convert WT stats plots
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan01.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan01.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan02.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan02.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan03.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan03.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan04.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan04.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan05.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan05.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan06.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan06.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan07.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan07.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan08.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan08.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan09.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan09.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan10.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan10.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan11.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan11.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan12.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan12.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan13.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan13.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan14.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan14.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_AMSUA_Chan15.ps  ${figsDir}/monitoring_wt_stats_AMSUA_Chan15.gif
    convert -density ${GifDensity} -rotate -90 visu_wt_stats_MHS_Chan01.ps  ${figsDir}/monitoring_wt_stats_MHS_Chan01.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_MHS_Chan02.ps  ${figsDir}/monitoring_wt_stats_MHS_Chan02.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_MHS_Chan03.ps  ${figsDir}/monitoring_wt_stats_MHS_Chan03.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_MHS_Chan04.ps  ${figsDir}/monitoring_wt_stats_MHS_Chan04.gif
    #convert -density ${GifDensity} -rotate -90 visu_wt_stats_MHS_Chan05.ps  ${figsDir}/monitoring_wt_stats_MHS_Chan05.gif
    checkStatus $? "wtMonitor"
    rm -f  *.ps list
    rm -f batchMonitoringWT_n18.pro
    echo "End of step wtMonitor"
}


#===============================================================
# Name:		    DetermineYesterdExtAndAlanysExt
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Determine Yesterd
#
#
# Input Variables:
# 	- NdayBack: TB-Bias assessed w. data from NdayBack, due to GDAS delay.
#	  NdayBack between 0 & mxDaysAllowed-1
#
#
# Outputs:
#      - None
#
#===============================================================
function DetermineYesterdExtAndAlanysExt() {
    local NdayBack=$1
    local date_y=$(date +%Y)
    local date_j=$(date +%j)
    local jday
    
    #---Translate date_j to integer jday
    (( jday=$(echo ${date_j}|cut -c1-1)*100+$(echo ${date_j}|cut -c2-2)*10+$(echo ${date_j}|cut -c3-3) ))
    #---Find the julian day of one day before, which is target julian day
    (( jday=${jday}-1 ))
    #---Get the regular date of one day before, which is target date

    local rdirExt=`yyyyjjj2yyyymmdd ${date_y} ${jday}`

    #---Construct NWP_GridAnalysis data extension based on the NdayBack if no date is provided
    #---Find the year and julian day of ${NdayBack} days before processed data
    date_y=$(echo ${rdirExt}|cut -c1-4)
    (( jday=${jday}-${NdayBack} ))
    #----Get the GDAS regular date of $NdayBack days before processed data

    local rdirAnalysExt=`yyyyjjj2yyyymmdd ${date_y} ${jday}`

    #---Find the year and julian day of ${NdayBack}+1 days before processed data
    date_y=$(echo ${rdirExt}|cut -c1-4)
    (( jday=${jday}+1 ))
    #----make sure that when we add 1 to Julian day, to take care of changing year
    local leap=`leapYearEst ${date_y}`
    if (( ${leap} == 28 ))
        then
	if (( ${jday} == 366 ))
	    then
	    (( jday=1 ))
	    (( date_y=${date_y}+1 ))
	fi
    fi
    if (( ${leap} == 29 ))
        then
	if (( ${jday} == 367 ))
	    then
	    (( jday=1 ))
	    (( date_y=${date_y}+1 ))
	fi
    fi

    #----Get the GDAS regular date of $NdayBack days before processed data
    
    local rdirNextAnalysExt=`yyyyjjj2yyyymmdd ${date_y} ${jday}`

    echo "${rdirExt}#${rdirAnalysExt}#${rdirNextAnalysExt}"
    
}

#===============================================================
# Name:		    determineYesterdExtAndAlanysExt
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Will return a string like this: 2007-04-19#2007-04-18#2007-04-19
#
#
# Input Variables:
# 	- nDayBack: TB-Bias assessed w. data from NdayBack, 
#	 due to GDAS delay. NdayBack between 0 & mxDaysAllowed-1
#
#
# Outputs:
#      - 2007-04-19#2007-04-18#2007-04-19(example)
#
#===============================================================
function determineYesterdExtAndAlanysExt() {
    local nDayBack=$1  # relative to yesterday
    
    local rdirExt 
    local rdirAnalysExt
    local rdirNextAnalysExt
    
    local nDayBackPlus
    declare -i nDay
    
    let nDayBackPlus=nDayBack+1
    
    rdirExt=`date -d "1 day ago" +%Y-%m-%d`
    rdirAnalysExt=`date -d "${nDayBackPlus} days ago" +%Y-%m-%d`
    rdirNextAnalysExt=`date -d "${nDayBack} days ago" +%Y-%m-%d`
    
    echo "${rdirExt}#${rdirAnalysExt}#${rdirNextAnalysExt}"
}


#===============================================================
# Name:		    DetermineExtAndAlanysExtFromArgument
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Determine extension
#
#
# Input Variables:
# 	- Arg: Argument
#
#
# Outputs:
#      -  2007-04-19#2007-04-18#2007-04-19(example)
#
#===============================================================
function DetermineExtAndAlanysExtFromArgument() {
    local Arg=$1
    local jday

    local date_j=`yyyymmdd2jjj ${Arg}`
    (( jday=$(echo ${date_j}|cut -c1-1)*100+$(echo ${date_j}|cut -c2-2)*10+$(echo ${date_j}|cut -c3-3) ))
    
    local date_y=$(echo ${Arg}|cut -c1-4)
    (( jday=${jday}+1 ))

    #----make sure that when we add 1 to Julian day, to take care of changing year
    local leap=`leapYearEst ${date_y}`
    if (( ${leap} == 28 ))
        then
	if (( ${jday} == 366 ))
	    then
	    (( jday=1 ))
	    (( date_y=${date_y}+1 ))
	fi
    fi
    if (( ${leap} == 29 ))
        then
	if (( ${jday} == 367 ))
	    then
	    (( jday=1 ))
	    (( date_y=${date_y}+1 ))
	fi
    fi
    local rdirExt=${Arg}
    local rdirAnalysExt=${Arg}
    local rdirNextAnalysExt=`yyyyjjj2yyyymmdd ${date_y} ${jday}`
    
    echo "${rdirExt}#${rdirAnalysExt}#${rdirNextAnalysExt}"
}


#===============================================================
# Name:		    determineExtAndAlanysExtFromArgument
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	
#	determine extension based on an input argument
#
# Input Variables:
# 	- Arg: Argument
#
#
# Outputs:
#      - 2007-04-19#2007-04-18#2007-04-19(example)
#
#===============================================================
function determineExtAndAlanysExtFromArgument() {
    local Arg=$1

    local rdirExt 
    local rdirAnalysExt
    local rdirNextAnalysExt
    
    rdirExt=${Arg}
    rdirAnalysExt=${Arg}
    rdirNextAnalysExt=`date -d "${Arg} 1 day" +%Y-%m-%d`
    
    echo "${rdirExt}#${rdirAnalysExt}#${rdirNextAnalysExt}"
}




#===============================================================
# Name:		    QualityCheckOfArgument
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Quality check of argument
#
#
# Input Variables:
# 	- Arg: Argument
#
#
# Outputs:
#      - None
#
#===============================================================

function QualityCheckOfArgument() {
    local Arg=$1
    local date_y=$(echo ${Arg}|cut -c1-2)
    (( yr=$(echo ${date_y})*1 ))
    if [ ${yr} -ne 20 ] 
	then
	op_msg "Error: Argument must be a date in this case. Example: mirs_xxx 2006-02-01"
	exit 1
    fi
}


#===============================================================
# Name:		    ExtractRdrFileNamesfromOrbit1
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Extract Orbit information from a single file input
#
#
# Input Variables:
# 	- rdrFile: RDR file (NSS.AMAX.NN.D06032.S0122.E0317.B0361819.WI)
# 	- rdrPath: RDR path 
#
#
# Outputs:
#      - Orbit information ( NN.D06032.S0122.E0317.B0361819.WI )
#
#===============================================================
function ExtractRdrFileNamesfromOrbit1() {
    local rdrFile=$1
    local rdrPath=$2
    local orbitInfo=`echo ${rdrFile} | awk -F. '{print$3"."$4"."$5"."$6"."$7"."$8}'`
    
    #local file=${rdrPath}/${rdrFile}
    #if [[ ! -f ${file} ]] ; then
    #	echo "${file} does NOT exist!"
    #	exit 99
    #fi
    echo ${orbitInfo}
}


#===============================================================
# Name:		    ExtractRdrFileNamesfromOrbitNpp
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Extract Orbit information from a single file input of NPP ATMS. The
#   create time is not included as it may differ from TDR/SDR and GATMO files.
#
#
# Input Variables:
# 	- rdrFile: RDR file (TATMS_npp_d20030125_t084657_e084833_b00015_c20071214193913_den_OPS_SEG.h5)
# 	- rdrPath: RDR path 
#
#
# Outputs:
#      - Orbit information ( NN.D06032.S0122.E0317.B0361819.WI )
#
#===============================================================
function ExtractRdrFileNamesfromOrbitNpp() {
    local rdrFile=$1
    local rdrPath=$2
    local orbitInfo=`echo ${rdrFile} | awk -F_ '{print$3"_"$4"_"$5"_"$6}'`

    echo ${orbitInfo}
}


#===============================================================
# Name:		    ExtractRdrFileNamesfromOrbitTrmm
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Extract Orbit information from a single file input of TRMM TMI
#
#
# Input Variables:
# 	- rdrFile: RDR file (1B11.110118.75049.6.HDF)
# 	- rdrPath: RDR path 
#
#
# Outputs:
#      - Orbit information (1B11.110118.75049.6)
#
#===============================================================
function ExtractRdrFileNamesfromOrbitTrmm() {
    local rdrFile=$1
    local rdrPath=$2
    local orbitInfo=`echo ${rdrFile} | awk -F. '{print$1"."$2"."$3"."$4}'`
    echo ${orbitInfo}
}


#===============================================================
# Name:		    ExtractRdrFileNamesfromOrbitMT
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Extract Orbit information from a single file input of MT
#
#
# Input Variables:
# 	- rdrFile: RDR file (MT1MADSL1A__1.00_9_06_I_2011_12_09_00821_00822_00.h5)
# 	- rdrPath: RDR path 
#
#
# Outputs:
#      - Orbit information (2011_12_09_00821_00822_00)
#
#===============================================================
function ExtractRdrFileNamesfromOrbitMT() {
    local rdrFile=$1
    local orbitInfo=`echo ${rdrFile} | cut -c 25-49`
    echo ${orbitInfo}
}

#===============================================================
# Input Variables:
# 	- rdrFile: GW1AM2_201211010056_190B_L1SNBTBR_0000000.h5
#
# Outputs:
#      - Orbit: GW1AM2_201211010056_190B_L1SNBTBR_0000000
#
#===============================================================
function ExtractRdrFileNamesfromOrbitGcomw1() {
    local rdrFile=$1
    local orbitInfo=`echo ${rdrFile} | cut -c 1-41`
    echo ${orbitInfo}
}

#===============================================================
# Name:		    ExtractRdrFileNamesfromOrbit2
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Extract Orbit information from sat with 2 sensors
#
#
# Input Variables:
# 	- rdrfile: RDR file
# 	- rdrPath_amsua: RDR sensor 1 path
# 	- rdrPath_mhs: RDR sensor 2 path
#
#
# Outputs:
#      -  Orbit information ( NN.D06032.S0122.E0317.B0361819.WI )
#
#===============================================================
function ExtractRdrFileNamesfromOrbit2() {
    local rdrfile=$1
    local rdrPath_amsua=$2
    local rdrPath_mhs=$3
    local Ins_ID=`echo $rdrfile |awk -F. '{print$2}'`
    local Sat_ID=`echo $rdrfile |awk -F. '{print$3}'|tr 'a-z' 'A-Z'`
    local orbitInfo=`echo $rdrfile |awk -F. '{print$3"."$4"."$5"."$6"."$7"."$8}'`
    local OrbDateTime=`echo $rdrfile |awk -F. '{print$4"."$5"."$6}'`
    local rdrfile_amsua=${rdrPath_amsua}/NSS.AMAX.${orbitInfo}
    local rdrfile_mhs=${rdrPath_mhs}/NSS.MHSX.${orbitInfo}
    
    #----Sanity checks of the inputs in the orbital processing
    #SanityChecksOrbitProcess ${Sat_ID} ${Ins_ID} ${rdrfile_amsua} ${rdrfile_mhs}
    echo ${orbitInfo}
}


#===============================================================
# Name:		    ErrMessDue2UsageInOrbitalMode
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	???
#
#
# Input Variables:
#
#
# Outputs:
#      - None
#
#===============================================================
function ErrMessDue2UsageInOrbitalMode() {
	op_msg "Usage: $1 <Sensor Data filename>."
	op_msg $2
	exit 99
}


#===============================================================
# Name:		    ErrMessDue2UsageInDailyMode
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	???
#
#
# Input Variables:
#
#
# Outputs:
#      - None
#
#===============================================================
function ErrMessDue2UsageInDailyMode() {
	op_msg "Error:No more than 1 argument allowed in the script daily processing."         
	op_msg "The argument shoud refer to the regular date (ex 2006-02-24) that the testbed"  
	op_msg "should process. By default, it will process the most recent day."               
	exit 99
}


#===============================================================
# Name:		    ConstructList
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Construct file list
#
#
# Input Variables:
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
# 	- Dir: Directroy to be generate if not exist
# 	- Orbit: Orbit
# 	- List: List
# 	- ext1: Extension 1
# 	- ext2: Extension 2
#
#
# Outputs:
#      - None
#
#===============================================================
function ConstructList() {
   local nargs=$#
   if [ $nargs -ne 6 ] 
 	then
   	op_msg "Error in ConstructList: check number of arguments. Found:$nargs"
  	exit 1
   fi	
   local processingMode=$1
   local Dir=$2
   local Orbit=$3
   local List=$4
   local ext1=$5
   local ext2=$6
   if [ ${processingMode} -eq 1 ] 
   then
       ls -1 ${Dir}/${ext1}${ext2}>${List}                  #create the list from directory
   elif [ ${processingMode} -eq 0 ] 
   then
       ls -1 ${Dir}/${ext1}${Orbit}${ext2} > ${List}
   fi
}


#===============================================================
# Name:		    ConstructListFM
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Construct file list, only pick out matched ones and skipped
#	non-matched files.
#
# Input Variables:
# 	- processingMode: Processing mode (0:Orbit mode,  1:Daily mode)
#	-
#	-
#	-
#	-
#	-
#	-
#	-
#	-
#	-
#	-
#
#
# Outputs:
#      - None
#
#===============================================================
function ConstructListFM() {
   if [ $# -ne 10 ] 
 	then
   	op_msg "Error: Check number of argument in function constructListFM, found: $#"
  	exit 1
   fi	
    
    local processingMode=$1
    local orbitInfo=$2
    local dir1=$3
    local dir2=$4
    local list1=$5
    local list2=$6
    local leftExt1=$7
    local leftExt2=$8
    local rightExt1=$9
    local rightExt2=${10}
    
    if   [[ ${processingMode} -eq 0 ]] 
    then
	ls ${dir1}/${leftExt1}${orbitInfo}${rightExt1} > ${list1}
	ls ${dir2}/${leftExt2}${orbitInfo}${rightExt2} > ${list2}
    elif [[ ${processingMode} -eq 1 ]]
    then
    	
	tmpList1=(`ls $dir1`)
	local num1=${#tmpList1[*]}
	#echo $num1
	
	rm -f ${list1} ${list2}
	touch  ${list1} ${list2}
	
	local i=0
	while [[ $i -lt $num1 ]]; do

	  local file1=${tmpList1[$i]}
	  local orbInfo=`echo $file1 |awk -F. '{print $2"."$3"."$4"."$5"."$6"."$7}'`
	  #local file2=${leftExt2}${orbInfo}${rightExt2}
	  local file2=${leftExt2}${orbInfo}
	  
	  #echo "file1=$file1"
	  #echo "file2=$file2"
	  
	  if [[ -f ${dir2}/${file2} ]]
	  then
	    ls ${dir1}/${file1} >> ${list1}
	    ls ${dir2}/${file2} >> ${list2}
	  fi
	  
	  (( i += 1 ))

	done

    fi
}


#===============================================================
# Name:		    determineFileExt
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Determinde file extension(replace - with _ )
#
#
# Input Variables:
# 	- dirExt: Directroy extension ( yyyy-mm-dd )
#
#
# Outputs:
#      - yyyy_mm_dd
#
#===============================================================
function determineFileExt() {
   local dirExt=$1
   local yyyy=$(echo ${dirExt}|cut -c1-4)
   local   mm=$(echo ${dirExt}|cut -c6-7)
   local   dd=$(echo ${dirExt}|cut -c9-10)
   local fileExt=${yyyy}_${mm}_${dd}
   echo $fileExt
}


#===============================================================
# Name:		    SanityChecksOrbitProcess
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Sanity check file existence
#
#
# Input Variables:
# 	- Sat_ID: Satellite ID
# 	- Ins_ID: Instrument ID
# 	- rdrfile_amsua: RDR sensor 1 file
# 	- rdrfile_mhs: RDR sensor 2 file
#
#
# Outputs:
#      - None
#
#===============================================================
function SanityChecksOrbitProcess() {
   local Sat_ID=$1
   local Ins_ID=$2
   local rdrfile_amsua=$3
   local rdrfile_mhs=$4

   echo '${Sat_ID}'
   if [[ "${Sat_ID}" != "NN" && "${Sat_ID}" != "M2" && "${Sat_ID}" != "NP" && "${Sat_ID}" != "M1" ]]
   then
       op_msg "Error: RDR data must be for NN/NP/M1/M2, EXIT."
       exit 99
   fi
   if [[ "${Ins_ID}" != "AMAX" && "${Ins_ID}" != "MHSX" ]]
   then
       op_msg "Error: Input File must be AMSUA or MHS, EXIT."
       exit 99
   fi
   if [[ ! -f ${rdrfile_amsua} || ! -f ${rdrfile_mhs} ]]
   then
       op_msg "Error: Both AMSU-A and MHS files need to be available. Missing File(s): ${rdrfile_amsua} ${rdrfile_mhs}. EXIT."
       exit 99
   fi
}


#===============================================================
# Name:		    SanityChecksOrbitProcess1
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Sanity check single sensor satellite condition
#
#
# Input Variables:
# 	- rdrFile: RDR file
# 	- rdrPath: RDR path 
#
#
# Outputs:
#      - None
#
#===============================================================
function SanityChecksOrbitProcess1() {
    local rdrFile=$1
    local rdrPath=$2

    local fullPathFile=${rdrPath}/${rdrFile}
    
    if [[ ! -f ${fullPathFile} ]]
    then
	op_msg "Error: Missing File ${fullPathFile}"
       	exit 99
    fi
    
}


#===============================================================
# Name:		    SanityChecksOrbitProcess2
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Sanity check double sensor satellite condition
#
#
# Input Variables:
# 	- rdrFile:   RDR file
# 	- pathAmsua: RDR sensor 1 path
# 	- pathMhs:   RDR sensor 2 path
#
#
# Outputs:
#      - None
#
#===============================================================
function SanityChecksOrbitProcess2() {
    local rdrFile=$1
    local pathAmsua=$2
    local pathMhs=$3
    
    local Ins_ID=`echo $rdrFile |awk -F. '{print $2}'`
    local Sat_ID=`echo $rdrFile |awk -F. '{print $3}'|tr 'a-z' 'A-Z'`
    local orbitInfo=`echo $rdrFile |awk -F. '{print $3"."$4"."$5"."$6"."$7"."$8}'`

    local rdrfile_amsua=${pathAmsua}/NSS.AMAX.${orbitInfo}
    local rdrfile_mhs=${pathMhs}/NSS.MHSX.${orbitInfo}

    if [[ "${Sat_ID}" != "NN" && "${Sat_ID}" != "M2" && "${Sat_ID}" != "NP" && "${Sat_ID}" != "M1" ]]
    then
       	op_msg "Error: RDR data must be for N18/MetOp-A/N19"
       	exit 99
    fi
    if [[ "${Ins_ID}" != "AMAX" && "${Ins_ID}" != "MHSX" ]]
    then
       	op_msg "Error: Input File must be AMSUA or MHS"
       	exit 99
    fi
    
    if [[ ! -f ${rdrfile_amsua} ]]
    then
	op_msg "Error: Missing File ${rdrfile_amsua}"
       	exit 99
    fi
    
    if [[ ! -f ${rdrfile_mhs} ]]
    then
	op_msg "Error: Missing File ${rdrfile_mhs}"
       	exit 99
    fi
    
}


#===============================================================
# Name:		    determineSpatialResol
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	determine spatial resolution used in FM
#
#
# Input Variables:
# 	- iFMtype: Footprint matching type .0->to lowest resolution (AMSUA), 
#		1->to highest resolution (MHS)
#
#
# Outputs:
#      - Spatial resolution used
#
#===============================================================
function determineSpatialResol() {
    local satId=$1
    local iFMtype=$2
    local extResol
    
    if [[ ${satId} == "n18" || ${satId} == "metopA" || ${satId} == "n19" || ${satId} == "npp" || ${satId} == "metopB" ]] ; then
    	if   [[ $iFMtype -eq 0 ]] ; then 
       	    extResol='LR'
        elif [[ $iFMtype -eq 1 ]] ; then 
	    extResol='HR'
	fi
    elif [[ ${satId} == "f16" || ${satId} == "f17" || ${satId} == "f18" ]] ; then
    	if   [[ $iFMtype -eq 0 ]] ; then 
       	    extResol='UAS'
        elif [[ $iFMtype -eq 1 ]] ; then 
	    extResol='LAS'
	elif [[ $iFMtype -eq 2 ]] ; then
    	    extResol='ENV'
    	elif [[ $iFMtype -eq 3 ]] ; then
    	    extResol='IMG'
	fi
    elif [[ ${satId} == "aqua" || ${satId} == "fy3ri" ]] ; then
        extResol=''
    elif [[ ${satId} == "trmm" ||  ${satId} == "gpm" ]] ; then
    	if   [[ $iFMtype -eq -1 ]] ; then 
       	    extResol='CR'
        elif [[ $iFMtype -eq 0 ]] ; then 
	    extResol='LR'
        elif [[ $iFMtype -eq 1 ]] ; then 
	    extResol='HR'
	fi
    elif [[ ${satId} == "mtma" ]] ; then
    	if   [[ $iFMtype -eq -1 ]] ; then 
       	    extResol='CR'
        elif [[ $iFMtype -eq 0 ]] ; then 
	    extResol='LR'
        elif [[ $iFMtype -eq 1 ]] ; then 
	    extResol='HR'
	fi
    elif [[ ${satId} == "mtsa" ]] ; then
    	if   [[ $iFMtype -eq -1 ]] ; then 
       	    extResol='CR'
        elif [[ $iFMtype -eq 0 ]] ; then 
	    extResol='LR'
        elif [[ $iFMtype -eq 1 ]] ; then 
	    extResol='HR'
	fi
    elif [[ ${satId} == "gcomw1" ]] ; then
    	if   [[ $iFMtype -eq -1 ]] ; then 
       	    extResol='CR'
        elif [[ $iFMtype -eq 0 ]] ; then 
	    extResol='LR'
        elif [[ $iFMtype -eq 1 ]] ; then 
	    extResol='HR'
	fi
    fi

    echo ${extResol} 
}


#===============================================================
# Name:		    op_msg
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	direct message to a logfile
#
#
# Input Variables:
# 	- logFile: Log file
#
#
# Outputs:
#      - logfile output
#
#===============================================================
function op_msg() {
    #echo "$1" >> ${Logfile}
    echo "$1" 
}


#===============================================================
# Name:		    DirGen
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Generate directory
#
#
# Input Variables:
#
#
# Outputs:
#      - Dir get generated if not exist
#
#===============================================================
function DirGen() {
    iDirSelect=-1
    if (( $# != 3 & $# != 2))
	then 
	op_msg "Maximum of 3 arguments needed!"
	exit 1
    fi
    if [ -d $1 ] 
	then 
	echo "$2  Directory: $1"
	iDirSelect=0
    else
	mkdir -p $1
	if [ -d $1 ] 
	    then 
	    echo "Creating directory: $1"
	    iDirSelect=1
	else
	    echo "Error in creating directory: $1"
	    echo "Using alternative directory: $3"
	    mkdir -p $3
	    iDirSelect=2
	fi
    fi
}


#===============================================================
# Name:		    CreatNamList
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	To generate a name list
#
#
# Input Variables:
# 	- fileout:  file output
# 	- namListLabel: name list lable
#
#
# Outputs:
#      - Name list get generated
#
#===============================================================
function CreatNamList() {
    local fileout=$1
    local namListLabel=$2
    local nargs=$#
    let nargs=$nargs-2
    local args="$@"
    echo $namListLabel > $fileout
    ivar=1
    for var in $args
      do
      if [ $ivar -gt 2 ] 
	  then
	  echo "    "$var >> $fileout
      fi	
      let ivar=ivar+1
    done
    echo "/" >> $fileout
}
 

#===============================================================
# Name:		    makeClean
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Make clean
#
#
# Input Variables:
# 	- direct: Directory to which action to take(clean,make,etc)
#
#
# Outputs:
#      - None
#
#===============================================================
function makeClean() {
    local direct=$1
    cd ${direct}
    make clean
    rm -f *~
}


#===============================================================
# Name:		    DoMake
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Make executables
#
#
# Input Variables:
# 	- direct: Directory to which action to take(clean,make,etc)
# 	- makeOrNot: Switches between making (1) the executables on the fly or not (0)
#
#
# Outputs:
#      - None
#
#===============================================================
function DoMake() {
    local direct=$1
    local makeOrNot=$2
    
    if [[ ${makeOrNot} -eq 1 ]] ; then 
	cd ${direct}
	make 
    fi
}


#===============================================================
# Name:		    PurgeDir
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Purge directory
#
#
# Input Variables:
# 	- mxDaysAllowed: Maximum days of data to be keeped.
#
#
# Outputs:
#      - None
#
#===============================================================
function PurgeDir() {
    local mxDaysAllowed=$2
    if [[ ! -d $1 ]]
	then
	op_msg "Error: directory not found:$1"
	exit 1
    fi
    set $(ls -1 -d $1/200*)
    local dirs="$@"
    local ndirs=$#
    local ndirs2bpurged=0
    if [ $ndirs -gt $mxDaysAllowed ] 
	then
	let ndirs2bpurged=$ndirs-$mxDaysAllowed
	echo "Maximum Number of Archival-Days allowed exceeded: $ndirs"
	echo "Starting the purge...Number to be purged:$ndirs2bpurged"
	local ndirPurged=0
	for idir in $dirs
	  do
	  let ndirPurged=ndirPurged+1
	  if [ $ndirPurged -le $ndirs2bpurged ] 
	      then
	      rm -rf $idir/
	      echo "$ndirPurged ======= $idir  Purged !"
	  fi
	done
    fi
}


#===============================================================
# Name:		    DirExistEst
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Dir exists or not
#
#
# Input Variables:
#
#
# Outputs:
#      - None
#
#===============================================================
function DirExistEst() {
  if (( $# != 2 ))
  then
    op_msg "Error in calling function $0."
    op_msg "Usage: $0 dir_name(i.e. RDR-AMSUA) dir(i.e. /home/pub/data)"
    exit 3
  fi
  if [[ ! -d $2 ]]
  then
    op_msg "ERROR: $1 direcotry: not found. Exit!  $2 "
    exit 4
  elif [[ ! -r $2 ]]
  then
    op_msg "ERROR: $1 directory: not readable. Exit!"
    exit 4
  else
    echo "$1 directory: $2"
  fi
}



#===============================================================
# Name:		    DataExistEst
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Check data extension exist or not
#
#
# Input Variables:
#
#
# Outputs:
#      - None
#
#===============================================================
function DataExistEst() {
  if (( $# != 3 ))
  then
    op_msg "Error in calling function $0."
    op_msg "Usage: $0 sensor(i.e. AMSU-A) data(i.e. RDR) file_list(i.e. /home/pub/data/datalist.dat)"
    exit 3
  fi
  local xarr;
  xarr=$(cat $3)
  if [[ "$xarr" == "" ]]
  then
    op_msg "ERROR: No $1 $2 data files found. Exit!"
    exit 4
  fi
  local num_files;
  num_files=$(cat $3 |wc -l)  
  return $num_files
}


#===============================================================
# Name:		    leapYearEst
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	
#	return number of days in February
#
# Input Variables:
#	year  - 
#
# Outputs:
#      - leap days
#
#===============================================================
function leapYearEst() {
    if (( $# != 1 ))
    then 
        op_msg "Error in calling function $0."
        op_msg "Usage: $0 year(i.e. 2006)"
        exit 1
    fi
    local n1 n2 n3;
    ((n1=$1%400))
    ((n2=$1%4))
    ((n3=$1%100))
    local leap;
    if (( ${n1} == 0 ))
    then
        leap=29
    elif (( ${n2} == 0 && ${n3} != 0 ))
    then
        leap=29
    else
        leap=28
    fi
    echo ${leap}
}


#===============================================================
# Name:		    determineNdayBackDateExt
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	return a date in yyyy-mm-dd format
#
#
# Input Variables:
# 	- rdirExt: 
# 	- NdayBack: TB-Bias assessed w. data from NdayBack, due to GDAS delay.
#	  NdayBack between 0 & mxDaysAllowed-1
#
#
# Outputs:
#      - determineNdayBackDateExt 2007-04-05 2		==> 2007-04-03
#
#===============================================================
function determineNdayBackDateExt() {	
    local rdirExt=$1 	
    local NdayBack=$2
    local yyyymmdd=`date -d "$rdirExt $NdayBack days ago" +%Y-%m-%d`
    echo $yyyymmdd
}


#===============================================================
# Name:		    determineTodayDateExt
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	return yesterday's date in yyyy-mm-dd format
#	Only for some systems(Linux) has date built-in functionality
#
#
# Input Variables:
#	- none
#
# Outputs:
#      - yyyy-mm-dd 
#
#===============================================================
function determineTodayDateExt() {
    local yyyymmdd=`date -d "1 day ago"  +%Y-%m-%d`
    echo $yyyymmdd
}


#===============================================================
# Name:		    checkStatus
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	check previuos execution status, if positive, then exit the script
#
#
# Input Variables:
# 	- status: 
#
#
# Outputs:
#      - None
#
#===============================================================
function checkStatus() {
  local status=$1
  local step=$2
  if [[ ${status} -gt 0 ]] ; then
    op_msg "Error occurred in step: ${step}"
    exit ${status}
  fi
}

#=================================================================================================
#
#    Translate regular date to julian day
#    Usage:  yyyymmdd yyyy-mm-dd (i.e. 2006-03-14)
#    Output: must use a variable to receive jjj, sort of like function return something
#    
#	jjj=`yyyymmdd2jjj yyyy-mm-dd`
#    
#=================================================================================================
#===============================================================
# Name:		    yyyymmdd2jjj
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#    	Translate regular date to julian day
#
#    	Usage:  yyyymmdd yyyy-mm-dd (i.e. 2006-03-14)
#
# Input Variables:
# 	- arg: Argument
#
#
# Outputs:
#	- must use a variable to receive jjj, sort of like function return something
#      		(  jjj=`yyyymmdd2jjj yyyy-mm-dd`  )
#
#===============================================================
function yyyymmdd2jjj() {
    if (( $# != 1 ))
    then 
        op_msg "Error in calling function $0."
        op_msg "Usage: $0 yyyy-mm-dd (i.e. 2006-03-14)"
        exit 1
    fi  
    
    local arg=$1
    
    #declare -i yyyy mm dd
    local yyyy=`echo $arg | cut -c1-4`
    local mm=`echo $arg | cut -c6-7`
    local dd=`echo $arg | cut -c9-10`

    local m1=`echo $arg | cut -c6-6`
    local m2=`echo $arg | cut -c7-7`
    
    local d1=`echo $arg | cut -c9-9`
    local d2=`echo $arg | cut -c10-10`
    
    local JulianDate1 JulianDate2
    
    JulianDate1=( 0  31  60  91  121  152  182  213  244  274  305  335 )
    JulianDate2=( 0  31  59  90  120  151  181  212  243  273  304  334 )
    
    local div4 div100 div400
    
    let "div4=$yyyy % 4"
    let "div100=$yyyy % 100"
    let "div400=$yyyy % 400"
    
    local leap=0

    if [[ ( $div4 -eq 0 && $div100 -ne 0 ) || ( $div400 -eq 0 ) ]] ; then
    	leap=1
    fi
    
    local im id jday 
    
    if [[ $m1 == "0" ]] ; then
      im=$m2
    else
      im=$mm
    fi
    
    
    if [[ $d1 == "0" ]] ; then
      id=$d2
    else
      id=$dd
    fi
    
    
    if [[ $leap -eq 1 ]] ; then
    	let im=im-1
    	let jday=${JulianDate1[$im]}+id
    else
    	let im=im-1
    	let jday=${JulianDate2[$im]}+id
    fi
    
    if (( ${jday} < 10 ))
    then
        jday=00${jday}
    elif (( ${jday} < 100 ))
    then
        jday=0${jday}
    fi

    echo ${jday}
}

#===============================================================
# Usg:  yyyyjjj2yyyymmdd yyyy jjj
# Exe:  yyyyjjj2yyyymmdd 2007 109   ==> 2007-04-19
#	yyyymmdd=`yyyyjjj2yyyymmdd 2007 109`
#
# Input:  yyyy jjj
# Output: yyyy-mm-dd
#===============================================================
function yyyyjjj2yyyymmdd() {
    # Check the number of parameters
    if (( $# != 2 ))
    then 
        op_msg "Error in calling function $0."
        op_Msg "Usage: $0 year(i.e. 2006) julian_day(i.e. 23,  not 023)"
        exit 1
    fi
    # Validate julian date between -364 and 366 
    if [ $2 -gt 366 -o $2 -lt -364 ]
    then
        op_msg "Input Julian day error. EXIT!!!"
        exit 1
    fi
    # Apply input to local variables
    local year jday leap;
    year=$1
    jday=$2
    # If input julian day is negative or zero, subtract 1 to year and get positive jday for last year
    if [ ${jday} -lt 1 ]
    then
        (( year=${year}-1 ))
        # Calculate julian day depending on the leap year
        leap=`leapYearEst ${year}`
        if (( ${leap} == 28 ))
        then
            (( jday=${jday}+365 ))
        fi
        if (( ${leap} == 29 ))
        then
            (( jday=${jday}+366 ))
        fi
    fi

    # Estimate if current year is leap year
    local leap=`leapYearEst ${year}`
    # Define mday array for leap and non-leap year
    if (( ${leap} == 29 ))    # leap year
    then
        declare -a mday=(31 29 31 30 31 30 31 31 30 31 30 31)
    fi
    if (( ${leap} == 28 ))    # regular year
    then
        declare -a mday=(31 28 31 30 31 30 31 31 30 31 30 31)
    fi
    # Calculate month and day using julian day
    local index nday month day; 
    nday=0
    index=0
    while [ ${nday} -lt ${jday} ]
    do
        (( nday=${nday}+${mday[${index}]} ))
        (( index=${index}+1 ))
        if [ ${nday} -ge ${jday} ]
        then
            month=${index}
            (( index=${index}-1 ))
            (( day=${jday}-${nday}+${mday[${index}]} ))
        fi
    done
    # Compose month and day to be mm and dd
    if [ ${month} -lt 10 ]
    then 
        month=0${month}
    fi
    if [ ${day} -lt 10 ]
    then
        day=0${day}
    fi
    
    echo "${year}-${month}-${day}"
}


#===============================================================
# Name:		    jday2caldate
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Convert julian day into calendar date
#
#
# Input Variables:
# 	- yyyy: 
# 	- jjj: 
#
#
# Outputs:
#      - yyyy-mm-dd
#
#===============================================================

function jday2caldate() {

    local yyyy=$1
    local jjjtmp=$2
    
    let "jjj = 10#${jjjtmp}"
    
    local div4 div100 div400
    
    let "div4=$yyyy % 4"
    let "div100=$yyyy % 100"
    let "div400=$yyyy % 400"

    local leap=0

    if [[ ( $div4 -eq 0 && $div100 -ne 0 ) || ( $div400 -eq 0 ) ]] ; then
    	leap=1
    fi
    
    local JulianDate1 JulianDate2
    
    JulianDate1=( 0  31  60  91  121  152  182  213  244  274  305  335 366)
    JulianDate2=( 0  31  59  90  120  151  181  212  243  273  304  334 365)
    
    local mm=-1
    local dd=-1
    local i
    
    if [[ $leap -eq 1 ]] ; then
	i=0
	while [[ $i -le 11 ]]
	do
	    if [[ ( $jjj -gt ${JulianDate1[$i]} ) &&  ( $jjj -le ${JulianDate1[$i+1]} ) ]] ; then
		let mm=i+1
		let dd=jjj-${JulianDate1[$i]}
	    fi 
	    let i=i+1 
	done
	
    else
	i=0
	while [[ $i -le 11 ]]
	do
	    if [[ ( $jjj -gt ${JulianDate2[$i]} ) &&  ( $jjj -le ${JulianDate2[$i+1]} ) ]] ; then
		let mm=i+1
		let dd=jjj-${JulianDate2[$i]}
	    fi 
	    let i=i+1 
	done
    fi
    
    if [[ $mm -lt 10 ]] ; then
      	mm=0${mm}
    fi
    
    if [[ $dd -lt 10 ]] ; then
      	dd=0${dd}
    fi
    
    echo "$yyyy-$mm-$dd"
           
}

#jday2caldate 2007 109

#===============================================================
# Name:		    getDateFromFile
#
# Type:		    Bash Shell Script
#
# Description:
#	Parses an input level-lb file name to get the date
#
# Input Variables:
# 	- satId: satellite ID string
#       - inputPath: path to input files
#
#
#===============================================================

function getDateFromFile() {
    if [[ $# -ne 2 ]]
        then
        echo "Error: function getDateFromFile: check number of arguments"
        exit 1
    fi

    local satId=$1
    local inputPath=$2
    
    local file=""
    local ymd="xxxx-xx-xx"

    if [[ ${satId} == "n18" || ${satId} == "n19" || ${satId} == "metopA" || ${satId} == "metopB" ]] ; then
	filenum_amax=`ls -1 ${inputPath}/NSS.AMAX* 2> /dev/null | wc -l`
	filenum_mhsx=`ls -1 ${inputPath}/NSS.MHSX* 2> /dev/null | wc -l`
	if [[ ${filenum_amax} -eq ${filenum_mhsx} && ${filenum_amax} -gt 0 ]] ; then
	    file=`ls -1 ${inputPath}/NSS.AMAX* | head -1`
	    file=`basename ${file}`
	fi
    elif [[ ${satId} == "f16" || ${satId} == "f17" ||  ${satId} == "f18" ]] ; then
        filenum=`ls -1 ${inputPath}/NPR.TDRN* 2> /dev/null | wc -l`
        if [[ ${filenum} -gt 0 ]] ; then
	    file=`ls -1 ${inputPath}/NPR.TDRN* | head -1`
	    file=`basename ${file}`
	fi
    elif [[ ${satId} == "npp" ]] ; then
        filenum=`ls -1 ${inputPath}/*.h5 2> /dev/null | wc -l` 
        if [[ ${filenum} -gt 0 ]] ; then
	    file=`ls ${inputPath}/*.h5 | head -1`
	    file=`basename ${file}`
	fi
    elif [[ ${satId} == "trmm" ]] ; then
        filenum=`ls -1 ${inputPath}/*.HDF 2> /dev/null | wc -l` 
        if [[ ${filenum} -gt 0 ]] ; then
	    file=`ls ${inputPath}/*.HDF | head -1`
	    file=`basename ${file}`
	fi
    elif [[ ${satId} == "aqua" ]] ; then
        filenum=`ls -1 ${inputPath}/*.hdf 2> /dev/null | wc -l` 
        if [[ ${filenum} -gt 0 ]] ; then
	    file=`ls ${inputPath}/*.hdf | head -1`
	    file=`basename ${file}`
	fi
    elif [[ ${satId} == "mtma" || ${satId} == "mtsa" ]] ; then
        filenum=`ls -1 ${inputPath}/*.h5 2> /dev/null | wc -l` 
        if [[ ${filenum} -gt 0 ]] ; then
	    file=`ls ${inputPath}/*.h5 | head -1`
	    file=`basename ${file}`
	fi
    elif [[ ${satId} == "gcomw1" ]] ; then
        filenum=`ls -1 ${inputPath}/*.h5 2> /dev/null | wc -l` 
        if [[ ${filenum} -gt 0 ]] ; then
	    file=`ls ${inputPath}/*.h5 | head -1`
	    file=`basename ${file}`
	fi
    fi
    
    # only do this if we get a file
    if [[ ${file} != "" ]] ; then
    
      if [[ ${satId} == "n18" || ${satId} == "n19" || ${satId} == "metopA" || ${satId} == "f16" || ${satId} == "f17" || ${satId} == "f18" || ${satId} == "metopB" ]] ; then

	local yyjjj=`echo $file | cut -f4 -d'.'`
	local yy=`echo ${yyjjj} | cut -c2-3`
	local jjj=`echo ${yyjjj} | cut -c4-6`

	local y_val=$(echo $yy | sed 's/0*//')
	local j_val=$(echo $jjj | sed 's/0*//')
	local year=0

	if [[ ${y_val} -gt 50 ]] ; then
          let year=1900+y_val
	else
          let year=2000+y_val
	fi
	
	if [[ ${year} -gt 1950 ]] ; then
	  ymd=`jday2caldate ${year} ${j_val}`
	fi
	
      elif [[ ${satId} == "npp" ]] ; then

	local yyyymmdd=`echo $file | cut -c12-19`
	local yyyy=`echo ${yyyymmdd} | cut -c1-4`
	local mm=`echo ${yyyymmdd} | cut -c5-6`
	local dd=`echo ${yyyymmdd} | cut -c7-8`
	ymd=$yyyy-$mm-$dd

      elif [[ ${satId} == "trmm" ]] ; then

	local yymmdd=`echo $file | cut -f2 -d'.'`
	
	if [[ ${#yymmdd} -eq 6 ]] ; then # version 6 only use yymmdd
	
	  local yy=`echo ${yymmdd} | cut -c1-2`
	  local mm=`echo ${yymmdd} | cut -c3-4`
	  local dd=`echo ${yymmdd} | cut -c5-6`
	  local y_val=$(echo $yy | sed 's/0*//')
	  local yy1='20'
	
	  if [[ ${y_val} -gt 50 ]] ; then
            yy1='19'
	  else
            yy1='20'
	  fi
	  local yyyy=${yy1}${yy}
	else  # version 7 TRMM 1B11 use yyyymmdd now
	  
	  local yyyy=`echo ${yymmdd} | cut -c1-4`
	  local mm=`echo ${yymmdd} | cut -c5-6`
	  local dd=`echo ${yymmdd} | cut -c7-8`
	fi
	
	ymd=$yyyy-$mm-$dd

      elif [[ ${satId} == "aqua" ]] ; then

	local yyyymmdd=`echo $file | cut -c39-46`
	local yyyy=`echo ${yyyymmdd} | cut -c1-4`
	local mm=`echo ${yyyymmdd} | cut -c5-6`
	local dd=`echo ${yyyymmdd} | cut -c7-8`
	ymd=$yyyy-$mm-$dd

      elif [[ ${satId} == "mtma" ]] ; then

	local yyyy=`echo ${file} | cut -c25-28`
	local mm=`echo ${file} | cut -c30-31`
	local dd=`echo ${file} | cut -c33-34`
	ymd=$yyyy-$mm-$dd

      elif [[ ${satId} == "mtsa" ]] ; then

	local yyyy=`echo ${file} | cut -c29-32`
	local mm=`echo ${file} | cut -c34-35`
	local dd=`echo ${file} | cut -c37-38`
	ymd=$yyyy-$mm-$dd

      elif [[ ${satId} == "gcomw1" ]] ; then

	local yyyy=`echo ${file} | cut -c8-11`
	local mm=`echo ${file}   | cut -c12-13`
	local dd=`echo ${file}   | cut -c14-15`
	ymd=$yyyy-$mm-$dd

      fi
    
    fi

    echo $ymd

}


#===============================================================
# Name:		    WriteListOfOutputs
#
# Type:		    Bash Shell Script
#
# Description:
#	Write a list file containing the path/filenames
#       of generated netCDF outputs
#
# Input Variables:
# 	- edrDir: directory with EDR files
#       - depDir: directory with DEP files
#       - pathOut: directory to write to 
#       - ListFileName: filename of list to write
#       - nedtFile: noise file created to list
#       - edrList: list of output EDR files to list
#
#
#===============================================================

function WriteListOfOutputs() {	
    if [[ $# -ne 8 ]]
    then
	op_msg "Error: function WriteListOfOutputs: check number of arguments: $#"
	exit 1      
    fi
    local edrDir=$1
    local depDir=$2
    local ncDir=$3
    local fmsdrDir=$4
    local pathOut=$5
    local ListFileName=$6
    local nedtDir=$7
    local edrList=$8

    sndFile=`ls ${ncDir}/*SND*.nc`
    imgFile=`ls ${ncDir}/*IMG*.nc`
    edrFile=`more ${edrList}`
    depFile=`ls ${depDir}/DEP*`

    #---Check existence of either converted SND and IMG files
    if [[ -n "${imgFile}" || -n "${sndFile}" ]]
    then
	echo -n "" > ${pathOut}/${ListFileName}
    else
	op_msg "Error: No IMG or SND .nc files!"
	exit 1
    fi

    #---Check that null list file was created
    if [ ! -e ${pathOut}/${ListFileName} ]
    then
	op_msg "Error: List file containing output filenames not created"
	exit 1
    fi

    #---Check existence of SND and IMG and write to list if they exist
    if [ -n "${sndFile}" ]
    then
	echo `ls ${ncDir}/*SND*.nc >>  ${pathOut}/${ListFileName}`
    else
	op_msg "Error: MIRS SND .nc file not found!"
    fi
    if [ -n "${imgFile}" ]
    then
	echo `ls ${ncDir}/*IMG*.nc >> ${pathOut}/${ListFileName}`
    else
	op_msg "Error: MIRS IMG .nc file not found!"
    fi

    #---Check existence of EDR and DEP files used by QC DAP
    if [ -n "${edrFile}" ]
    then
	echo `more ${edrList} >>  ${pathOut}/${ListFileName}`
    else
	op_msg "Error: MIRS EDR file not found!"
    fi
    if [ -n "${depFile}" ]
    then
	echo "${depFile}" >> ${pathOut}/${ListFileName}
    else
	op_msg "Error: MIRS DEP file not found!"
    fi


    #---Check for NEDT, FMSDR files used by QC DAP
    fmsdrFile=`ls ${fmsdrDir}/FMSDR*`
    if [ -n "${fmsdrFile}" ]
    then
	echo `ls ${fmsdrDir}/FMSDR* >> ${pathOut}/${ListFileName}`
    else
	op_msg "Error: No FMSDR files created!"
    fi

    nedtFileBef=`ls ${nedtDir}/NEDT*befFM.dat`
    if [ -n "${nedtFileBef}" ]
    then
	echo "${nedtFileBef}" >> ${pathOut}/${ListFileName}
    else
	op_msg "Error: No NEDT file created before footprint matching!"
    fi

    nedtFileAft=`ls ${nedtDir}/NEDT*aftFM.dat`
    if [ -n "${nedtFileAft}" ]
    then
	echo "${nedtFileAft}" >> ${pathOut}/${ListFileName}
    else
	op_msg "Error: No NEDT file created after footprint matching!"
    fi

    echo "#End-of-PSF" >> ${pathOut}/${ListFileName}
}

#===============================================================
# Name:		    WriteListOfOutputs_QC
#
# Type:		    Bash Shell Script
#
# Description:
#	Write a list file containing the path/filenames
#       of generated QC outputs
#
# Input Variables:
#
#       - pathOut:      directory to write to
#       - fileExt:      filename extension
#       - ListFileName: filename of list to write
#       - satId:        satellite string ID
#
#
#===============================================================

function WriteListOfOutputs_QC() {	
    if [[ $# -ne 5 ]]
    then
	op_msg "Error: function WriteListOfOutputs_QC: check number of arguments: $#"
	exit 1      
    fi

    local nedtDir=$1
    local pathOut=$2
    local fileExt=$3
    local ListFileName=$4
    local satId=$5

    local QCFile=`ls ${nedtDir}/qcRecord_${satId}_d* | tail -1`
    local biasFile=`ls ${nedtDir}/biasRecord_${satId}_d* | tail -1`
    local nedtFile=`ls ${nedtDir}/nedtRecord_${satId}_d* | tail -1`

    pngFile=`ls ${pathOut}/*.png | head -1`
    local checkNedtFile=${pathOut}/qcNedt_Alert_${satId}_${fileExt}.txt
    local checkQCFile=${pathOut}/qcRetrieval_Alert_${satId}_${fileExt}.txt
    local checkBiasFile=${pathOut}/qcRadioBias_Alert_${satId}_${fileExt}.txt

    > ${pathOut}/${ListFileName}
    #---Check that null list file was created
    if [ ! -e ${pathOut}/${ListFileName} ]
    then
	op_msg "Error: List file containing output filenames not created"
	exit 1
    fi

    #---Check existence of output QC Monitor, Bias and NEDT files
    if [[ -e "${QCFile}" ]]
    then
	echo ${QCFile} > ${pathOut}/${ListFileName}
    else
	op_msg "Error: No QC Monitoring Record File!"
	exit 1
    fi
    if [[ -e "${biasFile}" ]]
    then
	echo ${biasFile} >> ${pathOut}/${ListFileName}
    else
	op_msg "Error: No Bias Monitoring Record File!"
	exit 1
    fi
    if [[ -e "${nedtFile}" ]]
    then
	echo ${nedtFile} >> ${pathOut}/${ListFileName}
    else
	op_msg "Error: No NEDT Monitoring Record File!"
	exit 1
    fi
    if [[ -s "${checkNedtFile}" ]]
    then
	echo ${checkNedtFile} >> ${pathOut}/${ListFileName}
    else
	op_msg "Check NEDT: OK"
    fi
    if [[ -s "${checkQCFile}" ]]
    then
	echo ${checkQCFile} >> ${pathOut}/${ListFileName}
    else
	op_msg "Check QC/Convergence: OK"
    fi
    if [[ -s "${checkBiasFile}" ]]
    then
	echo ${checkBiasFile} >> ${pathOut}/${ListFileName}
    else
	op_msg "Check Bias: OK"
    fi

    #---Check existence of output PNG files
    if [[ -e "${pngFile}" ]]
    then
	ls ${pathOut}/*.png >> ${pathOut}/${ListFileName}
    else
	op_msg "Warning: No PNG Image Files Created!"
	exit 1
    fi

    echo "#End-of-PSF" >> ${pathOut}/${ListFileName}
}


#===============================================================
# Name:		    mirs2nc
#
# Type:		    Bash Shell Script
#
# Description:
#         Convert files from MiRS binary to NetCDF4
#
# Input Variables:
# 	- edrPath    : directory with EDR files
#       - depPath    : directory with DEP files
#       - ncPath     : directory to write NETCDF4 files 
#       - exePath    : path of converter executable
#       - satId      : satellite string id
#
#
#===============================================================

function mirs2nc() {

    if [[ $# -ne 5 ]]
    then
        op_msg "Error: function mirs2nc: check number of arguments: $#"
        exit 1
    fi
    local edrPath=$1
    local depPath=$2
    local ncPath=$3
    local exePath=$4
    local satId=$5

    local edrList=`ls -1 ${edrPath}/EDR*ORB`
    local depList=`ls -1 ${depPath}/DEP*ORB`
    
    local edrFiles=($edrList)
    local depFiles=($depList)
    
    local nedr=${#edrFiles[@]}
    local ndep=${#depFiles[@]}
    
    
    #---Check existence of EDR and DEP files to convert
    if [[ ${nedr} -eq 0 ]] ; then
	op_msg "Error: NO EDR files"
	exit 1
    fi

    if [[ ${ndep} -eq 0 ]] ; then
	op_msg "Error: NO DEP files"
	exit 1
    fi

    if [[ ${nedr} -ne ${ndep} ]] ; then
	op_msg "Error: EDR file number != DEP file number"
	exit 1
    fi
    
    local slash_end=`echo ${ncPath} | grep '/$'`
    if [[ ${slash_end} == "" ]] ; then
        rm -f ${ncPath}/*.nc
    else
        #echo "orbit mode"
        rm -f ${ncPath}*.nc
    fi
    
    #---- convert mirs edr & dep into netcdf4 files one by one 
    for (( i=0; i<${nedr}; i++ )) ; do
      local edrFile=${edrFiles[$i]}
      local depFile=${depFiles[$i]}
      local edr=`basename ${edrFile}`
      
      if [[ ${satId} == "npp" ]] ; then
      
        local yyyymmdd=`echo ${edr} | cut -f4 -d_ | cut -c2-9` 
        local HMS_s=`echo ${edr} | cut -f5 -d_ | cut -c2-8` 
        local HMS_e=`echo ${edr} | cut -f6 -d_ | cut -c2-8` 
      
        local time=`date +%Y%m%d%H%S%M`
        local part=s${yyyymmdd}${HMS_s}_e${yyyymmdd}${HMS_e}_c${time}.nc
      
        imgFile=NPR-MIRS-IMG_v9_NPP_${part}
        sndFile=NPR-MIRS-SND_v9_NPP_${part}
	
      else
        
	imgFile=${edr/EDR_/IMG_}.nc
	sndFile=${edr/EDR_/SND_}.nc
	
      fi
      
      ${exePath}/mirs2nc ${edrFile} ${depFile} ${ncPath}/ ${imgFile} ${sndFile} 
      
    done
    
    echo "End of step mirs2nc"
}


#===============================================================
# Name:		    mirs2nc_parallel
#
# Type:		    Bash Shell Script
#
# Description:
#         Convert files from MiRS binary to NetCDF4 using 
#         multiple CPUs in a control mode.  You change ncpu
#         according to your own specification.
#
# Input Variables:
# 	- edrPath    : directory with EDR files
#       - depPath    : directory with DEP files
#       - ncPath     : directory to write NETCDF4 files 
#       - exePath    : path of converter executable
#       - satId      : satellite string id
#
#
#===============================================================

function mirs2nc_parallel() {

    if [[ $# -ne 5 ]]
    then
        op_msg "Error: function mirs2nc_parallel: check number of arguments: $#"
        exit 1
    fi
    local edrPath=$1
    local depPath=$2
    local ncPath=$3
    local exePath=$4
    local satId=$5

    local edrList=`ls -1 ${edrPath}/EDR*ORB`
    local depList=`ls -1 ${depPath}/DEP*ORB`
    
    local edrFiles=($edrList)
    local depFiles=($depList)
    
    local nedr=${#edrFiles[@]}
    local ndep=${#depFiles[@]}
    
    #---Check existence of EDR and DEP files to convert
    if [[ ${nedr} -eq 0 ]] ; then
	op_msg "Error: NO EDR files"
	exit 1
    fi

    if [[ ${ndep} -eq 0 ]] ; then
	op_msg "Error: NO DEP files"
	exit 1
    fi

    if [[ ${nedr} -ne ${ndep} ]] ; then
	op_msg "Error: EDR file number != DEP file number"
	exit 1
    fi

    local slash_end=`echo ${ncPath} | grep '/$'`
    if [[ ${slash_end} == "" ]] ; then
        rm -f ${ncPath}/*.nc
    else
        #echo "orbit mode"
        rm -f ${ncPath}*.nc
    fi
    
    # Our system has 24 CPU,  but we only want to use 20
    local ncpu=20
    
    #---- convert mirs edr & dep into netcdf4 files one by one 
    for (( i=0; i<${nedr}; i++ )) ; do
      local edrFile=${edrFiles[$i]}
      local depFile=${depFiles[$i]}
      local edr=`basename ${edrFile}`
      
      if [[ ${satId} == "npp" ]] ; then
      
        local yyyymmdd=`echo ${edr} | cut -f4 -d_ | cut -c2-9` 
        local HMS_s=`echo ${edr} | cut -f5 -d_ | cut -c2-8` 
        local HMS_e=`echo ${edr} | cut -f6 -d_ | cut -c2-8` 
      
        local time=`date +%Y%m%d%H%S%M`
        local part=s${yyyymmdd}${HMS_s}_e${yyyymmdd}${HMS_e}_c${time}.nc
      
        imgFile=NPR-MIRS-IMG_v7_NPP_${part}
        sndFile=NPR-MIRS-SND_v7_NPP_${part}
	
      else
        
	imgFile=${edr/EDR_/IMG_}.nc
	sndFile=${edr/EDR_/SND_}.nc
	
      fi
      
      ${exePath}/mirs2nc ${edrFile} ${depFile} ${ncPath}/ ${imgFile} ${sndFile} &
      
      local mod=$(($i % $ncpu))
      # wait until this bunch of orbit finish
      if [[ $mod -eq 0 && $i -ne 0 ]] ; then
          echo $i
          wait
      fi
      
    done
    
    wait
    
    echo "End of step mirs2nc_parallel"
}


#===============================================================
# To monitor NEDT
# only for POES(N18/N19/MetopA)
# this will call IDL code: src/testbed/nedtMonitoring/checkNEDT.pro
# 	- satId: Satellite short name (n18,metopA,n19)
# 	- nedtPath: the location where nedt files reside (data/TestbedData/nedt/n19_amsua_mhs)
# 	- ymd: date ( yyyym-mm-dd )
#	- tunFile: Tuning file: ( data/StaticData/TuningData/TunParams_[satId]_amsua_mhs.in )
# 	- dirSrc: nedt IDL src directory ( src/testbed/nedtMonitoring )
# 	- IDL: IDL executable ( example: /usr/local/bin/idl )
# 	- dirWorking: the working directory in which to put temporary files like checkResult and batchCheckNEDT.pro
# 	- email: comma separated email addresses ( example: "tom.mike@gmail.com, sam.chen@noaa.gov" )
# 	- website: the web site address where nedt plots located
#
#   Wanchun Chen 08/20/2010
#===============================================================

function qcNedt() {
  if [[ $# -ne 9 ]] ; then	
    op_msg "Error: function qcNedt: check number of arguments: $#"
    exit 1
  fi

  local satId=$1
  local nedtFile=$2
  local checkResult=$3
  local tunFile=$4
  local dirSrc=$5
  local IDL=$6
  local dirWorking=$7
  local email=$8
  local website=$9

  rm -f ${checkResult}
  
  if [[ -s ${nedtFile} ]] ; then

    cd ${dirSrc}
    echo "!QUIET=1"                           			                          >  ${dirWorking}/batchCheckNEDT.pro
    echo ".r ${dirSrc}/checkNEDT.pro" 			                                  >> ${dirWorking}/batchCheckNEDT.pro 
    echo "checkNEDT,nedtFile='${nedtFile}',nedtTxt='${checkResult}',tunFile='${tunFile}'" >> ${dirWorking}/batchCheckNEDT.pro 
    echo "exit"                               			                          >> ${dirWorking}/batchCheckNEDT.pro 

    ${IDL} ${dirWorking}/batchCheckNEDT.pro

    if [[ -s ${checkResult} ]] ; then
        echo "" >> ${checkResult}
        if [[ ! -z "${website}" ]] ; then
	  echo ${website} >> ${checkResult}
        fi
	if [[ ! -z "${email}" ]] ; then
	  mail -s "${satId} NEDT Alert for ${nedtFile}" ${email} < ${checkResult}
	fi
    	rm -f ${dirWorking}/batchCheckNEDT.pro   
    fi
  fi

  echo "End of step qcNedt"

}


#===============================================================
# To monitor retrieval quality
# 
#	- depDir: DEP directory
#	- depList: list of dep files
#	- pathMon: location to output those qc stats files
#		   ex: data/TestbedData/PerfsMonitoring/n18_amsua_mhs/orbitmon/
#	- IDL: IDL executable ( ex: /usr/local/bin/idl )
#	- dirSrc: directory where qcRetrieval.pro reside ( src/testbed/grid/ )
#	- checkFile: a temporary file to save abnormal stats
#	- namelist: namelist file of input parameter to qcRetrieval.pro
#	- email: double quoted, comma separated email list
#		ex: "Wanchun.Chen@noaa.gov, Kevin.Garrett@noaa.gov"
#
# Wanchun Chen      08/20/2010
#===============================================================

function qcRetrieval_IDL() {
    if [[ $# -ne 10 ]] ; then
      op_msg "Error: function qcRetrieval_IDL: check number of arguments: $#"
      exit 1
    fi

    local depDir=$1
    local depList=$2
    local pathMon=$3
    local IDL=$4
    local dirSrc=$5
    local checkResult=$6
    local namelist=$7
    local email=$8
    local website=$9
    local dirControl=${10}
    
    if [[ ! -s ${depList} ]] ; then
      ls -1 ${depDir}/DEP* > ${depList}
    fi
    
    local line=`wc -l ${depList} | awk '{print $1}'`
    if [[ ${line} -eq 0 ]] ; then
      op_msg "Error: No DEP files exist"
      exit 1
    fi
    
    echo ${depList}     >  ${namelist}
    echo ${pathMon}     >> ${namelist}
    echo ${checkResult} >> ${namelist}
    
    cd ${dirSrc}
    echo "!QUIET=1"                           >  ${dirControl}/batchQcRetrieval.pro
    echo ".r ${dirSrc}/qcRetrieval.pro"       >> ${dirControl}/batchQcRetrieval.pro 
    echo "QCRETRIEVAL,namelist='${namelist}'" >> ${dirControl}/batchQcRetrieval.pro 
    echo "exit"                               >> ${dirControl}/batchQcRetrieval.pro 

    ${IDL} ${dirControl}/batchQcRetrieval.pro
    rm -f ${dirControl}/batchQcRetrieval.pro
    
    if [[ -s ${checkResult} ]] ; then
        if [[ ! -z "${website}" ]] ; then
	  echo ${website} >> ${checkResult}
        fi
	if [[ ! -z "${email}" ]] ; then
	  mail -s "QC retrieval alert" ${email} < ${checkResult}
	fi 
    fi
    #rm -f ${checkResult} ${namelist}
    
    echo "End of step qcRetrieval_IDL"
}


#===============================================================
# To monitor retrieval quality
# 
#	- depDir: DEP directory
#	- depList: list of dep files
#	- pathMon: location to output those qc stats files
#		   ex: data/TestbedData/PerfsMonitoring/n18_amsua_mhs/orbitmon/
#	- IDL: IDL executable ( ex: /usr/local/bin/idl )
#	- dirSrc: directory where qcRetrieval.pro reside ( src/testbed/grid/ )
#	- checkFile: a temporary file to save abnormal stats
#	- namelist: namelist file of input parameter to qcRetrieval.pro
#	- email: double quoted, comma separated email list
#		ex: "Wanchun.Chen@noaa.gov, Kevin.Garrett@noaa.gov"
#
# Wanchun Chen      08/20/2010
#===============================================================

function qcRetrieval() {
    if [[ $# -ne 13 ]] ; then
      op_msg "Error: function qcRetrieval: check number of arguments: $#"
      exit 1
    fi

    local depDir=$1
    local depList=$2
    local pathMon=$3
    local IDL=$4
    local dirSrc=$5
    local checkResult=$6
    local namelist=$7
    local email=$8
    local website=$9
    local dirControl=${10}
    local satId=${11}
    local ymd=${12}
    local binDir=${13}
    
    if [[ ! -s ${depList} ]] ; then
      ls -1 ${depDir}/DEP* > ${depList}
    fi
    
    local line=`wc -l ${depList} | awk '{print $1}'`
    if [[ ${line} -eq 0 ]] ; then
      op_msg "Error: No DEP files exist"
      exit 1
    fi
    
    set  "listDep='${depList}'"	\
         "satId='${satId}/'"	\
         "pathMon='${pathMon}'"	\
         "fileAbnormal='${checkResult}'"
    
    CreatNamList ${namelist} '&qcRetrievalNameList' "$@"
    ${binDir}/qcRetrieval < ${namelist}
    checkStatus $? "qcRetrieval: ${satId} ${ymd}"

    if [[ -s ${checkResult} ]] ; then
        if [[ ! -z "${website}" ]] ; then
	  echo ${website} >> ${checkResult}
        fi
	if [[ ! -z "${email}" ]] ; then
	  mail -s "QC retrieval alert for ${satId} ${ymd}" ${email} < ${checkResult}
	fi 
    fi
    #rm -f ${checkResult} ${namelist}
    
    echo "End of step qcRetrieval"
}


#===============================================================
# To monitor radiometric bias
# 
# it calls IDL code: src/testbed/biasGenerAndMonit/checkBias.pro
#
# 	- satId: Satellite short name (n18,metopA,n19,f16,f18,trmm,npp,etc)
# 	- biasPath: bias files dir (data/SemiStaticData/biasCorrec)
# 	- nedtPath: the location where nedt files reside (data/TestbedData/nedt/n19_amsua_mhs)
#	- tunFile: Tuning file: ( data/StaticData/TuningData/TunParams_[satId]_amsua_mhs.in )
# 	- ymd: date ( yyyym-mm-dd )
# 	- dirSrc: nedt IDL src directory ( src/testbed/nedtMonitoring )
# 	- IDL: IDL executable ( example: /usr/local/bin/idl )
# 	- dirWorking: the working directory in which to put temporary files like checkResult and batchCheckNEDT.pro
# 	- nwpId: integer 1,2,3 to denote gdas,ecmwf and gfs, respectively.
# 	- email: comma separated email addresses ( example: "tom.mike@gmail.com, sam.chen@noaa.gov" )
# 	- website: the web site address where nedt plots located
# 
# Wanchun Chen      08/20/2010
#===============================================================

function qcRadioBias() {
  if [[ $# -ne 13 ]] ; then
    op_msg "Error: function qcRadioBias: check number of arguments: $#"
    exit 1
  fi

  local satId=$1
  local nedtFile=$2
  local tunFile=$3
  local biasFileDynamic=$4
  local biasFileStatic=$5
  local modelErrFile=$6
  local checkResult=$7
  local dirSrc=$8
  local IDL=$9
  local dirWorking=${10}
  local nwpId=${11}
  local email=${12}
  local website=${13}

  local nwpStr='gdas'

  if   [[ ${nwpId} -eq 1 ]] ; then
      nwpStr='gdas'
  elif [[ ${nwpId} -eq 2 ]] ; then
      nwpStr='ecmw'
  elif [[ ${nwpId} -eq 3 ]] ; then
      nwpStr='gfs'
  fi

  local biasFile2Use=${biasFile}_${nwpStr}
  local modelErrFile2Use=${modelErrFile}_${nwpStr}
  rm -f ${checkResult}

  if [[ -s ${biasFile2Use} && -s ${modelErrFile2Use} ]] ; then
  
    cd ${dirSrc}
    echo "!QUIET=1" > ${dirWorking}/batchCheckBias.pro
    echo ".r checkBias.pro" >> ${dirWorking}/batchCheckBias.pro 
    echo "checkBias,satId='${satId}',biasFileDynamic='${biasFile2Use}',biasFileStatic='${biasFileStatic}',modelErrFile='${modelErrFile2Use}',nedtFile='${nedtFile}',tunFile='${tunFile}',biasTxt='${checkResult}'" >> ${dirWorking}/batchCheckBias.pro 
    echo "exit" >> ${dirWorking}/batchCheckBias.pro 

    ${IDL} ${dirWorking}/batchCheckBias.pro

    if [[ -s ${checkResult} ]] ; then
      echo "" >> ${checkResult}
      if [[ ! -z "${website}" ]] ; then
        echo ${website} >> ${checkResult}
      fi
      if [[ ! -z "${email}" ]] ; then
        mail -s "${satId} radiometric bias abnormal for ${yyyy}-${mm}-${dd}" ${email} < ${checkResult}
      fi
      rm -f ${dirWorking}/batchCheckBias.pro   
    fi
    
  fi

  echo "End of step qcRadioBias"
}


#===============================================================
# Name:		    qcRadioGeo
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Level II and III Level qc 
#
#
# Input Variables:
#
#	- satId: satellite ID ( n18/metopA/n19/f16/f18/trmm/npp, etc)
#	- nwpId: reference data set ID ( 1:gdas; 2:ecmwf; 3: gfs; )
#	- ymd: yyyy-mm-dd
#	- edrDir: EDR data directory
#	- depDir: DEP data directory
#	- nwpDir: NWP data directory
#	- fwdDir: FWD data directory
#	- edrList: EDR file list
#	- depList: DEP file list
#	- nwpList: NWP file list
#	- fwdList: FWD file list
#	- gridSrc: IDL source code directory
#	- gridDir: gridded data directory
#	- binDir: executable directory
#	- grid_cntrl: control file in gridding step
#	- gridfactor: gridded factor ( 4: 0.25 degree; 2: 0.5 degree; 1: 1.0 degree )
#	- latmin: min latitude
#	- latmax: max latitude
#	- lonmin: min longitude 
#	- lonmax: max longitude 
#	- orbitmon_path: retrieval quality file directory
#	- figDir: images directory
#	- fig_cntrl: control file in image generation control step
#	- IDL: idl executable ( ex: /usr/local/bin/idl )
#	- region: region code ( 0-glbal,1-USA,2-Europe,3-Gulf of Mexico, 5-east china sea, 6-user defintion )
#	- version: revision version
#	- plot_edrdep: to plot MIRS EDR and DEP products or not ( 1-to do; 0-not plot )
#	- plot_nwp:    to plot NWP products or not ( 1-to do; 0-not plot )
#	- plot_fwd:    to plot FWD products or not ( 1-to do; 0-not plot )
#	- plot_bias:   to plot Bias/Asymmetry products or not ( 1-to do; 0-not plot )
#	- plot_p2p:    to do P2P comparison or not ( 1-to do; 0-not plot )
#	- fmType: fmType resolution type
#	- edrGridStr: gridEdr product id list
#	- edrP2PStr: p2pEdr product id list
#	- depGridStr: gridDep product id list
#	- depP2PStr: p2pDep product id list
#	- nwpGridStr: NWP product id list for gridEdr
#	- nwpP2PStr:: NWP product id list for p2pEdr
#
# Outputs:
#      	- None
#
#===============================================================
function qcRadioGeo() {

    if [[ $# -ne 39 ]] ; then
        op_msg "Error: function qcRadioGeo: check number of arguments: $#"
        exit 1
    fi
    
    local satId=$1
    local nwpId=$2
    local ymd=$3
    
    local edrDir=$4
    local depDir=$5
    local nwpDir=$6
    local fwdDir=$7
    
    local edrList=$8
    local depList=$9
    local nwpList=${10}
    local fwdList=${11}
    
    local gridSrc=${12}
    local gridDir=${13}
    local binDir=${14}

    local grid_cntrl=${15}
    local gridfactor=${16}
    local latmin=${17}
    local latmax=${18}
    local lonmin=${19}
    local lonmax=${20}
    
    local orbitmon_path=${21}
    
    local figDir=${22}
    local fig_cntrl=${23}
    local IDL=${24}
    local region=${25}
    local version=${26}
    
    local plot_edrdep=${27}
    local plot_nwp=${28}
    local plot_fwd=${29}
    local plot_bias=${30}
    local plot_p2p=${31}
    
    local fmType=${32}
    local edrGridStr=${33}
    local edrP2PStr=${34}
    local depGridStr=${35}
    local depP2PStr=${36}
    local nwpGridStr=${37}
    local nwpP2PStr=${38}
    local nedtExt=${39}
    
    local nwptxt
    local NWPTXT
    if [[ ${nwpId} -eq 1 ]] ; then
      nwptxt='gdas'
      NWPTXT='GDAS'
    elif [[ ${nwpId} -eq 2 ]] ; then
      nwptxt='ecmwf'
      NWPTXT='ECMW'
    elif [[ ${nwpId} -eq 3 ]] ; then
      nwptxt='gfs'
      NWPTXT='GFS'
    fi
    
    local yyyymmdd=`echo $ymd | cut -c1-4,6-7,9-10`
    local processMode=1 # daily mode
    
    ls -1 ${edrDir}/EDR_* > ${edrList}
    ls -1 ${depDir}/DEP_* > ${depList}
    
    local line_edr=`wc -l ${edrList} | awk '{print $1}'`
    local line_dep=`wc -l ${depList} | awk '{print $1}'`
    
    if [[ ${line_edr} -eq 0 || ${line_dep} -eq 0 ]] ; then
        op_msg "Error: No EDR or DEP files"
	exit 1
    fi
    
    # check if gridded files already exist or not, to avoid redudant work.
    local fileQc=${gridDir}/GRID_${satId}_${yyyymmdd}_qc_ds.dat
    
    # rr file check 
    local fileRr=${gridDir}/GRID_${satId}_${yyyymmdd}_rr_ds.dat
    
    # EDR gridding
    if [[ ! -e ${fileQc} ]] ; then
      echo "EDR gridding..."
      set "filesList='${edrList}'"	     	\
	  "satId='${satId}'"		     	\
	  "yyyymmdd='${yyyymmdd}'" 	     	\
	  "gridfactor=${gridfactor}"	     	\
	  "gridPath='${gridDir}/'" 	     	\
	  "latmin=${latmin}"		     	\
	  "latmax=${latmax}"		     	\
	  "lonmin=${lonmin}"  		     	\
	  "lonmax=${lonmax}"		     	\
	  "isMirs=0"			     	\
	  "processMode=${processMode}"          \
	  "fmType=${fmType}"                    \
	  "prodStr='${edrGridStr}'" 
      CreatNamList ${grid_cntrl}_gridEdr '&gridEdrNameList' "$@"
      ${binDir}/gridEdr < ${grid_cntrl}_gridEdr
      checkStatus $? "qcRadioGeo:gridEdr"
    fi
    
    # DEP gridding
    if [[ ! -e ${fileRr} ]] ; then 
      echo "DEP gridding..."
      set "filesList='${depList}'" 		\
    	  "satId='${satId}'" 			\
	  "yyyymmdd='${yyyymmdd}'" 		\
	  "gridfactor=${gridfactor}"		\
          "gridPath='${gridDir}/'"  		\
       	  "latmin=${latmin}"		     	\
       	  "latmax=${latmax}"		     	\
       	  "lonmin=${lonmin}"  		     	\
       	  "lonmax=${lonmax}"		     	\
	  "processMode=${processMode}"          \
	  "fmType=${fmType}"                    \
	  "prodStr='${depGridStr}'"
      CreatNamList ${grid_cntrl}_gridDep '&gridDepNameList' "$@"
      ${binDir}/gridDep < ${grid_cntrl}_gridDep
      checkStatus $? "qcRadioGeo:gridDep"
    fi
    
    
    # rr file is the last one to write out, we use it as indicator file
    # ex: GRID_n18_gdas_20100815_rr_ds.dat
    # ex: GRID_n18_ecmwf_20100815_rr_ds.dat
    # ex: GRID_n18_gfs_20100815_rr_ds.dat
    local fileRrNwp=${gridDir}/GRID_${satId}_${nwptxt}_${yyyymmdd}_rr_ds.dat
    
    # construct NWP file list 
    ls -1 ${nwpDir}/NWP_${NWPTXT}* > ${nwpList}_${nwptxt}
    local line1=`wc -l ${nwpList}_${nwptxt} | awk '{print $1}'`
    if [[ ! -e ${fileRrNwp} && ${line1} -ge 1 ]] ; then
      echo "NWP gridding..."
      set "filesList='${nwpList}_${nwptxt}'"	\
          "satId='${satId}'"			\
          "yyyymmdd='${yyyymmdd}'"	     	\
          "gridfactor=${gridfactor}"	      	\
    	  "gridPath='${gridDir}/'"	      	\
          "latmin=${latmin}"  		      	\
          "latmax=${latmax}"			\
          "lonmin=${lonmin}" 		      	\
          "lonmax=${lonmax}"  		      	\
          "isMirs=${nwpId}"		      	\
          "processMode=${processMode}"          \
	  "fmType=${fmType}"                    \
	  "prodStr='${nwpGridStr}'"  
      CreatNamList ${grid_cntrl}_${nwptxt}_gridEdr '&gridEdrNameList' "$@"
      ${binDir}/gridEdr < ${grid_cntrl}_${nwptxt}_gridEdr
      checkStatus $? "qcRadioGeo: gridEdr ${NWPTXT}"
    fi

    
    # scan angle file as an indicator file
    # GRID_n18_gdas_20100815_angle_ds.dat
    # GRID_n18_gfs_20100815_angle_ds.dat
    # GRID_n18_ecmwf_20100815_angle_ds.dat
    local fileTb=${gridDir}/GRID_${satId}_${nwptxt}_${yyyymmdd}_tb_ds.dat

    # construct FWD file list 
    ls -1 ${fwdDir}/FWD_${NWPTXT}* > ${fwdList}_${nwptxt}
    local line2=`wc -l ${fwdList}_${nwptxt} | awk '{print $1}'`
    # FWD(Rad) gridding
    if [[ ! -e ${fileTb} && ${line2} -ge 1 ]] ; then
      echo "FWD gridding..."
      set "filesList='${fwdList}_${nwptxt}'"	\
          "satId='${satId}'"		      	\
          "yyyymmdd='${yyyymmdd}'"	      	\
          "gridfactor=${gridfactor}"	      	\
    	  "gridPath='${gridDir}/'"	      	\
          "latmin=${latmin}"  		      	\
          "latmax=${latmax}"			\
          "lonmin=${lonmin}" 		      	\
          "lonmax=${lonmax}"  		      	\
          "isMirs=${nwpId}"                     \
	  "fmType=${fmType}"
      CreatNamList ${grid_cntrl}_${nwptxt}_gridRad '&gridRadNameList' "$@"
      ${binDir}/gridRad < ${grid_cntrl}_${nwptxt}_gridRad
      checkStatus $? "qcRadioGeo:gridRad ${NWPTXT}"
    fi
    
    
    # to compute bias and bias asymmetry
    local nchan=20
    local isatId=1
    
    if [[ ${satId}   == 'n18'    ]] ; then
      isatId=1
      nchan=20
    elif [[ ${satId} == 'metopA' ]] ; then
      isatId=2
      nchan=20
    elif [[ ${satId} == 'f16'    ]] ; then
      isatId=3
      nchan=24
    elif [[ ${satId} == 'n19'    ]] ; then
      isatId=4
      nchan=20
    elif [[ ${satId} == 'f18'    ]] ; then
      isatId=5
      nchan=24
    elif [[ ${satId} == 'npp'    ]] ; then
      isatId=6
      nchan=22
    elif [[ ${satId} == 'aqua'   ]] ; then
      isatId=7
      nchan=12
    elif [[ ${satId} == 'fy3ri'  ]] ; then
      isatId=8
      nchan=10      
    elif [[ ${satId} == 'trmm'  ]] ; then
      isatId=9
      nchan=9      
    elif [[ ${satId} == 'gpm'  ]] ; then
      isatId=10
      nchan=13   
    elif [[ ${satId} == 'f17'    ]] ; then
      isatId=18
      nchan=24 
    fi
    
    # lwp as indicator file. ex: GRID_n18_gfs_asym_20100814_lwp_ds.dat
    local fileAsym=${gridDir}/GRID_${satId}_${nwptxt}_asym_${yyyymmdd}_lwp_ds.dat
    if [[ ! -e ${fileAsym} ]] ; then
      echo "Bias/Asymmetry computing..."
      set "satId='${satId}'"		      	\
          "yyyymmdd='${yyyymmdd}'"	      	\
          "gridfactor=${gridfactor}"	      	\
    	  "gridPath='${gridDir}/'"	      	\
          "latmin=${latmin}"  		      	\
          "latmax=${latmax}"			\
          "lonmin=${lonmin}" 		      	\
          "lonmax=${lonmax}"  		      	\
          "NLAY=100"			      	\
          "NCHAN=${nchan}"		      	\
          "nwpData=${nwpId}"		      	\
	  "isatId=${isatId}"
      CreatNamList ${grid_cntrl}_${nwptxt}_gridBias '&gridBiasNameList' "$@"
      ${binDir}/gridBias < ${grid_cntrl}_${nwptxt}_gridBias
      checkStatus $? "qcRadioGeo: gridBias: ${nwptxt}"
    fi

    
    # EDR and DEP plot based on gridded data
    if [[ ${plot_edrdep} -eq 1 ]] ; then
      echo "MIRS (EDR/DEP) plotting..."
      echo ${satId}       >  ${fig_cntrl}
      echo ${gridfactor}  >> ${fig_cntrl}
      echo ${gridDir}/    >> ${fig_cntrl}
      echo ${figDir}/     >> ${fig_cntrl}
      echo ${ymd}         >> ${fig_cntrl}
      echo ${processMode} >> ${fig_cntrl}
      echo ${version}     >> ${fig_cntrl}
      echo ${latmin}      >> ${fig_cntrl}
      echo ${latmax}      >> ${fig_cntrl}
      echo ${lonmin}      >> ${fig_cntrl}
      echo ${lonmax}      >> ${fig_cntrl}
      echo ${region}      >> ${fig_cntrl}
      cd ${gridSrc}
      echo "!QUIET=1"                          >  ${gridSrc}/batchGridMirs.pro
      echo ".r gridMirs.pro"                   >> ${gridSrc}/batchGridMirs.pro
      echo "GridMirs, namelist='${fig_cntrl}'" >> ${gridSrc}/batchGridMirs.pro
      echo "exit"                              >> ${gridSrc}/batchGridMirs.pro
      ${IDL} ${gridSrc}/batchGridMirs.pro
      checkStatus $? "qcRadioGeo: gridMirs.pro"    
      rm -f ${gridSrc}/batchGridMirs.pro
    fi
    
    # NWP plot based on gridded data
    if [[ ${plot_nwp} -eq 1 ]] ; then
      echo "NWP plotting..."
      echo ${satId}      >  ${fig_cntrl}_${nwptxt}
      echo ${gridfactor} >> ${fig_cntrl}_${nwptxt}
      echo ${gridDir}/   >> ${fig_cntrl}_${nwptxt}
      echo ${figDir}/    >> ${fig_cntrl}_${nwptxt}
      echo ${ymd}        >> ${fig_cntrl}_${nwptxt}
      echo ${version}    >> ${fig_cntrl}_${nwptxt}
      echo dummy         >> ${fig_cntrl}_${nwptxt}
      echo dummy         >> ${fig_cntrl}_${nwptxt}
      echo dummy         >> ${fig_cntrl}_${nwptxt}
      echo ${latmin}     >> ${fig_cntrl}_${nwptxt}
      echo ${latmax}     >> ${fig_cntrl}_${nwptxt}
      echo ${lonmin}     >> ${fig_cntrl}_${nwptxt}
      echo ${lonmax}     >> ${fig_cntrl}_${nwptxt}
      echo ${nwpId}      >> ${fig_cntrl}_${nwptxt}
      cd ${gridSrc}
      echo "!QUIET=1"                                   >  ${gridSrc}/batchGridNwp.pro
      echo ".r gridNwp.pro"                             >> ${gridSrc}/batchGridNwp.pro
      echo "GridNwp, nameList='${fig_cntrl}_${nwptxt}'" >> ${gridSrc}/batchGridNwp.pro
      echo "exit"                                       >> ${gridSrc}/batchGridNwp.pro
      ${IDL} ${gridSrc}/batchGridNwp.pro
      checkStatus $? "geoRadio: gridNwp.pro"
      rm -f ${gridSrc}/batchGridNwp.pro
    fi
    
    # NWP NWP Bias Map/Asymmetry generation        
    if [[ ${plot_bias} -eq 1 ]] ; then
      echo "Bias/Asymmetry plotting..."
      echo ${satId}      >  ${fig_cntrl}_${nwptxt}
      echo ${gridfactor} >> ${fig_cntrl}_${nwptxt}
      echo ${gridDir}/   >> ${fig_cntrl}_${nwptxt}
      echo ${figDir}/    >> ${fig_cntrl}_${nwptxt}
      echo ${ymd}        >> ${fig_cntrl}_${nwptxt}
      echo ${version}    >> ${fig_cntrl}_${nwptxt}
      echo dummy         >> ${fig_cntrl}_${nwptxt}
      echo dummy         >> ${fig_cntrl}_${nwptxt}
      echo dummy         >> ${fig_cntrl}_${nwptxt}
      echo ${latmin}     >> ${fig_cntrl}_${nwptxt}
      echo ${latmax}     >> ${fig_cntrl}_${nwptxt}
      echo ${lonmin}     >> ${fig_cntrl}_${nwptxt}
      echo ${lonmax}     >> ${fig_cntrl}_${nwptxt}
      echo ${nwpId}      >> ${fig_cntrl}_${nwptxt}

      cd ${gridSrc}
      echo "!QUIET=1"                                   >  ${gridSrc}/batchGridNwp.pro
      echo ".r gridNwp.pro"                             >> ${gridSrc}/batchGridNwp.pro
      echo "GridNwp, nameList='${fig_cntrl}_${nwptxt}'" >> ${gridSrc}/batchGridNwp.pro
      echo "exit"                                       >> ${gridSrc}/batchGridNwp.pro
      ${IDL} ${gridSrc}/batchGridNwp.pro
      checkStatus $? "geoRadio: gridNwp.pro"
      rm -f ${gridSrc}/batchGridNwp.pro

      cd ${gridSrc}
      echo "!QUIET=1"                                       >  ${gridSrc}/batchNwpGridAsym.pro
      echo ".r gridNwpAsym.pro"                             >> ${gridSrc}/batchNwpGridAsym.pro
      echo "GridNwpAsym, nameList='${fig_cntrl}_${nwptxt}'" >> ${gridSrc}/batchNwpGridAsym.pro
      echo "exit"                                           >> ${gridSrc}/batchNwpGridAsym.pro
      ${IDL} ${gridSrc}/batchNwpGridAsym.pro
      checkStatus $? "geoRadio: gridNwp.pro"
      rm -f ${gridSrc}/batchNwpGridAsym.pro
    fi
    
    ## start P2P branch
    if [[ ${plot_p2p} -eq 1 ]] ; then
      
      local fileP2Pwv=${gridDir}/P2P_${satId}_${yyyymmdd}_wv_ds.dat
      # example: P2P_n18_20100817_wv_ds.dat 
      
      # do MIRS EDR and DEP P2P if necessary
      if [[ ! -e ${fileP2Pwv} ]] ; then
         
	 # P2P EDR
	 echo "EDR P2P..."
	 set "filesList='${edrList}'"	     	\
	     "satId='${satId}'"		     	\
	     "yyyymmdd='${yyyymmdd}'" 	     	\
	     "p2pPath='${gridDir}/'" 	     	\
	     "latmin=${latmin}"		     	\
	     "latmax=${latmax}"		     	\
	     "lonmin=${lonmin}" 		\
	     "lonmax=${lonmax}"		     	\
	     "isMirs=0"			     	\
	     "processMode=${processMode}"       \
	     "fmType=${fmType}"                 \
	     "prodStr='${edrP2PStr}'"
	 CreatNamList ${grid_cntrl}_p2pEdr '&p2pEdrNameList' "$@"
	 ${binDir}/p2pEdr < ${grid_cntrl}_p2pEdr
	 checkStatus $? "qcRadioGeo:p2pEdr"
         
	 # P2P DEP 
	 echo "DEP P2P..."
	 set "filesList='${depList}'" 		\
    	     "satId='${satId}'" 		\
	     "yyyymmdd='${yyyymmdd}'" 		\
             "p2pPath='${gridDir}/'"  		\
       	     "latmin=${latmin}"		     	\
       	     "latmax=${latmax}"		     	\
       	     "lonmin=${lonmin}"  		\
       	     "lonmax=${lonmax}"		     	\
	     "processMode=${processMode}"       \
	     "fmType=${fmType}"                 \
	     "prodStr='${depP2PStr}'"
	 CreatNamList ${grid_cntrl}_p2pDep '&p2pDepNameList' "$@"
	 ${binDir}/p2pDep < ${grid_cntrl}_p2pDep
	 checkStatus $? "qcRadioGeo:p2pDep"

      fi
      
      # P2P NWP
      echo "NWP P2P..."
      set "filesList='${nwpList}_${nwptxt}'"	\
          "satId='${satId}'"			\
          "yyyymmdd='${yyyymmdd}'"	     	\
    	  "p2pPath='${gridDir}/'"	      	\
          "latmin=${latmin}"  		      	\
          "latmax=${latmax}"			\
          "lonmin=${lonmin}" 		      	\
          "lonmax=${lonmax}"  		      	\
          "isMirs=${nwpId}"		      	\
          "processMode=${processMode}"          \
	  "fmType=${fmType}"                    \
	  "prodStr='${nwpP2PStr}'"
      CreatNamList ${grid_cntrl}_${nwptxt}_p2pEdr '&p2pEdrNameList' "$@"
      ${binDir}/p2pEdr < ${grid_cntrl}_${nwptxt}_p2pEdr
      checkStatus $? "qcRadioGeo: p2pEdr ${NWPTXT}"

      # P2P Rad(FWD)
      echo "Rad P2P..."
      set "filesList='${fwdList}_${nwptxt}'"	\
          "satId='${satId}'"		      	\
          "yyyymmdd='${yyyymmdd}'"	      	\
    	  "p2pPath='${gridDir}/'"	      	\
          "latmin=${latmin}"  		      	\
          "latmax=${latmax}"			\
          "lonmin=${lonmin}" 		      	\
          "lonmax=${lonmax}"  		      	\
          "isMirs=${nwpId}"
      CreatNamList ${grid_cntrl}_${nwptxt}_p2pRad '&p2pRadNameList' "$@"
      ${binDir}/p2pRad < ${grid_cntrl}_${nwptxt}_p2pRad
      checkStatus $? "qcRadioGeo: p2pRad ${NWPTXT}"

      # P2P IDL of MIRS vs NWP
      echo "P2P Plotting of MIRS vs NWP..."
      echo ${satId}      >  ${fig_cntrl}_${nwptxt}_p2p
      echo ${gridfactor} >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${gridDir}/   >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${figDir}/    >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${ymd}        >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${version}    >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${edrList}    >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${depList}    >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${biasPath}   >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${latmin}     >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${latmax}     >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${lonmin}     >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${lonmax}     >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${nwpId}      >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${nedtExt}    >> ${fig_cntrl}_${nwptxt}_p2p
      echo ${fmType}     >> ${fig_cntrl}_${nwptxt}_p2p
      cd ${gridSrc}
      echo "!QUIET=1"                                   	 >  ${gridSrc}/batchp2p_mirs_nwp.pro
      echo ".r p2p_mirs_nwp.pro"                           	 >> ${gridSrc}/batchp2p_mirs_nwp.pro
      echo "P2P_MIRS_NWP, nameList='${fig_cntrl}_${nwptxt}_p2p'" >> ${gridSrc}/batchp2p_mirs_nwp.pro
      echo "exit"                                                >> ${gridSrc}/batchp2p_mirs_nwp.pro
      ${IDL} ${gridSrc}/batchp2p_mirs_nwp.pro
      checkStatus $? "qcRadioGeo: (${NWPTXT}): p2p_mirs_nwp.pro"
      rm -f ${gridSrc}/batchp2p_mirs_nwp.pro
    
    fi
    ## end P2P branch
    
    echo "End of step qcRadioGeo"
}


#===============================================================
#
# To plot emissivity spectrum based on MIRS EDR data.
# 
# 02/09/2011         Wanchun Chen 
#
#===============================================================
function emspectrum() {
  
  if [[ $# -ne 11 ]] ; then
      op_msg "Error: function emspectrum: check number of arguments: $#"
      exit 1
  fi
  
  local satId=$1
  local ymd=$2
  local dirImg=$3
  local dirControl=$4
  local figControl=$5
  local edrList=$6
  local dirSrc=$7
  local version=$8
  local IDL=$9
  local dirGrid=${10}
  local dirBin=${11}
  
  # fortran to do compute
  echo "emspectrum() fortran part..."
  local yyyymmdd=`echo $ymd | cut -c1-4,6-7,9-10`
  local namelist1=${figControl}_emspectrum_fortran
  set "filesList='${edrList}'"    \
      "satId='${satId}'"	  \
      "yyyymmdd='${yyyymmdd}'"    \
      "gridPath='${dirGrid}/'"
  CreatNamList ${namelist1} '&emspectrumNameList' "$@"
  ${dirBin}/emspectrum < ${namelist1}
  checkStatus $? "emspectrum: emspectrum"
  
  
  # idl step to plot
  echo "emspectrum() IDL part..."
  local namelist2=${figControl}_emspectrum_idl
  
  echo ${satId}       > ${namelist2}
  echo ${ymd}        >> ${namelist2}
  echo "${dirGrid}/" >> ${namelist2}
  echo "${dirImg}/"  >> ${namelist2}
  echo ${version}    >> ${namelist2}
  
  cd ${dirSrc}
  echo "!QUIET=1" 			     > ${dirControl}/batchEM_${satId}
  echo ".r emspectrum.pro"		    >> ${dirControl}/batchEM_${satId}
  echo "EMSPECTRUM,namelist='${namelist2}'" >> ${dirControl}/batchEM_${satId}
  echo "exit"				    >> ${dirControl}/batchEM_${satId}

  ${IDL} ${dirControl}/batchEM_${satId}
  checkStatus $? "emspectrum $satId $ymd"
  rm -f ${dirControl}/batchEM_${satId}
  echo "End of step emspectrum"
}


#===============================================================
# Name:		    gridGen_parallel
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Level III EDR/DEP data generation ( grid & p2p ) 
#       Use multiple CPUs to speed up processing.
#
# Input Variables:
# 	- processingMode: 0-orbit, 1-daily
# 	- edrDir: EDR data directory
# 	- depDir: DEP data directory
# 	- OrbitInfo: part of orbit file name to identify the orbits
# 	- edrList: EDR file list
# 	- depList: DEP file list
# 	- extResol: Extension resolution (CR/LR/HR/UAS/LAS/EVN/IMG)
# 	- satId: satellite ID(n18,metopA,f16,etc)
# 	- fileExt: File extension ( yyyy-mm-dd, etc)
# 	- gridSrc: Grid generation source code directory
# 	- gridDir: Generated gridded data directory
#	- makeOrNot: on-the-fly make or not if executable not there
#	- binDir: Executables location
# 	- grid_cntrl: The control file to generate gridded data
# 	- gridFactor: 4->0.25 degree, 2->0.5 degree box, 1->1.0 box, etc
# 	- latmin: min latitude
# 	- latmax: max latitude
# 	- lonmin: min longitude
# 	- lonmax: max longitude
# 	- fmType: TRMM(-1:CR,0-LR,1-HR), POES(,0-LR,1-HR),DMSP(0-UAS,1-LAS,2-ENV,3-IMG)
# 	- edrGridStr: comma separated edr product ids to gridEdr.f90 ( em/temp/tskin/wv/tbc/tbu/tbl/tbf/angle/sfc/psfc/clwp/... )
# 	- edrP2PStr: comma separated edr product ids to p2pEdr.f90 ( em/temp/tskin/... )
# 	- depGridStr: comma separated dep product ids to gridDep.f90 ( rr/clw/tpw/rwp/... )
# 	- depP2PStr: comma separated dep product ids to p2pDep.f90 ( rr/clw/tpw/rwp/... )
#
# History:
#      	- 02/16/2011     Wanchun Chen
#
#===============================================================
function gridGen_parallel() {
    if [[ $# -ne 24 ]]
        then
        op_msg "Error: function gridGen_parallel: check number of arguments: $#"
        exit 1
    fi
    local processingMode=${1}
    local edrDir=$2
    local depDir=$3
    local OrbitInfo=$4
    local edrList=$5
    local depList=$6
    local extResol=$7
    local satId=$8
    local fileExt=$9
    local gridSrc=${10}
    local gridDir=${11}
    local makeOrNot=${12}
    local binDir=${13}
    local grid_cntrl=${14}
    local gridFactor=${15}
    local latmin=${16}
    local latmax=${17}
    local lonmin=${18}
    local lonmax=${19}
    local fmType=${20}
    local edrGridStr=${21}
    local edrP2PStr=${22}
    local depGridStr=${23}
    local depP2PStr=${24}
    
    echo "=============== GRID & P2P DATA GENERATION STEP ======================="
    ConstructList ${processingMode} ${edrDir} ${OrbitInfo} ${edrList} "*" "*"${extResol}".*"
    ConstructList ${processingMode} ${depDir} ${OrbitInfo} ${depList} "*" "*"${extResol}".*"
    
    if [[ ${processingMode} -eq 1 ]] ; then
        local yyyymmdd=`echo $fileExt | cut -c1-4,6-7,9-10`
    else
        local yyyymmdd=${OrbitInfo}
    fi
    
    # EDR grid
    prods=(`echo $edrGridStr | tr ',' ' '`)
    for prod in ${prods[@]}; do
      set "filesList='${edrList}'"	     	\
	  "satId='${satId}'"		     	\
	  "yyyymmdd='${yyyymmdd}'" 	     	\
	  "gridfactor=${gridFactor}"	     	\
	  "gridPath='${gridDir}/'" 	     	\
	  "latmin=${latmin}"		     	\
	  "latmax=${latmax}"		     	\
	  "lonmin=${lonmin}"  		     	\
	  "lonmax=${lonmax}"		     	\
	  "isMirs=0"			     	\
	  "processMode=${processingMode}"       \
	  "fmType=${fmType}"                    \
	  "prodStr='${prod}'"
      CreatNamList ${grid_cntrl}_gridEdr_${prod} '&gridEdrNameList' "$@"
      #DoMake ${gridSrc} ${makeOrNot}
      ${binDir}/gridEdr < ${grid_cntrl}_gridEdr_${prod} &
      #checkStatus $? "gridGen:gridEdr_${prod}"
    done
    wait
    
    # EDR p2p
    prods=(`echo $edrP2PStr | tr ',' ' '`)
    for prod in ${prods[@]}; do
      set "filesList='${edrList}'"	     	\
	  "satId='${satId}'"		     	\
	  "yyyymmdd='${yyyymmdd}'" 	     	\
	  "p2pPath='${gridDir}/'" 	     	\
	  "latmin=${latmin}"		     	\
	  "latmax=${latmax}"		     	\
	  "lonmin=${lonmin}"  		     	\
	  "lonmax=${lonmax}"		     	\
	  "isMirs=0"			     	\
	  "processMode=${processingMode}"       \
	  "fmType=${fmType}"                    \
	  "prodStr='${prod}'"
      CreatNamList ${grid_cntrl}_p2pEdr_${prod} '&p2pEdrNameList' "$@"
      #DoMake ${gridSrc} ${makeOrNot}
      ${binDir}/p2pEdr < ${grid_cntrl}_p2pEdr_${prod} &
      #checkStatus $? "gridGen:p2pEdr_${prod}"
    done
    wait

    #DEP grid
    prods=(`echo $depGridStr | tr ',' ' '`)
    for prod in ${prods[@]}; do
      set "filesList='${depList}'" 		\
    	  "satId='${satId}'" 			\
	  "yyyymmdd='${yyyymmdd}'" 		\
	  "gridfactor=${gridFactor}"		\
          "gridPath='${gridDir}/'"  		\
       	  "latmin=${latmin}"		     	\
       	  "latmax=${latmax}"		     	\
       	  "lonmin=${lonmin}"  		     	\
       	  "lonmax=${lonmax}"		     	\
	  "processMode=${processingMode}"       \
	  "fmType=${fmType}"                    \
	  "prodStr='${prod}'"
      CreatNamList ${grid_cntrl}_gridDep_${prod} '&gridDepNameList' "$@"
      #DoMake ${gridSrc} ${makeOrNot}
      ${binDir}/gridDep < ${grid_cntrl}_gridDep_${prod} &
      #checkStatus $? "gridGen:gridDep_${prod}"
    done
    wait

    #DEP p2p
    prods=(`echo $depP2PStr | tr ',' ' '`)
    for prod in ${prods[@]}; do
      set "filesList='${depList}'" 		\
    	  "satId='${satId}'" 			\
	  "yyyymmdd='${yyyymmdd}'" 		\
          "p2pPath='${gridDir}/'"  		\
       	  "latmin=${latmin}"		     	\
       	  "latmax=${latmax}"		     	\
       	  "lonmin=${lonmin}"  		     	\
       	  "lonmax=${lonmax}"		     	\
	  "processMode=${processingMode}"       \
	  "fmType=${fmType}"                    \
	  "prodStr='${prod}'"
      CreatNamList ${grid_cntrl}_p2pDep_${prod} '&p2pDepNameList' "$@"
      #DoMake ${gridSrc} ${makeOrNot}
      ${binDir}/p2pDep < ${grid_cntrl}_p2pDep_${prod} &
      #checkStatus $? "gridGen:p2pDep_${prod}"
    done
    wait
    
    echo "End of step gridGen_parallel"
}


#===============================================================
# Name:		    figsGen_parallel
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Gridded data figs generation, use multiple CPUs
#
#
# Input Variables:
# 	- satId: satellite ID(n18,metopA,f16,etc)
#	- gridfactor: grid factor(1,2,4,etc)
# 	- gridDir: EDR gridded data directory
# 	- figsGener: Image generation source code directory
# 	- figsDir: Generated image directory
# 	- IDL: IDL path
# 	- fileExt: File extension ( yyyy-mm-dd or part of orbit file name) 
# 	- figsGener_cntrl: The control file to generate images.
# 	- processMode: daily mode (1) or orbit mode(0)
# 	- controlDataPath: control data path
# 	- version: MIRS source revision number using svn
# 	- latmin: min latitude
# 	- latmax: max latitude
# 	- lonmin: min longitude
# 	- lonmax: max longitude
#
# History:
#      	- 02/16/2011     Wanchun Chen
#
#===============================================================
function figsGen_parallel() {
    if [[ $# -ne 15 ]] ; then
        op_msg "Error: function figsGen_parallel: check number of arguments: $#"
        exit 1
    fi
    local satId=$1
    local gridfactor=$2
    local gridDir=$3
    local figsGener=$4
    local figsDir=$5
    local IDL=$6
    local fileExt=$7
    local figsGener_cntrl=$8
    local processMode=$9
    local controlDataPath=${10}
    local version=${11}
    local latmin=${12}
    local latmax=${13}
    local lonmin=${14}
    local lonmax=${15}

    echo "=============== RUNNING figsGen_parallel() ======================="

    cd ${figsGener}
    
    local iregions=(0 1) 
    local regions=(glb us)
    local nregion=${#regions[@]}

    for (( i=0; i<${nregion}; i++ )) ; do
      region=${regions[$i]}
      echo ${satId}             >  ${figsGener_cntrl}_${region}
      echo ${gridfactor}        >> ${figsGener_cntrl}_${region}
      echo ${gridDir}/          >> ${figsGener_cntrl}_${region}
      echo ${figsDir}/          >> ${figsGener_cntrl}_${region}
      echo ${fileExt}           >> ${figsGener_cntrl}_${region}
      echo ${processMode}       >> ${figsGener_cntrl}_${region}
      echo ${version}           >> ${figsGener_cntrl}_${region}
      echo ${latmin}            >> ${figsGener_cntrl}_${region}
      echo ${latmax}            >> ${figsGener_cntrl}_${region}
      echo ${lonmin}            >> ${figsGener_cntrl}_${region}
      echo ${lonmax}            >> ${figsGener_cntrl}_${region}
      echo ${iregions[$i]}      >> ${figsGener_cntrl}_${region}
      
      # horizontal plot 
      echo "!QUIET=1"                                          >  ${controlDataPath}/batchGridMirs_${region}.pro
      echo ".r gridMirs.pro"                                   >> ${controlDataPath}/batchGridMirs_${region}.pro
      echo "GridMirs, namelist='${figsGener_cntrl}_${region}'" >> ${controlDataPath}/batchGridMirs_${region}.pro
      echo "exit"                                              >> ${controlDataPath}/batchGridMirs_${region}.pro
      ${IDL} ${controlDataPath}/batchGridMirs_${region}.pro &
      
      # vertical cross section ( has global/mexico gulf/china east sea in IDL code )
      if [[ $i -eq 0 ]] ; then
	echo "!QUIET=1"					          >  ${controlDataPath}/batchVertical_${region}.pro
	echo ".r vertical.pro"				          >> ${controlDataPath}/batchVertical_${region}.pro
	echo "Vertical, nameList='${figsGener_cntrl}_${region}'"  >> ${controlDataPath}/batchVertical_${region}.pro
	echo "exit" 					          >> ${controlDataPath}/batchVertical_${region}.pro
	${IDL} ${controlDataPath}/batchVertical_${region}.pro &
      fi
    
    done
    
    wait
    
    for (( i=0; i<${nregion}; i++ )) ; do
      region=${regions[$i]}
      rm -f ${controlDataPath}/batchGridMirs_${region}.pro
      if [[ $i -eq 0 ]] ; then
        rm -f ${controlDataPath}/batchVertical_${region}.pro
      fi	
    done
    
    echo "End of step figsGen_parallel"
}


#===============================================================
# Name:		    biasFigsGen_parallel
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Level III bias data generation to use multiple CPUs;
#	First, it will grid/p2p NWP and FWD files;
#	Second, it will compute NWP and FWD Bias;
#	Third, it will compute Geophysical and Radiometric Bias Asymmetry;
#	Fourth, it will call IDL program to do plotting.
#
#
# Input Variables:
# 	- edr4BiasDir: EDR data directory
# 	- nwp4BiasDir: NWP data directory
# 	- fwd4BiasDir: FWD data directory 
# 	- edr4BiasList: EDR file list
# 	- nwp4BiasList: NWP file list
# 	- fwd4BiasList: FWD file list
# 	- processingMode: 0:Orbit mode,  1:Daily mode
# 	- OrbitInfo: part of orbit file names
# 	- extResol: Extension resolution (CR/LR/HR/UAS/LAS/EVN/IMG)
# 	- satId: string satellite id (n18/n19/metopA/f16/f18/trmm/npp)
# 	- fileExt:  File extension ( yyyy-mm-dd, etc)
# 	- gridSrc: Grid generation source code directory
# 	- grid4BiasDir: Generated gridded data directory
# 	- makeOrNot: on-the-fly make or not if executable not there
# 	- binDir:  Executables location
# 	- gridNwp_cntrl: control file for NWP
# 	- gridFwd_cntrl: control file for FWD
# 	- gridBias_cntrl: control file for bias/asymmetry
# 	- gridfactor: 4->0.25 degree, 2->0.5 degree box, 1->1.0 box, etc
# 	- figsGener: image generation source code location
# 	- figsDir:  image location
# 	- IDL: IDL executable
# 	- fileExt: File extension ( yyyy-mm-dd, etc)
# 	- figsGener_cntrl: image generation control file
# 	- controlDataPath: control data path
# 	- dep4BiasDir:  DEP data directory
# 	- dep4BiasList: DEP file list
# 	- version: mirs revision svn number
# 	- biasPath: semi-static bias correction files location
# 	- latmin: min latitude
# 	- latmax: max latitude
# 	- lonmin: min longitude
# 	- lonmax: max longitude
# 	- nwpDataSrc: 1-gdas,2-ecmwf,3-gfs
# 	- fmType: TRMM(-1:CR,0-LR,1-HR), POES(,0-LR,1-HR),DMSP(0-UAS,1-LAS,2-ENV,3-IMG)
# 	- nwpGridStr: comma separated nwp gridded product ids to gridEdr.f90
# 	- nwpP2PStr: comma separated nwp P2P product ids to p2pEdr.f90
#
#
# History:
#      	- 02/16/2011     Wanchun Chen
#
#===============================================================
function biasFigsGen_parallel() {
    if [[ $# -ne 38 ]]
        then
        op_msg "Error: function biasFigsGen_parallel: check number of arguments: $#"
        exit 1
    fi
    local edr4BiasDir=$1 ; shift
    local nwp4BiasDir=$1 ; shift
    local fwd4BiasDir=$1 ; shift
    local edr4BiasList=$1 ; shift
    local nwp4BiasList=$1 ; shift
    local fwd4BiasList=$1 ; shift
    local processingMode=$1 ; shift
    local OrbitInfo=$1 ; shift
    local extResol=$1 ; shift
    local satId=$1 ; shift
    local fileExt=$1 ; shift
    local gridSrc=$1 ; shift
    local grid4BiasDir=$1 ; shift
    local makeOrNot=$1 ; shift
    local binDir=$1 ; shift
    local gridNwp_cntrl=$1 ; shift
    local gridFwd_cntrl=$1 ; shift
    local gridBias_cntrl=$1 ; shift
    local gridfactor=$1 ; shift
    local figsGener=$1 ; shift
    local figsDir=$1 ; shift
    local IDL=$1 ; shift
    local fileExt=$1 ; shift
    local figsGener_cntrl=$1 ; shift
    local controlDataPath=$1 ; shift
    local dep4BiasDir=$1 ; shift
    local dep4BiasList=$1 ; shift
    local version=$1 ; shift
    local biasPath=$1 ; shift
    local latmin=$1 ; shift
    local latmax=$1 ; shift
    local lonmin=$1 ; shift
    local lonmax=$1 ; shift
    local nwpDataSrc=$1 ; shift
    local fmType=$1 ; shift
    local nwpGridStr=$1 ; shift
    local nwpP2PStr=$1 ; shift
    local nedtExt=$1
    
    local nwpstr='gdas'
    local NWPSTR='GDAS'
    if [[ ${nwpDataSrc} -eq 2 ]] ; then
      nwpstr='ecmw'
      NWPSTR='ECMW'
    elif [[ ${nwpDataSrc} -eq 3 ]] ; then
      nwpstr='gfs'
      NWPSTR='GFS'
    fi

    echo "============ RUNNING BIAS DATA/IMG (${NWPSTR}) GENERATION STEP ==================="
    ConstructList ${processingMode} ${edr4BiasDir} ${OrbitInfo} ${edr4BiasList} "*" "*"${extResol}".*"
    ConstructList ${processingMode} ${dep4BiasDir} ${OrbitInfo} ${dep4BiasList} "*" "*"${extResol}".*"

    local yyyymmdd=`echo $fileExt | cut -c1-4,6-7,9-10`
    
    local nchan=20
    local isatId=1
    
    if [[ ${satId}   == 'n18'    ]] ; then
      isatId=1
      nchan=20
    elif [[ ${satId} == 'metopA' ]] ; then
      isatId=2
      nchan=20
    elif [[ ${satId} == 'metopB' ]] ; then
      isatId=14
      nchan=20
    elif [[ ${satId} == 'f16'    ]] ; then
      isatId=3
      nchan=24
    elif [[ ${satId} == 'n19'    ]] ; then
      isatId=4
      nchan=20
    elif [[ ${satId} == 'f17'    ]] ; then
      isatId=18
      nchan=24
    elif [[ ${satId} == 'f18'    ]] ; then
      isatId=5
      nchan=24
    elif [[ ${satId} == 'npp'    ]] ; then
      isatId=6
      nchan=22
    elif [[ ${satId} == 'aqua'   ]] ; then
      isatId=7
      nchan=12
    elif [[ ${satId} == 'gcomw1' ]] ; then
      isatId=15
      nchan=14
    elif [[ ${satId} == 'fy3ri'  ]] ; then
      isatId=8
      nchan=10      
    elif [[ ${satId} == 'trmm'   ]] ; then
      isatId=9
      nchan=9      
    elif [[ ${satId} == 'gpm'    ]] ; then
      isatId=10
      nchan=13    
    fi

    ls -1 ${nwp4BiasDir}/NWP_${NWPSTR}* > ${nwp4BiasList}_${nwpstr}
    ls -1 ${fwd4BiasDir}/FWD_${NWPSTR}* > ${fwd4BiasList}_${nwpstr}
    ls -1 ${dep4BiasDir}/DEP* > ${dep4BiasList}
	
    # NWP grid
    local prods=(`echo $nwpGridStr | tr ',' ' '`)
    for prod in ${prods[@]}; do
      set "filesList='${nwp4BiasList}_${nwpstr}'" \
          "satId='${satId}'"			  \
          "yyyymmdd='${yyyymmdd}'"	     	  \
          "gridfactor=${gridfactor}"	      	  \
    	  "gridPath='${grid4BiasDir}/'"	      	  \
          "latmin=${latmin}"  		      	  \
          "latmax=${latmax}"			  \
          "lonmin=${lonmin}" 		      	  \
          "lonmax=${lonmax}"  		      	  \
          "isMirs=${nwpDataSrc}"		  \
          "processMode=${processingMode}"	  \
	  "fmType=${fmType}"                      \
	  "prodStr='${prod}'"
      CreatNamList ${gridNwp_cntrl}_${nwpstr}_gridEdr_${prod} '&gridEdrNameList' "$@"
      #DoMake ${gridSrc} ${makeOrNot}
      ${binDir}/gridEdr < ${gridNwp_cntrl}_${nwpstr}_gridEdr_${prod} &
      #checkStatus $? "biasFigsGen: NWP ${NWPSTR} gridEdr_${prod}"
    done
    wait
    
    # NWP p2p
    local prods=(`echo $nwpP2PStr | tr ',' ' '`)
    for prod in ${prods[@]}; do
      set "filesList='${nwp4BiasList}_${nwpstr}'" \
          "satId='${satId}'"			  \
          "yyyymmdd='${yyyymmdd}'"	     	  \
    	  "p2pPath='${grid4BiasDir}/'"	      	  \
          "latmin=${latmin}"  		      	  \
          "latmax=${latmax}"			  \
          "lonmin=${lonmin}" 		      	  \
          "lonmax=${lonmax}"  		      	  \
          "isMirs=${nwpDataSrc}"		  \
          "processMode=${processingMode}"         \
	  "fmType=${fmType}"                      \
	  "prodStr='${prod}'"
      CreatNamList ${gridNwp_cntrl}_${nwpstr}_p2pEdr_${prod} '&p2pEdrNameList' "$@"
      #DoMake ${gridSrc} ${makeOrNot}
      ${binDir}/p2pEdr < ${gridNwp_cntrl}_${nwpstr}_p2pEdr_${prod} &
      #checkStatus $? "biasFigsGen: NWP ${NWPSTR} p2pEdr_${prod}"
    done
    wait
    
    # Rad(FWD) grid
    set "filesList='${fwd4BiasList}_${nwpstr}'"	\
        "satId='${satId}'"		      	\
        "yyyymmdd='${yyyymmdd}'"	      	\
        "gridfactor=${gridfactor}"	      	\
    	"gridPath='${grid4BiasDir}/'"	      	\
        "latmin=${latmin}"  		      	\
        "latmax=${latmax}"			\
        "lonmin=${lonmin}" 		      	\
        "lonmax=${lonmax}"  		      	\
        "isMirs=${nwpDataSrc}"                  \
	"fmType=${fmType}"
    CreatNamList ${gridFwd_cntrl}_${nwpstr}_gridRad '&gridRadNameList' "$@"
    #DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/gridRad < ${gridFwd_cntrl}_${nwpstr}_gridRad
    checkStatus $? "biasFigsGen: Rad ${NWPSTR} gridRad"

    # Rad(FWD) p2p
    set "filesList='${fwd4BiasList}_${nwpstr}'"	\
        "satId='${satId}'"		      	\
        "yyyymmdd='${yyyymmdd}'"	      	\
    	"p2pPath='${grid4BiasDir}/'"	      	\
        "latmin=${latmin}"  		      	\
        "latmax=${latmax}"			\
        "lonmin=${lonmin}" 		      	\
        "lonmax=${lonmax}"  		      	\
        "isMirs=${nwpDataSrc}"
    CreatNamList ${gridFwd_cntrl}_${nwpstr}_p2pRad '&p2pRadNameList' "$@"
    #DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/p2pRad < ${gridFwd_cntrl}_${nwpstr}_p2pRad
    checkStatus $? "biasFigsGen: Rad ${NWPSTR} p2pRad"

    # Compute bias and bias asymmetry
    echo "gridBias..."
    set "satId='${satId}'"		      	\
        "yyyymmdd='${yyyymmdd}'"	      	\
        "gridfactor=${gridfactor}"	      	\
    	"gridPath='${grid4BiasDir}/'"	      	\
        "latmin=${latmin}"  		      	\
        "latmax=${latmax}"			\
        "lonmin=${lonmin}" 		      	\
        "lonmax=${lonmax}"  		      	\
        "NLAY=100"			      	\
        "NCHAN=${nchan}"		      	\
        "nwpData=${nwpDataSrc}"		      	\
	"isatId=${isatId}"
    CreatNamList ${gridBias_cntrl}_${nwpstr} '&GridBiasNameList' "$@"
    #DoMake ${gridSrc} ${makeOrNot}
    ${binDir}/gridBias < ${gridBias_cntrl}_${nwpstr}
    checkStatus $? "biasFigsGen: gridBias: ${nwpstr}"

    echo ${satId}             	   >  ${figsGener_cntrl}_${nwpstr}_grid
    echo ${gridfactor}        	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${grid4BiasDir}/     	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${figsDir}/          	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${fileExt}           	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${version}           	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${edr4BiasList}      	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${dep4BiasList}      	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${biasPath}          	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${latmin}            	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${latmax}            	   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${lonmin}                 >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${lonmax}                 >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${nwpDataSrc}		   >> ${figsGener_cntrl}_${nwpstr}_grid
    echo ${nedtExt}		   >> ${figsGener_cntrl}_${nwpstr}_grid		
    echo ${fmType}		   >> ${figsGener_cntrl}_${nwpstr}_grid		

    echo ${satId}             	   >  ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${gridfactor}        	   >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${grid4BiasDir}/     	   >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${figsDir}/          	   >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${fileExt}           	   >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${processingMode}         >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${version}           	   >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${latmin}            	   >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${latmax}            	   >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${lonmin}                 >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${lonmax}                 >> ${figsGener_cntrl}_${nwpstr}_vertical
    echo ${nwpDataSrc}		   >> ${figsGener_cntrl}_${nwpstr}_vertical

    echo ${satId}		   >  ${figsGener_cntrl}_mspps
    echo ${gridfactor}  	   >> ${figsGener_cntrl}_mspps
    echo ${grid4BiasDir}/	   >> ${figsGener_cntrl}_mspps
    echo ${figsDir}/		   >> ${figsGener_cntrl}_mspps
    echo ${fileExt}		   >> ${figsGener_cntrl}_mspps
    echo ${version}		   >> ${figsGener_cntrl}_mspps
    echo ${edr4BiasList}	   >> ${figsGener_cntrl}_mspps
    echo ${dep4BiasList}	   >> ${figsGener_cntrl}_mspps
    echo ${nwp4BiasList}_${nwpstr} >> ${figsGener_cntrl}_mspps
    echo ${biasPath}		   >> ${figsGener_cntrl}_mspps
    echo ${latmin}		   >> ${figsGener_cntrl}_mspps
    echo ${latmax}		   >> ${figsGener_cntrl}_mspps
    echo ${lonmin}		   >> ${figsGener_cntrl}_mspps
    echo ${lonmax}		   >> ${figsGener_cntrl}_mspps

    local grids=(gridNwp gridNwpBias gridNwpAsym gridRad p2p_mirs_nwp vertical p2p_mirs_mspps)
    local controls=(\
    ${figsGener_cntrl}_${nwpstr}_grid \
    ${figsGener_cntrl}_${nwpstr}_grid \
    ${figsGener_cntrl}_${nwpstr}_grid \
    ${figsGener_cntrl}_${nwpstr}_grid \
    ${figsGener_cntrl}_${nwpstr}_grid \
    ${figsGener_cntrl}_${nwpstr}_vertical \
    ${figsGener_cntrl}_mspps)
    
    cd ${figsGener}
    
    local ngrid=${#grids[@]}
    for (( igrid=0; igrid<${ngrid}; igrid++ )) ; do

      local grid=${grids[$igrid]}
      local control=${controls[$igrid]}
      local batch_idl=${controlDataPath}/batch_${grid}_${nwpstr}.pro
      echo "!QUIET=1"                      >  ${batch_idl}
      echo ".r ${grid}.pro"                >> ${batch_idl}
      echo "${grid},namelist='${control}'" >> ${batch_idl}
      echo "exit"                          >> ${batch_idl}
      
      if [[ ${igrid} -lt 6 ]] ; then
        ${IDL} ${batch_idl} &
      fi
      
      if [[ ${igrid} -eq 6  && ${nwpDataSrc} -eq 1  && ( ${satId} == 'n18' || ${satId} == 'metopA' ) ]] ; then
        ${IDL} ${batch_idl} &
      fi
      
    done
    wait
    
    for (( igrid=0; igrid<${ngrid}; igrid++ )) ; do
      grid=${grids[$igrid]}
      rm -f ${controlDataPath}/batch_${grid}_${NWPSTR}.pro
    done
    
    echo "End of step biasFigsGen_parallel"
}


# to get julian day ( 1..366 )
function cal2julian() {

  local  yyyytmp=$1
  local  mmtmp=$2
  local  ddtmp=$3

  let "yyyy = 10#${yyyytmp}"
  let "mm = 10#${mmtmp}"
  let "dd = 10#${ddtmp}"
  
  # error checking
  if [[ ( $dd -gt 31 ) ||  ( $dd -lt 1 ) || ( $mm -gt 12 ) || ( $mm -lt 1 ) ]] ; then
    jday=-1
    exit
  fi   
   
  local JulianDate1=( 0  31  60  91  121  152  182  213  244  274  305  335 )
  local JulianDate2=( 0  31  59  90  120  151  181  212  243  273  304  334 )
 
  let "div4=$yyyy % 4"
  let "div100=$yyyy % 100" 
  let "div400=$yyyy % 400"
  
  local leap=0
  
  if [[ ( $div4 -eq 0 && $div100 -ne 0 ) || ( $div400 -eq 0 ) ]] ; then
    leap=1
  fi
  
  if [[ $leap -eq 1 ]] ; then
    let im=$mm-1
    let "jday=${JulianDate1[$im]} + $dd"
  else
    let im=$mm-1
    let "jday=${JulianDate2[$im]} + $dd"       
  fi
  
  echo $jday

}


# to get days difference
function ndays_diff() {
  local year1=$1
  local jday1tmp=$2
  local year2=$3
  local jday2tmp=$4
  
  let "jday1 = 10#${jday1tmp}"
  let "jday2 = 10#${jday2tmp}"
  
  local nday=0
  if [[ ${year1} -eq ${year2} ]] ; then
    let nday=jday2-jday1
  else
    local nleap=0
    for (( year=${year1} ; year<${year2} ; year++ )) ; do
      
	let "div4=$year % 4"
	let "div100=$year % 100" 
	let "div400=$year % 400"
    
	if [[ ( $div4 -eq 0 && $div100 -ne 0 ) || ( $div400 -eq 0 ) ]] ; then
    	  let nleap=nleap+1
  	fi
    done
    
    nday=`echo "(${year2}-${year1})*365+${nleap}+${jday2}-${jday1}" | bc`
    
  fi
  
  echo $nday

}


# to get those nwp file list based on starting and ending time
function getRefList() {

  local ymdh_s=$1
  local ymdh_e=$2

  local ymd_s=`echo ${ymdh_s} | cut -c1-10`
  local ymd_e=`echo ${ymdh_e} | cut -c1-10`

  local year1=`echo $ymd_s | cut -c1-4`
  local mm1=`echo $ymd_s | cut -c6-7`
  local dd1=`echo $ymd_s | cut -c9-10`
  local jday1=`cal2julian ${year1} ${mm1} ${dd1}`

  local year2=`echo $ymd_e | cut -c1-4`
  local mm2=`echo $ymd_e | cut -c6-7`
  local dd2=`echo $ymd_e | cut -c9-10`
  local jday2=`cal2julian ${year2} ${mm2} ${dd2}`

  local nday=`ndays_diff ${year1} ${jday1} ${year2} ${jday2}`

  #echo "nday=${nday}" 

  hh_s=`echo ${ymdh_s} | cut -c12-13`
  hh_e=`echo ${ymdh_e} | cut -c12-13`

  local hhs=(00 06 12 18)
  local nh=${#hhs[@]}
 
  local ihs=0
  local ihe=0

  for (( ih=0; ih<${nh}; ih++ )) ; do
    if [[ "${hhs[$ih]}" == "${hh_s}" ]] ; then
      let ihs=ih
    fi
  done

  for (( ih=0; ih<${nh}; ih++ )) ; do
    if [[ "${hhs[$ih]}" == "${hh_e}" ]] ; then
      let ihe=ih
    fi
  done


  if [[ ${nday} -eq 0 ]] ; then
    
    for (( ih=${ihs}; ih<=${ihe}; ih++ )) ; do
       echo "${ymd_s}.t${hhs[$ih]}"
    done
  
  elif [[ ${nday} -eq 1 ]] ; then
  
    for (( ih=${ihs}; ih<${nh}; ih++ )) ; do
       echo "${ymd_s}.t${hhs[$ih]}"
    done
  
    for (( ih=0; ih<=${ihe}; ih++ )) ; do
       echo "${ymd_e}.t${hhs[$ih]}"
    done
  
  else
  
    for (( ih=${ihs}; ih<${nh}; ih++ )) ; do
       echo "${ymd_s}.t${hhs[$ih]}"
    done
  
    for (( iday=1; iday<${nday}; iday++ )) ; do
      ymd=`date -d "${ymd_s} +${iday} day" +%Y-%m-%d`
      for (( ih=0; ih<${nh}; ih++ )) ; do
        echo "${ymd}.t${hhs[$ih]}"
      done
    done

    for (( ih=0; ih<=${ihe}; ih++ )) ; do
       echo "${ymd_e}.t${hhs[$ih]}"
    done

  fi

}


# return NEDT externsion based on a set of raw tdr granule files
# This is only for NPP ATMS granule case only
# 05/06/2011     Wanchun Chen

function determineNedtExt() {

  local satId=$1
  local dirRdr=$2
  local rdrType=$3
  
  local file1
  local filen
  
  if [[ ${rdrType} -eq 3 ]] ; then
    file1=`ls -1 ${dirRdr}/SATMS_npp_d* | head -1`
    filen=`ls -1 ${dirRdr}/SATMS_npp_d* | tail -1`
  elif [[ ${rdrType} -eq 1 ]] ; then
    file1=`ls -1 ${dirRdr}/TATMS_npp_d* | head -1`
    filen=`ls -1 ${dirRdr}/TATMS_npp_d* | tail -1`
  fi
  
  local base_file1=`basename ${file1}`
  local base_filen=`basename ${filen}`
  
  local str_start=`echo ${base_file1} | cut -f3-4 -d_`
  local str_end=`echo ${base_filen} | cut -f5-6 -d_`
  
  local nedtFile=${str_start}_${str_end}

  echo ${nedtFile}
  
}


function qcAvg() {
    if [[ $# -ne 4 ]] ; then
      op_msg "Error: function qcAvg: check number of arguments: $#"
      exit 1
    fi
    
    local satId=$1
    local ymd=$2
    local pathMon=$3
    local controlDataPath=$4
    
    local yyyy=`echo $ymd | cut -c1-4`
    local mm=`echo ${fileExt} | cut -c6-7`
    local dd=`echo ${fileExt} | cut -c9-10`
    local yyyymmdd=${yyyy}${mm}${dd}
    
    mkdir -p ${pathMon}/${ymd}/
    mv -f ${pathMon}/QC_MONS_${satId}_d${yyyymmdd}_t* ${pathMon}/${ymd}/ 2> /dev/null
    file_qc_tmp=${controlDataPath}/${satId}_qcMon_${ymd}_tmp
    cat ${pathMon}/${ymd}/QC_MONS_${satId}_d${yyyymmdd}_t* > ${file_qc_tmp} 2> /dev/null
    local nline=`wc -l ${file_qc_tmp} | awk '{print $1}'`
    
    rm -f ${pathMon}/QC_MONS_${satId}_${ymd}.HR
    if [[ ${nline} -ge 1 ]] ; then
      local col2=`awk '{sum+=$2} END { printf " %3d.00000\n",sum/NR}' ${file_qc_tmp}`
      local col3=`awk '{sum+=$3} END { printf " %7.4f\n",sum/NR}' ${file_qc_tmp}`
      local col4=`awk '{sum+=$4} END { printf " %7.4f\n",sum/NR}' ${file_qc_tmp}`
      local col5=`awk '{sum+=$5} END { printf " %7.4f\n",sum/NR}' ${file_qc_tmp}`
      local col6=`awk '{sum+=$6} END { printf " %7.4f\n",sum/NR}' ${file_qc_tmp}`
    
      echo "${yyyy}${col2}${col3}${col4}${col5}${col6}" > ${pathMon}/QC_MON_${satId}_${ymd}.HR
    else
      echo "NO granule QC files under ${pathMon}/${ymd} and NO avg QC file generated"
    fi
    
    rm -f ${file_qc_tmp}
    echo "End of step qcAvg"
}


#===============================================================
# Name:		    selMidGranule4NPP
#
#
# Type:		    Bash Shell Script
#
#
# Description:
#	Removes MiRS SDR, TDR, FMSDR and NEDT files associated with 1st
#       and 3rd granules when processing at NDE/ESPC for NPP. 
#       Should be called after the fm step so that only the 2nd 
#       granule is processed by the 1dvar (if AAPP code was used 
#       for fm).
#
#
# Input Variables:
# 	- fmsdrDir: Input directory containing FMSDR files
#
# History:
#      	- 05/31/2012     Kevin Garrett
#
#===============================================================

function selMidGranule4NPP() {
    if [[ $# -ne 1 ]] ; then
      op_msg "Error: function selMidGranule4NPP: check number of arguments: $#"
      exit 1
    fi
    
    local fmsdrDir=$1
    local fmsdrList=${fmsdrDir}/temp_list4fmsdr.list
    
    #Isolate 1st and 3rd granule file names
    ls ${fmsdrDir}/FMSDR* > ${fmsdrList} 
    local firstFile2del=`head -n 1 ${fmsdrList}`
    local secondFile2del=`tail -n 1 ${fmsdrList}`
    local granule1del=`basename ${firstFile2del}`
    local granule2del=`basename ${secondFile2del}`
    #Extract the orbit info string common for SDR, TDR, FMSDR and NEDT
    local str1del=`echo ${granule1del} | cut -f 3-6 -d "_"`
    local str2del=`echo ${granule2del} | cut -f 3-6 -d "_"`
    #Delete TDR, SDR, FMSDR, and NEDT for 1st and 3rd granules
    rm ${fmsdrDir}/*${str1del}*
    rm ${fmsdrDir}/*${str2del}*

    echo "End of step selMidGranule4NPP"

}


# old version if bais generation with IDL code
function biasGenIDL() {
    if [[ $# -ne 26 ]]
    then
	op_msg "Error: function biasGenIDL: check number of arguments: $#"
	exit 1      
    fi
    local determinBias=$1    
    local nwpDir_analys=$2    
    local extResol=$3    
    local NWPanalysList_4Bias=$4    
    local fwdDir_analys=$5    
    local FWDanalysList_4Bias=$6    
    local fmsdrDir4Bias=$7    
    local fmsdrList4Bias=$8    
    local biasGen_cntrl=$9    
    local iBiasComputMethod=${10}    
    local biasFile=${11}    
    local ModelErrFile=${12}    
    local figsDir4Bias=${13}    
    local norbits2process=${14}    
    local satId=${15}    
    local IDL=${16}
    local iSensor_ID=${17}
    local Topogr=${18}
    local binDir=${19}
    local modelErrNominalFile=${20}
    local controlDataPath=${21}
    local nwpDataSrc=${22}
    local minLat=${23}
    local maxLat=${24}
    local minLon=${25}
    local maxLon=${26}

    local nwpstr='gdas'
    local NWPSTR='GDAS'
    if [[ ${nwpDataSrc} -eq 2 ]] ; then
      nwpstr='ecmw'
      NWPSTR='ECMW'
    fi
    if [[ ${nwpDataSrc} -eq 3 ]] ; then
      nwpstr='gfs'
      NWPSTR='GFS'
    fi
    
    # from extension resolution --> fmType
    local fmType=0
    if [[ ${extResol} == "CR" ]] ; then
      fmType=-1
    elif [[ ${extResol} == "LR" ]] ; then 
      fmType=0
    elif [[ ${extResol} == "HR" ]] ; then 
      fmType=1
    elif [[ ${extResol} == "UAS" ]] ; then 
      fmType=0
    elif [[ ${extResol} == "LAS" ]] ; then 
      fmType=1
    elif [[ ${extResol} == "ENV" ]] ; then 
      fmType=2
    elif [[ ${extResol} == "IMG" ]] ; then 
      fmType=3
    fi
    
    echo "=============== RUNNING BIAS GENERATION STEP (${NWPSTR}) ======================="
    local biasFile_${nwpstr}=`echo ${biasFile} | sed -e "s/_${satId}_/_${satId}_${nwpstr}_/g"`
    local ModelErrFile_${nwpstr}=`echo ${ModelErrFile} | sed -e "s/_${satId}_/_${satId}_${nwpstr}_/g"`
    cd ${determinBias}
    ls -1 ${nwpDir_analys}/NWP_${NWPSTR}*${extResol}* > ${NWPanalysList_4Bias}_${nwpstr}
    ls -1 ${fwdDir_analys}/FWD_${NWPSTR}*${extResol}* > ${FWDanalysList_4Bias}_${nwpstr}
    ls -1 ${fmsdrDir4Bias}/FMSDR*${extResol}* > ${fmsdrList4Bias}
    echo ${NWPanalysList_4Bias}_${nwpstr}	>  ${biasGen_cntrl}_${nwpstr}
    echo ${FWDanalysList_4Bias}_${nwpstr}	>> ${biasGen_cntrl}_${nwpstr}
    echo ${fmsdrList4Bias}			>> ${biasGen_cntrl}_${nwpstr}
    echo ${iBiasComputMethod}			>> ${biasGen_cntrl}_${nwpstr}
    echo ${biasFile}_${nwpstr}  		>> ${biasGen_cntrl}_${nwpstr}
    echo ${ModelErrFile}_${nwpstr}		>> ${biasGen_cntrl}_${nwpstr}
    echo ${figsDir4Bias}/biasGen_${nwpstr}.ps   >> ${biasGen_cntrl}_${nwpstr}
    echo ${norbits2process}			>> ${biasGen_cntrl}_${nwpstr}
    echo ${iSensor_ID}			        >> ${biasGen_cntrl}_${nwpstr}
    echo ${fmType}			        >> ${biasGen_cntrl}_${nwpstr}
    echo ${minLat}			        >> ${biasGen_cntrl}_${nwpstr}
    echo ${maxLat}			        >> ${biasGen_cntrl}_${nwpstr}
    echo ${minLon}			        >> ${biasGen_cntrl}_${nwpstr}
    echo ${maxLon}			        >> ${biasGen_cntrl}_${nwpstr}
    
    echo "!QUIET=1"                                        >  ${controlDataPath}/batchCalibRad_${nwpstr}.pro
    echo ".r Calib_generic_rad.pro"                        >> ${controlDataPath}/batchCalibRad_${nwpstr}.pro
    echo "Calib_generic_rad,'${biasGen_cntrl}_${nwpstr}'"  >> ${controlDataPath}/batchCalibRad_${nwpstr}.pro
    echo "exit" 				           >> ${controlDataPath}/batchCalibRad_${nwpstr}.pro
    ${IDL} ${controlDataPath}/batchCalibRad_${nwpstr}.pro
    checkStatus $? "biasGen: ${nwpstr}"
    rm -r ${controlDataPath}/batchCalibRad_${nwpstr}.pro
    echo "End of step biasGenIDL" 
}
