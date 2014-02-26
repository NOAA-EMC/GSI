function get_machine_endian() {

local CC=gcc

cat > endian.c <<CEOF

#include <stdio.h>
#include <stdlib.h>

/**
 * return 1 if little endian
 * return 0 if big endian
 */
int getEndian()
{
    union {
        int theInteger;
        char theByte;
    } endianUnion;

    endianUnion.theInteger = 1 ;
    return endianUnion.theByte;
}

int main() {

    return getEndian();
}

CEOF


${CC} -o endian endian.c
./endian

endianTest=$?

rm -f endian endian.c

echo ${endianTest}

}




# This is STAR version of history GFS data
function get_avn() {

  if [[ $# -ne 3 ]] ; then
    echo "Error: function $0: check number of arguments. Found $#" 
    exit 1
  fi
  
  local avn_from=$1
  local avn_to=$2
  local yyyyjjj=$3

  cp ${avn_from}/avn_tpw00_${yyyyjjj}.bin  ${avn_to}/avn_tpw00.bin
  cp ${avn_from}/avn_tpw03_${yyyyjjj}.bin  ${avn_to}/avn_tpw03.bin
  cp ${avn_from}/avn_tpw06_${yyyyjjj}.bin  ${avn_to}/avn_tpw06.bin
  cp ${avn_from}/avn_tpw09_${yyyyjjj}.bin  ${avn_to}/avn_tpw09.bin
  cp ${avn_from}/avn_tpw12_${yyyyjjj}.bin  ${avn_to}/avn_tpw12.bin
  cp ${avn_from}/avn_tpw15_${yyyyjjj}.bin  ${avn_to}/avn_tpw15.bin
  cp ${avn_from}/avn_tpw18_${yyyyjjj}.bin  ${avn_to}/avn_tpw18.bin
  cp ${avn_from}/avn_tpw21_${yyyyjjj}.bin  ${avn_to}/avn_tpw21.bin

  cp ${avn_from}/avn_ts00_${yyyyjjj}.bin  ${avn_to}/avn_ts00.bin
  cp ${avn_from}/avn_ts03_${yyyyjjj}.bin  ${avn_to}/avn_ts03.bin
  cp ${avn_from}/avn_ts06_${yyyyjjj}.bin  ${avn_to}/avn_ts06.bin
  cp ${avn_from}/avn_ts09_${yyyyjjj}.bin  ${avn_to}/avn_ts09.bin
  cp ${avn_from}/avn_ts12_${yyyyjjj}.bin  ${avn_to}/avn_ts12.bin
  cp ${avn_from}/avn_ts15_${yyyyjjj}.bin  ${avn_to}/avn_ts15.bin
  cp ${avn_from}/avn_ts18_${yyyyjjj}.bin  ${avn_to}/avn_ts18.bin
  cp ${avn_from}/avn_ts21_${yyyyjjj}.bin  ${avn_to}/avn_ts21.bin

  cp ${avn_from}/avn_ts2m00_${yyyyjjj}.bin  ${avn_to}/avn_ts2m00.bin
  cp ${avn_from}/avn_ts2m03_${yyyyjjj}.bin  ${avn_to}/avn_ts2m03.bin
  cp ${avn_from}/avn_ts2m06_${yyyyjjj}.bin  ${avn_to}/avn_ts2m06.bin
  cp ${avn_from}/avn_ts2m09_${yyyyjjj}.bin  ${avn_to}/avn_ts2m09.bin
  cp ${avn_from}/avn_ts2m12_${yyyyjjj}.bin  ${avn_to}/avn_ts2m12.bin
  cp ${avn_from}/avn_ts2m15_${yyyyjjj}.bin  ${avn_to}/avn_ts2m15.bin
  cp ${avn_from}/avn_ts2m18_${yyyyjjj}.bin  ${avn_to}/avn_ts2m18.bin
  cp ${avn_from}/avn_ts2m21_${yyyyjjj}.bin  ${avn_to}/avn_ts2m21.bin

  cp ${avn_from}/avn_u00_${yyyyjjj}.bin  ${avn_to}/avn_u00.bin
  cp ${avn_from}/avn_u03_${yyyyjjj}.bin  ${avn_to}/avn_u03.bin
  cp ${avn_from}/avn_u06_${yyyyjjj}.bin  ${avn_to}/avn_u06.bin
  cp ${avn_from}/avn_u09_${yyyyjjj}.bin  ${avn_to}/avn_u09.bin
  cp ${avn_from}/avn_u12_${yyyyjjj}.bin  ${avn_to}/avn_u12.bin
  cp ${avn_from}/avn_u15_${yyyyjjj}.bin  ${avn_to}/avn_u15.bin
  cp ${avn_from}/avn_u18_${yyyyjjj}.bin  ${avn_to}/avn_u18.bin
  cp ${avn_from}/avn_u21_${yyyyjjj}.bin  ${avn_to}/avn_u21.bin

  cp ${avn_from}/avn_v00_${yyyyjjj}.bin  ${avn_to}/avn_v00.bin
  cp ${avn_from}/avn_v03_${yyyyjjj}.bin  ${avn_to}/avn_v03.bin
  cp ${avn_from}/avn_v06_${yyyyjjj}.bin  ${avn_to}/avn_v06.bin
  cp ${avn_from}/avn_v09_${yyyyjjj}.bin  ${avn_to}/avn_v09.bin
  cp ${avn_from}/avn_v12_${yyyyjjj}.bin  ${avn_to}/avn_v12.bin
  cp ${avn_from}/avn_v15_${yyyyjjj}.bin  ${avn_to}/avn_v15.bin
  cp ${avn_from}/avn_v18_${yyyyjjj}.bin  ${avn_to}/avn_v18.bin
  cp ${avn_from}/avn_v21_${yyyyjjj}.bin  ${avn_to}/avn_v21.bin

}



# This is OSDPD version of history GFS data
function get_avn2() {

  if [[ $# -ne 3 ]] ; then
    echo "Error: function $0: check number of arguments. Found $#" 
    exit 1
  fi
  
  local avn_from=$1
  local avn_to=$2
  local yyyymmdd=$3

  cp ${avn_from}/avn_tpw00.bin.${yyyymmdd}  ${avn_to}/avn_tpw00.bin
  cp ${avn_from}/avn_tpw03.bin.${yyyymmdd}  ${avn_to}/avn_tpw03.bin
  cp ${avn_from}/avn_tpw06.bin.${yyyymmdd}  ${avn_to}/avn_tpw06.bin
  cp ${avn_from}/avn_tpw09.bin.${yyyymmdd}  ${avn_to}/avn_tpw09.bin
  cp ${avn_from}/avn_tpw12.bin.${yyyymmdd}  ${avn_to}/avn_tpw12.bin
  cp ${avn_from}/avn_tpw15.bin.${yyyymmdd}  ${avn_to}/avn_tpw15.bin
  cp ${avn_from}/avn_tpw18.bin.${yyyymmdd}  ${avn_to}/avn_tpw18.bin
  cp ${avn_from}/avn_tpw21.bin.${yyyymmdd}  ${avn_to}/avn_tpw21.bin

  cp ${avn_from}/avn_ts00.bin.${yyyymmdd}  ${avn_to}/avn_ts00.bin
  cp ${avn_from}/avn_ts03.bin.${yyyymmdd}  ${avn_to}/avn_ts03.bin
  cp ${avn_from}/avn_ts06.bin.${yyyymmdd}  ${avn_to}/avn_ts06.bin
  cp ${avn_from}/avn_ts09.bin.${yyyymmdd}  ${avn_to}/avn_ts09.bin
  cp ${avn_from}/avn_ts12.bin.${yyyymmdd}  ${avn_to}/avn_ts12.bin
  cp ${avn_from}/avn_ts15.bin.${yyyymmdd}  ${avn_to}/avn_ts15.bin
  cp ${avn_from}/avn_ts18.bin.${yyyymmdd}  ${avn_to}/avn_ts18.bin
  cp ${avn_from}/avn_ts21.bin.${yyyymmdd}  ${avn_to}/avn_ts21.bin

  cp ${avn_from}/avn_ts2m00.bin.${yyyymmdd}  ${avn_to}/avn_ts2m00.bin
  cp ${avn_from}/avn_ts2m03.bin.${yyyymmdd}  ${avn_to}/avn_ts2m03.bin
  cp ${avn_from}/avn_ts2m06.bin.${yyyymmdd}  ${avn_to}/avn_ts2m06.bin
  cp ${avn_from}/avn_ts2m09.bin.${yyyymmdd}  ${avn_to}/avn_ts2m09.bin
  cp ${avn_from}/avn_ts2m12.bin.${yyyymmdd}  ${avn_to}/avn_ts2m12.bin
  cp ${avn_from}/avn_ts2m15.bin.${yyyymmdd}  ${avn_to}/avn_ts2m15.bin
  cp ${avn_from}/avn_ts2m18.bin.${yyyymmdd}  ${avn_to}/avn_ts2m18.bin
  cp ${avn_from}/avn_ts2m21.bin.${yyyymmdd}  ${avn_to}/avn_ts2m21.bin

  cp ${avn_from}/avn_u00.bin.${yyyymmdd}  ${avn_to}/avn_u00.bin
  cp ${avn_from}/avn_u03.bin.${yyyymmdd}  ${avn_to}/avn_u03.bin
  cp ${avn_from}/avn_u06.bin.${yyyymmdd}  ${avn_to}/avn_u06.bin
  cp ${avn_from}/avn_u09.bin.${yyyymmdd}  ${avn_to}/avn_u09.bin
  cp ${avn_from}/avn_u12.bin.${yyyymmdd}  ${avn_to}/avn_u12.bin
  cp ${avn_from}/avn_u15.bin.${yyyymmdd}  ${avn_to}/avn_u15.bin
  cp ${avn_from}/avn_u18.bin.${yyyymmdd}  ${avn_to}/avn_u18.bin
  cp ${avn_from}/avn_u21.bin.${yyyymmdd}  ${avn_to}/avn_u21.bin

  cp ${avn_from}/avn_v00.bin.${yyyymmdd}  ${avn_to}/avn_v00.bin
  cp ${avn_from}/avn_v03.bin.${yyyymmdd}  ${avn_to}/avn_v03.bin
  cp ${avn_from}/avn_v06.bin.${yyyymmdd}  ${avn_to}/avn_v06.bin
  cp ${avn_from}/avn_v09.bin.${yyyymmdd}  ${avn_to}/avn_v09.bin
  cp ${avn_from}/avn_v12.bin.${yyyymmdd}  ${avn_to}/avn_v12.bin
  cp ${avn_from}/avn_v15.bin.${yyyymmdd}  ${avn_to}/avn_v15.bin
  cp ${avn_from}/avn_v18.bin.${yyyymmdd}  ${avn_to}/avn_v18.bin
  cp ${avn_from}/avn_v21.bin.${yyyymmdd}  ${avn_to}/avn_v21.bin

}



#===============================================================
# Name:		    yyyyjjj2yyyymmdd
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
#      - yyyymmdd
#
#===============================================================
function yyyyjjj2yyyymmdd() {

    local yyyy=$1
    local jjj=$2
    
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
    
    #echo "$yyyy-$mm-$dd"
    echo "$yyyy$mm$dd"
           
}



# This is to call IDL program to plot images
function figsGen() {
    if [[ $# -ne 7 ]] ; then
        echo "Error: function figsGen: check number of arguments: $#"
        exit 1
    fi
    
    local satid=$1
    local date=$2
    local fileList=$3
    local figsDir=$4
    local idlExe=$5
    local idlSrc=$6
    local namelist=$7

    cd ${idlSrc}
    
    echo ${satid}	>  ${namelist}
    echo ${date}	>> ${namelist}
    echo ${fileList}	>> ${namelist}
    echo ${figsDir}/	>> ${namelist}
    echo ${idlExe}	>> ${namelist}
    echo ${idlSrc}	>> ${namelist}
    
    echo "!QUIET=1"              		>  ${idlSrc}/batchAMSUBMHS.pro
    echo ".r amsub_mhs.pro"			>> ${idlSrc}/batchAMSUBMHS.pro
    echo "amsub_mhs, namelist='${namelist}'"	>> ${idlSrc}/batchAMSUBMHS.pro
    echo "exit"                       		>> ${idlSrc}/batchAMSUBMHS.pro
    ${idlExe} ${idlSrc}/batchAMSUBMHS.pro
    rm -f ${idlSrc}/batchAMSUBMHS.pro
}
