#!/bin/ksh

#-----------------------------------------------------------------
# This script was written by Tim Marchok (timothy.marchok@noaa.gov)
#
# This script is used to cut apart the output files from TPC's
# verification program and grab the average intensity errors and the 
# errors relative to SHIFOR and write all those values back out to
# another ascii file that will be used as input to a program that 
# converts the data to GrADS format.  The awk portion of the script
# puts missing values of -999 in for the the 0, 60, 84 and 108h forecast
# periods, and also puts in -999 if the 72h values are missing (in
# case your input file is a 48h file).

# **** THIS VERSION CUTS APART THE INTENSITY FILES.  THE DIFFERENCE
# **** IS IN THE MATCH STATEMENT IN THE AWK PART, AND ALSO IT LOOKS
# **** FOR "RELATIVE TO SHF5" INSTEAD OF CLIP.

# Usage: sh ivercut.sh  full_path_file
# where full_path_file is full pathway name of file to be parsed. 

#set -x
#scrdir=/nfsuser/g01/wx20tm/hur/verify/scripts

export full_path_file=$1
export scrdir=$2          

ifile=`  basename ${full_path_file}`
datdir=`  dirname ${full_path_file}`

ifbasenum=` echo $ifile | awk -F. '{print NF}'`
let ifbasenum=ifbasenum-1
ifbase=` echo $ifile | cut -d. -f1-${ifbasenum}`

outfile="${ifbase}.dat"
gradsfile="${ifbase}.gr"
ctlfile="${ifbase}.ctl"

awk '

  {
    if (match($0," AVERAGE INTENSITY ERRORS") || match($0," average intensity errors")) {
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          break
        }
        else {
          model = $1
          h00   = $2
          h12   = $3
          h24   = $4
          h36   = $5
          h48   = $6
          h72   = $7
          h96   = $8
          h120  = $9
          if (model == "NCHG") continue
          if (model == "65KT") continue
          if (h00  == 0.0) { h00  = -999.0 }
          if (h12  == 0.0) { h12  = -999.0 }
          if (h24  == 0.0) { h24  = -999.0 }
          if (h36  == 0.0) { h36  = -999.0 }
          if (h48  == 0.0) { h48  = -999.0 }
          if (h72  == 0.0) { h72  = -999.0 }
          if (h96  == 0.0) { h96  = -999.0 }
          if (h120 == 0.0) { h120 = -999.0 }
          printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",model,h00,h12,h24,h36,h48,h72,h96,h120)
        }
      }
    }


    if (match($0,"RELATIVE TO SHF5") || match($0,"RELATIVE TO SHFR") || match($0,"RELATIVE TO ST5D") ) {
      getline
      while (1) {
        getline
        if (match($0,"#CASES")) {
          break
        }
        else {
          model = $1
          h00   = $2
          h12   = $3
          h24   = $4
          h36   = $5
          h48   = $6
          h72   = $7
          h96   = $8
          h120  = $9
          if (model == "NCHG") continue
          if (model == "65KT") continue
          if (h00  == 9999.0)                { h00  = -999.0 }
          if (h12  == 9999.0 || h12  == 0.0) { h12  = -999.0 }
          if (h24  == 9999.0 || h24  == 0.0) { h24  = -999.0 }
          if (h36  == 9999.0 || h36  == 0.0) { h36  = -999.0 }
          if (h48  == 9999.0 || h48  == 0.0) { h48  = -999.0 }
          if (h72  == 9999.0 || h72  == 0.0) { h72  = -999.0 }
          if (h96  == 9999.0 || h96  == 0.0) { h96  = -999.0 }
          if (h120 == 9999.0 || h120 == 0.0) { h120 = -999.0 }
          if (model == "SHF5" || model == "SHFR") {
            printf (" SHFR       0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0\n")
          }
          else {
            printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",model,h00,h12,h24,h36,h48,h72,h96,h120)
          }
        }
      }
    }

  } ' ${full_path_file} >${datdir}/${outfile}

${scrdir}/wrtdat.sh ${datdir}/${outfile}

nmodels=` cat ${datdir}/${outfile} | wc -l`

sed -e "s/_FNAME/${gradsfile}/g" \
    -e "s/_NMODELS/${nmodels}/g" \
    ${scrdir}/shell.ctl >${datdir}/${ctlfile}
