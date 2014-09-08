#!/bin/ksh
set -x

#-----------------------------------------------------------------
# This script was written by Tim Marchok (timothy.marchok@noaa.gov)
#
# This script is used to cut apart the output files from TPC's
# verification program and grab the average track errors and the 
# errors relative to CLIPER and write all those values back out to
# another ascii file that will be used as input to a program that 
# converts the data to GrADS format.  The awk portion of the script
# puts missing values of -999 in for the 0, 60, 84 and 108h forecast 
# periods, and also puts in -999 if the 72h values are missing (in 
# case your input file is a 48h file).

# Usage: sh tvercut.sh  full_path_file
# where full_path_file is full pathway name of file to be parsed. 

# **** THIS VERSION CUTS APART THE TRACK FILES.  THERE IS ANOTHER
# **** VERSION, CALLED IVERCUT.SH, THAT CUTS APART THE INTENSITY 
# **** FILES.

#set -x

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
    if (match($0," AVERAGE TRACK ERRORS") || match($0," average track errors")) {
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
          if (h00 == 0.0)  { h00 = -999.0 }
          if (h12 == 0.0)  { h12 = -999.0 }
          if (h24 == 0.0)  { h24 = -999.0 }
          if (h36 == 0.0)  { h36 = -999.0 }
          if (h48 == 0.0)  { h48 = -999.0 }
          if (h72 == 0.0)  { h72 = -999.0 }
          if (h96 == 0.0)  { h96 = -999.0 }
          if (h120 == 0.0) { h120 = -999.0 }
          printf (" %-4s    %6.1f %6.1f %6.1f %6.1f %6.1f -999.0 %6.1f -999.0 %6.1f -999.0 %6.1f\n",model,h00,h12,h24,h36,h48,h72,h96,h120)
        }
      }
    }


    if (match($0,"RELATIVE TO CLP5") || match($0,"RELATIVE TO CLIP")) {
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
          if (h00  == 9999.0)                { h00  = -999.0 }
          if (h12  == 9999.0 || h12  == 0.0) { h12  = -999.0 }
          if (h24  == 9999.0 || h24  == 0.0) { h24  = -999.0 }
          if (h36  == 9999.0 || h36  == 0.0) { h36  = -999.0 }
          if (h48  == 9999.0 || h48  == 0.0) { h48  = -999.0 }
          if (h72  == 9999.0 || h72  == 0.0) { h72  = -999.0 }
          if (h96  == 9999.0 || h96  == 0.0) { h96  = -999.0 }
          if (h120 == 9999.0 || h120 == 0.0) { h120 = -999.0 }
          if (model == "CLP5" || model == "CLIP") {
            printf (" CLIP       0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0\n")
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
