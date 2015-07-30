#!/bin/ksh

#-------------------------------------------------------------
# 
#  update_ctl_tdef.sh
#
#  Update the tdef line (time definition) in a given control 
#  file.
#
#-------------------------------------------------------------

function usage {
  echo "Usage:  update_ctl_tdef.sh control_file_path_and_name yyymmddcc"
}


#-------------------------------------------------------------
# check for correct usage and assign cmd line vars
#
  echo "enter update_ctl_tdef.sh"

  if [[ $# -lt 2 ]]; then
    usage
    exit 1
  fi

  ctl_file=$1
  datestr=$2

  iyy=`echo $datestr | cut -c1-4`
  imm=`echo $datestr | cut -c5-6`
  idd=`echo $datestr | cut -c7-8`
  ihh=`echo $datestr | cut -c9-10`

  echo year = $iyy
  echo month= $imm
  echo day  = $idd
  echo hour = $ihh

#-------------------------------------------------------------
# validate ctr_file
#
  if [[ ! -s $ctl_file ]]; then
     echo $ctl_file not found
     exit -1
  fi

#-------------------------------------------------------------
# Construct tmp_file from ctl_file name (minus any full or
# relative path)
#

  ctl_base=`basename ${ctl_file}`
  ctl_dir=`dirname ${ctl_file}`

  tmp_file="${ctl_dir}/tmp_${ctl_base}"
  echo "tmp_file = ${tmp_file}"

  if [[ -s $tmp_file ]]; then
     rm -f $tmp_file
  fi


#-------------------------------------------------------------
# Validate datestr
#

  datelen=`echo ${#datestr}`

  if [[ $datelen -ne 10 ]]; then
     echo "date must be in format yyyymmddcc, 10 digits total"
     exit -2
  fi

  if [[ $imm == "01" ]]; then
     smon="jan"
  elif [[ $imm == "02" ]]; then
     smon="feb"
  elif [[ $imm == "03" ]]; then
     smon="mar"
  elif [[ $imm == "04" ]]; then
     smon="apr"
  elif [[ $imm == "05" ]]; then
     smon="may"
  elif [[ $imm == "06" ]]; then
     smon="jun"
  elif [[ $imm == "07" ]]; then
     smon="jul"
  elif [[ $imm == "08" ]]; then
     smon="aug"
  elif [[ $imm == "09" ]]; then
     smon="sep"
  elif [[ $imm == "10" ]]; then
     smon="oct"
  elif [[ $imm == "11" ]]; then
     smon="nov"
  elif [[ $imm == "12" ]]; then
     smon="dec"
  else
     echo "invalid month"
     exit -3
  fi

  echo string month = $smon

  found_tdef=0
  while read line; do

    tdef=`echo $line | gawk '{print $1}'`

    if [[ $tdef == "tdef" ]]; then
#      echo "found tdef"
#      echo "$line"
      v2=`echo $line | gawk '{print $2}'`
      v3=`echo $line | gawk '{print $3}'`
      v5=`echo $line | gawk '{print $5}'`

      ndate="${ihh}Z${idd}${smon}${iyy}"
      newline="${tdef} ${v2} ${v3} ${ndate} ${v5}"
#      echo "$newline"
      echo "$newline" >> $tmp_file
      found_tdef=1
    else
      echo "$line" >> $tmp_file
    fi 

  done < "$ctl_file"

  if [[ $found_tdef -eq 1 ]]; then
     cp -f $tmp_file $ctl_file
     rm -f $tmp_file
  else
     echo "ERROR  tdef not found, no update to $ctl_base done"
  fi

  echo "exit update_ctl_tdef.sh"
exit
