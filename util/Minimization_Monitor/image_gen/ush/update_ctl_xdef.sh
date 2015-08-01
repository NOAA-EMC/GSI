#!/bin/ksh

#-------------------------------------------------------------
# 
#  update_ctl_xdef.sh
#
#  Update the xdef line (iteration definition) in a given control 
#  file.
#
#-------------------------------------------------------------

function usage {
  echo "Usage:  update_ctl_xdef.sh control_file_path_and_name num_iters"
}


#-------------------------------------------------------------
# check for correct usage and assign cmd line vars
#
  echo "enter update_ctl_xdef.sh"

  if [[ $# -lt 2 ]]; then
    usage
    exit 1
  fi

  ctl_file=$1
  num_iters=$2

#  iyy=`echo $datestr | cut -c1-4`
#  imm=`echo $datestr | cut -c5-6`
#  idd=`echo $datestr | cut -c7-8`
#  ihh=`echo $datestr | cut -c9-10`
#
#  echo year = $iyy
#  echo month= $imm
#  echo day  = $idd
#  echo hour = $ihh

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

  found_xdef=0
  while read line; do

    xdef=`echo $line | gawk '{print $1}'`

    if [[ $xdef == "xdef" ]]; then
#      echo "found xdef"
#      echo "$line"
      v3=`echo $line | gawk '{print $3}'`
      v4=`echo $line | gawk '{print $4}'`
      v5=`echo $line | gawk '{print $5}'`

      newline="${xdef} ${num_iters} ${v3} ${v4} ${v5}"
#      echo "$newline"
      echo "$newline" >> $tmp_file
      found_xdef=1
    else
      echo "$line" >> $tmp_file
    fi 

  done < "$ctl_file"

  if [[ $found_xdef -eq 1 ]]; then
     cp -f $tmp_file $ctl_file
     rm -f $tmp_file
  else
     echo "ERROR  tdef not found, no update to $ctl_base done"
  fi

  echo "exit update_ctl_xdef.sh"
exit
