#!/bin/ksh

#-------------------------------------------------------------
# 
#  update_ctl_fname.sh
#
#  Update/change the data file name in a given control file.
#
#-------------------------------------------------------------

function usage {
  echo "Usage:  update_ctl_fname.sh control_file_path_and_name new_data_file_name"
}


#-------------------------------------------------------------
# check for correct usage and assign cmd line vars
#
  echo "enter update_ctl_fname.sh"

  if [[ $# -lt 2 ]]; then
    usage
    exit 1
  fi

  ctl_file=$1
  filestr=$2


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
#  echo "tmp_file = ${tmp_file}"

  if [[ -s $tmp_file ]]; then
     rm -f $tmp_file
  fi


#-------------------------------------------------------------
#  The data file name should be on the first line and will
#  always start with "dset"

  found_dset=0
  while read line; do

    dset=`echo $line | gawk '{print $1}'`

    if [[ $dset == "dset" ]]; then
#      echo "found tdef"
#      echo "$line"
      fname="^${filestr}.%y4%m2%d2%h2.ieee_d"

      newline="${dset} ${fname}"
#      echo "$newline"
      echo "$newline" >> $tmp_file
      found_dset=1
    else
      echo "$line" >> $tmp_file
    fi 

  done < "$ctl_file"

  if [[ $found_dset -eq 1 ]]; then
     cp -f $tmp_file $ctl_file
     rm -f $tmp_file
  else
     echo "ERROR  dset line not found, no update to $ctl_base done"
  fi

  echo "exit update_ctl_fname.sh"
exit
