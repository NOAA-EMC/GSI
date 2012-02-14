#!/bin/ksh

#-----------------------------------------------------------------------------
#
#  script:   ck_stdout.sh
#
#  purpose:  Check the stdout files for any report of a problem reading the
#            diag files.  If an error message is found then cut it out of 
#            the stdout file and dump it to the outfile.
#
#
#  arguments:  outfile	 output file name 
#-----------------------------------------------------------------------------

set -ax
export list=$list0

nargs=$#


if [[ nargs -eq 1 ]]; then
   outfile=$1
   error_msg="PROBLEM reading diagnostic file"


   for type in ${SATYPE}; do
      echo ${type}
      if [[ -s "stdout.${type}" ]]; then
         echo  stdout.${type}
         match=`nawk "/$error_msg/" stdout.$type`

         match_len=`echo ${#match}`
         if [[ $match_len > 0 ]]; then
            echo "${type}  ${match}" >> $outfile
         fi
      fi
   done

else
#  
#  Usage message
#
   echo usage:
   echo ck_stdout.sh outfile
   echo              outfile  = output file name
fi

