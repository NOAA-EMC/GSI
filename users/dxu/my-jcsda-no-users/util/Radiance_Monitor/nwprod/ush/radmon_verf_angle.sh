#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         radmon_verf_angle.sh
# Script description:  Extract angle dependent data from radiance
#                      diagnostic files.
#
# Author:        Ed  Safford       Org: NP23         Date: 2012-02-02
#
# Abstract:  This script extracts angle dependent data from radiance
#            diagnostic files (which are an output from GSI runs),
#            storing the extracted data in small binary files.
#
#            This script is a child script of exgdas_vrfyrad.sh.sms.  The parent
#            script opens and uncompresses the radiance diagnostic file and copies
#            other supporting files into a temporary working directory.
#
#
# Script history log:
# 2012-02-02  Safford  initial script
#
# Usage:  radmon_verf_angle.sh PDATE
#
#   Input script positional parameters:
#     PDATE             processing date
#                       yyyymmddcc format; required
#
#   Imported Shell Variables:
#     SUFFIX            data source suffix
#                       defauls to opr
#     MAKE_CTL          switch to construct the meta-data control file
#                       defaults to 1 (on)
#     MAKE_DATA         switch to construct the binary data file
#                       defaults to 1 (on)
#     EXECgfs           executable directory
#                       defaults to current directory
#     FIXgfs            fixed data directory
#                       defaults to current directory
#     RAD_AREA          global or regional flag
#                       defaults to global
#     TANKverf_rad      data repository
#                       defaults to current directory
#     SATYPE            list of satellite/instrument sources
#                       defaults to none
#     INISCRIPT         preprocessing script
#                       defaults to none
#     LOGSCRIPT         log script
#                       defaults to none
#     ERRSCRIPT         error processing script
#                       defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT         postprocessing script
#                       defaults to none
#     VERBOSE           Verbose flag (YES or NO)
#                       defaults to NO
#     LITTLE_ENDIAN     flag to indicate LE machine
#                       defaults to 0 (big endian)
#     USE_ANL           use analysis files as inputs in addition to 
#                         the ges files.  Default is 0 (ges only)
#
#   Exported Shell Variables:
#     err           Last return code
#
#   Modules and files referenced:
#     scripts    : $INISCRIPT
#                  $LOGSCRIPT
#                  $ERRSCRIPT
#                  $ENDSCRIPT
#
#     programs   : $NCP
#                  $angle_exec
#
#     fixed data : $scaninfo
#
#     input data : $data_file
#
#     output data: $angle_file
#                  $angle_ctl
#                  $angle_stdout
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#  Control variable resolution priority
#    1 Command line argument.
#    2 Environment variable.
#    3 Inline default.
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
####################################################################
#  Command line arguments.
export PDATE=${1:-${PDATE:?}}

# Directories
FIXgfs=${FIXgfs:-$(pwd)}
EXECgfs=${EXECgfs:-$(pwd)}
TANKverf_rad=${TANKverf_rad:-$(pwd)}

# File names
INISCRIPT=${INISCRIPT:-}
LOGSCRIPT=${LOGSCRIPT:-}
ERRSCRIPT=${ERRSCRIPT:-}
ENDSCRIPT=${ENDSCRIPT:-}

# Other variables
MAKE_CTL=${MAKE_CTL:-1}
MAKE_DATA=${MAKE_DATA:-1}
RAD_AREA=${RAD_AREA:-glb}
SATYPE=${SATYPE:-}
VERBOSE=${VERBOSE:-NO}
LITTLE_ENDIAN=${LITTLE_ENDIAN:-0}
USE_ANL=${USE_ANL:-0}

if [[ $USE_ANL -eq 1 ]]; then
   gesanl="ges anl"
else
   gesanl="ges"
fi

err=0
angle_exec=radmon_angle
scaninfo=scaninfo.txt

if [[ "$VERBOSE" = "YES" ]]; then
   set -ax
   echo "$(date) executing $0 $* >&2"
fi
################################################################################
#  Preprocessing
$INISCRIPT
$LOGSCRIPT


#--------------------------------------------------------------------
#   Copy extraction program and supporting files to working directory

$NCP ${EXECgfs}/${angle_exec}  ./
$NCP $FIXgfs/gdas_radmon_scaninfo.txt  ./${scaninfo}

if [[ ! -s ./${angle_exec} || ! -s ./${scaninfo} ]]; then
   err=2
else
#--------------------------------------------------------------------
#   Run program for given time

   iyy=`echo $PDATE | cut -c1-4`
   imm=`echo $PDATE | cut -c5-6`
   idd=`echo $PDATE | cut -c7-8`
   ihh=`echo $PDATE | cut -c9-10`

   ctr=0
   fail=0

   for type in ${SATYPE}; do

      for dtype in ${gesanl}; do

         ctr=`expr $ctr + 1`

         if [[ $dtype == "anl" ]]; then
            data_file=${type}_anl.${PDATE}.ieee_d
            angl_file=angle.${data_file}
            ctl_file=${type}_anl.ctl
            angl_ctl=angle.${ctl_file}
            stdout_file=stdout.${type}_anl
            angl_stdout=angle.${stdout_file}
         else
            data_file=${type}.${PDATE}.ieee_d
            angl_file=angle.${data_file}
            ctl_file=${type}.ctl
            angl_ctl=angle.${ctl_file}
            stdout_file=stdout.${type}
            angl_stdout=angle.${stdout_file}
         fi

         rm input

         nchanl=-999
cat << EOF > input
 &INPUT
  satname='${type}',
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-720,
  incr=6,
  nchanl=${nchanl},
  suffix='${SUFFIX}',
  imkctl=${MAKE_CTL},
  imkdata=${MAKE_DATA},
  gesanl='${dtype}',
  little_endian=${LITTLE_ENDIAN},
  rad_area='${RAD_AREA}',
 /
EOF
         $TIMEX ./${angle_exec} < input >   ${stdout_file}
         if [[ $? -ne 0 ]]; then
             fail=`expr $fail + 1`
         fi
#-------------------------------------------------------------------
#  move data, control, and stdout files to $TANKverf_rad and compress
#
         if [[ -s ${data_file} ]]; then
            mv ${data_file} ${angl_file}
            mv ${angl_file} $TANKverf_rad/.
            ${COMPRESS} -f $TANKverf_rad/${angl_file}
         fi

         if [[ -s ${ctl_file} ]]; then
            mv ${ctl_file} ${angl_ctl}
            mv ${angl_ctl}  ${TANKverf_rad}/.
            ${COMPRESS} -f ${TANKverf_rad}/${angl_ctl}
         fi 

         if [[ -s ${stdout_file} ]]; then
            mv ${stdout_file} ${angl_stdout}
            mv ${angl_stdout}  ${TANKverf_rad}/.
            ${COMPRESS} -f ${TANKverf_rad}/${angl_stdout}
         fi

      done    # for dtype in ${gesanl} loop
   done    # for type in ${SATYPE} loop

   if [[ $fail -eq $ctr || $fail -gt $ctr ]]; then
      err=3
   fi
fi

################################################################################
#  Post processing
$ENDSCRIPT
set +x

if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXITING $0 error code ${err} >&2
fi

exit ${err}
