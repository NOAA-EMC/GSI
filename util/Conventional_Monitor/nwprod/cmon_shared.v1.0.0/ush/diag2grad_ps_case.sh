#!/bin/sh
set -xa

#-------------------------------------------------------
#
#  diag2grad_ps_case.sh
#
#-------------------------------------------------------

echo "-->  diag2grad_ps_case.sh"

   echo "CMON_SUFFIX   = $CMON_SUFFIX"
   echo "TANKDIR_cmon  = $TANKDIR_cmon"
   echo "type          = $type"
   echo "PDATE         = $PDATE"
   echo "EXECcmon      = $EXECcmon"
   echo "cycle         = $cycle"
   echo "nreal         = $nreal"
   echo "mtype         = $mtype (type = $type)"
   echo "subtype       = $subtype"
   echo "hint          = $hint"
   echo "workdir       = $workdir"

   ctype=`echo ${mtype} | cut -c3-5`
   nreal_ps=$nreal                 ### one less than the data items of diagnostic files
   nreal2_ps=`expr $nreal - 2`     ### the data items in the grads files 


if [ "$mtype" = 'ps180' -o "$mtype" = 'ps181' -o  "$mtype" = 'ps183' -o  "$mtype" = 'ps187' ]; then
   rm -f diag2grads
   cp $EXECcmon/grads_sfctime.x ./diag2grads
   rm -f input
cat <<EOF >input
      &input
       intype=' ps',stype='${mtype}',itype=$ctype,nreal=$nreal_ps,nreal2=$nreal2_ps,
       iscater=1,igrads=1,timecard='time11',subtype='${subtype}',isubtype=${subtype},
/
EOF
elif [ "$mtype" = 'ps120' ]; then
   rm -f diag2grads
   cp ${EXECcmon}/grads_sfc.x ./diag2grads
   rm -f input
cat <<EOF >input
      &input
       intype=' ps',stype='${mtype}',itype=$ctype,nreal=$nreal_ps,nreal2=$nreal2_ps,
       iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
/
EOF
fi

./diag2grads <input>stdout 2>&1 

rm -f ${mtype}_${subtype}.tmp


##############################################
#  Create the nt file, rename stdout, move nt,
#  grads, and scatter files to $TANDIR_cmon
##############################################
ntline=`tail -n1 stdout`
nt=`echo ${ntline} | sed 's/^ *//g' | sed 's/ *$//g'`
if [ ${#nt} = 1 ]; then
   ntfile="nt_${mtype}_${subtype}.${PDATE}"
   echo ${nt} > ${ntfile}
   cp ${ntfile} ${TANKDIR_cmon}/horz_hist/${cycle}/.
fi
 
mv stdout stdout_diag2grads_${mtype}_${subtype}.$cycle
dest_dir="${TANKDIR_cmon}/horz_hist/${cycle}"

for file in ps*grads; do 
   mv ${file} ${dest_dir}/${file}.${PDATE}
done

for file in ps*scater; do
   mv ${file} ${dest_dir}/${file}.${PDATE}
done


echo "<--  diag2grad_ps_case.sh"

exit
