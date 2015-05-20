#!/bin/sh

set -ax
date
export list=$listvar

export xsize=x800
export ysize=y650
export hint=10    ##(mb) the plot pressure interval press+-hint
gdate`$NDATE -720 $PDATE`

mkdir -p $LOGDIR

### plots destination


### working directory

tmpdir=/stmp/wx20xs/conv_monit/$SUFFIX

#### the list of data type, based on convinfo.txt file

ps_TYPE=" ps120 ps180 ps181 ps182 ps183 ps187 "
q_TYPE=" q120 q130 q132 q133 q180 q181 q182 q183 q187 "
t_TYPE=" t120 t130 t131 t132 t133 t180 t181 t182 t183 t187 "
uv_TYPE=" uv220 uv221 uv223 uv224 uv228 uv229 uv230 uv231 uv232 uv233 uv242 uv243 uv245 uv246 uv247 uv248 uv249 uv250 uv251 uv252 uv253 uv254 uv255 uv256 uv257 uv258 uv280 uv281 uv282 uv284 uv287"

rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf *
mkdir -p $PDATE
cd $PDATE
rm -f *


mkdir -p $TANKDIR/horz_hist/ges
mkdir -p $TANKDIR/horz_hist/anl

#rm -f $TANKDIR/horz_hist/ges/*${gdate}*
#rm -f $TANKDIR/horz_hist/anl/*${gdate}*

for type in ps q t uv
do
eval stype=\${${type}_TYPE}
eval nreal=\${nreal_${type}}
exec=read_${type}

## decoding the dignostic file

for dtype in ${stype}
do

rm -f fileout
for cycle in ges anl
do
cp $DATDIR/diag_conv_${cycle}.${PDATE} conv_diag

/bin/sh  $SCRIPTS/diag2grad_${type}_case.sh $SUFFIX $PDATE $FIXDIR $EXEDIR $cycle $nreal $TANKDIR/horz_hist/$cycle $dtype $hint $tmpdir/$PDATE 

done    #### done with cycle

done   ### done with dtype

done   ### done with type


#### export variables 

export listvar=PDATE,NDATE,DATDIR,TANKDIR,LLQ,WEBDIR,EXEDIR,FIXDIR,LOGDIR,SCRIPTS,GSCRIPTS,CTLDIR,STNMAP,GRADS,USER,SUB,SUFFIX,NPREDR,NCP,PLOT,PREFIX,ACOUNT,nreal_ps,nreal_q,nreal_t,nreal_uv,ps_TYPE,q_TYPE,t_TYPE,uv_TYPE,tmpdir,LOGDIR,hint,WS,WSUSER,xsize,ysize,listvar


##### submit the plot 

$SUB -a $ACOUNT -e $listvar -j plothist -q dev -t 0:55:00 -o $LOGDIR/plothist.log $SCRIPTS/plot_hist.sh  
$SUB -a $ACOUNT -e $listvar -j plothorz -q dev -t 0:55:00 -o $LOGDIR/plothorz.log $SCRIPTS/plot_horz.sh  
$SUB -a $ACOUNT -e $listvar -j plothorz_uv -q dev -t 1:15:00 -o $LOGDIR/plothorz_uv.log $SCRIPTS/plot_horz_uv.sh  



exit

