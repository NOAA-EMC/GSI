#!/bin/sh

tmpdir=/stmp/$USER/compfiles
mkdir -p $tmpdir

cd $tmpdir
adate=2012100200
opsdir=/ptmp/$USER/prhy1ops
expdir=/ptmp/$USER/prhy2oro


export filein="pgbanl.gfs.$adate"

cmp $opsdir/$filein $expdir/$filein

export err1=$?
 if [ $err1 -eq 0 ] ; then
   echo " your new post executable generates bit-identical GFS"
 else
 echo " your new post executable did not generate bit-identical "

/u/wx20mi/bin/grbtr ALL $opsdir/$filein ops${filein}.'$TR'
/u/wx20mi/bin/grbtr ALL $expdir/$filein exp${filein}.'$TR'

/global/save/wx20hc/bin/diffgb -x ops${filein}.10 exp${filein}.10>${filein}.10.diff

#/global/save/wx20hc/bin/diffgb -x ops${filein}.2 exp${filein}.2>${filein}.2.diff

#/global/save/wx20hc/bin/diffgb -x ops${filein}.3 exp${filein}.3>${filein}.3.diff

#/global/save/wx20hc/bin/diffgb -x ops${filein}.4 exp${filein}.4>${filein}.4.diff

fi
exit
