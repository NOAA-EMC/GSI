#!/bin/sh
 set -x

#------------------------------------------------------
#  6-model version, order is s no e k n m;
#  if a model is to be omitted, "no" is used for model name
#  4 times daily cycles
#  same as ../../monthly/WTS16/rmsdie16.4x.sh with slight changes
#------------------------------------------------------

if [ $# -lt 9 ]; then
  echo " Usage: pass in beginning, ending dates,no. of days; 6 model names "
  echo "  order is s x e k n m; subst "no" suppress a model "
  exit 8
fi

 sdate=$1
 edate=$2
 ndays=$3
 mdl1=$4
 mdl2=$5
 mdl3=$6
 mdl4=$7
 mdl5=$8
 mdl6=$9
 numi=$#
 nmdls=` expr $numi - 3 `

 sorcdir=$FHOME/monthly/WTS16
 gdir=$GRADSBIN
 FTPDIR=$HTMLDIR
 datadir=${rundir}/dataarch

 tmpdir=${rundir}/modr16
 tmpdir2=${rundir}/rms16grtest
 tmpdir1=${rundir}/rms161
 tmpgrads=${rundir}/gr16
 tmpek=${rundir}/upd16

 rm -r $tmpdir     2>/dev/null
 rm -r $tmpdir1    2>/dev/null
 rm -r $tmpdir2    2>/dev/null
 rm -r $tmpgrads   2>/dev/null
 rm -r $tmpek      2>/dev/null
 mkdir -p $tmpdir
 mkdir -p $tmpdir1
 mkdir -p $tmpdir2
 mkdir -p $tmpgrads
 mkdir -p $tmpek


 #level=500
 kbad1=0
 jbad1=0
 kbad2=0
 jbad2=0
#to turn msel off, set msel2=0
 msel1=1
 msel2=0
 killt1=0
 kac=0

#-----------------------------------
 cd $tmpdir1 ||exit 8

cp $sorcdir/rmsdie16.f .
xlf $sorcdir/rmsdie16.f -o $tmpdir1/rmsdie16.x

# ----------loop through fields, levels------
# ----               -do 1000,500mb rmsz
   for level in 500 1000
do
     for field in rmszn rmszs rmszcn rmszcs  
do
#   rm $tmpdir1/$field.$level
    rm stdin  2>/dev/null
    kac=0
    echo "$kbad1 $jbad1 $kbad2 $jbad2 $kac" >>stdin
    echo "$msel1 $msel2" >>stdin
    echo "$nmdls" >>stdin
    echo "$killt1" >>stdin

    mcnt=0
     for modl in $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6
  do
     mcnt=` expr $mcnt + 1 `
    ln -fs $datadir/${modl}$level/$field.$edate     fort.1$mcnt
    ln -fs $tmpdir/${modl}$level                    fort.3$mcnt
  done

    ln -fs $tmpdir1/$field.$level                fort.41
    ln -fs $tmpdir2/$field.$level                fort.42
    ln -fs $tmpgrads/$field.$level               fort.43
    ln -fs $tmpek/$field.$level                  fort.44
    ln -fs $tmpgrads/ppl.$field.$level           fort.45

    cat stdin
    $tmpdir1/rmsdie16.x <stdin

done
done

#   loop thru levs and fields---------------------------------
#     formatted output written to 41 to check

   for level in 850 200
do
     for field in rmsv rmsvc mse
do
     rm $tmpdir1/$field.$level.$edate
      rm stdin  2>/dev/null
      kac=0
#     if [ $field = acv ] ; then
#       kac=1
#     fi
      echo "$kbad1 $jbad1 $kbad2 $jbad2 $kac" >>stdin
      echo "$msel1 $msel2" >>stdin
      echo "$nmdls" >>stdin
      echo "$killt1" >>stdin
    mcnt=0
#   for modl in s x e k n
     for modl in $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6
  do
     mcnt=` expr $mcnt + 1 `

    ln -fs $datadir/${modl}$level/$field.$edate     fort.1$mcnt
    ln -fs $tmpdir/${modl}$level                    fort.3$mcnt
  done

    ln -fs $tmpdir1/$field.$level                fort.41
    ln -fs $tmpdir2/$field.$level                fort.42
    ln -fs $tmpgrads/$field.$level               fort.43
    ln -fs $tmpek/$field.$level                  fort.44
    ln -fs $tmpgrads/ppl.$field.$level           fort.45

    cat stdin
    $tmpdir1/rmsdie16.x <stdin

done
done
# ----------end fields,levels loops----------------|


###############################################
# now update control file:
# ---- get sdate group into control file form------
dd=`echo $sdate|cut -c7-8`
echo "day is "$dd
mm=`echo $sdate|cut -c5-6`
echo "month is "$mm
yyyy=`echo $sdate|cut -c1-4`
echo "year is "$yyyy
set -A mname JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
rm changedate
echo "month is "$MM

# ---------------------------------------
cp $sorcdir/rmszdie16.gs .
cp $sorcdir/rmszcdie16.gs .
cp $sorcdir/rmsvedie16.gs .
cp $sorcdir/msedie16.gs .
cp $sorcdir/leg.sxekz .
cp $sorcdir/lnspcc.t62.gs .
cp $sorcdir/rmsdie16.ctl .
cp $sorcdir/lns6.gs .
cp $sorcdir/leg.sxeknm .
cp $FHOME/grads/gradsoff.gs .
cp $FHOME/grads/rgbset.gs .
cp $FHOME/grads/cbarnew.gs .

# ---------------------------------------
#  first assign model names in control file to get ok0,then
#  put proper field in control file and plot each map
# ----------------------------------------
# rm changemod ok*    
#echo "s/modl_1/$mdl1/g" >> changemod
#echo "s/modl_2/$mdl2/g" >> changemod
#echo "s/modl_3/$mdl3/g" >> changemod
#echo "s/modl_4/$mdl4/g" >> changemod
#echo "s/modl_5/$mdl5/g" >> changemod
#echo "s/modl_6/$mdl6/g" >> changemod
 file=rmsdie16.ctl
# sed -f changemod $file >okzero
 cp $file okzero
# ----------------------------------------
#  do rmszdie first
# ----------------------------------------
 rm changefield1 changefield2 out.gr1  2>/dev/null
 echo "s/mod_field/rmszn.500/g" >>changefield1
 echo "s/mod_field/rmszs.500/g" >>changefield2
 sed -f changefield1 okzero >ok1
 sed -f changefield2 okzero >ok2
  $gdir/grads -lbc "run  $sorcdir/rmszdie16.gs \
   $sdate $edate $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6"
# ---------------------------------------
# now do rmsvedie16.gs...
# ----------------------------------------
 rm changefield3 changefield4 out.gr2  2>/dev/null
 echo "s/mod_field/rmsv.850/g" >>changefield3
 echo "s/mod_field/rmsv.200/g" >>changefield4
 sed -f changefield3 okzero >ok3
 sed -f changefield4 okzero >ok4
 $gdir/grads -lbc "run  $sorcdir/rmsvedie16.gs \
  $sdate $edate $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6"
#
# ---------------------------------------
# now do msedie16.gs...
# ----------------------------------------
 rm changefield5 changefield6 out.gr3  2>/dev/null
 echo "s/mod_field/mse.850/g" >>changefield5
 echo "s/mod_field/mse.200/g" >>changefield6
 sed -f changefield5 okzero >ok5
 sed -f changefield6 okzero >ok6
 $gdir/grads -lbc "run  $sorcdir/msedie16.gs \
  $sdate $edate $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6"
# ----------------------------------------
#  do rmszcdie16.gs 
# ----------------------------------------
 rm changefield7 changefield8 out.gr4  2>/dev/null
 echo "s/mod_field/rmszcn.500/g" >>changefield7
 echo "s/mod_field/rmszcs.500/g" >>changefield8
 sed -f changefield7 okzero >ok7
 sed -f changefield8 okzero >ok8
  $gdir/grads -lbc "run  $sorcdir/rmszcdie16.gs \
   $sdate $edate $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6"
# ---------------------------------------
 
echo "finished grads"

#
# ftp to web site:
chmod a+rw *.gif
cat << EOF >ftpin1
  binary
  cd $FTPDIR/html4x/graphics/die
  pwd
    put rmszdie.gif rmszdie.$yyyy.gif
    put rmsvedie.gif rmsvedie.$yyyy.gif
    put msedie.gif msedie.$yyyy.gif
    put rmszcdie.gif rmszcdie.$yyyy.gif

#   cd $FTPDIR/mapdisc.20060105
#   pwd
#   put rmszdie.gif rmszdie.$yyyy.gif
#   put rmsvedie.gif rmsvedie.$yyyy.gif
#   put msedie.gif msedie.$yyyy.gif
#   put rmszcdie.gif rmszcdie.$yyyy.gif
  quit
EOF
ftp -i -v rzdm.ncep.noaa.gov <ftpin1

exit
