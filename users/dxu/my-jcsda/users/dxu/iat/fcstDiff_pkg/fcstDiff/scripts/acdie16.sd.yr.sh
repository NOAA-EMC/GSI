#!/bin/sh
set -x

#------------------------------------------------------
#  6-model version, order is s no e k n m;
#  if a model is to be omitted, "no" is used for model name
#  standard deviation 
#  same as ../../monthly/WTS16/acdie16.sd.sh with slight changes
#------------------------------------------------------

 set -x
  if [ $# -lt 9 ]; then
    echo " Usage: pass in beginning, ending dates,no. of days; model names "
    exit 9
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

 tmpdir=${rundir}/mod16
 tmpdir1=${rundir}/sum16
 tmpdir2=${rundir}/sum162
 tmpgrads=${rundir}/gradsauto16
 rm -r $tmpdir 2>/dev/null
 rm -r $tmpdir1 2>/dev/null
 rm -r $tmpdir2 2>/dev/null
 rm -r $tmpgrads 2>/dev/null
 mkdir -p $tmpdir
 mkdir -p $tmpdir1
 mkdir -p $tmpdir2
 mkdir -p $tmpgrads


# level=500
 kbad1=0
 jbad1=0
 kbad2=0
 jbad2=0
#to turn msel off, set msel2=0
 msel1=1
 msel2=0
 killt1=0

#-------------------------------------
 cd $tmpdir1 ||exit 8

 xlf  $sorcdir/acdie16.sd.f -o $tmpdir1/acdie16.x

# ----------loop through fields, levels---------------------|

   for level in 500 1000
do
   for field in aczn aczs
do
     rm $tmpdir1/${field}$level.$edate.sum
     rm stdin 2>/dev/null
     echo "$kbad1 $jbad1 $kbad2 $jbad2 $msel1 $msel2" >stdin
     echo "$nmdls $killt1" >>stdin
     cat stdin

     mcnt=0

     for modl in $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6
  do
     mcnt=` expr $mcnt + 1 `
     ln -fs $datadir/${modl}$level/$field.$edate              fort.1$mcnt
     ln -fs $tmpdir/${modl}$level                             fort.3$mcnt
  done

     ln -fs $tmpdir1/${field}$level.$edate.sum          fort.41
     ln -fs $tmpdir2/gradstest.${field}$level.          fort.42
# - next two are for grads - regular dieoff and pan scatterplot
     ln -fs $tmpgrads/die16.${field}$level               fort.43
     ln -fs $tmpgrads/ppl16.${field}$level               fort.44

     $tmpdir1/acdie16.x <stdin
done
done
# --------------------------------------------------------
# ----------loop through fields, levels---------------------|

   for level in 200 850
do
   for field in acv
do
     rm $tmpdir1/${field}$level.$edate.sum
     rm stdin 2>/dev/null
     echo "$kbad1 $jbad1 $kbad2 $jbad2 $msel1 $msel2" >stdin
     echo "$nmdls $killt1" >>stdin
     cat stdin

     mcnt=0

     for modl in $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6
  do
     mcnt=` expr $mcnt + 1 `
     ln -fs $datadir/${modl}$level/$field.$edate              fort.1$mcnt
     ln -fs $tmpdir/${modl}$level                             fort.3$mcnt
  done

     ln -fs $tmpdir1/${field}$level.$edate.sum          fort.41
     ln -fs $tmpdir2/gradstest.${field}$level           fort.42
# - next two are for grads - regular dieoff and pan scatterplot
     ln -fs $tmpgrads/die16.${field}$level               fort.43
     ln -fs $tmpgrads/ppl16.${field}$level               fort.44

     $tmpdir1/acdie16.x <stdin
done
done
# --------------------------------------------------------
# now update control file:
# ---- get sdate group into control file form------
dd=`echo $sdate|cut -c7-8`
echo "day is "$dd
mm=`echo $sdate|cut -c5-6`
echo "month is "$mm
yyyy=`echo $sdate|cut -c1-4`
echo "year is "$yyyy

set -A mname JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
MM=${mname[$mm-1]}
echo "month is "$MM
rm changedmod 2>/dev/null
echo "s/mod_pid/`whoami`/g" >>changedmod
echo "s/mod_ndays/$ndays/g" >> changedmod
echo "s/MOD_DATE/00Z$dd$MM$yyyy/g" >>changedmod
echo "s/m_1/$mdl1/g" >> changedmod
echo "s/m_2/$mdl2/g" >> changedmod
echo "s/m_3/$mdl3/g" >> changedmod
echo "s/m_4/$mdl4/g" >> changedmod
echo "s/m_5/$mdl5/g" >> changedmod
echo "s/m_6/$mdl6/g" >> changedmod
#
# ------------------------------------------------
file1=$sorcdir/dpn1000.ctl
file2=$sorcdir/dps1000.ctl
file3=$sorcdir/dpn500.ctl
file4=$sorcdir/dps500.ctl
sed -f changedmod $file1 >ok1
sed -f changedmod $file2 >ok2
sed -f changedmod $file3 >ok3
sed -f changedmod $file4 >ok4
cp $sorcdir/die5n1000.sd.ctl ok5
cp $sorcdir/die5s1000.sd.ctl ok6
cp $sorcdir/die5n500.sd.ctl ok7
cp $sorcdir/die5s500.sd.ctl ok8
cp $sorcdir/dieacv850.ctl ok9
cp $sorcdir/dieacv200.ctl ok10
#
cp $sorcdir/scat2pan*.gs .
cp $sorcdir/acdie2hem16*.gs .
cp $sorcdir/aczd357.gs .
cp $sorcdir/lns6.gs .
cp $sorcdir/lnsmark.gs .
cp $sorcdir/legscatpan* .
cp $sorcdir/leg.sxeknm .
cp $sorcdir/leg6* .
cp $sorcdir/lnspccay.gs .
cp $sorcdir/acvdie16.gs .
cp $sorcdir/ssplot*.gs .
cp $FHOME/grads/gradsoff.gs .
cp $FHOME/grads/rgbset.gs .
cp $FHOME/grads/cbarnew.gs .

# ---------------------------------------
# first do aczdie2hem16...
# ----------------------------------------
 rm out.gr1 out.ps1 out.gr2 out.ps2   2>/dev/null
###$gdir/grads -lbc "run  $sorcdir/acdie2hem16.gs \
 $gdir/grads -lbc "run  $sorcdir/acdie2hem16.sdec.gs \
   $sdate $edate $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6"
# ---------------------------------------
#  do acv                
# ----------------------------------------
 rm out.gr9 out.ps9 out.gr10 out.ps10   2>/dev/null
 $gdir/grads -lbc "run  $sorcdir/acvdie16.gs \
  $sdate $edate $mdl1 $mdl2 $mdl3 $mdl4 $mdl5 $mdl6"
# ---------------------------------------
# now do  scat2pan.gs (now set for modl1 vs modl3 - EC)
# ----------------------------------------
 rm out.gr23 out.ps23 out.gr24 out.ps24   2>/dev/null
$gdir/grads -lbc "run $sorcdir/scat2pan16.gs \
 1 $ndays $sdate $edate $mdl1 $mdl4"
# ---------------------------------------
# now do ssplot.gs (need "portrait")
# ----------------------------------------
#rm out.gr5 out.ps5 out.gr6 out.ps6   2>/dev/null
#$gdir/grads -pbc "run $sorcdir/ssplot.gs 1 $ndays $mdl1 $mdl3"
$gdir/grads -pbc "run $sorcdir/ssplotbox.gs 1 $ndays $mdl1 $mdl4"
# ---------------------------------------
# finally,  aczd357.gs  (replaces aczd1216.gs) usually s,x,e
# ----------------------------------------
#rm out.gr5 out.ps5 out.gr6 out.ps6   2>/dev/null
$gdir/grads -lbc "run $sorcdir/aczd357.gs 1 $ndays $mdl1 $mdl2 $mdl3"
#
echo "finished grads"

#
# ftp to web site; for  aczdie printim makes all gif files                 
#                  for aczscat, default is 500mb (out.gr23 and 24); also
#                  produced are out.13 and out.14 for 1000mb)
chmod a+rw *.gif
cat << EOF >ftpin1
  binary
  cd $FTPDIR/html/dieyear       
  pwd
  put aczdie16n1.gif aczdie16n1.$yyyy.gif
  put aczdie16s1.gif aczdie16s1.$yyyy.gif
  put aczdie16n5.gif aczdie16n5.$yyyy.gif
  put aczdie16s5.gif aczdie16s5.$yyyy.gif
#
  put aczdie16n1.gif aczdie16n1.year.gif
  put aczdie16s1.gif aczdie16s1.year.gif
  put aczdie16n5.gif aczdie16n5.year.gif
  put aczdie16s5.gif aczdie16s5.year.gif
  put aczscatn.gif aczscatn.$yyyy.gif
  put aczscats.gif aczscats.$yyyy.gif
  put acvdie850.gif acvdie850.$yyyy.gif
  put acvdie200.gif acvdie200.$yyyy.gif
  put allN.gif allN.$yyyy.gif
  put allS.gif allS.$yyyy.gif
  put ssn.gif ssn.$yyyy.gif
  put sss.gif sss.$yyyy.gif
#cd $FTPDIR/mapdisc.20060105
#  pwd
#  put aczdie16n5.gif aczdie16n5.$yyyy.gif
#  put aczdie16s5.gif aczdie16s5.$yyyy.gif
#  put aczdie16n1.gif aczdie16n1.$yyyy.gif
#  put aczdie16s1.gif aczdie16s1.$yyyy.gif
#  put aczscatn.gif aczscatn.$yyyy.gif
#  put aczscats.gif aczscats.$yyyy.gif
#  put acvdie850.gif acvdie850.$yyyy.gif
#  put acvdie200.gif acvdie200.$yyyy.gif
#  put ssn.gif ssn.$yyyy.gif
#  put sss.gif sss.$yyyy.gif
EOF
ftp -i -v rzdm.ncep.noaa.gov <ftpin1


exit
