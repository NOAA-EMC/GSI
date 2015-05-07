#!/bin/ksh 
#set -x
########################################################################################################
# Binbin Zhou, NCEP/EMC, Feb 2005
#      prepg2g.sh is the script to prepare grid2grid verification, doing following steps 
#      after read in user-defined control file: 
#      (1) Check verification types:      
#          case 1: one verification data to verify multiple cycles of previous forecast
#          case 2: one cycle forecast verified by different verification data at different lead time
#      (2) According to the verification type (case 1 or case 2), 
#           (i)  construct the forecast and observation file names according to filename formats set by user
#           (ii) construct the forecast-verification time-macth pair for all forecast times 
#                and store them into a temp file g2g.ctl
#      (3) Search GRIB files for both regular variables and tendency variables. Tendency 
#          computation (3hr, 6hr, 12hr and 24hr) needs GRIB files previous 3, 6, 12, 24 hour before 
#      (4) Thin the GRIB files and concatenate all forecast GRIB into one GRIB file and all
#          verification GRIB files into one GRIB file
#      (5) If Wind speed is specified in the control, then also grab its U and V components
#      (6) Save all headers of user-defined parameters into g2g.ctl
#      usage: prepg2g.sh < user-defined control file
#
# Fanglin Yang, NCEP/EMC, Aug 2006
#      (1) Modified for processing forecasts from different centers
#      (2) replace wgrib with copygb for thining GRIB files.  copygb also allows the change of resolution
# Fanglin Yang, NCEP/EMC, Apr 2011
#      (1) Modified to allow maximum forecast length to change
#    
#########################################################################################################
###############################################################################################
export rundir=$1 
export exp=$2 
export gdtype=$3 
export group=$4 

fname=$exp
#ncep=`echo $exp |cut -c 1-2 `
#if [ $ncep = "pr" ]; then fname=gfs; fi

#
#  specify both forecast and verification GRIB files directory, path and file name
#
export fcstdir=$rundir/$exp/$exp
export obsvdir=$rundir/$exp/$exp

export fhead=$fname
export fgrbtype=pgrbf
export ftm=

export ohead=$fname
export ogrbtype=pgrbanl
if [ $group = "sfc" ]; then 
export ogrbtype=pgrbf00
fi

if [ $fname = "gfs" -a $obtype = "GDAS/2" ]; then 
export ohead=gdas1
export ogrbtype=pgrbf00
fi
export otm=
export otail=

echo  "forecast data format:     " ${fcstdir}".yyyymmdd/"${fhead}".tnnz."${fgrbtype}"fhh"${ftm}
echo  "observation data format:  " ${obsvdir}".yyyymmdd/"${ohead}".tnnz."${ogrbtype}${otm}

ndate=${ndate:-${NWPROD:-/nwprod}/util/exec/ndate}
cpygb=${cpygb:-${NWPROD:-/nwprod}/util/exec/copygb}
gbindex=${gbindex:-${NWPROD:-/nwprod}/util/exec/grbindex}
################################################################################################
################################################################################################
cp $exe/grid#104 $work
cp $exe/regions $work

  rm -f g2g.ctl

# tendency buttons and Cloud base/top case:
tnd03='close'
tnd06='close'
tnd12='close'
tnd24='close'
cloud_base_from_sfc=${cloud_base_from_sfc:-"no"}
lat_weight=${lat_weight:-"yes"}
###########################################


# Now begin to read user-control file #########################################################

 read LINE                     #Header 1
  echo $LINE >> g2g.ctl
 read LINE                     #Header 2
  echo $LINE >> g2g.ctl
  set -A mdl $LINE
  model=${mdl[1]}
  echo "model="$model

# Check case type #############################################################################
 read LINE                     #Header 3
   set -A tfcst $LINE                                                 
     fnum=${tfcst[1]}       ##number of forecast output
     fmin=${tfcst[2]}       ##starting forecast hour
     fmax=${tfcst[3]}       ##maximum forecast hour
     fout=${tfcst[4]}       ##forecast output frequency in hour

   if [ $fmin  -gt 1000000000 ] ; then    #case2:diff lead times from 1 forecast cycle vs diff verified data
     fcst[0]=$fmin       
   else                                   #case1:one verified data vs different previous cycle forecasts
     f[0]=$fmin                               
     t=1
     while [ $t -lt $fnum ]; do
       f[$t]=`expr $fout \* $t + $fmin `       
       t=`expr $t + 1 `         
     done
   fi

# Construct forecast and observation file names for different cases (1 or 2) #####################
# and form tendency file pairs

 read LINE                     #Header 4
   set -A tobsv $LINE

   echo verification time: ${tobsv[1]}
  
#------------------------------------------------------- begin case 1
   if [ ${tobsv[1]} -gt 1000000000 ] ; then    #case1:one verified data  vs different previous cycle forecasts
     cas=1
     obsv[0]=${tobsv[1]}
     oday[0]=`echo ${obsv[0]} | cut -c 1-8`
     obsv03[0]=`$ndate -3 ${obsv[0]}`
     oday03[0]=`echo ${obsv03[0]} | cut -c 1-8`
     obsv06[0]=`$ndate -6 ${obsv[0]}`
     oday06[0]=`echo ${obsv06[0]} | cut -c 1-8`
     obsv12[0]=`$ndate -12 ${obsv[0]}`
     oday12[0]=`echo ${obsv12[0]} | cut -c 1-8`
     obsv24[0]=`$ndate -24 ${obsv[0]}`
     oday24[0]=`echo ${obsv24[0]} | cut -c 1-8`

     to=`echo ${obsv[0]} | cut -c 9-10`                           #obsv cycle
     to03=`echo ${obsv03[0]} | cut -c 9-10`                       #obsv cycle for previous 3  hr
     to06=`echo ${obsv06[0]} | cut -c 9-10`                       #obsv cycle for previous 6  hr
     to12=`echo ${obsv12[0]} | cut -c 9-10`                       #obsv cycle for previous 12 hr
     to24=`echo ${obsv24[0]} | cut -c 9-10`                       #obsv cycle for previous 24 hr

     fileobsv[0]=$obsvdir.${oday[0]}/$ohead.t${to}z.${ogrbtype}${otail}$otm
     fileobsv03[0]=$obsvdir.${oday03[0]}/$ohead.t${to03}z.${ogrbtype}${otail}$otm
     fileobsv06[0]=$obsvdir.${oday06[0]}/$ohead.t${to06}z.${ogrbtype}${otail}$otm
     fileobsv12[0]=$obsvdir.${oday12[0]}/$ohead.t${to12}z.${ogrbtype}${otail}$otm
     fileobsv24[0]=$obsvdir.${oday24[0]}/$ohead.t${to24}z.${ogrbtype}${otail}$otm

     echo fileobsv[0]=${fileobsv[0]}

     nt=`expr $t - 1`                                             # t is # of fcst times $fnum      
     t=0
     echo ${fnum}"  "forecasts:observations >> g2g.ctl    
     fnumreal=0
     while [ $t -le $nt ]
     do
       pass=` $ndate -${f[$t]} ${obsv[0]}`
       fday[$t]=`echo ${pass} | cut -c 1-8`
       fcst[$t]=$pass
       tf=`echo ${pass} | cut -c 9-10`                                    #fcst cycle


       if [ ${f[$t]} -lt 10 ] ; then
         f[$t]='0'${f[$t]}
       fi
       filefcst[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f[$t]}$ftm

       echo filefcst[$t]=${filefcst[$t]}

                                                                                                                                          
       f03[$t]=`expr ${f[$t]} - 3`
       f06[$t]=`expr ${f[$t]} - 6`
       f12[$t]=`expr ${f[$t]} - 12`
       f24[$t]=`expr ${f[$t]} - 24`


       if [ ${f03[$t]} -lt 0 ] ; then
        f03[$t]='NN'
       elif [ ${f03[$t]} -ge 0 ] && [ ${f03[$t]} -lt 10 ] ; then
        f03[$t]='0'${f03[$t]}
       fi

       if [ ${f06[$t]} -lt 0 ] ; then
        f06[$t]='NN'
       elif [ ${f06[$t]} -ge 0 ] && [ ${f06[$t]} -lt 10 ] ; then
        f06[$t]='0'${f06[$t]}
       fi

       if [ ${f12[$t]} -lt 0 ] ; then
        f12[$t]='NN'
       elif [ ${f12[$t]} -ge 0 ] && [ ${f12[$t]} -lt 10 ] ; then
        f12[$t]='0'${f12[$t]}
       fi

       if [ ${f24[$t]} -lt 0 ] ; then
        f24[$t]='NN'
       elif [ ${f24[$t]} -ge 0 ] && [ ${f24[$t]} -lt 10 ] ; then
        f24[$t]='0'${f24[$t]}
       fi


       filefcst03[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f03[$t]}$ftm
       filefcst06[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f06[$t]}$ftm
       filefcst12[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f12[$t]}$ftm
       filefcst24[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f24[$t]}$ftm


       if [ -s ${filefcst[$t]} ] && [ -s  ${fileobsv[0]} ]; then
         fnumreal=`expr $fnumreal + 1 `
         echo ${fday[$t]}${tf}${f[$t]}" "${oday[0]}${to}00" "${fday[$t]}${tf}${f03[$t]}" "${oday03[0]}${to03}00" "${fday[$t]}${tf}${f06[$t]}" "${oday06[0]}${to06}00" "${fday[$t]}${tf}${f12[$t]}" "${oday12[0]}${to12}00" "${fday[$t]}${tf}${f24[$t]}" "${oday24[0]}${to24}00 >> g2g.ctl
       else
         echo forecast file: ${filefcst[$t]}  or observation ${fileobsv[0]} doest not exist
##       echo ${fday[$t]}${tf}${f[$t]}" "${oday[0]}${to}00" "${fday[$t]}${tf}${f03[$t]}" "${oday03[0]}${to03}00" "${fday[$t]}${tf}${f06[$t]}" "${oday06[0]}${to06}00" "${fday[$t]}${tf}${f12[$t]}" "${oday12[0]}${to12}00" "${fday[$t]}${tf}${f24[$t]}" "${oday24[0]}${to24}00 >> g2g.ctl
       fi
        t=`expr $t + 1`
     done

       old="${fnum}  forecasts:observations"              
       new="${fnumreal}  forecasts:observations"              
       sed "s?$old?$new?g" g2g.ctl >g2g.ctl.tmp
       mv g2g.ctl.tmp g2g.ctl
#------------------------------------------------------- end case 1
   else                   #case2:diff lead times from one forecast  vs diferent later-on verified data
#------------------------------------------------------- begin case 2

     cas=2
     fday[0]=`echo ${fcst[0]} | cut -c 1-8`
     tf=`echo ${fcst[0]} | cut -c 9-10`      #fcst cycle

     b[0]=${tobsv[1]}

     b03[0]=`expr ${b[0]} - 03`
     b06[0]=`expr ${b[0]} - 06`
     b12[0]=`expr ${b[0]} - 12`
     b24[0]=`expr ${b[0]} - 24`

     if [ b03[0] -lt 0 ] ; then
       b03[0]='NN'
     elif [ b03[0] -ge 0 ] && [ b03[0] -lt 10 ]; then
       b03[0]='0'${b03[0]}
     fi

     if [ b06[0] -lt 0 ] ; then
       b06[0]='NN'
     elif [ b06[0] -ge 0 ] && [ b06[0] -lt 10 ]; then
       b06[0]='0'${b06[0]}
     fi

     if [ b12[0] -lt 0 ] ; then
       b12[0]='NN'
     elif [ b12[0] -ge 0 ] && [ b12[0] -lt 10 ]; then
       b12[0]='0'${b12[0]}
     fi

     if [ b24[0] -lt 0 ] ; then
       b24[0]='NN'
     elif [ b24[0] -ge 0 ] && [ b24[0] -lt 10 ]; then
       b24[0]='0'${b24[0]}
     fi


     pass=`$ndate +${b[0]} ${fcst[0]}`
     pass03=`$ndate -3 $pass`
     pass06=`$ndate -6 $pass`
     pass12=`$ndate -12 $pass`
     pass24=`$ndate -24 $pass`

     oday[0]=`echo ${pass} | cut -c 1-8`
     obsv[0]=$pass
     oday03[0]=`echo ${pass03} | cut -c 1-8`  #observed day
     obsv03[0]=$pass03                        #observed time(valid time)
     oday06[0]=`echo ${pass06} | cut -c 1-8`  #observed day
     obsv06[0]=$pass06                        #observed time(valid time)
     oday12[0]=`echo ${pass12} | cut -c 1-8`  #observed day
     obsv12[0]=$pass12                        #observed time(valid time)
     oday24[0]=`echo ${pass24} | cut -c 1-8`  #observed day
     obsv24[0]=$pass24                        #observed time(valid time)

     if [ b[0] -lt 10 ] ; then
       b[0]='0'${b[0]}
     fi

     to=`echo ${pass} | cut -c 9-10`       #obsv cycle 
     filefcst[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b[0]}$ftm
     fileobsv[0]=$obsvdir.${oday[0]}/$ohead.t${to}z.${ogrbtype}${otail}$otm

     to03=`echo ${pass03} | cut -c 9-10`   #obsv cycle
     filefcst03[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b03[0]}$ftm
     fileobsv03[0]=$obsvdir.${oday03[0]}/$ohead.t${to03}z.${ogrbtype}${otail}$otm
     to06=`echo ${pass06} | cut -c 9-10`   #obsv cycle
     filefcst06[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b06[0]}$ftm
     fileobsv06[0]=$obsvdir.${oday06[0]}/$ohead.t${to06}z.${ogrbtype}${otail}$otm
     to12=`echo ${pass12} | cut -c 9-10`   #obsv cycle
     filefcst12[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b12[0]}$ftm
     fileobsv12[0]=$obsvdir.${oday12[0]}/$ohead.t${to12}z.${ogrbtype}${otail}$otm
     to24=`echo ${pass24} | cut -c 9-10`   #obsv cycle
     filefcst24[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b24[0]}$ftm
     fileobsv24[0]=$obsvdir.${oday24[0]}/$ohead.t${to24}z.${ogrbtype}${otail}$otm



     if [ -s ${filefcst[0]} ] && [ -s ${fileobsv[0]} ] ; then
         echo ${tobsv[0]}"  "forecasts~Ovservations >> g2g.ctl
         echo ${fday[0]}${tf}${b[0]}" "${oday[0]}${to}00" "${fday[0]}${tf}${b03[0]}" "${oday03[0]}${to03}00" "${fday[0]}${tf}${b06[0]}" "${oday06[0]}${to06}00" "${fday[0]}${tf}${b12[0]}" "${oday12[0]}${to12}00" "${fday[0]}${tf}${b24[0]}" "${oday24[0]}${to24}00 >> g2g.ctl
     else
         echo ${filefcst[0]} or ${fileobsv[0]} not exist
echo ${fday[0]}${tf}${b[0]}" "${oday[0]}${to}00" "${fday[0]}${tf}${b03[0]}" "${oday03[0]}${to03}00" "${fday[0]}${tf}${b06[0]}" "${oday06[0]}${to06}00" "${fday[0]}${tf}${b12[0]}" "${oday12[0]}${to12}00" "${fday[0]}${tf}${b24[0]}" "${oday24[0]}${to24}00 >> g2g.ctl
#         rm -f g2g.ctl
#         exit
     fi

     t=1
     while [ $t -lt ${tobsv[0]} ]
      do
       read LINE
       b[$t]=$LINE
       b03[$t]=`expr ${b[$t]} - 3`
       b06[$t]=`expr ${b[$t]} - 6`
       b12[$t]=`expr ${b[$t]} - 12`
       b24[$t]=`expr ${b[$t]} - 24`

       if [ ${b03[$t]} -lt 0 ] ; then
         b03[$t]='NN'
       elif [ ${b03[$t]} -ge 0 ] && [ ${b03[$t]} -lt 10 ] ; then
        b03[$t]='0'${b03[$t]} 
       fi

       if [ ${b06[$t]} -lt 0 ] ; then
         b06[$t]='NN'
       elif [ ${b06[$t]} -ge 0 ] && [ ${b06[$t]} -lt 10 ] ; then
        b06[$t]='0'${b06[$t]}
       fi

       if [ ${b12[$t]} -lt 0 ] ; then
         b12[$t]='NN'
       elif [ ${b12[$t]} -ge 0 ] && [ ${b12[$t]} -lt 10 ] ; then
        b12[$t]='0'${b12[$t]}
       fi

       if [ ${b24[$t]} -lt 0 ] ; then
         b24[$t]='NN'
       elif [ ${b24[$t]} -ge 0 ] && [ ${b24[$t]} -lt 10 ] ; then
        b24[$t]='0'${b24[$t]}
       fi


       pass=`$ndate +${b[$t]} ${fcst[0]}`
       pass03=`$ndate -3 $pass`
       pass06=`$ndate -6 $pass`
       pass12=`$ndate -12 $pass`
       pass24=`$ndate -24 $pass`

       oday[$t]=`echo ${pass} | cut -c 1-8`
       obsv[$t]=$pass 
       oday03[$t]=`echo ${pass03} | cut -c 1-8`
       obsv03[$t]=$pass03
       oday06[$t]=`echo ${pass06} | cut -c 1-8`
       obsv06[$t]=$pass06
       oday12[$t]=`echo ${pass12} | cut -c 1-8`
       obsv12[$t]=$pass12
       oday24[$t]=`echo ${pass24} | cut -c 1-8`
       obsv24[$t]=$pass24

       if [ b[$t] -lt 10 ] ; then
         b[$t]='0'${b[$t]}
       fi

       to=`echo ${pass} | cut -c 9-10`
       filefcst[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b[$t]}$ftm
       fileobsv[$t]=$obsvdir.${oday[$t]}/$ohead.t${to}z.${ogrbtype}${otail}$otm

       to03=`echo ${pass03} | cut -c 9-10`
       filefcst03[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b03[$t]}$ftm
       fileobsv03[$t]=$obsvdir.${oday03[$t]}/$ohead.t${to03}z.${ogrbtype}${otail}$otm
       to06=`echo ${pass06} | cut -c 9-10`
       filefcst06[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b06[$t]}$ftm
       fileobsv06[$t]=$obsvdir.${oday06[$t]}/$ohead.t${to06}z.${ogrbtype}${otail}$otm
       to12=`echo ${pass12} | cut -c 9-10`
       filefcst12[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b12[$t]}$ftm
       fileobsv12[$t]=$obsvdir.${oday12[$t]}/$ohead.t${to12}z.${ogrbtype}${otail}$otm
       to24=`echo ${pass24} | cut -c 9-10`
       filefcst24[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b24[$t]}$ftm
       fileobsv24[$t]=$obsvdir.${oday24[$t]}/$ohead.t${to24}z.${ogrbtype}${otail}$otm


       if [ -s ${filefcst[$t]} ] && [ -s ${fileobsv[$t]} ] ; then
         echo ${fday[0]}${tf}${b[$t]}" "${oday[$t]}${to}00" "${fday[0]}${tf}${b03[$t]}" "${oday03[$t]}${to03}00" "${fday[0]}${tf}${b06[$t]}" "${oday06[$t]}${to06}00" "${fday[0]}${tf}${b12[$t]}" "${oday12[$t]}${to12}00" "${fday[0]}${tf}${b24[$t]}" "${oday24[$t]}${to24}00 >> g2g.ctl
       else
         echo ${filefcst[$t]} or ${fileobsv[$t]} not exist
echo ${fday[0]}${tf}${b[$t]}" "${oday[$t]}${to}00" "${fday[0]}${tf}${b03[$t]}" "${oday03[$t]}${to03}00" "${fday[0]}${tf}${b06[$t]}" "${oday06[$t]}${to06}00" "${fday[0]}${tf}${b12[$t]}" "${oday12[$t]}${to12}00" "${fday[0]}${tf}${b24[$t]}" "${oday24[$t]}${to24}00 >> g2g.ctl
#         rm -f g2g.ctl
#         exit
       fi

       t=`expr $t + 1`
      done
   fi
#------------------------------------------------------- end case 2

 read LINE                     #Header 5
   echo $LINE >> g2g.ctl
   set -A obtyp $LINE
   loop=${obtyp[0]}
   while [ $loop -gt 1 ]
    do
     read LINE
     echo $LINE >> g2g.ctl
     loop=`expr $loop - 1`
   done

 read LINE                     #Header 6
   echo $LINE >> g2g.ctl
   set -A grdtyp $LINE
   loop=${grdtyp[0]}
   while [ $loop -gt 1 ]
    do
     read LINE
     echo $LINE >> g2g.ctl
     loop=`expr $loop - 1`
   done  

 read LINE                    #Header 7
   echo $LINE >> g2g.ctl
   set -A statyp $LINE
   loop=${statyp[0]}
   while [ $loop -gt 1 ]
    do
     read LINE
     echo $LINE >> g2g.ctl
     loop=`expr $loop - 1`
   done

 read LINE                   #Header 8
   set -A var $LINE
   k5[0]=${var[2]}
   k6[0]=${var[3]}
   k7[0]=${var[4]}

   echo $LINE >> g2g.ctl

   loop=1
   nvar=${var[0]}
   while [ $loop -lt $nvar ]
    do
     read LINE
     set -A var $LINE
     k5[$loop]=${var[1]}
     k6[$loop]=${var[2]}
     k7[$loop]=${var[3]}
     if [ ${k5[$loop]} -eq 0 ] ; then
       echo $LINE not exist
       exit
     fi
     echo $LINE  >> g2g.ctl
     loop=`expr $loop + 1`
   done


 read LINE                      #Header 9
   echo $LINE >> g2g.ctl
   set -A level $LINE
   nl=${level[0]}
   pres[0]=`echo ${level[1]} |cut -c 2- `

   loop=1
   while [ $loop -lt nl ]
    do
     read LINE
     set -A level $LINE
     echo $LINE >> g2g.ctl
     pres[0]=`echo ${level[0]} |cut -c 2- `
     loop=`expr $loop + 1`
   done

 echo $cloud_base_from_sfc >>  g2g.ctl  #Header 10
 echo $lat_weight >> g2g.ctl            #Header 11

#-------------FINISH FILE PREPARATION------------------------
#-------------NO NEED TO CHANGE BELOW------------------------
# Begin to thin the GRIB files #############################################################################################

   rm -f obsv.grib fcst.grib obsv03.grib fcst03.grib obsv06.grib fcst06.grib obsv12.grib fcst12.grib obsv24.grib fcst24.grib
   rm -f obsv.indx fcst.indx obsv03.indx fcst03.indx obsv06.indx fcst06.indx obsv12.indx fcst12.indx obsv24.indx fcst24.indx

   varslp=0

   echo CASE $cas Model: $model 

#------------------------------------------------------- begin case 1
   if [ $cas -eq 1 ] ; then                  # case 1

    echo CASE  1  : One verification time vs diff cycles of  forecasts

     varslp=0
     while [ $varslp -lt $nvar ]             # for all variables
      do
       if [ ${k5[$varslp]} -ne 32 ] ; then   # skip wind vector  

         $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv[0]} obsv.grib
         
         if [ $tnd03 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv03[0]} obsv03.grib; fi
         if [ $tnd06 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv06[0]} obsv06.grib; fi
         if [ $tnd12 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv12[0]} obsv12.grib; fi
         if [ $tnd24 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv24[0]} obsv24.grib; fi

         timelp=0
         while [ $timelp -lt $fnum ] # for all previous forecast cycles
          do  
           if [ -s ${filefcst[$timelp]} ]; then
            $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst[$timelp]} fcst.grib
           fi

          if [ -s ${filefcst03[$timelp]} ] && [ $tnd03 = 'open' ] ; then
            $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst03[$timelp]} fcst03.grib 
          fi
          if [ -s ${filefcst06[$timelp]} ] && [ $tnd06 = 'open' ] ; then
            $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst06[$timelp]} fcst06.grib 
          fi
          if [ -s ${filefcst12[$timelp]} ] && [ $tnd12 = 'open' ] ; then
            $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst12[$timelp]} fcst12.grib 
          fi
          if [ -s ${filefcst24[$timelp]} ] && [ $tnd24 = 'open' ] ; then
            $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst24[$timelp]} fcst24.grib 
          fi

          timelp=`expr $timelp + 1` 
         done
       fi
       varslp=`expr $varslp + 1`
     done

       nvar_1=`expr $nvar - 1`

      if [ ${k5[$nvar_1]} -eq 32 ] ; then  #if vector wind is specfied, must specifiy U and V (no matter if U or V are also spcified

         $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv[0]} obsv.grib
         if [ $tnd03 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv03[0]} obsv03.grib; fi
         if [ $tnd06 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv06[0]} obsv06.grib; fi
         if [ $tnd12 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv12[0]} obsv12.grib; fi
         if [ $tnd24 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv24[0]} obsv24.grib; fi

         $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv[0]} obsv.grib
         if [ $tnd03 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv03[0]} obsv03.grib; fi
         if [ $tnd06 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv06[0]} obsv06.grib; fi
         if [ $tnd12 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv12[0]} obsv12.grib; fi
         if [ $tnd24 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv24[0]} obsv24.grib; fi

         timelp=0
         while [ $timelp -lt $fnum ] # for all previous forecast cycles
         do
           if [ -s ${filefcst[$timelp]} ]; then
            $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst[$timelp]} fcst.grib         
            $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst[$timelp]} fcst.grib         
           fi

           if [ -s ${filefcst03[$timelp]} ] && [ $tnd03 = 'open' ] ; then
            $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst03[$timelp]} fcst03.grib         
            $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst03[$timelp]} fcst03.grib         
           fi
           if [ -s ${filefcst06[$timelp]} ] && [ $tnd06 = 'open' ] ; then
            $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst06[$timelp]} fcst06.grib         
            $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst06[$timelp]} fcst06.grib         
           fi
           if [ -s ${filefcst12[$timelp]} ] && [ $tnd12 = 'open' ] ; then
            $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst12[$timelp]} fcst12.grib         
            $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst12[$timelp]} fcst12.grib         
           fi
           if [ -s ${filefcst24[$timelp]} ] && [ $tnd24 = 'open' ] ; then
            $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst24[$timelp]} fcst24.grib         
            $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst24[$timelp]} fcst24.grib         
           fi

           timelp=`expr $timelp + 1`
         done
      fi

    if [ $cloud_base_from_sfc = "no" ] ; then
     varslp=0
     while [ $varslp -lt $nvar ]             # for cloud base/top, need surface height
      do
      if [ ${k5[$varslp]} -eq 7 ] ; then
         if [ ${k6[$varslp]} -eq 2 ] || [ ${k6[$varslp]} -eq 3 ] ; then
           $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -x  ${filefcst[0]} sfc.grib                    
           $gbindex sfc.grib sfc.indx
           varslp=$nvar
         fi
       fi
       varslp=`expr $varslp + 1`
     done
    fi

#------------------------------------------------------- end case 1
   else                                      # case 2
#------------------------------------------------------- begin case 2

     varslp=0
     while [ $varslp -lt $nvar ]             # for all variable 
       do
       if [ ${k5[$varslp]} -ne 32 ] ; then   # skip wind vector
         timelp=0
         while [ $timelp -lt ${tobsv[0]} ] # for all later-on verfied data
           do
            $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv[$timelp]} obsv.grib 

            if [ $tnd03 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv03[$timelp]} obsv03.grib; fi
            if [ $tnd06 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv06[$timelp]} obsv06.grib; fi
            if [ $tnd12 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv12[$timelp]} obsv12.grib; fi
            if [ $tnd24 = 'open' ] ; then $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${fileobsv24[$timelp]} obsv24.grib; fi

            if [ -s ${filefcst[$timelp]} ]; then
              $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst[$timelp]} fcst.grib 
            fi
            if [ -s ${filefcst03[$timelp]} ] && [ $tnd03 = 'open' ] ; then
              $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst03[$timelp]} fcst03.grib 
            fi
            if [ -s ${filefcst06[$timelp]} ] && [ $tnd06 = 'open' ] ; then
              $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst06[$timelp]} fcst06.grib 
            fi
            if [ -s ${filefcst12[$timelp]} ] && [ $tnd12 = 'open' ] ; then
              $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst12[$timelp]} fcst12.grib 
            fi
            if [ -s ${filefcst24[$timelp]} ] && [ $tnd24 = 'open' ] ; then
              $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -a -x  ${filefcst24[$timelp]} fcst24.grib 
            fi

            timelp=`expr $timelp + 1`
           done
        fi       
           varslp=`expr $varslp + 1`
      done
  
      nvar_1=`expr $nvar - 1`
      if [ ${k5[$nvar_1]} -eq 32 ] ; then  #if vector wind is specfied, also specifiy U and V (no matter if U,V already spcified

        timelp=0
        while [ $timelp -lt ${tobsv[0]} ] # for all later-on verfied data
        do
          if [ -s ${filefcst[$timelp]} ]; then
           $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst[$timelp]} fcst.grib         
           $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst[$timelp]} fcst.grib         
          fi
          if [ -s ${fileobsv[$timelp]} ]; then
           $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv[$timelp]} obsv.grib         
           $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv[$timelp]} obsv.grib         
          fi

           if [ -s ${filefcst03[$timelp]} ] && [ $tnd03 = 'open' ] ; then
             $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst03[$timelp]} fcst03.grib         
             $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst03[$timelp]} fcst03.grib         
           fi
           if [ -s ${filefcst06[$timelp]} ] && [ $tnd06 = 'open' ] ; then
             $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst06[$timelp]} fcst06.grib         
             $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst06[$timelp]} fcst06.grib         
           fi
           if [ -s ${filefcst12[$timelp]} ] && [ $tnd12 = 'open' ] ; then
             $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst12[$timelp]} fcst12.grib         
             $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst12[$timelp]} fcst12.grib         
           fi
           if [ -s ${filefcst24[$timelp]} ] && [ $tnd24 = 'open' ] ; then
             $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${filefcst24[$timelp]} fcst24.grib         
             $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${filefcst24[$timelp]} fcst24.grib         
           fi

           if [ -s ${fileobsv03[$timelp]} ] && [ $tnd03 = 'open' ] ; then
             $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv03[$timelp]} obsv03.grib         
             $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv03[$timelp]} obsv03.grib         
           fi
           if [ -s ${fileobsv06[$timelp]} ] && [ $tnd06 = 'open' ] ; then
             $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv06[$timelp]} obsv06.grib         
             $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv06[$timelp]} obsv06.grib         
           fi
           if [ -s ${fileobsv12[$timelp]} ] && [ $tnd12 = 'open' ] ; then
             $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv12[$timelp]} obsv12.grib         
             $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv12[$timelp]} obsv12.grib         
           fi
           if [ -s ${fileobsv24[$timelp]} ] && [ $tnd24 = 'open' ] ; then
             $cpygb ${gdtype} -i0 -k"4*-1,33,100" -a -x  ${fileobsv24[$timelp]} obsv24.grib         
             $cpygb ${gdtype} -i0 -k"4*-1,34,100" -a -x  ${fileobsv24[$timelp]} obsv24.grib         
           fi

           timelp=`expr $timelp + 1`
        done    
      fi

    if [ $cloud_base_from_sfc = "no" ] ; then
     varslp=0
     while [ $varslp -lt $nvar ]             # for cloud base/top, need surface height
      do
      if [ ${k5[$varslp]} -eq 7 ] ; then
         if [ ${k6[$varslp]} -eq 2 ] || [ ${k6[$varslp]} -eq 3 ] ; then
           $cpygb ${gdtype} -i0 -k"4*-1,${k5[$varslp]},${k6[$varslp]}" -x  ${filefcst[0]} sfc.grib                    
           $gbindex sfc.grib sfc.indx
           varslp=$nvar
         fi
       fi
       varslp=`expr $varslp + 1`
     done
    fi


  fi
#------------------------------------------------------- end case 2

      $gbindex fcst.grib  fcst.indx
      $gbindex obsv.grib obsv.indx
      if [ $tnd03 = 'open' ] ; then
        $gbindex fcst03.grib fcst03.indx
        $gbindex obsv03.grib obsv03.indx
      fi
      if [ $tnd06 = 'open' ] ; then      
       $gbindex fcst06.grib fcst06.indx
       $gbindex obsv06.grib obsv06.indx
      fi
      if [ $tnd12 = 'open' ] ; then
       $gbindex fcst12.grib fcst12.indx
       $gbindex obsv12.grib obsv12.indx
      fi
      if [ $tnd24 = 'open' ] ; then
       $gbindex fcst24.grib fcst24.indx
       $gbindex obsv24.grib obsv24.indx
      fi



   mv g2g.ctl g2g.ctl.$model
   mv fcst.grib fcst.grib.$model 
   mv fcst.indx fcst.indx.$model
   mv obsv.grib obsv.grib.$model
   mv obsv.indx obsv.indx.$model
 if [ $tnd03 = 'open' ] ; then
   mv fcst03.grib fcst03.grib.$model
   mv fcst03.indx fcst03.indx.$model
   mv obsv03.grib obsv03.grib.$model
   mv obsv03.indx obsv03.indx.$model
 fi
 if [ $tnd06 = 'open' ] ; then
   mv fcst06.grib fcst06.grib.$model
   mv fcst06.indx fcst06.indx.$model
   mv obsv06.grib obsv06.grib.$model
   mv obsv06.indx obsv06.indx.$model
 fi
 if [ $tnd12 = 'open' ] ; then
   mv fcst12.grib fcst12.grib.$model
   mv fcst12.indx fcst12.indx.$model
   mv obsv12.grib obsv12.grib.$model
   mv obsv12.indx obsv12.indx.$model
 fi
 if [ $tnd24 = 'open' ] ; then
   mv fcst24.grib fcst24.grib.$model
   mv fcst24.indx fcst24.indx.$model
   mv obsv24.grib obsv24.grib.$model
   mv obsv24.indx obsv24.indx.$model
 fi

exit
