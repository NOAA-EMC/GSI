#!/bin/ksh
set -ux

## set up common directories, utilities and environment variables
## for different platforms, and assign user specific parameters.

machine=${1:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
export rc=0


#==================================
## machine-independent parameters
#==================================
export anl_type=gfs            ;#analysis type: gfs, gdas, ecmwf, manl or canl
                                ##gfs/gdas--own anl of each exps, manl--mean in expnlist; canl--mean of GFS,EC and UK.
export sfcvsdb="YES"           ;#include the group of surface variables       
export gd=G2                   ;#grid resoultion on which vsdb stats are computed, G2->2.5deg, G3->1deg, G4->0.5deg
export doftp="NO"             ;#whether or not to send maps to web server
export scppgb="NO"             ;#copy files between machine? need passwordless ssh
export batch="YES"             ;#run jobs at batch nodes                              
export scorecard="YES"          ;#create scorecard text files and web display plate                          
if [ $machine != IBM -a $machine != WCOSS ]; then 
 export doftp="NO"
fi

#==================================
## user-specific parameters
#==================================
if [ $machine = IBM ]; then
 export vsdbsave=/global/noscrub/$LOGNAME/archive/vsdb_data  ;#place where vsdb database is saved
 export ACCOUNT=GFS-MTN                                ;#ibm computer ACCOUNT task
 export CUE2RUN=1                                      ;#dev or devhigh or 1
 export CUE2FTP=1                                      ;#queue for data transfer
 export GROUP=g01                                      ;#group of account, g01 etc
 if [ $doftp = YES ]; then
  export webhost=${webhost:-emcrzdm.ncep.noaa.gov}     ;#host for web display
  export webhostid=${webhostid:-$LOGNAME}              ;#login id on webhost 
  export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}   ;#where maps are displayed on webhost     
 fi 

#----------------------------
elif [ $machine = WCOSS ]; then
 chost=`echo $(hostname) |cut -c 1-1`
 export vsdbsave=/global/noscrub/$LOGNAME/archive/vsdb_data  ;#place where vsdb database is saved
 export ACCOUNT=GFS-MTN                                ;#ibm computer ACCOUNT task
 export CUE2RUN=dev                                    ;#dev or dev_shared         
 export CUE2FTP=transfer                               ;#queue for data transfer
 export GROUP=g01                                      ;#group of account, g01 etc
 if [ $doftp = YES ]; then
  export webhost=${webhost:-emcrzdm.ncep.noaa.gov}     ;#host for web display
  export webhostid=${webhostid:-$LOGNAME}              ;#login id on webhost 
  export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}   ;#where maps are displayed on webhost    
 fi 

#----------------------------
elif [ $machine = ZEUS ]; then
 export VSDBHOME=/scratch2/portfolios/NESDIS/h-sandy/noscrub/Deyong.Xu/vsdb_pkg/vsdb_v17
 export WORKSPACE=/scratch2/portfolios/NESDIS/drt/noscrub/Deyong.Xu/workspace/vsdb_workspace

 export vsdbsave=${WORKSPACE}/data/output/vsdb_data  ;#place where vsdb database is saved
 export ACCOUNT=h-sandy                                  ;#computer ACCOUNT task
 export CUE2RUN=batch                                  ;#default to batch queue
 export CUE2FTP=batch                                  ;#queue for data transfer
 export GROUP=g01                                      ;#group of account, g01 etc
 export doftp="NO"                                     ;#whether or not to sent maps to ftpdir
 if [ $doftp = YES ]; then
  export webhost=${webhost:-emcrzdm.ncep.noaa.gov}     ;#host for web display
  export webhostid=${webhostid:-$LOGNAME}              ;#login id on webhost 
  export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}   ;#where maps are displayed on webhost            
 fi 

#----------------------------
elif [ $machine = BADGER ]; then
 export vsdbsave=/data/users/dxu/vsdb_workspace/data/output/vsdb_data  ;#place where vsdb database is saved
 export ACCOUNT=glbss                                  ;#computer ACCOUNT task
 export CUE2RUN=batch                                  ;#default to batch queue
 export CUE2FTP=batch                                  ;#queue for data transfer
 export GROUP=g01                                      ;#group of account, g01 etc
 export doftp="NO"                                     ;#whether or not to sent maps to ftpdir
 if [ $doftp = YES ]; then
  export webhost=${webhost:-emcrzdm.ncep.noaa.gov}     ;#host for web display
  export webhostid=${webhostid:-$LOGNAME}              ;#login id on webhost 
  export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}   ;#where maps are displayed on webhost            
 fi 

#----------------------------
elif [ $machine = CARDINAL ]; then
 export VSDBHOME=/data/users/dxu/iat/vsdb_pkg/vsdb_v17
 export WORKSPACE=/data/users/dxu/workspace/vsdb_workspace 

 export vsdbsave=${WORKSPACE}/data/output/vsdb_data  ;#place where vsdb database is saved
 export ACCOUNT=glbss                                  ;#computer ACCOUNT task
 export CUE2RUN=batch                                  ;#default to batch queue
 export CUE2FTP=batch                                  ;#queue for data transfer
 export GROUP=g01                                      ;#group of account, g01 etc
 export doftp="NO"                                     ;#whether or not to sent maps to ftpdir
 if [ $doftp = YES ]; then
  export webhost=${webhost:-emcrzdm.ncep.noaa.gov}     ;#host for web display
  export webhostid=${webhostid:-$LOGNAME}              ;#login id on webhost 
  export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}   ;#where maps are displayed on webhost            
 fi 

#----------------------------
elif [ $machine = JIBB ]; then
 export VSDBHOME=/jibb/nobackup/vkrishn1/vsdb_pkg/vsdb_v17
 export WORKSPACE=/jibb/nobackup/vkrishn1/vsdb_workspace

 export vsdbsave=${WORKSPACE}/data/output/vsdb_data
 export ACCOUNT=j1068                                  ;#computer ACCOUNT task
 export CUE2RUN=batch                                  ;#default to batch queue
 export CUE2FTP=batch                                  ;#queue for data transfer
 export GROUP=61068                                      ;#group of account, g01 etc
 export doftp="NO"                                     ;#whether or not to sent maps to ftpdir
 if [ $doftp = YES ]; then
  export webhost=${webhost:-emcrzdm.ncep.noaa.gov}     ;#host for web display
  export webhostid=${webhostid:-$LOGNAME}              ;#login id on webhost
  export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}   ;#where maps are displayed on webhost
 fi

#----------------------------
elif [ $machine = JET ]; then
 export vsdbsave=/pan2/projects/gnmip/$LOGNAME/noscrub/archive/vsdb_data  ;#place where vsdb database is saved
 export ACCOUNT=gnmip                                  ;#computer ACCOUNT task
 export CUE2RUN=hfip                                   ;#default to batch queue
 export CUE2FTP=hfip                                   ;#queue for data transfer
 export GROUP=gnmip                                    ;#group of account, g01 etc
 export doftp="NO"                                     ;#whether or not to sent maps to ftpdir
 if [ $doftp = YES ]; then
  export webhost=${webhost:-emcrzdm.ncep.noaa.gov}     ;#host for web display
  export webhostid=${webhostid:-$LOGNAME}              ;#login id on webhost 
  export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}   ;#where maps are displayed on webhost            
 fi 

#----------------------------
else
 echo "machine $machine is not supportted by NCEP/ECM"
 echo "Please first install the verification package. exit" 
 export rc=1
 exit
fi


#=====================================
## common machine-dependent parameters
#=====================================
if [ $machine = IBM ]; then
 export vsdbhome=/global/save/wx24fy/VRFY/vsdb         ;#script home, do not change
 export obdata=/climate/save/wx24fy/obdata             ;#observation data for making 2dmaps
 export gstat=/global/shared/stat                      ;#global stats directory              
 export gfsvsdb=/climate/save/wx24fy/VRFY/vsdb_data    ;#operational gfs vsdb database
 export canldir=$gstat/canl                            ;#consensus analysis directory
 export ecmanldir=/global/shared/stat/ecm              ;#ecmwf analysis directory
 export OBSPCP=$gstat/OBSPRCP                          ;#observed precip for verification
 export gfswgnedir=$gstat/wgne1                        ;#operational gfs precip QPF scores
 export gfsfitdir=/climate/save/wx23ss                 ;#Suru operational model fit-to-obs database
 export SUBJOB=$vsdbhome/bin/sub_ibm                   ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                        ;#common utilities and libs included in /nwprod
 export GNOSCRUB=/global/noscrub                       ;#archive directory                          
 export STMP=/stmp                                     ;#temporary directory                          
 export PTMP=/ptmp                                     ;#temporary directory                          
 export GRADSBIN=/usrx/local/grads/bin                 ;#GrADS executables       
 export IMGCONVERT=/usrx/local/im_beta/bin/convert     ;#image magic converter
 export FC=/usr/bin/xlf90                              ;#fortran compiler
 export FFLAG=" "                                      ;#fortran compiler options

#----------------------------
elif [ $machine = WCOSS ]; then
 chost=`echo $(hostname) |cut -c 1-1`
 export vsdbhome=/global/save/Fanglin.Yang/VRFY/vsdb        ;#script home, do not change
 export obdata=/global/save/Fanglin.Yang/obdata             ;#observation data for making 2dmaps
 export gstat=/global/noscrub/Fanglin.Yang/stat             ;#global stats directory              
 export gfsvsdb=$gstat/vsdb_data                            ;#operational gfs vsdb database
 export canldir=$gstat/canl                                 ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                                ;#ecmwf analysis directory
 export OBSPCP=$gstat/OBSPRCP                               ;#observed precip for verification
 export gfswgnedir=$gstat/wgne1                             ;#operational gfs precip QPF scores
 export gfsfitdir=/global/save/Suranjana.Saha               ;#Suru operational model fit-to-obs database
 export SUBJOB=$vsdbhome/bin/sub_wcoss                      ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                             ;#common utilities and libs included in /nwprod
 export GNOSCRUB=/global/noscrub                            ;#archive directory                          
 export STMP=/stmpd2                                        ;#temporary directory                          
 export PTMP=/ptmpd2                                        ;#temporary directory                          
 export GRADSBIN=/usrx/local/GrADS/2.0.2/bin                ;#GrADS executables       
 export IMGCONVERT=/usrx/local/ImageMagick/6.8.3-3/bin/convert                ;#image magic converter
 export FC=/usrx/local/intel/composer_xe_2011_sp1.11.339/bin/intel64/ifort    ;#intel compiler
 export FFLAG="-O2 -convert big_endian -FR"                 ;#intel compiler options

#----------------------------
elif [ $machine = ZEUS ]; then
 # VSDB home directory
 export vsdbhome=${VSDBHOME}    ;#script home, do not change

 # step 1
 export gstat=${WORKSPACE}/data/input/gstat    ;#global stats directory
 export canldir=$gstat/canl                                 ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                                ;#ecmwf analysis directory

 # step 2
 export gfsvsdb=${WORKSPACE}/data/output/vsdb_data        ;#operational gfs vsdb database

 # step 3
 export OBSPCP=${WORKSPACE}/data/input/qpf/OBSPRCP        ;#observed precip for verification

 # step 4
 # "$gfswgnedir" is NOT used.
 # "$gstat/wgne1" used explicitly in script.
 export gfswgnedir=$gstat/wgne1                             ;#operational gfs precip QPF scores

 # step 5
 export gfsfitdir=${WORKSPACE}/data/input/f2o             ;#Suru operational model fit-to-obs database
 export obdata=${WORKSPACE}/data/input/plot2d/obdata      ;#observation data for making 2dmaps

 # step 6
 # "$gstat/gfs" used explicitly in script.

 export GNOSCRUB=${WORKSPACE}/data/output/conus_prcp ;#temporary directory
 export STMP=${WORKSPACE}/data/stmp     ;#temporary directory
 export PTMP=${WORKSPACE}/data/ptmp     ;#temporary directory

 export SUBJOB=$vsdbhome/bin/sub_zeus         ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                 ;#common utilities and libs included in /nwprod

 export GRADSBIN=/apps/grads/2.0.a9/bin                     ;#GrADS executables       
 export IMGCONVERT=/apps/ImageMagick/ImageMagick-6.7.6-8/bin/convert  ;#image magic converter
 export FC=/apps/intel/composerxe-2011.4.191/composerxe-2011.4.191/bin/intel64/ifort ;#intel compiler
 export FFLAG="-O2 -convert big_endian -FR"                 ;#intel compiler options

#----------------------------
elif [ $machine = BADGER ]; then
 export vsdbhome=/data/dxu/vsdb/vsdb_v17   ;#script home, do not change 
 export obdata=/data/dxu/vsdb/data/input/plot2d/obdata      ;#observation data for making 2dmaps
 export gstat=/data/dxu/vsdb/data/input/qpf    ;#global stats directory 
 export gfsvsdb=/data/dxu/vsdb/data/output/vsdb_data        ;#operational gfs vsdb database
 export canldir=$gstat/canl                                 ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                                ;#ecmwf analysis directory
 export OBSPCP=/data/dxu/vsdb/data/input/qpf/OBSPRCP        ;#observed precip for verification
 export gfswgnedir=$gstat/wgne1                             ;#operational gfs precip QPF scores
 export gfsfitdir=/data/dxu/vsdb/data/input/f2o             ;#Suru operational model fit-to-obs database
 export SUBJOB=$vsdbhome/bin/sub_badger         ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                 ;#common utilities and libs included in /nwprod
 export GNOSCRUB=/data/dxu/vsdb/data/intermediate  ;#temporary directory  
 export STMP=/data/users/dxu/vsdb_workspace/data/output/stmp     ;#temporary directory    
 export PTMP=/data/users/dxu/vsdb_workspace/data/output/stmp/ptmp     ;#temporary directory   

 export GRADSBIN=/opt/grads/2.0.1-intel-12.1/bin
 export IMGCONVERT=/usr/bin/convert
 export FC=/opt/intel/composer_xe_2011_sp1.10.319/bin/intel64/ifort
 export FFLAG="-O2 -convert big_endian -FR"

#----------------------------
elif [ $machine = CARDINAL ]; then
 # VSDB home directory
 export vsdbhome=${VSDBHOME}    ;#script home, do not change 

 # step 1
 export gstat=${WORKSPACE}/data/input/gstat    ;#global stats directory 
 export canldir=$gstat/canl                                 ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                                ;#ecmwf analysis directory

 # step 2
 export gfsvsdb=${WORKSPACE}/data/output/vsdb_data        ;#operational gfs vsdb database

 # step 3
 export OBSPCP=${WORKSPACE}/data/input/qpf/OBSPRCP        ;#observed precip for verification

 # step 4
 # "$gfswgnedir" is NOT used.
 # "$gstat/wgne1" used explicitly in script.
 export gfswgnedir=$gstat/wgne1                             ;#operational gfs precip QPF scores

 # step 5
 export gfsfitdir=${WORKSPACE}/data/input/f2o             ;#Suru operational model fit-to-obs database
 export obdata=${WORKSPACE}/data/input/plot2d/obdata      ;#observation data for making 2dmaps

 # step 6
 # "$gstat/gfs" used explicitly in script.

 export GNOSCRUB=${WORKSPACE}/data/output/conus_prcp ;#temporary directory  
 export STMP=${WORKSPACE}/data/stmp     ;#temporary directory    
 export PTMP=${WORKSPACE}/data/ptmp     ;#temporary directory   

 export SUBJOB=$vsdbhome/bin/sub_cardinal         ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                 ;#common utilities and libs included in /nwprod

 export GRADSBIN=/opt/grads/2.0.2-precompiled/bin
 export IMGCONVERT=/usr/bin/convert
 export FC=/opt/intel/composer_xe_2013_sp1.2.144/bin/intel64/ifort
 export FFLAG="-O2 -convert big_endian -FR"


#----------------------------
elif [ $machine = JIBB ]; then
 # VSDB home directory
 export vsdbhome=${VSDBHOME}    ;#script home, do not change 

 # step 1
 export gstat=${WORKSPACE}/data/input/qpf
 export canldir=$gstat/canl                                 ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                                ;#ecmwf analysis directory

 # step 2
 export gfsvsdb=${WORKSPACE}/data/output/vsdb_data        ;#operational gfs vsdb database

 # step 3
 export OBSPCP=${WORKSPACE}/data/input/qpf/OBSPRCP        ;#observed precip for verification

 # step 4
 # "$gfswgnedir" is NOT used.
 # "$gstat/wgne1" used explicitly in script.
 export gfswgnedir=$gstat/wgne1                             ;#operational gfs precip QPF scores

 # step 5
 export gfsfitdir=${WORKSPACE}/data/input/f2o             ;#Suru operational model fit-to-obs database
 export obdata=${WORKSPACE}/data/input/plot2d/obdata      ;#observation data for making 2dmaps

 # step 6
 # "$gstat/gfs" used explicitly in script.

 export GNOSCRUB=${WORKSPACE}/data/output/conus_prcp ;#temporary directory  
 export STMP=${WORKSPACE}/data/stmp     ;#temporary directory    
 export PTMP=${WORKSPACE}/data/ptmp     ;#temporary directory   

 export SUBJOB=$vsdbhome/bin/sub_jibb         ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                 ;#common utilities and libs included in /nwprod

 export GRADSBIN=/usr/local/bin
 export IMGCONVERT=/usr/bin/convert
 export FC=/usr/local/intel/Composer/composer_xe_2011_sp1.10.319/bin/intel64/ifort
 export FFLAG="-O2 -convert big_endian -FR"

#----------------------------
elif [ $machine = JET ]; then
 export vsdbhome=/pan2/projects/gnmip/Fanglin.Yang/VRFY/vsdb    ;#script home, do not change
 export obdata=/pan2/projects/gnmip/Fanglin.Yang/VRFY/obdata    ;#observation data for making 2dmaps
 export gstat=/pan2/projects/gnmip/Fanglin.Yang/VRFY/stat       ;#global stats directory              
 export gfsvsdb=$gstat/vsdb_data                            ;#operational gfs vsdb database
 export canldir=$gstat/canl                                 ;#consensus analysis directory
 export ecmanldir=$gstat/ecm                                ;#ecmwf analysis directory
 export OBSPCP=$gstat/OBSPRCP                               ;#observed precip for verification
 export gfswgnedir=$gstat/wgne1                             ;#operational gfs precip QPF scores
 export gfsfitdir=$gstat/surufits                           ;#Suru operational model fit-to-obs database
 export SUBJOB=$vsdbhome/bin/sub_jet                        ;#script for submitting batch jobs
 export NWPROD=$vsdbhome/nwprod                             ;#common utilities and libs included in /nwprod
 export GNOSCRUB=/pan2/projects/gnmip/$LOGNAME/noscrub      ;#temporary directory                          
 export STMP=/pan2/projects/gnmip/$LOGNAME/ptmp             ;#temporary directory                          
 export PTMP=/pan2/projects/gnmip/$LOGNAME/ptmp             ;#temporary directory                          
 export GRADSBIN=/opt/grads/2.0.a2/bin                      ;#GrADS executables       
 export IMGCONVERT=/usr/bin/convert                         ;#image magic converter
 export FC=/opt/intel/Compiler/11.1/072//bin/intel64/ifort  ;#intel compiler
 export FFLAG="-O2 -convert big_endian -FR"                 ;#intel compiler options

fi

