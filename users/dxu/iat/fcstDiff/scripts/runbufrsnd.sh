#!/bin/sh
set -xa
export SUB=/global/save/Dana.Carlis/svn/gfs/EXP-wcoss/para/bin/sub_wcoss
export CDATE=2013082500
export BASEDIR=/global/save/Dana.Carlis/svn/gfs/EXP-postsnd/para
export pid=$$
export COMOUT=/stmp/$USER/bufrsnd.${pid}
export ENDHOUR=120
$SUB -a GFS-MTN -e 'CDATE=$CDATE,COMOUT=$COMOUT,BASEDIR=$BASEDIR,ENDHOUR=$ENDHOUR' -q dev -p 4/1/S  -r 5000/4/4 -t 1:00:00 -j bufr_sounding -o $COMOUT/bufrsnd.out $BASEDIR/ush/postsnd.sh
