#!/bin/bash

yyyymmdd=`date --date="2 days ago" +%Y-%m-%d`

cd /home/pub/mirs_operational/scripts/

/home/pub/mirs_operational/scripts/n18_gdas.bash ${yyyymmdd} > \
/disk1/pub/mirs_operational/logs/n18_gdas_log_${yyyymmdd} 2>&1
