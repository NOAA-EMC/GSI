#!/bin/bash

yyyymmdd=`date --date="2 days ago" +%Y-%m-%d`

cd /home/pub/mirs_operational/scripts/

/home/pub/mirs_operational/scripts/npp_gdas.bash ${yyyymmdd} > \
/disk1/pub/mirs_operational/logs/npp_gdas_log_${yyyymmdd} 2>&1
