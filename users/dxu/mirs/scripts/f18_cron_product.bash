#!/bin/bash

cd /home/pub/mirs_operational/scripts

yyyymmdd=`date --date '1 days ago' +%Y-%m-%d`

/home/pub/mirs_operational/scripts/f18_product.bash ${yyyymmdd} > \
/disk1/pub/mirs_operational/logs/f18_product_log_${yyyymmdd} 2>&1
