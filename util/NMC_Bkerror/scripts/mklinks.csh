set date=2020070100
while ($date <= 2020070100) 
set YYYYMMDD=`echo $date | cut -c1-8`
set HH=`echo $date | cut -c9-10`
set nanal=1
while ($nanal <= 80) 
set charnanal="mem`printf %03i $nanal`"
ln -fs /scratch2/NCEPDEV/stmp1/Jeffrey.S.Whitaker/chgres_cube/enkfgdas.${YYYYMMDD}/${HH}/$charnanal/gdas.t${HH}z.atmf006.nc sfg_${date}_fhr06_${charnanal}
@ nanal = $nanal + 1
end
set date=`incdate $date 24`
end
