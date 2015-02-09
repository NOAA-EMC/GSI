* grads script that reads in 2 variable names,
* uses gxout stat to get max and min values,
* sets "min" to the lower of the 2 mins
* sets "max" to the higher of the 2 maxs
* does a 'set vrange min max' so none of
* the two quantities are chopped off of the graph




function setrange (args)


varone=subwrd(args,1)
vartwo=subwrd(args,2)
vthree=subwrd(args,3)
vfour =subwrd(args,4)

*say 'varone is equal to 'varone
*say 'vartwo is equal to 'vartwo

'set gxout stat'
'd 'varone

rec = sublin(result,8)

min1 = subwrd(rec,4)
say 'varone min is 'min1

max1 = subwrd(rec,5)
say 'varone max1 is 'max1

'd 'vartwo

rec = sublin(result,8)

min2 = subwrd(rec,4)
say 'vartwo min is 'min2

max2 = subwrd(rec,5)
say 'vartwo max1 is 'max2

min0 = min1
if (min2<min1); min0=min2; endif;

max0 = max1;
if (max2>max1); max0=max2; endif;


min00 = 1.e15
max00 =-1.e15
if( vthree != '')

'set gxout stat'
'd 'vthree

rec = sublin(result,8)

min1 = subwrd(rec,4)
say 'vthree min is 'min1

max1 = subwrd(rec,5)
say 'vthree max1 is 'max1

'd 'vfour

rec = sublin(result,8)

min2 = subwrd(rec,4)
say 'vfour min is 'min2

max2 = subwrd(rec,5)
say 'vfour max1 is 'max2

min00 = min1
if (min2<min1); min00=min2; endif;

max00 = max1;
if (max2>max1); max00=max2; endif

endif

min = min0
if (min00<min0); min=min00; endif;
max = max0;
if (max00>max0); max=max00; endif

yrange=max-min
dy=0.1*yrange
ymin=min-dy
ymax=max+dy

'set gxout line'
'set vrange 'ymin' 'ymax
say 'set vrange 'ymin' 'ymax

return

