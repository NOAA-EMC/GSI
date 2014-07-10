*
* script to draw a legend for multiple line plots
*
* (a stupid one - it asks all kinds of questions)
*
function lines(args)
option=subwrd(args,1)
xlow=subwrd(args,2)
ylow=subwrd(args,3)
xhigh=subwrd(args,4)
yhigh=subwrd(args,5)
*
'set vpage off'
while(1)
maxlength = 0

rc=read(option)
dum=sublin(rc,2)
numline=subwrd(dum,1)

i = 1
while (i<=numline)

rc=read(option)
dum=sublin(rc,2)
style.i=subwrd(dum,1)
thick.i=subwrd(dum,2)
color.i=subwrd(dum,3)
mark.i=subwrd(dum,4)
title.i=subwrd(dum,5)

thick.i=4
*title.i=substr(title.i,1,8)           

i = i+1
endwhile

say " "
say "Summary:"
say "-------"
say " "
i = 1
while (i<=numline)
if (style.i=1)
mtype="Solid          "
endif
if (style.i=2)
mtype="Long Dash      "
endif
if (style.i=3)
mtype="Short Dash     "
endif
if (style.i=4)
mtype="Long-Short Dash"
endif
if (style.i=5)
mtype="Dot            "
endif
if (style.i=6)
mtype="Dot-Dash       "
endif
if (style.i=7)
mtype="Dot-Dot-Dash   "
endif
say "Line ("i"): "mtype"  Thickness("thick.i")  Color("color.i")  Mark("mark.i") Legend: "title.i
length = getbits (title.i) 
if( length > maxlength )
maxlength = length
endif
i = i+1
endwhile
*
rc=read(option)
dum=sublin(rc,2)
doit=subwrd(dum,1)

if (doit="y")
 
* Determine Position and Draw Legend Box
* --------------------------------------
*xlow =0.
*ylow =0.
*xhigh=0.
*yhigh=0.
*say "Click on lower left corner of legend box "
*'query pos'
*xlow = subwrd(result,3)
*ylow = subwrd(result,4)
*say "position "xlow" "ylow
*say "Click on upper right corner of legend box "
*'query pos'
*xhigh = subwrd(result,3)
*yhigh = subwrd(result,4)
*say "position "xhigh" "yhigh
 
* White-out Legend Region
* -----------------------
'set line 0'
**'draw recf 'xlow' 'ylow' 'xhigh' 'yhigh
'set line 1'
 
x1spot = xlow   + (xhigh-xlow)/10
x2spot = x1spot + (xhigh-xlow)/3
xtspot = x2spot + (xhigh-xlow)/10
 
'set line 1 1 1'
**'draw rec 'xlow' 'ylow' 'xhigh' 'yhigh

* Write in Legend Titles
* ----------------------

vsize = (yhigh-ylow)/(2.5*numline)
hsize = (xhigh-(xhigh-xlow)/10-xtspot)/maxlength
'set strsiz 'hsize' 'vsize
'set string 1 l 6'

yspace = (yhigh-ylow)/(numline+1)
i = 1
while (i<=numline)
yspot = yhigh - i*yspace
'set  line 'color.i' 'style.i' '6        
'draw line 'x1spot' 'yspot' 'x2spot' 'yspot
'draw mark 'mark.i' 'x1spot' 'yspot' 0.1'
'draw mark 'mark.i' 'x2spot' 'yspot' 0.1'
ytspot = yspot-0.00
'set  ccolor 1'
'draw string 'xtspot' 'ytspot' 'title.i
i = i+1
endwhile
break
else
say "Try again"
endif
endwhile
*
function getbits (string)
tb = ""
i = 1
while (i<=80)
blank = substr(string,i,1)
if( blank = tb )
size = i-1
i = 81
else
i = i + 1
endif
endwhile
return size
