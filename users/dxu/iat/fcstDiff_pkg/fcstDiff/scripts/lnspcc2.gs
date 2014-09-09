*
* script to draw a legend for multiple line plots
*
* (a stupid one - it asks all kinds of questions)
*
function lines(args)
option=subwrd(args,1)
'set vpage off'
while(1)
maxlength = 0

if (option!= "")

rc=read(option)
dum=sublin(rc,2)
numline=subwrd(dum,1)

else

say " "
say 'How many lines are in Plot?'
pull numline
say "numline"numline

endif

i = 1
while (i<=numline)

if (option!= "")

rc=read(option)
dum=sublin(rc,2)
style.i=subwrd(dum,1)
thick.i=subwrd(dum,2)
cmark.i=subwrd(dum,3)
color.i=subwrd(dum,4)
title.i=subwrd(dum,5)

else

say " "
say "Style Type:"
say "----------"
say "1 - Solid"
say "2 - Long  Dash" 
say "3 - Short Dash" 
say "4 - Long-Short Dash"
say "5 - Dot"
say "6 - Dot-Dash"
say "7 - Dot-Dot-Dash"
say " "
say "Choose Line #"i"  Style(1-6)  <Thickness>  <Cmark>  <Color>"
pull stuff.i
style.i = getstyle(stuff.i)
thick.i = getthick(stuff.i)
cmark.i = getcmark(stuff.i)
color.i = getcolor(stuff.i)
say "Enter Legend"
pull title.i
endif

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
say "Line ("i"): "mtype"  Thickness("thick.i")  Cmark("cmark.i")  Color("color.i")  Legend: "title.i
length = getbits (title.i) 
if( length > maxlength )
maxlength = length
endif
i = i+1
endwhile
*
if (option!= "")

rc=read(option)
dum=sublin(rc,2)
doit=subwrd(dum,1)

else 

say " "
say "Is this okay? (y/n)"
pull doit

endif

if (doit="y")
 
* Determine Position and Draw Legend Box
* --------------------------------------
xlow =0.
ylow =0.
xhigh=0.
yhigh=0.
say "Click on lower left corner of legend box "
'query pos'
xlow = subwrd(result,3)
ylow = subwrd(result,4)
say "position "xlow" "ylow
say "Click on upper right corner of legend box "
'query pos'
xhigh = subwrd(result,3)
yhigh = subwrd(result,4)
say "position "xhigh" "yhigh
 
* White-out Legend Region
* -----------------------
'set line 0'
'draw recf 'xlow' 'ylow' 'xhigh' 'yhigh
'set line 1'
 
x1spot = xlow   + (xhigh-xlow)/10
x2spot = x1spot + (xhigh-xlow)/2
xtspot = x2spot + (xhigh-xlow)/10
x1spit=x1spot + (xhigh-xlow)/20
x2spit=x2spot - (xhigh-xlow)/20
 
'set line 1 1 1'
*'draw rec 'xlow' 'ylow' 'xhigh' 'yhigh

* Write in Legend Titles
* ----------------------

vsize = (yhigh-ylow)/(2.5*numline)
hsize = 1.0*(xhigh-(xhigh-xlow)/10-xtspot)/maxlength
cmsize = vsize*0.67
'set strsiz 'hsize' 'vsize
'set string 1 l 4'

yspace = (yhigh-ylow)/(numline+1)
i = 1

while (i<=numline)
yspot = yhigh - i*yspace
if( thick.i = 0 )
'set line 0'
else 
'set  line 'color.i' 'style.i' 'thick.i
endif
'draw line 'x1spit' 'yspot' 'x2spit' 'yspot
'set line 'color.i
*'set line 1'
* put markers at each end of line
'draw mark 'cmark.i' 'x1spot' 'yspot' 'cmsize 
'draw mark 'cmark.i' 'x2spot' 'yspot' 'cmsize 
ytspot = yspot-0.00
'draw string 'xtspot' 'ytspot' 'title.i
i = i+1
endwhile

break
else
say "Try again"
endif
endwhile
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

function getstyle (string) 
tb = " "
nl = ""
i = 1 
while (i<=80)
blank = substr(string,i,1)
if( blank = tb | blank = nl ) 
size = i-1
i = 81   
else 
i = i + 1
endif   
endwhile
style = substr(string,1,size)
cmark = 1
thick = 3
return style

function getthick (string) 
size = getbits(string)
if( size = 1 )
thick = 3
else

tb = " "
nl = ""
i = 1 
while (i<=80)
blank = substr(string,i,1)
if( blank = tb | blank = nl ) 
size2 = i-1
i = 81   
else 
i = i + 1
endif   
endwhile
len = size-size2
pos = size2+1
string2 = substr(string,pos,len)

i = 1 
while (i<=80)
blank = substr(string2,i,1)
if( blank != tb ) 
thk1 = i
i = 81   
else 
i = i + 1
endif   
endwhile
i = thk1
while (i<=80)
blank = substr(string2,i,1)
if( blank = tb | blank = nl ) 
thk2 = i-1
i = 81   
else 
i = i + 1
endif   
endwhile
thick = substr(string2,thk1,thk2-thk1+1)

endif
return thick

function getcmark (string) 
size = getbits(string)
if( size = 1 )
cmark = 1
else

tb = " "
nl = ""
i = 1 
while (i<=80)
blank = substr(string,i,1)
if( blank = tb | blank = nl ) 
size2 = i-1
i = 81   
else 
i = i + 1
endif   
endwhile
len = size-size2
pos = size2+1
string2 = substr(string,pos,len)

i = 1 
while (i<=80)
blank = substr(string2,i,1)
if( blank != tb ) 
thk1 = i
i = 81   
else 
i = i + 1
endif   
endwhile
i = thk1
while (i<=80)
blank = substr(string2,i,1)
if( blank = tb | blank = nl ) 
thk2 = i-1
i = 81   
else 
i = i + 1
endif   
endwhile
size2 = getbits(string2)
if( size2-thk2 = 0 )
cmark = 1
else
string3 = substr(string2,thk2+1,size2-thk2)

i = 1 
while (i<=80)
blank = substr(string3,i,1)
if( blank != tb ) 
col1 = i
i = 81   
else 
i = i + 1
endif   
endwhile
i = col1
while (i<=80)
blank = substr(string3,i,1)
if( blank = tb | blank = nl ) 
col2 = i-1
i = 81   
else 
i = i + 1
endif   
endwhile
cmark = substr(string3,col1,col2-col1+1)
endif

endif
return color
