function main(inpargs)

'reinit'
_fname = subwrd(inpargs,1)
year = subwrd(inpargs,2)
basin = subwrd(inpargs,3)
period = subwrd(inpargs,4)
datafile = _fname%'.dat'
outfile  = _fname%'.txt'

_cbasin    = 'Hurricane Track Errors - '%basin ' '%year
*_cmodtyp   = 'H207: Coupled HWRF; H023: Atmospheric HWRF; GHYB: GFDL'
_cmodtyp   = ' '%period                     
_reltitle  = '% Error Relative to CLIPER'
_topylabel = 'Average Track Error (nm)'

* Set the upper and lower y-axis bounds for the 
* top and bottom plots....

tvrhi  = 450
tvrlo  =   0
skvrhi =  100
skvrlo = -60

topyincr = 50
botyincr = 10

*------------------------------------------------------*
* +++ NOTHING BELOW HERE SHOULD NEED TO BE CHANGED +++ *
*------------------------------------------------------*

'open '_fname'.ctl'
'enable print '_fname'.meta'
'set display color white'
'clear'

rc = setcols()

* Get number of models by checking the ctl file
* for the number of time levels (each model is 
* stored on a different time level)....

'q file 1'
trec = sublin(result,5)
tsize = subwrd(trec,12)
*nmodels = tsize / 2
nmodels = tsize

'set line 1 1 7'
*'draw line 0 5.6 8.5 5.6'

*-------------------------------------------------
* First, do the top plot, which contains just the
* raw error values....
*-------------------------------------------------

'set vpage 0 8.5 5.55 11.0'

'set xaxis 0 120 12'
'set vrange 'tvrlo' 'tvrhi
'set yaxis 'tvrlo' 'tvrhi' 'topyincr
'set missconn on'
'set grads off'
'set grid horizontal 1 27'
* 'run setlevs.gs'

rc = readmods(datafile,nmodels)

modct = 1
while (modct <= nmodels) 

  'set t 'modct
  modstring = modparms(_modname.modct)
* modcol   = subwrd(modstring,1)
* linetype = subwrd(modstring,2)
* marktype = subwrd(modstring,3)
  modcol   = modct+1
  linetype = solid
  marktype = modct+1
  if (_modname.modct = 'CLIP' | _modname.modct = 'CLP5')
    modcol = 1
  endif


  'set ccolor 'modcol

  if (linetype = 'solid')
    'set cstyle 1'
    'set cthick 7'
  else
    'set cstyle 5'
    'set cthick 7'
  endif

  'set cmark 'marktype
  'set digsize 0.10'
  'd terr'

  if (modct = 1)
    'q gxinfo'
    xdum=sublin(result,3)
    ydum=sublin(result,4)
    xl = subwrd(xdum,4)
    xr = subwrd(xdum,6)
    ylo = subwrd(ydum,4)
    yhi = subwrd(ydum,6)
    ypstart = yhi - 0.15
    ytstart = yhi + 0.5
    ybstart = ylo - 0.40
    ytmp = yhi + ylo
    ymid = ytmp / 2
    xdiff = xr - xl
    xtstart = (xdiff/2) + xl
    xbstart = xl - 0.1
    hsiz = xdiff * 0.013
    vsiz = hsiz + 0.023
    xleg = 0.10
  endif

  pstring = _modname.modct
  rc = plotleg(xleg,ypstart,modct,modcol,linetype,pstring,marktype)

  if (modct = 1)

    xylabst = xbstart - 0.4
    'set string 1 bc 6 90'
    'set strsiz 0.14 0.14'
    'draw string 'xylabst' 'ymid' '_topylabel

    'set string 1 bc 6 0'
    'set strsiz 0.15 0.15'
    'draw string 'xtstart' 'ytstart' '_cbasin

    ytstart = ytstart - 0.3
    'set string 1 bc 6'
    'set strsiz 0.135 0.135'
    'draw string 'xtstart' 'ytstart' '_cmodtyp

    rc = readcases(outfile)
    rc = plotcases(xl,ylo,hsiz,vsiz)

    'set grid off'
    'set xlab off'
    'set ylab off'

  endif

  modct = modct + 1

endwhile

'print'
'disable print'

*'!/net/tpm/grads/linux/gxgif -r -x 672 -y 870 '_fname'.meta -o '_fname'.gif'

*'printim '_fname'.gif x638 y825'
'printim '_fname'.gif x766 y990'
'*/usrx/local/grads/bin/gxps  -c -i '_fname'.meta -o '_fname'.ps'

'quit'

*---------------------------------------------------*
*                                                   *
*---------------------------------------------------*
function plotleg(xpos,yt,modct,pcolor,pstyle,pstring,marktype)

* This function adds another entry onto the legend
* to the left of the plot.

ytmp1 = modct - 1
ytmp2 = ytmp1 * 0.30
ypos  = yt - ytmp2

'set string 'pcolor' l 5'
'set strsiz 0.10 0.13'
'draw string 'xpos' 'ypos' 'pstring

if (pstyle = 'dash') 
  'set line 'pcolor' 5 7'
else
  'set line 'pcolor' 1 4'
endif

xllt = xpos + 0.5
xlrt = xpos + 1.0
'draw line 'xllt' 'ypos' 'xlrt' 'ypos

xcenter = (xllt + xlrt) / 2
'draw mark 'marktype' 'xcenter' 'ypos' 0.11'

return 0


*---------------------------------------------------*
*                                                   *
*---------------------------------------------------*
function plotcases(xl,ylo,hsiz,vsiz)

* This function plots the number of cases under the 
* appropriate hour value on the x-axis.

ystart = ylo - 0.4
xstart = xl - 0.7

'set string 1 l 5'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' #CASES' 

'set string 1 c 6 0'
'set strsiz 'hsiz' 'vsiz

ict = 1
while (ict <= 11)

  if (ict = 6 | ict = 8 | ict = 10)
    ict = ict + 1
    continue
  endif

  'q ll2xy 'ict' 1'
  crec = sublin(result,1)
  xpos = subwrd(crec,1)

  cstr = '('_numcase.ict')'
  'set string 1 c 5'
  'draw string 'xpos' 'ystart' 'cstr

  ict = ict + 1
 
endwhile

return

*---------------------------------------------------*
*                                                   *
*---------------------------------------------------*
function readmods(datafile,nmodels)

* Read in all of the model names from the _fname.dat file.
* This file is a text file that contains the data which is
* actually being plotted from the binary file in this 
* script....

maxmodels = nmodels * 2
ict = 1
while (ict <= maxmodels)

  res = read(datafile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of track/intensity datafile '
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'datafile; endif
    if(rc = 8); say 'rc=8: 'datafile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'datafile; endif
    return 99
  endif

  mrec = sublin(res,2)
  _modname.ict = subwrd(mrec,1)

  ict = ict + 1

endwhile

return


*---------------------------------------------------*
*                                                   *
*---------------------------------------------------*
function readcases(outfile)

* Read the track verification output file to get the 
* number of cases at each forecast hour.

while (1)

  res = read(outfile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of verification output file: 'outfile
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'outfile; endif
    if(rc = 8); say 'rc=8: 'outfile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'outfile; endif
    return 99
  endif

  outrec = sublin(res,2)
  word1  = subwrd(outrec,1)

  if (word1 = '#CASES')
    _numcase.1  = subwrd(outrec,2)
    _numcase.2  = subwrd(outrec,3)
    _numcase.3  = subwrd(outrec,4)
    _numcase.4  = subwrd(outrec,5)
    _numcase.5  = subwrd(outrec,6)
    _numcase.7  = subwrd(outrec,7)
    _numcase.9  = subwrd(outrec,8)
    _numcase.11 = subwrd(outrec,9)
    break
  endif

endwhile

return

*---------------------------------------------------*
*                                                   *
*---------------------------------------------------*
function modparms(cmodel)

* This function returns a string that contains the 
* color for the input model, an indicator for 
* whether the line should be solid or dashed, and
* the type of mark to be used for this model....

* 20 = red
* 21 = green
* 22 = blue
* 23 = greenish-yellow (nogaps)
* 24 = purple (eta track)
* 25 = brown (ncep ensemble mean)
* 26 = pink (ecmwf hrc)

if (cmodel = 'PR32' | cmodel = 'pr32')
  return '20 solid 3'
endif

if (cmodel = 'PR35' | cmodel = 'pr35')
  return '21 dash 4'
endif

if (cmodel = 'PR39' | cmodel = 'pr39')
  return '22 solid 5'
endif

if (cmodel = 'GFDL' | cmodel = 'gfdl')
  return '24 solid 2'
endif

if (cmodel = 'AVNO' | cmodel = 'avno' | cmodel = 'AVNI' | cmodel = 'avni')
  return '25 dash 5'
endif


if (cmodel = 'UKM' | cmodel = 'ukm' | cmodel = 'VBAR' | cmodel = 'vbar')
  return '20 solid 2'
endif
if (cmodel = 'NGPS' | cmodel = 'ngps' | cmodel = 'NGPI' | cmodel = 'ngpi')
  return '21 dash 5'
endif

if (cmodel = 'LBAR' | cmodel = 'lbar' | cmodel = 'SHIP' | cmodel = 'ship')
  return '22 solid 6'
endif
if (cmodel = 'BAMD' | cmodel = 'bamd' | cmodel = 'DSHP' | cmodel = 'dshp')
  return '22 dash 8'
endif

if (cmodel = 'EMX' | cmodel = 'emx' | cmodel = 'EMXI' | cmodel = 'GHYB')
  return '22 solid 1'
endif

if (cmodel = 'UKX' | cmodel = 'ukx' | cmodel = 'UKXI' | cmodel = 'ukxi')
  return '23 dash 8'
endif

if (cmodel = 'BAMM' | cmodel = 'bamm')
  return '24 dash 5'
endif

if (cmodel = 'OFCL' | cmodel = 'H207' | cmodel = 'OFCI' | cmodel = 'ofci')
  return '20 solid 2'
endif
if (cmodel = 'HWRI' | cmodel = 'aemn' | cmodel = 'H007' | cmodel = 'aemi')
  return '25 dash 11'
endif

if (cmodel = 'CLIP' | cmodel = 'clip' | cmodel = 'CLP5' | cmodel = 'clp5')
  return '1 solid 7'
endif
if (cmodel = 'SHFR' | cmodel = 'shfr' | cmodel = 'SHF5' | cmodel = 'shf5')
  return '1 solid 7'
endif

if (cmodel = 'GFDT' | cmodel = 'H023')
  return '22 solid 4'
endif
if (cmodel = 'NGX' | cmodel = 'ngx' | cmodel = 'NGXI' | cmodel = 'ngxi')
  return '26 dash 8'
endif

if (cmodel = 'HCPL' | cmodel = 'HWRF')
  return '23 solid 3'
endif 

return

*---------------------------------------------------*
*                                                   *
*---------------------------------------------------*
function setcols()

* 20 = red
* 21 = bright green
* 22 = blue
* 23 = greenish-yellow (from ens nogaps)
* 24 = bright purple (from old eta track verif)
* 25 = brown (from ens NCEP mean)
* 26 = pink (from ens ECMWF hrc)
* 27 = light grey
* 28 = dark green

'set rgb 20 200   0   0'
'set rgb 21   0 215   0'
'set rgb 22  60 150 255'
'set rgb 23 125 165   0'
'set rgb 24 255   8 235'
'set rgb 25 220 140   7'
'set rgb 26 255   0 255'
'set rgb 27 225 225 225'
'set rgb 28   0 115   0'

return
