@../../../setup/paths_idl.pro

PRO biasMonitor, nameList=nameList

;*************************************************************** 
; Read in or control section here
;*************************************************************** 
bias_dir='/disk1/pub/mirs_oper/data/SemiStaticData/biasCorrec/'
figs_dir='./'
satId='npp'
nwpId=1
version='2970'
fmType=1

readFromList=1
if readFromList eq 1 then begin
  openr,iu,namelist,/get_lun
  readf,iu,format='(a)', bias_dir	   ;bias data Dir
  readf,iu,format='(a)', figs_dir	   ;figs Dir
  readf,iu,format='(a)', satId  	   ;satellite ID
  readf,iu,format='(i1)', nwpId            ;nwp ID ( 1-gdas, 2-ecmwf, 3-gfs )
  readf,iu,format='(a)', version           ;MIRS version number
  readf,iu,format='(I)', fmType            ;resolution
  close,iu
  free_lun,iu,/force
endif

;--------------------------------------
;     Set sensor-specific items
;--------------------------------------
satName=''
nwpStr=''

if ( satId eq 'n18' ) then begin
    NPOS = 30
    if fmType eq 1 then NPOS = 90
    NCHAN = 20
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    satName = 'N18'
endif

if ( satId eq 'n19' ) then begin
    NPOS = 30
    if fmType eq 1 then NPOS = 90
    NCHAN = 20
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    satName = 'N19'
endif

if ( satId eq 'metopA' ) then begin
    NPOS = 30
    if fmType eq 1 then NPOS = 90
    NCHAN = 20
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    satName = 'MetOp-A'
endif

if ( satId eq 'metopB' ) then begin
    NPOS = 30
    if fmType eq 1 then NPOS = 90
    NCHAN = 20
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
    satName = 'MetOp-B'
endif

if ( satId eq 'f16' ) then begin
    NPOS = 30
    if fmType eq 1 then NPOS = 60
    if fmType eq 2 then NPOS = 90
    if fmType eq 3 then NPOS = 180
    NCHAN = 24
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
    satName = 'F16'
endif

if ( satId eq 'f17' ) then begin
    NPOS = 30
    if fmType eq 1 then NPOS = 60
    if fmType eq 2 then NPOS = 90
    if fmType eq 3 then NPOS = 180
    NCHAN = 24
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
    satName = 'F17'
endif

if ( satId eq 'f18' ) then begin
    NPOS = 30
    if fmType eq 1 then NPOS = 60
    if fmType eq 2 then NPOS = 90
    if fmType eq 3 then NPOS = 180
    NCHAN = 24
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
    satName = 'F18'
endif

if ( satId eq 'npp' ) then begin
    NPOS = 32
    if fmType eq 1 then NPOS = 96
    NCHAN = 22
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]
    satName = 'NPP/ATMS'
endif

if ( satId eq 'trmm' ) then begin
    NPOS = 26
    if fmType eq -1 then NPOS = 26
    if fmType eq  0 then NPOS = 104
    if fmType eq  1 then NPOS = 208
    NCHAN = 9
    Channels2process  = [0,1,2,3,4,5,6,7,8]
    satName = 'TRMM'
endif

if ( satId eq 'gpm' ) then begin
    NPOS = 26
    if fmType eq -1 then NPOS = 26
    if fmType eq  0 then NPOS = 104
    if fmType eq  1 then NPOS = 208
    NCHAN = 13
    Channels2process  = [0,1,2,3,4,5,6,7,8,9,10,11,12]
    satName = 'GPM'
endif

; MTMA proxy data based on CR TMI
if ( satId eq 'mtma' ) then begin
    NPOS = 26
    if fmType eq -1 then NPOS = 27
    if fmType eq  0 then NPOS = 107
    if fmType eq  1 then NPOS = 214
    NCHAN = 9
    Channels2process  = [0,1,2,3,4,5,6,7,8]
    satName = 'MTMA'
endif

; MTSA proxy data based on LR N18
if ( satId eq 'mtsa' ) then begin
    if fmType eq -1 then NPOS = 26
    if fmType eq  0 then NPOS = 65
    if fmType eq  1 then NPOS = 130
    NCHAN = 6
    Channels2process  = [0,1,2,3,4,5]
    satName = 'MTSA'
endif

if ( nwpId eq 1 ) then nwpStr = '_gdas'
if ( nwpId eq 2 ) then nwpStr = '_ecmw'
if ( nwpId eq 3 ) then nwpStr = '_gfs'

;*************************************************************** 
; Search bias files ( filename: biasCorrec_n18_2007_11_04.dat )
;*************************************************************** 
if satId eq 'npp' then begin
  bias_files = FILE_SEARCH(bias_dir,'biasCorrec_'+satId+'_d????????_t???????_e???????_b?????.dat'+nwpStr, COUNT=nfiles_bias)
endif else begin
  bias_files = FILE_SEARCH(bias_dir,'biasCorrec_'+satId+'_????_??_??.dat'+nwpStr, COUNT=nfiles_bias)
endelse

;---- If no files exist with names matching the input arguments, a null scalar string is returned instead of a string array. 
;---- but still one element inside it!!! it's null string inside it. this is a bug in IDL ( STRING    = '' )
if nfiles_bias eq 0 then begin
  print, 'Warning: No biasCorec_* file under ' + bias_dir + ', exit biasMonitor.pro.'
  exit
endif

;nfiles_bias = N_ELEMENTS(bias_files)

biases       = fltarr(nfiles_bias,NPOS,NCHAN)
slopes       = fltarr(nfiles_bias,NPOS,NCHAN)
intercepts   = fltarr(nfiles_bias,NPOS,NCHAN)
meanTbsimus  = fltarr(nfiles_bias,NPOS,NCHAN)
meanTbmeases = fltarr(nfiles_bias,NPOS,NCHAN)

biases(*,*,*)       = -999.0
slopes(*,*,*)       = -999.0
intercepts(*,*,*)   = -999.0
meanTbsimus(*,*,*)  = -999.0
meanTbmeases(*,*,*) = -999.0


date = dblarr(nfiles_bias)

;*************************************************************** 
; Read bias files
;*************************************************************** 
FOR ifile = 0, nfiles_bias - 1 DO BEGIN

    if (satId eq 'npp' ) then begin
        yyyy = strmid(strtrim(file_basename(bias_files(ifile))),41,4,/reverse_offset)
        mm = strmid(strtrim(file_basename(bias_files(ifile))),37,2,/reverse_offset)
        dd = strmid(strtrim(file_basename(bias_files(ifile))),35,2,/reverse_offset)
        hh = strmid(strtrim(file_basename(bias_files(ifile))),31,2,/reverse_offset)
        min = strmid(strtrim(file_basename(bias_files(ifile))),29,2,/reverse_offset)
        ss = strmid(strtrim(file_basename(bias_files(ifile))),27,2,/reverse_offset)
    endif else begin
        yyyy = strmid(strtrim(file_basename(bias_files(ifile))),18,4,/reverse_offset)
        mm = strmid(strtrim(file_basename(bias_files(ifile))),13,2,/reverse_offset)
        dd = strmid(strtrim(file_basename(bias_files(ifile))),10,2,/reverse_offset)
        hh = 0
        min = 0
        ss = 0
    endelse
  
    ;print, yyyy, mm, dd
    year  = double(yyyy)
    month = double(mm)
    day   = double(dd)
    hour   = double(hh)
    minute = double(min)
    second = double(ss)
    
    dayFrac=(hour/24.d)+(minute/1440.d)+(second/86400.d)
    
    date(ifile)=JULDAY(month,day,year)
    date(ifile)=date(ifile)+dayFrac
    ReadBias,bias_files(ifile),nchan,npos,cfreq,meanBias,Slope,Intercept,meanTbsimu,meanTbmeas
    biases(ifile,*,*)       = meanBias(*,*)
    slopes(ifile,*,*)       = Slope(*,*)
    intercepts(ifile,*,*)   = Intercept(*,*)
    meanTbsimus(ifile,*,*)  = meanTbsimu(*,*)
    meanTbmeases(ifile,*,*) = meanTbmeas(*,*)
    
ENDFOR


;*************************************************************** 
; Search model error files
;*************************************************************** 
if satId eq 'npp' then begin
  modelerr_files  = FILE_SEARCH(bias_dir,'ModelErrFile_'+satId+'_d????????_t???????_e???????_b?????.dat'+nwpStr, COUNT=nfiles_modelerr)
endif else begin
  modelerr_files  = FILE_SEARCH(bias_dir,'ModelErrFile_'+satId+'_????_??_??.dat'+nwpStr, COUNT=nfiles_modelerr)
endelse

if nfiles_modelerr eq 0 then begin
  print, 'Warning: No ModelErrFile_* file under ' + bias_dir + ', exit biasMonitor.pro.'
  exit
endif

;nfiles_modelerr = N_ELEMENTS(modelerr_files)

stdvs      = fltarr(nfiles_modelerr,NCHAN) 
stdvs(*,*) = -999.0

;*************************************************************** 
; Read Model Error Files
;*************************************************************** 
FOR ifile = 0, nfiles_modelerr - 1 DO BEGIN
    ReadErr,modelerr_files(ifile),cfreq,nedt,NCHAN,ComputedOrNotFlag
    stdvs(ifile,*) = nedt(*)
ENDFOR


x = date
mindiff = 0.00001

min_bias = fltarr(NCHAN)
max_bias = fltarr(NCHAN)
for ichan = 0, NCHAN-1 do begin
  tmp = biases(*,*,ichan)
  ss = where(tmp gt -999 and tmp ne 0,cnt)
  if cnt gt 0 then begin
    minval = min(tmp(ss))
    maxval = max(tmp(ss))
  endif
  if cnt eq 0 then begin
    minval = -5
    maxval = 5
  endif
  adjustment = (maxval - minval) * 0.1
  min_bias(ichan) = minval - adjustment
  max_bias(ichan) = maxval + adjustment
endfor


min_slope = fltarr(NCHAN)
max_slope = fltarr(NCHAN)
for ichan = 0, NCHAN-1 do begin
  tmp = slopes(*,*,ichan)
  ss = where(tmp gt -999 and tmp ne 0,cnt)
  if cnt gt 0 then begin
    minval = min(tmp(ss))
    maxval = max(tmp(ss))
  endif
  if cnt eq 0 then begin
    minval = -2
    maxval = 2
  endif
  adjustment = (maxval - minval) * 0.1
  min_slope(ichan) = minval - adjustment
  max_slope(ichan) = maxval + adjustment
endfor


min_intercept = fltarr(NCHAN)
max_intercept = fltarr(NCHAN)
for ichan = 0, NCHAN-1 do begin
  tmp = intercepts(*,*,ichan)
  ss = where(tmp gt -999 and tmp ne 0,cnt)
  if cnt gt 0 then begin
    minval = min(tmp(ss))
    maxval = max(tmp(ss))
  endif
  if cnt eq 0 then begin
    minval = -100
    maxval = 100
  endif
  adjustment = (maxval - minval) * 0.1
  min_intercept(ichan) = minval - adjustment
  max_intercept(ichan) = maxval + adjustment
endfor


min_meanTbsimu = fltarr(NCHAN)
max_meanTbsimu = fltarr(NCHAN)
for ichan = 0, NCHAN-1 do begin
  tmp = meanTbsimus(*,*,ichan)
  ss = where(tmp gt 0,cnt)
  if cnt gt 0 then begin
    minval = min(tmp(ss))
    maxval = max(tmp(ss))
  endif
  if cnt eq 0 then begin
    minval = 150
    maxval = 325
  endif
  adjustment = (maxval - minval) * 0.1
  min_meanTbsimu(ichan) = minval - adjustment
  max_meanTbsimu(ichan) = maxval + adjustment
endfor


min_meanTbmeas = fltarr(NCHAN)
max_meanTbmeas = fltarr(NCHAN)
for ichan = 0, NCHAN-1 do begin
  tmp = meanTbmeases(*,*,ichan)
  ss = where(tmp gt 0,cnt)
  if cnt gt 0 then begin
    minval = min(tmp(ss))
    maxval = max(tmp(ss))
  endif
  if cnt eq 0 then begin
    minval = 150
    maxval = 325
  endif
  adjustment = (maxval - minval) * 0.1
  min_meanTbmeas(ichan) = minval - adjustment
  max_meanTbmeas(ichan) = maxval + adjustment
endfor


min_stdv = fltarr(NCHAN)
max_stdv = fltarr(NCHAN)
for ichan = 0, NCHAN-1 do begin
  tmp = stdvs(*,ichan)
  ss = where(tmp gt 0,cnt)
  if cnt gt 0 then begin
    minval = min(tmp(ss))
    maxval = max(tmp(ss))
  endif
  if cnt eq 0 then begin
    minval = 0
    maxval = 2.5
  endif
  adjustment = (maxval - minval) * 0.1
  min_stdv(ichan) = minval - adjustment
  max_stdv(ichan) = maxval + adjustment
endfor


ss = where(FIX(biases)       eq -999 or abs(biases)-0 lt mindiff, cnt)       &  if( cnt GT 0 ) then biases(ss)	     = !VALUES.F_NAN
ss = where(FIX(slopes)       eq -999 or abs(slopes)-0 lt mindiff, cnt)       &  if( cnt GT 0 ) then slopes(ss)	     = !VALUES.F_NAN
ss = where(FIX(intercepts)   eq -999 or abs(intercepts)-0 lt mindiff, cnt)   &  if( cnt GT 0 ) then intercepts(ss)   = !VALUES.F_NAN
ss = where(FIX(meanTbsimus)  eq -999 or abs(meanTbsimus)-0 lt mindiff, cnt)  &  if( cnt GT 0 ) then meanTbsimus(ss)  = !VALUES.F_NAN
ss = where(FIX(meanTbmeases) eq -999 or abs(meanTbmeases)-0 lt mindiff, cnt) &  if( cnt GT 0 ) then meanTbmeases(ss) = !VALUES.F_NAN
ss = where(FIX(stdvs)        eq -999 or abs(stdvs)-0 lt mindiff, cnt)        &  if( cnt GT 0 ) then stdvs(ss)        = !VALUES.F_NAN

;*************************************************************** 
; Channel Information
;*************************************************************** 
if satId eq 'n18' or satId eq 'metopA' or satId eq 'n19' or satId eq 'metopB' then $
  chan_strs=['23V','31V','50V','52V','53H',	  '54H','54V','55H','57H1','57H2',  $
             '57H3','57H4','57H5','57H6','89V1',  '89V2','157H','184H','186H','190H']

if satId eq 'f16' then $
  chan_strs=['50V','52V','53V','54V','55V','57RC','59RC','150H','190H','186H','184H',$
             '19H','19V','22V','37H','37V','91V','91H','63RC','60RC1','60RC2','60RC3','60RC4','60RC5']  	

if satId eq 'f17' then $
  chan_strs=['50H','52H','53H','54H','55H','57RC','59RC','150H','190H','186H','184H',$
             '19H','19V','22V','37H','37V','91V','91H','63RC','60RC1','60RC2','60RC3','60RC4','60RC5']  	

if satId eq 'f18' then $
  chan_strs=['50H','52H','53H','54H','55H','57RC','59RC','150H','190H','186H','184H',$
             '19H','19V','22V','37H','37V','91V','91H','63RC','60RC1','60RC2','60RC3','60RC4','60RC5']  	

if satId eq 'npp' then $
  chan_strs=['23V','31V','50H','51H','52H','53H','54H1','54H2','55H','57H1','57H2',  $
             '57H3','57H4','57H5','57H6','88V','165H','183H1','183H2','183H3','183H4','183H5']

;*************************************************************** 
; Plot bias
;*************************************************************** 

tickIntervals = [6,6,18]
size_strs = ['_small', '', '_big' ]

FOR ISIZE = 0, 2 DO BEGIN

  tickInterval = tickIntervals[ISIZE]
  size_str = size_strs[ISIZE]
  
  for ichan = 0, NCHAN-1 do begin
    if ( satId eq 'f16' or satId eq 'f17' or satId eq 'f18' ) and ichan ge 18 then CONTINUE
    chan = STRTRIM(STRING(ICHAN+1),2)

    png_file = figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_biasmean_glb_tb_ch' + chan + size_str + '.png'
    plot_bias, x, biases[*,*,ichan], 'MIRS ' + satName + ' Tb Mean Bias (K) - Ch' + chan + ':' + chan_strs(ichan) + ' (V' + version +')', 'Year',$ 
    	       'Mean Bias (K)', png_file, [min_bias[ichan],max_bias[ichan]], NPOS, tickinterval, ISIZE  
  
    png_file = figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_biasslope_glb_tb_ch' + chan + size_str + '.png'
    plot_bias, x, slopes[*,*,ichan], 'MIRS ' + satName + ' Tb Bias Slopes - Ch' + chan + ':' + chan_strs(ichan) +  ' (V' + version +')', 'Year',$ 
    	       'Slopes', png_file, [min_slope[ichan],max_slope[ichan]], NPOS, tickinterval, ISIZE   
  
    png_file = figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_biasintercept_glb_tb_ch' + chan + size_str +'.png'
    plot_bias, x, intercepts[*,*,ichan], 'MIRS ' + satName + ' Tb Bias Intercepts - Channel ' + chan + ':' + chan_strs(ichan) + ' (V' + version +')', 'Year',$ 
    	       'Intercepts',    png_file, [min_intercept[ichan],max_intercept[ichan]], NPOS, tickinterval, ISIZE   
  
    png_file = figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_tbSimu_glb_tb_ch' + chan + size_str + '.png'
    plot_bias, x, meanTbsimus[*,*,ichan], 'MIRS ' + satName + ' Simulated Tb (K) - Ch' + chan + ':' + chan_strs(ichan) + ' (V' + version +')', 'Year',$ 
    	       'Mean TB Simulated (K)',    png_file, [min_meanTbsimu[ichan],max_meanTbsimu[ichan]], NPOS, tickinterval, ISIZE   
  
    png_file = figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_tbMeas_glb_tb_ch' + chan + size_str + '.png'
    plot_bias, x, meanTbmeases[*,*,ichan], 'MIRS ' + satName + ' Measured Tb (K) - Ch' + chan + ':' + chan_strs(ichan) + ' (V' + version +')', 'Year',$ 
    	       'Mean TB Measured (K)',    png_file, [min_meanTbmeas[ichan],max_meanTbmeas[ichan]], NPOS, tickinterval, ISIZE  

  endfor
  
ENDFOR


;***************************************************************************************************
; Standard Deviation Plot
;***************************************************************************************************

set_plot,  'z'
device, set_resolution=[650,500]

TVLCT, 0,0,0,  1 ; black
TVLCT, 255,255,255, 255 ; white
TVLCT, 255,0,0,       2 ; red
TVLCT, 0,255,0,       3 ; green
TVLCT, 0,0,255,       4 ; blue

TVLCT, 0,255,255,   5
TVLCT, 255,0,255,   6
TVLCT, 255,128,0,   7
TVLCT, 128,128,128, 8

dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])
position=[0.1, 0.1, 0.95, 0.95]
charsz=0.75

for isize = 0, 2 do begin
for ichan = 0, NCHAN-1 do begin
  if (satId eq 'f16' and ichan ge 18) then CONTINUE
  chan = STRTRIM(STRING(ICHAN+1),2)
 
  if isize eq 0 then begin
    png_file=figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_stdv_glb_tb_ch' + chan + '_small.png'
    device, set_resolution=[325,250]
    position=[0.15, 0.15, 0.95, 0.925]
    charsz=0.6
    tickInterval=6
  endif
  
  if isize eq 1 then begin
    png_file=figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_stdv_glb_tb_ch' + chan + '.png'
    device, set_resolution=[650,500]
    position=[0.1, 0.15, 0.95, 0.95]
    charsz=1.0
    tickInterval=6
  endif
  
  if isize eq 2 then begin
    png_file=figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_stdv_glb_tb_ch' + chan + '_big.png'
    device, set_resolution=[3000,500]
    position=[0.1, 0.1, 0.95, 0.95]
    charsz=1.0
    tickInterval=18
  endif
  
  yrange=[ min_stdv(ichan), max_stdv(ichan) ]
  title='MIRS '+satName+' Standard Deviation - Ch' + chan + ':' + chan_strs(ichan) + ' (V' + version +')'
  
  plot,date,stdvs(*,ichan),title=title,ytitle='Standard Deviation', $
       yrange=yrange,position=position, XTICKUNITS=['Day'],XTICKFORMAT='LABEL_DATE', $
       /nodata,xstyle=1,ystyle=1,CHARSIZE=charsz,color=1,background=255,xticks=tickInterval
  oplot,date,stdvs(*,ichan),COLOR=4
  
  TVLCT, r, g, b, /Get
  Write_PNG, png_file, TVRD(), r, g, b

endfor
endfor

device, /close


END
