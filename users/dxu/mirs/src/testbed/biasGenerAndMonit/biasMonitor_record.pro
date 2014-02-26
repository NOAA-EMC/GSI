@../../../setup/paths_idl.pro

PRO biasMonitor_record, nameList=nameList

;*************************************************************** 
; Read in or control section here
;*************************************************************** 
biasFile='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386/biasCorrec_npp_d20120630_t1359070_e1359386_b03493.dat_gfs'
biasRecord_In='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386/biasRecord_npp_d20120630_t1358350_d20120630_e1359066.dat_gfs'
bias_dir='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386/'
figs_dir='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386'
ctime='20121111111111'
satId='npp'
nwpId=3
version='3035'
fmType=1


readFromList=1
if readFromList eq 1 then begin
  openr,iu,namelist,/get_lun
  readf,iu,format='(a)',  biasFile	   ;Dynamic bias file
  readf,iu,format='(a)',  biasRecord_In	   ;Bias Record file In
  readf,iu,format='(a)',  bias_dir	   ;bias data Dir
  readf,iu,format='(a)',  figs_dir	   ;figs Dir
  readf,iu,format='(a)',  ctime 	   ;Creation time of the bias record file
  readf,iu,format='(a)',  satId  	   ;satellite ID
  readf,iu,format='(i1)', nwpId            ;nwp ID ( 1-gdas, 2-ecmwf, 3-gfs )
  readf,iu,format='(a)',  version          ;MIRS version number
  readf,iu,format='(I)',  fmType           ;resolution
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


if ( nwpId eq 1 ) then nwpStr = '_gdas'
if ( nwpId eq 2 ) then nwpStr = '_ecmw'
if ( nwpId eq 3 ) then nwpStr = '_gfs'

;*************************************************************** 
; Read dynamic bias file and bias record file if it exists
;*************************************************************** 


biases       = fltarr(NPOS,NCHAN)
slopes       = fltarr(NPOS,NCHAN)
intercepts   = fltarr(NPOS,NCHAN)
meanTbsimus  = fltarr(NPOS,NCHAN)
meanTbmeases = fltarr(NPOS,NCHAN)

biases(*,*)       = -999.0
slopes(*,*)       = -999.0
intercepts(*,*)   = -999.0
meanTbsimus(*,*)  = -999.0
meanTbmeases(*,*) = -999.0


date = 0d

;*************************************************************** 
; Dynamic bias file
;*************************************************************** 

    if (satId eq 'npp' ) then begin
        yyyy = strmid(strtrim(file_basename(biasFile)),16,4)
        mm = strmid(strtrim(file_basename(biasFile)),20,2)
        dd = strmid(strtrim(file_basename(biasFile)),22,2)
        hh = strmid(strtrim(file_basename(biasFile)),26,2)
        min = strmid(strtrim(file_basename(biasFile)),28,2)
        ss = strmid(strtrim(file_basename(biasFile)),30,2)
    endif else begin
        yyyy = strmid(strtrim(file_basename(biasFile)),18,4)
        mm = strmid(strtrim(file_basename(biasFile)),13,2)
        dd = strmid(strtrim(file_basename(biasFile)),10,2)
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
    
    date=JULDAY(month,day,year)
    date=date+dayFrac
    ReadBias,biasFile,nchan,npos,cfreq,meanBias,Slope,Intercept,meanTbsimu,meanTbmeas
    biases(*,*)       = meanBias(*,*)
    slopes(*,*)       = Slope(*,*)
    intercepts(*,*)   = Intercept(*,*)
    meanTbsimus(*,*)  = meanTbsimu(*,*)
    meanTbmeases(*,*) = meanTbmeas(*,*)

;*************************************************************** 
; Record bias file
;*************************************************************** 
dummy=''
dummy_int=0
dummy_real=0.

newfileFlag=0
openr,rlun,biasRecord_In,/get_lun,error=open_err
if (open_err ne 0) then begin
    print, 'No Bias record file exists, creating new record...'
    nLines=0
    biasTotal=fltarr(1,NPOS,NCHAN)
    biasTotal[0,*,*]=biases
    julianday=date
    newfileFlag=1
endif else begin
    fmt='(i4,f8.3,' + STRTRIM(STRING(FIX(NPOS)),2) + 'f7.2)'
    readf,rlun,dummy,dummy_int,dummy_int,format='(a3,2(i4,1x))'
    readf,rlun,nLines,format='(i10)'    ;---number of bias records
    print, 'Number of bias records to read from file: ',nLines
    biasTotal=fltarr(nLines+1,NPOS,NCHAN)
    julianday=dblarr(nLines+1)
    temp_arr=fltarr(NPOS)
    temp_dbl=0d
    ;---read bias, date and orbit time info
    for iLine=0,nLines-1 do begin
        readf,rlun,temp_dbl,format='(f16.8)'
        julianday[iLine]=temp_dbl
        for iChan=0,NCHAN-1 do begin
            readf,rlun,dummy_int,dummy_real,temp_arr,format=fmt
            biasTotal[iLine,*,iChan]=temp_arr[0:NPOS-1]
        endfor
    endfor
endelse
;---Fill in latest record
julianday[nLines]=date
biasTotal[nlines,*,*]=biases

;*************************************************************** 
; Determine new Record bias file to write
;***************************************************************
IF (newfileFlag eq 1) THEN BEGIN
    IF (satId eq 'metopA') THEN BEGIN
        yyyy_string=strmid(strtrim(file_basename(biasFile)), 18,4)
        mm_string=strmid(strtrim(file_basename(biasFile)),   23,2)
        dd_string=strmid(strtrim(file_basename(biasFile)),   26,2)
        startDay_string=yyyy_string+mm_string+dd_string
        endDay_string=startDay_string
    ENDIF
    IF (satId ne 'npp' and satId ne 'metopA') THEN BEGIN
        yyyy_string=strmid(strtrim(file_basename(biasFile)), 15,4)
        mm_string=strmid(strtrim(file_basename(biasFile)),   20,2)
        dd_string=strmid(strtrim(file_basename(biasFile)),   23,2)
        startDay_string=yyyy_string+mm_string+dd_string
        endDay_string=startDay_string
    ENDIF
    IF (satId eq 'npp') THEN BEGIN
        startTime_string=strmid(strtrim(file_basename(biasFile)), 26,7)
        endTime_string=strmid(strtrim(file_basename(biasFile)),   35,7)
        startDay_string=strmid(strtrim(file_basename(biasFile)),  16,8)
        endDay_string=startDay_string
    ENDIF
ENDIF
IF (newfileFlag eq 0) THEN BEGIN
    IF (satId eq 'metopA') THEN BEGIN
        yyyy_string=strmid(strtrim(file_basename(biasFile)), 18,4)
        mm_string=strmid(strtrim(file_basename(biasFile)),   23,2)
        dd_string=strmid(strtrim(file_basename(biasFile)),   26,2)
        endDay_string=yyyy_string+mm_string+dd_string
        startDay_string=strmid(strtrim(file_basename(biasRecord_In)), 19,8)
    ENDIF
    IF (satId ne 'npp' and satId ne 'metopA') THEN BEGIN
        yyyy_string=strmid(strtrim(file_basename(biasFile)), 15,4)
        mm_string=strmid(strtrim(file_basename(biasFile)),   20,2)
        dd_string=strmid(strtrim(file_basename(biasFile)),   23,2)
        endDay_string=yyyy_string+mm_string+dd_string
        startDay_string=strmid(strtrim(file_basename(biasRecord_In)), 16,8)
    ENDIF
    IF (satId eq 'npp') THEN BEGIN
        startTime_string=strmid(strtrim(file_basename(biasRecord_In)), 26,7)
        endTime_string=strmid(strtrim(file_basename(biasFile)),        35,7)
        startDay_string=strmid(strtrim(file_basename(biasRecord_In)),  16,8)
        endDay_string=strmid(strtrim(file_basename(biasFile)),         16,8)
    ENDIF
ENDIF

IF (satId ne 'npp') THEN $
  timeSuffix='_D'+startDay_string+'_D'+endDay_string+'_c'+ctime+'.dat'+nwpStr
IF (satId eq 'npp') THEN $
  timeSuffix='_d'+startDay_string+'_t'+startTime_string+'_d'+endDay_string+'_e'+endTime_string+'_c'+ctime+'.dat'+nwpStr

biasRecord_Out=bias_dir+'/biasRecord_'+satId+timeSuffix

;*************************************************************** 
; Write new Record bias file to write
;***************************************************************
fmt='(i4,f8.3,' + STRTRIM(STRING(FIX(NPOS)),2) + 'f7.2)'
openw, wlun, biasRecord_Out,/get_lun
printf,wlun,satId,nchan,npos,format='(a3,2(i4,1x))'
printf,wlun,nLines+1,format='(i10)'
for iLine=0,nLines do begin
    printf,wlun,julianday[iLine],format='(f16.8)'
    for iChan=0,nCHAN-1 do begin
        printf,wlun,ichan,cfreq[ichan],biasTotal[iLine,0:NPOS-1,iChan],format=fmt
    endfor
endfor
free_lun,wlun

;---Check number of Bias records to plot, if <= 1, no exit
if (nLines+1 le 1) then begin
    print, '--------------------------------------------'
    print, 'Not enough Bias records to plot, exiting.'
    print, 'Number of records: ',nLines+1
    print, '--------------------------------------------'
    exit
endif

;*************************************************************** 
; PLOTTING SECTION
;*************************************************************** 
x = julianday
mindiff = 0.00001

min_bias = fltarr(NCHAN)
max_bias = fltarr(NCHAN)
for ichan = 0, NCHAN-1 do begin
  tmp = biasTotal(*,*,ichan)
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

ss = where(FIX(biasTotal)       eq -999 or abs(biasTotal)-0 lt mindiff, cnt)       &  if( cnt GT 0 ) then biasTotal(ss)	     = !VALUES.F_NAN

;*************************************************************** 
; Channel Information
;*************************************************************** 
if satId eq 'n18' or satId eq 'metopA' or satId eq 'n19' then $
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

  IF (ISIZE ne 1) THEN CONTINUE ;---Only make normal sized images

  tickInterval = tickIntervals[ISIZE]
  size_str = size_strs[ISIZE]
  
  for ichan = 0, NCHAN-1 do begin
    if ( satId eq 'f16' or satId eq 'f17' or satId eq 'f18' ) and ichan ge 18 then CONTINUE
    chan = STRTRIM(STRING(ICHAN+1),2)

    png_file = figs_dir + '/mirs_adv_poes_' + satId + '_biasmean_glb_tb_ch' + chan + size_str + timeSuffix + '.png'
    plot_bias, x[sort(x)], biasTotal[sort(x),*,ichan], 'MIRS ' + satName + ' Tb Mean Bias (K) - Ch' + chan + ':' + chan_strs(ichan) + ' (V' + version +')', 'Year',$ 
    	       'Mean Bias (K)', png_file, [min_bias[ichan],max_bias[ichan]], NPOS, tickinterval, ISIZE  

  endfor
  
ENDFOR



device, /close


END
