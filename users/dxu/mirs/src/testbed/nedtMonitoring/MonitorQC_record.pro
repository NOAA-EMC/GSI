@../../../setup/paths_idl.pro


;*******************************************************************************
; NAME: MonitorQC_record
;
; TYPE: IDL Program
;
; DESCRIPTION: 
;
;     Used ot monitor the MiRS QC summary flags and convergence, by
;     reading in flags from a single granule, orbit, or daily average
;     and appending it to a record of flags (master record) and also
;     to create time series images of the qc record.
;
;
; ARGUMENTS:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- namelistNedt	      I		    namelist of input
;
;
; HISTORY:
;
;     2012-08-31  Kevin Garrett Original coder
;
;
;*******************************************************************************

PRO MonitorQC_record, nameList=namelist

;*******************************************************************************
;    some variables initialization
;*******************************************************************************

satId='npp'

;qcFile='/home/pub/kgarrett/qc_d20120630_t1358350_e1359066/QC_MONS_npp_d20120630_t1358350_e1359066_b03493_c20120630201712265900_noaa_ops.h5.HR.ORB'
;qcRecord_In='/home/pub/kgarrett/qc_d20120630_t1358350_e135906/qcRecord_npp_d20120630_t1358350_d20120630_e1359066.dat'
;qcPath='/home/pub/kgarrett/qc_d20120630_t1358350_e1359066'
;figsDir='/home/pub/kgarrett/qc_d20120630_t1358350_e1359066/'

qcFile='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386/QC_MONS_npp_d20120630_t1359070_e1359386_b03493_c20120630201712265900_noaa_ops.h5.HR.ORB'
qcRecord_In='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386/qcRecord_npp_d20120630_t1358350_d20120630_e1359066.dat'
qcPath='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386'
ctime='20121111111111'
figsDir='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386/'

;---- read from namelist or not
fromNameList=1
if ( fromNameList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId		       	;Satellite ID
  readf,iu,format='(a)', qcFile		       	;qc File from granule/orbit
  readf,iu,format='(a)', qcRecord_In       	;qc File containing record
  readf,iu,format='(a)', qcPath  	       	;path where to write QC record
  readf,iu,format='(a)', ctime                  ;creation time of the new QC record file
  readf,iu,format='(a)', figsDir  	       	;path where to put image files
  free_lun,iu,/force
endif

print, ''
print, 'MonitorQC_record.pro ...'
print, 'satId='+satId
print, 'qcFile='+qcFile
print, 'qcRecord_In='+qcRecord_In
print, 'figsDir='+figsDir

satTitle=STRUPCASE(satId)
if satId eq 'npp' then satTitle='NPP/ATMS'
dummy=''

;---Create unique symbol for plotting
!p.charsize   = 0.75
csize         = 16
a             = findgen(csize+1) *(!pi*2./float(csize))
usersym,cos(a)/2,sin(a)/2,/fill
symsize    = 0.8

;---Create unique prefix for output image names
prefix='mirs_adv_poes_'+satId+'_amsuamhs__'
if satId eq 'f16' then prefix='mirs_adv_dmsp_'+satId+'_ssmis_'
if satId eq 'f17' then prefix='mirs_adv_dmsp_'+satId+'_ssmis_'
if satId eq 'f18' then prefix='mirs_adv_dmsp_'+satId+'_ssmis_'
if satId eq 'npp' then prefix='mirs_adv_npp_'+satId+'_atms_'

nqc=4
newfileFlag=0
;----read data record file if it exists, if not, start new record.
openr,rlun,qcRecord_In,/get_lun,error=open_err
if (open_err ne 0) then begin
    print, 'No QC record file exists, creating new record...'
    nLines=0
    qcTotal=fltarr(nLines+1,nqc)
    year=''
    julianday=0d
    newfileFlag=1
endif else begin
    readf,rlun,dummy
    readf,rlun,nLines ;---number of qc records
    qcTotal=fltarr(nLines+1,nqc)
    year=strarr(nLines+1)
    julianday=dblarr(nLines+1)
    temp_str=''
    temp_arr=fltarr(nqc)
    temp_dbl=0d
    ;---read qc, date and orbit time info
    for iLine=0,nLines-1 do begin
        readf,rlun,temp_str,temp_dbl,temp_arr,format='(i4,1x,f9.5,1x,4(f7.4,1x))'
        qcTotal[iLine,*]=temp_arr[0:nqc-1]
        year[iLine]=temp_str
        julianday[iLine]=temp_dbl
    endfor
endelse

;*******************************************************************************
;   READ DATA AND FILL ARRAYS
;*******************************************************************************
openr,rlun,qcFile,/get_lun
readf,rlun,format='(i4,1x,f9.5,1x,4(f7.4,1x))',var1,var2,var3,var4,var5,var6
year(nLines)        = var1
julianday(nLines)   = var2
qcTotal(nLines,0)   = var3
qcTotal(nLines,1)   = var4
qcTotal(nLines,2)   = var5
qcTotal(nLines,3)   = var6
free_lun,rlun

;---- filter out <= 0 values ----
ss        = where(julianday gt 0, cnt)
year      =  year(ss)
percQC0   =  qcTotal(ss,0)*100
percQC1   =  qcTotal(ss,1)*100
percQC2   =  qcTotal(ss,2)*100
Conv_Rate =  qcTotal(ss,3)*100
Jul_day   =  julianday(ss)
;Cal_day   =  Cal_day_tmp(ss)
;day_frac  =  day_frac_tmp(ss)

;---Get start and end times for record
IF (newfileFlag eq 1) THEN BEGIN
    IF (satId ne 'npp') THEN BEGIN
        startTime_string=strmid(strtrim(file_basename(qcFile)), 18,4)
        endTime_string=strmid(strtrim(file_basename(qcFile)),   24,4)
        startDay_string=strmid(strtrim(file_basename(qcFile)),  11,5)
        endDay_string=startDay_string
    ENDIF
    IF (satId eq 'npp') THEN BEGIN
        startTime_string=strmid(strtrim(file_basename(qcFile)), 23,7)
        endTime_string=strmid(strtrim(file_basename(qcFile)),   32,7)
        startDay_string=strmid(strtrim(file_basename(qcFile)),  13,8)
        endDay_string=startDay_string
    ENDIF
ENDIF
IF (newfileFlag eq 0) THEN BEGIN
    IF (satId eq 'metopA') THEN BEGIN
        startTime_string=strmid(strtrim(file_basename(qcRecord_In)), 24,4)
        endTime_string=strmid(strtrim(file_basename(qcFile)),        24,4)
        startDay_string=strmid(strtrim(file_basename(qcRecord_In)),  17,5)
        endDay_string=strmid(strtrim(file_basename(qcFile)),         11,5)
    ENDIF
    IF (satId ne 'npp' and satId ne 'metopA') THEN BEGIN
        startTime_string=strmid(strtrim(file_basename(qcRecord_In)), 21,4)
        endTime_string=strmid(strtrim(file_basename(qcFile)),        24,4)
        startDay_string=strmid(strtrim(file_basename(qcRecord_In)),  14,5)
        endDay_string=strmid(strtrim(file_basename(qcFile)),         11,5)
    ENDIF
    IF (satId eq 'npp') THEN BEGIN
        startTime_string=strmid(strtrim(file_basename(qcRecord_In)), 24,7)
        endTime_string=strmid(strtrim(file_basename(qcFile)),       32,7)
        startDay_string=strmid(strtrim(file_basename(qcRecord_In)), 14,8)
        endDay_string=strmid(strtrim(file_basename(qcFile)),        13,8)
    ENDIF
ENDIF

IF (satId ne 'npp') THEN $
  timeSuffix='_D'+startDay_string+'_S'+startTime_string+'_D'+endDay_string+'_E'+endTime_string+'_c'+ctime+'.dat'
IF (satId eq 'npp') THEN $
  timeSuffix='_d'+startDay_string+'_t'+startTime_string+'_d'+endDay_string+'_e'+endTime_string+'_c'+ctime+'.dat'

qcRecord_Out=qcPath+'/qcRecord_'+satId+timeSuffix
print,qcRecord_Out

openw,wlun,qcRecord_Out,/get_lun
printf,wlun,satId,format='(a3)'
printf,wlun,n_elements(julianday),format='(i10)'
for iLine=0,nLines do begin
    printf,wlun,year[iLine],julianday[iLine],qcTotal[iLine,*],format='(i4,1x,f9.5,1x,4(f7.4,1x))'
endfor
free_lun,wlun

IF (n_elements(year) lt 2) THEN BEGIN
    print, '--------------------------------------------'
    print, 'Not enough NEDT records to plot, exiting.'
    print, 'Number of records: ',n_elements(year)
    print, '--------------------------------------------'
    exit
ENDIF


;*******************************************************************************
;   PLOT SECTION 
;*******************************************************************************
PLOT_CONVERGENCE=1
PLOT_QC=1

set_plot,'z'
loadct,39

ChiSqAlert = 70
QC0Alert   = 30
QC2Alert   = 10

size_strings = ['_small', '', '_big' ]
size_chars   = [0.65, 1.0, 1.25];

FOR ISIZE = 0, 1 DO BEGIN ;---- START ISIZE LOOP ----
  
  ;---Only do medium size image
  IF ISIZE NE 1 THEN CONTINUE

  size_string = size_strings[ISIZE]
  size_char   = size_chars[ISIZE]
  
  if ( ISIZE EQ 0 ) THEN BEGIN
      device,set_resolution=[325, 250]
      xticks=6
  endif

  if ( ISIZE EQ 1 ) THEN BEGIN
      device,set_resolution=[650, 500]
      xticks=7
  endif


  IF (  PLOT_CONVERGENCE EQ 1 ) THEN BEGIN

      ;---Plot percentage of convergent profiles
      png_file = figsDir+prefix+'monitoring_convrate'+timeSuffix+'.png'
      ytitle = 'MIRS Convergence Rate (%)'
      tit='MIRS ' + satTitle + ' Convergence Rate '
      
      uniqIdx   = uniq(jul_day)
      days2plot = jul_day(uniq(jul_day))

      ;dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])
      dummy=LABEL_DATE(DATE_FORMAT=['%H:%I'])
      plot,jul_day[sort(jul_day)],Conv_rate[sort(jul_day)],psym=-8,title=tit, $
	color=1,background=255,ytitle=ytitle,yrange=[60.,100.],$ 
	XTICKUNITS=[ 'Day' ], XTICKFORMAT=['LABEL_DATE'], $
	xstyle=1,ystyle=1,xticks=xticks,CHARSIZE=size_char

      plots,[min(jul_day),max(jul_day)],[ChiSqAlert,ChiSqAlert],linestyle=1,color=0

      thisImage = TVRD()
      TVLCT, r, g, b, /Get
      Write_PNG, png_file, thisImage, r, g, b
  ENDIF


  IF (  PLOT_QC EQ 1 ) THEN BEGIN

      ;dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])	
      dummy=LABEL_DATE(DATE_FORMAT=['%H:%I'])
      ;---- Plot percentage of QC=0 ----
      png_file = figsDir+prefix+'monitoring_qc0'+timeSuffix+'.png'
      ytitle = 'MIRS QC=0 (%)'
      tit='MIRS ' + satTitle + ' Percentage QC=0 '

      uniqIdx   = uniq(jul_day)
      days2plot = jul_day(uniq(jul_day))

      plot,jul_day[sort(jul_day)],percQC0[sort(jul_day)],psym=-8,title=tit, $
	color=1,background=255,ytitle=ytitle,yrange=[0.,100.],$ 
	XTICKUNITS = ['Day'], XTICKFORMAT=['LABEL_DATE'], $
	xstyle=1,ystyle=1,xticks=xticks, CHARSIZE=size_char

      plots,[min(jul_day),max(jul_day)],[30,30],linestyle=1,color=0    

      thisImage = TVRD()
      TVLCT, r, g, b, /Get
      Write_PNG, png_file, thisImage, r, g, b


      ;---- Plot percentage of QC=1 ----
      png_file = figsDir+prefix+'monitoring_qc1'+timeSuffix+'.png'
      ytitle = 'MIRS QC=1 (%)'
      tit='MIRS ' + satTitle + ' Percentage QC=1 '

      uniqIdx   = uniq(jul_day)
      days2plot = jul_day(uniq(jul_day))

      plot,jul_day[sort(jul_day)],percQC1[sort(jul_day)],psym=-8,title=tit, $
	color=1,background=255,ytitle=ytitle,yrange=[0.,100.],$ 
	XTICKUNITS = ['Day'], XTICKFORMAT=['LABEL_DATE'], $
	xstyle=1,ystyle=1,xticks=xticks, CHARSIZE=size_char

      thisImage = TVRD()
      TVLCT, r, g, b, /Get
      Write_PNG, png_file, thisImage, r, g, b


      ;---- Plot percentage of QC=2 ----
      png_file = figsDir+prefix+'monitoring_qc2'+timeSuffix+'.png'
      ytitle = 'MIRS QC=2 (%)'
      tit='MIRS ' + satTitle + ' Percentage QC=2 '

      uniqIdx   = uniq(jul_day)
      days2plot = jul_day(uniq(jul_day))

      plot,jul_day[sort(jul_day)],percQC2[sort(jul_day)],psym=-8,title=tit, $
	color=1,background=255,ytitle=ytitle,yrange=[0.,100.],$ 
	XTICKUNITS = ['Day'], XTICKFORMAT=['LABEL_DATE'], $
	xstyle=1,ystyle=1,xticks=xticks, CHARSIZE=size_char

      plots,[min(jul_day),max(jul_day)],[10,10],linestyle=1,color=0  

      thisImage = TVRD()
      TVLCT, r, g, b, /Get
      Write_PNG, png_file, thisImage, r, g, b

  ENDIF

ENDFOR ;---- END ISIZE LOOP ----


END
