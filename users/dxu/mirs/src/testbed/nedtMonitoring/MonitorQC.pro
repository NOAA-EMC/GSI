@../../../setup/paths_idl.pro


;*******************************************************************************
; NAME: MonitorQC
;
; TYPE: IDL Program
;
; DESCRIPTION: 
;
;     Reads in year, ordinal day, and qc/convergence stats
;     from orbits processed by MiRS. The input is originally generated 
;     by the p2p_dep.f90 program.
;
; INPUTS:
;
;     satId   - satellite ID (n18,metop-A,f16,aqua-amsre)
;     figsDir - Directory for image generation output
;     qcDir   - Directory to read input file from
;
;
; HISTORY:
;
;     2008-12-08  Kevin Garrett Original coder
;     2011-02-04  Wanchun Chen  Add small sets and X-axis use Mmmdd/YYYY format
;
;
;*******************************************************************************

PRO MonitorQC, nameList=namelist

;*******************************************************************************
;    some variables initialization
;*******************************************************************************
satId='npp'
figsDir='./'
qcDir='/disk1/pub/mirs_operational/data/TestbedData/PerfsMonitoring/npp_atms/orbitmon/'

;---- read from namelist or not
fromNameList=1
if ( fromNameList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId		       	;Satellite ID
  readf,iu,format='(a)', figsDir  	       	;path where to put image files
  readf,iu,format='(a)', qcDir		       	;file extension of image
  free_lun,iu,/force
endif

print, 'MonitorQC.pro ...'
print, 'satId='+satId
print, 'qcDir='+qcDir
print, 'figsDir='+figsDir

;---Create unique symbol for plotting
!p.charsize   = 0.75
csize         = 16
a             = findgen(csize+1) *(!pi*2./float(csize))
usersym,cos(a)/2,sin(a)/2,/fill
symsize    = 0.8

;---Create unique prefix for output image names
prefix='mirs_adv_poes_'+satId+'_amsuamhs_orbit_'
if satId eq 'f16' then prefix='mirs_adv_dmsp_'+satId+'_ssmis_orbit_'
if satId eq 'f17' then prefix='mirs_adv_dmsp_'+satId+'_ssmis_orbit_'
if satId eq 'f18' then prefix='mirs_adv_dmsp_'+satId+'_ssmis_orbit_'
if satId eq 'npp' then prefix='mirs_adv_npoess_'+satId+'_atms_orbit_'

;---Get file list of input files containing data
if satId eq 'npp' then begin
  orbit_files = FILE_SEARCH(qcDir,'QC_MON_npp_????-??-??.HR', COUNT=nfile)
endif else begin
  orbit_files = FILE_SEARCH(qcDir,'QC_MON*', COUNT=nfile)
endelse

;If no files exist with names matching the input arguments, a null scalar string is returned instead of a string array. 
;but still one element inside it!!! this is a bug in IDL ( STRING    = '' )
if nfile eq 0 then begin
  print, 'Warning: No QC_MON* files under ' + qcDir + ', exit MonitorQC.pro.'
  exit
endif

;---Construct arrays to copy data to
nOrbits_tmp   = n_elements(orbit_files)
year_tmp      = make_array(nOrbits_tmp,/integer,value=-999)
time_tmp      = make_array(nOrbits_tmp,/float,value=-999.)
percQC0_tmp   = make_array(nOrbits_tmp,/float,value=-999.)
percQC1_tmp   = make_array(nOrbits_tmp,/float,value=-999.)
percQC2_tmp   = make_array(nOrbits_tmp,/float,value=-999.)
Conv_Rate_tmp = make_array(nOrbits_tmp,/float,value=-999.)
Jul_day_tmp   = make_array(nOrbits_tmp,/double,value=-999)
Cal_day_tmp   = make_array(nOrbits_tmp,/integer,value=-999)
day_frac_tmp  = make_array(nOrbits_tmp,/double,value=-999.)

;*******************************************************************************
;   READ DATA AND FILL ARRAYS
;*******************************************************************************
FOR iFile=0L,nOrbits_tmp-1L DO BEGIN
    openr,rlun,orbit_files(iFile),/get_lun
    readf,rlun,format='(i4,1x,f9.5,1x,4(f7.4,1x))',var1,var2,var3,var4,var5,var6
    year_tmp(iFile)      = var1
    time_tmp(iFile)      = var2
    percQC0_tmp(iFile)   = var3*100.
    percQC1_tmp(iFile)   = var4*100.
    percQC2_tmp(iFile)   = var5*100.
    Conv_Rate_tmp(iFile) = var6*100.
    free_lun,rlun
ENDFOR

;---- filter out <= 0 values ----
ss        = where(time_tmp gt 0, nOrbits)
print, 'nOrbits_tmp=',nOrbits_tmp
print, 'nOrbits=',nOrbits

if nOrbits le 1 then begin
  print, 'Warning: nOrbits <= 1, must be >= 2 to plot. Exit MonitorQC.pro.'
  exit
endif

year      =  year_tmp(ss)
time      =  time_tmp(ss)
percQC0   =  percQC0_tmp(ss)
percQC1   =  percQC1_tmp(ss)
percQC2   =  percQC2_tmp(ss)
Conv_Rate =  Conv_Rate_tmp(ss)
Jul_day   =  Jul_day_tmp(ss)
Cal_day   =  Cal_day_tmp(ss)
day_frac  =  day_frac_tmp(ss)

;---convert ordinal date to julian day (utilized by LABEL_DATE function)
odate = fix(time)
day_frac = abs(fix(time)-time)
FOR iOrbit=0L,nOrbits-1L DO BEGIN
    compCalDay,odate(iOrbit),year(iOrbit),calendarDay
    day_str = strmid(calendarDay,6,2)
    day = fix(day_str)
    mo_str  = strmid(calendarDay,4,2)
    month = fix(mo_str)
    Jul_Day(iOrbit) = JULDAY(month,day,year(iOrbit))
ENDFOR
JulDayFrac = Jul_Day+day_frac

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

nDays=fix(max(JulDayFrac)-min(JulDayFrac))

size_strings = ['_small', '', '_big' ]
size_chars   = [0.75, 1.0, 1.25];

FOR ISIZE = 0, 1 DO BEGIN ;---- START ISIZE LOOP ----
  
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
 
  if ( ISIZE EQ 2 ) THEN BEGIN
      device,set_resolution=[2000, 350]
      xticks=nDays/30
  endif


  IF (  PLOT_CONVERGENCE EQ 1 ) THEN BEGIN

      ;---Plot percentage of convergent profiles
      png_file = figsDir+prefix+'monitoring_convrate'+size_string+'.png'
      ytitle = 'MIRS Convergence Rate (%)'
      tit='MIRS ' + STRUPCASE(satId) + ' Convergence Rate'

      uniqIdx   = uniq(jul_day)
      days2plot = jul_day(uniq(jul_day))

      dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])
      plot,juldayfrac,Conv_rate,psym=-8,title=tit, $
	color=1,background=255,ytitle=ytitle,yrange=[60.,100.],$ 
	XTICKUNITS=[ 'Day' ], XTICKFORMAT=['LABEL_DATE'], $
	xstyle=1,ystyle=1,xticks=xticks,CHARSIZE=size_char

      plots,[juldayfrac[0],juldayfrac[nOrbits-1]],[ChiSqAlert,ChiSqAlert],linestyle=1,color=0

      thisImage = TVRD()
      TVLCT, r, g, b, /Get
      Write_PNG, png_file, thisImage, r, g, b

  ENDIF


  IF (  PLOT_QC EQ 1 ) THEN BEGIN

      dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])	

      ;---- Plot percentage of QC=0 ----
      png_file = figsDir+prefix+'monitoring_qc0'+size_string+'.png'
      ytitle = 'MIRS QC=0 (%)'
      tit='MIRS ' + STRUPCASE(satId) + ' Percentage QC=0'

      uniqIdx   = uniq(jul_day)
      days2plot = jul_day(uniq(jul_day))

      plot,juldayfrac,percQC0,psym=-8,title=tit, $
	color=1,background=255,ytitle=ytitle,yrange=[0.,100.],$ 
	XTICKUNITS = ['Day'], XTICKFORMAT=['LABEL_DATE'], $
	xstyle=1,ystyle=1,xticks=xticks, CHARSIZE=size_char

      plots,[juldayfrac[0],juldayfrac[nOrbits-1]],[30,30],linestyle=1,color=0    

      thisImage = TVRD()
      TVLCT, r, g, b, /Get
      Write_PNG, png_file, thisImage, r, g, b


      ;---- Plot percentage of QC=1 ----
      png_file = figsDir+prefix+'monitoring_qc1'+size_string+'.png'
      ytitle = 'MIRS QC=1 (%)'
      tit='MIRS ' + STRUPCASE(satId) + ' Percentage QC=1'

      uniqIdx   = uniq(jul_day)
      days2plot = jul_day(uniq(jul_day))

      plot,juldayfrac,percQC1,psym=-8,title=tit, $
	color=1,background=255,ytitle=ytitle,yrange=[0.,100.],$ 
	XTICKUNITS = ['Day'], XTICKFORMAT=['LABEL_DATE'], $
	xstyle=1,ystyle=1,xticks=xticks, CHARSIZE=size_char

      thisImage = TVRD()
      TVLCT, r, g, b, /Get
      Write_PNG, png_file, thisImage, r, g, b


      ;---- Plot percentage of QC=2 ----
      png_file = figsDir+prefix+'monitoring_qc2'+size_string+'.png'
      ytitle = 'MIRS QC=2 (%)'
      tit='MIRS ' + STRUPCASE(satId) + ' Percentage QC=2'

      uniqIdx   = uniq(jul_day)
      days2plot = jul_day(uniq(jul_day))

      plot,juldayfrac,percQC2,psym=-8,title=tit, $
	color=1,background=255,ytitle=ytitle,yrange=[0.,100.],$ 
	XTICKUNITS = ['Day'], XTICKFORMAT=['LABEL_DATE'], $
	xstyle=1,ystyle=1,xticks=xticks, CHARSIZE=size_char

      plots,[juldayfrac[0],juldayfrac[nOrbits-1]],[10,10],linestyle=1,color=0  

      thisImage = TVRD()
      TVLCT, r, g, b, /Get
      Write_PNG, png_file, thisImage, r, g, b

  ENDIF

ENDFOR ;---- END ISIZE LOOP ----


;---Write out file if there is QC or ChiSq violation in past 30 orbits
maxOrbits = min([31,nOrbits])
iAlert = where(Conv_rate((nOrbits-maxOrbits):nOrbits-1) lt ChiSqAlert $
            or percQC0((nOrbits-maxOrbits):nOrbits-1)   lt QC0Alert $
            or percQC2((nOrbits-maxOrbits):nOrbits-1)   gt QC2Alert,cnt)
if (cnt gt 0) then begin
    openw,wlun,figsDir+'checkQC.txt',/get_lun
    for i=0,cnt-1 do begin
        printf,wlun,format='(a55)','Orbit Data Quality Monitor: Orbit filename and metrics:'
        if (Conv_Rate(nOrbits-maxOrbits+iAlert(i)) lt ChiSqAlert) then $
          printf,wlun,'Convergence rate below threshold: ',ChiSqAlert
        if (percQC0(nOrbits-maxOrbits+iAlert(i)) lt QC0Alert) then $
          printf,wlun,'Percentage of QC=0 (good profiles) below threshold: ',QC0Alert
        if (percQC2(nOrbits-maxOrbits+iAlert(i)) gt QC2Alert) then $
            printf,wlun,'Percentage of QC=2 (failed profiles) above threshold: ',QC2Alert

        printf,wlun,format='(a160)',orbit_files(nOrbits-maxOrbits+iAlert(i))
        printf,wlun,format='(a10,i4)','    Year: ',year(nOrbits-maxOrbits+iAlert(i))
        printf,wlun,format='(a10,f9.5)','     Day: ',time(nOrbits-maxOrbits+iAlert(i))       
        printf,wlun,format='(a10,f8.4)',' % QC 0: ',percQC0(nOrbits-maxOrbits+iAlert(i))
        printf,wlun,format='(a10,f8.4)',' % QC 1: ',percQC1(nOrbits-maxOrbits+iAlert(i))
        printf,wlun,format='(a10,f8.4)',' % QC 2: ',percQC2(nOrbits-maxOrbits+iAlert(i))
        printf,wlun,format='(a10,f8.4)','Cnv Rate: ',Conv_Rate(nOrbits-maxOrbits+iAlert(i))
        printf,wlun,format='(a15)','==============='
    endfor
    close,wlun
    free_lun,wlun
endif
 
END
