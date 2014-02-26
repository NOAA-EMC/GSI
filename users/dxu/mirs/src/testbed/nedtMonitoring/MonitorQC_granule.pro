@../../../setup/paths_idl.pro


;*******************************************************************************
; NAME: MonitorQC_granule
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

PRO MonitorQC_granule, nameList=namelist

;*******************************************************************************
;    some variables initialization
;*******************************************************************************
satId='npp'
ymd='2012-01-17'
qcList='qcList_npp_2012-01-17'
figsDir='./'

;---- read from namelist or not
fromNameList=1
if ( fromNameList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId		       	;Satellite ID
  readf,iu,format='(a)', ymd		       	;yyyy-mm-dd
  readf,iu,format='(a)', qcList		       	;file extension of image
  readf,iu,format='(a)', figsDir  	       	;path where to put image files
  free_lun,iu,/force
endif

print, ''
print, 'MonitorQC_granule.pro ...'
print, 'satId='+satId
print, 'ymd='+ymd
print, 'qcList='+qcList
print, 'figsDir='+figsDir

satTitle=STRUPCASE(satId)
if satId eq 'npp' then satTitle='NPP/ATMS'
yyyymmdd=strmid(ymd,0,4)+strmid(ymd,5,2)+strmid(ymd,8,2)

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
readlist,qcList,listFiles,ndirs

;---Construct arrays to copy data to
nOrbits_tmp   = n_elements(listFiles)
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
    openr,rlun,listFiles(iFile),/get_lun
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
    year1=float(strmid(strtrim(file_basename(listFiles(iOrbit))),  13,4))
    month1=float(strmid(strtrim(file_basename(listFiles(iOrbit))), 17,2))
    day1=float(strmid(strtrim(file_basename(listFiles(iOrbit))),   19,2))
    hour1=float(strmid(strtrim(file_basename(listFiles(iOrbit))),  23,2))
    minute1=float(strmid(strtrim(file_basename(listFiles(iOrbit))),25,2))
    ; each granule is ~32 seconds, we take middle time, so adding 16 seconds here
    second1=float(strmid(strtrim(file_basename(listFiles(iOrbit))),27,3)) * 0.1 + 16
    Jul_Day(iOrbit) = JULDAY(month1,day1,year1, hour1,minute1,second1)
ENDFOR
JulDayFrac = Jul_Day

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
size_chars   = [0.65, 1.0, 1.25];

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
      png_file = figsDir+prefix+'monitoring_convrate_'+yyyymmdd+size_string+'.png'
      ytitle = 'MIRS Convergence Rate (%)'
      tit='MIRS ' + satTitle + ' Convergence Rate ' + ymd
      
      uniqIdx   = uniq(jul_day)
      days2plot = jul_day(uniq(jul_day))

      ;dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])
      dummy=LABEL_DATE(DATE_FORMAT=['%H:%I'])
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

      ;dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])	
      dummy=LABEL_DATE(DATE_FORMAT=['%H:%I'])
      ;---- Plot percentage of QC=0 ----
      png_file = figsDir+prefix+'monitoring_qc0_'+yyyymmdd+size_string+'.png'
      ytitle = 'MIRS QC=0 (%)'
      tit='MIRS ' + satTitle + ' Percentage QC=0 ' + ymd

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
      png_file = figsDir+prefix+'monitoring_qc1_'+yyyymmdd+size_string+'.png'
      ytitle = 'MIRS QC=1 (%)'
      tit='MIRS ' + satTitle + ' Percentage QC=1 ' + ymd

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
      png_file = figsDir+prefix+'monitoring_qc2_'+yyyymmdd+size_string+'.png'
      ytitle = 'MIRS QC=2 (%)'
      tit='MIRS ' + satTitle + ' Percentage QC=2 ' + ymd

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


END
