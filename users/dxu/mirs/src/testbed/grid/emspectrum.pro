@../../../setup/paths_idl.pro

PRO EMSPECTRUM, nameList=namelist

;*******************************************************************************
;    some variables initialization
;*******************************************************************************
satId='npp'
date='2011-11-09'
dirGrid='/disk1/pub/mirs_npp/data/TestbedData/Outputs/grid/npp_atms/2011-11-09/'
figsDir='./'
version='2734'

;---- read from namelist or not
fromNameList=1
if ( fromNameList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId		       	;Satellite ID
  readf,iu,format='(a)', date		       	;file extension of image
  readf,iu,format='(a)', dirGrid  	       	;path where to average emissivity spectrum file
  readf,iu,format='(a)', figsDir  	       	;path where to put image files
  readf,iu,format='(a)', version    		;version number
  close,iu
  free_lun,iu,/force
endif


yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

prefix='mirs_adv_poes_'+satId+'_amsuamhs_glb_'
if satId eq 'f16' or satId eq 'f17' or satId eq 'f18' then prefix='mirs_adv_dmsp_'+satId+'_ssmis_glb_'
if satId eq 'npp' then prefix='mirs_adv_npoess_'+satId+'_atms_glb_'
if satId eq 'mtma' then prefix='mirs_adv_mt_'+satId+'_madras_glb_'

NSFC=4
NANGLE=3

NCHAN=20
if satId eq 'f16' or satId eq 'f17' or satId eq 'f18' then NCHAN=24
if satId eq 'mtma'then NCHAN=9
if satId eq 'npp' then NCHAN=22

titleSat = strupcase(satId)
if satId eq 'npp' then titleSat = 'NPP/ATMS'

;    0    1   2   3   4   5    6    7    8    9    10  11  12  13  14  15  16  17  18    19     20   21    22    23 
;f16(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5)
;f17(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5)
;f18(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5)
;npp(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5)

if satId eq 'f16' then begin
  chans_adjust1 = [ 11,14, 17, 7,10,9,8 ]
  chans_adjust2 = [ 12,13,15,0,1,2,3,4,16 ]
endif

if satId eq 'f17' then begin
  chans_adjust1 = [ 11,14, 0,1,2,3,4, 17, 7,10,9,8 ]
  chans_adjust2 = [ 12,13,15,16 ]
endif

if satId eq 'f18' then begin
  chans_adjust1 = [ 11,14, 0,1,2,3,4, 17, 7,10,9,8 ]
  chans_adjust2 = [ 12,13,15,16 ]
endif

if satId eq 'mtma' then begin
  chans_adjust1 = [ 1,3,6,8 ]
  chans_adjust2 = [ 0,2,4,5,7 ]
endif

;*******************************************************************************
;    identifier definitions
;*******************************************************************************
prodId = 'emspectrum'
products = fltarr(NCHAN,NSFC)
avg_em_as = fltarr(NCHAN,NSFC,NANGLE)
avg_em_ds = fltarr(NCHAN,NSFC,NANGLE)

avg_em_as(*,*) = 0.0
avg_em_ds(*,*) = 0.0 

;*******************************************************************************
;    read in average data 
;*******************************************************************************
fileGrid=dirGrid+'AVG_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+'as'+'.dat'
openr,lun,fileGrid,/get_lun,/Swap_Endian
result = FILE_TEST(fileGrid)
if result eq 0 then begin
  print, 'File not exist: ' + fileGrid
endif

if result eq 1 then begin
  openr,lun,fileGrid,/get_lun,/Swap_Endian
  for iangle=0,NANGLE-1 do begin
    readu,lun,products
    avg_em_as(*,*,iangle) = products(*,*)
  endfor
  free_lun,lun
endif



fileGrid=dirGrid+'AVG_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+'ds'+'.dat'
openr,lun,fileGrid,/get_lun,/Swap_Endian
result = FILE_TEST(fileGrid)
if result eq 0 then begin
  print, 'File not exist: ' + fileGrid
endif

if result eq 1 then begin
  openr,lun,fileGrid,/get_lun,/Swap_Endian
  for iangle=0,NANGLE-1 do begin
    readu,lun,products
    avg_em_ds(*,*,iangle) = products(*,*)
  endfor
  free_lun,lun
endif


;*******************************************************************************
;    plot image
;*******************************************************************************
;---- N18/N19/MetopA/MetopB
FREQS = [ 23.800, 31.400, 50.300, 52.799, 53.595, 54.400, 54.941, 55.499, 57.290, 57.290, $
          57.290, 57.290, 57.290, 57.290, 89.000, 89.000,157.000, 183.311,183.311,190.311 ]
XTickVs = [ 23, 50, 89, 157, 190 ]

;---- NPP ATMS
if satId eq 'npp' then begin
  FREQS = [ 23.800, 31.400, 50.300, 51.760, 52.800, 53.596, 54.400, 54.940, 55.500, 57.290, $
            57.290, 57.290, 57.290, 57.290, 57.290, 88.200, 165.500, 183.310, 183.310, 183.310, $
            183.310, 183.310 ]
  XTickVs = [ 23, 50, 88, 165, 183 ]
endif

;---- F16 
FREQS1 = [ 19.35, 37.0, 91.655, 150.0, 183.31, 186.31, 190.31 ]
FREQS2 = [ 19.35, 22.235, 37.0, 50.3, 52.8, 53.596, 54.4, 55.5, 91.0 ]

XTickVs1 = [ 19, 37, 91, 150, 190 ]
XTickVs2 = [ 19, 37, 55, 91 ]

;---- F17
if satId eq 'f17' then begin
  FREQS1 = [ 19.35, 37.0, 50.3, 52.8, 53.596, 54.4, 55.5, 91.655, 150.0, 183.31, 186.31, 190.31 ]
  FREQS2 = [ 19.35, 22.235, 37.0, 91.0 ]

  XTickVs1 = [ 19, 37, 50, 91, 150, 190 ]
  XTickVs2 = [ 19, 37, 91 ]
endif

;---- F18
if satId eq 'f18' then begin
  FREQS1 = [ 19.35, 37.0, 50.3, 52.8, 53.596, 54.4, 55.5, 91.655, 150.0, 183.31, 186.31, 190.31 ]
  FREQS2 = [ 19.35, 22.235, 37.0, 91.0 ]

  XTickVs1 = [ 19, 37, 50, 91, 150, 190 ]
  XTickVs2 = [ 19, 37, 91 ]
endif

;---- MT/MADRAS
if satId eq 'mtma' then begin
  FREQS1 = [ 18.7, 36.5, 89.0, 157.0 ]
  FREQS2 = [ 18.7, 23.8, 36.5, 89.0, 157.0 ]

  XTickVs1 = [ 19, 37, 89, 157 ]
  XTickVs2 = [ 19, 24, 37, 89, 157 ]
endif

ntick1 = N_ELEMENTS(XTickVs1)-1
ntick2 = N_ELEMENTS(XTickVs2)-1

NFREQ = N_ELEMENTS(FREQS)
values_along_freq = fltarr(NFREQ)
sfcs  = [ 'Ocean', 'Ice', 'Land', 'Snow']
colors = indgen(NSFC) + 2  
symsize = 1.0
deg = '!9'+String(37B)+'!X'

titles = ['LZA: 0'+deg+'-20'+deg, 'LZA: 20'+deg+'-40'+deg, 'LZA: 40'+deg+'-60'+deg]
if satId eq 'f16' then titles = ['Horizontal', 'Vertical']
if satId eq 'f17' then titles = ['Horizontal', 'Vertical']
if satId eq 'f18' then titles = ['Horizontal', 'Vertical']
if satId eq 'mtma' then titles = ['Horizontal', 'Vertical']

yrange = [ 0.4, 1.0]
if satId eq 'f16' then yrange = [ 0.3, 1.0]
if satId eq 'f17' then yrange = [ 0.3, 1.0]
if satId eq 'f18' then yrange = [ 0.3, 1.0]
if satId eq 'mtma' then yrange = [ 0.3, 1.0]

cends = ['as', 'ds' ]
cendTxts = ['Ascending', 'Descending' ]

yticks = FIX( ( yrange(1) - yrange(0) ) * 10 )

for icend = 0, 1 do begin

  topTitle = 'MIRS ' + titleSat + ' Emissivity Spectrum ' + date + ' ' + cendTxts[icend] + ' (V' + version + ')'
  
  SET_PLOT, 'Z'
  DEVICE, SET_RESOLUTION=[650,500]

  TVLCT, 255,255,255,      255

  TVLCT, 0,0,0,       1
  TVLCT, 255,0,0,     2
  TVLCT, 0,255,0,     3
  TVLCT, 0,0,255,     4
  TVLCT, 0,255,255,   5
  TVLCT, 255,0,255,   6
  TVLCT, 255,128,0,   7
  TVLCT, 128,128,128, 8
  
  blackColor=1
  
  IF satId eq 'n18' or satId eq 'n19' or satId eq 'metopA' or satId eq 'metopB' or satId eq 'npp' THEN BEGIN
  
    !P.MULTI = [0,3,1]
    for iangle = 0, 2 do begin

      x1=0.075 + iangle*0.3
      x2=x1+0.25

      y1=0.10
      y2=0.875

      plot, FREQS, values_along_freq, CHARSIZE=1.5, TITLE=titles(iangle), COLOR=blackColor, BACKGROUND=255, $
    	    YRANGE=yrange, /NODATA, position=[x1,y1,x2,y2], XTickV=XTickVs, XTicks=4, Ystyle=1, Yticks=yticks

      for isfc=0,NSFC-1 do begin

	if icend eq 0 then values_along_freq(*) = avg_em_as(*,isfc,iangle)
	if icend eq 1 then values_along_freq(*) = avg_em_ds(*,isfc,iangle)

	oplot, FREQS, values_along_freq, COLOR=colors(isfc), PSYM=3,  LINESTYLE=0, THICK=4
	oplot, FREQS, values_along_freq, COLOR=colors(isfc), PSYM=-1, LINESTYLE=0, THICK=4
	
      endfor

    endfor

    XYOUTS, 0.075,  0.025, '__'+sfcs(0), COLOR=colors(0), CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL
    XYOUTS, 0.325,  0.025, '__'+sfcs(1), COLOR=colors(1), CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL
    XYOUTS, 0.550,  0.025, '__'+sfcs(2), COLOR=colors(2), CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL
    XYOUTS, 0.825,  0.025, '__'+sfcs(3), COLOR=colors(3), CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL

    XYOUTS, 0.150,  0.95, topTitle, COLOR=blackColor, CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL

    TVLCT, r, g, b, /Get
    png_file = figsDir + prefix + yyyymmdd +'_emspectrum_all_' + cends[icend] + '.png' 
    WRITE_PNG, png_file, TVRD(), r, g, b
    DEVICE,/CLOSE
  
  ENDIF ELSE BEGIN
  
    !P.MULTI = [0,2,1]
    
    y1=0.10
    y2=0.875
      
    ;---- left panel - horizontal ---------- 
    
    x1=0.075
    x2=x1+0.35

    NFREQ1 = N_ELEMENTS(FREQS1)
    values_along_freq = fltarr(NFREQ1)
    
    plot, FREQS1, values_along_freq, CHARSIZE=0.75, TITLE=titles(0), COLOR=blackColor, BACKGROUND=255, $
          YRANGE=yrange, /NODATA, position=[x1,y1,x2,y2], XTickV=XTickVs1, XTicks=ntick1, Ystyle=1, Yticks=yticks

    for isfc=0,NSFC-1 do begin

      if icend eq 0 then values_along_freq(*) = avg_em_as(chans_adjust1(*),isfc,0)
      if icend eq 1 then values_along_freq(*) = avg_em_ds(chans_adjust1(*),isfc,0)

      oplot, FREQS1, values_along_freq, COLOR=colors(isfc), PSYM=3,  LINESTYLE=0, THICK=4
      oplot, FREQS1, values_along_freq, COLOR=colors(isfc), PSYM=-1, LINESTYLE=0, THICK=4

    endfor


    ;---- right panel - vertical ------------
    
    x1=0.075 + 0.50
    x2=x1+0.35

    NFREQ2 = N_ELEMENTS(FREQS2)
    values_along_freq = fltarr(NFREQ2)
    
    plot, FREQS2, values_along_freq, CHARSIZE=0.75, TITLE=titles(1), COLOR=blackColor, BACKGROUND=255, $
          YRANGE=yrange, /NODATA, position=[x1,y1,x2,y2], XTickV=XTickVs2, XTicks=ntick2, Ystyle=1, Yticks=yticks

    for isfc=0,NSFC-1 do begin

      if icend eq 0 then values_along_freq(*) = avg_em_as(chans_adjust2(*),isfc,0)
      if icend eq 1 then values_along_freq(*) = avg_em_ds(chans_adjust2(*),isfc,0)

      oplot, FREQS2, values_along_freq, COLOR=colors(isfc), PSYM=3,  LINESTYLE=0, THICK=4
      oplot, FREQS2, values_along_freq, COLOR=colors(isfc), PSYM=-1, LINESTYLE=0, THICK=4

    endfor

    XYOUTS, 0.075,  0.025, '__'+sfcs(0), COLOR=colors(0), CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL
    XYOUTS, 0.325,  0.025, '__'+sfcs(1), COLOR=colors(1), CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL
    XYOUTS, 0.550,  0.025, '__'+sfcs(2), COLOR=colors(2), CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL
    XYOUTS, 0.825,  0.025, '__'+sfcs(3), COLOR=colors(3), CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL

    XYOUTS, 0.150,  0.95, topTitle, COLOR=blackColor, CHARSIZE=1.0, CHARTHICK=1.0, /NORMAL

    TVLCT, r, g, b, /Get
    png_file = figsDir + prefix + yyyymmdd +'_emspectrum_all_' + cends[icend] + '.png' 
    WRITE_PNG, png_file, TVRD(), r, g, b
    DEVICE,/CLOSE
  
  ENDELSE
  
  
endfor

end

