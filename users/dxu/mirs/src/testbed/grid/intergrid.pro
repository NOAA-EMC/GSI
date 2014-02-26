@../../../setup/paths_idl.pro
;***********************************************************************************************
;
;  Purpose:
;    To do MIRS inter-Comparison(mean bias, assymmetry and scattered plots)
;    based on gridded data sets
;
;  Dependence:
;    plot_grid.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ----------------------------------------------------------
;     02/25/2009         Wanchun Chen     Original Code
;     11/25/2009         Wanchun Chen     Added N18 vs NPP branch
;     02/16/2012         Wanchun Chen     Added more pairs
;
;***********************************************************************************************
Pro intergrid, nameList=nameList
Consts, Consts

;***************************************************************
;    some constants definitions
;***************************************************************

gridfactor=4
date='2012-02-22'
satId1='n18'
satId2='n19'
dirGrid1='/disk1/pub/archive/grid/n18_amsua_mhs/'+date+'/'
dirGrid2='/disk1/pub/archive/grid/n19_amsua_mhs/'+date+'/'
dirImg='./img/'
version='2933'
latmin=-90
latmax=90
lonmin=-180
lonmax=180

readFromList=1
if ( readFromList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(i)', gridfactor     ;gridfactor
  readf,iu,format='(a)', date  		;file extension of image
  readf,iu,format='(a)', satId1         ;Satellite ID 1
  readf,iu,format='(a)', satId2         ;Satellite ID 2
  readf,iu,format='(a)', dirGrid1    	;gridded data dir 1
  readf,iu,format='(a)', dirGrid2    	;gridded data dir 2
  readf,iu,format='(a)', dirImg         ;path where to put image files
  readf,iu,format='(a)', version    	;version number
  readf,iu,format='(f)', latmin         ;min lat
  readf,iu,format='(f)', latmax         ;max lat
  readf,iu,format='(f)', lonmin         ;min lon
  readf,iu,format='(f)', lonmax         ;max lon
  close,iu
  free_lun,iu,/force
endif


yyyy=strmid(date,0,4)
mm=strmid(date,5,2)
dd=strmid(date,8,2)
year=fix(yyyy)
month=fix(mm)
day=fix(dd)

jday_yesterday=JULDAY(month,day,year)

jday_year_begin = JULDAY(1,1,year)
day_of_year = FIX(jday_yesterday - jday_year_begin + 1)
jjj=STRTRIM(STRING(day_of_year),2)
jday_min = day_of_year - 1
jday_max = day_of_year + 1

pair=satId1+'_'+satId2

NLAY = 11
NLAY_GRID = 100
NCHAN = 20

NCHAN1 = NCHAN
NCHAN2 = NCHAN
NCHAN0 = NCHAN
inds1  = indgen(NCHAN0)
inds2  = indgen(NCHAN0)

;---- F16 vs F18 has lat 19 channels matched
if pair eq 'f16_f18' then begin
  NCHAN1 = 24
  NCHAN2 = 24
  NCHAN0 = 19
  inds1  = indgen(NCHAN0) + 5
  inds2  = indgen(NCHAN0) + 5
endif   

;---- only 13 exactly matched channels between POES and ATMS
;---- channel mathched: matchaned pattern
;N18: [1, 2, 5, 6, 8, 9,  10, 11, 12, 13, 14, 18, 19]
;NPP: [1, 2, 6, 7, 9, 10, 11, 12, 13, 14, 15, 22, 20]
;  index-wise in IDL or any 0 based array:
;N18: [0, 1, 4, 5, 7, 8,  9, 10, 11, 12, 13, 17, 18]
;NPP: [0, 1, 5, 6, 8, 9, 10, 11, 12, 13, 14, 21, 19]

if pair eq 'n18_npp' or pair eq 'n19_npp' or pair eq 'metopA_npp' or pair eq 'metopB_npp' then begin
  NCHAN1 = 20
  NCHAN2 = 22
  NCHAN0 = 13
  ind1s  = [0, 1, 4, 5, 7, 8,  9, 10, 11, 12, 13, 17, 18]
  ind2s  = [0, 1, 5, 6, 8, 9, 10, 11, 12, 13, 14, 21, 19]
endif   

NCOL=360*gridfactor
NROW=180*gridfactor

NBIN=24
BIN_BOX = 5 * indgen(NBIN+1) - 60
if pair eq 'f16_f18' then begin
  NBIN=30
  BIN_BOX = indgen(NBIN+1) + 1
endif

MISSING_FLT = -999.0
MISSING_INT = -999
CONT_MIN = 200L
NBINS_HIST = 200L

LWP_CUT = 0.05
RWP_CUT = 0.05
TIME_CUT = 1.0 ; 1.0 means 24 hours

leg_as = "Ascending"
leg_ds = "Descending"

;***************************************************************
;    some image name parameters
;***************************************************************
rotate = 'mirs_adv_poes_'
if pair eq 'f16_f18' then rotate='mirs_adv_dmsp_'

prefix = rotate+satId1+'_'+satId2+'_glb_'

prefix1 = rotate+satId1+'_amsuamhs_glb_'
prefix2 = rotate+satId2+'_amsuamhs_glb_'
if pair eq 'f16_f18' then begin
  prefix1 = rotate+satId1+'_ssmis_glb_'
  prefix2 = rotate+satId2+'_ssmis_glb_'
endif

prefix_bias = rotate+satId1+'_'+satId2+'_bias_glb_'
prefix_asym = rotate+satId1+'_'+satId2+'_asym_glb_'
prefix_hist = rotate+satId1+'_'+satId2+'_hist_glb_'
prefix_scat = rotate+satId1+'_'+satId2+'_scat_glb_'

yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

xrange=[-60,60]
xtitle='Local Zenith Angle (degree)'

if pair eq 'f16_f18' then begin
  xrange=[1,30]
  xtitle='Scan Position'
endif


;***************************************************************
;    read in static land/sea masks
;***************************************************************
sfcMask=BytArr(NCOL,NROW)
dum1=BytArr(NCOL,NROW)
dum2=BytArr(NCOL,NROW)
IF ( nCol EQ 1440 ) THEN OpenR,Lun,'lndsea25.tag',/Get_Lun,/Swap_If_Big_Endian
IF ( nCol EQ 1080 ) THEN OpenR,Lun,'lndsea30.tag',/Get_Lun,/Swap_If_Big_Endian
IF ( nCol EQ  720 ) THEN OpenR,Lun,'lndsea50.tag',/Get_Lun,/Swap_If_Big_Endian
ReadU,Lun,dum1
Free_Lun,Lun

;---- reverse index of land/sea mask to be consistent with IDL convention ----
for jrow=0, nrow-1 do begin  
  dum2[*,jrow] = dum1[*,nrow-1-jrow]
endfor

for icol=0, ncol-1 do begin
  if icol ge ncol/2 then begin
    sfcMask(icol,*) = dum2(icol-ncol/2,*)
  endif
  if icol lt ncol/2 then begin
    sfcMask(icol,*) = dum2(icol+ncol/2,*)
  endif
endfor

UNDEFINE,dum1
UNDEFINE,dum2


geo='angle'
if pair eq 'f16_f18' then geo = 'scanpos'

prodIds_sfc = ['psfc','tskin','tpw','clw','rr','iwp','lwp','rwp','swe','sice', 'sicefy','sicemy','snow','gs',geo,'scanday']
prodIds_lay = ['temp','wv']
prodIds_chan = ['em','tbc','tbu']


;---- 11 layers to be plotted ----
layIds=['100mb', '200mb', '300mb', '400mb', '500mb', '600mb','700mb', '800mb', '850mb', '900mb', '950mb']
LAYERS_INDEX=[43, 54, 62, 69, 75, 80, 84, 88, 90, 92, 94]
layers=[100,200,300,400,500,600,700,800,850,900,950]
yticknames_tmp=['100', '200', '300', '400', '500', '600','700', '800', '850', '900', '950']
yticknames=REVERSE(yticknames_tmp)
yticks=N_Elements(yticknames)-1

;---- plus minus sign stacked together
sign_pm = '!9' + String(43B) + '!X'

chanIds = ['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2',$
           '57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']

chanTxts = ['23.8 V','31.4 V','50.3 V','52.8 V',  '53.596'+sign_pm+'0.115 H','54.4 H','54.94 V','55.5 H',$
          '57.29 H',$
          '57.29'+sign_pm+'0.217 H',$
          '57.29'+sign_pm+'0.3222'+sign_pm+'0.048 H',$
          '57.29'+sign_pm+'0.3222'+sign_pm+'0.022 H',$
          '57.29'+sign_pm+'0.3222'+sign_pm+'0.010 H',$
          '57.29'+sign_pm+'0.3222'+sign_pm+'0.0045 H',$
          '89.0 V','89.0 V','157.0 H',$
	  '183.311'+sign_pm+'1.0 H','183.311'+sign_pm+'3.0 H','190.311 V']


if pair eq 'f16_f18' then begin
  chanIds = ['57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v',$
            '91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']
  chanTxts = ['57.29 RC', '59.4 RC', '150.0 H', $
              '183.31'+sign_pm+'6.6 H', '183.31'+sign_pm+'3 H', '183.31'+sign_pm+'1 H', $
	      '19.35 H', '19.35 V', '22.235 V', '37.0 H', '37.0 V',  $
              '91.655 V', '91.655 H', $
	      '63.283248'+sign_pm+'0.285271 RC', $
	      '60.792668'+sign_pm+'0.357892 RC', $
	      '60.792668'+sign_pm+'0.357892'+sign_pm+'0.002 RC',$
	      '60.792668'+sign_pm+'0.357892'+sign_pm+'0.006 RC',$
	      '60.792668'+sign_pm+'0.357892'+sign_pm+'0.016 RC',$
	      '60.792668'+sign_pm+'0.357892'+sign_pm+'0.050 RC' ]
endif

if pair eq 'n18_npp' or pair eq 'n19_npp' or pair eq 'metopA_npp' or pair eq 'metopB_npp' then begin
  chanIds=['23v','31v','53h','54h','55h','57h1','57h2','57h3','57h4','57h5','57h6','184h','186h']
  ;          1     2     5     6     8      9     10     11     12     13     14     18     19
  chanTxts = [$
  '23.8 V',$
  '31.4 V',$
  '53.596'+sign_pm+'0.115 H',$
  '54.4 H',$
  '55.5 H',$
  '57.29 H',$
  '57.29'+sign_pm+'0.217 H',$
  '57.29'+sign_pm+'0.3222'+sign_pm+'0.048 H',$
  '57.29'+sign_pm+'0.3222'+sign_pm+'0.022 H',$
  '57.29'+sign_pm+'0.3222'+sign_pm+'0.010 H',$
  '57.29'+sign_pm+'0.3222'+sign_pm+'0.0045 H',$
  '183.311'+sign_pm+'1.0 H',$
  '183.311'+sign_pm+'3.0 H' ]
endif

CHANNELS_INDEX=indgen(NCHAN)

nprod_sfc  = N_ELEMENTS(prodIds_sfc)
nprod_lay  = N_ELEMENTS(prodIds_lay)
nprod_chan = N_ELEMENTS(prodIds_chan)

sfcIds  = [ 'sea', 'ice', 'lnd',   'snw', 'all' ]
sfcTxts = [ 'Sea', 'Ice', 'Land', 'Snow', 'All' ]
nsfc    = N_ELEMENTS(sfcIds)

;---- combined as/ds for temp and wv bias and stdv ----
bias_temp = fltarr(4,NLAY) & bias_temp(*,*) = 0.0
stdv_temp = fltarr(4,NLAY) & stdv_temp(*,*) = 0.0

bias_wv   = fltarr(4,NLAY) & bias_wv(*,*)   = 0.0
stdv_wv   = fltarr(4,NLAY) & stdv_wv(*,*)   = 0.0

cends = ['as','ds']
ncend = n_elements(cends)
cendTxts = [' Asc ', ' Des ']

;***************************************************************
; loop over icend
;***************************************************************
for icend = 0, ncend - 1 do begin
  cend = cends(icend)
  cendTxt = cendTxts(icend)
  
  ;---- scan angle or scan position ----
  angle1s = fltarr(NCOL,NROW)
  file_angle1=dirGrid1+'GRID_'+satId1+'_'+yyyymmdd+'_'+geo+'_'+cend+'.dat'
  result = FILE_TEST(file_angle1)
  if result eq 0 then begin
    print, 'File not exist: ' + file_angle1
    continue 
  endif
  openr, lun, file_angle1, /get_lun, /Swap_Endian
  readu, lun, angle1s
  free_lun,lun,/force

  angle2s = fltarr(NCOL,NROW)
  file_angle2=dirGrid2+'GRID_'+satId2+'_'+yyyymmdd+'_'+geo+'_'+cend+'.dat'
  result = FILE_TEST(file_angle2)
  if result eq 0 then begin
    print, 'File not exist: ' + file_angle2
    continue 
  endif
  openr, lun, file_angle2, /get_lun, /Swap_Endian
  readu, lun, angle2s
  free_lun,lun,/force

  ;---- EDR sfcId type ----
  sfcEdr1s = fltarr(NCOL,NROW)
  file_sfc1 = dirGrid1+'GRID_'+satId1+'_'+yyyymmdd+'_sfc_'+cend+'.dat'
  result = FILE_TEST(file_sfc1)
  if result eq 0 then begin
    print, 'File not exist: ' + file_sfc1
    continue 
  endif
  openr, lun, file_sfc1, /get_lun, /Swap_Endian
  readu, lun, sfcEdr1s
  free_lun,lun,/force

  sfcEdr2s = fltarr(NCOL,NROW)
  file_sfc2 = dirGrid2+'GRID_'+satId2+'_'+yyyymmdd+'_sfc_'+cend+'.dat'
  result = FILE_TEST(file_sfc2)
  if result eq 0 then begin
    print, 'File not exist: ' + file_sfc2
    continue 
  endif
  openr, lun, file_sfc2, /get_lun, /Swap_Endian
  readu, lun, sfcEdr2s
  free_lun,lun,/force

  ;---- DEP sfcId type ----
  sfcDep1s = fltarr(NCOL,NROW)
  file_sfc1 = dirGrid1+'GRID_'+satId1+'_'+yyyymmdd+'_sfc2_'+cend+'.dat'
  result = FILE_TEST(file_sfc1)
  if result eq 0 then begin
    print, 'File not exist: ' + file_sfc1
    continue 
  endif
  openr, lun, file_sfc1, /get_lun, /Swap_Endian
  readu, lun, sfcDep1s
  free_lun,lun,/force

  sfcDep2s = fltarr(NCOL,NROW)
  file_sfc2 = dirGrid2+'GRID_'+satId2+'_'+yyyymmdd+'_sfc2_'+cend+'.dat'
  result = FILE_TEST(file_sfc2)
  if result eq 0 then begin
    print, 'File not exist: ' + file_sfc2
    continue 
  endif
  openr, lun, file_sfc2, /get_lun, /Swap_Endian
  readu, lun, sfcDep2s
  free_lun,lun,/force

  ;---- scanday ----
  scanday1s = fltarr(NCOL,NROW)
  file_scanday1=dirGrid1+'GRID_'+satId1+'_'+yyyymmdd+'_scanday_'+cend+'.dat'
  result = FILE_TEST(file_scanday1)
  if result eq 0 then begin
    print, 'File not exist: ' + file_scanday1
    continue 
  endif
  openr, lun, file_scanday1, /get_lun, /Swap_Endian
  readu, lun, scanday1s
  free_lun,lun,/force

  scanday2s = fltarr(NCOL,NROW)
  file_scanday2=dirGrid2+'GRID_'+satId2+'_'+yyyymmdd+'_scanday_'+cend+'.dat'
  result = FILE_TEST(file_scanday2)
  if result eq 0 then begin
    print, 'File not exist: ' + file_scanday2
    continue 
  endif
  openr, lun, file_scanday2, /get_lun, /Swap_Endian
  readu, lun, scanday2s
  free_lun,lun,/force
  
  ;---- scanday difference ----
  timediffs=fltarr(NCOL,NROW) & timediffs(*,*)=999.0
  for irow = 0,NROW-1 do begin 
  for icol = 0,NCOL-1 do begin
    if scanday1s(icol,irow) ge 0 and scanday2s(icol,irow) ge 0 then $
      timediffs(icol,irow) = ABS(scanday1s(icol,irow) - scanday2s(icol,irow))
  endfor
  endfor

  ;---- lwp -- play as a filter over ocean ----
  lwp1s = fltarr(NCOL,NROW)
  file_lwp1 = dirGrid1+'GRID_'+satId1+'_'+yyyymmdd+'_lwp_'+cend+'.dat'
  result = FILE_TEST(file_lwp1)
  if result eq 0 then begin
    print, 'File not exist: ' + file_lwp1
    continue 
  endif
  openr, lun, file_lwp1, /get_lun, /Swap_Endian
  readu, lun, lwp1s
  free_lun,lun,/force

  lwp2s = fltarr(NCOL,NROW)
  file_lwp2 = dirGrid2+'GRID_'+satId2+'_'+yyyymmdd+'_lwp_'+cend+'.dat'
  result = FILE_TEST(file_lwp2)
  if result eq 0 then begin
    print, 'File not exist: ' + file_lwp2
    continue 
  endif
  openr, lun, file_lwp2, /get_lun, /Swap_Endian
  readu, lun, lwp2s
  free_lun,lun,/force

  ;---- rwp -- play as a filter over land ----
  rwp1s = fltarr(NCOL,NROW)
  file_rwp1 = dirGrid1+'GRID_'+satId1+'_'+yyyymmdd+'_rwp_'+cend+'.dat'
  result = FILE_TEST(file_rwp1)
  if result eq 0 then begin
    print, 'File not exist: ' + file_rwp1
    continue 
  endif
  openr, lun, file_rwp1, /get_lun, /Swap_Endian
  readu, lun, rwp1s
  free_lun,lun,/force

  rwp2s = fltarr(NCOL,NROW)
  file_rwp2 = dirGrid2+'GRID_'+satId2+'_'+yyyymmdd+'_rwp_'+cend+'.dat'
  result = FILE_TEST(file_rwp2)
  if result eq 0 then begin
    print, 'File not exist: ' + file_rwp2
    continue 
  endif
  openr, lun, file_rwp2, /get_lun, /Swap_Endian
  readu, lun, rwp2s
  free_lun,lun,/force


  ;----------------------------------------------------------------
  ; sfcId products section begin
  ;----------------------------------------------------------------
  for iprod = 0, nprod_sfc - 1 do begin ; loop iprod for sfcId starts

    prodId = prodIds_sfc[iprod]
    print, 'plotting ' + prodId + ' ' + cend + ' ...'

    case  prodId of

    'angle':      begin
                          prodTxt  = ' Scan Angle '
                          minval_bias = -60
                          maxval_bias = 60
                          div      = 12
                          fmt      = '(I3)'
                          unit     = '(degree)'
                          nticks   = 6
                          minval   = -60
                          maxval   = 60
                  end

    'chisq':      begin
                          prodTxt  = ' Chi Square '
                          minval_bias = 0
                          maxval_bias = 10
                          div      = 10
                          fmt      = '(I2)'
                          unit     = ''
                          nticks   = 10
                          minval   = 0
                          maxval   = 10
        	 end

    'clw':        begin
                          prodTxt  = ' CLW '
                          minval_bias = -0.2
                          maxval_bias = 0.2
                          div      = 10
                          fmt      = '(f5.2)'
                          unit     = '(mm)'
                          nticks   = 7
                          minval   = 0.0
                          maxval   = 0.7
        	 end

    'gs':         begin
                          prodTxt  = ' Snow Grain Size Radius '
                          minval_bias = -0.1
                          maxval_bias = 0.1
                          div      = 10
                          fmt      = '(f5.2)'
                          unit     = '(mm)'
                          nticks   = 6
                          minval   = 0.1
                          maxval   = 0.7
               end

    'iwp':        begin
                          prodTxt  = ' Graupel Water Path '
                          minval_bias = -0.20
                          maxval_bias = 0.20
                          div      = 10
                          fmt      = '(f5.2)'
                          unit     = '(mm)'
                          nticks   = 10
                          minval   = 0.0
                          maxval   = 1.0
        	 end

    'lwp':        begin
                          prodTxt  = ' Liquid Water Path '
                          minval_bias = -0.2
                          maxval_bias = 0.2
                          div      = 10
                          fmt      = '(f5.2)'
                          unit     = '(mm)'
                          nticks   = 10
                          minval   = 0.0
                          maxval   = 1.0
        	 end

    'nattempt':   begin
                          prodTxt  = ' Number of Attempts '
                          minval_bias = -4
                          maxval_bias = 4
                          div      = 8
                          fmt      = '(I2)'
                          unit     = ''
                          nticks   = 2
                          minval   = 0
                          maxval   = 2
        	end

    'niter':      begin
                          prodTxt  = ' Number of Iterations '
                          minval_bias = -14
                          maxval_bias = 14
                          div      = 14
                          fmt      = '(I3)'
                          unit     = ''
                          nticks   = 7
                          minval   = 0
                          maxval   = 7
        	 end

    'psfc':       begin
                          prodTxt  = ' Surface Pressure '
                          minval_bias = -100
                          maxval_bias = 100
                          div      = 10
                          fmt      = '(I4)'
                          unit     = '(mb)'
                          nticks   = 7
                          minval   = 700
                          maxval   = 1050
                  end

    'qc':         begin

                          prodTxt = ' Geophysical Events '
                          minval_bias = -18
                          maxval_bias = 18
                          div      = 18
                          fmt      = '(I3)'
                          unit     = ''
                          nticks   = 9
                          minval   = 0
                          maxval   = 9
                  end

    'rr':         begin
                          prodTxt  = ' Rain Rate '
                          minval_bias = -5
                          maxval_bias = 5
                          div      = 10
                          fmt      = '(f5.1)'
                          unit     = '(mm/hr)'
                          nticks   = 10
                          minval   = 0
                          maxval   = 10
                  end

    'rwp':        begin
                          prodTxt  = ' Rain Water Path '
                          minval_bias = -0.2
                          maxval_bias = 0.2
                          div      = 10
                          fmt      = '(f5.2)'
                          unit     = '(mm)'
                          nticks   = 10
                          minval   = 0.0
                          maxval   = 1.0
        	 end

    'scanday':    begin
                          prodTxt  = ' Scan Time '
                          minval_bias = -4
                          maxval_bias = 4
                          div      = 8
                          fmt      = '(f5.1)'
                          unit     = '(hour)'
                          nticks   = 8
                          minval   = jday_min+0.5
                          maxval   = jday_max+0.5
                  end

    'scanpos':    begin
                          prodTxt  = ' Scan Position '
                          minval_bias = -30
                          maxval_bias = 30
                          div      = 12
                          fmt      = '(I3)'
                          unit     = ''
                          nticks   = 6
                          minval   = 1
                          maxval   = 30 ; NFOV
        	 end

    'sfcId':        begin
                          prodTxt  = ' Pre-Classified Sfc Type '
                          minval_bias = -6
                          maxval_bias = 6
                          div      = 6
                          fmt      = '(I2)'
                          unit     = ''
                          nticks   = 3
                          minval   = 0
                          maxval   = 3
        	 end

    'sfc2':       begin
                          prodTxt  = ' Post-Processed Sfc Type '
                          minval_bias = -6
                          maxval_bias = 6
                          div      = 6
                          fmt      = '(I2)'
                          unit     = ''
                          nticks   = 3
                          minval   = 0
                          maxval   = 3
        	 end

    'sice':       begin
                          prodTxt  = ' Sea Ice Concentration '
                          minval_bias = -10
                          maxval_bias = 10
                          div      = 10
                          fmt      = '(I3)'
                          unit     = '(%)'
                          nticks   = 10
                          minval   = 0
                          maxval   = 100
        	 end

    'sicefy':     begin
                          prodTxt  = ' First Year Sea Ice Concentration '
                          minval_bias = -10
                          maxval_bias = 10
                          div      = 10
                          fmt      = '(I3)'
                          unit     = '(%)'
                          nticks   = 10
                          minval   = 0
                          maxval   = 100
                  end

    'sicemy':     begin
                          prodTxt  = ' Multiple Year Sea Ice Concentration '
                          minval_bias = -10
                          maxval_bias = 10
                          div      = 10
                          fmt      = '(I3)'
                          unit     = '(%)'
                          nticks   = 10
                          minval   = 0
                          maxval   = 100
                  end

    'snow':       begin
                          prodTxt  = ' Snow Cover '
                          minval_bias = -1
                          maxval_bias = 1
                          div      = 2
                          fmt      = '(I2)'
                          unit     = ''
                          nticks   = 2
                          minval   = 0
                          maxval   = 2
                  end

    'swe':        begin
                          prodTxt  = ' Snow Water Equivalent '
                          minval_bias = -4
                          maxval_bias = 4
                          div      = 8
                          fmt      = '(I2)'
                          unit     = '(cm)'
                          nticks   = 8
                          minval   = 0
                          maxval   = 8
                  end

    'tpw':        begin
                          prodTxt  = ' TPW '
                          minval_bias = -10
                          maxval_bias = 10
                          div      = 10
                          fmt      = '(I3)'
                          unit     = '(mm)'
                          nticks   = 7
                          minval   = 0
                          maxval   = 70
                  end

    'tskin':      begin
                          prodTxt  = ' Skin Temperature '
                          minval_bias = -10
                          maxval_bias = 10
                          div      = 10
                          fmt      = '(I3)'
                          unit     = '(K)'
                          nticks   = 5
                          minval   = 200
                          maxval   = 325
                  end

    else:         begin
                          print, 'Unsupported sfcId prodId: '+ prodId
                  end
    endcase

    ;---- read in product set ----  
    fileProd1=dirGrid1+'GRID_'+satId1+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileProd1)
    if result eq 0 then begin
      print, 'File not exist: ' + fileProd1
      continue 
    endif

    fileProd2=dirGrid2+'GRID_'+satId2+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileProd2)
    if result eq 0 then begin
      print, 'File not exist: ' + fileProd2
      continue 
    endif

    product1s = fltarr(NCOL,NROW)
    openr,lun,fileProd1,/get_lun,/Swap_Endian
    readu,lun,product1s
    free_lun,lun

    product2s = fltarr(NCOL,NROW)
    openr,lun,fileProd2,/get_lun,/Swap_Endian
    readu,lun,product2s
    free_lun,lun
    
    ;---- scattered plot ----
    for isfc = 0, nsfc - 1 do begin
      sfcTxt = sfcTxts(isfc)
      if isfc eq (nsfc-1) then begin
	ss = where( ABS(product1s) le maxval  and $
                    ABS(product2s) le maxval, cnt)
      endif else begin
	ss = where( ABS(product1s) le maxval and $
                    ABS(product2s) le maxval and $
                    FIX(sfcDep1s)  eq isfc   and $
                    FIX(sfcDep2s)  eq isfc,  cnt)
      endelse
      sfcId = sfcIds(isfc)
          
      if cnt gt CONT_MIN then begin
        
	;---- bias plot ----
	png_scat=dirImg+prefix_scat+yyyymmdd+'_'+prodId+'_'+sfcId+'_'+cend+'.png'
	plot_scatter,minval,maxval,minval,maxval,product1s(ss),product2s(ss),satId1,satId2,$
	             cendTxt+sfcTxt+prodTxt+date+' (V' + version +')',png_scat, nticks=nticks
	
	;---- hist plot ----
	bias=fltarr(NCOL,NROW) & bias(*,*) = MISSING_FLT
	bias(ss) = product1s(ss) - product2s(ss)

	minvalue = minval_bias
	maxvalue = maxval_bias

	png_hist = dirImg + prefix_hist + yyyymmdd +'_' + prodId + '_'+sfcId+'_ad.png'
    	tit = 'MIRS '+strupcase(satId1)+' - '+ strupcase(satId2)+prodTxt+unit+' '+date+' (V'+version +')'
    	xtit=prodTxt+'( '+strupcase(satId1)+' - '+strupcase(satId2)+' ) Over ' + sfcTxt
    	ytit='Density (%) Over ' + sfcTxt

    	xlength=(maxvalue-minvalue)*1.0/(NBINS_HIST*1.0)
    	x=findgen(NBINS_HIST)*xlength+minvalue

	h_bias = bias(ss)
      	h=Histogram(h_bias, NBINS=NBINS_HIST)
      	h = h/float(total(h))*100.
      	xshift=minvalue
      	xrange=[minvalue ,maxvalue]
      	file_as=dirGrid1+satId1+'_'+satId2+'_hist_'+yyyymmdd+'_'+prodId+'_'+sfcId+'_as.dat'
        mean_bias=mean(bias(ss))

      	if cend eq 'as' then begin

	  openw, lunw, file_as, /get_lun
	  writeu, lunw, h
	  free_lun, lunw
          mean_as = mean_bias

      	endif else begin

	  h_ds = h
	  mean_ds = mean_bias

	  is_file_exist = FILE_TEST(file_as)

	  if is_file_exist eq 1 then begin 
	    openr, lunr, file_as, /get_lun

	    ;Get file status. 
	    status = FSTAT(lunr) 

	    h_as = FLTARR(status.size / 4) 
	    readu, lunr, h_as  
	    free_lun, lunr

	    ; then delete saved ascending stat file 
	    FILE_DELETE, file_as

	    ymax = max([h_as,h_ds])
	    yrange=[0,ymax*1.05]

	    plot_hist_line2, x, h_as, h_ds, tit, xtit, ytit, png_hist, xrange, yrange, leg_as, leg_ds, mean_as, mean_ds
 	  endif

      	endelse
	
      endif
      
    endfor 
    
    ;---- bias and asymmetry ----
    biasAsym2D,prodId,prodTxt,unit,product1s,product2s,satId1,satId2,yyyymmdd,cend,$
      angle1s,sfcDep1s,sfcDep2s,sfcMask,minval,maxval,minval_bias,maxval_bias,div,fmt,$
      NCOL,NROW,NBIN,LWP_CUT,RWP_CUT,dirImg,prefix_bias,prefix_asym,version,$
      latmin,latmax,lonmin,lonmax,timediffs,lwp1s,lwp2s,rwp1s,rwp2s

  endfor ; loop iprod for sfcId ends
  ;-------------------------------------------------------------------
  ; sfcId products section ends
  ;-------------------------------------------------------------------


  ;------------------------------------------------------------- -----
  ; layer products section begins
  ;-------------------------------------------------------------------
  for iprod = 0, nprod_lay - 1 do begin ; loop iprod for lay starts
    prodId = prodIds_lay[iprod]
    print, 'plotting ' + prodId + ' ' + cend + ' ...'

    case  prodId of

    'temp':       begin
                    prodTxt = ' Temperature '
                    minvalues = replicate(-6,11)
                    maxvalues = replicate(6,11)
                    div       = 10
                    fmt       = '(I3)'
                    unit      = '(K)'
                    minvals   = [180,200,200,210,220,225,230,235,235,235,235]
                    maxvals   = [240,245,250,270,280,285,290,305,310,315,315]
                    ticks     = [ 6,  9,  5,  6,  6,  6,  6,  7,  5,  8,  8 ]
                    TQ        = 0
                  end

    'wv':         begin
                    prodTxt = ' Water Vapor Content '
                    minvalues = [-0.05,-0.05,-0.05,-0.05,-1,-1,-2,-2,-2,-2,-2]
                    maxvalues = [ 0.05, 0.05, 0.05, 0.05, 1, 1, 2, 2, 2, 2, 2]
                    div       = 10
                    fmt       = '(f5.2)'
                    unit      = '(g/kg)'
                    minvals   = replicate(0.0,NLAY)
                    maxvals   = [0.1, 0.15, 1.5, 4.0, 8.0, 10.0, 15.0, 20.0, 20.0, 20.0, 20.0]
                    ticks     = [ 10,   5,   5,  10,   8,   10,   10,   10,   10,  10,    10 ]
                    TQ        = 1  
                  end

    'clwp':       begin
                    prodTxt  = ' CLW '
                    minvalues = replicate(-0.1,NLAY)
                    maxvalues = replicate(0.1,NLAY)
                    div       = 10
                    fmt       = '(f6.3)'
                    unit      = '(mm)'
                  end

    'rainp':      begin
                    prodTxt  = ' RAINP '
                    minvalues = replicate(-0.1,NLAY)
                    maxvalues = replicate(0.1,NLAY)
                    div       = 10
                    fmt       = '(f6.3)'
                    unit      = '(mm)'
                  end

    'graupelp':   begin
                    prodTitle =' Graupel '
                    minvalues = replicate(-0.1,NLAY)
                    maxvalues = replicate(0.1,NLAY)
                    div       = 10
                    fmt       = '(f6.3)'
                    unit      = '(mm)'
                  end

    else:         begin
                    print, 'Unsupported layer prodId: '+ prodId
                  end

    endcase

    tmp=fltarr(NCOL,NROW)

    fileProd1=dirGrid1+'GRID_'+satId1+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileProd1)
    if result eq 0 then begin
      print, 'File not exist: ' + fileProd1
      continue 
    endif

    fileProd2=dirGrid2+'GRID_'+satId2+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileProd2)
    if result eq 0 then begin
      print, 'File not exist: ' + fileProd2
      continue 
    endif

    product1s = fltarr(NCOL,NROW,NLAY)
    openr, lun, fileProd1, /get_lun, /Swap_Endian
    for ilay_grid = 0, NLAY_GRID - 1 do begin
      readu, lun, tmp
      ilay = where(ilay_grid eq LAYERS_INDEX,cnt)
      if cnt eq 1 then product1s(*,*,ilay) = tmp(*,*)
    endfor
    free_lun,lun,/force

    product2s = fltarr(NCOL,NROW,NLAY)
    openr, lun, fileProd2, /get_lun, /Swap_Endian
    for ilay_grid = 0, NLAY_GRID - 1 do begin
      readu, lun, tmp
      ilay = where(ilay_grid eq LAYERS_INDEX,cnt)
      if cnt eq 1 then product2s(*,*,ilay) = tmp(*,*)
    endfor
    free_lun,lun,/force


    bias = fltarr(4,NLAY) & bias(*,*)  = -9999.
    stdv = fltarr(4,NLAY) & stdv(*,*)  = -9999.
    
    ;---- scattered plot ----
    for ilay = 0, NLAY - 1 do begin ; loop ilay starts

      value1 = product1s(*,*,ilay)
      value2 = product2s(*,*,ilay)
      minval = minvals(ilay)
      maxval = maxvals(ilay)
      nticks = ticks(ilay)

      for  isfc = 0, nsfc - 1 do begin ; loop isfc starts
	sfcId = sfcIds(isfc)
	sfcTxt = sfcTxts(isfc)
	if isfc eq (nsfc-1) then $
          ss = where( value1 ge minval and value2 ge minval and $
                      value1 le maxval and value2 le maxval and $
                      timediffs le TIME_CUT and $
                      lwp1s le LWP_CUT and lwp2s le LWP_CUT,cnt)

	if isfc eq 0 or isfc eq 1 then $
          ss = where( sfcDep1s eq isfc and sfcDep2s eq isfc and $ 
                      value1 ge minval and value2 ge minval and $
                      value1 le maxval and value2 le maxval and $
                      timediffs le TIME_CUT and $
                      rwp1s le RWP_CUT and rwp2s le RWP_CUT,cnt)

	if isfc eq 2 or isfc eq 3 then $
          ss = where( sfcDep1s eq isfc and sfcDep2s eq isfc and $ 
                      value1 ge minval and value2 ge minval and $
                      value1 le maxval and value2 le maxval and $
                      timediffs le TIME_CUT and $
                      lwp1s le LWP_CUT and lwp2s le LWP_CUT,cnt)

	if cnt gt CONT_MIN then begin
	
          if isfc lt nsfc-1 then begin
            c10 = moment(value1(ss)-value2(ss))
            stats1 = c10[0]
            stats2 = stdev(value1(ss)-value2(ss))
            bias(isfc,ilay) = stats1
            stdv(isfc,ilay) = stats2 
          endif
	  
	  ;---- scatter plot ----
          png_scat = dirImg+prefix_scat+yyyymmdd+'_'+prodId+'_'+layIds(ilay)+'_'+sfcId+'_'+cend+'.png'
          plot_scatter, minval,maxval,minval,maxval,value1(ss),value2(ss),satId1,satId2,cendTxt+' '+$
	                sfcTxt+prodTxt+'@ '+layIds(ilay)+' '+date+' (V' + version +')',png_scat,nticks=nticks
			
	  ;---- hist plot ----
	  bias=fltarr(NCOL,NROW) & bias(*,*) = MISSING_FLT
	  bias(ss) = value1(ss) - value2(ss)
	 
	  minvalue=minvalues(ilay)
	  maxvalue=maxvalues(ilay)
    
	  png_hist = dirImg + prefix_hist + yyyymmdd +'_' + prodId + '_' + layIds[ilay] + '_'+sfcId+'_ad.png'
    	  tit = 'MIRS '+strupcase(satId1)+' - '+ strupcase(satId2)+prodTxt+unit+' @ '+layIds[ilay]+' '+date+' (V'+version +')'
    	  xtit=prodTxt+'( '+strupcase(satId1)+' - '+strupcase(satId2)+' ) Over ' + sfcTxt
    	  ytit='Density (%) Over ' + sfcTxt
    
    	  xlength=(maxvalue-minvalue)*1.0/(NBINS_HIST*1.0)
    	  x=findgen(NBINS_HIST)*xlength+minvalue
	 
	  h_bias = bias(ss)
      	  h=Histogram(h_bias, NBINS=NBINS_HIST)
      	  h = h/float(total(h))*100.
      	  xshift=minvalue
      	  xrange=[minvalue ,maxvalue]
      	  file_as=dirGrid1+satId1+'_'+satId2+'_hist_'+yyyymmdd+'_'+prodId+'_'+layIds[ilay]+'_'+sfcId+'_as.dat'
          mean_bias=mean(bias(ss))
	  
      	  if cend eq 'as' then begin

	    openw, lunw, file_as, /get_lun
	    writeu, lunw, h
	    free_lun, lunw
            mean_as = mean_bias
	    
      	  endif else begin

	    h_ds = h
	    mean_ds = mean_bias
	    
	    is_file_exist = FILE_TEST(file_as)
	    
	    if is_file_exist eq 1 then begin 
	      openr, lunr, file_as, /get_lun

	      ;Get file status. 
	      status = FSTAT(lunr) 

	      h_as = FLTARR(status.size / 4) 
	      readu, lunr, h_as  
	      free_lun, lunr

	      ; then delete saved ascending stat file 
	      FILE_DELETE, file_as

	      ymax = max([h_as,h_ds])
	      yrange=[0,ymax*1.05]

	      plot_hist_line2, x, h_as, h_ds, tit, xtit, ytit, png_hist, xrange, yrange, leg_as, leg_ds, mean_as, mean_ds
 	    endif
	    
      	  endelse
			
	endif

      endfor ; loop isfc ends

    endfor ; loop ilay ends

    ;---- add as/ds combined to be used later ----  
    if TQ eq 0 then begin
      bias_temp(*,*) = bias_temp(*,*) + 0.5 * bias(*,*)
      stdv_temp(*,*) = stdv_temp(*,*) + 0.5 * stdv(*,*)
    endif else begin
      bias_wv(*,*)   = bias_wv(*,*)   + 0.5 * bias(*,*)
      stdv_wv(*,*)   = stdv_wv(*,*)   + 0.5 * stdv(*,*)
    endelse 

    ;---- mean bias vertical bias distribution
    title_bias='MIRS '+strupcase(satId1)+' - '+strupcase(satId2)+prodTxt+$
               ' Mean Bias Vert. Distri. '+date+cendTxt + ' (V' + version +')'
    fileImg=dirImg+prefix+yyyymmdd+'_'+prodId+'_mean_vert_'+cend+'.png'
    xrange=[-6,6]
    yrange=[950,100]
    plot_line4, bias(0,*), bias(1,*), bias(2,*), bias(3,*), layers, $
        	'Mean Bias (K)', 'Pressure (mb)', title_bias, $
		xrange,yrange,fileImg,'Sea','Sea Ice','Land','Snow',$
        	1, yticknames, yticks, TQ

    ;---- stdv vertical bias distribution
    title_stdv='MIRS '+strupcase(satId1)+' - '+strupcase(satId2)+$
               prodTxt+' STDV Vert. Distri. '+date+$
               cendTxt+' (V' + version +')'
    fileImg=dirImg+prefix+yyyymmdd+'_'+prodId+'_stdv_vert_'+cend+'.png'
    xrange=[0,6]
    yrange=[950,100]
    plot_line4, stdv(0,*), stdv(1,*), stdv(2,*), stdv(3,*), layers, $
        	'Standard Deviation', 'Pressure (mb)', title_stdv, $
		xrange,yrange,fileImg,'Sea','Sea Ice','Land','Snow',$
        	0, yticknames, yticks, TQ
    
    ;---- bias and asymmetry ----
    biasAsym3D,prodId,prodTxt,unit,product1s,product2s,satId1,satId2,yyyymmdd,cend,$
      angle1s,sfcDep1s,sfcDep2s,sfcMask,minvalues,maxvalues,div,fmt,$
      NCOL,NROW,NBIN,LWP_CUT,RWP_CUT,dirImg,prefix_bias,prefix_asym,version,$
      latmin,latmax,lonmin,lonmax,timediffs,lwp1s,lwp2s,rwp1s,rwp2s,NLAY,layIds

  endfor ; loop iprod for lay ends
  ;-------------------------------------------------------------------
  ; layer products section ends
  ;-------------------------------------------------------------------
  

  ;-------------------------------------------------------------------
  ; channel products section begins
  ;-------------------------------------------------------------------
  for iprod = 0, nprod_chan - 1 do begin ; loop iprod for chan starts
    prodId = prodIds_chan[iprod]
    print, 'plotting ' + prodId + ' ' + cend + ' ...'

    case  prodId of

    'em':         begin
                    prodTxt = ' Emissivity '
                    minvalues = replicate(-0.1,NCHAN0)
                    maxvalues = replicate(0.1,NCHAN0)
                    div       = 10
                    fmt       = '(f5.2)'
                    unit      = ''
                  end

    'tbc':         begin
                    prodTxt = ' Corr. TB '
                    minvalues = replicate(-15,NCHAN0)
                    maxvalues = replicate(15,NCHAN0)
                    div       = 9
                    fmt       = '(I3)'
                    unit      = '(K)'
                  end

    'tbu':       begin
                    prodTxt   = ' UnCorr. TB '
                    minvalues = replicate(-15,NCHAN0)
                    maxvalues = replicate(15,NCHAN0)
                    div       = 10
                    fmt       = '(I3)'
                    unit      = '(K)'
                  end
    else:         begin
                    print, 'Unsupported channel prodId: '+ prodId
                  end

    endcase

    tmp=fltarr(NCOL,NROW)

    fileProd1=dirGrid1+'GRID_'+satId1+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileProd1)
    if result eq 0 then begin
      print, 'File not exist: ' + fileProd1
      continue 
    endif

    fileProd2=dirGrid2+'GRID_'+satId2+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileProd2)
    if result eq 0 then begin
      print, 'File not exist: ' + fileProd2
      continue 
    endif

    product1s = fltarr(NCOL,NROW,NCHAN1)
    openr, lun, fileProd1, /get_lun, /Swap_Endian
    for ichan = 0, NCHAN1 - 1 do begin
      readu, lun, tmp
      product1s(*,*,ichan) = tmp(*,*)
    endfor
    free_lun,lun,/force

    product2s = fltarr(NCOL,NROW,NCHAN2)
    openr, lun, fileProd2, /get_lun, /Swap_Endian
    for ichan = 0, NCHAN2 - 1 do begin
      readu, lun, tmp
      product2s(*,*,ichan) = tmp(*,*)
    endfor
    free_lun,lun,/force
    
    ;---- pick out matched channels based on pre-defined index values ----
    save_product1s = product1s
    save_product2s = product2s
    undefine,product1s
    undefine,product2s
    product1s = fltarr(NCOL,NROW,NCHAN0)
    product2s = fltarr(NCOL,NROW,NCHAN0)

    for ichan = 0, NCHAN0-1 do begin
      product1s(*,*,ichan) = save_product1s(*,*,inds1(ichan))
      product2s(*,*,ichan) = save_product2s(*,*,inds2(ichan))
    endfor
    
    if prodId eq 'tbc' or prodId eq 'tbu' then begin

      minval1 = [125,125,170,170,200, 200,210,200,180,190, 180,180,180,200,140, 140,140,200,160,140]
      maxval1 = [325,325,310,290,270, 250,240,240,250,250, 250,250,280,290,320, 320,320,290,300,300]
      ticks   = [ 8,  8,  7,  6,  7,   5,  6,  8,  7,  6,   7,  7, 10,  9,  9,   8,  8,  9,  7,  8 ]
	 
      if pair eq 'f16_f18' then begin
	;       57rc:59rc:150h: 190h:186h:184h: 19h:19v:22v: 37h:37v: 91v:91h: 63rc:60rc1:60rc2:60rc3:60rc4:60rc5
	minval1=[190, 190, 140, 140, 160, 160,  75, 150,140, 100,150, 140,140, 200,  180,  210,  200,  190, 190]
	maxval1=[240, 240, 300, 300, 300, 300,  300,300,300, 300,310, 300,300, 260,  250,  270,  270,  260, 240]
	ticks=  [ 5,   5,   8,   8,   7,   7,    9,  6,  8,   8,  8,   8,  8,   6,    7,    6,    7,    7,   10] 
      endif
      
      if pair eq 'n18_npp' or pair eq 'n19_npp' or pair eq 'metopA_npp' or pair eq 'metopB_npp' then begin
        ;            1   2   3   4   5   6   7   8   9   10  11 12  13  14  15  16  17  18  19  20  21  22
        ;minvals = [140,140,170,170,170,200,200,210,200,200,180,180,180,180,200,150,150,220,220,220,200,220]
        ;maxvals = [300,300,290,290,290,270,250,230,230,230,230,250,250,280,290,300,300,300,300,280,300,265]
        ;ticks   = [ 8,  8,  6,  6,  6,  7,  5,  4,  6,  6,  5,  7,  7, 10,  9,  5,  5,  8,  8,  6, 10,  9]
	;           1   2   6	7   9	10  11 12  13  14  15  22  20 
	minval1 = [140,140,200,200,200,200,180,180,180,180,200,220,220]
        maxval1 = [300,300,270,250,230,230,230,250,250,280,290,265,280]
	ticks   = [ 8,  8,  7,  5,  6,  6,  5,  7,  7, 10,  9,  9,  6 ]
      endif
	
      minvals = [ [minval1],[minval1],[minval1],[minval1],[minval1] ]
      maxvals = [ [maxval1],[maxval1],[maxval1],[maxval1],[maxval1] ]

    endif else begin  ;  em branch starts
      
      ;----( NCHAN0,NSFC ) -- default to POES-POES pair
      mins_sea  = [0.40,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.60,0.65,0.65,0.65]
      maxs_sea  = [0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95]
      mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.60,0.60,0.60,0.60,0.60,0.60]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]

      if pair eq 'f16_f18' then begin ; F16 vs F18 has 19 matched channels, first 5 not matched F16(v) F18(h)
	mins_sea  = [0.50,0.50,0.50,0.50,0.50,0.50,0.25,0.55,0.55,0.30,0.60,0.70,0.40,0.55,0.55,0.55,0.55,0.55,0.55] 
	maxs_sea  = [0.95,0.95,0.95,0.90,0.90,0.90,0.90,0.95,0.95,0.90,0.95,0.95,0.90,0.95,0.95,0.95,0.95,0.95,0.95]
	mins_lnd  = [0.50,0.50,0.70,0.70,0.70,0.70,0.65,0.75,0.75,0.60,0.75,0.70,0.70,0.55,0.55,0.55,0.55,0.55,0.55]
	maxs_lnd  = [1.00,1.00,0.95,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,0.95,0.95,0.95,0.95,0.95,0.95]
      endif

      if pair eq 'n18_npp' or pair eq 'n19_npp' or pair eq 'metopA_npp' or pair eq 'metopB_npp' then begin
	;              1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22
	mins_sea  = [0.40,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
	maxs_sea  = [0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95]
	mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60]
	maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
	;              1    2                   6    7         9   10   11    12  13   14   15                        20        22 
	ind1 = [1,2,6,7,9,10,11,12,13,14,15,22,20]
	ind0 = ind1-1
	mins_sea = mins_sea(ind0)
	maxs_sea = maxs_sea(ind0)
	mins_lnd = mins_lnd(ind0)  
	maxs_lnd = maxs_lnd(ind0)     
      endif
      
      ;---- default to sea values, then overwrite by land values if necessary
      mins_all = mins_sea
      maxs_all = maxs_sea

      for ichan = 0, NCHAN0 - 1 do begin
	if mins_lnd(ichan) lt mins_all(ichan) then mins_all(ichan) = mins_lnd(ichan)
	if maxs_lnd(ichan) gt maxs_all(ichan) then maxs_all(ichan) = maxs_lnd(ichan)
      endfor

      ;---- ice and snow use the same ranges as all
      mins_ice = mins_all
      maxs_ice = maxs_all

      mins_snw = mins_all
      maxs_snw = maxs_all

      minvals = [ [mins_sea], [mins_ice], [mins_lnd], [mins_snw], [mins_all] ]
      maxvals = [ [maxs_sea], [maxs_ice], [maxs_lnd], [maxs_snw], [maxs_all] ]
      ticks   = replicate(10,NCHAN0)
     
    endelse  ; em branch ends

    
    ;---- scattered plot ----
    for ichan = 0, NCHAN0 - 1 do begin ; ichan starts

      value1 = product1s(*,*,ichan)
      value2 = product2s(*,*,ichan)
      nticks = ticks(ichan)

      ;for  isfc = 0, 0 do begin ; isfc starts
      for  isfc = 0, nsfc - 1 do begin ; isfc starts

        minval = minvals(ichan,isfc)
        maxval = maxvals(ichan,isfc)

	sfcId = sfcIds(isfc)
	sfcTxt = sfcTxts(isfc)
	
	if isfc eq (nsfc-1) then begin
          ss = where( value1 ge minval and value2 ge minval and $
                      value1 le maxval and value2 le maxval and $
                      timediffs le TIME_CUT,cnt)
	endif else begin
          ss = where( sfcDep1s eq isfc and sfcDep2s eq isfc and $ 
                      value1 ge minval and value2 ge minval and $
                      value1 le maxval and value2 le maxval and $
                      timediffs le TIME_CUT,cnt)
	endelse

	if cnt gt CONT_MIN then begin
          
	  ;---- bias plot ----
          png_scat = dirImg+prefix_scat+yyyymmdd+'_'+prodId+'_'+chanIds(ichan)+'_'+sfcId+'_'+cend+'.png'
          plot_scatter, minval,maxval,minval,maxval,value1(ss),value2(ss),satId1,satId2,cendTxt+' '+sfcTxt+prodTxt+'@ Ch'+$
                	strtrim(ichan+1,2)+'('+chanTxts(ichan)+') '+date+' (V' + version +')',png_scat,nticks=nticks
	  
	  ;---- hist plot ----
	  bias = fltarr(NCOL,NROW) & bias(*,*) = MISSING_FLT
	  bias(ss) = value1(ss) - value2(ss)
	 
	  minvalue=minvalues(ichan)
	  maxvalue=maxvalues(ichan)
    
	  png_hist = dirImg + prefix_hist + yyyymmdd +'_' + prodId + '_' + chanIds[ichan] + '_'+sfcId+'_ad.png'
    	  tit = 'MIRS '+strupcase(satId1)+' - '+ strupcase(satId2)+prodTxt+unit+' @ Ch '$
	      +strtrim(ichan+1,2)+'('+chanTxts[ichan]+') '+date+' (V'+version +')'
    	  xtit=prodTxt+'( '+strupcase(satId1)+' - '+strupcase(satId2)+' ) Over ' + sfcTxt
    	  ytit='Density (%) Over ' + sfcTxt
    
    	  xlength=(maxvalue-minvalue)*1.0/(NBINS_HIST*1.0)
    	  x=findgen(NBINS_HIST)*xlength+minvalue
	 
	  h_bias = bias(ss)
      	  h=Histogram(h_bias, NBINS=NBINS_HIST)
      	  h = h/float(total(h))*100.
      	  xshift=minvalue
      	  xrange=[minvalue ,maxvalue]
      	  file_as=dirGrid1+satId1+'_'+satId2+'_hist_'+yyyymmdd+'_'+prodId+'_'+chanIds[ichan]+'_'+sfcId+'_as.dat'
          mean_bias=mean(bias(ss))
	  
      	  if cend eq 'as' then begin

	    openw, lunw, file_as, /get_lun
	    writeu, lunw, h
	    free_lun, lunw
            mean_as = mean_bias
	    
      	  endif else begin

	    h_ds = h
	    mean_ds = mean_bias
	    
	    is_file_exist = FILE_TEST(file_as)
	    
	    if is_file_exist eq 1 then begin 
	      openr, lunr, file_as, /get_lun

	      ;Get file status. 
	      status = FSTAT(lunr) 

	      h_as = FLTARR(status.size / 4) 
	      readu, lunr, h_as  
	      free_lun, lunr

	      ; then delete saved ascending stat file 
	      FILE_DELETE, file_as

	      ymax = max([h_as,h_ds])
	      yrange=[0,ymax*1.05]

	      plot_hist_line2, x, h_as, h_ds, tit, xtit, ytit, png_hist, xrange, yrange, leg_as, leg_ds, mean_as, mean_ds
 	    endif
	    
      	  endelse
	
	endif else begin
	  plot_scatter_dummy, minval,maxval,minval,maxval,satId1,satId2,cendTxt+' '+sfcTxt+prodTxt+'@ '+$
                	      chanTxts(ichan)+' '+date+' (V' + version +')',fileImg,nticks=nticks
	endelse
			
      endfor ; isfc ends

    endfor ; ichan ends
    
    ;---- bias and asymmetry ----
    biasAsym3D,prodId,prodTxt,unit,product1s,product2s,satId1,satId2,yyyymmdd,cend,angle1s,$
      sfcDep1s,sfcDep2s,sfcMask,minvalues,maxvalues,div,fmt,NCOL,NROW,NBIN,LWP_CUT,RWP_CUT,dirImg,$
      prefix_bias,prefix_asym,version,latmin,latmax,lonmin,lonmax,timediffs,$
      lwp1s,lwp2s,rwp1s,rwp2s,NCHAN0,chanIds

  endfor ; loop iprod for chan ends
  ;-------------------------------------------------------------------
  ; channel products section ends
  ;-------------------------------------------------------------------

endfor  ; loop icend ends


;---------------------------------------------------------------------
;plot combined (ad) for temp and wv bias/stdv vertical distri.
;---------------------------------------------------------------------

;---- clear possible negative values and re-set them as -9999.0 ------
ss = where(bias_temp lt -1000.0, cnt)
if cnt gt 0 then bias_temp(ss) = -9999.0

ss = where(stdv_temp lt -1000.0, cnt)
if cnt gt 0 then stdv_temp(ss) = -9999.0

ss = where(bias_wv lt -1000.0, cnt)
if cnt gt 0 then bias_wv(ss) = -9999.0

ss = where(stdv_wv lt -1000.0, cnt)
if cnt gt 0 then stdv_wv(ss) = -9999.0


;---- temp bias ----
TQ = 0
title='MIRS ' + strupcase(satId1) + ' - ' + strupcase(satId2) + $
      ' Temp. Mean Bias Vert. Distri. ' + date + $
      ' Combined' + ' (V' + version +')'
fileImg=dirImg+prefix+yyyymmdd+'_temp_mean_vert_ad.png'
xrange=[-6,6]
yrange=[950,100]
plot_line4,bias_temp(0,*),bias_temp(1,*),bias_temp(2,*),bias_temp(3,*),$
           layers, 'Mean Bias (K)', 'Pressure (mb)', title, $
	   xrange, yrange, fileImg, 'Sea', 'Sea Ice', 'Land', 'Snow',$
           1, yticknames, yticks, TQ

;---- temp stdv ----
title='MIRS ' + strupcase(satId1)  + ' - ' + strupcase(satId2) + $
      ' Temp. Stdv Vert. Distri. ' + date + $
      ' Combined' + ' (V' + version +')'
fileImg=dirImg+prefix+yyyymmdd+'_temp_stdv_vert_ad.png'
xrange=[0,6]
yrange=[950,100]
plot_line4,stdv_temp(0,*),stdv_temp(1,*),stdv_temp(2,*),stdv_temp(3,*),$
           layers, 'Standard Deviation', 'Pressure (mb)', title,  $
	   xrange, yrange, fileImg, 'Sea', 'Sea Ice', 'Land', 'Snow', $
           0, yticknames, yticks, TQ


;---- wv bias ----
TQ = 1
title='MIRS ' + strupcase(satId1) + ' - ' + strupcase(satId2) + $
      ' Water Vapor Mean Bias Vert. Distri. ' + date + $
      ' Combined' + ' (V' + version +')'
fileImg=dirImg+prefix+yyyymmdd+'_wv_mean_vert_ad.png'
xrange=[-100,100]
yrange=[950,100]
plot_line4, bias_wv(0,*), bias_wv(1,*), bias_wv(2,*), bias_wv(3,*),$
            layers, 'Mean Bias (%)', 'Pressure (mb)', title,  $
	    xrange, yrange, fileImg, 'Sea', 'Sea Ice', 'Land', 'Snow',$
            1, yticknames, yticks, TQ

;---- wv stdv ----
title='MIRS ' + strupcase(satId1)  + ' - ' + strupcase(satId2) + $
      ' Water Vapor. STDV Vert. Distri. ' + date + $
      ' Combined' + ' (V'+version+')'
fileImg=dirImg+prefix+yyyymmdd+'_wv_stdv_vert_ad.png'
xrange=[0,100]
yrange=[950,100]
plot_line4, stdv_wv(0,*), stdv_wv(1,*), stdv_wv(2,*), stdv_wv(3,*),$
            layers, 'Standard Deviation (%)', 'Pressure (mb)', title, $
	    xrange, yrange, fileImg, 'Sea', 'Sea Ice', 'Land', 'Snow', $
            0, yticknames, yticks, TQ

End
