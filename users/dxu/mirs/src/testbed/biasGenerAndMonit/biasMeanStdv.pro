@../../../setup/paths_idl.pro

PRO biasMeanStdv, nameList=nameList

;*************************************************************** 
; Read in or control section here
;*************************************************************** 
bias_dir='/disk1/pub/mirs_oper/data/SemiStaticData/biasCorrec'
figs_dir='./'
satId='npp'
nwpId=1
version='2970'

readFromList=1
if readFromList eq 1 then begin
  openr,iu,namelist,/get_lun
  readf,iu,format='(a)', bias_dir	   ;bias data Dir
  readf,iu,format='(a)', figs_dir	   ;figs Dir
  readf,iu,format='(a)', satId  	   ;satellite ID
  readf,iu,format='(i)', nwpId             ;nwpId
  readf,iu,format='(a)', version           ;MIRS version number
  close,iu
  free_lun,iu,/force
endif

;--------------------------------------
;     Set sensor-specific items
;--------------------------------------
satName=''
nwpStr=''

if ( satId eq 'n18' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'N18'
endif

if ( satId eq 'n19' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'N19'
endif

if ( satId eq 'metopA' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'MetOp-A'
endif

if ( satId eq 'metopB' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'MetOp-B'
endif

if ( satId eq 'f16' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'F16'
endif

if ( satId eq 'f17' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'F17'
endif

if ( satId eq 'f18' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'F18'
endif

if ( satId eq 'npp' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'NPP/ATMS'
endif

if ( satId eq 'mtma' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'MTMA'
endif

if ( satId eq 'mtsa' ) then begin
    NSFC  = 4
    NLAY = 11
    Layers2process  = ['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']
    satName = 'MTSA'
endif

if ( nwpId eq 1 ) then nwpStr = '_gdas'
if ( nwpId eq 2 ) then nwpStr = '_ecmwf'
if ( nwpId eq 3 ) then nwpStr = '_gfs'

;*************************************************************** 
; Search bias files ( filename: biasMeanStdv_metopA_ecmwf_20100802.dat )
;*************************************************************** 
bias_files  = FILE_SEARCH(bias_dir,'biasMeanStdv_'+satId+nwpStr+'_*.dat', COUNT=nfiles_bias)
;nfiles_bias = N_ELEMENTS(bias_files)
if nfiles_bias le 1 then begin
  print, 'No geophysical bias/stdv timeseries created for ', satId, ' No statistics files provided.'
  exit
endif

mean_temp = fltarr(nfiles_bias,NSFC,NLAY)
stdv_temp = fltarr(nfiles_bias,NSFC,NLAY)

mean_temp_as = fltarr(NSFC,NLAY)
stdv_temp_as = fltarr(NSFC,NLAY)

mean_temp_ds = fltarr(NSFC,NLAY)
stdv_temp_ds = fltarr(NSFC,NLAY)

mean_wv = fltarr(nfiles_bias,NSFC,NLAY-1)
stdv_wv = fltarr(nfiles_bias,NSFC,NLAY-1)

mean_wv_as = fltarr(NSFC,NLAY-1)
stdv_wv_as = fltarr(NSFC,NLAY-1)

mean_wv_ds = fltarr(NSFC,NLAY-1)
stdv_wv_ds = fltarr(NSFC,NLAY-1)


date = dblarr(nfiles_bias)

;*************************************************************** 
; Read bias files
;*************************************************************** 
FOR ifile = 0, nfiles_bias - 1 DO BEGIN
    
    yyyy = strmid(strtrim(file_basename(bias_files(ifile))),11,4,/reverse_offset)
    mm = strmid(strtrim(file_basename(bias_files(ifile))),7,2,/reverse_offset)
    dd = strmid(strtrim(file_basename(bias_files(ifile))),5,2,/reverse_offset)
    
    ;print, yyyy, mm, dd
    year  = float(yyyy)
    month = float(mm)
    day   = float(dd)
    
    date(ifile) = JULDAY(month,day,year)
    
    openr, lunr, bias_files(ifile), /get_lun
    
    ;---- Get file status
    info = FSTAT(lunr) 
    ;---- guard against non-correct size file
    if info.size ne 1344 then begin
        free_lun, lunr,/force
        continue
    endif
    
    readu, lunr, mean_temp_as, stdv_temp_as 
    readu, lunr, mean_temp_ds, stdv_temp_ds 
    readu, lunr, mean_wv_as,   stdv_wv_as 
    readu, lunr, mean_wv_ds,   stdv_wv_ds 
    free_lun, lunr,/force
    
    mean_temp(ifile,*,*) = ( mean_temp_as(*,*) + mean_temp_ds(*,*) ) / 2.0
    stdv_temp(ifile,*,*) = ( stdv_temp_as(*,*) + stdv_temp_ds(*,*) ) / 2.0

    mean_wv(ifile,*,*)   = ( mean_wv_as(*,*) + mean_wv_ds(*,*) ) / 2.0
    stdv_wv(ifile,*,*)   = ( stdv_wv_as(*,*) + stdv_wv_ds(*,*) ) / 2.0

ENDFOR



;*************************************************************** 
; Plot bias
;*************************************************************** 
x = date

range_temp_mean=[-6,6]
range_temp_stdv=[0,6]
range_wv_mean=[-100,100]
range_wv_stdv=[0,100]

bar_range_temp=[100,1000]
bar_range_wv=[200,1000]
div_temp=9
div_wv=8

NSFC=4
sfc_names=['sea','ice','lnd','snw']
sfc_titles=['Sea','Ice','Land','Snow']
size_strs=['_small', '', '_big' ]

FOR ISIZE=0, 2 DO BEGIN

  if ( ISIZE EQ 0 ) THEN BEGIN
      xticks=6
  endif
  if ( ISIZE EQ 1 ) THEN BEGIN
      xticks=6
  endif
  if ( ISIZE EQ 2 ) THEN BEGIN
      xticks=18
  endif

  size_str = size_strs[ISIZE]

  for isfc = 0, NSFC-1 do begin

    tmp=fltarr(nfiles_bias,NLAY)
    tmp(*,*)=mean_temp[*,isfc,*]
    png_file = figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_biasmean_glb_temp_' + sfc_names[isfc] + size_str + '.png'
    PLOT_BiasMeanStdv, x, tmp, '', 'Temperature Mean Bias (K)', $
  	  'MIRS ' + satName + ' Temp Mean Bias (K) Over ' + sfc_titles[isfc] + ' (V' + version +')',$
	  range_temp_mean, png_file, NLAY, xticks, 1, ISIZE, bar_range_temp, div_temp

    tmp(*,*)=stdv_temp[*,isfc,*]
    png_file = figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_biasstdv_glb_temp_' + sfc_names[isfc] + size_str + '.png'
    PLOT_BiasMeanStdv, x, tmp, '', 'Temperature Standard Deviation', $
  	  'MIRS ' + satName + ' Temp Stdv Over ' + sfc_titles[isfc] + ' (V' + version +')',$
	  range_temp_stdv, png_file, NLAY, xticks, 0, ISIZE, bar_range_temp, div_temp


    tmp=fltarr(nfiles_bias,NLAY-1)
    tmp(*,*)=mean_wv[*,isfc,*]
    png_file = figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_biasmean_glb_wv_' + sfc_names[isfc] + size_str + '.png'
    PLOT_BiasMeanStdv, x, tmp, '', 'Water Vapor (%)',$
  	  'MIRS ' + satName + ' WV Mean Bias (%) Over ' + sfc_titles[isfc] + ' (V' + version +')',$
	  range_wv_mean, png_file, NLAY-1, xticks, 1, ISIZE, bar_range_wv, div_wv

    tmp(*,*)=stdv_wv[*,isfc,*]
    png_file = figs_dir + '/mirs_adv_poes_' + satId + nwpStr + '_biasstdv_glb_wv_' + sfc_names[isfc] + size_str + '.png'
    PLOT_BiasMeanStdv, x, tmp, '', 'Water Vapor Standard Deviation',$
  	  'MIRS ' + satName + ' WV Stdv Over ' + sfc_titles[isfc] + ' (V' + version +')',$
	  range_wv_stdv, png_file, NLAY-1, xticks, 0, ISIZE, bar_range_wv, div_wv

  endfor

ENDFOR

END
