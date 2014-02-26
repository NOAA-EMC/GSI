;===============================================================================================
; Name:		CheckGeophysical
;
;
; Type:		IDL Subroutine
;
;
; Description:  Check geophysical bias abnormal
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- satId   	      I             Satellite ID
;	- biasPath	      I		    dir to get bias data
;	- nwpStr   	      I             ref data( gdas, ecmw, gfs ) 
;	- yyyymmddd  	      I             yyyymmdd
;	- geophysicalTxt      O             output file
;
;
; Subroutines needed:
;       - none
;
;
; History:
;       01-15-2010      Wanchun Chen	Original code
;
;===============================================================================================

Pro checkGeophysical, satId, biasPath, nwpStr, yyyymmdd, geophysicalTxt
  
  ;biasPath = '/net/orbit227l/disk1/pub/mirs_operational/data/SemiStaticData/biasCorrec/'
  ;satId = 'n18'
  ;nwpId = '_nwp_' + 'gdas' + '_'
  ;yyyymmdd = '20100112'
  
  nwpId = '_nwp_' + nwpStr + '_'
  
  layers=[100,200,300,400,500,600,700,800,850,900,950]
  n_layer=N_Elements(layers)
  
  layers_str1=['100mb','200mb','300mb','400mb','500mb','600mb','700mb','800mb','850mb','900mb','950mb']
  layers_str2=['200mb','300mb','400mb','500mb','600mb','700mb','800mb','850mb','900mb','950mb']
  
  bias_temp_as = fltarr(4,n_layer)
  stdv_temp_as = fltarr(4,n_layer)
  
  bias_temp_ds = fltarr(4,n_layer)
  stdv_temp_ds = fltarr(4,n_layer)

  bias_wv_as = fltarr(4,n_layer-1)
  stdv_wv_as = fltarr(4,n_layer-1)
  
  bias_wv_ds = fltarr(4,n_layer-1)
  stdv_wv_ds = fltarr(4,n_layer-1)

  ;biasMeanStdv_n19_nwp_gdas_20090828.dat
  file = biasPath + 'biasMeanStdv_'+satId+nwpId+yyyymmdd+'.dat'
  
  ;print, file
  
  result = FINDFILE(file)
  
  if result eq '' then begin
    openw,  lunw, geophysicalTxt, /get_lun
    printf, lunw, 'Statistics file not exist:'
    printf, lunw, file
    printf, lunw, '' 
  endif
  
  if result ne '' then begin
  
    openr, lun, file, /get_lun
    readu, lun, bias_temp_as, stdv_temp_as
    readu, lun, bias_temp_ds, stdv_temp_ds
    readu, lun, bias_wv_as, stdv_wv_as
    readu, lun, bias_wv_ds, stdv_wv_ds
    free_lun, lun
  
    openw, lunw, geophysicalTxt, /get_lun
    
    ;---- temperature stdv > 10
    ss = where( stdv_temp_as(0,*) ge 10, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Ascending temperature stdv over sea > 10 K'
      for ilay = 0, n_layer - 1 do begin 
        printf, lunw, layers_str1(ilay), ': ', stdv_temp_as(0,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_temp_as(2,*) ge 10, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Ascending temperature stdv over land > 10 K'
      for ilay = 0, n_layer - 1 do begin 
        printf, lunw, layers_str1(ilay), ': ', stdv_temp_as(2,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_temp_ds(0,*) ge 10, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Descending temperature stdv over sea > 10 K'
      for ilay = 0, n_layer - 1 do begin 
        printf, lunw, layers_str1(ilay), ': ', stdv_temp_ds(0,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_temp_ds(2,*) ge 10, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Descending temperature stdv over land > 10 K'
      for ilay = 0, n_layer - 1 do begin 
        printf, lunw, layers_str1(ilay), ': ', stdv_temp_ds(2,ilay)
      endfor
      printf, lunw, ''
    endif
    
    
    ;---- temperature stdv < 0
    ss = where( stdv_temp_as(0,*) lt 0, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Ascending temperature stdv over sea < 0'
      for ilay = 0, n_layer - 1 do begin 
        printf, lunw, layers_str1(ilay), ': ', stdv_temp_as(0,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_temp_as(2,*) lt 0, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Ascending temperature stdv over land < 0'
      for ilay = 0, n_layer - 1 do begin 
        printf, lunw, layers_str1(ilay), ': ', stdv_temp_as(2,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_temp_ds(0,*) lt 0, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Descending temperature stdv over sea < 0'
      for ilay = 0, n_layer - 1 do begin 
        printf, lunw, layers_str1(ilay), ': ', stdv_temp_ds(0,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_temp_ds(2,*) lt 0, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Descending temperature stdv over land < 0'
      for ilay = 0, n_layer - 1 do begin 
        printf, lunw, layers_str1(ilay), ': ', stdv_temp_ds(2,ilay)
      endfor
      printf, lunw, ''
    endif
    
    
    ;---- water vapor > 70
    ss = where( stdv_wv_as(0,*) ge 70, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Ascending water vapor stdv over sea > 70%'
      for ilay = 0, n_layer - 2 do begin 
        printf, lunw, layers_str2(ilay), ': ', stdv_wv_as(0,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_wv_as(2,*) ge 70, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Ascending water vapor stdv over land > 70%'
      for ilay = 0, n_layer - 2 do begin 
        printf, lunw, layers_str2(ilay), ': ', stdv_wv_as(2,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_wv_ds(0,*) ge 70, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Descending water vapor stdv over sea > 70%'
      for ilay = 0, n_layer - 2 do begin 
        printf, lunw, layers_str2(ilay), ': ', stdv_wv_ds(0,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_wv_ds(2,*) ge 70, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Descending water vapor stdv over land > 70%'
      for ilay = 0, n_layer - 2 do begin 
        printf, lunw, layers_str2(ilay), ': ', stdv_wv_ds(2,ilay)
      endfor
      printf, lunw, ''
    endif


    ;---- water vapor < 0
    ss = where( stdv_wv_as(0,*) lt 0, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Ascending water vapor stdv over sea < 0'
      for ilay = 0, n_layer - 2 do begin 
        printf, lunw, layers_str2(ilay), ': ', stdv_wv_as(0,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_wv_as(2,*) lt 0, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Ascending water vapor stdv over land < 0'
      for ilay = 0, n_layer - 2 do begin 
        printf, lunw, layers_str2(ilay), ': ', stdv_wv_as(2,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_wv_ds(0,*) lt 0, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Descending water vapor stdv over sea < 0'
      for ilay = 0, n_layer - 2 do begin 
        printf, lunw, layers_str2(ilay), ': ', stdv_wv_ds(0,ilay)
      endfor
      printf, lunw, ''
    endif
    
    ss = where( stdv_wv_ds(2,*) lt 0, cnt)
    if cnt ge 1 then begin
      printf, lunw, 'Descending water vapor stdv over land < 0'
      for ilay = 0, n_layer - 2 do begin 
        printf, lunw, layers_str2(ilay), ': ', stdv_wv_ds(2,ilay)
      endfor
      printf, lunw, ''
    endif

    

    free_lun, lunw
  
  endif


End
