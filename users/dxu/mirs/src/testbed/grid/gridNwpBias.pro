@../../../setup/paths_idl.pro
;*******************************************************************************
;
;  Purpose:
;    To Plot gridded data set into png images.
;
;  Dependence:
;    plot_grid.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ------------    --------------    ----------------------
;     09/27/2007      Wanchun Chen       Original Code
;     05/01/2012      Wanchun Chen       Further profiling using CLW and RR
;
;*******************************************************************************

Pro GridNwpBias, nameList=nameList
Consts, Consts

;*******************************************************************************
;    some constants definitions
;*******************************************************************************
gridfactor=4
date='2012-11-01'
satId='npp'
dirGrid='/data/data007/pub/mirs_oper/data/TestbedData/Outputs/grid/npp_atms/2012-11-01/'
dirImg='/data/data007/pub/mirs_oper/src/testbed/grid/img/'
version='3113'
dummy=''
biasPath='/data/data007/pub/mirs_oper/data/SemiStaticData/biasCorrec/'
latmin=-90
latmax=90
lonmin=-180
lonmax=180
nwpData=2

readFromList=1
if readFromList eq 1 then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId           ;Satellite ID
  readf,iu,format='(i)', gridfactor      ;gridfactor
  readf,iu,format='(a)', dirGrid         ;gridded data dir
  readf,iu,format='(a)', dirImg          ;path where to put image files
  readf,iu,format='(a)', date    	 ;file extension of image
  readf,iu,format='(a)', version    	 ;version number
  readf,iu,format='(a)', dummy   	 ;pass dummy variable
  readf,iu,format='(a)', dummy   	 ;pass dummy variable
  readf,iu,format='(a)', biasPath    	 ;biasPath
  readf,iu,format='(f)', latmin          ;min lat
  readf,iu,format='(f)', latmax          ;max lat
  readf,iu,format='(f)', lonmin          ;min lon
  readf,iu,format='(f)', lonmax          ;max lon
  readf,iu,format='(i)', nwpData         ;nwp Ref Data src ( 1-gdas, 2-ecmwf, 3-gfs )
  close,iu
  free_lun,iu,/force
endif

nwpDatas = [' ','gdas','ecmwf','gfs']
print, 'gridNwpBias.pro ... nwpData = ', nwpDatas[nwpData]
print, 'input grid dir='+dirGrid
print, 'output img dir='+dirImg 

rotate = 'mirs_adv_poes_'
if satId eq Consts.STR_SATID_F16    then rotate = 'mirs_adv_dmsp_'
if satId eq Consts.STR_SATID_F17    then rotate = 'mirs_adv_dmsp_'
if satId eq Consts.STR_SATID_F18    then rotate = 'mirs_adv_dmsp_'
if satId eq Consts.STR_SATID_AQUA   then rotate = 'mirs_adv_eos_'
if satId eq Consts.STR_SATID_GCOMW1 then rotate = 'mirs_adv_eos_'
if satId eq Consts.STR_SATID_FY3RI  then rotate = 'mirs_adv_eos_'
if satId eq Consts.STR_SATID_NPP    then rotate = 'mirs_adv_npoess_'
if satId eq Consts.STR_SATID_TRMM   then rotate = 'mirs_adv_eos_'
if satId eq Consts.STR_SATID_GPM    then rotate = 'mirs_adv_eos_'
if satId eq Consts.STR_SATID_MTMA   then rotate = 'mirs_adv_mt_'
if satId eq Consts.STR_SATID_MTSA   then rotate = 'mirs_adv_mt_'

nwpId='_gdas_'
nwpTxt='GDAS'
if ( nwpData eq 2 ) then nwpId='_ecmwf_'
if ( nwpData eq 2 ) then nwpTxt='ECMWF'

if ( nwpData eq 3 ) then nwpId='_gfs_'
if ( nwpData eq 3 ) then nwpTxt='GFS'

prefix=rotate+satId+nwpId+'bias_glb_'
yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

NLAY=100

NCHAN=20
if satId eq Consts.STR_SATID_F16    then NCHAN = 24 
if satId eq Consts.STR_SATID_F17    then NCHAN = 24
if satId eq Consts.STR_SATID_F18    then NCHAN = 24
if satId eq Consts.STR_SATID_AQUA   then NCHAN = 12
if satId eq Consts.STR_SATID_GCOMW1 then NCHAN = 14
if satId eq Consts.STR_SATID_FY3RI  then NCHAN = 10
if satId eq Consts.STR_SATID_NPP    then NCHAN = 22
if satId eq Consts.STR_SATID_TRMM   then NCHAN = 9
if satId eq Consts.STR_SATID_GPM    then NCHAN = 13
if satId eq Consts.STR_SATID_MTMA   then NCHAN = 9
if satId eq Consts.STR_SATID_MTSA   then NCHAN = 6

satTxt = strupcase(satId)
if satId eq Consts.STR_SATID_AQUA   then satTxt = 'AMSR-E'
if satId eq Consts.STR_SATID_GCOMW1 then satTxt = 'GCOMW1/AMSR2'
if satId eq Consts.STR_SATID_FY3RI  then satTxt = 'FY3/MWRI'
if satId eq Consts.STR_SATID_NPP    then satTxt = 'NPP/ATMS'
if satId eq Consts.STR_SATID_TRMM   then satTxt = 'TRMM/TMI'
if satId eq Consts.STR_SATID_GPM    then satTxt = 'GPM/GMI'
if satId eq Consts.STR_SATID_MTMA   then satTxt = 'MT/MADRAS'
if satId eq Consts.STR_SATID_MTSA   then satTxt = 'MT/SAPHIR'

NCOL=360*gridfactor
NROW=180*gridfactor

;---- CLW and RR as filter factor
CLW_CUT = 0.05
RR_CUT = 0.001

;*******************************************************************************
;    define products ID array to be plotted
;*******************************************************************************

prodIDs_chn = [ 'em' ]
prodIDs_lay = [ 'temp','wv' ]

prodIDs_sfc = [ 'tskin','tpw','clw','iwp','lwp','rr','psfc','swe','wspd' ]

;---- GDAS set ----
if nwpData EQ 1 then begin
  prodIDs_sfc = [ 'tskin','tpw','psfc','swe','wspd' ]
  if satId eq Consts.STR_SATID_MTMA then $
     prodIDs_sfc = [ 'tskin', 'tpw', 'wspd' ]
endif

;---- ECMWF set ----
if nwpData EQ 2 then begin
  prodIDs_sfc = [ 'tskin','tpw','clw','iwp','lwp','psfc','wspd' ]
  if satId eq Consts.STR_SATID_MTSA then $
     prodIDs_sfc = [ 'tskin', 'tpw', 'clw','iwp','lwp' ]
endif

;---- GFS set ----
if nwpData EQ 3 then begin
  prodIDs_sfc = [ 'tskin','tpw','clw','rr','psfc','wspd' ]
  if satId eq Consts.STR_SATID_MTSA then $
     prodIDs_sfc = [ 'tskin', 'tpw', 'clw']
endif


;*******************************************************************************
;    read in sfc types from static land/sea marks
;*******************************************************************************
sfcMask=BytArr(nCol,nRow)
dum1=BytArr(nCol,nRow)
dum2=BytArr(nCol,nRow)
IF ( nCol EQ 1440 ) THEN OpenR,Lun,'lndsea25.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 1080 ) THEN OpenR,Lun,'lndsea30.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 720  ) THEN OpenR,Lun,'lndsea50.tag',/Get_Lun, /Swap_If_Big_Endian
ReadU,Lun,dum1
Free_Lun,Lun

;---- reverse index of land/sea mask to be consistent with gridded data set
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


;*******************************************************************************
;    define some tokens
;*******************************************************************************
sfcs = [ 'sea', 'lnd', 'all' ]
nsfc = N_ELEMENTS(sfcs)

cends = [ 'as', 'ds' ]
ncend = N_ELEMENTS(cends)
cendtxts = [ ' Asc ', ' Des ' ]

conds = [ 'rainy', 'cloudy', 'clear', 'allcond' ]
ncond = n_elements(conds)
condTxts = [ 'Rainy ', 'Cloudy ', 'Clear ', 'All Cond. ' ]


;*******************************************************************************
; sfc products section
;*******************************************************************************

nprod_sfc = N_ELEMENTS(prodIDs_sfc)
for iprod = 0, nprod_sfc - 1 do begin
  
  prodId = prodIDs_sfc(iprod)

  case prodId of

  'tskin':      begin
                    minvalue = -4
                    maxvalue = 4
                    div      = 8
                    fmt      = '(I2)'
                    prodTxt  = ' Skin Temp. (K) '
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'psfc':       begin
                    minvalue = -100
                    maxvalue = 100
                    div      = 10
                    fmt      = '(I4)'
                    prodTxt  = ' Sfc Pressure (mb) '
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'tpw':        begin
                    minvalue = -10
                    maxvalue = 10
                    div      = 10
                    fmt      = '(I3)'
                    prodTxt  = ' TPW (mm) '
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'clw':        begin
                    minvalue = -0.1
                    maxvalue = 0.1
                    div      = 5
                    fmt      = '(f6.2)'
                    prodTxt  = ' CLW (mm) '
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'iwp':        begin
                    minvalue = -0.1
                    maxvalue = 0.1
                    div      = 5
                    fmt      = '(f6.2)'
                    prodTxt  = ' IWP (mm) '		    
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'lwp':        begin
                    minvalue = -0.1
                    maxvalue = 0.1
                    div      = 5
                    fmt      = '(f6.2)'
                    prodTxt  = ' LWP (mm) '		
                    nsfc_bot = 0	     
                    nsfc_top = 0	     
		end

  'rr':         begin
                    minvalue = -1.0
                    maxvalue = 1.0
                    div      = 5
                    fmt      = '(f6.2)'
                    prodTxt  = ' Rain Rate (mm/hr) '		     
                    nsfc_bot = 0		 
                    nsfc_top = 0		 
		end

  'swe':        begin
                    minvalue = -4
                    maxvalue = 4
                    div      = 8
                    fmt      = '(I2)'
                    prodTxt  = ' SWE (cm) '
                    nsfc_bot = 1
                    nsfc_top = 1
		end

  'wspd':      begin
                    minvalue  = -10
                    maxvalue  = 10
                    div       = 10
                    fmt       = '(I3)'
                    prodTxt   = ' Wind Speed (m/s) '
                    nsfc_bot  = 0
                    nsfc_top  = 0
                end

  else:         begin
                        print, 'Unsupported sfc prodId: '+ prodId
                end
  endcase


  for icend = 0, ncend - 1 do begin
    cend = cends(icend)
    cendTxt = cendtxts(icend)
    
    fileGrid = dirGrid + 'GRID_'+satId+nwpId+'bias_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
      continue 
    endif
    
    fileClw = dirGrid + 'GRID_'+satId+'_'+yyyymmdd+'_clw_'+cend+'.dat'
    result = FILE_TEST(fileClw)
    if result eq 0 then begin
      print, 'CLW File not exist: ' + fileClw
      continue 
    endif
    
    fileRr = dirGrid + 'GRID_'+satId+'_'+yyyymmdd+'_rr_'+cend+'.dat'
    result = FILE_TEST(fileRr)
    if result eq 0 then begin
      print, 'RR File not exist: ' + fileRr
      continue 
    endif
        
    products = fltarr(NCOL,NROW)
    openr, lun, fileGrid, /get_lun, /Swap_Endian
    readu, lun, products
    free_lun,lun,/force 
    
    clws = fltarr(NCOL,NROW)
    openr, lun, fileClw, /get_lun, /Swap_Endian
    readu, lun, clws
    free_lun,lun,/force 
    
    rrs = fltarr(NCOL,NROW)
    openr, lun, fileRr, /get_lun, /Swap_Endian
    readu, lun, rrs
    free_lun,lun,/force 

    ;---- define a conditions array to hold conditions
    conditions = intarr(NCOL,NROW)
    conditions(*,*) = -999

    for irow = 0, NROW - 1 do begin
    for icol = 0, NCOL - 1 do begin
      if rrs(icol,irow) ge RR_CUT then conditions(icol,irow) = 0 ; 0 denotes rainy
      if rrs(icol,irow) lt RR_CUT and clws(icol,irow) ge CLW_CUT then conditions(icol,irow) = 1 ; 1 denotes cloudy
      if rrs(icol,irow) lt RR_CUT and clws(icol,irow) lt CLW_CUT then conditions(icol,irow) = 2 ; 2 denotes clear
    endfor
    endfor
    
    tmp = products
    
    for icond = 0, ncond - 1 do begin ; loop icond start

      cond = conds(icond)
      condTxt = condTxts(icond)
      
      products = tmp
      ;---- filter out 
      if ( icond le 2 ) and ( prodId eq 'tpw' or prodId eq 'tskin' or prodId eq 'wspd' ) then begin
        ss = where( conditions ne icond, cnt )
        if cnt ge 1 then products(ss) = -999.0
      endif

      for isfc = nsfc_bot, nsfc_top do begin ; loop isfc start

        sfcPick = isfc
        sfc = sfcs(isfc)
        map_name = dirImg+prefix+yyyymmdd+'_'+prodId+'_'+cond+'_'+sfc+'_'+cend+'.png'
        title = condTxt+'MIRS '+satTxt+'-'+nwpTxt+prodTxt+date+cendTxt+'(r'+version +')'
        plot_grid, products,sfcMask,map_name,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,title,sfcPick,div,fmt
      
      endfor ; loop isfc end 
    
    endfor ; loop icond end
    
  endfor
 
endfor


;*******************************************************************************
; layer products section
;*******************************************************************************
layers=[100,200,300,400,500,600,700,800,850,900,950]
layers_index = [43, 54, 62, 69, 75, 80, 84, 88, 90, 92, 94]
titles=['100mb','200mb','300mb','400mb','500mb','600mb','700mb','800mb','850mb','900mb','950mb']
nlay_plot = N_ELEMENTS(layers)

nprod_lay = N_ELEMENTS(prodIDs_lay)
for iprod = 0, nprod_lay - 1 do begin ; iprod loop start
  
  prodId = prodIDs_lay(iprod)
  
  case prodId of
  
  'temp':      begin
                    minvalues = replicate(-10,nlay_plot)
                    maxvalues = replicate( 10,nlay_plot)
                    divs      = replicate( 10,nlay_plot)
                    fmt       = '(I3)'
                    prodTxt   = ' Temp. (K) '
                    nsfc_bot  = 0
                    nsfc_top  = 2
		end

  'wv':        begin
                    minvalues = [ -0.05, -0.05, -0.05, -0.05, -1, -1, -2, -2, -2, -2, -2]
                    maxvalues = [  0.05,  0.05,  0.05,  0.05,  1,  1,  2,  2,  2,  2,  2]
                    divs      = replicate( 10,nlay_plot)
                    format    = '(f5.2)'
                    prodTxt   = ' WV (g/kg) '
                    nsfc_bot  = 0
                    nsfc_top  = 2
		end

  else:         begin
                        print, 'Unsupported layer prodId: '+ prodId
                end
  endcase

  for icend = 0, ncend - 1 do begin ; icend loop start
    cend = cends(icend)
    cendTxt = cendtxts(icend)
    
    fileGrid = dirGrid + 'GRID_'+satId+nwpId+'bias_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
      continue 
    endif
    
    fileClw = dirGrid + 'GRID_'+satId+'_'+yyyymmdd+'_clw_'+cend+'.dat'
    result = FILE_TEST(fileClw)
    if result eq 0 then begin
      print, 'CLW File not exist: ' + fileClw
      continue 
    endif
    
    fileRr = dirGrid + 'GRID_'+satId+'_'+yyyymmdd+'_rr_'+cend+'.dat'
    result = FILE_TEST(fileRr)
    if result eq 0 then begin
      print, 'RR File not exist: ' + fileRr
      continue 
    endif
        
    products = fltarr(NCOL,NROW,NLAY)
    tmp=fltarr(NCOL,NROW)
    openr, lun, fileGrid, /get_lun, /Swap_Endian
    for ilay=0, NLAY-1 do begin
      readu, lun, tmp
      products(*,*,ilay) = tmp(*,*)
    endfor
    free_lun,lun,/force 
    
    
    clws = fltarr(NCOL,NROW)
    openr, lun, fileClw, /get_lun, /Swap_Endian
    readu, lun, clws
    free_lun,lun,/force 
    
    rrs = fltarr(NCOL,NROW)
    openr, lun, fileRr, /get_lun, /Swap_Endian
    readu, lun, rrs
    free_lun,lun,/force 
    
    ;---- define a conditions array to hold conditions
    conditions = intarr(NCOL,NROW)
    conditions(*,*) = -999
    
    for irow = 0, NROW - 1 do begin
    for icol = 0, NCOL - 1 do begin
      if rrs(icol,irow) ge RR_CUT then conditions(icol,irow) = 0 ; 0 denotes rainy
      if rrs(icol,irow) lt RR_CUT and clws(icol,irow) ge CLW_CUT then conditions(icol,irow) = 1 ; 1 denotes cloudy
      if rrs(icol,irow) lt RR_CUT and clws(icol,irow) lt CLW_CUT then conditions(icol,irow) = 2 ; 2 denotes clear
    endfor
    endfor
    
    for ilay = 0, nlay_plot - 1 do begin ; ilay loop start
      
      layer = '@ ' + titles[ilay]
      sb = fltarr(NCOL,NROW)
      sb = products(*,*,layers_index(ilay))
       
      tmp = sb
       
      for icond = 0, ncond - 1 do begin ; icond loop start
    
        cond = conds(icond)
        condTxt = condTxts(icond)
        
        sb = tmp 
          
        ;---- filter out 
        if icond le 2 then begin
          ss = where( conditions ne icond, cnt )
          if cnt ge 1 then sb(ss) = -999.0
        endif

        for isfc = nsfc_bot, nsfc_top do begin ; isfc loop start

          sfcPick = isfc
	  sfc = sfcs(isfc)
	  map_name = dirImg + prefix + yyyymmdd + '_' + prodId + '_' + titles[ilay]+'_'+cond+'_'+sfc + '_' + cend + '.png'
	  title = condTxt + 'MIRS ' + satTxt + '-' + nwpTxt + prodTxt + layer + ' ' + date + cendTxt + '(r' + version +')'
	  plot_grid, sb,sfcMask,map_name,minvalues[ilay],maxvalues[ilay], latmin,latmax,lonmin,lonmax,title,sfcPick,divs[ilay],fmt

        endfor ; isfc loop end

      endfor ; icond loop end

    endfor ; ilay loop end

  endfor ; icend loop end

endfor ; iprod loop end



;*******************************************************************************
; channle products section
;*******************************************************************************
;---- default to AMSU ( N19/N19/MetopA/MetopB )
titles=['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2',$
        '57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']
mins_sea = replicate(-0.1,20)
maxs_sea = replicate(0.1,20)
mins_lnd = replicate(-0.1,20)
maxs_lnd = replicate(0.1,20)
mins_all = replicate(-0.1,20)
maxs_all = replicate(0.1,20)
divs     = replicate(10,20)

if ( satId eq Consts.STR_SATID_F16 ) then begin
  titles   = ['50v','52v','53v','54v','55v','57rc','59rc','150h',$
              '190h','186h','184h','19h','19v','22v','37h','37v',$
              '91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']	 
  mins_sea = replicate(-0.1,24)
  maxs_sea = replicate(0.1,24)
  mins_lnd = replicate(-0.1,24)
  maxs_lnd = replicate(0.1,24)
  mins_all = replicate(-0.1,24)
  maxs_all = replicate(0.1,24)
  divs     = replicate(10,24)
endif

if ( satId eq Consts.STR_SATID_F17 ) then begin
  titles   = ['50h','52h','53h','54h','55h','57rc','59rc','150h',$
              '190h','186h','184h','19h','19v','22v','37h','37v',$
              '91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']     
  mins_sea = replicate(-0.1,24)
  maxs_sea = replicate(0.1,24)
  mins_lnd = replicate(-0.1,24)
  maxs_lnd = replicate(0.1,24)
  mins_all = replicate(-0.1,24)
  maxs_all = replicate(0.1,24)
  divs     = replicate(10,24)
endif

if ( satId eq Consts.STR_SATID_F18 ) then begin
  titles   = ['50h','52h','53h','54h','55h','57rc','59rc','150h',$
              '190h','186h','184h','19h','19v','22v','37h','37v',$
              '91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']     
  mins_sea = replicate(-0.1,24)
  maxs_sea = replicate(0.1,24)
  mins_lnd = replicate(-0.1,24)
  maxs_lnd = replicate(0.1,24)
  mins_all = replicate(-0.1,24)
  maxs_all = replicate(0.1,24)
  divs     = replicate(10,24)
endif

if ( satId eq Consts.STR_SATID_AQUA ) then begin
  titles   = ['7v','7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h']
  mins_sea = replicate(-0.1,12)
  maxs_sea = replicate(0.1,12)
  mins_lnd = replicate(-0.1,12)
  maxs_lnd = replicate(0.1,12)
  mins_all = replicate(-0.1,12)
  maxs_all = replicate(0.1,12)
  divs     = replicate(10,12)
endif

if ( satId eq Consts.STR_SATID_GCOMW1 ) then begin
  titles   = ['6v','6h','7v','7h','10v','10h','18v','18h','23v','23h','36v','36h','89v','89h']
  mins_sea = replicate(-0.1,NCHAN)
  maxs_sea = replicate(0.1,NCHAN)
  mins_lnd = replicate(-0.1,NCHAN)
  maxs_lnd = replicate(0.1,NCHAN)
  mins_all = replicate(-0.1,NCHAN)
  maxs_all = replicate(0.1,NCHAN)
  divs     = replicate(10,NCHAN)
endif

if ( satId eq Consts.STR_SATID_TRMM ) then begin
  titles   = ['11v','11h','19v','19h','21v','37v','37h','85v','85h']
  mins_sea = replicate(-0.1,9)
  maxs_sea = replicate(0.1,9)
  mins_lnd = replicate(-0.1,9)
  maxs_lnd = replicate(0.1,9)
  mins_all = replicate(-0.1,9)
  maxs_all = replicate(0.1,9)
  divs     = replicate(10,9)
endif

if ( satId eq Consts.STR_SATID_GPM ) then  begin
  titles   = [ '11v','11h','19v','19h','24v','37v','37h','89v','89h','166v','166h','183v1','183v2']
  mins_sea = [0.55,0.25,0.58,0.25,0.60,0.65,0.25,0.75,0.45,0.45,0.45,0.45,0.45]
  maxs_sea = [0.63,0.35,0.68,0.45,0.70,0.75,0.50,0.90,0.75,0.75,0.75,0.75,0.75]
  mins_lnd = [0.70,0.65,0.70,0.65,0.80,0.90,0.70,0.75,0.75,0.75,0.75,0.75,0.75]
  maxs_lnd = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
  mins_all = [0.40,0.60,0.40,0.60,0.40,0.40,0.60,0.40,0.60,0.40,0.60,0.65,0.65]
  maxs_all = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
  divs     = replicate(10,13)
endif

if ( satId eq Consts.STR_SATID_FY3RI ) then begin
  titles   = ['11v','11h','19v','19h','24v','24h','37v','37h','89v','89h']
  mins_sea = replicate(-0.1,10)
  maxs_sea = replicate(0.1,10)
  mins_lnd = replicate(-0.1,10)
  maxs_lnd = replicate(0.1,10)
  mins_all = replicate(-0.1,10)
  maxs_all = replicate(0.1,10)
  divs     = replicate(10,10)
endif

if ( satId eq Consts.STR_SATID_NPP ) then begin
  titles   = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3',$
              '57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
  mins_sea = replicate(-0.1,22)
  maxs_sea = replicate(0.1,22)
  mins_lnd = replicate(-0.1,22)
  maxs_lnd = replicate(0.1,22)
  mins_all = replicate(-0.1,22)
  maxs_all = replicate(0.1,22)
  divs     = replicate(10,22)
endif

if ( satId eq Consts.STR_SATID_MTMA ) then begin
  titles   = ['19v','19h','24v','37v','37h','89v','89h','157v','157h']
  mins_sea = replicate(-0.1,9)
  maxs_sea = replicate(0.1,9)
  mins_lnd = replicate(-0.1,9)
  maxs_lnd = replicate(0.1,9)
  mins_all = replicate(-0.1,9)
  maxs_all = replicate(0.1,9)
  divs     = replicate(10,9)
endif

if ( satId eq Consts.STR_SATID_MTSA ) then begin
  titles   = ['183h','184h','186h','187h','190h','194h']
  mins_sea = replicate(-0.1,6)
  maxs_sea = replicate(0.1,6)
  mins_lnd = replicate(-0.1,6)
  maxs_lnd = replicate(0.1,6)
  mins_all = replicate(-0.1,6)
  maxs_all = replicate(0.1,6)
  divs     = replicate(10,6)
endif



nprod_chn = N_ELEMENTS(prodIDs_chn)
for iprod = 0, nprod_chn - 1 do begin ; iprod loop start
  
  prodId = prodIDs_chn(iprod)
  
  case prodId of
  
  'em':         begin
                    fmt      = '(f5.2)'
                    prodTxt  = ' Emissivity '
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  else:         begin
                        print, 'Unsupported channel prodId: '+ prodId
                end
  endcase

  for icend = 0, ncend - 1 do begin ; icend loop start

    cend = cends(icend)
    cendTxt = cendtxts(icend)
    
    fileGrid = dirGrid+'GRID_'+satId+nwpId+'bias_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
      continue 
    endif
    
    fileClw = dirGrid + 'GRID_'+satId+'_'+yyyymmdd+'_clw_'+cend+'.dat'
    result = FILE_TEST(fileClw)
    if result eq 0 then begin
      print, 'CLW File not exist: ' + fileClw
      continue 
    endif
    
    fileRr = dirGrid + 'GRID_'+satId+'_'+yyyymmdd+'_rr_'+cend+'.dat'
    result = FILE_TEST(fileRr)
    if result eq 0 then begin
      print, 'RR File not exist: ' + fileRr
      continue 
    endif
        
    products = fltarr(NCOL,NROW,NCHAN)
    tmp=fltarr(NCOL,NROW)
    openr, lun, fileGrid, /get_lun, /Swap_Endian
    for ichan=0, NCHAN-1 do begin
      readu, lun, tmp
      products(*,*,ichan) = tmp(*,*)
    endfor
    free_lun,lun,/force 
    
    clws = fltarr(NCOL,NROW)
    openr, lun, fileClw, /get_lun, /Swap_Endian
    readu, lun, clws
    free_lun,lun,/force 
    
    rrs = fltarr(NCOL,NROW)
    openr, lun, fileRr, /get_lun, /Swap_Endian
    readu, lun, rrs
    free_lun,lun,/force 
    
    ;---- define a conditions array to hold conditions
    conditions = intarr(NCOL,NROW)
    conditions(*,*) = -999
    
    for irow = 0, NROW - 1 do begin
    for icol = 0, NCOL - 1 do begin
      if rrs(icol,irow) ge RR_CUT then conditions(icol,irow) = 0 ; 0 denotes rainy
      if rrs(icol,irow) lt RR_CUT and clws(icol,irow) ge CLW_CUT then conditions(icol,irow) = 1 ; 1 denotes cloudy
      if rrs(icol,irow) lt RR_CUT and clws(icol,irow) lt CLW_CUT then conditions(icol,irow) = 2 ; 2 denotes clear
    endfor
    endfor
    
    for ichan = 0, NCHAN - 1 do begin ; ichan loop start

      channel = '@ ' + titles[ichan]
      sb = fltarr(NCOL,NROW)
      sb = products(*,*,ichan)
      
      tmp = sb
      
      for icond = 0, ncond - 1 do begin ; icond loop start

        cond = conds(icond)
        condTxt = condTxts(icond)
        
        sb = tmp
        
        ;---- filter out 
        if icond le 2 then begin
          ss = where( conditions ne icond, cnt )
          if cnt ge 1 then sb(ss) = -999.0
        endif

        for isfc = nsfc_bot, nsfc_top do begin ; isfc loop start
  
          sfcPick = isfc
	  sfc = sfcs(isfc)

	  if isfc eq 0 then begin
	    minvals = mins_sea
	    maxvals = maxs_sea
	  endif
	  if isfc eq 1 then begin
	    minvals = mins_lnd
	    maxvals = maxs_lnd
	  endif
	  if isfc eq 2 then begin
	    minvals = mins_all
	    maxvals = maxs_all
	  endif

	  map_name = dirImg+prefix+yyyymmdd+'_'+prodId+'_'+titles[ichan]+'_'+ cond +'_'+sfc+'_'+cend+'.png'
	  title = condTxt+'MIRS '+satTxt+'-'+nwpTxt+prodTxt+channel+' '+date+cendTxt+'(r'+version +')'
	  plot_grid, sb,sfcMask,map_name,minvals[ichan],maxvals[ichan],latmin,latmax,lonmin,lonmax,title,sfcPick,divs[ichan],fmt

        endfor ; isfc loop end

      endfor ; icond loop end

    endfor ; ichan loop end

  endfor ; icend loop end

endfor ; iprod loop end


End
