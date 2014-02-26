@../../../setup/paths_idl.pro
;*******************************************************************************
;
;  Purpose:
;    To Plot point to point comparison of mirs vs nwp.
;  
;  Dependence:
;    choppvec.pro
;    utilities.pro
;
;  Record of revisions:
;        Date          Programmer       Description of change
;    ------------    --------------    -----------------------
;     09/01/2007       Wanchun Chen     Original Code
;     05/01/2012       Wanchun Chen     Modified to array version except TB
;
;*******************************************************************************

Pro P2P_MIRS_NWP, namelist=namelist
Consts, Consts

;***************************************************************
;    identifiers
;***************************************************************
satId='metopB'
gridfactor=4
date='2012-11-01'
dirGrid='/data/data007/pub/mirs_oper/data/TestbedData/Outputs/grid/metopB_amsua_mhs/2012-11-01/'
dirImg='/data/data007/pub/mirs_oper/src/testbed/grid/img/'
processMode=1
version='3006'
edrList=''
depList=''
biasPath='/data/data007/pub/mirs_oper/data/SemiStaticData/biasCorrec/'
latmin=-90
latmax=90
lonmin=-180
lonmax=180
nwpData=1
nedtExt='2012-11-01'
fmType=1

fromNameList=1
if ( fromNameList eq 1 ) then begin
  openr,iu,namelist,/get_lun
  readf,iu,format='(a)', satId          ;Satellite ID
  readf,iu,format='(i)', gridfactor     ;gridfactor
  readf,iu,format='(a)', dirGrid        ;gridded data dir
  readf,iu,format='(a)', dirImg         ;path where to put image files
  readf,iu,format='(a)', date           ;file extension of image
  readf,iu,format='(a)', version        ;version number
  readf,iu,format='(a)', edrList        ;edrList
  readf,iu,format='(a)', depList        ;depList
  readf,iu,format='(a)', biasPath       ;bias path to put bias files
  readf,iu,format='(f)', latmin         ;min lat
  readf,iu,format='(f)', latmax         ;max lat
  readf,iu,format='(f)', lonmin         ;min lon
  readf,iu,format='(f)', lonmax         ;max lon
  readf,iu,format='(i)', nwpData        ;nwp Data src ( 1-gdas, 2-ecmwf, 3-gfs )
  readf,iu,format='(a)', nedtExt        ;nedt extention ( mainly for NPP )
  readf,iu,format='(i)', fmType         ;resolution type
  close,iu
  free_lun,iu,/force
endif

nwpDatas = [' ','gdas','ecmwf','gfs']
print, 'p2p_mirs_nwp.pro ... ref nwpData = ', nwpDatas[nwpData]
print, 'in p2p dir='+dirGrid
print, 'ou img dir='+dirImg

maxCldAmount=0.05
maxRainAmount=0.001
latMax=90

if ( processMode eq 0 ) then yyyymmdd=date
if ( processMode eq 1 ) then yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

rotate = 'mirs_adv_poes_'
if satId eq Consts.STR_SATID_F16    then rotate = 'mirs_adv_dmsp_'
if satId eq Consts.STR_SATID_F17    then rotate = 'mirs_adv_dmsp_'
if satId eq Consts.STR_SATID_F18    then rotate = 'mirs_adv_dmsp_'
if satId eq Consts.STR_SATID_AQUA   then rotate = 'mirs_adv_eos_'
if satId eq Consts.STR_SATID_GCOMW1 then rotate = 'mirs_adv_eos_'
if satId eq Consts.STR_SATID_NPP    then rotate = 'mirs_adv_npoess_'
if satId eq Consts.STR_SATID_TRMM   then rotate = 'mirs_adv_eos_'
if satId eq Consts.STR_SATID_GPM    then rotate = 'mirs_adv_eos_'
if satId eq Consts.STR_SATID_MTMA   then rotate=  'mirs_adv_mt_'
if satId eq Consts.STR_SATID_MTSA   then rotate=  'mirs_adv_mt_'

nwpId='_gdas_'
nwpTxt='GDAS'

if ( nwpData eq 2 ) then nwpId='_ecmwf_'
if ( nwpData eq 2 ) then nwpTxt='ECMWF'

if ( nwpData eq 3 ) then nwpId='_gfs_'
if ( nwpData eq 3 ) then nwpTxt='GFS'

suffix=strcompress(strmid(date,0,4)+'_'+strmid(date,5,2)+'_'+strmid(date,8,2)+'.dat_gdas')
if ( nwpData eq 2 ) then suffix = strcompress(strmid(date,0,4)+'_'+strmid(date,5,2)+'_'+strmid(date,8,2)+'.dat_ecmw')
if ( nwpData eq 3 ) then suffix = strcompress(strmid(date,0,4)+'_'+strmid(date,5,2)+'_'+strmid(date,8,2)+'.dat_gfs')

if ( satId eq Consts.STR_SATID_NPP ) then begin
  if ( nwpData eq 1 ) then suffix = nedtExt + '.dat_gdas'
  if ( nwpData eq 2 ) then suffix = nedtExt + '.dat_ecmw'
  if ( nwpData eq 3 ) then suffix = nedtExt + '.dat_gfs'
endif

NLAY_ALL=100L
MAX_PROFILE=0L
MAX_FILE=0LL

NCHAN=20L
if satId eq Consts.STR_SATID_F16    then NCHAN = 24L
if satId eq Consts.STR_SATID_F17    then NCHAN = 24L
if satId eq Consts.STR_SATID_F18    then NCHAN = 24L
if satId eq Consts.STR_SATID_NPP    then NCHAN = 22L
if satId eq Consts.STR_SATID_AQUA   then NCHAN = 12L
if satId eq Consts.STR_SATID_GCOMW1 then NCHAN = 14L
if satId eq Consts.STR_SATID_TRMM   then NCHAN = 9L
if satId eq Consts.STR_SATID_GPM    then NCHAN = 13L
if satId eq Consts.STR_SATID_MTMA   then NCHAN = 9L
if satId eq Consts.STR_SATID_MTSA   then NCHAN = 6L

satTxt=strupcase(satId)
if satId eq Consts.STR_SATID_AQUA   then satTxt = 'AMSR-E'
if satId eq Consts.STR_SATID_GCOMW1 then satTxt = 'GCOMW1/AMSR2'
if satId eq Consts.STR_SATID_NPP    then satTxt = 'NPP/ATMS'
if satId eq Consts.STR_SATID_TRMM   then satTxt = 'TRMM/TMI'
if satId eq Consts.STR_SATID_GPM    then satTxt = 'GPM/GMI'

prefix=rotate+satId+nwpId+'glb_'


;***************************************************************
; get MAX_FILE and MAX_PROFILE from meta data file
;***************************************************************
openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_meta.txt',/get_lun
readf, lun, format='(i)',MAX_FILE
readf, lun, format='(i)',MAX_PROFILE
free_lun,lun,/force

print, 'MAX_FILE    = ',MAX_FILE 
print, 'MAX_PROFILE = ',MAX_PROFILE 


;***************************************************************
; define products to be plot
;***************************************************************
prodIds_1D = [ 'tskin', 'tpw', 'wspd', 'psfc' ]
if nwpData eq 2 then begin
    prodIds_1D = [ 'tskin', 'tpw', 'clw', 'iwp', 'lwp', 'wspd', 'psfc' ]
    if satId eq Consts.STR_SATID_TRMM or satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F18 or $
       satId eq Consts.STR_SATID_F17 then prodIds_1D = [ 'tskin', 'tpw', 'clw', 'iwp', 'lwp', 'wspd', 'psfc' ]
    if satId eq Consts.STR_SATID_MTMA then prodIds_1D = [ 'tskin', 'tpw', 'clw', 'iwp', 'lwp', 'wspd' ]
    if satId eq Consts.STR_SATID_MTSA then prodIds_1D = [ 'tskin', 'tpw', 'clw', 'iwp', 'lwp' ]
endif

if nwpData eq 3 then begin
    prodIds_1D = [ 'tskin', 'tpw', 'rr', 'wspd', 'psfc' ]
    if satId eq Consts.STR_SATID_TRMM or satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F18 or $
       satId eq Consts.STR_SATID_F17 then prodIds_1D = [ 'tskin', 'tpw', 'rr', 'wspd', 'psfc' ]
    if satId eq Consts.STR_SATID_MTMA then prodIds_1D = [ 'tskin', 'tpw', 'rr', 'wspd' ]
    if satId eq Consts.STR_SATID_MTMA then prodIds_1D = [ 'tskin', 'tpw', 'rr' ]
endif

prodIds_chan = [ 'em' ]
prodIds_lay  = [ 'temp', 'wv' ]

PLOT_TB = 1


;***************************************************************
; cends, sfcs, conds
;***************************************************************
cends = ['as','ds', 'ad']
ncend = n_elements(cends)
cendTxts = [ 'Asc', 'Des', 'Combined' ]

conds = [ 'rainy', 'cloudy', 'clear', 'allcond' ]
ncond = n_elements(conds)
condTxts = ['Rainy ', 'Cloudy ', 'Clear ', 'All Cond. ' ]

sfcIds  = [ 'sea', 'ice', 'lnd' ,'snw', 'all' ]
sfcInts = [   0,     1,     2,     3,     4   ]
nsfcId  = n_elements(sfcIds)
sfcTxts = [ ' Sea ', ' Ice ', ' Land ', ' Snow ', ' All Surf. ' ] 
nsfc = nsfcId


;***************************************************************
; 1D products section
;***************************************************************
nprod_1D = N_ELEMENTS(prodIds_1D)

for icend = 0, ncend-2 do begin ; loop icend starts
  
  cend = cends(icend)
  cendTxt = cendTxts(icend)
  
  ;---- auxiliarly data file
  file_sfc = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_sfc2_'+cend+'.dat'
  file_lat = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_lat_'+cend+'.dat'
  file_clw = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_clw_'+cend+'.dat'
  file_rr  = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_rr_'+cend+'.dat'
  if satId eq Consts.STR_SATID_MTSA   then file_clw = dirGrid + 'P2P_'+satId+nwpId+yyyymmdd+'_clw_'+cend+'.dat'
  
  result = FILE_TEST(file_sfc)
  if result eq 0 then begin
      print, 'Sfc File not exist: ' + file_sfc
      continue 
  endif
  
  result = FILE_TEST(file_lat)
  if result eq 0 then begin
      print, 'Lat File not exist: ' + file_lat
      continue 
  endif
  
  result = FILE_TEST(file_clw)
  if result eq 0 then begin
      print, 'Clw File not exist: ' + file_clw
      continue 
  endif
  
  result = FILE_TEST(file_rr)
  if result eq 0 then begin
      print, 'RR File not exist: ' + file_rr
      continue 
  endif
  
  ;---- read in auxiliarly data
  prod_sfc = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_sfc, /get_lun, /Swap_Endian
  readu, lun, prod_sfc
  free_lun,lun,/force
  
  prod_lat = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_lat, /get_lun, /Swap_Endian
  readu, lun, prod_lat
  free_lun,lun,/force
  
  prod_clw = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_clw, /get_lun, /Swap_Endian
  readu, lun, prod_clw
  free_lun,lun,/force
  
  prod_rr = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_rr, /get_lun, /Swap_Endian
  readu, lun, prod_rr
  free_lun,lun,/force

  ;---- define a cond array to hold weather condition to save future comparison
  prod_cond = intarr(MAX_PROFILE,MAX_FILE)
  prod_cond(*,*) = -999
  for ifile = 0L, MAX_FILE-1L do begin
  for iprof = 0L, MAX_PROFILE-1L do begin
    if prod_rr(iprof,ifile) ge maxRainAmount then prod_cond(iprof,ifile) = 0
    if prod_rr(iprof,ifile) lt maxRainAmount and prod_clw(iprof,ifile) ge maxCldAmount then prod_cond(iprof,ifile) = 1
    if prod_rr(iprof,ifile) lt maxRainAmount and prod_clw(iprof,ifile) lt maxCldAmount then prod_cond(iprof,ifile) = 2
  endfor
  endfor

  for iprod = 0, nprod_1D - 1 do begin ; loop iprod starts

    prodId = prodIds_1D[iprod]
    print, 'p2p_mirs_nwp.pro  plotting ' + prodId + ' ... ' + cend

    case  prodId of
  
    'tskin':    begin
                        prodTitle   = ' Skin Temp. (K) '
                        minval      = 200
                        maxval      = 340
                        gridScales  = [ 1.0, 1.0, 1.0, 1.0, 1.0 ]
                        symbolSizes = [ 1.0, 1.0, 1.0, 1.0, 1.0 ]
                        png_or_ps   = 0
                end

    'psfc':     begin
                        prodTitle   = ' Sfc Pressure (mb) '
                        minval      = 600
                        maxval      = 1100
                        gridScales  = [ 1.0, 1.0, 1.0, 1.0, 1.0 ]
                        symbolSizes = [ 1.0, 1.0, 1.0, 1.0, 1.0 ]
                        png_or_ps   = 0
                end

    'tpw':      begin
                        prodTitle   = ' TPW (mm) '
                        minval      = 0
                        maxval      = 80
                        gridScales  = [ 0.5, 0.1, 0.5, 0.5, 0.5 ]
                        symbolSizes = [ 1.5, 5.0, 1.5, 1.5, 1.5 ]
                        png_or_ps   = 0
                end

    'clw':      begin
                        prodTitle   = ' CLW (mm) '
                        minval      = 0
                        maxval      = 0.8
                        gridScales  = [ 0.005, 0.005, 0.005, 0.005, 0.005 ]
                        symbolSizes = [ 125, 125, 125, 125, 125 ]
                        png_or_ps   = 0
                end

    'iwp':      begin
                        prodTitle   = ' IWP (mm) '
                        minval      = 0
                        maxval      = 0.8
                        gridScales  = [ 0.005, 0.005, 0.005, 0.005, 0.005 ]
                        symbolSizes = [ 125, 125, 125, 125, 125 ]
                        png_or_ps   = 0
                end

    'lwp':      begin
                        prodTitle   = ' LWP (mm) '
                        minval      = 0
                        maxval      = 0.8
                        gridScales  = [ 0.005, 0.005, 0.005, 0.005, 0.005 ]
                        symbolSizes = [ 125, 125, 125, 125, 125 ]
                        png_or_ps   = 0
                end

    'rr':       begin
                        prodTitle   = ' Rain Rate (mm/h) '
                        minval      = 0.0
                        maxval      = 10
                        gridScales  = [ 0.05, 0.05, 0.05, 0.05, 0.05 ]
                        symbolSizes = [ 12.5, 12.5, 12.5, 12.5, 12.5 ]
                        png_or_ps   = 0
                end

    'wspd':     begin
                        prodTitle   = ' Wind Speed (m/s) '
                        minval      = 0
                        maxval      = 30
                        gridScales  = [ 0.5, 0.5, 0.5, 0.5, 0.5 ]
                        symbolSizes = [ 5.0, 5.0, 5.0, 5.0, 5.0 ]
                        png_or_ps   = 0
                end


    else:       begin
                        print, 'Unsupported 1D prodId: '+ prodId
                end
    endcase
    
    ;---- read in 2 data sets if 2 files exist
    file_mirs = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(file_mirs)
    if result eq 0 then begin
      print, 'File not exist: ' + file_mirs
      continue 
    endif
    
    file_nwp = dirGrid + 'P2P_'+satId+nwpId+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(file_nwp)
    if result eq 0 then begin
      print, 'File not exist: ' + file_nwp
      continue 
    endif
    
    prod_mirs = fltarr(MAX_PROFILE,MAX_FILE)
    openr, lun, file_mirs, /get_lun, /Swap_Endian
    readu, lun, prod_mirs
    free_lun,lun,/force

    prod_nwp = fltarr(MAX_PROFILE,MAX_FILE)
    openr, lun, file_nwp, /get_lun, /Swap_Endian
    readu, lun, prod_nwp
    free_lun,lun,/force
    
    for isfc = 0, nsfc - 1 do begin ; loop isfc starts
      
      sfcId = sfcIds(isfc)
      sfcInt = sfcInts(isfc)
      sfcTxt = sfcTxts(isfc)
      
      gridScale = gridScales(isfc)
      symbolSize = symbolSizes(isfc)
      
      for icond = 0, ncond - 1 do begin ; loop icond starts
        
        cond = conds(icond)
        condTxt = condTxts(icond)
	
        ;---- porudcts that impacted by CLW and RR
        if prodId eq 'tpw' or prodId eq 'tskin' or prodId eq 'wspd' or prodId eq 'psfc' then begin
        
          ;---- rainy
          if icond eq 0 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond and prod_sfc eq sfcInt, cnt )
          if icond eq 0 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond,cnt )

          ;---- cloudy
          if icond eq 1 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond  and prod_sfc eq sfcInt, cnt )
          if icond eq 1 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond ,cnt )

          ;---- clear
          if icond eq 2 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond and prod_sfc eq sfcInt, cnt )
          if icond eq 2 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond,cnt )

          ;---- all weather condition
          if icond eq 3 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_sfc eq sfcInt, cnt )
          if icond eq 3 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax,cnt )
        
	;---- porudcts that not consider CLW and RR impacts
        endif else begin
        
          if isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_sfc eq sfcInt, cnt )
          if isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax,cnt )

        endelse
        
        ;---- plot
        if cnt gt 2 then begin
          png_name = dirImg+prefix+'p2p_'+yyyymmdd+'_'+prodId+'_'+cond+'_'+sfcId+'_'+cend+'.png'
          title = condTxt+cendTxt+prodTitle+'Over'+sfcTxt+date+' (r' + version +')'
          densityScatter,minval,maxval,minval,maxval,prod_mirs(ss),prod_nwp(ss),satTxt+' MIRS',nwpTxt,title,1,1,stats,png_name,gridScale,symbolSize,png_or_ps
        endif
        
      endfor ; loop icond ends

    endfor ; loop isfc ends
    
  endfor ; loop iprod ends
  
endfor ; loop icend ends


;***************************************************************
; chan products section
;***************************************************************
nprod_chan = N_ELEMENTS(prodIds_chan)

for icend = 0, ncend-2 do begin ; loop icend starts
  
  cend = cends(icend)
  cendTxt = cendTxts(icend)

  ;---- auxiliarly data file
  file_sfc = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_sfc2_'+cend+'.dat'
  file_lat = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_lat_'+cend+'.dat'
  file_clw = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_clw_'+cend+'.dat'
  file_rr  = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_rr_'+cend+'.dat'
  
  result = FILE_TEST(file_sfc)
  if result eq 0 then begin
      print, 'Sfc File not exist: ' + file_sfc
      continue 
  endif
  
  result = FILE_TEST(file_lat)
  if result eq 0 then begin
      print, 'Lat File not exist: ' + file_lat
      continue 
  endif
  
  result = FILE_TEST(file_clw)
  if result eq 0 then begin
      print, 'Clw File not exist: ' + file_clw
      continue 
  endif
  
  result = FILE_TEST(file_rr)
  if result eq 0 then begin
      print, 'RR File not exist: ' + file_rr
      continue 
  endif
  
  ;---- read in auxiliarly data
  prod_sfc = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_sfc, /get_lun, /Swap_Endian
  readu, lun, prod_sfc
  free_lun,lun,/force
  
  prod_lat = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_lat, /get_lun, /Swap_Endian
  readu, lun, prod_lat
  free_lun,lun,/force
  
  prod_clw = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_clw, /get_lun, /Swap_Endian
  readu, lun, prod_clw
  free_lun,lun,/force
  
  prod_rr = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_rr, /get_lun, /Swap_Endian
  readu, lun, prod_rr
  free_lun,lun,/force
  
  
  ;---- define a cond array to hold weather condition to save future comparison
  prod_cond = intarr(MAX_PROFILE,MAX_FILE)
  prod_cond(*,*) = -999
  for ifile = 0L, MAX_FILE-1L do begin
  for iprof = 0L, MAX_PROFILE-1L do begin
    
    if prod_rr(iprof,ifile) ge maxRainAmount then prod_cond(iprof,ifile) = 0
    if prod_rr(iprof,ifile) lt maxRainAmount and prod_clw(iprof,ifile) ge maxCldAmount then prod_cond(iprof,ifile) = 1
    if prod_rr(iprof,ifile) lt maxRainAmount and prod_clw(iprof,ifile) lt maxCldAmount then prod_cond(iprof,ifile) = 2
    
  endfor
  endfor
  
  for iprod = 0, nprod_chan - 1 do begin ; loop iprod starts

    prodId = prodIds_chan[iprod]
    print, 'plotting ' + prodId + ' ... ' + cend

    case  prodId of
  
    'em':       begin
                        prodTitle   = ' Emissivity '
                        minval      = 0.0
                        maxval      = 1.0
                        gridScales  = [ 0.005, 0.005, 0.005, 0.005, 0.005 ]
                        symbolSizes = [ 230, 230, 230, 230, 230 ]
                        png_or_ps   = 0
                end


    else:       begin
                        print, 'Unsupported 1D prodId: '+ prodId
                end
    endcase
    
    
    if prodId eq 'em' then begin
      
      ;---- default to N18/N19/Metop-A values
      chanIds=['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2','57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']
      minvals=[0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4,0.4,0.5,0.5,0.5,0.5]
      maxvals=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]

      if ( satId eq Consts.STR_SATID_F16 ) then begin
        chanIds = ['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
        minvals = [0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4, 0.4,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5]
        maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      endif  

      if ( satId eq Consts.STR_SATID_F17 ) then begin
        chanIds = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
        minvals = [0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4, 0.4,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5]
        maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      endif  

      if ( satId eq Consts.STR_SATID_F18 ) then begin
        chanIds = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
        minvals = [0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4, 0.4,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5]
        maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      endif  

      if ( satId eq Consts.STR_SATID_AQUA ) then begin
        chanIds = ['7v','7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h' ] 
        minvals = [0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4]
        maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      endif  

      if ( satId eq Consts.STR_SATID_GCOMW1 ) then begin
        chanIds = ['6v','6h','7v','7h','10v','10h','18v','18h','23v','23h','36v','36h','89v','89h' ] 
        minvals = [0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4]
        maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      endif  

      if ( satId eq Consts.STR_SATID_TRMM ) then begin
        chanIds = ['11v','11h','19v','19h','21v','37v','37h','85v','85h' ] 
        minvals = [0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4]
        maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      endif  

      if ( satId eq Consts.STR_SATID_GPM ) then begin
        chanIds = ['11v','11h','19v','19h','24v','37v','37h','89v','89h','166h','166v','183v1','183v2']
        minvals = [0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4]
        maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      endif  

      if ( satId eq Consts.STR_SATID_NPP ) then begin
        chanIds = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
        minvals = [0.4,0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4, 0.4,0.4,0.4,0.4,0.4,0.4,0.5,0.5,0.5,0.5,0.5]
        maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      endif

      if ( satId eq Consts.STR_SATID_MTMA ) then begin
        chanIds =['19v','19h','24v','37v','37h','89v','89h','157v','157h'] 
        minvals=[0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4]
        maxvals=[1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      endif
    
      if ( satId eq Consts.STR_SATID_MTSA ) then begin
        chanIds =['183h','184h','186h','187h','190h','194h'] 
        minvals=[0.60,0.60,0.60,0.60,0.60,0.60]
        maxvals=[1.00,1.00,1.00,1.00,1.00,1.00]
      endif  

    
    endif
    
    ;---- MIRS and NWP files
    file_mirs = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(file_mirs)
    if result eq 0 then begin
      print, 'File not exist: ' + file_mirs
      continue 
    endif
    
    file_nwp = dirGrid + 'P2P_'+satId+nwpId+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(file_nwp)
    if result eq 0 then begin
      print, 'File not exist: ' + file_nwp
      continue 
    endif
    
    prod_mirs  = fltarr(MAX_PROFILE,MAX_FILE)
    prods_mirs = fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
    openr, lun, file_mirs, /get_lun, /Swap_Endian
    for ichan = 0, NCHAN-1 do begin
      readu, lun, prod_mirs
      prods_mirs(*,*,ichan) = prod_mirs(*,*)
    endfor  
    free_lun,lun,/force

    prod_nwp  = fltarr(MAX_PROFILE,MAX_FILE)
    prods_nwp = fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
    openr, lun, file_nwp, /get_lun, /Swap_Endian
    for ichan = 0, NCHAN-1 do begin
      readu, lun, prod_nwp
      prods_nwp(*,*,ichan) = prod_nwp(*,*)
    endfor
    free_lun,lun,/force
    
    ;---- plot
    for ichan = 0, NCHAN-1 do begin ; loop ichan starts
      
      chanId = chanIds(ichan)
      minval = minvals(ichan)
      maxval = maxvals(ichan)
      prod_mirs = prods_mirs(*,*,ichan)
      prod_nwp = prods_nwp(*,*,ichan)

      for isfc = 0, nsfc - 1 do begin ; loop isfc starts

        sfcId = sfcIds(isfc)
        sfcInt = sfcInts(isfc)
        sfcTxt = sfcTxts(isfc)

        gridScale = gridScales(isfc)
        symbolSize = symbolSizes(isfc)

        for icond = 0, ncond - 1 do begin ; loop icond starts

          cond = conds(icond)
          condTxt = condTxts(icond)

          ;---- rainy
          if icond eq 0 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond and prod_sfc eq sfcInt, cnt )
          if icond eq 0 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond,cnt )

          ;---- cloudy
          if icond eq 1 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond  and prod_sfc eq sfcInt, cnt )
          if icond eq 1 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond ,cnt )

          ;---- clear
          if icond eq 2 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond and prod_sfc eq sfcInt, cnt )
          if icond eq 2 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond,cnt )

          ;---- all weather condition
          if icond eq 3 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_sfc eq sfcInt, cnt )
          if icond eq 3 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax,cnt )


          ;---- plot
          if cnt gt 2 then begin
            png_name = dirImg+prefix+'p2p_'+yyyymmdd+'_'+prodId+'_'+chanId+'_'+cond+'_'+sfcId+'_'+cend+'.png'
            title = condTxt+cendTxt+prodTitle+'@ '+chanId+' Over'+sfcTxt+date+' (r' + version +')'
            densityScatter,minval,maxval,minval,maxval,prod_mirs(ss),prod_nwp(ss),satTxt+' MIRS',nwpTxt,title,1,1,stats,png_name,gridScale,symbolSize,png_or_ps
          endif

        endfor ; loop icond ends
      
      endfor ; loop isfc ends
    
    endfor ; loop ichan ends
    
  endfor ; loop iprod ends
  
endfor ; loop icend ends



;***************************************************************
; layer products section
;***************************************************************

layers = [100,200,300,400,500,600,700,800,850,900,950]
NLAY = N_Elements(layers)
layers_index = [43, 54, 62, 69, 75, 80, 84, 88, 90, 92, 94]
layIds=['100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb']

nprod_lay = N_ELEMENTS(prodIds_lay)

bias = fltarr(nsfc-1, NLAY, NCOND, nprod_lay, NCEND)
stdv = fltarr(nsfc-1, NLAY, NCOND, nprod_lay, NCEND)

for icend = 0, ncend-2 do begin ; loop icend starts
  
  cend = cends(icend)
  cendTxt = cendTxts(icend)
  
  ;---- auxiliarly data file
  file_sfc = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_sfc2_'+cend+'.dat'
  file_lat = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_lat_'+cend+'.dat'
  file_clw = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_clw_'+cend+'.dat'
  file_rr  = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_rr_'+cend+'.dat'
  
  result = FILE_TEST(file_sfc)
  if result eq 0 then begin
      print, 'Sfc File not exist: ' + file_sfc
      continue 
  endif
  
  result = FILE_TEST(file_lat)
  if result eq 0 then begin
      print, 'Lat File not exist: ' + file_lat
      continue 
  endif
  
  result = FILE_TEST(file_clw)
  if result eq 0 then begin
      print, 'Clw File not exist: ' + file_clw
      continue 
  endif
  
  result = FILE_TEST(file_rr)
  if result eq 0 then begin
      print, 'RR File not exist: ' + file_rr
      continue 
  endif
  
  ;---- read in auxiliarly data
  prod_sfc = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_sfc, /get_lun, /Swap_Endian
  readu, lun, prod_sfc
  free_lun,lun,/force
  
  prod_lat = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_lat, /get_lun, /Swap_Endian
  readu, lun, prod_lat
  free_lun,lun,/force
  
  prod_clw = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_clw, /get_lun, /Swap_Endian
  readu, lun, prod_clw
  free_lun,lun,/force
  
  prod_rr = fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, file_rr, /get_lun, /Swap_Endian
  readu, lun, prod_rr
  free_lun,lun,/force
  
  
  ;---- define a cond array to hold weather condition to save future comparison
  prod_cond = intarr(MAX_PROFILE,MAX_FILE)
  prod_cond(*,*) = -999
  for ifile = 0L, MAX_FILE-1L do begin
  for iprof = 0L, MAX_PROFILE-1L do begin
    if prod_rr(iprof,ifile) ge maxRainAmount then prod_cond(iprof,ifile) = 0
    if prod_rr(iprof,ifile) lt maxRainAmount and prod_clw(iprof,ifile) ge maxCldAmount then prod_cond(iprof,ifile) = 1
    if prod_rr(iprof,ifile) lt maxRainAmount and prod_clw(iprof,ifile) lt maxCldAmount then prod_cond(iprof,ifile) = 2
  endfor
  endfor
  
  for iprod = 0, nprod_lay - 1 do begin ; loop iprod starts

    prodId = prodIds_lay[iprod]
    print, 'plotting ' + prodId + ' ... ' + cend
    
    case  prodId of
  
    'temp':     begin
                        prodTitle   = ' Temp. (K) '
                        minvals     = [180,180,200,200,220,220,220,240,240,240,240]
                        maxvals     = [260,260,260,280,280,300,300,300,300,320,320]
                        gridScales  = [0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5]
                        symbolSizes = [2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5]
                        png_or_ps   = 0
                end

    'wv':       begin
                        prodTitle   = ' WV (g/kg) '
                        minvals     = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
                        maxvals     = [0.10,0.10, 0.20, 1.0, 2.0, 4.0, 6.0, 15.0, 15.0, 20.0, 20.0]
                        gridScales  = [0.001,0.001,0.001,0.005,0.01,0.05,0.05,0.1,0.1,0.1,0.1]
                        symbolSizes = [1150,1150,600,150,100,30,25,10,10,10,10]
                        maxvals_ice = [0.02,0.02, 0.1, 0.2, 1.0, 2.0, 2.0, 3.0, 4.0, 5.0, 6.0]
                        gridScales_ice = [0.0001,0.0001,0.001,0.001,0.005,0.01,0.01,0.05,0.05,0.05,0.05]
                        symbolSizes_ice = [4700,4700,1150,1100,150,100,100,50,30,25,25]
                        png_or_ps   = 0
                end

    else:       begin
                        print, 'Unsupported 1D prodId: '+ prodId
                end
    endcase
    
    ;---- MIRS and NWP P2P files
    file_mirs = dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(file_mirs)
    if result eq 0 then begin
      print, 'File not exist: ' + file_mirs
      continue 
    endif

    file_nwp = dirGrid + 'P2P_'+satId+nwpId+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(file_nwp)
    if result eq 0 then begin
      print, 'File not exist: ' + file_nwp
      continue 
    endif
    
    prod_mirs = fltarr(MAX_PROFILE,MAX_FILE)
    prods_mirs = fltarr(MAX_PROFILE,MAX_FILE,NLAY_ALL)
    openr, lun, file_mirs, /get_lun, /Swap_Endian
    for ilay=0, NLAY_ALL-1 do begin
      readu, lun, prod_mirs
      prods_mirs(*,*,ilay) = prod_mirs(*,*)
    endfor
    free_lun,lun,/force

    prod_nwp = fltarr(MAX_PROFILE,MAX_FILE)
    prods_nwp = fltarr(MAX_PROFILE,MAX_FILE,NLAY_ALL)
    openr, lun, file_nwp, /get_lun, /Swap_Endian
    for ilay=0, NLAY_ALL-1 do begin
      readu, lun, prod_nwp
      prods_nwp(*,*,ilay) = prod_nwp(*,*)
    endfor
    free_lun,lun,/force

    
    ;---- plot
    for ilay = 0, NLAY-1 do begin ; loop ilay starts
      
      layId = layIds(ilay)
      minval = minvals(ilay)
      maxval = maxvals(ilay)
      gridScale = gridScales(ilay)
      symbolSize = symbolSizes(ilay)

      prod_mirs = prods_mirs(*,*,layers_index(ilay))
      prod_nwp = prods_nwp(*,*,layers_index(ilay))

      for isfc = 0, nsfc - 1 do begin ; loop isfc starts

        sfcId = sfcIds(isfc)
        sfcInt = sfcInts(isfc)
        sfcTxt = sfcTxts(isfc)

        for icond = 0, ncond - 1 do begin ; loop icond starts

          cond = conds(icond)
          condTxt = condTxts(icond)

          ;---- rainy
          if icond eq 0 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond and prod_sfc eq sfcInt, cnt )
          if icond eq 0 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond,cnt )

          ;---- cloudy
          if icond eq 1 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond  and prod_sfc eq sfcInt, cnt )
          if icond eq 1 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond ,cnt )

          ;---- clear
          if icond eq 2 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond and prod_sfc eq sfcInt, cnt )
          if icond eq 2 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_cond eq icond,cnt )

          ;---- all weather condition
          if icond eq 3 and isfc lt 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax and prod_sfc eq sfcInt, cnt )
          if icond eq 3 and isfc eq 4 then ss = where( prod_mirs ge 0 and prod_nwp ge 0 and abs(prod_lat) le latMax,cnt )

          ;---- plot
          if cnt gt 2 then begin
            png_name = dirImg+prefix+'p2p_'+yyyymmdd+'_'+prodId+'_'+layId+'_'+cond+'_'+sfcId+'_'+cend+'.png'
            title = condTxt+cendTxt+prodTitle+'@ '+layId+' Over'+sfcTxt+date+' (r' + version +')'
            densityScatter,minval,maxval,minval,maxval,prod_mirs(ss),prod_nwp(ss),satTxt+' MIRS',nwpTxt,title,1,1,stats,png_name,gridScale,symbolSize,png_or_ps
            
            if isfc lt nsfc-1 and prodId eq 'temp' then begin
              bias(isfc,ilay,icond,iprod,icend) = stats(1)
              stdv(isfc,ilay,icond,iprod,icend) = stats(2)
            endif
            if isfc lt nsfc-1 and prodId eq 'wv' then begin
              bias(isfc,ilay,icond,iprod,icend) = stats(1)/stats(8) * 100
              stdv(isfc,ilay,icond,iprod,icend) = stats(2)/stats(8) * 100
            endif
            
          endif

        endfor ; loop icond ends
        
      endfor ; loop isfc ends
    
    endfor ; loop ilay ends
    
  endfor ; loop iprod ends
  
endfor ; loop icend ends



;***************************************************************
;  write out  bias mean and stdv for temp and wv for all cond
;  temp bias as, temp stdv as
;  temp bias ds, temp stdv ds
;  wv bias as, wv stdv as
;  wv bias ds, wv stdv ds
;  wv -- skip 100mb layer
;***************************************************************
openw, lunw, biasPath + '/biasMeanStdv_'+satId+nwpId+yyyymmdd+'.dat', /get_lun
writeu, lunw, bias(*,*,ncond-1,0,0), stdv(*,*,ncond-1,0,0)
writeu, lunw, bias(*,*,ncond-1,0,1), stdv(*,*,ncond-1,0,1)
writeu, lunw, bias(*,1:NLAY-1,ncond-1,1,0), stdv(*,1:NLAY-1,ncond-1,1,0)
writeu, lunw, bias(*,1:NLAY-1,ncond-1,1,1), stdv(*,1:NLAY-1,ncond-1,1,1)
free_lun,lunw,/force
    
    
    
;***************************************************************
; vertical distribution plot
;***************************************************************

;---- combined asc and des, just take average
bias(*,*,*,*,2) = ( bias(*,*,*,*,0) + bias(*,*,*,*,1) ) * 0.5
stdv(*,*,*,*,2) = ( stdv(*,*,*,*,0) + stdv(*,*,*,*,1) ) * 0.5
    
    
;--- Do not plot over sea ice and snow 
bias(1,*,*,*,*) =  -9999.  
stdv(1,*,*,*,*) =  -9999.  
    
bias(3,*,*,*,*) =  -9999.  
stdv(3,*,*,*,*) =  -9999.  
    
prodTxts = [ ' Temp. ', ' WV ' ]

parms = [ 'mean', 'stdv' ]
parmTxts = [ 'Mean Bias ', 'Stdv ' ]
nparm = n_elements(parms)
flags = [1,0] ;--- consistent with parms

for icend = 0, ncend - 1 do begin     ; loop icend starts
for iprod = 0, nprod_lay - 1 do begin ; loop iprod starts
for iparm = 0, nparm - 1 do begin     ; loop iparm starts
for icond = 0, ncond - 1 do begin     ; loop icond starts
    
    cend = cends(icend)
    cendTxt = cendTxts(icend)
    prodId = prodIds_lay(iprod)
    prodTxt = prodTxts(iprod)
    parm = parms(iparm)
    parmTxt = parmTxts(iparm)
    flag = flags(iparm)
    cond = conds(icond)
    condTxt = condTxts(icond)
    
    if prodId eq 'temp' then begin
      yrange = [950,100]
      TQ = 0
      layers = [100,200,300,400,500,600,700,800,850,900,950]
      yticknames = ['100', '200', '300', '400', '500', '600', '700', '800', '850', '900', '950']
    endif else begin
      yrange = [950,200]
      TQ = 1
      layers = [200,300,400,500,600,700,800,850,900,950]
      yticknames = ['200', '300', '400', '500', '600', '700', '800', '850', '900', '950']
    endelse
    
    ytickname = REVERSE(yticknames)
    yticks = N_Elements(ytickname)-1
    
    if iparm eq 0 then val_tmp = bias(*,*,icond,iprod,icend)
    if iparm eq 1 then val_tmp = stdv(*,*,icond,iprod,icend)
    
    if prodId eq 'temp' then val = val_tmp
    if prodId eq 'wv'   then begin
      val = fltarr(nsfc-1, NLAY-1)
      val(*,*) = val_tmp(*,1:NLAY-1)
    endif
    
    if prodId eq 'temp' and parm eq 'mean' then xrange = [-6,6]
    if prodId eq 'temp' and parm eq 'stdv' then xrange = [0,6]
    if prodId eq 'wv'   and parm eq 'mean' then xrange = [-100,100]
    if prodId eq 'wv'   and parm eq 'stdv' then xrange = [0,100]
    
    xtitle = parmTxt
    if prodId eq 'wv' then xtitle = parmTxt + '(%)'
    title = condTxt + 'MIRS ' + satTxt + prodTxt + parmTxt + 'Vert. Distri. ' + date + ' ' + cendTxt + ' (r' + version +')'
    file_png = dirImg+prefix+'p2p_'+yyyymmdd+'_'+prodId+'_'+parm+'_vert_'+cond+'_'+cend+'.png'

    plot_line4, val(0,*), val(1,*), val(2,*), val(3,*), layers, xtitle, 'Pressure (mb)', title, $
	        xrange, yrange, file_png, 'Sea', 'Sea Ice', 'Land', 'Snow', flag, ytickname, yticks, TQ
    
endfor
endfor
endfor
endfor




;***************************************************************
; Special TB plot
;***************************************************************

sfc_as = fltarr(MAX_PROFILE,MAX_FILE)
openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_sfc2_as.dat', /get_lun, /Swap_Endian
readu, lun, sfc_as
free_lun,lun,/force

sfc_ds = fltarr(MAX_PROFILE,MAX_FILE)
openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_sfc2_ds.dat', /get_lun, /Swap_Endian
readu, lun, sfc_ds
free_lun,lun,/force

;***************************************************************
;    read MIRS LAT/LON AS and DS 
;***************************************************************
lat_as=fltarr(MAX_PROFILE,MAX_FILE)
openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_lat_as.dat', /get_lun, /Swap_Endian
readu, lun, lat_as
free_lun,lun,/force

lat_ds=fltarr(MAX_PROFILE,MAX_FILE)
openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_lat_ds.dat', /get_lun, /Swap_Endian
readu, lun, lat_ds
free_lun,lun,/force


;***************************************************************
;    read MIRS RR AS 
;***************************************************************
rr_mirs_as=fltarr(MAX_PROFILE,MAX_FILE)
openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_rr_as.dat', /get_lun, /Swap_Endian
readu, lun, rr_mirs_as
free_lun,lun,/force

;***************************************************************
;    read MIRS RR DS 
;***************************************************************
rr_mirs_ds=fltarr(MAX_PROFILE,MAX_FILE)
openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_rr_ds.dat', /get_lun, /Swap_Endian
readu, lun, rr_mirs_ds
free_lun,lun,/force

;***************************************************************
;    read MIRS CLW
;***************************************************************
clw_mirs_as=fltarr(MAX_PROFILE,MAX_FILE)
openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_clw_as.dat', /get_lun, /Swap_Endian
readu, lun, clw_mirs_as
close, lun
free_lun,lun,/force

clw_mirs_ds=fltarr(MAX_PROFILE,MAX_FILE)
openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_clw_ds.dat', /get_lun, /Swap_Endian
readu, lun, clw_mirs_ds
close, lun
free_lun,lun,/force


IF ( PLOT_TB eq 1 ) THEN BEGIN

  print, 'plot tb...' 
  
  fwdId='_gdas_'
  if ( nwpData eq 2 ) then fwdId='_ecmwf_'
  if ( nwpData eq 3 ) then fwdId='_gfs_'

  prefix2=rotate+satId+fwdId+'asym_glb_'
  prefix3=rotate+satId+fwdId+'hist_glb_'

  nadirScanpos=0
  NUM_BIN=30
  BIN_BOX=4*findgen(NUM_BIN+1)-60
  if ( satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F18 or $
        satId eq Consts.STR_SATID_F17 ) then begin
    NUM_BIN=30
    BIN_BOX=findgen(NUM_BIN)+1
    nadirScanpos=14
  endif
  if ( satId eq Consts.STR_SATID_AQUA) then  begin
    NUM_BIN=191
    BIN_BOX=findgen(NUM_BIN)+1
    nadirScanpos=95
  endif
  if ( satId eq Consts.STR_SATID_GCOMW1) then  begin
    NUM_BIN=243
    BIN_BOX=findgen(NUM_BIN)+1
    nadirScanpos=121
  endif
  if ( satId eq Consts.STR_SATID_TRMM) then  begin
    NUM_BIN=26
    BIN_BOX=findgen(NUM_BIN)+1
    nadirScanpos=12
  endif
  if ( satId eq Consts.STR_SATID_GPM) then  begin
    NUM_BIN=26
    BIN_BOX=findgen(NUM_BIN)+1
    nadirScanpos=12
  endif
  if ( satId eq Consts.STR_SATID_FY3RI ) then  begin
    NUM_BIN=30
    BIN_BOX=4*findgen(NUM_BIN) + 2
    nadirScanpos=14
  endif
  if ( satId eq Consts.STR_SATID_NPP ) then  begin
    NUM_BIN=96
    BIN_BOX=findgen(NUM_BIN)+1
    nadirScanpos=47
  endif
  
; MT MADRAS parameters currently set to those of proxy data
; Will need to be updated with real data
  if satId eq Consts.STR_SATID_MTMA then begin
    NUM_BIN=60
    BIN_BOX=findgen(NUM_BIN)+1
    nadirScanpos=29
  endif

  if ( satId eq Consts.STR_SATID_MTSA ) then begin
    NUM_BIN=35
    BIN_BOX=4*findgen(NUM_BIN+1)-70
    xrange=[-60,60]
    nadirScanpos=17
  endif

  maxTbDiffallowed = 10

  tmp = fltarr(MAX_PROFILE,MAX_FILE)
  ;***************************************************************
  ;    read MIRS TB ( Scene%Ym -> tbu )
  ;***************************************************************
  tb_mirs_as=fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
  openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_tbu_as.dat', /get_lun, /Swap_Endian
  for ichan=0, NCHAN-1 do begin
    readu, lun, tmp
    tb_mirs_as(*,*,ichan) = tmp(*,*)
  endfor
  close, lun
  free_lun,lun,/force

  tb_mirs_ds=fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
  openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_tbu_ds.dat', /get_lun, /Swap_Endian
  for ichan=0, NCHAN-1 do begin
    readu, lun, tmp
    tb_mirs_ds(*,*,ichan) = tmp(*,*)
  endfor
  close, lun
  free_lun,lun,/force

  ;***************************************************************
  ;    read NWP FWD TB ( Rad%tb(FWD files) -> tb )
  ;***************************************************************
  tb_fwd_as=fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
  openr, lun, dirGrid + 'P2P_'+satId+nwpId+yyyymmdd+'_tb_as.dat', /get_lun, /Swap_Endian
  for ichan=0, NCHAN-1 do begin
    readu, lun, tmp
    tb_fwd_as(*,*,ichan) = tmp(*,*)
  endfor
  close, lun
  free_lun,lun,/force

  tb_fwd_ds=fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
  openr, lun, dirGrid + 'P2P_'+satId+nwpId+yyyymmdd+'_tb_ds.dat', /get_lun, /Swap_Endian
  for ichan=0, NCHAN-1 do begin
    readu, lun, tmp
    tb_fwd_ds(*,*,ichan) = tmp(*,*)
  endfor
  close, lun
  free_lun,lun,/force

  ;***************************************************************
  ;    read MIRS CLW
  ;***************************************************************
;  clw_mirs_as=fltarr(MAX_PROFILE,MAX_FILE)
;  openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_clw_as.dat', /get_lun, /Swap_Endian
;  readu, lun, clw_mirs_as
;  close, lun
;  free_lun,lun,/force

;  clw_mirs_ds=fltarr(MAX_PROFILE,MAX_FILE)
;  openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_clw_ds.dat', /get_lun, /Swap_Endian
;  readu, lun, clw_mirs_ds
;  close, lun
;  free_lun,lun,/force

  ;***************************************************************
  ;    read MIRS SCAN POSITION
  ;***************************************************************
  pos_as=fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_scanpos_as.dat', /get_lun, /Swap_Endian
  readu, lun, pos_as
  close, lun
  free_lun,lun,/force

  pos_ds=fltarr(MAX_PROFILE,MAX_FILE)
  openr, lun, dirGrid + 'P2P_'+satId+'_'+yyyymmdd+'_scanpos_ds.dat', /get_lun, /Swap_Endian
  readu, lun, pos_ds
  close, lun
  free_lun,lun,/force

  ;***************************************************************
  ;    read in static and dynamic bias files
  ;***************************************************************
  DynBiasFile = biasPath+'/biasCorrec_'+satId+'_'+suffix
  StatBiasFile = biasPath+'/biasCorrec_'+satId+'.dat'
  
  result = FILE_TEST(DynBiasFile)
  if result eq 0 then begin
      print, 'DynBiasFile not exist: ' + DynBiasFile
      return
  endif
  
  result = FILE_TEST(StatBiasFile)
  if result eq 0 then begin
      print, 'StatBiasFile not exist: ' + StatBiasFile
      return
  endif
  
  print, 'DynBiasFile='+DynBiasFile
  
  ReadBias,DynBiasFile,nchan,nposDynamic,cfreq,meanBiasDyn,Slope,Intercept,meanTbsimu,meanTbmeas
  print, 'StatBiasFile='+StatBiasFile
  ReadBias,StatBiasFile,nchan,nposStatic,cfreq,meanBias,Slope,Intercept,meanTbsimu,meanTbmeas

  ;***************************************************************
  ;    apply bias from dynamice bias file
  ;***************************************************************
  iToApply=-1
  iApplMethod=0
  applyBias_byChan=make_array(nchan,/int,value=1)
  tempCorr=fltarr(NCHAN)
  tbCorrDynamic_as=fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
  tbCorrDynamic_ds=fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
  tbCorrStatic_as=fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
  tbCorrStatic_ds=fltarr(MAX_PROFILE,MAX_FILE,NCHAN)
  ;Static and Dynamic on original uncorrected TBs ( Scene%Ym ) (ascending)
  FOR i=0L,MAX_PROFILE-1 DO BEGIN
      FOR j=0L,MAX_FILE-1 DO BEGIN
          if (tb_mirs_as(i,j,0) le 0 or pos_as(i,j) lt 0) then begin
              tbCorrStatic_as(i,j,*)=-999.
              tbCorrDynamic_as(i,j,*)=-999.
              CONTINUE
          endif
          iscPos=pos_as(i,j)
          iSfc=sfc_as(i,j)
          tempTB=reform(tb_mirs_as(i,j,*))
          apply_bias,tempTB,tempCorr,iscPos,nposDynamic,nposStatic,meanBias,Slope,Intercept,nchan, $
            iToApply,iApplMethod,iSfc,applyBias_byChan,applyBias_byChan, $
            applyBias_byChan,applyBias_byChan
          tbCorrStatic_as(i,j,*) = tempCorr

          iscPos=pos_as(i,j)
          iSfc=sfc_as(i,j)
          tempTB=reform(tb_mirs_as(i,j,*))
          apply_bias,tempTB,tempCorr,iscPos,nposDynamic,nposStatic,meanBiasDyn,Slope,Intercept,nchan, $
            iToApply,iApplMethod,iSfc,applyBias_byChan,applyBias_byChan, $
            applyBias_byChan,applyBias_byChan
          tbCorrDynamic_as(i,j,*) = tempCorr
      ENDFOR
  ENDFOR
  ;Static and Dynamic on original uncorrected TBs ( Scene%Ym ) (descending)
  FOR i=0L,MAX_PROFILE-1 DO BEGIN
      FOR j=0L,MAX_FILE-1 DO BEGIN
          if (tb_mirs_ds(i,j,0) le 0 or pos_ds(i,j) lt 0) then begin
              tbCorrStatic_ds(i,j,*)=-999.
              tbCorrDynamic_ds(i,j,*)=-999.
              CONTINUE
          endif

          iscPos=pos_ds(i,j)
          iSfc=sfc_ds(i,j)
          tempTB=reform(tb_mirs_ds(i,j,*))
          apply_bias,tempTB,tempCorr,iscPos,nposDynamic,nposStatic,meanBias,Slope,Intercept,nchan, $
            iToApply,iApplMethod,iSfc,applyBias_byChan,applyBias_byChan, $
            applyBias_byChan,applyBias_byChan
          tbCorrStatic_ds(i,j,*) = tempCorr

          iscPos=pos_ds(i,j)
          iSfc=sfc_ds(i,j)
          tempTB=reform(tb_mirs_ds(i,j,*))
          apply_bias,tempTB,tempCorr,iscPos,nposDynamic,nposStatic,meanBiasDyn,Slope,Intercept,nchan, $
            iToApply,iApplMethod,iSfc,applyBias_byChan,applyBias_byChan, $
            applyBias_byChan,applyBias_byChan
          tbCorrDynamic_ds(i,j,*) = tempCorr
      ENDFOR
  ENDFOR

  ;*************************************************************************
  ;    compute statistical residual bias from dynamic (ymCorrStatic - fwd)
  ;*************************************************************************
  resi_statistical_static_as = fltarr(NUM_BIN,NCHAN)
  resi_statistical_static_ds = fltarr(NUM_BIN,NCHAN)
  resi_statistical_static_sea_as = fltarr(NUM_BIN,NCHAN)
  resi_statistical_static_sea_ds = fltarr(NUM_BIN,NCHAN)
  resi_statistical_static_lnd_as = fltarr(NUM_BIN,NCHAN)
  resi_statistical_static_lnd_ds = fltarr(NUM_BIN,NCHAN)

  FOR iChan=0,nCHAN-1 DO BEGIN
      sim_as = reform(tb_fwd_as(*,*,ichan),MAX_PROFILE,MAX_FILE)
      sim_ds = reform(tb_fwd_ds(*,*,ichan),MAX_PROFILE,MAX_FILE)
      static_corr_as = reform(tbCorrStatic_as(*,*,ichan),MAX_PROFILE,MAX_FILE)
      static_corr_ds = reform(tbCorrStatic_ds(*,*,ichan),MAX_PROFILE,MAX_FILE)
      
      FOR iBin=0,NUM_BIN-1 DO BEGIN
          ;all as and ds
          ind=where(sim_as gt 0 and sim_as ne 'Inf' and static_corr_as gt 0 and clw_mirs_as lt 0.05 and pos_as eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_as(ind)-sim_as(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  resi_statistical_static_as(iBin,iChan)=mean(static_corr_as(ind(ind2))-sim_as(ind(ind2)))
              endif
          endif
          ind=where(sim_ds gt 0 and sim_ds ne 'Inf' and static_corr_ds gt 0 and clw_mirs_ds lt 0.05 and pos_ds eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_ds(ind)-sim_ds(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  resi_statistical_static_ds(iBin,iChan)=mean(static_corr_ds(ind(ind2))-sim_ds(ind(ind2)))
              endif
          endif

          ;sea as and ds
          ind=where(sfc_as eq Consts.OC_TYP and sim_as gt 0 and sim_as ne 'Inf' and static_corr_as gt 0 and clw_mirs_as lt 0.05 and pos_as eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_as(ind)-sim_as(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  resi_statistical_static_sea_as(iBin,iChan)=mean(static_corr_as(ind(ind2))-sim_as(ind(ind2)))
              endif
          endif
          ind=where(sfc_ds eq Consts.OC_TYP and sim_ds gt 0 and sim_ds ne 'Inf' and static_corr_ds gt 0 and clw_mirs_ds lt 0.05 and pos_ds eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_ds(ind)-sim_ds(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  resi_statistical_static_sea_ds(iBin,iChan)=mean(static_corr_ds(ind(ind2))-sim_ds(ind(ind2)))
              endif
          endif

          ;land as and ds
          ind=where(sfc_as eq Consts.LD_TYP and sim_as gt 0 and sim_as ne 'Inf' and static_corr_as gt 0 and clw_mirs_as lt 0.05 and pos_as eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_as(ind)-sim_as(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  resi_statistical_static_lnd_as(iBin,iChan)=mean(static_corr_as(ind(ind2))-sim_as(ind(ind2)))
              endif
          endif
          ind=where(sfc_ds eq Consts.LD_TYP and sim_ds gt 0 and sim_ds ne 'Inf' and static_corr_ds gt 0 and clw_mirs_ds lt 0.05 and pos_ds eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_ds(ind)-sim_ds(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  resi_statistical_static_lnd_ds(iBin,iChan)=mean(static_corr_ds(ind(ind2))-sim_ds(ind(ind2)))
              endif
          endif


      ENDFOR
  ENDFOR

  ;*************************************************************************
  ;    compute histogram residual bias from dynamic (ymCorrDynamic - fwd)
  ;*************************************************************************
  maxCldAmount=999
  resi_hist_dynamic_as = fltarr(NUM_BIN,NCHAN)
  resi_hist_dynamic_ds = fltarr(NUM_BIN,NCHAN)
  resi_hist_dynamic_sea_as = fltarr(NUM_BIN,NCHAN)
  resi_hist_dynamic_sea_ds = fltarr(NUM_BIN,NCHAN)
  resi_hist_dynamic_lnd_as = fltarr(NUM_BIN,NCHAN)
  resi_hist_dynamic_lnd_ds = fltarr(NUM_BIN,NCHAN)

  FOR iChan=0,nCHAN-1 DO BEGIN
      sim_as = reform(tb_fwd_as(*,*,ichan),MAX_PROFILE,MAX_FILE)
      sim_ds = reform(tb_fwd_ds(*,*,ichan),MAX_PROFILE,MAX_FILE)
      dynamic_corr_as = reform(tbCorrDynamic_as(*,*,ichan),MAX_PROFILE,MAX_FILE)
      dynamic_corr_ds = reform(tbCorrDynamic_ds(*,*,ichan),MAX_PROFILE,MAX_FILE)

      FOR iBin=0,NUM_BIN-1 DO BEGIN
          ;all as and ds
          ind=where(sim_as gt 0 and sim_as ne 'Inf' and dynamic_corr_as gt 0 and clw_mirs_as lt maxCldAmount and pos_as eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = dynamic_corr_as(ind)-sim_as(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_dynamic_as(iBin,iChan)   = mean(loc(indHist))
              endif
          endif

          ind=where(sim_ds gt 0 and sim_ds ne 'Inf' and dynamic_corr_ds gt 0 and clw_mirs_ds lt maxCldAmount and pos_ds eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = dynamic_corr_ds(ind)-sim_ds(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_dynamic_ds(iBin,iChan)   = mean(loc(indHist))
              endif
          endif

          ;ocean as and ds
          ind=where(sfc_as eq Consts.OC_TYP and sim_as gt 0 and sim_as ne 'Inf' and clw_mirs_as lt maxCldAmount and $
                    dynamic_corr_as gt 0 and pos_as eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = dynamic_corr_as(ind)-sim_as(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_dynamic_sea_as(iBin,iChan)   = mean(loc(indHist))
              endif
          endif

          ind=where(sfc_ds eq Consts.OC_TYP and sim_ds gt 0 and sim_ds ne 'Inf' and clw_mirs_ds lt maxCldAmount and $
                    dynamic_corr_ds gt 0 and pos_ds eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = dynamic_corr_ds(ind)-sim_ds(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_dynamic_sea_ds(iBin,iChan)   = mean(loc(indHist))
              endif
          endif

          ;land as and ds
          ind=where(sfc_as eq Consts.LD_TYP and sim_as gt 0 and sim_as ne 'Inf' and clw_mirs_as lt maxCldAmount and $
                    dynamic_corr_as gt 0 and pos_as eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = dynamic_corr_as(ind)-sim_as(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_dynamic_lnd_as(iBin,iChan)   = mean(loc(indHist))
              endif
          endif
          ind=where(sfc_ds eq Consts.LD_TYP and sim_ds gt 0 and sim_ds ne 'Inf' and clw_mirs_ds lt maxCldAmount and $
                    dynamic_corr_ds gt 0 and pos_ds eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = dynamic_corr_ds(ind)-sim_ds(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_dynamic_lnd_ds(iBin,iChan)   = mean(loc(indHist))
              endif
          endif
      ENDFOR
  ENDFOR

  ;***************************************************************
  ;    compute residual bias from static bias (ymCorr - fwd)
  ;***************************************************************
  maxTbDiffallowed = 10
  resi_hist_static_as = fltarr(NUM_BIN,NCHAN)
  resi_hist_static_ds = fltarr(NUM_BIN,NCHAN)
  resi_hist_static_sea_as = fltarr(NUM_BIN,NCHAN)
  resi_hist_static_sea_ds = fltarr(NUM_BIN,NCHAN)
  resi_hist_static_lnd_as = fltarr(NUM_BIN,NCHAN)
  resi_hist_static_lnd_ds = fltarr(NUM_BIN,NCHAN)

  FOR iChan=0,nCHAN-1 DO BEGIN
      sim_as = reform(tb_fwd_as(*,*,ichan),MAX_PROFILE,MAX_FILE)
      sim_ds = reform(tb_fwd_ds(*,*,ichan),MAX_PROFILE,MAX_FILE)
      static_corr_as = reform(tbCorrStatic_as(*,*,ichan),MAX_PROFILE,MAX_FILE)
      static_corr_ds = reform(tbCorrStatic_ds(*,*,ichan),MAX_PROFILE,MAX_FILE)
      FOR iBin=0,NUM_BIN-1 DO BEGIN
          ;all as and ds
          ind=where(sim_as gt 0 and sim_as ne 'Inf' and static_corr_as gt 0 and clw_mirs_as lt maxCldAmount and pos_as eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_as(ind)-sim_as(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_static_as(iBin,iChan)   = mean(loc(indHist))
              endif
          endif

          ind=where(sim_ds gt 0 and sim_ds ne 'Inf' and static_corr_ds gt 0 and clw_mirs_ds lt maxCldAmount and pos_ds eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_ds(ind)-sim_ds(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_static_ds(iBin,iChan)   = mean(loc(indHist))
              endif
          endif

          ;ocean as and ds
          ind=where(sfc_as eq Consts.OC_TYP and sim_as gt 0 and sim_as ne 'Inf' and $
                    static_corr_as gt 0 and clw_mirs_as lt maxCldAmount and pos_as eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_as(ind)-sim_as(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_static_sea_as(iBin,iChan)   = mean(loc(indHist))
              endif
          endif

          ind=where(sfc_ds eq Consts.OC_TYP and sim_ds gt 0 and sim_ds ne 'Inf' and $
                    static_corr_ds gt 0 and clw_mirs_ds lt maxCldAmount and pos_ds eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_ds(ind)-sim_ds(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_static_sea_ds(iBin,iChan)   = mean(loc(indHist))
              endif
          endif

          ;land as and ds
          ind=where(sfc_as eq Consts.LD_TYP and sim_as gt 0 and sim_as ne 'Inf' and $
                    static_corr_as gt 0 and clw_mirs_as lt maxCldAmount and pos_as eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_as(ind)-sim_as(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_static_lnd_as(iBin,iChan)   = mean(loc(indHist))
              endif
          endif
          ind=where(sfc_ds eq Consts.LD_TYP and sim_ds gt 0 and sim_ds ne 'Inf' and $
                    static_corr_ds gt 0 and clw_mirs_ds lt maxCldAmount and pos_ds eq iBin+1, ncount)
          if (ncount gt 0) then begin
              tbDiff = static_corr_ds(ind)-sim_ds(ind)
              ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
              if ( ncount2 gt 1 ) then begin
                  res=histogram(tbDiff(ind2),nbins=40,locations=loc)
                  res=res/float(max(res))*100.
                  indHist=where(res ge 80.)
                  resi_hist_static_lnd_ds(iBin,iChan)   = mean(loc(indHist))
              endif
          endif
      ENDFOR
  ENDFOR

  ;***************************************************************
  ;    plot ym - tb angle asymmetry 
  ;***************************************************************
  maxCldAmount=999
  xrange=[-60,60]
  yrange=[-5,5]
  xtitle='Local Zenith Angle (degree)'

  channels_str=['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2','57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']
  
  if ( satId eq Consts.STR_SATID_F16 ) then begin
	channels_str=['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
        xrange=[1,30]
	xtitle='Scan Position'
  endif  
  if ( satId eq Consts.STR_SATID_F17 ) then begin
	channels_str=['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
        xrange=[1,30]
	xtitle='Scan Position'
  endif  
  if ( satId eq Consts.STR_SATID_F18 ) then begin
	channels_str=['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']  	
        xrange=[1,30]
	xtitle='Scan Position'
  endif  
  if ( satId eq Consts.STR_SATID_AQUA ) then begin
	channels_str=['7v','7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h' ] 
        xrange=[1,191]
        xtitle='Scan Position'
  endif
  if ( satId eq Consts.STR_SATID_GCOMW1 ) then begin
	channels_str=['6v','6h','7v','7h','10v','10h','18v','18h','23v','23h','36v','36h','89v','89h' ] 
        xrange=[1,243]
        xtitle='Scan Position'
  endif
  if ( satId eq Consts.STR_SATID_TRMM ) then begin
	channels_str=['11v','11h','19v','19h','21v','37v','37h','85v','85h' ] 
        xrange=[1,26]
        xtitle='Scan Position'
  endif
  if ( satId eq Consts.STR_SATID_GPM ) then begin
	channels_str=[ '11v','11h','19v','19h','24v','37v','37h','89v','89h','166h','166v','183v1','183v2' ]
        xrange=[1,26]
        xtitle='Scan Position'
  endif
  if ( satId eq Consts.STR_SATID_NPP ) then begin
        channels_str = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
        xrange=[1,96]
        xtitle='Scan Position'
  endif  
  
  if ( satId eq Consts.STR_SATID_MTMA ) then begin
        channels_str =['19v','19h','24v','37v','37h','89v','89h','157v','157h'] 
        xrange=[1,60]
        xtitle='Scan Position'
  endif

  if ( satId eq Consts.STR_SATID_MTSA ) then begin
        channels_str =['183h','184h','186h','187h','190h','194h'] 
        xrange=[-60,60]
        xtitle='Local Zenith Angle (degree)'
  endif  

  for ichan = 0, NCHAN-1 do begin
    ;-------------------------------------
    ;-- all
    ;-------------------------------------
    lndsea='all'
    ytitle='Tb Bias All (K)'
    Lines2Plot = [1,1,1,1,1]
    x1=-999
    x2=-999
    x3=-999
    meanx1=x1
    meanx2=x2
    meanx3=x3
    ;---Residual/Correction vs. Scan Angle
    title='MIRS ' + strupcase(satId) + ' Tb Bias Correction at ' + channels_str[ichan] + ' ' + date + ' Asc' + ' (V' + version +')'
    map_name=dirImg+prefix2+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_as.png'
    plot_lines_4Bias, bin_box, resi_hist_static_as(*,ichan),resi_hist_dynamic_as(*,ichan),resi_statistical_static_as(*,ichan),meanBias(*,ichan),meanBiasDyn(*,ichan), $
      xtitle, ytitle, title, xrange, yrange, map_name, "Static","Dynamic","Statistical","Static","Dynamic", Lines2Plot

    title='MIRS ' + strupcase(satId) + ' Tb Bias Correction at ' + channels_str[ichan] + ' '+ date + ' Des' + ' (V' + version +')'
    map_name=dirImg+prefix2+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_ds.png'
    plot_lines_4Bias, bin_box, resi_hist_static_ds(*,ichan),resi_hist_dynamic_ds(*,ichan),resi_statistical_static_ds(*,ichan),meanBias(*,ichan),meanBiasDyn(*,ichan), $
      xtitle, ytitle, title, xrange, yrange, map_name, "Static","Dynamic","Statistical","Static","Dynamic", Lines2Plot

    ;---Histograms Ascending
    title='Histograms: ' + strupcase(satId) + ' Tb Bias Correction Nadir Asc (V'+version+')'
    map_name=dirImg+prefix3+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_as.png'

    ym=reform(tbCorrStatic_as(*,*,ichan))       ;Corrected Static - fwd
    fwd=reform(tb_fwd_as(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0 and fwd ne 'Inf' and clw_mirs_as lt maxCldAmount and pos_as eq nadirScanpos,cnt1)
    IF (cnt1 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x1=ym(ind(ind2))-fwd(ind(ind2))
        meanx1=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tbCorrDynamic_as(*,*,ichan))      ;Corrected Dynamic - fwd 
    fwd=reform(tb_fwd_as(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_as lt maxCldAmount and pos_as eq nadirScanpos,cnt2)
    IF (cnt2 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x2=ym(ind(ind2))-fwd(ind(ind2))
        meanx2=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tb_mirs_as(*,*,ichan))            ;Uncorrected - fwd
    fwd=reform(tb_fwd_as(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_as lt maxCldAmount and pos_as eq nadirScanpos,cnt3)
    IF (cnt3 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x3=ym(ind(ind2))-fwd(ind(ind2))
        meanx3=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    IF (cnt1 gt 0 and cnt2 gt 0 and cnt3 gt 0) THEN BEGIN
        PLOT_HIST_4BIAS,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDIF ELSE BEGIN
        PLOT_HIST_4BIAS_BLANK,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDELSE

    ;---Histograms Descending
    title='Histograms: ' + strupcase(satId) + ' Tb Bias Correction Nadir Des (V'+version+')'
    map_name=dirImg+prefix3+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_ds.png'

    ym=reform(tbCorrStatic_ds(*,*,ichan))       ;Corrected Static - fwd
    fwd=reform(tb_fwd_ds(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0 and fwd ne 'Inf' and clw_mirs_ds lt maxCldAmount and pos_ds eq nadirScanpos,cnt1)
    IF (cnt1 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x1=ym(ind(ind2))-fwd(ind(ind2))
        meanx1=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tbCorrDynamic_ds(*,*,ichan))      ;Corrected Dynamic - fwd 
    fwd=reform(tb_fwd_ds(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_ds lt maxCldAmount and pos_ds eq nadirScanpos,cnt2)
    IF (cnt2 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x2=ym(ind(ind2))-fwd(ind(ind2))
        meanx2=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tb_mirs_ds(*,*,ichan))            ;Uncorrected - fwd
    fwd=reform(tb_fwd_ds(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_ds lt maxCldAmount and pos_ds eq nadirScanpos,cnt3)
    IF (cnt3 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x3=ym(ind(ind2))-fwd(ind(ind2))
        meanx3=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    IF (cnt1 gt 0 and cnt2 gt 0 and cnt3 gt 0) THEN BEGIN
        PLOT_HIST_4BIAS,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDIF ELSE BEGIN
        PLOT_HIST_4BIAS_BLANK,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDELSE

    ;-------------------------------------
    ;-- sea
    ;-------------------------------------
    ;---Residual/Correction vs. Scan Angle
    lndsea='sea'
    ytitle='Tb Bias Over Ocean (K)'
    Lines2Plot = [1,1,1,1,1]

    title='MIRS ' + strupcase(satId) + ' Tb Bias Correction at ' + channels_str[ichan] + ' '+ date + ' Asc' + ' (V' + version +')'
    map_name=dirImg+prefix2+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_as.png'
    plot_lines_4Bias, bin_box, resi_hist_static_sea_as(*,ichan),resi_hist_dynamic_sea_as(*,ichan),resi_statistical_static_sea_as(*,ichan), $
      meanBias(*,ichan),meanBiasDyn(*,ichan),xtitle, ytitle, title, xrange, yrange, map_name, "Static","Dynamic","Statistical","Static","Dynamic", Lines2Plot

    title='MIRS ' + strupcase(satId) + ' Tb Bias Correction at ' + channels_str[ichan] + ' '+ date + ' Des' + ' (V' + version +')'
    map_name=dirImg+prefix2+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_ds.png'
    plot_lines_4Bias, bin_box, resi_hist_static_sea_ds(*,ichan),resi_hist_dynamic_sea_ds(*,ichan),resi_statistical_static_sea_ds(*,ichan), $
      meanBias(*,ichan),meanBiasDyn(*,ichan),xtitle, ytitle, title, xrange, yrange, map_name, "Static","Dynamic","Statistical","Static","Dynamic", Lines2Plot

    ;---Histograms Ascending
    title='Histograms: ' + strupcase(satId) + ' Tb Bias Correction Nadir Asc (V'+version+')'
    map_name=dirImg+prefix3+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_as.png'

    ym=reform(tbCorrStatic_as(*,*,ichan))       ;Corrected Static - fwd
    fwd=reform(tb_fwd_as(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0 and fwd ne 'Inf' and clw_mirs_as lt maxCldAmount and sfc_as eq 0 and pos_as eq nadirScanpos,cnt1)
    IF (cnt1 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x1=ym(ind(ind2))-fwd(ind(ind2))
        meanx1=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tbCorrDynamic_as(*,*,ichan))      ;Corrected Dynamic - fwd 
    fwd=reform(tb_fwd_as(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0 and fwd ne 'Inf' and clw_mirs_as lt maxCldAmount and sfc_as eq 0 and pos_as eq nadirScanpos,cnt2)
    IF (cnt2 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x2=ym(ind(ind2))-fwd(ind(ind2))
        meanx2=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tb_mirs_as(*,*,ichan))                 ;Uncorrected - fwd
    fwd=reform(tb_fwd_as(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0 and fwd ne 'Inf' and clw_mirs_as lt maxCldAmount and sfc_as eq 0 and pos_as eq nadirScanpos,cnt3)
    IF (cnt3 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x3=ym(ind(ind2))-fwd(ind(ind2))
        meanx3=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    IF (cnt1 gt 0 and cnt2 gt 0 and cnt3 gt 0) THEN BEGIN
        PLOT_HIST_4BIAS,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDIF ELSE BEGIN
        PLOT_HIST_4BIAS_BLANK,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDELSE

    ;---Histograms Descending
    title='Histograms: ' + strupcase(satId) + ' Tb Bias Correction Nadir Des (V'+version+')'
    map_name=dirImg+prefix3+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_ds.png'

    ym=reform(tbCorrStatic_ds(*,*,ichan))       ;Corrected Static - fwd
    fwd=reform(tb_fwd_ds(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_ds lt maxCldAmount and sfc_ds eq 0 and pos_ds eq nadirScanpos,cnt1)
    IF (cnt1 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x1=ym(ind(ind2))-fwd(ind(ind2))
        meanx1=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tbCorrDynamic_ds(*,*,ichan))      ;Corrected Dynamic - fwd 
    fwd=reform(tb_fwd_ds(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_ds lt maxCldAmount and sfc_ds eq 0 and pos_ds eq nadirScanpos,cnt2)
    IF (cnt2 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x2=ym(ind(ind2))-fwd(ind(ind2))
        meanx2=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tb_mirs_ds(*,*,ichan))                 ;Uncorrected - fwd
    fwd=reform(tb_fwd_ds(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_ds lt maxCldAmount and sfc_ds eq 0 and pos_ds eq nadirScanpos,cnt3)
    IF (cnt3 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x3=ym(ind(ind2))-fwd(ind(ind2))
        meanx3=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    IF (cnt1 gt 0 and cnt2 gt 0 and cnt3 gt 0) THEN BEGIN
        PLOT_HIST_4BIAS,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDIF ELSE BEGIN
        PLOT_HIST_4BIAS_BLANK,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDELSE


    ;-------------------------------------
    ;-- land
    ;-------------------------------------
    ;---Residual/Correction vs. Scan Angle
    lndsea='lnd'
    ytitle='TB Bias Over Land (K)'

    title='MIRS ' + strupcase(satId) + ' Tb Bias Correction at ' + channels_str[ichan] + ' ' + date + ' Asc' + ' (V' + version +')'
    map_name=dirImg+prefix2+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_as.png'
    plot_lines_4Bias, bin_box, resi_hist_static_lnd_as(*,ichan),resi_hist_dynamic_lnd_as(*,ichan),resi_statistical_static_lnd_as(*,ichan), $
      meanBias(*,ichan),meanBiasDyn(*,ichan),xtitle, ytitle, title, xrange, yrange, map_name, "Static","Dynamic","Statistical","Static","Dynamic", Lines2Plot

    title='MIRS ' + strupcase(satId) + ' Tb Bias Correction at ' + channels_str[ichan] + ' '+ date + ' Des' + ' (V' + version +')'
    map_name=dirImg+prefix2+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_ds.png'
    plot_lines_4Bias, bin_box, resi_hist_static_lnd_ds(*,ichan),resi_hist_dynamic_lnd_ds(*,ichan),resi_statistical_static_lnd_ds(*,ichan), $
      meanBias(*,ichan),meanBiasDyn(*,ichan),xtitle, ytitle, title, xrange, yrange, map_name, "Static","Dynamic","Statistical","Static","Dynamic", Lines2Plot

    ;---Histograms
    title='Histograms: ' + strupcase(satId) + ' Tb Bias Correction Nadir Asc (V'+version+')'
    map_name=dirImg+prefix3+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_as.png'

    ym=reform(tbCorrStatic_as(*,*,ichan))       ;Corrected Static - fwd
    fwd=reform(tb_fwd_as(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_as lt maxCldAmount and sfc_as eq 2 and pos_as eq nadirScanpos,cnt1)
    IF (cnt1 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x1=ym(ind(ind2))-fwd(ind(ind2))
        meanx1=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tbCorrDynamic_as(*,*,ichan))      ;Corrected Dynamic - fwd 
    fwd=reform(tb_fwd_as(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_as lt maxCldAmount and sfc_as eq 2 and pos_as eq nadirScanpos,cnt2)
    IF (cnt2 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x2=ym(ind(ind2))-fwd(ind(ind2))
        meanx2=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tb_mirs_as(*,*,ichan))            ;Uncorrected - fwd
    fwd=reform(tb_fwd_as(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_as lt maxCldAmount and sfc_as eq 2 and pos_as eq nadirScanpos,cnt3)
    IF (cnt3 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x3=ym(ind(ind2))-fwd(ind(ind2))
        meanx3=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    IF (cnt1 gt 0 and cnt2 gt 0 and cnt3 gt 0) THEN BEGIN
        PLOT_HIST_4BIAS,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDIF ELSE BEGIN
        PLOT_HIST_4BIAS_BLANK,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDELSE

    ;---Histograms Descending
    title='Histograms: ' + strupcase(satId) + ' Tb Bias Correction Nadir Des (V'+version+')'
    map_name=dirImg+prefix3+yyyymmdd+'_tb_'+channels_str[ichan]+'_'+lndsea+'_ds.png'

    ym=reform(tbCorrStatic_ds(*,*,ichan))       ;Corrected Static - fwd
    fwd=reform(tb_fwd_ds(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_ds lt maxCldAmount and sfc_ds eq 2 and pos_ds eq nadirScanpos,cnt1)
    IF (cnt1 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x1=ym(ind(ind2))-fwd(ind(ind2))
        meanx1=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tbCorrDynamic_ds(*,*,ichan))      ;Corrected Dynamic - fwd 
    fwd=reform(tb_fwd_ds(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_ds lt maxCldAmount and sfc_ds eq 2 and pos_ds eq nadirScanpos,cnt2)
    IF (cnt2 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x2=ym(ind(ind2))-fwd(ind(ind2))
        meanx2=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    ym=reform(tb_mirs_ds(*,*,ichan))             ;Uncorrected - fwd
    fwd=reform(tb_fwd_ds(*,*,ichan))
    ind=where(ym gt 0 and fwd gt 0  and fwd ne 'Inf' and clw_mirs_ds lt maxCldAmount and sfc_ds eq 2 and pos_ds eq nadirScanpos,cnt3)
    IF (cnt3 gt 0) THEN BEGIN
        ind2=where(abs( (ym(ind)-fwd(ind))-mean(ym(ind)-fwd(ind)) ) le maxTbDiffallowed)
        x3=ym(ind(ind2))-fwd(ind(ind2))
        meanx3=mean(ym(ind(ind2))-fwd(ind(ind2)))
    ENDIF

    IF (cnt1 gt 0 and cnt2 gt 0 and cnt3 gt 0) THEN BEGIN
        PLOT_HIST_4BIAS,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDIF ELSE BEGIN
        PLOT_HIST_4BIAS_BLANK,x1,x2,x3,40,title,'TB Departures (K)','Normalized PDF',meanx1,meanx2,meanx3,-10,10, $
          'Static-Sim','Dynamic-Sim','Uncorr.-Sim',1,map_name
    ENDELSE

  endfor  

ENDIF



End
