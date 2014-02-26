@../../../setup/paths_idl.pro
;***********************************************************************************************
;
;  Purpose:
;    To grid and plot EDR products.
;
;  Dependence:
;    LoadSceneFile
;    plot_grid
;    plot_sfc_grid
;
;  Record of revisions:
;       Date            Programmer     	Description
;     ----------      --------------    --------------
;     01/28/2011       Wanchun Chen     Original Coder 
;
;***********************************************************************************************
Pro gridEdr, namelist=namelist
seconds_start = SYSTIME( /SECONDS )
Consts, Consts

;---- identifiers for namelist ----
satId='trmm'
gridfactor=2
dirImg='./'
date='2011-01-18'
processMode=1
version='2435'
latmin=-90
latmax=90
lonmin=-180
lonmax=180
edr_file_list='/home/pub/wchen/mirs_trmm/data/InputsData/trmm_edrFiles_2011-01-18.list'
resolutionStr='CR'

fromNameList=1
if ( fromNameList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId		       	;Satellite ID
  readf,iu,format='(i)', gridfactor	       	;gridfactor
  readf,iu,format='(a)', dirImg  	       	;path where to put image files
  readf,iu,format='(a)', date		       	;file extension of image
  readf,iu,format='(i)', processMode	       	;process Mode
  readf,iu,format='(a)', version    		;version number
  readf,iu,format='(f)', latmin              	;min lat
  readf,iu,format='(f)', latmax              	;max lat
  readf,iu,format='(f)', lonmin              	;min lon
  readf,iu,format='(f)', lonmax              	;max lon
  readf,iu,format='(a)', edr_file_list	       	;EDR file list
  readf,iu,format='(a)', resolutionStr	       	;extention of resolution
  close,iu
  free_lun,iu,/force
endif

print, 'gridEdr...'

resolution = '_' + STRLOWCASE(resolutionStr) + '_'
;---- default to N18/N19/Metop-A ----
prefix='mirs_adv_poes_'+satId+'_amsuamhs' + resolution
if satId eq Consts.STR_SATID_F16   then prefix='mirs_adv_dmsp_'  +satId+'_ssmis' + resolution
if satId eq Consts.STR_SATID_F17   then prefix='mirs_adv_dmsp_'  +satId+'_ssmis' + resolution
if satId eq Consts.STR_SATID_F18   then prefix='mirs_adv_dmsp_'  +satId+'_ssmis' + resolution
if satId eq Consts.STR_SATID_NPP   then prefix='mirs_adv_npoess_'+satId+'_atms'  + resolution
if satId eq Consts.STR_SATID_TRMM  then prefix='mirs_adv_eos_'   +satId+'_tmi'   + resolution
if satId eq Consts.STR_SATID_GPM   then prefix='mirs_adv_eos_'   +satId+'_gmi'   + resolution
if satId eq Consts.STR_SATID_AQUA  then prefix='mirs_adv_eos_'   +satId+'_amsre' + resolution
if satId eq Consts.STR_SATID_FY3RI then prefix='mirs_adv_eos_'   +satId+'_mwri'  + resolution

titleSatId='MIRS ' + STRUPCASE(satId)
if ( satId eq Consts.STR_SATID_AQUA  ) then titleSatId = 'MIRS ' + 'AMSR-E'
if ( satId eq Consts.STR_SATID_FY3RI ) then titleSatId = 'MIRS ' + 'FY3/MWRI'
if ( satId eq Consts.STR_SATID_TRMM  ) then titleSatId = 'MIRS ' + 'TRMM/TMI'
if ( satId eq Consts.STR_SATID_GPM   ) then titleSatId = 'MIRS ' + 'GPM/GMI'
if ( satId eq 'trmm_2a12'            ) then titleSatId = strupcase(satId)

if ( processMode eq 0 ) then yyyymmdd=date
if ( processMode eq 1 ) then yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

NCOL=360*gridfactor
NROW=180*gridfactor
NLAY=11
NCHAN=20
if ( satId eq Consts.STR_SATID_N18    ) then NCHAN = 20
if ( satId eq Consts.STR_SATID_N19    ) then NCHAN = 20
if ( satId eq Consts.STR_SATID_METOPA ) then NCHAN = 20
if ( satId eq Consts.STR_SATID_METOPB ) then NCHAN = 20
if ( satId eq Consts.STR_SATID_F16    ) then NCHAN = 24
if ( satId eq Consts.STR_SATID_F17    ) then NCHAN = 24
if ( satId eq Consts.STR_SATID_F18    ) then NCHAN = 24
if ( satId eq Consts.STR_SATID_AQUA   ) then NCHAN = 12
if ( satId eq Consts.STR_SATID_FY3RI  ) then NCHAN = 10
if ( satId eq Consts.STR_SATID_NPP    ) then NCHAN = 22
if ( satId eq Consts.STR_SATID_TRMM   ) then NCHAN = 9
if ( satId eq Consts.STR_SATID_GPM    ) then NCHAN = 13

titleSatId='MIRS ' + strupcase(satId)
if ( satId eq Consts.STR_SATID_AQUA  ) then titleSatId = 'MIRS ' + 'AMSR-E'
if ( satId eq Consts.STR_SATID_FY3RI ) then titleSatId = 'MIRS ' + 'FY3/MWRI'
if ( satId eq Consts.STR_SATID_TRMM  ) then titleSatId = 'MIRS ' + 'TRMM/TMI'
if ( satId eq Consts.STR_SATID_GPM   ) then titleSatId = 'MIRS ' + 'GPM/GMI'
if ( satId eq 'trmm_2a12'            ) then titleSatId = strupcase(satId)

;*******************************************************************************
; define products IDs to be plotted
;*******************************************************************************

prodIds_sfc   = [ 'chisq', 'nattempt', 'niter', 'qc', 'psfc', 'sfc', 'tskin' ]
;prodIds_lay  = [ 'temp', 'wv', 'clwp', 'rainp', 'graupelp' ]                   
prodIds_lay   = [ 'temp', 'wv' ]                   
prodIds_chan  = [ 'em', 'tbc', 'tbu' ]                   
prodIds_polar = [ 'sfc' ]


;*******************************************************************************
; filling parameters
;*******************************************************************************
PI=3.141593
adjustment=0.5
LATLIM_A=12

NFOV = 30 ; default to AMSUA

fmType = 0
if satId eq Consts.STR_SATID_F16   then begin
  if resolutionStr eq 'UAS' then NFOV = 30  & fmType = 0
  if resolutionStr eq 'LAS' then NFOV = 60  & fmType = 1
  if resolutionStr eq 'ENV' then NFOV = 90  & fmType = 2
  if resolutionStr eq 'IMG' then NFOV = 180 & fmType = 3
endif

if satId eq Consts.STR_SATID_F17   then begin
  if resolutionStr eq 'UAS' then NFOV = 30  & fmType = 0
  if resolutionStr eq 'LAS' then NFOV = 60  & fmType = 1
  if resolutionStr eq 'ENV' then NFOV = 90  & fmType = 2
  if resolutionStr eq 'IMG' then NFOV = 180 & fmType = 3
endif

if satId eq Consts.STR_SATID_F18   then begin
  if resolutionStr eq 'UAS' then NFOV = 30  & fmType = 0
  if resolutionStr eq 'LAS' then NFOV = 60  & fmType = 1
  if resolutionStr eq 'ENV' then NFOV = 90  & fmType = 2
  if resolutionStr eq 'IMG' then NFOV = 180 & fmType = 3
endif

if satId eq Consts.STR_SATID_AQUA  then begin
  NFOV = 191
  fmType = 1
endif

if satId eq Consts.STR_SATID_FY3RI then begin
  NFOV = 120
  fmType = 1
endif

if satId eq Consts.STR_SATID_NPP   then begin
  NFOV = 32
  fmType = 0
endif

if satId eq Consts.STR_SATID_TRMM  then begin
  if resolutionStr eq 'CR' then NFOV = 26  & fmType = -1
  if resolutionStr eq 'LR' then NFOV = 104 & fmType = 0
  if resolutionStr eq 'HR' then NFOV = 208 & fmType = 1
endif

if satId eq Consts.STR_SATID_GPM   then begin
  if resolutionStr eq 'CR' then NFOV = 26  & fmType = -1
  if resolutionStr eq 'LR' then NFOV = 104 & fmType = 0
  if resolutionStr eq 'HR' then NFOV = 208 & fmType = 1
endif

nadir_as = intarr(NCOL,NROW) & nadir_as(*,*) = NFOV
nadir_ds = intarr(NCOL,NROW) & nadir_ds(*,*) = NFOV


;*******************************************************************************
;  get fov_sizes
;*******************************************************************************
fov_sizes = fltarr(NFOV)
fov_sizes(*) = 1.0

if satId eq Consts.STR_SATID_N18 or satId eq Consts.STR_SATID_METOPA or $
   satId eq Consts.STR_SATID_N19 or satId eq Consts.STR_SATID_METOPB or $
   satId eq Consts.STR_SATID_NPP then begin
   if fmType eq 0 then FOV_A, fov_sizes
   if fmType eq 1 then FOV_B, fov_sizes
endif

if( satId eq Consts.STR_SATID_F16 ) then fov_sizes(*) = 0.6783
if( satId eq Consts.STR_SATID_F17 ) then fov_sizes(*) = 0.6783
if( satId eq Consts.STR_SATID_F18 ) then fov_sizes(*) = 0.6783


;*******************************************************************************
; read static land/sea masks
;*******************************************************************************
sfcMask=BytArr(nCol,nRow)
dum1=BytArr(nCol,nRow)
dum2=BytArr(nCol,nRow)
IF ( nCol EQ 1440 ) THEN OpenR,Lun,'lndsea25.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 1080 ) THEN OpenR,Lun,'lndsea30.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 720  ) THEN OpenR,Lun,'lndsea50.tag',/Get_Lun, /Swap_If_Big_Endian
ReadU,Lun,dum1
Free_Lun,Lun

;---- reverse index of land/sea mask to be consitent IDL convention ----
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
; grid identifiers, requires about 700 MB memory usage
;*******************************************************************************

grid_chisq_as    = fltarr(NCOL,NROW)       & grid_chisq_as(*,*)    = -999.0
grid_chisq_ds    = fltarr(NCOL,NROW)       & grid_chisq_ds(*,*)    = -999.0

grid_niter_as    = fltarr(NCOL,NROW)       & grid_niter_as(*,*)    = -999.0
grid_niter_ds    = fltarr(NCOL,NROW)       & grid_niter_ds(*,*)    = -999.0

grid_nattempt_as = fltarr(NCOL,NROW)       & grid_nattempt_as(*,*) = -999.0
grid_nattempt_ds = fltarr(NCOL,NROW)       & grid_nattempt_ds(*,*) = -999.0

grid_qc_as       = fltarr(NCOL,NROW)       & grid_qc_as(*,*)       = -999.0
grid_qc_ds       = fltarr(NCOL,NROW)       & grid_qc_ds(*,*)       = -999.0

grid_psfc_as     = fltarr(NCOL,NROW)       & grid_psfc_as(*,*)     = -999.0
grid_psfc_ds     = fltarr(NCOL,NROW)       & grid_psfc_ds(*,*)     = -999.0

grid_sfc_as      = fltarr(NCOL,NROW)       & grid_sfc_as(*,*)      = -999.0
grid_sfc_ds      = fltarr(NCOL,NROW)       & grid_sfc_ds(*,*)      = -999.0

grid_tskin_as    = fltarr(NCOL,NROW)       & grid_tskin_as(*,*)    = -999.0
grid_tskin_ds    = fltarr(NCOL,NROW)       & grid_tskin_ds(*,*)    = -999.0


grid_em_as       = fltarr(NCOL,NROW,NCHAN) & grid_em_as(*,*,*)     = -999.0
grid_em_ds       = fltarr(NCOL,NROW,NCHAN) & grid_em_ds(*,*,*)     = -999.0

grid_tbc_as      = fltarr(NCOL,NROW,NCHAN) & grid_tbc_as(*,*,*)    = -999.0
grid_tbc_ds      = fltarr(NCOL,NROW,NCHAN) & grid_tbc_ds(*,*,*)    = -999.0

grid_tbu_as      = fltarr(NCOL,NROW,NCHAN) & grid_tbu_as(*,*,*)    = -999.0
grid_tbu_ds      = fltarr(NCOL,NROW,NCHAN) & grid_tbu_ds(*,*,*)    = -999.0


grid_temp_as     = fltarr(NCOL,NROW,NLAY)  & grid_temp_as(*,*,*)   = -999.0
grid_temp_ds     = fltarr(NCOL,NROW,NLAY)  & grid_temp_ds(*,*,*)   = -999.0

grid_wv_as       = fltarr(NCOL,NROW,NLAY)  & grid_wv_as(*,*,*)     = -999.0
grid_wv_ds       = fltarr(NCOL,NROW,NLAY)  & grid_wv_ds(*,*,*)     = -999.0

qc_tmp           = intarr(4)               & qc_tmp(*)             = -999

;---- only plot certain layers of temp and wv ---- 
layers       = [100,200,300,400,500,600,700,800,850,900,950]
layers_index = [ 43, 54, 62, 69, 75, 80, 84, 88, 90, 92, 94]


;*******************************************************************************
;    loop read file from file list
;*******************************************************************************
openr, iu_edr, edr_file_list, /get_lun

file_edr=''

WHILE ( NOT EOF(iu_edr) ) DO BEGIN ; while loop for EDR files starts

  readf, iu_edr, file_edr, format='(a)'
  print, file_edr
  
  ;---- read scene from EDR file ----
  nProfs2read = 1000000000L
  LoadSceneFile,file_edr,topId,Scene,nProfs2read
  
  ;---- loop profiles in scene structure ----
  iprf=0L
  while ( iprf lt Scene.nProfsProcessed ) do begin ; iprf loop starts
  
    lat  = Scene.lat(iprf)
    lon  = Scene.lon(iprf)
    cend = Scene.Direc(iprf)
    psfc = Scene.SfcPressVec(iprf)
    
    ;---- grid box boundary ----
    loncorr=abs(1/cos(PI*lat/180.0))
    if ( loncorr gt 200 ) then loncorr=200
    ifov = iprf MOD NFOV
    
    gridlon_left  = FIX((lon + 180.0) * gridfactor + adjustment)
    gridlon_right = FIX((lon + 180.0) * gridfactor + adjustment)
    gridlat_bot   = FIX((lat + 90.0 ) * gridfactor + adjustment)
    gridlat_top   = FIX((lat + 90.0 ) * gridfactor + adjustment)

    if satId eq Consts.STR_SATID_N18 or satId eq Consts.STR_SATID_METOPA or $
       satId eq Consts.STR_SATID_N19 or satId eq Consts.STR_SATID_METOPB or $
       satId eq Consts.STR_SATID_NPP then begin
         
      lonleft  = lon - 0.5 * fov_sizes(ifov) * loncorr
      lonright = lon + 0.5 * fov_sizes(ifov) * loncorr
      gridlon_left  = FIX( (lonleft  + 180.0) * gridfactor + adjustment )
      gridlon_right = FIX( (lonright + 180.0) * gridfactor + adjustment )
      
      if( fmType eq 0 ) then begin
        if ( abs(ifov-(NFOV-1.)/2.) lt (LATLIM_A - 0.4 )) then begin
          gridlat_bot = FIX((lat+90) * gridfactor)
          gridlat_top = gridlat_bot + 1 
	endif else begin
          gridlat_bot = FIX((lat+90.0) * gridfactor - 1 + adjustment)
          gridlat_top = FIX((lat+90.0) * gridfactor + 1 + adjustment)
	endelse
      endif else begin
          gridlat_bot = FIX((lat+90) * gridfactor) 
          gridlat_top = FIX((lat+90) * gridfactor) 
      endelse
    
    endif
    
    if satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F18 or $
          satId eq Consts.STR_SATID_F17 then begin

      lonleft  = lon - 0.5 * fov_size(ifov) * loncorr
      lonright = lon + 0.5 * fov_size(ifov) * loncorr
      gridlon_left  = FIX((lonleft  + 180.0) * gridfactor + adjustment)
      gridlon_right = FIX((lonright + 180.0) * gridfactor + adjustment)

      gridlat_bot   = FIX((lat+90.0) * gridfactor - 1 + adjustment)
      gridlat_top   = FIX((lat+90.0) * gridfactor + 1 + adjustment)

    endif
    
    if gridlon_left  lt 0    then gridlon_left  = 0
    if gridlon_right lt 0    then gridlon_right = 0
    if gridlon_left  ge NCOL then gridlon_left  = NCOL - 1
    if gridlon_right ge NCOL then gridlon_right = NCOL - 1

    if gridlat_bot   lt 0    then gridlat_bot   = 0
    if gridlat_top   lt 0    then gridlat_top   = 0
    if gridlat_top   ge NROW then gridlat_top   = NROW - 1
    if gridlat_bot   ge NROW then gridlat_bot   = NROW - 1

    near_nadir = FIX(abs(ifov - (NFOV-1.)/2.) + 0.6)
    
    ;---- begin section for lat/lon box ----
    for lonbox=gridlon_left, gridlon_right do begin
    for latbox=gridlat_bot,  gridlat_top   do begin
        
      ;---- asc ----
      nadirLogic_as = near_nadir le nadir_as(lonbox,latbox)
      if satId eq Consts.STR_SATID_AQUA or satId eq Consts.STR_SATID_FY3RI or $
         satId eq Consts.STR_SATID_TRMM or satId eq Consts.STR_SATID_GPM then $
        nadirLogic_as = 1B  

      if ( cend eq 0 and nadirLogic_as eq 1B ) then begin
	grid_sfc_as(lonbox,latbox) = Scene.SfcTypVec(iprf)
	qc_tmp(*) =  Scene.qc(iprf,*)
	if qc_tmp(0) eq 0 then  grid_qc_as(lonbox,latbox) = 0.0
	if qc_tmp(0) eq 2 then  grid_qc_as(lonbox,latbox) = 1.0
	if qc_tmp(0) ne 2 and qc_tmp(0) ge 0 then begin
	  if BTEST( qc_tmp(1), 3 ) eq 1 then grid_qc_as(lonbox,latbox) = 2.0 
	  if BTEST( qc_tmp(1), 4 ) eq 1 then grid_qc_as(lonbox,latbox) = 3.0
	  if BTEST( qc_tmp(1), 5 ) eq 1 then grid_qc_as(lonbox,latbox) = 4.0 
	  if BTEST( qc_tmp(2), 1 ) eq 1 then grid_qc_as(lonbox,latbox) = 5.0 
	  if BTEST( qc_tmp(2), 4 ) eq 1 then grid_qc_as(lonbox,latbox) = 6.0 
	  if BTEST( qc_tmp(2), 1 ) eq 1 and BTEST( qc_tmp(2), 4 ) eq 1 then $
             grid_qc_as(lonbox,latbox) = 7.0
	  if ( BTEST( qc_tmp(1), 2 ) eq 1 or BTEST( qc_tmp(2), 5 ) eq 1 ) and $
	       BTEST( qc_tmp(2), 2 ) eq 1 then grid_qc_as(lonbox,latbox) = 8.0
	  if ( BTEST( qc_tmp(1), 2 ) eq 0 and  BTEST( qc_tmp(2), 5 ) eq 0 ) and $
	       BTEST( qc_tmp(2), 2 ) eq 1 then grid_qc_as(lonbox,latbox) = 9.0
	endif
	if ( Scene.qc(iprf,0) le 2 ) then begin
	  nadir_as(lonbox,latbox) = near_nadir
	  grid_chisq_as(lonbox,latbox)=Scene.ChiSq(iprf)
	  grid_niter_as(lonbox,latbox)=Scene.nIter(iprf)
	  grid_nattempt_as(lonbox,latbox)=Scene.nAttempt(iprf)
	  grid_psfc_as(lonbox,latbox)=Scene.SfcPressVec(iprf)
	  grid_tskin_as(lonbox,latbox)=Scene.TskinVec(iprf)
	  for ilay = 0, NLAY-1 do begin
            if Scene.PresLayVec(iprf,layers_index(ilay)) lt psfc then begin
              grid_temp_as(lonbox,latbox,ilay) = Scene.TempLayVec(iprf,layers_index(ilay))
              grid_wv_as(lonbox,latbox,ilay)   = Scene.AbsorbLayVec(iprf,layers_index(ilay),0)
            endif
	  endfor
	  for ichan = 0, Scene.nchan-1 do begin
	    grid_em_as(lonbox,latbox,ichan)  = Scene.EmissVec(iprf,ichan)
	    grid_tbc_as(lonbox,latbox,ichan) = Scene.YmCorr(iprf,ichan)
	    grid_tbu_as(lonbox,latbox,ichan) = Scene.Ym(iprf,ichan)
	  endfor
	endif
    	;---- qc failed filled with -99.0 ----
	if (  Scene.qc(iprf,0) eq 2 ) then begin
	  grid_psfc_as(lonbox,latbox)     = -99.0
	  grid_sfc_as(lonbox,latbox)      = -99.0
	  grid_tskin_as(lonbox,latbox)    = -99.0
	  for ilay = 0, NLAY-1 do begin
            if Scene.PresLayVec(iprf,layers_index(ilay)) lt psfc then begin
              grid_temp_as(lonbox,latbox,ilay) = -99.0
              grid_wv_as(lonbox,latbox,ilay)   = -99.0
            endif
	  endfor
	  for ichan = 0, Scene.nchan-1 do begin
	    grid_em_as(lonbox,latbox,ichan)  = -99.0
	    grid_tbc_as(lonbox,latbox,ichan) = -99.0
	    grid_tbu_as(lonbox,latbox,ichan) = -99.0
	  endfor
    	endif
      endif
      ;---- end asc ----

      ;---- des ----
      nadirLogic_ds = near_nadir le nadir_ds(lonbox,latbox)
      if satId eq Consts.STR_SATID_AQUA or satId eq Consts.STR_SATID_FY3RI or $
         satId eq Consts.STR_SATID_TRMM or satId eq Consts.STR_SATID_GPM then $
        nadirLogic_ds = 1B  

      if ( cend eq 1 and nadirLogic_ds eq 1B ) then begin
	grid_sfc_ds(lonbox,latbox) = Scene.SfcTypVec(iprf)
	qc_tmp(*) =  Scene.qc(iprf,*)
	if qc_tmp(0) eq 0 then  grid_qc_ds(lonbox,latbox) = 0
	if qc_tmp(0) eq 2 then  grid_qc_ds(lonbox,latbox) = 1
	if qc_tmp(0) ne 2 and qc_tmp(0) ge 0 then begin
	  if BTEST( qc_tmp(1), 3 ) eq 1 then grid_qc_ds(lonbox,latbox) = 2 
	  if BTEST( qc_tmp(1), 4 ) eq 1 then grid_qc_ds(lonbox,latbox) = 3
	  if BTEST( qc_tmp(1), 5 ) eq 1 then grid_qc_ds(lonbox,latbox) = 4 
	  if BTEST( qc_tmp(2), 1 ) eq 1 then grid_qc_ds(lonbox,latbox) = 5 
	  if BTEST( qc_tmp(2), 4 ) eq 1 then grid_qc_ds(lonbox,latbox) = 6 
	  if BTEST( qc_tmp(2), 1 ) eq 1 and BTEST( qc_tmp(2), 4 ) eq 1 then $
             grid_qc_ds(lonbox,latbox) = 7
	  if ( BTEST( qc_tmp(1), 2 ) eq 1 or BTEST( qc_tmp(2), 5 ) eq 1 ) and $
	       BTEST( qc_tmp(2), 2 ) eq 1 then grid_qc_ds(lonbox,latbox) = 8
	  if ( BTEST( qc_tmp(1), 2 ) eq 0 and  BTEST( qc_tmp(2), 5 ) eq 0 ) and $
	       BTEST( qc_tmp(2), 2 ) eq 1 then grid_qc_ds(lonbox,latbox) = 9
	endif

    	if ( Scene.qc(iprf,0) le 2 ) then begin
	  nadir_ds(lonbox,latbox) = near_nadir
	  grid_chisq_ds(lonbox,latbox)=Scene.ChiSq(iprf)
	  grid_niter_ds(lonbox,latbox)=Scene.nIter(iprf)
	  grid_nattempt_ds(lonbox,latbox)=Scene.nAttempt(iprf)
	  grid_psfc_ds(lonbox,latbox)=Scene.SfcPressVec(iprf)
	  grid_tskin_ds(lonbox,latbox)=Scene.TskinVec(iprf)
	  for ilay = 0, NLAY-1 do begin
            if Scene.PresLayVec(iprf,layers_index(ilay)) lt psfc then begin
              grid_temp_ds(lonbox,latbox,ilay) = Scene.TempLayVec(iprf,layers_index(ilay))
              grid_wv_ds(lonbox,latbox,ilay)   = Scene.AbsorbLayVec(iprf,layers_index(ilay),0)
            endif
	  endfor
	  for ichan = 0, Scene.nchan-1 do begin
	    grid_em_ds(lonbox,latbox,ichan)  = Scene.EmissVec(iprf,ichan)
	    grid_tbc_ds(lonbox,latbox,ichan) = Scene.YmCorr(iprf,ichan)
	    grid_tbu_ds(lonbox,latbox,ichan) = Scene.Ym(iprf,ichan)
	  endfor
    	endif
    	;---- qc failed filled with -99.0 ----
	if ( Scene.qc(iprf,0) eq 2 ) then begin
	  grid_psfc_ds(lonbox,latbox)    = -99.0
	  grid_sfc_ds(lonbox,latbox)     = -99.0
	  grid_tskin_ds(lonbox,latbox)   = -99.0
	  for ilay = 0, NLAY-1 do begin
            if Scene.PresLayVec(iprf,layers_index(ilay)) lt psfc then begin
              grid_temp_ds(lonbox,latbox,ilay) = -99.0
              grid_wv_ds(lonbox,latbox,ilay)   = -99.0
            endif
	  endfor
	  for ichan = 0, Scene.nchan-1 do begin
	    grid_em_ds(lonbox,latbox,ichan)  = -99.0
	    grid_tbc_ds(lonbox,latbox,ichan) = -99.0
	    grid_tbu_ds(lonbox,latbox,ichan) = -99.0
	  endfor
    	endif
      endif
      ;---- end dec
   
    endfor 
    endfor 
    ;---- end section for lat/lon box ----
    
    iprf=iprf+1L

  endwhile ; iprf loop ends
  
ENDWHILE ; while loop for EDR files ends
	
close, iu_edr
free_lun, iu_edr  


;*******************************************************************************
; cends,sfcs tokens
;*******************************************************************************
cends = ['as','ds']
ncend = n_elements(cends)
cendTxts = [' Asc ', ' Des ']
sfcPicks = [0,1,2]  ; 0-sea, 1-land, 2-all
nsfc = n_elements(sfcPicks)
sfcTxts  = [ '_sea_', '_lnd_', '_all_' ]

;---- which plot_grid subroutine to call ----
;0:plot_grid, 1:plot_sfc_grid, 2:plot_gridQCF
sub2use = 0

prods_as = fltarr(NCOL,NROW)
prods_ds = fltarr(NCOL,NROW)
products = fltarr(NCOL,NROW)

;*******************************************************************************
; 1D products section
;*******************************************************************************
nprod_sfc = N_ELEMENTS(prodIds_sfc)

for iprod = 0, nprod_sfc - 1 do begin

  prodId = prodIds_sfc[iprod]
  print, 'plotting ' + prodId + '...'

  case  prodId of
  
  'angle':      begin
                        prodTitle = ' Scan Angle '
                        minvalue  = -60
                        maxvalue  = 60
                        div       = 12
                        fmt       = '(I3)'
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                        prods_as  = grid_angle_as
                        prods_ds  = grid_angle_ds
                end

  'chisq':      begin
                        prodTitle = ' Chi Square '
                        minvalue  = 0
                        maxvalue  = 10
                        div       = 10
                        fmt       = '(I2)'
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                        prods_as  = grid_chisq_as
                        prods_ds  = grid_chisq_ds
                end

  'clw':        begin
                        prodTitle = ' CLW (mm) '
                        minvalue  = -0.01
                        maxvalue  = 0.70
                        div       = 6
                        fmt       = '(f5.2)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                        prods_as  = grid_clw_as
                        prods_ds  = grid_clw_ds
                end

  'gs':         begin
                        prodTitle = ' Snow Grain Size Radius (mm) '
                        minvalue  = 0.3
                        maxvalue  = 0.7
                        div       = 4
                        fmt       = '(f4.1)'
                        nsfc_bot  = 1
                        nsfc_top  = 1
                        sub2use   = 0
                end

  'iwp':        begin
                        prodTitle = ' Graupel Water Path ( mm ) '
                        minvalue  = -0.005
                        maxvalue  = 0.20
                        div       = 4
                        fmt       = '(f6.3)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'lwp':        begin
                        prodTitle = ' Liquid Water Path ( mm ) '
                        minvalue  = -0.01
                        maxvalue  = 0.7
                        div       = 7
                        fmt       = '(f5.2)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'nattempt':   begin
                        prodTitle = ' Number of Attempts '
                        minvalue  = 0
                        maxvalue  = 2
                        div       = 2
                        fmt       = '(I2)'
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                        prods_as  = grid_nattempt_as
                        prods_ds  = grid_nattempt_ds
                end

  'niter':      begin
                        prodTitle = ' Number of Iterations '
                        minvalue  = 0
                        maxvalue  = 7
                        div       = 7
                        fmt       = '(I2)'
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                        prods_as  = grid_niter_as
                        prods_ds  = grid_niter_ds
                end

  'psfc':       begin
                        prodTitle = ' Surface Pressure (mb) '
                        minvalue  = 700
                        maxvalue  = 1050
                        div       = 7
                        fmt       = '(I4)'
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                        prods_as  = grid_psfc_as
                        prods_ds  = grid_psfc_ds
                end

  'qc':         begin
  
                        prodTitle = ' Geophysical Events '
                        minvalue  = 0.0
                        maxvalue  = 9.1
                        div       = 9
                        fmt       = '(I2)'
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 2
                        prods_as  = grid_qc_as
                        prods_ds  = grid_qc_ds
                end

  'rr':         begin
  
                        prodTitle = ' Rain Rate ( mm/hr ) '
                        minvalue  = -1.0
                        maxvalue  = 10
                        div       = 5
                        fmt       = '(f5.2)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'rwp':        begin
                        prodTitle = ' Rain Water Path ( mm ) '
                        minvalue  = -0.01
                        maxvalue  = 0.50
                        div       = 5
                        fmt       = '(f5.2)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'scanday':    begin
                        prodTitle = ' Scan Julian Day '
                        div       = 4
                        fmt       = '(I2)'
                        nsfc_bot  = 0
                        nsfc_top  = 2

                        ;----  scanday ( min and max values are changing )
                        yyyy=strmid(yyyymmdd,0,4)
                        mm=strmid(yyyymmdd,4,2)
                        dd=strmid(yyyymmdd,6,2)
                        year=fix(yyyy)
                        month=fix(mm)
                        day=fix(dd)
                        jday=JULDAY(month,day,year)
                        jday_year_begin=JULDAY(1,1,year)

                        jday_min = jday - jday_year_begin
                        jday_max = jday - jday_year_begin + 2

                        minvalue = jday_min + 0.5
                        maxvalue = jday_max + 0.5
                        sub2use   = 0
                end

  'scanpos':    begin
                        prodTitle = ' Scan Position '
                        minvalue  = 0
                        maxvalue  = 30
                        div       = 6
                        fmt       = '(I3)'
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'sfc':        begin
                        prodTitle = ' Pre-Classified Sfc Type '
                        minvalue  = 0
                        maxvalue  = 3
                        div       = 3
                        fmt       = '(I2)'
                        nsfc_bot  = 2
                        nsfc_top  = 2
                        sub2use   = 1
                        prods_as  = grid_sfc_as
                        prods_ds  = grid_sfc_ds
                end

  'sfc2':       begin
                        prodTitle = ' Post-Processed Sfc Type '
                        minvalue  = 0
                        maxvalue  = 3
                        div       = 3
                        fmt       = '(I2)'
                        nsfc_bot  = 2
                        nsfc_top  = 2
                        sub2use   = 1
                end

  'sice':       begin
                        prodTitle = ' Sea Ice Concentration (%) '
                        minvalue  = 0
                        maxvalue  = 100
                        div       = 10
                        fmt       = '(I3)'
                        nsfc_bot  = 0
                        nsfc_top  = 0
                        sub2use   = 0
                end

  'sicefy':     begin
                        prodTitle = ' First Year Sea Ice Concentration (%) '
                        minvalue  = 0
                        maxvalue  = 100
                        div       = 10
                        fmt       = '(I3)'
                        nsfc_bot  = 0
                        nsfc_top  = 0
                        sub2use   = 0
                end

  'sicemy':     begin
                        prodTitle = ' Multiple Year Sea Ice Concentration (%) '
                        minvalue  = 0
                        maxvalue  = 100
                        div       = 10
                        fmt       = '(I3)'
                        nsfc_bot  = 0
                        nsfc_top  = 0
                        sub2use   = 0
                end

  'snow':       begin
                        prodTitle = ' Snow Cover '
                        minvalue  = 0
                        maxvalue  = 3
                        div       = 3
                        fmt       = '(I2)'
                        nsfc_bot  = 1
                        nsfc_top  = 1
                        sub2use   = 1
                end

  'swe':        begin
                        prodTitle = ' Snow Water Equivalent (cm) '
                        minvalue  = 0
                        maxvalue  = 8
                        div       = 8
                        fmt       = '(I2)'
                        nsfc_bot  = 1
                        nsfc_top  = 1
                        sub2use   = 0
                end

  'tpw':        begin
                        prodTitle = ' TPW (mm) '
                        minvalue  = -5
                        maxvalue  = 70
                        div       = 15
                        fmt       = '(I3)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'tskin':      begin
                        prodId    = 'tskin'
                        prodTitle = ' Skin Temperature (K) '
                        minvalue  = 200
                        maxvalue  = 325
                        div       = 5
                        fmt       = '(I3)'
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                        prods_as  = grid_tskin_as
                        prods_ds  = grid_tskin_ds
                end
  
  else:         begin
                        print, 'Unsupported 1D prodId: '+ prodId
                end
  endcase
  
  for icend = 0, ncend-1 do begin ; loop icend starts
    cend = cends(icend)
    cendTxt = cendTxts(icend)
    
    if icend eq 0 then products = prods_as else products = prods_ds
    
    for isfc = nsfc_bot, nsfc_top do begin
      sfcPick = sfcPicks(isfc)
      sfcTxt  = sfcTxts(isfc)
      map_name = dirImg + prefix + yyyymmdd + '_' + prodId + sfcTxt + cend + '.png'
      if prodId eq 'gs'   or prodId eq 'sfc' or prodId eq 'sfc2' or $
         prodId eq 'snow' or prodId eq 'swe' then $
        map_name = dirImg + prefix + yyyymmdd + '_' + prodId + '_cyl' + sfcTxt + cend + '.png'
      title = titleSatId + prodTitle + date +  cendTxt + ' (V' + version +')'
      
      if sub2use eq 0 then $
        plot_grid,products,sfcMask,map_name,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,$
                  title,sfcPick,div,fmt,color_table_index=ctable
      if sub2use eq 1 then $
        plot_sfc_grid,products,map_name,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,title
      if sub2use eq 2 then $
        plot_gridQCF,products,sfcMask,map_name,minvalue,maxvalue,$
	             latmin,latmax,lonmin,lonmax,title,sfcPick,div,fmt
    endfor
    
  endfor ; loop icend ends

endfor

UNDEFINE, prods_as
UNDEFINE, prods_ds
UNDEFINE, products


;*******************************************************************************
; layer products section
;*******************************************************************************

titles=['100mb','200mb','300mb','400mb','500mb','600mb', '700mb','800mb','850mb','900mb','950mb']

nprod_lay = N_ELEMENTS(prodIds_lay)

prods_as = fltarr(NCOL,NROW,NLAY)
prods_ds = fltarr(NCOL,NROW,NLAY)
products = fltarr(NCOL,NROW,NLAY)

for iprod = 0, nprod_lay - 1 do begin

  prodId = prodIds_lay[iprod]
  print, 'plotting ' + prodId + ' ...'
  
  case  prodId of
  
  'temp':       begin
                  prodTitle = ' Temperature (K) '
                  minvalues = [200,200,210,220,230,240,250,250,255,255,255]
                  maxvalues = [230,230,260,270,280,285,290,290,300,300,305]
                  divs      = [6,6,10,10,10,9,8,8,9,9,10]
                  fmt       = '(I3)'
                  prods_as  = grid_temp_as
                  prods_ds  = grid_temp_ds
                  ctable    = 33
                end

  'wv':         begin
                  prodTitle = ' Water Vapor Content (g/kg) '
                  minvalues = replicate(0.0, NLAY)
                  maxvalues = [0.1,0.15,1.0,2.0,3.5,6.0,12.0,12.0,14.0,14.0,16.0]
                  divs      = [10,10,10,10,7,6,6,6,7,7,8]
                  fmt       = '(f5.2)'
                  prods_as  = grid_wv_as
                  prods_ds  = grid_wv_ds
                  ctable    = 33
                end

  'clwp':       begin
                  prodTitle = ' CLW (mm) '
                  minvalues = replicate(-0.005, NLAY)
                  maxvalues = [0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15]
                  divs      = replicate(5, NLAY)
                  fmt       = '(f6.3)'
                  ctable    = 41
                end

  'rainp':      begin
                  prodTitle = ' Rain (mm) '
                  minvalues = replicate(-0.005, NLAY)
                  maxvalues = replicate(0.10,   NLAY)
                  divs      = replicate(5,      NLAY)
                  fmt       = '(f6.3)'
                  ctable    = 41
                end

  'graupelp':   begin
                  prodTitle =' Graupel (mm) '
                  minvalues = replicate(-0.005, NLAY)
                  maxvalues = replicate(0.05,   NLAY)
                  divs      = replicate(5,      NLAY)
                  fmt       = '(f6.3)'
                  ctable    = 41
                end

  else:         begin
                  print, 'Unsupported layer prodId: '+ prodId
                end

  endcase

  for icend = 0, ncend - 1 do begin ; loop icend starts

    cend = cends(icend)
    cendTxt = cendTxts(icend)

    if icend eq 0 then products = prods_as else products = prods_ds

    ;---- plot profile layers
    for ilay = 0, NLAY - 1 do begin

      sb=fltarr(NCOL,NROW)
      layer = 'at ' + titles[ilay]

      for isfc = 0, nsfc-1 do begin
        sfcPick = sfcPicks(isfc)
        sfcTxt = sfcTxts(isfc)
        map_name = dirImg+prefix+yyyymmdd+'_'+prodId +'_'+titles[ilay]+sfcTxt+cend+'.png'
        title = titleSatId+prodTitle+layer+' '+date+cendTxt+' (V'+version +')'
        sb = products(*,*,ilay)
        plot_grid,sb,sfcMask,map_name,minvalues[ilay],maxvalues[ilay],latmin,latmax,$
                  lonmin,lonmax,title,sfcPick,divs[ilay],fmt,color_table_index=ctable
      endfor

    endfor

  endfor ; loop icend ends

endfor

UNDEFINE, prods_as
UNDEFINE, prods_ds
UNDEFINE, products


;*******************************************************************************
; channel products section
;*******************************************************************************

if satId eq Consts.STR_SATID_N18 or satId eq Consts.STR_SATID_METOPA or $
   satId eq Consts.STR_SATID_N19 or satId eq Consts.STR_SATID_METOPB then begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10',$
               '11','12','13','14','15','16','17','18','19','20']
  chan_ids  = ['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2',$
               '57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']
endif                    

if satId eq Consts.STR_SATID_F16 then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10','11','12',$
               '13','14','15','16','17','18','19','20','21','22','23','24']
  chan_ids  = ['50v', '52v', '53v', '54v', '55v', '57rc', '59rc', '150h', $
               '190h', '186h','184h', '19h', '19v', '22v', '37h', '37v',  $
               '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
endif

if satId eq Consts.STR_SATID_F17 then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10','11','12',$
               '13','14','15','16','17','18','19','20','21','22','23','24']
  chan_ids  = ['50v', '52v', '53v', '54v', '55v', '57rc', '59rc', '150h', $
               '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', $
               '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
endif

if satId eq Consts.STR_SATID_F18 then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10','11','12',$
               '13','14','15','16','17','18','19','20','21','22','23','24']
  chan_ids  = ['50v', '52v', '53v', '54v', '55v', '57rc', '59rc', '150h', $
               '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', $
               '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
endif

if satId eq Consts.STR_SATID_AQUA then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10','11','12']
  chan_ids  = ['7v','7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h']
endif

if satId eq Consts.STR_SATID_FY3RI then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10']
  chan_ids  = ['11v','11h','19v','19h','24v','24h','37v','37h','89v','89h']	   
endif

if satId eq Consts.STR_SATID_NPP then begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10','11',$
               '12','13','14','15','16','17','18','19','20','21','22']
  chan_ids  = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2',$
               '57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
endif  

if satId eq Consts.STR_SATID_TRMM then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9']
  chan_ids  = ['11v','11h','19v','19h','21v','37v','37h','85v','85h' ]
endif

if satId eq Consts.STR_SATID_GPM then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10','11','12','13']
  chan_ids  = ['11v','11h','19v','19h','24v','37v','37h','89v','89h','166v','166h','183v1','183v2']
endif

nprod_chan = N_ELEMENTS( prodIds_chan )

prods_as = fltarr(NCOL,NROW,NCHAN)
prods_ds = fltarr(NCOL,NROW,NCHAN)
products = fltarr(NCOL,NROW,NCHAN)

for iprod = 0, nprod_chan - 1 do begin

  prodId = prodIds_chan[iprod]
  print, 'plotting ' + prodId + ' ...'

  if prodId eq 'em' then begin
    prodTitle = ' Emissivity '
    fmt       = '(f4.2)' 
    prods_as  = grid_em_as
    prods_ds  = grid_em_ds
  endif

  if prodId eq 'tbc' then begin
    prodTitle = ' Corr. Measured TB (K) '
    fmt       = '(I3)' 
    prods_as  = grid_tbc_as
    prods_ds  = grid_tbc_ds
  endif

  if prodId eq 'tbu' then begin
    prodTitle = ' UnCorr. Measured TB (K) '
    fmt       = '(I3)' 
    prods_as  = grid_tbu_as
    prods_ds  = grid_tbu_ds
  endif

  if prodId eq 'em' then begin

    if satId eq Consts.STR_SATID_N18 or satId eq Consts.STR_SATID_METOPA or $
       satId eq Consts.STR_SATID_N19 or satId eq Consts.STR_SATID_METOPB then begin

      mins_sea  = [0.45,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                   0.50,0.50,0.50,0.50,0.60,0.60,0.60,0.65,0.65,0.65]
      maxs_sea  = [0.55,0.60,0.70,0.70,0.65,0.65,0.70,0.65,0.65,0.65,$
                   0.65,0.65,0.65,0.65,0.75,0.75,0.80,0.75,0.80,0.85]
      mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
                   0.65,0.65,0.65,0.65,0.60,0.60,0.60,0.65,0.65,0.65]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
                   1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      mins_all  = [0.45,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                   0.50,0.50,0.50,0.50,0.60,0.60,0.60,0.65,0.65,0.65]
      maxs_all  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
                   1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      divs      = replicate(10,20)
    endif                    

    if satId eq Consts.STR_SATID_F16 then  begin
      mins_sea  = [0.65, 0.65, 0.65, 0.50, 0.70, 0.60, 0.50, 0.60, $
                   0.60, 0.50, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, $
                   0.70, 0.70, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_sea  = [1.00, 1.00, 1.00, 0.65, 0.80, 0.80, 0.85, 0.80, $
                   0.85, 0.80, 0.80, 0.85, 0.85, 0.85, 0.85, 0.85, $
                   0.80, 0.80, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70]
      mins_lnd  = [0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.80, 0.90, $
                   0.90, 0.70, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, $
                   0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_lnd  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                   0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70]
      mins_all  = [0.50, 0.50, 0.50, 0.50, 0.65, 0.65, 0.50, 0.60, $
                   0.65, 0.60, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, $
                   0.65, 0.65, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50]
      maxs_all  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                   0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70]
      divs      = replicate(10,24)
    endif

    if satId eq Consts.STR_SATID_F17 then  begin
      mins_sea  = [0.40, 0.40, 0.40, 0.40, 0.40, 0.60, 0.50, 0.60, $
        	   0.60, 0.50, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, $
        	   0.70, 0.70, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_sea  = [0.90, 0.90, 0.90, 0.90, 0.90, 0.80, 0.85, 0.80, $
        	   0.85, 0.80, 0.80, 0.85, 0.85, 0.85, 0.85, 0.85, $
        	   0.80, 0.80, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70]
      mins_lnd  = [0.60, 0.60, 0.60, 0.60, 0.60, 0.65, 0.80, 0.90, $
        	   0.90, 0.70, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, $
        	   0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_lnd  = [0.90, 0.90, 0.90, 0.90, 0.90, 1.00, 1.00, 1.00, $
        	   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
        	   0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70]
      mins_all  = [0.40, 0.40, 0.40, 0.40, 0.40, 0.65, 0.50, 0.60, $
        	   0.65, 0.60, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, $
        	   0.65, 0.65, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50]
      maxs_all  = [0.90, 0.90, 0.90, 0.90, 0.90, 1.00, 1.00, 1.00, $
        	   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
        	   0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70]
      divs      = replicate(10,24)
    endif

    if satId eq Consts.STR_SATID_F18 then  begin
      mins_sea  = [0.40, 0.40, 0.40, 0.40, 0.40, 0.60, 0.50, 0.60, $
        	   0.60, 0.50, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, $
        	   0.70, 0.70, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_sea  = [0.90, 0.90, 0.90, 0.90, 0.90, 0.80, 0.85, 0.80, $
        	   0.85, 0.80, 0.80, 0.85, 0.85, 0.85, 0.85, 0.85, $
        	   0.80, 0.80, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70]
      mins_lnd  = [0.60, 0.60, 0.60, 0.60, 0.60, 0.65, 0.80, 0.90, $
        	   0.90, 0.70, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, $
        	   0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_lnd  = [0.90, 0.90, 0.90, 0.90, 0.90, 1.00, 1.00, 1.00, $
        	   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
        	   0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70]
      mins_all  = [0.40, 0.40, 0.40, 0.40, 0.40, 0.65, 0.50, 0.60, $
        	   0.65, 0.60, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, $
        	   0.65, 0.65, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50]
      maxs_all  = [0.90, 0.90, 0.90, 0.90, 0.90, 1.00, 1.00, 1.00, $
        	   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
        	   0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70, 0.70]
      divs      = replicate(10,24)
    endif

    if satId eq Consts.STR_SATID_AQUA then  begin
      mins_sea  = [0.65,0.65,0.65,0.50,0.70,0.60,0.50,0.60,0.60,0.50,0.65,0.65]
      maxs_sea  = [1.00,1.00,1.00,0.65,0.80,0.80,0.85,0.80,0.85,0.80,0.80,0.85]
      mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.80,0.90,0.90,0.70,0.75,0.75]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      mins_all  = [0.50,0.50,0.50,0.50,0.65,0.65,0.50,0.60,0.65,0.60,0.65,0.65]
      maxs_all  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      divs      = replicate(10,12)
    endif

    if satId eq Consts.STR_SATID_FY3RI then  begin
      mins_sea  = [0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35]
      maxs_sea  = [0.70, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.80, 0.80]
      mins_lnd  = [0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65]
      maxs_lnd  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
      mins_all  = [0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35]
      maxs_all  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
      divs      = [  4,   5,    4,    5,     4,    5,    4,    5,    4,    5 ] 
    endif

    if satId eq Consts.STR_SATID_NPP then begin
      mins_sea  = [0.45,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                   0.50,0.50,0.50,0.50,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
      maxs_sea  = [0.55,0.60,0.70,0.70,0.65,0.65,0.65,0.70,0.65,0.65,0.65,$
                   0.65,0.65,0.65,0.65,0.75,0.80,0.75,0.80,0.85,0.85,0.85]
      mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
                   0.65,0.65,0.65,0.65,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
                   1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      mins_all  = [0.45,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                   0.50,0.50,0.50,0.50,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
      maxs_all  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
                   1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      divs      = replicate(10,22)
    endif  

    if satId eq Consts.STR_SATID_TRMM then  begin
      mins_sea  = [0.65, 0.50, 0.70, 0.60, 0.50, 0.60, 0.50, 0.65, 0.65]
      maxs_sea  = [1.00, 0.65, 0.80, 0.80, 0.85, 0.85, 0.80, 0.80, 0.85]
      mins_lnd  = [0.65, 0.65, 0.65, 0.65, 0.80, 0.90, 0.70, 0.75, 0.75]
      maxs_lnd  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
      mins_all  = [0.50, 0.50, 0.65, 0.65, 0.50, 0.65, 0.60, 0.65, 0.65]
      maxs_all  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
      divs      = replicate(10,9)
    endif

    if satId eq Consts.STR_SATID_GPM then  begin
      mins_sea  = [0.55,0.25,0.58,0.25,0.60,0.65,0.25,0.75,0.45,0.45,0.45,0.45,0.45]
      maxs_sea  = [0.63,0.35,0.68,0.45,0.70,0.75,0.50,0.90,0.75,0.75,0.75,0.75,0.75]
      mins_lnd  = [0.70,0.65,0.70,0.65,0.80,0.90,0.70,0.75,0.75,0.75,0.75,0.75,0.75]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      mins_all  = [0.40,0.60,0.40,0.60,0.40,0.40,0.60,0.40,0.60,0.40,0.60,0.65,0.65]
      maxs_all  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
      divs      = replicate(10,13)
    endif

  endif 

  if prodId eq 'tbc' or prodId eq 'tbu' then begin

    if satId eq Consts.STR_SATID_N18 or satId eq Consts.STR_SATID_METOPA or $
       satId eq Consts.STR_SATID_N19 or satId eq Consts.STR_SATID_METOPB then begin
      mins_all = [140,140,170,170,200, 200,210,200,200,180, $
                  180,180,180,200,150, 150,150,220,220,200]
      maxs_all = [300,300,290,290,270, 250,230,230,230,230, $
                  250,250,280,290,300, 300,300,265,280,300]
      divs     = [8,8,6,6,7, 5,5,6,6,5, 7,10,7,9,5, 5,5,9,6,5]
    endif                    

    if satId eq Consts.STR_SATID_F16 then  begin
      mins_all = [ 200,  210,  215,  210,  200,  200,  210,  150, $
                   200,  200,  225,  100,  140,  140,  100,  140, $
                  140,  100,  205,   200,  230,  225,  220,  215]
      maxs_all = [ 270,  270,  250,  230,  225,  225,  225,  300, $
                   290,  275,  250,  300,  300,  300,  300,  300, $
                   300,  300,  240,  230,  260,  260,  250,  230]
      divs     = [  7,    6,    7,    4,    5,    5,    5,    5,  $
                    9,    5,    5,    10,   8,    8,    10,   8,  $
                    8,    10,   7,    6,    6,    7,    6,    5]  
    endif

    if satId eq Consts.STR_SATID_F17 then  begin
      mins_all = [ 200,  210,  215,  210,  200,  200,  210,  150, $
                   200,  200,  225,  100,  140,  140,  100,  140, $
                   140,  100,  205,  200,  230,  225,  220,  215]
      maxs_all = [ 270,  270,  250,  230,  225,  225,  225,  300, $
                   290,  275,  250,  300,  300,  300,  300,  300, $
                   300,  300,  240,  230,  260,  260,  250,  230]
      divs     = [  7,    6,    7,    4,    5,    5,    5,    5,  $
                    9,    5,    5,    10,   8,    8,    10,   8,  $
                    8,    10,   7,    6,    6,    7,    6,    5]  
    endif

    if satId eq Consts.STR_SATID_F18 then  begin
      mins_all = [ 200,  210,  215,  210,  200,  200,  210,  150, $
                   200,  200,  225,  100,  140,  140,  100,  140, $
                   140,  100,  205,  200,  230,  225,  220,  215]
      maxs_all = [ 270,  270,  250,  230,  225,  225,  225,  300, $
                   290,  275,  250,  300,  300,  300,  300,  300, $
                   300,  300,  240,  230,  260,  260,  250,  230]
      divs     = [  7,    6,    7,    4,    5,    5,    5,    5,  $
                    9,    5,    5,    10,   8,    8,    10,   8,  $
                    8,    10,   7,    6,    6,    7,    6,    5]  
    endif

    if satId eq Consts.STR_SATID_AQUA then  begin
      mins_all = [  75, 50,125, 50,150, 75,175, 75,150,100,175,125 ]
      maxs_all = [ 315,300,300,300,315,315,315,300,315,315,315,300 ]
      divs     = [  6,  5,  7,  5,  11, 6,  7,  9,  11, 5,  7,  7  ]
    endif

    if satId eq Consts.STR_SATID_FY3RI then  begin
      mins_all = [ 125, 50,150, 75,175, 75,150,100,175,125 ]
      maxs_all = [ 300,300,315,315,315,300,315,315,315,300 ]
      divs     = [  7,  5,  11, 6,  7,  9,  11, 5,  7,  7  ]
    endif

    if satId eq Consts.STR_SATID_NPP then begin
      mins_all = [140,140,170,170,170,200, 200,210,200,200,180, $
                  180,180,180,200,150,150, 220,220,220,200,200]
      maxs_all = [300,300,290,290,290,270, 250,230,230,230,230, $
                  250,250,280,290,300,300, 300,300,300,300,300]
      divs     = [8,8,6,6,7, 5,5,6,6,5, 7,10,7,9,5, 5,5,9,6,5,6,6]
    endif  

    if satId eq Consts.STR_SATID_TRMM then  begin
      mins_all = [ 125,   50,  150,   75,  175,  150,  100,  175,  125 ]
      maxs_all = [ 300,  300,  315,  315,  315,  315,  315,  315,  300 ]
      divs     = [  7,    5,   11,    6,    7,    11,   5,    7,    7  ]
    endif

    if satId eq Consts.STR_SATID_GPM then  begin
      mins_all = [ 125,	 50, 150,  75, 175, 150, 100, 175, 125, 250, 265, 240, 240 ]
      maxs_all = [ 300,	300, 315, 315, 315, 315, 315, 315, 300, 300, 295, 280, 280 ]
      divs     = [ 7,	 5,   11,  6,   7,   11,  5,   7,   7,   5,   6,   8,   8  ]
    endif
    
    ;---- lnd and sea the same ranges as all
    mins_lnd = mins_all
    maxs_lnd = maxs_all
    mins_sea = mins_all
    maxs_sea = maxs_all
    
  endif 
 
  for icend = 0, ncend-1 do begin ; loop icend starts

    cend = cends(icend)
    cendTxt = cendTxts(icend)

    if icend eq 0 then products = prods_as else products = prods_ds
    
    ;---- plot profile channels
    for ichan = 0, NCHAN - 1 do begin

      sb = fltarr(NCOL,NROW)
      channel = 'at chan ' + chan_nums[ichan] + ' (' + chan_ids[ichan] + ')'
      
      for isfc = 0, nsfc-1 do begin
        sfcPick = sfcPicks(isfc)
        sfcTxt = sfcTxts(isfc)
	if isfc eq 0 then begin
	  minvalues = mins_sea
	  maxvalues = maxs_sea
	endif
	if isfc eq 1 then begin
	  minvalues = mins_lnd
	  maxvalues = maxs_lnd
	endif
	if isfc eq 2 then begin
	  minvalues = mins_all
	  maxvalues = maxs_all
	endif
        map_name = dirImg+prefix+yyyymmdd+'_'+prodId+'_'+chan_ids[ichan]+sfcTxt+cend+'.png'
        title = titleSatId+prodTitle+channel+' '+date+cendTxt+' (V'+version +')'
        sb = products(*,*,ichan)
        plot_grid,sb,sfcMask,map_name,minvalues[ichan],maxvalues[ichan],latmin,latmax,$
	          lonmin,lonmax,title,sfcPick,divs[ichan],fmt
      endfor

    endfor

  endfor ; loop icend ends

endfor

UNDEFINE, prods_as
UNDEFINE, prods_ds
UNDEFINE, products


;*******************************************************************************
; polar plots section
;*******************************************************************************

nprod_polar = N_ELEMENTS( prodIds_polar )

prods_as = fltarr(NCOL,NROW)
prods_ds = fltarr(NCOL,NROW)
products = fltarr(NCOL,NROW)

for iprod = 0, nprod_polar - 1 do begin

  prodId = prodIds_polar[iprod]
  print, 'plotting ' + prodId + ' polar ...'

  case prodId of
 
  'gs':         begin
                  prodTitle = ' Snow Grain Size Radius (mm) ' 
                  minvalue  = 0.3
                  maxvalue  = 0.7
                  div       = 4
                  fmt       = '(f4.1)'
                  nsfc_bot  = 1
                  nsfc_top  = 1
                end

  'sfc':        begin
                  prodTitle = ' Pre-Classified Sfc Type '
                  minvalue  = 0
                  maxvalue  = 3
                  div	    = 3
                  fmt	    = '(I2)'
                  symsize   = 0.8
                  nsfc_bot  = 2
                  nsfc_top  = 2
                  prods_as  = grid_sfc_as
                  prods_ds  = grid_sfc_ds
                end

  'sfc2':       begin
                  prodTitle = ' Post-Processed Sfc Type '
                  minvalue  = 0
                  maxvalue  = 3
                  div	    = 3
                  fmt	    = '(I2)'
                  symsize   = 0.8
                  nsfc_bot  = 2
                  nsfc_top  = 2
                end

  'sice':       begin
                  prodTitle = ' Sea Ice Concentration (%) ' 
                  minvalue  = 0
                  maxvalue  = 100
                  div	    = 10
                  fmt	    = '(I3)'
                  nsfc_bot  = 0
                  nsfc_top  = 0
                end

  'sicefy':     begin
                  prodTitle = ' First Year Sea Ice Concentration (%) ' 
                  minvalue  = 0
                  maxvalue  = 100
                  div	    = 10
                  fmt	    = '(I3)'
                  nsfc_bot  = 0
                  nsfc_top  = 0
                end

  'sicemy':     begin
                  prodTitle = ' Multiple Year Sea Ice Concentration (%) ' 
                  minvalue  = 0
                  maxvalue  = 100
                  div	    = 10
                  fmt	    = '(I3)'
                  nsfc_bot  = 0
                  nsfc_top  = 0
                end

  'snow':       begin
                  prodTitle = ' Snow & Ice ' 
                  minvalue  = 0
                  maxvalue  = 8
                  div       = 8
                  fmt       = '(I2)'
                  nsfc_bot  = 1
                  nsfc_top  = 1
                end

  'swe':        begin
                  prodTitle = ' Snow Water Equivalent (cm) ' 
                  minvalue  = 0
                  maxvalue  = 8
                  div	    = 8
                  fmt	    = '(I2)'
                  nsfc_bot  = 1
                  nsfc_top  = 1
                end

  else:         begin
                  print, 'Unsupported polar prodId: '+ prodId
                end
	
  endcase
  
  lats=fltarr(NCOL,NROW)
  for ilat=0,NROW-1 do begin
    lats(*,ilat)=-89.75 + (1.0/gridfactor)*ilat
  endfor
  lons=fltarr(NCOL,NROW)
  for ilon=0,NCOL-1 do begin
    lons(ilon,*)=-179.75 + (1.0/gridfactor)*ilon
  endfor

  for icend = 0, ncend-1 do begin ; loop icend starts
  
    cend = cends(icend)
    cendTxt = cendTxts(icend)
    
    if icend eq 0 then begin
      products = prods_as
      sfcs = grid_sfc_as
    endif else begin
      products = prods_ds
      sfcs = grid_sfc_ds
    endelse
      
    for isfc = nsfc_bot, nsfc_top do begin
      sfcPick=sfcPicks(isfc)
      sfcTxt=sfcTxts(isfc)

      ;---- Northern Hemisphere
      centrlat  = 90.0
      orientlon = -80.0
      latmin_ps = 0.0
      latmax_ps = 90.0
      ind = where( lats ge latmin_ps and lats le latmax_ps )
      map_name = dirImg + prefix + yyyymmdd + '_' + prodId + '_pn' + sfcTxt + cend + '.png'
      title = titleSatId + ' N. H.' + prodTitle + date +  cendTxt + ' (V' + version +')'
      if prodId eq 'sfc' or prodId eq 'sfc2' then begin
         plot_sfc_polar,products(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                        lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,symsize
      endif else begin
         plot_polar,product(ind),sfcs(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                   lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,sfcPick,div,fmt
      endelse

      ;---- Southern Hemisphere
      centrlat  = -90.0
      orientlon = 0.0
      latmin_ps = -90.0
      latmax_ps = 0.0
      ind = where( lats ge latmin_ps and lats le latmax_ps )
      map_name = dirImg + prefix + yyyymmdd + '_' + prodId + '_ps' + sfcTxt + cend + '.png'
      title = titleSatId + ' S. H.' + prodTitle + date +  cendTxt + ' (V' + version +')'
      if prodId eq 'sfc' or prodId eq 'sfc2' then begin
        plot_sfc_polar,products(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                       lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,symsize
      endif else begin
        plot_polar,product(ind),sfcs(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                   lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,sfcPick,div,fmt
      endelse

    endfor

  endfor ; loop icend ends

endfor

UNDEFINE, prods_as
UNDEFINE, prods_ds
UNDEFINE, products

seconds_finish = SYSTIME( /SECONDS )
print, 'Time used = ', seconds_finish-seconds_start, ' seconds'

End
