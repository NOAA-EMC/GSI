@../../../setup/paths_idl.pro
;***********************************************************************************************
;
;  Purpose:
;    To grid and plot DEP products.
;
;  Dependence:
;    ReadDEP
;    plot_grid
;    plot_sfc_grid
;
;  Record of revisions:
;       Date            Programmer     	Description
;     ----------      --------------    --------------
;     01/27/2011       Wanchun Chen     Original Coder 
;
;***********************************************************************************************
Pro gridDep, namelist=namelist
seconds_start = SYSTIME( /SECONDS )
Consts, Consts

;---- identifiers for namelist ----
satId='trmm'
gridfactor=2
dirImg='./'
date='2011-01-18'
processMode=1
version='2345'
latmin=-90
latmax=90
lonmin=-180
lonmax=180
dep_file_list='/home/pub/wchen/mirs_trmm/data/InputsData/trmm_depFiles_2011-01-18.list'
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
  readf,iu,format='(a)', dep_file_list	       	;DEP file list
  readf,iu,format='(a)', resolutionStr	       	;extention of resolution
  close,iu
  free_lun,iu,/force
endif

print, 'gridDep...'

resolution = '_' + STRLOWCASE(resolutionStr) + '_'

;---- default to N18/N19/Metop-A ----
prefix='mirs_adv_poes_'+satId+'_amsuamhs' + resolution
if satId eq Consts.STR_SATID_F16   then prefix='mirs_adv_dmsp_'  +satId+'_ssmis' + resolution
if satId eq Consts.STR_SATID_F18   then prefix='mirs_adv_dmsp_'  +satId+'_ssmis' + resolution
if satId eq Consts.STR_SATID_NPP   then prefix='mirs_adv_npoess_'+satId+'_atms'  + resolution
if satId eq Consts.STR_SATID_TRMM  then prefix='mirs_adv_eos_'   +satId+'_tmi'   + resolution
if satId eq Consts.STR_SATID_GPM   then prefix='mirs_adv_eos_'   +satId+'_gmi'   + resolution
if satId eq Consts.STR_SATID_AQUA  then prefix='mirs_adv_eos_'   +satId+'_amsre' + resolution
if satId eq Consts.STR_SATID_FY3RI then prefix='mirs_adv_eos_'   +satId+'_mwri'  + resolution

titleSatId='MIRS ' + STRUPCASE(satId)
if satId eq Consts.STR_SATID_AQUA  then titleSatId = 'MIRS ' + 'AMSR-E'
if satId eq Consts.STR_SATID_FY3RI then titleSatId = 'MIRS ' + 'FY3/MWRI'
if satId eq Consts.STR_SATID_TRMM  then titleSatId = 'MIRS ' + 'TRMM/TMI'
if satId eq Consts.STR_SATID_GPM   then titleSatId = 'MIRS ' + 'GPM/GMI'
if satId eq 'trmm_2a12' 	   then titleSatId = strupcase(satId)

if processMode eq 0 then yyyymmdd=date
if processMode eq 1 then yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

NCOL=360*gridfactor
NROW=180*gridfactor

grid_clw_as    = fltarr(NCOL,NROW) & grid_clw_as(*,*)    = -999.0
grid_clw_ds    = fltarr(NCOL,NROW) & grid_clw_ds(*,*)    = -999.0

grid_gs_as     = fltarr(NCOL,NROW) & grid_gs_as(*,*)     = -999.0
grid_gs_ds     = fltarr(NCOL,NROW) & grid_gs_ds(*,*)     = -999.0

grid_iwp_as    = fltarr(NCOL,NROW) & grid_iwp_as(*,*)    = -999.0
grid_iwp_ds    = fltarr(NCOL,NROW) & grid_iwp_ds(*,*)    = -999.0

grid_lwp_as    = fltarr(NCOL,NROW) & grid_lwp_as(*,*)    = -999.0
grid_lwp_ds    = fltarr(NCOL,NROW) & grid_lwp_ds(*,*)    = -999.0

grid_rr_as     = fltarr(NCOL,NROW) & grid_rr_as(*,*)     = -999.0
grid_rr_ds     = fltarr(NCOL,NROW) & grid_rr_ds(*,*)     = -999.0

grid_rwp_as    = fltarr(NCOL,NROW) & grid_rwp_as(*,*)    = -999.0
grid_rwp_ds    = fltarr(NCOL,NROW) & grid_rwp_ds(*,*)    = -999.0

grid_sice_as   = fltarr(NCOL,NROW) & grid_sice_as(*,*)   = -999.0
grid_sice_ds   = fltarr(NCOL,NROW) & grid_sice_ds(*,*)   = -999.0

grid_sfc2_as   = fltarr(NCOL,NROW) & grid_sfc2_as(*,*)   = -999.0
grid_sfc2_ds   = fltarr(NCOL,NROW) & grid_sfc2_ds(*,*)   = -999.0

grid_sicefy_as = fltarr(NCOL,NROW) & grid_sicefy_as(*,*) = -999.0
grid_sicefy_ds = fltarr(NCOL,NROW) & grid_sicefy_ds(*,*) = -999.0

grid_sicemy_as = fltarr(NCOL,NROW) & grid_sicemy_as(*,*) = -999.0
grid_sicemy_ds = fltarr(NCOL,NROW) & grid_sicemy_ds(*,*) = -999.0

grid_snow_as   = fltarr(NCOL,NROW) & grid_snow_as(*,*)   = -999.0
grid_snow_ds   = fltarr(NCOL,NROW) & grid_snow_ds(*,*)   = -999.0

grid_swe_as    = fltarr(NCOL,NROW) & grid_swe_as(*,*)    = -999.0
grid_swe_ds    = fltarr(NCOL,NROW) & grid_swe_ds(*,*)    = -999.0

grid_tpw_as    = fltarr(NCOL,NROW) & grid_tpw_as(*,*)    = -999.0
grid_tpw_ds    = fltarr(NCOL,NROW) & grid_tpw_ds(*,*)    = -999.0


;*******************************************************************************
;   read in static land/sea mask
;*******************************************************************************
sfcMask=BytArr(nCol,nRow)
dum1=BytArr(nCol,nRow)
dum2=BytArr(nCol,nRow)
Lun=-99
IF ( nCol EQ 1440 ) THEN OpenR,Lun,'lndsea25.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 1080 ) THEN OpenR,Lun,'lndsea30.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 720  ) THEN OpenR,Lun,'lndsea50.tag',/Get_Lun, /Swap_If_Big_Endian
if ( Lun gt 0 ) then begin
  ReadU,Lun,dum1
  Free_Lun,Lun
endif

;---- reverse index of land/sea mask to be consitent with IDL convention ----
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

if( satId eq 'f16')  then fov_sizes(*) = 0.6783
if( satId eq 'f18')  then fov_sizes(*) = 0.6783


;*******************************************************************************
;    loop read file from file list
;*******************************************************************************
print, 'reading DEP and gridding ...'

openr, iu_dep, dep_file_list, /get_lun

file_dep=''

WHILE ( NOT EOF(iu_dep) ) DO BEGIN

  readf, iu_dep, file_dep, format='(a)'
  print, file_dep
  
  ;---- read DEP ----
  ReadDEP, file_dep, nprofiles, Dep
  
  ;---- loop profiles in DEP ---
  iprf=0L
  while ( iprf lt Dep.nProfsProcessed ) do begin

    lat  = Dep.lat(iprf)
    lon  = Dep.lon(iprf)
    cend = Dep.node(iprf)
    
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
    
    if satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F18 then begin

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

    for lonbox = gridlon_left, gridlon_right do begin
    for latbox = gridlat_bot,  gridlat_top   do begin
        
	;---- asc ----
	nadirLogic_as = near_nadir le nadir_as(lonbox,latbox)
	if satId eq Consts.STR_SATID_AQUA or satId eq Consts.STR_SATID_FY3RI or $
           satId eq Consts.STR_SATID_TRMM or satId eq Consts.STR_SATID_GPM then $
          nadirLogic_as = 1B  
	
        if ( cend eq 0 and nadirLogic_as eq 1B ) then begin
           
	   nadir_as(lonbox,latbox) = near_nadir
	
	  if( Dep.qc(0,iprf) lt 2 ) then begin
	    grid_clw_as(lonbox,latbox)    = Dep.clw(iprf)
	    grid_gs_as(lonbox,latbox)     = Dep.SnowGS(iprf)
	    grid_iwp_as(lonbox,latbox)    = Dep.gwp(iprf)
	    grid_lwp_as(lonbox,latbox)    = Dep.lwp(iprf)
	    grid_rwp_as(lonbox,latbox)    = Dep.rwp(iprf)
	    grid_sfc2_as(lonbox,latbox)   = Dep.iTypSfc(iprf)
	    grid_sice_as(lonbox,latbox)   = Dep.sic(iprf)
	    grid_sicefy_as(lonbox,latbox) = Dep.sic_fy(iprf)
	    grid_sicemy_as(lonbox,latbox) = Dep.sic_my(iprf)
	    grid_snow_as(lonbox,latbox)   = Dep.SnowCover(iprf)
	    grid_swe_as(lonbox,latbox)    = Dep.swe(iprf)
 	    grid_tpw_as(lonbox,latbox)    = Dep.tpw(iprf)
 	    grid_rr_as(lonbox,latbox)     = Dep.rr(iprf)
          endif
          if ( Dep.qc(0,iprf) eq 2 ) then begin
	    grid_clw_as(lonbox,latbox)    = -99.0
	    grid_gs_as(lonbox,latbox)     = -99.0
	    grid_iwp_as(lonbox,latbox)    = -99.0
	    grid_lwp_as(lonbox,latbox)    = -99.0
	    grid_rwp_as(lonbox,latbox)    = -99.0
	    grid_sfc2_as(lonbox,latbox)   = -99.0
	    grid_sice_as(lonbox,latbox)   = -99.0
	    grid_sicefy_as(lonbox,latbox) = -99.0
	    grid_sicemy_as(lonbox,latbox) = -99.0
	    grid_snow_as(lonbox,latbox)   = -99.0
	    grid_swe_as(lonbox,latbox)    = -99.0
 	    grid_tpw_as(lonbox,latbox)    = -99.0
 	    grid_rr_as(lonbox,latbox)     = -99.0
         endif
        
	endif
	
	;---- des ----
        nadirLogic_ds = near_nadir le nadir_ds(lonbox,latbox)
	if satId eq Consts.STR_SATID_AQUA or satId eq Consts.STR_SATID_FY3RI or $
           satId eq Consts.STR_SATID_TRMM or satId eq Consts.STR_SATID_GPM then $
          nadirLogic_ds = 1B  
	
        if ( cend eq 1 and nadirLogic_ds eq 1B ) then begin
           
	  nadir_ds(lonbox,latbox) = near_nadir

	  if( Dep.qc(0,iprf) lt 2 ) then begin
	    grid_clw_ds(lonbox,latbox)    = Dep.clw(iprf)
	    grid_gs_ds(lonbox,latbox)     = Dep.SnowGS(iprf)
	    grid_iwp_ds(lonbox,latbox)    = Dep.gwp(iprf)
	    grid_lwp_ds(lonbox,latbox)    = Dep.lwp(iprf)
	    grid_rwp_ds(lonbox,latbox)    = Dep.rwp(iprf)
	    grid_sfc2_ds(lonbox,latbox)   = Dep.iTypSfc(iprf)
	    grid_sice_ds(lonbox,latbox)   = Dep.sic(iprf)
	    grid_sicefy_ds(lonbox,latbox) = Dep.sic_fy(iprf)
	    grid_sicemy_ds(lonbox,latbox) = Dep.sic_my(iprf)
	    grid_snow_ds(lonbox,latbox)   = Dep.SnowCover(iprf)
	    grid_swe_ds(lonbox,latbox)    = Dep.swe(iprf)
 	    grid_tpw_ds(lonbox,latbox)    = Dep.tpw(iprf)
 	    grid_rr_ds(lonbox,latbox)     = Dep.rr(iprf)
          endif
          if ( Dep.qc(0,iprf) eq 2 ) then begin
	    grid_clw_ds(lonbox,latbox)    = -99.0
	    grid_gs_ds(lonbox,latbox)     = -99.0
	    grid_iwp_ds(lonbox,latbox)    = -99.0
	    grid_lwp_ds(lonbox,latbox)    = -99.0
	    grid_rwp_ds(lonbox,latbox)    = -99.0
	    grid_sfc2_ds(lonbox,latbox)   = -99.0
	    grid_sice_ds(lonbox,latbox)   = -99.0
	    grid_sicefy_ds(lonbox,latbox) = -99.0
	    grid_sicemy_ds(lonbox,latbox) = -99.0
	    grid_snow_ds(lonbox,latbox)   = -99.0
	    grid_swe_ds(lonbox,latbox)    = -99.0
 	    grid_tpw_ds(lonbox,latbox)    = -99.0
 	    grid_rr_ds(lonbox,latbox)     = -99.0
         endif
       
       endif
   
    endfor 
    endfor 
    
    iprf=iprf+1L

  endwhile 
  
ENDWHILE
  
close,iu_dep
free_lun,iu_dep


;*******************************************************************************
; cends,sfcs tokens
;*******************************************************************************
cends = ['as','ds']
ncend = n_elements(cends)
cendTxts = [' Asc ', ' Des ']
sfcPicks = [0,1,2]  ; 0-sea, 1-land, 2-all
nsfc = n_elements(sfcPicks)
sfcTxts  = [ '_sea_', '_lnd_', '_all_' ]

prodIds = ['clw','gs','iwp','lwp','rr','rwp','sfc2','sice','sicefy','sicemy','snow','swe','tpw']
nprod = N_ELEMENTS(prodIds)

prods_as = fltarr(NCOL,NROW)
prods_ds = fltarr(NCOL,NROW)
products = fltarr(NCOL,NROW)

for iprod = 0, nprod - 1 do begin

  prodId = prodIds[iprod]
  print, 'plotting  ' + prodId + ' ...'

  case prodId of

  'clw':        begin
                        prodTitle = ' CLW (mm) '
                        minvalue  = -0.01
                        maxvalue  = 0.70
                        div       = 6
                        fmt       = '(f5.2)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
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
                        prods_as  = grid_gs_as
                        prods_ds  = grid_gs_ds
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
                        prods_as  = grid_iwp_as
                        prods_ds  = grid_iwp_ds
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
                        prods_as  = grid_lwp_as
                        prods_ds  = grid_lwp_ds
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
                        prods_as  = grid_rr_as
                        prods_ds  = grid_rr_ds
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
                        prods_as  = grid_rwp_as
                        prods_ds  = grid_rwp_ds
                end

  'sice':       begin
                        prodTitle = ' Sea Ice Concentration (%) '
                        minvalue  = 0
                        maxvalue  = 100
                        div       = 10
                        fmt       = '(I3)'
                        nsfc_bot  = 0
                        nsfc_top  = 0
                        prods_as  = grid_sice_as
                        prods_ds  = grid_sice_ds
                end

  'sicefy':     begin
                        prodTitle = ' First Year Sea Ice Concentration (%) '
                        minvalue  = 0
                        maxvalue  = 100
                        div       = 10
                        fmt       = '(I3)'
                        nsfc_bot  = 0
                        nsfc_top  = 0
                        prods_as  = grid_sicefy_as
                        prods_ds  = grid_sicefy_ds
                end

  'sicemy':     begin
                        prodTitle = ' Multiple Year Sea Ice Concentration (%) '
                        minvalue  = 0
                        maxvalue  = 100
                        div       = 10
                        fmt       = '(I3)'
                        nsfc_bot  = 0
                        nsfc_top  = 0
                        prods_as  = grid_sicemy_as
                        prods_ds  = grid_sicemy_ds
                end

  'swe':        begin
                        prodTitle = ' Snow Water Equivalent (cm) '
                        minvalue  = 0
                        maxvalue  = 8
                        div       = 8
                        fmt       = '(I2)'
                        nsfc_bot  = 1
                        nsfc_top  = 1
                        prods_as  = grid_swe_as
                        prods_ds  = grid_swe_ds
                end

  'sfc2':       begin
                        prodTitle = ' Post-Processed Sfc Type '
                        minvalue  = 0
                        maxvalue  = 3
                        div       = 3
                        fmt       = '(I2)'
                        nsfc_bot  = 2
                        nsfc_top  = 2
                        prods_as  = grid_sfc2_as
                        prods_ds  = grid_sfc2_ds
                end
                
  'snow':       begin
                        prodTitle = ' Snow Cover '
                        minvalue  = 0
                        maxvalue  = 3
                        div       = 3
                        fmt       = '(I2)'
                        nsfc_bot  = 1
                        nsfc_top  = 1
                        prods_as  = grid_snow_as
                        prods_ds  = grid_snow_ds
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
                        prods_as  = grid_tpw_as
                        prods_ds  = grid_tpw_ds
                end
                
  else:         begin
                        print, 'Unsupported 1D prodId: '+ prodId
                end
  endcase

  for icend = 0, ncend-1 do begin ; loop icend starts
    cend = cends(icend)
    cendTxt = cendTxts(icend)
    
    if icend eq 0 then products = prods_as else products = prods_ds
    
    for isfc = nsfc_bot, nsfc_top do begin ; loop isfc starts
      sfcPick = sfcPicks(isfc)
      sfcTxt = sfcTxts(isfc)
      map_name = dirImg + prefix + yyyymmdd + '_' + prodId + sfcTxt + cend + '.png'
      if prodId eq 'gs'   or prodId eq 'sfc2' or $
         prodId eq 'snow' or prodId eq 'swe' then $
        map_name = dirImg + prefix + yyyymmdd + '_' + prodId + '_cyl' + sfcTxt + cend + '.png'
      title = titleSatId + prodTitle + date +  cendTxt + ' (V' + version +')'
      
      if prodId eq 'sfc2' or prodId eq 'snow' then begin
        plot_sfc_grid,products,map_name,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,title
      endif else begin
        plot_grid,products,sfcMask,map_name,minvalue,maxvalue,latmin,latmax,lonmin,lonmax,$
                  title,sfcPick,div,fmt,color_table_index=ctable
      endelse
    endfor ; loop isfc ends
    
  endfor ; loop icend ends

endfor

seconds_finish = SYSTIME( /SECONDS )
print, 'Time used = ', seconds_finish-seconds_start, ' seconds'

End

