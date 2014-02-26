@../../../setup/paths_idl.pro
;*******************************************************************************
;
;  Purpose:
;    To Plot radiometric and radiometric bias images.
;
;  Dependence:
;    plot_grid.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ------------    --------------    ----------------------
;     09/01/2007       Wanchun Chen     Original Code
;
;*******************************************************************************

Pro GridRad, namelist=namelist
Consts, Consts

;***************************************************************
;    some constants definitions
;***************************************************************
gridfactor=4
date='2012-04-14'
satId='npp'
dirGrid='/disk1/pub/mirs_oper/data/TestbedData/Outputs/grid/npp_atms/'+date+'/'
dirImg='./'
version='2972'
dummy=''
biasPath='/disk1/pub/mirs_oper/data/SemiStaticData/biasCorrec/'
latmin=-90
latmax=90
lonmin=-180
lonmax=180
nwpData=1

readFromList=1
if ( readFromList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId                  ;Satellite ID
  readf,iu,format='(i)', gridfactor             ;gridfactor
  readf,iu,format='(a)', dirGrid                ;gridded data dir
  readf,iu,format='(a)', dirImg                 ;path where to put image files
  readf,iu,format='(a)', date                   ;file extension of image
  readf,iu,format='(a)', version                ;version number
  readf,iu,format='(a)', dummy                  ;pass dummy variable
  readf,iu,format='(a)', dummy                  ;pass dummy variable
  readf,iu,format='(a)', biasPath               ;biasPath
  readf,iu,format='(f)', latmin                 ;min lat
  readf,iu,format='(f)', latmax                 ;max lat
  readf,iu,format='(f)', lonmin                 ;min lon
  readf,iu,format='(f)', lonmax                 ;max lon
  readf,iu,format='(i)', nwpData                ;nwp Data src ( 1-gdas, 2-ecmwf, 3-gfs )
  close,iu
  free_lun,iu,/force
endif

print, 'gridRad.pro ...'
print, 'dir in grid='+dirGrid 
print, 'dir out img='+dirImg 

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

NCOL=360*gridfactor
NROW=180*gridfactor

suffix=strcompress(strmid(date,0,4)+'_'+strmid(date,5,2)+'_'+strmid(date,8,2)+'.dat_gdas')
nwpId='_gdas_'
nwpTitle=' GDAS '

if nwpData eq 2 then suffix= strcompress(strmid(date,0,4)+'_'+strmid(date,5,2)+'_'+strmid(date,8,2)+'.dat_ecmw')
if nwpData eq 2 then nwpId='_ecmwf_'
if nwpData eq 2 then nwpTitle=' ECMWF '

if nwpData eq 3 then suffix= strcompress(strmid(date,0,4)+'_'+strmid(date,5,2)+'_'+strmid(date,8,2)+'.dat_gfs')
if nwpData eq 3 then nwpId='_gfs_'
if nwpData eq 3 then nwpTitle=' GFS '

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

satTxt=strupcase(satId)
if satId eq Consts.STR_SATID_AQUA   then satTxt = 'AMSR-E'
if satId eq Consts.STR_SATID_GCOMW1 then satTxt = 'GCOMW1/AMSR2'
if satId eq Consts.STR_SATID_FY3RI  then satTxt = 'FY3/MWRI'
if satId eq Consts.STR_SATID_NPP    then satTxt = 'NPP/ATMS'
if satId eq Consts.STR_SATID_TRMM   then satTxt = 'TRMM/TMI'
if satId eq Consts.STR_SATID_GPM    then satTxt = 'GPM/GMI'
if satId eq Consts.STR_SATID_MTMA   then satTxt = 'MT/MADRAS'
if satId eq Consts.STR_SATID_MTSA   then satTxt = 'MT/SAPHIR'

;***************************************************************
;    read in sfc types from static land/sea marks
;***************************************************************
sfcMask=BytArr(nCol,nRow)
dum1=BytArr(nCol,nRow)
dum2=BytArr(nCol,nRow)
IF ( nCol EQ 1440 ) THEN OpenR,Lun,'lndsea25.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 1080 ) THEN OpenR,Lun,'lndsea30.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 720  ) THEN OpenR,Lun,'lndsea50.tag',/Get_Lun, /Swap_If_Big_Endian
ReadU,Lun,dum1
Free_Lun,Lun

;---- reverse index of land/sea mask to be consitent with gridded data set
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


;***************************************************************
; flgs to turn on/off to plot or not
;***************************************************************
PLOT_TB=1
PLOT_TBL_BIAS=1
PLOT_TBC_BIAS=1

PLOT_TBL_ASYM=0
PLOT_TBC_ASYM=1


;***************************************************************
; plot setup
;***************************************************************
fmt='(I3)' 

;---- default to POES (N18/N19/Metop-A/Metop-B)
chans    = ['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2','57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']
mins_all = [140,140,170,170,200, 200,210,200,200,180, 180,180,180,200,150, 150,150,220,220,200]
maxs_all = [300,300,290,290,270, 250,230,230,230,230, 250,250,280,290,300, 300,300,265,280,300]
divs     = [8,8,6,6,7, 5,5,6,6,5, 7,7,10,9,5, 5,5,9,6,5]

if ( satId eq Consts.STR_SATID_F16 ) then begin
  chans    = ['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']     
  mins_all = [ 200,  210,  215,  210,  200,  200,   210,   150,   200,   200,   225,   100,  140,  140,  100,  140,  140,  100,  205,    200,    230,    225,    220,    215  ]
  maxs_all = [ 270,  270,  250,  230,  225,  225,   225,   300,   290,   275,   250,   300,  300,  300,  300,  300,  300,  300,  240,    230,    260,    260,    250,    230  ]
  divs     = [  7,    6,    7,    4,    5,    5,     5,     5,     9,     5,     5,     10,   8,    8,    10,   8,    8,    10,   7,      6,      6,      7,      6,      5   ]  
endif

if ( satId eq Consts.STR_SATID_F17 ) then begin
  chans    = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']     
  mins_all = [ 200,  210,  215,  210,  200,  200,   210,   150,   200,   200,   225,   100,  140,  140,  100,  140,  140,  100,  205,   200,     230,     225,   220,    215  ]
  maxs_all = [ 270,  270,  250,  230,  225,  225,   225,   300,   290,   275,   250,   300,  300,  300,  300,  300,  300,  300,  240,   230,     260,     260,   250,    230  ]
  divs     = [  7,    6,    7,    4,    5,    5,     5,     5,     9,     5,     5,     10,   8,    8,    10,   8,    8,    10,   7,     6,       6,       7,     6,      5   ]  
endif

if ( satId eq Consts.STR_SATID_F18 ) then begin
  chans    = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']     
  mins_all = [ 200,  210,  215,  210,  200,  200,   210,   150,   200,   200,   225,   100,  140,  140,  100,  140,  140,  100,  205,   200,     230,     225,   220,    215  ]
  maxs_all = [ 270,  270,  250,  230,  225,  225,   225,   300,   290,   275,   250,   300,  300,  300,  300,  300,  300,  300,  240,   230,     260,     260,   250,    230  ]
  divs     = [  7,    6,    7,    4,    5,    5,     5,     5,     9,     5,     5,     10,   8,    8,    10,   8,    8,    10,   7,     6,       6,       7,     6,      5   ]  
endif

if ( satId eq Consts.STR_SATID_AQUA ) then begin
  chans    = [ '7v','7h',  '11v', '11h', '19v', '19h',  '24v',  '24h',  '37v',  '37h', '89v','89h' ]
  mins_all = [  75,  50,   125,	   50,	  150,	 75,	 175,	  75,	 150,	 100,	175,  125  ]
  maxs_all = [ 315,  300,  300,	  300,	  315,	315,	 315,	 300,	 315,	 315,	315,  300  ]
  divs     = [  6,    5,    7,	   5,	  11,	 6,	  7,	  9,	  11,	  5,	 7,    7   ]  
endif

if ( satId eq Consts.STR_SATID_GCOMW1 ) then begin
  chans    = [ '6v',  '6h',  '7v', '7h',  '10v',  '10h',  '18v',  '18h',  '23v',  '23h',  '36v',  '36h',  '89v',  '89h' ]
  mins_all = [  75,    50,    75,   50,    125,     50,    150,     75,    175,    75,     150,    100,    175,    125  ]
  maxs_all = [ 315,    300,   315,  300,   300,    300,    315,    315,    315,    300,    315,    315,    315,    300  ]
  divs     = [  6,      5,     6,    5,     7,       5,     11,     6,      7,      9,      11,     5,      7,      7   ]  
endif

if ( satId eq Consts.STR_SATID_TRMM ) then begin
  chans    = ['11v','11h','19v','19h','21v','37v','37h','85v','85h' ]       
  mins_all = [ 125,   50,  150,   75,  175,  150,  100,  175,  125  ]
  maxs_all = [ 300,  300,  315,  315,  315,  315,  315,  315,  300  ]
  divs     = [  7,    5,   11,    6,    7,    11,   5,    7,    7   ]  
endif

if ( satId eq Consts.STR_SATID_FY3RI ) then begin
  chans    = ['11v', '11h', '19v', '19h', '24v', '24h', '37v', '37h', '89v', '89h' ]
  mins_all = [125,    50,    150,   75,    175,	  75,	 150,   100,   175,   125  ]
  maxs_all = [300,   300,    315,  315,    315,	  300,	 315,   315,   315,   300  ]
  divs     = [ 7,     5,      11,   6,      7,     9,    11,     5,     7,     7   ]  
endif

if ( satId eq Consts.STR_SATID_NPP ) then begin
  chans    = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2','57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
  mins_all = [140,140,170,170,170,200,200,210,200,200,180,180,180,180,200,150,150,220,220,220,200,220]
  maxs_all = [300,300,290,290,290,270,250,230,230,230,230,250,250,280,290,300,300,300,300,280,300,265]
  divs     = [ 8,  8,  6,  6,  6,  7,  5,  4,  6,  6,  5,  7,  7, 10,  9,  5,  5,  8,  8,  6, 10,  9]
endif

if ( satId eq Consts.STR_SATID_MTMA ) then begin
  chans    = ['19v','19h','24v','37v','37h','89v','89h','157v','157h' ]       
  mins_all = [ 150,   75,  175,  150,  100,  175,  125, 175,  125  ]
  maxs_all = [ 315,  315,  315,  315,  315,  315,  300, 315,  300 ]
  divs     = [ 11,    6,    7,    11,   5,    7,    7,  7,    7  ]  
endif

if ( satId eq Consts.STR_SATID_MTSA ) then begin
  chans    = ['183h','184h','186h','187h','190h','194h']
  mins_all = [220,220,220,200,200,200]
  maxs_all = [265,265,280,300,300,300]
  divs     = [9,9,6,6,6,6]
endif

;---- lnd and sea the same ranges as all
mins_lnd = mins_all
maxs_lnd = maxs_all
mins_sea = mins_all
maxs_sea = maxs_all

;---- TRMM TMI is different
if satId eq Consts.STR_SATID_TRMM then  begin
  mins_lnd = [240,200,250,225,250,255,230,250,235]
  maxs_lnd = [310,300,305,290,300,305,295,305,290]
  mins_sea = [160, 75,175,100,175,190,100,225,175]
  maxs_sea = [195,125,250,200,275,250,250,280,275]	
endif

;---- MT MADRAS is different
if satId eq Consts.STR_SATID_MTMA then  begin
  mins_lnd = [250,225,250,255,230,250,235,250,235]
  maxs_lnd = [305,290,300,305,295,305,290,305,300]
  mins_sea = [175,100,175,190,100,225,175,225,175]
  maxs_sea = [250,200,275,250,250,280,275,300,300]	
endif

cends = ['as','ds']
NCEND = n_elements(cends)
cendTxts = [' Asc ', ' Des ']
sfcPicks = [0,1,2]  ; 0-sea, 1-land, 2-all
NSFC = n_elements(sfcPicks)
sfcTxts  = [ '_sea_', '_lnd_', '_all_' ]
sfcs     = [ 'sea', 'lnd', 'all' ]

mins = fltarr(NCHAN,NSFC)
maxs = fltarr(NCHAN,NSFC)

mins(*,0) = mins_sea(*)
mins(*,1) = mins_lnd(*)
mins(*,2) = mins_all(*)

maxs(*,0) = maxs_sea(*)
maxs(*,1) = maxs_lnd(*)
maxs(*,2) = maxs_all(*)


;***************************************************************
;
; Rad%tb (FWD files) -> gridded tb (ex. GRID_trmm_gdas_20110109_tb_as.dat)
; 
;***************************************************************
IF (  PLOT_TB EQ 1 ) THEN BEGIN

  prefix = rotate+satId+nwpId+'glb_'
  
  for icend = 0, NCEND - 1 do begin
    cend = cends(icend)
    cendTxt = cendTxts(icend)
    
    tb=fltarr(NCOL,NROW,NCHAN)
    tmp=fltarr(NCOL,NROW)
    openr, lun, dirGrid + 'GRID_'+satId+nwpId+yyyymmdd+'_tb_'+cend+'.dat', /get_lun, /Swap_Endian
    for ichan=0, NCHAN-1 do begin
      readu, lun, tmp
      tb(*,*,ichan) = tmp(*,*)
    endfor
    close, lun
    free_lun,lun,/force
    
    for ichan = 0, NCHAN - 1 do begin
      
      sb = fltarr(NCOL,NROW)
      sb = tb(*,*,ichan)
      channel = 'at chan ' + strtrim(ichan+1,2) + ' (' + chans[ichan] + ')'
      
      for isfc = 0, NSFC - 1 do begin
        sfcPick = sfcPicks(isfc)
	sfcTxt = sfcTxts(isfc)
        map_name = dirImg + prefix + yyyymmdd +'_tb_' + chans[ichan] + sfcTxt + cend + '.png'
        title = 'MIRS ' + satTxt + ' Simulated TB (K) using' + nwpTitle + channel + ' ' + date + cendTxt + '(r' + version +')'
        plot_grid, sb, sfcMask, map_name, mins[ichan,isfc], maxs[ichan,isfc], latmin,latmax,lonmin,lonmax, title, sfcPick, divs[ichan], fmt   
      endfor
    
    endfor
  
  endfor
  
ENDIF


;***************************************************************
;
; TBL - Corrected Measured clear-sky TB, Scene%YmCorr( clw < 0.05, MAX_CLD4CLR: misc/Consts.f90 )
; The bias is with respect to simulated TB from FWD file ( Rad%tb -> gridded tb ),
; prods5 in gridBias.f90: Scene%YmCorr(clw<0.05) - Rad%tb
;
;***************************************************************
IF (  PLOT_TBL_BIAS EQ 1 ) THEN BEGIN

  prefix = rotate+satId+nwpId+'bias_glb_'
  minvalue = -5
  maxvalue =  5  
  div = 10
  fmt = '(I3)'
  
  for icend = 0, NCEND - 1 do begin
    cend = cends(icend)
    cendTxt = cendTxts(icend)
    
    tbl=fltarr(NCOL,NROW,NCHAN)
    tmp=fltarr(NCOL,NROW)
    openr, lun, dirGrid + 'GRID_'+satId+nwpId+'bias_'+yyyymmdd+'_tbl_'+cend+'.dat', /get_lun, /Swap_Endian
    for ichan = 0, NCHAN - 1 do begin
      readu, lun, tmp
      tbl(*,*,ichan) = tmp(*,*)
    endfor
    close, lun
    free_lun,lun,/force
  
    for ichan = 0, NCHAN-1 do begin

      sb = fltarr(NCOL,NROW)
      sb = tbl(*,*,ichan)
      channel = 'at ' + chans[ichan]

;      print,'channel,min(sb),max(sb): ',ichan,min(sb),max(sb)
      for isfc = 0, NSFC - 1 do begin
        sfcPick = sfcPicks(isfc)
	sfcTxt = sfcTxts(isfc)
        title = 'MIRS ' + satTxt + ' (Corr. Meas. Clr-Sky - Sim. ) TB (K) ' + channel + ' ' + date + cendTxt + '(r' + version +')'
	map_name = dirImg + prefix + yyyymmdd +'_tbl_' + chans[ichan] + sfcTxt + cend + '.png'
        plot_grid, sb, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, fmt
      endfor
    
    endfor

  endfor

ENDIF



;*******************************************************************************
;
; TB residual is defined as Scene%YmCorr(grid_tbc) - Scene%YFwd(grid_tbf)
; Gridd bias file are generated by src/testbed/grid/gridBias.f90
; grid_tbc - grid_tbf
;
;*******************************************************************************

IF (  PLOT_TBC_BIAS EQ 1 ) THEN BEGIN

  prefix=rotate+satId+nwpId+'bias_glb_'
  minvalue = -5
  maxvalue =  5  
  div = 10
  fmt = '(I3)'
  
  for icend = 0, NCEND - 1 do begin
    cend = cends(icend)
    cendTxt = cendTxts(icend)
    
    tbc=fltarr(NCOL,NROW,NCHAN)
    tmp=fltarr(NCOL,NROW)
    openr, lun, dirGrid + 'GRID_'+satId+'_mirs'+nwpId+'bias_'+yyyymmdd+'_tbc_'+cend+'.dat', /get_lun, /Swap_Endian
    for ichan = 0, NCHAN - 1 do begin
      readu, lun, tmp
      tbc(*,*,ichan) = tmp(*,*)
    endfor
    close, lun
    free_lun,lun,/force
  
    for ichan = 0, NCHAN-1 do begin

      sb = fltarr(NCOL,NROW)
      sb = tbc(*,*,ichan)
      channel = 'at ' + chans[ichan]
;      print,'tbc: channel,min(sb),max(sb): ',ichan,min(sb),max(sb)
    
      for isfc = 0, NSFC - 1 do begin
        sfcPick = sfcPicks(isfc)
	sfcTxt = sfcTxts(isfc)
        title = 'MIRS ' + satTxt + ' (Single Channel - Fitting) Residual (K) ' + channel + ' ' + date + cendTxt + '(r' + version +')'
        map_name = dirImg + prefix + yyyymmdd + '_tbc_' + chans[ichan] + sfcTxt + cend + '.png'
        plot_grid, sb, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, fmt
      endfor
    
    endfor

  endfor
  
ENDIF



;***************************************************************
;    bin box definition for asym
;***************************************************************
yrange=[-5,5]

;---- default to N18/N19/MetopA/MetopB/NPP
NUM_BIN=30
BIN_BOX=4*findgen(NUM_BIN+1)-60
xrange=[-60,60]
xtitle='Local Zenith Angle (degree)'

if ( satId eq Consts.STR_SATID_NPP ) then begin
  NUM_BIN=35
  BIN_BOX=4*findgen(NUM_BIN+1)-70
  xrange=[-70,70]
  xtitle='Local Zenith Angle (degree)'
endif

if ( satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F17 or satId eq Consts.STR_SATID_F18 ) then begin
  NUM_BIN=30
  BIN_BOX=findgen(NUM_BIN)+1
  xrange=[1,30]
  xtitle='Scan Position'
endif

if ( satId eq Consts.STR_SATID_AQUA ) then begin
  NUM_BIN=191
  BIN_BOX=findgen(NUM_BIN)+1
  xrange=[1,191]
  xtitle='Scan Position'
endif

if ( satId eq Consts.STR_SATID_GCOMW1 ) then begin
  NUM_BIN=243
  BIN_BOX=findgen(NUM_BIN)+1
  xrange=[1,243]
  xtitle='Scan Position'
endif

if ( satId eq Consts.STR_SATID_TRMM ) then begin
  NUM_BIN=26
  BIN_BOX=findgen(NUM_BIN)+1
  xrange=[1,26]
  xtitle='Scan Position'
endif

if ( satId eq Consts.STR_SATID_GPM ) then begin
  NUM_BIN=26
  BIN_BOX=findgen(NUM_BIN)+1
  xrange=[1,26]
  xtitle='Scan Position'
endif

if ( satId eq Consts.STR_SATID_FY3RI ) then begin
  NUM_BIN=30
  BIN_BOX=4*findgen(NUM_BIN) + 2
  xrange=[1,120]
  xtitle='Scan Position'
endif

; NB: MT MADRAS values ok for proxy data based on CR TMI
; Need to update for real data
if ( satId eq Consts.STR_SATID_MTMA ) then begin
  NUM_BIN=60
  BIN_BOX=findgen(NUM_BIN)+1
  xrange=[1,60]
  xtitle='Scan Position'
endif

; NB: MT SAPHIR Values ok for proxy data based on LR N18
; Need to update for real data
if ( satId eq Consts.STR_SATID_MTSA ) then begin
  NUM_BIN=35
  BIN_BOX=4*findgen(NUM_BIN+1)-70
  xtitle='Local Zenith Angle (degree)'
  xrange=[-60,60]
endif

;***************************************************************
;
; TBL - Corrected Measured clear-sky TB, Scene%YmCorr( clw < 0.05, MAX_CLD4CLR: misc/Consts.f90 )
; The asym is with respect to simulated TB from FWD file ( Rad%tb -> gridded tb ),
; prods5 in gridBias.f90: Scene%YmCorr(clw<0.05) - Rad%tb
;
;***************************************************************
IF (  PLOT_TBL_ASYM EQ 1 ) THEN BEGIN

  prefix=rotate+satId+nwpId+'asym_glb_'
  
  for icend = 0, NCEND - 1 do begin
  
    cend = cends(icend)
    cendTxt = cendTxts(icend)

    ;***************************************************************
    ;    clear-ksy corrected tb asymmetry with respect to nwp
    ;***************************************************************
    ym_all=fltarr(NUM_BIN,NCHAN)
    ym_sea=fltarr(NUM_BIN,NCHAN)
    ym_sice=fltarr(NUM_BIN,NCHAN)
    ym_lnd=fltarr(NUM_BIN,NCHAN)
    ym_snow=fltarr(NUM_BIN,NCHAN)

    openr, lun, dirGrid + 'GRID_'+satId+nwpId+'asym_'+yyyymmdd+'_tbl_'+cend+'.dat', /get_lun, /Swap_Endian

    NCOND = 4
    NSFC_ALL = 5
    ym1=fltarr(NUM_BIN,NSFC_ALL,NCOND,NCHAN)

    readu, lun, ym1
    close, lun
    free_lun,lun,/force

;    ym_sea[*,*]=y1[*,0,NCOND-1,*]
;    ym_sice[*,*]=y1[*,1,NCOND-1,*]
;    ym_lnd[*,*]=y1[*,2,NCOND-1,*]
;    ym_snow[*,*]=y1[*,3,NCOND-1,*]
;    ym_all[*,*]=y1[*,4,NCOND-1,*]
;    ym(*,*,0) = ym_sea(*,*)
;    ym(*,*,1) = ym_lnd(*,*)
;    ym(*,*,2) = ym_all(*,*)

    ym = fltarr(NUM_BIN,NCHAN,NSFC)
    ym(*,*,0) = ym1(*,0,NCOND-1,*)
    ym(*,*,1) = ym1(*,2,NCOND-1,*)
    ym(*,*,2) = ym1(*,4,NCOND-1,*)


    ;***************************************************************
    ;   clear-sky corrected tb, Scene%YmCorr(clw<0.05) -> tbl
    ;***************************************************************
    ymCorrClr=fltarr(NCOL,NROW,NCHAN)
    tmp=fltarr(NCOL,NROW)
    openr, lun, dirGrid + 'GRID_'+satId+'_'+yyyymmdd+'_tbl_'+cend+'.dat', /get_lun, /Swap_Endian
    for ichan=0, NCHAN-1 do begin
      readu, lun, tmp
      ymCorrClr(*,*,ichan) = tmp(*,*)
    endfor
    close, lun
    free_lun,lun,/force

    ;***************************************************************
    ;  tb ( Rad%tb -> tb )
    ;***************************************************************
    fwd=fltarr(NCOL,NROW,NCHAN)
    tmp=fltarr(NCOL,NROW)
    openr, lun, dirGrid + 'GRID_'+satId+nwpId+yyyymmdd+'_tb_'+cend+'.dat', /get_lun, /Swap_Endian
    for ichan=0, NCHAN-1 do begin
      readu, lun, tmp
      fwd(*,*,ichan) = tmp(*,*)
    endfor
    close, lun
    free_lun,lun,/force

    ;***************************************************************
    ;   scanpos
    ;***************************************************************
    scanpos=fltarr(NCOL,NROW)
    tmp=fltarr(NCOL,NROW)
    openr, lun, dirGrid + 'GRID_'+satId+'_'+yyyymmdd+'_scanpos_'+cend+'.dat', /get_lun, /Swap_Endian
    readu, lun, tmp
    scanpos(*,*) = tmp(*,*)
    close, lun
    free_lun,lun,/force

    ;***************************************************************
    ;   sfc2 type 
    ;***************************************************************
    sfc2=fltarr(NCOL,NROW)
    tmp=fltarr(NCOL,NROW)
    openr, lun, dirGrid + 'GRID_'+satId+'_'+yyyymmdd+'_sfc2_'+cend+'.dat', /get_lun, /Swap_Endian
    readu, lun, tmp
    sfc2(*,*) = tmp(*,*)
    close, lun
    free_lun,lun,/force


    ;***************************************************************
    ;    read in static and dynamic bias files
    ;***************************************************************
    DynBiasFile = biasPath+'/biasCorrec_'+satId+'_'+suffix
    StatBiasFile = biasPath+'/biasCorrec_'+satId+'.dat'
    ReadBias,DynBiasFile,nchan,npos,cfreq,meanBiasDyn,Slope,Intercept,meanTbsimu,meanTbmeas
    ReadBias,StatBiasFile,nchan,npos,cfreq,meanBias,Slope,Intercept,meanTbsimu,meanTbmeas

    ;***************************************************************************
    ;    compute bias from histogram (Scene%ymCorr(clw<0.05)(tbl) - Rad%tb(FWD files,simulated TB(tb))
    ;***************************************************************************
    maxTbDiffallowed = 10
    ym_hist = fltarr(NUM_BIN,NCHAN,NSFC)

    FOR iChan=0,nCHAN-1 DO BEGIN
      sim = reform(fwd(*,*,ichan),ncol,nrow)
      corr = reform(ymCorrClr(*,*,ichan),ncol,nrow)

      FOR iBin = 0,NUM_BIN-1 DO BEGIN
	FOR iSfc = 0, NSFC - 1 do begin

	  if iSfc eq 0 then ind=where(sfc2 eq Consts.OC_TYP and fwd(*,*,iChan) gt 0 and corr gt 0 and scanpos eq iBin+1, ncount) ; sea
	  if iSfc eq 1 then ind=where(sfc2 eq Consts.LD_TYP and fwd(*,*,iChan) gt 0 and corr gt 0 and scanpos eq iBin+1, ncount) ; lnd
	  if iSfc eq 2 then ind=where(                          fwd(*,*,iChan) gt 0 and corr gt 0 and scanpos eq iBin+1, ncount) ; all

	  tbDiff = sim(ind) - corr(ind)
          ind2=where(abs(tbDiff-mean(tbDiff)) le maxTbDiffallowed,ncount2)
          if ( ncount2 gt 1 ) then begin
              res=histogram(tbDiff(ind2),nbins=40,locations=loc)
              res=res/float(max(res))*100.
              indHist=where(res ge 80.)
              ym_hist(iBin,iChan,iSfc) = mean(loc(indHist))
          endif

	ENDFOR 
      ENDFOR
    ENDFOR

    for ichan = 0, NCHAN - 1 do begin
      for isfc = 0, NSFC - 1 do begin

	lndsea = sfcs(isfc)
	ytitle = 'TB Bias Over ' + lndsea
	Lines2Plot = [1,1,1,1]

	title='MIRS ' + strupcase(satId) + ' Corr.(clr-sky) - Sim. TB (K) at ' + chans[ichan] + ' ' + date + cendTxt + '(r' + version +')'
	map_name=dirImg+prefix+yyyymmdd+'_tbl_'+chans[ichan]+'_'+lndsea+'_'+cend+'.png'
	plot_lines_4Bias, bin_box, ym_hist(*,ichan,isfc), ym(*,ichan,isfc), meanBias(*,ichan), meanBiasDyn(*,ichan), xtitle, ytitle, title, $
                          xrange, yrange, map_name, "HistAdj(Static)", "Statistical(Static)","HistAdj(Static)", "HistAdj(Dynamic)", Lines2Plot
      endfor
    endfor  
  
  endfor
  
ENDIF



;***************************************************************
; TB residual asym with respect to Scene%YFwd
; Scene%YmCorr - Scene%YFwd asym
;***************************************************************
IF (  PLOT_TBC_ASYM EQ 1 ) THEN BEGIN

  prefix = rotate+satId+nwpId+'asym_glb_'
  
  for icend = 0, NCEND - 1 do begin
  
    cend = cends(icend)
    cendTxt = cendTxts(icend)

    yfwd_all=fltarr(NUM_BIN,NCHAN)
    yfwd_sea=fltarr(NUM_BIN,NCHAN)
    yfwd_sice=fltarr(NUM_BIN,NCHAN)
    yfwd_lnd=fltarr(NUM_BIN,NCHAN)
    yfwd_snow=fltarr(NUM_BIN,NCHAN)

    openr, lun, dirGrid + 'GRID_'+satId+'_mirs'+nwpId+'asym_'+yyyymmdd+'_tbc_'+cend+'.dat', /get_lun, /Swap_Endian

    NCOND = 4
    NSFC_ALL = 5
    yfwd=fltarr(NUM_BIN,NSFC_ALL,NCOND,NCHAN)

    readu, lun, yfwd
    close, lun
    free_lun,lun,/force

    yfwd_sea[*,*]=yfwd[*,0,NCOND-1,*]
    yfwd_sice[*,*]=yfwd[*,1,NCOND-1,*]
    yfwd_lnd[*,*]=yfwd[*,2,NCOND-1,*]
    yfwd_snow[*,*]=yfwd[*,3,NCOND-1,*]
    yfwd_all[*,*]=yfwd[*,4,NCOND-1,*]
    for ichan = 0, NCHAN - 1 do begin

      for isfc = 0, NSFC - 1 do begin
	lndsea = sfcs(isfc)
	ytitle='TB residual over ' + lndsea
	title = 'MIRS ' + satTxt + ' Single Channel TB - Fitting Residual at ' + chans[ichan] + ' ' + date + cendTxt + '(r' + version +')'
	map_name = dirImg+prefix+yyyymmdd+'_tbc_'+chans[ichan]+'_'+lndsea+'_'+cend+'.png'

	if isfc eq 0 then plot_line2, bin_box, yfwd_sea(*,ichan), yfwd_sice(*,ichan), xtitle, ytitle, title, xrange, yrange, map_name, "Sea", "Sea Ice"
	if isfc eq 1 then plot_line2, bin_box, yfwd_lnd(*,ichan), yfwd_snow(*,ichan), xtitle, ytitle, title, xrange, yrange, map_name, "Land", "Snow"
	if isfc eq 2 then plot_line,  bin_box, yfwd_all(*,ichan), xtitle, ytitle, title, xrange, yrange, map_name
      endfor

    endfor  
 
  endfor
  
ENDIF


End
