@../../../setup/paths_idl.pro
;***********************************************************************************************
;
;  Purpose:
;    To Plot gridded data set into png images.
;
;  Dependence:
;    plot_grid.pro
;
;  Record of revisions:
;       Date            Programmer     	Description
;     ----------      --------------    --------------
;     01/25/2011       Wanchun Chen     Original Coder 
;     08/18/2011       Wanchun Chen     Add em polar
;
;***********************************************************************************************

Pro GridMirs, namelist=namelist
seconds_start = SYSTIME( /SECONDS )
Consts, Consts

;---- identifiers for namelist
satId=Consts.STR_SATID_N18
gridfactor=4
date='2012-07-18'
dirGrid='/disk1/pub/mirs_oper/data/TestbedData/Outputs/grid/n18_amsua_mhs/'+date+'/'
dirImg='./'
processMode=1
version='3065'
latmin=-90
latmax=90
lonmin=-180
lonmax=180
region=0

;---- read those identifiers from namelist or not
readFromList=1
if ( readFromList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId           ;Satellite ID
  readf,iu,format='(i)', gridfactor      ;gridfactor
  readf,iu,format='(a)', dirGrid         ;gridded data dir
  readf,iu,format='(a)', dirImg          ;path where to put image files
  readf,iu,format='(a)', date            ;file extension of image
  readf,iu,format='(i)', processMode     ;process Mode ( 0-orbit, 1-daily )
  readf,iu,format='(a)', version         ;version number
  readf,iu,format='(f)', latmin          ;min lat
  readf,iu,format='(f)', latmax          ;max lat
  readf,iu,format='(f)', lonmin          ;min lon
  readf,iu,format='(f)', lonmax   	 ;max lon
  readf,iu,format='(f)', region          ;region flag for plotting
  close,iu
  free_lun,iu,/force
endif

;--- lat/lon/regionStr ---
regionStr=''
;---Global
if (region eq 0) then begin
    latmin=-90.0
    latmax=90.0
    lonmin=-180.0
    lonmax=180.0
    regionStr='glb_'
endif
;---United States
if (region eq 1) then begin
    latmin=20.0
    latmax=55.0
    lonmin=-130.0
    lonmax=-60.0
    regionStr='us_'
endif
;----Europe
if (region eq 2) then begin
    latmin=30.0
    latmax=60.0
    lonmin=-15.0
    lonmax=30.0
    regionStr='eu_'
endif
;----Gulf of Mexico
if (region eq 3) then begin
    latmin  = 17
    latmax  = 32
    lonmin  = -98
    lonmax  = -68
    regionStr='gulf_'
    if ( satId eq Consts.STR_SATID_AQUA ) then regionStr='glb_' 
endif
;----East China Sea
if (region eq 5) then begin
    latmin  = 5
    latmax  = 35
    lonmin  = 110
    lonmax  = 130
    regionStr='china_'
endif
;----Tropical
if (region eq 6) then begin
    latmin  = 15
    latmax  = 35
    lonmin  = -100
    lonmax  = -60
    regionStr='trop_'
endif

print, 'gridMirs.pro ... ' + regionStr
print, 'dirGrid='+dirGrid
print, 'dirImg='+dirImg

;---- prefix of image file name
prefix=''
if( satId eq Consts.STR_SATID_N18    or $
    satId eq Consts.STR_SATID_N19    or $
    satId eq Consts.STR_SATID_METOPA or $
    satId eq Consts.STR_SATID_METOPB                               ) then $
        prefix='mirs_adv_poes_'  +satId+'_amsuamhs_' + regionStr

if( satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F18 ) then $
	prefix='mirs_adv_dmsp_'  +satId+'_ssmis_'    + regionStr

if( satId eq Consts.STR_SATID_F17                                  ) then $
	prefix='mirs_adv_dmsp_'  +satId+'_ssmis_'    + regionStr

if( satId eq Consts.STR_SATID_AQUA                                 ) then $
	prefix='mirs_adv_eos_'   +satId+'_amsre_'    + regionStr

if( satId eq Consts.STR_SATID_GCOMW1                               ) then $
	prefix='mirs_adv_eos_'   +satId+'_amsr2_'    + regionStr

if( satId eq Consts.STR_SATID_FY3RI                                ) then $
	prefix='mirs_adv_eos_'   +satId+'_mwri_'     + regionStr

if( satId eq Consts.STR_SATID_NPP                                  ) then $
	prefix='mirs_adv_npoess_'+satId+'_atms_'     + regionStr

if( satId eq Consts.STR_SATID_TRMM                                 ) then $
	prefix='mirs_adv_eos_' + satId + '_tmi_'     + regionStr

if( satId eq Consts.STR_SATID_GPM                                  ) then $
	prefix='mirs_adv_eos_' + satId + '_gmi_'     + regionStr

if( satId eq 'trmm2a12'                                            ) then $
	prefix='mirs_' + satId + '_' + regionStr

if( satId eq Consts.STR_SATID_MTMA                                 ) then $
	prefix='mirs_adv_mt_' + satId + '_madras_'     + regionStr

if( satId eq Consts.STR_SATID_MTSA                                 ) then $
	prefix='mirs_adv_mt_' + satId + '_saphir_'     + regionStr

if processMode eq 0 then yyyymmdd=date
if processMode eq 1 then yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)


NLAY=100
NCHAN=20
if ( satId eq Consts.STR_SATID_N18    ) then NCHAN = 20
if ( satId eq Consts.STR_SATID_N19    ) then NCHAN = 20
if ( satId eq Consts.STR_SATID_METOPA ) then NCHAN = 20
if ( satId eq Consts.STR_SATID_METOPB ) then NCHAN = 20
if ( satId eq Consts.STR_SATID_F16    ) then NCHAN = 24
if ( satId eq Consts.STR_SATID_F17    ) then NCHAN = 24
if ( satId eq Consts.STR_SATID_F18    ) then NCHAN = 24
if ( satId eq Consts.STR_SATID_AQUA   ) then NCHAN = 12
if ( satId eq Consts.STR_SATID_GCOMW1 ) then NCHAN = 14
if ( satId eq Consts.STR_SATID_FY3RI  ) then NCHAN = 10
if ( satId eq Consts.STR_SATID_NPP    ) then NCHAN = 22
if ( satId eq Consts.STR_SATID_TRMM   ) then NCHAN = 9
if ( satId eq Consts.STR_SATID_GPM    ) then NCHAN = 13
if ( satId eq 'trmm2a12'              ) then NCHAN = 9
if ( satId eq Consts.STR_SATID_MTMA   ) then NCHAN = 9
if ( satId eq Consts.STR_SATID_MTSA   ) then NCHAN = 6

titleSatId='MIRS ' + strupcase(satId)
if ( satId eq Consts.STR_SATID_AQUA   ) then titleSatId = 'MIRS ' + 'AMSR-E'
if ( satId eq Consts.STR_SATID_GCOMW1 ) then titleSatId = 'MIRS ' + 'GCOM-W1/AMSR2'
if ( satId eq Consts.STR_SATID_FY3RI  ) then titleSatId = 'MIRS ' + 'FY3/MWRI'
if ( satId eq Consts.STR_SATID_TRMM   ) then titleSatId = 'MIRS ' + 'TRMM/TMI'
if ( satId eq Consts.STR_SATID_GPM    ) then titleSatId = 'MIRS ' + 'GPM/GMI'
if ( satId eq 'trmm2a12'              ) then titleSatId = 'TRMM_2A12'
if ( satId eq Consts.STR_SATID_NPP    ) then titleSatId = 'MIRS ' + 'NPP/ATMS'
if ( satId eq Consts.STR_SATID_MTMA   ) then titleSatId = 'MIRS ' + 'MT/MADRAS'
if ( satId eq Consts.STR_SATID_MTSA   ) then titleSatId = 'MIRS ' + 'MT/SAPHIR'

NCOL=360*gridfactor
NROW=180*gridfactor


;***************************************************************
; define products to be plotted
;***************************************************************
;---- 1-D products ----
prodIds_1D = [ 'chisq', 'clw', 'gs', 'iwp', 'lwp', 'nattempt', 'niter', $
               'psfc', 'rr', 'rwp', 'sice', 'sicefy', 'sicemy', 'swe', 'tpw', 'tskin', $
	       'qc', 'sfc', 'sfc2', 'snow' ]

if ( satId eq Consts.STR_SATID_N18 or $
     satId eq Consts.STR_SATID_N19 or $
     satId eq Consts.STR_SATID_NPP or $
     satId eq Consts.STR_SATID_METOPA or $
     satId eq Consts.STR_SATID_METOPB ) and $
   ( processMode eq 1 ) then $
  prodIds_1D = [ 'chisq', 'clw', 'gs', 'iwp', 'lwp', 'nattempt', 'niter', $
               'psfc', 'rr', 'rwp', 'sice', 'sicefy', 'sicemy', 'swe', 'tpw', 'tskin', $
	       'qc', 'sfc', 'sfc2', 'snow', 'angle', 'scanday' ]

if ( satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F18 or $
      satId eq Consts.STR_SATID_F17 ) and ( processMode eq 1 ) then $
  prodIds_1D = [ 'chisq', 'clw', 'gs', 'iwp', 'lwp', 'nattempt', 'niter', $
               'psfc', 'rr', 'rwp', 'sice', 'sicefy', 'sicemy', 'swe', 'tpw', 'tskin', $
	       'qc', 'sfc', 'sfc2', 'snow', 'scanpos', 'wspd' ]

if satId eq Consts.STR_SATID_TRMM or satId eq Consts.STR_SATID_GPM then $
  prodIds_1D = [ 'chisq', 'clw', 'iwp', 'lwp', 'nattempt', 'niter',     $
               'psfc', 'rr', 'rwp', 'swe', 'tpw', 'tskin', 'qc', 'sfc', 'wspd' ]

if satId eq Consts.STR_SATID_MTMA  then $
  prodIds_1D = [ 'chisq', 'clw', 'iwp', 'lwp', 'nattempt', 'niter',     $
               'psfc', 'rr', 'rwp', 'tpw', 'tskin', 'qc', 'sfc', 'sfc2', 'wspd' ]

if satId eq Consts.STR_SATID_MTSA then $
  prodIds_1D = [ 'chisq', 'clw', 'iwp', 'lwp', 'nattempt', 'niter',     $
               'psfc', 'rr', 'rwp', 'tpw', 'tskin', 'qc', 'sfc', 'sfc2' ]

if satId eq 'trmm2a12' then $
  prodIds_1D = [ 'clw', 'iwp', 'rr', 'rwp', 'sfc2' ]

;---- layer products ----
prodIds_lay = [ 'temp', 'wv', 'clwp', 'rainp', 'graupelp' ]                   
if satId eq 'trmm2a12' then UNDEFINE, prodIds_lay

;---- channel products ----
prodIds_chan = [ 'em', 'tbc', 'tbu' ]
if satId eq 'trmm2a12' then UNDEFINE, prodIds_chan

;---- polarographic plots product ----
prodIds_polar = [ 'gs', 'sfc', 'sfc2', 'sice', 'sicefy', 'sicemy', 'snow', 'swe' ]
IF satId EQ Consts.STR_SATID_TRMM or satId eq Consts.STR_SATID_GPM then prodIds_polar = [ 'sfc', 'swe' ]
IF satId EQ 'trmm2a12' then UNDEFINE, prodIds_polar
IF satId EQ  Consts.STR_SATID_MTMA or satId EQ  Consts.STR_SATID_MTSA then UNDEFINE, prodIds_polar

PLOT_EM_POLAR=1
if satId eq 'trmm2a12' then PLOT_EM_POLAR=0
IF satId EQ  Consts.STR_SATID_MTMA or satId EQ  Consts.STR_SATID_MTSA then PLOT_EM_POLAR=0


;***************************************************************
; read static land/sea masks
;***************************************************************
sfcMask=BytArr(nCol,nRow)
dum1=BytArr(nCol,nRow)
dum2=BytArr(nCol,nRow)
IF ( nCol EQ 1440 ) THEN OpenR,Lun,'lndsea25.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 1080 ) THEN OpenR,Lun,'lndsea30.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ  720 ) THEN OpenR,Lun,'lndsea50.tag',/Get_Lun, /Swap_If_Big_Endian
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
; cends,sfcs tokens
;***************************************************************
cends = ['as','ds']
ncend = n_elements(cends)
cendTxts = [' Asc ', ' Des ']
sfcPicks = [0,1,2]  ; 0-sea, 1-land, 2-all
nsfc = n_elements(sfcPicks)
sfcTxts  = [ '_sea_', '_lnd_', '_all_' ]


;---- which plot_grid subroutine to call ----
;0:plot_grid, 1:plot_sfc_grid, 2:plot_gridQCF
sub2use = 0

;***************************************************************
; 1D products section
;***************************************************************
nprod_1D = N_ELEMENTS(prodIds_1D)

for iprod = 0, nprod_1D - 1 do begin

  prodId = prodIds_1D[iprod]
  print, 'plotting ' + prodId + ' ...'

  case  prodId of
  
  'angle':      begin
                        prodTitle = ' Scan Angle '
                        minvalue  = -60
                        maxvalue  = 60
                        div       = 12
                        fmt       = '(I3)'
                        ctable    = 33
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'chisq':      begin
                        prodTitle = ' Chi Square '
                        minvalue  = 0
                        maxvalue  = 10
                        div       = 10
                        fmt       = '(I2)'
                        ctable    = 33
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'clw':        begin
                        prodTitle = ' CLW (mm) '
                        minvalue  = 0.0
                        maxvalue  = 0.7
                        div       = 7
                        fmt       = '(f5.2)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'gs':         begin
                        prodTitle = ' Snow Grain Size Radius (mm) '
                        minvalue  = 0.3
                        maxvalue  = 0.7
                        div       = 4
                        fmt       = '(f4.1)'
                        ctable    = 33
                        nsfc_bot  = 1
                        nsfc_top  = 1
                        sub2use   = 0
                end

  'iwp':        begin
                        prodTitle = ' Graupel Water Path ( mm ) '
                        minvalue  = 0.0
                        maxvalue  = 0.2
                        div       = 4
                        fmt       = '(f5.2)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'lwp':        begin
                        prodTitle = ' Liquid Water Path ( mm ) '
                        minvalue  = 0.0
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
                        ctable    = 33
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'niter':      begin
                        prodTitle = ' Number of Iterations '
                        minvalue  = 0
                        maxvalue  = 7
                        div       = 7
                        fmt       = '(I2)'
                        ctable    = 33
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'psfc':       begin
                        prodTitle = ' Surface Pressure (mb) '
                        minvalue  = 700
                        maxvalue  = 1050
                        div       = 7
                        fmt       = '(I4)'
                        ctable    = 33
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
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
                end

  'rr':         begin
  
                        prodTitle = ' Rain Rate ( mm/hr ) '
                        minvalue  = 0.0
                        maxvalue  = 10
                        div       = 5
                        fmt       = '(f4.1)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end

  'rwp':        begin
                        prodTitle = ' Rain Water Path ( mm ) '
                        minvalue  = 0.0
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
                        fmt       = '(F6.2)'
                        ctable    = 33
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
                        ctable    = 33
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
                        ctable    = 33
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
                        ctable    = 33
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
                        ctable    = 33
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
                        ctable    = 33
                        nsfc_bot  = 1
                        nsfc_top  = 1
                        sub2use   = 0
                end

  'tpw':        begin
                        prodTitle = ' TPW (mm) '
                        minvalue  = 0
                        maxvalue  = 70
                        div       = 14
                        fmt       = '(I2)'
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
                        ctable    = 33
                        nsfc_bot  = 0
                        nsfc_top  = 2
                        sub2use   = 0
                end
  
  'wspd':      begin
                        prodId    = 'wspd'
                        prodTitle = ' Wind Speed (m/s) '
                        minvalue  = 0
                        maxvalue  = 26
                        div       = 13
                        fmt       = '(I2)'
                        ctable    = 41
                        nsfc_bot  = 0
                        nsfc_top  = 0
                        sub2use   = 0
                end
  
  else:         begin
                        print, 'Unsupported 1D prodId: '+ prodId
                end
  endcase
  
  for icend = 0, ncend-1 do begin ; loop icend starts
    cend = cends(icend)
    cendTxt = cendTxts(icend)
    
    fileGrid = dirGrid+'GRID_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    if prodId eq 'snow' then fileGrid = dirGrid+'GRID_'+satId+'_'+yyyymmdd+'_sfc2_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
      continue 
    endif
    
    products = fltarr(NCOL,NROW)
    openr,lun,fileGrid,/get_lun,/Swap_Endian
    readu,lun,products
    free_lun,lun
    
    for isfc = nsfc_bot, nsfc_top do begin
      sfcPick = sfcPicks(isfc)
      sfcTxt = sfcTxts(isfc)
      map_name = dirImg + prefix + yyyymmdd + '_' + prodId + sfcTxt + cend + '.png'
      if prodId eq 'gs'   or prodId eq 'sfc'    or prodId eq 'sfc2'   or $
         prodId eq 'sice' or prodId eq 'sicefy' or prodId eq 'sicemy' or $
         prodId eq 'snow' or prodId eq 'swe'    then $
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
    
    ;---- release resource
    UNDEFINE, products
    
  endfor ; loop icend ends

endfor



;***************************************************************
; layer products section
;***************************************************************
;---- pressure layers ----
layers=[100,200,300,400,500,600,700,800,850,900,950]
nlay_plot=n_elements(layers)
layers_index = [43, 54, 62, 69, 75, 80, 84, 88, 90, 92, 94]
titles=['100mb','200mb','300mb','400mb','500mb','600mb',$
        '700mb','800mb','850mb','900mb','950mb']

nprod_lay = N_ELEMENTS(prodIds_lay)

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
                  ctable    = 33
                end

  'wv':         begin
                  prodTitle = ' Water Vapor Content (g/kg) '
                  minvalues = replicate(0.0,nlay_plot)
                  maxvalues = [0.1,0.15,1.0,2.0,3.5,6.0,12.0,12.0,14.0,14.0,16.0]
                  divs      = [10,10,10,10,7,6,6,6,7,7,8]
                  fmt       = '(f5.2)'
                  ctable    = 33
                end

  'clwp':       begin
                  prodTitle = ' CLW (mm) '
                  minvalues = replicate(0.0,nlay_plot)
                  maxvalues = [0.1,0.1,0.1,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15]
                  divs      = replicate(5,nlay_plot)
                  fmt       = '(f5.2)'
                  ctable    = 41
                end

  'rainp':      begin
                  prodTitle = ' Rain (mm) '
                  minvalues = replicate(0.0,  nlay_plot)
                  maxvalues = replicate(0.10, nlay_plot)
                  divs      = replicate(5,    nlay_plot)
                  fmt       = '(f5.2)'
                  ctable    = 41
                end

  'graupelp':   begin
                  prodTitle =' Graupel (mm) '
                  minvalues = replicate(0.0,  nlay_plot)
                  maxvalues = replicate(0.05, nlay_plot)
                  divs      = replicate(5,    nlay_plot)
                  fmt       = '(f5.2)'
                  ctable    = 41
                end

  else:         begin
                  print, 'Unsupported layer prodId: '+ prodId
                end

  endcase

  for icend = 0, ncend - 1 do begin ; loop icend starts

    cend = cends(icend)
    cendTxt = cendTxts(icend)

    ;---- read in data
    fileGrid = dirGrid+'GRID_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
      continue 
    endif

    products = fltarr(NCOL,NROW,NLAY)
    tmp=fltarr(NCOL,NROW)
    openr,lun,fileGrid,/get_lun,/Swap_Endian
    for ilay = 0, NLAY - 1 do begin
      readu, lun, tmp
      products(*,*,ilay) = tmp(*,*)
    endfor
    free_lun,lun

    ;---- plot profile layers
    for ilay = 0, nlay_plot - 1 do begin

      sb=fltarr(NCOL,NROW)
      layer = 'at ' + titles[ilay]
      minvalue = minvalues[ilay]
      maxvalue = maxvalues[ilay]
      div      = divs[ilay]
      for isfc = 0, nsfc-1 do begin
        sfcPick = sfcPicks(isfc)
        sfcTxt = sfcTxts(isfc)
        map_name = dirImg+prefix+yyyymmdd+'_'+prodId +'_'+titles[ilay]+sfcTxt+cend+'.png'
        title = titleSatId+prodTitle+layer+' '+date+cendTxt+' (V'+version +')'
        sb = products(*,*,layers_index(ilay))      
        plot_grid,sb,sfcMask,map_name,minvalue,maxvalue,latmin,latmax,$
                  lonmin,lonmax,title,sfcPick,div,fmt,color_table_index=ctable
      endfor

    endfor

    ;---- release resource
    UNDEFINE, products

  endfor ; loop icend ends

endfor



;***************************************************************
; channel products section
;***************************************************************

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
  chan_ids  = ['50h', '52h', '53h', '54h', '55h', '57rc', '59rc', '150h', $
               '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', $
               '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
endif

if satId eq Consts.STR_SATID_F18 then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10','11','12',$
               '13','14','15','16','17','18','19','20','21','22','23','24']
  chan_ids  = ['50h', '52h', '53h', '54h', '55h', '57rc', '59rc', '150h', $
               '190h', '186h', '184h', '19h', '19v', '22v', '37h', '37v', $
               '91v', '91h', '63rc', '60rc1', '60rc2', '60rc3', '60rc4', '60rc5']
endif

if satId eq Consts.STR_SATID_AQUA then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10','11','12']
  chan_ids  = ['7v','7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h']
endif

if satId eq Consts.STR_SATID_GCOMW1 then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9','10','11','12','13','14']
  chan_ids  = ['6v','6h','7v','7h','10v','10h','18v','18h','23v','23h','36v','36h','89v','89h']
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

if satId eq Consts.STR_SATID_MTMA then  begin
  chan_nums = ['1','2','3','4','5','6','7','8','9']
  chan_ids  = ['19v','19h','24v','37v','37h','89v','89h','157v','157h']
endif

if satId eq Consts.STR_SATID_MTSA then  begin
  chan_nums = ['1','2','3','4','5','6']
  chan_ids  = ['183h','184h','186h','187h','190h','194h']
endif

nprod_chan = N_ELEMENTS( prodIds_chan )

for iprod = 0, nprod_chan - 1 do begin

  prodId = prodIds_chan[iprod]
  print, 'plotting ' + prodId + ' ...'

  if prodId eq 'em' then begin
    prodTitle = ' Emissivity '
    fmt       = '(f4.2)' 
  endif

  if prodId eq 'tbc' then begin
    prodTitle = ' Corr. Measured TB (K) '
    fmt       = '(I3)' 
  endif

  if prodId eq 'tbu' then begin
    prodTitle = ' UnCorr. Measured TB (K) '
    fmt       = '(I3)' 
  endif

  if prodId eq 'em' then begin

    divs = replicate(10,NCHAN)
    
    if satId eq Consts.STR_SATID_N18 or satId eq Consts.STR_SATID_METOPA or $
       satId eq Consts.STR_SATID_N19 or satId eq Consts.STR_SATID_METOPB then begin
      mins_sea  = [0.40,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                   0.50,0.50,0.50,0.50,0.60,0.60,0.60,0.65,0.65,0.65]
      maxs_sea  = [0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,$
                   0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95]
      mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
                   0.65,0.65,0.65,0.65,0.60,0.60,0.60,0.60,0.60,0.60]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
                   1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
    endif                    

    if satId eq Consts.STR_SATID_F16 then  begin
      mins_sea  = [0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.50, $
                   0.50, 0.50, 0.50, 0.25, 0.55, 0.55, 0.30, 0.60, $
                   0.70, 0.40, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_sea  = [1.00, 1.00, 1.00, 1.00, 1.00, 0.95, 0.95, 0.95, $
                   0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.90, 0.95, $
                   0.95, 0.90, 0.70, 0.95, 0.95, 0.95, 0.95, 0.95]
      mins_lnd  = [0.65, 0.65, 0.65, 0.65, 0.65, 0.50, 0.50, 0.70, $
                   0.70, 0.70, 0.70, 0.65, 0.75, 0.75, 0.60, 0.75, $
                   0.70, 0.70, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_lnd  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.95, $
                   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                   1.00, 1.00, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
    endif

    if satId eq Consts.STR_SATID_F17 then  begin
      mins_sea  = [0.30, 0.30, 0.30, 0.30, 0.30, 0.50, 0.50, 0.50, $
                   0.50, 0.50, 0.50, 0.25, 0.55, 0.55, 0.30, 0.60, $
                   0.70, 0.40, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55] 
      maxs_sea  = [0.90, 0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.95, $
                   0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.90, 0.95, $
                   0.95, 0.90, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
      mins_lnd  = [0.60, 0.60, 0.60, 0.60, 0.60, 0.50, 0.50, 0.70, $
                   0.70, 0.70, 0.70, 0.65, 0.75, 0.75, 0.60, 0.75, $
                   0.70, 0.70, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_lnd  = [0.95, 0.95, 0.95, 0.95, 0.95, 1.00, 1.00, 0.95, $
                   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                   1.00, 1.00, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
    endif

    if satId eq Consts.STR_SATID_F18 then  begin
      mins_sea  = [0.30, 0.30, 0.30, 0.30, 0.30, 0.50, 0.50, 0.50, $
                   0.50, 0.50, 0.50, 0.25, 0.55, 0.55, 0.30, 0.60, $
                   0.70, 0.40, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55] 
      maxs_sea  = [0.90, 0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.95, $
                   0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.90, 0.95, $
                   0.95, 0.90, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
      mins_lnd  = [0.60, 0.60, 0.60, 0.60, 0.60, 0.50, 0.50, 0.70, $
                   0.70, 0.70, 0.70, 0.65, 0.75, 0.75, 0.60, 0.75, $
                   0.70, 0.70, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
      maxs_lnd  = [0.95, 0.95, 0.95, 0.95, 0.95, 1.00, 1.00, 0.95, $
                   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                   1.00, 1.00, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
    endif

    if satId eq Consts.STR_SATID_AQUA then begin
      mins_sea  = [0.65,0.65,0.65,0.50,0.70,0.60,0.50,0.60,0.60,0.50,0.65,0.65]
      maxs_sea  = [1.00,1.00,1.00,0.65,0.80,0.80,0.85,0.80,0.85,0.80,0.80,0.85]
      mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.80,0.90,0.90,0.70,0.75,0.75]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
    endif

    if satId eq Consts.STR_SATID_GCOMW1 then begin
      mins_sea  = [0.65,0.65,0.65,0.65,0.65,0.50,0.70,0.60,0.50,0.60,0.60,0.50,0.65,0.65]
      maxs_sea  = [1.00,1.00,1.00,1.00,1.00,0.65,0.80,0.80,0.85,0.80,0.85,0.80,0.80,0.85]
      mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.80,0.90,0.90,0.70,0.75,0.75]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
    endif

    if satId eq Consts.STR_SATID_FY3RI then  begin
      mins_sea  = [0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35]
      maxs_sea  = [0.70, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.80, 0.80]
      mins_lnd  = [0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65]
      maxs_lnd  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
      divs      = [  4,   5,    4,    5,     4,    5,    4,    5,    4,    5 ] 
    endif

    if satId eq Consts.STR_SATID_NPP then begin
      mins_sea  = [0.40,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
	           0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
      maxs_sea  = [0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,$
	           0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95]
      mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
	           0.65,0.65,0.65,0.65,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
                   1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
    endif  

    if satId eq Consts.STR_SATID_TRMM then  begin
      mins_sea  = [0.50, 0.20, 0.55, 0.25, 0.55, 0.60, 0.25, 0.70, 0.40]
      maxs_sea  = [0.60, 0.40, 0.60, 0.45, 0.60, 0.65, 0.45, 0.80, 0.50]
      mins_lnd  = [0.80, 0.65, 0.80, 0.65, 0.80, 0.90, 0.70, 0.80, 0.75]
      maxs_lnd  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
    endif

    if satId eq Consts.STR_SATID_GPM then  begin
      mins_sea  = [0.55,0.25,0.58,0.25,0.60,0.65,0.25,0.75,0.45,0.45,0.45,0.45,0.45]
      maxs_sea  = [0.63,0.35,0.68,0.45,0.70,0.75,0.50,0.90,0.75,0.75,0.75,0.75,0.75]
      mins_lnd  = [0.70,0.65,0.70,0.65,0.80,0.90,0.70,0.75,0.75,0.75,0.75,0.75,0.75]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
    endif
    
    if satId eq Consts.STR_SATID_MTMA then  begin
      mins_sea = [0.55, 0.25, 0.55, 0.60, 0.25, 0.70, 0.40, 0.70, 0.40]
      maxs_sea = [0.60, 0.45, 0.60, 0.65, 0.45, 0.80, 0.50, 0.80, 0.50]
      mins_lnd = [0.80, 0.65, 0.80, 0.90, 0.70, 0.80, 0.75, 0.80, 0.75]
      maxs_lnd = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
    endif

    if satId eq Consts.STR_SATID_MTSA then  begin
      mins_sea  = [0.65,0.65,0.65,0.65,0.65,0.65]
      maxs_sea  = [0.95,0.95,0.95,0.95,0.95,0.95]
      mins_lnd  = [0.60,0.60,0.60,0.60,0.60,0.60]
      maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00]
    endif

    ;---- default to use sea, then overwrite by land values if necessary
    mins_all = mins_sea
    maxs_all = maxs_sea
    
    for ichan = 0, NCHAN - 1 do begin
      if mins_lnd(ichan) lt mins_all(ichan) then mins_all(ichan) = mins_lnd(ichan)
      if maxs_lnd(ichan) gt maxs_all(ichan) then maxs_all(ichan) = maxs_lnd(ichan)
    endfor

  endif 

  
  if prodId eq 'tbc' or prodId eq 'tbu' then begin

    if satId eq Consts.STR_SATID_N18 or satId eq Consts.STR_SATID_METOPA or $
       satId eq Consts.STR_SATID_N19 or satId eq Consts.STR_SATID_METOPB then begin
      mins_all = [140,140,170,170,200, 200,210,200,200,180, $
                  180,180,180,200,150, 150,150,220,220,200]
      maxs_all = [300,300,290,290,270, 250,230,230,230,230, $
                  250,250,280,290,300, 300,300,265,280,300]
      divs     = [8,8,6,6,7, 5,5,6,6,5, 7,7,10,9,5, 5,5,9,6,5]
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

    if satId eq Consts.STR_SATID_F17 then begin
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

    if satId eq Consts.STR_SATID_F18 then begin
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

    if satId eq Consts.STR_SATID_AQUA then begin
      mins_all = [  75, 50,125, 50,150, 75,175, 75,150,100,175,125 ]
      maxs_all = [ 315,300,300,300,315,315,315,300,315,315,315,300 ]
      divs     = [  6,  5,  7,  5,  11, 6,  7,  9,  11, 5,  7,  7  ]
    endif

    if satId eq Consts.STR_SATID_GCOMW1 then begin
      mins_all = [  75, 50, 75, 50,125, 50,150, 75,175, 75,150,100,175,125 ]
      maxs_all = [ 315,300,315,300,300,300,315,315,315,300,315,315,315,300 ]
      divs     = [  6,  5,  6,  5,  7,  5,  11, 6,  7,  9,  11, 5,  7,  7  ]
    endif

    if satId eq Consts.STR_SATID_FY3RI then  begin
      mins_all = [ 125, 50,150, 75,175, 75,150,100,175,125 ]
      maxs_all = [ 300,300,315,315,315,300,315,315,315,300 ]
      divs     = [  7,  5,  11, 6,  7,  9,  11, 5,  7,  7  ]
    endif

    if satId eq Consts.STR_SATID_NPP then begin
       ;            1   2   3   4   5   6   7   8   9   10  11 12  13  14  15  16  17  18  19  20  21  22
       mins_all = [140,140,170,170,170,200,200,210,200,200,180,180,180,180,200,150,150,220,220,220,200,220]
       maxs_all = [300,300,290,290,290,270,250,230,230,230,230,250,250,280,290,300,300,300,300,280,300,265]
       divs     = [ 8,  8,  6,  6,  6,  7,  5,  4,  6,  6,  5,  7,  7, 10,  9,  5,  5,  8,  8,  6, 10,  9]
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
    
    if satId eq Consts.STR_SATID_MTMA then begin
      mins_all = [ 150,   75,  175,  150,  100,  175,  125, 175,  125  ]
      maxs_all = [ 315,  315,  315,  315,  315,  315,  300, 315,  300 ]
      divs     = [ 11,    6,    7,    11,   5,    7,    7,  7,    7  ]  
    endif

    if satId eq Consts.STR_SATID_MTSA then begin
      mins_all = [220,220,220,200,200,200]
      maxs_all = [265,265,280,300,300,300]
      divs     = [9,9,6,6,6,6]
    endif

    ;---- lnd and sea the same ranges as all
    mins_lnd = mins_all
    maxs_lnd = maxs_all
    mins_sea = mins_all
    maxs_sea = maxs_all
    
    ;---- TRMM TMI
    if satId eq Consts.STR_SATID_TRMM then  begin
      mins_lnd = [240,200,250,225,250,255,230,250,235]
      maxs_lnd = [310,300,305,290,300,305,295,305,290]
      mins_sea = [160, 75,175,100,175,190,100,225,175]
      maxs_sea = [195,125,250,200,275,250,250,280,275]    
    endif
    
    ;---- MT MADRAS
    if satId eq Consts.STR_SATID_MTMA then  begin
      mins_lnd = [250,225,250,255,230,250,235,250,235]
      maxs_lnd = [305,290,300,305,295,305,290,305,300]
      mins_sea = [175,100,175,190,100,225,175,225,175]
      maxs_sea = [250,200,275,250,250,280,275,300,300]	
    endif

  endif 
 
  for icend = 0, ncend-1 do begin ; loop icend starts

    cend = cends(icend)
    cendTxt = cendTxts(icend)

    ;---- read in data
    fileGrid = dirGrid+'GRID_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
      continue 
    endif

    products = fltarr(NCOL,NROW,NCHAN)
    tmp = fltarr(NCOL,NROW)
    openr,lun,fileGrid,/get_lun,/Swap_Endian
    for ichan = 0, NCHAN - 1 do begin
      readu, lun, tmp
      products(*,*,ichan) = tmp(*,*)
    endfor
    free_lun,lun

    ;---- plot profile layers
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

    ;---- release resource
    UNDEFINE, products

  endfor ; loop icend ends

endfor



;***************************************************************
; polar plots section
;***************************************************************

nprod_polar = N_ELEMENTS( prodIds_polar )

for iprod = 0, nprod_polar - 1 do begin

  prodId = prodIds_polar[iprod]
  print, 'plotting ' + prodId + ' polar ...'

  case prodId of
 
  'sfc':        begin
                  prodTitle = ' Pre-Classified Sfc Type '
                  minvalue  = 0
                  maxvalue  = 3
                  div	    = 3
                  fmt	    = '(I2)'
                  symsize   = 0.8
                  nsfc_bot  = 2
                  nsfc_top  = 2
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

  'snow':       begin
                  prodTitle = ' Snow & Ice ' 
                  minvalue  = 0
                  maxvalue  = 8
                  div       = 8
                  fmt       = '(I2)'
                  nsfc_bot  = 1
                  nsfc_top  = 1
                end

  'gs':         begin
                  prodTitle = ' Snow Grain Size Radius (mm) ' 
                  minvalue  = 0.3
                  maxvalue  = 0.7
                  div       = 4
                  fmt       = '(f4.1)'
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
    
    fileGrid = dirGrid+'GRID_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
      continue 
    endif
   
    products = fltarr(NCOL,NROW)
    openr,lun,fileGrid,/get_lun,/Swap_Endian
    readu,lun,products
    free_lun,lun

    fileSfc = dirGrid+'GRID_'+satId+'_'+yyyymmdd+'_'+'sfc2'+'_'+cend+'.dat'
    result = FILE_TEST(fileSfc)
    if result eq 0 then begin
      print, 'Post-Processed Gridded surface file not exist: ' + fileSfc
      continue 
    endif

    sfcs=fltarr(NCOL,NROW)
    openr,lun,fileSfc,/get_lun,/Swap_Endian
    readu,lun,sfcs
    free_lun,lun

    for isfc = nsfc_bot, nsfc_top do begin
      sfcPick=sfcPicks(isfc)
      sfcTxt=sfcTxts(isfc)

      ;---- Northern Hemisphere
      centrlat  = 90.0
      orientlon = -80.0
      latmin_ps = 0.0
      if prodId eq 'sice' or prodId eq 'sicefy' or prodId eq 'sicemy' or prodId eq 'swe' or prodId eq 'gs' then latmin_ps = 45.0
      latmax_ps = 90.0
      ind = where( lats ge latmin_ps and lats le latmax_ps )
      map_name = dirImg + prefix + yyyymmdd + '_' + prodId + '_pn' + sfcTxt + cend + '.png'
      title = titleSatId + ' N. H.' + prodTitle + date +  cendTxt + ' (V' + version +')'
      if prodId eq 'sfc' or prodId eq 'sfc2' then $
         plot_sfc_polar,products(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                        lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,symsize
      if prodId eq 'snow' then $
        plot_snow_polar,products(ind),sfcs(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                        lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,sfcPick,div,fmt
      if prodId eq 'sice' or prodId eq 'sicefy' or prodId eq 'sicemy' or $
         prodId eq 'gs'   or prodId eq 'swe'    then  $
        plot_polar,products(ind),sfcs(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                   lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,sfcPick,div,fmt

      ;---- Southern Hemisphere
      centrlat  = -90.0
      orientlon = 0.0
      latmin_ps = -90.0
      latmax_ps = 0.0
      if prodId eq 'sice' or prodId eq 'sicefy' or prodId eq 'sicemy' or prodId eq 'swe' then latmax_ps = -45.0
      ind = where( lats ge latmin_ps and lats le latmax_ps )
      map_name = dirImg + prefix + yyyymmdd + '_' + prodId + '_ps' + sfcTxt + cend + '.png'
      title = titleSatId + ' S. H.' + prodTitle + date +  cendTxt + ' (V' + version +')'
      if prodId eq 'sfc' or prodId eq 'sfc2' then $
        plot_sfc_polar,products(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                       lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,symsize
      if prodId eq 'snow' then $
        plot_snow_polar,products(ind),sfcs(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                        lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,sfcPick,div,fmt
      if prodId eq 'sice' or prodId eq 'sicefy' or prodId eq 'sicemy' or $
         prodId eq 'gs'   or prodId eq 'swe'    then  $
        plot_polar,products(ind),sfcs(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,$
                   lonmin,lonmax,centrlat,orientlon,lats(ind),lons(ind),title,sfcPick,div,fmt

    endfor

    ;---- release resource
    UNDEFINE, products

  endfor ; loop icend ends

endfor


;***************************************************************
; emissivity polar plot section
;***************************************************************  
IF PLOT_EM_POLAR EQ 1 THEN BEGIN

  print, 'plotting em polar ...'
  
  lats=fltarr(NCOL,NROW)
  for ilat=0L,NROW-1L do begin
    lats(*,ilat)=-89.75 + (1.0/gridfactor)*ilat
  endfor

  lons=fltarr(NCOL,NROW)
  for ilon=0L,NCOL-1L do begin
    lons(ilon,*)=-179.75 + (1.0/gridfactor)*ilon
  endfor

  prodId    = 'em'
  prodTitle = ' Emissivity '
  fmt       = '(f4.2)' 
  divs      = replicate(10,NCHAN)

  if satId eq Consts.STR_SATID_N18 or satId eq Consts.STR_SATID_METOPA or $
     satId eq Consts.STR_SATID_N19 or satId eq Consts.STR_SATID_METOPB then begin
    mins_sea  = [0.40,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                 0.50,0.50,0.50,0.50,0.60,0.60,0.60,0.65,0.65,0.65]
    maxs_sea  = [0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,$
                 0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95]
    mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
                 0.65,0.65,0.65,0.65,0.60,0.60,0.60,0.60,0.60,0.60]
    maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
                 1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
  endif                    

  if satId eq Consts.STR_SATID_F16 then begin
    mins_sea  = [0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.50, $
                 0.50, 0.50, 0.50, 0.25, 0.55, 0.55, 0.30, 0.60, $
                 0.70, 0.40, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
    maxs_sea  = [1.00, 1.00, 1.00, 1.00, 1.00, 0.95, 0.95, 0.95, $
                 0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.90, 0.95, $
                 0.95, 0.90, 0.70, 0.95, 0.95, 0.95, 0.95, 0.95]
    mins_lnd  = [0.65, 0.65, 0.65, 0.65, 0.65, 0.50, 0.50, 0.70, $
                 0.70, 0.70, 0.70, 0.65, 0.75, 0.75, 0.60, 0.75, $
                 0.70, 0.70, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
    maxs_lnd  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.95, $
                 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                 1.00, 1.00, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
  endif

  if satId eq Consts.STR_SATID_F17 then begin
    mins_sea  = [0.30, 0.30, 0.30, 0.30, 0.30, 0.50, 0.50, 0.50, $
                 0.50, 0.50, 0.50, 0.25, 0.55, 0.55, 0.30, 0.60, $
                 0.70, 0.40, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55] 
    maxs_sea  = [0.90, 0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.95, $
                 0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.90, 0.95, $
                 0.95, 0.90, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
    mins_lnd  = [0.60, 0.60, 0.60, 0.60, 0.60, 0.50, 0.50, 0.70, $
                 0.70, 0.70, 0.70, 0.65, 0.75, 0.75, 0.60, 0.75, $
                 0.70, 0.70, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
    maxs_lnd  = [0.95, 0.95, 0.95, 0.95, 0.95, 1.00, 1.00, 0.95, $
                 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                 1.00, 1.00, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
  endif

  if satId eq Consts.STR_SATID_F18 then begin
    mins_sea  = [0.30, 0.30, 0.30, 0.30, 0.30, 0.50, 0.50, 0.50, $
                 0.50, 0.50, 0.50, 0.25, 0.55, 0.55, 0.30, 0.60, $
                 0.70, 0.40, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55] 
    maxs_sea  = [0.90, 0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.95, $
                 0.90, 0.90, 0.90, 0.90, 0.95, 0.95, 0.90, 0.95, $
                 0.95, 0.90, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
    mins_lnd  = [0.60, 0.60, 0.60, 0.60, 0.60, 0.50, 0.50, 0.70, $
                 0.70, 0.70, 0.70, 0.65, 0.75, 0.75, 0.60, 0.75, $
                 0.70, 0.70, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]
    maxs_lnd  = [0.95, 0.95, 0.95, 0.95, 0.95, 1.00, 1.00, 0.95, $
                 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, $
                 1.00, 1.00, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95]
  endif

  if satId eq Consts.STR_SATID_AQUA then begin
    mins_sea  = [0.65,0.65,0.65,0.50,0.70,0.60,0.50,0.60,0.60,0.50,0.65,0.65]
    maxs_sea  = [1.00,1.00,1.00,0.65,0.80,0.80,0.85,0.80,0.85,0.80,0.80,0.85]
    mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.80,0.90,0.90,0.70,0.75,0.75]
    maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
  endif

  if satId eq Consts.STR_SATID_GCOMW1 then begin
    mins_sea  = [0.65,0.65,0.65,0.65,0.65,0.50,0.70,0.60,0.50,0.60,0.60,0.50,0.65,0.65]
    maxs_sea  = [1.00,1.00,1.00,1.00,1.00,0.65,0.80,0.80,0.85,0.80,0.85,0.80,0.80,0.85]
    mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.80,0.90,0.90,0.70,0.75,0.75]
    maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
  endif

  if satId eq Consts.STR_SATID_FY3RI then begin
    mins_sea  = [0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35]
    maxs_sea  = [0.70, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.80, 0.80]
    mins_lnd  = [0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65]
    maxs_lnd  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
    divs      = [  4,   5,    4,    5,     4,    5,    4,    5,    4,    5 ] 
  endif

  if satId eq Consts.STR_SATID_NPP then begin
    mins_sea  = [0.40,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
	         0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
    maxs_sea  = [0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,$
	         0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95]
    mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
	         0.65,0.65,0.65,0.65,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60]
    maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
                 1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
  endif  

  if satId eq Consts.STR_SATID_TRMM then begin
    mins_sea  = [0.50, 0.20, 0.55, 0.25, 0.55, 0.60, 0.25, 0.70, 0.40]
    maxs_sea  = [0.60, 0.40, 0.60, 0.45, 0.60, 0.65, 0.45, 0.80, 0.50]
    mins_lnd  = [0.80, 0.65, 0.80, 0.65, 0.80, 0.90, 0.70, 0.80, 0.75]
    maxs_lnd  = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
  endif

  if satId eq Consts.STR_SATID_GPM then begin
    mins_sea  = [0.55,0.25,0.58,0.25,0.60,0.65,0.25,0.75,0.45,0.45,0.45,0.45,0.45]
    maxs_sea  = [0.63,0.35,0.68,0.45,0.70,0.75,0.50,0.90,0.75,0.75,0.75,0.75,0.75]
    mins_lnd  = [0.70,0.65,0.70,0.65,0.80,0.90,0.70,0.75,0.75,0.75,0.75,0.75,0.75]
    maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
  endif

  ;---- default to use sea, then overwrite by land values if necessary
  mins_all = mins_sea
  maxs_all = maxs_sea

  for ichan = 0, NCHAN - 1 do begin
    if mins_lnd(ichan) lt mins_all(ichan) then mins_all(ichan) = mins_lnd(ichan)
    if maxs_lnd(ichan) gt maxs_all(ichan) then maxs_all(ichan) = maxs_lnd(ichan)
  endfor


  for icend = 0, ncend-1 do begin ; loop icend starts

    cend = cends(icend)
    cendTxt = cendTxts(icend)

    ;---- read in sfc2 data
    fileSfc = dirGrid+'GRID_'+satId+'_'+yyyymmdd+'_'+'sfc2'+'_'+cend+'.dat'
    result = FILE_TEST(fileSfc)
    if result eq 0 then begin
      print, 'Post-Processed Gridded surface file not exist: ' + fileSfc
      continue 
    endif

    sfcs=fltarr(NCOL,NROW)
    openr,lun,fileSfc,/get_lun,/Swap_Endian
    readu,lun,sfcs
    free_lun,lun

    ;---- read in data
    fileGrid = dirGrid+'GRID_'+satId+'_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
      continue 
    endif

    products = fltarr(NCOL,NROW,NCHAN)
    tmp = fltarr(NCOL,NROW)
    openr,lun,fileGrid,/get_lun,/Swap_Endian
    for ichan = 0, NCHAN - 1 do begin
      readu, lun, tmp
      products(*,*,ichan) = tmp(*,*)
    endfor
    free_lun,lun

    ;---- plot profile layers
    for ichan = 0, NCHAN - 1 do begin ; loop ichan starts

      product = products(*,*,ichan)
      channel = 'at chan ' + chan_nums(ichan) + ' (' + chan_ids(ichan) + ')'
      
      for isfc = 0, nsfc-1 do begin ; loop isfc starts
      
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
        
	;---- reform
	sfcs1 = reform(sfcs)
	lats1 = reform(lats)
	lons1 = reform(lons)
	prod1 = reform(product)
	
	;---- Northern Hemisphere
	centrlat  = 90.0
	orientlon = -80.0
	latmin_ps = 45.0
	latmax_ps = 90.0

	ind = where( lats1 ge latmin_ps and lats1 le latmax_ps)
	map_name = dirImg+prefix+yyyymmdd+'_'+prodId+'_'+chan_ids(ichan)+'_pn'+sfcTxt+cend+'.png'
	title = titleSatId+' N. H.'+prodTitle+channel+date+cendTxt+' (V' + version +')'
	plot_polar,prod1(ind),sfcs1(ind),map_name,minvalues(ichan),maxvalues(ichan),latmin_ps,latmax_ps,$
                   lonmin,lonmax,centrlat,orientlon,lats1(ind),lons1(ind),title,sfcPick,div,fmt

	;---- Southern Hemisphere
	centrlat  = -90.0
	orientlon = 0.0
	latmin_ps = -90.0
	latmax_ps = -45.0
	ind = where( lats ge latmin_ps and lats le latmax_ps )
	map_name = dirImg+prefix+yyyymmdd+'_'+prodId+'_'+chan_ids(ichan)+'_ps'+sfcTxt+cend+'.png'
	title = titleSatId+' S. H.'+prodTitle+channel+date+cendTxt+' (V' + version +')'
	plot_polar,prod1(ind),sfcs1(ind),map_name,minvalues(ichan),maxvalues(ichan),latmin_ps,latmax_ps,$
                   lonmin,lonmax,centrlat,orientlon,lats1(ind),lons1(ind),title,sfcPick,div,fmt

       endfor ; loop isfc ends

    endfor ; loop ichan ends

    ;---- release resource
    UNDEFINE, products

  endfor ; loop icend ends

ENDIF

seconds_finish = SYSTIME( /SECONDS )
print, 'Time used = ', seconds_finish-seconds_start, ' seconds'

End
