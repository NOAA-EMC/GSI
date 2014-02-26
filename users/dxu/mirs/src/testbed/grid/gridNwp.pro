@../../../setup/paths_idl.pro
;*******************************************************************************
;
;  Purpose:
;    To Plot NWP gridded data set into png images.
;
;  Dependence:
;    plot_grid.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ------------    --------------    ----------------------
;     09/01/2007       Wanchun Chen     Original Code
;     07/14/2009       Wanchun Chen     Add GFS branch
;     01/21/2011       Wanchun Chen     Use product IDs loops
;
;*******************************************************************************

Pro GridNwp, namelist=namelist
Consts, Consts

;*******************************************************************************
; identifiers to be read from namelist
;*******************************************************************************
gridfactor=4
date='2011-01-18'
satId='n18'
dirGrid='/home/pub/wchen/mirs_ifort_linux_x64/data/TestbedData/Outputs/grid/n18_amsua_mhs/2011-01-18/'
dirImg='./'
version='2333'
dummy=''
biasPath='/home/pub/wchen/mirs_ifort_linux_x64/data/SemiStaticData/biasCorrec/'
latmin=-90
latmax=90
lonmin=-180
lonmax=180
nwpData=1

readFromList=1
if ( readFromList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId           ;Satellite ID
  readf,iu,format='(i)', gridfactor      ;gridfactor
  readf,iu,format='(a)', dirGrid         ;gridded data dir
  readf,iu,format='(a)', dirImg          ;path where to put image files
  readf,iu,format='(a)', date    	 ;file extension of image
  readf,iu,format='(a)', version    	 ;Svn version control number
  readf,iu,format='(a)', dummy   	 ;pass dummy variable
  readf,iu,format='(a)', dummy   	 ;pass dummy variable
  readf,iu,format='(a)', biasPath    	 ;biasPath
  readf,iu,format='(f)', latmin          ;min lat
  readf,iu,format='(f)', latmax          ;max lat
  readf,iu,format='(f)', lonmin          ;min lon
  readf,iu,format='(f)', lonmax          ;max lon
  readf,iu,format='(i)', nwpData         ;nwp Data src ( 1-gdas, 2-ecmwf, 3-gfs )
  close,iu
  free_lun,iu,/force
endif

rotate='mirs_adv_poes_'
if satId eq Consts.STR_SATID_F16    then rotate='mirs_adv_dmsp_'
if satId eq Consts.STR_SATID_F17    then rotate='mirs_adv_dmsp_'
if satId eq Consts.STR_SATID_F18    then rotate='mirs_adv_dmsp_'
if satId eq Consts.STR_SATID_AQUA   then rotate='mirs_adv_eos_'
if satId eq Consts.STR_SATID_GCOMW1 then rotate='mirs_adv_eos_'
if satId eq Consts.STR_SATID_FY3RI  then rotate='mirs_adv_eos_'
if satId eq Consts.STR_SATID_NPP    then rotate='mirs_adv_npoess_'
if satId eq Consts.STR_SATID_TRMM   then rotate='mirs_adv_eos_'
if satId eq Consts.STR_SATID_GPM    then rotate='mirs_adv_eos_'
if satId eq Consts.STR_SATID_MTMA   then rotate='mirs_adv_mt_'
if satId eq Consts.STR_SATID_MTSA   then rotate='mirs_adv_mt_'

nwpId='_gdas_'
nwpTitle='GDAS Collocated '
if ( nwpData eq 2 ) then begin
  nwpId='_ecmwf_'
  nwpTitle='ECMWF Collocated '
endif
if ( nwpData eq 3 ) then begin
  nwpId='_gfs_'
  nwpTitle='GFS Collocated '
endif

print, 'gridNwp.pro ... ' + nwpTitle 
print, 'in grid dir='+dirGrid 
print, 'out img dir='+dirImg 

prefix=rotate+satId+nwpId+'glb_'

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


;*******************************************************************************
;    define products ID array to be plotted
;*******************************************************************************

prodIDs_chn = [ 'em' ]
prodIDs_lay = [ 'temp','wv' ]

prodIDs_sfc = [ 'chisq','clw','iwp','psfc','rr','sfc','swe','tpw','tskin' ]

;---- GDAS set ----
if nwpData EQ 1 then begin
  prodIDs_sfc = [ 'chisq','psfc','sfc','swe','tpw','tskin','wspd' ]
  if satId eq Consts.STR_SATID_MTSA then $
    prodIDs_sfc = [ 'chisq','psfc','sfc','swe','tpw','tskin' ]
endif

;---- ECMWF set ----
if nwpData EQ 2 then begin
  prodIDs_sfc = [ 'chisq','iwp','clw','psfc','sfc','tpw','tskin','wspd' ]
  if satId eq Consts.STR_SATID_MTSA then $
    prodIDs_sfc = [ 'chisq','iwp','clw','psfc','sfc','tpw','tskin' ]
endif

;---- GFS set ----
if nwpData EQ 3 then begin
  prodIDs_sfc = [ 'chisq','clw','iwp','psfc','rr','sfc','tpw','tskin','wspd' ]
  if satId eq Consts.STR_SATID_MTSA then $
    prodIDs_sfc = [ 'chisq','clw','iwp','psfc','rr','sfc','tpw','tskin' ]
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
cends = [ 'as', 'ds' ]
cendtxts = [ ' Asc ', ' Des ' ]

ncend = N_ELEMENTS(cends)
nsfc = N_ELEMENTS(sfcs)


;*******************************************************************************
; sfc products section
;*******************************************************************************

nprod_sfc = N_ELEMENTS(prodIDs_sfc)
for iprod = 0, nprod_sfc - 1 do begin
  
  prodId = prodIDs_sfc(iprod)
  print, 'plotting ' + prodId + ' ...'
  case prodId of

  'chisq':      begin
                    minvalue = 0
                    maxvalue = 10
                    div      = 10
                    fmt      = '(I2)'
                    prodtxt  = ' Chi Square '
                    ctable   = 33
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'tskin':      begin
                    minvalue = 200
                    maxvalue = 325
                    div      = 5
                    fmt      = '(I3)'
                    prodtxt  = ' Skin Temperature (K) '
                    ctable   = 33
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'tpw':        begin
                    minvalue = 0
                    maxvalue = 70
                    div      = 14
                    format   = '(I2)'
                    prodtxt  = ' TPW (mm) '
                    ctable   = 41
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'clw':        begin
                    minvalue = 0.0
                    maxvalue = 0.7
                    div      = 7
                    fmt      = '(f5.2)'
                    prodtxt  = ' CLW (mm) '
                    ctable   = 41
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'iwp':        begin
                    minvalue = 0.0
                    maxvalue = 0.2
                    div      = 4
                    fmt      = '(f5.2)'
                    prodtxt  = ' IWP (mm) '		    
                    ctable   = 41
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  'lwp':        begin
                    minvalue = 0.0
                    maxvalue = 0.7
                    div      = 7
                    fmt      = '(f5.2)'
                    prodtxt  = ' LWP (mm) '		
                    ctable   = 41
                    nsfc_bot = 0	     
                    nsfc_top = 2	     
		end
		
  'psfc':       begin
                    minvalue = 800
                    maxvalue = 1050
                    div      = 10
                    fmt      = '(I4)'
                    prodtxt  = ' Surface Pressure (mb) '		     
                    ctable   = 33
                    nsfc_bot = 0		 
                    nsfc_top = 2		 
		end

  'rr':         begin
                    minvalue = 0.0
                    maxvalue = 10.0
                    div      = 5
                    fmt      = '(f4.1)'
                    prodtxt  = ' Rain Rate (mm/hr) '		     
                    ctable   = 41
                    nsfc_bot = 0		 
                    nsfc_top = 2		 
		end

  'sfc':        begin
                    minvalue = 0
                    maxvalue = 3
                    div      = 3
                    fmt      = '(I2)'
                    prodtxt  = ' Surface Type '		     
                    ctable   = 33
                    nsfc_bot = 0		 
                    nsfc_top = 2		 
		end

  'swe':        begin
                    minvalue = 0
                    maxvalue = 8
                    div      = 8
                    fmt      = '(I2)'
                    prodtxt  = ' Snow Water Equivalent (cm) '
                    ctable   = 33
                    nsfc_bot = 1
                    nsfc_top = 1
		end

  'wspd':      begin
                    minvalue = 0
                    maxvalue = 26
                    div      = 13
                    fmt      = '(I2)'
                    prodtxt  = ' Wind Speed (m/s) '
                    ctable   = 41
                    nsfc_bot = 0
                    nsfc_top = 0
                end

  else:         begin
                        print, 'Unsupported sfc prodId: '+ prodId
                end
  endcase

  for icend = 0, ncend - 1 do begin
    cend = cends(icend)
    cendtxt = cendtxts(icend)

    fileGrid = dirGrid + 'GRID_'+satId+nwpId+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
      continue 
    endif
    
    products = fltarr(NCOL,NROW)
    openr, lun, fileGrid, /get_lun, /Swap_Endian
    readu, lun, products
    free_lun,lun,/force 
    
    for isfc = nsfc_bot, nsfc_top do begin
      sfcPick = isfc
      sfc = sfcs(isfc)
      map_name = dirImg + prefix + yyyymmdd + '_' + prodId + '_' + sfc + '_' + cend + '.png'
      title = nwpTitle + satTxt + prodtxt + date + cendtxt + '(V' + version +')'
      plot_grid, products,sfcMask,map_name,minvalue,maxvalue,latmin,latmax,lonmin,$
                 lonmax,title,sfcPick,div,fmt,color_table_index=ctable
    endfor
    
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
  print, 'plotting ' + prodId + ' ...'
  
  case prodId of
  
  'temp':      begin
                    minvalues = [200,200,210,220,230,240,250,250,255,255,255]
                    maxvalues = [230,230,260,270,280,285,290,290,300,300,305]
                    divs      = [6,6,10,10,10,9,8,8,9,9,10]
                    fmt       = '(I3)'
                    prodtxt   = ' Temperature (K) '
                    nsfc_bot  = 0
                    nsfc_top  = 2
		end

  'wv':        begin
                    minvalues = replicate(0.0,nlay_plot)
                    maxvalues = [0.1, 0.15, 1.0, 2.0, 3.5, 6.0, 12.0, 12.0, 14.0, 14.0, 16.0]
                    divs      = [10,10,10,10,7,6,6,6,7,7,8]
                    format    = '(f5.2)'
                    prodtxt   = ' Water Vapor(g/kg) '
                    nsfc_bot  = 0
                    nsfc_top  = 2
		end

  else:         begin
                        print, 'Unsupported layer prodId: '+ prodId
                end
  endcase

  for icend = 0, ncend - 1 do begin ; icend loop start
    cend = cends(icend)
    cendtxt = cendtxts(icend)
    
    fileGrid = dirGrid + 'GRID_'+satId+nwpId+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
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
    
    for ilay = 0, nlay_plot - 1 do begin ; ilay loop start
      
      layer = 'at ' + titles[ilay]
      sb = fltarr(NCOL,NROW)
      sb = products(*,*,layers_index(ilay))
    
      for isfc = nsfc_bot, nsfc_top do begin ; isfc loop start
	sfcPick = isfc
	sfc = sfcs(isfc)
	map_name = dirImg + prefix + yyyymmdd + '_' + prodId $
	         + '_' + titles[ilay]+'_'+ sfc + '_' + cend + '.png'
	title = nwpTitle + satTxt + prodtxt $
	      + layer + ' ' + date + cendtxt + '(V' + version +')'
	plot_grid, sb,sfcMask,map_name,minvalues[ilay],maxvalues[ilay],latmin,latmax,$
                   lonmin,lonmax,title,sfcPick,divs[ilay],fmt
      endfor ; isfc loop end
    
    endfor ; ilay loop end
    
  endfor ; icend loop end
 
endfor ; iprod loop end



;*******************************************************************************
; channel products section ( only emissivity here )
;*******************************************************************************
divs = replicate(10,NCHAN)

;---- use POES as default ones ( N18/N19/MetopA/MetopB )
titles=['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2',$
        '57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']
mins_sea  = [0.40,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
	     0.50,0.50,0.50,0.50,0.60,0.60,0.60,0.65,0.65,0.65]
maxs_sea  = [0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,$
	     0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95]
mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
	     0.65,0.65,0.65,0.65,0.60,0.60,0.60,0.60,0.60,0.60]
maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
             1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]

if ( satId eq Consts.STR_SATID_F16 ) then begin
  titles   = ['50v','52v','53v','54v','55v','57rc','59rc','150h',$
              '190h','186h','184h','19h','19v','22v','37h','37v',$
              '91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']	 
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

if ( satId eq Consts.STR_SATID_F17 ) then begin
  titles   = ['50h','52h','53h','54h','55h','57rc','59rc','150h',$
              '190h','186h','184h','19h','19v','22v','37h','37v',$
              '91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']     
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

if ( satId eq Consts.STR_SATID_F18 ) then begin
  titles   = ['50h','52h','53h','54h','55h','57rc','59rc','150h',$
              '190h','186h','184h','19h','19v','22v','37h','37v',$
              '91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']     
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

if ( satId eq Consts.STR_SATID_AQUA ) then begin
  titles   = ['7v', '7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h' ]
  mins_sea = [0.65, 0.65, 0.65, 0.50, 0.70, 0.60, 0.50, 0.60, 0.60, 0.50, 0.65, 0.65 ]
  maxs_sea = [1.00, 1.00, 1.00, 0.65, 0.80, 0.80, 0.85, 0.80, 0.85, 0.80, 0.80, 0.85 ]
  mins_lnd = [0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.80, 0.90, 0.90, 0.70, 0.75, 0.75 ]
  maxs_lnd = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00 ]
endif

if ( satId eq Consts.STR_SATID_GCOMW1 ) then begin
  titles   = ['6v', '6h', '7v', '7h','10v','10h','18v','18h','23v','23h','36v','36h','89v','89h' ]
  mins_sea = [0.65, 0.65, 0.65, 0.65, 0.65, 0.50, 0.70, 0.60, 0.50, 0.60, 0.60, 0.50, 0.65, 0.65 ]
  maxs_sea = [1.00, 1.00, 1.00, 1.00, 1.00, 0.65, 0.80, 0.80, 0.85, 0.80, 0.85, 0.80, 0.80, 0.85 ]
  mins_lnd = [0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.80, 0.90, 0.90, 0.70, 0.75, 0.75 ]
  maxs_lnd = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00 ]
endif

if ( satId eq Consts.STR_SATID_TRMM ) then begin
  titles   = ['11v','11h','19v','19h','21v','37v','37h','85v','85h']
  mins_sea = [0.50, 0.20, 0.55, 0.25, 0.55, 0.60, 0.25, 0.70, 0.40]
  maxs_sea = [0.60, 0.40, 0.60, 0.45, 0.60, 0.65, 0.45, 0.80, 0.50]
  mins_lnd = [0.80, 0.65, 0.80, 0.65, 0.80, 0.90, 0.70, 0.80, 0.75]
  maxs_lnd = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
endif

if ( satId eq Consts.STR_SATID_GPM ) then  begin
  titles   = [ '11v','11h','19v','19h','24v','37v','37h','89v','89h','166v','166h','183v1','183v2']
  mins_sea = [0.55,0.25,0.58,0.25,0.60,0.65,0.25,0.75,0.45,0.45,0.45,0.45,0.45]
  maxs_sea = [0.63,0.35,0.68,0.45,0.70,0.75,0.50,0.90,0.75,0.75,0.75,0.75,0.75]
  mins_lnd = [0.70,0.65,0.70,0.65,0.80,0.90,0.70,0.75,0.75,0.75,0.75,0.75,0.75]
  maxs_lnd = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
endif

if ( satId eq Consts.STR_SATID_FY3RI ) then begin
  titles   = ['11v','11h','19v','19h','24v','24h','37v','37h','89v','89h']
  mins_sea = [0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15 ]
  maxs_sea = [0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.70, 0.75, 0.75 ]
  mins_lnd = [0.65, 0.65, 0.65, 0.65, 0.80, 0.90, 0.90, 0.70, 0.75, 0.75 ]
  maxs_lnd = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00 ]
endif

if ( satId eq Consts.STR_SATID_NPP ) then begin
  titles   = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2',$
              '57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
  mins_sea  = [0.40,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
	       0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
  maxs_sea  = [0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,$
	       0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95]
  mins_lnd  = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
	       0.65,0.65,0.65,0.65,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60]
  maxs_lnd  = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
               1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
endif

if ( satId eq Consts.STR_SATID_MTMA ) then begin
  titles   = [ '19v','19h','24v','37v','37h','89v','89h','157v','157h']
  mins_sea = [0.55, 0.25, 0.55, 0.60, 0.25, 0.70, 0.40, 0.70, 0.40]
  maxs_sea = [0.60, 0.45, 0.60, 0.65, 0.45, 0.80, 0.50, 0.80, 0.50]
  mins_lnd = [0.80, 0.65, 0.80, 0.90, 0.70, 0.80, 0.75, 0.80, 0.75]
  maxs_lnd = [1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00]
endif

;---- MT SAPHIR 
if ( satId eq Consts.STR_SATID_MTSA ) then begin
  titles=['183h','184h','186h','187h','190h','194h']
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


nprod_chn = N_ELEMENTS(prodIDs_chn)
for iprod = 0, nprod_chn - 1 do begin ; iprod loop start
  
  prodId = prodIDs_chn(iprod)
  print, 'plotting ' + prodId + ' ...'
  
  case prodId of
  
  'em':         begin
                    fmt      = '(f5.2)'
                    prodtxt  = ' Emissivity '
                    ctable   = 33
                    nsfc_bot = 0
                    nsfc_top = 2
		end

  else:         begin
                        print, 'Unsupported channel prodId: '+ prodId
                end
  endcase

  for icend = 0, ncend - 1 do begin ; icend loop start
    cend = cends(icend)
    cendtxt = cendtxts(icend)
    
    fileGrid = dirGrid+'GRID_'+satId+nwpId+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileGrid)
    if result eq 0 then begin
      print, 'File not exist: ' + fileGrid
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
    
    for ichan = 0, NCHAN - 1 do begin ; ichan loop start
      
      channel = 'at ' + titles[ichan]
      sb = fltarr(NCOL,NROW)
      sb = products(*,*,ichan)
    
      for isfc = nsfc_bot, nsfc_top do begin ; isfc loop start
	sfcPick = isfc
	sfc = sfcs(isfc)
	
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
	
	map_name = dirImg + prefix + yyyymmdd + '_' + prodId + '_' + $
	           titles[ichan]+'_'+ sfc + '_' + cend + '.png'
	title = nwpTitle + satTxt + prodtxt + $
	        channel + ' ' + date + cendtxt + '(V' + version +')'
	plot_grid, sb,sfcMask,map_name,minvalues[ichan],maxvalues[ichan],latmin,latmax,$
                   lonmin,lonmax,title,sfcPick,divs[ichan],fmt,color_table_index=ctable   
      endfor ; isfc loop end
    
    endfor ; ichan loop end
    
  endfor ; icend loop end
 
endfor ; iprod loop end


End


