@../../../setup/paths_idl.pro
;*******************************************************************************
;
;  Purpose:
;    To Plot NWP bias asymmetry images.
;
;  Dependence:
;    plot_line.pro
;    plot_line2.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ------------    --------------    ----------------------
;     09/01/2007       Wanchun Chen	Original Code
;     07/14/2009       Wanchun Chen	Add GFS branch
;     08/17/2011       Wanchun Chen	All array version
;     05/01/2011       Wanchun Chen	Further filtering using CLW and RR
;
;*******************************************************************************

Pro GridNwpAsym, nameList=nameList
Consts, Consts

;***************************************************************
;    identifiers
;***************************************************************
gridfactor=4
date='2012-04-29'
satId='n18'
dirGrid='/disk1/pub/mirs_oper/src/testbed/grid/data/'
dirImg='./img/'
version='3006'
dummy=''
biasPath='/disk1/pub/mirs_oper/data/SemiStaticData/biasCorrec/'
latmin=-90
latmax=90
lonmin=-180
lonmax=180
nwpData=1

fromNameList=1
if (fromNameList eq 1) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId                  ;Satellite ID
  readf,iu,format='(i)', gridfactor             ;gridfactor
  readf,iu,format='(a)', dirGrid                ;gridded data dir
  readf,iu,format='(a)', dirImg                 ;path where to put image files
  readf,iu,format='(a)', date    		;file extension of image
  readf,iu,format='(a)', version    		;version number
  readf,iu,format='(a)', dummy   		;pass dummy variable
  readf,iu,format='(a)', dummy   		;pass dummy variable
  readf,iu,format='(a)', biasPath    		;biasPath
  readf,iu,format='(f)', latmin              	;min lat
  readf,iu,format='(f)', latmax              	;max lat
  readf,iu,format='(f)', lonmin              	;min lon
  readf,iu,format='(f)', lonmax              	;max lon
  readf,iu,format='(i)', nwpData              	;nwp Data src ( 1-gdas, 2-ecmwf, 3-gfs )
  close,iu
  free_lun,iu,/force
endif

print, 'gridNwpAsym.pro ...'
print, 'in grid dir='+dirGrid
print, 'out img dir='+dirImg 

yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

NLAY=100
NLAY_PLOT=11
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

nwpId='_gdas_'
nwpTitle='GDAS '
if nwpData eq 2 then nwpId='_ecmwf_'
if nwpData eq 2 then nwpTitle='ECMWF '
if nwpData eq 3 then nwpId='_gfs_'
if nwpData eq 3 then nwpTitle='GFS '

prefix=rotate+satId+nwpId+'asym_glb_'


;***************************************************************
;    product id definition
;***************************************************************

;---- generic definiton ----
prodIds = [ 'clw', 'lwp', 'iwp', 'rr', 'swe', 'tpw', 'tskin', 'em', 'temp', 'wv', 'psfc' ]

;---- nwpData == 1 gdas case ----
if nwpData eq 1 then begin
   prodIds = [ 'swe', 'tpw', 'tskin', 'em', 'temp', 'wv', 'psfc' ]

  if satId eq Consts.STR_SATID_F16  or $
     satId eq Consts.STR_SATID_F17  or $
     satId eq Consts.STR_SATID_F18  or $
     satId eq Consts.STR_SATID_TRMM or $
     satId eq Consts.STR_SATID_GPM     then $
     prodIds = [ 'swe', 'tpw', 'tskin', 'wspd', 'em', 'temp', 'wv', 'psfc' ]
  if satId eq Consts.STR_SATID_MTMA then $
     prodIds = [ 'tpw', 'tskin', 'wspd', 'em', 'temp', 'wv' ]
  if satId eq Consts.STR_SATID_MTSA then $
     prodIds = [ 'tpw', 'tskin', 'em', 'temp', 'wv' ]

endif

;---- nwpData == 2 ecmwf case ----
if nwpData eq 2 then begin
   prodIds = [ 'clw', 'iwp', 'lwp', 'swe', 'tpw', 'tskin', 'em', 'temp', 'wv', 'psfc' ]

  if satId eq Consts.STR_SATID_F16  or $
     satId eq Consts.STR_SATID_F17  or $
     satId eq Consts.STR_SATID_F18  or $
     satId eq Consts.STR_SATID_TRMM or $
     satId eq Consts.STR_SATID_GPM     then $
     prodIds = [ 'clw', 'iwp', 'lwp', 'swe', 'tpw', 'tskin', 'wspd', 'em', 'temp', 'wv', 'psfc' ]
  if satId eq Consts.STR_SATID_MTMA then $
     prodIds = [ 'clw', 'iwp', 'lwp', 'tpw', 'tskin', 'wspd', 'em', 'temp', 'wv' ]
  if satId eq Consts.STR_SATID_MTSA then $
     prodIds = [ 'clw', 'iwp', 'lwp', 'tpw', 'tskin', 'em', 'temp', 'wv' ]

endif

;---- nwpData == 3 gfs case ----
if nwpData eq 3 then begin
   prodIds = [ 'rr', 'clw', 'swe', 'tpw', 'tskin', 'em', 'temp', 'wv', 'psfc' ]

  if satId eq Consts.STR_SATID_F16  or $
     satId eq Consts.STR_SATID_F17  or $
     satId eq Consts.STR_SATID_F18  or $
     satId eq Consts.STR_SATID_TRMM or $
     satId eq Consts.STR_SATID_GPM     then $
     prodIds = [ 'rr', 'clw', 'swe', 'tpw', 'tskin', 'wspd', 'em', 'temp', 'wv', 'psfc' ]
  if satId eq Consts.STR_SATID_MTMA then $
     prodIds = [ 'rr', 'clw', 'tpw', 'tskin', 'wspd', 'em', 'temp', 'wv' ]
  if satId eq Consts.STR_SATID_MTSA then $
     prodIds = [ 'clw', 'tpw', 'tskin', 'em', 'temp', 'wv' ]

endif


;***************************************************************
;    bin box definition
;***************************************************************

;---- default to N18/N19/MetopA/ATMS/MetopB
NBIN=30
BIN_BOX=4*findgen(NBIN+1)-57.5
xrange=[-60,60]
xtitle='Local Zenith Angle (degree)'

if satId eq Consts.STR_SATID_NPP then begin
  NBIN=35
  BIN_BOX=4*findgen(NBIN+1)-67.5
  xrange=[-70,70]
  xtitle='Local Zenith Angle (degree)'
endif

if satId eq Consts.STR_SATID_F16 or satId eq Consts.STR_SATID_F18 or $
     satId eq Consts.STR_SATID_F17 then begin
  NBIN=30
  BIN_BOX=findgen(NBIN)+1
  xrange=[1,30]
  xtitle='Scan Position'
endif

if satId eq Consts.STR_SATID_AQUA then begin
  NBIN=191
  BIN_BOX=findgen(NBIN)+1
  xrange=[1,191]
  xtitle='Scan Position'
endif

if satId eq Consts.STR_SATID_GCOMW1 then begin
  NBIN=243
  BIN_BOX=findgen(NBIN)+1
  xrange=[1,243]
  xtitle='Scan Position'
endif

if satId eq Consts.STR_SATID_TRMM then begin
  NBIN=26
  BIN_BOX=findgen(NBIN)+1
  xrange=[1,26]
  xtitle='Scan Position'
endif

if satId eq Consts.STR_SATID_GPM then begin
  NBIN=26
  BIN_BOX=findgen(NBIN)+1
  xrange=[1,26]
  xtitle='Scan Position'
endif

if satId eq Consts.STR_SATID_FY3RI then begin
  xrange=[0,120]
  xtitle='Scan Position'
  BIN_BOX=4*findgen(NBIN) + 2
endif

; MT MADRAS parameters currently set to those of proxy data
; Will need to be updated with real data
if satId eq Consts.STR_SATID_MTMA then begin
  NBIN=60
  BIN_BOX=findgen(NBIN)+1
;  xrange=[1,60]
  xrange=[1,27]
  xtitle='Scan Position'
endif

if satId eq Consts.STR_SATID_MTSA then begin
  NBIN=26
  BIN_BOX=4*findgen(NBIN+1)-54
  xrange=[-60,60]
  xtitle='Local Zenith Angle (degree)'
endif

;***************************************************************
;    define some tokens
;***************************************************************
sfcs = [ 'sea', 'lnd', 'all' ]
sfcTxts = [ 'Sea', 'Land', 'All Surface' ]

cends = [ 'as', 'ds' ]
cendtxts = [ ' Asc ', ' Des ' ]

conds = [ 'rainy', 'cloudy', 'clear', 'allcond' ]
condTxts = [ 'Rainy ', 'Cloudy ', 'Clear ', 'All Cond. ' ]

NCEND = N_ELEMENTS(cends)
NCOND = n_elements(conds)

;NSFC = N_ELEMENTS(sfcs)
NSFC = 5

NPROD = N_ELEMENTS(prodIds)

chans_str = ['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2',$
             '57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']

if satId eq Consts.STR_SATID_F16   then $
   chans_str = ['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h','19h',$
                '19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']	    

if satId eq Consts.STR_SATID_F17   then $
   chans_str = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h',$
                '19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']	    

if satId eq Consts.STR_SATID_F18   then $
   chans_str = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h','19h',$
                '19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']	    

if satId eq Consts.STR_SATID_AQUA  then $
   chans_str = ['7v','7h','11v','11h','19v','19h','24v','24h','37v','37h','89v','89h']

if satId eq Consts.STR_SATID_GCOMW1  then $
   chans_str = ['6v','6h','7v','7h','10v','10h','18v','18h','23v','23h','36v','36h','89v','89h']

if satId eq Consts.STR_SATID_TRMM  then $
   chans_str = ['11v','11h','19v','19h','21v','37v','37h','85v','85h']

if satId eq Consts.STR_SATID_GPM   then $
   chans_str = ['11v','11h','19v','19h','24v','37v','37h','89v','89h','166v','166h','183v1','183v2']

if satId eq Consts.STR_SATID_FY3RI then $
   chans_str = ['11v','11h','19v','19h','24v','24h','37v','37h','89v','89h']

if satId eq Consts.STR_SATID_NPP   then $
   chans_str = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2',$
                '57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']

if satId eq Consts.STR_SATID_MTMA  then $
   chans_str = ['19v','19h','24v','37v','37h','89v','89h','157v','157h']

if satId eq Consts.STR_SATID_MTSA  then $
   chans_str = ['183h','184h','186h','187h','190h','194h']

lays_ind = [43, 54, 62, 69, 75, 80, 84, 88, 90, 92, 94]
lays_str = ['100mb','200mb','300mb','400mb','500mb','600mb','700mb','800mb','850mb','900mb','950mb']


;***************************************************************
;    plot section
;***************************************************************
for iprod = 0, NPROD - 1 do begin
  
  prodId = prodIds(iprod)

  case prodId of

  'tskin':      begin
                    prodtxt     = 'Skin Temp. (K) '
                    nsfc_bot    = 0
                    nsfc_top    = 2
		    nlev        = 1
		    NLEV_PLOT   = nlev
                    levs_ind    = indgen(nlev)
		    levs_str    = ['']
		    yranges_min = [-10]
                    yranges_max = [10]
		end

  'psfc':       begin
                    prodtxt     = 'Sfc Pressure (mb) '
                    nsfc_bot    = 0
                    nsfc_top    = 2
		    nlev        = 1
		    NLEV_PLOT   = nlev
                    levs_ind    = indgen(nlev)
		    levs_str    = ['']
		    yranges_min = [-100]
                    yranges_max = [100]
		end

  'tpw':        begin
                    prodtxt     = 'TPW (mm) '
                    nsfc_bot    = 0
                    nsfc_top    = 2
		    nlev        = 1
		    NLEV_PLOT   = nlev
                    levs_ind    = indgen(nlev)
		    levs_str    = ['']
		    yranges_min = [-10]
                    yranges_max = [10]
		end

  'clw':        begin
                    prodtxt     = 'CLW (mm) '
                    nsfc_bot    = 0
                    nsfc_top    = 2
		    nlev        = 1
		    NLEV_PLOT   = nlev
                    levs_ind    = indgen(nlev)
		    levs_str    = ['']
		    yranges_min = [-0.10]
                    yranges_max = [0.10]
		end

  'iwp':        begin
                    prodtxt     = 'IWP (mm) '		    
                    nsfc_bot    = 0
                    nsfc_top    = 2
		    nlev        = 1
		    NLEV_PLOT   = nlev
                    levs_ind    = indgen(nlev)
		    levs_str    = ['']
		    yranges_min = [-0.10]
                    yranges_max = [0.10]
		end

  'lwp':        begin
                    prodtxt     = 'LWP (mm) '		
                    nsfc_bot    = 0	     
                    nsfc_top    = 0	     
		    nlev        = 1
		    NLEV_PLOT   = nlev
                    levs_ind    = indgen(nlev)
		    levs_str    = ['']
		    yranges_min = [-0.10]
                    yranges_max = [0.10]
		end

  'rr':         begin
                    prodtxt     = 'Rain Rate (mm/hr) '		     
                    nsfc_bot    = 0		 
                    nsfc_top    = 0		 
		    nlev        = 1
		    NLEV_PLOT   = nlev
                    levs_ind    = indgen(nlev)
		    levs_str    = ['']
		    yranges_min = [-1.0]
                    yranges_max = [1.0]
		end

  'swe':        begin
                    prodtxt     = 'SWE (cm) '
                    nsfc_bot    = 1
                    nsfc_top    = 1
		    nlev        = 1
		    NLEV_PLOT   = nlev
                    levs_ind    = indgen(nlev)
		    levs_str    = ['']
		    yranges_min = [-4.0]
                    yranges_max = [4.0]
		end

  'wspd':      begin
                    prodtxt     = 'Wind Speed (m/s) '
                    nsfc_bot    = 0
                    nsfc_top    = 0
		    nlev        = 1
		    NLEV_PLOT   = nlev
                    levs_ind    = indgen(nlev)
		    levs_str    = ['']
		    yranges_min = [-10]
                    yranges_max = [10]
                end

  'temp':      begin
		    NLEV        = NLAY
                    NLEV_PLOT   = NLAY_PLOT
                    yranges_min = replicate(-10,NLEV_PLOT)
                    yranges_max = replicate( 10,NLEV_PLOT)
                    prodtxt     = 'Temp. (K) '
                    nsfc_bot    = 0
                    nsfc_top    = 2
		    levs_ind    = lays_ind
                    levs_str    = lays_str
		end

  'wv':        begin
		    NLEV        = NLAY
                    NLEV_PLOT   = NLAY_PLOT
		    yranges_min = [-0.06,-0.06,-0.06,-0.06,-1.0,-1.0,-2.0,-2.0,-2.0,-2.0,-2.0]
                    yranges_max = [ 0.06, 0.06, 0.06, 0.06, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0]
                    prodtxt     = 'WV (g/kg) '
                    nsfc_bot    = 0
                    nsfc_top    = 2
		    levs_ind    = lays_ind
		    levs_str    = lays_str
		end

  'em':         begin
		    NLEV        = NCHAN
                    NLEV_PLOT   = NCHAN
		    yranges_min = replicate(-0.2,NLEV_PLOT)
                    yranges_max = replicate( 0.2,NLEV_PLOT)
                    prodtxt     = 'Emissivity '
                    nsfc_bot    = 0
                    nsfc_top    = 2
                    levs_ind    = indgen(NCHAN)
		    levs_str    = chans_str
		end

  else:         begin
                        print, 'Unsupported prodId: '+ prodId
                end
  endcase


  for icend = 0, NCEND - 1 do begin ; icend loop start
  
    cend = cends(icend)
    cendtxt = cendtxts(icend)
    
    fileAsym = dirGrid+'GRID_'+satId+nwpId+'asym_'+yyyymmdd+'_'+prodId+'_'+cend+'.dat'
    result = FILE_TEST(fileAsym)
    if result eq 0 then begin
      print, 'File not exist: ' + fileAsym
      continue 
    endif
    
    asym = fltarr(NBIN,NSFC,NCOND,NLEV)
   
    openr, lun, fileAsym, /get_lun, /Swap_Endian
    readu, lun, asym
    free_lun,lun,/force 
    
    for ilev = 0, NLEV_PLOT - 1 do begin ; ilev loop start
      
      level = '@ ' +  levs_str(ilev)
      
      for icond = 0, NCOND - 1 do begin ; icond loop start
        
        cond = conds(icond)
        condTxt = condTxts(icond)
        
        for isfc = nsfc_bot, nsfc_top do begin ; isfc loop start

	  sfc = sfcs(isfc)
	  sfcTxt = sfcTxts(isfc)
	  ytitle = prodtxt + 'Over ' + sfcTxt

	  if NLEV_PLOT gt 1 then begin $ 
	    title=condTxt+satTxt+' MIRS-'+nwpTitle+prodtxt+level+' '+date+cendTxt+'(r' + version +')'
	    file_png=dirImg+prefix+yyyymmdd+'_'+prodId+'_'+levs_str(ilev)+'_'+cond+'_'+sfc+'_'+cend+'.png'
	  endif else begin
	    title=condTxt+satTxt+' MIRS-'+nwpTitle+prodtxt+date+cendTxt+'(r' + version +')'
	    file_png=dirImg+prefix+yyyymmdd+'_'+prodId+'_'+cond+'_'+sfc+'_'+cend+'.png'
	  endelse

	  if isfc eq 0 then plot_line2,bin_box,asym(*,0,icond,levs_ind(ilev)),asym(*,1,icond,levs_ind(ilev)),$
	     xtitle,ytitle,title,xrange,[yranges_min(ilev),yranges_max(ilev)],file_png,'Sea','Sea Ice'

	  if isfc eq 1 then plot_line2,bin_box,asym(*,2,icond,levs_ind(ilev)),asym(*,3,icond,levs_ind(ilev)),$
	     xtitle,ytitle,title,xrange,[yranges_min(ilev),yranges_max(ilev)],file_png,'Land','Snow'

	  if isfc eq 2 then plot_line, bin_box,asym(*,4,icond,levs_ind(ilev)),xtitle, ytitle, title, $
	     xrange, [yranges_min(ilev),yranges_max(ilev)], file_png

        endfor ; isfc loop end
      
      endfor ; icond loop end
      
    endfor ; ilev loop end
    
  endfor ; icend loop end
 
endfor ; iprod loop end


End
