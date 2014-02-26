@../../../setup/paths_idl.pro

Pro Climate, namelist=namelist

satId = 'n18'
dir_in  = '/disk1/pub/archive/climate/n18_amsua_mhs/'
dir_out = './img/'
climateType = 0
climateList = 'pentad.list'

readFromList=1

if ( readFromList eq 1 ) then begin
  openr,iu,namelist,/get_lun
  readf,iu,format='(a)', satId                  ;Satellite ID
  readf,iu,format='(a)', dir_in                 ;avg data dir
  readf,iu,format='(a)', dir_out                ;dir to put images
  readf,iu,format='(i)', climateType            ;0-pentad,1-weekly,2-monthly
  readf,iu,format='(a)', climateList            ;climate  List
  close,iu
  free_lun,iu,/force
endif

NLAY  = 11
NCHAN = 20
if satId eq 'n18' or satId eq 'n19' or satId eq 'metopA' or satId eq 'metopB' then NCHAN = 20
if satId eq 'f16'  then NCHAN = 24 
if satId eq 'f17'  then NCHAN = 24 
if satId eq 'f18'  then NCHAN = 24 
if satId eq 'trmm' then NCHAN = 9
if satId eq 'npp'  then NCHAN = 22

titSat = STRUPCASE(satId)
if satId eq 'npp'  then titSat = 'NPP/ATMS'
if satId eq 'trmm' then titSat = 'TRMM/TMI'

gridSteps = [ 1.0, 2.5 ] 
degStrs   = [ '1.0deg', '2.5deg' ]

symSizes   = [ 1.5, 4.0 ]

lats_start = [ -89.0, -87.5 ]
lons_start = [ -179.0, -177.5 ]


latmin = -90.0
latmax = 90.0
lonmin = -180.0
lonmax = 180.0

deg = '!9'+String(37B)+'!X '
degTitles = [ '1'+deg, '2.5'+deg ]

rr_maxvals = [ 4.5, 4.5, 2.0 ]

climateTypeStrs = [ 'pentad', 'weekly', 'monthly' ]

position = [0.1, 0.2, 0.95, 0.9]
xsize = (position(2) - position(0)) * !D.X_VSize
ysize = (position(3) - position(1)) * !D.Y_VSize
xstart = position(0) * !D.X_VSize
ystart = position(1) * !D.Y_VSize

xtitle = 'Longitude'
ytitle = 'Latitude'

prodIds_sfc  = [ 'clw', 'gs', 'iwp', 'lwp', 'rr', 'rwp', 'sice', 'sicefy', 'sicemy', 'swe', 'tpw', 'tskin', 'sfc', 'snow' ]

prodTxts_sfc = [ 'CLW (mm)', 'Snow Grain Size Radius (mm)', 'Graupel Water Path (mm)', 'Liquid Water Path (mm)', 'Rain Rate (mm/hr)',$
                 'Rain Water Path (mm)', 'Sea Ice Concentration (%)', 'First Year Sea Ice Concentration (%)', 'Multiple Year Sea Ice Concentration (%)',$
		 'Snow Water Equivalent (cm)', 'TPW (mm)', 'Skin Temperature (K)', 'Surface Type', 'Snow Cover' ]

prodIds_chan = [ 'em', 'tbc', 'tbu' ]
prodTxts_chan = [ 'Emissivity', 'Corr. TB (K)', 'Un-Corr. TB (K)' ]

prodIds_polar = [ 'gs', 'sfc', 'sice', 'sicefy', 'sicemy', 'snow', 'swe' ]
prodTxts_polar = ['Snow Grain Size Radius (mm)', 'Surface Type','Sea Ice Concentration (%)', 'First Year Sea Ice Concentration (%)',$
                  'Multiple Year Sea Ice Concentration (%)', 'Snow Cover', 'Snow Water Equivalent (cm)' ]

prodIds_lay = [ 'temp', 'wv' ]                   
prodTxts_lay = [ 'Temp. (K)', 'WV (g/kg)' ]                   

nprod_sfc = n_elements(prodIds_sfc)
nprod_polar = n_elements(prodIds_polar)

minvals_sfc = fltarr(nprod_sfc) & minvals_sfc(1) = 0.3 & minvals_sfc(11) = 200
maxvals_sfc = [0.7, 0.7, 0.3, 0.7, 4.5, 0.7, 100, 100, 100, 8.0, 70, 320, 3, 3 ]
divs_sfc    = [ 7,   4,   6,   7,   5,   7,   5,   5,   5,   8,   7, 6,  3, 3 ]
fmts_sfc    = ['f4.1', 'f4.1', 'f4.1', 'f4.1','f4.1', 'f4.1','i3','i3','i3','i2','i2', 'i3','i1','i1' ]


minvals_polar = fltarr(nprod_polar) & minvals_polar(0) = 0.3
maxvals_polar = [ 0.7, 3, 100, 100, 100, 8.0, 3 ]
divs_polar    = [ 4,   3,  5,   5,   5,   8,  3 ]
fmts_polar    = [ 'f4.1', 'i1', 'i3', 'i3', 'i3', 'i2', 'i1' ]


FOR IDEG = 0, 1 DO BEGIN  ; start loop for 1.0/2.5 degree
   
    NCOL = long(360/gridSteps(ideg))
    NROW = long(180/gridSteps(ideg))
    
    sfcMask=BytArr(nCol,nRow)
    sfcPick=2

    tmp=fltarr(NCOL,NROW)
    
    longitude = lonmin + findgen(NCOL)*gridSteps(ideg)
    latitude  = latmin + findgen(NROW)*gridSteps(ideg)
    
    lats=fltarr(NCOL,NROW)
    for ilat=0,NROW-1 do begin
      lats(*,ilat) = lats_start[ideg] + gridSteps(ideg)*ilat
    endfor
    lons=fltarr(NCOL,NROW)
    for ilon=0,NCOL-1 do begin
      lons(ilon,*) = lons_start[ideg] + gridSteps(ideg)*ilon 
    endfor
    

    dateStr=''
    openr, iu, climateList, /get_lun
    while( not eof(iu) ) do begin

        readf, iu, format='(a)', dateStr
        
        date1 = strmid(dateStr,0,4)   + '/' + strmid(dateStr,5,2)  + '/' + strmid(dateStr,8,2)
        date2 = strmid(dateStr,11,4)  + '/' + strmid(dateStr,16,2) + '/' + strmid(dateStr,19,2)
	dateStrTitle= date1 + ' - ' + date2
        if climateType eq 2 then dateStrTitle = strmid(dateStr,0,4)   + '-' + strmid(dateStr,5,2)
	
       
        ;*******************************************
        ;open sfc data as sfs mask first
        ;*******************************************
    	avg_file = dir_in  + satId + '_sfc_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
    	openr, u, avg_file, /get_lun, /swap_endian, ERROR=err
	IF err NE 0 THEN BEGIN
	  print, 'files are missing: ', dateStr
	  GOTO, BAD
	ENDIF
    	readu, u, tmp
    	close, u
    	free_lun, u, /force
	
    	sfcMask = intarr(NCOL,NROW)
	for irow=0,NROW-1 do begin
	for icol=0,NCOL-1 do begin
	    sfcMask(icol,irow) = FIX(tmp(icol,irow))
	endfor
	endfor
	
 	;*******************************************
        ; 1-D variables
        ;*******************************************
	for iprod = 0, nprod_sfc-1 do begin
	    
	    prodId = prodIds_sfc[iprod]
            prodTxt = prodTxts_sfc[iprod]
	    
            avg_file = dir_in  + satId + '_'+prodId+'_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
	    if prodId eq 'snow' then avg_file = dir_in  + satId + '_'+'sfc'+'_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
	    ;print, avg_file
            openr, u, avg_file, /get_lun, /swap_endian
            readu, u, tmp
            free_lun, u, /force
            
	    png_file = dir_out + 'mirs_' + satId + '_' + prodId +'_avrg_' + degStrs(ideg) + '_' + dateStr + '.png'
	    if prodId eq 'sfc' or  prodId eq 'sice' or  prodId eq 'sicefy' or  prodId eq 'sicemy' or prodId eq 'snow' or  prodId eq 'swe' then $
	    png_file = dir_out + 'mirs_' + satId + '_' + prodId +'_avrg_' + degStrs(ideg) + '_' + dateStr + '_cyl.png'
	    if prodId eq 'tskin' then png_file = dir_out + 'mirs_' + satId + '_' + 'ts' +'_avrg_' + degStrs(ideg) + '_' + dateStr + '.png'
	    
    	    title = 'MIRS ' + titSat + ' ' + prodTxt +' ' + degTitles(ideg) + climateTypeStrs(climateType) + ' average ' + dateStrTitle
            minval = minvals_sfc[iprod]
            maxval = maxvals_sfc[iprod]
            div = divs_sfc[iprod]
            fmt = '('+ fmts_sfc[iprod] + ')'
    	    
	    if prodId eq 'sfc' or prodId eq 'snow' then begin
	        plot_sfc_grid, tmp, png_file, minval, maxval, latmin,latmax,lonmin,lonmax,title
	    endif else begin
	        plot_climate, tmp,title,xtitle,ytitle,png_file,latmin,latmax,lonmin,lonmax,longitude,latitude,position,xsize,ysize,xstart,ystart,minval,maxval,div,fmt,color_table_index=44
	    endelse
	    
	    ;sfcPick=2
            ;plot_grid, tmp,sfcMask,png_file,minval,maxval,latmin,latmax,lonmin,lonmax,title,sfcPick,div,fmt,color_table_index=44  
	 
	 endfor
	 
	 
 	;*******************************************
        ; polar plot
        ;*******************************************
	 for iprod = 0, nprod_polar-1 do begin   

	    prodId = prodIds_polar[iprod]
            prodTxt = prodTxts_polar[iprod]
           
            avg_file = dir_in  + satId + '_'+prodId+'_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
	    if prodId eq 'snow' then avg_file = dir_in  + satId + '_'+'sfc'+'_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
	    ;print, avg_file
            openr, u, avg_file, /get_lun, /swap_endian
            readu, u, tmp
            free_lun, u, /force
	   
	    minval = minvals_polar[iprod]
            maxval = maxvals_polar[iprod]
            div = divs_polar[iprod]
            fmt = '('+ fmts_polar[iprod] + ')'
	    
            ;---- polar north
	    sfcPick=1
	    centrlat  = 90.0
	    orientlon = -80.0
	    latmin_ps = 0.0
	    latmax_ps = 90.0
	    ind = where( lats ge latmin_ps and lats le latmax_ps )
	    png_file = dir_out + 'mirs_' + satId + '_'+prodId+'_avrg_' + degStrs(ideg) + '_' + dateStr + '_pn.png'
	    title = 'MIRS ' + titSat + ' NH ' + prodTxt + ' ' + degTitles(ideg) + climateTypeStrs(climateType) + ' average ' + dateStrTitle
	    plot_polar,tmp(ind),sfcMask(ind),png_file,minval,maxval,latmin_ps,latmax_ps,lonmin,lonmax,$
        	       centrlat,orientlon,lats(ind),lons(ind),title,sfcPick,div,fmt,symsize=symSizes(ideg)

            ;---- polar south
	    centrlat  = -90.0
	    orientlon = 0.0
	    latmin_ps = -90.0
	    latmax_ps = 0.0
	    ind = where( lats ge latmin_ps and lats le latmax_ps )
	    png_file = dir_out + 'mirs_' + satId + '_'+prodId+'_avrg_' + degStrs(ideg) + '_' + dateStr + '_ps.png'
	    title = 'MIRS ' + titSat + ' SH ' + prodTxt + ' ' + degTitles(ideg) + climateTypeStrs(climateType) + ' average ' + dateStrTitle
	    plot_polar,tmp(ind),sfcMask(ind),png_file,minval,maxval,latmin_ps,latmax_ps,lonmin,lonmax,$
        	       centrlat,orientlon,lats(ind),lons(ind),title,sfcPick,div,fmt,symsize=symSizes(ideg)
	 endfor
	
	
 	;*******************************************
        ;em
        ;*******************************************
	minvals = [0.45,0.45,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.60,0.60,0.60,0.65,0.65,0.65]
	maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
	divs = replicate(10,20)
	fmt = '(f4.2)'
	chans = ['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2','57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']

	if( satId eq 'f16' ) then begin
	  minvals = [0.50,0.50,0.50,0.50,0.65,0.65,0.50,0.60,0.65,0.60,0.65, 0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.50,0.50,0.50,0.50,0.50,0.50]
	  maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00, 1.00,1.00,1.00,1.00,1.00,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70]
	  divs    = replicate(10,24)
	  chans   = ['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h',$
        	     '19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']
	endif
	
	if( satId eq 'f17' ) then begin
	  minvals = [0.50,0.50,0.50,0.50,0.65,0.65,0.50,0.60,0.65,0.60,0.65, 0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.50,0.50,0.50,0.50,0.50,0.50]
	  maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00, 1.00,1.00,1.00,1.00,1.00,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70]
	  divs    = replicate(10,24)
	  chans   = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h',$
        	     '19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']
	endif
	
	if( satId eq 'f18' ) then begin
	  minvals = [0.50,0.50,0.50,0.50,0.65,0.65,0.50,0.60,0.65,0.60,0.65, 0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.50,0.50,0.50,0.50,0.50,0.50]
	  maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00, 1.00,1.00,1.00,1.00,1.00,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70]
	  divs    = replicate(10,24)
	  chans   = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h',$
        	     '19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']
	endif

	if( satId eq 'trmm' ) then begin
	  minvals = [0.50,0.20,0.50,0.25,0.55,0.60,0.25,0.70,0.40]
	  maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
	  divs    = replicate(10,9)
	  chans   = ['11v','11h','19v','19h','21v','37v','37h','85v','85h' ]
	endif

	if( satId eq 'npp' ) then begin
	  minvals = [0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
                     0.65,0.65,0.65,0.65,0.60,0.60,0.65,0.65,0.65,0.65,0.65]
	  maxvals = [1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,$
                     1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00]
	  divs    = replicate(10,22)
	  chans   = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2',$
                     '57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
	endif

	avg_file = dir_in  + satId + '_em_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
	openr, u, avg_file, /get_lun, /swap_endian
	for ichan = 0, NCHAN - 1 do begin
	    readu, u, tmp
	    png_file = dir_out + 'mirs_' + satId + '_em_avrg_' + degStrs(ideg) + '_' + dateStr + '_' + chans(ichan) + '.png'
	    title = 'MIRS ' + titSat + ' Emissivity @ ' + chans(ichan) + ' ' + degTitles(ideg) + climateTypeStrs(climateType) + ' avg ' + dateStrTitle
	    minval = minvals[ichan]
	    maxval = maxvals[ichan]
	    div = divs[ichan]
	    plot_climate, tmp, title, xtitle, ytitle, png_file, latmin,latmax,lonmin,lonmax,longitude,latitude,position,xsize,ysize,xstart,ystart,minval,maxval,div,fmt
	endfor
	close, u
	free_lun, u, /force
	
	
	;*******************************************
	;Corrected TB - ymCorr
	;*******************************************
	minvals = [140,140,170,170,200, 200,210,200,200,180, 180,180,180,200,150, 150,150,220,220,200]
	maxvals = [300,300,290,290,270, 250,230,230,230,230, 250,250,280,290,300, 300,300,265,280,300]
	divs = [8,8,6,6,7, 5,5,6,6,5, 7,10,7,9,5, 5,5,9,6,5]
	chans = ['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2','57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190v']
	fmt = '(I3)'

	if( satId eq 'f16' ) then begin
	  chans = ['50v','52v','53v','54v','55v','57rc','59rc','150h','190h','186h','184h',$
        	   '19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']
	  minvals = [ 200,210,215,210,200,200,210,150,200,200,225,100,140,140,100,140,140,100,205,200,230,225,220,215]
	  maxvals = [ 270,270,250,230,225,225,225,300,290,275,250,300,300,300,300,300,300,300,240,230,260,260,250,230]
	  divs =    [ 7, 6, 7, 4, 5, 5, 5, 5, 9, 5, 5, 10, 8, 8, 10, 8, 8, 10, 7, 6, 6, 7, 6, 5]
	endif
	if( satId eq 'f17' ) then begin
	  chans = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h',$
        	   '19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']
	  minvals = [ 200,210,215,210,200,200,210,150,200,200,225,100,140,140,100,140,140,100,205,200,230,225,220,215]
	  maxvals = [ 270,270,250,230,225,225,225,300,290,275,250,300,300,300,300,300,300,300,240,230,260,260,250,230]
	  divs =    [ 7, 6, 7, 4, 5, 5, 5, 5, 9, 5, 5, 10, 8, 8, 10, 8, 8, 10, 7, 6, 6, 7, 6, 5]
	endif
	if( satId eq 'f18' ) then begin
	  chans = ['50h','52h','53h','54h','55h','57rc','59rc','150h','190h','186h','184h',$
        	   '19h','19v','22v','37h','37v','91v','91h','63rc','60rc1','60rc2','60rc3','60rc4','60rc5']
	  minvals = [ 200,210,215,210,200,200,210,150,200,200,225,100,140,140,100,140,140,100,205,200,230,225,220,215]
	  maxvals = [ 270,270,250,230,225,225,225,300,290,275,250,300,300,300,300,300,300,300,240,230,260,260,250,230]
	  divs =    [ 7, 6, 7, 4, 5, 5, 5, 5, 9, 5, 5, 10, 8, 8, 10, 8, 8, 10, 7, 6, 6, 7, 6, 5]
	endif
	
	if( satId eq 'trmm' ) then begin
	  chans   = ['11v','11h','19v','19h','21v','37v','37h','85v','85h']
	  minvals = [ 125,   50,  150,   75,  175,  150,  100,  175,  125 ]
	  maxvals = [ 300,  300,  315,  315,  315,  315,  315,  315,  300 ]
	  divs    = [  7,    5,   11,    6,    7,    11,   5,    7,    7  ] 
	endif
	    
	if( satId eq 'npp' ) then begin
	  chans   = ['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2',$
                     '57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
	  
	  minvals = [140,140,170,170,170,200, 200,210,200,200,180, $
                     180,180,180,200,150,150, 220,220,220,200,200]
	  maxvals = [300,300,290,290,290,270, 250,230,230,230,230, $
                     250,250,280,290,300,300, 300,300,300,300,300]
	  divs    = [8,8,6,6,7, 5,5,6,6,5, 7,10,7,9,5, 5,5,9,6,5,6,6]
	endif
	    
	avg_file = dir_in  + satId + '_ymCorr_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
	openr, u, avg_file, /get_lun, /swap_endian
	for ichan = 0, NCHAN - 1 do begin
	    readu, u, tmp
	    png_file = dir_out + 'mirs_' + satId + '_tbc_avrg_' + degStrs(ideg) + '_' + dateStr + '_' + chans(ichan) + '.png'
	    title = 'MIRS ' + titSat + ' TB(K) @ ' + chans(ichan) + ' ' + degTitles(ideg) + climateTypeStrs(climateType) + ' avg ' + dateStrTitle
	    minval = minvals[ichan]
	    maxval = maxvals[ichan]
	    div = divs[ichan]
	    plot_climate,tmp,title,xtitle,ytitle,png_file,latmin,latmax,lonmin,lonmax,longitude,latitude,position,xsize,ysize,xstart,ystart,minval,maxval,div,fmt
	endfor
	close, u
	free_lun, u, /force


	;*******************************************
	;Un-Corrected TB - ym
	;*******************************************
	avg_file = dir_in  + satId + '_ym_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
	openr, u, avg_file, /get_lun, /swap_endian
	for ichan = 0, NCHAN - 1 do begin
	    readu, u, tmp
	    png_file = dir_out + 'mirs_' + satId + '_tbu_avrg_' + degStrs(ideg) + '_' + dateStr + '_' + chans(ichan) + '.png'
	    title = 'MIRS ' + titSat + ' Un-Corr. TB(K) @ ' + chans(ichan) + ' ' + degTitles(ideg) + climateTypeStrs(climateType) + ' avg ' + dateStrTitle
	    minval = minvals[ichan]
	    maxval = maxvals[ichan]
	    div = divs[ichan]
	    plot_climate, tmp,title,xtitle,ytitle,png_file,latmin,latmax,lonmin,lonmax,longitude,latitude,position,xsize,ysize,xstart,ystart,minval,maxval,div,fmt
	endfor
	close, u
	free_lun, u, /force

	
	;*******************************************
	;temp
	;*******************************************
	minvals = [200,200,210,220,230,240,250,250,255,255,255]
	maxvals = [230,230,260,270,280,285,290,290,300,300,305]
	divs = [6,6,10,10,10,9,8,8,9,9,10]
	fmt = '(I3)'
	layers = [ '100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb' ]
	avg_file = dir_in  + satId + '_temp_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
	openr, u, avg_file, /get_lun, /swap_endian
	for ilay = 0, NLAY - 1 do begin    
	  readu, u, tmp
	  png_file = dir_out + 'mirs_' + satId + '_temp_avrg_' + degStrs(ideg) + '_' + dateStr + '_' + layers(ilay) + '.png'
	  title = 'MIRS ' + titSat + ' Temp.(K) @ ' + layers(ilay) + ' ' + degTitles(ideg) + climateTypeStrs(climateType) + ' avg ' + dateStrTitle
	  minval = minvals[ilay]
	  maxval = maxvals[ilay]
	  div = divs[ilay]
	  plot_climate, tmp,title,xtitle,ytitle,png_file,latmin,latmax,lonmin,lonmax,longitude,latitude,position,xsize,ysize,xstart,ystart,minval,maxval,div,fmt
	endfor
	
	
	;*******************************************
	;wv
	;*******************************************
	minvals = [0.0,  0.0, 0.0, 0.0, 0.0, 0.0,  0.0,  0.0,  0.0,  0.0,  0.0]
	maxvals = [0.1, 0.15, 1.0, 2.0, 3.5, 6.0, 12.0, 12.0, 14.0, 14.0, 16.0]
	divs = [10,10,10,10,7,6,6,6,7,7,8]
	fmt = '(f5.2)'
	layers = [ '100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb' ]
	avg_file = dir_in  + satId + '_wv_' + degStrs(ideg) + '_' + STRTRIM(dateStr,2)
	openr, u, avg_file, /get_lun, /swap_endian
	for ilay = 0, NLAY - 1 do begin    
	  readu, u, tmp
	  png_file = dir_out + 'mirs_' + satId + '_wv_avrg_' + degStrs(ideg) + '_' + dateStr + '_' + layers(ilay) + '.png'
	  title = 'MIRS ' + titSat + ' WV(g/kg) @ ' + layers(ilay) + ' ' + degTitles(ideg) + climateTypeStrs(climateType) + ' avg ' + dateStrTitle
	  minval = minvals[ilay]
	  maxval = maxvals[ilay]
	  div = divs[ilay]
	  plot_climate, tmp,title,xtitle,ytitle,png_file,latmin,latmax,lonmin,lonmax,longitude,latitude,position,xsize,ysize,xstart,ystart,minval,maxval,div,fmt
	endfor
	
	
	BAD: 
    
    endwhile
    close, iu
    free_lun, iu, /force


ENDFOR  ; end loop for 1.0/2.5 degree 


end
