;$Id: MonitorNEDT.pro 1637 2008-11-07 20:38:03Z wchen $
@../../../setup/paths_idl.pro
;===============================================================
; Name:		MonitorNEDT
;
;
; Type:		IDL Subroutine
;
;
; Description:  Used to monitor the NEDT used by MIRS, day after day.
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- namelistNedt	      I		    namelist of input
;
;
; Subroutines needed:
;       - readErr
;
;
; History:
;       2005      Sid Ahmed Boukabara, 	IMSG Inc @ NOAA/NESDIS/ORA
;	2009	  Wanchun Chen 		modified to plot both small and big images,
;					add sensor id to make more generic
;
;===============================================================
PRO MonitorNEDT,namelist=namelist

satId='npp'
nedtList='/disk1/pub/mirs_npp/data/InputsData/npp_nedtDirs_atms_2011-11-09.list'
imgDir='./'

readFromList=1
if ( readFromList eq 1 ) then begin
  openr,iu,namelist,/get_lun
  readf,iu,format='(a)', satId                  ; Satellite ID
  readf,iu,format='(a)', nedtList               ; file list of NEDT files
  readf,iu,format='(a)', imgDir                 ; generated image location
  close,iu
  free_lun,iu,/force
endif

print, 'MonitorNEDT.pro...'
print, 'satId='+satId
print, 'nedtList='+nedtList
print, 'imgDir='+imgDir

prefix='nedtLife_'+satId

titleSat = STRUPCASE(satId)

NCHAN=20
titles=['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2',$
        '57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190h']
if satId eq 'npp' then begin
    NCHAN=22
    titles=['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2',$
            '57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
    titleSat = 'NPP/ATMS'
endif

;---- NEDT nominal values ( from: data/StaticData/NominalNedts/satId_NoiseFile.dat )
if satId eq 'n18' then $
NominalNedts = [0.235,0.182,0.277,0.168,0.190,0.144,0.168,0.195,0.165,0.204,$
                0.226,0.309,0.423,0.678,0.134,0.244,0.382,0.559,0.434,0.355]

if satId eq 'n19' then $
NominalNedts = [0.192,0.166,0.209,0.148,0.160,0.143,0.166,0.171,0.165,0.206,$
                0.221,0.310,0.424,0.678,0.135,0.252,0.400,0.582,0.455,0.354]

if satId eq 'metopA' then $
NominalNedts = [0.205,0.220,0.255,0.143,0.168,0.133,0.441,0.160,0.173,0.213,$
                0.246,0.329,0.451,0.740,0.111,0.228,0.369,0.507,0.406,0.359]

if satId eq 'metopB' then $
NominalNedts = [0.205,0.220,0.255,0.143,0.168,0.133,0.441,0.160,0.173,0.213,$
                0.246,0.329,0.451,0.740,0.111,0.228,0.369,0.507,0.406,0.359]

if satId eq 'npp' then $
NominalNedts = [0.210,0.200,0.210,0.200,0.220,0.260,0.250,0.530,0.560,0.390,0.380,$    
                0.350,0.340,0.450,0.260,0.220,0.190,0.190,1.230,1.180,0.860,0.580]

;----Set input filename
readlist,nedtList,listFiles,ndirs
nList         = n_elements(listFiles)
if nList lt 2 then begin
  print, 'Warning: only one nedt file, at least 2 nedt files are required, exit MonitorNEDT.pro.'
  exit
endif

date          = dblarr(nList)
flag          = make_array(nList,/integer,value=0)
computedOrNot = make_array(nList,nchan,/integer,value=0)
nDayEff = 0

;---- read in data
for iday=0,nList-1 do begin
    if (satID eq 'n18' or satId eq 'n19' or satId eq 'metopA' or satId eq 'metopB') then begin
        day=float(strmid(strtrim(file_basename(listFiles(iday))), 11,2,/reverse_offset))
        mnth=float(strmid(strtrim(file_basename(listFiles(iday))),14,2,/reverse_offset))
        yr=float(strmid(strtrim(file_basename(listFiles(iday))),  19,4,/reverse_offset))
    endif
    if (satId eq 'npp') then begin
        day=float(strmid(strtrim(file_basename(listFiles(iday))), 36,2,/reverse_offset))
        mnth=float(strmid(strtrim(file_basename(listFiles(iday))),38,2,/reverse_offset))
        yr=float(strmid(strtrim(file_basename(listFiles(iday))),  42,4,/reverse_offset))
    endif
    date(iday)=JULDAY(mnth,day,yr)
    readErr,listFiles(iday),cfreq,nedt,nchan,ComputedOrNotFlag
    nDayEff    = nDayEff+1
    if (nDayEff eq 1) then nedtTotal=fltarr(nList,nchan)
    nedtTotal(iDay,0:nchan-1) = nedt(0:nchan-1)
    computedOrNot(iDay,0:nchan-1) = ComputedOrNotFlag(0:nchan-1)
endfor

;---- sanity check
if date(nList-1) le date(0) then begin
  print, 'Warning: date range for axis has zero length, exit MonitorNEDT.pro'
  exit
endif

;---- define second set of variables to plot latest 31 days data
nList2         = 31
if nList lt 31 then nList2 = nList
date2          = dblarr(nList2)
flag2          = make_array(nList2,/integer,value=0)
computedOrNot2 = make_array(nList2,nchan,/integer,value=0)
nedtTotal2     = fltarr(nList2,nchan)

if nList ge nList2 then begin
  date2(*)            = date((nList-nList2):(nList-1))
  flag2(*)            = flag((nList-nList2):(nList-1))
  computedOrNot2(*,*) = computedOrNot((nList-nList2):(nList-1),*)
  nedtTotal2(*,*)     = nedtTotal((nList-nList2):(nList-1),*)
endif

if nList lt nList2 then begin
  date2(0:(nList-1))            = date(0:(nList-1))
  flag2(0:(nList-1))            = flag(0:(nList-1))
  computedOrNot2(0:(nList-1),*) = computedOrNot(0:(nList-1),*)
  nedtTotal2(0:(nList-1),*)     = nedtTotal(0:(nList-1),*)
endif

prefix2='nedtMonth_'+satId


;---- symbol definition
!p.charsize   = 0.75
csize	      = 16
a	      = findgen(csize+1) *(!pi*2./float(csize))
usersym,cos(a)/2,sin(a)/2,/fill
symsize       = 0.8

SET_PLOT,'Z'

FOR ISET = 0, 1 DO BEGIN  ; start Loop around time range

IF ISET EQ 1 THEN BEGIN
  UNDEFINE, date
  UNDEFINE, flag
  UNDEFINE, computedOrNot
  UNDEFINE, nedtTotal
  nList         = nList2
  date          = date2
  flag          = flag2
  computedOrNot = computedOrNot2
  nedtTotal     = nedtTotal2
  prefix        = prefix2
ENDIF


nMeans2Calc=fix(nList/10)
meanNEDTs=fltarr(nchan,nMeans2Calc+1)

FOR ichan=0, nchan-1 DO BEGIN
    ind=where(flag eq 0,ncount)
    ;---Get min/max values for channel
    ymin=min(nedtTotal(ind,ichan))
    ymax=max(nedtTotal(ind,ichan))
    ymean=mean(nedtTotal(ind,ichan))
    ;---Get y axis minimum
    ymin_plot=min(nedtTotal[ind,ichan])-stdev(nedtTotal[ind,ichan])
    ;---Get ymax for plotting
    cnt=0
    imean=0
    idx=where(computedOrNot[ind,ichan] eq 0,cntDefault)
    if (cntDefault eq 0) then idx=ind
    for ilist=0,n_elements(idx)-1,10 do begin
        if ilist lt n_elements(idx)-10  then cnt=cnt+10
        if ilist ge n_elements(idx)-10  then cnt=cnt+(n_elements(idx)-ilist-1)
        meanNEDTs[ichan,imean]=mean(nedtTotal[idx[ilist:cnt],ichan])
        imean=imean+1
    endfor
    ymax_plot=max(meanNEDTs[ichan,*])+4*stdev(nedtTotal[ind,ichan])
    ;if ymax_plot lt ymax then ymax_plot=max(meanNEDTs[ichan,*])+7*stdev(nedtTotal[ind,ichan])
    ;if ymax_plot lt ymax then ymax_plot=max(meanNEDTs[ichan,*])+10*stdev(nedtTotal[ind,ichan])
    if max(nedtTotal[ind,ichan]) lt 1 then ymax_plot=max(nedtTotal[ind,ichan])+2*stdev(nedtTotal[ind,ichan])

    ;---xmin/xmax
    xmin=min(date(ind))
    xmax=max(date(ind))
    ;---Set default NEDTs to -999
    idx = where(computedOrNot(ind,ichan) eq 1,cnt)
    if (cnt gt 0) then nedtTotal(ind(idx),ichan)=-999.
    ;---- we plot 3 sets of images
    for isize=0, 2 do begin
    	if ( isize eq 0 ) then begin
	    xticks = 6
	    device,set_resolution=[325, 250]
	    charsize=0.75
	    png_file = imgDir + prefix + '_ch' + strtrim(string(ichan+1),2) + '_small.png'
	endif
    	if ( isize eq 1 ) then begin
	    xticks = 6
	    device,set_resolution=[650, 500]
	    charsize=1.25
	    png_file = imgDir + prefix + '_ch' + strtrim(string(ichan+1),2) + '.png'
	endif
    	if ( isize eq 2 ) then begin
	    xticks = 18
	    device,set_resolution=[2000, 350]
	    charsize=1.25
	    png_file = imgDir + prefix + '_ch' + strtrim(string(ichan+1),2) + '_big.png'
	endif
	
	TVLCT, 0,0,0,         1 ; black
	TVLCT, 255,255,255, 255 ; white
	tit='MIRS ' + titleSat + ' NEDT Ch' + STRTRIM(ichan+1,2) + ' (' + titles(ichan) + ')'
    	dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])
	plot,date(ind),nedtTotal(ind,ichan),psym=-8,title=tit,color=1,background=255,$
    	     ytitle='NEDT [K]',yrange=[ymin_plot,ymax_plot],$ 
    	     XTICKUNITS = [ 'Day' ], XTICKFORMAT=['LABEL_DATE'], $
    	     xstyle=1,ystyle=1,xticks=xticks,CHARSIZE=charsize,MIN_VALUE=-98
	oplot,date, MAKE_ARRAY(nList, /Float, VALUE=NominalNedts(ichan)), linestyle=2, color=1
	
    	xyouts, 0.2, 0.85, 'max=' + strtrim(string(ymax, format='(F5.2)'),2), COLOR=1, CHARSIZE=charsize, CHARTHICK=1.0, /normal 
    	xyouts, 0.2, 0.80, 'mean='+ strtrim(string(ymean,format='(F5.2)'),2), COLOR=1, CHARSIZE=charsize, CHARTHICK=1.0, /normal
    	xyouts, 0.2, 0.75, 'spec='+ strtrim(string(NominalNedts(ichan),format='(F4.2)'),2), COLOR=1, CHARSIZE=charsize, CHARTHICK=1.0, /normal
    	  
    	TVLCT, r, g, b, /Get
    	Write_PNG, png_file, TVRD(), r, g, b

    endfor

ENDFOR

ENDFOR ; end Loop around time range

END
