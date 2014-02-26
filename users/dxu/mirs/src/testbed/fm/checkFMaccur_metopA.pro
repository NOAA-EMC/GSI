@../../../setup/paths_idl.pro
; Program used to take a quick look at accuracy of the FM process
;
; Sid-Ahmed Boukabara. 2005.
; 08/09/2007	Wanchun Chen, modified to plot png format instead PS file 
;

PRO checkFMaccur_metopA, QCcheckPath, satId, date, processMode

;----Set input filename
;QCcheckPath='../../../data/TestbedData/PerfsMonitoring/metopA_amsua_mhs/'
;satId='n18'
;date='2007-08-09'

if ( processMode eq 1 ) then begin
  QCcheckDir=QCcheckPath+"/"+date
  yyyymmdd=strmid(date,0,4) + strmid(date,5,2) + strmid(date,8,2)
  prefix='mirs_adv_poes_'+satId+'_fm_glb_'+yyyymmdd
endif

if ( processMode eq 0 ) then begin
  QCcheckDir=QCcheckPath
  yyyymmdd=date
  prefix='mirs_adv_poes_'+satId+'_fm_glb_'+yyyymmdd
endif


if ( satId eq 'n18'    ) then satName='N18'
if ( satId eq 'metopA' ) then satName='MetOp-A'


list=FILE_SEARCH(QCcheckPath,'QCcheck[0-1]*')

nList      =n_elements(List)
nscanL     =0L
nFovs      =0L
np         =0L
mxScanL    =3000
mxp        =20
FMerrTot   =fltarr(nList,mxScanL,90,mxp)
nScanLTot  =lonarr(nList)
Err        =fltarr(nList,90,6)
Err_as     =fltarr(nList,90,6)
Err_ds     =fltarr(nList,90,6)

FOR ifile=0,nList-1 DO BEGIN
    openr,iu,list[ifile],/get_lun;,/f77_unformatted;,/swap_endian
    readf,iu,format='(3i10)',nScanL,nFovs,np
    ;print,nFovs,np 
    FMerr=fltarr(nScanL,nFovs,np)
    readf,iu,format='(11f12.5)',FMerr
    close,iu
    nScanLTot(ifile) = nScanL
    FMerrTot(ifile,0:nScanL-1,0:nFovs-1,0:np-1) = FMerr(0:nScanL-1,0:nFovs-1,0:np-1)
    FOR ifov=0,nFovs-1 DO BEGIN
	
	;---- combined
        ind=where(FMerr(0:nScanL-1,ifov,3) eq 0  and FMerr(0:nScanL-1,ifov,0) gt 0. $
              and FMerr(0:nScanL-1,ifov,1) gt 0. )
        Err(ifile,ifov,0) = mean(FMerr(ind,ifov,0)-FMerr(ind,ifov,1))
        Err(ifile,ifov,1) = stdev(FMerr(ind,ifov,0)-FMerr(ind,ifov,1))
        Err(ifile,ifov,2) = mean(FMerr(ind,ifov,2))
        Err(ifile,ifov,3) = mean(FMerr(ind,ifov,0))
        Err(ifile,ifov,4) = mean(FMerr(ind,ifov,1))
	
	;---- ascending
        ind_as=where(FMerr(0:nScanL-1,ifov,3) eq 0  and FMerr(0:nScanL-1,ifov,0) gt 0. $
                 and FMerr(0:nScanL-1,ifov,1) gt 0. and FMerr(0:nScanL-1,ifov,9) eq 0)
	
	;print, FMerr(0:nScanL-1,ifov,9)
		 
	if ( n_elements(ind_as) gt 1 ) then begin
        	Err_as(ifile,ifov,0) = mean(FMerr(ind_as,ifov,0)-FMerr(ind_as,ifov,1))
        	Err_as(ifile,ifov,1) = stdev(FMerr(ind_as,ifov,0)-FMerr(ind_as,ifov,1))
        	Err_as(ifile,ifov,2) = mean(FMerr(ind_as,ifov,2))
        	Err_as(ifile,ifov,3) = mean(FMerr(ind_as,ifov,0))
        	Err_as(ifile,ifov,4) = mean(FMerr(ind_as,ifov,1))
	endif
	
	;---- descending
        ind_ds=where(FMerr(0:nScanL-1,ifov,3) eq 0  and FMerr(0:nScanL-1,ifov,0) gt 0. $
                 and FMerr(0:nScanL-1,ifov,1) gt 0. and FMerr(0:nScanL-1,ifov,9) eq 1)
	if ( n_elements(ind_ds) gt 1 ) then begin
        	Err_ds(ifile,ifov,0) = mean(FMerr(ind_ds,ifov,0)-FMerr(ind_ds,ifov,1))
        	Err_ds(ifile,ifov,1) = stdev(FMerr(ind_ds,ifov,0)-FMerr(ind_ds,ifov,1))
        	Err_ds(ifile,ifov,2) = mean(FMerr(ind_ds,ifov,2))
        	Err_ds(ifile,ifov,3) = mean(FMerr(ind_ds,ifov,0))
        	Err_ds(ifile,ifov,4) = mean(FMerr(ind_ds,ifov,1))
	endif
	
        ;---fill in the scan position as well
        FMerrTot(ifile,0:nScanL-1,ifov,np)=ifov+1
	
    ENDFOR
ENDFOR


;---- write out mean
openw,  iu, QCcheckDir+'/QCcheck_mean_'+yyyymmdd+'.dat', /get_lun
writeu, iu, nList, nFovs
writeu, iu, Err_as(*,*,0),Err_ds(*,*,0)
close,iu

;---- write out stdv
openw,  iu, QCcheckDir+'/QCcheck_stdv_'+yyyymmdd+'.dat', /get_lun
writeu, iu, nList, nFovs
writeu, iu, Err_as(*,*,1),Err_ds(*,*,1)
close,iu



sels: 
    impr = 0

    sz      = n_elements(FMerrTot(*,*,*,7))
    tb89_A  = reform(FMerrTot(*,*,*,0) ,[sz])
    tb89_B  = reform(FMerrTot(*,*,*,1) ,[sz])
    xtab    = tb89_A-tb89_B
    qc      = reform(FMerrTot(*,*,*,3),[sz])
    xLat    = reform(FMerrTot(*,*,*,7),[sz])
    xLon    = reform(FMerrTot(*,*,*,8),[sz])
    Direc   = reform(FMerrTot(*,*,*,9),[sz])
    Day     = reform(FMerrTot(*,*,*,10),[sz])
    ScanPos = reform(FMerrTot(*,*,*,np),[sz])
    ;----Latitude/Longitude ROI
    ind     = where(qc eq 0 and tb89_A gt 0 and tb89_B gt 0)

    minLat  = min(xLat(ind))
    maxLat  = max(xLat(ind))
    minlon  = min(xLon(ind))
    maxlon  = max(xLon(ind))
    
    ;---Y range
    ;minY    = min(xtab(ind))
    ;maxY    = max(xtab(ind))
    minY    = -5.
    maxY    =  5.
    
    ;---Predominant Day 
    mDay=mean(fix(day(where(qc eq 0 and day gt 0))))
    
imp:
    
    ;---- device setup
    set_plot,'z'
    Loadct,39
    TVLCT, r, g, b, /Get
    r(0)=255&g(0)=255&b(0)=255
    r(255)=0&g(255)=0&b(255)=0
    
    !p.font=1
    !p.charsize=0.8
    csize         = 16
    a             = findgen(csize+1) *(!pi*2./float(csize))
    usersym,cos(a)/2,sin(a)/2,/fill
    

    plot_DeltaTB89_std_mean   = 0
    plot_DeltaTB89_meanOnly   = 1
    plot_DeltaTB89_stdvOnly   = 1
    plot_DeltaTB89_assym      = 0
    plot_DeltaTB89_vs_ScanPos = 0
    plot_map_deltaTB          = 1
    plot_map_TB_AMSUA         = 0
    plot_map_TB_MHS           = 0



    IF (plot_map_deltaTB eq 1) THEN BEGIN
        erase
        !p.multi=1
        minY    = -5.
        maxY    = 5.
	
	cend='as'
        ind     = where(qc eq 0 and tb89_A gt 0. and tb89_B gt 0. and Direc eq 0 and fix(day) eq fix(mday)) 
        tit     = satName +' TB Difference @ 89 GHz (AMSUA-MHS) ' + date
        mapPlot,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,xtab,'[K]',0.6,8,1,0,'(f7.2)'
        map_continents,/continents,/noborder,/hires,fill_continents=0,color=18
	thisImage = TVRD()
	png_file = QCcheckDir+"/"+prefix + '_delta_' + cend + '.png'
	Write_PNG, png_file, thisImage, r, g, b

        erase
	cend='ds'
        ind     = where(qc eq 0 and tb89_A gt 0. and tb89_B gt 0. and Direc eq 1 and fix(day) eq fix(mday)) 
        tit     = satName +' TB Difference @ 89 GHz (AMSUA-MHS) ' + date
        mapPlot,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,xtab,'[K]',0.6,8,1,0,'(f7.2)'
        map_continents,/continents,/noborder,/hires,fill_continents=0,color=18
	thisImage = TVRD()
	png_file = QCcheckDir+"/"+prefix + '_delta_' + cend + '.png'
	Write_PNG, png_file, thisImage, r, g, b
    ENDIF


    IF (plot_DeltaTB89_stdvOnly eq 1) THEN BEGIN
        ;-----------------
        erase
        !p.multi=1
        x=findgen(nFovs)+1
        ymin=0.
        ;ymax=max(Err(*,*,1))
        ymax=4.95
	
	cend='as'
        plot,x,Err(0,*,1),xtitle='AMSU-A Scan Position Index',ytitle='Stdev of DeltaTB [AMSUA-MHS] @ 89 GHz [K]',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5, title=satName + ' ' + date, /noData
        FOR ifile=0,nList-1 DO BEGIN
            oplot,x,Err_as(ifile,*,1),psym=-8,symsize=0.5, color=(ifile+1)*12
	    xyouts, 0.00, 0.05 + ifile  * 0.05, '_orbit ' + strtrim(string(ifile+1),2), color=(ifile+1)*12, CHARSIZE=1.0, CHARTHICK=0.8, /normal
        ENDFOR
	thisImage = TVRD()
	png_file = QCcheckDir+"/"+prefix + '_stdv_' + cend + '.png'
	Write_PNG, png_file, thisImage, r, g, b
		
        erase
	cend='ds'
        plot,x,Err(0,*,1),xtitle='AMSU-A Scan Position Index',ytitle='Stdev of DeltaTB [AMSUA-MHS] @ 89 GHz [K]',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5, title=satName + ' ' + date, /noData
        FOR ifile=0,nList-1 DO BEGIN
            oplot,x,Err_ds(ifile,*,1),psym=-8,symsize=0.5, color=(ifile+1)*12
	    xyouts, 0.00, 0.05 + ifile  * 0.05, '_orbit ' + strtrim(string(ifile+1),2), color=(ifile+1)*12, CHARSIZE=1.0, CHARTHICK=0.8, /normal
        ENDFOR
	thisImage = TVRD()
	png_file = QCcheckDir+"/"+prefix + '_stdv_' + cend + '.png'
	Write_PNG, png_file, thisImage, r, g, b
		
    ENDIF


    IF (plot_DeltaTB89_meanOnly eq 1) THEN BEGIN
        ;-----------------
        erase
        !p.multi=1
        x=findgen(nFovs)+1
        ymin=min(Err(*,*,0))
        ymax=max(Err(*,*,0))
	
	cend='as'
        plot,x,Err(0,*,0),xtitle='AMSU-A Scan Position Index',ytitle='Mean TB Diffeence [AMSUA-MHS] @ 89 GHz [K]',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5, title=satName + ' ' + date, /noData
        FOR ifile=0,nList-1 DO BEGIN
            oplot,x,Err_as(ifile,*,0),psym=-8,symsize=0.5, color=(ifile+1)*12
	    xyouts, 0.00, 0.05 + ifile  * 0.05, '_orbit ' + strtrim(string(ifile+1),2), color=(ifile+1)*12, CHARSIZE=1.0, CHARTHICK=0.8, /normal
        ENDFOR
	thisImage = TVRD()
	png_file = QCcheckDir+"/"+prefix + '_mean_' + cend + '.png'
	Write_PNG, png_file, thisImage, r, g, b
	
        erase
	cend='ds'
        plot,x,Err(0,*,0),xtitle='AMSU-A Scan Position Index',ytitle='Mean TB Diffeence [AMSUA-MHS] @ 89 GHz [K]',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5, title=satName + ' ' + date, /noData
        FOR ifile=0,nList-1 DO BEGIN
            oplot,x,Err_ds(ifile,*,0),psym=-8,symsize=0.5, color=(ifile+1)*12
	    xyouts, 0.00, 0.05 + ifile  * 0.05, '_orbit ' + strtrim(string(ifile+1),2), color=(ifile+1)*12, CHARSIZE=1.0, CHARTHICK=0.8, /normal
        ENDFOR
	thisImage = TVRD()
	png_file = QCcheckDir+"/"+prefix + '_mean_' + cend + '.png'
	Write_PNG, png_file, thisImage, r, g, b
	
    ENDIF



    IF (plot_map_TB_MHS eq 1) THEN BEGIN
    	ERASE
        !p.multi=1
        ind     = where(qc eq 0 and tb89_A gt 0. and tb89_B gt 0. and Direc eq 1  and fix(day) eq fix(mday)) 
        minY    = min(TB89_B(ind))
        maxY    = max(TB89_B(ind))
        tit     = 'Global Distribution of TB @ 89 GHz (MHS)'
        mapPlot,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,TB89_B,'[K]',0.9,8,1,0,'(f7.2)'
        map_continents,/continents,/noborder,/hires,fill_continents=0,color=255
	thisImage = TVRD()
	png_file = 'mhs.png'
	Write_PNG, png_file, thisImage, r, g, b

    ENDIF


    IF (plot_map_TB_AMSUA eq 1) THEN BEGIN
        ERASE
        !p.multi=1
        minY    = min(TB89_A(ind))
        maxY    = max(TB89_A(ind))
        ind     = where(qc eq 0 and tb89_A gt 0. and tb89_B gt 0. and Direc eq 1  and fix(day) eq fix(mday)) 
        tit     = 'Global Distribution of TB @ 89 GHz (AMSUA)'
        mapPlot,minlat,maxlat,minlon,maxlon,xlat,xlon,ind,tit,minY,maxY,TB89_A,'[K]',0.9,8,1,0,'(f7.2)'
        map_continents,/continents,/noborder,/hires,fill_continents=0,color=18
	thisImage = TVRD()
	png_file = 'amsua.png'
	Write_PNG, png_file, thisImage, r, g, b
    ENDIF


    IF (plot_DeltaTB89_vs_ScanPos eq 1) THEN BEGIN
        ERASE
        !p.multi=[2,1,2,0,0]
        ind     = where(qc eq 0 and tb89_A gt 0. and tb89_B gt 0. and fix(day) eq fix(mday)) 
        ymin=min(xtab(ind))
        ymax=max(xtab(ind))
        plot,ScanPos(ind),xtab(ind),psym=8,xtitle='Scan Position Index',$
          ytitle='Mean DeltaTB @89 GHz [K]',yrange=[ymin,ymax],ystyle=1
        iplot=0
        FOR ifile=0,nList-1 DO BEGIN
            FOR iscan=0,nScanLTot(ifile)-1 DO BEGIN
                y1  = FMerrTot(ifile,iscan,0:nFovs-1,0)
                y2  = FMerrTot(ifile,iscan,0:nFovs-1,1)
                y   = y1-y2
                x   = findgen(nFovs)+1
                ind = where(FMerrTot(ifile,iscan,0:nFovs-1,3) eq 0,ncount) 
                IF (ncount gt 0 and iplot eq 0) THEN BEGIN
                    plot,x(ind),y(ind),psym=8,xtitle='Scan Position Index',$
                      ytitle='Mean DeltaTB @89 GHz [K]',yrange=[ymin,ymax],ystyle=1
                    iplot=iplot+1
                ENDIF
                IF (ncount gt 0 and iplot ne 0) THEN BEGIN
                    oplot,x(ind),y(ind),psym=8
                    iplot=iplot+1
                ENDIF
            ENDFOR
        ENDFOR
	thisImage = TVRD()
	png_file = 'scanPos.png'
	Write_PNG, png_file, thisImage, r, g, b
    ENDIF


    IF (plot_DeltaTB89_std_mean eq 1) THEN BEGIN
        ;-----------------
        erase
        !p.multi=[3,1,3,0,0]
        x=findgen(nFovs)+1
        ymin=min(Err(*,*,0))
        ymax=max(Err(*,*,0))
        plot,x,Err(0,*,0),xtitle='Scan Position Index',ytitle='Mean DeltaTB @ 89 GHz [K]',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5
        FOR ifile=1,nList-1 DO BEGIN
            oplot,x,Err(ifile,*,0),psym=-8,symsize=0.5
        ENDFOR
        ymin=0.
        ;ymax=max(Err(*,*,1))
        ymax=4.95
        plot,x,Err(0,*,1),xtitle='Scan Position Index',ytitle='Stdev DeltaTB @ 89 GHz [K]',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5
        FOR ifile=1,nList-1 DO BEGIN
            oplot,x,Err(ifile,*,1),psym=-8,symsize=0.5
        ENDFOR
        ymin=0.
        ymax=max(Err(*,*,2))
        plot,x,Err(0,*,2),xtitle='Scan Position Index',ytitle='Mean Stdev of DeltaTB@89 GHz [K] during FM',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5
        FOR ifile=1,nList-1 DO BEGIN
            oplot,x,Err(ifile,*,2),psym=-8,symsize=0.5
        ENDFOR
	thisImage = TVRD()
	png_file = prefix + '_std_mean.png'
	Write_PNG, png_file, thisImage, r, g, b
    ENDIF


    IF (plot_DeltaTB89_assym eq 1 ) THEN BEGIN
        ;-----------------
        erase
        !p.multi=[3,1,3,0,0]
        x=findgen(nFovs)+1
        ymin=min(Err(*,*,3))
        ymax=max(Err(*,*,3))
        plot,x,Err(0,*,3),xtitle='Scan Position Index',ytitle='Mean AMSUA TB @ 89 GHz [K]',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5
        FOR ifile=1,nList-1 DO BEGIN
            oplot,x,Err(ifile,*,3),psym=-8,symsize=0.5
        ENDFOR
        ymin=min(Err(*,*,4))
        ymax=max(Err(*,*,4))
        plot,x,Err(0,*,4),xtitle='Scan Position Index',ytitle='Mean MHS TB @ 89 GHz [K]',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5
        FOR ifile=1,nList-1 DO BEGIN
            oplot,x,Err(ifile,*,4),psym=-8,symsize=0.5
        ENDFOR
        ymin=min(Err(*,*,0))
        ymax=max(Err(*,*,0))
        plot,x,Err(0,*,0),xtitle='Scan Position Index',ytitle='Mean DeltaTB @89 GHz [K]',$
          yrange=[ymin,ymax],ystyle=1,psym=-8,symsize=0.5,charsize=1.5
        FOR ifile=1,nList-1 DO BEGIN
            oplot,x,Err(ifile,*,0),psym=-8,symsize=0.5
        ENDFOR
	thisImage = TVRD()
	png_file = 'assym.png'
	Write_PNG, png_file, thisImage, r, g, b
    ENDIF


END
