;$Id: MonitorWT_n18.pro 590 2007-10-24 19:58:30Z sidb $
@../../../setup/paths_idl.pro
;===============================================================
; Name:		MonitorWT
;
;
; Type:		IDL Subroutine
;
;
; Description:  Used to monitor the Warm Targets (WT) used to compute the
;               NEDTs, day after day
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- wtList=wtList      I             List of warm target files
;	- ext=ext            I             extension to be added to
;                                          output file (like 'MHS')  
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       07-22-2005      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO MonitorWT,wtList=wtList,ext=ext

close,/all
set_plot,'ps'
loadct,39
;----Set input filename
readlist,wtList,listDirs,ndirs
nList      = n_elements(listDirs)
mxOrbits   = 400
nOrbitsVec = intarr(nList)
date       = dblarr(nList)
year       = intarr(nList)
months     = intarr(nList)
Days       = intarr(nList)
flag       = make_array(nList,/integer,value=0)
nDayEff    = 0
nscanLvecTot = lonarr(nList,mxOrbits)

for iday=0,nList-1 do begin
    spawn,'ls -1 '+listDirs(iday)+'/*wt* >list'
    day=float(strmid(strtrim(listDirs(iday)),1,2,/reverse_offset))
    mnth=float(strmid(strtrim(listDirs(iday)),4,2,/reverse_offset))
    yr=float(strmid(strtrim(listDirs(iday)),9,4,/reverse_offset))
    date(iday)=yr+(mnth-1)/12.+min([day/30.,1.])/12.
    Year(iday)=yr
    Months(iday)=mnth
    Days(iday)=day
    readlist,'list',list,nfiles
    IF (nfiles eq 0) THEN flag(iday) = 1
    IF (nfiles ne 0) THEN begin
        flag(iday) = 0
        nDayEff    = nDayEff+1
        print,list[0]
        readWT,list[0],cfreq,wt,nchan,nOrbits,nScanLVec,nWarmView
        nscanLvecTot(iDay,0:nOrbits-1) = nScanLVec(0:nOrbits-1)
        if (nDayEff eq 1) then begin
            mxScanL=max(nScanLVec(0:nOrbits-1))
            wtTotal=fltarr(nList,mxScanL,nchan,nWarmView,mxOrbits)
            IF (nOrbits gt mxOrbits) THEN STOP,'Error:Increase mxOrbits'
        endif
        FOR iorbit=0L,nOrbits-1 DO BEGIN
            wtTotal(iDay,0:nScanLvec(iorbit)-1,*,*,iOrbit) = wt(0:nScanLvec(iorbit)-1,*,*,iOrbit)
        ENDFOR
        nOrbitsVec(iDay)=nOrbits
    ENDIF

endfor


;IF (nDayEff ne 1) THEN BEGIN
;    print, 'Error: This code works only for one day currently.'
;    STOP
;ENDIF


;----test
nChan=1
;--------

;------------------------------------------------------------------------------
; Compute stats
stdevWT=fltarr(nList,nChan,mxOrbits,nWarmView)

FOR iDay=0,nList-1 DO BEGIN
    FOR ichan=0,nChan-1 DO BEGIN
        FOR iOrbit=0,nOrbitsVec(iDay)-1 DO BEGIN
            FOR iView=0,nWarmView-1 DO BEGIN
                ind=where(wtTotal(iDay,0:nScanLvecTot(iDay,iorbit)-1,iChan,iView,iOrbit) gt 0.)
                stdevWT(iDay,ichan,iOrbit,iView)=stdev(wtTotal(iDay,ind,iChan,iView,iOrbit))
            ENDFOR
        ENDFOR
    ENDFOR
ENDFOR

;------------------------------------------------------------------------------

    close,/all

    !p.font=1
    !p.charsize   = 1.4
    csize         = 16
    a             = findgen(csize+1) *(!pi*2./float(csize))
    usersym,cos(a)/2,sin(a)/2,/fill
    symsize    = 0.4
    sty        = findgen(nWarmView)
    cols       = findgen(nWarmview)*(250)/(nWarmview-1)

    plot_wt        = 1
    plot_wtStats   = 1

    IF (plot_wtStats eq 1) THEN BEGIN
        for ichan=0,nchan-1 do begin
            ERASE
            !p.multi=1
            ficres='visu_wt_stats_'+ext+'_Chan'+string(ichan+1,'(i2.2)')+'.ps'
            print,' PostScript File created:',ficres
            device,filename=ficres,/color,/landscape,ysize=18,xsize=24
            FOR iday=0,nList-1 DO BEGIN
                IF (flag(iDay) eq 0) THEN BEGIN
                    iPlot=0
                    tit='Warm Target Stdev Monitoring for Channel #'+string(ichan+1,'(i2)')+' '+Ext+$
                      ' Day:'+string(Year(iday))+string(Months(iday))+string(Days(iday))
                    ymin=min(stdevWT(iDay,ichan,*,*))
                    ymax=max(stdevWT(iDay,ichan,*,*))
                    FOR j=0,nWarmView-1 DO BEGIN
                        IF (iplot eq 0 ) THEN BEGIN
                            y=stdevWT(iDay,ichan,0:nOrbitsVec(iDay)-1,j)
                            x=findgen(nOrbitsVec(iDay))+1
                            plot,x,y,psym=-8,title=tit,xtitle='Orbit Number',color=cols[j],linestyle=sty[j],$
                              ytitle='Count-based Warm Target Stdev [K]',yrange=[ymin,ymax],xtickformat='(f8.3)',$
                              thick=3,ystyle=1
                            iplot=1
                        ENDIF
                        IF (iplot ne 0) THEN BEGIN
                            y=stdevWT(iDay,ichan,0:nOrbitsVec(iDay)-1,j)
                            oplot,x,y,psym=-8,color=cols[j],linestyle=sty[j],thick=3
                        ENDIF
                    ENDFOR
                    psm=findgen(nWarmView) & psm(*)=8
                    comm='Warm View:'+string(findgen(nWarmview)+1)
                    plot_legend,1,nWarmview,sty,psm,cols,comm,1.
                ENDIF
            ENDFOR
        endfor

    ENDIF

    IF (plot_wt eq 1) THEN BEGIN
        for ichan=0,nchan-1 do begin
            ficres='visu_wt_'+ext+'_Chan'+string(ichan+1,'(i2.2)')+'.ps'
            print,' PostScript File created:',ficres
            device,filename=ficres,/color,/landscape,ysize=18,xsize=24
            FOR iday=0,nList-1 DO BEGIN
                IF (flag(iDay) eq 0) THEN BEGIN
                    ERASE
                    !p.multi=[15,3,5,0,0]
                    FOR k=0,nOrbitsVec(iDay)-1 DO BEGIN
                        iPlot=0
                        tit='WT Monit. Ch#'+string(ichan+1,'(i2)')+' '+Ext+$
                          ' Day:'+string(Year(iday))+string(Months(iday))+string(Days(iday))+' Orb#'+string(k+1)
                        ;FOR j=0,0 DO BEGIN
                        FOR j=0,nWarmView-1 DO BEGIN
                            ind=where(wtTotal(iDay,0:nScanLvecTot(iDay,k)-1,ichan,j,k) gt 0.,ncount)
                            IF (iplot eq 0 and ncount gt 0) THEN BEGIN
                                x=findgen(ncount)+1
                                y=wtTotal(iDay,ind,ichan,j,k)
                                ymin=min(wtTotal(iDay,ind,ichan,0:nWarmView-1,k))
                                ymax=max(wtTotal(iDay,ind,ichan,0:nWarmView-1,k))
                                ;ymin=286.
                                ;ymax=287.5
                                plot,x,y,psym=-3,title=tit,xtitle='ScanLine #',color=cols[j],$
                                  ytitle='Count-based WT TB [K]',yrange=[ymin,ymax],xtickformat='(f8.3)',ystyle=1
                                m=mean(y)
                                s=stdev(y)
                                plots,[0,n_elements(ind)],[m,m],color=cols[j]
                                plots,[0,n_elements(ind)],[m-s,m-s],color=cols[j]
                                plots,[0,n_elements(ind)],[m+s,m+s],color=cols[j]
                                iplot=1
                                print,ichan,iday,k,j,ncount,m,s
                            ENDIF
                            IF (iplot ne 0 and ncount gt 0) THEN BEGIN
                                x=findgen(ncount)+1
                                y=wtTotal(iDay,ind,ichan,j,k)
                                oplot,x,y,psym=-3,color=cols[j]
                                m=mean(y)
                                s=stdev(y)
                                plots,[0,n_elements(ind)],[m,m],color=cols[j]
                                plots,[0,n_elements(ind)],[m-s,m-s],color=cols[j]
                                plots,[0,n_elements(ind)],[m+s,m+s],color=cols[j]
                                print,ichan,iday,k,j,ncount,m,s
                            ENDIF
                        ENDFOR
                    ENDFOR
                ENDIF
            ENDFOR
        endfor
    ENDIF

    device,/close
    close,/all
    print,'End of IDL-processing...(WT monitoring)'

END
