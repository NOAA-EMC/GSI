; Program used to take a quick look at the warm targets
; computed by the program:rdr2tdr_mhs.f90
;
; Sid-Ahmed Boukabara. 2005.
;------------------------------------------------------------------
close,/all

;------------------------------------------------------------------
PRINT, 'Do you want to read again the data (1/0)?'
read,answer
IF (answer eq 0) then GOTO, sels
Loadct,39
device='x'
set_plot,'x'
;----Set input filename
file_in   = 'fort.56'
openr,iu,file_in,/get_lun,/f77_unformatted
nch_m     =0L
nEffFiles =0L
nWarmView =0L
readu,iu,nch_m,nEffFiles,nWarmView
nEffScanLvec=intarr(nEffFiles)
nElts=lonarr(nch_m)
readu,iu,nEffScanLvec
WTarg=fltarr(max(nEffScanLvec)*nWarmView*nEffFiles,nch_m)
Wtarg(*,*)=-999.
x=0.
FOR ichan = 0L, nch_m-1 DO BEGIN
    ii=0L
    FOR k=0L,nEffFiles-1 DO BEGIN
        FOR i=0L,nEffScanLvec(k)-1 DO BEGIN
            FOR j=0L,nWarmView-1 DO BEGIN
                readu,iu,x
                IF (x gt 0.) THEN BEGIN
                    wTarg(ii,ichan)=x
                    ii=ii+1L
                ENDIF
            ENDFOR
        ENDFOR
    ENDFOR
    nElts(ichan)=ii
    print,ichan,ii,nEffFiles*max(nEffScanLvec)*nWarmView
ENDFOR


close,iu

sels: 
    impr = 0

    stdevVec=fltarr(nch_m)
    FOR ichan=0,nch_m-1 DO BEGIN
        stdevVec(ichan)=stdev(wTarg(0:nElts(ichan)-1,ichan))
    ENDFOR


imp:
    !p.font=1
    csize         = 16
    a             = findgen(csize+1) *(!pi*2./float(csize))
    usersym,cos(a)/2,sin(a)/2,/fill
    


    plot_Warmtarget = 0
    plot_NEDT       = 1


    IF (plot_Warmtarget eq 1) THEN BEGIN
        ERASE
        !p.multi=[15,3,5,0,0]
        FOR ichan=0,nch_m-1 DO BEGIN
            y=wTarg(0:nElts(ichan)-1,ichan)
            ymin=min(y)
            ymax=max(y)
            plot,y,xtitle='Index',ytitle='Warm Target Equivalent TB [K]',$
              title='NOAA18/AMSUA Channel#'+string(ichan+1,'(i2)')+' Stdev:'+string(st,'(f7.2)'),$
              psym=1,symsize=0.5,yrange=[ymin,ymax],charsize=1.2,ystyle=1
        ENDFOR
    ENDIF
    IF (plot_NEDT eq 1) THEN BEGIN
        erase
        !p.multi=1
        x=findgen(nch_m)+1
        plot,x,stdevVec,xtitle='Channel Index',ytitle='NEDT [K]',charsize=1.5,title='NOAA-18 AMSU/A',$
          psym=-8,symsize=2
    ENDIF


    IF (impr eq 1) THEN begin
        device,/close
        device ='x'
        GOTO, bo
    ENDIF
    Print,'________________________________________________'
    boucle:print,'PS/Exit(1/0)'
    read,impr
    IF (impr gt 3) THEN GOTO, boucle
    IF (impr eq 0) THEN GOTO, bo
ip: IF (impr eq 1) THEN BEGIN
        ficres='visu.ps'
        print,' PostScript File created:',ficres
        set_plot,'ps'
        device,filename=ficres,/color,ysize=23,xsize=16,yoffset=2,/portrait
        goto,imp
    ENDIF
    bo: print,'________________________________________________'
    impr=0
    devicd = 'x'
    set_plot,'x'
    fin:close,/all
    print,'End of procesing...'

END
