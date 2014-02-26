; Program used to take a quick look at the warm targets
; computed by the program:rdr2tdr_amsua.f90
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
;file_in   = 'fort.56'
file_in   = '/net/orbit006l/home/sidb/mirs_working/data/TestbedData/nedt/metopA_amsua/metopA_amsua_wt_2007_12_27.dat'
openr,iu,file_in,/get_lun
nch_m     =0L
nEffFiles =0L
nWarmView =0L
ligne=''
readf,iu,format='(25x,i8)',nch_m
readf,iu,format='(25x,i8)',nEffFiles
readf,iu,format='(a)',ligne
nEffScanLvec = lonarr(nEffFiles)
readf,iu,format='(25x,20i8)',nEffScanLvec
nElts        = lonarr(nch_m,nEffFiles)
cfreq        = fltarr(nch_m)
readf,iu,format='(25x,i8)',nWarmView
readf,iu,format='(a)',ligne
readf,iu,format='(10f10.3)',cfreq
readf,iu,format='(a)',ligne
WTarg        = fltarr(max(nEffScanLvec)*nWarmView,nEffFiles,nch_m)
LatArr       = fltarr(max(nEffScanLvec)*nWarmView,nEffFiles)
Wtarg(*,*)   = -999.
x            = 0.
WTvec        = fltarr(nWarmView)
ichan0       = 0L
k0           = 0L
i0           = 0L
FOR ichan = 0L, nch_m-1 DO BEGIN
    FOR k=0L,nEffFiles-1 DO BEGIN
        ii=0L
        FOR i=0L,nEffScanLvec(k)-1 DO BEGIN
            readf,iu,format='(3i6,10f10.3)',ichan0,k0,i0,WTvec
            FOR j=0L,nWarmView-1 DO BEGIN
                IF (WTvec(j) gt 0.) THEN BEGIN
                    wTarg(ii,k,ichan)=WTvec(j)
                    ii=ii+1L
                ENDIF
            ENDFOR
        ENDFOR
        nElts(ichan,k)=ii
        print,ichan,k,ii,max(nEffScanLvec)*nWarmView
    ENDFOR
ENDFOR


close,iu

sels: 
    impr = 0

    stdevVec=fltarr(nch_m,nEffFiles)
    FOR ichan=0,nch_m-1 DO BEGIN
        FOR k=0L,nEffFiles-1 DO BEGIN
            stdevVec(ichan,k)=stdev(wTarg(0:nElts(ichan,k)-1,k,ichan))
        ENDFOR
    ENDFOR


imp:
    !p.font=1
    csize         = 16
    a             = findgen(csize+1) *(!pi*2./float(csize))
    usersym,cos(a)/2,sin(a)/2,/fill
    


    plot_Warmtarget = 1
    plot_NEDT       = 1


    IF (plot_Warmtarget eq 1) THEN BEGIN
        ERASE
        !p.multi=[15,3,5,0,0]
        FOR ichan=0,nch_m-1 DO BEGIN
            FOR k=0L,nEffFiles-1 DO BEGIN
                y=wTarg(0:nElts(ichan,k)-1,k,ichan)
                ymin=min(y)
                ymax=max(y)
                x=LatArr(*,k)
                plot,y,xtitle='Index',ytitle='Warm Target Equivalent TB [K]',$
                  title='METOP-A/AMSU Channel#'+string(ichan+1,'(i2)')+' Stdev:'+string(stdev(y),'(f7.2)'),$
                  psym=1,symsize=0.5,yrange=[ymin,ymax],charsize=1.2,ystyle=1
            ENDFOR
        ENDFOR
    ENDIF
    IF (plot_NEDT eq 1) THEN BEGIN
        erase
        !p.multi=1
        x=findgen(nch_m)+1
        plot,x,stdevVec(*,0),xtitle='Channel Index',ytitle='NEDT [K]',charsize=1.5,title='METOP-A AMSU',$
          psym=-8,symsize=1.5
        FOR ifile=1,nEffFiles-1 DO BEGIN
            oplot,x,stdevVec(*,ifile),psym=-8,symsize=1.5
        ENDFOR
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
