;
;  Plot_Histogram_2d_ScanAngle.pro
;
;  Program to plot 2D histograms of scan-dependent properties derived
;  from diag files.
;
;  To use, edit entries below down to the horizontal line.
;
;  Run with .run Plot_Histogram_2d_ScanAngle
;

; Directories containing required diag files.  dir2 is only used when
; plotting differences.
dir1='~/Diag_Files/HIRS_Metop/'
dir2='/jcsda/noscrub/wx23adc/nalli/'

; Instrument to use.  "instrument" should be in the form used in the
; diag file names.
instrument='hirs4_metop-a'
instrument_text='HIRS Metop-A'


; Guess (ges) or analysis (anl) fields?  
gesanl='ges'

; Required dates
date=['2010040100', '2010040106', '2010040112', '2010040118', '2010040200', '2010040206',$
 '2010040212', '2010040218', '2010040300', '2010040306', '2010040312', '2010040318', $
 '2010040400', '2010040406', '2010040412', '2010040418', '2010040500', '2010040506', $
 '2010040512', '2010040518', '2010040600', '2010040606', '2010040612', '2010040618', $
 '2010040700', '2010040706', '2010040712', '2010040718', '2010040800', '2010040806', $
 '2010040812', '2010040818', '2010040900', '2010040906', '2010040912', '2010040918', $
 '2010041000', '2010041006', '2010041012', '2010041018', '2010041100', '2010041106', $
 '2010041112', '2010041118', '2010041200', '2010041206', '2010041212', '2010041218', $
 '2010041300', '2010041306', '2010041312', '2010041318', '2010041400', '2010041406', $
 '2010041412', '2010041418', '2010041500', '2010041506', '2010041512', '2010041518', $
 '2010041600', '2010041606', '2010041612'] 

miny=-3  ; Min radiance departure
maxy=2   ; Max radiance departure
dy=0.1   ; Radiance interval
dx=1     ; Scan position interval

chan1=7     ; First channel to plot
chan2=7     ; Last channel to plot

; Which channels to read in (saves memory especially when using IASI
; or AIRS):
chan_in1=0    ; First channel to read_in
chan_in2=14   ; Last channel to read_in

; Set to one to use given surface type
use_sea=1
use_land=1
use_ice=1
use_snow=1

; Field to plot
; 0=Obs
; 1=DEPAR_BC    
; 2=DEPAR_NBC   
; 3=ERRINV     
; 4=QC_FLAG    
; 5=SURF_EMISS  
; 6=TLAPCHAN 
; -1=Applied Bias Correction
Field=2

; Used Obs (0=All, 1=Used)
Plot_Used=0

; Prefix to automatically generated postscript file name
ps_file_prefix=''

; Set to 1 for a difference plot
difference_plot=0

; Set to zero to avoid re-reading in all the data!
read_files=1

; Normalise 2D histograms by number of obs in each scan bin
normalise=0

fac=1.    ; The fraction of the maximum value corresponding to the maximum on the colour scale

plot_1d_histograms=0

; Options to read or write IDL save files containing diag data
write_save=0
read_save=0

;==========================================================================================================

case Field of
0:  Field_Text='Observation Value'
1:  Field_Text='Bias-corrected first-guess departure'
2:  Field_Text='Un-bias-corrected first-guess departure'
3:  Field_Text='Errinv'
4:  Field_Text='QC Flag'
5:  Field_Text='TLapchan'
-1: Field_Text='Applied Bias Correction'
Else: Begin
       print,'Incorrect Field Value: ',Field
       stop
      End
endcase

water_fac=1000.
land_fac=1000.
ice_fac=1000.
snow_fac=1000.
if (use_sea eq 1) then water_fac=1.
if (use_land eq 1) then land_fac=1.
if (use_snow eq 1) then snow_fac=1.
if (use_ice eq 1) then ice_fac=1.
; If all surfaces chosen make sure all obs get through
if (use_sea eq 1 and use_land eq 1 and $
    use_snow eq 1 and use_ice eq 1) then begin
   water_fac=0.
   land_fac=0.
   snow_fac=0.
   ice_fac=0.
endif

num_dates=size(date) & num_dates=num_dates(1)

case Plot_Used of
0: Plot_Used_text='All'
Else: Plot_Used_text='Used'
endcase

a=' '

if (read_files ne 1) then begin

  chan1=chan1-chan_in1
  chan2=chan2-chan_in1

endif else begin

  nobs1=0L
  file=dir1+'/diag_'+instrument+'_ges.'+date(0)
  ierr=FILE_TEST(file+'.sav')
  ierr1=FILE_TEST(file)
  if (ierr1 ne 1 and ierr ne 1) then begin
     print,'WARNING: '+file+' not found'
  endif else if (ierr eq 1 and read_save eq 1) then begin
     restore,file+'.sav'
  endif else begin
     read_diags,file,obs_in,meta_in,nobs_in,nchan,chaninfo=chaninfo
     if (write_save eq 1) then $ 
       save,file=file+'.sav',obs_in,meta_in,nobs_in,nchan,chaninfo
  endelse 
  ; Check requested input channels are OK
  if (chan_in1 lt 0 or chan_in1 gt nchan or chan_in1 gt chan_in2) then begin
    print,'chan_in1 is invalid, should be between 0 and ',min(chan_in2,nchan)
    stop
  endif
  if (chan_in2 lt 0 or chan_in2 gt nchan or chan_in1 gt chan_in2) then begin
    print,'chan_in2 is invalid, should be between ',chan_in1,' and ',nchan
    stop
  endif
  if (chan1 lt chan_in1 or chan1 gt chan_in2) then begin
    print,'chan1 is invalid, should be between ',chan_in1,' and ',chan_in2
    stop
  endif
  if (chan2 lt chan1 or chan2 gt chan_in2) then begin
    print,'chan2 is invalid, should be between ',chan1,' and ',chan_in2
    stop
  endif
  chan1=chan1-chan_in1
  chan2=chan2-chan_in1
   ; Transfer to obs variables
  if (field ge 0) then begin
    tmp=obs_in.(field)
  endif else begin
    tmp=obs_in.depar_nbc-obs_in.depar_bc
  endelse
  obs1=transpose(tmp(chan_in1:chan_in2,*))
  obs1_errinv=transpose(reform(obs_in.errinv(chan_in1:chan_in2)))
  meta1=meta_in
  nobs1=nobs_in

if (num_dates gt 1) then begin
  for idate=1,num_dates-1 do begin
    file=dir1+'/diag_'+instrument+'_ges.'+date(idate)
    ierr=FILE_TEST(file+'.sav')
    ierr1=FILE_TEST(file)
    if (ierr1 ne 1 and ierr ne 1) then begin
       print,'WARNING: '+file+' not found'
    endif else if (ierr eq 1 and read_save eq 1) then begin
       restore,file+'.sav'
    endif else begin
      read_diags,file,obs_in,meta_in,nobs_in,nchan,chaninfo=chaninfo
      if (write_save eq 1) then $
         save,file=file+'.sav',obs_in,meta_in,nobs_in,nchan,chaninfo
    endelse 
    if (field ge 0) then begin
      tmp=obs_in.(field)
    endif else begin
      tmp=obs_in.depar_nbc-obs_in.depar_bc
    endelse
    obs1=[obs1,transpose(tmp(chan_in1:chan_in2,*))]
    obs1_errinv=$
      [obs1_errinv,transpose(reform(obs_in.errinv(chan_in1:chan_in2)))]
    meta1=[meta1,meta_in]
    nobs1=nobs1+nobs_in
  endfor
endif

if (difference_plot eq 1) then begin

  nobs2=0L
  file=dir2+'/diag_'+instrument+'_ges.'+date(0)
  ierr=FILE_TEST(file+'.sav')
  ierr1=FILE_TEST(file)
  if (ierr1 ne 1 and ierr ne 1) then begin
     print,'WARNING: '+file+' not found'
  endif else if (ierr eq 1 and read_save eq 1) then begin
     restore,file+'.sav'
  endif else begin
     read_diags,file,obs_in,meta_in,nobs_in,nchan,chaninfo=chaninfo
     if (write_save eq 1) then $ 
       save,file=file+'.sav',obs_in,meta_in,nobs_in,nchan,chaninfo
  endelse 
   ; Transfer to obs variables
  if (field ge 0) then begin
    tmp=obs_in.(field)
  endif else begin
    tmp=obs_in.depar_nbc-obs_in.depar_bc
  endelse
  obs2=transpose(tmp(chan_in1:chan_in2,*))
  obs2_errinv=transpose(reform(obs_in.errinv(chan_in1:chan_in2)))
  meta2=meta_in
  nobs2=nobs_in

if (num_dates gt 1) then begin
  for idate=1,num_dates-1 do begin
    file=dir2+'/diag_'+instrument+'_ges.'+date(idate)
    ierr=FILE_TEST(file+'.sav')
    ierr1=FILE_TEST(file)
    if (ierr1 ne 1 and ierr ne 1) then begin
       print,'WARNING: '+file+' not found'
    endif else if (ierr eq 1 and read_save eq 1) then begin
       restore,file+'.sav'
    endif else begin
      read_diags,file,obs_in,meta_in,nobs_in,nchan,chaninfo=chaninfo
      if (write_save eq 1) then $
         save,file=file+'.sav',obs_in,meta_in,nobs_in,nchan,chaninfo
    endelse 
    if (field ge 0) then begin
      tmp=obs_in.(field)
    endif else begin
      tmp=obs_in.depar_nbc-obs_in.depar_bc
    endelse
    obs2=[obs2,transpose(tmp(chan_in1:chan_in2,*))]
    obs2_errinv=$
      [obs2_errinv,transpose(reform(obs_in.errinv(chan_in1:chan_in2)))]
    meta2=[meta2,meta_in]
    nobs2=nobs2+nobs_in
  endfor
endif

endif
endelse


for ichan=chan1,chan2 do begin

IF (Plot_Used eq 1) then begin
  use=where(obs1_errinv(*,ichan) gt 0. and $
     (meta1.water_coverage gt 0.999*water_fac or $
      meta1.land_coverage gt 0.999*land_fac or $
      meta1.snow_coverage gt 0.999*snow_fac or $
      meta1.ice_coverage gt 0.999*ice_fac),ct)
endif else begin
  use=where(meta1.water_coverage gt 0.999*water_fac or $
      meta1.land_coverage gt 0.999*land_fac or $
      meta1.snow_coverage gt 0.999*snow_fac or $
      meta1.ice_coverage gt 0.999*ice_fac,ct)
endelse

v=fltarr(2,ct)

ii=0L
for iob=0L,ct-1 do begin
    v(0,ii)=meta1(use(iob)).scan_pos
    v(1,ii)=obs1(use(iob),ichan)
    if (Difference_plot eq 1) then v(1,ii)=obs2(ichan,use(iob))-v1(1,ii)
    ii=ii+1
endfor

max_scan=max(meta1.scan_pos)

mean_depar=fltarr(max_scan)
stddev_depar=fltarr(max_scan)
for iscan=0,max_scan-1 do begin
  xxscan=where(meta1(use).scan_pos eq iscan+1,ct)
  if (ct gt 1) then begin
    mean_depar(iscan)=mean(v(1,xxscan))
    stddev_depar(iscan)=stddev(v(1,xxscan))
  endif else begin
    mean_depar(iscan)=-9999.
    stddev_depar(iscan)=0.
  endelse
endfor


h1=hist_2d(v(0,*),v(1,*),bin1=dx,bin2=dy,min1=1,min2=miny,max1=max_scan,max2=maxy)
h1=float(h1)

hsum=fltarr(max_scan)
if (normalise eq 1) then begin
  for iscan=0,max_scan-1 do begin
     hsum(iscan)=total(h1(iscan,*))
     if (hsum(iscan) gt 0.) then h1(iscan,*)=h1(iscan,*)/hsum(iscan)
  endfor
endif

h=bytscl(float(h1),min=0,max=fac*(max(h1)))
;h=bytscl(float(h1),min=0)

zeroes=where(h eq 0, ct)

; Plot out the 2D Histogram

channel_text=strtrim(string(chaninfo(ichan+chan_in1).channel_number,format='(i5)'),2)
title=Field_Text+': '+instrument_text+' '+channel_text+ '; '+$
Plot_Used_Text+' Obs'
ytitle='First Guess Departure (K)'
psfilename=ps_file_prefix+strtrim(instrument,2)+'_Ch'+$
    channel_text+'_ScanBias_'+Plot_Used_Text+'.ps'

if (!d.name eq 'X') then begin
  window,1,retain=2
  device,decomposed=0
  loadct,5
endif else begin
  device,/color,file=psfilename,/land,bits_per_pixel=8
  if (ct gt 0) then h(zeroes)=!p.background
endelse

plot,[0.5,max_scan+0.5],[miny,maxy],/nodata,xs=1,ys=1,ymargin=[12,2]

tvimage,h,axes=1,position=[0.1,0.1,0.9,0.9],$
xr=[0.5,max_scan+0.5],$
yr=[miny,maxy],/nointerpolation,$
axkeywords={ytitle: ytitle,$
xtitle:'Scan Positon',title:title},/overplot

x=1.+findgen(max_scan)
oplot,x,mean_depar,thick=3,min_value=-9000.
oplot,x,mean_depar,thick=3,line=2,color=70,min_value=-9000.
oplot,x,mean_depar+stddev_depar,thick=3,line=1,min_value=-9000.
oplot,x,mean_depar-stddev_depar,thick=3,line=1,min_value=-9000.,max_value=9000.

oplot,[-1000,1000],[0,0]

if (normalise eq 1) then begin
  colorbar,minrange=0,maxrange=fac*max(h1),$
      position=[0.2,0.05,0.8,0.1],$
      format='(f5.3)',title='Normalised Number'
endif else begin
  colorbar,minrange=0,maxrange=fac*max(h1),$
      position=[0.2,0.05,0.8,0.1],$
      format='(i5)',title='Number'
endelse

if (!d.name eq 'X') then begin
  read,a
endif 

if (plot_1d_histograms eq 1) then begin

num_bins_x=size(reform(h1(0,*))) & num_bins_x=num_bins_x(1)
num_bins_y=size(reform(h1(*,0))) & num_bins_y=num_bins_y(1)

x=miny+dy*findgen(num_bins_x)
scan_no=0.5+dx*(findgen(num_bins_y)+0.5)
scan_edge1=where(scan_no le 2.,ct1)
scan_edge2=where(scan_no ge max_scan-1,ct2)

plot,[x(0),x(num_bins_x-1)],[0,1.1],xs=1,ys=1,/nodata,$
     xtitle='First Guess Departure (K)',ytitle='Relative Number',title=title
for i=1,num_bins_y-2 do oplot,x,h1(i,*)/float(max(h1(i,*))),color=100
if (ct1 gt 0) then for i=0,ct1-1 do oplot,x,h1(scan_edge1(i),*)/float(max(h1(scan_edge1(i),*))),thick=3
if (ct2 gt 0) then for i=0,ct2-1 do oplot,x,h1(scan_edge2(i),*)/float(max(h1(scan_edge2(i),*))),thick=3
xyouts,0.1,0.9,'Scan Positions 1,2,29,30',/normal
xyouts,0.1,0.85,'Other Scan Positions',color=100,/normal
oplot,[0,0],[0,10]

if (!d.name eq 'X') then read,a

endif

if (!d.name eq 'PS') then device,/close
endfor

end 



