pro diag_stats,obs_in,meta,nchans,bias_corrected,sd,bias,
  xlatmin=latmin,xlatmax=latmax,$
  xlonmin=lonmin,xlonmax=lonmax,num=num, $
  minval=minval,maxval=maxval,no_qc=no_qc,land=land

; Returns statistics for the observations contain in a diag file
; previously read by read_diags.pro
;
; Inputs:
;  obs_in          obsdata structure from read_diags.pro
;  meta            metadata structure from read_diags.pro
;  nchans          number of channels from read_diags.pro
;  bias_corrected  if set to 1 use bias corrected departures,
;                    otherwise not bias corrected 
; Optional Inputs:
;  xlatmin   )      
;  xlonmin   )     Latitude and longtiude limits (default is global)
;  xlatmax   )
;  xlatmax   )     
;  no_qc           If non-zero qc checks are not applied (default=0)
;  land            If non-zero statistics are for land (default=sea)
; 
; Outputs:
;  sd              standard deviations of departures for each channel
;  bias            mean departures for each channel
;
; Optional Outputs:
;  minval          minimum departure for each channel
;  maxval          maximum departure for each channel
;
; History:
;  8th June 2010  Initial commit to branch.   A. Collard
;

if (keyword_set(xlatmin) eq 0) then latmin=-90.
if (keyword_set(xlatmax) eq 0) then latmax=90.
if (keyword_set(xlonmin) eq 0) then lonmin=-180.
if (keyword_set(xlonmax) eq 0) then lonmax=360.
if (keyword_set(no_qc)  eq 0) then no_qc=0
if (keyword_set(land)   eq 0) then land=0

if (bias_corrected eq 1) then begin
   idepar=1
endif else begin
   idepar=2
endelse

sd       = fltarr(nchans)
bias     = fltarr(nchans)
obs      = fltarr(nchans)
num      = lonarr(nchans)
maxval   = fltarr(nchans)
minval   = fltarr(nchans)

for i=0,nchans-1 do begin
  depar1=obs_in.(idepar)
  depar=reform(depar1(i,*))
  if (land ne 0) then begin
    use=where(abs(obs_in(*).qc_flag(i)) le 3 and $
      abs(obs_in(*).qc_flag(i)) ne 1 and $
      meta(*).land_coverage gt 0.99 and $
      (abs(depar) lt 100.) and obs_in(*).errinv(i) gt 0.0 and $
      meta(*).cenlat ge latmin and $
      meta(*).cenlat le latmax and $
      meta(*).cenlon ge lonmin and $
      meta(*).cenlon le lonmax, ct)
  endif else if (no_qc eq 0) then begin
    use=where(obs_in(*).qc_flag(i) eq 0 and $
      (abs(depar) lt 100.) and obs_in(*).errinv(i) gt 0.0 and $
      meta(*).cenlat ge latmin and $
      meta(*).cenlat le latmax and $
      meta(*).cenlon ge lonmin and $
      meta(*).cenlon le lonmax, ct)
  endif else begin
    use=where(meta(*).cenlat ge latmin and $
      (abs(depar) lt 100.) and $
      meta(*).cenlat le latmax and $
      meta(*).cenlon ge lonmin and $
      meta(*).cenlon le lonmax, ct)
  endelse
  if (ct eq 0) then begin 
    sd(i)=-999.
    bias(i)=-999.
    obs(i)=-999.
    num(i)=0
    maxval(i)=-999.
    minval(i)=-999.
  endif else begin
    if (ct eq 1) then begin
      sd(i)=0.
    endif else begin
      sd(i)=stddev(depar(use))
    endelse
    bias(i)=total(depar(use))/ct
    maxval(i)=-999.
    minval(i)=-999.
    maxval(i)=max(depar(use))
    minval(i)=min(depar(use))
    num(i)=ct
  endelse
endfor



end
