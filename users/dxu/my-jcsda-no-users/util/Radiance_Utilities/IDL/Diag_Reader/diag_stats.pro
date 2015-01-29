pro diag_stats,obs_in,meta,nchans,$
  sum_bc,sumsq_bc,sum_nbc,sumsq_nbc,num,$
  minval_bc=minval_bc,maxval_bc=maxval_bc,$
  minval_nbc=minval_nbc,maxval_nbc=maxval_nbc,$
  xlatmin=latmin,xlatmax=latmax,$
  xlonmin=lonmin,xlonmax=lonmax, $
  no_qc=no_qc,surface=surface

; Returns statistics for the observations contain in a diag file
; previously read by read_diags.pro
;
; Inputs:
;  obs_in          obsdata structure from read_diags.pro
;  meta            metadata structure from read_diags.pro
;  nchans          number of channels from read_diags.pro
;
; Optional Inputs:
;  xlatmin   )      
;  xlonmin   )     Latitude and longtiude limits (default is global)
;  xlatmax   )
;  xlatmax   )     
;  no_qc           If non-zero qc checks are not applied (default=0)
;  surface         If 0 all surface types are considered
;                     1 statistics are for sea (default)
;                     2 statistics are for land
;                     3 statistics are for snow and ice
; 
; Outputs:  (need to be defined on input)
;  sum_bc             sums of bias-corrected departures for each channel
;  sumsq_bc           sums of bias-corrected squares of departures for each channel
;  sum_nbc            sums of unbias-corrected departures for each channel
;  sumsq_nbc          sums of unbias-corrected squares of departures for each channel
;
; optional Outputs:  (need to be defined on input)
;  minval_bc          minimum bias-corrected departure for each channel
;  maxval_bc          maximum bias-corrected departure for each channel
;  minval_nbc         minimum unbias-corrected departure for each channel
;  maxval_nbc         maximum unbias-corrected departure for each channel
;
; History:
;  8th June 2010  Initial commit to branch.   A. Collard
; 13th April 2011 Convert to producing sums.  A. Collard.
;

if (arg_present(xlatmin) eq 0) then latmin=-90.
if (arg_present(xlatmax) eq 0) then latmax=90.
if (arg_present(xlonmin) eq 0) then lonmin=-180.
if (arg_present(xlonmax) eq 0) then lonmax=360.
if (arg_present(no_qc)   eq 0) then no_qc=0
if (arg_present(surface) eq 0) then surface = 1


if (no_qc eq 0) then begin
  errinv_min = 1.e-5
endif else begin
  errinv_min = -999.
endelse

for i=0,nchans-1 do begin
  if (surface eq 1) then begin  ; Sea
    use=where( meta(*).water_coverage gt 0.99 and $
      (abs(obs_in.depar_nbc) lt 100.) and (abs(obs_in.depar_bc) lt 100.) and $
      obs_in(*).errinv(i) gt errinv_min and $
      meta(*).cenlat ge latmin and $
      meta(*).cenlat le latmax and $
      meta(*).cenlon ge lonmin and $
      meta(*).cenlon le lonmax, ct)
  endif else if (surface eq 2) then begin  ; Land
    use=where( meta(*).land_coverage gt 0.99 and $
      (abs(obs_in.depar_nbc) lt 100.) and (abs(obs_in.depar_bc) lt 100.) and $
      obs_in(*).errinv(i) gt errinv_min and $
      meta(*).cenlat ge latmin and $
      meta(*).cenlat le latmax and $
      meta(*).cenlon ge lonmin and $
      meta(*).cenlon le lonmax, ct)
  endif else if (surface eq 3) then begin  ; Snow and Ice
    use=where( (meta(*).ice_coverage+meta(*).snow_coverage) gt 0.99 and $
      (abs(obs_in.depar_nbc) lt 100.) and (abs(obs_in.depar_bc) lt 100.) and $
      obs_in(*).errinv(i) gt errinv_min and $
      meta(*).cenlat ge latmin and $
      meta(*).cenlat le latmax and $
      meta(*).cenlon ge lonmin and $
      meta(*).cenlon le lonmax, ct)
  endif else begin  ; All surfaces
    use=where((abs(obs_in.depar_nbc) lt 100.) and (abs(obs_in.depar_bc) lt 100.) and $
      obs_in(*).errinv(i) gt errinv_min and $
      meta(*).cenlat ge latmin and $
      meta(*).cenlat le latmax and $
      meta(*).cenlon ge lonmin and $
      meta(*).cenlon le lonmax, ct)
  endelse
 
  num(i) = num(i)+ct
 
  if (ct gt 0.) then begin

   sum_bc(i) = sum_bc(i)+total(obs_in(use).depar_bc(i))
   sumsq_bc(i) = sumsq_bc(i)+total(obs_in(use).depar_bc(i)*obs_in(use).depar_bc(i))
   if (arg_present(minval_bc) ne 0) then minval_bc(i) = min([minval_bc(i),obs_in(use).depar_bc(i)])
   if (arg_present(maxval_bc) ne 0) then maxval_bc(i) = max([maxval_bc(i),obs_in(use).depar_bc(i)])
   sum_nbc(i) = sum_nbc(i)+total(obs_in(use).depar_nbc(i))
   sumsq_nbc(i) = sumsq_nbc(i)+total(obs_in(use).depar_nbc(i)*obs_in(use).depar_nbc(i))
   if (arg_present(minval_nbc) ne 0) then minval_nbc(i) = min([minval_nbc(i),obs_in(use).depar_nbc(i)])
   if (arg_present(maxval_nbc) ne 0) then maxval_nbc(i) = max([maxval_nbc(i),obs_in(use).depar_nbc(i)])

  endif

endfor

end
