pro read_diags,file,obsdata,metadata,nobs,nchan,verbose=verbose,$
     ChanInfo=ChanInfo
;
; Reads in diagnostic files for ** satellite radaiance data only ****
;
; Inputs:
;   file       Filename of diagnostic file
;
; Optional input:
;   verbose    If set to 1 produces more output to screen
;
;
; Outputs:
;   obsdata    Structure containing channel-dependent contents of file
;   metadata   Structure containing non-channel-dependent contents of file
;   nobs       Number of observations in file
;   nchan      Number of channels per observation
;
; Optional output:
;   ChanInfo   Structure containing channel properties
;  
; See the code below for specific contents of the structures.
;
; History:
; 8th June 2010  Initial version to be committed to branch.   A. Collard.
;

if (n_elements(verbose) eq 0) then verbose = 0

close,1

a=0
b=0
metadata=0
obsdata=0

; Define variables to be used for reading file.

isis='123456789abcedfghijk'
plat='123456789a'
obstype='123456789a'
iter=0L
nchan=0L
npred=0L
analdate=0L
num_real=0L
num_int=0L
pchan=0L
iextra=0L
jextra=0L
iuse_rad=0L
nuchan=0L
ich=0L

; First pass is just to get number of obs in file

openr,1,file,/f77_unformatted

readu,1,isis,plat,obstype,iter,nchan,npred,analdate,num_real,$
    pchan,iextra,jextra

if (iextra gt 0) then begin
  print,'Cannot deal with iextra gt 0 for now'
  stop
endif

for i=0,nchan-1 do begin
  readu,1,freq,pol,wave,varch,tlap,iuse_rad,nuchan,ich
  if (verbose eq 1) then print,freq,pol,wave,varch,tlap,iuse_rad,nuchan,ich
endfor

diagbuf=fltarr(num_real)
diagbufchan=fltarr(pchan+npred+1,nchan)

nobs=0
while (nobs lt 1000000L and ~ EOF(1)) do begin
  readu,1,diagbuf,diagbufchan
  nobs=nobs+1
endwhile
print,nobs,' observations to be read in'

close,1

; Now read in data 

openr,1,file,/f77_unformatted

readu,1,isis,plat,obstype,iter,nchan,npred,analdate,num_real,$
    pchan,iextra,jextra

if (iextra gt 0) then begin
  print,'Cannot deal with iextra gt 0 for now'
  stop
endif


; Channel Info array
c={ freq:0., $                  ; Channel Frequency
    pol:0., $                   ; Channel polarization
    wave:0., $                  ; Channel wavenumber
    varch: 9., $                ; Tb variance
    tlap: 0., $                 ; Mean lapse rate for channel (fixed)
    iuse: 0L, $                 ; Useage flag
    channel_number: 0L, $       ; Native channel number
    ich: 0L }                   ; Global channel number
obs_struct_size=n_tags(b)

ChanInfo = replicate(c,nchan)

for i=0,nchan-1 do begin
  readu,1,c
  ChanInfo(i)=c
endfor

diagbuf=fltarr(num_real)
diagbufchan=fltarr(pchan+npred+1,nchan)

a={ cenlat:0.,$                 ; observation latitude (degrees)
    cenlon:0.,$                 ; observation longitude (degrees)
    elevation:0.,$              ; model elevation at observation location
    time:0.,$                   ; obs time (hours relative to analysis time)
    scan_pos:0.,$               ; sensor scan position
    satzen:0.,$                 ; satellite zenith angle (degrees)
    satazimuth:0.,$             ; satellite azimuth angle (degrees)
    solzen:0.,$                 ; solar zenith angle (degrees)
    solazimuth:0.,$             ; solar azimuth angle (degrees)
    sun_glint:0.,$              ; sun glint angle (degrees) (sgagl)
    water_coverage:0.,$         ; fractional coverage by water
    land_coverage:0.,$          ; fractional coverage by land
    ice_coverage:0.,$           ; fractional coverage by ice
    snow_coverage:0.,$          ; fractional coverage by snow
    water_temperature:0.,$      ; surface temperature over water (K)
    land_temperature:0.,$       ; surface temperature over land (K)
    ice_temperature:0.,$        ; surface temperature over ice (K)
    snow_temperature:0.,$       ; surface temperature over snow (K)
    soil_temperature:0.,$       ; soil temperature (K)
    soil_moisture_content:0.,$  ; soil moisture
    land_type:0.,$              ; surface land type
    vegetation_fraction:0.,$    ; vegetation fraction
    snow_depth:0.,$             ; snow depth
    wind_speed:0.,$             ; surface wind speed (m/s)
    cldfrac_clw:0.,$            ; cloud fraction (%)
    cldp_tpwc:0. }              ; cloud top pressure (hPa)

if (n_tags(a) ne num_real) then begin
  print,'Number of elemenents in diagbuf is different to that expected for Meta structure:',n_tags(a)
  stop
endif

metadata=replicate(a,nobs)


; There are more elements that can go into this structure but this is
; enough for now.
b={ obs:fltarr(nchan), $        ; Observed BT
    Depar_BC:fltarr(nchan), $   ; Observed - Calculated with Bias Correction
    Depar_NBC:fltarr(nchan), $  ; Observed - Calculated without Bias Correction
    errinv:fltarr(nchan), $     ; Inverse of observation error
    qc_flag:fltarr(nchan), $    ; QC Flag
    surf_emiss:fltarr(nchan), $ ; Surface Emissivity
    tlapchan:fltarr(nchan), $   ; Stability Index
    bias_terms:fltarr(nchan,npred+1) } ; Bias corrections terms
obs_struct_size=n_tags(b)

ObsData = replicate(b,nobs)


diagbuf=fltarr(num_real)
diagbufchan=fltarr(pchan+npred+1,nchan)

for iobs=0,nobs-1 do begin

  readu,1,diagbuf,diagbufchan
  for i=0,num_real-1 do begin
    metadata(iobs).(i)=diagbuf(i)
  endfor

  for i=0,6 do begin
    obsdata(iobs).(i)=diagbufchan(i,*)
  endfor

  for i=0,npred-1 do begin
    obsdata(iobs).bias_terms(*,i)=diagbufchan(7+i,*)
  endfor

endfor

close,1

end
