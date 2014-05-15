pro read_diags,file,obsdata,metadata,nobs,nchan,verbose=verbose,$
    Old_Version=Old_Version,ChanInfo=ChanInfo,Swap_Endian=Swap_Endian,$
    diagbufex=diagbufex
;
; Reads in diagnostic files for ** satellite radiance data only ****
;
; Inputs:
;   file       Filename of diagnostic file
;
; Optional input:
;   verbose     If set to 1 produces more output to screen
;   Old_Version Set to non-zero if using diagnostic files previous to
;               the introduction of version numbers:
;                   1 = Version used prior to bias correction changes
;                       at revision 10314 (all operation runs prior to 
;                       introduction of scan angles in variational
;                       bias correction).  DEFAULT
;                   2 = Versions after 10314 but before the
;                       introduction of version numbers in the files.
;                       Scan angle from external file. 
;                   3 = Versions after 10314 but before the
;                       introduction of version numbers in the files.
;                       Variational scan angle bias correction. 
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
; 6th Jan  2011  Added /Swap_If_Little_Endian, assuming the diag files
;                are always produced on a big endian machine (e.g.,
;                CCS,vapor).   A. Collard. 
; 7th Jan  2011  Extended the definitions of the channel structures to
;                explicitly specify the bias correction terms.
; 8th Apr  2011  Fixed a couple of bugs when working with most recent versions
;                of diag files.
;

if (n_elements(verbose) eq 0) then verbose = 0
if (n_elements(Old_Version) eq 0) then Old_Version = 1
if (n_elements(Swap_Endian) eq 0) then Swap_Endian = 0

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
idiag=0L
angord=0L
iversion=0L

; First pass is just to get number of obs in file

openr,1,file,/f77_unformatted,swap_endian=swap_endian

ON_IOERROR,Old_Version
readu,1,isis,plat,obstype,iter,nchan,npred,analdate,num_real,$
    pchan,iextra,jextra,idiag,angord,iversion
if (verbose eq 1) then print,'Version number, angord = ',iversion,angord

Old_Version=0
Old_Version: If (Old_Version ne 0) then begin
  close,1
  ON_IOERROR,NULL
  openr,1,file,/f77_unformatted,swap_endian=swap_endian
  readu,1,isis,plat,obstype,iter,$
    nchan,npred,analdate,num_real,pchan,iextra,jextra
endif

if (iextra gt 0 and jextra gt 0) then begin
  diagbufex=fltarr(iextra,jextra)
endif else begin
  diagbufex=0.0
endelse

for i=0,nchan-1 do begin
  readu,1,freq,pol,wave,varch,tlap,iuse_rad,nuchan,ich
  if (verbose eq 1) then print,freq,pol,wave,varch,tlap,iuse_rad,nuchan,ich
endfor

diagbuf=fltarr(num_real)

case Old_Version of
0:
1: idiag=pchan+npred+1
2: idiag=pchan+7
3: idiag=pchan+11
else: Old_Version = -1L 
endcase 

if (Old_Version eq -1) then begin
  print,'Old_Version =',Old_Version,$
       ' is not supported - refer to comments'+$
       ' in read_diags.pro for details' 
  stop
endif

diagbufchan=fltarr(idiag,nchan)

nobs=0L
while (nobs lt 1000000L and ~ EOF(1)) do begin
  if (iextra gt 0 and jextra gt 0) then begin
    readu,1,diagbuf,diagbufchan,diagbufex
  endif else begin
    readu,1,diagbuf,diagbufchan
  endelse
  nobs=nobs+1L
endwhile
if (verbose eq 1) then print,nobs,' observations to be read in'

close,1

; Now read in data 

if (verbose eq 1) then print,'Opening file for second pass'

openr,1,file,/f77_unformatted,swap_endian=swap_endian

if (old_version gt 0) then begin
  readu,1,isis,plat,obstype,iter,nchan,npred,analdate,num_real,$
      pchan,iextra,jextra
endif else begin
  readu,1,isis,plat,obstype,iter,nchan,npred,analdate,num_real,$
      pchan,iextra,jextra,idiag,angord,iversion
endelse

if (verbose eq 1) then print,'Setting up structures'

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


if (old_version ne 0) then begin
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
endif


; Define structure for channel-by-channel information.
case old_version of 
0:  ; Zero dealt with below
1:  b={ obs:fltarr(nchan), $        ; Observed BT
        Depar_BC:fltarr(nchan), $   ; Observed - Calculated with Bias Correction
        Depar_NBC:fltarr(nchan), $  ; Observed - Calculated without Bias Correction
        errinv:fltarr(nchan), $     ; Inverse of observation error
        qc_flag:fltarr(nchan), $    ; QC Flag
        surf_emiss:fltarr(nchan), $ ; Surface Emissivity
        tlapchan:fltarr(nchan), $   ; Stability Index
        bias_scanangle_term:fltarr(nchan), $     ; External scan angle bias corrections term
        bias_lap_term:fltarr(nchan), $           ; Lapse rate bias corrections term
        bias_lap2_term:fltarr(nchan), $          ; Square of lapse rate bias corrections terms
        bias_constant_term:fltarr(nchan), $      ; Constant bias corrections term
        bias_cosscanangle_term:fltarr(nchan), $  ; Old scan-angle bias corrections term
                                                 ; (obsolete with new scan angle corrections)
        bias_clwsst_term:fltarr(nchan) }         ; Cloud liquid water/SST bias corrections term
2:  b={ obs:fltarr(nchan), $        ; Observed BT
        Depar_BC:fltarr(nchan), $   ; Observed - Calculated with Bias Correction
        Depar_NBC:fltarr(nchan), $  ; Observed - Calculated without Bias Correction
        errinv:fltarr(nchan), $     ; Inverse of observation error
        qc_flag:fltarr(nchan), $    ; QC Flag
        surf_emiss:fltarr(nchan), $ ; Surface Emissivity
        tlapchan:fltarr(nchan), $   ; Stability Index
        bias_constant_term:fltarr(nchan), $           ; Constant bias corrections term
        bias_cosscanangle_term:fltarr(nchan), $       ; Cosine scan-angle bias corrections term
        bias_clw_term:fltarr(nchan), $                ; Cloud liquid water bias corrections term
        bias_lap2_term:fltarr(nchan), $               ; Square of lapse rate bias corrections term
        bias_lap_term:fltarr(nchan), $                ; Lapse rate bias corrections terms
        bias_totscanangle_term:fltarr(nchan),$        ; Total scan angle bias corrections terms
        bias_sst_term:fltarr(nchan) }                 ; Sea surface temperature corrections terms
3:  b={ obs:fltarr(nchan), $        ; Observed BT
        Depar_BC:fltarr(nchan), $   ; Observed - Calculated with Bias Correction
        Depar_NBC:fltarr(nchan), $  ; Observed - Calculated without Bias Correction
        errinv:fltarr(nchan), $     ; Inverse of observation error
        qc_flag:fltarr(nchan), $    ; QC Flag
        surf_emiss:fltarr(nchan), $ ; Surface Emissivity
        tlapchan:fltarr(nchan), $   ; Stability Index
        bias_constant_term:fltarr(nchan), $           ; Constant bias corrections term
        bias_cosscanangle_term:fltarr(nchan), $       ; Cos scan-angle bias corrections term
                                                      ; (obsolete with new scan angle corrections)
        bias_clw_term:fltarr(nchan), $                ; Cloud liquid water bias corrections term
        bias_lap2_term:fltarr(nchan), $               ; Square of lapse rate bias corrections term
        bias_lap_term:fltarr(nchan), $                ; Lapse rate bias corrections terms
        bias_scanangle_terms:fltarr(nchan,4),$        ; Scan angle bias corrections terms
        bias_totscanangle_term:fltarr(nchan),$        ; Total scan angle bias corrections terms
        bias_sst_term:fltarr(nchan) }                 ; Sea surface temperature corrections terms
endcase 

if (old_version eq 0) then begin

   if (iversion lt 13784) then begin
   
     b={ obs:fltarr(nchan), $        ; Observed BT
          Depar_BC:fltarr(nchan), $   ; Observed - Calculated with Bias Correction
          Depar_NBC:fltarr(nchan), $  ; Observed - Calculated without Bias Correction
          errinv:fltarr(nchan), $     ; Inverse of observation error
          qc_flag:fltarr(nchan), $    ; QC Flag
          surf_emiss:fltarr(nchan), $ ; Surface Emissivity
          tlapchan:fltarr(nchan), $   ; Stability Index
          bias_constant_term:fltarr(nchan), $           ; Constant bias corrections term
          bias_cosscanangle_term:fltarr(nchan), $       ; Cos scan-angle bias corrections term
                                                        ; (obsolete with new scan angle corrections)
          bias_clw_term:fltarr(nchan), $                ; Cloud liquid water bias corrections term
          bias_lap2_term:fltarr(nchan), $               ; Square of lapse rate bias corrections term
          bias_lap_term:fltarr(nchan), $                ; Lapse rate bias corrections term
          bias_scanangle_terms:fltarr(nchan,angord+1),$ ; Scan angle bias corrections terms
          bias_sst_term:fltarr(nchan) }                 ; Sea surface temperature corrections terms
   endif else if (iversion lt 19180) then begin
        b={ obs:fltarr(nchan), $        ; Observed BT
            Depar_BC:fltarr(nchan), $   ; Observed - Calculated with Bias Correction
            Depar_NBC:fltarr(nchan), $  ; Observed - Calculated without Bias Correction
            errinv:fltarr(nchan), $     ; Inverse of observation error
            qc_flag:fltarr(nchan), $    ; QC Flag
            surf_emiss:fltarr(nchan), $ ; Surface Emissivity
            tlapchan:fltarr(nchan), $   ; Stability Index
            tb_tz:fltarr(nchan), $      ; SST temperature gradient
            bias_constant_term:fltarr(nchan), $           ; Constant bias corrections term
            bias_cosscanangle_term:fltarr(nchan), $       ; Cos scan-angle bias corrections term
                                                          ; (obsolete with new scan angle corrections)
            bias_clw_term:fltarr(nchan), $                ; Cloud liquid water bias corrections term
            bias_lap2_term:fltarr(nchan), $               ; Square of lapse rate bias corrections term
            bias_lap_term:fltarr(nchan), $                ; Lapse rate bias corrections terms
            bias_scanangle_terms:fltarr(nchan,angord+1),$ ; Scan angle bias corrections terms (last term is total scan correction)
            bias_sst_term:fltarr(nchan) }                 ; Sea surface temperature corrections terms
   endif else if (iversion lt 30303) then begin
       b={ obs:fltarr(nchan), $        ; Observed BT
          Depar_BC:fltarr(nchan), $   ; Observed - Calculated with Bias Correction
          Depar_NBC:fltarr(nchan), $  ; Observed - Calculated without Bias Correction
          errinv:fltarr(nchan), $     ; Inverse of observation error
          qc_flag:fltarr(nchan), $    ; QC Flag
          surf_emiss:fltarr(nchan), $ ; Surface Emissivity
          tlapchan:fltarr(nchan), $   ; Stability Index
          tb_tz:fltarr(nchan), $      ; SST temperature gradient
          bias_constant_term:fltarr(nchan), $           ; Constant bias corrections term
          bias_cosscanangle_term:fltarr(nchan), $       ; Cos scan-angle bias corrections term
                                                        ; (obsolete with new scan angle corrections)
          bias_clw_term:fltarr(nchan), $                ; Cloud liquid water bias corrections term
          bias_lap2_term:fltarr(nchan), $               ; Square of lapse rate bias corrections term
          bias_lap_term:fltarr(nchan), $                ; Lapse rate bias corrections term
          bias_cos_term:fltarr(nchan), $                ; Cosine of solar zenith angle term
          bias_sin_term:fltarr(nchan), $                ; Sine of solar zenith angle term
          bias_scanangle_terms:fltarr(nchan,angord+1),$ ; Scan angle bias corrections terms
          bias_sst_term:fltarr(nchan) }                 ; Sea surface temperature corrections terms
   endif else begin          
       b={ obs:fltarr(nchan), $        ; Observed BT
          Depar_BC:fltarr(nchan), $   ; Observed - Calculated with Bias Correction
          Depar_NBC:fltarr(nchan), $  ; Observed - Calculated without Bias Correction
          errinv:fltarr(nchan), $     ; Inverse of observation error
          qc_flag:fltarr(nchan), $    ; QC Flag
          surf_emiss:fltarr(nchan), $ ; Surface Emissivity
          tlapchan:fltarr(nchan), $   ; Stability Index
          tb_tz:fltarr(nchan), $      ; SST temperature gradient
          bias_constant_term:fltarr(nchan), $           ; Constant bias corrections term
          bias_cosscanangle_term:fltarr(nchan), $       ; Cos scan-angle bias corrections term
                                                        ; (obsolete with new scan angle corrections)
          bias_clw_term:fltarr(nchan), $                ; Cloud liquid water bias corrections term
          bias_lap2_term:fltarr(nchan), $               ; Square of lapse rate bias corrections term
          bias_lap_term:fltarr(nchan), $                ; Lapse rate bias corrections term
          bias_cos_term:fltarr(nchan), $                ; Cosine of solar zenith angle term
          bias_sin_term:fltarr(nchan), $                ; Sine of solar zenith angle term
          bias_emiss_term:fltarr(nchan), $              ; Surface emissivity term
          bias_scanangle_terms:fltarr(nchan,angord+1),$ ; Scan angle bias corrections terms
          bias_sst_term:fltarr(nchan) }                 ; Sea surface temperature corrections terms
   endelse

   if (iversion lt 13784) then begin

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
          cldp_tpwc:0.}               ; cloud top pressure (hPa)

   endif else begin

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
          cldp_tpwc:0., $             ; cloud top pressure (hPa)
          tref: 0., $                 ; For NSST
          dtw:  0., $                 ; For NSST
          dtc:  0., $                 ; For NSST
          tz_tr: 0. }                 ; For NSST

   endelse
endif
 
if (n_tags(a) ne num_real) then begin
  print,'Number of elemenents in diagbuf is different to that expected for Meta structure:',n_tags(a)
  stop
endif

metadata=replicate(a,nobs)

obs_struct_size=n_tags(b)

ObsData = replicate(b,nobs)

if (verbose eq 1) then print,'Populating structures'

ON_IOERROR,NULL

for iobs=0L,nobs-1 do begin
  if (iextra gt 0 and jextra gt 0) then begin
    readu,1,diagbuf,diagbufchan,diagbufex
  endif else begin
    readu,1,diagbuf,diagbufchan
  endelse
  for i=0,num_real-1 do begin
    metadata(iobs).(i)=diagbuf(i)
  endfor

  if (old_version ne 0 or angord eq 0) then begin
    for i=0,idiag-1 do begin
      obsdata(iobs).(i)=diagbufchan(i,*)
    endfor
  endif else if (iversion lt 13784) then begin
    obsdata(iobs).obs                              = diagbufchan(0,*)
    obsdata(iobs).Depar_BC                         = diagbufchan(1,*)
    obsdata(iobs).Depar_NBC                        = diagbufchan(2,*)
    obsdata(iobs).errinv                           = diagbufchan(3,*)
    obsdata(iobs).qc_flag                          = diagbufchan(4,*)
    obsdata(iobs).surf_emiss                       = diagbufchan(5,*)
    obsdata(iobs).tlapchan                         = diagbufchan(6,*)
    obsdata(iobs).bias_constant_term               = diagbufchan(7,*)
    obsdata(iobs).bias_cosscanangle_term           = diagbufchan(8,*)
    obsdata(iobs).bias_clw_term                    = diagbufchan(9,*)
    obsdata(iobs).bias_lap2_term                   = diagbufchan(10,*)
    obsdata(iobs).bias_lap_term                    = diagbufchan(11,*)
        for iang=0,angord do begin
      obsdata(iobs).bias_scanangle_terms(*,iang)   = diagbufchan(12+iang,*)
    endfor
    obsdata(iobs).bias_sst_term                    = diagbufchan(13,*)
  endif else if (iversion lt 19180) then begin
    obsdata(iobs).obs                              = diagbufchan(0,*)
    obsdata(iobs).Depar_BC                         = diagbufchan(1,*)
    obsdata(iobs).Depar_NBC                        = diagbufchan(2,*)
    obsdata(iobs).errinv                           = diagbufchan(3,*)
    obsdata(iobs).qc_flag                          = diagbufchan(4,*)
    obsdata(iobs).surf_emiss                       = diagbufchan(5,*)
    obsdata(iobs).tlapchan                         = diagbufchan(6,*)
    obsdata(iobs).tb_tz                            = diagbufchan(7,*)
    obsdata(iobs).bias_constant_term               = diagbufchan(8,*)
    obsdata(iobs).bias_cosscanangle_term           = diagbufchan(9,*)
    obsdata(iobs).bias_clw_term                    = diagbufchan(10,*)
    obsdata(iobs).bias_lap2_term                   = diagbufchan(11,*)
    obsdata(iobs).bias_lap_term                    = diagbufchan(12,*)
    obsdata(iobs).bias_cos_term                    = diagbufchan(13,*)
    obsdata(iobs).bias_sin_term                    = diagbufchan(14,*)
    for iang=0,angord do begin
      obsdata(iobs).bias_scanangle_terms(*,iang)   = diagbufchan(15+iang,*)
    endfor
    obsdata(iobs).bias_sst_term                    = diagbufchan(16+angord,*)
  endif else begin
    obsdata(iobs).obs                              = diagbufchan(0,*)
    obsdata(iobs).Depar_BC                         = diagbufchan(1,*)
    obsdata(iobs).Depar_NBC                        = diagbufchan(2,*)
    obsdata(iobs).errinv                           = diagbufchan(3,*)
    obsdata(iobs).qc_flag                          = diagbufchan(4,*)
    obsdata(iobs).surf_emiss                       = diagbufchan(5,*)
    obsdata(iobs).tlapchan                         = diagbufchan(6,*)
    obsdata(iobs).tb_tz                            = diagbufchan(7,*)
    obsdata(iobs).bias_constant_term               = diagbufchan(8,*)
    obsdata(iobs).bias_cosscanangle_term           = diagbufchan(9,*)
    obsdata(iobs).bias_clw_term                    = diagbufchan(10,*)
    obsdata(iobs).bias_lap2_term                   = diagbufchan(11,*)
    obsdata(iobs).bias_lap_term                    = diagbufchan(12,*)
    obsdata(iobs).bias_cos_term                    = diagbufchan(13,*)
    obsdata(iobs).bias_sin_term                    = diagbufchan(14,*)
    obsdata(iobs).bias_emiss_term                  = diagbufchan(15,*)
    for iang=0,angord do begin
      obsdata(iobs).bias_scanangle_terms(*,iang)   = diagbufchan(16+iang,*)
    endfor
    obsdata(iobs).bias_sst_term                    = diagbufchan(17,*)
  endelse

endfor

close,1

return

Error_Message:  print,'Error Reading input file'
  stop

end
