pro read_conv,file,ps=ps,t=t,q=q,uv=uv,sst=sst,gps=gps,$
       spd=spd,pw=pw,verbose=verbose,Swap_Endian=Swap_Endian


ips=0 & it=0 & iq=0 & iuv=0 & 
isst=0 & igps=0 & ispd=0 & ipw=0

if (arg_present(verbose) eq 0) then verbose = 0
if (arg_present(ps) gt 0) then ips = 1
if (arg_present(t) gt 0) then it = 1
if (arg_present(q) gt 0) then iq = 1
if (arg_present(uv) gt 0) then iuv = 1
if (arg_present(sst) gt 0) then isst = 1
if (arg_present(gps) gt 0) then igps = 1
if (arg_present(spd) gt 0) then ispd = 1
if (arg_present(pw) gt 0) then ipw = 1
if (arg_present(Swap_Endian) eq 0) then Swap_Endian = 0

close,1

a=0
b=0
metadata=0
obsdata=0

; Define variables to be used for reading file.

idate=0l
parameter='abc'
nchar=1L
nobs_in=1L
mype=0L
nreal=0L

num_obs_ps   = 0L
num_obs_t    = 0L
num_obs_q    = 0L
num_obs_uv   = 0L
num_obs_sst  = 0L
num_obs_gps  = 0L
num_obs_spd  = 0L
num_obs_pw   = 0L


; First pass is just to get number of obs in file

openr,1,file,/f77_unformatted,swap_endian=swap_endian

readu,1,idate

nobs=0
while (nobs lt 1000000L and ~ EOF(1)) do begin
  parameter='abc'
  readu,1,parameter,nchar,nreal,nobs_in,mype
  if (nobs_in gt 0) then begin 
   cdiagbuf=replicate('12345678',nobs_in)
    rdiagbuf=fltarr(nreal,nobs_in)
    readu,1,cdiagbuf,rdiagbuf
    case parameter of
     ' ps' : num_obs_ps  = num_obs_ps  + nobs_in
     '  t' : num_obs_t   = num_obs_t   + nobs_in
     '  q' : num_obs_q   = num_obs_q   + nobs_in
     ' uv' : num_obs_uv  = num_obs_uv  + nobs_in
     'sst' : num_obs_sst = num_obs_sst + nobs_in
     'gps' : num_obs_gps = num_obs_gps + nobs_in
     'spd' : num_obs_spd = num_obs_spd + nobs_in
     ' pw' : num_obs_pw  = num_obs_pw  + nobs_in
     else  :
    endcase
    nobs=nobs+1
  endif else begin
    readu,1
  endelse
endwhile

close,1

if (verbose eq 1) then print,num_obs_ps,num_obs_t,num_obs_q,$
  num_obs_uv,num_obs_sst,num_obs_gps,num_obs_spd,$
  num_obs_pw,' observations to be read in'

; ps, t, q, sst, gps, spd, pw

a={ station_id:'XXXXXXXX',$     ; Station ID
    obstype:0L,$                ; observation type
    obssubtype: 0L,$            ; observation subtype           
    latitude:0.,$               ; observation latitude (degrees)
    longitude:0.,$              ; observation longitude (degrees)
    elevation:0.,$              ; station elevation (m)
    pressure:0.,$               ; observation pressure (hPa)
    height:0.,$                 ; observation height (m)
    time:0.,$                   ; obs time (hours relative to analysis time)
    qc_mark: 0L, $              ; Input quality mark
    qc_flag: 0L, $              ; Setup or event flag
    use_flag: 0L, $             ; Prepbufr data use flag 
    anl_use_flag: 0L, $         ; Analysis data use flag
    qc_rel_weight: 0.,$         ; Non-linear qc relative weight
    errinv_input: 0., $         ; Prepbufr inverse ob error    
    errinv_adjust: 0., $        ; Read_Prepbufr inverse ob error    
    errinv_final: 0., $         ; Final inverse ob error    
    obsvalue: 0., $             ; Observation value    
    depar_bc: 0., $             ; Observation departure with bias correction
    depar_nbc: 0.,$             ; Obs departure without bias correction
    extra_field: 0.}            ; For q this is guess saturation spec. hum.

; gps                           ; For sst this is measurement type
                                ; For spd this is 10m wind reduction factor
a_gps={ station_id:'XXXXXXXX',$     ; Station ID
    obstype:0L,$                ; observation type
    obssubtype: 0L,$            ; observation subtype (0=refrac, 1=bend)  
    latitude:0.,$               ; observation latitude (degrees)
    longitude:0.,$              ; observation longitude (degrees)
    incr_refrac:0.,$            ; incremental refractivity (x100 %) (depart?)
    pressure:0.,$               ; observation pressure (hPa)
    height:0.,$                 ; observation height (m)
    time:0.,$                   ; obs time (hours relative to analysis time)
    qc_mark: 0L, $              ; Input quality mark
    qc_flag: 0L, $              ; Setup or event flag
    use_flag: 0L, $             ; Prepbufr data use flag 
    anl_use_flag: 0L, $         ; Analysis data use flag
    qc_rel_weight: 0.,$         ; Non-linear qc relative weight
    errinv_input: 0., $         ; Prepbufr inverse ob error    
    errinv_adjust: 0., $        ; Read_Prepbufr inverse ob error    
    errinv_final: 0., $         ; Final inverse ob error    
    obsvalue: 0., $             ; Observation value    
    trefges: 0., $              ; temperature at obs location in Kelvin
    hobl: 0.,$                  ; model vertical grid  (midpoint)
    extra_field: 0.}            ; For q this is guess saturation spec. hum.
                                ; For sst this is measurement type
                                ; For spd this is 10m wind reduction factor
; uv

a_uv ={ station_id:'XXXXXXXX',$ ; Station ID
    obstype:0L,$                ; observation type
    obssubtype: 0L,$            ; observation subtype           
    latitude:0.,$               ; observation latitude (degrees)
    longitude:0.,$              ; observation longitude (degrees)
    elevation:0.,$              ; station elevation (m)
    pressure:0.,$               ; observation pressure (hPa)
    height:0.,$                 ; observation height (m)
    time:0.,$                   ; obs time (hours relative to analysis time)
    qc_mark: 0L, $              ; Input qulaity mark
    qc_flag: 0L, $              ; Setup or event flag
    use_flag: 0L, $             ; Prepbufr data use flag 
    anl_use_flag: 0L, $         ; Analysis data use flag
    qc_rel_weight: 0.,$         ; Non-linear qc relative weight
    errinv_input: 0., $         ; Prepbufr inverse ob error    
    errinv_adjust: 0., $        ; Read_Prepbufr inverse ob error    
    errinv_final: 0., $         ; Final inverse ob error    
    u_obs: 0.,$                 ; U component Observation value    
    u_depar_bc: 0.,$            ; U component obs depar with bias correction
    u_depar_nbc: 0.,$           ; U component obs depar without bias correction
    v_obs: 0., $                ; U component Observation value    
    v_depar_bc: 0., $           ; U component obs depar with bias correction
    v_depar_nbc: 0.,$           ; U component obs depar without bias correction
    red_fac_10m: 0.}            ; 10m wind reduction factor



; Only initialise arrays that we are returning:

if (ips gt 0) then begin
  if (num_obs_ps eq 0) then begin
    ps=a
    for i=1,20 do ps.(i)=-999
  endif else begin
    ps=replicate(a,num_obs_ps)
  endelse
endif

if (it gt 0) then begin
  if (num_obs_t eq 0) then begin
    t=a
    for i=1,20 do t.(i)=-999
  endif else begin
    t=replicate(a,num_obs_t)
  endelse
endif

if (iq gt 0) then begin
  if (num_obs_q eq 0) then begin
    q=a
    for i=1,20 do q.(i)=-999
  endif else begin
    q=replicate(a,num_obs_q)
  endelse
endif

if (iuv gt 0) then begin
  if (num_obs_uv eq 0) then begin
    uv=a_uv
    for i=1,23 do uv.(i)=-999
  endif else begin
    uv=replicate(a_uv,num_obs_uv)
  endelse
endif

if (isst gt 0) then begin
  if (num_obs_sst eq 0) then begin
    sst=a
    for i=1,20 do sst.(i)=-999
  endif else begin
    sst=replicate(a,num_obs_sst)
  endelse
endif

if (igps gt 0) then begin
  if (num_obs_gps eq 0) then begin
    gps=a_gps
    for i=1,20 do gps.(i)=-999
  endif else begin
    gps=replicate(a_gps,num_obs_gps)
  endelse
endif

if (ispd gt 0) then begin
  if (num_obs_spd eq 0) then begin
    spd=a
    for i=1,20 do spd.(i)=-999
  endif else begin
    spd=replicate(a,num_obs_spd)
  endelse
endif

if (ipw gt 0) then begin
  if (num_obs_pw eq 0) then begin
    pw=a
    for i=1,20 do pw.(i)=-999
  endif else begin
    pw=replicate(a,num_obs_pw)
  endelse
endif

; Now read in data 

openr,1,file,/f77_unformatted,swap_endian=swap_endian

readu,1,idate

num_obs_ps   = 0L
num_obs_t    = 0L
num_obs_q    = 0L
num_obs_uv   = 0L
num_obs_sst  = 0L
num_obs_gps  = 0L
num_obs_spd  = 0L
num_obs_pw   = 0L

for iobs=0,nobs-1 do begin
  parameter='abc'
  readu,1,parameter,nchar,nreal,nobs_in,mype
  if (nobs_in gt 0) then begin
    cdiagbuf=replicate('12345678',nobs_in)
    rdiagbuf=fltarr(nreal,nobs_in)
    readu,1,cdiagbuf,rdiagbuf

    if (parameter eq ' ps' and ips eq 1) then begin
      for i=0,nreal-1 do ps(num_obs_ps:num_obs_ps+nobs_in-1).(i+1) = $
          reform(rdiagbuf(i,*))
      ps(num_obs_ps:num_obs_ps+nobs_in-1).station_id=cdiagbuf
      num_obs_ps=num_obs_ps+nobs_in
    endif 

    if (parameter eq '  t' and it eq 1) then begin
      for i=0,nreal-1 do t(num_obs_t:num_obs_t+nobs_in-1).(i+1)=reform(rdiagbuf(i,*))
      t(num_obs_t:num_obs_t+nobs_in-1).station_id=cdiagbuf
      num_obs_t=num_obs_t+nobs_in
    endif

    if (parameter eq '  q' and iq eq 1) then begin
      for i=0,nreal-1 do q(num_obs_q:num_obs_q+nobs_in-1).(i+1)=reform(rdiagbuf(i,*))
      q(num_obs_q:num_obs_q+nobs_in-1).station_id=cdiagbuf
      num_obs_q=num_obs_q+nobs_in
    endif

    if (parameter eq ' uv' and iuv eq 1) then begin
      for i=0,nreal-1 do uv(num_obs_uv:num_obs_uv+nobs_in-1).(i+1)=reform(rdiagbuf(i,*))
      uv(num_obs_uv:num_obs_uv+nobs_in-1).station_id=cdiagbuf
      num_obs_uv=num_obs_uv+nobs_in
    endif

    if (parameter eq 'sst' and isst eq 1) then begin
      for i=0,nreal-1 do sst(num_obs_sst:num_obs_sst+nobs_in-1).(i+1)=reform(rdiagbuf(i,*))
      sst(num_obs_sst:num_obs_sst+nobs_in-1).station_id=cdiagbuf
      num_obs_sst=num_obs_sst+nobs_in
    endif

    if (parameter eq 'gps' and igps eq 1) then begin
      for i=0,nreal-1 do gps(num_obs_gps:num_obs_gps+nobs_in-1).(i+1)=reform(rdiagbuf(i,*))
      gps(num_obs_gps:num_obs_gps+nobs_in-1).station_id=cdiagbuf
      num_obs_gps=num_obs_gps+nobs_in
    endif

    if (parameter eq 'spd' and ispd eq 1) then begin
      for i=0,nreal-1 do spd(num_obs_spd:num_obs_spd+nobs_in-1).(i+1)=reform(rdiagbuf(i,*))
      spd(num_obs_spd:num_obs_spd+nobs_in-1).station_id=cdiagbuf
      num_obs_spd=num_obs_spd+nobs_in
    endif

    if (parameter eq ' pw' and ipw eq 1) then begin
      for i=0,nreal-1 do pw(num_obs_pw:num_obs_pw+nobs_in-1).(i+1)=reform(rdiagbuf(i,*))
      pw(num_obs_pw:num_obs_pw+nobs_in-1).station_id=cdiagbuf
      num_obs_pw=num_obs_pw+nobs_in
    endif

  endif else begin
    readu,1
  endelse

endfor

EndFile1: close,1

end
