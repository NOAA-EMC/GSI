dirs='/global/noscrub/wx23adc/'+$
    ['prAMSRE/','prAMSRE_active/']

prefix=['Control_','Test_']

surface=['Sea','Sea']


date1='2010051100'
date2='2010051412'

outfile='AMSRE_Active_'+surface(0)+'_'+date1+'-'+date2+'.ps'

Expt_Name=['Ctrl','Test']

instruments=['airs_aqua',$
             'amsre_hig_aqua',$
             'amsre_low_aqua',$
             'amsre_mid_aqua',$
             'amsua_aqua',$
             'amsua_metop-a',$
             'amsua_n15',$
             'amsua_n18',$
             'amsub_n15',$
             'amsub_n16',$
             'amsub_n17',$
;             'conv',$                NOT SUPPORTED
             'hirs3_n16',$
             'hirs3_n17',$
             'hirs4_metop-a',$
             'hirs4_n18',$
             'iasi_metop-a',$
             'mhs_metop-a',$
             'mhs_n18',$
 ;            'pcp_ssmi_dmsp',$       NOT SUPPORTED
 ;            'pcp_tmi_trmm',$        NOT SUPPORTED
;             'sbuv2_n16',$           NOT SUPPORTED
;             'sbuv2_n17',$           NOT SUPPORTED
;             'sbuv2_n18',$           NOT SUPPORTED
             'sndrd1_g11',$
             'sndrd1_g13',$
             'sndrd2_g11',$
             'sndrd2_g13',$
             'sndrd3_g11',$
             'sndrd3_g13',$
             'sndrd4_g11',$
             'sndrd4_g13',$
;             'ssmi_f13',$
             'ssmi_f15']

iplot_ges=1
iplot_anl=1

;---------------------------------------------------------------------

if (!d.name eq 'PS') then begin
  device,ysize=9,yoffset=1,/inches,/color,$
           filename=outfile
endif else begin
  window,0,ysize=1000,retain=2
endelse

!p.multi=[0,1,3]

colour_setup
common colours

colors=[default,green,red,orange,blue,cyan,purple,magenta,lime,yellow]

num_instr=size(instruments) & num_instr=num_instr(1)
num_expts=size(dirs) & num_expts=num_expts(1)

for iinst=0,num_instr-1 do begin

  for iexpt=0,num_expts-1 do begin

; First do stats WRT first-guess

  if (iplot_ges eq 1) then begin

    file=dirs(iexpt)+prefix(iexpt)+instruments(iinst)+'_ges_'+$
      surface(iexpt)+'_'+date1+'_'+date2+'.nc'
    ierr=FILE_TEST(file)
    if (ierr ne 1) then begin
      print,'WARNING: '+file+' not found - ignoring this instrument'
      goto,loop
    endif else begin
      a=read_netcdf(file,data,quiet=1)
    endelse 
    
    if (iexpt eq 0) then begin
      nchan=data.n_channels
      sd_bc_ges   = fltarr(num_expts,nchan)
      bias_bc_ges = fltarr(num_expts,nchan)
      sd_nbc_ges   = fltarr(num_expts,nchan)
      bias_nbc_ges = fltarr(num_expts,nchan)
      sd_bc_anl   = fltarr(num_expts,nchan)
      bias_bc_anl = fltarr(num_expts,nchan)
      sd_nbc_anl   = fltarr(num_expts,nchan)
      bias_nbc_anl = fltarr(num_expts,nchan)
      num_ges      = lonarr(num_expts,nchan)
      num_anl      = lonarr(num_expts,nchan)
    endif
    
    
    ; Determine position of required fields in structure
    idepar_bc=where(strtrim(data.variablenames) eq $
                    'Obs-Calc dTb [Bias Corrected]',ct)
    if (ct eq 0) then begin
      print,'ERROR:  Bias corrected departures not found, exiting'
      stop
    endif

    idepar_nbc=where(strtrim(data.variablenames) eq $
                     'Obs-Calc dTb [NOT Bias Corrected]',ct)
    if (ct eq 0) then begin
      print,'ERROR:  Un-bias corrected departures not found, exiting'
      stop
    endif
   
    sd_nbc_ges(iexpt,*)=reform(data.stddev(idepar_nbc,*))
    bias_nbc_ges(iexpt,*)=reform(data.mean(idepar_nbc,*))
    sd_bc_ges(iexpt,*)=reform(data.stddev(idepar_bc,*))
    bias_bc_ges(iexpt,*)=reform(data.mean(idepar_bc,*))
    num_ges(iexpt,*)=reform(data.nsamples(*))

    missing=where(data.nsamples(*) eq 0, ct) 
    if (ct gt 0) then begin 
      sd_nbc_ges(iexpt,missing)=0.
      bias_nbc_ges(iexpt,missing)=0.
      sd_bc_ges(iexpt,missing)=0.
      bias_bc_ges(iexpt,missing)=0.
    endif  

  endif
   
; Now do stats WRT analysis

  if (iplot_anl eq 1) then begin

    file=dirs(iexpt)+prefix(iexpt)+instruments(iinst)+'_anl_'+$
      surface(iexpt)+'_'+date1+'_'+date2+'.nc'
    ierr=FILE_TEST(file)
    if (ierr ne 1) then begin
      print,'WARNING: '+file+' not found - ignoring this instrument'
    endif else begin
      a=read_netcdf(file,data,quiet=1)
    endelse 
    
    if (iexpt eq 0 and iplot_ges eq 0) then begin
      nchan=data.n_channels
      sd_bc_ges   = fltarr(num_expts,nchan)
      bias_bc_ges = fltarr(num_expts,nchan)
      sd_nbc_ges   = fltarr(num_expts,nchan)
      bias_nbc_ges = fltarr(num_expts,nchan)
      sd_bc_anl   = fltarr(num_expts,nchan)
      bias_bc_anl = fltarr(num_expts,nchan)
      sd_nbc_anl   = fltarr(num_expts,nchan)
      bias_nbc_anl = fltarr(num_expts,nchan)
      num_ges      = lonarr(num_expts,nchan)
      num_anl      = lonarr(num_expts,nchan)
    endif

    ; Determine position of required fields in structure
    idepar_bc=where(strtrim(data.variablenames) eq $
                    'Obs-Calc dTb [Bias Corrected]',ct)
    if (ct eq 0) then begin
      print,'ERROR:  Bias corrected departures not found, exiting'
      stop
    endif

    idepar_nbc=where(strtrim(data.variablenames) eq $
                     'Obs-Calc dTb [NOT Bias Corrected]',ct)
    if (ct eq 0) then begin
      print,'ERROR:  Un-bias corrected departures not found, exiting'
      stop
    endif

    sd_nbc_anl(iexpt,*)=reform(data.stddev(idepar_nbc,*))
    bias_nbc_anl(iexpt,*)=reform(data.mean(idepar_nbc,*))
    sd_bc_anl(iexpt,*)=reform(data.stddev(idepar_bc,*))
    bias_bc_anl(iexpt,*)=reform(data.mean(idepar_bc,*))
    num_anl(iexpt,*)=reform(data.nsamples(*))

    missing=where(data.nsamples(*) eq 0, ct)
   if (ct gt 0) then begin 
      sd_nbc_anl(iexpt,missing)=0.
      bias_nbc_anl(iexpt,missing)=0.
      sd_bc_anl(iexpt,missing)=0.
      bias_bc_anl(iexpt,missing)=0.
    endif  

  endif

endfor

  tmp=reform([sd_nbc_ges,sd_nbc_anl,bias_nbc_ges,bias_nbc_anl])
  ymax=max(tmp)
  missing=where(tmp lt -900.,num_missing)
  if (num_missing gt 0) then tmp(missing)=0.0
  ymin=min(tmp)

  ichan=indgen(nchan)+1
  plot,[1,nchan],[ymin,ymax],xs=1,xtitle='Channel Number',ytitle='Departure',$
    title='Statistics before bias correction '+instruments(iinst)+' '+$
           date1+'-'+date2,/nodata
  
  for iexpt=0,num_expts-1 do begin
    icol_off=iexpt*2
    if (iplot_anl eq 1) then $
      oplot,ichan,sd_nbc_anl(iexpt,*),color=colors(icol_off+1),min_value=-100,line=1
    if (iplot_ges eq 1) then $
      oplot,ichan,sd_nbc_ges(iexpt,*),color=colors(icol_off),min_value=-100,line=1
    if (iplot_anl eq 1) then $
      oplot,ichan,bias_nbc_anl(iexpt,*),color=colors(icol_off+1),min_value=-100
    if (iplot_ges eq 1) then $
      oplot,ichan,bias_nbc_ges(iexpt,*),color=colors(icol_off),min_value=-100
    if (iplot_ges eq 1) then xyouts,0.1,0.97-iexpt*0.02,Expt_Name(iexpt)+' (Ges)',$
      /normal,charthick=2,color=colors(icol_off)
    if (iplot_anl eq 1) then xyouts,0.25,0.97-iexpt*0.02,Expt_Name(iexpt)+' (Anal)',$
      /normal,charthick=2,color=colors(icol_off+1)
  endfor
  oplot,[0,nchan+1],[0,0]
  
   xyouts,0.65,0.97,$
    'Solid = Mean Departure!CDotted = Std Dev Departure',$
    /normal,charthick=2 

  tmp=reform([sd_bc_ges,sd_bc_anl,bias_bc_ges,bias_bc_anl])
  ymax=max(tmp)
  missing=where(tmp lt -900.,num_missing)
  if (num_missing gt 0) then tmp(missing)=0.0
  ymin=min(tmp)

  ichan=indgen(nchan)+1
  plot,[1,nchan],[ymin,ymax],xs=1,xtitle='Channel Number',ytitle='Departure',$
    title='Statistics after bias correction '+instruments(iinst)+' '+$
           date1+'-'+date2,/nodata
  
  for iexpt=0,num_expts-1 do begin
    icol_off=iexpt*2
    if (iplot_anl eq 1) then $
      oplot,ichan,sd_bc_anl(iexpt,*),color=colors(icol_off+1),min_value=-100,line=1
    if (iplot_ges eq 1) then $
      oplot,ichan,sd_bc_ges(iexpt,*),color=colors(icol_off),min_value=-100,line=1
    if (iplot_anl eq 1) then $
      oplot,ichan,bias_bc_anl(iexpt,*),color=colors(icol_off+1),min_value=-100
    if (iplot_ges eq 1) then $
      oplot,ichan,bias_bc_ges(iexpt,*),color=colors(icol_off),min_value=-100
   endfor
  oplot,[0,nchan+1],[0,0]
 

  ymin=0
  ymax=max(num_ges)
  plot,[1,nchan],[ymin,ymax],xs=1,xtitle='Channel Number',ytitle='Number',$
    title='Data Numbers for '+instruments(iinst)+' '+$
           date1+'-'+date2,/nodata
  
  for iexpt=0,num_expts-1 do begin
    icol_off=iexpt*2
    if (iplot_ges eq 0) then begin
      oplot,ichan,num_ges(iexpt,*),color=colors(icol_off+1),min_value=-100
    endif else begin
      oplot,ichan,num_anl(iexpt,*),color=colors(icol_off),min_value=-100
    endelse
  endfor
  oplot,[0,nchan+1],[0,0]

loop:

endfor

if (!d.name eq 'PS') then device,/close

end

