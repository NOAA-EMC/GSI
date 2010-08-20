;dirs=[ '/export/lnx403/wd20ca/trial_stats']
dirs='/ptmp/wx23adc/Diags/'+['prAMSRE/',$
    'prAMSRE_active']

date=['2010051100','2010051106','2010051112','2010051118',$
      '2010051200','2010051206','2010051212','2010051218',$
      '2010051300','2010051306','2010051312','2010051318',$
      '2010051400','2010051406','2010051412']
num_dates=size(date) & num_dates=num_dates(1)

outfile='AMSRE_Active_'+date(0)+'-'+date(num_dates-1)+'.ps'

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

write_save=1
read_save=1

iplot_ges=1
iplot_anl=1
land=0

;---------------------------------------------------------------------

if (!d.name eq 'PS') then begin
  device,ysize=9,yoffset=1,/inches,/color,$
           filename=outfile
endif else begin
  window,0,ysize=1000,retain=2
endelse

set_filled_circle

!p.multi=[0,1,3]

colour_setup
common colours

colors=[default,green,red,orange,blue,cyan,purple,magenta,lime,yellow]


num_instr=size(instruments) & num_instr=num_instr(1)
num_expts=size(dirs) & num_expts=num_expts(1)

for iinst=0,num_instr-1 do begin

  for iexpt=0,num_expts-1 do begin

; First do stats WRT first-guess

  nobs=0L
  file=dirs(iexpt)+'/diag_'+instruments(iinst)+'_ges.'+date(0)
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
  obs=obs_in
  meta=meta_in
  nobs=nobs_in


  if (num_dates gt 1) then begin
    for idate=1,num_dates-1 do begin
      file=dirs(iexpt)+'/diag_'+instruments(iinst)+'_ges.'+date(idate)
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
      obs=[obs,obs_in]
      meta=[meta,meta_in]
      nobs=nobs+nobs_in
    endfor
  endif

  if (nobs eq 0L) then begin
       print,'WARNING: '+instruments(iinst)+' not found - ignoring this instrument'
       goto,loop
  endif

; we need to get the number of channels from the first call to the
; diag reading routine for each instrument
    if (iexpt eq 0) then begin
       sd_bc_ges   = fltarr(num_expts,nchan)
       bias_bc_ges = fltarr(num_expts,nchan)
       sd_nbc_ges   = fltarr(num_expts,nchan)
       bias_nbc_ges = fltarr(num_expts,nchan)
       sd_bc_anl   = fltarr(num_expts,nchan)
       bias_bc_anl = fltarr(num_expts,nchan)
       sd_nbc_anl   = fltarr(num_expts,nchan)
       bias_nbc_anl = fltarr(num_expts,nchan)
       num          = lonarr(num_expts,nchan)
       num_in       = lonarr(num_expts)
       iuse         = lonarr(num_expts,nchan)
    endif
    num_in(iexpt)=nobs

; Obtain stats for bias-corrected observations
    diag_stats,obs,meta,nobs,nchan,1,sd,bias,num=num1,land=land
    sd_bc_ges(iexpt,*)=sd
    bias_bc_ges(iexpt,*)=bias
    num(iexpt,*)=num1
; Obtain stats for unbias-corrected observations
    diag_stats,obs,meta,nobs,nchan,0,sd,bias,land=land
    sd_nbc_ges(iexpt,*)=sd
    bias_nbc_ges(iexpt,*)=bias
    iuse(iexpt,*)=chaninfo.iuse(*)

; Now do stats WRT analysis
  if (iplot_anl eq 1) then begin
    file=dirs(iexpt)+'/diag_'+instruments(iinst)+'_anl.'+date(0)
    ierr=FILE_TEST(file+'.sav')
    if (ierr eq 1) then begin
       restore,file+'.sav'
    endif else begin
       read_diags,file,obs_in,meta_in,nobs_in,nchan,chaninfo=chaninfo
       save,file=file+'.sav',obs_in,meta_in,nobs_in,nchan,chaninfo
    endelse 
    obs=obs_in
    meta=meta_in
    nobs=nobs_in


    if (num_dates gt 1) then begin
      for idate=1,num_dates-1 do begin
        file=dirs(iexpt)+'/diag_'+instruments(iinst)+'_anl.'+date(idate)
        ierr=FILE_TEST(file+'.sav')
        if (ierr eq 1) then begin
           restore,file+'.sav'
        endif else begin
          read_diags,file,obs_in,meta_in,nobs_in,nchan,chaninfo=chaninfo
          save,file=file+'.sav',obs_in,meta_in,nobs_in,nchan,chaninfo
        endelse 
        obs=[obs,obs_in]
        meta=[meta,meta_in]
        nobs=nobs+nobs_in
      endfor
    endif

; Obtain stats for bias-corrected observations
      diag_stats,obs,meta,nobs,nchan,1,sd,bias,land=land
      sd_bc_anl(iexpt,*)=sd
      bias_bc_anl(iexpt,*)=bias
; Obtain stats for unbias-corrected observations
      diag_stats,obs,meta,nobs,nchan,0,sd,bias,land=land
      sd_nbc_anl(iexpt,*)=sd
      bias_nbc_anl(iexpt,*)=bias
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
           date(0)+'-'+date(num_dates-1),/nodata
  
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
    use=where(reform(iuse(iexpt,*)) ge 1,ct)
    if (ct gt 0) then begin
      ypos=(ymin + iexpt*0.02*(ymax-ymin))+fltarr(ct)
      oplot,ichan(use),ypos,color=colors(icol_off),psym=8    
    endif
    if (iplot_ges eq 1) then xyouts,0.1,0.97-iexpt*0.02,Expt_Name(iexpt)+' (Ges)',$
      /normal,charthick=2,color=colors(icol_off)
    if (iplot_anl eq 1) then xyouts,0.25,0.97-iexpt*0.02,Expt_Name(iexpt)+' (Anal)',$
      /normal,charthick=2,color=colors(icol_off)
  endfor
  oplot,[0,nchan+1],[0,0]
  
   xyouts,0.65,0.97,$
    'Solid = Mean Departure!CDotted = Std Dev Departure!CDots=Used Obs',$
    /normal,charthick=2 

  tmp=reform([sd_bc_ges,sd_bc_anl,bias_bc_ges,bias_bc_anl])
  ymax=max(tmp)
  missing=where(tmp lt -900.,num_missing)
  if (num_missing gt 0) then tmp(missing)=0.0
  ymin=min(tmp)

  ichan=indgen(nchan)+1
  plot,[1,nchan],[ymin,ymax],xs=1,xtitle='Channel Number',ytitle='Departure',$
    title='Statistics after bias correction '+instruments(iinst)+' '+$
           date(0)+'-'+date(num_dates-1),/nodata
  
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
    if (ct gt 0) then begin
      ypos=(ymin + iexpt*0.02*(ymax-ymin))+fltarr(ct)
      oplot,ichan(use),ypos,color=colors(icol_off),psym=8    
    endif    
  endfor
  oplot,[0,nchan+1],[0,0]
 

  ymin=0
  ymax=max(num_in)
  plot,[1,nchan],[ymin,ymax],xs=1,xtitle='Channel Number',ytitle='Number',$
    title='Data Numbers for '+instruments(iinst)+' '+$
           date(0)+'-'+date(num_dates-1),/nodata
  
  for iexpt=0,num_expts-1 do begin
    icol_off=iexpt*2
    if (iplot_ges eq 0) then begin
      oplot,ichan,num(iexpt,*),color=colors(icol_off+1),min_value=-100
    endif else begin
      oplot,ichan,num(iexpt,*),color=colors(icol_off),min_value=-100
    endelse
    oplot,[ichan(0),ichan(nchan-1)],num_in(iexpt)*[1,1],color=colors(icol_off),min_value=-100
    if (ct gt 0) then begin
      ypos=(ymin + iexpt*0.02*(ymax-ymin))+fltarr(ct)
      oplot,ichan(use),ypos,color=colors(icol_off),psym=8    
    endif    
  endfor
  oplot,[0,nchan+1],[0,0]

loop:

endfor

if (!d.name eq 'PS') then device,/close

end
