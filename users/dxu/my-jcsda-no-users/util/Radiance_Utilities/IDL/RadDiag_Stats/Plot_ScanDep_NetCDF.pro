dirs='/global/noscrub/wx23adc/'+$
    ['prAMSRE/','prAMSRE_active/']

prefix=['Control_','Test_']

surface=['Sea','Sea']


date1='2010051100'
date2='2010051412'

outfile='AMSRE_Active_'+surface(0)+'_'+date1+'-'+date2+'.ps'

Expt_Name=['Ctrl','Test']

;instruments=['amsre_low_aqua','amsre_low_aqua','amsre_low_aqua','amsre_low_aqua',$
;            'amsre_mid_aqua','amsre_mid_aqua','amsre_mid_aqua',$
;            'amsre_mid_aqua','amsre_mid_aqua','amsre_mid_aqua',$
;            'amsre_hig_aqua','amsre_hig_aqua','amsre_hig_aqua','amsre_hig_aqua' ]
channels=[1,2,3,4,5,6,7,8,9,10,11,12]
instruments=['hirs3_n16','hirs3_n16','hirs3_n16','hirs3_n16','hirs3_n16']

iplot_ges=1
iplot_anl=1

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

head=' '

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
      endif else begin
        a=read_netcdf(file,data,quiet=1)
      endelse 
    
      if (iexpt eq 0) then begin
        nfov=data.n_fovs
        sd_bc_ges   = fltarr(num_expts,nfov)
        bias_bc_ges = fltarr(num_expts,nfov)
        sd_nbc_ges   = fltarr(num_expts,nfov)
        bias_nbc_ges = fltarr(num_expts,nfov)
        sd_bc_anl   = fltarr(num_expts,nfov)
        bias_bc_anl = fltarr(num_expts,nfov)
        sd_nbc_anl   = fltarr(num_expts,nfov)
        bias_nbc_anl = fltarr(num_expts,nfov)
        num_ges      = lonarr(num_expts,nfov)
        num_anl      = lonarr(num_expts,nfov)
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
      
      ichan=where(data.channel(*) eq channels(iinst),ct)
      if (ct ne 1) then begin 
        print,'ERROR: Channel ',channels(iinst),' not found for ',instruments(iinst),' exiting'
        stop
      endif
      ichan=ichan(0)
      
      sd_nbc_ges(iexpt,*)=reform(data.scan_stddev(idepar_nbc,ichan,*))
      bias_nbc_ges(iexpt,*)=reform(data.scan_mean(idepar_nbc,ichan,*))
      sd_bc_ges(iexpt,*)=reform(data.scan_stddev(idepar_bc,ichan,*))
      bias_bc_ges(iexpt,*)=reform(data.scan_mean(idepar_bc,ichan,*))
      num_ges(iexpt,*)=reform(data.scan_nsamples(ichan,*))
      
      missing=where(reform(data.nsamples(ichan,*)) eq 0, ct) 
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
        nfov=data.n_fovs
        sd_bc_ges   = fltarr(num_expts,nfov)
        bias_bc_ges = fltarr(num_expts,nfov)
        sd_nbc_ges   = fltarr(num_expts,nfov)
        bias_nbc_ges = fltarr(num_expts,nfov)
        sd_bc_anl   = fltarr(num_expts,nfov)
        bias_bc_anl = fltarr(num_expts,nfov)
        sd_nbc_anl   = fltarr(num_expts,nfov)
        bias_nbc_anl = fltarr(num_expts,nfov)
        num_ges      = lonarr(num_expts,nfov)
        num_anl      = lonarr(num_expts,nfov)
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

      ichan=where(data.channel(*) eq channels(iinst),ct)
      if (ct ne 1) then begin 
        print,'ERROR: Channel ',channels(iinst),' not found for ',instruments(iinst),' exiting'
        stop
      endif
      ichan=ichan(0)
      
      sd_nbc_anl(iexpt,*)=reform(data.scan_stddev(idepar_nbc,ichan,*))
      bias_nbc_anl(iexpt,*)=reform(data.scan_mean(idepar_nbc,ichan,*))
      sd_bc_anl(iexpt,*)=reform(data.scan_stddev(idepar_bc,ichan,*))
      bias_bc_anl(iexpt,*)=reform(data.scan_mean(idepar_bc,ichan,*))
      num_anl(iexpt,*)=reform(data.scan_nsamples(ichan,*))

      missing=where(reform(data.nsamples(ichan,*)) eq 0, ct) 
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

  ifov=indgen(nfov)+1
  plot,[1,nfov],[ymin,ymax],xs=1,xtitle='FOV Number',ytitle='Departure',$
    title='Statistics before bias correction '+instruments(iinst)+' Channel '+$
           string(channels(iinst),format='(i4)')+' '+date1+'-'+date2,/nodata
  
  for iexpt=0,num_expts-1 do begin
    icol_off=iexpt*2
    if (iplot_anl eq 1) then $
      oplot,ifov,sd_nbc_anl(iexpt,*),color=colors(icol_off+1),min_value=-100,line=1
    if (iplot_ges eq 1) then $
      oplot,ifov,sd_nbc_ges(iexpt,*),color=colors(icol_off),min_value=-100,line=1
    if (iplot_anl eq 1) then $
      oplot,ifov,bias_nbc_anl(iexpt,*),color=colors(icol_off+1),min_value=-100
    if (iplot_ges eq 1) then $
      oplot,ifov,bias_nbc_ges(iexpt,*),color=colors(icol_off),min_value=-100
;    use=where(reform(iuse(iexpt,*)) ge 1,ct)
;    if (ct gt 0) then begin
;      ypos=(ymin + iexpt*0.02*(ymax-ymin))+fltarr(ct)
;      oplot,ifov(use),ypos,color=colors(icol_off),psym=8    
;    endif
    if (iplot_ges eq 1) then xyouts,0.1,0.97-iexpt*0.02,Expt_Name(iexpt)+' (Ges)',$
      /normal,charthick=2,color=colors(icol_off)
    if (iplot_anl eq 1) then xyouts,0.25,0.97-iexpt*0.02,Expt_Name(iexpt)+' (Anal)',$
      /normal,charthick=2,color=colors(icol_off+1)
  endfor
  oplot,[0,nfov+1],[0,0]
  
   xyouts,0.65,0.97,$
    'Solid = Mean Departure!CDotted = Std Dev Departure',$
    /normal,charthick=2 

  tmp=reform([sd_bc_ges,sd_bc_anl,bias_bc_ges,bias_bc_anl])
  ymax=max(tmp)
  missing=where(tmp lt -900.,num_missing)
  if (num_missing gt 0) then tmp(missing)=0.0
  ymin=min(tmp)

  ifov=indgen(nfov)+1
  plot,[1,nfov],[ymin,ymax],xs=1,xtitle='FOV Number',ytitle='Departure',$
    title='Statistics after bias correction '+instruments(iinst)+' Channel '+$
           string(channels(iinst),format='(i4)')+' '+date1+'-'+date2,/nodata
  
  for iexpt=0,num_expts-1 do begin
    icol_off=iexpt*2
    if (iplot_anl eq 1) then $
      oplot,ifov,sd_bc_anl(iexpt,*),color=colors(icol_off+1),min_value=-100,line=1
    if (iplot_ges eq 1) then $
      oplot,ifov,sd_bc_ges(iexpt,*),color=colors(icol_off),min_value=-100,line=1
    if (iplot_anl eq 1) then $
      oplot,ifov,bias_bc_anl(iexpt,*),color=colors(icol_off+1),min_value=-100
    if (iplot_ges eq 1) then $
      oplot,ifov,bias_bc_ges(iexpt,*),color=colors(icol_off),min_value=-100
 ;   if (ct gt 0) then begin
 ;     ypos=(ymin + iexpt*0.02*(ymax-ymin))+fltarr(ct)
 ;     oplot,ifov(use),ypos,color=colors(icol_off),psym=8    
 ;   endif    
  endfor
  oplot,[0,nfov+1],[0,0]
 

  ymin=0
  ymax=max(num_ges)
  plot,[1,nfov],[ymin,ymax],xs=1,xtitle='FOV Number',ytitle='Number',$
    title='Data Numbers for '+instruments(iinst)+' Channel '+$
           string(channels(iinst),format='(i4)')+' '+date1+'-'+date2,/nodata
  
  for iexpt=0,num_expts-1 do begin
    icol_off=iexpt*2
    if (iplot_ges eq 0) then begin
      oplot,ifov,num_ges(iexpt,*),color=colors(icol_off+1),min_value=-100
    endif else begin
      oplot,ifov,num_anl(iexpt,*),color=colors(icol_off),min_value=-100
    endelse
;    oplot,[ifov(0),ifov(nfov-1)],num_in(iexpt)*[1,1],color=colors(icol_off),min_value=-100
;    if (ct gt 0) then begin
;      ypos=(ymin + iexpt*0.02*(ymax-ymin))+fltarr(ct)
;      oplot,ifov(use),ypos,color=colors(icol_off),psym=8    
;    endif    
  endfor
  oplot,[0,nfov+1],[0,0]
  
loop:
  
  if (!d.name eq 'X') then read,head
endfor

if (!d.name eq 'PS') then device,/close

end
