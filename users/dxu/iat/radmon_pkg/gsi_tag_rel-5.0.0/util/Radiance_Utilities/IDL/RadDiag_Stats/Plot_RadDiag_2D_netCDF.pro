file='CRTM_2.0.2_iasi_metop-a_ges_AllSurf_2010051018_2010051300_2D' ; omit extension

channel = 404

variable=1  ; 0=Bias Corrected, 1=NOT bias corrected, 2=bias correction

statistic=1 ; 0=Count, 1=Mean, 2=StdDev

latmin=-90.
latmax=90.
lonmin=-180.
lonmax=180.

Instrument = 'MetOp-A IASI'            ; for title
GesAnl = 'First Guess Departure'       ; guess or analysis fields (for title)
dates = '2010051018 - 2010051300'      ; for title

if (statistic eq 0) then GesAnl=''

; Change things above this line
;===============================================================

infile=file+'.nc'
psfile=file+'.ps'

title=strtrim(GesAnl)+', !C'+strtrim(instrument)+' Channel '+$
  strtrim(string(channel,format='(i4)'))+', '+dates

if (!d.name eq 'X') then begin
  device,decomposed=0
  window,0,retain=2
  loadct,5
endif

if (!d.name eq 'PS') then begin
  device,/land,/color,bits_per_pixel=8,filename=psfile
  loadct,5
endif

a=read_netcdf(infile,data,/global_attributes)

ichan=where(data.channel eq channel,ct)
if (ct eq 0) then begin
   print,'Channel ',channel,' not found'
   stop
endif

count=reform(data.nsamples(ichan,*,*))
zeroes=where(count eq 0,ct_zero)

case statistic of 
0: var_tmp=count
1: begin
     var_tmp=reform(data.mean(*,ichan,*,*))
     title='Mean '+title
     captitle='Departure (K)'
   end
2: begin
     var_tmp=reform(data.stddev(*,ichan,*,*))
     title='Std Dev of '+title
     captitle='Departure (K)'
   end
else: begin
    print,'Please use a valid value for statistic (0-2)'
    stop
   end
endcase

if (statistic eq 0) then begin
  var=var_tmp
  title='Number of Observations'+title
  captitle='Number of Observations'
endif else if (variable eq 2) then begin
  var=reform(var_tmp(0,*,*)-var_tmp(1,*,*))
  title='Bias Correction '+title
endif else if (variable eq 1) then begin
  var=reform(var_tmp(variable,*,*))
  title='Un-bias-corrected '+title
endif else if (variable eq 0) then begin
  var=reform(var_tmp(variable,*,*))
  title='Bias-corrected '+title
endif else begin
  print,'Please use a valid value for variable (0-2)'
  stop
endelse

if (ct_zero gt 0) then var(zeroes)=-999
min_val=min(var(where(var gt -900)))

neg_longs=where(data.longitude gt 180.)
data.longitude(neg_longs)=data.longitude(neg_longs)-360.
isort=sort(data.longitude)
data.longitude=data.longitude(isort)
for i=0,data.n_latitudes-1 do var(*,i)=var(isort,i)



map_set,limit=[latmin,lonmin,latmax,lonmax],/noerase,$ 
          position=[0.1, 0.25, 0.9, 0.9]
image=var
missing=where(image lt min_val,ct)
image=bytscl(image,min=min_val)
if (!d.name eq 'PS' and ct gt 0) then image(missing)=!p.background
result = MAP_IMAGE(image,Startx,Starty,xs,ys, COMPRESS=1, $  
            LATMIN=latmin, LONMIN=lonmin, $  
            LATMAX=latmax, LONMAX=lonmax, missing=!p.background)  
TV, result, Startx, Starty, xsize=xs, ysize=ys
map_set,limit=[latmin,lonmin,latmax,lonmax],/noerase,$ 
          position=[0.1, 0.25, 0.9, 0.9],title=title,/grid
map_continents,/hires
colorbar,minrange=min_val,maxrange=max(var),$
position=[0.2,0.15,0.8,0.2],$
format='(f6.2)',title=captitle

if (!d.name eq 'PS') then device,/close

end
