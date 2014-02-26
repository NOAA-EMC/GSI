@../../../setup/paths_idl.pro

PRO rdr2tdr_aqua_amsre, namelist=namelist_amsre
Consts, Consts

;***************************************************************
;    some constants definitions
;***************************************************************
satId='aqua'
rdrList_as='/net/orbit138l/disk2/pub/wchen/mirs_working/data/InputsData/aqua_amsre_rdrFiles_2008-09-28.list_as'
rdrList_ds='/net/orbit138l/disk2/pub/wchen/mirs_working/data/InputsData/aqua_amsre_rdrFiles_2008-09-28.list_ds'
tdrDir='/net/orbit138l/disk2/pub/wchen/mirs_working/data/TestbedData/DynamicData/tdr/aqua_amsre/2008-09-28/'

;readFromList=0
readFromList=1

if ( readFromList eq 1 ) then begin
  openr,iu,namelist_amsre,/get_lun 
  readf,iu,format='(a)', satId                  ;Satellite ID
  readf,iu,format='(a)', rdrList_as             ;RDR file list as
  readf,iu,format='(a)', rdrList_ds             ;RDR file list ds
  readf,iu,format='(a)', tdrDir                 ;TDR file location
  close,iu
  free_lun,iu,/force
endif


;******************************************************************
;to process ascending data
;******************************************************************
hdf_file='AMSR_E_L2A_BrightnessTemperatures_V08_200809280000_A.hdf'
tdr_file='TDR_AMSR_E_L2A_BrightnessTemperatures_V08_200809280000_A'
;TDR_AMAX.NN.D06032.S0122.E0317.B0361819.WI

openr, iu_as, rdrList_as, /get_lun  
while ( not eof(iu_as) ) do begin
  readf, iu_as, hdf_file, format='(a)'
  print,hdf_file
  fileid=EOS_SW_OPEN(hdf_file,/read)
  if ( fileid eq -1 ) then continue
  
  ;*******************************************************************
  ; read low scan data
  ;*******************************************************************
  gridid=EOS_SW_ATTACH(fileid,'Low_Res_Swath')
  
  ;Time is seconds since 0000 UTC on 1/1/1993
  success=EOS_SW_READFIELD(gridID,'Time', TIME_LRS)
  
  success=EOS_SW_READFIELD(gridID,'Latitude', LAT_LRS)
  success=EOS_SW_READFIELD(gridID,'Longitude',LON_LRS)

  success=EOS_SW_READFIELD(gridID,'Earth_Incidence', SATZ_LRS)
  success=EOS_SW_READFIELD(gridID,'Earth_Azimuth',SATAZ_LRS)
  success=EOS_SW_READFIELD(gridID,'Sun_Elevation',SUNELEV_LRS)
  
  success=EOS_SW_READFIELD(gridID,'6.9V_Res.1_TB',TB6V_LRS)
  success=EOS_SW_READFIELD(gridID,'6.9H_Res.1_TB',TB6H_LRS)
  
  success=EOS_SW_READFIELD(gridID,'10.7V_Res.2_TB',TB10V_LRS)
  success=EOS_SW_READFIELD(gridID,'10.7H_Res.2_TB',TB10H_LRS)
  
  success=EOS_SW_READFIELD(gridID,'18.7V_Res.2_TB',TB19V_LRS)
  success=EOS_SW_READFIELD(gridID,'18.7H_Res.2_TB',TB19H_LRS)
  
  success=EOS_SW_READFIELD(gridID,'23.8V_Res.3_TB',TB23V_LRS)
  success=EOS_SW_READFIELD(gridID,'23.8H_Res.3_TB',TB23H_LRS)
  
  success=EOS_SW_READFIELD(gridID,'36.5V_Res.3_TB',TB37V_LRS)
  success=EOS_SW_READFIELD(gridID,'36.5H_Res.3_TB',TB37H_LRS)

  success=EOS_SW_READFIELD(gridID,'89.0V_Res.4_TB',TB89V_LRS)
  success=EOS_SW_READFIELD(gridID,'89.0H_Res.4_TB',TB89H_LRS)
 
  success=EOS_SW_INQDIMS(gridid,params,dim)
  
  scan_lrs=dim(0) & pix_lrs=dim(1)
  print, scan_lrs, pix_lrs 
  
  temp=EOS_SW_DETACH(gridid)
  
  ;*******************************************************************
  ; close file id
  ;*******************************************************************
  temp=EOS_SW_CLOSE(fileID)
  
  ;*******************************************************************
  ; scale the value
  ;*******************************************************************

  lat   = fltarr(pix_lrs,scan_lrs)
  lon   = fltarr(pix_lrs,scan_lrs)
  satz  = fltarr(pix_lrs,scan_lrs)
  sataz = fltarr(pix_lrs,scan_lrs)
  sunz  = fltarr(pix_lrs,scan_lrs)

  lat(*,*) = LAT_LRS(*,*)
  lon(*,*) = LON_LRS(*,*)
  satz(*,*) = SATZ_LRS(*,*)*0.0050
  sataz(*,*) = SATAZ_LRS(*,*)*0.01
;  sunz(*,*) = 90.0 - SUNELEV_LRS(*,*)*0.1
  sunz(*,*) = SUNELEV_LRS(*,*)*0.1
  sunz(*,*) = sunz(*,*)+satz(*,*)

  tb6v  = fltarr(pix_lrs,scan_lrs)
  tb6h  = fltarr(pix_lrs,scan_lrs)
  tb10v = fltarr(pix_lrs,scan_lrs)
  tb10h = fltarr(pix_lrs,scan_lrs)
  tb19v = fltarr(pix_lrs,scan_lrs)
  tb19h = fltarr(pix_lrs,scan_lrs)
  tb23v = fltarr(pix_lrs,scan_lrs)
  tb23h = fltarr(pix_lrs,scan_lrs)
  tb37v = fltarr(pix_lrs,scan_lrs)
  tb37h = fltarr(pix_lrs,scan_lrs)
  tb89v = fltarr(pix_lrs,scan_lrs)
  tb89h = fltarr(pix_lrs,scan_lrs)

  tb6v(*,*)=(TB6V_LRS(*,*)+32768)/100.
  tb6h(*,*)=(TB6H_LRS(*,*)+32768)/100.
  
  tb10v(*,*)=(TB10V_LRS(*,*)+32768)/100.
  tb10h(*,*)=(TB10H_LRS(*,*)+32768)/100.
  
  tb19v(*,*)=(TB19V_LRS(*,*)+32768)/100.
  tb19h(*,*)=(TB19H_LRS(*,*)+32768)/100.
  
  tb23v(*,*)=(TB23V_LRS(*,*)+32768)/100.
  tb23h(*,*)=(TB23H_LRS(*,*)+32768)/100.
  
  tb37v(*,*)=(TB37V_LRS(*,*)+32768)/100.
  tb37h(*,*)=(TB37H_LRS(*,*)+32768)/100.
  
  tb89v(*,*)=(TB89V_LRS(*,*)+32768)/100.
  tb89h(*,*)=(TB89H_LRS(*,*)+32768)/100.
    
;**********************************************************
; RFI mitigation
;**********************************************************

;threshold for FRI identification
 rfimin= -5
 rfimax=  5

 diff10_6h = 6
 diff10_6v = 4
 diff6_18h = 10
 diff6_18v = 5

; coefficients:
;use TB10.65 to predict TB6.925; stdev_h = 1.66425 ; stdev_v = 1.53857

a0= -9.68610 & a1= 1.10629   & a2= -0.0718768  & a3= 0  &  a4= 0
b0= -8.99197 & b1= 0.951212  & b2= 0.0752778   & b3= 0  &  b4= 0


;use TB18.7 to predict TB10.65; stdev_h = 1.29012 ; stdev_v = 1.48666

c0= 10.6396  & c1= 1.04417   & c2= -0.0904358  & c3= 0  &  c4= 0
d0= 9.31105  & d1= 0.913560  & d2= 0.0403827   & d3= 0  &  d4= 0


;use TB18.7 to predict TB6.925; stdev_h = 2.21522 ; stdev_v = 1.97212

e0= 15.3020  & e1= 1.11368   & e2= -0.175613  & e3= 0  &  e4= 0
f0= 17.4359  & f1= 0.903253  & f2= 0.0175537  & f3= 0  &  f4= 0


; detect and mitigate RFI-related contaminations
; V-pol

for scan = 0, scan_lrs-1 do begin
for pix  = 0, pix_lrs-1  do begin

       if  tb10v(pix,scan)-tb19v(pix,scan) ge rfimax  then begin
             if tb10v(pix,scan)-tb6v(pix,scan) ge  diff10_6v then begin
                    ; TB10 is contaminated by RFI
                 if tb6v(pix,scan)-tb19v(pix,scan) ge diff6_18v then begin
                    ; TB6 is also  contaminated by RFI, use TB18 to predict TB6 and TB10
                    tb6v(pix,scan)=e0+e1*tb19v(pix,scan)+e2*tb19h(pix,scan)+e3*tb19v(pix,scan)^2+e4*tb19h(pix,scan)^2
                    tb10v(pix,scan)=c0+c1*tb19v(pix,scan)+c2*tb19h(pix,scan)+c3*tb19v(pix,scan)^2+c4*tb19h(pix,scan)^2
                 endif else begin
                    ; TB10 is contaminated by RFI, but not for TB6, use TB18 to predict TB10
                    tb10v(pix,scan)=c0+c1*tb19v(pix,scan)+c2*tb19h(pix,scan)+c3*tb19v(pix,scan)^2+c4*tb19h(pix,scan)^2
                 endelse
             endif 
       endif else begin
            if tb6v(pix,scan)-tb10v(pix,scan) ge rfimax  then begin
                    ; TB6 is contaminated by RFI, but not for TB10, use TB10 to predict TB6
                 tb6v(pix,scan)=a0+a1*tb10v(pix,scan)+a2*tb10h(pix,scan)+a3*tb10v(pix,scan)^2+a4*tb10h(pix,scan)^2
            endif
       endelse

; detect and mitigate RFI-related contaminations
; H-pol

       if  tb10h(pix,scan)-tb19h(pix,scan) ge rfimax  then begin
             if tb10h(pix,scan)-tb6h(pix,scan) ge  diff10_6h then begin
                    ; TB10 is contaminated by RFI
                 if tb6h(pix,scan)-tb19h(pix,scan) ge diff6_18h then begin
                    ; TB6 is also  contaminated by RFI, use TB18 to predict TB6 and TB10
                    tb6h(pix,scan)=f0+f1*tb19h(pix,scan)+f2*tb19v(pix,scan)+f3*tb19h(pix,scan)^2+f4*tb19v(pix,scan)^2
                    tb10h(pix,scan)=d0+d1*tb19h(pix,scan)+d2*tb19v(pix,scan)+d3*tb19h(pix,scan)^2+d4*tb19v(pix,scan)^2
                 endif else begin
                    ; TB10 is contaminated by RFI, but not for TB6, use TB18 to predict TB10
                    tb10h(pix,scan)=d0+d1*tb19h(pix,scan)+d2*tb19v(pix,scan)+d3*tb19h(pix,scan)^2+d4*tb19v(pix,scan)^2
                 endelse
             endif 
       endif else begin
            if tb6h(pix,scan)-tb10h(pix,scan) ge rfimax  then begin
                    ; TB6 is contaminated by RFI, but not for TB10, use TB10 to predict TB6
               tb6h(pix,scan)=b0+b1*tb10h(pix,scan)+b2*tb10v(pix,scan)+b3*tb10h(pix,scan)^2+b4*tb10v(pix,scan)^2
            endif
       endelse

endfor
endfor

;**********************************************************
; date information from hdf file
;**********************************************************
  pos = strpos(hdf_file, '.hdf')
  
  yyyy_file=strmid(hdf_file,pos-14,4)
  mm_file=strmid(hdf_file,pos-10,2)
  dd_file=strmid(hdf_file,pos-8,2)
  date = yyyy_file +  mm_file +  dd_file

  year_file  = long(yyyy_file)
  month_file = long(mm_file)   
  day_file   = long(dd_file)
  jday_file  = getJday(year_file, month_file, day_file)
  
  ;get number of leap years [1993,year_file)
  nyears_leap=0;
  for year=1993, year_file-1 do begin
    if ( ( year mod 4 eq 0 and year mod 100 ne 0 ) or year mod 400 eq 0 ) then nyears_leap = nyears_leap+1
  endfor
  
  ; time in seconds from 0000 1/1/1993 UTC time to start of current day
  time_offset = 0L
  time_offset = ( (year_file-1993)*365 + nyears_leap ) * 24 * 3600  + (jday_file-1) * 24 * 3600
  ;print, 'time_offset=', time_offset
  
;**********************************************************
; output
;**********************************************************

;All channel data are MISSING for the first scan and the last scan.
;89GHz V/H data are MISSING for first 7 scans and last 7 scans.
;Omit first 7 scans and last 7 scans from output
nscan = scan_lrs - (7+7)
nscan = long(nscan)

;Nominal nFovs=243
;All channel data are MISSING for iFov 1-23 and 220-243
;89GHz V/H data are MISSING for iFov 1-25 and 217-243
;Omit iFov 1-25 and 217-243 from output
npix = pix_lrs - (25+27)
npix = long(npix)

node = 0 ;0:asc 1:des
node = long(node)

;Strictly speaking, scanDAY, scanYear and scanUTC should be computed from TIME_LRS
;TIME_LRS is one dimention array here, number of scan lines 
;TIME_LRS is seconds since start of 0000 1993 1/1 UTC time

scanYear = lonarr(scan_lrs)
scanDay  = lonarr(scan_lrs)
scanUTC  = lonarr(scan_lrs)

for iscan=0, scan_lrs - 1 do begin
  scanYear(iscan) = year_file
;  scanDay(iscan)  = day_file
  scanDay(iscan)  = jday_file
;  scanUTC(iscan)  = TIME_LRS(iscan) - time_offset
  scanUTC(iscan)  = 1000*(TIME_LRS(iscan) - time_offset) ;msec
endfor


;dummy
nqc = 1
nqc = long(nqc)
qc = intarr(nqc)
qc(0) = 0
qc = long(qc)

nchan = 12
nchan = long(nchan)

cfreq = fltarr(nchan)
pol = intarr(nchan)
cfreq(0)  =  6.925 & pol(0)  = 4
cfreq(1)  =  6.925 & pol(1)  = 5
cfreq(2)  = 10.650 & pol(2)  = 4
cfreq(3)  = 10.650 & pol(3)  = 5
cfreq(4)  = 18.700 & pol(4)  = 4
cfreq(5)  = 18.700 & pol(5)  = 5
cfreq(6)  = 23.800 & pol(6)  = 4
cfreq(7)  = 23.800 & pol(7)  = 5
cfreq(8)  = 36.500 & pol(8)  = 4
cfreq(9)  = 36.500 & pol(9)  = 5
cfreq(10) = 89.000 & pol(10) = 4
cfreq(11) = 89.000 & pol(11) = 5
pol = long(pol)

;Open a file for writing. Note that the F77_UNFORMATTED keyword is   
;necessary to tell IDL to write the data in a format readable by a   
;FORTRAN program.  

pos = strpos(hdf_file,'AMSR_E_')
tdr_file = 'TDR_'+ strmid(hdf_file, pos, 52)
OPENW, iu_out, tdrDir+tdr_file, /F77_UNFORMATTED, /SWAP_ENDIAN, /GET_LUN  

;HEADER
writeu,iu_out,nscan,npix,nqc,nchan
writeu,iu_out,cfreq
writeu,iu_out,pol
;DATA BY SCANLINE
;Omit first 7 scans and last 7 scans
for iscan=7,scan_lrs-8 do begin
  writeu,iu_out,node
  writeu,iu_out,scanDay(iscan),scanYear(iscan)
  writeu,iu_out,scanUTC(iscan)
;Omit iFov 1-25 and 217-243 from output
  writeu,iu_out,lat(25:215,iscan)
  writeu,iu_out,lon(25:215,iscan)
  writeu,iu_out,satz(25:215,iscan)
  writeu,iu_out,sataz(25:215,iscan)
  writeu,iu_out,sunz(25:215,iscan)
  writeu,iu_out,tb6v(25:215,iscan),tb6h(25:215,iscan),tb10v(25:215,iscan),tb10h(25:215,iscan),$
  	tb19v(25:215,iscan),tb19h(25:215,iscan),tb23v(25:215,iscan),tb23h(25:215,iscan),$
	tb37v(25:215,iscan),tb37h(25:215,iscan),tb89v(25:215,iscan),tb89h(25:215,iscan)
  writeu,iu_out,qc
endfor
close,iu_out
free_lun,iu_out,/force

endwhile

close,iu_as
free_lun,iu_as,/force



;******************************************************************
;to process descending data
;******************************************************************
hdf_file='AMSR_E_L2A_BrightnessTemperatures_V08_200809280000_A.hdf'
tdr_file='TDR_AMSR_E_L2A_BrightnessTemperatures_V08_200809280000_A'
;TDR_AMAX.NN.D06032.S0122.E0317.B0361819.WI

openr, iu_ds, rdrList_ds, /get_lun  
while ( not eof(iu_ds) ) do begin
  readf, iu_ds, hdf_file, format='(a)'
  print,hdf_file
  fileid=EOS_SW_OPEN(hdf_file,/read)
  if ( fileid eq -1 ) then continue
  
  ;*******************************************************************
  ; read low scan data
  ;*******************************************************************
  gridid=EOS_SW_ATTACH(fileid,'Low_Res_Swath')
  
  ;Time is seconds since 0000 UTC on 1/1/1993
  success=EOS_SW_READFIELD(gridID,'Time', TIME_LRS)
  
  success=EOS_SW_READFIELD(gridID,'Latitude', LAT_LRS)
  success=EOS_SW_READFIELD(gridID,'Longitude',LON_LRS)

  success=EOS_SW_READFIELD(gridID,'Earth_Incidence', SATZ_LRS)
  success=EOS_SW_READFIELD(gridID,'Earth_Azimuth',SATAZ_LRS)
  success=EOS_SW_READFIELD(gridID,'Sun_Elevation',SUNELEV_LRS)
  
  success=EOS_SW_READFIELD(gridID,'6.9V_Res.1_TB',TB6V_LRS)
  success=EOS_SW_READFIELD(gridID,'6.9H_Res.1_TB',TB6H_LRS)
  
  success=EOS_SW_READFIELD(gridID,'10.7V_Res.2_TB',TB10V_LRS)
  success=EOS_SW_READFIELD(gridID,'10.7H_Res.2_TB',TB10H_LRS)
  
  success=EOS_SW_READFIELD(gridID,'18.7V_Res.2_TB',TB19V_LRS)
  success=EOS_SW_READFIELD(gridID,'18.7H_Res.2_TB',TB19H_LRS)
  
  success=EOS_SW_READFIELD(gridID,'23.8V_Res.3_TB',TB23V_LRS)
  success=EOS_SW_READFIELD(gridID,'23.8H_Res.3_TB',TB23H_LRS)
  
  success=EOS_SW_READFIELD(gridID,'36.5V_Res.3_TB',TB37V_LRS)
  success=EOS_SW_READFIELD(gridID,'36.5H_Res.3_TB',TB37H_LRS)

  success=EOS_SW_READFIELD(gridID,'89.0V_Res.4_TB',TB89V_LRS)
  success=EOS_SW_READFIELD(gridID,'89.0H_Res.4_TB',TB89H_LRS)
 
  success=EOS_SW_INQDIMS(gridid,params,dim)
  
  scan_lrs=dim(0) & pix_lrs=dim(1)
  print, scan_lrs, pix_lrs 
  
  temp=EOS_SW_DETACH(gridid)
  
  ;*******************************************************************
  ; close file id
  ;*******************************************************************
  temp=EOS_SW_CLOSE(fileID)
  
  ;*******************************************************************
  ; scale the value
  ;*******************************************************************

  lat   = fltarr(pix_lrs,scan_lrs)
  lon   = fltarr(pix_lrs,scan_lrs)
  satz  = fltarr(pix_lrs,scan_lrs)
  sataz = fltarr(pix_lrs,scan_lrs)
  sunz  = fltarr(pix_lrs,scan_lrs)

  lat(*,*) = LAT_LRS(*,*)
  lon(*,*) = LON_LRS(*,*)
  satz(*,*) = SATZ_LRS(*,*)*0.0050
  sataz(*,*) = SATAZ_LRS(*,*)*0.01
  ;  sunz(*,*) = 90.0 - SUNELEV_LRS(*,*)*0.1
  sunz(*,*) = SUNELEV_LRS(*,*)*0.1
  sunz(*,*) = sunz(*,*)+satz(*,*)

  tb6v  = fltarr(pix_lrs,scan_lrs)
  tb6h  = fltarr(pix_lrs,scan_lrs)
  tb10v = fltarr(pix_lrs,scan_lrs)
  tb10h = fltarr(pix_lrs,scan_lrs)
  tb19v = fltarr(pix_lrs,scan_lrs)
  tb19h = fltarr(pix_lrs,scan_lrs)
  tb23v = fltarr(pix_lrs,scan_lrs)
  tb23h = fltarr(pix_lrs,scan_lrs)
  tb37v = fltarr(pix_lrs,scan_lrs)
  tb37h = fltarr(pix_lrs,scan_lrs)
  tb89v = fltarr(pix_lrs,scan_lrs)
  tb89h = fltarr(pix_lrs,scan_lrs)

  tb6v(*,*)=(TB6V_LRS(*,*)+32768)/100.
  tb6h(*,*)=(TB6H_LRS(*,*)+32768)/100.
  
  tb10v(*,*)=(TB10V_LRS(*,*)+32768)/100.
  tb10h(*,*)=(TB10H_LRS(*,*)+32768)/100.
  
  tb19v(*,*)=(TB19V_LRS(*,*)+32768)/100.
  tb19h(*,*)=(TB19H_LRS(*,*)+32768)/100.
  
  tb23v(*,*)=(TB23V_LRS(*,*)+32768)/100.
  tb23h(*,*)=(TB23H_LRS(*,*)+32768)/100.
  
  tb37v(*,*)=(TB37V_LRS(*,*)+32768)/100.
  tb37h(*,*)=(TB37H_LRS(*,*)+32768)/100.
  
  tb89v(*,*)=(TB89V_LRS(*,*)+32768)/100.
  tb89h(*,*)=(TB89H_LRS(*,*)+32768)/100.
    
;**********************************************************
; RFI mitigation
;**********************************************************

;threshold for FRI identification
 rfimin= -5
 rfimax=  5

 diff10_6h = 6
 diff10_6v = 4
 diff6_18h = 10
 diff6_18v = 5

; coefficients:
;use TB10.65 to predict TB6.925; stdev_h = 1.66425 ; stdev_v = 1.53857

a0= -9.68610 & a1= 1.10629   & a2= -0.0718768  & a3= 0  &  a4= 0
b0= -8.99197 & b1= 0.951212  & b2= 0.0752778   & b3= 0  &  b4= 0


;use TB18.7 to predict TB10.65; stdev_h = 1.29012 ; stdev_v = 1.48666

c0= 10.6396  & c1= 1.04417   & c2= -0.0904358  & c3= 0  &  c4= 0
d0= 9.31105  & d1= 0.913560  & d2= 0.0403827   & d3= 0  &  d4= 0


;use TB18.7 to predict TB6.925; stdev_h = 2.21522 ; stdev_v = 1.97212

e0= 15.3020  & e1= 1.11368   & e2= -0.175613  & e3= 0  &  e4= 0
f0= 17.4359  & f1= 0.903253  & f2= 0.0175537  & f3= 0  &  f4= 0


; detect and mitigate RFI-related contaminations
; V-pol

for scan = 0, scan_lrs-1 do begin
for pix  = 0, pix_lrs-1  do begin

       if  tb10v(pix,scan)-tb19v(pix,scan) ge rfimax  then begin
             if tb10v(pix,scan)-tb6v(pix,scan) ge  diff10_6v then begin
                    ; TB10 is contaminated by RFI
                 if tb6v(pix,scan)-tb19v(pix,scan) ge diff6_18v then begin
                    ; TB6 is also  contaminated by RFI, use TB18 to predict TB6 and TB10
                    tb6v(pix,scan)=e0+e1*tb19v(pix,scan)+e2*tb19h(pix,scan)+e3*tb19v(pix,scan)^2+e4*tb19h(pix,scan)^2
                    tb10v(pix,scan)=c0+c1*tb19v(pix,scan)+c2*tb19h(pix,scan)+c3*tb19v(pix,scan)^2+c4*tb19h(pix,scan)^2
                 endif else begin
                    ; TB10 is contaminated by RFI, but not for TB6, use TB18 to predict TB10
                    tb10v(pix,scan)=c0+c1*tb19v(pix,scan)+c2*tb19h(pix,scan)+c3*tb19v(pix,scan)^2+c4*tb19h(pix,scan)^2
                 endelse
             endif 
       endif else begin
            if tb6v(pix,scan)-tb10v(pix,scan) ge rfimax  then begin
                    ; TB6 is contaminated by RFI, but not for TB10, use TB10 to predict TB6
                 tb6v(pix,scan)=a0+a1*tb10v(pix,scan)+a2*tb10h(pix,scan)+a3*tb10v(pix,scan)^2+a4*tb10h(pix,scan)^2
            endif
       endelse

; detect and mitigate RFI-related contaminations
; H-pol

       if  tb10h(pix,scan)-tb19h(pix,scan) ge rfimax  then begin
             if tb10h(pix,scan)-tb6h(pix,scan) ge  diff10_6h then begin
                    ; TB10 is contaminated by RFI
                 if tb6h(pix,scan)-tb19h(pix,scan) ge diff6_18h then begin
                    ; TB6 is also  contaminated by RFI, use TB18 to predict TB6 and TB10
                    tb6h(pix,scan)=f0+f1*tb19h(pix,scan)+f2*tb19v(pix,scan)+f3*tb19h(pix,scan)^2+f4*tb19v(pix,scan)^2
                    tb10h(pix,scan)=d0+d1*tb19h(pix,scan)+d2*tb19v(pix,scan)+d3*tb19h(pix,scan)^2+d4*tb19v(pix,scan)^2
                 endif else begin
                    ; TB10 is contaminated by RFI, but not for TB6, use TB18 to predict TB10
                    tb10h(pix,scan)=d0+d1*tb19h(pix,scan)+d2*tb19v(pix,scan)+d3*tb19h(pix,scan)^2+d4*tb19v(pix,scan)^2
                 endelse
             endif 
       endif else begin
            if tb6h(pix,scan)-tb10h(pix,scan) ge rfimax  then begin
                    ; TB6 is contaminated by RFI, but not for TB10, use TB10 to predict TB6
               tb6h(pix,scan)=b0+b1*tb10h(pix,scan)+b2*tb10v(pix,scan)+b3*tb10h(pix,scan)^2+b4*tb10v(pix,scan)^2
            endif
       endelse

endfor
endfor

;**********************************************************
; date information from hdf file
;**********************************************************
  pos = strpos(hdf_file, '.hdf')
  
  yyyy_file=strmid(hdf_file,pos-14,4)
  mm_file=strmid(hdf_file,pos-10,2)
  dd_file=strmid(hdf_file,pos-8,2)
  date = yyyy_file +  mm_file +  dd_file

  year_file  = long(yyyy_file)
  month_file = long(mm_file)   
  day_file   = long(dd_file)
  
  jday_file = getJday(year_file, month_file, day_file)
  
  ;get number of leap years [1993,year_file)
  nyears_leap=0;
  for year=1993, year_file-1 do begin
    if ( ( year mod 4 eq 0 and year mod 100 ne 0 ) or year mod 400 eq 0 ) then nyears_leap = nyears_leap+1
  endfor
  
  ; time in seconds from 0000 1/1/1993 UTC time to start of current day
  time_offset = 0L
  time_offset = ( (year_file-1993)*365 + nyears_leap ) * 24 * 3600  + (jday_file-1) * 24 * 3600
  ;print, 'time_offset=', time_offset
  
;**********************************************************
; output
;**********************************************************

;All channel data are MISSING for the first scan and the last scan.
;89GHz V/H data are MISSING for first 7 scans and last 7 scans.
;Omit first 7 scans and last 7 scans from output
nscan = scan_lrs - (7+7)
nscan = long(nscan)

;Nominal nFovs=243
;All channel data are MISSING for iFov 1-23 and 220-243
;89GHz V/H data are MISSING for iFov 1-25 and 217-243
;Omit iFov 1-25 and 217-243 from output
npix = pix_lrs - (25+27)
npix = long(npix)

node = 1 ;0:asc 1:des
node = long(node)

;Strictly speaking, scanDAY, scanYear and scanUTC should be computed from TIME_LRS
;TIME_LRS is one dimention array here, number of scan lines 
;TIME_LRS is seconds since start of 0000 1993 1/1 UTC time

scanYear = lonarr(scan_lrs)
scanDay  = lonarr(scan_lrs)
scanUTC  = lonarr(scan_lrs)

for iscan=0, scan_lrs - 1 do begin
  scanYear(iscan) = year_file
;  scanDay(iscan)  = day_file
  scanDay(iscan)  = jday_file
;  scanUTC(iscan)  = TIME_LRS(iscan) - time_offset
  scanUTC(iscan)  = 1000*(TIME_LRS(iscan) - time_offset) ;msec
endfor


;dummy
nqc = 1
nqc = long(nqc)
qc = intarr(nqc)
qc(0) = 0
qc = long(qc)

nchan = 12
nchan = long(nchan)

cfreq = fltarr(nchan)
pol = intarr(nchan)
cfreq(0)  =  6.925 & pol(0)  = 4
cfreq(1)  =  6.925 & pol(1)  = 5
cfreq(2)  = 10.650 & pol(2)  = 4
cfreq(3)  = 10.650 & pol(3)  = 5
cfreq(4)  = 18.700 & pol(4)  = 4
cfreq(5)  = 18.700 & pol(5)  = 5
cfreq(6)  = 23.800 & pol(6)  = 4
cfreq(7)  = 23.800 & pol(7)  = 5
cfreq(8)  = 36.500 & pol(8)  = 4
cfreq(9)  = 36.500 & pol(9)  = 5
cfreq(10) = 89.000 & pol(10) = 4
cfreq(11) = 89.000 & pol(11) = 5
pol = long(pol)

;Open a file for writing. Note that the F77_UNFORMATTED keyword is   
;necessary to tell IDL to write the data in a format readable by a   
;FORTRAN program.  

pos = strpos(hdf_file,'AMSR_E_')
tdr_file = 'TDR_'+ strmid(hdf_file, pos, 52)
OPENW, iu_out, tdrDir+tdr_file, /F77_UNFORMATTED, /SWAP_ENDIAN, /GET_LUN  

;HEADER
writeu,iu_out,nscan,npix,nqc,nchan
writeu,iu_out,cfreq
writeu,iu_out,pol
;DATA BY SCANLINE
;Omit first 7 scans and last 7 scans
for iscan=7,scan_lrs-8 do begin
  writeu,iu_out,node
  writeu,iu_out,scanDay(iscan),scanYear(iscan)
  writeu,iu_out,scanUTC(iscan)
;Omit iFov 1-25 and 217-243 from output
  writeu,iu_out,lat(25:215,iscan)
  writeu,iu_out,lon(25:215,iscan)
  writeu,iu_out,satz(25:215,iscan)
  writeu,iu_out,sataz(25:215,iscan)
  writeu,iu_out,sunz(25:215,iscan)
  writeu,iu_out,tb6v(25:215,iscan),tb6h(25:215,iscan),tb10v(25:215,iscan),tb10h(25:215,iscan),$
  	tb19v(25:215,iscan),tb19h(25:215,iscan),tb23v(25:215,iscan),tb23h(25:215,iscan),$
	tb37v(25:215,iscan),tb37h(25:215,iscan),tb89v(25:215,iscan),tb89h(25:215,iscan)
  writeu,iu_out,qc
endfor
close,iu_out
free_lun,iu_out,/force

endwhile

close,iu_ds
free_lun,iu_ds,/force




END
