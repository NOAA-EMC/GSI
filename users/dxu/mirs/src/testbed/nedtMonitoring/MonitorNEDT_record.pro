;$Id: MonitorNEDT.pro 1637 2008-11-07 20:38:03Z wchen $
@../../../setup/paths_idl.pro
;===============================================================
; Name:		MonitorNEDT_record
;
;
; Type:		IDL Subroutine
;
;
; Description:  Used to monitor the NEDT used by MIRS, reading and storing
;               all NEDT values from day to day, or granule to granule,from
;               one master file (record).
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- namelistNedt	      I		    namelist of input
;
;
; Subroutines needed:
;       - readErr
;
;
; History:
;       08-29-2012      Kevin Garrett, initial version
;
;===============================================================
PRO MonitorNEDT_record,namelist=namelist

satId='npp'
nedtFile='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386/NEDT_SATMS_npp_d20120630_t1359070_e1359386_b03493_c20120630201602584086_noaa_ops_befFM.dat'
nedtRecord_In='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386/nedtRecord_npp_d20120630_t1358350_d20120630_e1359066_befFM.dat'
nedtPath='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386'
ctime='20121111111111'
imgDir='/home/pub/kgarrett/qc_d20120630_t1359070_e1359386/'


readFromList=1
if ( readFromList eq 1 ) then begin
  openr,iu,namelist,/get_lun
  readf,iu,format='(a)', satId                  ; Satellite ID
  readf,iu,format='(a)', nedtFile               ; Nedt file
  readf,iu,format='(a)', nedtRecord_In          ; Record of Nedt in
  readf,iu,format='(a)', nedtPath               ; Path to read/write Nedt files
  readf,iu,format='(a)', ctime                  ; creation time of the file
  readf,iu,format='(a)', imgDir                 ; generated image location
  close,iu
  free_lun,iu,/force
endif

print, 'MonitorNEDT_record.pro...'
print, 'satId='+satId
print, 'nedtFile='+nedtFile
print, 'nedtRecord_In='+nedtRecord_In
print, 'nedtPath='+nedtPath
print, 'ctime='+ctime
print, 'imgDir='+imgDir

prefix='nedtRecord_'+satId
titleSat = STRUPCASE(satId)

NCHAN=20
titles=['23v','31v','50v','52v','53h','54h','54v','55h','57h1','57h2',$
        '57h3','57h4','57h5','57h6','89v1','89v2','157h','184h','186h','190h']
if satId eq 'npp' then begin
    NCHAN=22
    titles=['23v','31v','50h','51h','52h','53h','54h1','54h2','55h','57h1','57h2',$
            '57h3','57h4','57h5','57h6','88v','165h','183h1','183h2','183h3','183h4','183h5']
    titleSat = 'NPP/ATMS'
endif

;---- NEDT nominal values ( from: data/StaticData/NominalNedts/satId_NoiseFile.dat )
if satId eq 'n18' then $
NominalNedts = [0.235,0.182,0.277,0.168,0.190,0.144,0.168,0.195,0.165,0.204,$
                0.226,0.309,0.423,0.678,0.134,0.244,0.382,0.559,0.434,0.355]
if satId eq 'n19' then $
NominalNedts = [0.192,0.166,0.209,0.148,0.160,0.143,0.166,0.171,0.165,0.206,$
                0.221,0.310,0.424,0.678,0.135,0.252,0.400,0.582,0.455,0.354]
if satId eq 'metopA' then $
NominalNedts = [0.205,0.220,0.255,0.143,0.168,0.133,0.441,0.160,0.173,0.213,$
                0.246,0.329,0.451,0.740,0.111,0.228,0.369,0.507,0.406,0.359]
if satId eq 'npp' then $
NominalNedts = [0.210,0.200,0.210,0.200,0.220,0.260,0.250,0.530,0.560,0.390,0.380,$    
                0.350,0.340,0.450,0.260,0.220,0.190,0.190,1.230,1.180,0.860,0.580]
freq=fltarr(nCHAN)
dummy=''

;----read data record file if it exists, if not, start new record.
openr,rlun,nedtRecord_In,/get_lun,error=open_err
if (open_err ne 0) then begin
    print, 'No NEDT record file exists, creating new record...'
    nLines=0
    nNoise=nChan
    nedtTotal=fltarr(nLines+1,nChan)
    dates=''
    starttimes=''
    endtimes=''
    julianday=0d
endif else begin
    readf,rlun,dummy
    readf,rlun,nLines ;---number of noise records
    readf,rlun,nNoise ;---number of Channel Noise values
    readf,rlun,freq   ;---frequencies
    nedtTotal=fltarr(nLines+1,nChan)
    dates=strarr(nLines+1)
    starttimes=strarr(nLines+1)
    endtimes=strarr(nLines+1)
    julianday=dblarr(nLines+1)
    temp_arr=fltarr(nChan)
    temp_str=strarr(3)
    temp_dbl=0d
    ;---read noise, date and orbit time info
    for iLine=0,nLines-1 do begin
        readf,rlun,temp_arr,temp_str,temp_dbl,format='(22(f7.4,1x),a8,1x,2(a7,1x),f16.8)'
        nedtTotal[iLine,*]=temp_arr[0:nCHAN-1]
        dates[iLine]=temp_str[0]
        starttimes[iLine]=temp_str[1]
        endtimes[iLine]=temp_str[2]
        julianday[iLine]=temp_dbl
    endfor
free_lun,rlun
endelse

date          = 0L
flag          = 0
computedOrNot = 0
nDayEff       = 0

;---- read in new granule noise data
if (satID eq 'n18' or satId eq 'n19' or satId eq 'metopA') then begin
    day=float(strmid(strtrim(file_basename(nedtFile)), 11,2,/reverse_offset))
    mnth=float(strmid(strtrim(file_basename(nedtFile)),14,2,/reverse_offset))
    yr=float(strmid(strtrim(file_basename(nedtFile)),  19,4,/reverse_offset))
endif
if (satId eq 'npp') then begin
    yr=float(strmid(strtrim(file_basename(nedtFile)),  73,4,/reverse_offset))
    mnth=float(strmid(strtrim(file_basename(nedtFile)),69,2,/reverse_offset))
    day=float(strmid(strtrim(file_basename(nedtFile)), 67,2,/reverse_offset))
    HH=fix(strmid(strtrim(file_basename(nedtFile)),    63,2,/reverse_offset))
    MM=fix(strmid(strtrim(file_basename(nedtFile)),    61,2,/reverse_offset))
    ; each granule is ~32 seconds, we take middle time, so adding 16 seconds here
    SS=FIX(float(strmid(strtrim(file_basename(nedtFile)),  59,3,/reverse_offset)) * 0.1 ) + 16
    starttimes[nLines]=strmid(strtrim(file_basename(nedtFile)),  63,7,/reverse_offset)
    endtimes[nLines]=strmid(strtrim(file_basename(nedtFile)),  54,7,/reverse_offset)

endif

;----Transform date/time into Julian day
julianday[nLines]=JULDAY(mnth,day,yr,HH, MM, SS )
readErr,nedtFile,cfreq,nedt,nchan,ComputedOrNotFlag
;----add current granule NEDT to record array
nedtTotal(nLines,0:nchan-1) = nedt(0:nchan-1)

;----Manipulate strings containing time information for file naming
mnthString=string(fix(mnth))
if (mnth lt 10) then mnthString='0'+mnthString
dayString=string(fix(day))
if (day lt 10) then dayString='0'+dayString
dates[nLines]=strcompress(string(fix(yr))+mnthString+dayString,/remove_all)
;---write out new NEDT record file
startDate=dates[0]
startTime=starttimes[0]
endDate=dates[nLines]
endTime=endtimes[nLines]
nedtRecord_Out=nedtPath+'/'+prefix+'_d'+startDate+'_t'+startTime+'_d'+endDate+'_e'+endTime+'_c'+ctime+'_befFM.dat'

openw,wlun,nedtRecord_Out,/get_lun
printf,wlun,satId,format='(a3)'
printf,wlun,n_elements(dates),format='(i10)'
printf,wlun,nCHAN,format='(i2)'
printf,wlun,cfreq,format='(22(f8.3,1x))'
for iLine=0,nLines do begin
    printf,wlun,nedtTotal[iLine,0:nCHAN-1],dates[iLine],starttimes[iLine],endtimes[iLine],julianday[iLine], $
      format='(22(f7.4,1x),a8,1x,2(a7,1x),f16.8)'
endfor
free_lun,wlun

;---Check number of NEDT records to plot, if <= 1, no exit
if (nLines+1 le 1) then begin
    print, '--------------------------------------------'
    print, 'Not enough NEDT records to plot, exiting.'
    print, 'Number of records: ',n_elements(dates)
    print, '--------------------------------------------'
    exit
endif

;---- PLOTTING
;---- symbol definition
!p.charsize   = 0.75
csize	      = 16
a	      = findgen(csize+1) *(!pi*2./float(csize))
usersym,cos(a)/2,sin(a)/2,/fill
symsize       = 0.8

SET_PLOT,'Z'
nMeans2Calc=fix(nLines+1/10)
meanNEDTs=fltarr(nchan,nMeans2Calc+1)

FOR ichan=0, nchan-1 DO BEGIN
    ;---Get min/max values for channel
    ymin=min(nedtTotal(*,ichan))
    ymax=max(nedtTotal(*,ichan))
    ymean=mean(nedtTotal(*,ichan))
    ;---Get ymin and ymax for plotting
    ymin_plot=min(nedtTotal[*,ichan])-stdev(nedtTotal[*,ichan])
    ymax_plot=max(nedtTotal[*,ichan])+4*stdev(nedtTotal[*,ichan])
    ;if ymax_plot lt ymax then ymax_plot=max(meanNEDTs[ichan,*])+7*stdev(nedtTotal[ind,ichan])
    ;if ymax_plot lt ymax then ymax_plot=max(meanNEDTs[ichan,*])+10*stdev(nedtTotal[ind,ichan])
    if max(nedtTotal[*,ichan]) lt 1 then ymax_plot=max(nedtTotal[*,ichan])+2*stdev(nedtTotal[*,ichan])

    ;---xmin/xmax
    xmin=min(dates)
    xmax=max(dates)

    ;---- we plot 3 sets of images (small and big images skipped for now)
    for isize=0, 2 do begin
        if ( isize ne 1) then CONTINUE
    	if ( isize eq 0 ) then begin
	    xticks = 6
	    device,set_resolution=[325, 250]
	    charsize=0.75
	    png_file = imgDir + prefix + '_ch' + strtrim(string(ichan+1),2) +'_d'+startDate+'_t'+startTime+'_d'+endDate+'_e'+endTime+'_c'+ctime+ '_small.png'
	endif
    	if ( isize eq 1 ) then begin
	    xticks = 6
	    device,set_resolution=[650, 500]
	    charsize=1.25
	    png_file = imgDir + prefix + '_ch' + strtrim(string(ichan+1),2) +'_d'+startDate+'_t'+startTime+'_d'+endDate+'_e'+endTime+'_c'+ctime+ '.png'
	endif
    	if ( isize eq 2 ) then begin
	    xticks = 18
	    device,set_resolution=[2000, 350]
	    charsize=1.25
	    png_file = imgDir + prefix + '_ch' + strtrim(string(ichan+1),2) +'_d'+startDate+'_t'+startTime+'_d'+endDate+'_e'+endTime+'_c'+ctime+ '_big.png'
	endif
	
	TVLCT, 0,0,0,         1 ; black
	TVLCT, 255,255,255, 255 ; white
	tit='MIRS ' + titleSat + ' NEDT Ch' + STRTRIM(ichan+1,2) + ' (' + titles(ichan) + ')'

        if (nLines+1 le 2700 and satId eq 'npp') then begin
            dummy=LABEL_DATE(DATE_FORMAT=['%H:%I'])
            if isize eq 2 then dummy=LABEL_DATE(DATE_FORMAT=['%H:%I:%S'])
	
            plot,julianday[sort(julianday)],nedtTotal(sort(julianday),ichan),psym=-8,title=tit,color=1,background=255,$
              ytitle='NEDT [K]',yrange=[ymin_plot,ymax_plot],$ 
              XTICKUNITS = [ 'Hour' ], XTICKFORMAT=['LABEL_DATE'], $
              xstyle=1,ystyle=1,xticks=xticks,CHARSIZE=charsize,MIN_VALUE=-98
            oplot,julianday[sort(julianday)], MAKE_ARRAY(nLines+1, /Float, VALUE=NominalNedts(ichan)), linestyle=2, color=1
        endif else begin
            
            dummy=LABEL_DATE(DATE_FORMAT=['%M%D!C%Y'])
            plot,julianday[sort(julianday)],nedtTotal[sort(julianday),ichan],psym=-8,title=tit,color=1,background=255,$
              ytitle='NEDT [K]',yrange=[ymin_plot,ymax_plot],$ 
              XTICKUNITS = [ 'Day' ], XTICKFORMAT=['LABEL_DATE'], $
              xstyle=1,ystyle=1,xticks=xticks,CHARSIZE=charsize,MIN_VALUE=-98
            oplot,julianday[sort(julianday)], MAKE_ARRAY(nLines+1, /Float, VALUE=NominalNedts(ichan)), linestyle=2, color=1
        endelse

    	xyouts, 0.2, 0.85, 'max=' + strtrim(string(ymax, format='(F5.2)'),2), COLOR=1, CHARSIZE=charsize, CHARTHICK=1.0, /normal 
    	xyouts, 0.2, 0.80, 'mean='+ strtrim(string(ymean,format='(F5.2)'),2), COLOR=1, CHARSIZE=charsize, CHARTHICK=1.0, /normal
    	xyouts, 0.2, 0.75, 'spec='+ strtrim(string(NominalNedts(ichan),format='(F4.2)'),2), COLOR=1, CHARSIZE=charsize, CHARTHICK=1.0, /normal
    	  
    	TVLCT, r, g, b, /Get
    	Write_PNG, png_file, TVRD(), r, g, b
    endfor
ENDFOR

END
