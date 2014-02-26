;---------------------------------------------------------------------------------
; Summary of all subroutines related to I/O processes for the
; different dropsondes files.
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------

;-------------------------------------------------------------------------------------
;
;
; Dropsondes I/O subroutines
;
;
;-------------------------------------------------------------------------------------

;===============================================================
; Name:		LoadDS
;
;
; Type:		IDL Subroutine
;
;
; Description:  Loads the content of a large number of files 
;               corresponding to a number of dropsondes.
;               Puts the content into a structure that is
;               passed back to calling script (see content description
;               of the structure).
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- nfilesDS           I              Number of files to read
;	- filesDS            I              Dropsondes Files to read 
;	- DS                 O              Dropsondes data structure
;
;
; Subroutines needed:
;       - ReadDSHdr
;       - ReadDS
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO LoadDS,nfilesDS,filesDS,DS
  mxLevs  = 3000
  ;---Set Up the DropSonde (DS) structure
  DS={                                           $
       ;----Header
       nDS:      nfilesDS,                       $  ;Number of files corresponding to DS data
       mxLevs:   mxLevs,                         $  ;Max levels (used for setting up the structure)
       DS_SN:    lonarr(nfilesDS),               $  ;DS serial number vector
       DSdate:   lonarr(nfilesDS),               $  ;DS date vector
       DStime:   lonarr(nfilesDS),               $  ;DS time vector
       DSaircft: strarr(nfilesDS),               $  ;DS aircraft ID vector
       DSlat:    fltarr(nfilesDS),               $  ;DS latitudes (at dropping)
       DSlon:    fltarr(nfilesDS),               $  ;DS longitudes (at dropping)
       DShours:  lonarr(nfilesDS),               $  ;DS hours (at dropping)
       DSmins:   lonarr(nfilesDS),               $  ;DS minutes (at dropping)
       DSsecs:   fltarr(nfilesDS),               $  ;DS seconds (at dropping)
       DSmonth:  lonarr(nfilesDS),               $  ;DS month (at dropping)
       DSday:    lonarr(nfilesDS),               $  ;DS day (at dropping)
       DSyear:   lonarr(nfilesDS),               $  ;DS Year day (at dropiing)
       DSjday:   lonarr(nfilesDS),               $  ;DS julian day (at dropiing)
       ;----Body of the DS
       nLevs:    lonarr(nfilesDS),               $  ;Number of levels for every DS
       tsecProf: fltarr(nfilesDS,mxLevs),        $  ;Time in secs for every layer, every DS
       Press:    fltarr(nfilesDS,mxLevs),        $  ;Pressure profile for every DS 
       Temp:     fltarr(nfilesDS,mxLevs),        $  ;Temperature profile for every DS
       RelHum:   fltarr(nfilesDS,mxLevs),        $  ;Relative humidity profile for every DS
       qProf:    fltarr(nfilesDS,mxLevs),        $  ;Mixing Ratio (WV) profile for every DS
       Z:        fltarr(nfilesDS,mxLevs),        $  ;Altitude profile for every DS
       WSProf:   fltarr(nfilesDS,mxLevs),        $  ;Wind Speed profile for every DS
       FP:       fltarr(nfilesDS,mxLevs),        $  ;Flag of Pressure profile for every DS
       FT:       fltarr(nfilesDS,mxLevs),        $  ;Flag of Temperature profile for every DS
       FH:       fltarr(nfilesDS,mxLevs),        $  ;Flag of Relative humidity profile for every DS
       FW:       fltarr(nfilesDS,mxLevs),        $  ;Flag of Wind Speed profile for every DS
       LatProf:  fltarr(nfilesDS,mxLevs),        $  ;Latitude profile for every DS
       LonProf:  fltarr(nfilesDS,mxLevs),        $  ;Longitude profile for every DS
       WSatSurf: fltarr(nfilesDS),               $  ;Saturated water vapor at surface level 
       Psurf:    fltarr(nfilesDS),               $  ;Surface pressure
       TPW:      fltarr(nfilesDS)                $  ;Total Precipitable Water
      }
  FOR ifile=0,nfilesDS-1 DO BEGIN
      ;----Read header first
      ReadDSHdr,iu,filesDS(ifile),sondeSN,date,time,ACid,lat,lon,hours,mins,secs,month,day,year,jday
      DS.DS_SN(ifile)     = sondeSN
      DS.DSdate(ifile)    = date
      DS.DStime(ifile)    = time
      DS.DSaircft(ifile)  = ACid
      DS.DSlat(ifile)     = lat
      DS.DSlon(ifile)     = lon
      DS.DShours(ifile)   = hours
      DS.DSmins(ifile)    = mins
      DS.DSsecs(ifile)    = secs
      DS.DSmonth(ifile)   = month
      DS.DSday(ifile)     = day
      DS.DSyear(ifile)    = year
      DS.DSjday(ifile)    = jday
      print,'Dropsonde#',ifile,DS.DSlon(ifile),DS.DSlat(ifile),DS.DSjday(ifile),DS.DShours(ifile),DS.DSmins(ifile),sondeSN
      ;---Then read body of the dropsonde file
      ReadDS,iu,mxLevs,nLevs,tsecProf,Press,Temp,RelHum,Z,WSProf,FP,FT,    $
        FH,FW,LatProf,LonProf,qProf,WSatSurf,Psurf
      DS.nLevs(ifile)              = nLevs
      DS.WSatSurf(ifile)           = WSatSurf
      DS.Psurf(ifile)              = Psurf
      DS.tsecProf(ifile,0:nLevs-1) = tsecProf(0:nLevs-1)
      DS.Press(ifile,0:nLevs-1)    = Press(0:nLevs-1)
      DS.Temp(ifile,0:nLevs-1)     = Temp(0:nLevs-1)
      DS.RelHum(ifile,0:nLevs-1)   = RelHum(0:nLevs-1)
      DS.Z(ifile,0:nLevs-1)        = Z(0:nLevs-1)
      DS.WSProf(ifile,0:nLevs-1)   = WSProf(0:nLevs-1)
      DS.FP(ifile,0:nLevs-1)       = FP(0:nLevs-1)
      DS.FT(ifile,0:nLevs-1)       = FT(0:nLevs-1)
      DS.FH(ifile,0:nLevs-1)       = FH(0:nLevs-1)
      DS.FW(ifile,0:nLevs-1)       = FW(0:nLevs-1)
      DS.LatProf(ifile,0:nLevs-1)  = LatProf(0:nLevs-1)
      DS.LonProf(ifile,0:nLevs-1)  = LonProf(0:nLevs-1)
      DS.qProf(ifile,0:nLevs-1)    = qProf(0:nLevs-1)
      ;---compute TPW
      ind=where(Press lt max([Psurf,max(press(where(press gt 0)))]) and press gt 0 and DS.qProf(ifile,*) ge 0,nLay)
      ColumIntegr_LayW,nLay,[Press(ind),Psurf],Psurf,$
        DS.qProf(ifile,ind),water 
      if (water lt 0) then stop
      DS.TPW(ifile)  = water
      close,iu
      free_lun,iu
  ENDFOR
END


;===============================================================
; Name:		ReadDS
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads the content of a drosonde file (after header
;               has been read and file open).
;
;
; Arguments:
;
;	    Name       Type	    Description
;      ---------------------------------------------------
;	- iu            I           Unit number
;	- mxLevs        I           Max number of levels for declaration
;	- nLevs         O           Actual number of levels in DS profile
;	- tsecProf      O           Time profile (in secs) 
;	- Press         O           Pressure profile
;	- Temp          O           Temperature profile
;	- RelHum        O           Relative humidity profile
;	- Zprof         O           Altitude profile
;	- WSprof        O           Wind speed profile
;	- FPprof        O           Flag press profile
;	- FTprof        O           Flag temperature profile
;	- FHprof        O           Flag humidity profile
;	- FWprof        O           Flag Wind profile
;	- Latprof       O           Latitude profile
;	- Lonprof       O           Longitude profile
;	- qProf         O           WV mixing ratio profile
;	- WSatSurf      O           Saturated Water vapor at surface
;	- Psurf         O           Surface pressure
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ReadDS,iu,mxLevs,nLevs,tsecProf,Press,Temp,RelHum,Zprof,WSprof,FPprof,FTprof,FHprof,FWprof,$
           Latprof,Lonprof,qProf,WSatSurf,Psurf
  nlevs    = 0L
  WSatSurf = -999.
  Psurf    = -999.
  tsecProf = fltarr(mxLevs)
  Press    = fltarr(mxLevs)
  Temp     = fltarr(mxLevs)
  RelHum   = fltarr(mxLevs)
  Zprof    = fltarr(mxLevs)
  WSprof   = fltarr(mxLevs)
  FPprof   = fltarr(mxLevs)
  FTprof   = fltarr(mxLevs)
  FHprof   = fltarr(mxLevs)
  FWprof   = fltarr(mxLevs)
  Latprof  = fltarr(mxLevs)
  Lonprof  = fltarr(mxLevs)
  qProf    = fltarr(mxLevs)
  while (~EOF(iu)) DO BEGIN
      readf,iu,ix,tsec,p,t,rh,z,wd,ws,u,v,ns,wz,zw,fp,ft,fh,fw,lat,lon
      nlevs=nlevs+1
      IF (nlevs ge mxLevs) THEN STOP,'Error:MxLevs must be increased:'
      tsecProf(nlevs-1) = tsec
      Press(nlevs-1)    = p
      Temp(nlevs-1)     = t+273.15
      RelHum(nlevs-1)   = rh
      Zprof(nlevs-1)    = z
      WSprof(nlevs-1)   = ws
      FPprof(nlevs-1)   = fp
      FTprof(nlevs-1)   = ft
      FHprof(nlevs-1)   = fh
      FWprof(nlevs-1)   = fw
      Latprof(nlevs-1)  = lat
      Lonprof(nlevs-1)  = lon
      ;----convert rh to mix ratio
      IF (rh ge 0.) THEN BEGIN
          qProf(nlevs-1)    = RelHum_to_Mixingratio(rh/100.,T+273.15,P)*1000. ;kg/kg->g/kg
      ENDIF ELSE BEGIN
          qProf(nlevs-1)    = -999.
      ENDELSE
      ;----Wind Speed at 10 meters (if exists)
      IF (abs(z-10) le 5) THEN BEGIN
          WSatSurf=ws
          Psurf=p
      ENDIF
  ENDWHILE
  Psurf=max([Psurf,max(Press(nlevs-1))])
END


;===============================================================
; Name:		ReadDSHdr
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads header of the dropsonde file (containing one
;               dropsonde profile measurements)
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                O              Unit number
;	- fileDS            I              Dropsondes file name
;	- sondeSN           O              Sonde serial number
;	- date              O              Date of dropping
;	- time              O              Time of dropping
;	- ACid              O              Aircraft ID
;	- lat               O              Drop location latitude
;	- lon               O              Drop location longitude
;	- hours             O              Hours at time of dropping
;	- mins              O              Minutes at time of dropping
;	- secs              O              Seconds at time of dropping
;	- month             O              Month at time of dropping
;	- day               O              Day at time of dropping
;	- year              O              Year at time of dropping
;	- jday              O              Julian day at time of dropping
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ReadDSHdr,iu,fileDS,sondeSN,date,time,ACid,lat,lon,hours,mins,secs,month,day,year,jday
  OPENR,iu,fileDS,/get_lun,error=err
  IF (err ne 0) THEN BEGIN
      print,'Error in opening file:',fileDS
      STOP
  ENDIF
  ligne=''
  sondeSN=0L
  ACid=''
  date=0L
  time=0L
  readf,iu,format='(a)',ligne
  readf,iu,format='(66x,i20)',sondeSN
  readf,iu,format='(a)',ligne
  readf,iu,format='(10x,a15,25x,i7,8x,i7)',ACid,date,time
  hours=fix(time/10000.)
  mins=fix((time-hours*10000.)/100.)
  secs=fix(time-hours*10000.-mins*100.)
  year=fix(date/10000.)
  month=fix((date-year*10000.)/100.)
  day=fix(date-year*10000.-month*100.)
  year=2000+year
  jdayPope=julday(month,day,year)
  jday0=julday(1,1,year)
  jDay=fix(jdayPope-jday0+1)
  ;---read non-used comments
  FOR i=1,11 DO BEGIN
      readf,iu,format='(a)',ligne
  ENDFOR
  ;---Read lat/lon
  Dlat=''
  Dlon=''
  readf,iu,format='(20x,f7.2,1x,a1)',lat,DLat
  readf,iu,format='(20x,f7.2,1x,a1)',lon,Dlon
  IF (Dlat eq 'S') THEN lat=-lat
  IF (Dlon eq 'W') THEN lon=-lon
  ;---read non-used comments
  FOR i=1,4 DO BEGIN
      readf,iu,format='(a)',ligne
  ENDFOR
END

