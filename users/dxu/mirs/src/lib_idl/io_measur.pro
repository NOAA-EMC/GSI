;$Id: io_measur.pro 2681 2011-08-01 19:53:52Z wchen $
;---------------------------------------------------------------------------------
; Summary of all subroutines related to I/O processes for the
; different radiometric files.
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
; Updated in June 06 to account for as many angles as channels (to
; accomodate Windsat). Sid Ahmed Boukabara
;---------------------------------------------------------------------------------


;-------------------------------------------------------------------------------------
;
;
; TB-Bias I/O subroutines
;
;
;-------------------------------------------------------------------------------------


;===============================================================
; Name:		WriteBias
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes the bias values into a file
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                 O             Unit number
;	- file               I             File name
;       - nchan2plot         I             #channels 4 which to output biases
;	- npos               I             # scan position within scanline
;	- cfreq              I             Central frequencies
;	- meanBias           I             Biases (npos x nchan)
;	- Slope              I             Slopes (npos x nchan)
;	- Intercept          I             Intercepts (npos x nchan)
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

PRO WriteBias,iu,file,nchan2plot,npos,cfreq,meanBias,Slope,Intercept,meanTbsimu,meanTbmeas
    openw,iu,file,/get_lun
    printf,iu,format='(2i4)',nchan2plot,npos
    
    fmt='(i4,f8.3,' + STRTRIM(STRING(FIX(npos)),2) + 'f7.2)'
    
    ;----Bias
    FOR ichan=0,nchan2plot-1 DO BEGIN
        printf,iu,format=fmt,ichan,cfreq(ichan),meanBias(0:npos-1,ichan)
    ENDFOR
    ;----Slope
    FOR ichan=0,nchan2plot-1 DO BEGIN
        printf,iu,format=fmt,ichan,cfreq(ichan),Slope(0:npos-1,ichan)
    ENDFOR
    ;----Intercept
    FOR ichan=0,nchan2plot-1 DO BEGIN
        printf,iu,format=fmt,ichan,cfreq(ichan),Intercept(0:npos-1,ichan)
    ENDFOR
    ;----Mean Tb (simulated)
    FOR ichan=0,nchan2plot-1 DO BEGIN
        printf,iu,format=fmt,ichan,cfreq(ichan),meanTbsimu(0:npos-1,ichan)
    ENDFOR
    ;----Mean Tb (measured)
    FOR ichan=0,nchan2plot-1 DO BEGIN
        printf,iu,format=fmt,ichan,cfreq(ichan),meanTbmeas(0:npos-1,ichan)
    ENDFOR
    close,iu
END

;===============================================================
; Name:		ReadBias
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads bias elements (bias, interept, slope) from
;               a file
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- file               I             File name
;       - nchan              O             #channels 
;	- npos               O             # scan position within scanline
;	- cfreq              O             Central frequencies
;	- meanBias           O             Biases (npos x nchan)
;	- Slope              O             Slopes (npos x nchan)
;	- Intercept          O             Intercepts (npos x nchan)
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ReadBias,file,nchan,npos,cfreq,meanBias,Slope,Intercept,meanTbsimu,meanTbmeas
    openr,iu,file,/get_lun
    nchan=0L
    npos=0L
    readf,iu,format='(2i4)',nchan,npos
    cfreq      = fltarr(nchan)
    X0         = fltarr(npos)
    meanBias   = fltarr(npos,nchan)
    Slope      = fltarr(npos,nchan)
    Intercept  = fltarr(npos,nchan)
    meanTbsimu = fltarr(npos,nchan)
    meanTbmeas = fltarr(npos,nchan)
    
    fmt='(i4,f8.3,' + STRTRIM(STRING(FIX(npos)),2) + 'f7.2)'
    
    ;---Bias
    FOR ichan=0,nchan-1 DO BEGIN
        readf,iu,format=fmt,ichan0,cfreq0,X0
        cfreq[ichan]=cfreq0
        meanBias[0:npos-1,ichan]=X0[0:npos-1]
    ENDFOR
    ;---Slope
    FOR ichan=0,nchan-1 DO BEGIN
        readf,iu,format=fmt,ichan0,cfreq0,X0
        cfreq[ichan]=cfreq0
        Slope[0:npos-1,ichan]=X0[0:npos-1]
    ENDFOR
    ;---Intercept
    FOR ichan=0,nchan-1 DO BEGIN
        readf,iu,format=fmt,ichan0,cfreq0,X0
        cfreq[ichan]=cfreq0
        Intercept[0:npos-1,ichan]=X0[0:npos-1]
    ENDFOR
    ;---Mean Tbs (simulated)
    FOR ichan=0,nchan-1 DO BEGIN
        readf,iu,format=fmt,ichan0,cfreq0,X0
        cfreq[ichan]=cfreq0
        meanTbsimu[0:npos-1,ichan]=X0[0:npos-1]
    ENDFOR
    ;---Mean Tbs (measured)
    FOR ichan=0,nchan-1 DO BEGIN
        readf,iu,format=fmt,ichan0,cfreq0,X0
        cfreq[ichan]=cfreq0
        meanTbmeas[0:npos-1,ichan]=X0[0:npos-1]
    ENDFOR
    close,iu
    free_lun,iu,/force
END





;-------------------------------------------------------------------------------------
;
;
; Radiometric I/O subroutines
;
;
;-------------------------------------------------------------------------------------

;===============================================================
; Name:		WriteRadHdrScanLMode
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes header of radiance file (scanline mode)
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                 O           Unit number
;	- file               I           File name
;	- nScanL             I           Number of scanlines
;	- nFovs              I           Number of FOVs per scanline
;	- nqc                I           Size of QC vector
;	- nchan              I           Number of channels
;	- cfreq              I           Central frequencies
;	- pol                I           Polarizations
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

PRO WriteRadHdrScanLMode,iu,file,nScanL,nFovs,nqc,nchan,cfreq,pol
    ;openw,iu,file,/get_lun,/f77_unformatted
    openw,iu,file,/get_lun
    writeu,iu,nScanL,nFovs,nqc,nchan
    writeu,iu,cfreq
    writeu,iu,pol
END



;===============================================================
; Name:		ReadRadHdrScanLMode
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads header of radiance file (scanline mode)
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                 O           Unit number
;	- file               O           File name
;	- nScanL             O           Number of scanlines
;	- nFovs              O           Number of FOVs per scanline
;	- nqc                O           Size of QC vector
;	- nchan              O           Number of channels
;	- cfreq              O           Central frequencies
;	- pol                O           Polarizations
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

PRO ReadRadHdrScanLMode,iu,file,nScanL,nFovs,nqc,nchan,cfreq,pol
    nscanL     =0L
    nFovs      =0L
    nqc        =0L
    nchan      =0L
    openr,iu,file,/get_lun,/f77_unformatted,/swap_if_little_endian
    readu,iu,nScanL,nFovs,nqc,nchan
    cfreq=fltarr(nchan)
    pol=intarr(nchan)
    readu,iu,cfreq
    readu,iu,pol
END

;===============================================================
; Name:		WriteRadMeasScanLMode
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes radiance measurements nd associated info
;               (lat, lon, time, angle, etc) into the measurement
;               file. Assumed to be open previously.
;               The measurements are stored in scanline mode.
;
;
; Arguments:
;
;	    Name     	 Type	    Description
;      ---------------------------------------------------
;	- iu               I       Unit number 
;	- nqc              I       Size of QC vector           
;	- qc               I       QC vector
;	- nchan            I       Number of channels
;	- nFovs            I       Number of FOVs per scanline
;	- angle            I       Viewing Angle
;	- RelAziAngle      I       Relative Azimuth Angle
;	- SolZenAngle      I       Solar Zenith Angle
;	- tb               I       TB array
;	- lat              I       Latitude
;	- lon              I       Longitude
;	- node             I       Orbit mode (asc/des)
;	- scanUTC          I       UTC time
;	- scanDAY          I       Julian Day
;	- scanYear         I       Year
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

PRO WriteRadMeasScanLMode,iu,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,$
       scanUTC,scanDAY,scanYear,RelAziAngle,SolZenAngle
    writeu,iu,node
    writeu,iu,scanDAY,scanYear
    writeu,iu,scanUTC
    writeu,iu,lat
    writeu,iu,lon
    writeu,iu,angle
    writeu,iu,RelAziAngle
    writeu,iu,SolZenAngle
    writeu,iu,tb
    IF (nqc  gt 0) THEN BEGIN
        writeu,iu,qc
    ENDIF
END

;===============================================================
; Name:		ReadRadMeasScanLMode
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads radiance measurements and associated info
;               (lat, lon, time, angle, etc) from the measurement
;               file. Assumed to be open previously.
;               The measurements are stored in scanline mode.
;
;
; Arguments:
;
;	    Name		    Type	    Description
;      ---------------------------------------------------
;	- iu               I       Unit number 
;	- nqc              I       Size of QC vector           
;	- qc               I       QC vector
;	- nchan            I       Number of channels
;	- nFovs            I       Number of FOVs per scanline
;	- angle            I       Viewing Angle
;	- RelAziAngle      I       Relative Azimuth Angle
;	- SolZenAngle      I       Solar Zenith Angle
;	- tb               I       TB array
;	- lat              I       Latitude
;	- lon              I       Longitude
;	- node             I       Orbit mode (asc/des)
;	- scanUTC          I       UTC time
;	- scanDAY          I       Julian Day
;	- scanYear         I       Year
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

PRO ReadRadMeasScanLMode,iu,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,$
       scanUTC,scanDAY,scanYear,RelAziAngle,SolZenAngle
    node        = 0L
    scanDay     = 0L
    scanYear    = 0L
    scanUTC     = 0L
    Readu,iu, node
    Readu,iu, scanDAY,scanYear
    Readu,iu, scanUTC
    lat         = fltarr(nFovs)
    lon         = fltarr(nFovs)
    angle       = fltarr(nFovs)
    RelAziAngle = fltarr(nFovs)
    SolZenAngle = fltarr(nFovs)
    tb          = fltarr(nFovs,nchan)
    Readu,iu, lat
    Readu,iu, lon
    Readu,iu, angle
    Readu,iu, RelAziAngle
    Readu,iu, SolZenAngle
    Readu,iu, tb
    IF (nqc  gt 0) THEN BEGIN
        qc=lonarr(nqc)
        Readu,iu, qc
    ENDIF
END

;===============================================================
; Name:		LoadRadFile
;
;
; Type:		IDL Subroutine
;
;
; Description:  This subroutine uses ReadRadHdr and ReadRad 
;               to load data into a structure containing 
;               radiance measurements for different files.
;               
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- nfilesRad          I           Number of radiance files to read
;	- filesRad           I           List of files of read
;	- Rad                O           Structure containing
;                                        measurements (see below for desc) 
;	- Idtop              X           Dummy (not needed here)
;
;
; Subroutines needed:
;       - ReadRadHdr
;       - ReadRad
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO LoadRadFile,nfilesRad,filesRad,Rad,Idtop
  nChanVec=intarr(nfilesRad)
  nPosScanVec=intarr(nfilesRad)
  nScanLinesVec=intarr(nfilesRad)
  nProfVec=lonarr(nfilesRad)
  ius     =intarr(nfilesRad)
  nqcVec  =intarr(nfilesRad)
  FOR ifile=0,nfilesRad-1 DO BEGIN
      ReadRadHdr,IdTop,iu,filesRad(ifile),nMeasurData,nchan,cfreq,polar,nqc,nPosScan,nScanLines
      nChanVec(ifile)      = nchan
      nProfVec(ifile)      = nMeasurData
      ius(ifile)           = iu
      nqcVec(ifile)        = nqc
      nPosScanVec(ifile)   = nPosScan
      nScanLinesVec(ifile) = nScanLines
      ;---consistency checks
      IF (ifile ge 1) THEN BEGIN
          IF (nChanVec(ifile) ne nChanVec(ifile-1)) THEN BEGIN
              STOP,'Error: # Channels ionconsistent in Rad files'
          ENDIF
          IF (nPosScanVec(ifile) ne nPosScanVec(ifile-1)) THEN BEGIN
              print,'Error: # Scan Positions ionconsistent in Rad files:',ifile,nPosScanVec(ifile),$
                nPosScanVec(ifile-1),filesRad(ifile),filesRad(ifile-1)
              stop
          ENDIF
          IF (nScanLinesVec(ifile) ne nScanLinesVec(ifile-1)) THEN BEGIN
              print,'Error: # Scan lines inconsistent in Rad files:',ifile,nScanLinesVec(ifile),$
                nScanLinesVec(ifile-1),filesRad(ifile),filesRad(ifile-1)
              ;stop
          ENDIF
          IF (nProfVec(ifile) ne nProfVec(ifile-1)) THEN BEGIN
              print,'Warning: # Profiles inconsistent in Rad files:'+$
              ' Minimum number used.',nProfVec(ifile),nProfVec(ifile-1)
          ENDIF
          FOR ichan=0,nchan-1 DO BEGIN
              IF (polar0(ichan) ne polar(ichan)) THEN BEGIN
                  print,'Error: Polarities inconsistent between Rad files @ chan#',ichan
                  STOP
              ENDIF
          ENDFOR
          FOR ichan=0,nchan-1 DO BEGIN
              IF (abs(fix(cfreq0(ichan)*10.)-fix(cfreq(ichan)*10.)) gt 2)  THEN BEGIN
                  print,'Error: Centr Frequencies inconsistent between Rad files @ chan#',ichan
		  print, cfreq0(ichan),cfreq(ichan)
                  STOP
              ENDIF
          ENDFOR
      ENDIF
      polar0=polar
      cfreq0=cfreq
  ENDFOR
  nprof=min(nProfVec(0:nfilesRad-1))
  nPosScan=min(nPosScanVec(0:nfilesRad-1))
  nScanLines=min(nScanLinesVec(0:nfilesRad-1))
  ;---Set Up the Rad structure
  Rad={                                             $
       ;----Header
       nfilesRad:   nfilesRad,                      $           ;Number of files 
       nProf:       nprof,                          $           ;Number of profiles
       nchan:       nchan,                          $           ;Number of channels
       nPosScan:    nPosScan,                       $           ;Number of scan positions
       nScanLines:  nScanLines,                     $           ;Number of scan lines
       nqc:         fltarr(nfilesRad),              $           ;Size of QC vector
       Cfreq:       cfreq,                          $           ;Central frequencies
       Polarity:    polar,                          $           ;Polarizations
       tb:          fltarr(nfilesRad,nProf,nchan),  $           ;Array of TBs
       Angle:       fltarr(nfilesRad,nProf,nchan),  $           ;Array of viewing angles
       RelAziAngle: fltarr(nfilesRad,nProf),        $           ;Array of relative azimuth angles
       SolZenAngle: fltarr(nfilesRad,nProf),        $           ;Array of solar zenith angles
       Lat:         fltarr(nfilesRad,nProf),        $           ;Array of latitude
       Lon:         fltarr(nfilesRad,nProf),        $           ;Array of longitude
       Direc:       intarr(nfilesRad,nProf),        $           ;Array of orbot mode flags
       ScanPos:     intarr(nfilesRad,nProf),        $           ;Scan positions
       ScanLine:    intarr(nfilesRad,nProf),        $           ;Scan lines
       Year:        lonarr(nfilesRad,nProf),        $           ;Year
       Day:         lonarr(nfilesRad,nProf),        $           ;Day
       Time:        fltarr(nfilesRad,nProf),        $           ;UTC Time
       Hours:       fltarr(nfilesRad,nProf),        $           ;Hours
       Mins:        fltarr(nfilesRad,nProf),        $           ;Minutes
       Secs:        fltarr(nfilesRad,nProf),        $           ;Seconds
       qc:          intarr(nfilesRad,nProf,14)      $           ;QC information
      }
  ;---Read the TBs
  FOR ifile=0,nfilesRad-1 DO BEGIN
      ReadRad,IdTop,ius(ifile),nprof,nchan,tb,nqcVec(ifile),qc,Angle,lat,lon,direc,iscanPos,$
        iscanLine,Year,Day,Time,nprofsEff,RelAziAngle,SolZenAngle
      nProfVec(ifile)                      = nprofsEff
      IF (nprofsEff le nProf) THEN nprof   = nprofsEff
      rad.tb(ifile,0:nProf-1,0:nchan-1)    = tb(0:nProf-1,0:nchan-1)
      rad.Angle(ifile,0:nProf-1,0:nchan-1) = Angle(0:nProf-1,0:nchan-1)
      rad.RelAziAngle(ifile,0:nProf-1)     = RelAziAngle(0:nProf-1)
      rad.SolZenAngle(ifile,0:nProf-1)     = SolZenAngle(0:nProf-1)
      rad.Lat(ifile,0:nProf-1)             = Lat(0:nProf-1)
      rad.Lon(ifile,0:nProf-1)             = Lon(0:nProf-1)
      rad.Direc(ifile,0:nProf-1)           = Direc(0:nProf-1)
      rad.ScanPos(ifile,0:nProf-1)         = iscanPos(0:nProf-1)
      rad.ScanLine(ifile,0:nProf-1)        = iscanLine(0:nProf-1)
      rad.nqc(ifile)                       = nqcVec(ifile)
      rad.Year(ifile,0:nProf-1)            = Year(0:nProf-1)
      rad.Day(ifile,0:nProf-1)             = Day(0:nProf-1)
      rad.Time(ifile,0:nProf-1)            = Time(0:nProf-1)
      rad.Hours(ifile,0:nProf-1)           = fix(Time(0:nProf-1)/3600.)
      rad.Mins(ifile,0:nProf-1)            = fix((rad.Time(ifile,0:nProf-1)-fix(rad.Hours(ifile,0:nProf-1))*3600.)/60.)
      rad.Secs(ifile,0:nProf-1)            = (rad.Time(ifile,0:nProf-1)-(rad.Hours(ifile,0:nProf-1))*3600.-(rad.Mins(ifile,0:nProf-1)*60.))
      IF (nqcVec(ifile) gt 0)THEN BEGIN
          rad.qc(ifile,0:nProf-1,0:nqcVec(ifile)-1)   = qc(0:nProf-1,0:nqcVec(ifile)-1)
      ENDIF ELSE BEGIN
          rad.qc(ifile,0:nProf-1,0:1) =0
      ENDELSE
      close,ius(ifile)
      free_lun,ius(ifile)
  ENDFOR
  Rad.nprof = nprof
END



;===============================================================
; Name:		ReadRad
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads content of radiance file. Assumed open
;               elsewhere. Storage is point-by-point.
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- IdTop              I              Not used.
;	- iu                 I              Unit number
;	- nprof              I              Number of profiles
;	- nchan              I              Number of channels
;	- tb                 O              TB array
;	- nqc                I              Size of QC array
;	- qc                 O              QC array
;	- Angle              O              Viewing angle
;       - RelAziAngle        O              Relative Azimuth Angle
;       - SolZenAngle        O              Solar Zenith Angle
;	- lat                O              Latitude
;	- lon                O              Longitude
;	- direc              O              Orbit mode
;	- iscanPos           O              Scan position
;	- Year               O              Year
;	- Day                O              Day
;	- Time               O              Time
;	- nprofsEff          O              Efective number of profiles
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

PRO ReadRad,IdTop,iu,nprof,nchan,tb,nqc,qc,Angle,lat,lon,direc,iscanPos,iscanLine,Year,Day,Time,nprofsEff,RelAziAngle,SolZenAngle
    IF (nqc gt 0) THEN BEGIN
        qc=lonarr(nprof,nqc)
        qc0=lonarr(nqc)
    ENDIF
    tb             = fltarr(nprof,nchan)
    Angle          = fltarr(nprof,nchan)
    RelAziAngle    = fltarr(nprof)
    SolZenAngle    = fltarr(nprof)
    tb0            = fltarr(nchan)
    lat            = fltarr(nprof)
    lon            = fltarr(nprof)
    Year           = lonarr(nprof)
    Day            = lonarr(nprof)
    time           = fltarr(nprof)
    direc          = lonarr(nprof)
    iscanPos       = lonarr(nprof)
    iscanLine      = lonarr(nprof)
    scanyear       = 0L
    scanDay        = 0L
    node           = 0L
    iPos           = 0L
    iLine          = 0L
    ioErr          = 0
    nprofsEff      = 0L
    ON_IOerror,endRead
    FOR iprof=0L,nprof-1L DO BEGIN
        readu,iu,y,z,w,v
        nProfsEff            = nprofsEff+1
        lat(iprof)           = y
        lon(iprof)           = z
        RelAziAngle(iprof)   = w
        SolZenAngle(iprof)   = v
        readu,iu,node,iPos,iLine,scanyear,scanday,x
        direc(iprof)    = node
        iscanPos(iprof) = iPos
        iscanLine(iprof) = iLine
        Year(iprof)     = scanyear
        Day(iprof)      = scanday
        time(iprof)     = x
        readu,iu,tb0 
        angle(iprof,0:nchan-1) = tb0(0:nchan-1)
        readu,iu,tb0 
        tb(iprof,0:nchan-1)=tb0(0:nchan-1)
        IF (nqc gt 0) THEN BEGIN
            readu,iu,qc0
            qc(iprof,0:nqc-1)=qc0(0:nqc-1)
        ENDIF
    ENDFOR
    RETURN
    endRead: ioErr=1
    print, 'Warning: Effective Number of radiances read:',nprofsEff
END


PRO WriteRad,fileRad,nprof,nchan,nPosScan,nScanLines,nqc,cfreq,polar,lat,lon,direc,iscanPos,iscanLine,Year,Day,Time,$
             Angle,tb,qc,RelAziAngle,SolZenAngle
    OPENW,iu,fileRad,/get_lun,error=err,/f77_unformatted
    IF (err ne 0) THEN BEGIN
        print,'UNSUCCESSFUL OPENING:',!ERROR_STATE.MSG
        RETURN
    ENDIF
    writeu,iu,long(nprof)
    writeu,iu,nchan
    writeu,iu,nPosScan
    writeu,iu,nScanLines
    writeu,iu,nqc
    writeu,iu,cfreq
    writeu,iu,polar
    FOR iprof=0L,nprof-1L DO BEGIN
        writeu,iu,lat(iprof),lon(iprof),RelAziAngle(iprof),SolZenAngle(iprof)
        writeu,iu,direc(iprof),iscanPos(iprof),iscanLine(iprof),Year(iprof),Day(iprof),time(iprof)
        writeu,iu,angle(iprof,0:nchan-1)
        writeu,iu,tb(iprof,0:nchan-1)
        IF (nqc gt 0) THEN BEGIN
            writeu,iu,qc(iprof,0:nqc-1)
        ENDIF
    ENDFOR
    close,iu
    free_lun,iu
    RETURN
END


;===============================================================
; Name:		ReadRadHdr
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads the header of a radiance file after opening it
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- TopId              I              Not used 
;	- iu                 O              Unit number
;	- fileRad            I              Name of radiance file
;	- nMeasurData        O              Number of measuremnts in file
;	- nchan              O              Number of channels
;	- cfreq              O              Central frequencies
;	- polar              O              Poalrizations
;	- nqc                O              Size of QC 
;	- nPosScan           O              Number of scan positions
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

PRO ReadRadHdr,TopId,iu,fileRad,nMeasurData,nchan,cfreq,polar,nqc,nPosScan,nScanLines
    OPENR,iu,fileRad,/get_lun,error=err,/f77_unformatted,/swap_if_little_endian
    IF (err ne 0) THEN BEGIN
        ;res=DIALOG_MESSAGE(!ERROR_STATE.MSG,DISPLAY_NAME='ERROR',/ERROR,$
        ;                   TITLE='UNSUCCESSFUL OPENING',DIALOG_PARENT=topId)
        print,'UNSUCCESSFUL OPENING:',!ERROR_STATE.MSG
        RETURN
    ENDIF
    nchan=0L
    nMeasurData=0L
    nPosScan=0L
    nScanLines=0L
    nqc=0L
    readu,iu,nMeasurData 
    readu,iu,nchan
    readu,iu,nPosScan,nScanLines
    readu,iu,nqc
    cfreq=fltarr(nchan)
    polar=lonarr(nchan)
    readu,iu,cfreq
    readu,iu,polar
End 


;-------------------------------------------------------------------------------------
;
;
; Radiometric I/O subroutines  (A S C I I   version)
;
;
;-------------------------------------------------------------------------------------
;===============================================================
; Name:		WriteRadHdrScanLMode_ascii
;
;
; Type:		IDL Subroutine
;
;
; Description:
;
;
; Arguments:  Writes header of radiance file in scanline mode 
;             (ASCII format)
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                 O            Unit number
;	- file               I            Name of output file
;	- nScanL             I            Number of scan line positions
;	- nFovs              I            Number of FOVs per scanline
;	- nqc                I            Size of QC
;	- nchan              I            Number of channels
;	- cfreq              I            Central frequencies
;	- pol                I            Polarizations
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

PRO WriteRadHdrScanLMode_ascii,iu,file,nScanL,nFovs,nqc,nchan,cfreq,pol
    ;openw,iu,file,/get_lun,/f77_unformatted
    openw,iu,file,/get_lun
    printf,iu,format='(4i4)',nScanL,nFovs,nqc,nchan
    printf,iu,format='(10f10.5)',cfreq
    printf,iu,format='(20i3)',pol
END

;===============================================================
; Name:		ReadRadHdrScanLMode_ascii
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads header of radiance file in scanline mode 
;               (ASCII format)
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                 O            Unit number
;	- file               I            Name of output file
;	- nScanL             O            Number of scan line positions
;	- nFovs              O            Number of FOVs per scanline
;	- nqc                O            Size of QC
;	- nchan              O            Number of channels
;	- cfreq              O            Central frequencies
;	- pol                O            Polarizations
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

PRO ReadRadHdrScanLMode_ascii,iu,file,nScanL,nFovs,nqc,nchan,cfreq,pol
    nscanL     =0L
    nFovs      =0L
    nqc        =0L
    nchan      =0L
    openr,iu,file,/get_lun
    readf,iu,format='(4i4)',nScanL,nFovs,nqc,nchan
    cfreq=fltarr(nchan)
    pol=lonarr(nchan)
    readf,iu,format='(10f10.5)',cfreq
    readf,iu,format='(20i3)',pol
END


;===============================================================
; Name:		WriteRadMeasScanLMode_ascii
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes radiance measurement and other associated 
;               info into an ASCII-formatted scanline mode file.
;
;
; Arguments:
;
;	    Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                 I              Unit number
;	- nqc                I              Size of QC array
;	- qc                 I              QC array
;	- nchan              I              Number of channels
;	- nFovs              I              Number of FOVs per scanline            
;	- angle              I              Viewing angle
;	- relAziangle        I              Relative Azimuth angle
;	- solZenangle        I              Solar Zenith angle
;	- tb                 I              TB array
;	- lat                I              Latitude
;	- lon                I              Longitude
;	- node               I              Orbit mode
;	- scanUTC            I              UTC time
;	- scanDAY            I              Julian day
;	- scanYear           I              Year
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

PRO WriteRadMeasScanLMode_ascii,iu,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,$
       scanUTC,scanDAY,scanYear,RelAziangle,SolZenangle
    printf,iu,format='(i4,3i10)', node,scanDAY,scanYear,scanUTC
    printf,iu,format='(10f8.2)', lat
    printf,iu,format='(10f8.2)', lon
    printf,iu,format='(10f8.2)', angle
    printf,iu,format='(10f8.2)', RelAziangle
    printf,iu,format='(10f8.2)', SolZenangle
    printf,iu,format='(10f8.2)', tb
    IF (nqc  gt 0) THEN BEGIN
        printf,iu,format='(10i4)', qc
    ENDIF
END

;===============================================================
; Name:		ReadRadMeasScanLMode_ascii
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads radiance measurement and other associated 
;               info from an ASCII-formatted scanline mode file.
;
;
; Arguments:
;
;	    Name		    Type	    Description
;      ---------------------------------------------------
;	- iu                 I              Unit number
;	- nqc                I              Size of QC array
;	- qc                 O              QC array
;	- nchan              I              Number of channels
;	- nFovs              I              Number of FOVs per scanline            
;	- angle              O              Viewing angle
;	- relAziangle        I              Relative Azimuth angle
;	- solZenangle        I              Solar Zenith angle
;	- tb                 O              TB array
;	- lat                O              Latitude
;	- lon                O              Longitude
;	- node               O              Orbit mode
;	- scanUTC            O              UTC time
;	- scanDAY            O              Julian day
;	- scanYear           O              Year
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

PRO ReadRadMeasScanLMode_ascii,iu,nqc,qc,nchan,nFovs,angle,tb,lat,lon,node,$
       scanUTC,scanDAY,scanYear,RelAziangle,SolZenangle
    node     = 0L
    scanDay  = 0L
    scanYear = 0L
    scanUTC  = 0L
    readf,iu,format='(i4,3i10)', node,scanDAY,scanYear,scanUTC
    lat         = fltarr(nFovs)
    lon         = fltarr(nFovs)
    angle       = fltarr(nFovs)
    RelAziangle = fltarr(nFovs)
    SolZenangle = fltarr(nFovs)
    tb          = fltarr(nFovs,nchan)
    readf,iu,format='(10f8.2)', lat
    readf,iu,format='(10f8.2)', lon
    readf,iu,format='(10f8.2)', angle
    readf,iu,format='(10f8.2)', RelAziangle
    readf,iu,format='(10f8.2)', SolZenangle
    readf,iu,format='(10f8.2)', tb
    IF (nqc  gt 0) THEN BEGIN
        qc=lonarr(nqc)
        readf,iu,format='(10i4)', qc
    ENDIF
END

