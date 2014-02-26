;$Id: io_misc.pro 2865 2012-01-24 18:52:52Z chrisg $
;---------------------------------------------------------------------------------
; Summary of all subroutines related to I/O processes for the
; different miscellaneous files.
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------

;===============================================================
; Name:	        ReadRAOBMetaData
;
; Type:		IDL Subroutine
;
; Description:  Creates Data Structures for RAOB metadata
;               from ICDB by reading in ascii file with the data.
;
; Arguments:
;
;	    Name	   Type	    Description
;      ---------------------------------------------------
;       - RAOBfile           I       RAOB file containing metadata
;       - nProfiles          O       Number of profiles
;       - nDays              O       Number of days of data
;       - idx_nDailyRecs     0       Number of records from each day
;	- RAOB               O       Data structure for RAOB metadata
;
;
; Subroutines needed:
;       - None
;
;
; History:
;      09/27/2007   Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ReadRAOBMetaData,RAOBfile,nProfiles,nDays,idx_nDailyRecs,RAOB

    nProfiles=0L
    openr, rlun, RAOBfile, /get_lun
    readf, rlun, nDays
    readf, rlun, nProfiles
    idx_nDailyRecs      = intarr(1)
    tempArr             = fltarr(1,33)
    RAOB={                                     $
         Lat:             fltarr(nProfiles),   $
         Lon:             fltarr(nProfiles),   $
         TimeDiff:        fltarr(nProfiles),   $
         DistDiff:        fltarr(nProfiles),   $
         ClosePar:        fltarr(nProfiles),   $
         StaIDint:        intarr(nProfiles,3), $
         StaID:           strarr(nProfiles),   $
         Sta_rptType:     intarr(nProfiles),   $
         Sta_Instr:       intarr(nProfiles),   $
         Sta_Elev:        intarr(nProfiles),   $
         PassFail:        intarr(nProfiles),   $
         VertExtent:      intarr(nProfiles),   $
         TCapped:         intarr(nProfiles),   $
         SuperAdiabtc:    intarr(nProfiles),   $
         ProfInv:         intarr(nProfiles),   $
         MRscreen:        intarr(nProfiles),   $
         QCapped:         intarr(nProfiles),   $
         SfcDataBad:      intarr(nProfiles),   $
         WVanomaly:       intarr(nProfiles),   $
         WVevent1:        intarr(nProfiles),   $
         WVxEvent1:       intarr(nProfiles),   $
         WVevent2:        intarr(nProfiles),   $
         WVnegEvent:      intarr(nProfiles),   $
         WVxNegEvent:     intarr(nProfiles),   $
         TPresHigh:       fltarr(nProfiles),   $
         TPresLow:        fltarr(nProfiles),   $
         MRPresHigh:      fltarr(nProfiles),   $
         MRPresLow:       fltarr(nProfiles),   $
         PresTextrpld:    intarr(nProfiles),   $
         PresMRextrpld:   intarr(nProfiles),   $
         sfcTyp:          intarr(nProfiles),   $
         TPW:             fltarr(nProfiles)    $
         }
    FOR ir=0L,nProfiles - 1 DO BEGIN
        readf, rlun, tempArr
        RAOB.lat(ir)            = tempArr(0)
        RAOB.lon(ir)            = tempArr(1)
        RAOB.TimeDiff(ir)       = tempArr(2)
        RAOB.DistDiff(ir)       = tempArr(3)
        RAOB.ClosePar(ir)       = tempArr(4)
        RAOB.StaIDint(ir,0:2)   = tempArr(5:7)
        RAOB.Sta_rptType(ir)    = tempArr(8)
        RAOB.Sta_Instr(ir)      = tempArr(9)
        RAOB.Sta_Elev(ir)       = tempArr(10)
        RAOB.PassFail(ir)       = tempArr(11)
        RAOB.VertExtent(ir)     = tempArr(12)
        RAOB.TCapped(ir)        = tempArr(13)
        RAOB.SuperAdiabtc(ir)   = tempArr(14)
        RAOB.ProfInv(ir)        = tempArr(15)
        RAOB.MRscreen(ir)       = tempArr(16)
        RAOB.QCapped(ir)        = tempArr(17)
        RAOB.SfcDataBad(ir)     = tempArr(18)
        RAOB.WVanomaly(ir)      = tempArr(19)
        RAOB.WVevent1(ir)       = tempArr(20)
        RAOB.WVxEvent1(ir)      = tempArr(21)
        RAOB.WVevent2(ir)       = tempArr(22)
        RAOB.WVnegEvent(ir)     = tempArr(23)
        RAOB.WVxNegEvent(ir)    = tempArr(24)
        RAOB.TPresHigh(ir)      = tempArr(25)
        RAOB.TPresLow(ir)       = tempArr(26)
        RAOB.MRPresHigh(ir)     = tempArr(27)
        RAOB.MRPresLow(ir)      = tempArr(28)
        RAOB.PresTextrpld(ir)   = tempArr(29)
        RAOB.PresMRextrpld(ir)  = tempArr(30)
        RAOB.sfcTyp(ir)         = tempArr(31)
        RAOB.TPW(ir)            = tempArr(32)
    ENDFOR
    readf, rlun, idx_nDailyRecs
    free_lun, rlun
    ;---Put Station ID bytes into a string array
    StaID      = intarr(nProfiles,6)
    StaIDstr   = strarr(nProfiles,6)
    StaID(*,0) = strmid(strcompress(string(RAOB.StaIDint(*,0)),/remove_all),0,2)
    StaID(*,1) = strmid(strcompress(string(RAOB.StaIDint(*,0)),/remove_all),2,4)
    StaID(*,2) = strmid(strcompress(string(RAOB.StaIDint(*,1)),/remove_all),0,2)
    StaID(*,3) = strmid(strcompress(string(RAOB.StaIDint(*,1)),/remove_all),2,4)
    StaID(*,4) = strmid(strcompress(string(RAOB.StaIDint(*,2)),/remove_all),0,2)
    StaID(*,5) = strmid(strcompress(string(RAOB.StaIDint(*,2)),/remove_all),2,4)
    FOR ir=0L,nProfiles-1 DO BEGIN
        FOR iid=0,5 DO BEGIN
            StaIDstr(ir,iid) = strcompress(string(byte(StaID(ir,iid),0)))
        ENDFOR
        RAOB.StaID(ir) = StaIDstr(ir,0) + StaIDstr(ir,1) + StaIDstr(ir,2) + StaIDstr(ir,3) + StaIDstr(ir,4)
    ENDFOR
END

;===============================================================
; Name:	        ReadATOVSMetaData
;
; Type:		IDL Subroutine
;
; Description:  Creates Data Structures for ATOVS metadata
;               from ICDB by reading in ascii file with the data.
;
; Arguments:
;
;	    Name	   Type	    Description
;      ---------------------------------------------------
;       - ATOVSfile          I       ATOVS file containing metadata
;       - nProfiles          O       Number of profiles
;       - nDays              O       Number of days of data
;       - idx_nDailyRecs     0       Number of records from each day
;	- ATOVS              O       Data structure for ATOVS metadata
;
;
; Subroutines needed:
;       - None
;
;
; History:
;      09/27/2007   Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ReadATOVSMetaData,ATOVSfile,nProfiles,nDays,idx_nDailyRecs,ATOVS

    nProfiles=0L
    openr, rlun, ATOVSfile, /get_lun
    readf, rlun, nDays
    readf, rlun, nProfiles
    tempArr            = fltarr(1,11)
    ATOVS={                                  $
          sfcElev:      intarr(nProfiles), $
          sfcTemp:      fltarr(nProfiles), $
          SST:          fltarr(nProfiles), $
          TPW:          fltarr(nProfiles), $
          precip:       intarr(nProfiles), $
          dayNight:     intarr(nProfiles), $
          proc:         intarr(nProfiles), $
          SuperAdiabtc: intarr(nProfiles), $
          ObsQuality:   intarr(nProfiles), $
          RetrvlFlag:   intarr(nProfiles), $
          ChComboFlag:  intarr(nProfiles)  $
          }
    FOR ir=0L,nProfiles - 1 DO BEGIN
        readf, rlun, tempArr
        ATOVS.sfcElev(ir)      = tempArr(0)
        ATOVS.sfcTemp(ir)      = tempArr(1)
        ATOVS.SST(ir)          = tempArr(2)
        ATOVS.TPW(ir)          = tempArr(3)
        ATOVS.precip(ir)       = tempArr(4)
        ATOVS.dayNight(ir)     = tempArr(5)
        ATOVS.proc(ir)         = tempArr(6)
        ATOVS.SuperAdiabtc(ir) = tempArr(7)
        ATOVS.ObsQuality(ir)   = tempArr(8)
        ATOVS.RetrvlFlag(ir)   = tempArr(9)
        ATOVS.ChComboFlag(ir)  = tempArr(10)
    ENDFOR
    readf, rlun, idx_nDailyRecs
    free_lun, rlun
END
;===============================================================
; Name:	        ReadMIRSMetaData
;
; Type:		IDL Subroutine
;
; Description:  Creates Data Structures for MIRS metadata
;               from ICDB by reading in ascii file with the data.
;
; Arguments:
;
;	    Name	   Type	    Description
;      ---------------------------------------------------
;       - MIRSfile           I       MIRS file containing metadata
;       - nProfiles          O       Number of profiles
;       - nDays              O       Number of days of data
;       - idx_nDailyRecs     0       Number of records from each day
;	- MIRS               O       Data structure for MIRS metadata
;
;
; Subroutines needed:
;       - None
;
;
; History:
;      01/08/2008   Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ReadMIRSMetaData,MIRSfile,nProfiles,nDays,idx_nDailyRecs,MIRS

    nProfiles = 0L
    openr, rlun, MIRSfile, /get_lun
    readf, rlun, nDays
    readf, rlun, nProfiles
    tempArr            = fltarr(1,6)
    MIRS={                               $
          closePar:    fltarr(nProfiles), $
          DistDiff:    fltarr(nProfiles), $
          TimeDiff:    fltarr(nProfiles), $
          AngleDiff:   fltarr(nProfiles), $
          ScanPosDiff: fltarr(nProfiles), $
          chiSq:       fltarr(nProfiles)  $
          }
    FOR ir=0L,nProfiles - 1 DO BEGIN
        readf,rlun,tempArr
        MIRS.closePar(ir)    = tempArr(0)
        MIRS.DistDiff(ir)    = tempArr(1)
        MIRS.TimeDiff(ir)    = tempArr(2)
        MIRS.AngleDiff(ir)   = tempArr(3)
        MIRS.ScanPosDiff(ir) = tempArr(4)
        MIRS.chiSq(ir)       = tempArr(5)
    ENDFOR
    readf, rlun, idx_nDailyRecs
    free_lun, rlun
END

;===============================================================
; Name:	        ReadCOSMICMetaData
;
; Type:		IDL Subroutine
;
; Description:  Creates Data Structures for COSMIC metadata
;               from ICDB by reading in ascii file with the data.
;
; Arguments:
;
;	    Name	   Type	    Description
;      ---------------------------------------------------
;       - COSMICfile         I       COSMIC file containing metadata
;       - nProfiles          O       Number of profiles
;       - nDays              O       Number of days of data
;       - idx_nDailyRecs     0       Number of records from each day
;	- COSMIC             O       Data structure for COSMIC metadata
;
;
; Subroutines needed:
;       - None
;
;
; History:
;      04/24/2008   Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ReadCOSMICMetaData,COSMICfile,nProfiles,nDays,idx_nDailyRecs,COSMIC

    nProfiles = 0L
    openr, rlun, COSMICfile, /get_lun
    readf, rlun, nDays
    readf, rlun, nProfiles
    tempArr            = fltarr(1,4)
    COSMIC={                              $
          closePar:    fltarr(nProfiles), $
          DistDiff:    fltarr(nProfiles), $
          TimeDiff:    fltarr(nProfiles), $
          QC:          fltarr(nProfiles)  $
          }
    FOR ir=0L,nProfiles - 1 DO BEGIN
        readf,rlun,tempArr
        COSMIC.closePar(ir)    = tempArr(0)
        COSMIC.DistDiff(ir)    = tempArr(1)
        COSMIC.TimeDiff(ir)    = tempArr(2)
        COSMIC.QC(ir)          = tempArr(3)
    ENDFOR
    readf, rlun, idx_nDailyRecs
    free_lun, rlun
END


PRO plotStat,stats,iSfcSelect,istat,meanPresLay,ptop,xtit,tit,scalFactSiz,xrang,yrang,thck,nAlgos,psm,linesty,col,LabelsRetr
  ind=where(stats(iSfcSelect,0,*,istat) gt -99. and meanPresLay ge ptop,ncount)
  IF (ncount ge 2) THEN BEGIN 
      plot,stats(iSfcSelect,0,ind,istat),meanPresLay(ind),xtitle=xtit,ytitle='Pressure (mb)',$
        title=tit,psym=psm(0),symsize=1.5,charsize=0.6*scalFactSiz,/ylog,xrange=xrang,yrange=yrang,$
        thick=thck,xthick=thck,ythick=thck,xstyle=1,ystyle=1
      for ialgo=1,nAlgos-1 DO BEGIN
          ind=where(stats(iSfcSelect,ialgo,*,istat) gt -99. and meanPresLay ge ptop,ncount)
          IF (ncount ge 2) THEN BEGIN 
              oplot,stats(iSfcSelect,ialgo,ind,istat),meanPresLay(ind),linestyle=linesty(ialgo),$
                psym=psm(ialgo),color=col(ialgo),symsize=1.5,thick=thck
          ENDIF
      endfor
      plot_legend,0,nAlgos,linesty,psm,col,LabelsRetr,scalFactSiz*0.7
  ENDIF
END

;===============================================================
; Name:		ReadMetaData
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads Metadata of collocation databases MDB, ICDB
;
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       10-03-2007      Sid Ahmed Boukabara, NOAA/NESDIS/ORA
;
;===============================================================
PRO ReadMetaData,metadatafile,nProfiles,RAOBlat,RAOBlon,RaobTPW,DiffDist,DiffTime,PressLowLev,$
                 PressHighLev,RetrTPW,SatZenAngle,CldLiqAmt,CldAmt,MatchFlg,RaobFlag,RaobVertFlg,$
                 RaobGapFlg,RaobInvFlg,RaobMRFlg,ClimLimFlg,SfcMissFlg,RAOByear,RAOBmonth,RAOBday,$
                 RAOBhour,RAOBminute,RaobTyp,RaobTerr,MHSspot,PrecipFlg,RetrFlg,CombAMSUHIRS,$
                 ProcessFlg,ObsQualFlg,RaobPtopWV,RaobPtopT,stationID
    openr,iu,metadatafile,/get_lun
    nProfiles=0L
    readf,iu,format='(i6)',nProfiles
    RAOBlat      = fltarr(nProfiles)
    RAOBlon      = fltarr(nProfiles)
    RaobTPW      = fltarr(nProfiles)
    DiffDist     = fltarr(nProfiles)
    DiffTime     = fltarr(nProfiles)
    PressLowLev  = fltarr(nProfiles)
    PressHighLev = fltarr(nProfiles)
    RetrTPW      = fltarr(nProfiles)
    SatZenAngle  = fltarr(nProfiles)
    CldLiqAmt    = fltarr(nProfiles)
    CldAmt       = fltarr(nProfiles)
    RaobPtopWV   = fltarr(nProfiles)
    RaobPtopT    = fltarr(nProfiles)
    MatchFlg     = intarr(nProfiles)
    RaobFlag     = intarr(nProfiles)
    RaobVertFlg  = intarr(nProfiles)
    RaobGapFlg   = intarr(nProfiles)
    RaobInvFlg   = intarr(nProfiles)
    RaobMRFlg    = intarr(nProfiles)
    ClimLimFlg   = intarr(nProfiles)
    SfcMissFlg   = intarr(nProfiles)
    RAOByear     = intarr(nProfiles)
    RAOBmonth    = intarr(nProfiles)
    RAOBday      = intarr(nProfiles)
    RAOBhour     = intarr(nProfiles)
    RAOBminute   = intarr(nProfiles)
    RaobTyp      = intarr(nProfiles)
    RaobTerr     = intarr(nProfiles)
    MHSspot      = intarr(nProfiles)
    PrecipFlg    = intarr(nProfiles)
    RetrFlg      = intarr(nProfiles)
    CombAMSUHIRS = intarr(nProfiles)
    ProcessFlg   = intarr(nProfiles)
    ObsQualFlg   = intarr(nProfiles)
    stationID    = lonarr(nProfiles)
    ;---Bias
    iprof0   = 0L
    Station0 = '123456'
    ID0      = 0L
    FOR iprof=0L,nProfiles-1 DO BEGIN
        readf,iu,format='(9i6)',iprof0,MatchFlg0,RaobFlag0,RaobVertFlg0,$
          RaobGapFlg0,RaobInvFlg0,RaobMRFlg0,ClimLimFlg0,SfcMissFlg0
        MatchFlg(iprof)     = MatchFlg0
        RaobFlag(iprof)     = RaobFlag0
        RaobVertFlg(iprof)  = RaobVertFlg0
        RaobGapFlg(iprof)   = RaobGapFlg0
        RaobInvFlg(iprof)   = RaobInvFlg0
        RaobMRFlg(iprof)    = RaobMRFlg0
        ClimLimFlg(iprof)   = ClimLimFlg0
        SfcMissFlg(iprof)   = SfcMissFlg0
        readf,iu,format='(3f9.2,5i8)', RAOBlat0,RAOBlon0,RaobTPW0,RAOByear0,$
          RAOBmonth0,RAOBday0,RAOBhour0,RAOBminute0
        RAOBlat(iprof)      = RAOBlat0
        RAOBlon(iprof)      = RAOBlon0
        RaobTPW(iprof)      = RaobTPW0
        RAOByear(iprof)     = RAOByear0
        RAOBmonth(iprof)    = RAOBmonth0
        RAOBday(iprof)      = RAOBday0
        RAOBhour(iprof)     = RAOBhour0
        RAOBminute(iprof)   = RAOBminute0
        readf,iu,format='(4f9.2,2i8)', DiffDist0,DiffTime0,PressLowLev0,$
          PressHighLev0,RaobTyp0,RaobTerr0
        DiffDist(iprof)     = DiffDist0
        DiffTime(iprof)     = DiffTime0
        PressLowLev(iprof)  = PressLowLev0
        PressHighLev(iprof) = PressHighLev0
        RaobTyp(iprof)      = RaobTyp0
        RaobTerr(iprof)     = RaobTerr0
        readf,iu,format='(6i6)', MHSspot0,PrecipFlg0,RetrFlg0,$
          CombAMSUHIRS0,ProcessFlg0,ObsQualFlg0
        MHSspot(iprof)      = MHSspot0
        PrecipFlg(iprof)    = PrecipFlg0
        RetrFlg(iprof)      = RetrFlg0
        CombAMSUHIRS(iprof) = CombAMSUHIRS0
        ProcessFlg(iprof)   = ProcessFlg0
        ObsQualFlg(iprof)   = ObsQualFlg0
        readf,iu,format='(6f9.2)', RetrTPW0,SatZenAngle0,CldLiqAmt0,CldAmt0,RaobPtopWV0,RaobPtopT0
        RetrTPW(iprof)      = RetrTPW0
        SatZenAngle(iprof)  = SatZenAngle0
        CldLiqAmt(iprof)    = CldLiqAmt0
        CldAmt(iprof)       = CldAmt0
        RaobPtopWV(iprof)   = RaobPtopWV0
        RaobPtopT(iprof)    = RaobPtopT0
        readf,iu,format='(a6,i8)', Station0,ID0
        stationID(iprof)    = ID0
    ENDFOR
END


;===============================================================
; Name:		readInstrConfig
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads instrumental configuration from a file
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- filename           I           Name of instrum config file
;	- nchan              O           Number of channels
;	- cfreq              O           Central frequencies
;	- polar              O           polarizations
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

PRO readInstrConfig,filename,nchan,cfreq,polar
  ligne=''
  openr,iu,filename,/get_lun
  readf,iu,format='(25x,i8)',nchan
  cfreq=fltarr(nchan)
  polar=intarr(nchan)
  readf,iu,format='(a)',ligne
  readf,iu,format='(*)',cfreq
  readf,iu,format='(a)',ligne
  readf,iu,format='(*)',polar
  readf,iu,format='(a)',ligne
  close,iu
  free_lun,iu,/force
END 


;===============================================================
; Name:		readErr
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads model or intrumental errors from a file
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- filename           I           Name of error file
;	- cfreq              O           Central frequencies
;	- err                O           Errors for all channels
;	- nchan              O           Number of channels
;       - computedOrNotFlag  I           Whether error was computed
;                                         or default value 
;
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

PRO readErr,filename,cfreq,err,nchan,ComputedOrNotFlag
  ligne=''
  openr,iu,filename,/get_lun
  ON_IOerror,endRead
  readf,iu,format='(25x,i8)',nchan
  cfreq=fltarr(nchan)
  ComputedOrNotFlag=fltarr(nchan)
  err=fltarr(nchan)
  readf,iu,format='(a)',ligne
  readf,iu,format='(10f10.3)',cfreq
  readf,iu,format='(a)',ligne
  readf,iu,format='(10f10.3)',err
  readf,iu,format='(a)',ligne
  readf,iu,format='(20i4)',ComputedOrNotFlag
  close,iu
  free_lun,iu,/force

  endRead:
  close,iu
  free_lun,iu,/force

END


;===============================================================
; Name:		writeErr
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes model or intrumental errors into a file
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- filename           I           Name of error file
;	- cfreq              I           Central frequencies
;	- err                I           Errors for all channels
;	- nchan              I           Number of channels
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

PRO writeErr,filename,cfreq,err,nchan
  openw,iu,filename,/get_lun
  printf,iu,format='(a25,i8)','nChan =',nChan
  printf,iu,format='(a25)','CentrFreq ='
  printf,iu,format='(10f10.3)',cfreq(0:nChan-1)
  printf,iu,format='(a25)','Error ='
  printf,iu,format='(10f10.3)',err(0:nChan-1)
  close,iu
  free_lun,iu,/force
END


;===============================================================
; Name:		ReadWT
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads the warm target values from a file
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- filename           I           Name of file containing warm targets
;	- CentrFreq          O           Central frequencies
;	- warmTargetTb       O           Array of warm targets (by orbit,chan,..)
;	- nchan              O           Number of channels
;	- nOrbits            O           Number of orbits
;	- nScanLvec          O           Number of scanlines
;	- nWarmView          O           Number of warm views available
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

PRO ReadWT,filename,CentrFreq,warmTargetTb,nchan,nOrbits,nScanLvec,nWarmView
  ligne=''  
  nChan=0L
  openr,iu,filename,/get_lun
  readf,iu,format='(25x,i8)',  nChan
  readf,iu,format='(25x,i8)',  nOrbits
  readf,iu,format='(a)',ligne
  nScanLvec=intarr(nOrbits)
  CentrFreq=intarr(nChan)
  readf,iu,format='(25x,20i8)',nScanLvec
  readf,iu,format='(25x,i8)',nWarmView
  readf,iu,format='(a)',ligne
  readf,iu,format='(10f10.3)', CentrFreq
  readf,iu,format='(a)',ligne
  mxScanL=max(nScanLvec(0:nOrbits-1))
  wt=fltarr(nWarmView)
  warmTargetTb=fltarr(mxScanL,nchan,nWarmView,nOrbits)
  FOR ichan = 0, nchan-1 DO BEGIN
      FOR k=0,nOrbits-1 DO BEGIN
          FOR i=0,nScanLvec(k)-1 DO BEGIN
              readf,iu,format='(3i6,10f10.3)', ichan0,k0,i0,wt
              warmTargetTb(i,ichan,0:nWarmView-1,k)=wt(0:nWarmView-1)
          ENDFOR
      ENDFOR
  ENDFOR
  CLOSE,iu
  free_lun,iu
END 




;===============================================================
; Name:		readlist
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads the content of an ASCII list file
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- filename          I            Name of list file
;	- list              O            List of files contained in filename
;	- nfiles            O            Numbe rof files
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

PRO readlist,filename,list,nfiles
  openr,iu,filename,/get_lun
  list=strarr(10000)
  nfiles=0
  file=''
  x=0
  while (x eq x) do begin
      ON_IOERROR,ex
      readf,iu,format='(a)',file
      list[nfiles]=file
      nfiles=nfiles+1
  ENDWHILE
ex:  
  close,iu
  free_lun,iu,/force
  if (nfiles ne 0) then list=list[0:nfiles-1]
END


;===============================================================
; Name:		readTuningFile
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads the content of an ASCII formatted Tuning File
; file.  Returns only what channels are used/not used. (ChanSel flag)
;
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- filename          I            Name of Tuning file
;       - nChan             I            Number of Channels
;       - chansel           O            channel flag
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       12-08-2009      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================

PRO readTuningFile,filename,nChan,chansel
   openr,iu,filename,/get_lun
   temp=''
   ChanSel=intarr(nChan)
   ON_IOerror,endRead
   FOR i=0,118 DO BEGIN
       readf,iu,temp
   ENDFOR
   FOR iChan=0,nChan-1 DO BEGIN
       readf,iu,temp
       str_length=strlen(temp)
       ChanSel(iChan)=fix(strmid(temp,str_length-1,1))
   ENDFOR
   endRead:
   close,iu
   free_lun,iu,/force
END   
