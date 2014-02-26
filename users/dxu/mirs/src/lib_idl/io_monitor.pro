;---------------------------------------------------------------------------------
; Summary of all subroutines related to I/O processes for the
; monitoring file (iteration by iteration).
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------




;===============================================================
; Name:		LoadMonitorFile
;
;
; Type:		IDL Subroutine
;
;
; Description:  Loads the content of the Monitoring file and puts it
;               in a structure. The file is used to minotor the 
;               iterative process of the MIRS 1dvar algorithm. See the
;               description of the structure elements for more
;               information about the content of the file.
;
;
; Arguments:
;
;      Name	     Type     Description
;      ---------------------------------------------------
;	- fileRetr    I      Name of file containing monitoring data       
;	- topId       I      Top-program ID (not used)
;       - MoniTor     I/O    Structure containing all data relatibe
;                            to iterative process.
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

PRO LoadMonitorFile,fileRetr,topId,MoniTor
  mxProf         = 8000
  mxEDRs         = 15
  mxPar          = 110
  mxIter         = 8
  MxAttempts     = 1
  mxCh           = 40
  ligne=''
  typeMonitor=size(Monitor,/type)
  IF (typeMonitor ne 8) THEN BEGIN
      MoniTor ={                                         $
      ;---General data
      nData:         0,                                  $ ;Number of measurements
      nTun:          0,                                  $ ;Number of attempts requested
      nLay:          0,                                  $ ;Number of layers
      ;---Atmosphere-related data
      nEDRs_atm:      indgen(MxAttempts),                $ ;Number of Atmosph EDRs
      EDR_label_atm:  strarr(MxAttempts,mxEDRs),         $ ;Labels of Atmosph EDRs
      EDR_nEOF_atm:   intarr(MxAttempts,mxEDRs),         $ ;#EOFs selected for Atm EDRs
      Press_Lev:      fltarr(mxPar),                     $ ;Level pressure grid
      ;---Surface-related data
      nEDRs_sfc:      indgen(MxAttempts),                $ ;Number of Surface EDRs
      EDR_label_sfc:  strarr(MxAttempts,mxEDRs),         $ ;Labels of Surface EDRs
      EDR_nEOF_sfc:   intarr(MxAttempts,mxEDRs),         $ ;#EOFs selected for Sfc EDRs
      atmClass:       intarr(mxProf),                    $ ;Profile-by-profile atmospheric classification
      sfcClass:       intarr(mxProf),                    $ ;Profile-by-profile surface classification
      ;---Channel-Selection/Convergence data
      ChanSelectFlag: indgen(MxAttempts),                $ ;Channels selection flag
      ChiSqThresh:    fltarr(MxAttempts),                $ ;Chisq Threshold used at each attempt
      ChiSqVec:       fltarr(mxProf,mxIter),             $ ;Array of ChiSq for each profile & iteration
      ;---State-vector Info
      nEDRselecVec:   intarr(mxProf),                    $ ;#EDRs selected for retrieval 4 each profile
      nIterTotVec:    intarr(mxProf),                    $ ;#Iterations used for every profile
      nAttempTotVec:  intarr(mxProf),                    $ ;#attempts used for each profile
      ParamLabelVec:  strarr(mxProf,mxEDRs),             $ ;Labels of EDRs retrieved at each profile
      ParamLengthvec: intarr(mxProf,mxEDRs),             $ ;Lengths of EDRs
      Xg:             fltarr(mxProf,mxEDRs,mxPar,mxIter),$ ;EDRs as they vary per profile, iteration
      Xb:             fltarr(mxProf,mxEDRs,mxPar),       $ ;Background used for retrieved EDRs
      TPWvec:         fltarr(mxProf,mxIter),             $ ;TPW (integrated) deducted from Xg
      TPWvec_bkg:     fltarr(mxProf),                    $ ;TPW (integrated) deducted from Xb
      RHvec:          fltarr(mxProf,mxPar,mxIter),       $ ;Relative Humidity convered from mr in Xg
      RHvec_bkg:      fltarr(mxProf,mxPar),              $ ;Relative humidity converted from mr in Xb
      SfcPress:       fltarr(mxProf),                    $ ;Surface pressure for every profile
      CLWvec:         fltarr(mxProf,mxIter),             $ ;CLW (integrated) deducted from Xg
      RWPvec:         fltarr(mxProf,mxIter),             $ ;RWP (integrated) deducted from Xg
      IWPvec:         fltarr(mxProf,mxIter),             $ ;IWP (integrated) deducted from Xg
      GWPvec:         fltarr(mxProf,mxIter),             $ ;GWP (integrated) deducted from Xg
      SWPvec:         fltarr(mxProf,mxIter),             $ ;SWP (integrated) deducted from Xg
      CLWvec_bkg:     fltarr(mxProf),                    $ ;CLW (integrated) deducted from Xb
      RWPvec_bkg:     fltarr(mxProf),                    $ ;RWP (integrated) deducted from Xb
      IWPvec_bkg:     fltarr(mxProf),                    $ ;IWP (integrated) deducted from Xb
      GWPvec_bkg:     fltarr(mxProf),                    $ ;GWP (integrated) deducted from Xb
      SWPvec_bkg:     fltarr(mxProf),                    $ ;SWP (integrated) deducted from Xb
      ;---Indices
      iTemp:          intarr(mxProf),                    $ ;Temperature index within Xg & Xb
      iWvap:          intarr(mxProf),                    $ ;Humidity index within Xg & Xb
      iOzon:          intarr(mxProf),                    $ ;Ozone index within Xg & Xb
      iClw:           intarr(mxProf),                    $ ;Cloud index within Xg & Xb
      iRain:          intarr(mxProf),                    $ ;Rain index within Xg & Xb
      iSnow:          intarr(mxProf),                    $ ;Snow index within Xg & Xb
      iIce:           intarr(mxProf),                    $ ;Ice index within Xg & Xb
      iGrpl:          intarr(mxProf),                    $ ;Graupel index within Xg & Xb
      iEmiss:         intarr(mxProf),                    $ ;Emissivity index within Xg & Xb
      iRefl:          intarr(mxProf),                    $ ;Reflectivity index within Xg & Xb
      iWindSp:        intarr(mxProf),                    $ ;Wind speed index within Xg & Xb
      iTskin:         intarr(mxProf),                    $ ;Skin temperature index within Xg & Xb
      ;---Radiance Vector monitoring:'
      nchanVec:       intarr(mxProf),                    $ ;Number channels available in each retrieval
      nchvec:         intarr(mxProf),                    $ ;Number of channels effectively used in retr
      ShrkRadVec:     intarr(mxProf,mxCh),               $ ;Indexes to used Tbs vector used in retr
      PolarVec:       intarr(mxProf,mxCh),               $ ;Polarization of channels in each retrieval
      FreqVec:        fltarr(mxProf,mxCh),               $ ;Frequency of channels in each retrieval
      Ysim:           fltarr(mxProf,mxCh,mxIter),        $ ;Array os imulated TBs in each profile & iteration
      Ym:             fltarr(mxProf,mxCh),               $ ;Measured TBs used in retrieval
      Imap:           intarr(mxProf,mxCh)                $ ;Not used
      }
  ENDIF
  OPENR,iu,fileRetr,/get_lun,error=err
  IF (err ne 0) THEN BEGIN
      res=DIALOG_MESSAGE(!ERROR_STATE.MSG,DISPLAY_NAME='ERROR',/ERROR,$
                         TITLE='UNSUCCESSFUL OPENING',DIALOG_PARENT=topId)
      RETURN
  ENDIF
  ;************************************************
  ;  HEADER
  ;************************************************
  readf,iu,format='(a)',ligne
  readf,iu,format='(25x,i8)',nData
  IF (nData gt mxProf) THEN STOP,'Error: ndata > MxProf'
  Monitor.nData=nData
  readf,iu,format='(25x,i8)',nTun
  Monitor.nTun=nTun
  readf,iu,format='(a)',ligne
  readf,iu,format='(25x,i8)',nLay
  Monitor.nLay=nLay
  readf,iu,format='(a)',ligne
  press_lev=fltarr(nLay)
  readf,iu,format='(10f10.3)',press_lev
  Monitor.press_lev(1:nLay)=press_lev(0:nLay-1)
  nedrs_atm=intarr(nTun)
  readf,iu,format='(25x,2i15)',nedrs_atm
  Monitor.nEDRs_atm[0:Monitor.nTun-1]=nedrs_atm[0:nTun-1]
  FOR i=0,nTun-1 DO BEGIN
      EDR_label_atm=strarr(nEDRs_atm(i))
      readf,iu,format='(25x,15a10)',EDR_Label_atm
      Monitor.EDR_label_atm[i,0:nEDRs_atm(i)-1] = EDR_Label_atm
      EDR_nEOF_atm=intarr(nEDRs_atm(i))
      readf,iu,format='(25x,15i10)',EDR_nEOF_atm
      Monitor.EDR_nEOF_atm[i,0:nEDRs_atm(i)-1] = EDR_nEOF_atm
  ENDFOR
  readf,iu,format='(a)',ligne
  nedrs_sfc=intarr(nTun)
  readf,iu,format='(25x,2i15)',nedrs_sfc
  Monitor.nEDRs_sfc[0:Monitor.nTun-1]=nedrs_sfc[0:nTun-1]
  FOR i=0,nTun-1 DO BEGIN
      EDR_label_sfc=strarr(nEDRs_sfc(i))
      readf,iu,format='(25x,15a10)',EDR_Label_sfc
      Monitor.EDR_label_sfc[i,0:nEDRs_sfc(i)-1] = EDR_Label_sfc
      EDR_nEOF_sfc=intarr(nEDRs_sfc(i))
      readf,iu,format='(25x,15i10)',EDR_nEOF_sfc
      Monitor.EDR_nEOF_sfc[i,0:nEDRs_sfc(i)-1] = EDR_nEOF_sfc
  ENDFOR
  readf,iu,format='(a)',ligne
  ChanSelectFlag = intarr(nTun)
  readf,iu,format='(25x,2i15)',ChanSelectFlag
  Monitor.ChanSelectFlag[0:nTun-1] = ChanSelectFlag[0:nTun-1]
  ChiSqThresh = fltarr(nTun)
  readf,iu,format='(25x,2f15.2)',ChiSqThresh
  Monitor.ChiSqThresh[0:nTun-1] = ChiSqThresh[0:nTun-1]
  readf,iu,format='(a)',ligne
  readf,iu,format='(a)',ligne
  readf,iu,format='(a)',ligne
  ;**************************************************
  ; BODY OF THE MONITORING FILE
  ;**************************************************
  MoniTor.iTemp(0:nData-1)   = -1
  MoniTor.iWvap(0:nData-1)   = -1
  MoniTor.iOzon(0:nData-1)   = -1
  MoniTor.iClw(0:nData-1)    = -1
  MoniTor.iRain(0:nData-1)   = -1
  MoniTor.iSnow(0:nData-1)   = -1
  MoniTor.iIce(0:nData-1)    = -1
  MoniTor.iGrpl(0:nData-1)   = -1
  MoniTor.iEmiss(0:nData-1)  = -1
  MoniTor.iRefl(0:nData-1)   = -1
  MoniTor.iWindSp(0:nData-1) = -1
  MoniTor.iTskin(0:nData-1)  = -1
  ON_IOERROR, bad_num
  TotNumber                  = 0
  FOR ipr=0L,nData-1 DO BEGIN
      readf,iu,format='(a)',ligne
      readf,iu,format='(25x,i10)',iprof0
      print , 'processing profile#',ipr,iprof0
      readf,iu,format='(25x,i10)',nEDRselec
      MoniTor.nEDRselecVec(ipr) = nEDRselec
      readf,iu,format='(25x,i10)',nIterTot
      MoniTor.nIterTotVec(ipr)  = nIterTot
      readf,iu,format='(25x,i10)',nAttempTot
      MoniTor.nAttempTotVec(ipr) = nAttempTot
      ParamLabel =strarr(nEDRselec)
      ParamLength=intarr(nEDRselec)
      readf,iu,format='(25x,10a10)',ParamLabel
      MoniTor.ParamLabelVec(ipr,0:nEDRselec-1) = ParamLabel(0:nEDRselec-1)
      FOR iEDR=0,nEDRselec-1 DO BEGIN
          T=(strcompress(ParamLabel(iEDR),/remove_all) eq 'EMIS')
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'TEMP')   THEN MoniTor.iTemp(ipr)   = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'WVAP')   THEN MoniTor.iWvap(ipr)   = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'OZON')   THEN MoniTor.iOzon(ipr)   = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'CLW')    THEN MoniTor.iClw(ipr)    = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'RAIN')   THEN MoniTor.iRain(ipr)   = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'SNOW')   THEN MoniTor.iSnow(ipr)   = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'ICE')    THEN MoniTor.iIce(ipr)    = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'GRPL')   THEN MoniTor.iGrpl(ipr)   = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'EMIS')   THEN MoniTor.iEmiss(ipr)  = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'REFL')   THEN MoniTor.iRefl(ipr)   = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'WINDSP') THEN MoniTor.iWindSp(ipr) = iEDR
          IF (strcompress(ParamLabel(iEDR),/remove_all) eq 'TSKIN')  THEN MoniTor.iTskin(ipr)  = iEDR
      ENDFOR
      readf,iu,format='(25x,10i10)',ParamLength
      MoniTor.ParamLengthvec(ipr,0:nEDRselec-1) = ParamLength(0:nEDRselec-1)
      readf,iu,format='(a)',ligne
      readf,iu,format='(25x,f12.4)',SfcPress
      MoniTor.SfcPress(ipr) = SfcPress
      atmclass=0L
      sfcClass=0L
      readf,iu,format='(25x,i4)',atmClass
      MoniTor.atmClass(ipr) = atmClass
      readf,iu,format='(25x,i4)',sfcClass
      MoniTor.sfcClass(ipr) = sfcClass
      readf,iu,format='(a)',ligne
      x=fltarr(nIterTot+1)
      ParamLabel0=''
      FOR iEDR=0,nEDRselec-1 DO BEGIN
          FOR iparam=0,ParamLength(iEDR)-1 DO BEGIN
              readf,iu,format='(i2,a10,i4,40f12.6)',iEDR0,ParamLabel0,iparam0,Xb0,x
              MoniTor.Xg(ipr,iEDR,iParam,0:nIterTot)=x
              Monitor.Xb(ipr,iEDR,iParam) = Xb0
          ENDFOR
      ENDFOR
      ;---Compute TPW
      FOR iter=0,nIterTot DO BEGIN
          ColumIntegr_LayW,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
            MoniTor.Xg(ipr,MoniTor.iWvap(ipr),0:ParamLength(MoniTor.iWvap(ipr))-1,iTer),water 
          Monitor.TPWVec(ipr,iTer)  = water
      END
      ColumIntegr_LayW,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
        MoniTor.Xb(ipr,MoniTor.iWvap(ipr),0:ParamLength(MoniTor.iWvap(ipr))-1),water 
      Monitor.TPWVec_bkg(ipr)  = water
      ;---Compute relative humidity
      FOR iter=0,nIterTot DO BEGIN
          FOR ilay=0,ParamLength(MoniTor.iWvap(ipr))-1 DO BEGIN
              T=MoniTor.Xg(ipr,MoniTor.iTemp(ipr),ilay,iTer)
              P=Press_lev(iLay)
              Q=MoniTor.Xg(ipr,MoniTor.iWvap(ipr),ilay,iTer)/1000.
              Monitor.RHvec(ipr,iLay,iter) = Mixingratio_to_RelHum(Q,T,P)*100.
          ENDFOR
      END
      FOR ilay=0,ParamLength(MoniTor.iWvap(ipr))-1 DO BEGIN
          T=MoniTor.Xb(ipr,MoniTor.iTemp(ipr),ilay)
          P=Press_lev(iLay)
          Q=MoniTor.Xb(ipr,MoniTor.iWvap(ipr),ilay)/1000.
          Monitor.RHvec_bkg(ipr,iLay) = Mixingratio_to_RelHum(Q,T,P)*100.
      ENDFOR
      ;---Integrate hydrometeors profiles
      ;---CLW
      IF (ParamLength(MoniTor.iclw(ipr)) gt 1) THEN BEGIN
          FOR iter=0,nIterTot DO BEGIN
              ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
                MoniTor.Xg(ipr,MoniTor.iClw(ipr),0:ParamLength(MoniTor.iclw(ipr))-1,iTer),integ
              Monitor.CLWVec(ipr,iTer)  = integ
          END
          ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
            MoniTor.Xb(ipr,MoniTor.iclw(ipr),0:ParamLength(MoniTor.iclw(ipr))-1),integ 
          Monitor.CLWVec_bkg(ipr)  = integ
      ENDIF ELSE BEGIN
          Monitor.CLWVec(ipr,iTer) = 0.
          Monitor.CLWVec_bkg(ipr)  = 0.
      ENDELSE
      ;---Rain
      IF (MoniTor.iRain(ipr) ge 0) THEN BEGIN
          FOR iter=0,nIterTot DO BEGIN
              ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
                MoniTor.Xg(ipr,MoniTor.iRain(ipr),0:ParamLength(MoniTor.iRain(ipr))-1,iTer),integ
              Monitor.RWPVec(ipr,iTer)  = integ
          END
          ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
            MoniTor.Xb(ipr,MoniTor.irain(ipr),0:ParamLength(MoniTor.irain(ipr))-1),integ 
          Monitor.RWPVec_bkg(ipr)  = integ
      ENDIF
      ;---Snow
      IF (MoniTor.iSnow(ipr) ge 0) THEN BEGIN
          FOR iter=0,nIterTot DO BEGIN
              ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
                MoniTor.Xg(ipr,MoniTor.iSnow(ipr),0:ParamLength(MoniTor.iSnow(ipr))-1,iTer),integ
              Monitor.SWPVec(ipr,iTer)  = integ
          END
          ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
            MoniTor.Xb(ipr,MoniTor.isnow(ipr),0:ParamLength(MoniTor.isnow(ipr))-1),integ 
          Monitor.SWPVec_bkg(ipr)  = integ
      ENDIF
      ;---Ice
      IF (MoniTor.iIce(ipr) ge 0) THEN BEGIN
          FOR iter=0,nIterTot DO BEGIN
              ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
                MoniTor.Xg(ipr,MoniTor.iIce(ipr),0:ParamLength(MoniTor.iIce(ipr))-1,iTer),integ
              Monitor.IWPVec(ipr,iTer)  = integ
          END
          ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
            MoniTor.Xb(ipr,MoniTor.iIce(ipr),0:ParamLength(MoniTor.iIce(ipr))-1),integ 
          Monitor.IWPVec_bkg(ipr)  = integ
      ENDIF
      ;---Graupel
      IF (MoniTor.iGrpl(ipr) ge 0) THEN BEGIN
          FOR iter=0,nIterTot DO BEGIN
              ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
                MoniTor.Xg(ipr,MoniTor.iGrpl(ipr),0:ParamLength(MoniTor.iGrpl(ipr))-1,iTer),integ
              Monitor.GWPVec(ipr,iTer)  = integ
          END
          ColumIntegr,nLay,Monitor.Press_lev(0:nLay-1),SfcPress,$
            MoniTor.Xb(ipr,MoniTor.iGrpl(ipr),0:ParamLength(MoniTor.iGrpl(ipr))-1),integ 
          Monitor.GWPVec_bkg(ipr)  = integ
      ENDIF
      ;----------
      readf,iu,format='(a)',ligne
      readf,iu,format='(a)',ligne
      readf,iu,format='(25x,i10)',nchan
      MoniTor.nchanVec(ipr) = nchan
      readf,iu,format='(25x,i10)',nch
      MoniTor.nchvec(ipr) = nch
      ShrkRad=intarr(nch)
      readf,iu,format='(25x,100i3)',ShrkRad
      MoniTor.ShrkRadVec(ipr,0:nch-1) = ShrkRad(0:nch-1)
      x=fltarr(nIterTot+1)
      FOR ich=0,nch-1 DO BEGIN
          readf,iu,format='(3i4,f12.4,40f8.2)',ich0,imap0,pol0,freq0,x,ym0
          MoniTor.PolarVec(ipr,ich) = pol0
          MoniTor.FreqVec(ipr,ich)  = freq0
          MoniTor.Ysim(ipr,ich,0:nIterTot) = x(0:nIterTot)
          MoniTor.Ym(ipr,ich) = Ym0
          Monitor.Imap(ipr,ich)=imap0
      ENDFOR
      readf,iu,format='(25x,40f12.3)', x
      Monitor.ChiSqVec(ipr,0:nIterTot)=x
      TotNumber = TotNumber+1
  ENDFOR
  bad_num: print, 'Number of profiles:',TotNumber
  Monitor.nData = TotNumber
END


