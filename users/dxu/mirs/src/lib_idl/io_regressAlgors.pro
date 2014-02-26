;---------------------------------------------------------------------------------
; Summary of all subroutines related to I/O processes for the
; different types algorithm coefficients files, etc.
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------



;===============================================================
; Name:		readRegressAlgor
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads the regression coefficients file.
;
;
; Arguments:
;
;	    Name     Type	    Description
;      ---------------------------------------------------
;	- AlgoFile    I      Name file to be read
;	- sfcTyp      I/O    Surface type vector (2 be filled)
;	- Algor       I/O    Algorithms coeffs array (2 be filled)
;	- Labels      I/O    Labels of EDRs to be read (2 b filled)
;	- nElemts     I/O    # elements representing each EDR
;	- nIndepVar   I/O    # Independent variables used 4 each EDR
;	- nEDRs       O      # EDRs for which we have regress coeffs
;	- TbIndxV     I/O    Indexes of TB to be used in regr alg. for each EDR
;	- ialgo       I      Index of algo to be filled (which type)      
;	- nLay        O      Number of layers of profiles
;	- nLev        O      Number of levels of profiles
;	- nchan       O      Number of channels of emissivity/reflec.
;	- cfreq       O      Central frequencies of channels
;	- pol         O      Polarizations of channels
;	- presLay     O      Layer based pressure grid
;	- presLev     O      Level based pressure grid
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

PRO readRegressAlgor,AlgoFile,sfcTyp,Algor,Labels,nElemts,nIndepVar,iEDR,TbIndxV,FormIndxV,ialgo,$
                     nLay,nLev,nchan,nAngBins,bins,cfreq,pol,presLay,presLev,err
  ligne=''
  label=''
  err=0
  openr,iu,AlgoFile,/get_lun,error=err
  IF (err ne 0) THEN BEGIN
      print ,!ERROR_STATE.MSG
      RETURN
  ENDIF
  ;---Header
  readf,iu,format='(60x,i4)',nLay
  readf,iu,format='(60x,i4)',nLev
  readf,iu,format='(60x,i4)',nchan
  readf,iu,format='(60x,i4)',nAngBins
  readf,iu,format='(60x,20i4)',bins
  cfreq   = fltarr(nchan)
  pol     = intarr(nchan)
  presLay = fltarr(nLay)
  presLev = fltarr(nLev)
  readf,iu,format='(60x,100f12.4)',cfreq
  readf,iu,format='(60x,100i3)',pol
  readf,iu,format='(60x,120f9.3)',presLay
  readf,iu,format='(60x,120f9.3)',presLev
  ;---Body
  FOR iAng=0,nAngBins-1 DO BEGIN
      readf,iu,format='(a)',ligne
      readf,iu,format='(60x,a30)',label
      Labels(iEDR)=label
      readf,iu,format='(60x,i4)',sfcT
      sfcTyp(ialgo)=sfcT
      readf,iu,format='(60x,i4)',binnum
      readf,iu,format='(60x,i4)',nEl
      nElemts(iEDR)=nEl
      readf,iu,format='(60x,i4)',nInd
      nIndepVar(ialgo,iEDR)=nInd
      readf,iu,format='(a)',ligne
      TBindx=intarr(nchan)
      readf,iu,format='(60x,100i3)',TBindx
      FormIndx=intarr(nchan)
      readf,iu,format='(60x,100i3)',FormIndx
      TbIndxV(ialgo,iEDR,0:nchan-1) = TBindx(0:nchan-1)
      FormIndxV(ialgo,iEDR,0:nchan-1) = FormIndx(0:nchan-1)
      stat=intarr(nElemts(iEDR))
      readf,iu,format='(60x,200i4)',stat
      mCorr=fltarr(nElemts(iEDR))
      readf,iu,format='(60x,200f12.4)',mCorr  
      reg=fltarr(nIndepVar(ialgo,iEDR)+1)
      FOR ivar=0,nElemts(iEDR)-1 DO BEGIN
          readf,iu,format='(20x,i4)',ivar0
          readf,iu,format='(12x,30f12.5)',reg
          readf,iu,format='(a)',ligne
          Algor(ialgo,iEDR,ivar,0:nIndepVar(ialgo,iEDR),iAng)=reg
      ENDFOR
  ENDFOR
  close,iu
  free_lun,iu
END


;===============================================================
; Name:		writeHdrRegrAlgors
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes just the header of the regression 
;               coefficients file.
;
;
; Arguments:
;
;      Name	   Type	    Description
;      ---------------------------------------------------
;	- iu         O     Unit number
;	- file       I     File name that will contain the regress coeffs.
;	- nLay       I     Number of layers of profiles
;	- nLev       I     Number of levels of profiles
;	- nchan      I     Number of channels
;	- cfreq      I     Central frequencies
;	- pol        I     Polarizations of channels
;	- presLay    I     Layer based pressure grid
;	- presLev    I     Level based pressure grid
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

PRO writeHdrRegrAlgors,iu,file,edr,nLay,nLev,nchan,nAngBins,bins,cfreq,pol,presLay,presLev
   openw,iu,file+'_'+edr+'.dat',/get_lun
   printf,iu,format='(a60,i4)','NLay:',nLay
   printf,iu,format='(a60,i4)','NLev:',nLev
   printf,iu,format='(a60,i4)','NChan:',nchan
   printf,iu,format='(a60,i4)','NAngleBins:',nAngBins
   printf,iu,format='(a60,20i4)','Angle Bin Range:',bins
   printf,iu,format='(a60,100f12.4)','CFREQ:',cfreq(0:nchan-1)
   printf,iu,format='(a60,100i3)','Polarization:',pol(0:nchan-1)
   printf,iu,format='(a60,120f9.3)','Layer-based press grid:',presLay(0:nLay-1)
   printf,iu,format='(a60,120f9.3)','Level-based press grid:',presLev(0:nLev-1)
END

;===============================================================
; Name:		writeHdrRegrAlgors
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes just the header of the regression 
;               coefficients file.
;
;
; Arguments:
;
;      Name	   Type	    Description
;      ---------------------------------------------------
;	- iu         O     Unit number
;	- file       I     File name that will contain the regress coeffs.
;	- nLay       I     Number of layers of profiles
;	- nLev       I     Number of levels of profiles
;	- presLay    I     Layer based pressure grid
;	- presLev    I     Level based pressure grid
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       08-28-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================

PRO writeHdrRegrAlgors4RR,iu,file,nLay,nLev,presLay,presLev
   openw,iu,file,/get_lun
   printf,iu,format='(a60,i4)','NLay:',nLay
   printf,iu,format='(a60,i4)','NLev:',nLev
   printf,iu,format='(a60,120f9.3)','Layer-based press grid:',presLay(0:nLay-1)
   printf,iu,format='(a60,120f9.3)','Level-based press grid:',presLev(0:nLev-1)
END

;===============================================================
; Name:		writeRegrAlgors
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes the regression coefficients and associated
;               parameters in to a file. This is for a particular
;               surface type.
;
;
; Arguments:
;
;      Name	       Type	    Description
;      ---------------------------------------------------
;	- iu            I          Unit number
;	- label         I          Label of the type of the algorithm
;	- TBindx2Use    I          Number of TBs used for this regression. 
;	- nElmts        I          Number of elements representing EDR
;	- reg           I          Regression coefficients (nElmts x (TBindx2Use+2))
;	- const         I          Offsets of the regression coeffs (nElmts)
;	- Corr          I          Correlation coefficients
;	- mCorr         I          Multi-linear regression performance-correlation
;	- stat          I          Regression stability index
;	- iSfcSelect    I          Surface backgorund index
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

PRO writeRegrAlgors,iu,label,TBindx2Use,FormTBindxArr,nElmts,nInd,reg,const,Corr,mCorr,stat,iSfcSelect,iAngBin
   printf,iu,format='(a60)','----------------------------------------------------'
   printf,iu,format='(a60,a30)','Regression-based Algorithm for:',Label
   printf,iu,format='(a60,i4)','Surface Background [0:ocean,1:sea-ice, 2:Land, 3:Snow]:',iSfcSelect
   printf,iu,format='(a60,i4)','AngleBin Center:',iAngBin
   printf,iu,format='(a60,i4)','Number of element(s) for this EDR:',nElmts
   printf,iu,format='(a60,i4)','Number of Independent variables:',nInd
   printf,iu,format='(a60,a30)','Independent variables:',' TBs + abs(angle) + CLW + Latitude'
   printf,iu,format='(a60,100i3)','Channels Indexes Used:',TBindx2Use+1
   printf,iu,format='(a60,100i3)','TB Form Indexes Used:',FormTBindxArr
   printf,iu,format='(a60,200i4)','Regression stability [0:success, 1:error, 2:small pivot]:',stat
   printf,iu,format='(a60,200f12.4)','Multilinear-Regreesion Perf. (correlation):',mCorr
   FOR ivar=0,nElmts-1 DO BEGIN
       printf,iu,format='(a20,i4)','Element#',ivar+1
       printf,iu,format='(a12,30f12.5)','a0,1,2...n:',const(ivar),reg(ivar,*)
       printf,iu,format='(a12,30f12.5)','corr.:',corr(ivar,*)
   ENDFOR
END

;===============================================================
; Name:		writeRegrAlgors4RR
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes the regression coefficients and associated
;               parameters used to calculate the Rain Rate
;
;
; Arguments:
;
;      Name	       Type	    Description
;      ---------------------------------------------------
;	- iu            I          Unit number
;	- label         I          Label of the type of the algorithm
;	- TBindx2Use    I          Number of TBs used for this regression. 
;	- nElmts        I          Number of elements representing EDR
;	- reg           I          Regression coefficients (nElmts x (TBindx2Use+2))
;	- const         I          Offsets of the regression coeffs (nElmts)
;	- Corr          I          Correlation coefficients
;	- mCorr         I          Multi-linear regression performance-correlation
;	- stat          I          Regression stability index
;	- iSfcSelect    I          Surface backgorund index
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       08-28-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/STAR
;
;===============================================================

PRO writeRegrAlgors4RR,iu,label,GeophParamIdx,FormGeophParam,nElmts,reg,const,Corr,mCorr,stat,iSfcSelect
   printf,iu,format='(a60)','----------------------------------------------------'
   printf,iu,format='(a60,a30)','Regression-based Algorithm for:',Label
   printf,iu,format='(a60,i4)','Surface Background [0:ocean,1:sea-ice, 2:Land, 3:Snow]:',iSfcSelect
   printf,iu,format='(a60,i4)','Number of element(s) for this EDR:',nElmts
   printf,iu,format='(a60,i4)','Number of Independent variables:',n_elements(reg(0,*))
   printf,iu,format='(a60,a30)','Independent variables:',' T + Q + CLW + GRPL'
   printf,iu,format='(a60,100i3)','Parameters Used:',GeophParamIdx+1
   printf,iu,format='(a60,100i3)','Form (Natural/Logarithmic) Used:',FormGeophParam
   printf,iu,format='(a60,200i4)','Regression stability [0:success, 1:error, 2:small pivot]:',stat
   printf,iu,format='(a60,200f12.4)','Multilinear-Regreesion Perf. (correlation):',mCorr
   FOR ivar=0,nElmts-1 DO BEGIN
       printf,iu,format='(a20,i4)','Element#',ivar+1
       printf,iu,format='(a12,30f12.5)','a0,1,2...n:',const(ivar),reg(ivar,*)
       printf,iu,format='(a12,30f12.5)','corr.:',corr(ivar,*)
   ENDFOR
END
