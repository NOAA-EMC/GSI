;$Id: io_covBkg.pro 511 2007-10-02 13:44:20Z sidb $
;---------------------------------------------------------------------------------
; Summary of all subroutines related to I/O processes for the
; different covariance/background files.
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------
;===============================================================
; Name:		writeOutHdrCovMatrx
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes out the covariance matrix header.
;
;
; Arguments:
;
;	    Name        Type	    Description
;      ---------------------------------------------------
;	- iAtmOrSfc      I          Type of covariance matrx (Sfc or Atm)            
;	- file_Out       I          Name of the output file 
;	- iout           O          Unit umber obtained at opening
;	- np             I          Number of params to store in cov matrx
;	- iParam         I          Parameters IDs
;	- DescParam      I          Parameters Labels
;	- nTyp           I          Number of classes to be stored in same file.
;	- iTyp           I          Classes IDs
;	- DescTyp        I          Classes string Labels
;	- pres_lay       I          Layer based pressure grid (if Atm Type)
;	- pres_lev       I          Level based pressure grid (if Atm Type)
;	- cfreq          I          Central frequencies (if Sfc Type)
;	- polar          I          Polarizations (if Sfc Type)
;	- iMode          I          Mode of storage of params (lin,log, etc)
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

PRO writeOutHdrCovMatrx,iAtmOrSfc,file_Out,iout,np,iParam,DescParam,nTyp,iTyp,DescTyp,$
                pres_lay,pres_lev,cfreq,polar,iMode
  openw, iOut, file_out,/get_lun
  IF (iAtmOrSfc eq 0) then printf, iOut, format='(a)','Atmospheric Covar/Bkg/Transf Matrices'
  IF (iAtmOrSfc eq 1) then printf, iOut, format='(a)','Surface Covar/Bkg/Transf Matrices'
  printf, iOut, format='(a)',             '--------------------------------------'
  ;----Parameters contained in the file
  printf, iOut, format='(a25,i8)',        'Number Parameters: ', fix(np)
  printf, iOut, format='(a25,10i8)',      'Parameters IDs   : ', fix(iParam(0:np-1))
  printf, iOut, format='(a25,10a8)',      'Parameters Descr : ', DescParam(0:np-1)
  printf, iOut, format='(a25,10i8)',      'Space (0:G, 1:Log) : ', iMode(0:np-1)
  ;----Classes contained in the file
  printf, iOut, format='(a25,i8)',        'Number Classes   : ', fix(nTyp)
  printf, iOut, format='(a25,10i8)',      'Classes IDs      : ', fix(iTyp(0:nTyp-1))
  printf, iOut, format='(a25,10a20)',     'Classes Descrip  : ', DescTyp(0:nTyp-1)
  ;----Pressure levels/layers
  IF (iAtmOrSfc eq 0) then begin
      printf, iOut, format='(a25,i8)',    'Number of Levels: ',  n_elements(reform(pres_lev))
      printf, iOut, format='(a25)',       'Level pressures :'
      printf, iOut, format='(10f10.3)',                          pres_lev
      printf, iOut, format='(a25,i8)',    'Number of Layers: ',  n_elements(reform(pres_lay))
      printf, iOut, format='(a25)',       'Layer pressures :'
      printf, iOut, format='(10f10.3)',                          pres_lay
  ENDIF
  ;----Frequencies/Polarities
  IF (iAtmOrSfc eq 1) then begin
      printf, iOut, format='(a25,i8)',    'Number of channels: ',n_elements(cfreq)
      printf, iOut, format='(a25)',       'Frequencies :'
      printf, iOut, format='(10f10.3)',                          cfreq
      printf, iOut, format='(a25)',       'Polarizations :'
      printf, iOut, format='(30i3)',                             polar 
  ENDIF
END

;===============================================================
; Name:		ReadHdrCovMatrx
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads in the covariance matrix header.
;
;
; Arguments:
;
;	    Name       Type	    Description
;      ---------------------------------------------------
;	- iAtmOrSfc     I          Type of covariance matrx (Sfc or Atm)  
;	- file_in       I          Name of file containing the cov matrx
;	- iIn           I          Unit number of already-opened file
;	- np            O          Number of params stored in cov matrx
;	- iParam        O          Parameters IDs
;	- DescParam     O          Parameters Labels
;	- nTyp          O          Number of classes stored in same file.
;	- iTyp          O          Classes IDs
;	- DescTyp       O          Classes string Labels
;	- pres_lay      O          Layer based pressure grid (if Atm Type)
;	- pres_lev      O          Level based pressure grid (if Atm Type)
;	- cfreq         O          Central frequencies (if Sfc Type)
;	- polar         O          Polarizations (if Sfc Type)
;	- iMode         O          Mode of storage of params (lin,log, etc)
;	- nLev          O          Number of levels (if Atm Type)
;	- nLay          O          Number of layers (if Atm Type)
;	- nFreq         O          Number of channels (frequencies) (if Sfc Type)
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

PRO ReadHdrCovMatrx,iAtmOrSfc,file_in,iIn,np,iParam,DescParam,nTyp,iTyp,DescTyp,$
                pres_lay,pres_lev,cfreq,polar,iMode,nLev,nLay,nFreq
  openr, iIn, file_in,/get_lun
  ligne=''
  readf, iIn, format='(a)',ligne
  readf, iIn, format='(a)',ligne
  ;----Parameters contained in the file
  readf, iIn, format='(25x,i8)',        np
  iParam    = intarr(np)
  iMode     = intarr(np)
  DescParam = strarr(np)
  readf, iIn, format='(25x,10i8)',      iParam
  readf, iIn, format='(25x,10a8)',      DescParam
  readf, iIn, format='(25x,10i8)',      iMode
  readf, iIn, format='(25x,i8)',        nTyp
  iTyp      = intarr(nTyp)
  DescTyp   = strarr(nTyp)
  readf, iIn, format='(25x,10i8)',      iTyp
  readf, iIn, format='(25x,10a20)',     DescTyp
  ;----Pressure levels/layers
  IF (iAtmOrSfc eq 0) then begin
      readf, iIn, format='(25x,i8)',    nLev
      pres_lev = fltarr(nLev)
      readf, iIn, format='(a25)',       ligne
      readf, iIn, format='(10f10.3)',   pres_lev
      readf, iIn, format='(25x,i8)',    nLay
      readf, iIn, format='(a25)',       ligne
      pres_lay = fltarr(nLay)
      readf, iIn, format='(10f10.3)',   pres_lay
  ENDIF
  ;----Frequencies/Polarities
  IF (iAtmOrSfc eq 1) then begin
      readf, iIn, format='(25x,i8)',    nFreq
      readf, iIn, format='(a25)',       ligne
      cfreq  = fltarr(nFreq)
      polar  = intarr(nFreq)
      readf, iIn, format='(10f10.3)',   cfreq
      readf, iIn, format='(a25)',       ligne
      readf, iIn, format='(30i3)',      polar 
  ENDIF
END


;===============================================================
; Name:		writeOutInPiecesCovMatrx
;
;
; Type:		IDL Subroutine
;
;
; Description:  Writes out the individual pieces of the 
;               covariance matrix, corresponding to individual EDRs.
;
;
; Arguments:
;
;      Name	      Type	Description
;      ---------------------------------------------------
;	- iout          I       Unit number
;	- np            I       Number of EOFs (of particular EDR)
;	- DescrPar      I       Descriptin label of the EDR
;	- IndxPar       I       Index/ID of the EDR
;	- Sa            I       Covar Matrx of the EDR  (np x np)
;	- U             I       Transf Matrx of the EDR (np x np)
;	- Xb            I       Background for the EDR  (np)
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

PRO writeOutInPiecesCovMatrx,iout,np,DescrPar,IndxPar,Sa,U,Xb
  printf, iOut, format='(a)',         '-----------------------------------------'      
  ;---write out general info about EDR
  printf, iOut, format='(a25,i8)',   'EDR ID#',IndxPar          
  printf, iOut, format='(a25,a10)',  'EDR Label:',DescrPar
  printf, iOut, format='(a25,i8)',   'Number EOFs(EDR):',np
  ;---write out covariance
  printf, iOut, format='(a)',             'Covariance:'
  FOR ip=0,np-1 DO BEGIN
      printf, iOut, format='(10f10.4)',  Sa(ip,0:np-1)
  ENDFOR
  printf, iOut, format='(a)',         ''      
  ;---write out transformation matrix
  printf, iOut, format='(a)',             'Transf. Matrix:'
  FOR ip=0,np-1 DO BEGIN
      printf, iOut, format='(10f10.4)',  U(ip,0:np-1)
  ENDFOR
  printf, iOut, format='(a)',         ''      
  ;---write out background vector
  printf, iOut, format='(a)',             'Background:'
  printf, iOut, format='(10f10.4)',  Xb(0:np-1)
END



;===============================================================
; Name:		ReadInPiecesCovMatrx
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads in the individual pieces of the 
;               covariance matrix, corresponding to individual EDRs.
;
;
; Arguments:
;
;      Name	      Type	    Description
;      ---------------------------------------------------
;	- iIn           O       Unit number
;	- np            O       Number of EOFs (of particular EDR)
;	- DescrPar      O       Descriptin label of the EDR
;	- IndxPar       O       Index/ID of the EDR
;	- Sa            O       Covar Matrx of the EDR  (np x np)
;	- U             O       Transf Matrx of the EDR (np x np)
;	- Xb            O       Background for the EDR  (np)
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

PRO ReadInPiecesCovMatrx,iIn,np,DescrPar,IndxPar,Sa,U,Xb
  ligne    = ''
  DescrPar = ''
  readf, iIn, format='(a)',   ligne 
  ;---read-in general info about EDR
  readf, iIn, format='(25x,i8)',   IndxPar          
  readf, iIn, format='(25x,a10)',  DescrPar
  readf, iIn, format='(25x,i8)',   np
  ;---read in covariance
  Sa  = fltarr(np,np)
  X   = fltarr(np)
  readf, iIn, format='(a)',   ligne
  FOR ip=0,np-1 DO BEGIN
      readf, iIn, format='(10f10.4)',X  
      Sa(ip,0:np-1)=X(0:np-1)
  ENDFOR
  readf, iIn, format='(a)',   ligne   
  ;---read in transformation matrix
  U  = fltarr(np,np)
  readf, iIn, format='(a)',   ligne
  FOR ip=0,np-1 DO BEGIN
      readf, iIn, format='(10f10.4)',  X
      U(ip,0:np-1)=X(0:np-1)
  ENDFOR
  readf, iIn, format='(a)',   ligne   
  ;---read in background vector
  Xb=fltarr(np)
  readf, iIn, format='(a)',   ligne
  readf, iIn, format='(10f10.4)',  Xb
END


;===============================================================
; Name:		ReadCovBkgFile
;
;
; Type:		IDL Subroutine
;
;
; Description:  Reads an ASCII-written cov matrx (different format)
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- Filename          I             File name
;	- nPar              O             Number of parameters
;	- M                 O             Mean background (nPar)
;	- C                 O             Cov matrx (nPar x nPar)
;	- ityp              O             Surface Type
;	- TypDesc           O             Description label of type
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

PRO ReadCovBkgFile,Filename,nPar,M,C,ityp,TypDesc
  ligne=''
  openr,iu,strcompress(Filename,/remove_all),/get_lun
  readf,iu,format='(6x,i6)',nPar
  readf,iu,format='(a)',ligne
  M=fltarr(nPar)
  C0=fltarr(nPar)
  C=fltarr(nPar,nPar)
  iTyp=0L
  readf,iu,format='(i4,1x,24(f7.3,1x))',ityp,M
  readf,iu,format='(a)',ligne
  FOR i=0,nPar-1 DO BEGIN
      readf,iu,format='(24(f8.5,1x))',C0
      C(i,0:nPar-1)=C0(0:nPar-1)
  ENDFOR
  IF (iTyp eq 0) THEN TypDesc='oc'
  IF (iTyp eq 1) THEN TypDesc='seaice'
  IF (iTyp eq 2) THEN TypDesc='ld'
  IF (iTyp eq 3) THEN TypDesc='snow'
  IF (iTyp eq 4) THEN TypDesc='desert'
  close,iu
  free_lun,iu
END

