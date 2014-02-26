;$Id: algors.pro 3289 2013-05-15 19:22:20Z kgarrett $
;---------------------------------------------------------------------------------
; Summary of all subroutines related to regression algorithms
;
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------

;===============================================================
; Name:		ApplyRegr
;
;
; Type:		IDL Subroutine
;
;
; Description:  Applies the regression coefficients to the 
;               dependent variables and produces the EDRs
;
;
; Arguments:
;
;	    Name           Type	    Description
;      ---------------------------------------------------
;	- nprof              I     Number of profiles (not used)
;	- nchan              I     Number of channels (not used)
;	- nAlgors            I     Number of algorithms (not used)
;	- tb                 I     Brightness temperatures array
;	- angle              I     Viewing angle
;	- lat                I     Latitude of the measurements
;	- Algors             I     Algorithms coefficients array
;	- nElts              I     # elements representing each EDR
;	- nIndepVar          I     # Indep variables used for each EDR
;	- TbIndxArr          I     Index array of which TB 2 use 4 EDRs
;	- EDR                O     EDRs to be inverted from measurements
;	- sfcTyp             I     Surface type (used to select from nIndepVar)
;	- iEDR               I     Index of the particular EDR to invert
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

PRO ApplyRegr,nprof,sensor_id,nchan,nAlgors,tb,angle,AngleBin,lat,clw,Algors,nElts,$
              nIndepVar,TbIndxArr,FormTbIndxArr,EDR,sfcTyp,iEDR,ExistAlgs

  nElemts=nElts(iEDR)
  EDR=fltarr(nElemts)
  IF (ExistAlgs(sfcTyp,iEDR) eq 1) THEN BEGIN ;---algorithm exists
      nInd=nIndepVar(sfcTyp,iEDR)
      TbIndx=reform(TbIndxArr(sfcTyp,iEDR,0:nchan-1),nchan)
      FormIndx=reform(FormTbIndxArr(sfcTyp,iEDR,0:nchan-1),nchan)
      FOR ivar=0,nElemts-1 DO BEGIN
          a0=Algors(sfcTyp,iEDR,ivar,0,AngleBin)
          EDR(ivar)=a0
          IF (EDR(ivar) eq -99.00) THEN BEGIN
              EDR(ivar)=-999.
          ENDIF ELSE BEGIN
              ;---Loop over spectral indep. vars 
              FOR iInd=0,nchan-1 DO BEGIN
                  IF (FormIndx(TbIndx(iInd)-1) eq 0) THEN BEGIN
                      EDR(ivar)=EDR(ivar)+tb(TbIndx(iInd)-1)*Algors(sfcTyp,iEDR,ivar,1+iInd,AngleBin)
                  ENDIF
                  IF (FormIndx(TbIndx(iInd)-1) eq 1 and tb(TbIndx(iInd)-1) le 299.999) THEN BEGIN
                      EDR(ivar)=EDR(ivar)+alog(300.-tb(TbIndx(iInd)-1))*Algors(sfcTyp,iEDR,ivar,1+iInd,AngleBin)
                  ENDIF
                  IF (FormIndx(TbIndx(iInd)-1) eq 1 and tb(TbIndx(iInd)-1) gt 299.999) THEN BEGIN
                      EDR(ivar)=-999.
                  ENDIF
              ENDFOR
              ;---weight by non-spectral indep. vars (angle,lat)
              IF (EDR(ivar) gt -999.) THEN BEGIN
                  ;---IF retrieving CLW or EDR trained not using CLW predictor
                  IF (clw lt 0) THEN BEGIN
                      ;---Angle predictor only used for crosstrack sensors
                      IF (Sensor_ID lt 3 or Sensor_ID eq 4 or Sensor_ID eq 6 $
                          or Sensor_ID eq 13 or Sensor_ID eq 14) THEN        $
                        EDR(ivar)=EDR(ivar)+abs(angle)*Algors(sfcTyp,iEDR,ivar,nInd-1,AngleBin)
                      ;---All sensors use latitude
                      EDR(ivar)=EDR(ivar)+lat*Algors(sfcTyp,iEDR,ivar,nInd,AngleBin)
                  ENDIF
                  ;---IF retrieving all other EDRs with CLW as predictor
                  IF (clw ge 0) THEN BEGIN
                      ;---Angle predictor only used for crosstrack sensors
                      IF (Sensor_ID lt 3 or Sensor_ID eq 4 or Sensor_ID eq 6 $
                          or Sensor_ID eq 13 or Sensor_ID eq 14) THEN        $
                        EDR(ivar)=EDR(ivar)+abs(angle)*Algors(sfcTyp,iEDR,ivar,nInd-2,AngleBin)
                      ;---All sensors use CLW and latitude
                      EDR(ivar)=EDR(ivar)+clw*Algors(sfcTyp,iEDR,ivar,nInd-1,AngleBin)
                      EDR(ivar)=EDR(ivar)+lat*Algors(sfcTyp,iEDR,ivar,nInd,AngleBin)
                  ENDIF

              ENDIF
          ENDELSE
      ENDFOR
  ENDIF
  IF (ExistAlgs(sfcTyp,iEDR) eq 0) THEN BEGIN ;---algorithm does not exist
      FOR ivar=0,nElemts-1 DO BEGIN
          EDR(ivar)=-999.
      ENDFOR
  ENDIF
  
END



;===============================================================
; Name:		generRegr
;
;
; Type:		IDL Subroutine
;
;
; Description:  Generates the regression coefficients for an EDR.
;               Note that the dependent variables are a set of 
;               brightnes measurements as well as the latitude and 
;               the viewing angle.
;
;
; Arguments:
;
;      Name		   Type	    Description
;      ---------------------------------------------------
;	- TBindx2Use        I      Index array of TBs to be used for the EDR
;	- ind               I      Filter index
;	- ncount            I      Number of profiles of the dataset
;	- tbm               I      Measurements to use for the regression
;	- angle             I      Viewing angle
;	- lat               I      Latitude of measurements
;	- depVar            I      Variable vector to be regressed 
;	- reg               O      Regression coefficients
;	- const             O      Offset used in regression algorithm
;	- Corr              O      Correlation Factors obtained
;	- mCorr             O      mCorrelation
;	- stat              O      Status flag of the regression process
;	- yfit              O      The regressed variables as estimated by fit
;	- sigm              O      Sigma of the obtained coefficients
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

PRO generRegr,TBindx2Use,FormTBindxArr,sensor_id,ind,ncount,tbm,angle,lat,clw,depVar,reg,const,Corr,mCorr,stat,yfit,sigm

   nTerms = n_elements(reg)
   X      = fltarr(nTerms,ncount)

   FOR iterm=0,n_elements(TBindx2Use)-1 DO BEGIN
       IF (FormTBindxArr(iterm) eq 0) THEN BEGIN
           X[iterm,0:ncount-1] = tbm(ind,TBindx2Use(iterm))
       ENDIF
       IF (FormTBindxArr(iterm) eq 1) THEN BEGIN
           X[iterm,0:ncount-1] = alog(300.-tbm(ind,TBindx2Use(iterm)))
       ENDIF
   ENDFOR
   iCLW = where(clw(ind) ge 0,cnt)
   IF (cnt gt 0) THEN BEGIN
       IF (Sensor_ID lt 3 or Sensor_ID eq 4 or Sensor_ID eq 6 or Sensor_ID eq 13 or Sensor_ID eq 14) THEN X[nTerms-3,0:ncount-1] = angle(ind)
       X[nTerms-2,0:ncount-1] = clw(ind)
       X[nTerms-1,0:ncount-1] = lat(ind)
   ENDIF
   IF (cnt eq 0) THEN BEGIN
       IF (Sensor_ID lt 3 or Sensor_ID eq 4 or Sensor_ID eq 6 or Sensor_ID eq 13 or Sensor_ID eq 14) THEN X[nTerms-2,0:ncount-1] = angle(ind)
       X[nTerms-1,0:ncount-1] = lat(ind)
   ENDIF 

   Y                      = depVar(ind)
   reg=regress(X,Y,const=const,Correlation=Corr,/double,mcorrelation=mCorr,status=stat,Yfit=yfit,sigma=sigm)
END

;===============================================================
; Name:		generRegr4Bias
;
;
; Type:		IDL Subroutine
;
;
; Description:  Generates the regression coefficients for bias
;               computation for TB correction application.
;               Bias training set can be any array of geophysical
;               parameters of which the bias is sensitive to.
; 
;
;
; Arguments:
;
;      Name		   Type	    Description
;      ---------------------------------------------------
;	- GeophParamIndx    I      Index array of Geophysical Paramter
;                                  to be used to comput bias
;       - ind               I      index of profiles to use
;	- ncount            I      Number of profiles of the dataset
;	- indepVars         I      Independent variables
;	- depVar            I      Variable vector to be regressed (bias)
;	- reg               O      Regression coefficients
;	- const             O      Offset used in regression algorithm
;	- Corr              O      Correlation Factors obtained
;	- mCorr             O      mCorrelation
;	- stat              O      Status flag of the regression process
;	- yfit              O      The regressed variables as estimated by fit
;	- sigm              O      Sigma of the obtained coefficients
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       05-13-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO generRegr4Bias,GeophParamIndx,FormGeophParam,ind,ncount,indepVars,depVar,reg,const,Corr,mCorr,stat,yfit,sigm

   nTerms          = n_elements(GeophParamIndx)
   X               = fltarr(nTerms,ncount)

   FOR iterm=0,n_elements(GeophParamIndx)-1 DO BEGIN
       IF (FormGeophParam(iterm) eq 0) THEN BEGIN
           X[iterm,0:ncount-1] = indepVars(ind,GeophParamIndx(iterm))
       ENDIF
       IF (FormGeophParam(iterm) eq 1) THEN BEGIN
           X[iterm,0:ncount-1] = alog(indepVars(ind,GeophParamIndx(iterm)))
       ENDIF
   ENDFOR
   Y               = depVar(ind)
   reg=regress(X,Y,const=const,Correlation=Corr,/double,mcorrelation=mCorr,status=stat,Yfit=yfit,sigma=sigm)

END

;===============================================================
; Name:		generRegr4RR
;
;
; Type:		IDL Subroutine
;
;
; Description:  Generates the regression coefficients for bias
;               computation for TB correction application.
;               Bias training set can be any array of geophysical
;               parameters of which the bias is sensitive to.
; 
;
;
; Arguments:
;
;      Name		   Type	    Description
;      ---------------------------------------------------
;	- GeophParamIndx    I      Index array of Geophysical Paramter
;                                  to be used to comput bias
;       - ind               I      index of profiles to use
;	- ncount            I      Number of profiles of the dataset
;	- indepVars         I      Independent variables
;	- depVar            I      Variable vector to be regressed (bias)
;	- reg               O      Regression coefficients
;	- const             O      Offset used in regression algorithm
;	- Corr              O      Correlation Factors obtained
;	- mCorr             O      mCorrelation
;	- stat              O      Status flag of the regression process
;	- yfit              O      The regressed variables as estimated by fit
;	- sigm              O      Sigma of the obtained coefficients
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       05-13-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO generRegr4RR,GeophParamIndx,ind,ncount,indepVars,depVar,reg,const,Corr,mCorr,stat,yfit,sigm

   nTerms          = n_elements(GeophParamIndx)
   X               = fltarr(nTerms,ncount)

   FOR iterm=0,n_elements(GeophParamIndx)-1 DO BEGIN
       X[iterm,0:ncount-1] = indepVars(GeophParamIndx(iterm),ind)
   ENDFOR
   Y               = depVar(ind)
   reg=regress(X,Y,const=const,Correlation=Corr,/double,mcorrelation=mCorr,status=stat,Yfit=yfit,sigma=sigm)
END

;===============================================================
; Name:		mspps_ocean
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes the TPW, CLW, and SEA Ice 
;               over ocean from AMSU-A channels
;               
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- TB1                I              TB @ 23 GHz
;	- TB2                I              TB @ 31 GHz
;	- TB3                I              TB @ 50 GHz
;	- TS                 I              Surface Temperature
;       - TL                 I              Atmospheric temperature at sfc
;	- E23                I              Emiss @ 23GHz
;	- E31                I              Emiss @ 31 GHz
;	- ANGLE              I              Zenith angle
;	- La5                I              Latitude
;	- iScanPos           I              Scan Position
;	- TPW                O              Integrated water vapor (TPW)
;	- CLW                O              Integarted cloud (CLW)
;	- SeaIce             O              Sea Ice Concentration 
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       11-10-2007      Cezar Kongoli, PSGS QSS Group Inc. @ NOAA/NESDIS/ORA
;       11-19-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO mspps_ocean,sensor_id,TB1,TB2,TB3,TS,E23,E31,Lat,node,ANGLE,TPW,CLW,SeaIce

;  Antenna Temperature Assymetry correction
 corr_asc  =  fltarr(3,6)
 corr_desc =  fltarr(3,6)
 corr      =  fltarr(3)

if (sensor_id eq 1) then begin 

 corr_asc[0,0:5]  = [-2.28626,      68.2791,      9.08166,  -2.39233e-08,   0.00146645,   0.000136875]
 corr_asc[1,0:5] = [-1.77232,      62.2333,      7.74059,  -4.61005e-08,   0.00298159,  -0.000294810]
 corr_asc[2,0:5] = [ 3.25164,      60.1840,     17.9200,   -0.0115559,    -0.00411548,   0.000423336]

 corr_desc[0,0:5] = [ 6.97408,      77.7821,     36.1330,   -0.687439,     -0.0520968,   -0.000590253]
 corr_desc[1,0:5] = [-1.34805,     -66.8981,      9.67602,   1.44355e-08,  -0.00364870,  -0.000270088]
 corr_desc[2,0:5] = [ 1.73330,      54.9991,     11.7439,   -3.00480e-05,   0.0100818,    0.000744985]

endif

if (sensor_id eq 2) then begin

 corr_asc[0,0:5] = [11.2799,     -80.1359,      38.2282,     -1.25344,    0.0715132,   -0.000717713]
 corr_asc[0,0:5] = [8.39946,      62.2558,     -32.7638,     -1.38113,   -0.0887914,   -0.000920122]
 corr_asc[0,0:5] = [1.34206,      57.8274,      13.0421, -7.23213e-05,   -0.0088870,    0.000909128]

 corr_asc[0,0:5] = [11.1418,      -80.8421,      38.6220,     -1.24613,   0.0754005,   -0.000846537]
 corr_asc[0,0:5] = [-2.00399,      64.2291,      10.5110, -5.82077e-08,   0.00301246,   0.000110321]
 corr_asc[0,0:5] = [0.222654,      48.7142,     -4.52047, -4.83124e-09,   0.00641666,   0.00107191]

endif

theta = angle
ich = 0
if (node eq 0.0) then begin
   for ich = 0,2 do begin
        corr(ich) = corr_asc(ich,0)*exp(-( ( (theta-corr_asc(ich,1)) / corr_asc(ich,2)) ^ 2.0 )/2.) + corr_asc(ich,3) + corr_asc(ich,4)*theta + corr_asc(ich,5) * (theta ^ 2.0)
  endfor
endif

if (node eq 1.0) then begin
   for ich = 0,2 do begin
        corr(ich) = corr_desc(ich,0)*exp(- ( ((theta-corr_desc(ich,1)) /corr_desc(ich,2)) ^ 2.0 )/2.) + corr_desc(ich,3) + corr_desc(ich,4)*theta + corr_desc(ich,5) * (theta ^ 2.0)
  endfor
endif

  tb23 = tb1 + corr(0)
  tb31 = tb2 + corr(1)
  tb50 = tb3 + corr(2)
  CLW_upper = 6.0
  TPW_upper = 75.0
  CLW_below = 0.0
  TPW_below = 0.0
  SeaIce_upper = 100.0
  SeaIce_below = 30.0
  CLW_undetermined = -99.0
  TPW_undetermined = -99.0
  ;---Sea Ice Concentration

  IF (abs(lat) lt 50.0) THEN BEGIN
      SeaIce = 0.0
  ENDIF ELSE BEGIN
      PI=3.141593
      mu = cos(angle*PI/180.)
      eps_water = 0.1824 + 0.9048 * mu - 0.6221 * mu * mu
      a = 1.84 - 0.723 * mu
      b = -0.00088
      c = 0.0066 + 0.0029 * mu
      d = -0.00926
      eps = a + b*tb23 + c*tb31 + d*tb50
      diffTB = tb23 - tb31
      IF (diffTB lt 5.0) THEN BEGIN
          eps_ice = 0.93
      ENDIF
      IF (diffTB ge 5.0 AND diffTB le 10.0) THEN BEGIN
          eps_ice = 0.87
      ENDIF
      IF (diffTB gt 10.0) THEN BEGIN
          eps_ice = 0.83
      ENDIF
      SeaIce = 100 * (eps - eps_water)/(eps_ice - eps_water)
      IF (SeaIce lt SeaIce_below) THEN BEGIN
         SeaIce = 0.0
      ENDIF
      IF (SeaIce ge SeaIce_upper) THEN BEGIN
          SeaIce = SeaIce_upper
      ENDIF
  ENDELSE
  ;--- TPW and CLW
  TL    = 285.-273.15
  kv23  = 4.80423E-3   &  kv31  = 1.93241E-3
  al_23 = 1.18201E-1   &  al_31 = 1.98774E-1
  bl_23 = -3.48761E-3  &  bl_31 = -5.45692E-3
  cl_23 = 5.01301E-5   &  cl_31 = 7.18339E-5
  ao_23 = 3.21410E-2   &  ao_31 = 5.34214E-2
  bo_23 = -6.31860E-5  &  bo_31 = -1.04835E-4

  coeA   =    0.968
  coeB   =   -1.878

  coe1_a =  -2.2682
  coe1_b =  -2.7575
  coe1_c = 3.064293419
  coe1_d = -1.844769074
  coe1_e = 1.100935855
  coe1_f = 2.573123026


  coe2_a= 44.7
  coe2_b= -73.142
  coe2_c= 27.95998241
  coe2_d= -2.699010169
  coe2_e= 1.116107868
  coe2_f= 4.454987186

  coe3_a= 35.984
  coe3_b= -79.406
  coe3_c= 37.87657236
  coe3_d= -3.907292323
  coe3_e= 1.20336045
  coe3_f= 3.959459873

  m     = cos(angle*3.14159/180.)
  to23  = ao_23+bo_23*TS
  to31  = ao_31+bo_31*TS
  kL23  = al_23+bl_23*TL+cl_23*TL^2
  kL31  = al_31+bl_31*TL+cl_31*TL^2
  a0    = -0.5*kv23/(kv23*kl31-kv31*kl23)
  b0    = 0.5*kl23/(kv23*kl31-kv31*kl23)
  a1    = kv31/kv23
  b1    = kl31/kl23
  a2    = -2.*(to31-a1*to23)/m+(1.-a1)*alog(TS)+alog(1.-e31)-a1*alog(1.-e23)
  b2    = -2.*(to31-b1*to23)/m+(1.-b1)*alog(TS)+alog(1.-e31)-b1*alog(1.-e23)
  tpwtmp = coeA * (m*b0*(alog(TS - tb31) - b1*alog(TS - tb23) - b2)) + coeB
  clwtmp = m*a0*(alog(TS - tb31) - a1*alog(TS - tb23) - a2)

if (clwtmp < 0.0) then begin
    clwtmp = 0.0
endif

if(tpwtmp lt 0) then begin
   tpwtmp = 0.0
endif else begin
    if(clwtmp gt 0. and  clwtmp lt 0.2) then begin
        temp = tpwtmp + coe1_a*m*m+ coe1_b*m + coe1_c
        if(temp gt  0.) then  begin
          tpwtmp = coe1_d*alog(temp) + coe1_e*temp + coe1_f
        endif
    endif
    if (clwtmp ge 0.2 and  clwtmp lt 0.8) then begin
        temp = tpwtmp + coe2_a*m*m+ coe2_b*m + coe2_c
        if(temp gt 0.) then begin
          tpwtmp = coe2_d*alog(temp) + coe2_e*temp + coe2_f
        endif
    endif
     
    if (clwtmp ge 0.8) then begin
        temp = tpwtmp + coe3_a*m*m+ coe3_b*m + coe3_c
        if(temp gt 0.) then begin
          tpwtmp = coe3_d*alog(temp) + coe3_e*temp + coe3_f
        endif
    endif
endelse

CLW = clwtmp
TPW = tpwtmp

if (TPW ge TPW_upper) then begin
   TPW = TPW_upper
endif

if(CLW ge CLW_upper) then begin
   CLW = CLW_upper
endif 

df1 = 2.85 + 0.020 * tb23 - 0.028 * tb50
if( (abs(lat) gt 50.0)  and  (df1 gt 0.2) ) then begin
  CLW = CLW_undetermined
  TPW = TPW_undetermined
endif

if(SeaIce gt 0.0) then begin
  CLW = CLW_undetermined
  TPW = TPW_undetermined
endif  

END



;===============================================================
; Name:		mspps_land
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes land Surface temperature, emissivities
;               at AMSU-A 23,31 and 50 GHz, snow cover and Snow
;               Water Equivalent (SWE) over land from
;               AMSU-A & AMSU-B/MHS TBs
;               
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- TB1                I              TB @ 23 GHz
;	- TB2                I              TB @ 31 GHz
;	- TB3                I              TB @ 50 GHz
;	- TB4                I              TB @ 52 GHz
;	- TB5                I              TB @ 53.6 GHz
;	- TB15               I              TB @ 89 GHz   (channel 1 AMSU-B/MHS)
;	- TB17               I              TB @ 157 GHz  (channel 2 MHS)
;	- TB19               I              TB @ 183 GHz  (channel 4 AMSU-B/MHS)
;	- lat                I              latitude
;	- lon                I              longitude
;	- Angle              I              Zenith angle
;	- LST                O              Land surface temperature
;	- emiss1             O              Emissivity at 23 GHz
;	- emiss2             O              Emissivity at 31 GHz
;	- emiss3             O              Emissivity at 50 GHz
;       - SCOVER             0              Snow cover
;       - SWE                0              Snow Water Equivalent (SWE)
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       11-10-2007      Cezar Kongoli, PSGS QSS Group Inc. @ NOAA/NESDIS/ORA
;       11-19-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================


PRO mspps_land,TB1,TB2,TB3,TB4,TB5,TB15,TB17,TB19,Lat,Lon,ANGLE,LST,emiss1,emiss2,emiss3,SCOVER,SWE
  tb23  = TB1
  tb31  = TB2
  tb50  = TB3
  tb52  = TB4
  tb53  = TB5
  tb89  = TB15
  tb157 = TB17
  tb183 = TB19
   ;---LST
  a0 = 2.9079E+2
  a1 = -8.5059E-1
  a2 = 1.9821E-3
  a3 = 6.1433E-1
  a4 = -2.3579E-3
  a5 = -1.1493E-0
  a6 = 5.4709E-3
  temp_lower_limit = 150.0
  temp_upper_limit = 350.0
  m    =cos(angle*3.14159/180.)
  dt   = 15.0 * (m - 0.54)
  LST = a0 + a1 * tb23 + a2 * tb23 * tb23 + a3 * tb31 + a4 * tb31 * tb31 + a5 * tb50 + a6 * tb50 * tb50 - dt
  if( LST lt temp_lower_limit) then begin 
      LST = temp_lower_limit
  endif
  if( LST gt temp_upper_limit) then begin 
      LST = temp_upper_limit
  endif
  ;---Emissivities
  b01 = -2.5404E-1
  b11 = 1.1326E-2
  b21 = -1.9479E-5
  b31 = -4.5763E-3
  b41 = 1.7833E-5
  b51 = 3.2324E-3
  b61 = -1.9056E-5
  b02 = -2.2606E-1
  b12 = 3.4481E-3
  b22 = -9.7185E-6
  b32 = 4.3299E-3
  b42 = 5.3281E-6
  b52 = 1.8668E-3
  b62 = -1.5369E-5
  b03 = 8.9494E-2
  b13 = -3.6615E-3
  b23 = 4.2390E-7
  b33 = 1.0636E-2
  b43 = -6.4559E-6
  b53 = -4.2449E-4
  b63 = -6.6878E-6
  fem1_lower_limit = 0.3
  fem2_lower_limit = 0.3
  fem3_lower_limit = 0.3
  fem1_upper_limit = 1.0
  fem2_upper_limit = 1.0
  fem3_upper_limit = 1.0
  fem1 = b01 + b11 * tb23 + b21 * tb23 * tb23 + b31 * tb31 + b41 * tb31 * tb31 + b51 * tb50 + b61 * tb50 * tb50
  fem2 = b02 + b12 * tb23 + b22 * tb23 * tb23 + b32 * tb31 + b42 * tb31 * tb31 + b52 * tb50 + b62 * tb50 * tb50
  fem3 = b03 + b13 * tb23 + b23 * tb23 * tb23 + b33 * tb31 + b43 * tb31 * tb31 + b53 * tb50 + b63 * tb50 * tb50
  if (fem1 lt fem1_lower_limit) then begin 
      fem1 = fem1_lower_limit
  endif
  if (fem2 lt fem2_lower_limit) then begin 
      fem2 = fem2_lower_limit
  endif
  if (fem3 lt fem3_lower_limit) then begin 
      fem3 = fem3_lower_limit
  endif
  if (fem1 gt fem1_upper_limit) then begin 
      fem1 = fem1_upper_limit
  endif
  if (fem2 gt fem2_upper_limit) then begin 
      fem2 = fem2_upper_limit
  endif
  if (fem3 gt fem3_upper_limit) then begin 
      fem3 = fem3_upper_limit
  endif
  emiss1 = fem1
  emiss2 = fem2
  emiss3 = fem3
  ;---Snow cover
  A0a     =    3.37
  A1a     =    0.595
  A2a     =    1.582
  A0b     =  -1.319
  A1b     =  -1.555
  A2b     =  -1.55
  A20     =    6.89584
  A21     =    0.96874
  SCOVER  = 0
  fsnow   = SCOVER
  acoslza = cos(angle*3.14159/180.)
  a23     = TB1
  a31     = TB2
  a50     = TB3
  b89     = TB15
  b150    = A20 + A21* TB17
  a52     = TB4
  a53     = TB5
  b180    = TB19
  df3     = 10.2+0.036*a23-0.074*a50
  tt      = 168.0 + 0.49*b89
  sc89    = a23 - b89 - 3.
  sc31    = a23 - a31 - 2.
  scat    = sc89
  if(sc31 lt 3 and a23 le  215) then begin
      if(lat lt -62. or (lat ge 59.6 and lat le 83.4 and lon ge -71.8 and lon le -14.1)) then begin
          fsnow = 1
      endif
  endif else begin
      if(sc89 ge 1.0) then begin
          fsnow = 1
          if(a23 ge 262.0 or a23 ge tt) then begin
              if(a52 gt 0.0 and a53 gt 0.0) then begin
                  A0 = A0a + A0b * acoslza
                  A1 = A1a + A1b * acoslza
                  A2 = A2a + A2b * acoslza
                  a53L = A0 - A1 * a53+ A2 * a52+ 4.0 * (1.0 - acoslza)
              endif
              sc150 = b89 - b150
              sc180 = a53L - b180               
              if((a23 lt 268.0) and (sc150 gt 3.0) and (sc180 lt -7.0) and (a53L lt 250.)) then begin
                  fsnow = 1
              endif else begin
                  fsnow = -99.0
              endelse
          endif 
          if(df3 le 0.4) then begin
              fsnow = -99
          endif
      endif
  endelse
  SCOVER = fsnow
  ;---SWE
  SWE = 0.0
  IF (SCOVER GT 0.) THEN BEGIN 
      SI31 = tb23 - tb31
      SI89 = tb23 - tb89
      SI89_1 = tb31 - tb89
      Ratio = SI89_1 / SI31
      IF(Ratio gt 8.0) then begin 
          SWE = 0.08*SI89 + 1.1
      endif else begin
          SWE = 0.6*SI31 + 1.7
      endelse
      IF(SWE le 0.0) then begin 
          SWE = 0.1
      endif
      if(SWE ge 30.0) then begin
          SWE = 30.0
      endif
     
     if(lat lt -60.0) then begin
        SWE = -99.0
     endif

     if(lat gt 26.25 and lat lt 41.25 and lon gt 78.75 and lon lt 105.75) then begin
       SWE = -99.0
     endif
     if( lat gt 79. and lon gt -68. and lon lt -8.) then begin
       SWE = -99.0
     endif
     if( lat gt 72. and lat lt 79. and lon gt -73.5 and lon lt -16.) then begin
       SWE = -99.0
     endif
     if( lat gt 67. and lat lt 72. and lon gt -56. and lon lt -20.) then begin
       swe = -99.0
     endif
     if(lat gt 60. and lat lt 67. and lon gt -56. and lon lt -32.) then begin
       SWE = -99.0
     endif

  ENDIF
END



;===============================================================
; Name:		mspps_rain
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes the Ice Water Path, the Effective Particle Deameter and 
;               the surface rain rate over land and ocean for the AMSU-MHS sensor
;              
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;       - sfcTyp             I              surface type ( 0 for ocean, 1 for ice, 2 for land, and 3 for snow)
;	- TB1                I              TB @ 23 GHz
;	- TB2                I              TB @ 31 GHz
;	- TB16               I              TB @ 89 GHz (MHS channel 1)
;	- TB17               I              TB @ 157 GHz (MHS channel 2)
;	- TB18               I              TB @ 190 GHz (channel 3 MHS)
;	- TB19               I              TB @ 190 GHz  (channel 4 MHS)
;	- TB20               I              TB @  190 GHz (channel 5 MHS)
;	- lat                I              latitude
;	- lon                I              longitude
;	- Angle              I              Local Zenith angle
;	- ts                 I              surface temperature
;	- em89               I              Emissivity at 89 GHz (over ocean)
;	- em157              I              Emissivity at 157 GHz (over ocean)
;       - tpw                I              total precipitable water over ocean (mm)
;       - clw                I              cloud liquid water (mm) over ocean
;       - IWP                O              Ice Water Path (kg / m2)
;       - RR                 O              Rain Rate (cm /hour)
;       - De                 O              particle effective diameter 9mm)
; Subroutines needed:
;       - None
;
;
; History:
;       03-31-2008      Cezar Kongoli, PSGS QSS Group Inc. @ NOAA/NESDIS/ORA
;
;===============================================================

PRO mspps_rain,sfcTyp,lat,lon,TB1,TB2,TB16,TB17,TB18,TB19,TB20,angle,ts,em89,em157,clw,tpw,IWP,De,RR

    SSMIArea   =     182

    coe_rr = fltarr(5,6)

    coe_rr[0,0:5] = [ 8.4352514756117, -0.060296877987446, -0.00054936395647573, -0.033675721411174, $
                 0.00083674824564975, -0.00086265338741617]
    coe_rr[1,0:5] = [ 3.6702179933091, -0.017302697728341, -0.00026283313777525, -0.014163338358315, $
                   0.00035417305674957, -0.00052079582170665]
    coe_rr[2,0:5] = [ 4.1652486678776, -0.037312486505719, -0.00015803016559758, -0.012934749819459, $
                  0.00051213974089996, -0.00037569662434794]
    coe_rr[3,0:5] = [3.2898081570968, -0.015392931621478, -0.00036783945503082,  0.000891466966662, $
                 0.00028204892832443, -0.00047162080113714]
    coe_rr[4,0:5] = [278.867, -3.69141000, 0.58227500, -0.0175858, 0.000240862, 0.00000000000000000]


    b0 = fltarr(4)
    b1 = fltarr(4)
    b0[0:3] = [1.50155, 1.93515, 4.17168, 7.81001]
    b1[0:3] = [0.0591993, 0.566360, 2.03765, 4.91456]


    A20  =  6.89584
    A21  =  0.96874
    A50  =  -8.09859
    A51  =  1.03174
    b150 = A20 + A21 * TB17
    b176 = A50 + A51 * TB20
    b89  = TB16
    b182 = TB18
    b180 = TB19
    em150 = em157  ; temporarily

    IWP = -999.0
    De  = -999.0
    RR  = -999.0
 
    ICE_DEN = 920.0

    INDETERM_MULTI_YEAR_ICE = -14
    INDETERM_FROZEN         = -13
    INDETERM_ELEV           = -12
    INDETERM_DESERT         = -11
    INDETERM                = -10
    INDETERM_COAST          = -9
    INDETERM_SICE           = -8
    INDETERM_SNOW           = -7
    INDETERM_RAIN           = -6
    INDETERM_CLW            = -5

    IWP_UPPER = 2.0
    RR_UPPER = 30.00

    kv_89 = 0.0115839
    kv_150 = 0.029519

    al_89 = 1.03486
    bl_89 = -0.0097151
    cl_89 = -0.000065914

    a0_89 = 0.108333
    b0_89 = -0.000221042

    al_150 = 1.7129
    bl_150 = 0.0051329
    cl_150 = -0.00022475
    a0_150 = 0.030698
    b0_150 = -0.000071433

    T_CLOUD_LAYER = 2

    PI=3.141593

    theta = angle * PI / 180.
    mu = cos(theta)
    clw1 = clw
    tpw1 = tpw
    if (sfcTyp le 1) then begin
       kl_89 = al_89 + bl_89 * T_CLOUD_LAYER + cl_89 * T_CLOUD_LAYER ^ 2
       kl_150 = al_150 + bl_150 * T_CLOUD_LAYER + cl_150 * T_CLOUD_LAYER ^ 2

       tu0_89 = a0_89 + b0_89 * ts
       tu0_150 = a0_150 + b0_150 * ts

       if(clw < 0.0) then clw1 = 0. 
       tul_89 = kl_89 * clw1
       tul_150 = kl_150 * clw1

       tuv_89 = kv_89 * tpw1
       tuv_150 = kv_150 * tpw1

       trans_89 = exp(-(tu0_89 + tuv_89 + tul_89)/mu)
       pred89 = ts * (1 - (1 - em89) * trans_89 ^ 2)

       trans_150 = exp(-(tu0_150 + tuv_150 + tul_150)/mu)
       pred150 = ts * (1 - (1 - em150) * trans_150 ^ 2)
    endif

    if (sfcTyp ge 2) then begin
       a23 = TB1
       a31 = TB2
       pred89 = 17.88 + 1.61* a23 - 0.67 * a31
       pred150 = 33.78 + 1.69* a23 - 0.80* a31
    endif
    
    if(abs(angle) gt 0. and  abs(angle) lt 10.) then begin
       lza = angle
       lza0=0.0
       lza1=10.0
       b89_0=183.073-0.649864*b89+0.00355388*b89*b89
       b150_0=89.4492+0.133525*b150+0.00193974*b150*b150
       b89_1=168.617-0.526129*b89+0.00329590*b89*b89
       b150_1=85.7358+0.169666*b150+0.00185847*b150*b150
       b89=(b89_1-b89_0)*(abs(lza)-lza0)/(lza1-lza0)+b89_0
       b150=(b150_1-b150_0)*(abs(lza)-lza0)/(lza1-lza0)+b150_0
    endif 
    if(abs(angle) ge 10. and abs(angle) lt 20.) then begin
       lza = angle  
       lza0=10.0
       lza1=20.0
       b89_0=168.617-0.526129*b89+0.00329590*b89*b89
       b150_0=85.7358+0.169666*b150+0.00185847*b150*b150
       b89_1=135.886-0.239320*b89+0.00268872*b89*b89
       b150_1=72.1034+0.300571*b150+0.00156526*b150*b150
       b89=(b89_1-b89_0)*(abs(lza)-lza0)/(lza1-lza0)+b89_0
       b150=(b150_1-b150_0)*(abs(lza)-lza0)/(lza1-lza0)+b150_0
    endif
    if(abs(angle) ge 20. and abs(angle) lt 30.0) then begin
       lza = angle
       lza0=20.0
       lza1=30.0
       b89_0=135.886-0.239320*b89+0.00268872*b89*b89
       b150_0=72.1034+0.300571*b150+0.00156526*b150*b150
       b89_1=99.8433+0.0911668*b89+0.00196905*b89*b89
       b150_1=51.6176+0.501623*b150+0.00110930*b150*b150
       b89=(b89_1-b89_0)*(abs(lza)-lza0)/(lza1-lza0)+b89_0
       b150=(b150_1-b150_0)*(abs(lza)-lza0)/(lza1-lza0)+b150_0
    endif
    if(abs(angle) ge 30. and abs(angle) lt 40.) then begin
       lza = angle
       lza0=30.0
       lza1=40.0
       b89_0=99.8433+0.0911668*b89+0.00196905*b89*b89
       b150_0=51.6176+0.501623*b150+0.00110930*b150*b150
       b89_1=52.4938+0.535288*b89+0.000986296*b89*b89
       b150_1=26.8442+0.753185*b150+0.000528123*b150*b150
       b89=(b89_1-b89_0)*(abs(lza)-lza0)/(lza1-lza0)+b89_0
       b150=(b150_1-b150_0)*(abs(lza)-lza0)/(lza1-lza0)+b150_0
   endif
   if(abs(angle) ge 40. and abs(angle) lt 50.) then begin
       lza = angle 
       lza0=40.0
       lza1=50.0
       b89_0=52.4938+0.535288*b89+0.000986296*b89*b89
       b150_0=26.8442+0.753185*b150+0.000528123*b150*b150
       b89_1=7.92203+0.981133*b89-0.0000394*b89*b89
       b150_1=-2.74337+1.06524*b150-0.000209793*b150*b150
       b89=(b89_1-b89_0)*(abs(lza)-lza0)/(lza1-lza0)+b89_0
       b150=(b150_1-b150_0)*(abs(lza)-lza0)/(lza1-lza0)+b150_0
   endif 
   if(abs(angle) ge 50) then begin
       b89=7.92203+0.981133*b89-0.0000394*b89*b89
       b150=-2.74337+1.06524*b150-0.000209793*b150*b150
   endif

   omega89 = (pred89 - b89)/b89
   omega150 = (pred150 - b150)/b150
   ratio = omega89/omega150
   iwp =0.0
   de = 0.0
   if(omega89 gt 0.0 and omega150 gt 0.0 and b176 < 265.0) then begin
;   conditions for the existance of detectable clouds 
       mu = cos(angle * PI / 180.)
       de = 0.0
       iwp = 0.0
       if((ratio gt 0.0) and (ratio le 0.8)) then begin
;  Calculate the ice particle effective diameters
            
;----------------------------
; Calculate ice particle
; effective diameters
;----------------------------
           coef_a = -0.300323
           coef_b = 4.30881
           coef_c = -3.98255
           coef_d = 2.78323
           de = coef_a + coef_b * ratio + coef_c *ratio ^ 2  + coef_d * ratio ^ 3
; Calculate the ice water path 
           omega=omega89
           coef_iwp_a = -1.19301
           coef_iwp_b = 2.08831
           coef_iwp_c = -0.857469
           if(de le 1.0) then begin
               omega=omega150
               coef_iwp_a = -0.294459
               coef_iwp_b = 1.38838
               coef_iwp_c = -0.753624
           endif
           if (de gt 0.0) then begin
               omegan = exp(coef_iwp_a + coef_iwp_b * alog(de) +  coef_iwp_c * alog(de) ^ 2)
               if(omegan gt 0.0) then iwp = (omega * de * 1.e-3 * mu * ICE_DEN / omegan)
           endif    
       endif         
   endif      
   if(iwp gt 3.0)  then iwp = 3.0
   if(iwp lt 0.0)  then iwp = 0.0
   if(de gt 3.5)   then de = 3.5 
   if(de lt 0.0)   then de = 0.0
   
   if( sfcTyp eq 3.0) then begin
;no retrieval over surface covered with snow */
       iwp = INDETERM_SNOW
       de  = INDETERM_SNOW
   endif

   if(sfcTyp eq 1.0) then begin
; no retrieval over sea ice 
       iwp = INDETERM_SICE
       de =  INDETERM_SICE
   endif
   if(ts lt 269.0) then begin
; high latitude frozen surface undetected by snow and sea ice  */
       iwp = INDETERM_FROZEN
       de = INDETERM_FROZEN
   endif
   if(sfcTyp ge 2.0 and b89 lt b150 and de gt 0) then begin
       iwp = INDETERM
       de  = INDETERM
   endif
  if( sfctyp le 1.0 and b89 lt b150  and de gt 0) then begin
       iwp = INDETERM
       de = INDETERM
  endif
  if((ts - b176) lt 10.0 and sfcTyp ge 2.0 and iwp  gt 0.0) then begin
 ; possibly over desert
       iwp = INDETERM_DESERT
       de = INDETERM_DESERT
  endif
  if((b89-b150) lt 0 and lat gt 22. and lat < 47. $
                 and lon gt 65. and  lon lt 112 and iwp  gt 0.0) then begin
;special check over the Himalayas Mountains
       iwp = INDETERM_ELEV
       de = INDETERM_ELEV
 endif

; calculate the convective index
 dta=b89 - b150
 dta1=b182 - b176
 dta2=b180 - b176
 dta3=b182 - b180
 clr=0
 if(dta gt 0 and dta2 gt 0 and dta2 gt dta1 and dta2 gt dta3) then clr=1
 if(dta gt 0 and dta2 gt 0 and dta1 gt 0 and dta3 gt 0 and $
         dta1 gt dta2 and dta1 gt dta3 and dta2 gt dta3) then clr=2
 if(dta gt 0 and dta2 gt 0 and dta1 gt 0 and dta3 gt 0 && $
                 dta1 gt dta2 and dta1 gt dta3 and dta2 le dta3) then clr=3

 conv_index =clr

; calculate rain rate
 coef0 =  0.08925
 coef1 = 20.8194
 coef2 = -2.9117


 if (iwp le 0.0) then rr = iwp
 if (iwp gt 0.0) then begin
     rr = 0.
     if(conv_index ge 3) then begin
         coef0 = 0.08925
         coef1 = 20.8194
         coef2 = -2.9117
     endif
; Ocean
     if(sfctyp le 1) then begin
         if(clw1 ge 0.2 and iwp ge 0.05 and de ge 0.3) then $
                rr = coef0 + coef1 * iwp + coef2 * iwp ^ 2
     endif
     if(sfctyp ge 2) then begin
         if( iwp ge 0.05 and de ge 0.3 and (b89 - b150) gt 3.0 ) then $
                rr = coef0 + coef1 * iwp + coef2 * iwp ^ 2
     endif
 endif
 if (iwp ge IWP_UPPER) then iwp = IWP_UPPER
 if (rr ge  RR_UPPER) then rr = RR_UPPER


; rain rate correction 

 if(rr > 0) then begin
     if(sfctyp le 1  ) then ind = 0
     if(sfctyp ge 2  ) then ind = 2

     ltd = abs(lat)
     lza1 = abs(angle)
     rr_temp = rr
     mu = coe_rr[ind,0]+coe_rr[ind,1]*lza1+coe_rr[ind,2]*lza1*lza1+ $
              coe_rr[ind,3]*ltd+coe_rr[ind,4]*ltd*lza1+coe_rr[ind,5]*ltd*ltd

     sigma = coe_rr[ind+1,0]+coe_rr[ind+1,1]*lza1+coe_rr[ind+1,2]*lza1*lza1+ $
            coe_rr[ind+1,3]*ltd+coe_rr[ind+1,4]*ltd*lza1+coe_rr[ind+1,5]*ltd*ltd

     suma = mu+2*sigma

     alfa = 0.3+lza1*0.0051

     AMSUArea = coe_rr[4,4]*lza1 ^ 4 +coe_rr[4,3]* lza1 ^ 3 $
               +coe_rr[4,2]* lza1 ^ 2 +coe_rr[4,1]*lza1+coe_rr[4,0]

     if(sfctyp ge 2) then  slope = 1.3*sqrt(AMSUArea/SSMIArea)
     if(sfcTyp le 1)  then slope = 1.5*sqrt(AMSUArea/SSMIArea)
     exponente = -0.5*((suma-mu)/sigma) ^ 2
     factor = 1 - (1-alfa)*exp(exponente)
     ordinate = factor*suma - slope*suma

     if(rr_temp lt  suma) then begin
         exponente = -0.5 * ((rr_temp - mu)/sigma) ^ 2
         if(rr_temp lt mu) then exponente = -0.5 * ((rr_temp - mu)/(sigma*2)) ^ 2
         factor = 1 - (1 - alfa)*exp(exponente)
         rr  = factor * rr_temp
     endif
     if (rr_temp ge  suma) then rr = slope * rr_temp + ordinate
 endif

 if (rr eq  -10 or  rr eq 0) then begin
     ci = conv_index
     if(sfcTyp ge 2 and  rr eq  -10) then begin
         if(ci eq 0)  then rr = 0.0
         if(ci eq 1)  then rr = 1.97
         if(ci eq 2)  then rr = 5.95
         if(ci eq 3)  then rr = 10.95
     endif
     if(sfcTyp le 1) then begin
         if(clw1 >= 0.4) then begin
             if(ci eq 0) then rr = b0[0]+b1[0]*clw1
             if(ci eq 1) then rr = b0[1]+b1[1]*clw1
             if(ci eq 2) then rr = b0[2]+b1[2]*clw1 
             if(ci eq 3) then rr = b0[3]+b1[3]*clw1
         endif
         if(clw1 lt 0.4 and clw1 ge 0) then  begin
             if(ci eq 0 or ci eq 1 or ci eq 2 or ci eq 3) then rr = 0
         endif
     endif
 endif    

 if (rr ge 40.0)  then rr = 40.0

 if (de lt 0.0 )  then de  = -999.0
 if (iwp lt 0.0)  then iwp = -999.0
 if (rr lt 0.0 )  then rr  = -999.0

END


;===============================================================
; Name:		mspps_snowfall_land
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes snowfall extent over land 
;              
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;       - sfcTyp             I              surface type ( 0 for ocean, 1 for ice, 2 for land, and 3 for snow)
;	- TB1                I              TB @ 23 GHz (AMSU- A channel 1)
;	- TB2                I              TB @ 31 GHz (AMSU-A Channel 2)
;	- TB3                I              TB @ 50 GHz (AMSU-A Chgnnel 3)
;	- TB4                I              TB @ 52 GHz (AMSU-A channel 4)
;	- TB5                I              TB @ 53.6 GHz  (AMSU-A Channel 5)
;	- TB16               I              TB @ 89 GHz (MHS channel 1)
;	- TB17               I              TB @ 157 GHz (MHS channel 2)
;	- TB18               I              TB @ 190 GHz (channel 3 MHS)
;	- TB19               I              TB @ 190 GHz  (channel 4 MHS)
;	- TB20               I              TB @  190 GHz (channel 5 MHS)
;	- Angle              I              Local Zenith angle
;	- ts                 I              surface temperature
;	- snowfall           I              snowfall extent (1 for snowfall and 0 for no snowfall)
; Subroutines needed:
;       - None
;
;
; History:
;       03-31-2008      Cezar Kongoli, PSGS QSS Group Inc. @ NOAA/NESDIS/ORA
;
;===============================================================

PRO mspps_snowfall_land,sfctyp,TB1,TB2,TB4,TB5,TB16,TB17,TB18,TB19,TB20,angle,ts,snowfall

 A0a   =    3.37
 A1a   =    0.595
 A2a   =    1.582

 A0b   =   -1.319
 A1b   =   -1.555
 A2b   =   -1.55

 A20  =  6.89584
 A21  =  0.96874
 A50  =  -8.09859
 A51  =  1.03174
 TFR = 267.0

 b150 = A20 + A21 * TB17
 b176 = A50 + A51 * TB20
 b89  = TB16
 b182 = TB18
 b180 = TB19
 a23 = TB1
 a31 = TB2
 a52 = TB4
 a53 = TB5
 a53L = 0.0
 PI=3.141593
 COLD_SNOW_LO=243
 COLD_SNOW_HI=245
 acoslza = cos(angle* PI / 180.)
 A0 = A0a + A0b * acoslza
 A1 = A1a + A1b * acoslza
 A2 = A2a + A2b * acoslza
 a53L = A0 - A1 * a53+ A2 * a52+ 4 * (1 - acoslza)
 if(sfcTyp le 1) then snowfall = -999.0
 snowfall = 0.0
 if(sfctyp eq 3.0 or ts le TFR) then begin
     if(a53L ge COLD_SNOW_LO and a53L le COLD_SNOW_HI) then begin
         snowfall = 0.0
         threshold = 242.5 + 5*acoslza
         depression = b180 - threshold
         if(depression lt 0) then snowfall = 1
     endif
     if(a53L lt COLD_SNOW_LO) then snowfall = -999.0
     if(a53L gt COLD_SNOW_HI) then begin
         snowfall = 0
         if((b89 - b150) ge 4.) then begin
             if((b176 lt 255) and (b180 lt 253) and (b182 lt 250)) then snowfall = 1
             if( (b176 ge 255.) and (b180 le 253.) and (a23 le 262) )  then begin
                 if((b150 - b176) ge -16. && (b176 - b180) ge -3. && (b89 - b150) le 10.) then snowfall = 1
             endif
         endif
     endif
 endif
END

;===============================================================
; Name:		ssmis_clw
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes clw from ssmis channels
;              Note: Not validated, use with caution!
;              
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- TB19v             I              TB 19 GHz v
;	- TB22v             I              TB 22 GHz v
;	- TB37v             I              TB 37 GHz v
;	- SfcType           I              surface type (0 for ocean, 1 for ice, 2 for land, and 3 for snow)
;	- CLW               O              cloud liquid water
;       
;
; Original Code:
;       Kevin Garrett, IMSG @ NOAA/NESDIS/STAR  05/14/2008
;============================================================
PRO ssmis_clw,TB19v,TB22v,TB37v,TB91h,SfcType,CLW
 
 RT=285
 CLW = 0.0
 ALG1 = -999.
 ALG2 = -999.
 ALG3 = -999.
 IF (SfcType eq 0) THEN BEGIN
     TPW = 232.89 - 0.1486*TB19v - 0.3695*TB37v - (1.8291 - 0.006193*TB22v)*TB22v
     IF ( (TB19v lt RT) and (TB22v lt RT) ) THEN BEGIN
         ALG1 = -3.20 * ( alog(290.0-TB19v) - 2.80 - 0.42*alog(290.0-TB22v) ) 
     ENDIF
     
     IF ( (TB37v lt RT) and (TB22v lt RT) ) THEN BEGIN
         ALG2 = -1.66 * ( alog(290.0-TB37v) - 2.90 - 0.349*alog(290.0-TB22v) )
     ENDIF

     IF ( (TB91h lt RT) and (TB22v lt RT) ) THEN BEGIN
         ALG3 = -0.44 * ( alog(290.0-TB91h) + 1.60 - 1.354*alog(290.0-TB22v) )
     ENDIF
     
     IF ( ALG1 gt 0.70 ) THEN CLW = ALG1 ;original threshold 0.60
     IF ( ALG2 gt 0.28 ) THEN CLW = ALG2 ;original threshold 0.28
     IF ( TPW  lt 30.0 ) THEN CLW = ALG3
     
     IF (CLW gt 6.0 or CLW lt 0.0) THEN CLW = 0.0
 ENDIF ELSE BEGIN
     CLW = -999.0
 ENDELSE
       

END


