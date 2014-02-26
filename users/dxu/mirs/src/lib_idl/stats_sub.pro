;---------------------------------------------------------------------------------
; Summary of subroutines related to computing statistical data and
; performing EOF decomposition.
;
; S.A. Boukabara IMSG Inc. @ NOAA/NESDIS 2005-2006
;
;---------------------------------------------------------------------------------

;===============================================================
; Name:		Stats
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes basic matrix statistics (mean and covariance
;               matrix) from a set of profiles.
;
;
; Arguments:
;
;      Name	     Type	Description
;      ---------------------------------------------------
;	- Gmatrx       I        Array of profiles of params (nProfiles x nParams)
;	- nParams      I        Number of parameters
;	- nProfiles    I        Number of profiles
;	- M            O        Mean vector (nParams)
;	- C            O        Covariance matrix (nParams x nParams)
;
;
; Subroutines needed:
;       - None
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;       09-08-2010      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
;
; Record of revisions:
;    Date          Programmer               Description of change
; ============    ==============         =============================
;  09-08-2010   Flavio Iturbide-Sanchez  Eliminated the effec of
;                                        non-valid data (-999.0) 
;===============================================================

PRO Stats,Gmatrx,nParams,nProfiles,M,C
   Gloc = Gmatrx 
   ;---Matrix to allocate the X-E(X) values
   Gloc1= fltarr(nProfiles,nParams)
   Gloc1(*,*)=0.0
   ;---Detrend the data and Compute Mean
   M    = fltarr(nParams)
   FOR ip=0,nParams-1 DO begin
       ind=where(Gloc(0:nProfiles-1,ip) gt -999.,ncount)
       IF (ncount ne 0) THEN BEGIN
           M(ip) = Total(Gloc(ind,ip))/ncount
           For is=0L,ncount-1 DO begin
               Gloc1(ind(is),ip)=Gloc(ind(is),ip)-M(ip)
           EndFor 
       ENDIF
       IF (ncount eq 0) THEN BEGIN
           Gloc1(0:nProfiles-1,ip)=0.0
           M(ip)=0.0
       ENDIF
   EndFor
   ;---Compute covariance
   C=MATRIX_MULTIPLY (transpose(Gloc1),Gloc1)
   C=C/(nProfiles-1.)
   RETURN
END


;===============================================================
; Name:		ComputeStats
;
;
; Type:		IDL Subroutine
;
;
; Description:  Uses the previous stats subroutine to compute
;               the mean and covariance matrix of different EDRs:
;               Temperature, Humidity, Cloud, ozone, rain, snow, 
;               ice, graupel, emissivity, reflectivity, winspeed,
;               skin temperature, delta-temperature and surface
;               pressure.
;
;
; Subroutines needed:
;       - Stats
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ComputeStats,nProfiles_T,nProfiles_Q,nProfiles_CLD,nProfiles_sfc,nLay,Temp_lay,      $
                 M_Temp,C_temp,Abso_lay,iH2O,M_WVap,C_WVap,                              $
                 iO3,M_ozon,C_ozon,nParmCLW,clw,M_clw,C_clw,nParmRain,rain,M_rain,C_rain,$
                 nParmSnow,snow,M_snow,C_snow,nParmIce,ice,M_ice,C_ice,nParmGrpl,grpl,   $
                 M_grpl,C_grpl,nchan,emiss,M_emiss,C_emiss,refl,M_refl,C_refl,WindSp,    $
                 M_WindSp,C_WindSp,Tskin,M_Tskin,C_Tskin,deltaT,M_DeltaT,C_DeltaT,       $
                 SfcPres,M_SfcPres,C_SfcPres
  ;---Temperature 
  Stats,Temp_lay,nLay,nProfiles_T,M_Temp,C_temp
  ;---Water vapor 
  Stats,Abso_lay(0:nProfiles_Q-1,0:nLay-1,iH2O),nLay,nProfiles_Q,M_WVap,C_WVap
  ;---Ozone
  Stats,Abso_lay(0:nProfiles_Q-1,0:nLay-1,iO3),nLay,nProfiles_Q,M_ozon,C_ozon
  ;---Clw
  Stats,clw(0:nProfiles_CLD-1,0:nParmCLW-1),nParmCLW,nProfiles_CLD,M_clw,C_clw
  ;---Rain
  Stats,rain(0:nProfiles_CLD-1,0:nParmRain-1),nParmRain,nProfiles_CLD,M_rain,C_rain
  ;---Snow
  Stats,snow(0:nProfiles_CLD-1,0:nParmSnow-1),nParmSnow,nProfiles_CLD,M_snow,C_snow
  ;---Ice
  Stats,ice(0:nProfiles_CLD-1,0:nParmIce-1),nParmIce,nProfiles_CLD,M_ice,C_ice
  ;---Graupel
  Stats,grpl(0:nProfiles_CLD-1,0:nParmGrpl-1),nParmGrpl,nProfiles_CLD,M_grpl,C_grpl
  ;---Emissivity
  Stats,emiss(0:nProfiles_sfc-1,0:nchan-1),nchan,nProfiles_sfc,M_emiss,C_emiss
  ;---Reflectivity
  Stats,Refl(0:nProfiles_sfc-1,0:nchan-1),nchan,nProfiles_sfc,M_refl,C_refl
  ;---Surface parameters
  Stats,WindSp,1,nProfiles_sfc,M_WindSp,C_WindSp
  Stats,Tskin,1,nProfiles_sfc,M_Tskin,C_Tskin
  Stats,deltaT,1,nProfiles_sfc,M_DeltaT,C_DeltaT
  Stats,SfcPres,1,nProfiles_sfc,M_SfcPres,C_SfcPres
END



;===============================================================
; Name:		EOFdecomp
;
;
; Type:		IDL Subroutine
;
;
; Description:  EOF-decompose already existing covariance matrix
;               for temperature, water vapor, ozone, cloud, rain,
;               snow, ice, graupel, emissivity, reflectivity, wind
;               speed, skin temperature, delta-Temperatue and surface
;               pressure. It heavily relies on the subroutine 
;               EigenvDecomp. It generates the transformation  
;               matrix, eigenvalue matrx and eigenvalues.
;               This subroutine assumes that C and M have been
;               generated already and are passed as inputs.
;
;
; Subroutines needed:
;       - EigenvDecomp
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO EOFdecomp,C_temp,D_temp,A_temp,NEIGENV_temp,C_WVap,D_WVap,A_WVap,NEIGENV_WVap,                   $
              C_ozon,D_ozon,A_ozon,NEIGENV_ozon,C_clw,D_clw,A_clw,NEIGENV_clw,                       $
              C_rain,D_rain,A_rain,NEIGENV_rain,C_snow,D_snow,A_snow,NEIGENV_snow,                   $
              C_ice,D_ice,A_ice,NEIGENV_ice,C_grpl,D_grpl,A_grpl,NEIGENV_grpl,                       $
              C_emiss,D_emiss,A_emiss,NEIGENV_emiss,C_refl,D_refl,A_refl,NEIGENV_refl,               $
              C_WindSp,D_windSp,A_windsp,neigenv_windsp,C_Tskin,D_Tskin,A_tskin,neigenv_tskin,       $
              C_DeltaT,D_deltaT,A_deltat,neigenv_deltat,C_SfcPres,D_SfcPres,A_SfcPres,neigenv_SfcPres
  ;---Temperature 
  EigenvDecomp,C_temp,D_temp,A_temp,NEIGENV_temp
  ;---Water vapor 
  EigenvDecomp,C_WVap,D_WVap,A_WVap,NEIGENV_WVap
  ;---Ozone
  EigenvDecomp,C_ozon,D_ozon,A_ozon,NEIGENV_ozon
  ;---Clw
  EigenvDecomp,C_clw,D_clw,A_clw,NEIGENV_clw
  ;---Rain
  EigenvDecomp,C_rain,D_rain,A_rain,NEIGENV_rain
  ;---Snow
  EigenvDecomp,C_snow,D_snow,A_snow,NEIGENV_snow
  ;---Ice
  EigenvDecomp,C_ice,D_ice,A_ice,NEIGENV_ice 
  ;---Graupel
  EigenvDecomp,C_grpl,D_grpl,A_grpl,NEIGENV_grpl
  ;---Emissivity
  EigenvDecomp,C_emiss,D_emiss,A_emiss,NEIGENV_emiss
  ;---Reflectivity
  EigenvDecomp,C_refl,D_refl,A_refl,NEIGENV_refl
  ;---Surface parameters
  neigenv_windsp  = 1
  A_windsp        = make_array(1,1,value=1.)
  neigenv_tskin   = 1
  A_tskin         = make_array(1,1,value=1.)
  neigenv_deltat  = 1
  A_deltat        = make_array(1,1,value=1.)
  neigenv_SfcPres = 1
  A_SfcPres       = make_array(1,1,value=1.)
END



;===============================================================
; Name:		ComputeStatsEOFdecomp
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes the statistics for the EOF decomposition
;               for temperature, water vapor, ozone, cloud, rain,
;               snow, ice, graupel, emissivity, reflectivity, wind
;               speed, skin temperature, delta-Temperatue and surface
;               pressure. It heavily relies on the subroutines Stats 
;               and EigenvDecomp. It generates the mean, covariance
;               matrix, eigenvalue and eigenvectors matrices, # of 
;               eigenvalues.
;
;
; Subroutines needed:
;       - Stats
;       - EigenvDecomp
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO ComputeStatsEOFdecomp,nProfiles,                                          $
                          nLay,Temp_lay,M_Temp,C_temp,A_temp,NEIGENV_temp,    $
                          Abso_lay,iH2O,M_WVap,C_WVap,A_WVap,NEIGENV_WVap,    $
                          iO3,M_ozon,C_ozon,A_ozon,NEIGENV_ozon,              $
                          nParmCLW,clw,M_clw,C_clw,A_clw,NEIGENV_clw,         $
                          nParmRain,rain,M_rain,C_rain,A_rain,NEIGENV_rain,   $
                          nParmSnow,snow,M_snow,C_snow,A_snow,NEIGENV_snow,   $
                          nParmIce,ice,M_ice,C_ice,A_ice,NEIGENV_ice,         $
                          nParmGrpl,grpl,M_grpl,C_grpl,A_grpl,NEIGENV_grpl,   $
                          nchan,emiss,M_emiss,C_emiss,A_emiss,NEIGENV_emiss,  $
                          refl,M_refl,C_refl,A_refl,NEIGENV_refl,             $
                          WindSp,M_WindSp,C_WindSp,A_windsp,neigenv_windsp,   $
                          Tskin,M_Tskin,C_Tskin,A_tskin,neigenv_tskin,        $
                          deltaT,M_DeltaT,C_DeltaT,A_deltat,neigenv_deltat,   $
                          SfcPres,M_SfcPres,C_SfcPres,A_SfcPres,neigenv_SfcPres
  ;---Temperature 
  Stats,Temp_lay,nLay,nProfiles,M_Temp,C_temp
  EigenvDecomp,C_temp,D_temp,A_temp,NEIGENV_temp
  ;---Water vapor 
  Stats,Abso_lay(0:nProfiles-1,0:nLay-1,iH2O),nLay,nProfiles,M_WVap,C_WVap
  EigenvDecomp,C_WVap,D_WVap,A_WVap,NEIGENV_WVap
  ;---Ozone
  Stats,Abso_lay(0:nProfiles-1,0:nLay-1,iO3),nLay,nProfiles,M_ozon,C_ozon
  EigenvDecomp,C_ozon,D_ozon,A_ozon,NEIGENV_ozon
  ;---Clw
  Stats,clw(0:nProfiles-1,0:nParmCLW-1),nParmCLW,nProfiles,M_clw,C_clw
  EigenvDecomp,C_clw,D_clw,A_clw,NEIGENV_clw
  ;---Rain
  Stats,rain(0:nProfiles-1,0:nParmRain-1),nParmRain,nProfiles,M_rain,C_rain
  EigenvDecomp,C_rain,D_rain,A_rain,NEIGENV_rain
  ;---Snow
  Stats,snow(0:nProfiles-1,0:nParmSnow-1),nParmSnow,nProfiles,M_snow,C_snow
  EigenvDecomp,C_snow,D_snow,A_snow,NEIGENV_snow
  ;---Ice
  Stats,ice(0:nProfiles-1,0:nParmIce-1),nParmIce,nProfiles,M_ice,C_ice
  EigenvDecomp,C_ice,D_ice,A_ice,NEIGENV_ice 
  ;---Graupel
  Stats,grpl(0:nProfiles-1,0:nParmGrpl-1),nParmGrpl,nProfiles,M_grpl,C_grpl
  EigenvDecomp,C_grpl,D_grpl,A_grpl,NEIGENV_grpl
  ;---Emissivity
  Stats,emiss(0:nProfiles-1,0:nchan-1),nchan,nProfiles,M_emiss,C_emiss
  EigenvDecomp,C_emiss,D_emiss,A_emiss,NEIGENV_emiss
  ;---Reflectivity
  Stats,Refl(0:nProfiles-1,0:nchan-1),nchan,nProfiles,M_refl,C_refl
  EigenvDecomp,C_refl,D_refl,A_refl,NEIGENV_refl
  ;---Surface parameters
  Stats,WindSp,1,nProfiles,M_WindSp,C_WindSp
  neigenv_windsp  = 1
  A_windsp        = make_array(1,1,value=1.)
  Stats,Tskin,1,nProfiles,M_Tskin,C_Tskin
  neigenv_tskin   = 1
  A_tskin         = make_array(1,1,value=1.)
  Stats,deltaT,1,nProfiles,M_DeltaT,C_DeltaT
  neigenv_deltat  = 1
  A_deltat        = make_array(1,1,value=1.)
  Stats,SfcPres,1,nProfiles,M_SfcPres,C_SfcPres
  neigenv_SfcPres = 1
  A_SfcPres       = make_array(1,1,value=1.)
END


;===============================================================
; Name:		Eigenvdecomp
;
;
; Type:		IDL Subroutine
;
;
; Description:  Performs the Eigenvalue decomposition of a
;               covariance matrix. The eigenvetors will be
;               transformation matrix (A). 
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ---------------------------------------------------
;	- C                  I              Covariance matrix
;	- D                  O              Cov matrx in EOF space
;	- A                  O              Transformation Matrx
;	- NEIGENV            O              Number of eigenvalues
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

PRO Eigenvdecomp,C,D,A,NEIGENV
   ;------------------------------------
   ;  Reduction into Eigenvector space
   ;------------------------------------

   ;---First method using QL algorithm on tridiagonal matrix 
   ;A = C
   ;TRIRED,A,D,E,  /DOUBLE   ;-> compute tridiagonal form of C (into A)
   ;TRIQL, D,E,A,  /DOUBLE   ;-> compute eigenvalues (in D) and eigenvectors (in A)
   ;EIGENVAL = D
   ;EIGENVEC = A
   ;---Second method of decomposition in Eigenvalues using Householder and QL method
   D=EIGENQL(C,/ASCENDING,/DOUBLE,EIGENVECTORS=A,residual=res)
   EIGENVAL = D
   EIGENVEC = A
   ;---Diagonalize the cov matrx
   NEIGENV  = N_ELEMENTS(EIGENVAL)
   Lambda = MATRIX_MULTIPLY(MATRIX_MULTIPLY(transpose(A),C),A)
   ;---or by using:
   Lambda_exact=diag_matrix(D)
   ;---get back matrix C by re-versing the diagonal matrix Lambda (4 verif.)
   C0=MATRIX_MULTIPLY(MATRIX_MULTIPLY(A,Lambda_exact),transpose(A))
   RETURN
END




;===============================================================
; Name:		StatsIndivParams
;
;
; Type:		IDL Subroutine
;
;
; Description:  Generates a mean, variance, standard deviation and 
;               a unity transformation matrix for an individual 
;               parameter.
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================

PRO StatsIndivParams,Param,nProfiles,M_Param,C_Param,D_Param,A_Param,NEIGENV_Param
  M_Param = mean(Param)
  C_Param = variance(Param)
  D_Param = stdev(Param)
  A_Param = 1
  NEIGENV_Param=1
END



;===============================================================
; Name:		STDEV
;
;
; Type:		IDL Subroutine
;
;
; Description:  Computes the standard deviation of a vector.
;
;
; History:
;       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
;
;===============================================================
FUNCTION STDEV, X, Double = Double, NaN = NaN
  ON_ERROR, 2
  ;RETURN, sqrt((moment( X, Double=Double, Maxmoment=2, NaN = NaN ))[1])
  RETURN, stddev( X, Double=Double, NaN = NaN )
END
