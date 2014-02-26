!$Id: utils.f90 3342 2013-10-09 17:26:00Z amims $
!-----------------------------------------------------------------------------------------------
! Name:        utils
! 
! Type:        F90 module
!
! Description:
!       Module that contains various utility subroutines.
!       Based in part on Q. Liu's utility.f90 module located
!       under the CRTM subdirectory.
!
! Modules needed:
!       -Consts
!       -noise
!       -ErrorHandling
!
! Subroutines contained:
!       -ComputeTPW
!       -LINT
!       -ESTET,TVIRT 
!       -Saturate_Humidity
!       -mixRatio_2_e
!       -RelHum_to_mixingratio
!       -ColumIntegr
!       -Mixingratio_to_RelHum
!       -composeEmiss
!       -DeterminNlayEff
!       -DeterminLayIndxOnTopPress
!       -DeterminTopLayer
!       -compute_height
!       -apply_bias
!       -intrpLevs2Lays
!       -interprofile
!       -LayersICWC
!       -AdjustEmiss
!       -interpolprof
!       -LayersHydro
!       -Levs2Laysintrp
!       -DeterminTopLayerFill
!       -DeterminBotLayerFill
!       -Modify_Beamwidth
!       -SeaIceTypeAdjust
! Data type included:
!       -none
! 
! History:
!       2006   S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE utils
  USE Consts
  USE noise
  USE ErrorHandling
  USE mathfcts
  USE type_kinds

  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: ComputeTPW,LINT,ESTET,TVIRT,Saturate_Humidity,mixRatio_2_e,e_2_mixRatio
  PUBLIC :: RelHum_to_mixingratio,ColumIntegr,Mixingratio_to_RelHum,Density_to_mm
  PUBLIC :: composeEmiss,DeterminNlayEff,DeterminLayIndxOnTopPress,DeterminToplayer
  PUBLIC :: compute_height,apply_bias,intrpLevs2Lays,Levs2Laysintrp
  PUBLIC :: interprofile,LayersICWC,AdjustEmiss,interpolprof,LayersHydro
  PUBLIC :: DeterminTopLayerFill,DeterminBotLayerFill,Modify_Beamwidth,SeaIceTypeAdjust

  !---INTRINSIC functions used in this module
  INTRINSIC :: COS,SIZE,PACK,COUNT,FLOAT,INT,LOG,MAX,SIGN,ABS,EXP,ALL

CONTAINS

!===============================================================
! Name:         compute_height
!
!
! Type:         Subroutine
!
!
! Description:  Computes the height profile from the pressure,  
!               temperature and water vapor profiles
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       -Nlev               I             # levels
!       -Nlay               I             # layers
!       -Plev               I             Level pressure grid
!       -Tlay               I             Layer temperature profile
!       -H2O                I             Layer WV profile
!       -Ps                 I             Surface pressure
!       -Z                  O             height profile
!       -Zsfc               I             height at the surface
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  subroutine compute_height(Nlev,Nlay,Plev,Tlay,H2O,Ps,Z,Zsfc)
    IMPLICIT NONE
    INTEGER :: Nlev,Nlay,i,k
    REAL    :: Tlay(Nlay),H2O(Nlay),Plev(Nlev),Z(Nlev),ZZ(Nlev),Ps,PP,TT,dP,dz,ZSfc
    ZZ(1:Nlev) = 0.
    Z(1:Nlev)  = 0.
    zz(1)      = Zsfc
    LayLoop: do i=1,Nlev-1
       if(Plev(i+1).le.Ps) then
          PP=(Plev(i+1)+Plev(i))/2.0
          TT=TLay(i)
          dP=Plev(i+1)-Plev(i)
          dz=dP/PP*287.0*TT/9.8
          ZZ(i+1)=ZZ(i)+dz
       else
          PP=(Ps+Plev(i))/2.0
          TT=Tlay(i)
          dP=Ps-Plev(i)
          dz=dP/PP*287.0*TT/9.8
          ZZ(i+1)=ZZ(i)+dz
          EXIT LayLoop
       endif
    enddo LayLoop
    K=i+1
    if(K.gt.Nlev) K=Nlev
    DO i=1,K
       Z(i)=ZZ(K)-ZZ(i)
    enddo
    return
  end subroutine compute_height


!===============================================================
! Name:         DeterminLayIndxOnTopPress
!
!
! Type:         Subroutine
!
!
! Description:  determine sthe index of the pressure layer just
!               above the topSensPress pressure value.
!
!
! Arguments:
!
!      Name                  Type            Description
!      ---------------------------------------------------
!       - nLay                I              # Layers
!       - pres_lay            I              Layer pressure grid
!       - topSensPress        I              Top pressure
!       - iLayTop             O              Index of press layer just above topSensPress
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE DeterminLayIndxOnTopPress(nLay,pres_lay,topSensPress,iLayTop)
    INTEGER              :: nLay,i,iLayTop
    REAL                 :: topSensPress
    REAL,   DIMENSION(:) :: pres_lay
    iLayTop  = nLay
    Do i=1,nLay
       IF (pres_lay(iLayTop).gt.topSensPress) THEN
          iLayTop = iLayTop - 1
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE DeterminLayIndxOnTopPress

!===============================================================
! Name:         DeterminNlayEff
!
!
! Type:         Subroutine
!
!
! Description:  Determines the effective number of layers (above surface).
!
!
! Arguments:
!
!      Name                  Type             Description
!      ---------------------------------------------------
!      - nLay                 I               # layers
!      - pres_lay             I               Layers pressure grid
!      - SfcPress             I               Surface pressure
!      - nLayEff              O               Effective numnber of layers
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================


  SUBROUTINE DeterminNlayEff(nLay,pres_lay,SfcPress,nLayEff)
    INTEGER               :: nLay,nLayEff,i
    REAL                  :: SfcPress
    REAL,    DIMENSION(:) :: pres_lay
    nLayEff=0
    Do i=1,nLay
       IF (pres_lay(i).le.SfcPress .and. SfcPress/pres_lay(i).gt.1.01) THEN
          nLayEff = nLayEff + 1
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE DeterminNlayEff


!===============================================================
! Name:        DeterminTopLayer
!
!
! Type:         Subroutine
!
!
! Description:  Determines the highest layer in profile with valid value.
!
!
! Arguments:
!
!      Name                 Type             Description
!      ---------------------------------------------------
!       - profile            I               Profile used to determine top layer
!       - nLay               I               # layers in profile
!       - pres_lay           I               index of top layer
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-27-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE DeterminTopLayer(profile,nLay,iTopLay)

    INTEGER                 :: nLay,iTopLay,i
    REAL,   DIMENSION(:)    :: profile
    iTopLay=1
    DO i=1,nLay
       IF (profile(i) .gt. 0) THEN 
          iTopLay = i
          RETURN
       ENDIF
    ENDDO
  END SUBROUTINE DeterminTopLayer


!===============================================================
! Name:        DeterminTopLayerFill
!
!
! Type:         Subroutine
!
!
! Description:  Determines the highest layer in profile with valid value.
!               This version allows the specification of whether zero is a valid value
!
!
! Arguments:
!
!      Name                 Type             Description
!      ---------------------------------------------------
!       - profile            I               Profile used to determine top layer
!       - nLay               I               # layers in profile
!       - iTopLay            I               index of top layer
!       - zeroValid          I               is zero valid (0=no,1=yes)
!
!
! Modules needed:
!       - None
!
!
! History:
!       09-16-2011      C. Grassotti, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE DeterminTopLayerFill(profile,nLay,iTopLay,zeroValid)

    INTEGER                 :: nLay,iTopLay,zeroValid,i
    REAL,   DIMENSION(:)    :: profile
    iTopLay=1
    if(zeroValid .eq. 0)then
       DO i=1,nLay
          IF (profile(i) .gt. 0.) THEN 
             iTopLay = i
             RETURN
          ENDIF
       ENDDO
    else
       DO i=1,nLay
          IF (profile(i) .ge. 0.) THEN 
             iTopLay = i
             RETURN
          ENDIF
       ENDDO

    endif

  END SUBROUTINE DeterminTopLayerFill

!===============================================================
! Name:        DeterminBotLayerFill
!
!
! Type:         Subroutine
!
!
! Description:  Determines the lowest layer in profile with valid value.
!               This version allows the specification of whether zero is a valid value
!
!
! Arguments:
!
!      Name                 Type             Description
!      ---------------------------------------------------
!       - profile            I               Profile used to determine top layer
!       - nLay               I               # layers in profile
!       - iBotLay            I               index of bottom layer
!       - zeroValid          I               is zero valid (0=no,1=yes)
!
!
! Modules needed:
!       - None
!
!
! History:
!       09-16-2011      C. Grassotti, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE DeterminBotLayerFill(profile,nLay,iBotLay,zeroValid)

    INTEGER                 :: nLay,iBotLay,zeroValid,i
    REAL,   DIMENSION(:)    :: profile
    iBotLay=nLay
    if(zeroValid .eq. 0)then
       DO i=nLay,1,-1
          IF (profile(i) .gt. 0) THEN 
             iBotLay = i
             RETURN
          ENDIF
       ENDDO
    else
       DO i=nLay,1,-1
          IF (profile(i) .ge. 0) THEN 
             iBotLay = i
             RETURN
          ENDIF
       ENDDO

    endif

  END SUBROUTINE DeterminBotLayerFill




!===============================================================
! Name:          composeEmiss
!
!
! Type:          Function
!
!
! Description:  Puts together the emissivity given the polarization 
!               chosen. Could be pure V or pure H or a mixture.
!
!
! Arguments:
!
!      Name                Type           Description
!      ---------------------------------------------------
!       - ipol               I            Polarization selection (see definition in CRTM)
!       - evert              I            Vertical pol Emissivity
!       - ehorz              I            Horizontal pol Emissivity
!       - ang                I            Angle
!       - composeEmiss       O            Resulting emissivity
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  REAL FUNCTION composeEmiss(ipol,evert,ehorz,ang)
    INTEGER :: ipol
    REAL    :: evert,ehorz,ang
    !---Data to compute emissivity
    REAL,    DIMENSION(3,5)            ::  zvpol      ! zvpol and zhpol tell emiss
    REAL,    DIMENSION(3,5)            ::  zhpol      ! how much v and h pol to use
    REAL                               ::  cnad2,snad2,ratio,pcc,pc2,ps2
    !ipol:
    !   1 average of vertical and horizontal
    !   2 nominal vertical at nadir, rotating
    !      with view angle
    !   3 nominal horizontal at nadir, rotating
    !      with view angle
    !   4 vertical
    !   5 horizontal

    !---Data needed to compute polarization-dependent emissivity
    zvpol(1:3,1) = (/ 0.5, 0.0, 0.0 /)
    zvpol(1:3,2) = (/ 0.0, 0.0, 1.0 /)
    zvpol(1:3,3) = (/ 0.0, 1.0, 0.0 /)
    zvpol(1:3,4) = (/ 1.0, 0.0, 0.0 /)
    zvpol(1:3,5) = (/ 0.0, 0.0, 0.0 /)
    
    zhpol(1:3,1) = (/ 0.5, 0.0, 0.0 /)
    zhpol(1:3,2) = (/ 0.0, 1.0, 0.0 /)
    zhpol(1:3,3) = (/ 0.0, 0.0, 1.0 /)
    zhpol(1:3,4) = (/ 0.0, 0.0, 0.0 /)
    zhpol(1:3,5) = (/ 1.0, 0.0, 0.0 /)
    
    pcc    = cos(Ang*PI/180.0) 
    pc2    = pcc*pcc
    ps2    = 1.0 - pc2 
    ratio  = (satheight+earthrad)/earthrad
    snad2  = ps2/(ratio*ratio)
    cnad2  = 1.0-snad2
    composeEmiss = evert  * (zvpol(1,ipol) + zvpol(2,ipol)*snad2 + zvpol(3,ipol)*cnad2)   &
         + ehorz * (zhpol(1,ipol) + zhpol(2,ipol)*snad2 + zhpol(3,ipol)*cnad2)
    RETURN
  END FUNCTION composeEmiss


!===============================================================
! Name:          mixRatio_2_e
!
!
! Type:          Function
!
!
! Description:  Converts mixing ratio to partial pressure
!
!
! Arguments:
!
!      Name                Type            Description
!      ---------------------------------------------------
!       - w                 I              mixing ratio
!       - p                 I              total pressure
!       - mixRatio_2_e      O              partial pressure
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION mixRatio_2_e(w,p)
    REAL :: w,p,mixRatio_2_e
    mixRatio_2_e = (w*p)/(.622 + w)
    RETURN
  END FUNCTION mixRatio_2_e

  ! --------------------------------------------------------
  !  INPUT:  e  vapor pressure, p pressure  Pa 
  ! OUTPUT   e_2_mixRatio, g/kg 
  ! --------------------------------------------------------
  Function e_2_mixRatio(e,p)
    REAL :: e,p,e_2_mixRatio
    e_2_mixRatio = ((.622*e)/(p-e))*1000.
  END Function e_2_mixRatio

  Function Mixingratio_to_RelHum(mixingratio,T,P)
    ! --------------------------------------------------------
    !  INPUT:  T  temperature in Kelvin, P  hPa, mixingratio kg/kg 
    ! OUTPUT   Mixingratio_to_RelHum     relative humidity  (0 - 1) 
    ! --------------------------------------------------------
    REAL :: T,P,mixingratio,Mixingratio_to_RelHum
    Mixingratio_to_RelHum=mixingratio*(P*100.0)*  &
         1000.0/(2.16685*287.04)/Saturate_Humidity(T)
  END Function Mixingratio_to_RelHum


  Function ColumIntegr(nLay,P,Psurf,hydr)
    INTEGER            :: nLay,i
    REAL               :: Psurf,ColumIntegr
    REAL, DIMENSION(:) :: P,hydr
    ColumIntegr=0.0
    DO i=1,nlay
       if(P(i) .le. psurf .and. P(i) .gt. 0 .and. hydr(i) .gt. 0.0 ) then 
          ColumIntegr=ColumIntegr+(hydr(i))
       endif
    ENDDO
  END Function ColumIntegr


  Function Saturate_Humidity(T)
    ! --------------------------------------------------------
    !  INPUT:  T  temperature in Kelvin
    ! OUTPUT   Saturate_Humidity  in Pa
    ! --------------------------------------------------------
    REAL :: T,Saturate_Humidity
    Saturate_Humidity=611.0*10.**(7.5*(T-273.15 )/(T-35.85))
  END Function Saturate_Humidity


  Function RelHum_to_mixingratio(Rel,T,P)
    ! --------------------------------------------------------
    !  INPUT:  T  temperature in Kelvin, P  hPa, Rel  0 - 1
    ! OUTPUT   RelHum_to_mixingratio   g/kg 
    ! --------------------------------------------------------
    REAL :: T,P,Rel,RelHum_to_mixingratio
    RelHum_to_mixingratio=2.16685*287.04/(P*100.0)*  &
         Saturate_Humidity(T)*Rel
  END Function RelHum_to_mixingratio


  !----------------------------------------------------
  !   Calculate saturation vapor pressure
  !    INPUT:   Temperature in K
  !    OUTPUT:  ESTET
  !----------------------------------------------------
  FUNCTION ESTET(T)
    REAL :: T,ESTET
    ESTET = 611.0 * 10.0** (7.5 * ( T - 273.15 ) / ( T - 35.85 ))
    RETURN
  END FUNCTION ESTET


  !----------------------------------------------------
  !   Calculate virtual temperature
  !    INPUT:   -T:Temperature in K
  !             -RH: Relative humidity
  !             -P:Pressure
  !    OUTPUT:  ESTET
  !----------------------------------------------------

  FUNCTION TVIRT(RH,T,P)
    REAL :: RH,T,P,TVIRT,ES
    ES = ESTET ( T )
    TVIRT = T * ( 1. + 0.00378 * ( RH * ES / P)  )
    RETURN
  END FUNCTION TVIRT


  !------------------------------------------------------
  !   Convert profile in g/m^3 to integrated kg/m^2
  !    INPUT:  Layer-d  (g/m^3), Level height profile z (m)
  !    OUTPUT: Integrated d (kg/m**2)
  !------------------------------------------------------
  SUBROUTINE Density_to_mm(d,Z,Out_Amount)  
    REAL, DIMENSION(:), INTENT(IN)  :: d,Z
    REAL,               INTENT(OUT) :: Out_Amount
    REAL               :: dZ,Layer_Amount,ScalegTokg
    INTEGER            :: iLay,nLay
    dZ=0.0
    Layer_Amount=0.0
    Out_Amount=0.0
    ScalegTokg=0.001
    nLay=SIZE(Z)-1
    DO iLay=1,nLay
       dZ=ABS(Z(iLay)-Z(iLay+1))
       Layer_Amount=d(iLay)*dZ
       Out_Amount=Out_Amount+(Layer_Amount*ScalegTokg)
    ENDDO
  END SUBROUTINE Density_to_mm

  !------------------------------------------------------
  !   compute column water content
  !    INPUT:   Level-P hPa,  Layer-H2O  g/kg
  !    OUTPUT:   water   kg/m**2
  !------------------------------------------------------
  SUBROUTINE ComputeTPW(P,Psurf,h2oIn,water)  
    !---Input/Output variables
    REAL, DIMENSION(:), INTENT(IN) :: P,h2oIn
    REAL                           :: Psurf,water
    !---Local variables
    REAL, DIMENSION(SIZE(h2oIn))   :: h2o
    REAL               :: dP
    INTEGER            :: i,nl

    nl=SIZE(P)
    water=0.0
    if (all(h2oIn(:) .lt. 0)) then
       water=-999.
       return
    endif
    h2o = h2oIn
    LevLoop: DO i=1,nl
       if (h2oIn(i) .lt. 0) h2o(i)=0.
       if (h2oIn(i+1) .lt. 0) h2o(i+1)=0
       if(i.lt.nl) then
          if(P(i+1).lt.psurf) then
             dP=P(i+1)-P(i)
             water=water+(h2o(i)+h2o(i+1))*0.5*dP
          else
             water=water+(h2o(i)+h2o(i+1))*0.5*(psurf-P(i))
             EXIT LevLoop
          endif
       else
          water=water+h2o(i)*0.5*(psurf-P(i))
       endif
       !print *,i,water/10./9.8
    ENDDO LevLoop
    water=water/10.0/9.8
    return
  END subroutine ComputeTPW

  !--------------------------------------------------------------------
  !  Interpolation subroutine, based on Louis Garand's ITWG 
  !  publicly available routine. Transformed to handle vectors instead
  !  of arrays to facilitate its use in a profiles loop. made the 
  !  interpolation in single precision (as opposed to double). SAB.
  !--------------------------------------------------------------------
  SUBROUTINE LINT (PVLEV,PVI,KNIDIM,KNI,KNO,PPO,PVO)
    !**s/r LINT  - Linear interpolation and constant value extrapolation.
    !
    !
    !Author  : J. Halle *CMDA/AES  December 29, 1998
    ! L. Garand Revised June 2005: to insure that all input values participate in fit
    !    with properly balanced weights when KNI > KNO
    !
    !Arguments
    !     i   PVLEV(KNIDIM)    : Vertical levels (source) 
    !     i   PVI(KNIDIM)      : Vector to be interpolated (source)
    !     i   KNIDIM           : Max dimension of input levels (source) 
    !     i   KNI              : Actual number of input levels (source)
    !     i   KNO              : Number of output levels (destination)
    !     i   PPO(KNO)         : Vertical levels (destination)
    !     o   PVO(KNO)         : Interpolated profiles (destination) 
    !
    !    -------------------
    !*    Purpose: Performs the vertical interpolation in log of pressure
    !              and constant value extrapolation of one-dimensional vectors.
    IMPLICIT NONE
    INTEGER JI,JK,JO,IK,IORDER,K,KNIDIM,KNI,KNO,IUP,IDOWN,IKMOD
    INTEGER II,NSUP,NDOWN,IL(0:KNO+1),NPT
    REAL SUMD,DD(2),SUMW,weight(0:KNI+1),XSUP,XDOWN
    REAL PVLEV(KNIDIM),PPO(KNO),PVO(KNO),PVI(KNIDIM)
    REAL ZPI(0:KNI+1),ZPO(KNO),ZPVI(0:KNI+1),ZP,XI,ZRT,ZP1,ZP2
    !

    ZPI(0    ) = PVLEV(1)/2.
    ZPI(KNI+1) = 2000.0
    !
    !**      1.1 Determine if input pressure levels are in ascending or
    !     .      descending order.
    !     .     -------------------------------------------------------
    !
    IF ( PVLEV(1) .LT. PVLEV(KNI) ) THEN
       IORDER = 1
    ELSE
       IORDER = -1
    ENDIF
    !
    DO JK = 1, KNI
       ZPI(JK) = PVLEV(JK)
    ENDDO
    !
    !**   2.2 Destination levels
    !     .   ------------------
    !
    DO JK = 1, KNO
       ZPO(JK) = PPO(JK)
    ENDDO
    !
    DO JO=1,KNO
       IL(JO) = 0
    ENDDO
    !
    DO JO=1,KNO 
       DO JI=1,KNI
          ZRT = ZPO(JO)
          ZP = ZPI(JI)
          XI = SIGN(1.,IORDER*(ZRT-ZP))
          IL(JO) = IL(JO) + INT(MAX(0.,XI))
       ENDDO
    ENDDO
    !
    IL(0) = IL(1)
    IL(KNO+1)= IL(KNO)
    DO JK = 1, KNI
       ZPVI(JK) = PVI(JK)
    ENDDO
    ZPVI(0    ) = PVI(1  )
    ZPVI(KNI+1) = PVI(KNI)
    DO JO=1,KNO 
       SUMD=0.
       SUMW=0.
       IK = IL(JO)
       ZP = ZPO(JO)
       IUP= IK
       IDOWN=IK+1          
       NPT = 2
       ZP1= ZPI(IUP)
       ZP2= ZPI(IDOWN)
       DD(1) = log(ZP/ZP1)
       DD(2) = log(ZP2/ZP)
       SUMD = DD(1)+DD(2)
       ! neighbors of JO 
       weight(IK)= 1.
       weight(IK+1)= 1.
       ! search for upward participants (excluding neighbors of JO)
       NSUP = IL(JO) -IL(JO-1) -2         
       ! weights defined as 1/(NSUP+1),2/(NSUP+1),...,NSUP/(NSUP+1)
       if(JO.gt.1.AND.NSUP.GT.0)then
          IUP=IL(JO-1)+2
          II=0
          XSUP= 1.0/FLOAT(NSUP+1)
          DO K= IUP,IL(JO)-1
             NPT=NPT+1
             II=II+1
             WEIGHT(K)=FLOAT(II)*XSUP
             SUMW=SUMW+weight(K)
          ENDDO
       endif
       ! search for downward participants (excluding neighbors of JO)
       NDOWN = IL(JO+1) -IL(JO) -2
       if(JO.LT.KNO.AND.NDOWN.GT.0)then
          IDOWN=IL(JO+1)-1
          II=0
          XDOWN= 1.0/FLOAT(NDOWN+1)
          DO K=IL(JO)+2,IDOWN
             NPT=NPT+1
             II=II+1
             WEIGHT(K)=FLOAT(NDOWN-II+1)*XDOWN
             SUMW=SUMW+weight(K)
          ENDDO
       endif
       ! special cases
       IKMOD=0
       IF(NSUP.eq.-1.AND.NDOWN.EQ.0)weight(IK)=0.5*weight(IK)
       IF(NDOWN.eq.-1.and.NSUP.EQ.0)weight(IK+1)= 0.5*weight(IK+1)
       IF(NSUP.eq.-2.and.ndown.eq.0)IKMOD=1
       IF(NSUP.eq.0.and.ndown.eq.-2)IKMOD=1
       IF(NSUP.eq.-1.and.ndown.eq.-1.and.KNI.gt.kno)IKMOD=1
       SUMW=sumW+weight(IK)+weight(IK+1)
       PVO(JO)=0.
       do k =IUP,IDOWN
          JK=K-IUP+1
          ! linear interpolation in log P case        
          if(NPT.eq.2.and.(NSUP+NDOWN).LE.-2.and.ikmod.eq.0) THEN
             PVO(JO) =  PVO(JO)+ (1.-DD(JK)/SUMD)* ZPVI(K)
             !            write(*,88) Jo,K,JK,DD(JK),SUMD,ZPVI(K)
             ! 88         format(' jo k jk dd sumd zpvi:',3i5,4f10.3)
          else
             ! weighted average case
             !            write(*,51)JO,k,JK,ZPVI(K),weight(k),SUMW
             ! 51         format('lo k jk zpvi wg sumw:',3i5,5f10.3)
             PVO(JO) =  PVO(JO)+  ZPVI(K)*weight(K)/SUMW
          ENDIF
       ENDDO
    ENDDO
    RETURN
  END SUBROUTINE LINT


!===============================================================
! Name:         apply_bias
!
!
! Type:         Subroutine
!
!
! Description:  
!
!
! Arguments:
!
!         Name                    Type              Description
!      ---------------------------------------------------
!         iToApply                   I              Where to apply bias (-2: do not apply, -1: apply everywhere.
!                                                   control over which channels be corrected for each surface type 
!                                                   would be controlled in following vectors:
!         applyBias_oc_byChan        I              Flag vector of channels where to apply bias over ocean
!         applyBias_ic_byChan        I              Flag vector of channels where to apply bias over sea ice
!         applyBias_ld_byChan        I              Flag vector of channels where to apply bias over land
!         applyBias_sn_byChan        I              Flag vector of channels where to apply bias over snow
!         applyBias_ew_byChan        I              Flag vector of channels where to apply bias over everywhere
!
! Modules needed:
!       - None
!
!
! History:
!       08-31-2007      Cezar Kongoli
!       10-23-2007      Sid Boukabara. Added capacity to ignore bias application altogether (no boas file provided)
!       11-15-2007      Sid Boukabara. Modified the code for applying the bias (-2: not apply, -1 everywhere, >=0 surface type)
!       11-22-2007      Sid Boukabara. Added capacity to choose which channels to correct for bias, by surface type
!
!===============================================================
  SUBROUTINE apply_bias(fmTB,ecfmTB,iscPos,nscposRad,nscposBias,posBias,posSlope,  & 
                      posIntercept,nchan,iToApply,iApplMethod,iSfc,errReadingBias,&
                      applyBias_oc_byChan,applyBias_ic_byChan,applyBias_ld_byChan,&
                      applyBias_sn_byChan)
    INTEGER                               :: iToApply,iApplMethod,iscPos,nchan,iSfc,ichan,nscposRad,nscposBias,errReadingBias
    REAL                                  :: rposFactor
    INTEGER                               :: indPos
    REAL,    DIMENSION(:,:),  POINTER     :: posBias,posSlope,posIntercept
    REAL,    DIMENSION(:)                 :: fmTB,ecfmTB
    INTEGER, DIMENSION(:)                 :: applyBias_oc_byChan,applyBias_ic_byChan,applyBias_ld_byChan
    INTEGER, DIMENSION(:)                 :: applyBias_sn_byChan

    ecfmTB(1:nchan) = fmTB(1:nchan) 
    IF (iToApply .eq. -2) return ! do not apply bias anywhere
    !----Scale hi-resolution scan positions to low res, or vice versa
    rposFactor = (nscposRad*1.0) / (nscposBias*1.0)
    indPos     = INT( (iscPos-1) / rposFactor ) + 1
    
    !---Calibrate
    !---Test if we had invalid bias information (which case, no bias is applied)
    IF (errReadingBias.eq.1) THEN
       print *, 'Warning: Bias application mode requested. No valid bias info found.'
       return
    ENDIF
    !---Assume that if nposBias=1 then no scanPos-dependence of the bias
    IF (nscposBias.eq.1) indPos=1
    IF (indPos.gt.nscposBias) CALL ErrHandl(ErrorType,Err_ScanPosPb,'(ipos > nscan)')
    IF (indPos.lt. 1)         CALL ErrHandl(ErrorType,Err_ScanPosPb,'(ipos <1)')
    !--------------------------------------------------------------------------------
    ! Apply bias everywhere (which channels to apply depend on surface type)
    !--------------------------------------------------------------------------------
    IF (iToApply .eq. -1) THEN ! apply everywhere
       do ichan=1,nchan
          IF (iSfc .eq. OC_TYP .and. applyBias_oc_byChan(ichan).eq.1) THEN 
             IF (iApplMethod .eq.0) ecfmTB(ichan)=fmTB(ichan)-posBias(indPos,ichan)
             IF (iApplMethod .eq.1) ecfmTB(ichan)=fmTB(ichan)*posSlope(indPos,ichan)+posIntercept(indPos,ichan)
          ENDIF
          IF (iSfc .eq. SEAICE_TYP .and. applyBias_ic_byChan(ichan).eq.1) THEN 
             IF (iApplMethod .eq.0) ecfmTB(ichan)=fmTB(ichan)-posBias(indPos,ichan)
             IF (iApplMethod .eq.1) ecfmTB(ichan)=fmTB(ichan)*posSlope(indPos,ichan)+posIntercept(indPos,ichan)
          ENDIF
          IF (iSfc .eq. LD_TYP .and. applyBias_ld_byChan(ichan).eq.1) THEN 
             IF (iApplMethod .eq.0) ecfmTB(ichan)=fmTB(ichan)-posBias(indPos,ichan)
             IF (iApplMethod .eq.1) ecfmTB(ichan)=fmTB(ichan)*posSlope(indPos,ichan)+posIntercept(indPos,ichan)
          ENDIF
          IF (iSfc .eq. SNOW_TYP .and. applyBias_sn_byChan(ichan).eq.1) THEN 
             IF (iApplMethod .eq.0) ecfmTB(ichan)=fmTB(ichan)-posBias(indPos,ichan)
             IF (iApplMethod .eq.1) ecfmTB(ichan)=fmTB(ichan)*posSlope(indPos,ichan)+posIntercept(indPos,ichan)
          ENDIF
       enddo
    ENDIF
  END SUBROUTINE apply_bias



  SUBROUTINE AdjustBiasAdHoc(ecfmTB,nchan,iSfc,iscPos,applyBias_oc_byChan,applyBias_ic_byChan,applyBias_ld_byChan,&
       applyBias_sn_byChan,sensor_id)
    INTEGER                               :: nchan,iSfc,iscPos,ichan,sensor_id
    REAL,    DIMENSION(:)                 :: ecfmTB
    INTEGER, DIMENSION(:)                 :: applyBias_oc_byChan,applyBias_ic_byChan
    INTEGER, DIMENSION(:)                 :: applyBias_sn_byChan,applyBias_ld_byChan

    !---Ad-hoc corrections to N18 TBs
    IF (sensor_id .eq. sensor_id_n18) THEN
       do ichan=1,nchan
          IF (iSfc .eq. OC_TYP .and. applyBias_oc_byChan(ichan).eq.1) THEN 
             ecfmTB(1)=ecfmTB(1)-1.
             ecfmTB(2)=ecfmTB(2)-3.
             ecfmTB(3)=ecfmTB(3)-1.
             ecfmTB(4)=ecfmTB(4)-0.5
             ecfmTB(5)=ecfmTB(5)-1.5
             ecfmTB(6)=ecfmTB(6)+0.2
             ecfmTB(7)=ecfmTB(7)+0.1
             ecfmTB(8)=ecfmTB(8)+0.2
             ecfmTB(9)=ecfmTB(9)+0.2
             ecfmTB(10)=ecfmTB(10)+0.1
             ecfmTB(15)=ecfmTB(15)-4.
             !ecfmTB(16)=ecfmTB(16)-4.
             !ecfmTB(17)=ecfmTB(17)-1.
             !ecfmTB(18)=ecfmTB(18)-4.
             !ecfmTB(19)=ecfmTB(19)-2.
          ENDIF
          IF (iSfc .eq. SEAICE_TYP .and. applyBias_ic_byChan(ichan).eq.1) THEN 
             ecfmTB(1)=ecfmTB(1)-1.
             ecfmTB(2)=ecfmTB(2)-3.
             ecfmTB(3)=ecfmTB(3)-1.
             ecfmTB(4)=ecfmTB(4)-0.5
             ecfmTB(5)=ecfmTB(5)-1.5
             ecfmTB(6)=ecfmTB(6)+0.2
             ecfmTB(7)=ecfmTB(7)+0.1
             ecfmTB(8)=ecfmTB(8)+0.2
             ecfmTB(9)=ecfmTB(9)+0.2
             ecfmTB(10)=ecfmTB(10)+0.1
             ecfmTB(15)=ecfmTB(15)-4.
             !ecfmTB(16)=ecfmTB(16)-4.
             !ecfmTB(17)=ecfmTB(17)-1.
             !ecfmTB(18)=ecfmTB(18)-4.
             !ecfmTB(19)=ecfmTB(19)-2.
          ENDIF
          IF (iSfc .eq. LD_TYP .and. applyBias_ld_byChan(ichan).eq.1) THEN 
             ecfmTB(1)=ecfmTB(1)-1.
             ecfmTB(2)=ecfmTB(2)-3.
             ecfmTB(3)=ecfmTB(3)-1.
             ecfmTB(4)=ecfmTB(4)-0.5
             ecfmTB(5)=ecfmTB(5)-1.5
             ecfmTB(6)=ecfmTB(6)+0.2
             ecfmTB(7)=ecfmTB(7)+0.1
             ecfmTB(8)=ecfmTB(8)+0.2
             ecfmTB(9)=ecfmTB(9)+0.2
             ecfmTB(10)=ecfmTB(10)+0.1
             ecfmTB(15)=ecfmTB(15)-4.
             !ecfmTB(16)=ecfmTB(16)-4.
             !ecfmTB(17)=ecfmTB(17)-1.
             !ecfmTB(18)=ecfmTB(18)-4.
             !ecfmTB(19)=ecfmTB(19)-2.
          ENDIF
          IF (iSfc .eq. SNOW_TYP .and. applyBias_sn_byChan(ichan).eq.1) THEN 
             ecfmTB(1)=ecfmTB(1)-1.
             ecfmTB(2)=ecfmTB(2)-3.
             ecfmTB(3)=ecfmTB(3)-1.
             ecfmTB(4)=ecfmTB(4)-0.5
             ecfmTB(5)=ecfmTB(5)-1.5
             ecfmTB(6)=ecfmTB(6)+0.2
             ecfmTB(7)=ecfmTB(7)+0.1
             ecfmTB(8)=ecfmTB(8)+0.2
             ecfmTB(9)=ecfmTB(9)+0.2
             ecfmTB(10)=ecfmTB(10)+0.1
             ecfmTB(15)=ecfmTB(15)-4.
             !ecfmTB(16)=ecfmTB(16)-4.
             !ecfmTB(17)=ecfmTB(17)-1.
             !ecfmTB(18)=ecfmTB(18)-4.
             !ecfmTB(19)=ecfmTB(19)-2.
          ENDIF
       enddo
    ENDIF

  END SUBROUTINE AdjustBiasAdHoc




!===============================================================
! Name:         intrpLevs2Lays
!
!
! Type:         F90 Subroutine
!
!
! Description:  Converts any atmospheric profiles given in LAYERS
!               to any pressure grid in LAYERS
!
!
! Arguments:
!
!      Name                 Type       Description
!      ---------------------------------------------------
!       - XProfIn            I          Profile to interpolate
!       - PresLevsIn         I          Grid of input profile levels    
!       - nLevOut            I          Number of levels to intrp to
!       - PresLevsOut        I          Grid of levels to intrp to
!       - PresBot            O          Highest pressure of valid data (closest to sfc)
!       - PresTop            O          Lowest pressure of valid data (closest to toa)
!       - PresLayOut         I          Pressure layer grid to transform intrpd levels to 
!       - XProfOut           O          Output profile at pressure layers
!       - qc                 O          Output qc flag
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-17-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================


    SUBROUTINE intrpLevs2Lays(XProfIn,PresLevsIn,nLevOut,PresLevsOut,PresBot,PresTop,PresLayOut,XProfOut,qc)

      IMPLICIT NONE
      !---I/O variables   
      INTEGER,                 INTENT(IN)   :: nLevOut
      REAL,    DIMENSION(:),   INTENT(IN)   :: XProfIn
      REAL,    DIMENSION(:),   INTENT(IN)   :: PresLevsIn
      REAL,    DIMENSION(:),   INTENT(IN)   :: PresLevsOut
      REAL,                    INTENT(OUT)  :: PresBot,PresTop
      REAL,    DIMENSION(:),   INTENT(IN)   :: PresLayOut
      REAL,    DIMENSION(:),   INTENT(OUT)  :: XProfOut
      INTEGER(2), DIMENSION(:),INTENT(OUT)  :: qc
      !---Local variables
      INTEGER                               :: nVal_Levs
      INTEGER                               :: i,iLev
      INTEGER, DIMENSION(:),   ALLOCATABLE  :: idx_inLevs
      REAL                                  :: PresDiff
      REAL,    DIMENSION(:),   ALLOCATABLE  :: XProfTrun,PresProfTrun
      REAL,    DIMENSION(nLevOut)           :: XProfLevl
      
      !---Determine number of levels with good data 
      nVal_Levs = COUNT(XProfIn(:) .ge. 0.)
      IF (nVal_Levs .lt. 10) THEN
         XProfOut(:) = -999.
         qc(1) = 1
         RETURN
      ENDIF
      IF (nVal_Levs .ge. 10) THEN
         !---Create index of array elements with good data, these are the elements to interpolate
         ALLOCATE (idx_inLevs(nVal_Levs),XProfTrun(nVal_Levs),PresProfTrun(nVal_Levs))
         idx_inLevs = PACK( (/(i,i=1,SIZE(XProfIn(:)))/), (XProfIn(:) .ge. 0.))
         PresTop = PresLevsIn(idx_inLevs(1))
         PresBot = PresLevsIn(idx_inLevs(nVal_Levs))
         XProfTrun(:)    = XProfIn(idx_inLevs)
         PresProfTrun(:) = PresLevsIn(idx_inLevs)
         !---Check for gaps in profile and depth of profile
         DO iLev=1,nVal_levs-1
            PresDiff = PresProfTrun(iLev+1)-PresProfTrun(iLev)
            IF (PresDiff .gt. 100) qc(1) = 1 
         ENDDO
         !---Call linear interpolation subroutine
         CALL LINT(PresProfTrun,XProfTrun,nVal_Levs,nVal_Levs,nLevOut,PresLevsOut,XProfLevl)
         !---Convert to layers
         XProfOut(:)=-999.
         LayLoop: DO iLev=1,nLevOut-1
            IF (PresLayOut(iLev) .lt. PresTop .or. PresLayOut(iLev) .gt. PresBot) CYCLE LayLoop
              xProfOut(iLev) = (xProfLevl(iLev)+xProfLevl(iLev+1))/2
         ENDDO LayLoop
         DEALLOCATE(idx_inLevs,XProfTrun,PresProfTrun)        
      ENDIF
      RETURN
    END SUBROUTINE intrpLevs2Lays
 

  !---------------------------------------------------------------
  ! Name:         Levs2Laysintrp
  !
  !
  ! Type:         F90 Subroutine
  !
  !
  ! Description:  Converts any atmospheric profiles given in LAYERS
  !               to any pressure grid in LAYERS.If data is not defined
  !               at certain output pressure layers, previous layer value
  !               is used.
  !
  !
  ! Arguments:
  !
  !      Name                 Type       Description
  !      ---------------------------------------------------
  !       - XProfIn            I          Profile to interpolate
  !       - PresLevsIn         I          Grid of input profile levels
  !       - psfc               I          Surface Pressure  
  !       - presthr            I          Threshold pressure
  !       - nLevOut            I          Number of levels to intrp to
  !       - PresLevsOut        I          Grid of levels to intrp to
  !       - PresBot            O          Highest pressure of valid data (closest to sfc)
  !       - PresTop            O          Lowest pressure of valid data (closest to toa)
  !       - PresLayOut         I          Pressure layer grid to transform intrpd levels to 
  !       - XProfOut           O          Output profile at pressure layers
  !       - fg                 I          Flag that defined the values below PresBot: 
  !                                       -1:-999.0,0:default,1:linear,2:log
  !       - qc                 O          Output qc flag
  !
  !
  ! Modules needed:
  !       - None
  !
  !
  ! History:
  !       03-17-2008      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
  !       08-17-2010      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/ORA
  !
  !---------------------------------------------------------------

  SUBROUTINE Levs2Laysintrp(XProfIn,PresLevsIn,psfc,presthr,nLevOut,PresLevsOut,PresBot,PresTop,PresLayOut,XProfOut,fg,qc)

    IMPLICIT NONE
    !---I/O variables   
    INTEGER,                 INTENT(IN)   :: nLevOut
    INTEGER,                 INTENT(IN)   :: fg
    REAL,    DIMENSION(:),   INTENT(IN)   :: XProfIn
    REAL,    DIMENSION(:),   INTENT(IN)   :: PresLevsIn
    REAL,    DIMENSION(:),   INTENT(IN)   :: PresLevsOut
    REAL,                    INTENT(OUT)  :: PresBot,PresTop
    REAL,                    INTENT(IN)   :: psfc,presthr
    REAL,    DIMENSION(:),   INTENT(IN)   :: PresLayOut
    REAL,    DIMENSION(:),   INTENT(OUT)  :: XProfOut
    INTEGER(2), DIMENSION(:),INTENT(OUT)  :: qc
    !---Local variables
    INTEGER                               :: nVal_Levs
    INTEGER                               :: i,iLev
    INTEGER, DIMENSION(:),   ALLOCATABLE  :: idx_inLevs
    REAL                                  :: PresDiff
    REAL,    DIMENSION(:),   ALLOCATABLE  :: XProfTrun,PresProfTrun
    REAL,    DIMENSION(nLevOut)           :: XProfLevl
    REAL                                  :: m1,qo,po
    INTEGER                               :: mfg

    !---initializing Variables 
    m1=0.0
    qo=0.0
    po=0.0
    mfg=0
    !---Determine number of levels with good data 
    nVal_Levs = COUNT(XProfIn(:) .ge. 0.)
    IF (nVal_Levs .lt. 10) THEN
       XProfOut(:) = -999.
       qc(1) = 1
       RETURN
    ENDIF
    IF (nVal_Levs .ge. 10) THEN
       !---Create index of array elements with good data, these are the elements to interpolate
       ALLOCATE (idx_inLevs(nVal_Levs),XProfTrun(nVal_Levs),PresProfTrun(nVal_Levs))
       idx_inLevs = PACK( (/(i,i=1,SIZE(XProfIn(:)))/), (XProfIn(:) .ge. 0.))
       PresTop = PresLevsIn(idx_inLevs(1))
       PresBot = PresLevsIn(idx_inLevs(nVal_Levs))
       XProfTrun(:)    = XProfIn(idx_inLevs)
       PresProfTrun(:) = PresLevsIn(idx_inLevs)
       !---Check for gaps in profile and depth of profile
       DO iLev=1,nVal_levs-1
          PresDiff = PresProfTrun(iLev+1)-PresProfTrun(iLev)
          IF (PresDiff .gt. 100) qc(1) = 1 
       ENDDO
       !---Call linear interpolation subroutine
       CALL LINT(PresProfTrun,XProfTrun,nVal_Levs,nVal_Levs,nLevOut,PresLevsOut,XProfLevl)
       !---Convert to layers
       XProfOut(:)=0.0
       LayLoop: DO iLev=1,nLevOut-1
          xProfOut(iLev) = (xProfLevl(iLev)+xProfLevl(iLev+1))/2

          IF(fg .EQ. 1 .AND. PresLayOut(iLev) .GT. psfc .AND. mfg .EQ. 0) THEN
             m1=(xProfOut(iLev) - xProfOut(iLev-1))/(PresLayOut(iLev)-PresLayOut(iLev-1))
             qo=xProfOut(iLev)
             po=PresLayOut(iLev)
             mfg=1           
          ENDIF
          IF(fg .EQ. 2 .AND. PresLayOut(iLev) .GT. psfc .AND. mfg .EQ. 0) THEN
             m1=(log(xProfOut(iLev))-log(xProfOut(iLev-1)))/(PresLayOut(iLev)-PresLayOut(iLev-1))
             qo=xProfOut(iLev)
             po=PresLayOut(iLev)
             mfg=1
          ENDIF
       ENDDO LayLoop

       DO iLev=1,nLevOut-1
          IF (fg .EQ. -1 .AND. psfc .LT. presthr .AND. PresLayOut(iLev) .GT. psfc) THEN
             xProfOut(iLev)=-999.0
          ENDIF
          IF(fg .EQ. 1 .AND. PresLayOut(iLev) .GT. PresBot) THEN
             xProfOut(iLev)=qo+m1*(PresLayOut(iLev) - po)
          ENDIF
          IF(fg .EQ. 2 .AND. PresLayOut(iLev) .GT. PresBot) THEN
             xProfOut(iLev)=qo*exp(m1*( PresLayOut(iLev) - po))
          ENDIF
       ENDDO

       DEALLOCATE(idx_inLevs,XProfTrun,PresProfTrun)        
    ENDIF
    RETURN
  END SUBROUTINE Levs2Laysintrp



!===============================================================
! Name:         interprofile
!
!
! Type:         F90 Subroutine
!
!
! Description:  Interpolates a parameter profile
!               to a given pressure grid
!
!
! Arguments:
!
!      Name                Type        Description
!      ---------------------------------------------------
!       - XProfIn            I          Profile to interpolate
!       - PressIn            I          Grid of input profile levels    
!       - nLevOut            I          Number of vertical levels to interpolate to
!       - PressOut           I          Grid of levels to interpolate to
!       - PresBot            O          Highest pressure of valid data (closest to sfc)
!       - PresTop            O          Lowest pressure of valid data (closest to top)
!       - XProfOut           O          Output parameter profile at desired grid levels
!       - qc                 O          Output qc flag
!
!
! Modules needed:
!       - None
!
!
! History:
!       04-18-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE interprofile(XProfIn,PressIn,nLevOut,PressOut,PresBot,PresTop,XProfOut,qc)
    IMPLICIT NONE
    !---I/O variables   
    INTEGER,                 INTENT(IN)   :: nLevOut
    REAL,    DIMENSION(:),   INTENT(IN)   :: XProfIn
    REAL,    DIMENSION(:),   INTENT(IN)   :: PressIn
    REAL,    DIMENSION(:),   INTENT(IN)   :: PressOut
    REAL,                    INTENT(OUT)  :: PresBot,PresTop
    REAL,    DIMENSION(:),   INTENT(OUT)  :: XProfOut
    INTEGER(2), DIMENSION(:),   INTENT(OUT)  :: qc

    !---Local variables
    INTEGER                               :: nVal_Levs
    INTEGER                               :: i,iLev
    INTEGER, DIMENSION(:),   ALLOCATABLE  :: idx_inLevs
    REAL                                  :: PresDiff
    REAL,    DIMENSION(:),   ALLOCATABLE  :: XProfTrun,PresProfTrun
    REAL,    DIMENSION(nLevOut)           :: XProfLevl

    !---Determine number of vertical levels with good data 
    nVal_Levs = COUNT(XProfIn(:) .ge. 0.)
    IF (nVal_Levs .lt. 2) THEN
       XProfOut(:) = -999.
       qc(1) = 1
       RETURN
    ENDIF
    IF (nVal_Levs .ge. 2) THEN
       !---Create index of array elements with good data, these are the elements to interpolate
       ALLOCATE (idx_inLevs(nVal_Levs),XProfTrun(nVal_Levs),PresProfTrun(nVal_Levs))
       idx_inLevs      = PACK( (/(i,i=1,SIZE(XProfIn(:)))/), (XProfIn(:) .ge. 0))
       PresTop         = PressIn(idx_inLevs(1))
       PresBot         = PressIn(idx_inLevs(nVal_Levs))
       XProfTrun(:)    = XProfIn(idx_inLevs)
       PresProfTrun(:) = PressIn(idx_inLevs)
       !---Check for gaps in profile and depth of profile
       DO iLev=1,nVal_levs-1
          PresDiff = PresProfTrun(iLev+1)-PresProfTrun(iLev)
          IF (PresDiff .gt. 100) qc(1) = 1 
       ENDDO
       !---Call a linear interpolation subroutine
       CALL LINT(PresProfTrun(:),XProfTrun(:),nVal_Levs,nVal_Levs,nLevOut,PressOut,XProfLevl)
       !---Fill the output profile
       LayLoop: DO iLev=1,nLevOut
          IF (PressOut(iLev) .gt. PresBot .or. PressOut(iLev) .lt. PresTop) THEN
             XProfOut(iLev)=-999
          ELSE
             xProfOut(iLev)= XProfLevl(iLev)
          ENDIF
       ENDDO LayLoop
       DEALLOCATE(idx_inLevs,XProfTrun,PresProfTrun)        
    ENDIF
    RETURN
  END SUBROUTINE interprofile


  !---------------------------------------------------------------
  ! Name:         interpolprof
  !
  !
  ! Type:         F90 Subroutine
  !
  !
  ! Description:  Interpolates a parameter profile
  !               to a given pressure grid. All interpolated
  !               values are greater than zero.
  !
  !
  ! Arguments:
  !
  !      Name                Type        Description
  !      ---------------------------------------------------
  !       - XProfIn            I          Profile to interpolate
  !       - PressIn            I          Grid of input profile levels    
  !       - nLevOut            I          Number of vertical levels to interpolate to
  !       - PressOut           I          Grid of levels to interpolate to
  !       - PresBot            O          Highest pressure of valid data (closest to sfc)
  !       - PresTop            O          Lowest pressure of valid data (closest to top)
  !       - XProfOut           O          Output parameter profile at desired grid levels
  !       - qc                 O          Output qc flag
  !
  !
  ! Modules needed:
  !       - None
  !
  !
  ! History:
  !       08-17-2010      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
  !
  !---------------------------------------------------------------

  SUBROUTINE interpolprof(XProfIn,PressIn,psfc,nLevOut,PressOut,PresBot,PresTop,XProfOut,qc)
    IMPLICIT NONE
    !---I/O variables   
    INTEGER,                 INTENT(IN)   :: nLevOut
    REAL,    DIMENSION(:),   INTENT(IN)   :: XProfIn
    REAL,    DIMENSION(:),   INTENT(IN)   :: PressIn
    REAL,    DIMENSION(:),   INTENT(IN)   :: PressOut
    REAL,                    INTENT(OUT)  :: PresBot,PresTop
    REAL,    DIMENSION(:),   INTENT(OUT)  :: XProfOut
    INTEGER(2), DIMENSION(:),   INTENT(OUT)  :: qc
    REAL,                    INTENT(IN)   :: psfc

    !---Local variables
    INTEGER                               :: nVal_Levs
    INTEGER                               :: i,iLev
    INTEGER, DIMENSION(:),   ALLOCATABLE  :: idx_inLevs
    REAL                                  :: PresDiff
    REAL,    DIMENSION(:),   ALLOCATABLE  :: XProfTrun,PresProfTrun
    REAL,    DIMENSION(nLevOut)           :: XProfLevl

    !---Determine number of vertical levels with good data 
    nVal_Levs = COUNT(XProfIn(:) .ge. 0.)
    PresBot = psfc
    IF (nVal_Levs .lt. 2) THEN
       XProfOut(:) = -999.
       qc(1) = 1
       RETURN
    ENDIF
    IF (nVal_Levs .ge. 2) THEN
       !---Create index of array elements with good data, these are the elements to interpolate
       ALLOCATE (idx_inLevs(nVal_Levs),XProfTrun(nVal_Levs),PresProfTrun(nVal_Levs))
       idx_inLevs      = PACK( (/(i,i=1,SIZE(XProfIn(:)))/), (XProfIn(:) .ge. 0))
       PresTop         = PressIn(idx_inLevs(1))
       PresBot         = PressIn(idx_inLevs(nVal_Levs))
       XProfTrun(:)    = XProfIn(idx_inLevs)
       PresProfTrun(:) = PressIn(idx_inLevs)
       PresBot = psfc
       !---Check for gaps in profile and depth of profile
       DO iLev=1,nVal_levs-1
          PresDiff = PresProfTrun(iLev+1)-PresProfTrun(iLev)
          IF (PresDiff .gt. 100) qc(1) = 1 
       ENDDO
       !---Call a linear interpolation subroutine
       CALL LINT(PresProfTrun(:),XProfTrun(:),nVal_Levs,nVal_Levs,nLevOut,PressOut,XProfLevl)
       !---Fill the output profile
       LayLoop: DO iLev=1,nLevOut
       IF(XProfLevl(iLev) .GE. 0) THEN
          xProfOut(iLev)= XProfLevl(iLev)
       ELSE
          xProfOut(iLev)=0.0
       ENDIF
    ENDDO LayLoop
       DEALLOCATE(idx_inLevs,XProfTrun,PresProfTrun)        
    ENDIF
    RETURN
  END SUBROUTINE interpolprof


!===============================================================
! Name:        LayersICWC
!
!
! Type:         F90 Subroutine
!
!
! Description:  Compute the integrated cloud water content at 
!               layers
!
!
! Arguments:
!
!      Name                Type           Description
!      ---------------------------------------------------
!       - ProfIn             I         Input Profile (g/kg)
!       - PressIn            I         Pressure levels of input profile (hPa)     
!       - XProfOut           O         Output parameter profile (mm)
!
!
! Modules needed:
!       - None
!
!
! History:
!       05-30-2008      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================
  
SUBROUTINE LayersICWC(ProfIn,PressIn,ProfICWC)  
    IMPLICIT NONE
    !---I/O variables   
    REAL,    DIMENSION(:),   INTENT(IN)   :: ProfIn
    REAL,    DIMENSION(:),   INTENT(IN)   :: PressIn
    REAL,    DIMENSION(:),   INTENT(OUT)  :: ProfICWC
    !---Input/Output variables
    INTEGER                               :: nLev,iLev
    REAL                                  :: dPress

    nLev=SIZE(ProfIn)
    DO iLev=1,nLev-1
       dPress=PressIn(iLev+1)-PressIn(iLev)
       ProfICWC(iLev)=(ProfIn(iLev)+ProfIn(iLev+1))*0.5*dPress
       ProfICWC(iLev)=ProfICWC(iLev)/10.0/9.8
    ENDDO
END SUBROUTINE LayersICWC


!--------------------------------------------------------------------------
! Name:        LayersHydro
!
!
! Type:         F90 Subroutine
!
!
! Description:  Compute the integrated cloud water content at 
!               layers
!
!
! Arguments:
!
!      Name                Type           Description
!      ---------------------------------------------------
!       - ProfIn             I         Input Profile (g/kg)
!       - PressIn            I         Pressure levels of input profile (hPa)     
!       - XProfOut           O         Output parameter profile (mm)
!
!
! Modules needed:
!       - None
!
!
! History:
!       08-17-2010      Flavio Iturbide-Sanchez, IMSG Inc @ NOAA/NESDIS/STAR
!
!--------------------------------------------------------------------------

SUBROUTINE LayersHydro(ProfIn,psfc,PressIn,ProfICWC)  
  IMPLICIT NONE
  !---I/O variables   
  REAL,    DIMENSION(:),   INTENT(IN)   :: ProfIn
  REAL,    DIMENSION(:),   INTENT(IN)   :: PressIn
  REAL,    DIMENSION(:),   INTENT(OUT)  :: ProfICWC
  REAL,                    INTENT(IN)   :: psfc
  REAL,    DIMENSION(SIZE(ProfIn))      :: h2o
  !---Input/Output variables
  INTEGER                               :: nLev,iLev
  REAL                                  :: dPress
  
  nLev=SIZE(ProfIn)
  ProfICWC=0.0
  h2o(:)=ProfIn(:)

  DO iLev=1,nLev-1
     IF(ProfIn(iLev)   .LT. 0.0) h2o(iLev)  = 0.0
     IF(ProfIn(iLev+1) .LT. 0.0) h2o(iLev+1)= 0.0
     
     IF(PressIn(iLev+1) .LE. psfc) THEN
        dPress=PressIn(iLev+1)-PressIn(iLev)
        ProfICWC(iLev)=(h2o(iLev)+h2o(iLev+1))*0.5*dPress
        ProfICWC(iLev)=ProfICWC(iLev)/10.0/9.8
     ELSE
        ProfICWC(iLev)=0.0
     ENDIF
     
  ENDDO
END SUBROUTINE LayersHydro



!===============================================================
! Name:         AdjustEmiss
!
!
! Type:         Subroutine
!
!
! Description:  Sets fill valued emissivities to valid values
!               based on similar channels 
!              
! Modules needed:
!       - None
!
! Subroutines called:
!       - LINT
!       - GRNF
!
!
! History:
!       11-28-2007      Kevin Garrett, IMSG Inc @ NOAA/NESDIS/ORA
!       01-30-2009      Kevin Garrett:
!                        -Moved to utils.f90 and use interpolation
!                        method for amsua/mhs emissivity adjustment
!                        -Added cFreq to argument list
!       07-25-2011      Kevin Garrett:
!                        -Added TMI adjustment and 22 GHz interpolation
!
!===============================================================

  SUBROUTINE AdjustEmiss(EmissIn,EmissOut,nChan,cFreq,sensor_id)
    !---I/O Variables
    INTEGER,               INTENT(IN)  :: nChan,sensor_id
    REAL,    DIMENSION(:), INTENT(IN)  :: EmissIn
    REAL,    DIMENSION(:), INTENT(OUT) :: EmissOut
    REAL,    DIMENSION(:)              :: cFreq
    !---Internal Variables
    INTEGER                            :: cntValid
    INTEGER, PARAMETER                 :: nChanAMSU=15,nChanMHS=5
    REAL,    DIMENSION(:), ALLOCATABLE :: Emiss2Intp,Freq2Intp
    REAL,    DIMENSION(:), ALLOCATABLE :: Emiss2Intp2,Freq2Intp2

    INTEGER                            :: iChan
    REAL,    PARAMETER                 :: scaleFac=0.0005
    REAL                               :: rn
    REAL,    DIMENSION(nChan)          :: Em,random_noise
    REAL,    DIMENSION(4)              :: EmTMIBase_H
    REAL,    DIMENSION(5)              :: EmTMIBase_V
    

    EmissOut=EmissIn
    Em=EmissIn
    !---Calculate random noise which will be added to some channels
    DO iChan=1,nChan
       call GRNF(rn)
       random_noise(iChan) = rn*scaleFac
    ENDDO
    !---Adjust AMSU/MHS Channels
    IF (sensor_id .eq. sensor_id_n18 .or. sensor_id .eq. sensor_id_metopA .or. &
        sensor_id .eq. sensor_id_n19 .or. sensor_id .eq. sensor_id_metopB) THEN
       IF (Em(1) .ge. 1 .or. Em(1) .le. 0 .or. Em(15) .le. 0 .or. Em(15) .ge. 1) THEN 
          EmissOut=-999.
          RETURN
       ENDIF
       IF (Em(2) .gt. 1 .or. Em(2) .le. 0) EmissOut(2)=EmissOut(1)-random_noise(2)
       IF (Em(16) .gt. 0 .or. Em(16) .lt. 1) THEN
          EmissOut(15:16)=(Em(15)+Em(16))/2
       ELSE
          EmissOut(16)=Em(15)
       ENDIF
       IF (EmissIn(3) .gt. 0 .and. EmissIn(3) .lt. 1) THEN
          cntValid = COUNT(EmissIn(3:15) .eq. EmissIn(3:15))
          ALLOCATE(Emiss2Intp(2),Freq2Intp(2),Emiss2Intp2(cntValid),Freq2Intp2(cntValid))
          Emiss2Intp(1)=EmissIn(3)
          Emiss2Intp(2)=EmissOut(15)
          Freq2Intp(1)=cFreq(3)
          Freq2Intp(2)=cFreq(15)
          Freq2Intp2(1:cntValid)=cFreq(3:15)
          !---Interpolate for AMSU sounding channels
          CALL LINT(Freq2Intp,Emiss2Intp,2,2,cntValid,Freq2Intp2,Emiss2Intp2)
          EmissOut(3:15)=Emiss2Intp2(1:cntValid)
       ENDIF
       IF (EmissIn(3) .le. 0 .or. EmissIn(3) .ge. 1) THEN
          cntValid = COUNT(EmissIn(2:15) .eq. EmissIn(2:15))
          ALLOCATE(Emiss2Intp(2),Freq2Intp(2),Emiss2Intp2(cntValid),Freq2Intp2(cntValid))
          Emiss2Intp(1)=EmissIn(2)
          Emiss2Intp(2)=EmissOut(15)
          Freq2Intp(1)=cFreq(2)
          Freq2Intp(2)=cFreq(15)
          Freq2Intp2(1:cntValid)=cFreq(2:15)
          !---Interpolate for AMSU sounding channels
          CALL LINT(Freq2Intp,Emiss2Intp,2,2,cntValid,Freq2Intp2,Emiss2Intp2)
          EmissOut(2:15)=Emiss2Intp2(1:cntValid)
       ENDIF
       DEALLOCATE(Freq2Intp,Emiss2Intp,Freq2Intp2,Emiss2Intp2)
       IF (Em(17).le.0 .and. Em(20) .gt. 0) THEN
          cntValid = COUNT(EmissIn(16:20) .eq. EmissIn(16:20))
          ALLOCATE(Emiss2Intp(2),Freq2Intp(2),Emiss2Intp2(cntValid),Freq2Intp2(cntValid))
          Emiss2Intp(1)=EmissIn(16)
          Emiss2Intp(2)=EmissOut(20)
          Freq2Intp(1)=cFreq(16)
          Freq2Intp(2)=cFreq(20)
          Freq2Intp2(1:cntValid)=cFreq(16:20)
          !---Interpolate for AMSU sounding channels
          CALL LINT(Freq2Intp,Emiss2Intp,2,2,cntValid,Freq2Intp2,Emiss2Intp2)
          EmissOut(16:20)=Emiss2Intp2(1:cntValid)
          DEALLOCATE(Freq2Intp,Emiss2Intp,Freq2Intp2,Emiss2Intp2)
       ENDIF
       IF (Em(17).gt.0 .and. Em(20) .gt. 0) THEN
          cntValid = COUNT(EmissIn(17:20) .eq. EmissIn(17:20))
          ALLOCATE(Emiss2Intp(2),Freq2Intp(2),Emiss2Intp2(cntValid),Freq2Intp2(cntValid))
          Emiss2Intp(1)=EmissIn(17)
          Emiss2Intp(2)=EmissOut(20)
          Freq2Intp(1)=cFreq(17)
          Freq2Intp(2)=cFreq(20)
          Freq2Intp2(1:cntValid)=cFreq(17:20)
          !---Interpolate for AMSU sounding channels
          CALL LINT(Freq2Intp,Emiss2Intp,2,2,cntValid,Freq2Intp2,Emiss2Intp2)
          EmissOut(17:20)=Emiss2Intp2(1:cntValid)
          DEALLOCATE(Freq2Intp,Emiss2Intp,Freq2Intp2,Emiss2Intp2)
       ENDIF
       IF (Em(17).gt. 0 .and. Em(20) .lt. 0.) THEN
          EmissOut(18:20) = Em(17)
       ENDIF
       IF (Em(17).lt.0 .and. Em(20) .lt. 0.) THEN
          EmissOut(17:20) = Em(16)
       ENDIF
    ENDIF
    !---Adjust SSMI/S Channels
    IF (sensor_id .eq. sensor_id_f16) THEN
       DO ichan=2,7
          IF (Em(ichan) .lt. 0) EmissOut(ichan) = EmissOut(1)-random_noise(ichan)
       ENDDO
       IF (Em(8) .lt. 0) EmissOut(8) = Em(18)-random_noise(8)
       IF (Em(9) .lt. 0) EmissOut(9) = Em(18)-random_noise(9)
       IF (Em(10) .lt. 0) EmissOut(10) = EmissOut(9)-random_noise(10)
       IF (Em(11) .lt. 0) EmissOut(11) = EmissOut(9)-random_noise(11)
       DO ichan=19,24
          IF (Em(ichan) .lt. 0) EmissOut(ichan) = Em(1)-random_noise(ichan)
       ENDDO
    ENDIF
    IF (sensor_id .eq. sensor_id_f17) THEN
       DO ichan=2,7
          IF (Em(ichan) .lt. 0) EmissOut(ichan) = EmissOut(1)-random_noise(ichan)
       ENDDO
       IF (Em(8) .lt. 0) EmissOut(8) = Em(18)-random_noise(8)
       IF (Em(9) .lt. 0) EmissOut(9) = Em(18)-random_noise(9)
       IF (Em(10) .lt. 0) EmissOut(10) = EmissOut(9)-random_noise(10)
       IF (Em(11) .lt. 0) EmissOut(11) = EmissOut(9)-random_noise(11)
       DO ichan=19,24
          IF (Em(ichan) .lt. 0) EmissOut(ichan) = Em(1)-random_noise(ichan)
       ENDDO
    ENDIF
    IF (sensor_id .eq. sensor_id_f18) THEN
       DO ichan=2,7
          IF (Em(ichan) .lt. 0) EmissOut(ichan) = EmissOut(1)-random_noise(ichan)
       ENDDO
       IF (Em(8) .lt. 0) EmissOut(8) = Em(18)-random_noise(8)
       IF (Em(9) .lt. 0) EmissOut(9) = Em(18)-random_noise(9)
       IF (Em(10) .lt. 0) EmissOut(10) = EmissOut(9)-random_noise(10)
       IF (Em(11) .lt. 0) EmissOut(11) = EmissOut(9)-random_noise(11)
       DO ichan=19,24
          IF (Em(ichan) .lt. 0) EmissOut(ichan) = Em(1)-random_noise(ichan)
       ENDDO
    ENDIF
    !---Adjust NPP ATMS Channels
    IF (sensor_id .eq. sensor_id_npp) THEN
       IF (Em(1) .ge. 1 .or. Em(1) .le. 0 .or. Em(16) .ge. 1 .or. Em(16) .le. 0 &
            .or. Em(3) .le. 0 .or. Em(3) .ge. 1 .or. Em(17) .le. 0 .or. Em(17) .ge. 1) THEN 
          EmissOut=-999.
          RETURN
       ENDIF
       IF (Em(2) .gt. 1 .or. Em(2) .le. 0) EmissOut(2)=EmissOut(1)-random_noise(2)
       IF (EmissIn(3) .gt. 0 .and. EmissIn(3) .lt. 1 .and. EmissIn(17) .gt. 0 .and. EmissIn(17) .lt. 1) THEN
          cntValid = COUNT(EmissIn(3:17) .eq. EmissIn(3:17))
          ALLOCATE(Emiss2Intp(2),Freq2Intp(2),Emiss2Intp2(cntValid),Freq2Intp2(cntValid))
          Emiss2Intp(1)=EmissIn(3)
          Emiss2Intp(2)=EmissIn(17)
          Freq2Intp(1)=cFreq(3)
          Freq2Intp(2)=cFreq(17)
          Freq2Intp2(1:cntValid)=cFreq(3:17)
          !---Interpolate for AMSU sounding channels
          CALL LINT(Freq2Intp,Emiss2Intp,2,2,cntValid,Freq2Intp2,Emiss2Intp2)
          EmissOut(3:15)=Emiss2Intp2(1:cntValid-2)
          EmissOut(17)=Emiss2Intp2(cntValid)
       ENDIF
       DEALLOCATE(Freq2Intp,Emiss2Intp,Freq2Intp2,Emiss2Intp2)
       EmissOut(18)=EmissOut(17)-random_noise(18)
       EmissOut(19)=EmissOut(18)-random_noise(19)
       EmissOut(20)=EmissOut(18)-random_noise(20)
       EmissOut(21)=EmissOut(18)-random_noise(21)
       EmissOut(22)=EmissOut(18)-random_noise(22)
    ENDIF
    !---Adjust TRMM TMI Channels
    IF (sensor_id .eq. sensor_id_trmm) THEN
       !---Organize H and V pol by ascending frequency
       !---HPOL
       EmTMIBase_H(1)=EmissIn(2)
       EmTMIBase_H(2)=EmissIn(4)
       EmTMIBase_H(3)=EmissIn(7)
       EmTMIBase_H(4)=EmissIn(9)
       !---VPOL
       EmTMIBase_V(1)=EmissIn(1)
       EmTMIBase_V(2)=EmissIn(3)
       EmTMIBase_V(3)=EmissIn(5)
       EmTMIBase_V(4)=EmissIn(6)
       EmTMIBase_V(5)=EmissIn(8)
       !---If any 10 or 85 GHz are not valid, set to -999
       IF (EmissIn(1) .le. 0) EmissOut(1)=-999.
       IF (EmissIn(2) .le. 0) EmissOut(2)=-999.
       IF (EmissIn(8) .le. 0) EmissOut(8)=-999.
       IF (EmissIn(9) .le. 0) EmissOut(9)=-999.

       !---Interpolate for H pol 19 and 37 if missing
       IF (EmissIn(4) .le. 0 .or. EmissIn(7) .le. 0) THEN
          cntValid = COUNT(EmTMIBase_H(1:4) .eq. EmTMIBase_H(1:4))
          IF (cntValid .ne. 4) THEN
             EmissOut=999.
             RETURN
          ENDIF
          ALLOCATE(Emiss2Intp(2),Freq2Intp(2),Emiss2Intp2(cntValid),Freq2Intp2(cntValid))
          Emiss2Intp(1)=EmTMIBase_H(1)
          Emiss2Intp(2)=EmTMIBase_H(4)
          Freq2Intp(1)=cFreq(2)
          Freq2Intp(2)=cFreq(9)
          Freq2Intp2(1)=cFreq(2)
          Freq2Intp2(2)=cFreq(4)
          Freq2Intp2(3)=cFreq(7)
          Freq2Intp2(4)=cFreq(9)
          !---Interpolate
          CALL LINT(Freq2Intp,Emiss2Intp,2,2,cntValid,Freq2Intp2,Emiss2Intp2)
          EmissOut(2)=Emiss2Intp2(1)
          EmissOut(4)=Emiss2Intp2(2)
          EmissOut(7)=Emiss2Intp2(3)
          EmissOut(9)=Emiss2Intp2(4)
          DEALLOCATE(Emiss2Intp,Freq2Intp,Emiss2Intp2,Freq2Intp2)
       ENDIF

       !---Interpolate for V pol 19 and 37 if missin
       IF (EmissIn(3) .le. 0 .or. EmissIn(6) .le. 0) THEN
          cntValid = COUNT(EmTMIBase_V(1:5) .eq. EmTMIBase_V(1:5))
          IF (cntValid .ne. 5) THEN
             EmissOut=999.
             RETURN
          ENDIF
          ALLOCATE(Emiss2Intp(2),Freq2Intp(2),Emiss2Intp2(cntValid),Freq2Intp2(cntValid))
          Emiss2Intp(1)=EmTMIBase_V(1)
          Emiss2Intp(2)=EmTMIBase_V(5)
          Freq2Intp(1)=cFreq(1)
          Freq2Intp(2)=cFreq(8)
          Freq2Intp2(1)=cFreq(1)
          Freq2Intp2(2)=cFreq(3)
          Freq2Intp2(3)=cFreq(5)
          Freq2Intp2(4)=cFreq(6)
          Freq2Intp2(5)=cFreq(8)
          !---Interpolate
          CALL LINT(Freq2Intp,Emiss2Intp,2,2,cntValid,Freq2Intp2,Emiss2Intp2)
          EmissOut(1)=Emiss2Intp2(1)
          EmissOut(3)=Emiss2Intp2(2)
          EmissOut(5)=Emiss2Intp2(3)
          EmissOut(6)=Emiss2Intp2(4)
          EmissOut(8)=Emiss2Intp2(5)
          DEALLOCATE(Emiss2Intp,Freq2Intp,Emiss2Intp2,Freq2Intp2)
       ENDIF

       !Set 22 GHz to -999 to always get interpolated value
       EmTMIBase_V(3)=-999.
       !---Interpolate 22 GHz V pol
       IF (EmTMIBase_V(3) .le. 0) THEN
          cntValid = COUNT(EmTMIBase_V(2:4) .eq. EmTMIBase_V(2:4))
          IF (cntValid .ne. 3) THEN
             EmissOut=999.
             RETURN
          ENDIF
          ALLOCATE(Emiss2Intp(2),Freq2Intp(2),Emiss2Intp2(cntValid),Freq2Intp2(cntValid))
          Emiss2Intp(1)=EmTMIBase_V(2)
          Emiss2Intp(2)=EmTMIBase_V(4)
          Freq2Intp(1)=cFreq(3)
          Freq2Intp(2)=cFreq(6)
          Freq2Intp2(1)=cFreq(3)
          Freq2Intp2(2)=cFreq(5)
          Freq2Intp2(3)=cFreq(6)
          !---Interpolate
          CALL LINT(Freq2Intp,Emiss2Intp,2,2,cntValid,Freq2Intp2,Emiss2Intp2)
          EmissOut(3)=Emiss2Intp2(1)
          EmissOut(5)=Emiss2Intp2(2)
          EmissOut(6)=Emiss2Intp2(3)
          DEALLOCATE(Emiss2Intp,Freq2Intp,Emiss2Intp2,Freq2Intp2)
       ENDIF
    ENDIF

    DO iChan=1,nChan
       IF (EmissOut(iChan) .ge. 1. .or. EmissOut(iChan) .le. 0) EmissOut = -999
    ENDDO

    RETURN
  END SUBROUTINE AdjustEmiss


!-----------------------------------------
! Name: $Id: utils.f90 3342 2013-10-09 17:26:00Z amims $
!
! Purpose:
!   Manipulate the effective beam width of an image. For example, convert ATMS
!   to AMSU-A-like resolution while reducing the noise.
!
! Method:
!   1) Pad the image to a power of 2 in each dimension.
! If FFT technique is to be used then: 
!   2) Assuming Gaussian beam shapes, calcluate the input and output Modulation
!      Transfer Functions (MTF).
!   3) FFT image to frequency domain (2-D).
!   4) Multiply by output MTF divided by input MTF. If a cut-off is specified
!      (when attempting to make the beam width narrower), attenuate further
!      by an exponential function - factor of 2 at the cutoff. 
!   5) FFT back to image domain 
! Finally,
!   6) Over-write the input image, with averaging if requested.
!
! COPYRIGHT
!    This software was developed within the context of the EUMETSAT Satellite
!    Application Facility on Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 1 December 2006, between EUMETSAT and the
!    Met Office, UK, by one or more partners within the NWP SAF. The partners
!    in the NWP SAF are the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2010, EUMETSAT, All Rights Reserved.
!
! History:
! Version    Date     Comment
!
!  1.0   22/07/2010   N.C.Atkinson
!  1.1   21/11/2011   Convert to f90. A. Collard
!  1.2   13/04/2012   Added to MiRS.  K. Garrett
!
! Code Description:
!   FORTRAN 77, following AAPP standards
!
! Declarations:


SUBROUTINE Modify_Beamwidth ( nx, ny, image, sampling_dist,& 
     beamwidth, newwidth, mtfcutoff, nxaverage, nyaverage, qc_dist)
     

      IMPLICIT NONE
! Parameters
      INTEGER(LONG) nxmax
      PARAMETER (nxmax=128)    !Max number of spots per scan line
      INTEGER(LONG) nymax
      PARAMETER (nymax=8192)   !Max number of lines. Allow full orbit ATMS
      REAL(DOUBLE) minval
      PARAMETER (minval=0.0)   !Values less than this are treated as missing
      REAL(DOUBLE), PARAMETER    :: missing_value = DEFAULT_VALUE_REAL

! Arguments
      INTEGER,      INTENT(IN) :: nx, ny         !Size of image
      REAL,         INTENT(INOUT) ::  image(nx,ny)   !BT or radiance image
      REAL, INTENT(IN) ::     sampling_dist  !typically degrees
      REAL, INTENT(IN) ::     beamwidth      !ditto
      REAL, INTENT(IN) ::     newwidth       !ditto
      REAL, INTENT(IN) ::     mtfcutoff      !0.0 to 1.0
      INTEGER, INTENT(IN) ::  nxaverage      !Number of samples to average (or zero)
      INTEGER, INTENT(IN) ::  nyaverage      !Number of samples to average (or zero)
      INTEGER, INTENT(IN) ::  qc_dist        !Number of samples around missing data to set to 
                                                     !missing
       
! Local variables
      INTEGER(LONG) nxpad, nypad, dx, dy
      INTEGER(LONG) i,j,k,ix,iy, ii, jj
      INTEGER(LONG) ifirst
      INTEGER(LONG) xpow2, ypow2
      INTEGER(LONG) nxav2, nyav2, naverage
      INTEGER(LONG) deltax, minii, maxii, minjj, maxjj
      REAL(DOUBLE)    mtfxin(nxmax),mtfxout(nxmax)
      REAL(DOUBLE)    mtfyin(nymax),mtfyout(nymax)
      REAL(DOUBLE)    mtfin,mtfout
      REAL(DOUBLE)    mtfpad(nxmax,nymax)
      REAL(DOUBLE)    imagepad(nxmax,nymax)
      REAL(DOUBLE)    work(nymax)
      REAL(DOUBLE)    f,df,factor
      REAL(DOUBLE)    PI, LN2, LNcsquared
      LOGICAL missing, gooddata_map(nxmax,nymax)


! End of declarations
!-----------------------------------------
      
      PI = 4.0*atan(1.0)
      LN2 = LOG(2.0)
      IF (mtfcutoff .GT. 0.0) LNcsquared = LOG(mtfcutoff)**2
      nxav2 = nxaverage/2
      nyav2 = nyaverage/2
      naverage = nxaverage*nyaverage


!1) Pad the image up to the nearest power of 2 in each dimension, by reversing
!the points near the edge.

      xpow2 = INT(LOG(nx*1.0)/LN2 + 1.0)
      ypow2 = INT(LOG(ny*1.0)/LN2 + 1.0)
      nxpad = 2**xpow2
      nypad = 2**ypow2
      dx = (nxpad - nx)/2
      dy = (nypad - ny)/2

      IF (nxpad .GT. nxmax) THEN
         write(*,*) 'nx too large, maximum allowed value is ',nxmax-1
         STOP
      END IF
      
      IF (nypad .GT. nymax) THEN
         write(*,*) 'ny too large, maximum allowed value is ',nymax-1
         STOP
      END IF

!Loop over scan positions
      DO j=dy+1,dy+ny
        DO i=dx+1,dx+nx
          imagepad(i,j) = image(i-dx,j-dy)   !Take a copy of the input data
!xxx???          IF (imagepad(i,j).LT.minval) imagepad(i,j) = -100
          gooddata_map(i,j) = .TRUE.   ! Initialised for step 6)
        ENDDO

!Interpolate missing points in the along-track direction

        ifirst = -1
        missing = .false.
        
        DO i=dx+1,dx+nx
          IF (.not.missing) THEN
            IF (imagepad(i,j) .GE. minval) THEN
              ifirst = i
            ELSE
              missing = .true.
            ENDIF
          ELSE
            IF (imagepad(i,j) .GE. minval) THEN  !First good point after missing
               missing = .false.
               IF (ifirst .eq. -1) THEN
                  DO k=dx+1,i-1
                     imagepad(k,j) = imagepad(i,j)      !Constant
                  ENDDO
               ELSE
                  DO k=ifirst+1,i-1
                     factor = (i-k)*1.0/(i-ifirst)      !Interpolate
                     imagepad(k,j) = imagepad(ifirst,j)*factor + &
                          imagepad(i,j)*(1.0-factor)
                  ENDDO
               ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF (missing) THEN         !Last scan is missing
          IF (ifirst .GE. 1) then
            DO k=ifirst+1,dx+nx
              imagepad(k,j) = imagepad(ifirst,j)     !Constant
            ENDDO
          ENDIF
        ENDIF          

!Continue padding the edges

        DO i=1,dx
          imagepad(i,j) = imagepad(dx+dx+2-i,j)
        ENDDO
        DO i=nx+dx+1,nxpad
          imagepad(i,j) = imagepad(nx+dx+nx+dx-i,j)
        ENDDO
     ENDDO
     DO j=1,dy
        DO i=1,nxpad
           imagepad(i,j) = imagepad(i,dy+dy+2-j)
        ENDDO
     ENDDO
     DO j=ny+dy+1,nypad
        DO i=1,nxpad
           imagepad(i,j) = imagepad(i,ny+dy+ny+dy-j)
        ENDDO
     ENDDO

!2) Compute the MTF modifications. Assume beams are Gaussian.

      IF (newwidth .GT. 0) THEN
        df = 1.0/nxpad
        DO i=1,nxpad/2+1
          f = df*(i-1)      !DC to Nyquist
          mtfxin(i) = exp(-(PI*f*beamwidth/(2*sampling_dist))**2/LN2)
          mtfxout(i) = exp(-(PI*f*newwidth/(2*sampling_dist))**2/LN2)
          IF (i.GT.1.AND.i.LT.nxpad/2+1) THEN
            mtfxin(nxpad-i+2) = mtfxin(i)
            mtfxout(nxpad-i+2) = mtfxout(i)
          ENDIF
        ENDDO
        df = 1.0/nypad
        DO i=1,nypad/2+1
          f = df*(i-1)      !DC to Nyquist
          mtfyin(i) = exp(-(PI*f*beamwidth/(2*sampling_dist))**2/LN2)
          mtfyout(i) = exp(-(PI*f*newwidth/(2*sampling_dist))**2/LN2)
          IF (i.GT.1.AND.i.LT.nypad/2+1) THEN
            mtfyin(nypad-i+2) = mtfyin(i)
            mtfyout(nypad-i+2) = mtfyout(i)
          ENDIF
        ENDDO
        DO i=1,nxpad
          DO j=1,nypad
            mtfin = mtfxin(i)*mtfyin(j)
            mtfout = mtfxout(i)*mtfyout(j)
            if (mtfcutoff .GT. 0.0) THEN
              mtfpad(i,j) = (mtfout * &
                exp(-LN2/LNcsquared*(LOG(mtfout))**2))/mtfin
            else
              mtfpad(i,j) = mtfout/mtfin
            endif
          
!            if (mtfout.GE.mtfcutoff) THEN
!              mtfpad(i,j) = mtfout/mtfin
!            ELSE
!              mtfpad(i,j) = 0.0  !Sharp cut-off
!            ENDIF
          ENDDO
        ENDDO

!3) Fourier transform, line by line then column by column.
!After each FFT, points 1 to nxpad/2+1 contain the real part of the spectrum,
!the rest contain the imaginary part in reverse order.

        DO j=1,nypad
           CALL SFFTCF(imagepad(:,j),nxpad,xpow2)
        ENDDO
        DO i=1,nxpad
           DO j=1,nypad
              work(j) = imagepad(i,j)
           ENDDO
           CALL SFFTCF(work,nypad,ypow2)
           DO j=1,nypad
              imagepad(i,j) = work(j)
           ENDDO
        ENDDO

!4) Multiply the spectrum by the MTF factor

        DO j=1,nypad
           DO i=1,nxpad
            imagepad(i,j) = imagepad(i,j)*mtfpad(i,j)
          ENDDO
        ENDDO

!5) Inverse Fourier transform, column by column then line by line 

        DO i=1,nxpad
          DO j=1,nypad
            work(j) = imagepad(i,j)
          ENDDO
          CALL SFFTCB(work,nypad,ypow2)
          DO j=1,nypad
            imagepad(i,j) = work(j)
          ENDDO
        ENDDO
        DO j=1,nypad
          CALL SFFTCB(imagepad(:,j),nxpad,xpow2)
        ENDDO
      ENDIF   !New width is specified

!6) Reset missing values in gooddata_map, based on qc_dist and the values in the 
!   input image array

     DO j=1,ny
        DO i=1,nx
           IF (image(i,j) <= minval) THEN
              minjj=max(j+dy-qc_dist,0)
              maxjj=min(j+dy+qc_dist,nymax)
              DO jj=minjj,maxjj
                 deltax=INT(SQRT(REAL(qc_dist**2 - (jj-j-dy)**2 )))
                 minii=max(i+dx-deltax,0)
                 maxii=min(i+dx+deltax,nxmax)
                 DO ii=minii,maxii
                    gooddata_map(ii,jj)=.FALSE.
                 END DO
              END DO
           END IF
        END DO
     END DO

!7) Over-write the input image (points that are not missing)

     DO j=1,ny
        DO i=1,nx
           IF (gooddata_map(i+dx,j+dy)) THEN
              IF (nxav2 == 0. .AND. nyav2 == 0) THEN
                 image(i,j) = imagepad(i+dx,j+dy)
              ELSE
                 image(i,j) = 0.0             !Do averaging
                 DO ix = -nxav2,nxav2
                    DO iy = -nyav2,nyav2
                       image(i,j) = image(i,j) + imagepad(i+dx+ix,j+dy+iy)
                    ENDDO
                 ENDDO
                 image(i,j) = image(i,j)/naverage
              ENDIF
           ELSE
              image(i,j) = missing_value
           END IF
        ENDDO
     ENDDO
     
     RETURN
   END SUBROUTINE Modify_Beamwidth

  subroutine SeaIceTypeAdjust(SensorID,nchan,CentrFreq,lat,Tskin_bkg,Tskin_ext,&
       Emis_bkg,Emis_ext,sfcMixed_ice_ocean,sfcClass,atmClass,ChanSel)

    integer, intent(in) :: nchan,SensorID
    logical, intent(inout) :: sfcMixed_ice_ocean
    integer, intent(inout) :: sfcClass,atmClass
    integer, intent(inout), dimension(nchan) :: Chansel
    real, intent(in) :: lat,Tskin_bkg,Tskin_ext
    real, intent(in), dimension(nchan) :: CentrFreq, Emis_bkg, Emis_ext
    
    
    integer :: ichan
    real :: mindif,freqdif,grLoHibg,grLoHifg,rmsd_emis
    integer, save :: ilowFreq,ihighFreq
    logical, save :: seaice_init = .true.
    
  !---Only process AMSUA/MHS for now 
    if(.not.(SensorID .eq. sensor_id_n18 .or. SensorID .eq. sensor_id_n19 .or. &
         SensorID .eq. sensor_id_metopA .or. SensorID .eq. sensor_id_metopB))return

  !
  !---Establish channel indices of low and high frequencies for mixed ocean/ice detection
  !
    if(seaice_init)then
       print *,'SeaIceTypeadjust:computing ilowFreq, ihighFreq'
       mindif=9999.
       ilowFreq=-1
       do ichan=1,nchan
          freqdif=abs(CentrFreq(ichan)-31.4005)
          if(freqdif .lt. mindif .and. ChanSel(ichan) .eq. 1)then
             ilowFreq=ichan
             mindif=freqdif
          endif
       enddo
       if(ilowFreq .lt. 0)then
          print *,'ERROR: ilowFreq not found'
          stop
       endif
       mindif=9999.
       ihighFreq=-1
       do ichan=1,nchan
          freqdif=abs(CentrFreq(ichan)-89.0022)
          if(freqdif .lt. mindif .and. ChanSel(ichan) .eq. 1)then
             ihighFreq=ichan
             mindif=freqdif
          endif
       enddo
       if(ihighFreq .lt. 0)then
          print *,'ERROR: ilowFreq not found'
          stop
       endif
       print *,'ilowFreq,CentrFreq(ilowFreq)=',ilowFreq,CentrFreq(ilowFreq)
       print *,'ihighFreq,CentrFreq(ihighFreq)=',ihighFreq,CentrFreq(ihighFreq)
       seaice_init=.false.
    endif

!--- Turn off AMSU channel 2 (31v) for any scene with sfcClass=1 (ice)
    if(sfcClass .eq. 1)then
       ChanSel(2)=0
    endif

  !---Other scene criteria: lat,Tskin,SfcType
    if(.not. (abs(lat) .gt. 45. .and. &
         (Tskin_bkg .le. 273. .or. Tskin_ext .le. 273.) .and. &
         (SfcClass .eq. 0 .or. SfcClass .eq. 1)))return

!    print *,'call SeaIceTypeAdjust: Tskin_bkg,Tskin_ext',Tskin_bkg,Tskin_ext



!              ALLOCATE(emisBG(nchan))
!              emisBG(1:nchan)=&
!                   XbSfcTot(GeophStatsT_Sfc(1)%EDR_IDs(iEDR_emis):GeophStatsT_Sfc(1)%EDR_IDs(iEDR_emis)+&
!                   GeophStatsT_Sfc(1)%Stats(iEDR_emis,1)%npEDR-1,sfcClass+1,1)
!              print *,'emisBG(1:nchan)=',emisBG(1:nchan)
!              print *,'Scene_ext%Emiss(1:nchan)=',Scene_ext%Emiss(1:nchan)
!              print *,'Scene_ext%Emiss(1:nchan)-emisBG(1:nchan)=',&
!                   Scene_ext%Emiss(1:nchan)-emisBG(1:nchan)
!              print *,'(Scene_ext%Emiss(1:nchan)-emisBG(1:nchan))**2=',&
!                   (Scene_ext%Emiss(1:nchan)-emisBG(1:nchan))**2
!              print *,'sum(Scene_ext%Emiss(1:nchan)-emisBG(1:nchan))**2=',&
!                   sum(Scene_ext%Emiss(1:nchan)-emisBG(1:nchan))**2
!              print *,'sqrt(sum(Scene_ext%Emiss(1:nchan)-emisBG(1:nchan))**2)=',&
!                   sqrt(sum(Scene_ext%Emiss(1:nchan)-emisBG(1:nchan))**2)

    grLoHibg=Emis_bkg(ilowFreq)-Emis_bkg(ihighFreq)
    grLoHifg=Emis_ext(ilowFreq)-Emis_ext(ihighFreq)
!              print *,'grLoHibg=',grLoHibg
!              print *,'grLoHifg=',grLoHifg
    rmsd_emis=&
         sqrt(sum(Emis_ext(1:nchan)-&
         Emis_bkg(1:nchan))**2)
!              print *,'rmsd_emis=',rmsd_emis
              !---check if fg/bg mixing required
    if((rmsd_emis .gt. 0.7) .and. &
         ((grLoHibg .gt. 0. .and. grLoHifg .lt. -0.05) .or. &
         (grLoHibg .lt. -0.05 .and. grLoHifg .gt. 0.)) .and. &
         (abs(grLoHibg-grLoHifg) .gt. 0.05)) then
       sfcMixed_ice_ocean=.TRUE.

                 !---Switch sfc type and/or atmosphere type
       if(sfcClass .eq. 1 .and. grLoHifg .lt. -0.05)then
!          print *,'reset SfcClass to 0'
          sfcClass=0
!                    scalFactEF_oc_byChan(2)=10.
          ChanSel(2)=0
       elseif(sfcClass .eq. 0 .and. grLoHifg .ge. -0.05)then
!          print *,'reset SfcClass to 1'
          sfcClass=1
!                    scalFactEF_ic_byChan(2)=10.
          ChanSel(2)=0
       endif
       if(atmClass .eq. 1)then
!                    atmClass=0
       elseif(atmClass .eq. 0 .and. grLoHifg .ge. -0.05)then
          atmClass=1
       endif
!                 print *,'Modified: ChanSel=',ChanSel(1:nchan)

    endif


  end subroutine SeaIceTypeAdjust


END MODULE utils
