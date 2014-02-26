!$Id: ssmis_heriAlgors.f90 2922 2012-02-07 21:13:34Z kgarrett $
MODULE SSMIS_HERIALGORS
!========================================================================================
!
!  Purpose:
!    Algorithms used to retrieve products from SSMIS observations (Ta)
!
!  Subroutines:
!    DETERM_RWP()          - mm (Grody, 1991, JGR; Ferraro et al., 1994) 
!    DETERM_SNOW_COVER()   - %  (Grody, 1991) 
!    DETERM_SEA_ICE()      - %  (NOAA TESTING ALGORITHM)  
!    DETERM_TPW()          - mm (Alishouse et al., 1990, IEEE
!    DETERM_CLW()          - g/m**2 (Weng and Grody, 1994, JGR)   
!    DETERM_WINDSP()       - m/s    (Cal/val plus LWP as a screener)
!
!    TA_CORRECTION()       - SSMIS(F16) to F15 correction
!    TA2TB()               - Antenna temperature to brightness temperature
!
!  Record of Revisions:
!        Date        Programmer            Description of Change
!    ============  ==============  ==========================================
!     2006/07/07     Ninghai Sun    Create original program.
!     2006/08/04     Ninghai Sun    Add emissivity and Tskin retrieval.
!
!========================================================================================
IMPLICIT NONE

PRIVATE
PUBLIC :: DETERM_RWP, DETERM_SNOW_COVER, DETERM_SEA_ICE 
PUBLIC :: DETERM_TPW, DETERM_CLW, DETERM_WINDSP
PUBLIC :: DETERM_EMISS, DETERM_TSKIN
CONTAINS

  SUBROUTINE DETERM_RWP(nChan, Ta, Stype, RWP)
  !==============================================================================
  !
  !  Purpose:
  !    To calculate rainfall ammount. Blended scattering/emission over ocean.
  !  
  !  Paper:
  !    Grody, 1991, JGR
  !    Ferraro et al., 1994  
  !
  !  Output unit:
  !    mm
  !
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/07/07     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: nChan, Stype
  REAL, DIMENSION(nChan), INTENT(IN) :: Ta
  REAL, INTENT(OUT) :: RWP

  INTEGER, PARAMETER :: RT=285
  REAL :: SCT, Q19, Q37, TTT, TT
  REAL :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h
 
    ! Ta correction from F16 to F15
    CALL TA_CORRECTION(nChan, Ta, Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h)

    TTT = 44.0 + 0.85*Ta19v
    TT = 168.0 + 0.49*Ta85v

    IF ( Stype == 1 ) THEN         ! Over Ocean
      ! Check for Scattering
      SCT = -182.7 + 0.75*Ta19v + 2.543*Ta22v - 0.00543*Ta22v*Ta22v - Ta85v
      RWP = 0.0
      
      IF ( SCT >= 10.0 ) THEN
        RWP =  0.00188*SCT**2.03434
        IF ( Ta22v <= TTT ) RWP = 0.0
        IF ( (Ta22v > 257.0) .AND. ((Ta22v-Ta19v) < 2.0) ) RWP = 0.0
      ELSE
        ! Check for Emission
        IF ( (Ta19v < RT) .AND. (Ta22v < RT) ) THEN
          Q19 = -2.70 * ( ALOG(290.0-Ta19v) - 2.80 - 0.42*ALOG(290.0-Ta22v) )
        ENDIF
        
        IF ( Q19 >= 0.60 ) THEN
          RWP = 0.001707*(Q19*100.0)**1.7359
        ELSE
          IF ( (Ta37v < RT) .AND. (Ta22v < RT) ) THEN
            Q37 = -1.15 * (ALOG(290.0-Ta37v) - 2.90 - 0.349*ALOG(290.0-Ta22v) )
          ENDIF
          IF ( Q37 >= 0.20 ) RWP = 0.001707*(Q37*100.0)**1.7359
        ENDIF
      ENDIF

      RWP = 1.25*RWP   ! BEAM FILLING CORRECTION
    
    ELSE    ! Over Land
      
      SCT = 438.5 - 0.46*Ta19v - 1.735*Ta22v + 0.00589*Ta22v*Ta22v - Ta85v
      RWP = 0.0
      
      IF ( SCT >= 10.0 ) THEN
        RWP = 0.00513*SCT**1.9468
        IF ( (Ta22v < 257.0) .AND. (Ta22v < TT) ) RWP = 0.0
        IF ( (Ta19v-Ta19h) > 20.0 ) RWP = 0.0
        IF ( (Ta85v > 250.0) .AND. ((Ta19v-Ta19h) > 7.0) ) RWP = 0.0
      ENDIF
    
    ENDIF
  
    IF ( RWP > 35.0 ) RWP = 35.0
    
  END SUBROUTINE DETERM_RWP


  SUBROUTINE DETERM_SNOW_COVER(nChan, Ta, Stype, Snow_Cover)
  !==============================================================================
  !
  !  Purpose:
  !    To calculate snow cover. 
  !  
  !  Paper:
  !    Grody, 1991, JGR
  !
  !  Output unit:
  !    %
  !
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/07/07     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: nChan, Stype
  REAL, DIMENSION(nChan), INTENT(IN) :: Ta
  REAL, INTENT(OUT) :: Snow_Cover

  REAL :: SCAT, SC37, PD19, SCX, TT
  REAL :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h

    ! Ta correction from F16 to F15
    CALL TA_CORRECTION(nChan, Ta, Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h)

    IF ( Stype /= 1 ) THEN     ! If not over water
      SCAT = Ta22v - Ta85v
      SC37 = Ta19v - Ta37v
      PD19 = Ta19v - Ta19h
      SCX  = Ta37v - Ta85v
      IF ( SC37 > SCAT ) SCAT = SC37
      TT = (165 + 0.49*Ta85v)
      IF ( SCAT > 0.0 ) THEN
        Snow_Cover = 100.0
        IF ( (Ta22v >= 254.0) .AND. (SCAT <= 2.0) ) Snow_Cover = 0.0
        IF ( (Ta22v >= 258.0) .OR. (Ta22v >= TT) ) Snow_Cover = 0.0
        IF ( (PD19 >= 18.0) .AND. (SC37 <= 10.0) .AND. (SCX <= 10.0) ) Snow_Cover = 0.0
        IF ( (SCAT <= 6.0) .AND. (PD19 >= 8.0) ) Snow_Cover = 0.0
      ELSE  
        Snow_Cover = 0.0
      END IF
      
      ! Latest Glacial Ice Check ( 02/95 )
      IF ( (Ta22v <= 210.0) .OR. ((Ta22v <= 229.0) .AND. (PD19 >= 23.0)) ) Snow_Cover = 100.0
    ELSE      ! Over water, there is no snow cover.
      Snow_Cover = -999.0
    END IF

  END SUBROUTINE DETERM_SNOW_COVER


  SUBROUTINE DETERM_SEA_ICE(nChan, Ta, Stype, Sea_Ice)
  !==============================================================================
  !
  !  Purpose:
  !    To calculate sea ice cover. 
  !  
  !  Paper:
  !    NOAA Testing algorithm
  !
  !  Output unit:
  !    %
  !
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/07/07     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: nChan, Stype
  REAL, DIMENSION(nChan), INTENT(IN) :: Ta
  REAL, INTENT(OUT) :: Sea_Ice

  REAL :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h
    
    ! Ta correction from F16 to F15
    CALL TA_CORRECTION(nChan, Ta, Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h)

    IF ( Stype == 1 ) THEN   ! Over water
      Sea_Ice = 91.9 - 2.994*Ta22v + 2.846*Ta19v - 0.386*Ta37v + 0.495*Ta85v &
                + 1.005*Ta19h - 0.904*Ta37h
      IF ( Sea_Ice >= 70.0 ) THEN
        Sea_Ice = 100.0
      ELSE
        Sea_Ice = 0.0
      END IF
    ELSE         ! Over Land, there is no sea ice!
      Sea_Ice = -999.0
    END IF
    
  END SUBROUTINE DETERM_SEA_ICE


  SUBROUTINE DETERM_TPW(nChan, Ta, Stype, TPW)
  !==============================================================================
  !
  !  Purpose:
  !    To calculate total precipitable water . 
  !  
  !  Paper:
  !    Alishouse et al., 1990, IEEE/new Screening
  !
  !  Output unit:
  !    mm
  !
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/07/07     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: nChan, Stype
  REAL, DIMENSION(nChan), INTENT(IN) :: Ta
  REAL, INTENT(OUT) :: TPW

  REAL :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h
  REAL :: Tb19v, Tb19h, Tb22v, Tb37v, Tb37h, Tb85v, Tb85h
  REAL :: Sea_Ice, SCT

    ! Ta correction from F16 to F15
    CALL TA_CORRECTION(nChan, Ta, Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h)

    ! Ta to Tb conversion
    CALL TA2TB( Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h, &
                Tb19v, Tb19h, Tb22v, Tb37v, Tb37h, Tb85v, Tb85h )

    ! Call DETERM_SEA_ICE to find the Sea_Ice
    CALL DETERM_SEA_ICE(nChan, Ta, Stype, Sea_Ice)

    ! Calculate TPW
    TPW = -999.0
    
    SCT = -182.7 + 0.75*Tb19v + 2.543*Tb22v - 0.00543*Tb22v*Tb22v - Tb85v
    
    ! If over either land or sea ice, no CWV is available
    IF ( (Stype /= 1) .OR. (Sea_Ice == 100.0) .OR. (SCT > 10.0) ) RETURN
    
    TPW = 232.89 - 0.1486*Tb19v - 0.3695*Tb37v &
           - (1.8291 - 0.006193*Tb22v)*Tb22v
    
    TPW = -3.753 + 1.507*TPW - 0.01933*TPW**2 + 0.0002191*TPW**3
    
    IF ( TPW < 0.0 ) TPW = 0.0
    IF ( TPW > 100.0 ) TPW = 0.0
    
    RETURN

  END SUBROUTINE DETERM_TPW


  SUBROUTINE DETERM_CLW(nChan, Ta, Stype, CLW)
  !==============================================================================
  !
  !  Purpose:
  !    To calculate cloud liquid water. 
  !  
  !  Paper:
  !    Weng and Grody, 1994, JGR
  !
  !  Output unit:
  !    g/m^2
  !
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/07/07     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: nChan, Stype
  REAL, DIMENSION(nChan), INTENT(IN) :: Ta
  REAL, INTENT(OUT) :: CLW 

  INTEGER(4), PARAMETER :: RT=285
  REAL :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h
  REAL :: Tb19v, Tb19h, Tb22v, Tb37v, Tb37h, Tb85v, Tb85h
  REAL :: Sea_Ice, TPW
  REAL :: ALG1, ALG2, ALG3

    ! Ta correction from F16 to F15
    CALL TA_CORRECTION(nChan, Ta, Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h)

    ! Ta to Tb conversion
    CALL TA2TB( Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h, &
                Tb19v, Tb19h, Tb22v, Tb37v, Tb37h, Tb85v, Tb85h )

    ! Call DETERM_SEA_ICE to find the Sea_Ice
    CALL DETERM_SEA_ICE(nChan, Ta, Stype, Sea_Ice)

    ! Calculate TPW Over Ocean
    IF ( (Stype == 1) .AND. (Sea_Ice /= 100.0) ) THEN
      ALG1 = -999.0
      ALG2 = -999.0
      ALG3 = -999.0
      TPW = 232.89 - 0.1486*Tb19v - 0.3695*Tb37v - (1.8291 - 0.006193*Tb22v)*Tb22v
      
      IF ( (Ta19v < RT) .AND. (Ta22v < RT) ) THEN
        ALG1 = -3.20 * ( ALOG(290.0-Ta19v) - 2.80 - 0.42*ALOG(290.0-Ta22v) )          !TA
        ! ALG1 = -2.70 * ( ALOG(290.0-Ta19v) - 2.80 - 0.42*ALOG(290.0-Ta22v) )      !TA
        ! ALG1 = -2.70 * ( ALOG(290.0-Tb19v) - 2.84 - 0.40*ALOG(290.0-Tb22v) )      !TB
      END IF

      IF ( (Ta37v < RT) .AND. (Ta22v < RT) ) THEN
        ALG2 = -1.66 * ( ALOG(290.0-Ta37v) - 2.90 - 0.349*ALOG(290.0-Ta22v) )   !TA
        ! ALG2 = -1.15 * ( ALOG(290.0-Ta37v) - 2.90 - 0.349*ALOG(290.0-Ta22v) )   !TA
        ! ALG2 = -1.15 * ( ALOG(290.0-Tb37v) - 2.99 - 0.32*ALOG(290.0-Tb22v) )    !TB
      END IF

      IF ( (Ta85h < RT) .AND. (Ta22v < RT) ) THEN
        ALG3 = -0.44 * ( ALOG(290.0-Ta85h) + 1.60 - 1.354*ALOG(290.0-Ta22v) )     !TA
        ! ALG3 = -0.44 * ( ALOG(290.0-Ta85h) + 1.60 - 1.354*ALOG(290.0-Ta22v) )     !TA
        ! ALG3 = -0.44 * ( ALOG(290.0-Tb85h) + 1.11 - 1.26*ALOG(290.0-Tb22v) )      !TB
      END IF

      IF ( ALG1 > 0.70 ) THEN        !original threshold 0.60
        CLW = ALG1
      ELSE IF ( ALG2 > 0.28 ) THEN   !original threshold 0.28
        CLW = ALG2
      ELSE IF ( TPW < 30.0 ) THEN
        CLW = ALG3
      ELSE
        CLW = ALG2
      END IF
       
      IF ( CLW > 6.0 ) CLW = 0.0 
     
    ELSE    
      ! Over Land
      CLW = -999.0
    
    END IF

  END SUBROUTINE DETERM_CLW


  SUBROUTINE DETERM_WINDSP(nChan, Ta, Stype, WindSp)
  !==============================================================================
  !
  !  Purpose:
  !    To calculate surface wind speed. 
  !  
  !  Paper:
  !    Cal/Val plus LWP as a screener
  !
  !  Output unit:
  !    m/s   
  !
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/07/07     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: nChan, Stype
  REAL, DIMENSION(nChan), INTENT(IN) :: Ta
  REAL, INTENT(OUT) :: WindSp 

  REAL :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h
  REAL :: Tb19v, Tb19h, Tb22v, Tb37v, Tb37h, Tb85v, Tb85h
  REAL :: Sea_Ice, CLW

    ! Ta correction from F16 to F15
    CALL TA_CORRECTION(nChan, Ta, Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h)

    ! Ta to Tb conversion
    CALL TA2TB( Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h, &
                Tb19v, Tb19h, Tb22v, Tb37v, Tb37h, Tb85v, Tb85h )

    ! Call DETERM_SEA_ICE to find the Sea_Ice
    CALL DETERM_SEA_ICE(nChan, Ta, Stype, Sea_Ice)

    ! Call DETERM_CLW to find the CLW
    CALL DETERM_CLW(nChan, Ta, Stype, CLW)

    ! Calculate Wind_Speed over Ocean
    IF ( (Stype == 1) .AND. (Sea_Ice /= 100.0) .AND. (CLW < 0.20) ) THEN
      WindSp = 147.9 + 1.0969*Tb19v - 0.4555*Tb22v - 1.76*Tb37v + 0.786*Tb37h
    ELSE
      WindSp = -999.0
    END IF
    
  END SUBROUTINE DETERM_WINDSP



  SUBROUTINE DETERM_EMISS(nChan, Ta, Stype, Emiss)
  !==============================================================================
  !
  !  Purpose:
  !    To calculate land emissivity. 
  !  
  !  Output unit:
  !
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/08/04     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: nChan, Stype
  REAL, DIMENSION(nChan), INTENT(IN) :: Ta
  REAL, DIMENSION(7), INTENT(OUT) :: Emiss

  REAL :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h
  INTEGER :: iChan

    ! Ta correction from F16 to F15
    CALL TA_CORRECTION(nChan, Ta, Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h)

    ! Calculate emissivity over land from Ta
    IF ( Stype /= 1 ) THEN
      ! Over Land
      ! 19v
      Emiss(1) = 5.0980E-1 + 4.4664E-3*Ta19v - 6.0427E-6*Ta19h - 2.5285E-3*Ta22v + &
                 2.3725E-3*Ta37v + 9.8163E-4*Ta37h - 2.2269E-3*Ta85v - 1.3193E-3*Ta85h
      ! 19h
      Emiss(2) = 4.2900E-1 + 1.0685E-3*Ta19v + 4.0082E-3*Ta19h - 2.9672E-3*Ta22v + &
                 1.4281E-3*Ta37v + 1.7393E-3*Ta37h - 1.0247E-3*Ta85v - 2.2088E-3*Ta85h
      ! 22v
      Emiss(3) = EMISS(1)
      ! 37v
      Emiss(4) = 3.8159E-1 - 1.5225E-3*Ta19v + 1.7213E-4*Ta19h - 3.7164E-4*Ta22v + &
                 6.5607E-3*Ta37v + 8.1213E-4*Ta37h - 1.7678E-3*Ta85v -1.7250E-3*Ta85h
      ! 37h
      Emiss(5) = 2.6220E-1 - 1.5095E-3*Ta19v - 1.9587E-5*Ta19h + 5.0142E-4*Ta22v + &
                 6.8795E-4*Ta37v + 5.7910E-3*Ta37h - 7.1539E-4*Ta85v -2.1267E-3*Ta85h
      ! 85v
      Emiss(6) = -9.4351E-1 + (4.1137E-3 - 7.0109E-6*Ta37v)*Ta37v &
                            + (1.5677E-2 - 3.1055E-5*Ta85v)*Ta85v &
                            - (6.5089E-3 - 1.4984E-5*Ta85h)*Ta85h
      ! 85h
      Emiss(7) = -9.7879E-1 + (3.0851E-3 - 5.2696E-6*Ta37v)*Ta37v &
                            + (7.4612E-3 - 2.2772E-5*Ta85v)*Ta85v &
                            + (2.9755E-3 + 4.5324E-6*Ta85h)*Ta85h
      ! Error handler
      DO iChan = 1, 7
        IF ( Emiss(iChan) > 1.0 ) Emiss(iChan) = 1.0
      ENDDO
    
    ELSE
      RETURN
    ENDIF

  END SUBROUTINE DETERM_EMISS
  
  
  
  SUBROUTINE DETERM_TSKIN(nChan, Ta, Stype, Tskin)
  !==============================================================================
  !
  !  Purpose:
  !    To calculate land skin temperature. 
  !  
  !  Output unit:
  !    k   
  !
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/08/04     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: nChan, Stype
  REAL, DIMENSION(nChan), INTENT(IN) :: Ta
  REAL, INTENT(OUT) :: Tskin

  REAL :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h

    ! Ta correction from F16 to F15
    CALL TA_CORRECTION(nChan, Ta, Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h)

    ! Calculate Skin Temperature of Land from Ta
    IF ( Stype == 1 ) THEN
      ! Not work over ocean
      Tskin = -999.0
    ELSE
      Tskin = 2.5090E2 - (1.7167E0 - 5.5144E-3*Ta22v)*Ta22v    &
                       - (1.0829E-1 + 1.9755E-3*Ta37v)*Ta37v   &
                       + (1.1763E0 - 6.3551E-4*Ta85v)*Ta85v
    ENDIF

  END SUBROUTINE DETERM_TSKIN
  
  
  
  SUBROUTINE TA_CORRECTION(nChan, Ta, Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h)
  !==============================================================================
  !
  !  Purpose:
  !    To correct SSM/IS (F16) antenna temperature to SSM/I (F15) 
  !       SSM/I     C_Freq     SSM/IS
  !      -------   ---------  --------
  !       Chan1     19.35v     Chan13
  !       Chan2     19.35h     Chan12
  !       Chan3     22.235v    Chan14
  !       Chan4     37.0v      Chan16
  !       Chan5     37.0h      Chan15
  !       Chan6     91.655v    Chan17
  !       Chan7     91.655h    Chan18
  !  
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/07/17     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: nChan
  REAL, DIMENSION(nChan), INTENT(IN) :: Ta
  REAL, INTENT(OUT) :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h

  REAL(4), PARAMETER, DIMENSION(7) :: AP=(/7.80472,7.44254,6.76383,7.34409,8.55426,6.57813,6.45397/)
  REAL(4), PARAMETER, DIMENSION(7) :: BP=(/0.967519,0.969424,0.959808,0.958955,0.954316,0.980339,0.978795/)
    
    Ta19v = AP(1) + BP(1)*Ta(13)
    Ta19h = AP(2) + BP(2)*Ta(12)
    Ta22v = AP(3) + BP(3)*Ta(14)
    Ta37v = AP(4) + BP(4)*Ta(16)
    Ta37h = AP(5) + BP(5)*Ta(15)
    Ta85v = AP(6) + BP(6)*Ta(17)
    Ta85h = AP(7) + BP(7)*Ta(18)

  END SUBROUTINE TA_CORRECTION


  SUBROUTINE TA2TB(Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h, &
                   Tb19v, Tb19h, Tb22v, Tb37v, Tb37h, Tb85v, Tb85h)
  !==============================================================================
  !
  !  Purpose:
  !    To convert F16-to-F15 corrected antenna temperature(Ta_corrected) to 
  !    brightness temperature(Tb)
  !    Doppler correction, Cross-poloarization and Spill-over correction
  !  
  !  Record of Revisions:
  !        Date        Programmer            Description of Change
  !    ============  ==============  ==========================================
  !     2006/07/17     Ninghai Sun    Create original program.
  !
  !==============================================================================
  IMPLICIT NONE

  REAL, INTENT(IN) :: Ta19v, Ta19h, Ta22v, Ta37v, Ta37h, Ta85v, Ta85h
  REAL, INTENT(OUT) :: Tb19v, Tb19h, Tb22v, Tb37v, Tb37h, Tb85v, Tb85h 

  REAL(4), PARAMETER, DIMENSION(7) :: AP=(/0.969,0.969,0.974,0.986,0.986,0.988,0.988/)
  REAL(4), PARAMETER, DIMENSION(7) :: BP=(/0.00473,0.00415,0.0107,0.0217,0.02612,0.01383,0.01947/)
   
  INTEGER(4) :: iChan
  REAL(4), DIMENSION(7) :: CP, DP

    DO iChan=1, 7
      CP(iChan) = 1.0 / ( AP(iChan) * ( 1.0 - BP(iChan) ) )
      DP(iChan) = CP(iChan) * BP(iChan)
    END DO

    Tb19v = CP(1)*Ta19v - DP(1)*Ta19h 
    Tb19h = CP(2)*Ta19h - DP(2)*Ta19v 
    Tb22v = CP(3)*Ta22v - DP(3)*(0.653*Ta19h + 96.6)
    Tb37v = CP(4)*Ta37v - DP(4)*Ta37h 
    Tb37h = CP(5)*Ta37h - DP(5)*Ta37v 
    Tb85v = CP(6)*Ta85v - DP(6)*Ta85h 
    Tb85h = CP(7)*Ta85h - DP(7)*Ta85v 

  END SUBROUTINE TA2TB

END MODULE SSMIS_HERIALGORS
!
! End of Module
!


