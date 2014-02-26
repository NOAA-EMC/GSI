!
! -------------------------------------------------------------------------------
!   
! module history log:
!   2004-10-26  Quanhua Liu
!
!   Microwave cloud module to compute optical depth (Ext0), scattering 
!   coefficient (Scat0) and weighted asymmetry factor (g0) , and adjoint parts.
! ------------------------------------------------------------------------------
 MODULE mw_cloud_opt
  ! -- Utility modules
  USE Binary_File_Utility 
  USE Type_Kinds
  USE Error_Handler
  USE CRTM_Parameters

    PUBLIC :: mw_read_cloud_opt
    PUBLIC :: mw_compute_cloud_opt
    PUBLIC :: mw_compute_cloud_opt_TL
    PUBLIC :: mw_compute_cloud_opt_AD

   PRIVATE 
 
   INTEGER, PARAMETER :: MW_nchannels = 31
   INTEGER, PARAMETER :: Nsize_cloud = 4
   INTEGER, PARAMETER :: Ndensity_cloud = 4
   REAL( fp_kind ), SAVE :: density_cloud(Ndensity_cloud)
   REAL( fp_kind ), SAVE :: Size_cloud(Nsize_cloud)
   REAL( fp_kind ), SAVE :: Frequency_cloud(MW_nchannels)
   REAL( fp_kind ), SAVE :: Rain_ext0(MW_nchannels,Nsize_cloud)
   REAL( fp_kind ), SAVE :: Rain_w0(MW_nchannels,Nsize_cloud)
   REAL( fp_kind ), SAVE :: Rain_g0(MW_nchannels,Nsize_cloud)
    
   REAL( fp_kind ), SAVE :: Snow_ext0(MW_nchannels,Nsize_cloud)
   REAL( fp_kind ), SAVE :: Snow_w0(MW_nchannels,Nsize_cloud)
   REAL( fp_kind ), SAVE :: Snow_g0(MW_nchannels,Nsize_cloud)

   REAL( fp_kind ), SAVE :: Gh_ext0(MW_nchannels,Nsize_cloud)
   REAL( fp_kind ), SAVE :: Gh_w0(MW_nchannels,Nsize_cloud)
   REAL( fp_kind ), SAVE :: Gh_g0(MW_nchannels,Nsize_cloud)

   REAL( fp_kind ), SAVE :: Ice_ext0(MW_nchannels,Nsize_cloud)
   REAL( fp_kind ), SAVE :: Ice_w0(MW_nchannels,Nsize_cloud)
   REAL( fp_kind ), SAVE :: Ice_g0(MW_nchannels,Nsize_cloud)

 CONTAINS

    FUNCTION mw_read_cloud_opt(mw_cloud_opt_filename,Message_Log) RESULT ( error_status )
!
  IMPLICIT NONE

    INTEGER :: Error_Status,FileID, i, j
    character( * ) :: mw_cloud_opt_filename
    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'mw_read_cloud_opt'
!
    Error_Status = Open_Text_File( TRIM( mw_cloud_opt_filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( mw_cloud_opt_filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    Error_Status = FAILURE
    RETURN
    END IF
!
    READ(FileID, *)
    READ(FileID, *) (Frequency_cloud(i), i=1,MW_nchannels)
    READ(FileID, *) (density_cloud(i), i=1, Ndensity_cloud)
    READ(FileID, *) (Size_cloud(i), i=1, Nsize_cloud)
    READ(FileID, *)
     DO i = 1, MW_nchannels
     READ(FileID, *) (Rain_ext0(i,j),j=1,Nsize_cloud), &
                     (Rain_w0(i,j),j=1,Nsize_cloud), &
                     (Rain_g0(i,j),j=1,Nsize_cloud)
     ENDDO
!
    READ(FileID, *)
     DO i = 1, MW_nchannels
     READ(FileID, *) (Snow_ext0(i,j),j=1,Nsize_cloud), &
                     (Snow_w0(i,j),j=1,Nsize_cloud), &
                     (Snow_g0(i,j),j=1,Nsize_cloud)
     ENDDO
!
    READ(FileID, *)
     DO i = 1, MW_nchannels
     READ(FileID, *) (Gh_ext0(i,j),j=1,Nsize_cloud), &
                     (Gh_w0(i,j),j=1,Nsize_cloud), &
                     (Gh_g0(i,j),j=1,Nsize_cloud)
     ENDDO
!
    READ(FileID, *)
     DO i = 1, MW_nchannels
     READ(FileID, *) (Ice_ext0(i,j),j=1,Nsize_cloud), &
                     (Ice_w0(i,j),j=1,Nsize_cloud), &
                     (Ice_g0(i,j),j=1,Nsize_cloud)
     ENDDO
!
    CLOSE(FileID)

      Error_Status = SUCCESS
    
    end FUNCTION mw_read_cloud_opt


    FUNCTION mw_compute_cloud_opt(Frequency, Temperature, Cloud_Liquid, &
         Rain, Snow, Graupel, Ice, Ext0, Scat0, g0, &
         Rain_Size, Snow_Size, Graupel_Size, Ice_Size) RESULT ( error_status )
!
  IMPLICIT NONE

    INTEGER :: Error_Status,FileID, i, j, i1, i2, j1, j2
    ! -- Error message log file
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'mw_Compute_cloud_opt'
    REAL( fp_kind ), INTENT( IN ) :: Frequency, Temperature, Cloud_Liquid, Rain
    REAL( fp_kind ), INTENT( IN ) :: Snow, Graupel, Ice
    REAL( fp_kind ), OPTIONAL, INTENT( IN ) :: Rain_Size, Snow_Size, Graupel_SIze, Ice_Size 
    REAL( fp_kind ), INTENT( OUT ) :: Ext0, g0, Scat0
    REAL( fp_kind ) :: Ext, w, g , dx, dy
!
    Error_Status = SUCCESS
    Ext0 = ZERO
    Scat0 = ZERO
    g0 = ZERO

     IF( Cloud_Liquid > ZERO ) THEN
       call  cloud_abs_coef(1,Temperature,Frequency,Ext0)
       Ext0 = Ext0 * Cloud_Liquid
     END IF 

     IF( RAIN == ZERO .and. SNOW == ZERO .and. GRAUPEL == ZERO .and. ICE == ZERO ) RETURN

     call search_index(  MW_nchannels, Frequency_cloud, Frequency, i1, i2)
     IF( i1 == i2 ) THEN
       dx = ZERO
     ELSE
       dx = (Frequency-Frequency_cloud(i1))/(Frequency_cloud(i2)-Frequency_cloud(i1))
     END IF

    IF( RAIN > ZERO ) THEN
       dy = ZERO
      IF( PRESENT( RAIN_SIZE ) ) THEN
     call search_index(  Nsize_cloud, Size_cloud, RAIN_SIZE, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (RAIN_SIZE - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =Rain_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+Rain_ext0(i2,j1)*dx*(ONE-dy) &
            +Rain_ext0(i1,j2)*(ONE-dx)*dy+Rain_ext0(i2,j2)*dx*dy
        w =Rain_w0(i1,j1)*(ONE-dx)*(ONE-dy)+Rain_w0(i2,j1)*dx*(ONE-dy) &
            +Rain_w0(i1,j2)*(ONE-dx)*dy+Rain_w0(i2,j2)*dx*dy
        g =Rain_g0(i1,j1)*(ONE-dx)*(ONE-dy)+Rain_g0(i2,j1)*dx*(ONE-dy) &
            +Rain_g0(i1,j2)*(ONE-dx)*dy+Rain_g0(i2,j2)*dx*dy

        Ext = Ext * RAIN
        Ext0 = Ext0 + Ext 
        Scat0 = Scat0 + w * Ext
        g0 = g0 + g * w * Ext 
    END IF

    IF( SNOW > ZERO ) THEN
       dy = ZERO
      IF( PRESENT( SNOW_SIZE ) ) THEN
     call search_index(  Nsize_cloud, Size_cloud, SNOW_SIZE, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (SNOW_SIZE - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =SNOW_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+SNOW_ext0(i2,j1)*dx*(ONE-dy) &
            +SNOW_ext0(i1,j2)*(ONE-dx)*dy+SNOW_ext0(i2,j2)*dx*dy
        w =SNOW_w0(i1,j1)*(ONE-dx)*(ONE-dy)+SNOW_w0(i2,j1)*dx*(ONE-dy) &
            +SNOW_w0(i1,j2)*(ONE-dx)*dy+SNOW_w0(i2,j2)*dx*dy
        g =SNOW_g0(i1,j1)*(ONE-dx)*(ONE-dy)+SNOW_g0(i2,j1)*dx*(ONE-dy) &
            +SNOW_g0(i1,j2)*(ONE-dx)*dy+SNOW_g0(i2,j2)*dx*dy

        Ext = Ext * SNOW 
        Ext0 = Ext0 + Ext 
        Scat0 = Scat0 + w * Ext
        g0 = g0 + g * w * Ext 
    END IF

    IF( GRAUPEL > ZERO ) THEN
       dy = ZERO
      IF( PRESENT( Graupel_Size ) )  THEN
     call search_index(  Nsize_cloud, Size_cloud, Graupel_Size, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (Graupel_Size - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =Gh_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+Gh_ext0(i2,j1)*dx*(ONE-dy) &
            +Gh_ext0(i1,j2)*(ONE-dx)*dy+Gh_ext0(i2,j2)*dx*dy
        w =Gh_w0(i1,j1)*(ONE-dx)*(ONE-dy)+Gh_w0(i2,j1)*dx*(ONE-dy) &
            +Gh_w0(i1,j2)*(ONE-dx)*dy+Gh_w0(i2,j2)*dx*dy
        g =Gh_g0(i1,j1)*(ONE-dx)*(ONE-dy)+Gh_g0(i2,j1)*dx*(ONE-dy) &
            +Gh_g0(i1,j2)*(ONE-dx)*dy+Gh_g0(i2,j2)*dx*dy

        Ext = Ext * Graupel 
        Ext0 = Ext0 + Ext 
        Scat0 = Scat0 + w * Ext
        g0 = g0 + g * w * Ext 
    END IF

    IF( ICE > ZERO ) THEN
       dy = ZERO
      IF( PRESENT( Ice_Size ) ) THEN
     call search_index(  Nsize_cloud, Size_cloud, Ice_Size, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (Ice_Size - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =Ice_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+Ice_ext0(i2,j1)*dx*(ONE-dy) &
            +Ice_ext0(i1,j2)*(ONE-dx)*dy+Ice_ext0(i2,j2)*dx*dy
        w =Ice_w0(i1,j1)*(ONE-dx)*(ONE-dy)+Ice_w0(i2,j1)*dx*(ONE-dy) &
            +Ice_w0(i1,j2)*(ONE-dx)*dy+Ice_w0(i2,j2)*dx*dy
        g =Ice_g0(i1,j1)*(ONE-dx)*(ONE-dy)+Ice_g0(i2,j1)*dx*(ONE-dy) &
            +Ice_g0(i1,j2)*(ONE-dx)*dy+Ice_g0(i2,j2)*dx*dy

        Ext = Ext * Ice 
        Ext0 = Ext0 + Ext 
        Scat0 = Scat0 + w * Ext
        g0 = g0 + g * w * Ext 
    END IF

      Error_Status = SUCCESS
    
    end FUNCTION mw_compute_cloud_opt
!
      subroutine cloud_abs_coef(ITYPE,  & ! INPUT  cloud phase (1: liquid, others: ice
                          Temperature,  & ! INPUT  cloudy temperature (layer mean)
                            Frequency,  & ! INPUT  frequency
                             abs_coef)    ! OUTPUT cloud absorption coefficient
! ------------------------------------------------------------------------
!   Function: computating absorption coefficients of cloud
!             in microwave range from 1 GHz - 200 GHz where Rayleigh
!             approximation is sufficient.
! ------------------------------------------------------------------------
       IMPLICIT NONE
       INTEGER :: ITYPE,i,j,k
       REAL( fp_kind ), INTENT(IN) :: Temperature,Frequency
       REAL( fp_kind ), INTENT(OUT) :: abs_coef
       REAL( fp_kind ) :: eswi,pi,t1,t2,t3,T,F
       REAL( fp_kind ) :: EW0,TPTW,X,Y,TPTWFQ,e_real,e_imag,B2x,ext_clw
       data eswi/4.9/,pi/3.14159/
       SAVE eswi,pi
!
       T=Temperature
       F=Frequency
       if(T.lt.260.0.and.ITYPE.EQ.1) then
       T=260.0
       endif
       IF(ITYPE.EQ.1) THEN     !  Liquid water
!   converting Kelvin to Celsius
      t1=t-273.15
      t2=t1*t1
      t3=t2*t1
!   compute dielctric constant (e_real, e_imag)
        EW0=88.045-0.4147*t1+6.295E-4*t2+1.075E-5*t3
        TPTW=1.1109E-10-3.824E-12*t1+6.938E-14*t2-5.096E-16*t3
        X=TPTW*F*1.0E+9
        TPTWFQ=1+X*X
        Y=(EW0-ESWI)/TPTWFQ
        e_real=ESWI+Y
        e_imag=-X*Y
      ELSE                !   ice water
        e_real=3.15
        e_imag=-0.001
      ENDIF
!    compute absorption coefficient of cloud water
            B2x=(e_real+2.0)**2+e_imag**2
            ext_clw=3.0*e_imag/B2x
            abs_coef=-6.0 * pi * ext_clw * f/300.0
   END subroutine cloud_abs_coef
!
      subroutine cloud_abs_coef_TL(ITYPE,  & ! INPUT  cloud phase (1: liquid, others: ice
                          Temperature,  & ! INPUT  cloudy temperature (layer mean)
                            Frequency,  & ! INPUT  frequency
                             abs_coef,  & ! INPUT  absorption coefficients 
                       Temperature_TL,  & ! INPUT  Temperature tagent 
                         abs_coef_TL)   ! OUTPUT cloud absorption coefficient
! ------------------------------------------------------------------------
!   Function: computating absorption coefficients of cloud
!             in microwave range from 1 GHz - 200 GHz where Rayleigh
!             approximation is sufficient.
! ------------------------------------------------------------------------
       IMPLICIT NONE
       INTEGER :: ITYPE,i,j,k
       REAL( fp_kind ), INTENT(IN) :: Temperature,Frequency
       REAL( fp_kind ), INTENT(OUT) :: abs_coef
       REAL( fp_kind ) :: eswi,pi,t1,t2,t3,T,F
       REAL( fp_kind ) :: EW0,TPTW,X,Y,TPTWFQ,e_real,e_imag,B2x,ext_clw

       REAL( fp_kind ) :: Temperature_TL, abs_coef_TL
       REAL( fp_kind ) :: t1_TL, t_TL, t2_TL, t3_TL, EW0_TL, TPTW_TL, ext_clw_TL
       REAL( fp_kind ) :: X_TL, TPTWFQ_TL, Y_TL, e_real_TL, e_imag_TL, B2x_TL
       data eswi/4.9/,pi/3.14159/
       SAVE eswi,pi
!
       T=Temperature
       T_TL=Temperature_TL
       F=Frequency
       if(T.lt.260.0.and.ITYPE.EQ.1) then
       T=260.0
       T_TL=0.0
       endif
       IF(ITYPE.EQ.1) THEN     !  Liquid water
!   converting Kelvin to Celsius
      t1=t-273.15
      t2=t1*t1
      t3=t2*t1
      t1_TL = t_TL
      t2_TL = 2.0 * t1 * t1_TL
      t3_TL = 3.0 * t2 * t1_TL
!   compute dielctric constant (e_real, e_imag)
        EW0=88.045-0.4147*t1+6.295E-4*t2+1.075E-5*t3
        TPTW=1.1109E-10-3.824E-12*t1+6.938E-14*t2-5.096E-16*t3
        X=TPTW*F*1.0E+9
        TPTWFQ=1+X*X
        Y=(EW0-ESWI)/TPTWFQ
        e_real=ESWI+Y
        e_imag=-X*Y
      EW0_TL = -0.4147*t1_TL +6.295E-4*t2_TL +1.075E-5*t3_TL
      TPTW_TL=-3.824E-12*t1_TL +6.938E-14*t2_TL -5.096E-16*t3_TL
      X_TL=TPTW_TL *F*1.0E+9
      TPTWFQ_TL = 2.0 * X * X_TL
      Y_TL = 1.0/TPTWFQ/TPTWFQ * ( EW0_TL*TPTWFQ - (EW0-ESWI)*TPTWFQ_TL)
      e_real_TL = Y_TL
      e_imag_TL = -X_TL * Y - X * Y_TL
      ELSE                !   ice water
        e_real=3.15
        e_imag=-0.001
        e_real_TL = 0.0
        e_imag_TL = 0.0
      ENDIF
!    compute absorption coefficient of cloud water
            B2x=(e_real+2.0)**2+e_imag**2
            ext_clw=3.0*e_imag/B2x
            abs_coef=-6.0 * pi * ext_clw * f/300.0
      B2x_TL = 2.0 * (e_real+2.0)*e_real_TL + 2.0 * e_imag*e_imag_TL
      ext_clw_TL = 3.0/B2x/B2x * (e_imag_TL * B2x - e_imag * B2x_TL)
      abs_coef_TL = -6.0 * pi * ext_clw_TL * f/300.0

   END subroutine cloud_abs_coef_TL

!
      subroutine cloud_abs_coef_AD(ITYPE,  & ! INPUT  cloud phase (1: liquid, others: ice
                          Temperature,  & ! INPUT  cloudy temperature (layer mean)
                            Frequency,  & ! INPUT  frequency
                             abs_coef,  & ! INPUT  absorption coefficients 
                         abs_coef_AD,   & ! INPUT cloud absorption coefficient
                       Temperature_AD)    ! OUTPUT  Temperature tagent 
! ------------------------------------------------------------------------
!   Function: computating absorption coefficients of cloud
!             in microwave range from 1 GHz - 200 GHz where Rayleigh
!             approximation is sufficient.
! ------------------------------------------------------------------------
       IMPLICIT NONE
       INTEGER :: ITYPE,i,j,k
       REAL( fp_kind ), INTENT(IN) :: Temperature,Frequency
       REAL( fp_kind ), INTENT(INOUT) :: abs_coef
       REAL( fp_kind ) :: eswi,pi,t1,t2,t3,T,F
       REAL( fp_kind ) :: EW0,TPTW,X,Y,TPTWFQ,e_real,e_imag,B2x,ext_clw

       REAL( fp_kind ) :: Temperature_AD, abs_coef_AD
       REAL( fp_kind ) :: t1_AD, t_AD, t2_AD, t3_AD, EW0_AD, TPTW_AD, ext_clw_AD
       REAL( fp_kind ) :: X_AD, TPTWFQ_AD, Y_AD, e_real_AD, e_imag_AD, B2x_AD
       data eswi/4.9/,pi/3.14159/
       SAVE eswi,pi
!
       T=Temperature
       F=Frequency

       T_AD=0.0
     
       if(T.lt.260.0.and.ITYPE.EQ.1) then
       T=260.0
       endif

       IF(ITYPE.EQ.1) THEN     !  Liquid water
!   converting Kelvin to Celsius
      t1=t-273.15
      t2=t1*t1
      t3=t2*t1
!   compute dielctric constant (e_real, e_imag)
        EW0=88.045-0.4147*t1+6.295E-4*t2+1.075E-5*t3
        TPTW=1.1109E-10-3.824E-12*t1+6.938E-14*t2-5.096E-16*t3
        X=TPTW*F*1.0E+9
        TPTWFQ=1+X*X
        Y=(EW0-ESWI)/TPTWFQ
        e_real=ESWI+Y
        e_imag=-X*Y
      ELSE                !   ice water
        e_real=3.15
        e_imag=-0.001
      ENDIF

!    compute absorption coefficient of cloud water
            B2x=(e_real+2.0)**2+e_imag**2
            ext_clw=3.0*e_imag/B2x
            abs_coef=-6.0 * pi * ext_clw * f/300.0
     ext_clw_AD =  -6.0 * pi * abs_coef_AD * f/300.0
     e_imag_AD = 3.0/B2x * ext_clw_AD
     B2x_AD = - 3.0 * e_imag/B2x/B2x * ext_clw_AD
     e_real_AD = 2.0 * (e_real+2.0)*B2x_AD
     e_imag_AD = 2.0 * e_imag* B2x_AD + e_imag_AD

       IF(ITYPE.EQ.1) THEN     !  Liquid water
!   converting Kelvin to Celsius
      t1=t-273.15
      t2=t1*t1
      t3=t2*t1
!   compute dielctric constant (e_real, e_imag)
        EW0=88.045-0.4147*t1+6.295E-4*t2+1.075E-5*t3
        TPTW=1.1109E-10-3.824E-12*t1+6.938E-14*t2-5.096E-16*t3
        X=TPTW*F*1.0E+9
        TPTWFQ=1+X*X
        Y=(EW0-ESWI)/TPTWFQ
        e_real=ESWI+Y
        e_imag=-X*Y

      Y_AD = -X * e_imag_AD
      X_AD = -e_imag_AD * Y
      Y_AD = Y_AD + e_real_AD
      EW0_AD =  1.0/TPTWFQ * Y_AD
      TPTWFQ_AD = -(EW0-ESWI)*Y_AD/TPTWFQ/TPTWFQ
      X_AD = X_AD + 2.0 * X * TPTWFQ_AD
      TPTW_AD = X_AD  *F*1.0E+9
      t3_AD = -5.096E-16*TPTW_AD
      t2_AD = 6.938E-14*TPTW_AD
      t1_AD = -3.824E-12*TPTW_AD
      t3_AD = t3_AD +1.075E-5*EW0_AD
      t2_AD = t2_AD +6.295E-4*EW0_AD
      t1_AD = t1_AD -0.4147*EW0_AD

      t1_AD = t1_AD + t2 * t3_AD
      t2_AD = t2_AD + t1 * t3_AD
      t1_AD = t1_AD + 2.0 * t1 * t2_AD
      t_AD = t1_AD
     
      ELSE                !   ice water
        e_real=3.15
        e_imag=-0.001
        e_real_AD = 0.0
        e_imag_AD = 0.0
      ENDIF
       if(Temperature.lt.260.0.and.ITYPE.EQ.1) then
       t_AD = 0.0
       endif

       Temperature_AD = Temperature_AD + T_AD

   END subroutine cloud_abs_coef_AD
!
     SUBROUTINE search_index( N, X, X0, i1, i2)
!
     INTEGER, INTENT( IN ) :: N
     REAL( fp_kind ), DIMENSION( : ), INTENT( IN ) :: X
     REAL( fp_kind ) :: X0
     INTEGER :: i1, i2, i
!
     IF( X0 .LE. X(1) ) THEN
       i1 = 1
       i2 = 1
     ELSE IF( X0 .GE. X(N) ) THEN
       i1 = N
       i2 = N
     ELSE
       DO i = 1, N - 1
        IF( X0 .LT. X(i) ) GO TO 801
       END DO
 801   i1 = i-1
       i2 = i
     END IF
     RETURN
     END SUBROUTINE search_index


    FUNCTION mw_compute_cloud_opt_TL(Frequency, Temperature, Cloud_Liquid, &
         Rain, Snow, Graupel, Ice, Ext0, Scat0, g0, &
         Temperature_TL, Cloud_Liquid_TL, Rain_TL, Snow_TL, Graupel_TL, Ice_TL, &
         Ext0_TL, Scat0_TL, g0_TL, &
         Rain_Size, Snow_Size, Graupel_Size, Ice_Size, &
         Rain_Size_TL, Snow_Size_TL, Graupel_Size_TL, Ice_Size_TL) RESULT ( error_status ) 
!
  IMPLICIT NONE

    INTEGER :: Error_Status,FileID, i, j, i1, i2, j1, j2
    ! -- Error message log file
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'mw_Compute_cloud_opt'
    REAL( fp_kind ), INTENT( IN ) :: Frequency, Temperature, Cloud_Liquid, Rain
    REAL( fp_kind ), INTENT( IN ) :: Snow, Graupel, Ice
    REAL( fp_kind ), OPTIONAL, INTENT( IN ) :: Rain_Size, Snow_Size, Graupel_Size, Ice_Size 
    REAL( fp_kind ), INTENT( OUT ) :: Ext0, g0, Scat0
    REAL( fp_kind ) :: Ext, w, g , dx, dy
    REAL( fp_kind ) :: Ext0_TL, Scat0_TL, g0_TL
    REAL( fp_kind ) :: Temperature_TL, Cloud_Liquid_TL, Rain_TL, Snow_TL, Graupel_TL, Ice_TL
    REAL( fp_kind ), OPTIONAL, INTENT( IN OUT ) :: Rain_Size_TL, Snow_Size_TL, Graupel_Size_TL, Ice_Size_TL 
    REAL( fp_kind ) :: dx_TL, dy_TL, Ext_TL, w_TL, g_TL
!
    Error_Status = SUCCESS

    Ext0_TL = ZERO
    Scat0_TL = ZERO
    g0_TL = ZERO

     IF( Cloud_Liquid > ZERO ) THEN
       call  cloud_abs_coef(1,Temperature,Frequency,Ext0)
       Ext0 = Ext0 * Cloud_Liquid
       Ext0_TL =  Ext0_TL * Cloud_Liquid + Ext0 * Cloud_Liquid_TL
       call  cloud_abs_coef_TL(1,Temperature,Frequency,Ext0,Temperature_TL,Ext0_TL)
     END IF 

     IF( RAIN == ZERO .and. SNOW == ZERO .and. GRAUPEL == ZERO .and. ICE == ZERO ) RETURN

     call search_index(  MW_nchannels, Frequency_cloud, Frequency, i1, i2)
     IF( i1 == i2 ) THEN
       dx = ZERO
       dx_TL = ZERO
     ELSE
       dx = (Frequency-Frequency_cloud(i1))/(Frequency_cloud(i2)-Frequency_cloud(i1))
       dx_TL = ZERO
     END IF

    IF( RAIN > ZERO ) THEN
       dy = ZERO
       dy_TL = ZERO
      IF( PRESENT( RAIN_SIZE ) ) THEN
     call search_index(  Nsize_cloud, Size_cloud, RAIN_SIZE, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (RAIN_SIZE - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       dy_TL = RAIN_SIZE_TL/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =Rain_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+Rain_ext0(i2,j1)*dx*(ONE-dy) &
            +Rain_ext0(i1,j2)*(ONE-dx)*dy+Rain_ext0(i2,j2)*dx*dy
        w =Rain_w0(i1,j1)*(ONE-dx)*(ONE-dy)+Rain_w0(i2,j1)*dx*(ONE-dy) &
            +Rain_w0(i1,j2)*(ONE-dx)*dy+Rain_w0(i2,j2)*dx*dy
        g =Rain_g0(i1,j1)*(ONE-dx)*(ONE-dy)+Rain_g0(i2,j1)*dx*(ONE-dy) &
            +Rain_g0(i1,j2)*(ONE-dx)*dy+Rain_g0(i2,j2)*dx*dy

        Ext = Ext * RAIN
        Ext0 = Ext0 + Ext 
        Scat0 = Scat0 + w * Ext
        g0 = g0 + g * w * Ext 

        Ext_TL =-Rain_ext0(i1,j1)*dx_TL*(ONE-dy) &
               -Rain_ext0(i1,j1)*(ONE-dx)*dy_TL &
               +Rain_ext0(i2,j1)*dx_TL*(ONE-dy) &
               -Rain_ext0(i2,j1)*dx * dy_TL &
               -Rain_ext0(i1,j2)*dx_TL * dy &
               +Rain_ext0(i1,j2)*(ONE-dx)*dy_TL &
               +Rain_ext0(i2,j2)*dx_TL * dy &
               +Rain_ext0(i2,j2)*dx * dy_TL 

        w_TL =-Rain_w0(i1,j1)*dx_TL*(ONE-dy) &
               -Rain_w0(i1,j1)*(ONE-dx)*dy_TL &
               +Rain_w0(i2,j1)*dx_TL*(ONE-dy) &
               -Rain_w0(i2,j1)*dx * dy_TL &
               -Rain_w0(i1,j2)*dx_TL * dy &
               +Rain_w0(i1,j2)*(ONE-dx)*dy_TL &
               +Rain_w0(i2,j2)*dx_TL * dy &
               +Rain_w0(i2,j2)*dx * dy_TL 

        g_TL =-Rain_g0(i1,j1)*dx_TL*(ONE-dy) &
               -Rain_g0(i1,j1)*(ONE-dx)*dy_TL &
               +Rain_g0(i2,j1)*dx_TL*(ONE-dy) &
               -Rain_g0(i2,j1)*dx * dy_TL &
               -Rain_g0(i1,j2)*dx_TL * dy &
               +Rain_g0(i1,j2)*(ONE-dx)*dy_TL &
               +Rain_g0(i2,j2)*dx_TL * dy &
               +Rain_g0(i2,j2)*dx * dy_TL 

        Ext_TL = Ext_TL * RAIN + Ext * RAIN_TL
        Ext0_TL = Ext0_TL + Ext_TL 
        Scat0_TL = Scat0_TL + w_TL * Ext + w * Ext_TL
        g0_TL = g0_TL + g_TL * w * Ext + g * w_TL * Ext + g * w * Ext_TL 

    END IF

    IF( SNOW > ZERO ) THEN
       dy = ZERO
       dy_TL = ZERO
      IF( PRESENT( SNOW_SIZE ) ) THEN
     call search_index(  Nsize_cloud, Size_cloud, SNOW_SIZE, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (SNOW_SIZE - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       dy_TL=SNOW_SIZE_TL/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =SNOW_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+SNOW_ext0(i2,j1)*dx*(ONE-dy) &
            +SNOW_ext0(i1,j2)*(ONE-dx)*dy+SNOW_ext0(i2,j2)*dx*dy
        w =SNOW_w0(i1,j1)*(ONE-dx)*(ONE-dy)+SNOW_w0(i2,j1)*dx*(ONE-dy) &
            +SNOW_w0(i1,j2)*(ONE-dx)*dy+SNOW_w0(i2,j2)*dx*dy
        g =SNOW_g0(i1,j1)*(ONE-dx)*(ONE-dy)+SNOW_g0(i2,j1)*dx*(ONE-dy) &
            +SNOW_g0(i1,j2)*(ONE-dx)*dy+SNOW_g0(i2,j2)*dx*dy

        Ext = Ext * SNOW 
        Ext0 = Ext0 + Ext 
        Scat0 = Scat0 + w * Ext
        g0 = g0 + g * w * Ext 

        Ext_TL =-SNOW_ext0(i1,j1)*dx_TL*(ONE-dy) &
               -SNOW_ext0(i1,j1)*(ONE-dx)*dy_TL &
               +SNOW_ext0(i2,j1)*dx_TL*(ONE-dy) &
               -SNOW_ext0(i2,j1)*dx * dy_TL &
               -SNOW_ext0(i1,j2)*dx_TL * dy &
               +SNOW_ext0(i1,j2)*(ONE-dx)*dy_TL &
               +SNOW_ext0(i2,j2)*dx_TL * dy &
               +SNOW_ext0(i2,j2)*dx * dy_TL 

        w_TL =-SNOW_w0(i1,j1)*dx_TL*(ONE-dy) &
               -SNOW_w0(i1,j1)*(ONE-dx)*dy_TL &
               +SNOW_w0(i2,j1)*dx_TL*(ONE-dy) &
               -SNOW_w0(i2,j1)*dx * dy_TL &
               -SNOW_w0(i1,j2)*dx_TL * dy &
               +SNOW_w0(i1,j2)*(ONE-dx)*dy_TL &
               +SNOW_w0(i2,j2)*dx_TL * dy &
               +SNOW_w0(i2,j2)*dx * dy_TL 

        g_TL =-SNOW_g0(i1,j1)*dx_TL*(ONE-dy) &
               -SNOW_g0(i1,j1)*(ONE-dx)*dy_TL &
               +SNOW_g0(i2,j1)*dx_TL*(ONE-dy) &
               -SNOW_g0(i2,j1)*dx * dy_TL &
               -SNOW_g0(i1,j2)*dx_TL * dy &
               +SNOW_g0(i1,j2)*(ONE-dx)*dy_TL &
               +SNOW_g0(i2,j2)*dx_TL * dy &
               +SNOW_g0(i2,j2)*dx * dy_TL 

        Ext_TL = Ext_TL * SNOW + Ext * SNOW_TL
        Ext0_TL = Ext0_TL + Ext_TL 
        Scat0_TL = Scat0_TL + w_TL * Ext + w * Ext_TL
        g0_TL = g0_TL + g_TL * w * Ext + g * w_TL * Ext + g * w * Ext_TL 

    END IF

    IF( GRAUPEL > ZERO ) THEN
       dy = ZERO
       dy_TL = ZERO
      IF( PRESENT( Graupel_Size ) )  THEN
     call search_index(  Nsize_cloud, Size_cloud, Graupel_Size, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (Graupel_Size - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       dy_TL=Graupel_Size_TL/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =Gh_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+Gh_ext0(i2,j1)*dx*(ONE-dy) &
            +Gh_ext0(i1,j2)*(ONE-dx)*dy+Gh_ext0(i2,j2)*dx*dy
        w =Gh_w0(i1,j1)*(ONE-dx)*(ONE-dy)+Gh_w0(i2,j1)*dx*(ONE-dy) &
            +Gh_w0(i1,j2)*(ONE-dx)*dy+Gh_w0(i2,j2)*dx*dy
        g =Gh_g0(i1,j1)*(ONE-dx)*(ONE-dy)+Gh_g0(i2,j1)*dx*(ONE-dy) &
            +Gh_g0(i1,j2)*(ONE-dx)*dy+Gh_g0(i2,j2)*dx*dy

        Ext = Ext * Graupel 
        Ext0 = Ext0 + Ext 
        Scat0 = Scat0 + w * Ext
        g0 = g0 + g * w * Ext 

        Ext_TL =-Gh_ext0(i1,j1)*dx_TL*(ONE-dy) &
               -Gh_ext0(i1,j1)*(ONE-dx)*dy_TL &
               +Gh_ext0(i2,j1)*dx_TL*(ONE-dy) &
               -Gh_ext0(i2,j1)*dx * dy_TL &
               -Gh_ext0(i1,j2)*dx_TL * dy &
               +Gh_ext0(i1,j2)*(ONE-dx)*dy_TL &
               +Gh_ext0(i2,j2)*dx_TL * dy &
               +Gh_ext0(i2,j2)*dx * dy_TL 

        w_TL =-Gh_w0(i1,j1)*dx_TL*(ONE-dy) &
               -Gh_w0(i1,j1)*(ONE-dx)*dy_TL &
               +Gh_w0(i2,j1)*dx_TL*(ONE-dy) &
               -Gh_w0(i2,j1)*dx * dy_TL &
               -Gh_w0(i1,j2)*dx_TL * dy &
               +Gh_w0(i1,j2)*(ONE-dx)*dy_TL &
               +Gh_w0(i2,j2)*dx_TL * dy &
               +Gh_w0(i2,j2)*dx * dy_TL 

        g_TL =-Gh_g0(i1,j1)*dx_TL*(ONE-dy) &
               -Gh_g0(i1,j1)*(ONE-dx)*dy_TL &
               +Gh_g0(i2,j1)*dx_TL*(ONE-dy) &
               -Gh_g0(i2,j1)*dx * dy_TL &
               -Gh_g0(i1,j2)*dx_TL * dy &
               +Gh_g0(i1,j2)*(ONE-dx)*dy_TL &
               +Gh_g0(i2,j2)*dx_TL * dy &
               +Gh_g0(i2,j2)*dx * dy_TL 

        Ext_TL = Ext_TL * Graupel + Ext * Graupel_TL
        Ext0_TL = Ext0_TL + Ext_TL 
        Scat0_TL = Scat0_TL + w_TL * Ext + w * Ext_TL
        g0_TL = g0_TL + g_TL * w * Ext + g * w_TL * Ext + g * w * Ext_TL 
    END IF

    IF( ICE > ZERO ) THEN
       dy = ZERO
       dy_TL = ZERO
      IF( PRESENT( Ice_Size ) ) THEN
     call search_index(  Nsize_cloud, Size_cloud, Ice_Size, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (Ice_Size - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       dy_TL=Ice_Size_TL/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =Ice_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+Ice_ext0(i2,j1)*dx*(ONE-dy) &
            +Ice_ext0(i1,j2)*(ONE-dx)*dy+Ice_ext0(i2,j2)*dx*dy
        w =Ice_w0(i1,j1)*(ONE-dx)*(ONE-dy)+Ice_w0(i2,j1)*dx*(ONE-dy) &
            +Ice_w0(i1,j2)*(ONE-dx)*dy+Ice_w0(i2,j2)*dx*dy
        g =Ice_g0(i1,j1)*(ONE-dx)*(ONE-dy)+Ice_g0(i2,j1)*dx*(ONE-dy) &
            +Ice_g0(i1,j2)*(ONE-dx)*dy+Ice_g0(i2,j2)*dx*dy

        Ext = Ext * Ice 
        Ext0 = Ext0 + Ext 
        Scat0 = Scat0 + w * Ext
        g0 = g0 + g * w * Ext 

        Ext_TL =-Ice_ext0(i1,j1)*dx_TL*(ONE-dy) &
               -Ice_ext0(i1,j1)*(ONE-dx)*dy_TL &
               +Ice_ext0(i2,j1)*dx_TL*(ONE-dy) &
               -Ice_ext0(i2,j1)*dx * dy_TL &
               -Ice_ext0(i1,j2)*dx_TL * dy &
               +Ice_ext0(i1,j2)*(ONE-dx)*dy_TL &
               +Ice_ext0(i2,j2)*dx_TL * dy &
               +Ice_ext0(i2,j2)*dx * dy_TL 

        w_TL =-Ice_w0(i1,j1)*dx_TL*(ONE-dy) &
               -Ice_w0(i1,j1)*(ONE-dx)*dy_TL &
               +Ice_w0(i2,j1)*dx_TL*(ONE-dy) &
               -Ice_w0(i2,j1)*dx * dy_TL &
               -Ice_w0(i1,j2)*dx_TL * dy &
               +Ice_w0(i1,j2)*(ONE-dx)*dy_TL &
               +Ice_w0(i2,j2)*dx_TL * dy &
               +Ice_w0(i2,j2)*dx * dy_TL 

        g_TL =-Ice_g0(i1,j1)*dx_TL*(ONE-dy) &
               -Ice_g0(i1,j1)*(ONE-dx)*dy_TL &
               +Ice_g0(i2,j1)*dx_TL*(ONE-dy) &
               -Ice_g0(i2,j1)*dx * dy_TL &
               -Ice_g0(i1,j2)*dx_TL * dy &
               +Ice_g0(i1,j2)*(ONE-dx)*dy_TL &
               +Ice_g0(i2,j2)*dx_TL * dy &
               +Ice_g0(i2,j2)*dx * dy_TL 

        Ext_TL = Ext_TL * Ice + Ext * Ice_TL
        Ext0_TL = Ext0_TL + Ext_TL 
        Scat0_TL = Scat0_TL + w_TL * Ext + w * Ext_TL
        g0_TL = g0_TL + g_TL * w * Ext + g * w_TL * Ext + g * w * Ext_TL 
    END IF

      Error_Status = SUCCESS
    
    end FUNCTION mw_compute_cloud_opt_TL


    FUNCTION mw_compute_cloud_opt_AD(Frequency, Temperature, Cloud_Liquid, & ! Input
         Rain, Snow, Graupel, Ice, Ext0, Scat0, g0, &  ! Input
         Ext0_AD, Scat0_AD, g0_AD, &                   ! Input
         Temperature_AD, Cloud_Liquid_AD, Rain_AD, Snow_AD, Graupel_AD, Ice_AD, & ! Output
         Rain_Size, Snow_Size, Graupel_Size, Ice_Size, &  ! Input
         Rain_Size_AD, Snow_Size_AD, Graupel_Size_AD, Ice_Size_AD) RESULT ( error_status ) 
!
  IMPLICIT NONE

    INTEGER :: Error_Status,FileID, i, j, i1, i2, j1, j2
    ! -- Error message log file
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'mw_Compute_cloud_opt'
    REAL( fp_kind ), INTENT( IN ) :: Frequency, Temperature, Cloud_Liquid, Rain
    REAL( fp_kind ), INTENT( IN ) :: Snow, Graupel, Ice
    REAL( fp_kind ), OPTIONAL, INTENT( IN ) :: Rain_Size, Snow_Size, Graupel_Size, Ice_Size 
    REAL( fp_kind ), INTENT( OUT ) :: Ext0, g0, Scat0
    REAL( fp_kind ) :: Ext, w, g , dx, dy, Ext1
    REAL( fp_kind ) :: Ext0_AD, Scat0_AD, g0_AD
    REAL( fp_kind ) :: Temperature_AD, Cloud_Liquid_AD, Rain_AD, Snow_AD, Graupel_AD, Ice_AD
    REAL( fp_kind ), OPTIONAL, INTENT( IN OUT ) :: Rain_Size_AD, Snow_Size_AD, Graupel_Size_AD, Ice_Size_AD 
    REAL( fp_kind ) :: dx_AD, dy_AD, Ext_AD, w_AD, g_AD
!
    dx_AD = ZERO
    dy_AD = ZERO
    Ext_AD = ZERO
    w_AD = ZERO
    g_AD = ZERO

    Error_Status = SUCCESS

     IF( RAIN == ZERO .and. SNOW == ZERO .and. GRAUPEL == ZERO .and. ICE == ZERO ) GO TO 801 

     call search_index(  MW_nchannels, Frequency_cloud, Frequency, i1, i2)
     IF( i1 == i2 ) THEN
       dx = ZERO
       dx_AD = ZERO
     ELSE
       dx = (Frequency-Frequency_cloud(i1))/(Frequency_cloud(i2)-Frequency_cloud(i1))
       dx_AD = ZERO
     END IF

    IF( RAIN > ZERO ) THEN
       dy = ZERO
      IF( PRESENT( RAIN_SIZE ) ) THEN
     call search_index(  Nsize_cloud, Size_cloud, RAIN_SIZE, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (RAIN_SIZE - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =Rain_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+Rain_ext0(i2,j1)*dx*(ONE-dy) &
            +Rain_ext0(i1,j2)*(ONE-dx)*dy+Rain_ext0(i2,j2)*dx*dy
        w =Rain_w0(i1,j1)*(ONE-dx)*(ONE-dy)+Rain_w0(i2,j1)*dx*(ONE-dy) &
            +Rain_w0(i1,j2)*(ONE-dx)*dy+Rain_w0(i2,j2)*dx*dy
        g =Rain_g0(i1,j1)*(ONE-dx)*(ONE-dy)+Rain_g0(i2,j1)*dx*(ONE-dy) &
            +Rain_g0(i1,j2)*(ONE-dx)*dy+Rain_g0(i2,j2)*dx*dy

        Ext1=Ext
        Ext = Ext * RAIN

        Ext_AD = g * w * g0_AD
        w_AD =  g * g0_AD * Ext
        g_AD =  g0_AD * w * Ext
 
        Ext_AD = Ext_AD + w * Scat0_AD
        w_AD = w_AD + Scat0_AD * Ext

        Ext_AD = Ext_AD + Ext0_AD

!        RAIN_AD = Ext * Ext_AD
        RAIN_AD = Ext1 * Ext_AD
        Ext_AD = Ext_AD * RAIN

        dy_AD = -Rain_g0(i1,j1)*(ONE-dx)*g_AD   &
                -Rain_g0(i2,j1)*dx *g_AD        &
                +Rain_g0(i1,j2)*(ONE-dx)*g_AD   &
                +Rain_g0(i2,j2)*dx * g_AD

        dy_AD = dy_AD -Rain_w0(i1,j1)*(ONE-dx)*w_AD &
                -Rain_w0(i2,j1)*dx * w_AD            &
                +Rain_w0(i1,j2)*(ONE-dx)*w_AD        &
                +Rain_w0(i2,j2)*dx * w_AD

        dy_AD = dy_AD -Rain_ext0(i1,j1)*(ONE-dx)*Ext_AD   &
                -Rain_ext0(i2,j1)*dx * Ext_AD             &
                +Rain_ext0(i1,j2)*(ONE-dx)*Ext_AD         &
                +Rain_ext0(i2,j2)*dx * Ext_AD

      IF( PRESENT( RAIN_SIZE ) ) THEN
       IF( j1 .NE. j2 ) THEN
       RAIN_SIZE_AD = dy_AD/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      END IF
       dy_AD = ZERO

    END IF

    IF( SNOW > ZERO ) THEN
       dy = ZERO
      IF( PRESENT( SNOW_SIZE ) ) THEN
     call search_index(  Nsize_cloud, Size_cloud, SNOW_SIZE, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (SNOW_SIZE - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =SNOW_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+SNOW_ext0(i2,j1)*dx*(ONE-dy) &
            +SNOW_ext0(i1,j2)*(ONE-dx)*dy+SNOW_ext0(i2,j2)*dx*dy
        w =SNOW_w0(i1,j1)*(ONE-dx)*(ONE-dy)+SNOW_w0(i2,j1)*dx*(ONE-dy) &
            +SNOW_w0(i1,j2)*(ONE-dx)*dy+SNOW_w0(i2,j2)*dx*dy
        g =SNOW_g0(i1,j1)*(ONE-dx)*(ONE-dy)+SNOW_g0(i2,j1)*dx*(ONE-dy) &
            +SNOW_g0(i1,j2)*(ONE-dx)*dy+SNOW_g0(i2,j2)*dx*dy

        Ext1=Ext
        Ext = Ext * SNOW 

        Ext_AD = g * w * g0_AD
        w_AD =  g * g0_AD * Ext
        g_AD =  g0_AD * w * Ext
        
        Ext_AD = Ext_AD + w * Scat0_AD
        w_AD = w_AD + Scat0_AD * Ext

        Ext_AD = Ext_AD + Ext0_AD

!        SNOW_AD = Ext * Ext_AD
        SNOW_AD = Ext1 * Ext_AD
        Ext_AD = Ext_AD * SNOW 

        dy_AD = -SNOW_g0(i1,j1)*(ONE-dx)*g_AD   &
                -SNOW_g0(i2,j1)*dx *g_AD        &
                +SNOW_g0(i1,j2)*(ONE-dx)*g_AD   &
                +SNOW_g0(i2,j2)*dx * g_AD

        dy_AD = dy_AD -SNOW_w0(i1,j1)*(ONE-dx)*w_AD &
                -SNOW_w0(i2,j1)*dx * w_AD            &
                +SNOW_w0(i1,j2)*(ONE-dx)*w_AD        &
                +SNOW_w0(i2,j2)*dx * w_AD

        dy_AD = dy_AD -SNOW_ext0(i1,j1)*(ONE-dx)*Ext_AD   &
                -SNOW_ext0(i2,j1)*dx * Ext_AD             &
                +SNOW_ext0(i1,j2)*(ONE-dx)*Ext_AD         &
                +SNOW_ext0(i2,j2)*dx * Ext_AD

      IF( PRESENT( RAIN_SIZE ) ) THEN
       IF( j1 .NE. j2 ) THEN
       SNOW_SIZE_AD = dy_AD/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      END IF
       dy_AD = ZERO
    END IF

    IF( GRAUPEL > ZERO ) THEN
       dy = ZERO
      IF( PRESENT( Graupel_Size ) )  THEN
     call search_index(  Nsize_cloud, Size_cloud, Graupel_Size, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (Graupel_Size - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =Gh_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+Gh_ext0(i2,j1)*dx*(ONE-dy) &
            +Gh_ext0(i1,j2)*(ONE-dx)*dy+Gh_ext0(i2,j2)*dx*dy
        w =Gh_w0(i1,j1)*(ONE-dx)*(ONE-dy)+Gh_w0(i2,j1)*dx*(ONE-dy) &
            +Gh_w0(i1,j2)*(ONE-dx)*dy+Gh_w0(i2,j2)*dx*dy
        g =Gh_g0(i1,j1)*(ONE-dx)*(ONE-dy)+Gh_g0(i2,j1)*dx*(ONE-dy) &
            +Gh_g0(i1,j2)*(ONE-dx)*dy+Gh_g0(i2,j2)*dx*dy

        Ext1=Ext
        Ext = Ext * Graupel 

        Ext_AD = g * w * g0_AD
        w_AD =  g * g0_AD * Ext
        g_AD =  g0_AD * w * Ext
        
        Ext_AD = Ext_AD + w * Scat0_AD
        w_AD = w_AD + Scat0_AD * Ext

        Ext_AD = Ext_AD + Ext0_AD

!        Graupel_AD = Ext * Ext_AD
        Graupel_AD = Ext1 * Ext_AD
        Ext_AD = Ext_AD * Graupel 

        dy_AD = -gh_g0(i1,j1)*(ONE-dx)*g_AD   &
                -gh_g0(i2,j1)*dx *g_AD        &
                +gh_g0(i1,j2)*(ONE-dx)*g_AD   &
                +gh_g0(i2,j2)*dx * g_AD

        dy_AD = dy_AD -gh_w0(i1,j1)*(ONE-dx)*w_AD &
                -gh_w0(i2,j1)*dx * w_AD            &
                +gh_w0(i1,j2)*(ONE-dx)*w_AD        &
                +gh_w0(i2,j2)*dx * w_AD

        dy_AD = dy_AD -gh_ext0(i1,j1)*(ONE-dx)*Ext_AD   &
                -gh_ext0(i2,j1)*dx * Ext_AD             &
                +gh_ext0(i1,j2)*(ONE-dx)*Ext_AD         &
                +gh_ext0(i2,j2)*dx * Ext_AD

      IF( PRESENT( RAIN_SIZE ) ) THEN
       IF( j1 .NE. j2 ) THEN
       Graupel_SIZE_AD = dy_AD/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      END IF
       dy_AD = ZERO

    END IF

    IF( ICE > ZERO ) THEN
       dy = ZERO
      IF( PRESENT( Ice_Size ) ) THEN
     call search_index(  Nsize_cloud, Size_cloud, Ice_Size, j1, j2)
       IF( j1 .NE. j2 ) THEN
       dy = (Ice_Size - Size_cloud(j1))/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      ELSE
      j1 = 1
      j2 = j1
      END IF
        Ext =Ice_ext0(i1,j1)*(ONE-dx)*(ONE-dy)+Ice_ext0(i2,j1)*dx*(ONE-dy) &
            +Ice_ext0(i1,j2)*(ONE-dx)*dy+Ice_ext0(i2,j2)*dx*dy
        w =Ice_w0(i1,j1)*(ONE-dx)*(ONE-dy)+Ice_w0(i2,j1)*dx*(ONE-dy) &
            +Ice_w0(i1,j2)*(ONE-dx)*dy+Ice_w0(i2,j2)*dx*dy
        g =Ice_g0(i1,j1)*(ONE-dx)*(ONE-dy)+Ice_g0(i2,j1)*dx*(ONE-dy) &
            +Ice_g0(i1,j2)*(ONE-dx)*dy+Ice_g0(i2,j2)*dx*dy

        Ext1=Ext
        Ext = Ext * Ice 
        Ext0 = Ext0 + Ext 
        Scat0 = Scat0 + w * Ext
        g0 = g0 + g * w * Ext 

        Ext_AD = g * w * g0_AD
        w_AD =  g * g0_AD * Ext
        g_AD =  g0_AD * w * Ext
        
        Ext_AD = Ext_AD + w * Scat0_AD
        w_AD = w_AD + Scat0_AD * Ext

        Ext_AD = Ext_AD + Ext0_AD

!        Ice_AD = Ext * Ext_AD
        Ice_AD = Ext1 * Ext_AD
        Ext_AD = Ext_AD * Ice 

        dy_AD = -Ice_g0(i1,j1)*(ONE-dx)*g_AD   &
                -Ice_g0(i2,j1)*dx *g_AD        &
                +Ice_g0(i1,j2)*(ONE-dx)*g_AD   &
                +Ice_g0(i2,j2)*dx * g_AD

        dy_AD = dy_AD -Ice_w0(i1,j1)*(ONE-dx)*w_AD &
                -Ice_w0(i2,j1)*dx * w_AD            &
                +Ice_w0(i1,j2)*(ONE-dx)*w_AD        &
                +Ice_w0(i2,j2)*dx * w_AD

        dy_AD = dy_AD -Ice_ext0(i1,j1)*(ONE-dx)*Ext_AD   &
                -Ice_ext0(i2,j1)*dx * Ext_AD             &
                +Ice_ext0(i1,j2)*(ONE-dx)*Ext_AD         &
                +Ice_ext0(i2,j2)*dx * Ext_AD

      IF( PRESENT( RAIN_SIZE ) ) THEN
       IF( j1 .NE. j2 ) THEN
       Ice_SIZE_AD = dy_AD/(Size_cloud(j2)-Size_cloud(j1))
       END IF
      END IF
       dy_AD = ZERO
    END IF

 801 CONTINUE
     IF( Cloud_Liquid > ZERO ) THEN
       call  cloud_abs_coef(1,Temperature,Frequency,Ext1)
       Cloud_Liquid_AD = Ext1 * Ext0_AD
       Ext0_AD = Ext0_AD * Cloud_Liquid
       call  cloud_abs_coef_AD(1,Temperature,Frequency,Ext1,Ext0_AD,Temperature_AD)
     END IF 

    Ext0_AD = ZERO
    Scat0_AD = ZERO
    g0_AD = ZERO

      Error_Status = SUCCESS

   END FUNCTION mw_compute_cloud_opt_AD 

 END MODULE mw_cloud_opt

