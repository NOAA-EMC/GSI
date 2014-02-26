!
  SUBROUTINE rttov_fastem4_k(Frequency   ,    &  ! Input
                              Zenith_Angle,    &  ! Input
                              Temperature ,    &  ! Input
                              Salinity    ,    &  ! Input
                              Wind_Speed  ,    &  ! Input
                              Emissivity_k,   &  ! Input
                              Reflectivity_k, &  ! Input
                              Temperature_k,  &  ! Output
                              Salinity_k ,    &  ! Output
                              Wind_Speed_k,   &  ! Output
                              Emissivity  ,    &  ! Output
                              Reflectivity,    &  ! Output
                              Transmittance,   &  ! Input, may not be used
                              Rel_Azimuth  ,   &  ! Input, may not be used
                              Transmittance_k,&  ! Output
                              Rel_Azimuth_k )    ! Output
  ! Description:
  ! K of rttov_fastem4, this file is the same as rttov_fastem4_k, except for
  ! the suffix change from _k to _k.
  ! To compute FASTEM-4 emissivities and reflectivities.
  !
  ! Copyright:
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2009, EUMETSAT, All Rights Reserved.
  !
  ! Method:
  ! An improved fast microwave sea surface emissivity model, FASTEM4
  ! Liu, Q., S. English, F. Weng, 2009: Report in prepare
  !
  ! It is an extension of the FASTEM-3 English 2003.
  ! http://www.metoffice.com/research/interproj/nwpsaf/rtm/evalfastems.pdf
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       27/08/2009  New F90 code (Q. Liu)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  !
  ! Imported Parameters:
    USE mod_rttov_fastem4_coef, ONLY : FresnelVariables_type, PermittivityVariables_type,&
        fp, ZERO, POINT_5, ONE, TWO, THREE,PI,DEGREES_TO_RADIANS,transmittance_limit_lower,&
        transmittance_limit_upper, e0, min_f, max_f, min_wind, max_wind, A_COEF, Lcoef,&
        Scoef, t_c, b_coef, FR_COEFF,x,y
    IMPLICIT NONE
    ! Arguments
    REAL(fp),        INTENT(IN)     :: Frequency
    REAL(fp),        INTENT(IN)     :: Zenith_Angle
    REAL(fp),        INTENT(IN)     :: Temperature
    REAL(fp),        INTENT(IN)     :: Salinity
    REAL(fp),        INTENT(IN)     :: Wind_Speed
    REAL(fp),        INTENT(IN)  :: Transmittance
    REAL(fp),        INTENT(IN)  :: Rel_Azimuth
    REAL(fp),        INTENT(INOUT)  :: Emissivity_k(4), Reflectivity_k(4)
    REAL(fp),        INTENT(OUT)    :: Emissivity(4), Reflectivity(4)
    REAL(fp),        INTENT(INOUT)  :: Temperature_k,Salinity_k,Wind_Speed_k
    REAL(fp),        INTENT(INOUT)  :: Transmittance_k, Rel_Azimuth_k

!INTF_END

  !local variable

    REAL(fp) :: cos_z, Foam_Cover
    REAL(fp) :: scor, small_corr, Azimuth_Emi(4),RV_Fresnel,RH_Fresnel
    REAL(fp) :: Ev,Eh,RvL,RhL,RvS,RhS
    REAL(fp) :: zreflmod_v,zreflmod_h,zrough_v,zrough_h
    INTEGER :: i, j, L, m

    REAL(fp) :: Foam_Rv,Foam_Rh
    ! Local variables for the permittivity model
    REAL(fp) :: einf, sigma25
    REAL(fp) :: tau1, tau2, es, e1
    REAL(fp) :: perm_Real, perm_imag
!    REAL(fp) ::
    TYPE(PermittivityVariables_type) :: iVar
    COMPLEX( fp ) :: Permittivity

    ! Local variables for Fresnel reflectance
    COMPLEX(fp) :: zRv ! Vertical
    COMPLEX(fp) :: zRh ! Horizontal
    TYPE(FresnelVariables_type) :: frVar

    ! Local variables for small-scale
    REAL(fp) :: windspeed, freq_S
    ! Local variables for large-scale
    REAL(fp) :: seczen, zc(12)

    ! Local variables for including transmittance
    REAL(fp) :: variance,varm,opdpsfc,zx(9)

    ! Foam reflectance
    REAL(fp) :: Fv, Fh, Foam_ref

    ! Local variables for azimuth angle
    REAL(fp) :: asc(4,3), fre_c, phi

  ! ad part
    REAL(fp) :: scor_k, small_corr_k, Azimuth_Emi_k(4)
    REAL(fp) :: Foam_Cover_k,RV_Fresnel_k,RH_Fresnel_k
    REAL(fp) :: Ev_k,Eh_k,RvL_k,RhL_k,RvS_k,RhS_k
    COMPLEX( fp ) ::  Permittivity_k

    REAL( fp ) :: ac_k, sc_k, phi_k,wind10_k,zreflmod_v_k,zreflmod_h_k

    REAL(fp) :: sigma_k
    REAL(fp) :: einf_k, e1_k, es_k, ce1_k, ces_k
    REAL(fp) :: t_k, t_sq_k, t_cu_k, S_k, beta_k, delta_k, sigma25_k
    REAL(fp) :: tau1_k, tau2_k, ctau1_k, ctau2_k, f1_k, f2_k, del1_k, del2_k
    REAL(fp) :: perm_Real_k, perm_imag_k
    ! Local variables
    COMPLEX(fp) :: z1_k, z2_k
    COMPLEX(fp) :: zRv_k           ! Vertical
    COMPLEX(fp) :: zRh_k           ! Horizontal
    REAL(fp)    :: rzRv_k,izRv_k  ! Vertical
    REAL(fp)    :: rzRh_k,izRh_k  ! Horizontal
    COMPLEX(fp) :: denom
    REAL(fp) :: variance_k,varm_k,opdpsfc_k,zx_k(9),zrough_v_k,zrough_h_k


    cos_z = cos( Zenith_Angle*DEGREES_TO_RADIANS )

  ! Permittivity Calculation
  ! ------------------------
    !1.2 calculate permittivity using double-debye formula
    !-----------------------------------------------------
    !Set values for temperature polynomials (convert from kelvin to celsius)
    iVar%t = Temperature - 273.15_fp
    iVar%t_sq = iVar%t * iVar%t     !quadratic
    iVar%t_cu = iVar%t_sq * iVar%t  !cubic
    iVar%S = Salinity
    !-----------------------------------------------------
    !1.2 Pure or fresh water
    !-----------------------------------------------------
    einf = A_COEF(0) + A_COEF(1)*iVar%t
    es   = A_COEF(2) + A_COEF(3)*iVar%t  + A_COEF(4)*iVar%t_sq + A_COEF(5)*iVar%t_cu
    e1   = A_COEF(9) + A_COEF(10)*iVar%t + A_COEF(11)*iVar%t_sq
    tau1 = A_COEF(15) + A_COEF(16)*iVar%t + A_COEF(17)*iVar%t_sq + A_COEF(18)*iVar%t_cu
    tau2 = A_COEF(22) + A_COEF(23)*iVar%t + A_COEF(24)*iVar%t_sq + A_COEF(25)*iVar%t_cu

    iVar%es_k = es
    iVar%e1_k = e1
    iVar%tau1_k = tau1
    iVar%tau2_k = tau2
    perm_imag = ZERO

    IF( iVar%S > ZERO ) THEN
      iVar%delta = 25.0_fp - iVar%t
      iVar%beta  = A_COEF(29) +A_COEF(30)*iVar%delta +A_COEF(31)*iVar%delta**2  &
            + iVar%S*(A_COEF(32) +A_COEF(33)*iVar%delta +A_COEF(34)*iVar%delta**2)
      sigma25 = iVar%S*(A_COEF(35) +A_COEF(36)*iVar%S +A_COEF(37)*iVar%S**2  &
              +A_COEF(38)*iVar%S**3)
      iVar%sigma = sigma25*exp(-iVar%delta*iVar%beta)

      iVar%ces = ONE + iVar%S*(A_COEF(6) + A_COEF(7)*iVar%S + A_COEF(8)*iVar%t )
      iVar%ce1 = ONE + iVar%S*(A_COEF(12) + A_COEF(13)*iVar%S +A_COEF(14)*iVar%t )
      iVar%ctau1 = ONE + iVar%S*(A_COEF(19) +A_COEF(20)*iVar%t + A_COEF(21)*iVar%t_sq)
      iVar%ctau2 = ONE + iVar%S*(A_COEF(26) + A_COEF(27)*iVar%t + A_COEF(28)*iVar%S**2 )
      es = iVar%es_k * iVar%ces
      e1 = iVar%e1_k * iVar%ce1
      tau1 = iVar%tau1_k * iVar%ctau1
      tau2 = iVar%tau2_k * iVar%ctau2
      perm_imag = -iVar%sigma/(TWO*PI*e0*Frequency)
    END IF
    !Define two relaxation frequencies, f1 and f2
    iVar%f1 = Frequency*tau1
    iVar%f2 = Frequency*tau2
    iVar%del1 = es - e1
    iVar%del2 = e1 - einf

    perm_Real = einf + iVar%del1/(ONE + iVar%f1**2) + iVar%del2/(ONE + iVar%f2**2)
    perm_imag = -perm_imag + iVar%del1*iVar%f1/(ONE + iVar%f1**2)  &
              + iVar%del2*iVar%f2/(ONE + iVar%f2**2)
    Permittivity = Cmplx(perm_Real,-perm_imag,fp)

  ! Compute Fresnel reflectance code, adopted from Masahiro Kazumori, JMA
  !
    ! Compute the complex reflectivity components
    frVar%z1 = SQRT(permittivity - ONE + (cos_z*cos_z))
    frVar%z2 = permittivity * cos_z
    zRh = (cos_z  -frVar%z1) / (cos_z  +frVar%z1)
    zRv = (frVar%z2-frVar%z1) / (frVar%z2+frVar%z1)

    ! The square of the vertical abs value
    frVar%rzRv = REAL(zRv,fp)
    frVar%izRv = AIMAG(zRv)
    Rv_Fresnel = frVar%rzRv**2 + frVar%izRv**2

    ! The square of the horizontal abs value
    frVar%rzRh = REAL(zRh,fp)
    frVar%izRh = AIMAG(zRh)
    Rh_Fresnel = frVar%rzRh**2 + frVar%izRh**2


  ! Apply small-scale correction
  ! --------------------------------
    windspeed = Wind_Speed
    IF( windspeed < min_wind ) windspeed = min_wind
    IF( windspeed > max_wind ) windspeed = max_wind

    freq_S = Frequency
    IF( freq_S < min_f ) freq_S = min_f
    IF( freq_S > max_f ) freq_S = max_f

    scor = Scoef(1) *windspeed*freq_S +Scoef(2) *windspeed*freq_S**2 &
           + Scoef(3) *windspeed**2* freq_S +Scoef(4) *windspeed**2* freq_S**2 &
           + Scoef(5) *windspeed**2 /freq_S +Scoef(6) *windspeed**2 /freq_S**2 &
           + Scoef(7) *windspeed + Scoef(8) *windspeed**2

    small_corr = exp(-scor*cos_z*cos_z )
    RvS = Rv_Fresnel * small_corr

    RhS = Rh_Fresnel * small_corr

  ! Large Scale Correction Calculation
  ! ----------------------------------
    seczen = ONE/cos_z
    ! compute fitting coefficients for a given frequency
    DO j = 1, 12
      zc(j) = Lcoef(j*3-2) + Lcoef(j*3-1)*frequency + Lcoef(j*3)*frequency**2
    END DO

    RvL = zc(1) + zc(2)*seczen + zc(3)*seczen**2 + zc(4)*Wind_Speed &
       + zc(5)*Wind_Speed**2 + zc(6)*Wind_Speed*seczen
    RhL = zc(7) + zc(8)*seczen + zc(9)*seczen**2 + zc(10)*Wind_Speed &
       + zc(11)*Wind_Speed**2 + zc(12)*Wind_Speed*seczen

  ! Compute foam coverage after Tang, 1974
  ! ----------------------------------
    Foam_Cover = 7.75E-06_fp * Wind_Speed ** 3.231_fp

  ! The foam vertical and horizontal reflectanc codes, adopted from Masahiro Kazumori, JMA
  ! ----------------------------------
    Fv = ONE + Zenith_Angle*(FR_COEFF(1)+ Zenith_Angle*(FR_COEFF(2)  &
       + Zenith_Angle*FR_COEFF(3))) + FR_COEFF(4)*Zenith_Angle**10
    Foam_Rv = FR_COEFF(5)
    Fh = ONE + Zenith_Angle*(FR_COEFF(6) +  Zenith_Angle*(FR_COEFF(7)  &
       + Zenith_Angle*FR_COEFF(8)))
    Foam_Rh = ONE + FR_COEFF(9)*Fh

  ! Added frequency dependence derived from Stogry model, 1971
  ! ----------------------------------
    Foam_ref = 0.4_fp * exp(-0.05_fp*Frequency )
    Foam_Rv = Foam_Rv * Foam_ref
    Foam_Rh = Foam_Rh * Foam_ref

    Ev = (ONE-Foam_Cover)*(ONE - RvS + RvL) + Foam_Cover*(ONE-Foam_Rv)
    Eh = (ONE-Foam_Cover)*(ONE - RhS + RhL) + Foam_Cover*(ONE-Foam_Rh)
    Emissivity(1) = Ev
    Emissivity(2) = Eh

    zreflmod_v = ONE
    zreflmod_h = ONE

  ! correction for anisotropic downward radiation, adopted from the FASTEM3
  ! ----------------------------------
    IF( Transmittance > transmittance_limit_lower .and. Transmittance < transmittance_limit_upper) THEN
        !Using the Cox and Munk model to compute slope variance
        variance = 0.00512_fp * Wind_Speed + 0.0030_fp
        varm     = variance * t_c(43)
        variance = varm * ( t_c(44) * Frequency + t_c(45) )

        IF ( variance >= varm ) THEN
          variance = varm
        END IF
        IF ( variance <= ZERO  ) THEN
          variance = ZERO
        END IF
        !Compute surface to space optical depth
        opdpsfc = -log(Transmittance ) * cos_z
        !Define nine predictors for the effective angle calculation
        zx(1) = ONE
        zx(2) = variance
        zx(4) = ONE / cos_z
        zx(3) = zx(2) * zx(4)
        zx(5) = zx(3) * zx(3)
        zx(6) = zx(4) * zx(4)
        zx(7) = zx(2) * zx(2)
        zx(8) = log(opdpsfc)
        zx(9) = zx(8) * zx(8)

        zrough_h = ONE
        zrough_v = ONE
        DO i = 1, 7
           j = i-1
          !Switched h to v Deblonde SSMIS june 7, 2001
          zrough_h = zrough_h + zx(i) *(t_c(1+j*3) + zx(8)*t_c(2+j*3) + zx(9)*t_c(3+j*3))
          zrough_v = zrough_v + zx(i) *(t_c(22+j*3)+ zx(8)*t_c(23+j*3)+ zx(9)*t_c(24+j*3))

        END DO
        zreflmod_v = (ONE-Transmittance ** zrough_v)/(ONE-Transmittance )
        zreflmod_h = (ONE-Transmittance ** zrough_h)/(ONE-Transmittance )

    END IF

  ! azimuthal component, emissivity for full Stokes parameters
  ! --------------------------------
    Azimuth_Emi = ZERO
    IF( abs(Rel_Azimuth) <= 360.0_fp ) THEN
      Fre_C = ZERO
      IF( Frequency >= min_f .or. Frequency <= max_f ) THEN
        DO i = 1, 8
          IF( Frequency >= x(i) .and. Frequency < x(i+1) ) THEN
            Fre_C = y(i) + (y(i+1)-y(i))/(x(i+1)-x(i))*(Frequency-x(i))
          END IF
        END DO
      END IF

      phi = Rel_Azimuth * DEGREES_TO_RADIANS

      DO m = 1, 3
        L = 10*(m-1)
        asc(1,m) = b_coef(L+1) +b_coef(L+2)*Frequency +b_coef(L+3)*seczen &
          +b_coef(L+4)*seczen*Frequency +b_coef(L+5)*wind_speed  &
          +b_coef(L+6)*wind_speed*Frequency +b_coef(L+7)*wind_speed**2  &
          +b_coef(L+8)*Frequency*wind_speed**2 +b_coef(L+9)*wind_speed*seczen &
          +b_coef(L+10)*wind_speed*seczen*Frequency
        Azimuth_Emi(1) = Azimuth_Emi(1) + asc(1,m)*cos(m*phi)

        L = 10*(m-1) + 30
        asc(2,m) = b_coef(L+1) +b_coef(L+2)*Frequency +b_coef(L+3)*seczen &
          +b_coef(L+4)*seczen*Frequency +b_coef(L+5)*wind_speed  &
          +b_coef(L+6)*wind_speed*Frequency +b_coef(L+7)*wind_speed**2  &
          +b_coef(L+8)*Frequency*wind_speed**2 +b_coef(L+9)*wind_speed*seczen &
          +b_coef(L+10)*wind_speed*seczen*Frequency
        Azimuth_Emi(2) = Azimuth_Emi(2) + asc(2,m)*cos(m*phi)

        L = 10*(m-1) + 60
        asc(3,m) = b_coef(L+1) +b_coef(L+2)*Frequency +b_coef(L+3)*seczen &
          +b_coef(L+4)*seczen*Frequency +b_coef(L+5)*wind_speed &
          +b_coef(L+6)*wind_speed*Frequency +b_coef(L+7)*wind_speed**2  &
          +b_coef(L+8)*Frequency*wind_speed**2 +b_coef(L+9)*wind_speed*seczen &
          +b_coef(L+10)*wind_speed*seczen*Frequency
        Azimuth_Emi(3) = Azimuth_Emi(3) + asc(3,m)*sin(m*phi)

        L = 10*(m-1) + 90
        asc(4,m) = b_coef(L+1) +b_coef(L+2)*Frequency +b_coef(L+3)*seczen &
          +b_coef(L+4)*seczen*Frequency +b_coef(L+5)*wind_speed  &
          +b_coef(L+6)*wind_speed*Frequency +b_coef(L+7)*wind_speed**2  &
          +b_coef(L+8)*Frequency*wind_speed**2 +b_coef(L+9)*wind_speed*seczen &
          +b_coef(L+10)*wind_speed*seczen*Frequency
        Azimuth_Emi(4) = Azimuth_Emi(4) + asc(4,m)*sin(m*phi)

      END DO
      Azimuth_Emi = Azimuth_Emi * Fre_C

    END IF

    Emissivity(1) = Emissivity(1) + Azimuth_Emi(1)
    Emissivity(2) = Emissivity(2) + Azimuth_Emi(2)
    Emissivity(3) = Azimuth_Emi(3)
    Emissivity(4) = Azimuth_Emi(4)
    Reflectivity(1)  = zreflmod_v * (ONE-Emissivity(1))
    Reflectivity(2)  = zreflmod_h * (ONE-Emissivity(2))
    ! Reflectivities not computed for 3rd or 4th elements of Stokes vector, 
    ! as never used subsequently, as atmospheric source term = zero.
    Reflectivity(3:4)  = ZERO

  !  ****** End of Forward Part ******
  !  ****** start of adjoint code ******

    wind10_k = ZERO
    phi_k = 0.0_fp
    zreflmod_v_k = ZERO
    zreflmod_h_k = ZERO

    Emissivity_k(2) = Emissivity_k(2) -zreflmod_h*Reflectivity_k(2)
    zreflmod_h_k = zreflmod_h_k + Reflectivity_k(2)* (ONE-Emissivity(2))

    Emissivity_k(1) = Emissivity_k(1) -zreflmod_v*Reflectivity_k(1)
    zreflmod_v_k = zreflmod_v_k + Reflectivity_k(1)* (ONE-Emissivity(1))
    Azimuth_Emi_k(4) = Emissivity_k(4)
    Azimuth_Emi_k(3) = Emissivity_k(3)
    Azimuth_Emi_k(2) = Emissivity_k(2)
    Azimuth_Emi_k(1) = Emissivity_k(1)

    ! azimuthal component
    ! --------------------------------
  IF( abs(Rel_Azimuth) <= 360.0_fp ) THEN

    seczen = ONE/cos_z
    phi = Rel_Azimuth * DEGREES_TO_RADIANS

    Azimuth_Emi_k = Azimuth_Emi_k * Fre_C

    DO m = 1, 3
      phi_k = phi_k +m*Azimuth_Emi_k(4)*asc(4,m)*cos(m*phi)
      sc_k = Azimuth_Emi_k(4)*sin(m*phi)
      L = 10*(m-1) + 90
      wind10_k = wind10_k + ( b_coef(L+5) +b_coef(L+6)*Frequency )*sc_k  &
          +( TWO*(b_coef(L+7) +b_coef(L+8)*Frequency)*wind_speed &
          +b_coef(L+9)*seczen +b_coef(L+10)*seczen*Frequency )*sc_k

      phi_k = phi_k +m*Azimuth_Emi_k(3)*asc(3,m)*cos(m*phi)
      sc_k = Azimuth_Emi_k(3)*sin(m*phi)
      L = 10*(m-1) + 60
      wind10_k = wind10_k + ( b_coef(L+5) +b_coef(L+6)*Frequency )*sc_k  &
           +( TWO*(b_coef(L+7) +b_coef(L+8)*Frequency)*wind_speed  &
           +b_coef(L+9)*seczen +b_coef(L+10)*seczen*Frequency )*sc_k

      phi_k = phi_k -m*Azimuth_Emi_k(2)*asc(2,m)*sin(m*phi)
      ac_k = Azimuth_Emi_k(2)*cos(m*phi)
      L = 10*(m-1) + 30
      wind10_k = wind10_k + ( b_coef(L+5) +b_coef(L+6)*Frequency )*ac_k  &
           +( TWO*(b_coef(L+7) +b_coef(L+8)*Frequency)*wind_speed  &
           +b_coef(L+9)*seczen +b_coef(L+10)*seczen*Frequency )*ac_k

      phi_k = phi_k -m*Azimuth_Emi_k(1)*asc(1,m)*sin(m*phi)
      ac_k = Azimuth_Emi_k(1)*cos(m*phi)
      L = 10*(m-1)
      wind10_k = wind10_k + ( b_coef(L+5) +b_coef(L+6)*Frequency )*ac_k  &
           +( TWO*(b_coef(L+7) +b_coef(L+8)*Frequency)*wind_speed  &
           +b_coef(L+9)*seczen +b_coef(L+10)*seczen*Frequency )*ac_k

    END DO

    Rel_Azimuth_k = Rel_Azimuth_k + phi_k* DEGREES_TO_RADIANS

  END IF


    Azimuth_Emi = ZERO
    Azimuth_Emi_k = ZERO
  !  print *, zreflmod_v_k,zreflmod_h_k
  ! correction for anisotropic downward radiation
  IF( Transmittance > transmittance_limit_lower .and. Transmittance < transmittance_limit_upper) THEN

      zrough_v_k = ZERO
      zrough_h_k = ZERO

      zrough_h_k = - zreflmod_h_k * &
            & ( Transmittance**zrough_h *log(Transmittance) ) / &
            & (ONE-Transmittance)

      Transmittance_k = Transmittance_k + zreflmod_h_k/(ONE-Transmittance)* &
        & (-zrough_h * Transmittance**(zrough_h-ONE) + zreflmod_h)

      zrough_v_k = -zreflmod_v_k * &
        & ( Transmittance**zrough_v * log(Transmittance) ) / &
        & (ONE-Transmittance)

      Transmittance_k = Transmittance_k + zreflmod_v_k/(ONE-Transmittance)* &
        & (-zrough_v * Transmittance**(zrough_v-ONE) + zreflmod_v)

      zx_k(:) = ZERO

      DO i = 1,7
         j = i-1
         !Switched h to v Deblonde SSMIS june 7, 2001

         zx_k(9) = zx_k(9) + zrough_v_k * zx(i) * t_c(24+j*3)
         zx_k(8) = zx_k(8) + zrough_v_k * zx(i) * t_c(23+j*3)
         zx_k(i) = zx_k(i) + zrough_v_k *(t_c(22+j*3)+zx(8)*t_c(23+j*3)+zx(9) &
                  *t_c(24+j*3) )

         zx_k(9) = zx_k(9) + zrough_h_k * zx(i) * t_c(3+j*3)
         zx_k(8) = zx_k(8) + zrough_h_k * zx(i) * t_c(2+j*3)
         zx_k(i) = zx_k(i) + zrough_h_k*(t_c(1+j*3) +zx(8)*t_c(2+j*3)   &
                  + zx(9)  *  t_c(3+j*3) )
      END DO
        zrough_v_k = ZERO
        zrough_h_k = ZERO

        !Define nine predictors for the effective angle calculation
        zx_k(8) = zx_k(8) + zx_k(9) * 2 * zx(8)
        opdpsfc_k = zx_k(8) / opdpsfc
        zx_k(2) = zx_k(2) + zx_k(7) * 2 * zx(2)
        zx_k(3) = zx_k(3) + zx_k(5) * 2 * zx(3)
        zx_k(2) = zx_k(2) + zx_k(3) * zx(4)
        zx_k(4) = ZERO
        variance_k = zx_k(2)
        zx_k(1) = ZERO

        !Compute surface to space optical depth
        Transmittance_k = Transmittance_k - opdpsfc_k*cos_z /Transmittance

        IF ( variance < varm ) THEN
           varm_k = variance_k * ( t_c(44) *Frequency + t_c(45) )
        ELSE
           varm_k = variance_k
        END IF

        variance_k = varm_k * t_c(43)
        wind10_k = wind10_k + variance_k * 0.00512_fp

  END IF

    zreflmod_v_k = ZERO
    zreflmod_h_k = ZERO
    Eh_k = Emissivity_k(2)
    Ev_k = Emissivity_k(1)
    RhL_k = (ONE-Foam_Cover)*Eh_k
    RhS_k = -(ONE-Foam_Cover)*Eh_k
    Foam_Cover_k = Eh_k*(RhS - RhL-Foam_Rh)

    RvL_k = (ONE-Foam_Cover)*Ev_k
    RvS_k = -(ONE-Foam_Cover)*Ev_k

    Foam_Cover_k = Foam_Cover_k  + Ev_k*(RvS - RvL-Foam_Rv)
    wind10_k = wind10_k + 7.75E-06_fp*3.231_fp*Wind_Speed**2.231_fp*Foam_Cover_k


    ! Large Scale Correction Calculation
    ! ----------------------------------
    wind10_k = wind10_k + zc(10)*RhL_k &
       + TWO*zc(11)*Wind_Speed*RhL_k + zc(12)*RhL_k*seczen

    wind10_k = wind10_k + zc(4)*RvL_k &
       + TWO*zc(5)*Wind_Speed*RvL_k + zc(6)*RvL_k*seczen

    small_corr_k = Rh_Fresnel *RhS_k
    Rh_Fresnel_k = RhS_k * small_corr

    small_corr_k = small_corr_k + Rv_Fresnel *RvS_k
    Rv_Fresnel_k = RvS_k * small_corr

    ! Small scale Correction Calculation
    scor_k = -small_corr_k*cos_z*cos_z *small_corr

    wind10_k = wind10_k + ( Scoef(1)*frequency +Scoef(2)*frequency**2 &
            +TWO*Scoef(3) *windspeed*frequency +TWO*Scoef(4)*windspeed* frequency**2 &
            +TWO*Scoef(5) *windspeed /frequency +TWO*Scoef(6) *windspeed/frequency**2 &
            +Scoef(7) + TWO*Scoef(8) *windspeed  )*scor_k

    ! Fresnel reflectivity calculation
    ! --------------------------------
    permittivity_k = ZERO

     ! The adjoint of the horizontal reflectivity
    izRh_k = TWO*frVar%izRh*Rh_Fresnel_k
    rzRh_k = TWO*frVar%rzRh*Rh_Fresnel_k
    Rh_Fresnel_k = ZERO
    zRh_k = CMPLX(rzRh_k, -izRh_k, fp)  ! complex conjugate

    ! The adjoint of the vertical reflectivity
    izRv_k = TWO*frVar%izRv*Rv_Fresnel_k
    rzRv_k = TWO*frVar%rzRv*Rv_Fresnel_k
    Rv_Fresnel_k  = ZERO
    zRv_k = CMPLX(rzRv_k, -izRv_k, fp)  ! complex conjugate

    ! The adjoint of the complex vertical polarised component
    denom = (frVar%z2+frVar%z1)**2
    z1_k = -TWO*frVar%z2*zRv_k / denom
    z2_k =  TWO*frVar%z1*zRv_k / denom

    ! The adjoint of the complex horizontal polarised component
    z1_k = z1_k - ( TWO*cos_z*zRh_k / (cos_z+frVar%z1)**2 )

    ! The adjoint of the preserved variables
    permittivity_k = permittivity_k + CONJG(cos_z*z2_k)
    permittivity_k = permittivity_k + CONJG(POINT_5*z1_k/frVar%z1)


    ! Permittivity Calculation
    ! ------------------------
    t_k = ZERO
    S_k = ZERO
    t_sq_k = ZERO
    einf_k = ZERO

!    perm_imag_k = -AIMAG(Permittivity_k)

    perm_imag_k = -AIMAG(Permittivity_k)
    perm_Real_k = REAL(Permittivity_k,fp)
    Permittivity_k = ZERO

    f2_k = (iVar%del2-TWO*iVar%del2*iVar%f2**2/(ONE + iVar%f2**2)) &
          *perm_imag_k/(ONE + iVar%f2**2)

    del2_k = iVar%f2*perm_imag_k/(ONE + iVar%f2**2)

    f1_k = (iVar%del1-TWO*iVar%del1*iVar%f1**2/(ONE + iVar%f1**2))  &
          *perm_imag_k/(ONE + iVar%f1**2)
    del1_k = iVar%f1*perm_imag_k/(ONE + iVar%f1**2)

    perm_imag_k = -perm_imag_k

    f2_k = f2_k -TWO*iVar%del2*iVar%f2*perm_Real_k/(ONE + iVar%f2**2)**2
    del2_k = del2_k + perm_Real_k/(ONE + iVar%f2**2)
    f1_k = f1_k -TWO*iVar%del1*iVar%f1*perm_Real_k/(ONE + iVar%f1**2)**2
    del1_k = del1_k + perm_Real_k/(ONE + iVar%f1**2)
    einf_k = perm_Real_k


    einf_k = einf_k -del2_k
    e1_k = del2_k
    e1_k = e1_k - del1_k
    es_k = del1_k

    tau2_k = Frequency*f2_k
    tau1_k = Frequency*f1_k
    IF( iVar%S > ZERO ) THEN
      sigma_k = -perm_imag_k/(TWO*PI*e0*Frequency)
      ctau2_k = iVar%tau2_k * tau2_k
      tau2_k = tau2_k * iVar%ctau2
      ctau1_k = iVar%tau1_k * tau1_k
      tau1_k = tau1_k * iVar%ctau1
      ce1_k = iVar%e1_k * e1_k
      e1_k = e1_k * iVar%ce1
      ces_k = iVar%es_k * es_k
      es_k = es_k * iVar%ces
      S_k = S_k + (A_COEF(26) + A_COEF(27)*iVar%t + THREE*A_COEF(28)*iVar%S**2)*ctau2_k
      t_k = t_k + iVar%S*A_COEF(27)*ctau2_k
      t_sq_k = t_sq_k + iVar%S*A_COEF(21)*ctau1_k
      t_k = t_k + iVar%S*A_COEF(20)*ctau1_k
      S_k = S_k + ctau1_k*(A_COEF(19) +A_COEF(20)*iVar%t + A_COEF(21)*iVar%t_sq)
      t_k = t_k + A_COEF(14)*iVar%S*ce1_k
      S_k = S_k + (A_COEF(12)+TWO*A_COEF(13)*iVar%S+A_COEF(14)*iVar%t)*ce1_k
      t_k = t_k + A_COEF(8)*iVar%S*ces_k
      S_k = S_k + (A_COEF(6) + TWO*A_COEF(7)*iVar%S + A_COEF(8)*iVar%t)*ces_k
      beta_k = -iVar%delta*iVar%sigma*sigma_k
      delta_k = -sigma_k*iVar%beta*iVar%sigma
      sigma25_k = sigma_k*exp(-iVar%delta*iVar%beta)
      S_k = S_k + sigma25_k*(A_COEF(35) +A_COEF(36)*iVar%S   &
           +A_COEF(37)*iVar%S**2 +A_COEF(38)*iVar%S**3)
      S_k = S_k + iVar%S*(A_COEF(36) +TWO*A_COEF(37)*iVar%S   &
           +THREE*A_COEF(38)*iVar%S**2)*sigma25_k
      delta_k = delta_k + (A_COEF(30)+TWO*A_COEF(31)*iVar%delta+iVar%S*A_COEF(33)  &
               +iVar%S*TWO*A_COEF(34)*iVar%delta)*beta_k
      S_k = S_k + beta_k*(A_COEF(32) +A_COEF(33)*iVar%delta +A_COEF(34)*iVar%delta**2)
      t_k = t_k -delta_k
    END IF

      t_cu_k = A_COEF(25)*tau2_k
      t_sq_k = t_sq_k + A_COEF(24)*tau2_k
      t_k = t_k + A_COEF(23)*tau2_k
      t_cu_k = t_cu_k + A_COEF(18)*tau1_k
      t_sq_k = t_sq_k + A_COEF(17)*tau1_k
      t_k = t_k + A_COEF(16)*tau1_k

      t_sq_k = t_sq_k + A_COEF(11)*e1_k
      t_k = t_k + A_COEF(10)*e1_k
      t_cu_k = t_cu_k + A_COEF(5)*es_k
      t_sq_k = t_sq_k + A_COEF(4)*es_k
      t_k = t_k + A_COEF(3)*es_k
      t_k = t_k + A_COEF(1)*einf_k

      t_k = t_k + THREE * iVar%t_sq * t_cu_k
      t_k = t_k + TWO * iVar%t * t_sq_k
      Temperature_k = Temperature_k + t_k
      Salinity_k = S_k

    wind_speed_k = wind10_k

   RETURN

  END SUBROUTINE rttov_fastem4_k
!
