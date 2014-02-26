!
   MODULE Surface_Emissivity_Model

   ! -----------------------
  ! Disable implicit typing
  ! -----------------------
  IMPLICIT NONE
  ! ------------
  ! Visibilities
  ! ------------
    PRIVATE                                                                                                             
    PUBLIC :: EMISS_MW
    PUBLIC :: EMISS_MW_TL
    PUBLIC :: EMISS_MW_AD

 CONTAINS

   SUBROUTINE EMISS_MW(Nch,use_channel,channel_index,PolStatus,f0,stype, &
    zenith,surfw,ts,emissivity,reflection)
! ------------------------------------------------------------------------
!
! Function:
!  Compute surface emissivity.
!  Reflectivity = 1 - Emissivity (for intensity, and
!  Reflectivity = Emissivity for other components.
!
!   INPUT:
!  NAME         TYPE    UNIT     DESCRIPTION
!  stype         I4     NONE     surface type
!  zenith        R8     Degree   local zenith angle
!  surfw         R8     m/s      surface wind at 10 meter
!  ts            R8     F degree surface temperature
!
!   OUTPUT
!   emissivity   R8     NONE      emissivity
! -----------------------------------------------------------------------
  USE MOD_SURFACE_COEF, ONLY : SINDEX,perm_static1,perm_infinite1, &
          perm_relaxation1,rough_small1,rough_large1,emc, &
          avn_soil_mois,avn_snow_depth, &   
          avn_soil_t,avn_veg_frac,avn_canopy_water,avn_veg_type, &
          avn_soil_type,surface_bta,surface_btb,snow_type,surface_bta,surface_btb

  USE CRTM_parameters, ONLY : satheight,earthrad

    ! -- Utility modules
  USE Type_Kinds
  USE Error_Handler
!
  IMPLICIT NONE
!
  INTEGER :: kk,Nch,stype,use_channel
  INTEGER, DIMENSION( : ), INTENT (IN ) :: channel_index, PolStatus
  INTEGER :: index1,ifastem_version
  REAL( fp_kind ) :: zenith,surfw,ts
  REAL( fp_kind ), DIMENSION( : ) :: emissivity, reflection, f0
                                                                                                               
  REAL( fp_kind ) :: pi,pcc,pir,pc2,ps2,ratio,snad2,cnad2,pts
  REAL( fp_kind ) ::  u10mps,freqghz,tc,tc2,tc3,tau1,tau2,del1,del2,einf,fen,fen2
  REAL( fp_kind ) ::  den1,den2,perm_real,freqghz2,u10mps2,sec,sec2,usec,zc1,zc2
  REAL( fp_kind ) ::  zc3,zc4,zc5,zc6,zc7,zc8,zc9,zc10,zc11,zc12,xcorr21,xcorr22,pems,pref
  REAL( fp_kind ) ::  wvnmcm1,perm_free,sigma,perm_imag3
                                                                                                               
  REAL( fp_kind ) ::  perm_imag   ! permittivity (imaginary part)
  REAL( fp_kind ) ::  perm_real1  !    .... real part
  REAL( fp_kind ) ::  perm_real2  !    .... real part
  REAL( fp_kind ) ::  perm_imag1  !    .... imaginary part
  REAL( fp_kind ) ::  perm_imag2  !    .... imaginary part
  REAL( fp_kind ) ::  rverts      ! flat specular reflectivity (vertical)
  REAL( fp_kind ) ::  rhorzs      ! flat specular reflectivity (horizontal)
  REAL( fp_kind ) ::  rvertsr     !    .... real part
  REAL( fp_kind ) ::  rvertsi     !    .... imaginary part
  REAL( fp_kind ) ::  rhorzsr     !    .... real part
  REAL( fp_kind ) ::  rhorzsi     !    .... imaginary part
  !
  REAL( fp_kind ) ::  xcorr10     ! 
  REAL( fp_kind ) ::  xcorr1      ! correction small scale (Bragg scattering)
  REAL( fp_kind ) ::  rv          ! modifyed reflectivity
  REAL( fp_kind ) ::  rh          ! modifyed reflectivity
  REAL( fp_kind ) ::  evertr      ! effective rough ocean emissivity (vert.)
  REAL( fp_kind ) ::  ehorzr      ! effective rough ocean emissivity (horiz.)
  REAL( fp_kind ) ::  evert       ! polarised emissivity (vert.)
  REAL( fp_kind ) ::  ehorz       ! polarised emissivity (horiz.)
  REAL( fp_kind ) ::  ffoam       ! fractional foam coverage
  REAL( fp_kind ) ::  opdpsfc     ! surface to space optical depth
  REAL( fp_kind ) ::  zroughv     ! ratio of cos of effective to zenith angles (vert)
  REAL( fp_kind ) ::  zroughh     ! ratio of cos of effective to zenith angles (horz)
  REAL( fp_kind ) ::  rvert       ! reflectivity (vert.)
  REAL( fp_kind ) ::  rhorz       ! reflectivity (horiz.)
  REAL( fp_kind ) ::  zreflmodv   ! reflectivity decrease factor (horiz.)
  REAL( fp_kind ) ::  zreflmodh   ! reflectivity decrease factor (vert.)
  REAL( fp_kind ) ::  variance    ! wave slope variance
  REAL( fp_kind ) ::  varm        ! maximum wave slope variance
  ! additional land surface emissivity model parameters
  REAL( fp_kind ) ::  perm_static      ! static land permittivity
  REAL( fp_kind ) ::  perm_infinite    ! infinite frequency land permittivity
  REAL( fp_kind ) ::  freqr            ! relaxation frequency land
  REAL( fp_kind ) ::  small_rough      ! small scale roughness
  REAL( fp_kind ) ::  large_rough      ! large scale roughness
  REAL( fp_kind ) ::  qdepol           ! depolarisation
  REAL( fp_kind ) ::  delta            ! intermediate roughness parameter
  REAL( fp_kind ) ::  delta2           ! intermediate roughness parameter
                                                                                                               
  ! SsirEM scalars
  REAL( fp_kind ) ::  a0                    ! emissivity epsilon0 coefficient
  REAL( fp_kind ) ::  a1                    ! emissivity epsilon1 coefficient
  REAL( fp_kind ) ::  a2                    ! emissivity epsilon2 coefficient
  REAL( fp_kind ) ::  xzn1                  ! polynomial basis function
  REAL( fp_kind ) ::  xzn2                  ! polynomial basis function
  REAL( fp_kind ) ::  xza                   ! normalized satellite zenith angle
                                                                                                               
  ! FASTEM complex variables
  COMPLEx :: junk1           
  COMPLEx :: xperm              ! permittivity
  COMPLEx :: perm1              ! Fresnel reflectivity complex variables
  COMPLEx :: perm2              ! ..
  COMPLEx :: rvth               ! ..
  COMPLEx :: rhth               ! ..
  INTEGER   :: ipol             ! polarisation indice for zvpol and zhpol
                                ! == mpol +1
                                !   1 average of vertical and horizontal
                                !   2 nominal vertical at nadir, rotating
                                !      with view angle
                                !   3 nominal horizontal at nadir, rotating
                                !      with view angle
                                !   4 vertical
                                !   5 horizontal
  REAL( fp_kind ),  DIMENSION(3,5) ::  zvpol      ! zvpol and zhpol tell emiss
  REAL( fp_kind ), DIMENSION(3,5) ::  zhpol      ! how much v and h pol to use

 REAL( fp_kind ) :: esh, esv
!
!  NOAA  landEM
       real theta,frequ1,em_vector(2),t_skin,ps
!
!
  DATA zvpol / 0.5, 0.0, 0.0, &
               0.0, 0.0, 1.0, &
               0.0, 1.0, 0.0, &
               1.0, 0.0, 0.0, &
               0.0, 0.0, 0.0 /
  DATA zhpol / 0.5, 0.0, 0.0, &
               0.0, 1.0, 0.0, &
               0.0, 0.0, 1.0, &
               0.0, 0.0, 0.0, &
               1.0, 0.0, 0.0 /

   SAVE zvpol,zhpol
  !- End of header ------------------------------------------------------
  !
!!        emissivity = 0.3 + 0.001 * ts + 0.01 * surfw
!!         if(surfw.gt.0.0) return

  !  using local zenith at the surface for Frensnel formula
!



           ifastem_version=36
           PI=ACOS(-1.0)
           pcc = cos(zenith*PI/180.0) 
           pc2 = pcc*pcc
           ps2 = 1.0 - pc2 
  !  satellite_to_earthcenter/earth_radius
  !  Sine nadir angle (at satellite)square 
          ratio=(satheight+earthrad)/earthrad
          snad2=ps2/(ratio*ratio)
          cnad2=1.0-snad2
           ! Get surface skin temperature
           pts = ts

  !  test  using constatnt temperature
!           pts = 300.0


           ! Get windspeed
           u10mps = surfw
           ! Get amsu frequency 
   DO 103 kk=1,use_channel
!
           freqghz=f0(kk)
          ipol = PolStatus(kk)

       IF(stype.eq.1) then

           ! Calculate piom (ELLISON et al.) xperm
           ! Convert from kelvin to centigrate and define quadratic and
           ! Cubic functions for later polynomials
           tc  = pts-273.15
           tc2 = tc*tc
           tc3 = tc2*tc
           ! Define two relaxation frequencees, tau1 and tau2
           tau1 = emc(1) + emc(2)*tc + emc(3)*tc2
           tau2 = emc(4) + emc(5)*tc + emc(6)*tc2 + emc(7)*tc3
           ! static xperm estatic=del1+del2+einf
           del1 = emc(8)  + emc(9)*tc  + emc(10)*tc2 + emc(11)*tc3
           del2 = emc(12) + emc(13)*tc + emc(14)*tc2 + emc(15)*tc3
           einf = emc(18) + emc(19)*tc
           ! Calculate xperm using double-debye formula
           fen        = 2.0*emc(20)*freqghz*0.001
           fen2       = fen*fen
           den1       = 1.0 + fen2*tau1*tau1
           den2       = 1.0 + fen2*tau2*tau2
           perm_real1 = del1/den1
           perm_real2 = del2/den2
           perm_imag1 = del1*fen*tau1/den1
           perm_imag2 = del2*fen*tau2/den2

           !----comments
           perm_free    = 8.854E-3_fp_kind                                                         
           sigma        = 2.906_fp_kind + 0.09437_fp_kind * tc
           perm_imag3   = sigma / (2.0_fp_kind * pi * perm_free * freqghz)
           !-----


           perm_real  = perm_real1 + perm_real2 + einf

           perm_imag  = perm_imag1 + perm_imag2 + perm_imag3


           xperm      = CMPLx(perm_real,perm_imag)
           !           
           ! Calculate complex fresnel reflection coefficients
           !        
           junk1=cmplx(ps2, 0.0)
           perm1   = sqrt(xperm - junk1)
           perm2   = xperm*pcc
           rhth    = (  pcc - perm1)/(  pcc + perm1)                     
           rvth    = (perm2 - perm1)/(perm2 + perm1)

           rvertsr = DBLE(rvth)
           rvertsi = Aimag(rvth)
           rverts  = rvertsr*rvertsr + rvertsi*rvertsi
           rhorzsr = DBLE(rhth)
           rhorzsi = Aimag(rhth)
           rhorzs  = rhorzsr*rhorzsr + rhorzsi*rhorzsi

           xcorr1  = exp(emc(21)*u10mps*pc2/(freqghz*freqghz))

           rv = rverts*xcorr1
           rh = rhorzs*xcorr1
           freqghz2 = freqghz*freqghz
           u10mps2  = u10mps*u10mps
           sec      = 1.0/pcc
           sec2     = sec*sec
           usec     = u10mps*sec

           zc1  = emc(24+ifastem_version) + &
                     emc(25+ifastem_version)*freqghz + &
                     emc(26+ifastem_version)*freqghz2
           zc2  = emc(27+ifastem_version) + &
                     emc(28+ifastem_version)*freqghz + &
                     emc(29+ifastem_version)*freqghz2
           zc3  = emc(30+ifastem_version) + &
                     emc(31+ifastem_version)*freqghz + &
                     emc(32+ifastem_version)*freqghz2
           zc4  = emc(33+ifastem_version) + &
                     emc(34+ifastem_version)*freqghz + &
                     emc(35+ifastem_version)*freqghz2
           zc5  = emc(36+ifastem_version) + &
                     emc(37+ifastem_version)*freqghz + &
                     emc(38+ifastem_version)*freqghz2
           zc6  = emc(39+ifastem_version) + &
                     emc(40+ifastem_version)*freqghz + &
                     emc(41+ifastem_version)*freqghz2
           zc7  = emc(42+ifastem_version) + &
                     emc(43+ifastem_version)*freqghz + &
                     emc(44+ifastem_version)*freqghz2
           zc8  = emc(45+ifastem_version) + &
                     emc(46+ifastem_version)*freqghz + &
                     emc(47+ifastem_version)*freqghz2
           zc9  = emc(48+ifastem_version) + &
                     emc(49+ifastem_version)*freqghz + &
                     emc(50+ifastem_version)*freqghz2
           zc10 = emc(51+ifastem_version) + &
                     emc(52+ifastem_version)*freqghz + &
                     emc(53+ifastem_version)*freqghz2
           zc11 = emc(54+ifastem_version) + &
                     emc(55+ifastem_version)*freqghz + &
                     emc(56+ifastem_version)*freqghz2
           zc12 = emc(57+ifastem_version) + &
                     emc(58+ifastem_version)*freqghz + &
                     emc(59+ifastem_version)*freqghz2
           ! Calculate correction for this polarisation
           xcorr21 =   zc1                      &
                        + zc2*sec                  &
                        + zc3*sec2                 &
                        + zc4*u10mps               &
                        + zc5*u10mps2+zc6*usec
           xcorr21 = xcorr21/100.0
           xcorr22 =   zc7                      &
                        + zc8*sec                  &
                        + zc9*sec2                 &
                        + zc10*u10mps              &
                        + zc11*u10mps2             &
                        + zc12*usec
           xcorr22 = xcorr22/100.0
           evertr     = 1.0 - rv + xcorr21
           ehorzr     = 1.0 - rh + xcorr22
           ! Calculate foam emissivity correction
           ffoam     = emc(22)*(u10mps**emc(23))
           evert     = evertr - ffoam*evertr + ffoam
           ehorz     = ehorzr - ffoam*ehorzr + ffoam
              rvert=1.0-evert
              rhorz=1.0-ehorz
           ! Calculate emissivity
           pems =  evert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + ehorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
           pref =  rvert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + rhorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
!
!   correction for AMSU  by Liu    7/22/2003
         if(freqghz.le.160.0) then
           xcorr10=-0.0500408+0.00145620*freqghz-0.00000815066*freqghz*freqghz
!!           xcorr10=-0.0581531+0.00160144*freqghz-0.00000875832*freqghz*freqghz

         if(freqghz.le.60.0.and.freqghz.gt.50.0) then
            xcorr10=-0.006
         endif
           pems=pems+xcorr10
           pref=pref-xcorr10
         endif

        ELSE
                                   
  ! Land fastem code
           index1=SINDEX(stype)
           ! coherent surface scattering model coefficients
           perm_static=perm_static1(index1)
           perm_infinite=perm_infinite1(index1)
           freqr=perm_relaxation1(index1)
           small_rough=rough_small1(index1)
           large_rough=rough_large1(index1)
!
           wvnmcm1=freqghz/30.0
           ! simple debye + fresnel model gives reflectivities
           fen=freqghz/freqr
           fen2=fen**2.0
           den1=1.0+fen2
           perm_real=(perm_static+perm_infinite*fen2)/den1
           perm_imag=fen*(perm_static-perm_infinite)/den1
           xperm=CMPLX(perm_real,perm_imag)
           junk1=cmplx(ps2, 0.0)
           perm1   = sqrt(xperm - junk1)
!!           perm1 = CSQRT(xperm - ps2)
           perm2  = xperm*PCC
           rhth = (  pcc - perm1)/(  pcc + perm1)
           rvth = (perm2 - perm1)/(perm2 + perm1)
           rvertsr=DBLE(rvth)
           rvertsi=Aimag(rvth)
           rverts=rvertsr*rvertsr+rvertsi*rvertsi
           rhorzsr=DBLE(rhth)
           rhorzsi=Aimag(rhth)
           rhorzs=rhorzsr*rhorzsr+rhorzsi*rhorzsi
           ! apply small_rough scale roughness correction
           delta = 4.0*pi*wvnmcm1*0.1*small_rough
           delta2=delta*delta
           xcorr1=exp(-delta2*pc2)
           evertr=1.0-rverts*xcorr1
           ehorzr=1.0-rhorzs*xcorr1
           ! apply large scale roughness correction
           qdepol = 0.35 - 0.35*EXP(-0.60*freqghz*large_rough**2.)
           evert=evertr*(1.0-qdepol)+ehorzr*qdepol
           ehorz=ehorzr*(1.0-qdepol)+evertr*qdepol
           rvert=1.0-evert
           rhorz=1.0-ehorz
                                                                                                                 
          em_vector(2)=evert
          em_vector(1)=ehorz
!
!           if( evert.ge.0.0) go to 1033    ! using English model
!
!  *************************   NOAA  landEM   **********************
!
    theta=zenith
    t_skin=ts
    frequ1=f0(kk)
!
    if(stype.gt.3.and.stype.lt.24) then
    call LandEM(zenith, f0(kk),avn_soil_mois, avn_veg_frac, avn_veg_type, &
            avn_soil_type, avn_soil_t, ts, avn_snow_depth, esh, esv)
          em_vector(1) = esh
          em_vector(2) = esv
    else
    if(maxval(surface_bta) > 100.0 .or. maxval(surface_btb) > 100.0) then
   call SIceEM(theta,frequ1,avn_snow_depth,t_skin,surface_bta,surface_btb,em_vector)
    else
    call LandEM(zenith, f0(kk), avn_soil_mois, avn_veg_frac, avn_veg_type, &
            avn_soil_type, avn_soil_t, ts, avn_snow_depth, esh, esv)
          em_vector(1) = esh
          em_vector(2) = esv
    endif
    endif
!
     evert=em_vector(2)
     ehorz=em_vector(1)
     rvert=1.0-evert
     rhorz=1.0-ehorz
!
 1033  continue
           pems =  evert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + ehorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
           pref =  rvert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + rhorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                      + zhpol(3,ipol)*cnad2)
        ENDIF
  !
      emissivity(kk)=pems
      reflection(kk)=pref
 103  continue

  RETURN

 END SUBROUTINE EMISS_MW


   SUBROUTINE EMISS_MW_TL(Nch,use_channel,channel_index,PolStatus,f0,stype, &
    zenith,surfw,ts,emissivity,reflection,surfw_TL,ts_TL,emissivity_TL)
! ------------------------------------------------------------------------
!
! Function:
!  Compute surface emissivity.
!  Reflectivity = 1 - Emissivity (for intensity, and
!  Reflectivity = Emissivity for other components.
!
!   INPUT:
!  NAME         TYPE    UNIT     DESCRIPTION
!  stype         I4     NONE     surface type
!  zenith        R8     Degree   local zenith angle
!  surfw         R8     m/s      surface wind at 10 meter
!  ts            R8     F degree surface temperature
!
!   OUTPUT
!   emissivity   R8     NONE      emissivity
! -----------------------------------------------------------------------
  USE MOD_SURFACE_COEF, ONLY : SINDEX,perm_static1,perm_infinite1, &
          perm_relaxation1,rough_small1,rough_large1,emc, &
          avn_soil_mois,avn_snow_depth, &   
          avn_soil_t,avn_veg_frac,avn_canopy_water,avn_veg_type, &
          avn_soil_type,surface_bta,surface_btb,snow_type,surface_bta,surface_btb

  USE CRTM_parameters, ONLY : satheight,earthrad

    ! -- Utility modules
  USE Type_Kinds
  USE Error_Handler
!
  IMPLICIT NONE
!
  INTEGER :: kk,Nch,stype,use_channel
  INTEGER, DIMENSION( : ), INTENT (IN ) :: channel_index, PolStatus
  INTEGER :: index1,ifastem_version
 REAL( fp_kind ) :: ffoam_TL,evert_TL,ehorz_TL,rvert_TL, rhorz_TL,pems_TL,pref_TL,evertr_TL,ehorzr_TL
 REAL( fp_kind ) :: u10mps_TL,xcorr1_TL,rv_TL,rh_TL,u10mps2_TL,usec_TL,xcorr22_TL,xcorr21_TL
  REAL( fp_kind ) :: zenith,surfw,ts
  REAL( fp_kind ) :: surfw_TL,ts_TL
  REAL( fp_kind ) :: pts_TL,tc_TL,tc2_TL,tc3_TL,tau1_TL,tau2_TL,del1_TL,del2_TL,einf_TL
  REAL( fp_kind ) :: den1_TL,den2_TL,perm_real1_TL,perm_real2_TL,perm_imag1_TL,perm_imag2_TL
  REAL( fp_kind ) :: perm_real_TL,perm_imag_TL,rvertsr_TL,rvertsi_TL,rverts_TL
  REAL( fp_kind ) :: rhorzsr_TL,rhorzsi_TL,rhorzs_TL
  REAL( fp_kind ), DIMENSION( : ) :: emissivity, reflection, f0
  REAL( fp_kind ), DIMENSION( : ) :: emissivity_TL

  REAL( fp_kind ) :: pi,pcc,pir,pc2,ps2,ratio,snad2,cnad2,pts
  REAL( fp_kind ) :: u10mps,freqghz,tc,tc2,tc3,tau1,tau2,del1,del2,einf,fen,fen2
  REAL( fp_kind ) :: den1,den2,perm_real,freqghz2,u10mps2,sec,sec2,usec,zc1,zc2
  REAL( fp_kind ) :: zc3,zc4,zc5,zc6,zc7,zc8,zc9,zc10,zc11,zc12,xcorr21,xcorr22,pems,pref
  REAL( fp_kind ) :: wvnmcm1

  REAL( fp_kind )  :: perm_imag   ! permittivity (imaginary part)
  REAL( fp_kind )  :: perm_real1  !    .... real part
  REAL( fp_kind )  :: perm_real2  !    .... real part
  REAL( fp_kind )  :: perm_imag1  !    .... imaginary part
  REAL( fp_kind )  :: perm_imag2  !    .... imaginary part
  REAL( fp_kind )  :: rverts      ! flat specular reflectivity (vertical)
  REAL( fp_kind )  :: rhorzs      ! flat specular reflectivity (horizontal)
  REAL( fp_kind )  :: rvertsr     !    .... real part
  REAL( fp_kind )  :: rvertsi     !    .... imaginary part
  REAL( fp_kind )  :: rhorzsr     !    .... real part
  REAL( fp_kind )  :: rhorzsi     !    .... imaginary part
  !
  REAL( fp_kind ) :: xcorr1      ! correction small scale (Bragg scattering)
  REAL( fp_kind )      :: rv          ! modifyed reflectivity
  REAL( fp_kind )      :: rh          ! modifyed reflectivity
  REAL( fp_kind )      :: evertr      ! effective rough ocean emissivity (vert.)
  REAL( fp_kind )      :: ehorzr      ! effective rough ocean emissivity (horiz.)
  REAL( fp_kind )      :: evert       ! polarised emissivity (vert.)
  REAL( fp_kind )      :: ehorz       ! polarised emissivity (horiz.)
  REAL( fp_kind )      :: ffoam       ! fractional foam coverage
  REAL( fp_kind )      :: opdpsfc     ! surface to space optical depth
  REAL( fp_kind )      :: zroughv     ! ratio of cos of effective to zenith angles (vert) 
  REAL( fp_kind )      :: zroughh     ! ratio of cos of effective to zenith angles (horz) 
  REAL( fp_kind )      :: rvert       ! reflectivity (vert.)
  REAL( fp_kind )      :: rhorz       ! reflectivity (horiz.)
  REAL( fp_kind )      :: zreflmodv   ! reflectivity decrease factor (horiz.)
  REAL( fp_kind )      :: zreflmodh   ! reflectivity decrease factor (vert.)
  REAL( fp_kind )      :: variance    ! wave slope variance
  REAL( fp_kind )      :: varm        ! maximum wave slope variance
  ! additional land surface emissivity model parameters
  REAL( fp_kind )      :: perm_static      ! static land permittivity
  REAL( fp_kind )      :: perm_infinite    ! infinite frequency land permittivity
  REAL( fp_kind )      :: freqr            ! relaxation frequency land
  REAL( fp_kind )      :: small_rough      ! small scale roughness
  REAL( fp_kind )      :: large_rough      ! large scale roughness
  REAL( fp_kind )      :: qdepol           ! depolarisation
  REAL( fp_kind )      :: delta            ! intermediate roughness parameter
  REAL( fp_kind )      :: delta2           ! intermediate roughness parameter

  ! SsirEM scalars
  REAL( fp_kind ) :: a0                    ! emissivity epsilon0 coefficient
  REAL( fp_kind ) :: a1                    ! emissivity epsilon1 coefficient
  REAL( fp_kind ) :: a2                    ! emissivity epsilon2 coefficient
  REAL( fp_kind ) :: xzn1                  ! polynomial basis function
  REAL( fp_kind ) :: xzn2                  ! polynomial basis function
  REAL( fp_kind ) :: xza                   ! normalized satellite zenith angle
 
  ! FASTEM complex variables
  COMPLEX :: xperm_TL, perm1_TL, perm2_TL,rhth_TL,rvth_TL,junk1
  COMPLEx :: xperm              ! permittivity
  COMPLEx :: perm1              ! Fresnel reflectivity complex variables
  COMPLEx :: perm2              ! ..
  COMPLEx :: rvth               ! ..
  COMPLEx :: rhth               ! ..
  INTEGER   :: ipol             ! polarisation indice for zvpol and zhpol
                                ! == mpol +1
                                !   1 average of vertical and horizontal
                                !   2 nominal vertical at nadir, rotating
                                !      with view angle
                                !   3 nominal horizontal at nadir, rotating
                                !      with view angle
                                !   4 vertical
                                !   5 horizontal
  REAL( fp_kind ), DIMENSION(3,5) ::  zvpol      ! zvpol and zhpol tell emiss
  REAL( fp_kind ), DIMENSION(3,5) ::  zhpol      ! how much v and h pol to use
 REAL( fp_kind ) :: esh, esv
!
!  NOAA  landEM
       real theta,frequ1,em_vector(2),t_skin,ps
!
!
  DATA zvpol / 0.5, 0.0, 0.0, &
               0.0, 0.0, 1.0, &
               0.0, 1.0, 0.0, &
               1.0, 0.0, 0.0, &
               0.0, 0.0, 0.0 /
  DATA zhpol / 0.5, 0.0, 0.0, &
               0.0, 1.0, 0.0, &
               0.0, 0.0, 1.0, &
               0.0, 0.0, 0.0, &
               1.0, 0.0, 0.0 /

   SAVE zvpol,zhpol
  !- End of header ------------------------------------------------------
  !
  !  using local zenith at the surface for Frensnel formula
!
           ifastem_version=36
           PI=ACOS(-1.0)
           pcc = cos(zenith*PI/180.0) 
           pc2 = pcc*pcc
           ps2 = 1.0 - pc2 
  !  satellite_to_earthcenter/earth_radius
  !  Sine nadir angle (at satellite)square 
          ratio=(satheight+earthrad)/earthrad
          snad2=ps2/(ratio*ratio)
          cnad2=1.0-snad2
           ! Get surface skin temperature
           pts = ts
           pts_TL = ts_TL
           ! Get windspeed
           u10mps = surfw
           u10mps_TL = surfw_TL

           ! Get amsu frequency 
   DO 103 kk=1,use_channel
!
           freqghz=f0(kk)
       ipol = PolStatus(kk)

       IF(stype.eq.1) then

           ! Calculate piom (ELLISON et al.) xperm
           ! Convert from kelvin to centigrate and define quadratic and
           ! Cubic functions for later polynomials
           tc  = pts-273.15
           tc_TL  = pts_TL
           tc2 = tc*tc
           tc2_TL = 2.0*tc*tc_TL
           tc3 = tc2*tc
           tc3_TL = tc2_TL*tc+tc2*tc_TL
           ! Define two relaxation frequencees, tau1 and tau2
           tau1 = emc(1) + emc(2)*tc + emc(3)*tc2
           tau1_TL =  emc(2)*tc_TL + emc(3)*tc2_TL
           tau2 = emc(4) + emc(5)*tc + emc(6)*tc2 + emc(7)*tc3
           tau2_TL = emc(5)*tc_TL + emc(6)*tc2_TL + emc(7)*tc3_TL
           ! static xperm estatic=del1+del2+einf
           del1 = emc(8)  + emc(9)*tc  + emc(10)*tc2 + emc(11)*tc3
           del1_TL =  emc(9)*tc_TL  + emc(10)*tc2_TL + emc(11)*tc3_TL
           del2 = emc(12) + emc(13)*tc + emc(14)*tc2 + emc(15)*tc3
           del2_TL = emc(13)*tc_TL + emc(14)*tc2_TL + emc(15)*tc3_TL
           einf = emc(18) + emc(19)*tc
           einf_TL = emc(19)*tc_TL
           ! Calculate xperm using double-debye formula
           fen        = 2.0*emc(20)*freqghz*0.001
           fen2       = fen*fen
           den1       = 1.0 + fen2*tau1*tau1
           den1_TL       =  2.0 * fen2 * tau1 * tau1_TL
           den2       = 1.0 + fen2*tau2*tau2
           den2_TL       =  2.0 * fen2 * tau2 * tau2_TL


           perm_real1 = del1/den1
           perm_real1_TL = del1_TL/den1-den1_TL*perm_real1/den1
           perm_real2 = del2/den2
           perm_real2_TL = del2_TL/den2-den2_TL*perm_real2/den2
           perm_imag1 = del1*fen*tau1/den1
           perm_imag1_TL = del1_TL*fen*tau1/den1+del1*fen*tau1_TL/den1 &
                         -den1_TL * perm_imag1/den1
           perm_imag2 = del2*fen*tau2/den2
           perm_imag2_TL = del2_TL*fen*tau2/den2+del2*fen*tau2_TL/den2 &
                         -den2_TL * perm_imag2/den2
           perm_real  = perm_real1 + perm_real2 + einf
           perm_real_TL  = perm_real1_TL + perm_real2_TL + einf_TL
           perm_imag  = perm_imag1 + perm_imag2
           perm_imag_TL  = perm_imag1_TL + perm_imag2_TL
           xperm      = CMPLx(perm_real,perm_imag)
           xperm_TL      = CMPLx(perm_real_TL,perm_imag_TL)
           !           
           ! Calculate complex fresnel reflection coefficients
           !        
           junk1=cmplx(ps2,0.0)
           perm1   = sqrt(xperm - junk1)
!           perm1   = csqrt(xperm - ps2)
           perm1_TL   = xperm_TL/(2.0*perm1)

           perm2   = xperm*pcc
           perm2_TL   = xperm_TL*pcc
           rhth    = (  pcc - perm1)/(  pcc + perm1)                     
           rhth_TL    =  - perm1_TL/(  pcc + perm1)-perm1_TL*rhth/(  pcc + perm1) 
           rvth    = (perm2 - perm1)/(perm2 + perm1)
           rvth_TL = (perm2_TL-perm1_TL)/(perm2+perm1)-(perm2_TL+perm1_TL)*rvth/(perm2+perm1)

           rvertsr = DBLE(rvth)
           rvertsr_TL = DBLE(rvth_TL)
           rvertsi = Aimag(rvth)
           rvertsi_TL = Aimag(rvth_TL)
           rverts  = rvertsr*rvertsr + rvertsi*rvertsi
           rverts_TL  = 2.0*rvertsr*rvertsr_TL +2.0* rvertsi*rvertsi_TL
           rhorzsr = DBLE(rhth)
           rhorzsr_TL = DBLE(rhth_TL)
           rhorzsi = Aimag(rhth)
           rhorzsi_TL = Aimag(rhth_TL)
           rhorzs  = rhorzsr*rhorzsr + rhorzsi*rhorzsi
           rhorzs_TL  = 2.0*rhorzsr*rhorzsr_TL + 2.0*rhorzsi*rhorzsi_TL
           ! Calculate small scale xcorr to reflection coefficients
           !        

           !          if (freqghz.gt.0.1) then
      xcorr1  = exp(emc(21)*u10mps*pc2/(freqghz*freqghz))
    xcorr1_TL  = emc(21)*u10mps_TL*pc2/(freqghz*freqghz)*exp(emc(21)*u10mps*pc2/(freqghz*freqghz))

           !          else
           !            xcorr1=1.0
           !          endif
           rv = rverts*xcorr1
           rh = rhorzs*xcorr1
           rv_TL = rverts*xcorr1_TL + rverts_TL * xcorr1
           rh_TL = rhorzs*xcorr1_TL + rhorzs_TL * xcorr1
           ! Calculate large scale geometric correction
           ! effective specular emissivity calculated
           !
           ! Calculate frequency, windspeed and view angle squared for
           ! polynomials
           freqghz2 = freqghz*freqghz
           u10mps2  = u10mps*u10mps
           u10mps2_TL  = 2.0*u10mps*u10mps_TL
           sec      = 1.0/pcc
           sec2     = sec*sec
           usec     = u10mps*sec
           usec_TL     = u10mps_TL*sec

           ! jp=1 => v pol, jp=2 => h pol 
           ! jp: two polaristions
           ! jc: six coefficients (constant, u, u^2, sec, sec^2, u*sec)	
           ! Select element from data array emc elements 24-59 for this model
           ! Coefficients 1-5 for this polarisation stored in zc

           zc1  = emc(24+ifastem_version) + &
                     emc(25+ifastem_version)*freqghz + &
                     emc(26+ifastem_version)*freqghz2
           zc2  = emc(27+ifastem_version) + &
                     emc(28+ifastem_version)*freqghz + &
                     emc(29+ifastem_version)*freqghz2
           zc3  = emc(30+ifastem_version) + &
                     emc(31+ifastem_version)*freqghz + &
                     emc(32+ifastem_version)*freqghz2
           zc4  = emc(33+ifastem_version) + &
                     emc(34+ifastem_version)*freqghz + &
                     emc(35+ifastem_version)*freqghz2
           zc5  = emc(36+ifastem_version) + &
                     emc(37+ifastem_version)*freqghz + &
                     emc(38+ifastem_version)*freqghz2
           zc6  = emc(39+ifastem_version) + &
                     emc(40+ifastem_version)*freqghz + &
                     emc(41+ifastem_version)*freqghz2
           zc7  = emc(42+ifastem_version) + &
                     emc(43+ifastem_version)*freqghz + &
                     emc(44+ifastem_version)*freqghz2
           zc8  = emc(45+ifastem_version) + &
                     emc(46+ifastem_version)*freqghz + &
                     emc(47+ifastem_version)*freqghz2
           zc9  = emc(48+ifastem_version) + &
                     emc(49+ifastem_version)*freqghz + &
                     emc(50+ifastem_version)*freqghz2
           zc10 = emc(51+ifastem_version) + &
                     emc(52+ifastem_version)*freqghz + &
                     emc(53+ifastem_version)*freqghz2
           zc11 = emc(54+ifastem_version) + &
                     emc(55+ifastem_version)*freqghz + &
                     emc(56+ifastem_version)*freqghz2
           zc12 = emc(57+ifastem_version) + &
                     emc(58+ifastem_version)*freqghz + &
                     emc(59+ifastem_version)*freqghz2
           ! Calculate correction for this polarisation
           xcorr21 =   zc1                      &
                        + zc2*sec                  &
                        + zc3*sec2                 &
                        + zc4*u10mps               &
                        + zc5*u10mps2+zc6*usec
           xcorr21_TL =                         &
                        + zc4*u10mps_TL               &
                        + zc5*u10mps2_TL+zc6*usec_TL
           xcorr21 = xcorr21/100.0
           xcorr21_TL = xcorr21_TL/100.0
           xcorr22 =   zc7                      &
                        + zc8*sec                  &
                        + zc9*sec2                 &
                        + zc10*u10mps              &
                        + zc11*u10mps2             &
                        + zc12*usec
           xcorr22_TL =                         &
                        + zc10*u10mps_TL              &
                        + zc11*u10mps2_TL             &
                        + zc12*usec_TL
           xcorr22 = xcorr22/100.0
           xcorr22_TL = xcorr22_TL/100.0
           evertr     = 1.0 - rv + xcorr21
           ehorzr     = 1.0 - rh + xcorr22
           evertr_TL     =  -rv_TL+xcorr21_TL
           ehorzr_TL     =  -rh_TL+xcorr22_TL
           ! Calculate foam emissivity correction
           ffoam     = emc(22)*(u10mps**emc(23))
           evert     = evertr - ffoam*evertr + ffoam
           ehorz     = ehorzr - ffoam*ehorzr + ffoam   
           ffoam_TL     = emc(23)*u10mps_TL*ffoam/u10mps
           evert_TL     = evertr_TL -ffoam_TL*evertr-ffoam*evertr_TL+ffoam_TL
           ehorz_TL     = ehorzr_TL - ffoam_TL*ehorzr -ffoam*ehorzr_TL+ ffoam_TL   

              rvert=1.0-evert
              rhorz=1.0-ehorz
              rvert_TL=-evert_TL
              rhorz_TL=-ehorz_TL
           ! Calculate emissivity
           pems =  evert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + ehorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
           pems_TL =  evert_TL  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + ehorz_TL *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
           pref =  rvert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + rhorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
           pref_TL =  rvert_TL  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + rhorz_TL *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
!
!   correction for AMSU  by Liu    7/22/2003
         if(freqghz.le.160.0) then
           xcorr1=-0.0500408+0.00145620*freqghz-0.00000815066*freqghz*freqghz
           xcorr1_TL = 0.0
!!           xcorr1=-0.0581531+0.00160144*freqghz-0.00000875832*freqghz*freqghz
         if(freqghz.le.60.0.and.freqghz.gt.50.0) then
            xcorr1=-0.006
            xcorr1_TL=0.0
         endif
           pems=pems+xcorr1
           pref=pref-xcorr1
           pems_TL=pems_TL+xcorr1_TL
           pref_TL=pref_TL-xcorr1_TL
         endif

        ELSE
!   land surface
      emissivity_TL(kk)=0.0
                                                                                             
        ENDIF
  !
      emissivity(kk)=pems
      reflection(kk)=pref

      emissivity_TL(kk)=pems_TL
 103  continue
  RETURN

 END SUBROUTINE EMISS_MW_TL

!
   SUBROUTINE EMISS_MW_AD(Nch,use_channel,channel_index,PolStatus,f0,stype, &
    zenith,surfw,ts,emissivity,reflection,emissivity_K,winds_K,surface_temperature_K)
! ------------------------------------------------------------------------
!
!    note !!
!  This subroutine does not take account for the temperature effect on
!   surface emissivity!
!
! Function:
!  Compute surface emissivity.
!  Reflectivity = 1 - Emissivity (for intensity, and
!  Reflectivity = Emissivity for other components.
!
!   INPUT:
!  NAME         TYPE    UNIT     DESCRIPTION
!  stype         I4     NONE     surface type
!  zenith        R8     Degree   local zenith angle
!  surfw         R8     m/s      surface wind at 10 meter
!  ts            R8     F degree surface temperature
!
!   OUTPUT
!   emissivity   R8     NONE      emissivity
! -----------------------------------------------------------------------
  USE MOD_SURFACE_COEF, ONLY : SINDEX,perm_static1,perm_infinite1, &
          perm_relaxation1,rough_small1,rough_large1,emc, &
          avn_soil_mois,avn_snow_depth, &   
          avn_soil_t,avn_veg_frac,avn_canopy_water,avn_veg_type, &
          avn_soil_type,surface_bta,surface_btb,snow_type

  USE CRTM_Parameters, ONLY : satheight,earthrad
    ! -- Utility modules
  USE Type_Kinds
  USE Error_Handler
!
  IMPLICIT NONE
!
  INTEGER :: kk,Nch,stype,use_channel
  INTEGER :: index1,ifastem_version
  INTEGER, DIMENSION( : ), INTENT (IN ) :: channel_index, PolStatus
  REAL( fp_kind ) :: zenith,surfw,ts
  REAL( fp_kind ), DIMENSION( : ) :: emissivity, reflection, f0
  REAL( fp_kind ), DIMENSION( : ) :: emissivity_K, winds_K, surface_temperature_K
  REAL( fp_kind ) :: xcorr1_K,u10mps_K,xcorr22_K,xcorr21_K,rv_K,rh_K,evert_K
  REAL( fp_kind ) :: ffoam_K,ehorz_K,pems_K

  REAL( fp_kind ) :: rhorz_K, rvert_K, ehorzr_K, evertr_K, pref_K,  rhorzs_K, rverts_K

!
  REAL( fp_kind ) :: pi,pcc,pir,pc2,ps2,ratio,snad2,cnad2,pts
  REAL( fp_kind ) ::  u10mps,freqghz,tc,tc2,tc3,tau1,tau2,del1,del2,einf,fen,fen2
  REAL( fp_kind ) ::  den1,den2,perm_real,freqghz2,u10mps2,sec,sec2,usec,zc1,zc2
  REAL( fp_kind ) ::  zc3,zc4,zc5,zc6,zc7,zc8,zc9,zc10,zc11,zc12,xcorr21,xcorr22,pems,pref
  REAL( fp_kind ) ::  wvnmcm1
                                                                                                               
  REAL( fp_kind ) ::  perm_imag   ! permittivity (imaginary part)
  REAL( fp_kind ) ::  perm_real1  !    .... real part
  REAL( fp_kind ) ::  perm_real2  !    .... real part
  REAL( fp_kind ) ::  perm_imag1  !    .... imaginary part
  REAL( fp_kind ) ::  perm_imag2  !    .... imaginary part
  REAL( fp_kind ) ::  rverts      ! flat specular reflectivity (vertical)
  REAL( fp_kind ) ::  rhorzs      ! flat specular reflectivity (horizontal)
  REAL( fp_kind ) ::  rvertsr     !    .... real part
  REAL( fp_kind ) ::  rvertsi     !    .... imaginary part
  REAL( fp_kind ) ::  rhorzsr     !    .... real part
  REAL( fp_kind ) ::  rhorzsi     !    .... imaginary part
  !
  REAL( fp_kind ) ::  xcorr10     ! 
  REAL( fp_kind ) ::  xcorr1      ! correction small scale (Bragg scattering)
  REAL( fp_kind ) ::  rv          ! modifyed reflectivity
  REAL( fp_kind ) ::  rh          ! modifyed reflectivity
  REAL( fp_kind ) ::  evertr      ! effective rough ocean emissivity (vert.)
  REAL( fp_kind ) ::  ehorzr      ! effective rough ocean emissivity (horiz.)
  REAL( fp_kind ) ::  evert       ! polarised emissivity (vert.)
  REAL( fp_kind ) ::  ehorz       ! polarised emissivity (horiz.)
  REAL( fp_kind ) ::  ffoam       ! fractional foam coverage
  REAL( fp_kind ) ::  opdpsfc     ! surface to space optical depth
  REAL( fp_kind ) ::  zroughv     ! ratio of cos of effective to zenith angles (vert)
  REAL( fp_kind ) ::  zroughh     ! ratio of cos of effective to zenith angles (horz)
  REAL( fp_kind ) ::  rvert       ! reflectivity (vert.)
  REAL( fp_kind ) ::  rhorz       ! reflectivity (horiz.)
  REAL( fp_kind ) ::  zreflmodv   ! reflectivity decrease factor (horiz.)
  REAL( fp_kind ) ::  zreflmodh   ! reflectivity decrease factor (vert.)
  REAL( fp_kind ) ::  variance    ! wave slope variance
  REAL( fp_kind ) ::  varm        ! maximum wave slope variance
  ! additional land surface emissivity model parameters
  REAL( fp_kind ) ::  perm_static      ! static land permittivity
  REAL( fp_kind ) ::  perm_infinite    ! infinite frequency land permittivity
  REAL( fp_kind ) ::  freqr            ! relaxation frequency land
  REAL( fp_kind ) ::  small_rough      ! small scale roughness
  REAL( fp_kind ) ::  large_rough      ! large scale roughness
  REAL( fp_kind ) ::  qdepol           ! depolarisation
  REAL( fp_kind ) ::  delta            ! intermediate roughness parameter
  REAL( fp_kind ) ::  delta2           ! intermediate roughness parameter
                                                                                                               
  ! SsirEM scalars
  REAL( fp_kind ) ::  a0                    ! emissivity epsilon0 coefficient
  REAL( fp_kind ) ::  a1                    ! emissivity epsilon1 coefficient
  REAL( fp_kind ) ::  a2                    ! emissivity epsilon2 coefficient
  REAL( fp_kind ) ::  xzn1                  ! polynomial basis function
  REAL( fp_kind ) ::  xzn2                  ! polynomial basis function
  REAL( fp_kind ) ::  xza                   ! normalized satellite zenith angle
                                                                                                               
  ! FASTEM complex variables
  COMPLEx :: junk1         
  COMPLEx :: xperm              ! permittivity
  COMPLEx :: perm1              ! Fresnel reflectivity complex variables
  COMPLEx :: perm2              ! ..
  COMPLEx :: rvth               ! ..
  COMPLEx :: rhth               ! ..
  INTEGER   :: ipol             ! polarisation indice for zvpol and zhpol
                                ! == mpol +1
                                !   1 average of vertical and horizontal
                                !   2 nominal vertical at nadir, rotating
                                !      with view angle
                                !   3 nominal horizontal at nadir, rotating
                                !      with view angle
                                !   4 vertical
                                !   5 horizontal
  REAL( fp_kind ),  DIMENSION(3,5) ::  zvpol      ! zvpol and zhpol tell emiss
  REAL( fp_kind ), DIMENSION(3,5) ::  zhpol      ! how much v and h pol to use
  REAL( fp_kind ) :: esh, esv
!
  REAL( fp_kind ) ::  usec_K, u10mps2_K
  REAL( fp_kind ) :: rvertsr_K,rvertsi_K,perm_real_K,perm_imag_K,pts_K,tc_K,tc2_K,tc3_K,rhorzsi_K,rhorzsr_K
  REAL( fp_kind ) :: perm_imag1_K,perm_imag2_K,perm_real1_K,perm_real2_K,einf_K,del2_K,tau2_K,den2_K,del1_K,tau1_K,den1_K
  complex rvth_K, rhth_K, perm2_K, perm1_K, xperm_K
!  NOAA  landEM
       real theta,frequ1,em_vector(2),t_skin,ps
!
!
  DATA zvpol / 0.5, 0.0, 0.0, &
               0.0, 0.0, 1.0, &
               0.0, 1.0, 0.0, &
               1.0, 0.0, 0.0, &
               0.0, 0.0, 0.0 /
  DATA zhpol / 0.5, 0.0, 0.0, &
               0.0, 1.0, 0.0, &
               0.0, 0.0, 1.0, &
               0.0, 0.0, 0.0, &
               1.0, 0.0, 0.0 /

   SAVE zvpol,zhpol
  !- End of header ------------------------------------------------------
  !
           winds_K (: ) = 0.0
!        emissivity = 0.3 + 0.001 * ts + 0.01 * surfw
!!          winds_K (: ) = 0.01 * emissivity_K( : )
!!       surface_temperature_K( : ) = surface_temperature_K( : ) + 0.001 * emissivity_K( : )
          
!!         if(surfw.gt.0.0) return
  !  using local zenith at the surface for Frensnel formula
!
           ifastem_version=36
           PI=ACOS(-1.0)
           pcc = cos(zenith*PI/180.0) 
           pc2 = pcc*pcc
           ps2 = 1.0 - pc2 
  !  satellite_to_earthcenter/earth_radius
  !  Sine nadir angle (at satellite)square 
          ratio=(satheight+earthrad)/earthrad
          snad2=ps2/(ratio*ratio)
          cnad2=1.0-snad2
           ! Get surface skin temperature
           pts = ts


  !  test  using constatnt temperature

           ! Get windspeed
           u10mps = surfw
           ! Get amsu frequency 
   DO 103 kk=1,use_channel
!
      pts_K = 0.0
           freqghz=f0(kk)
       ipol = PolStatus(kk)

     pems_K = emissivity_K(kk) 
           rv_K=0.0
           rh_K=0.0
           rhorz_K=0.0
           rvert_K=0.0
           ehorzr_K=0.0
           evertr_K=0.0
           pref_K = 0.0
           xcorr1_K = 0.0
           xcorr21_K = 0.0
           xcorr22_K = 0.0
           ehorz_K = 0.0
           rhorz_K = 0.0
           evert_K = 0.0
           rvert_K = 0.0
           u10mps_K = 0.0
           u10mps2_K = 0.0
           usec_K = 0.0
           rverts_K = 0.0
           rhorzs_K = 0.0
       IF(stype.eq.1) then
!           perm_real = 20.0 + 0.03 * pts
!          perm_imag = -0.02*pts
                                                                                                                 
           ! Calculate piom (ELLISON et al.) xperm
           ! Convert from kelvin to centigrate and define quadratic and
           ! Cubic functions for later polynomials
           tc  = pts-273.15
           tc2 = tc*tc
           tc3 = tc2*tc
           ! Define two relaxation frequencees, tau1 and tau2
           tau1 = emc(1) + emc(2)*tc + emc(3)*tc2
           tau2 = emc(4) + emc(5)*tc + emc(6)*tc2 + emc(7)*tc3
           ! static xperm estatic=del1+del2+einf
           del1 = emc(8)  + emc(9)*tc  + emc(10)*tc2 + emc(11)*tc3
           del2 = emc(12) + emc(13)*tc + emc(14)*tc2 + emc(15)*tc3
           einf = emc(18) + emc(19)*tc
           ! Calculate xperm using double-debye formula
           fen        = 2.0*emc(20)*freqghz*0.001
           fen2       = fen*fen
           den1       = 1.0 + fen2*tau1*tau1
           den2       = 1.0 + fen2*tau2*tau2
           perm_real1 = del1/den1
           perm_real2 = del2/den2
           perm_imag1 = del1*fen*tau1/den1
           perm_imag2 = del2*fen*tau2/den2
           perm_real  = perm_real1 + perm_real2 + einf
           perm_imag  = perm_imag1 + perm_imag2

           xperm      = CMPLx(perm_real,perm_imag)
!           xperm = cdsqrt(xperm)
           junk1=cmplx(ps2, 0.0)
           perm1   = sqrt(xperm - junk1)
           perm2   = xperm*pcc
           rhth    = (  pcc - perm1)/(  pcc + perm1)                     
           rvth    = (perm2 - perm1)/(perm2 + perm1)
           rvertsr = DBLE(rvth)
           rvertsi = Aimag(rvth)
           rverts  = rvertsr*rvertsr + rvertsi*rvertsi
           rhorzsr = DBLE(rhth)
           rhorzsi = Aimag(rhth)
           rhorzs  = rhorzsr*rhorzsr + rhorzsi*rhorzsi

           xcorr1  = exp(emc(21)*u10mps*pc2/(freqghz*freqghz))
           rv = rverts*xcorr1
           rh = rhorzs*xcorr1
           freqghz2 = freqghz*freqghz
           u10mps2  = u10mps*u10mps
           sec      = 1.0/pcc
           sec2     = sec*sec
           usec     = u10mps*sec
                                                                                                                 
           zc1  = emc(24+ifastem_version) + &
                     emc(25+ifastem_version)*freqghz + &
                     emc(26+ifastem_version)*freqghz2
           zc2  = emc(27+ifastem_version) + &
                     emc(28+ifastem_version)*freqghz + &
                     emc(29+ifastem_version)*freqghz2
           zc3  = emc(30+ifastem_version) + &
                     emc(31+ifastem_version)*freqghz + &
                     emc(32+ifastem_version)*freqghz2
           zc4  = emc(33+ifastem_version) + &
                     emc(34+ifastem_version)*freqghz + &
                     emc(35+ifastem_version)*freqghz2
           zc5  = emc(36+ifastem_version) + &
                     emc(37+ifastem_version)*freqghz + &
                     emc(38+ifastem_version)*freqghz2
           zc6  = emc(39+ifastem_version) + &
                     emc(40+ifastem_version)*freqghz + &
                     emc(41+ifastem_version)*freqghz2
           zc7  = emc(42+ifastem_version) + &
                     emc(43+ifastem_version)*freqghz + &
                     emc(44+ifastem_version)*freqghz2
           zc8  = emc(45+ifastem_version) + &
                     emc(46+ifastem_version)*freqghz + &
                     emc(47+ifastem_version)*freqghz2
           zc9  = emc(48+ifastem_version) + &
                     emc(49+ifastem_version)*freqghz + &
                     emc(50+ifastem_version)*freqghz2
           zc10 = emc(51+ifastem_version) + &
                     emc(52+ifastem_version)*freqghz + &
                     emc(53+ifastem_version)*freqghz2
           zc11 = emc(54+ifastem_version) + &
                     emc(55+ifastem_version)*freqghz + &
                     emc(56+ifastem_version)*freqghz2
           zc12 = emc(57+ifastem_version) + &
                     emc(58+ifastem_version)*freqghz + &
                     emc(59+ifastem_version)*freqghz2
           ! Calculate correction for this polarisation
           xcorr21 =   zc1                      &
                        + zc2*sec                  &
                        + zc3*sec2                 &
                        + zc4*u10mps               &
                        + zc5*u10mps2+zc6*usec
           xcorr21 = xcorr21/100.0

           xcorr22 =   zc7                      &
                        + zc8*sec                  &
                        + zc9*sec2                 &
                        + zc10*u10mps              &
                        + zc11*u10mps2             &
                        + zc12*usec
           xcorr22 = xcorr22/100.0
           evertr     = 1.0 - rv + xcorr21
           ehorzr     = 1.0 - rh + xcorr22
           ! Calculate foam emissivity correction
           ffoam     = emc(22)*(u10mps**emc(23))
           evert     = evertr - ffoam*evertr + ffoam
           ehorz     = ehorzr - ffoam*ehorzr + ffoam
              rvert=1.0-evert
              rhorz=1.0-ehorz
           ! Calculate emissivity
           pems =  evert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + ehorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
           pref =  rvert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + rhorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
!
!   correction for AMSU  by Liu    7/22/2003
         if(freqghz.le.160.0) then
           xcorr10=-0.0500408+0.00145620*freqghz-0.00000815066*freqghz*freqghz
!!           xcorr1=-0.0581531+0.00160144*freqghz-0.00000875832*freqghz*freqghz
         if(freqghz.le.60.0.and.freqghz.gt.50.0) then
            xcorr10=-0.006
         endif
           pems=pems+xcorr10
           pref=pref-xcorr10
         endif
           !           
!   adjoint part
      evert_K=pems_K * (zvpol(1,ipol)+ zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)
      ehorz_K=pems_K * (zhpol(1,ipol)+ zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
      rvert_K=pref_K * (zvpol(1,ipol)+ zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)
      rhorz_K=pref_K * (zhpol(1,ipol)+ zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)

      ehorz_K = ehorz_K - rhorz_K
      evert_K = evert_K - rvert_K
      ffoam_K=ehorz_K
      ffoam_K=ffoam_K-ehorz_K*ehorzr
      ehorzr_K=ehorz_K
      ehorzr_K=ehorzr_K-ffoam*ehorz_K
      ffoam_K= ffoam_K+evert_K
      ffoam_K= ffoam_K-evert_K*evertr
      evertr_K=evert_K
      evertr_K=evertr_K-ffoam*evert_K
      u10mps_K=emc(23)*ffoam*ffoam_K/u10mps

      rh_K=-ehorzr_K
      xcorr22_K= ehorzr_K
      rv_K=-evertr_K
      xcorr21_K=evertr_K
      xcorr22_K=xcorr22_K/100.0
      u10mps_K=u10mps_K+zc10*xcorr22_K
      usec_K = usec_K + zc12*xcorr22_K
      u10mps2_K = u10mps2_K + zc11*xcorr22_K

      xcorr21_K=xcorr21_K/100.0
      u10mps_K=u10mps_K+zc4*xcorr21_K
      usec_K = usec_K + zc6*xcorr21_K
      u10mps2_K = u10mps2_K + zc5*xcorr21_K

      u10mps_K = u10mps_K + usec_K * sec
      u10mps_K = u10mps_K + 2.0 * u10mps*u10mps2_K
      xcorr1_K=xcorr1_K+rhorzs*rh_K
      rhorzs_K=rhorzs_K+xcorr1*rh_K
      xcorr1_K=xcorr1_K+rverts*rv_K
      rverts_K=rverts_K+xcorr1*rv_K
           ! Calculate small scale xcorr to reflection coefficients
      xcorr1  = exp(emc(21)*u10mps*pc2/(freqghz*freqghz))
      u10mps_K=u10mps_K+emc(21)*pc2/(freqghz*freqghz)*xcorr1*xcorr1_K

  ! -------  temperature part  -------------------------------

      rhorzsr_K = rhorzs_K * 2.0 * rhorzsr
      rhorzsi_K = rhorzs_K * 2.0 * rhorzsi
      rhth_K = cmplx(rhorzsr_K, -rhorzsi_K)
      rvertsr_K = 2.0 * rvertsr * rverts_K
      rvertsi_K = 2.0 * rvertsi * rverts_K
      rvth_K = cmplx(rvertsr_K, -rvertsi_K)
      perm2_K =  rvth_K/(perm2 + perm1)
      perm1_K = -rvth_K/(perm2 + perm1)
      perm2_K =  perm2_K - rvth * rvth_K/(perm2 + perm1)
      perm1_K =  perm1_K - rvth * rvth_K/(perm2 + perm1)
      perm1_K = perm1_K -  rhth_K/(  pcc + perm1)
      perm1_K = perm1_K - rhth * rhth_K/(  pcc + perm1)

      xperm_K = perm2_K * pcc
      xperm_K = xperm_K + perm1_K/perm1/2.0
      perm_real_K=dble(xperm_K)
      perm_imag_K=-Aimag(xperm_K)
      perm_imag1_K = perm_imag_K
      perm_imag2_K = perm_imag_K
      perm_real1_K = perm_real_K
      perm_real2_K = perm_real_K
      einf_K = perm_real_K
      del2_K = perm_imag2_K * fen*tau2/den2
      tau2_K = perm_imag2_K * del2*fen/den2
      den2_K = - perm_imag2_K * perm_imag2/den2
      del1_K = perm_imag1_K * fen*tau1/den1
      tau1_K = del1*fen*perm_imag1_K/den1
      den1_K = - perm_imag1_K * perm_imag1/den1
      del2_K = del2_K + perm_real2_K/den2
      den2_K = den2_K - perm_real2 * perm_real2_K/den2
      del1_K = del1_K + perm_real1_K/den1
      den1_K = den1_K - perm_real1 * perm_real1_K/den1
      tau2_K = tau2_K + 2.0 * den2_K * fen2*tau2
      tau1_K = tau1_K + 2.0 * den1_K * fen2*tau1
      tc_K = emc(19)*einf_K
      tc3_K = emc(15)*del2_K
      tc2_K = emc(14)*del2_K
      tc_K = tc_K + emc(13)*del2_K
      tc3_K = tc3_K + emc(11)*del1_K
      tc2_K = tc2_K + emc(10)*del1_K
      tc_K = tc_K + emc(9)*del1_K
      tc3_K = tc3_K + emc(7)*tau2_K
      tc2_K = tc2_K + emc(6)*tau2_K
      tc_K = tc_K + emc(5)*tau2_K
      tc2_K = tc2_K + emc(3)*tau1_K
      tc_K = tc_K + emc(2)*tau1_K
      tc2_K = tc2_K + tc * tc3_K
      tc_K = tc_K + tc2 * tc3_K
      tc_K = tc_K + 2.0 * tc * tc2_K
      pts_K = tc_K

    ELSE 
  ! Land fastem code
           index1=SINDEX(stype)
           ! coherent surface scattering model coefficients
           perm_static=perm_static1(index1)
           perm_infinite=perm_infinite1(index1)
           freqr=perm_relaxation1(index1)
           small_rough=rough_small1(index1)
           large_rough=rough_large1(index1)
!
           wvnmcm1=freqghz/30.0
           ! simple debye + fresnel model gives reflectivities
           fen=freqghz/freqr
           fen2=fen**2.0
           den1=1.0+fen2
           perm_real=(perm_static+perm_infinite*fen2)/den1
           perm_imag=fen*(perm_static-perm_infinite)/den1
           xperm=CMPLX(perm_real,perm_imag)
           junk1=cmplx(ps2, 0.0)
           perm1   = sqrt(xperm - junk1)
!!           perm1 = CSQRT(xperm - ps2)
           perm2  = xperm*PCC
           rhth = (  pcc - perm1)/(  pcc + perm1)
           rvth = (perm2 - perm1)/(perm2 + perm1)
           rvertsr=DBLE(rvth)
           rvertsi=Aimag(rvth)
           rverts=rvertsr*rvertsr+rvertsi*rvertsi
           rhorzsr=DBLE(rhth)
           rhorzsi=Aimag(rhth)
           rhorzs=rhorzsr*rhorzsr+rhorzsi*rhorzsi
           ! apply small_rough scale roughness correction
           delta = 4.0*pi*wvnmcm1*0.1*small_rough
           delta2=delta*delta
           xcorr1=exp(-delta2*pc2)
           evertr=1.0-rverts*xcorr1
           ehorzr=1.0-rhorzs*xcorr1
           ! apply large scale roughness correction
           qdepol = 0.35 - 0.35*EXP(-0.60*freqghz*large_rough**2.)
           evert=evertr*(1.0-qdepol)+ehorzr*qdepol
           ehorz=ehorzr*(1.0-qdepol)+evertr*qdepol
           rvert=1.0-evert
           rhorz=1.0-ehorz
                                                                                                                 
          em_vector(2)=evert
          em_vector(1)=ehorz
!
!           if( evert.ge.0.0) go to 1033    ! using English model
!
!  *************************   NOAA  landEM   **********************
!
    theta=zenith
    t_skin=ts
    frequ1=f0(kk)
!
    if(stype.gt.3.and.stype.lt.24) then
    call LandEM(zenith, f0(kk),avn_soil_mois, avn_veg_frac, avn_veg_type, &
            avn_soil_type, avn_soil_t, ts, avn_snow_depth, esh, esv)
          em_vector(1) = esh
          em_vector(2) = esv
    else
    if(maxval(surface_bta) > 100.0 .or. maxval(surface_btb) > 100.0) then
   call SIceEM(theta,frequ1,avn_snow_depth,t_skin,surface_bta,surface_btb,em_vector)
    else
    call LandEM(zenith, f0(kk), avn_soil_mois, avn_veg_frac, avn_veg_type, &
            avn_soil_type, avn_soil_t, ts, avn_snow_depth, esh, esv)
          em_vector(1) = esh
          em_vector(2) = esv
    endif
    endif
!
     evert=em_vector(2)
     ehorz=em_vector(1)
     rvert=1.0-evert
     rhorz=1.0-ehorz
!
 1033  continue
           pems =  evert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + ehorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                       + zhpol(3,ipol)*cnad2)
           pref =  rvert  *                         &
                      (zvpol(1,ipol)                   &
                       + zvpol(2,ipol)*snad2    &
                       + zvpol(3,ipol)*cnad2)   &
                    + rhorz *                          &
                      (zhpol(1,ipol)                   &
                       + zhpol(2,ipol)*snad2    &
                      + zhpol(3,ipol)*cnad2)
        ENDIF
  !
      emissivity(kk)=pems
      reflection(kk)=pref
!
      winds_K(kk)=u10mps_K

     !  using constant temperature
!       pts_K = 0.0

      surface_temperature_K(kk) = surface_temperature_K(kk) + pts_K

 103  continue
  RETURN
 END SUBROUTINE EMISS_MW_AD
!

   END MODULE Surface_Emissivity_Model
