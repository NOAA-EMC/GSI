!
MODULE mod_rttov_fastem4_coef

  ! Description:
  ! Constant coefficients and parameters for FASTEM-4.  
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
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  INTEGER, PARAMETER :: fp = SELECTED_REAL_KIND(15) ! Double precision
  PUBLIC :: FresnelVariables_type
  PUBLIC :: PermittivityVariables_type
  PUBLIC :: fp
  ! Environment setup
  ! -----------------
  ! Module use
!  INTEGER, PUBLIC, PARAMETER  :: fp = SELECTED_REAL_KIND(15)
  REAL(fp), PUBLIC, PARAMETER :: ZERO     = 0.0_fp
  REAL(fp), PUBLIC, PARAMETER :: POINT_5  = 0.5_fp
  REAL(fp), PUBLIC, PARAMETER :: ONE      = 1.0_fp
  REAL(fp), PUBLIC, PARAMETER :: TWO      = 2.0_fp  
  REAL(fp), PUBLIC, PARAMETER :: THREE    = 3.0_fp  
  REAL(fp), PUBLIC, PARAMETER :: PI = 3.141592653589793238462643383279_fp  
  REAL(fp), PUBLIC, PARAMETER :: DEGREES_TO_RADIANS = PI/180.0_fp
  REAL(fp), PUBLIC, PARAMETER :: transmittance_limit = 0.1_fp
    
!
  REAL(fp), PUBLIC, PARAMETER :: e0 = 0.0088419_fp
  ! minimum and maximum frequency
  REAL( fp ), PUBLIC, PARAMETER ::  min_f = 1.4_fp
  REAL( fp ), PUBLIC, PARAMETER ::  max_f = 200.0_fp
  ! minimum and maximum wind speed
  REAL( fp ), PUBLIC, PARAMETER ::  min_wind = 0.3_fp
  REAL( fp ), PUBLIC, PARAMETER ::  max_wind = 35.0_fp
   
  ! The fitting coefficients for the JCSDA permittivity model
  ! see the ref. (An Improved Fast Microwave Sea Surface Emissivity Model, FASTEM4,
  ! Technical Report, UK Met. Office)  
  REAL(fp), PUBLIC, PARAMETER :: A_COEF(0:38) = (/ 3.8_fp, 0.0248033_fp, 87.9181727_fp, &
    -0.4031592248_fp,    0.0009493088010_fp,  -0.1930858348E-05_fp, -0.002697_fp,       &
    -7.3E-06_fp,        -8.9E-06_fp,           5.723_fp,             0.022379_fp,       &
    -0.00071237_fp,     -6.28908E-03_fp,     1.76032E-04_fp,        -9.22144E-05_fp,    &
     0.1124465_fp,      -0.0039815727_fp,    0.00008113381_fp,      -0.00000071824242_fp,&
    -2.39357E-03_fp,     3.1353E-05_fp,       -2.52477E-07_fp,       0.003049979018_fp, &
    -3.010041629E-05_fp, 0.4811910733E-05_fp, -0.4259775841E-07_fp,  0.149_fp,          &
    -8.8E-04_fp,        -1.05E-04_fp,          2.033E-02_fp,         1.266E-04_fp,  &
     2.464E-06_fp,      -1.849E-05_fp,         2.551E-07_fp,        -2.551E-08_fp,  &
     0.182521_fp,       -1.46192E-03_fp,       2.09324E-05_fp,      -1.28205E-07_fp/)


  ! fitting coefficients for the large-scale correction (Monte-Carlo, including small-scale)
  REAL(fp), PARAMETER :: Lcoef_mc(36) = (/ &
   2.546198E-02_fp, 1.268603E-03_fp, 5.970470E-07_fp,-1.587135E-02_fp,-1.452018E-03_fp, &
  -8.418206E-07_fp, 1.277578E-03_fp, 3.271675E-04_fp, 3.577548E-07_fp, 2.298474E-03_fp, &
   6.346938E-05_fp,-1.781176E-07_fp,-8.289674E-06_fp,-8.386874E-08_fp, 4.203651E-10_fp, &
  -1.082530E-03_fp,-3.706245E-05_fp, 1.134826E-07_fp, 4.003590E-02_fp, 3.665413E-05_fp, &
   1.677992E-06_fp,-3.735051E-02_fp, 1.395696E-04_fp,-2.095581E-06_fp, 8.182831E-03_fp, &
  -3.247136E-05_fp, 5.305700E-07_fp, 4.740625E-04_fp,-1.776569E-05_fp, 1.204442E-07_fp, &
  -8.289674E-06_fp,-8.386874E-08_fp, 4.203651E-10_fp, 7.418811E-04_fp, 4.417262E-05_fp, &
  -1.850792E-07_fp /)
 
  ! fitting coefficients for the large-scale correction
  REAL(fp),  PUBLIC, PARAMETER :: Lcoef(36) = (/ &
  -9.197134E-02_fp, 8.310678E-04_fp,-6.065411E-07_fp, 1.350073E-01_fp,-1.032096E-03_fp, &
   4.259935E-07_fp,-4.373322E-02_fp, 2.545863E-04_fp, 9.835554E-08_fp,-1.199751E-03_fp, &
   1.360423E-05_fp,-2.088404E-08_fp,-2.201640E-05_fp, 1.951581E-07_fp,-2.599185E-10_fp, &
   4.477322E-04_fp,-2.986217E-05_fp, 9.406466E-08_fp,-7.103127E-02_fp,-4.713113E-05_fp, &
   1.754742E-06_fp, 9.720859E-02_fp, 1.374668E-04_fp,-2.591771E-06_fp,-2.687455E-02_fp, &
  -3.677779E-05_fp, 7.548377E-07_fp,-3.049506E-03_fp,-5.412826E-05_fp, 2.285387E-07_fp, &
  -2.201640E-05_fp, 1.951581E-07_fp,-2.599185E-10_fp, 2.297488E-03_fp, 3.787032E-05_fp, &
  -1.553581E-07_fp /) 
  
  ! fitting coefficients for the large-scale correction, test 
  REAL(fp), PARAMETER :: Lcoef_n(36) = (/ &
  -6.295769E-02_fp, 8.976402E-04_fp,-1.353478E-06_fp, 9.238382E-02_fp,-1.139358E-03_fp, &
   1.587908E-06_fp,-3.001973E-02_fp, 2.919056E-04_fp,-2.959508E-07_fp, 8.618365E-04_fp, &
   1.984034E-05_fp,-8.422975E-08_fp,-8.711030E-06_fp,-1.785806E-08_fp, 3.148173E-10_fp, &
  -8.762560E-04_fp,-3.302121E-05_fp, 1.330427E-07_fp,-4.810724E-02_fp,-2.342925E-05_fp, &
   1.282490E-06_fp, 6.415778E-02_fp, 9.334242E-05_fp,-1.848531E-06_fp,-1.664414E-02_fp, &
  -1.972511E-05_fp, 5.045198E-07_fp,-1.372860E-03_fp,-4.690695E-05_fp, 1.701659E-07_fp, &
  -8.711030E-06_fp,-1.785806E-08_fp, 3.148173E-10_fp, 1.358441E-03_fp, 3.372608E-05_fp, &
  -1.213529E-07_fp /)

  ! fitting coefficients from the FASTEM3
  REAL(fp), PARAMETER :: Lcoef_f3(36) = (/ &
  -0.182390E-01_fp,-0.434790E-04_fp, 0.646320E-06_fp, 0.278640E-01_fp, 0.878460E-04_fp, &
  -0.102670E-05_fp,-0.101890E-01_fp,-0.426820E-04_fp, 0.396520E-06_fp, 0.730720E-03_fp, &
   0.261790E-04_fp,-0.950500E-07_fp, 0.295330E-05_fp, 0.443690E-07_fp,-0.140160E-09_fp, &
  -0.717940E-03_fp,-0.267870E-04_fp, 0.949560E-07_fp,-0.334690E-02_fp, 0.951660E-04_fp, &
   0.964400E-07_fp, 0.470780E-02_fp,-0.148970E-03_fp,-0.987460E-07_fp,-0.142750E-02_fp, &
   0.565380E-04_fp, 0.118850E-07_fp,-0.137840E-02_fp,-0.216950E-04_fp, 0.793130E-07_fp, &
   0.237840E-06_fp, 0.869500E-08_fp, 0.282490E-10_fp, 0.138790E-02_fp, 0.209470E-04_fp, &
  -0.797900E-07_fp /)

  ! fitting coefficients from the FASTEM1
  REAL(fp), PARAMETER :: Lcoef_f1(36) = (/ &
  -0.637180E-01_fp, 0.253920E-03_fp, 0.357570E-06_fp, 0.942930E-01_fp,-0.332840E-03_fp, &
  -0.647720E-06_fp,-0.329280E-01_fp, 0.965450E-04_fp, 0.281590E-06_fp, 0.252680E-02_fp, &
   0.343870E-04_fp,-0.156360E-06_fp,-0.156670E-05_fp, 0.139490E-06_fp,-0.407630E-09_fp, &
  -0.141320E-02_fp,-0.356560E-04_fp, 0.142870E-06_fp,-0.240700E-01_fp,-0.563890E-03_fp, &
   0.325230E-05_fp, 0.296010E-01_fp, 0.704680E-03_fp,-0.426440E-05_fp,-0.751250E-02_fp, &
  -0.191930E-03_fp, 0.125940E-05_fp,-0.288250E-02_fp,-0.102650E-04_fp, 0.226700E-07_fp, &
  -0.119070E-04_fp,-0.263170E-06_fp, 0.114600E-08_fp, 0.406300E-02_fp, 0.200030E-04_fp, &
  -0.781640E-07_fp /)


  ! fitting coefficients for the small-scale correction
  REAL(fp),  PUBLIC, PARAMETER :: Scoef(8) = (/ &   
    -5.0208480E-06_fp,   2.3297951E-08_fp,   4.6625726E-08_fp,  -1.9765665E-09_fp, &
    -7.0469823E-04_fp,   7.5061193E-04_fp,   9.8103876E-04_fp,   1.5489504E-04_fp /)

  ! fitting coefficients for the downward radiation using transmittance
  REAL(fp),  PUBLIC, PARAMETER :: t_c(45) = (/ &
  -0.675700E-01_fp, 0.214600E+00_fp,-0.363000E-02_fp, 0.636730E+01_fp, 0.900610E+00_fp, &
  -0.524880E+00_fp,-0.370920E+01_fp,-0.143310E+01_fp, 0.397450E+00_fp, 0.823100E-01_fp, &
  -0.255980E+00_fp, 0.552000E-02_fp, 0.208000E+01_fp, 0.244920E+01_fp,-0.456420E+00_fp, &
  -0.224900E-01_fp, 0.616900E-01_fp,-0.344000E-02_fp,-0.507570E+01_fp,-0.360670E+01_fp, &
   0.118750E+01_fp, 0.124950E+00_fp, 0.121270E+00_fp, 0.714000E-02_fp, 0.736620E+01_fp, &
  -0.114060E+00_fp,-0.272910E+00_fp,-0.504350E+01_fp,-0.336450E+00_fp, 0.161260E+00_fp, &
  -0.154290E+00_fp,-0.141070E+00_fp,-0.809000E-02_fp, 0.395290E+01_fp, 0.958580E+00_fp, &
  -0.159080E+00_fp, 0.368500E-01_fp, 0.307100E-01_fp, 0.810000E-03_fp,-0.619960E+01_fp, &
  -0.172580E+01_fp, 0.641360E+00_fp, 0.100000E+01_fp, 0.200000E-01_fp, 0.300000E+00_fp /)

  ! fitting coefficients for the azimuth dependence, S1 and S4 trained by 
  REAL(fp), PUBLIC, PARAMETER :: b_coef(120) = (/ &
   1.318143E-02_fp,-1.660586E-04_fp,-7.102244E-03_fp, 8.771616E-05_fp,-3.418311E-03_fp, &
   3.784895E-05_fp, 5.763184E-05_fp,-6.290578E-07_fp, 1.839451E-03_fp,-1.856317E-05_fp, &
   4.605486E-03_fp, 5.781246E-05_fp,-2.746737E-03_fp,-4.690045E-05_fp, 1.512049E-04_fp, &
  -7.411844E-09_fp,-3.476559E-06_fp, 1.466902E-07_fp,-6.472364E-05_fp,-1.776898E-06_fp, &
   1.205735E-03_fp,-1.748276E-05_fp,-6.002919E-04_fp, 1.174144E-05_fp,-1.735732E-04_fp, &
   2.148296E-06_fp, 2.955853E-06_fp,-3.609258E-08_fp, 9.669164E-05_fp,-1.282544E-06_fp, &
   6.459324E-03_fp,-7.570050E-05_fp,-3.777932E-03_fp, 4.270676E-05_fp,-1.247285E-03_fp, &
   1.136239E-05_fp, 2.123934E-05_fp,-2.377368E-07_fp, 7.070105E-04_fp,-5.092876E-06_fp, &
  -1.863094E-02_fp, 2.768660E-04_fp, 7.624930E-03_fp,-1.397481E-04_fp, 3.550912E-03_fp, &
  -5.533696E-05_fp,-6.557083E-05_fp, 9.948138E-07_fp,-1.626538E-03_fp, 2.307157E-05_fp, &
  -7.610401E-04_fp, 1.293120E-05_fp, 3.796897E-04_fp,-5.562741E-06_fp, 8.865672E-05_fp, &
  -1.313724E-06_fp, 7.009076E-08_fp, 2.426378E-08_fp,-8.192732E-05_fp, 5.333771E-07_fp, &
  -6.296038E-03_fp, 3.835747E-05_fp, 3.013694E-03_fp,-9.366178E-06_fp, 1.680703E-03_fp, &
  -5.745778E-06_fp,-2.942056E-05_fp, 1.889216E-07_fp,-9.058433E-04_fp,-1.136992E-06_fp, &
  -2.880306E-02_fp, 2.418851E-04_fp, 1.290535E-02_fp,-8.803702E-05_fp, 5.057109E-06_fp, &
  -2.715428E-05_fp,-6.912266E-05_fp, 7.852767E-07_fp, 5.337096E-04_fp, 6.585635E-06_fp, &
  -1.834561E-03_fp, 2.896784E-05_fp, 7.613927E-04_fp,-1.367783E-05_fp, 4.887281E-04_fp, &
  -5.810380E-06_fp,-9.568319E-06_fp, 1.207029E-07_fp,-2.210790E-04_fp, 2.159904E-06_fp, &
  -5.854263E-04_fp, 5.546263E-06_fp, 2.485058E-04_fp,-1.531698E-06_fp, 1.243394E-04_fp, &
  -1.575561E-06_fp,-2.437488E-06_fp, 2.986237E-08_fp,-5.555700E-05_fp, 6.076001E-07_fp, &
   6.042016E-03_fp,-1.135219E-04_fp,-2.231061E-03_fp, 5.729232E-05_fp,-1.543391E-03_fp, &
   2.288614E-05_fp, 2.828443E-05_fp,-4.384802E-07_fp, 7.080137E-04_fp,-9.827192E-06_fp, &
  -2.054959E-04_fp, 1.806305E-07_fp, 1.144686E-04_fp, 4.638982E-07_fp, 3.581176E-05_fp, &
  -3.870976E-07_fp,-6.861957E-07_fp, 6.989780E-09_fp,-1.526136E-05_fp, 1.887424E-07_fp /)


    ! Frequency-dependent azimuth correction
    REAL(fp), PUBLIC, PARAMETER :: x(9) = (/ 0.0_fp, 1.4_fp, 6.8_fp, 10.7_fp, 19.35_fp, &
                                   37._fp, 89._fp, 150._fp, 200._fp/)
    REAL(fp), PUBLIC, PARAMETER :: y(9) = (/ 0.0_fp, 0.1_fp, 0.6_fp, 0.9_fp, 1._fp, &
                                   1.0_fp, 0.4_fp, 0.2_fp, 0.0_fp/)
  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
 ! =============================================================
  ! Routine for forward model foam reflectivity
  ! Function dependence is on zenith angle only
  ! so no TL or AD routine.
  ! See Eqns(18.44a) (18.44b) in
  !   Ulaby, F.T. et al. (1986) Microwave Remote Sensing, Active
  !     and Passive, vol.3, From Theory to Applications, pp1457.
  ! =============================================================
  REAL(fp), PUBLIC, PARAMETER :: FR_COEFF(9) = &
    (/ -9.946e-4_fp, 3.218e-5_fp, -1.187e-6_fp, &
          7.e-20_fp,     0.07_fp, -1.748e-3_fp, &
       -7.336e-5_fp, 1.044e-7_fp,     -0.93_fp /)

  TYPE :: FresnelVariables_type
!    PRIVATE
    ! The intermediate terms
    COMPLEX(fp) :: z1, z2
    ! The real and imaginary components
    REAL(fp)    :: rzRv,izRv  ! Vertical
    REAL(fp)    :: rzRh,izRh  ! Horizontal
  END TYPE FresnelVariables_type


  ! For the Ellison et al (2003) permittivity model
  ! -----------------------------------------------
  TYPE :: PermittivityVariables_type
!    PRIVATE
    REAL(fp) :: t, t_sq, t_cu                   ! Temperature in degC
    REAL(fp) :: f1, f2, del1, del2, tau1_k, tau2_k, es_k, e1_k
    REAL(fp) :: ces, ctau1, ctau2, ce1, delta, beta, sigma, S
  END TYPE PermittivityVariables_type

!
END MODULE mod_rttov_fastem4_coef
!
