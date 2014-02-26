!
MODULE MOD_SURFACE_COEF

   USE Type_Kinds
   PRIVATE

   INTEGER, PARAMETER :: surfacetype=7
   REAL( fp_kind ), PUBLIC :: avn_soil_mois,avn_snow_depth,avn_soil_t,avn_veg_frac
   REAL( fp_kind ), PUBLIC ::  avn_canopy_water,avn_veg_type,avn_soil_type
   REAL, PUBLIC ::  surface_bta(4),surface_btb(2) 
   INTEGER, PUBLIC ::  snow_type

   REAL, PUBLIC,SAVE,dimension(surfacetype) :: perm_static1
   REAL, PUBLIC,SAVE,dimension(surfacetype) :: perm_infinite1
   REAL, PUBLIC,SAVE,dimension(surfacetype) :: perm_relaxation1
   REAL, PUBLIC,SAVE,dimension(surfacetype) :: rough_small1
   REAL, PUBLIC,SAVE,dimension(surfacetype) :: rough_large1 
   REAL, PUBLIC,SAVE,dimension(140) :: emc 
   INTEGER, PUBLIC,SAVE,dimension(24) :: SINDEX
!
!  land surface model reference:
!  Hewison, T., 2001: Airborne measurements of forest and agricultural
!  land surface emissivity at millimeter wavelengths,
!  IEEE Trans. Geosci. Remote Sensing, 39, 393-400.
!  water at 0 celsius degree
   DATA perm_static1/88.045,40.8,2.64,2.22,2.20,1.57,1.66/
   DATA perm_infinite1/4.9,3.03,2.25,1.64,1.94,1.22,1.01/
   DATA perm_relaxation1/9.00171,0.44,63.6,51.9,67.4,87.3,163.0/
   DATA rough_small1/0.1,0.1,0.1,0.1,0.1,0.1,0.1/
   DATA rough_large1/0.3,0.3,0.3,0.3,0.3,0.3,0.3/
!  1. Water                2. Old snow                   3. Fresh snow
!  4. Compacted soil       5. Tilled soil                6. Sand
!  7. Rock                 8. Irrigated low vegetation   9. Meadow grass
! 10. Scrub               11. Broadleaf forest          12. Pine forest
! 13. Tundra              14. Grass soil                15. Roadleaf-pine forest
! 16. Grass scrub         17. Oil grass                 18. Urban concrete
! 19. Pine brush          20. Broadleaf brush           21. Wet soil
! 22. Scrub soil          23. Broadleaf 70-pine 30      24. New ice

! surface    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
 DATA SINDEX/1, 2, 2, 3, 3, 3, 3, 5, 5, 5, 7, 7, 5, 5, 7, 5, 5, 3, 5, 5, 5, 5, 5, 2/

!  water     140 coefficients
!    [1-3]: Temperature polynomial coefficients for Tau1 - Lamkaouchi (1997)
!    [4-7]: Temperature polynomial coefficients for Tau2 - Lamkaouchi (1997)
!   [8-11]: Temperature polynomial coefficients for Del1 - Lamkaouchi (1997)
!  [12-15]: Temperature polynomial coefficients for Del2 - Lamkaouchi (1997)
!  [16-17]: Temperature polynomial coefficients for static permittivity - Lamkaouchi (1997)
!  [18-19]: Temperature polynomial coefficients for infinite freq. permittivity - Lamkaouchi (1997)
!     [20]: Stored value of Pi  - temporary, use RTTOV pi when available.
!     [21]: Scaling factor for small scale correction - see English (2000)
!     [22]: First coefficient in Monahan foam model (neutral stability)  - see English (2000)
!     [23]: Power coefficient in Monahan foam model (neutral stability)  - see English (2000)
!  [24-41]: Horizontal polarisation coefficients Fastem version 1
!  [42-59]: Vertical polarisation coefficients Fastem version 1
!  [60-77]: Horizontal polarisation coefficients Fastem version 1
!  [78-95]: Vertical polarisation coefficients Fastem version 1
! Remainder only when angle parameterisation used
! [96-116]: Horizontal polarisation effective angle correction
![117-137]: Vertical polarisation effective angle correction
!    [138]: Scaling factor for Cox and Munk total roughness
!    [139]: Gradient of slope variance with frequency
!    [140]: Slope variance at Frequency=zero
   DATA emc/0.175300E+02,-0.617700E+00, 0.894800E-02, 0.318400E+01, 0.191900E-01, &
 -0.108700E-01, 0.258200E-03, 0.684000E+02,-0.406400E+00, 0.228300E-01, &
 -0.530600E-03, 0.476300E+01, 0.154100E+00,-0.337200E-01, 0.844300E-03, &
  0.782900E+02,-0.434600E-02, 0.531300E+01,-0.114800E-01, 0.314200E+01, &
 -0.100000E+01, 0.195000E-04, 0.255000E+01,-0.182400E+01,-0.434800E-02, &
  0.646300E-04, 0.278600E+01, 0.878500E-02,-0.102700E-03,-0.101900E+01, &
 -0.426800E-02, 0.396500E-04, 0.730700E-01, 0.261800E-02,-0.950500E-05, &
  0.295300E-03, 0.443700E-05,-0.140200E-07,-0.717900E-01,-0.267900E-02, &
  0.949600E-05,-0.334700E+00, 0.951700E-02, 0.964400E-05, 0.470800E+00, &
 -0.149000E-01,-0.987500E-05,-0.142700E+00, 0.565400E-02, 0.118900E-05, &
 -0.137800E+00,-0.217000E-02, 0.793100E-05, 0.237800E-04, 0.869500E-06, &
  0.282500E-08, 0.138800E+00, 0.209500E-02,-0.797900E-05,-0.637200E+01, &
  0.253900E-01, 0.357600E-04, 0.942900E+01,-0.332800E-01,-0.647700E-04, &
 -0.329300E+01, 0.965400E-02, 0.281600E-04, 0.252700E+00, 0.343900E-02, &
 -0.156400E-04,-0.156700E-03, 0.139500E-04,-0.407600E-07,-0.141300E+00, &
 -0.356600E-02, 0.142900E-04,-0.240700E+01,-0.563900E-01, 0.325200E-03, &
  0.296000E+01, 0.704700E-01,-0.426400E-03,-0.751300E+00,-0.191900E-01, &
  0.125900E-03,-0.288200E+00,-0.102700E-02, 0.226700E-05,-0.119100E-02, &
 -0.263200E-04, 0.114600E-06, 0.406300E+00, 0.200000E-02,-0.781600E-05, &
 -0.675700E-01, 0.214600E+00,-0.363000E-02, 0.636700E+01, 0.900600E+00, &
 -0.524900E+00,-0.370900E+01,-0.143300E+01, 0.397500E+00, 0.823100E-01, &
 -0.256000E+00, 0.552000E-02, 0.208000E+01, 0.244900E+01,-0.456400E+00, &
 -0.224900E-01, 0.616900E-01,-0.344000E-02,-0.507600E+01,-0.360700E+01, &
  0.118800E+01, 0.124900E+00, 0.121300E+00, 0.714000E-02, 0.736600E+01, &
 -0.114100E+00,-0.272900E+00,-0.504300E+01,-0.336500E+00, 0.161300E+00, &
 -0.154300E+00,-0.141100E+00,-0.809000E-02, 0.395300E+01, 0.958600E+00, &
 -0.159100E+00, 0.368500E-01, 0.307100E-01, 0.810000E-03,-0.620000E+01, &
 -0.172600E+01, 0.641400E+00, 0.100000E+01, 0.200000E-01, 0.300000E+00/


END MODULE MOD_SURFACE_COEF
