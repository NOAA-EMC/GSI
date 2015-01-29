 module soil_utils
!$$$  module documentation block
!
! module:    soil_utils
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: collection of routines that perform soil/land
!           related calculations
!
! program history log:
!   2005-05-20  gayno   - initial version
!
! usage: use soil_utils
!
! remarks: routines operate on 1-d arrays because the gfs
!   physics routines operate that way.
!
! attributes:
!   language: fortran 90
!   machine:  ibm sp
!
!$$$

 contains

 subroutine z0_nmm_formulation(vegtype, z0init, z0, ijmdl)
!$$$  subprogram documentation block
!
! subprogram:    z0_nmm_formulation
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: calculate roughness length based on the formulation
!   in nmm.
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call z0_nmm_formulation(vegtype, z0init, z0, ijmdl)
!
!   input argument list: 
!     ijmdl          - model grid dimension
!     vegtype        - vegetation (or landuse) type
!     z0init         - standard deviation of topography
!
!   output argument list: 
!     z0             - roughness length in meters
!
! remarks: vegetation type must be 24 category usgs.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                     :: ij
 integer, intent(in)         :: ijmdl
 integer, intent(in)         :: vegtype(ijmdl)

 real, intent(out)           :: z0(ijmdl)
 real                        :: z0veg(24)      ! in meters
 real, intent(in )           :: z0init(ijmdl) 
 real, parameter             :: z0max = 0.01
 real, parameter             :: z0land = 0.10

 data z0veg / 1.00,  0.07,  0.07,  0.07,  0.07,  0.15, &
              0.08,  0.03,  0.05,  0.86,  0.80,  0.85, &
              2.65,  1.09,  0.80,  0.001, 0.04,  0.05, &
              0.01,  0.04,  0.06,  0.05,  0.03,  0.001 /

 do ij = 1, ijmdl

   z0(ij) = z0init(ij) * z0max + z0land + z0veg(vegtype(ij))

 enddo

 return

 end subroutine z0_nmm_formulation

 subroutine z0_from_vegtype_usgs(vegtype, z0, ijmdl)
!$$$  subprogram documentation block
!
! subprogram:    z0_from_vegtype_usgs
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: calculate roughness length based on the
!   usgs vegetation category
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call z0_from_vegtype_usgs(vegtype, z0, ijmdl)
!
!   input argument list: 
!     ijmdl          - model grid dimension
!     vegtype        - vegetation (or landuse) type
!
!   output argument list: 
!     z0             - roughness length in meters
!
! remarks: vegetation type must be 24 category usgs.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                     :: ij
 integer, intent(in)         :: ijmdl
 integer, intent(in)         :: vegtype(ijmdl)

 real, intent(out)           :: z0(ijmdl)
 real                        :: z0_data(24)  ! in cm

!note: these are the winter values.

 data z0_data /50.,  5.,  5.,  5.,  5., 20.,  &
               10., 10., 10., 15., 50., 50.,  &
               50., 50., 50., .01, 20., 40.,  &
               10., 10., 30., 15.,  5.,  5.  /

 do ij = 1, ijmdl

  z0(ij) = z0_data(vegtype(ij)) * 0.01

 enddo

 return

 end subroutine z0_from_vegtype_usgs

 subroutine calc_soil_parms(smlow, smhigh, maxsmc, bb, &
                            satdk, satpsi, defined_soil,  &
                            refsmc, wltsmc, drysmc)
!$$$  subprogram documentation block
!
! subprogram:    calc_soil_parms
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: calculate some soil type specific parameters
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: calc_soil_parms(smlow, smhigh, maxsmc, bb,
!                        satdk, satpsi, defined_soil, 
!                        refsmc, wltsmc, drysmc)
!
!   input argument list: 
!     bb             - soil 'b' parameter
!     defined_soil   - number of soil types
!     maxsmc         - maximum soil moisture content
!     satdk          - saturated soil hydraulic conductivity
!     satpsi         - saturated soil potential
!     smhigh         - not sure
!     smlow          - not sure
!
!   output argument list: 
!     drysmc         - air dry soil moisture content limit
!     refsmc         - reference soil moisture. (onset of
!                      soil moisture stress)
!     wltsmc         - wilting point soil moisture content
!
! remarks: logic lifted from noah lsm
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer, intent(in)          :: defined_soil
 integer                      :: i

 real, intent(in)             :: bb(defined_soil)
 real, intent(out)            :: drysmc(defined_soil)
 real                         :: f11(defined_soil)
 real, intent(in)             :: maxsmc(defined_soil)
 real, intent(out)            :: refsmc(defined_soil)
 real                         :: refsmc1
 real, intent(in)             :: satdk(defined_soil)
 real                         :: satdw(defined_soil)
 real, intent(in)             :: satpsi(defined_soil)
 real, intent(in)             :: smhigh
 real, intent(in)             :: smlow
 real, intent(out)            :: wltsmc(defined_soil)
 real                         :: wltsmc1

 DO I = 1,DEFINED_SOIL

   if (maxsmc(i) > 0.0) then

   SATDW(I)  = BB(I)*SATDK(I)*(SATPSI(I)/MAXSMC(I))
   F11(I) = ALOG10(SATPSI(I)) + BB(I)*ALOG10(MAXSMC(I)) + 2.0
   REFSMC1 = MAXSMC(I)*(5.79E-9/SATDK(I)) **(1.0/(2.0*BB(I)+3.0))
   REFSMC(I) = REFSMC1 + (MAXSMC(I)-REFSMC1) / SMHIGH
   WLTSMC1 = MAXSMC(I) * (200.0/SATPSI(I))**(-1.0/BB(I))
   WLTSMC(I) = WLTSMC1 - SMLOW * WLTSMC1

!----------------------------------------------------------------------
!  CURRENT VERSION DRYSMC VALUES THAT EQUATE TO WLTSMC.
!  FUTURE VERSION COULD LET DRYSMC BE INDEPENDENTLY SET VIA NAMELIST.
!----------------------------------------------------------------------

   DRYSMC(I) = WLTSMC(I)

   end if

 END DO

 end subroutine calc_soil_parms

 subroutine calc_albedo(lsmask, veg_type, ijmdl, salp,    &
                        snup, num_veg_types,              &
                        snowfree_albedo, mxsnow_albedo,   &
                        snow_liq_equiv, greenfrc, albedo)
!$$$  subprogram documentation block
!
! subprogram:    calc_albedo
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: calculate albedo based on snow water equivalent, 
!   snowfree albedo and maximum snow albedo.
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: calc_albedo(lsmask, veg_type, ijmdl, salp,  
!                    snup, num_veg_types,             
!                    snowfree_albedo, mxsnow_albedo,  
!                    snow_liq_equiv, greenfrc, albedo)
!
!   input argument list: 
!     greenfrc           - greenness fraction (decimal)
!     ijmdl              - dimension of model grid
!     lsmask             - land/sea mask (0-nonland; >0 land)
!     mxsnow_albedo      - maximum snow albedo
!     num_veg_types      - number of vegetation types
!     salp               - snow distribution shape parameter
!     snow_liq_equiv     - snow - liq equiv depth (m)
!     snowfree_albedo    - snow free albedo
!     snup               - veg type dependent snow depth 
!                          threshold where max snow albedo
!                          effect is first attained
!     veg_type           - vegetation type
!
!   output argument list: 
!     albedo             - albedo incl effects of snow  
!
! remarks: logic lifted from noah lsm
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                       :: ij
 integer, intent(in)           :: ijmdl
 integer, intent(in)           :: num_veg_types
 integer, intent(in)           :: veg_type(ijmdl)

 real, intent(inout)           :: albedo(ijmdl) 
 real, intent(in)              :: greenfrc(ijmdl) 
 real, intent(in)              :: lsmask(ijmdl)
 real, intent(in)              :: mxsnow_albedo(ijmdl)
 real                          :: rsnow
 real, intent(in)              :: salp
 real, intent(in)              :: snow_liq_equiv(ijmdl) 
 real                          :: snofac
 real, intent(in)              :: snowfree_albedo(ijmdl)
 real, intent(in)              :: snup(num_veg_types)

 do ij = 1, ijmdl

   if (lsmask(ij) > 0.0) then

     if (snow_liq_equiv(ij) > 0.0) then

       if (snow_liq_equiv(ij) < snup(veg_type(ij))) then
         rsnow = snow_liq_equiv(ij) / snup(veg_type(ij))
         snofac = 1.0 - ( EXP(-salp*RSNOW) - RSNOW*EXP(-salp))

       else
         snofac = 1.0
       end if

       albedo(ij) = snowfree_albedo(ij) +           &
                    (1.0-greenfrc(ij)) * snofac *   &
                    (mxsnow_albedo(ij)-snowfree_albedo(ij))

       if (albedo(ij) > mxsnow_albedo(ij))  then
         albedo(ij) = mxsnow_albedo(ij)
       end if

     else

       albedo(ij) = snowfree_albedo(ij)

     end if

   end if

 enddo

 return

 end subroutine calc_albedo

 subroutine calc_liq_soilm (soil_type, soilm_tot, soil_temp, soilm_liq,   &
                            lsmask, beta, psis, smcmax, max_soil_types,   &
                            ijmdl, nsoil )
!$$$  subprogram documentation block
!
! subprogram:    calc_liq_soilm
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: calculate liquid portion of total soil moisture
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: calc_liq_soilm (soil_type, soilm_tot, soil_temp, soilm_liq, 
!                        lsmask, beta, psis, smcmax, max_soil_types, 
!                        ijmdl, nsoil )
!
!   input argument list: 
!     beta           - soil 'b' parameter
!     ijmdl          - dimension of model grid
!     lsmask         - land mask (0-not land;>0 land)
!     max_soil_types - number of soil type categories
!     nsoil          - number of soil layers
!     psis           - saturated soil potential
!     smcmax         - maximum soil moisture content
!     soil_temp      - soil temperature (k)
!     soil_type      - soil type
!     soilm_tot      - total volumetric soil moisture (liq+frozen)
!
!   output argument list: 
!     soilm_liq      - volumetric soil moisutre (liq portion)
!
! remarks: logic lifted from noah lsm
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 use consts, only               : blim,        &
                                  frz_h20,     &
                                  grav,        &
                                  hlice

 implicit none

 integer                       :: ij, n
 integer, intent(in)           :: ijmdl
 integer, intent(in)           :: max_soil_types
 integer, intent(in)           :: nsoil
 integer, intent(in)           :: soil_type(ijmdl)

 real, intent(in)              :: beta(max_soil_types)
 real                          :: bx
 real                          :: fk
 real, intent(in)              :: lsmask(ijmdl)
 real, intent(in)              :: psis(max_soil_types)
 real, intent(in)              :: smcmax(max_soil_types)
 real, intent(in)              :: soil_temp(ijmdl,nsoil)
 real, intent(inout)           :: soilm_liq(ijmdl,nsoil)
 real, intent(in)              :: soilm_tot(ijmdl,nsoil)

!-----------------------------------------------------------------------
! first guess following explicit solution for Flerchinger Eqn from Koren
! et al, JGR, 1999, Eqn 17 (KCOUNT=0 in FUNCTION FRH2O).
!-----------------------------------------------------------------------

 do ij = 1, ijmdl

   if (lsmask(ij) > 0.0) then

     do n = 1, nsoil

       if (soil_temp(ij,n) < (frz_h20-0.0001)) then

         bx = beta(soil_type(ij))

         if ( beta(soil_type(ij)) .gt. blim ) bx = blim

         fk=(((hlice/(grav*(-psis(soil_type(ij)))))*           &
            ((soil_temp(ij,n)-frz_h20)/soil_temp(ij,n)))**             &
            (-1/bx))*smcmax(soil_type(ij))

         if (fk .lt. 0.02) fk = 0.02

         soilm_liq(ij,n) = min ( fk, soilm_tot(ij,n) )

!-----------------------------------------------------------------------
! now use iterative solution for liquid soil water content using
! FUNCTION FRH2O with the initial guess for SH2O from above explicit
! first guess.
!-----------------------------------------------------------------------

         soilm_liq(ij,n) = frh2O(soil_temp(ij,n),                        &
                           soilm_tot(ij,n), soilm_liq(ij,n),             &
                           smcmax(soil_type(ij)),beta(soil_type(ij)),    &
                           psis(soil_type(ij)))

       else  ! temp above freezing. all moisture is liquid

         soilm_liq(ij,n) = soilm_tot(ij,n)

       end if

     enddo

   end if

 enddo

 return

 end subroutine calc_liq_soilm

 FUNCTION FRH2O (TKELV,SMC,SH2O,SMCMAX,BEXP,PSIS)
!$$$  function documentation block
!
! function:   frh2o
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  calculate supercooled soil moisture
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: x = frh2o (tkelv,smc,sh2o,smcmax,bexp,psis)
!
!   input argument list: 
!     tkelv        - temperature (Kelvin)
!     smc          - total soil moisture content (volumetric)
!     sh2O         - liquid soil moisture content (volumetric)
!     smcmax       - saturation soil moisture content
!     b            - soil type "b" parameter
!     psis         - saturated soil matric potential
!
!   output argument list: 
!     frh2O        - supercooled liquid water content
!
! remarks: stolen from noah lsm code
!
!   CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
!   TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION TO
!   SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
!   (1999, JGR, VOL 104(D16), 19569-19585).
! 
!   NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
!   ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
!   4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
!   1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
!   REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
!   KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
!   LIMIT OF FREEZING POINT TEMPERATURE [AT0.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 use consts,  only           : blim,        &
                               ck,          &
                               frz_h20,     &
                               grav,        &
                               hlice

 IMPLICIT NONE

 INTEGER NLOG
 INTEGER KCOUNT

 REAL BEXP
 REAL BX
 REAL DENOM
 REAL DF
 REAL DSWL
 REAL ERROR
 REAL FK
 REAL FRH2O
 REAL PSIS
 REAL SH2O
 REAL SMC
 REAL SMCMAX
 REAL SWL
 REAL SWLK
 REAL TKELV

 PARAMETER(ERROR = 0.005)

! ----------------------------------------------------------------------
! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
! ----------------------------------------------------------------------

 BX = BEXP
 IF (BEXP .GT. BLIM) BX = BLIM

! ----------------------------------------------------------------------
! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
! ----------------------------------------------------------------------

 NLOG=0
 KCOUNT=0

 IF (CK .NE. 0.0) THEN

! ----------------------------------------------------------------------
! OPTION 1: ITERATED SOLUTION FOR NONZERO CK
! IN KOREN ET AL, JGR, 1999, EQN 17
! ----------------------------------------------------------------------
! INITIAL GUESS FOR SWL (frozen content)
! ----------------------------------------------------------------------

   SWL = SMC-SH2O

! ----------------------------------------------------------------------
! KEEP WITHIN BOUNDS.
! ----------------------------------------------------------------------

   IF (SWL .GT. (SMC-0.02)) SWL = SMC-0.02
   IF (SWL .LT. 0.) SWL = 0.

! ----------------------------------------------------------------------
!  START OF ITERATIONS
! ----------------------------------------------------------------------

   DO WHILE ( (NLOG .LT. 10) .AND. (KCOUNT .EQ. 0) )

     NLOG = NLOG+1
     DF = ALOG(( PSIS*GRAV/HLICE ) * ( ( 1.+CK*SWL )**2. ) *      &
        ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-frz_h20)/TKELV)
     DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
     SWLK = SWL - DF/DENOM

! ----------------------------------------------------------------------
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
! ----------------------------------------------------------------------

     IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
     IF (SWLK .LT. 0.) SWLK = 0.

! ----------------------------------------------------------------------
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
! ----------------------------------------------------------------------

     DSWL = ABS(SWLK-SWL)
     SWL = SWLK

! ----------------------------------------------------------------------
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! ----------------------------------------------------------------------

     IF ( DSWL .LE. ERROR )  THEN
       KCOUNT = KCOUNT+1
     ENDIF

   END DO

! ----------------------------------------------------------------------
!  END OF ITERATIONS
! ----------------------------------------------------------------------
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
! ----------------------------------------------------------------------

   FRH2O = SMC - SWL

! ----------------------------------------------------------------------
! END OPTION 1
! ----------------------------------------------------------------------

 ENDIF

!-----------------------------------------------------------------------
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
! IN KOREN ET AL., JGR, 1999, EQN 17
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
! ----------------------------------------------------------------------

 IF (KCOUNT .EQ. 0) THEN

   FK = (((HLICE/(GRAV*(-PSIS)))*                  &
        ((TKELV-frz_h20)/TKELV))**(-1/BX))*SMCMAX

   IF (FK .LT. 0.02) FK = 0.02

   FRH2O = MIN (FK, SMC)

 ENDIF

 RETURN

 END function frh2o

 subroutine adjust_soilt_for_orog(soilt, orog_input, orog_output,  &
                                  lsmask_output, ijmdl, nsoil)
!$$$  subprogram documentation block
!
! subprogram:   adjust_soilt_for_orog
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  adjust soil temperature for differences in terrain
!   height between two grids of the same dimension.
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call adjust_soilt_for_orog(soilt, orog_input, orog_output,  
!                                   lsmask_output, ijmdl, nsoil)
!
!   input argument list: 
!     ijmdl          - model grid dimension
!     lsmask_output  - land mask of output grid (0-no land;>0 some land)
!     nsoil          - number of soil layers
!     orog_input     - terrain height of input grid (m)
!     orog_output    - terrain height of output grid (m)
!     soilt          - soil temperature (K)
!
!   output argument list: 
!     soilt          - soil temperature adjusted to new terrain
!
! remarks: adjustment is based on standard atmospheric lapse rate
!   and occurs only at points where the terrain difference exceeds
!   100 meters.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 use consts, only           : lapse_rate

 implicit none

 integer                   :: ij
 integer, intent(in)       :: ijmdl
 integer, intent(in)       :: nsoil
 
 real, intent(in)          :: lsmask_output(ijmdl)
 real, intent(in)          :: orog_input(ijmdl)
 real, intent(in)          :: orog_output(ijmdl)
 real, intent(inout)       :: soilt(ijmdl,nsoil)

 do ij = 1, ijmdl

    if (lsmask_output(ij) > 0.0   .and.       &
        abs(orog_input(ij)-orog_output(ij)) > 100.0) then

      soilt(ij,:) = soilt(ij,:)  +    &
                   ((orog_input(ij) - orog_output(ij)) * lapse_rate)
  
    end if

 enddo

 return

 end subroutine adjust_soilt_for_orog

!------------------------------------------------------------------------
! rescale soil moisture for changing soil type.
!
!  works on 2-d arrays.   want to get rid of this version someday.
!
!------------------------------------------------------------------------

 subroutine rescale_soilm_2d(smci, smc, isltpki, isltpk,             &
                          smcdry, smcdryt, smcwilt, smcwiltt,     &
                          smcref, smcreft, smcmax, smcmaxt,       &
                          lsmask, shdfac, imdl, jmdl, nsoil,      &
                          max_soil_types)

!------------------------------------------------------------------------
!SMCDRY,SMCWILT,SMCREF,SMCMAX
!=dry,wilt, ref, max(sat) input (old) values
!
!SMCDRYT,SMCWILTT,SMCREFT,SMCMAXT
!=dry,wilt, ref, max(sat) target (new) values
!
!ISLTPKI = input (old) soil type, i.e. 1-9 Zobler
!ISLTPK = target (new) soil type, i.e. 1-19 STATSGO+NCAR
!
!SMCI = input (old) soil moisture
!SMC  = target (new) soil moisture
!
!SHDFAC = green vegetation fraction
!SMCDIR = top soil layer rescaled soil moisture (non-veg part, 1-SHDFAC)
!SMCTRA = top soil layer rescaled soil moisture (veg part, SHDFAC)
!
!algorithm (implied within I,J do loops):
! RESCALE TOP SOIL LAYER TOTAL SOIL MOISTURE:
!   DIRECT EVAPORATION PART:
!     RESCALE BETWEEN SMCDRY AND SMCMAX FOR DIRECT SOIL EVAPORATION
!   TRANSPIRATION PART:
!     RESCALE BETWEEN SMCWLT AND SMCREF FOR SMCWLT<SMC<SMCREF, OR
!     RESCALE BETWEEN SMCREF AND SMCMAX FOR SMCREF<SMC<SMCMAX
!-----------------------------------------------------------------------

 implicit none

 integer                   :: i
 integer, intent(in)       :: imdl
 integer, intent(in)       :: isltpki(imdl,jmdl)
 integer, intent(in)       :: isltpk(imdl,jmdl)
 integer                   :: j
 integer, intent(in)       :: jmdl
 integer                   :: k
 integer, intent(in)       :: max_soil_types
 integer, intent(in)       :: nsoil

 real                      :: f1
 real                      :: fn
 real, intent(in)          :: lsmask(imdl,jmdl)
 real, intent(in)          :: shdfac(imdl,jmdl)
 real, intent(out)         :: smc(imdl,jmdl,nsoil)
 real                      :: smcdir
 real, intent(in)          :: smcdry(max_soil_types)
 real, intent(in)          :: smcdryt(max_soil_types)
 real, intent(in)          :: smci(imdl,jmdl,nsoil)
 real, intent(in)          :: smcmax(max_soil_types)
 real, intent(in)          :: smcmaxt(max_soil_types)
 real, intent(in)          :: smcref(max_soil_types)
 real, intent(in)          :: smcreft(max_soil_types)
 real                      :: smctra
 real, intent(in)          :: smcwilt(max_soil_types)
 real, intent(in)          :: smcwiltt(max_soil_types)

 do j = 1, jmdl
   do i = 1, imdl
 
! even if the soil type is not changing, 
! the algorithm will do bounds checking.  can't always
! assume your input data is good.

     if (lsmask(i,j) > 0.0) then

! DIRECT EVAPORATION PART:
        F1=(SMCI(I,J,1)-SMCDRY(ISLTPKI(I,J)))/          &
          (SMCMAX(ISLTPKI(I,J))-SMCDRY(ISLTPKI(I,J)))
        SMCDIR=SMCDRYT(ISLTPK(I,J)) + F1*        &
          (SMCMAXT(ISLTPK(I,J))-SMCDRYT(ISLTPK(I,J)))
! TRANSPIRATION PART:
        IF (SMCI(I,J,1) .LT. SMCREF(ISLTPKI(I,J))) THEN
          F1=(SMCI(I,J,1)-SMCWILT(ISLTPKI(I,J)))/       &
            (SMCREF(ISLTPKI(I,J))-SMCWILT(ISLTPKI(I,J)))
          SMCTRA=SMCWILTT(ISLTPK(I,J)) + F1*     &
            (SMCREFT(ISLTPK(I,J))-SMCWILTT(ISLTPK(I,J)))
        ELSE
          F1=(SMCI(I,J,1)-SMCREF(ISLTPKI(I,J)))/        &
            (SMCMAX(ISLTPKI(I,J))-SMCREF(ISLTPKI(I,J)))
          SMCTRA=SMCREFT(ISLTPK(I,J)) + F1*      &
            (SMCMAXT(ISLTPK(I,J))-SMCREFT(ISLTPK(I,J)))
        ENDIF
! WEIGHT BY GREEN VEGETATION FRACTION:
        SMC(I,J,1)=(1.0-SHDFAC(I,J))*SMCDIR + SHDFAC(I,J)*SMCTRA
! LIMIT CHECKS ON SOIL MOISTURE:
        SMC(I,J,1)=MIN(SMC(I,J,1),SMCMAXT(ISLTPK(I,J)))
        SMC(I,J,1)=MAX(SMCDRYT(ISLTPK(I,J)),SMC(I,J,1))
!-----------------------------------------------------------------------
! RESCALE BOTTOM 3 SOIL LAYERS TOTAL SOIL MOISTURE:
!   RESCALE BETWEEN SMCWLT AND SMCREF FOR SMCWLT<SMC<SMCREF, OR
!   RESCALE BETWEEN SMCREF AND SMCMAX FOR SMCREF<SMC<SMCMAX
!-----------------------------------------------------------------------
        DO K=2,NSOIL
          IF (SMCI(I,J,K) .LT. SMCREF(ISLTPKI(I,J))) THEN
            FN=(SMCI(I,J,K)-SMCWILT(ISLTPKI(I,J)))/        &
              (SMCREF(ISLTPKI(I,J))-SMCWILT(ISLTPKI(I,J)))
            SMC(I,J,K)=SMCWILTT(ISLTPK(I,J)) + FN*         &
              (SMCREFT(ISLTPK(I,J))-SMCWILTT(ISLTPK(I,J)))
          ELSE
            FN=(SMCI(I,J,K)-SMCREF(ISLTPKI(I,J)))/         &
              (SMCMAX(ISLTPKI(I,J))-SMCREF(ISLTPKI(I,J)))
            SMC(I,J,K)=SMCREFT(ISLTPK(I,J)) + FN*          &
              (SMCMAXT(ISLTPK(I,J))-SMCREFT(ISLTPK(I,J)))
          ENDIF
! LIMIT CHECKS ON SOIL MOISTURE:
          SMC(I,J,K)=MIN(SMC(I,J,K),SMCMAXT(ISLTPK(I,J)))
          SMC(I,J,K)=MAX(SMCWILTT(ISLTPK(I,J)),SMC(I,J,K))
        ENDDO

     endif

   enddo
 enddo

 return

 end subroutine rescale_soilm_2d

 subroutine rescale_soilm(smci, smc, isltpki, isltpk,             &
                          smcdry, smcdryt, smcwilt, smcwiltt,     &
                          smcref, smcreft, smcmax, smcmaxt,       &
                          lsmask, shdfac, ijmdl, nsoil,      &
                          max_soil_types)
!$$$  subprogram documentation block
!
! subprogram:   rescale_soilm
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  rescale total soil moisture for a change in soil type
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call rescale_soilm(smci, smc, isltpki, isltpk,           
!                           smcdry, smcdryt, smcwilt, smcwiltt,   
!                           smcref, smcreft, smcmax, smcmaxt,     
!                           lsmask, shdfac, ijmdl, nsoil,     
!                           max_soil_types)
!
!   input argument list: 
!     ijmdl          - model grid dimension
!     isltpki        - input soil type
!     isltpk         - target soil type
!     lsmask         - land mask of grid (0-no land;>0 some land)
!     max_soil_types - number of soil categories
!     nsoil          - number of soil layers
!     shdfac         - greenness fraction
!     smcdry         - input air dry value of soil moisture
!     smcdryt        - target air dry value of soil moisture
!     smci           - input volumetric soil moisture
!     smcmax         - input max value of soil moisture
!     smcmaxt        - target max value of soil moisture
!     smcref         - input reference soil moisture value
!     smcreft        - target reference soil moisture value
!     smcwilt        - input wilting point value of soil moisture
!     smcwiltt       - target wilting point value of soil moisture
!
!   output argument list: 
!     smc            - output volumetric soil moisture
!
! remarks: based on utility from mike ek.
!
!   rescale top soil layer total soil moisture:
!   direct evaporation part:
!     rescale between smcdry and smcmax for direct soil evaporation
!   transpiration part:
!     rescale between smcwlt and smcref for smcwlt<smc<smcref, or
!     rescale between smcref and smcmax for smcref<smc<smcmax
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                   :: ij
 integer, intent(in)       :: ijmdl
 integer, intent(in)       :: isltpki(ijmdl)
 integer, intent(in)       :: isltpk(ijmdl)
 integer                   :: k
 integer, intent(in)       :: max_soil_types
 integer, intent(in)       :: nsoil

 real                      :: f1
 real                      :: fn
 real, intent(in)          :: lsmask(ijmdl)
 real, intent(in)          :: shdfac(ijmdl)
 real, intent(out)         :: smc(ijmdl,nsoil)
 real                      :: smcdir
 real, intent(in)          :: smcdry(max_soil_types)
 real, intent(in)          :: smcdryt(max_soil_types)
 real, intent(in)          :: smci(ijmdl,nsoil)
 real, intent(in)          :: smcmax(max_soil_types)
 real, intent(in)          :: smcmaxt(max_soil_types)
 real, intent(in)          :: smcref(max_soil_types)
 real, intent(in)          :: smcreft(max_soil_types)
 real                      :: smctra
 real, intent(in)          :: smcwilt(max_soil_types)
 real, intent(in)          :: smcwiltt(max_soil_types)

 do ij = 1, ijmdl
 
! even if the soil type is not changing, 
! the algorithm will do bounds checking.  can't always
! assume your input data is good.

     if (lsmask(ij) > 0.0) then

! DIRECT EVAPORATION PART:
        F1=(SMCI(IJ,1)-SMCDRY(ISLTPKI(IJ)))/          &
          (SMCMAX(ISLTPKI(IJ))-SMCDRY(ISLTPKI(IJ)))
        SMCDIR=SMCDRYT(ISLTPK(IJ)) + F1*        &
          (SMCMAXT(ISLTPK(IJ))-SMCDRYT(ISLTPK(IJ)))
! TRANSPIRATION PART:
        IF (SMCI(IJ,1) .LT. SMCREF(ISLTPKI(IJ))) THEN
          F1=(SMCI(IJ,1)-SMCWILT(ISLTPKI(IJ)))/       &
            (SMCREF(ISLTPKI(IJ))-SMCWILT(ISLTPKI(IJ)))
          SMCTRA=SMCWILTT(ISLTPK(IJ)) + F1*     &
            (SMCREFT(ISLTPK(IJ))-SMCWILTT(ISLTPK(IJ)))
        ELSE
          F1=(SMCI(IJ,1)-SMCREF(ISLTPKI(IJ)))/        &
            (SMCMAX(ISLTPKI(IJ))-SMCREF(ISLTPKI(IJ)))
          SMCTRA=SMCREFT(ISLTPK(IJ)) + F1*      &
            (SMCMAXT(ISLTPK(IJ))-SMCREFT(ISLTPK(IJ)))
        ENDIF
! WEIGHT BY GREEN VEGETATION FRACTION:
        SMC(IJ,1)=(1.0-SHDFAC(IJ))*SMCDIR + SHDFAC(IJ)*SMCTRA
! LIMIT CHECKS ON SOIL MOISTURE:
        SMC(IJ,1)=MIN(SMC(IJ,1),SMCMAXT(ISLTPK(IJ)))
        SMC(IJ,1)=MAX(SMCDRYT(ISLTPK(IJ)),SMC(IJ,1))
!-----------------------------------------------------------------------
! RESCALE BOTTOM 3 SOIL LAYERS TOTAL SOIL MOISTURE:
!   RESCALE BETWEEN SMCWLT AND SMCREF FOR SMCWLT<SMC<SMCREF, OR
!   RESCALE BETWEEN SMCREF AND SMCMAX FOR SMCREF<SMC<SMCMAX
!-----------------------------------------------------------------------
        DO K=2,NSOIL
          IF (SMCI(IJ,K) .LT. SMCREF(ISLTPKI(IJ))) THEN
            FN=(SMCI(IJ,K)-SMCWILT(ISLTPKI(IJ)))/        &
              (SMCREF(ISLTPKI(IJ))-SMCWILT(ISLTPKI(IJ)))
            SMC(IJ,K)=SMCWILTT(ISLTPK(IJ)) + FN*         &
              (SMCREFT(ISLTPK(IJ))-SMCWILTT(ISLTPK(IJ)))
          ELSE
            FN=(SMCI(IJ,K)-SMCREF(ISLTPKI(IJ)))/         &
              (SMCMAX(ISLTPKI(IJ))-SMCREF(ISLTPKI(IJ)))
            SMC(IJ,K)=SMCREFT(ISLTPK(IJ)) + FN*          &
              (SMCMAXT(ISLTPK(IJ))-SMCREFT(ISLTPK(IJ)))
          ENDIF
! LIMIT CHECKS ON SOIL MOISTURE:
          SMC(IJ,K)=MIN(SMC(IJ,K),SMCMAXT(ISLTPK(IJ)))
          SMC(IJ,K)=MAX(SMCWILTT(ISLTPK(IJ)),SMC(IJ,K))
        ENDDO

     endif

 enddo

 return

 end subroutine rescale_soilm

 end module soil_utils