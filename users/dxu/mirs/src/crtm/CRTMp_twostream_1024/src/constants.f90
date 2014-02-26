module constants
!$$$   module documentation block
!                .      .    .                                       .
! module:    constants
!   prgmmr: treadon          org: np23                date: 2003-09-25
!
! abstract:  This module contains the definition of various constants
!            used in the gsi code
!
! program history log:
!   2003-09-25  treadon - original code
!   2004-03-02  treadon - allow global and regional constants to differ
!   2004-06-16  treadon - update documentation
!
! Subroutines Included:
!   sub init_constants  - compute derived constants, set regional/global constants
!
! Variable Definitions:
!   see below
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use type_kinds, only: single,fp_kind,double
  implicit none

! Declare constants
  integer izero
  real(fp_kind) rearth,grav,omega,rd,rv,cp,cv,cvap,cliq
  real(fp_kind) csol,hvap,hfus,psat,t0c,ttp,jcal
  real(fp_kind) fv,deg2rad,rad2deg,pi,tiny1
  real(fp_kind) ozcon,rozcon,tpwcon,rd_over_g,rd_over_cp,g_over_rd
  real(fp_kind) amsua_clw_d1,amsua_clw_d2,constoz,zero,one
  real(fp_kind) rearth_equator,stndrd_atmos_ps
  real(fp_kind) semi_major_axis,semi_minor_axis,n_a,n_b
  real(fp_kind) eccentricity,grav_polar,grav_ratio
  real(fp_kind) grav_equator,earth_omega,grav_constant
  real(fp_kind) flattening,eccentricity_linear,somigliana
  real(fp_kind) dldt,dldti,hsub,psatk,tmix,xa,xai,xb,xbi
  real(fp_kind) eps,epsm1,omeps
  real(fp_kind) elocp,cpr,el2orc,cclimit,climit,epsq
  real(fp_kind) pcpeff0,pcpeff1,pcpeff2,pcpeff3,rcp,c0,delta
  real(fp_kind) h1000,factor1,factor2,rhcbot,rhctop,dx_max,dx_min,dx_inv
  real(fp_kind) h300,half,cmr,cws,ke2,row,rrow
  real(single) ones
  real(double) oned


! Define constants common to global and regional applications
!           name     value                  description                     units
!           ----     -----                  -----------                     -----
  parameter(rearth_equator= 6.37813662e6_fp_kind) ! equatorial earth radius (m)
  parameter(omega  = 7.2921e-5_fp_kind)  !  angular velocity of earth       (1/s)
  parameter(cp     = 1.0046e+3_fp_kind)  !  specific heat of air @pressure  (J/kg/K)
  parameter(cvap   = 1.8460e+3_fp_kind)  !  specific heat of h2o vapor      (J/kg/K)
  parameter(csol   = 2.1060e+3_fp_kind)  !  specific heat of solid h2o (ice)(J/kg/K)
  parameter(hvap   = 2.5000e+6_fp_kind)  !  latent heat of h2o condensation (J/kg)
  parameter(hfus   = 3.3358e+5_fp_kind)  !  latent heat of h2o fusion       (J/kg)
  parameter(psat   = 6.1078e+2_fp_kind)  !  pressure at h2o triple point    (Pa)
  parameter(t0c    = 2.7315e+2_fp_kind)  !  temperature at zero celsius     (K)
  parameter(ttp    = 2.7316e+2_fp_kind)  !  temperature at h2o triple point (K)
  parameter(jcal   = 4.1855e+0_fp_kind)  !  joules per calorie              ()
  parameter(stndrd_atmos_ps = 1013.25e3) ! 1976 US standard atmosphere ps   (Pa)

! Numeric constants
  parameter(izero  = 0)
  parameter(zero   = 0.0_fp_kind)
  parameter(ones   = 1.0_single)
  parameter(one    = 1.0_fp_kind)
  parameter(oned   = 1.0_double)
  parameter(tiny1   = 1.e-12_fp_kind)

! Constants for gps refractivity
  parameter(n_a=77.6) !K/mb
  parameter(n_b=3.73e+5) !K^2/mb

! Parameters below from WGS-84 model software inside GPS receivers.
  parameter(semi_major_axis = 6378.1370e3_fp_kind)    !                     (m)
  parameter(semi_minor_axis = 6356.7523142e3_fp_kind) !                     (m)
  parameter(grav_polar = 9.8321849378_fp_kind)        !                     (m/s2)
  parameter(grav_equator = 9.7803253359_fp_kind)      !                     (m/s2) 
  parameter(earth_omega = 7.292115e-5_fp_kind)        !                     (rad/s)
  parameter(grav_constant = 3.986004418e14_fp_kind)   !                     (m3/s2)

! Derived geophysical constants
  parameter(flattening = (semi_major_axis-semi_minor_axis)/semi_major_axis)!() 
  parameter(somigliana = &
       (semi_minor_axis/semi_major_axis) * (grav_polar/grav_equator) - one)!()
  parameter(grav_ratio = (earth_omega*earth_omega * &
       semi_major_axis*semi_major_axis * semi_minor_axis) / grav_constant) !()

! Derived thermodynamic constants
  parameter ( dldti = cvap-csol )
  parameter ( hsub = hvap+hfus )
  parameter ( psatk = psat*0.001_fp_kind )
  parameter ( tmix = ttp-20._fp_kind )
  parameter ( elocp = hvap/cp )
  parameter ( rcp  = one/cp )

! Constants used in GFS moist physics
  parameter ( h300 = 300._fp_kind )
  parameter ( half = 0.5_fp_kind )
  parameter ( cclimit = 0.001_fp_kind )
  parameter ( climit = 1.e-20_fp_kind)
  parameter ( epsq = 2.e-12_fp_kind )
  parameter ( h1000 = 1000.0_fp_kind)
  parameter ( rhcbot=0.85 )
  parameter ( rhctop=0.85 )
  parameter ( dx_max=-8.8818363 )
  parameter ( dx_min=-5.2574954 )
  parameter ( dx_inv=one/(dx_max-dx_min) )
  parameter ( c0=0.002_fp_kind )
  parameter ( delta=0.6077338_fp_kind )
  parameter ( pcpeff0=1.591_fp_kind )
  parameter ( pcpeff1=-0.639_fp_kind )
  parameter ( pcpeff2=0.0953_fp_kind )
  parameter ( pcpeff3=-0.00496_fp_kind )
  parameter ( cmr = one/0.0003_fp_kind )
  parameter ( cws = 0.025_fp_kind )
  parameter ( ke2 = 0.00002_fp_kind )
  parameter ( row = 1000._fp_kind )
  parameter ( rrow = one/row )

! Constant used to process ozone
  parameter ( constoz = 604229.0_fp_kind)

! Constants used in cloud liquid water correction for AMSU-A
! brightness temperatures
  parameter ( amsua_clw_d1 = 0.754_fp_kind )
  parameter ( amsua_clw_d2 = -2.265_fp_kind )

contains
  subroutine init_constants(regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_constants     set derived, regional, global constants
!     prgmmr:    treadon          org: np23           date: 2004-03-02
!
! abstract:  This routine sets derived constants as well as those that
!            are specific to regional or global applications of the
!            gsi
!
! program history log:
!   2004-03-02  treadon
!   2004-06-16  treadon, documentation
!
!   input argument list:
!     regional - if .true., set regional gsi constants;
!                otherwise (.false.), use global constants
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    logical regional
    real(fp_kind) reradius,g,r_d,r_v,cliq_wrf

!   Trigonometric constants
    pi      = acos(-one)
    deg2rad = pi/180.0_fp_kind
    rad2deg = one/deg2rad

!   Geophysical parameters used in conversion of geopotential to
!   geometric height
    eccentricity_linear = sqrt(semi_major_axis**2 - semi_minor_axis**2)
    eccentricity = eccentricity_linear / semi_major_axis

!   Define regional constants here
    if (regional) then

!      Name given to WRF constants
       reradius = 1./6370.e03
       g        = 9.81
       r_d      = 287.04
       r_v      = 461.6
       cliq_wrf = 4190.

!      Transfer WRF constants into unified GSI constants
       rearth = 1./reradius
       grav   = g
       rd     = r_d
       rv     = r_v
       cv     = cp-r_d
       cliq   = cliq_wrf

!   Define global constants here
    else
       rearth = 6.3712e+6_fp_kind
       grav   = 9.80665e+0_fp_kind
       rd     = 2.8705e+2_fp_kind
       rv     = 4.6150e+2_fp_kind
       cv     = 7.1760e+2_fp_kind
       cliq   = 4.1855e+3_fp_kind
    endif


!   Now define derived constants which depend on constants
!   which differ between global and regional applications.

!   Constants related to ozone assimilation
    ozcon = grav*21.4e-9_fp_kind
    rozcon= one/ozcon

!   Constant used in vertical integral for precipitable water
    tpwcon = 100.0_fp_kind/grav

!   Derived atmospheric constants
    fv         = rv/rd-one    ! used in virtual temperature equation 
    dldt       = cvap-cliq
    xa         = -(dldt/rv)
    xai        = -(dldti/rv)
    xb         = xa+hvap/(rv*ttp)
    xbi        = xai+hsub/(rv*ttp)
    eps        = rd/rv
    epsm1      = rd/rv-one
    omeps      = one-eps
    factor1    = (cvap-cliq)/rv
    factor2    = hvap/rv-factor1*t0c
    cpr        = cp*rd
    el2orc     = hvap*hvap/(rv*cp)
    rd_over_g  = rd/grav
    rd_over_cp = rd/cp
    g_over_rd  = grav/rd

    return
  end subroutine init_constants

end module constants
