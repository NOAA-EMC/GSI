subroutine landem(thetar,freq,mv,veg_frac,veg_tp,soil_tp, &
     t_soil,t_skin,snow_depth,esh,esv)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    landem      noaa/nesdis emissivity model over land/ice
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: noaa/nesdis emissivity model to compute microwave emissivity over
!       various land surface conditions (land/ice/snow)
!
!    reference: weng, f, b. yan, and n. grody, 2001: 
!      "development of mircowave land emissivity model and data sets 
!       for satellite data assimilation and remote sensing applications 
!            (jgr, revised) 
!
!   version: beta 
!
! program history log:
!
! input argument list:
!       theta       -  local zenith angle (0 - 60.0)
!       freq        -  frequency in ghz	( 0 - 90.0) 
!       mv          -  volumetric moisture content in soil (0.0 - 1.0)	 (gdas)
!       veg_frac    -  vegetation fraction (0 - 1.0)      (gdas)
!       veg_tp      -  vegetation type		     (gdas, not used)
!                       1: broadleave evergreen trees
!                       2: broadleave deciduous trees
!                       3: broad & needle mixed forest
!                       4: needleleave evergreen trees
!                       5: needleleave deciduous trees
!                       6: broadleave tree with groundcover (savana)
!                       7: groundcover only (perenial groundcover)
!                       8: broadleave shrubs with perenial groundcover
!                       9: broadleave shrubs with bare soil
!                       10: dwarf trees & shrubs with bare soil
!                       11: bare soil'
!                       12: cultivations (use paramater 7)
!                       13: glacial		
!
!       soil_tp     -  soil type                 (gdas, not used)
!                       1: loamy sand (coarse)
!                       2: silty clayloam (medium)
!                       3: light clay (fine)
!                       4: sand loam (coarse-medium)
!                       5: sandy clay (coarse-fine)
!                       6: clay loam (medium-fine)
!                       7: sandy clay loam (coarse-med-fine)
!                       8: loam (organic)
!                       9: ice (use loamy sand property)
!
!
!       t_soil      -  soil temperature (k)	 (gdas)
!       t_skin      -  scattering layer temperature (k)			 (gdas)
!       snow_depth  -  scatter medium depth (mm?) 			(gdas)
!
!
! output argument list:
!       esh         -  emissivity for horizontal polarization
!       esv         -  emissivity for vertical polarization
!
! important internal variables:
!
!       rhob        -  bulk volume density of the soil (1.18-1.12)
!       rhos        -  density of the solids (2.65 g.cm^3 for solid soil material)
!       sand        -  sand fraction (sand + clay = 1.0)
!       clay        -  clay fraction 
!       lai         -  leaf area index (eg. lai = 4.0 for corn leaves)
!       sigma       -  surface roughness formed between medium 1 and 2, 
!                      expressed as he standard deviation of roughtness height (mm)
!       leaf_thick  --  leaf thickness (mm)
!       rad         -  radius of dense medium scatterers (mm)
!       va          -  fraction volume of dense medium scatterers(0.0 - 1.0)
!       ep          -  dielectric constant of ice or sand particles, complex value
!                               (e.g, 3.0+i0.0)
!
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind
  use constants
  implicit none

  real(fp_kind) rhob,rhos,sand,clay
  parameter(rhob = 1.18_fp_kind, rhos = 2.65_fp_kind, &
       sand = 0.8_fp_kind, clay = 0.2_fp_kind)

  real(fp_kind) theta,thetar,freq,mv,mge,veg_frac,veg_tp,soil_tp,t_soil,t_skin,snow_depth
  real(fp_kind) b,theta_i,theta_t,mu,r12_h,r12_v,r21_h,r21_v,r23_h,r23_v, &
      t21_v,t21_h,t12_v,t12_h,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,esh,esv,&
      lai,leaf_thick,rad,sigma,va,ep_real,ep_imag

  complex esoil, eveg, esnow, eair
  logical regional

   regional = .true.
   call init_constants(regional)
   theta=thetar * deg2rad

  eair = cmplx(one,-zero)
  if (snow_depth .gt.zero) then 
                              ! ice dielectric constant 
     ep_real = 3.2_fp_kind
     ep_imag = -0.0005_fp_kind
     sigma = one

      IF(snow_depth > 1000.0 ) snow_depth = 1000.0

     va = 0.4_fp_kind + 0.0004_fp_kind*snow_depth
     rad = one + 0.005_fp_kind*snow_depth
     call snow_diel(freq, ep_real, ep_imag, rad, va, esnow)
     call soil_diel(freq, t_soil, mv, rhob, rhos, sand, clay, esoil)
!    theta_t = asin(real(sin(theta)*csqrt(eair)/csqrt(esnow)))
!    call reflectance(eair, esnow, theta, theta_t, r12_v, r12_h)
!    call transmitance(eair, esnow, theta, theta_t, t12_v, t12_h)
     theta_i = asin(real(sin(theta)*csqrt(eair)/csqrt(esnow)))
     call reflectance(esnow, eair, theta_i,  theta, r21_v, r21_h)
     call transmitance(esnow, eair, theta_i, theta, t21_v, t21_h)
     mu  = cos(theta_i)

     theta_t = asin(real(sin(theta_i)*csqrt(esnow)/csqrt(esoil)))
     call reflectance(esnow, esoil, theta_i, theta_t, r23_v, r23_h)
     call rough_reflectance(freq, theta_i, sigma, r23_v, r23_h)
     call snow_optic(freq,rad,snow_depth,va,ep_real, ep_imag,gv,gh,& 
            ssalb_v,ssalb_h,tau_v,tau_h)
     call two_stream_solution(t_skin,mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,r12_h, &
       r12_v,r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,t12_v,t12_h,esv,esh)
  else 
     sigma = half
     lai = 3.0_fp_kind*veg_frac + half
     mge = half*veg_frac
     leaf_thick = 0.07_fp_kind
     mu  = cos(theta)
!    r12_h    = zero
!    r12_v    = zero
     r21_h    = zero
     r21_v    = zero
     t21_h    = one
     t21_v    = one
!    t12_v    = one
!    t12_h    = one
     call soil_diel(freq, t_soil, mv, rhob, rhos, sand, clay, esoil)
     theta_t = asin(real(sin(theta)*csqrt(eair)/csqrt(esoil)))
     call reflectance(eair, esoil, theta, theta_t, r23_v, r23_h)
     call rough_reflectance(freq, theta, sigma, r23_v, r23_h)
     call canopy_diel(freq, mge, eveg)
     call canopy_optic(lai,freq,theta,eveg,leaf_thick,gv,gh,ssalb_v,ssalb_h,tau_v,tau_h)
     call two_stream_solution(t_skin,mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,&
          r12_h,r12_v,r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,t12_v,t12_h,esv,esh)

  endif
  return
  end subroutine landem

  subroutine canopy_optic(lai,frequency,theta,esv,d,gv,gh,&
        ssalb_v,ssalb_h,tau_v, tau_h)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    canopy_optic compute optic parameters for canopy
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute optic parameters for canopy
!
! program history log:
!
! input argument list:
!
!      lai         -  leaf area index
!      frequency   -  frequency (ghz)
!      theta       -  incident angle
!      esv         -  leaf dielectric constant
!      d           -  leaf thickness (mm)
!
! output argument list:
!
!      gv           -  asymmetry factor for v pol
!      gh           -  asymmetry factor for h pol
!      ssalb_v      -  single scattering albedo at v. polarization
!      ssalb_h      -  single scattering albedo at h. polarization
!      tau_v        -  optical depth at v. polarization           
!      tau_h        -  optical depth at h. polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind 
  use constants
  implicit none
  real(fp_kind) threshold
  real(fp_kind) frequency,theta,d,lai,ssalb_v,ssalb_h,tau_v,tau_h,gv, gh, mu
  complex ix,k0,kz0,kz1,rhc,rvc,esv,expval1,factt,factrvc,factrhc,junk1
  real(fp_kind) rh,rvert,th,tv,av,ah,astar,rstar
  threshold=0.999_fp_kind
  mu = cos(theta)
  ix  = cmplx(zero,one)
  k0  = cmplx(2._fp_kind*pi*frequency/300.0_fp_kind, zero)   ! 1/mm
  kz0 = k0*mu
  kz1 = k0*sqrt(esv - sin(theta)**2)
  rhc = (kz0 - kz1)/(kz0 + kz1)
  rvc = (esv*kz0 - kz1)/(esv*kz0 + kz1)  
  expval1=exp(-2._fp_kind*ix*kz1*d)
  factrvc=one-rvc**2*expval1
  factrhc=one-rhc**2*expval1
  factt=4._fp_kind*kz0*kz1*exp(ix*(kz0-kz1)*d)
       junk1=cmplx(one,0.0)
  rvert = cabs(rvc*(junk1 - expval1)/factrvc)**2
  rh = cabs(rhc*(junk1 - expval1)/factrhc)**2
!!  rvert = cabs(rvc*(one - expval1)/factrvc)**2
!!  rh = cabs(rhc*(one - expval1)/factrhc)**2
  th = cabs(factt/((kz1+kz0)**2*factrhc))**2
  tv = cabs(esv*factt/((kz1+esv*kz0)**2*factrvc))**2
! av = one - rvert - tv
! ah = one - rh - th
! astar = av + ah
! rstar = rvert + rh
! randomly oriented leaves
  gv = half
  gh = half
! tau_v = half*lai*(astar + rstar)
  tau_v = half*lai*(2.0-tv-th)
  tau_h = tau_v
! tau_h = half*lai*(astar + rstar)
! ssalb_v = rstar/ (astar + rstar)
  ssalb_v = min((rvert+rh)/ (2.0_fp_kind-tv-th),threshold)
  ssalb_h = ssalb_v
! ssalb_h = rstar/ (astar + rstar)
  return
  end subroutine canopy_optic

  subroutine snow_optic(frequency,a,h,f,ep_real,ep_imag,gv,gh,&
               ssalb_v,ssalb_h,tau_v,tau_h)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    landem      comput optic parameters for snow
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute optic parameters for snow
!
! program history log:
!
! input argument list:
!
!      theta        -  local zenith angle (degree)
!      frequency    -  frequency (ghz)
!      ep_real      -  real part of dielectric constant of particles
!      ep_imag      -  imaginary part of dielectric constant of particles
!      a            -  particle radiu (mm)
!      h            -  snow depth(mm)
!      f            -  fraction volume of snow (0.0 - 1.0)
!
! output argument list:
!
!       ssalb       -  single scattering albedo
!       tau         -  optical depth
!       g           -  asymmetry factor
!
!   important internal variables:
!
!       ks          -  scattering coeffcient (/mm)
!       ka          -  absorption coeffient (/mm)
!       kp          -  eigenvalue of two-stream approximation
!       y           -  = yr+iyi
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind
  use constants
  implicit none
  real(fp_kind) yr,yi,ep_real,ep_imag
  real(fp_kind) frequency,a,h,f,ssalb_v,ssalb_h,tau_v,tau_h,gv,gh,k
  real(fp_kind) ks1,ks2,ks3,ks,kr1,kr2,kr3,kr,ki1,ki2,ki3,ki,ka
  real(fp_kind) fact1,fact2,fact3,fact4,fact5

  k = 2._fp_kind*pi/(300._fp_kind/frequency)
  yr  = (ep_real - one)/(ep_real + 2.0_fp_kind)
  yi =  - ep_imag/(ep_real + 2.0_fp_kind)
  fact1 = (one+2.0_fp_kind*f)**2
  fact2 = one-f*yr
  fact3 = (one-f)**4
  fact4 = f*(k*a)**3
  fact5 = one+2.0_fp_kind*f*yr
  ks1 = k*sqrt(fact2/fact5)
  ks2 = fact4*fact3/fact1
  ks3 = (yr/fact2)**2
  ks = ks1*ks2*ks3  
  kr1 = fact5/fact2
! kr2 = 2.0_fp_kind*fact4*fact3/fact1
  kr2 = 2.0_fp_kind*ks2
  kr3 = 2.0_fp_kind*yi*yr/(fact2**3)
  kr = k*sqrt(kr1+kr2*kr3)

  ki1 = 3.0_fp_kind*f*yi/fact2**2
! ki2 = 2.0_fp_kind*fact4*fact3/fact1
  ki2 = kr2
! ki3 = (yr/fact2)**2
  ki3 = ks3
  ki  = k**2/(2.0_fp_kind*kr)*(ki1+ki2*ki3)
! ka = ki - ks
  gv = half
  gh = half
  ssalb_v = ks / ki
  if(ssalb_v .gt. 0.999_fp_kind) ssalb_v = 0.999_fp_kind
  ssalb_h = ssalb_v
  tau_v = 2.0_fp_kind*ki*h
  tau_h = tau_v
  return 
  end subroutine snow_optic
!
  subroutine soil_diel(freq,t_soil,vmc,rhob,rhos,sand,clay,esm)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    soil_diel   calculate the dielectric properties of soil
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute the dilectric constant of the bare soil
!
! program history log:
!
! input argument list:
!
!      theta        -  local zenith angle (degree)
!      frequency    -  frequency (ghz)
!      t_soil       -  soil temperature
!      vmc          -  volumetric moisture content (demensionless)
!      rhob         -  bulk volume density of the soil (1.18-1.12)
!      rhos         -  density of the solids (2.65 g.cm^3 for
!                       solid soil material)
!      sand         -  sand fraction (sand + clay = 1.0)
!      clay         -  clay fraction
!
! output argument list:
!
!      esm          -  dielectric constant for bare soil
!
! important internal variables:
!
!      esof         -  the permittivity of free space  
!      eswo         -  static dieletric constant 
!      tauw         -  relaxation time of water  
!      s            -  salinity
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind
  use constants
  implicit none
  real(fp_kind) esof
  real(fp_kind)    f,a,b,tauw,freq,t_soil,vmc,rhob,rhos,sand,clay
  real(fp_kind)    s,alpha,beta,ess,rhoef,t,eswi,eswo
  complex ix,esm,esw,es1,es2
! 
! ix  = cmplx(zero, one)
! s = zero
  alpha = 0.65_fp_kind
  beta  = 1.09_fp_kind - 0.11_fp_kind*sand + 0.18_fp_kind*clay
  ess = (1.01_fp_kind + 0.44_fp_kind*rhos)**2 - 0.062_fp_kind                              !a2
  rhoef = -1.645_fp_kind + 1.939_fp_kind*rhob - 0.020213_fp_kind*sand + 0.01594_fp_kind*clay       !a4
  t = t_soil - 273.0_fp_kind
  f = freq*1.0e9
! the permittivity at the high frequency limit
  eswi = 5.5_fp_kind
! the permittivity of free space (esof)
  esof = 8.854e-12
! static dieletric constant (eswo)
  eswo = 87.134_fp_kind+(-1.949e-1+(-1.276e-2+2.491e-4*t)*t)*t
! a = 1.0+(1.613e-5*t-3.656e-3+(3.210e-5-4.232e-7*s)*s)*s
! eswo = eswo*a
! relaxation time of water (tauw)
  tauw = 1.1109e-10+(-3.824e-12+(6.938e-14-5.096e-16*t)*t)*t
! b = 1.0+(2.282e-5*t-7.638e-4+(-7.760e-6+1.105e-8*s)*s)*s
! tauw = tauw*b
  if (vmc .gt. zero) then
    es1 = cmplx(eswi, - rhoef *(rhos - rhob)/(2._fp_kind*pi*f*esof*rhos*vmc)) 
  else
    es1 = cmplx(eswi, zero)
  endif
  es2 = cmplx(eswo - eswi,zero)/cmplx(one, f*tauw)
  esw = es1 + es2
  esm = one + (ess**alpha - one)*rhob/rhos + vmc**beta*esw**alpha - vmc       
  esm = esm**(one/alpha)
  if(aimag(esm) .ge.zero) esm = cmplx(real(esm), -0.0001_fp_kind)

  return
  end subroutine soil_diel
!        
  subroutine snow_diel(frequency,ep_real,ep_imag,rad,frac,ep_eff)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    snow_diel   compute dielectric constant of snow
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute dielectric constant of snow
!
!
! program history log:
!
! input argument list:
!
!       frequency   -  frequency (ghz)
!       ep_real     -  real part of dielectric constant of particle
!       ep_imag     -  imaginary part of dielectric constant of particle
!       rad         -  particle radiu (mm)
!       frac        -  fraction volume of snow (0.0 - 1.0)
!
! output argument list:
!
!       ep_eff      -  dielectric constant of the dense medium
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind
  use constants
  implicit none
  real(fp_kind) ep_imag,ep_real
  real(fp_kind) frequency,rad,frac,k0,yr,yi
  complex  y,ep_r,ep_i,ep_eff,fracy
  k0 = 2._fp_kind*pi/(300._fp_kind/frequency)
  yr  = (ep_real - one)/(ep_real + 2.0_fp_kind)
  yi =   ep_imag/(ep_real + 2.0_fp_kind)
  y = cmplx(yr, yi)
  fracy=frac*y
  ep_r = (one + 2.0_fp_kind*fracy)/(one - fracy)
  ep_i = 2._fp_kind*fracy*y*(k0*rad)**3*(one-frac)**4/((one-fracy)**2*(one+2._fp_kind*frac)**2)
  ep_eff = ep_r - cmplx(zero,one)*ep_i
  if (aimag(ep_eff).ge.zero) ep_eff = cmplx(real(ep_eff), -0.0001_fp_kind)
  return 
  end subroutine snow_diel

  subroutine canopy_diel(frequency,mg,esv)

!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   canopy_diel compute the dielectric constant of the vegetation canopy
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute the dielectric constant of the vegetation canopy
!     geomatrical optics approximation for vegetation canopy
!     work for horizontal leaves
!
! program history log:
!
! input argument list:
!
!      frequency    -  frequency (ghz)
!      mg           -  gravimetric water content 
!     
! output argument list:
!
!      esv          -  dielectric constant of leaves
!
! remarks:
!
! references:
!
!     ulaby and el-rayer, 1987: microwave dielectric spectrum of vegetation part ii, 
!           dual-dispersion model, ieee trans geosci. remote sensing, 25, 550-557
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind
  use constants
  implicit none
  real(fp_kind)  frequency,  mg, en, vf, vb
  complex  esv, xx
  en = 1.7_fp_kind - (0.74_fp_kind - 6.16_fp_kind*mg)*mg
  vf = mg*(0.55_fp_kind*mg - 0.076_fp_kind)
  vb = 4.64_fp_kind*mg*mg/( one + 7.36_fp_kind*mg*mg)
  xx = cmplx(zero,one)
  esv = en + vf*(4.9_fp_kind + 75.0_fp_kind/(one + xx*frequency/18.0_fp_kind)-xx*(18.0_fp_kind/frequency)) + &
         vb*(2.9_fp_kind + 55.0_fp_kind/(one + sqrt(xx*frequency/0.18_fp_kind)))
  if (aimag(esv).ge.zero) esv = cmplx(real(esv), -0.0001_fp_kind)
  return
  end subroutine canopy_diel
  subroutine reflectance(em1, em2, theta_i, theta_t, rvert, rh)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    reflectance compute the surface reflectivity
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute the surface reflectivety using fresnel equations 
!    for a rough surface having a standard deviation of height of sigma  
!
! program history log:
!
! input argument list:
!      theta_i      -  incident angle (degree)
!      theta_t      -  transmitted angle (degree)
!      em1          -  dielectric constant of the medium 1
!      em2          -  dielectric constant of the medium 2
!
! output argument list:
!
!      rvert        -  reflectivity at vertical polarization
!      rh           -  reflectivity at horizontal polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind
  use constants
  implicit none
  real(fp_kind) theta_i, theta_t
  real(fp_kind) rh, rvert,cos_i,cos_t
  complex em1, em2, m1, m2, angle_i, angle_t
! compute the refractive index ratio between medium 2 and 1
! using dielectric constant (n = sqrt(e))
  cos_i=cos(theta_i)
  cos_t=cos(theta_t)
  angle_i = cmplx(cos_i, zero)
  angle_t = cmplx(cos_t, zero)
  m1 = csqrt(em1)
  m2 = csqrt(em2)
  rvert = (cabs((m1*angle_t-m2*angle_i)/(m1*angle_t+m2*angle_i)))**2
  rh = (cabs((m1*angle_i-m2*angle_t)/(m1*angle_i+m2*angle_t)))**2

  return 
  end subroutine reflectance

  subroutine transmitance(em1,em2,theta_i,theta_t,tv,th)

!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    transmitance    calculate transmitance
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute transmitance 
!
! program history log:
!
! input argument list:
!
!      theta        -  local zenith angle (degree)
!      theta_i      -  incident angle (degree)
!      theta_t      -  transmitted angle (degree)
!      em1          -  dielectric constant of the medium 1
!      em2          -  dielectric constant of the medium 2
!
! output argument list:
!
!      tv           -  transmisivity at vertical polarization
!      th           -  transmisivity at horizontal polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind
  use constants
  implicit none
  real(fp_kind) theta_i, theta_t
!
  real(fp_kind) th, tv, rr, cos_i,cos_t
!
  complex em1, em2, m1, m2, angle_i, angle_t
! compute the refractive index ratio between medium 2 and 1
! using dielectric constant (n = sqrt(e))
  cos_i=cos(theta_i)
  cos_t=cos(theta_t)
  angle_i = cmplx(cos_i, zero)
  angle_t = cmplx(cos_t, zero)
  m1 = csqrt(em1)
  m2 = csqrt(em2)
  rr = cabs(m2/m1)*cos_t/cos_i
  tv =  rr*(cabs(2.*m1*angle_i/(m1*angle_t + m2*angle_i)))**2
  th =  rr*(cabs(2.*m1*angle_i/(m1*angle_i + m2*angle_t)))**2
  return
  end subroutine transmitance

  subroutine rough_reflectance(frequency,theta,sigma,rvert,rh)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rought_reflectance calculate surface relectivity      
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute the surface reflectivety for a rough surface 
!    having a standard devoation of height of sigma  
!
!
! program history log:
!
! input argument list:
!
!      frequency    -  frequency (ghz)
!      theta        -  local zenith angle (degree)
!      sigma        -  standard deviation of rough surface height 
!                      smooth surface:0.38, medium: 1.10, rough:2.15 cm
!
!    internal variables
!
!
! output argument list:
!
!      rvert         -  reflectivity at vertical polarization
!      rh            -  reflectivity at horizontal polarization
!
!
!   important internal variables:
!
!      k0           -  a propagation constant or wavenumber in a free space
!
! remarks:
!
! references:
!
!   wang, j. and b. j. choudhury, 1992: passive microwave radiation from soil: examples...
!    passive microwave remote sensing of .. ed. b. j. choudhury, etal vsp. 
!    also wang and choudhury (1982)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind
  use constants
  implicit none
  real(fp_kind) theta, frequency
! real(fp_kind) p, q, sigma, rh, rvert, rh_s, rv_s
  real(fp_kind) p, q, rh, rvert, rh_s, rv_s, sigma
! rh_s = rh
! rv_s = rvert
  rh_s = 0.3_fp_kind*rh
  rv_s = 0.3_fp_kind*rvert
! p = 0.3
  q = 0.35_fp_kind*(one - exp(-0.60_fp_kind*frequency*sigma**2.0_fp_kind))  
! rh = (q*rv_s + (one - q)*rh_s)*p
! rv = (q*rh_s + (one - q)*rv_s)*p
  rh = rh_s + q*(rv_s-rh_s)
  rvert = rv_s + q*(rh_s-rv_s)
  return 
  end subroutine rough_reflectance

  subroutine two_stream_solution(b,mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,r12_h, & 
      r12_v,r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,t12_v,t12_h,esv,esh)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    two_stream_solution
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: two stream solution
!
!   version: beta 
!
! program history log:
!
! input argument list:
!
!      b            -  scattering layer temperature (k)			 (gdas)
!      mu           -  cos(theta)
!      gv           -  asymmetry factor for v pol
!      gh           -  asymmetry factor for h pol
!      ssalb_v      -  single scattering albedo at v. polarization
!      ssalb_h      -  single scattering albedo at h. polarization
!      tau_v        -  optical depth at v. polarization           
!      tau_h        -  optical depth at h. polarization
!      r12_v        -  reflectivity at vertical polarization
!      r12_h        -  reflectivity at horizontal polarization
!      r21_v        -  reflectivity at vertical polarization
!      r21_h        -  reflectivity at horizontal polarization
!      r23_v        -  reflectivity at vertical polarization
!      r23_h        -  reflectivity at horizontal polarization
!      t21_v        -  transmisivity at vertical polarization
!      t21_h        -  transmisivity at horizontal polarization
!      t12_v        -  transmisivity at vertical polarization
!      t12_h        -  transmisivity at horizontal polarization
!
! output argument list:
!
!       esh         -  emissivity for horizontal polarization
!       esv         -  emissivity for vertical polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use type_kinds, only: fp_kind
  use constants
  implicit none
  real(fp_kind) b, mu, gv, gh, ssalb_h, ssalb_v, tau_h,tau_v,r12_h, &
    r12_v, r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, t12_v, t12_h, esv, esh
! real(fp_kind) esh0, esh1, esh2, esv0, esv1, esv2
! real(fp_kind) esh1, esv1
  real(fp_kind) alfa_v, alfa_h, kk_h, kk_v, gamma_h, gamma_v, beta_v, beta_h
  real(fp_kind) fact1,fact2
  alfa_h = sqrt((1.0 - ssalb_h)/(1.0 - gh*ssalb_h))
  kk_h = sqrt ((1.0 - ssalb_h)*(1.0 -  gh*ssalb_h))/mu
  beta_h = (1.0 - alfa_h)/(1.0 + alfa_h)
  gamma_h = (beta_h -r23_h)/(1.0-beta_h*r23_h)
  alfa_v = sqrt((1.0-ssalb_v)/(1.0 - gv*ssalb_v))
  kk_v = sqrt ((1.0-ssalb_v)*(1.0 - gv*ssalb_v))/mu
  beta_v = (1.0 - alfa_v)/(1.0 + alfa_v)
  gamma_v = (beta_v -r23_v)/(1.0-beta_v*r23_v)
! esh0 = i0/b*r12_h
! esh0 = zero
! esh1 = (1.0 - beta_h)*(1.0 + gamma_h*exp(-2.0*kk_h*tau_h))
! esh1 = esh1/((1.0-beta_h*r21_h)-(beta_h-r21_h)*gamma_h*exp(-2.0*kk_h*tau_h))
! esh2 = i0/b*t12_h*(beta_h-gamma_h*exp(-2.0*kk_h*tau_h))
! esh2 = esh2 /((1.0-beta_h*r21_h)-(beta_h-r21_h)*gamma_h*exp(-2.0*kk_h*tau_h))
! esh2 = 0.0
! esv0 = i0/b*r12_v
! esv0 = 0.0
! esv1 = (1.0-beta_v)*(1.0+gamma_v*exp(-2.0*kk_v*tau_v))
! esv1 = esv1/((1.0-beta_v*r21_v)-(beta_v-r21_v)*gamma_v*exp(-2.0*kk_v*tau_v))
! esv2 = i0/b*t12_v*(beta_v - gamma_v*exp(-2.0*kk_v*tau_v))
! esv2 = esv2 /((1.0-beta_v*r21_v)-(beta_v-r21_v)*beta_v*exp(-2.0*kk_v*tau_v))
! esv2 = 0.0
! esh  = esh0 + t21_h*(esh1 + esh2)
! esv  = esv0 + t21_v*(esv1 + esv2)
  fact1=gamma_h*exp(-2.0_fp_kind*kk_h*tau_h)
  fact2=gamma_v*exp(-2.0_fp_kind*kk_v*tau_v)
  esh  = t21_h*(one - beta_h)*(one + fact1) &
         /(one-beta_h*r21_h-(beta_h-r21_h)*fact1)
  esv  = t21_v*(one - beta_v)*(one + fact2) &
         /(one-beta_v*r21_v-(beta_v-r21_v)*fact2)
  return
  end subroutine two_stream_solution
