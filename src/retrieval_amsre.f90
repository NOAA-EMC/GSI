subroutine retrieval_amsre(tb,amsre_low, amsre_mid, amsre_hig,  &
                        uu5,vv5,f10, sst, &
                        tpwc,clw,si85,kraintype,ierr )

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  retrieval_amsre         make retrieval from AMSR-E observation
!
!   prgmmr: kazumori          org: np23                date: 2005-10-20
!
! abstract: This subroutine create retrievals from AMSR-E observation
!
! program history log:
!   2005-10-20  kazumori - create retrieval subroutine for AMSR-E
!   2005-10-21  kazumori - reformated for GSI
!   2006-04-26  kazumori - removed extra qc and comment changed
!   2006-07-27  kazumori - modified bias correction of retrieval
!                          and clean up the code
!   2008-04-16  safford  - rm unused uses and vars
!
!   input argument list:
!     tb      - Observed brightness temperature [K]
!     amsre_low   - logical true if amsre_low is processed
!     amsre_mid   - logical true if amsre_mid is processed
!     amsre_hig   - logical true if amsre_hig is processed
!     uu5   - guess surface wind (u-component)
!     vv5   - guess surface wind (v-component)
!     f10   - 10meter factor
!     sst   - sea surface temperature[K]
!
!   output argument list:
!     tpwc    - column water vapor over ocean  [kg/m2]
!     clw     - column water vapor over ocean  [kg/m2]
!     si85    - scattering index over ocean
!     kraintype - kraintype flag
!     ierr    - error flag
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: zero,izero

  implicit none
  
! Input variable
  real(r_kind),dimension(12),intent(in)::tb
  real(r_kind),intent(in):: uu5,vv5,f10
  real(r_kind),intent(in):: sst
  logical,intent(in):: amsre_low
  logical,intent(in):: amsre_mid
  logical,intent(in):: amsre_hig

! Output variable
  integer(i_kind),intent(out)     ::kraintype,ierr
  real(r_kind),intent(out)::tpwc,clw
  real(r_kind)::si85

! Internal variable
  integer(i_kind) :: nchanl1
  real(r_kind) :: degre,wind
  real(r_kind) :: rwp,cwp,vr,vc
  real(r_kind),dimension(12)::tbo

! Initialize variable
  nchanl1 = 12   ! Total AMSR-E channel number=12
  ierr = izero; kraintype=izero
  rwp =zero;cwp=zero;vr=zero;vc=zero

  if(amsre_low .or. amsre_mid) degre=55.0_r_kind
  if(amsre_hig) degre=54.5_r_kind

! Gross error check on all channels.  If there are any
! bad channels, skip this obs.

  if ( any(tb < 50.0_r_kind) .or. any(tb > 400.0_r_kind ) ) then
    ierr = 1
    return
  end if

  tbo(:)=tb(:)

  wind = f10*sqrt(uu5*uu5+vv5*vv5)

  call RCWPS_Alg(degre,tbo,sst,wind,rwp,cwp,vr,vc)

  tpwc=vr  ! 18.7GHz
!  tpwc=vc ! 36.5GHz
  clw=cwp
  clw = clw - 0.03   ! remove bias
  si85=rwp

!  =======   TPW over ocean (when no rain)  ====================

  if(kraintype==izero) then
    tpwc = max(zero,tpwc)
  end if !no rain

!  =======   CLW over ocean (when no rain)  ====================

  if(kraintype==izero) then

!    Ensure clw is non-negative.
    clw    = max(zero,clw)
 
!     upper limit of 6.0 kg/m2.
!     if(clw>6.0_r_kind) clw=zero

  end if  !no rain

  return
end subroutine retrieval_amsre


subroutine RCWPS_Alg(theta,tbo,sst,wind,rwp,cwp,vr,vc)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: RCWPS_Alg   make retrieval from AMSR-E observation
!
!   prgrmmr: Banghua Yan and Fuzhong Weng   org: NESDIS              date: 2004-09-20
!
! abstract:
!     Retrieve rain water path, cloud water path, and total precipitable water over oceans
!              from AMSR-E measurements
!     Please refer to the following paper for details
!
!  (1) Weng. F., L. Zhao, R. R. Ferraro, G. Poe, X. Li., and N. Grody, 2003:
!      Advanced microwave sounding unit cloud and precipitation algorithms, Radio Science, 38,
!      Mar 33, 1-12.
!  (2) Yan, B. and F. Weng,2004: "Rain and cloud water paths derived from Aqua AMSR-E measurements',
!     the 13th conference on satellite meteorolgy and oceanography, Norfolk in VA, Sept. 20-23.
!  (3) Yan, B. and F. Weng: New applications of AMSR-E measurements under tropical cyclones,
!      Part II: retrieval of liquid water path
!      submitted to J. Geophys. Res., 2005.
!
! program history log:
!      Algorithms created                                    : 03/02/04
!      beta version is released in ORA
!                                                            :  10/22/04
!      betat version is release to EMC                       :  06/14/05
!
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses and vars
!
!   input argument list:
!     tbo(1): Vertically polarized AMSR-E brighness temperature at 6.925 GHz
!     tbo(3):                                                      10.65 GHz
!     tbo(5):                                                      18.7  GHz
!     tbo(7):                                                      23.8  GHz
!     tbo(9):                                                      36.5  GHz
!     tbo(11):                                                     89    GHz  (not used here)
!     tbo(2): Horizontally polarized AMSR-E brighness temperature at 6.925 GHz
!     tbo(4):                                                        10.65 GHz
!     tbo(6):                                                        18.7  GHz
!     tbo(8):                                                        23.8  GHz
!     tbo(10):                                                       36.5  GHz
!     tbo(12):                                                       89    GHz  (not used here)
!     theta  : local zenith angle in degree
!     sst: sea surface temperature (K)
!     ssw: sea surface wind        (m/s)
!
!   output argument list:
!     rwp: rain water path         (mm)
!     cwp: cloud water path        (mm)
!     vr : total precipitable water retrieved at 18.7 GHz FOV
!     vc : total precipitable water retrieved at 36.5 GHz FOV
!
!   important internal variable:
!     tl        : cloud layer temperature
!     angle     : local zenith angle in radian
!     umu       : cosine of angle
!     freq(*)   : six frequencies (6.9, 10.7, 18.7, 23.8, 36.5, 89.0)
!     kw(*)     : vapor water mass absorption  coefficient at six frequencies
!     ko2_coe(*): fitting coefficients to calculate oxygen optical thickness at six frequencies
!     kl_coe(*) : fitting coefficients to calculate liquid water mass absorption coefficient at six frequencies
!
!   comments:
!
!     Called Subroutines:
!     TBE_FROM_TBO(): scattering correction for AMSR-E measurements
!     TBA_FROM_TBE(): RTM bias correction
!     emis_water()  : sea surface emissivity
!
!     Remarks:
!      Questions/comments: Please send to Fuzhong.Weng@noaa.gov and Banghua.Yan@noaa.gov
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: deg2rad,zero,half,one,two,five

  implicit none

  integer(i_kind) nch
  parameter(nch=6)

  real(r_kind),intent(in) ::  tbo(nch*2)
  real(r_kind),intent(in) ::  wind,theta,sst

  real(r_kind),intent(out)::  rwp,vr,vc,cwp

  integer(i_kind) ich,i,polar_status
  real(r_kind)  angle,frequency,emissivity
  real(r_kind)  freq(nch),ev(nch),eh(nch)
  real(r_kind)  tbe(nch*2),tauo(nch),kl(nch),kw(nch),tv(nch),th(nch),tvmin(nch),thmin(nch)
  real(r_kind)  ko2_coe(nch,3),kl_coe(nch,3)
  real(r_kind)  umu,tl
  real(r_kind)  a0,a1,a2,b0,b1,b2
  data freq/6.925_r_kind,10.65_r_kind,18.7_r_kind,23.8_r_kind,37._r_kind,89.0_r_kind/
  data  kw/ 7.253225e-5_r_kind,1.8841e-4_r_kind,1.6962e-3_r_kind,5.268469e-3_r_kind,1.96235e-3_r_kind,9.399432e-3_r_kind/
  data  (ko2_coe(1,i),i=1,3)/1.045148e-002_r_kind,  2.909099e-005_r_kind, -1.258838e-007_r_kind/
  data  (ko2_coe(2,i),i=1,3)/1.144385e-002_r_kind,  3.133540e-005_r_kind, -1.367926e-007_r_kind/
  data  (ko2_coe(3,i),i=1,3)/1.545055e-002_r_kind,  4.119837e-005_r_kind, -1.825797e-007_r_kind/
  data  (ko2_coe(4,i),i=1,3)/2.016777e-002_r_kind,  5.265939e-005_r_kind, -2.361456e-007_r_kind/
  data  (ko2_coe(5,i),i=1,3)/5.452020e-002_r_kind,  1.377254e-004_r_kind, -6.308589e-007_r_kind/
  data  (ko2_coe(6,i),i=1,3)/6.360679e-002_r_kind,  1.084979e-004_r_kind, -6.453394e-007_r_kind/

  data (kl_coe(1,i),i=1,3)/ 1.045782e-002_r_kind, -3.353543e-004_r_kind,  5.306014e-006_r_kind/
  data (kl_coe(2,i),i=1,3)/ 2.459446e-002_r_kind, -7.827213e-004_r_kind,  1.225936e-005_r_kind/
  data (kl_coe(3,i),i=1,3)/ 7.431756e-002_r_kind, -2.276136e-003_r_kind,  3.413946e-005_r_kind/
  data (kl_coe(4,i),i=1,3)/ 1.182024e-001_r_kind, -3.487585e-003_r_kind,  5.012487e-005_r_kind/
  data (kl_coe(5,i),i=1,3)/ 2.677390e-001_r_kind, -6.890666e-003_r_kind,  8.319393e-005_r_kind/
  data (kl_coe(6,i),i=1,3)/ 1.034859e+000_r_kind, -9.715101e-003_r_kind, -6.591484e-005_r_kind/

! Convert degree to radian
  angle = theta *deg2rad
  umu = cos(angle)

! A temporal assumption about cloud layer temperature (to be updated when it is available)
  tl = sst-20.0_r_kind-273.15_r_kind
  if (tl .gt. 10.0_r_kind) tl = 10.0_r_kind

! scatteing correction
  call TBE_FROM_TBO(tbo,tbe)

! Adjust TBE to TBA required in the algorithms
  call TBA_FROM_TBE(tbe,tv,th)

! Calculate KW and KL and tau_o2(taut) and emissivity
  do ich = 1, nch
    tauo(ich) = ko2_coe(ich,1) +  ko2_coe(ich,2)*sst + ko2_coe(ich,3)*sst*sst
    kl(ich)   = kl_coe(ich,1)  +  kl_coe(ich,2)*tl   + kl_coe(ich,3)*tl*tl
    frequency = freq(ich)
    polar_status = 0
    call emis_water(angle,frequency,sst,wind,polar_status, emissivity)
    eh(ich) = emissivity
    polar_status = 1
    call emis_water(angle,frequency,sst,wind,polar_status, emissivity)
    ev(ich) = emissivity
    tvmin(ich) = sst*( one - dexp(-tauo(ich)/umu)*dexp(-tauo(ich)/umu)*(one-ev(ich)) )
    thmin(ich) = sst*( one - dexp(-tauo(ich)/umu)*dexp(-tauo(ich)/umu)*(one-eh(ich)) )

! Quality control
    if (tv(ich) .lt. tvmin(ich)) tv(ich) = tvmin(ich)
    if (th(ich) .lt. thmin(ich)) th(ich) = thmin(ich)
  enddo

! Calculate a0, a1, a2 and b0, b1 and b2 at 18.7 GHz over 23.8 GHz

! 18.7 over 23.8 GHz: rain water path
  a0 = -half*kw(4)/(kw(4)*kl(3)-kw(3)*kl(4))
  b0 =  half*kl(4)/(kw(4)*kl(3)-kw(3)*kl(4))
  a1 =  kw(3)/kw(4)
  b1 =  kl(3)/kl(4)
  a2 = -two*(tauo(3) - a1*tauo(4))/umu  +(one-a1)*dlog(sst) + dlog(one-ev(3)) - a1*dlog(one-ev(4))
  b2 = -two*(tauo(3)  - b1*tauo(4))/umu +(one-b1)*dlog(sst) + dlog(one-ev(3)) - b1*dlog(one-ev(4))

  if ( ( sst-tv(3) .gt. 0.01_r_kind ) .and. ( sst-tv(4) .gt. 0.01_r_kind ) ) then
    rwp = a0*umu*( dlog(sst-tv(3) ) - a1*dlog(sst-tv(4))-a2)
    vr = b0*umu*( dlog(sst-tv(3)) - b1*dlog(sst-tv(4))-b2 )

! Clear conditions
    if (rwp .lt. zero) rwp = zero
    if (vr .lt. zero) vr = zero
  else

! Invalid retrieval
    rwp = -999.0_r_kind
    vr  = -999.0_r_kind
  endif

! 36.5 over 23.8 GHz: cloud water path
  a0 = -half*kw(4)/(kw(4)*kl(5)-kw(5)*kl(4))
  b0 =  half*kl(4)/(kw(4)*kl(5)-kw(5)*kl(4))
  a1 =  kw(5)/kw(4)
  b1 =  kl(5)/kl(4)
  a2 = -two*(tauo(5) - a1*tauo(4))/umu  +(one-a1)*dlog(sst) + &
                           dlog(one-ev(5)) - a1*dlog(one-ev(4))
  b2 = -two*(tauo(5)  - b1*tauo(4))/umu +(one-b1)*dlog(sst) + &
                           dlog(one-ev(5)) - b1*dlog(one-ev(4))
  if ( ( sst-tv(4) .gt. 0.01_r_kind ) .and. ( sst-tv(5) .gt. 0.01_r_kind ) ) then
    cwp = a0*umu*( dlog(sst-tv(5) ) - a1*dlog(sst-tv(4))-a2)
    vc = b0*umu*( dlog(sst-tv(5)) - b1*dlog(sst-tv(4))-b2 )
    if(cwp .lt. zero) cwp = zero
    if(vc .lt. zero) vc = zero
  else
    cwp = -999.0_r_kind
    vc  = -999.0_r_kind
  endif

! Quality control: remove residual effect of sea roughness on 18.7 GHz
  if ( cwp .le. 0.3_r_kind) rwp = cwp
  if (cwp .le. 0.2_r_kind .and. wind .ge. five) rwp = zero

end subroutine RCWPS_Alg



subroutine TBE_FROM_TBO(tbo,tb)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: TBE_FROM_TBO
!
!   prgrmmr: Banghua Yan      org: NESDIS        date:  2004-04-20
!
! abstract:
!     Perform corrections for scattering effect for AMSR-E measurements (6.9 ~ 36.5 GHz)
!     Note: Scattering effects on the tbo at 89 GHz are to be corrected
!
! program history log:
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses
!
!   input argument list:
!     tbo(*) : AMSRE measurements
!
!   output argument list:
!     tb(*)  : brightness temperatures with scattering correction
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: three
  implicit none

  integer(i_kind) nch,i,j,k
  parameter (nch = 6)

  real(r_kind),intent(in) :: tbo(nch*2)
  real(r_kind),intent(out):: tb(nch*2)

  real(r_kind) tbe(nch*2),tv18
  real(r_kind) coe_tbs(10,11)
  data (coe_tbs(1,k),k=1,11)/  &
    2.836349e+000_r_kind, 1.001083e+000_r_kind,-1.245813e-002_r_kind,-1.431959e-002_r_kind, 3.735422e-002_r_kind, &
    4.765791e-002_r_kind,-9.139793e-002_r_kind,-5.571145e-002_r_kind, 9.269717e-002_r_kind, 2.102336e-002_r_kind, &
   -3.769030e-002_r_kind/
  data (coe_tbs(2,k),k=1,11)/  &
    3.700653e+000_r_kind, 1.139987e-002_r_kind, 9.760773e-001_r_kind,-3.792810e-002_r_kind, 6.930733e-002_r_kind,&
    9.327399e-002_r_kind,-1.626923e-001_r_kind,-1.000955e-001_r_kind, 1.667856e-001_r_kind, 3.836404e-002_r_kind,&
   -6.907219e-002_r_kind/
  data (coe_tbs(3,k),k=1,11)/ &
    -1.238724e+000_r_kind, -4.068373e-002_r_kind,  1.732873e-003_r_kind,  1.050165e+000_r_kind, -6.747865e-003_r_kind, &
    -8.419795e-003_r_kind,  4.294594e-003_r_kind, -5.081398e-003_r_kind,  2.105786e-002_r_kind, -1.441340e-003_r_kind, &
    -9.873924e-003_r_kind/
  data (coe_tbs(4,k),k=1,11)/ &
    -1.341827e+000_r_kind, -2.947993e-002_r_kind, -1.031828e-003_r_kind,  3.924831e-002_r_kind,  9.954801e-001_r_kind, &
    -2.360948e-002_r_kind,  2.565339e-002_r_kind,  1.999745e-002_r_kind, -2.145227e-002_r_kind, -1.131899e-002_r_kind, &
    1.201244e-002_r_kind/
  data (coe_tbs(5,k),k=1,11)/  &
    4.851257e+000_r_kind,-1.001152e-002_r_kind,-1.666120e-002_r_kind,-2.234872e-002_r_kind, 5.869221e-002_r_kind, &
    1.113674e+000_r_kind,-1.946697e-001_r_kind,-1.325814e-001_r_kind, 2.210909e-001_r_kind, 4.219167e-002_r_kind, &
   -7.981353e-002_r_kind/
  data (coe_tbs(6,k),k=1,11)/  &
    2.223694e+000_r_kind,-1.482995e-002_r_kind,-5.777131e-003_r_kind, 2.200880e-003_r_kind, 2.359764e-002_r_kind, &
    5.473155e-002_r_kind, 9.022181e-001_r_kind,-6.822398e-002_r_kind, 1.177735e-001_r_kind, 2.131365e-002_r_kind, &
   -4.201306e-002_r_kind/
  data (coe_tbs(7,k),k=1,11)/  &
    2.246658e+000_r_kind,-2.905825e-002_r_kind,-1.400843e-002_r_kind, 7.278482e-003_r_kind, 5.651486e-002_r_kind, &
    9.341484e-002_r_kind,-2.202809e-001_r_kind, 8.776495e-001_r_kind, 2.594665e-001_r_kind, 3.959633e-002_r_kind, &
   -8.018037e-002_r_kind/
  data (coe_tbs(8,k),k=1,11)/  &
    7.324219e-003_r_kind,-3.075898e-002_r_kind,-2.037739e-003_r_kind, 2.657354e-002_r_kind, 1.731113e-002_r_kind, &
    3.657620e-002_r_kind,-1.028747e-001_r_kind,-5.361976e-002_r_kind, 1.126930e+000_r_kind, 1.459956e-002_r_kind, &
   -3.240352e-002_r_kind/
  data (coe_tbs(9,k),k=1,11)/ &
    4.557663e+000_r_kind, -1.428426e-001_r_kind,  1.443825e-002_r_kind,  1.916111e-001_r_kind, -7.515940e-003_r_kind,   &
    1.687996e-001_r_kind, -3.647460e-001_r_kind, -3.194418e-001_r_kind,  5.943681e-001_r_kind,  1.140872e+000_r_kind,   &
   -2.494820e-001_r_kind/
  data (coe_tbs(10,k),k=1,11)/ &
    7.333450e+000_r_kind, -1.293110e-001_r_kind,  1.874678e-002_r_kind,  1.842369e-001_r_kind, -2.854594e-002_r_kind,  &
    8.010966e-002_r_kind, -2.214617e-001_r_kind, -1.830799e-001_r_kind,  3.916801e-001_r_kind,  1.001790e-001_r_kind,  &
    7.973670e-001_r_kind/

!v,h-->h,v
  tb(1) = tbo(2)
  tb(2) = tbo(1)
  tb(3) = tbo(4)
  tb(4) = tbo(3)
  tb(5) = tbo(6)
  tb(6) = tbo(5)
  tb(7) = tbo(8)
  tb(8) = tbo(7)
  tb(9) = tbo(10)
  tb(10) = tbo(9)
  tb(11) = tbo(12)
  tb(12) = tbo(11)

! from tbscat to tbemiss
  do i = 1, 10
    tbe(i) = coe_tbs(i,1)
    do  j=1,10
      tbe(i) = tbe(i) + coe_tbs(i,j+1)*tb(j)
    enddo
    tbe(10) = tbe(10) + 0.4*(tbe(10)-tb(10))
  enddo

! tbo at 89 GHz are to be corrected
!
  tbe(11) = tb(11)
  tbe(12) = tb(12)

!h,v --> v,h
  tb(1) = tbe(2)
  tb(2) = tbe(1)
  tb(3) = tbe(4)
  tb(4) = tbe(3)
  tb(5) = tbe(6)
  tb(6) = tbe(5)
  tb(7) = tbe(8)
  tb(8) = tbe(7)
  tb(9) = tbe(10)
  tb(10) = tbe(9)
  tb(11) = tbe(12)
  tb(12) = tbe(11)

! correction of sea surface roughness

  tv18 = 0.0054_r_kind*tbo(7)*tbo(7) -1.9554_r_kind*tbo(7) +364.71_r_kind
  if ((tbo(5)-tv18 .ge. three)  .and. (tbo(2) .ge. 90.0)) &
    tb(5) = tb(5) - 5.5_r_kind*(tbo(5)-tv18)/(10.0_r_kind-three)
  return
  stop

end subroutine TBE_FROM_TBO

subroutine TBA_FROM_TBE(tbo,tvs,ths)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  TBA_FROM_TBO
!
!   prgmmr: Banghua Yan        org: NESDIS              date: 2004-04-20
!
! abstract:
!     Adjust AMSR-E measurements with scattering correction to algorithm-based brightness temperature
!
! program history log:
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses
!
!   input argument list:
!     tbo(*) : AMSRE measurements
!
!   output argument list:
!     tvs(*)  : algorithm-based brightness temperatures at a vertical polarization
!     ths(*)  : algorithm-based brightness temperatures at a horizontal polarization
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind, i_kind
  implicit none

  integer(i_kind) nch
  parameter (nch = 6)

  real(r_kind),intent(in) :: tbo(nch*2)
  real(r_kind),intent(out):: tvs(nch),ths(nch)

  real(r_kind) tb(nch*2)

!v,h-->h,v
  tb(1) = tbo(2)
  tb(2) = tbo(1)
  tb(3) = tbo(4)
  tb(4) = tbo(3)
  tb(5) = tbo(6)
  tb(6) = tbo(5)
  tb(7) = tbo(8)
  tb(8) = tbo(7)
  tb(9) = tbo(10)
  tb(10) = tbo(9)
  tb(11) = tbo(12)
  tb(12) = tbo(11)

  ths(1) = 2.617433e+001_r_kind + 6.600980e-001_r_kind*tb(1)
  tvs(1) = 6.504761e+000_r_kind + 9.540653e-001_r_kind*tb(2)
  ths(2) = 1.402604e+001_r_kind + 8.144087e-001_r_kind*tb(3)
  tvs(2) = 5.405548e+000_r_kind + 9.632518e-001_r_kind*tb(4)
  ths(3) = 4.261467e+000_r_kind + 9.567850e-001_r_kind*tb(5)
  tvs(3) = 2.251144e+000_r_kind + 9.880477e-001_r_kind*tb(6)
  ths(4) = 3.366165e+000_r_kind + 9.979538e-001_r_kind*tb(7)
  tvs(4) = -3.358444e+000_r_kind + 1.028767e+000_r_kind*tb(8)
  ths(5) = 2.409077e+001_r_kind + 8.443854e-001_r_kind*tb(9)
  tvs(5) = 3.148166e+001_r_kind + 8.714348e-001_r_kind*tb(10)
  ths(6) = 1.919507e+001_r_kind + 9.322882e-001_r_kind*tb(11)
  tvs(6) = 2.861269e+001_r_kind + 9.155271e-001_r_kind*tb(12)

end subroutine TBA_FROM_TBE

subroutine emis_water(angle,frequency,sst,wind,polar_status,emissivity)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  emis_water  compute oceanic emissivity
!
!   prgmmr: Fuzhong Weng and Banghua Yan         org: NESDIS
!
! abstract: compute oceanic emissivity at two polarizations
!       References:
!       (1) Klein, L.A., and C.T. Swift, 1977: An improved model for the dielectric constant of
!           sea water at microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111.
!       (2) Stogryn, A., 1972: The emissivity of sea foam at microwave frequencies, J. Geophys. Res.,
!            77, 1658-1666.
!       (3) Hollinger, J. P., 1971: Passive microwave measurements of sea surface roughness.
!            IEEE Transactions on Geoscience Electronics, GE-9(3), 165-169.
!
! program history
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses and vars
!
!   input argument list:
!         angle    :  incident angle in radian
!         sst      : temperature (K)
!         s        : sea water salinity (1/thousand)
!         frequency: (GHz)
!         wind     : wind speed (m/s)
!   output argument list:
!         rh       :  surface reflectance in horizontally polarized state
!         rv       :
!         eh       :  surface emitance in ....
!         ev       :
!   internal argument list:
!         foam     : foam fraction
!         g,tr     : emperical functions for wind induced
!                    changes in reflection coefficient
!         f        : frequency in Hz
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: zero,one,four
  implicit none

  real(r_kind) s
  parameter (s = 35.5_r_kind)

  real(r_kind),intent(in)    :: angle, wind
  real(r_kind),intent(in)    :: sst,frequency
  integer(i_kind),intent(in) :: polar_status

  real(r_kind),intent(out)   :: emissivity

  real(r_kind) t,degre
  real(r_kind) f
  real(r_kind) ref,rfoam,tr,g,rclear,foam,ev,eh,pi
  real(r_kind) ep,real_ep,imag_ep
  complex mu, eps, aid1,aid2,aid3,cang,refwat,rh,rv

  mu = cmplx (one,zero)
  f = frequency*1.0e9_r_kind
  pi = four*atan(one)
  degre = angle*180.0_r_kind/pi
  cang = cmplx(angle)
  t = sst ! - 273.15_r_kind

! complex dielectric properties of saline water
  call epsp(sst,s,f,ep)
  real_ep=ep
  call epspp(sst,s,f,ep)
  imag_ep=-ep
  eps=cmplx (real_ep,imag_ep)

! complex refractive index of saline water (not used)
  refwat = csqrt(eps)
  aid1 = csqrt(mu*eps-csin(cang)**2)
  aid2 = mu*ccos(cang)-aid1
  aid3 = mu*ccos(cang)+aid1
  rh = aid2/aid3
  aid2 = eps*ccos(cang)-aid1
  aid3 = eps*ccos(cang)+aid1
  rv = aid2/aid3
  if(wind.lt.7.0_r_kind) then
    foam=zero
  else
    foam=0.006_r_kind*(one-dexp(-f*1.0e-9_r_kind/7.5_r_kind))*(wind-7.0_r_kind)
  endif

! correction for wind induced foam free sea surface
  if(foam.lt.zero) foam=zero
  if(foam.gt.one) foam=one

! emperical functions for wind induced reflection changes for hp
  g = one - 1.748e-3_r_kind*degre-7.336e-5_r_kind*degre**2+ 1.044e-7_r_kind*degre**3
  tr = wind*(1.15e-1_r_kind+3.8e-5_r_kind*degre**2)*sqrt(f*1.0e-9_r_kind)
  rfoam = one-(208.0_r_kind+1.29e-9_r_kind*f)/t*g
  ref = (cabs(rh))**2
  rclear = ref - tr/t
  eh =one- (one-foam)*rclear-foam*rfoam

! emperical functions for wind induced reflection changes for vp
  g  = one - 9.946e-4_r_kind*degre+3.218e-5_r_kind*degre**2 -1.187e-6_r_kind*degre**3+7.e-20_r_kind*degre**10
  tr = wind*(1.17e-1_r_kind-2.09e-3_r_kind*dexp(7.32e-2_r_kind*degre))*sqrt(f*1.0e-9_r_kind)
  rfoam = one-(208.0_r_kind+1.29e-9_r_kind*f)/t*g
  ref = ( cabs(rv) )**2
  rclear = ref - tr/t
  ev = one-(one-foam)*rclear-foam*rfoam
  if(eh.gt.one) eh=one
  if(eh.lt.zero) eh=zero
  if(ev.gt.one) ev=one
  if(ev.lt.zero) ev=zero
  if (polar_status .eq. 1) then
    emissivity = ev
  else
    emissivity = eh
  endif

  return

end subroutine emis_water

subroutine  epsp(t1,s,f,ep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  epsp
!
!  prgmmr: Fuzhong Weng        org: NESDIS
!
! abstract: calculates the real part of the dielectric constant for saline water
!       References:
!    (1) Klein, L.A., and C.T. Swift, 1977: An improved model for the dielectric constant of
!        sea water at microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111.
!    (2) Ulaby, F.T., Moore, R.K., and Fung, A.K.,1986: Microwave remote sensing, active and passive, III,
!        From theory to applications, p.2024-2025.
!
! program history log:
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses
!
!   input argument list:
!         t1       : temperature (K)
!         s        : sea water salinity (1/thousand)
!         f        : (Hz)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!
  use kinds, only: r_kind
  use constants, only: one
  implicit none

  real(r_kind),intent(in) :: f,t1,s

  real(r_kind),intent(out):: ep

  real(r_kind) t,t2,eswi,eswo,a,b,esw,tswo,tsw

  t=t1-273.0_r_kind
  t2=(t-25.0_r_kind)
  eswi = 4.9_r_kind
  eswo = 87.134_r_kind-1.949e-1_r_kind*t-1.276e-2_r_kind*t*t+2.491e-4_r_kind*t**3
  a = one+1.613e-5_r_kind*t*s-3.656e-3_r_kind*s+3.210e-5_r_kind*s*s-4.232e-7_r_kind*s**3
  esw = eswo*a
  tswo = 1.1109e-10_r_kind-3.824e-12_r_kind*t+6.938e-14_r_kind*t**2-5.096e-16_r_kind*t**3
  b = one+2.282e-5_r_kind*t*s-7.638e-4_r_kind*s-7.760e-6_r_kind*s**2+1.105e-8_r_kind*s**3
  tsw = tswo*b
  ep = eswi +(esw-eswi)/(1.0+(f*tsw)**2)

  return

end subroutine

subroutine epspp (t1,s,f,ep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  epspp
!
!   prgmmr: Fuzhong Weng       org: NESDIS
!
! abstract: compute calculates the imaginair part  of the dielectric constant for saline water
!       References:
!    (1) Klein, L.A., and C.T. Swift, 1977: An improved model for the dielectric constant of
!        sea water at microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111.
!    (2) Ulaby, F.T., Moore, R.K., and Fung, A.K.,1986: Microwave remote sensing, active and passive, III,
!        From theory to applications, p.2024-2025.
!
! program history log:
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses
!
!   input argument list:
!         t1       : temperature (K)
!         s        : sea water salinity (1/thousand)
!         f        : (Hz)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind
  use constants, only: two,one,four
  implicit none

  real(r_kind),intent(in) :: f,t1,s
  real(r_kind),intent(out):: ep

  real(r_kind) t,t2,eswi,eswo,a,b,d,esw,tswo,tsw,sswo,fi,ssw
  real(r_kind) pi,eo

  t=t1-273.0_r_kind
  t2=t-25.0_r_kind
  eswi = 4.9_r_kind
  eo = 8.854e-12_r_kind
  pi = four * atan(one)
  eswo = 87.134_r_kind-1.949e-1_r_kind*t-1.276e-2_r_kind*t*t+2.491e-4_r_kind*t**3
  a = one+1.613e-5_r_kind*t*s-3.656e-3_r_kind*s+3.210e-5_r_kind*s*s-4.232e-7_r_kind*s**3
  esw = eswo*a
  tswo = 1.1109e-10_r_kind-3.824e-12_r_kind*t+6.938e-14_r_kind*t**2-5.096e-16_r_kind*t**3
  b = one+2.282e-5_r_kind*t*s-7.638e-4_r_kind*s-7.760e-6_r_kind*s**2+1.105e-8_r_kind*s**3
  tsw = tswo*b
  sswo = s*(0.18252_r_kind-1.4619e-3_r_kind*s+2.093e-5_r_kind*s**2-1.282e-7_r_kind*s**3)
  d = 25.0_r_kind-t
  fi = d*(2.033e-2_r_kind+1.266e-4_r_kind*d+2.464e-6_r_kind*d**2- s*(1.849e-5_r_kind-2.551e-7_r_kind*d+2.551e-8_r_kind*d*d))
  ssw = sswo*dexp(-fi)
  ep = tsw*f*(esw-eswi)/(one+(tsw*f)**2)
  ep = ep + ssw/(two*pi*eo*f)

  return

end subroutine

