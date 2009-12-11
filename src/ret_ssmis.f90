subroutine ret_ssmis(tb,nchanl, &
     ssmis_las, &
     tpwc,clw,ierr)
!$$$ subprogram documentation block
!
! subprogram:    ret_ssmis    retrieve various parameters for SSMIS
!
! prgmmr: weng, okamoto     org: np23                date: 2005-03-22
!
! abstract: retrieve clw from sounding ch using only 50.3GHz channel 
!     retrieve clw from SSM/I-like channels after mimicing frequency
!
!     These algorithm are based on
!
!     Weng, F., R. R. Ferraro, and N. C. Grody,2000: "Effects of AMSU cross-scan Symmetry of 
!          brightness temperatures on  retrieval of atmospheric and surface parameters", 
!          Ed. P. Pampaloni and S. Paloscia, VSP, Netherlands, 255-262, 2000 
!
!     Yan B. and F. Weng, 'Intercalibration between Special Sensor Microwave Imager and Sounder (SSMIS)
!         and Special Sensor Microwave Imager (SSM/I)', TGARS Special Issue on the DMSP SSMIS, 46, 984-995.
!
!
!     tpw:: total precipitable water 
!     clw :: column cloud water ove ocean  [kg/m2]
!         range 0-6.0kg/m2, precision:0.5kg/m2
!         Weng et.al.,1997,J.Climate vol.10   
!
! program history log:
!     2003-12-27  okamoto
!     2004-07-12  okamoto - simplify rain identification
!     2005-10-07  Xu & Pawlak - add documentation. Also, per Fuzhong Weng, deleted
!                 code for retrieval based on ssmis_las instrument since
!                 algorithm is intented only for ssmis_img, fixed indentation 
!     2006-04-27  Derber - modify for single profile
!     2007-01-24  kazumori - modify for UKMO preprocess SSMIS data, add retrieved tpw
!                            and bias correction of retrieved clw.
!     2008-04-16  safford  - rm unused uses and vars
!
!     2009-12-07  b.yan    - rewrite the ssmis TA level CLW agorithm and 
!                            remove the sea ice identification algorithm since this code frequently 
!                                   mis-identify ocean-pixels as sea ice-pixels
!
!     Reasons: in the previous version,there are the following bugs
!              (a) the ssmis data used in the gsi is tb level insetad of ta level
!              (b) the ssmis remapping coefficients are derived at ta level instead of tb level
!              (c) the polarization of the ssmis data is mis-used which will result in a totally wrong
!                  cloud liquid water path calculation
!              (d) the comments for the coefficients are not correct
!     In the new version of the code:
!              (a) TB2TA is needed to use the ssmis remapping coefficients derived at the ta level
!              (b) the remapping coefficients are updated , which are derived using the ssmis/ssmi SCO
!                  observations
!              (c) add the correct comments
!              (d) remove sea ice index and subroutine
!              (e) remove ssmis_las check if it is not necessary
!   input argument list:
!     tb      - Observed brightness temperature [K]
!     nchanl  - number of channels per obs
!     
!   Internal variable list:
!     tbx    - brightness temperatures at seven window channels
!     tby    - brightness temperatures at seven window channels which are consistent to f15 ssmi tb results
!     tax    - ssmis antenna temperatures at seven window channels
!     tay    - remapped ssmis antenna temperatures at seven window channels
!   output argument list:
!     clw     - column cloud water vapor over ocean  [kg/m2]
!     ierr    - error flag
!               [0]pass or escape this subroutine without doing anything
!               [1]tbb gross error
!               [2]polarization gross erro
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: izero,ione,zero,one
  
  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: nchanl
  real(r_kind),dimension(nchanl),intent(in   ) :: tb
  logical                       ,intent(in   ) :: ssmis_las
  integer(i_kind)               ,intent(  out) :: ierr
  real(r_kind)                  ,intent(  out) :: tpwc
  real(r_kind)                  ,intent(  out) :: clw

! Declare local variables
  real(r_kind)::rmis=-9.99e11_r_kind
  integer(i_kind) :: i
  integer(i_kind) :: iclwflg
  real(r_kind),parameter:: r285=285.0_r_kind
  real(r_kind),parameter:: r290=290.0_r_kind
  real(r_kind),dimension(7):: ap,bp,cp,dp,cp0,dp0,tbx,tby,tax,tay
  real(r_kind):: alg1,alg2,alg3

  ierr = izero
  clw = rmis

! Coefficients to remap SSMIS ssmi-like channels to SSMI TB
! Simultaneous Conical Overpass (SCO) data set-derived coefficients which are more accurate
!
  ap = (/0.00424_r_kind, -2.03627_r_kind, -2.52875_r_kind, 0.80170_r_kind, &
       -3.86053_r_kind, -7.43913_r_kind,1.53650_r_kind/)
  bp = (/1.00027_r_kind, 1.00623_r_kind, 0.99642_r_kind, 0.99139_r_kind, &
       1.00550_r_kind, 1.03121_r_kind, 0.99317_r_kind/)
     
! cp0 and dp0 for TA2TB
  cp0 = (/.969_r_kind, .969_r_kind, .974_r_kind, .986_r_kind,  &
          .986_r_kind, .988_r_kind, .988_r_kind/)
  dp0 = (/.00415_r_kind,.00473_r_kind,.0107_r_kind,.02612_r_kind,  &
           .0217_r_kind, .01383_r_kind, .01947_r_kind/)

  alg1 = rmis; alg2 = rmis; alg3 = rmis 
  tbx(:) = rmis;  tby(:) = rmis
 
! save brightness temperatures at seven window channels
  tbx(1:7) = tb(12:18)

! get ta2tb coefficients
  do i=1,7
     cp(i) = one/( cp0(i)*(one-dp0(i)) )
     dp(i) = cp(i) * dp0(i)
  end do

!   get ta from tb
  tax(1) = (tbx(1)*cp(2) + tbx(2)*dp(1))/(cp(1)*cp(2) - dp(1)*dp(2))
  tax(2) = (tbx(1)*dp(2) + tbx(2)*cp(1))/(cp(1)*cp(2) - dp(1)*dp(2))
  tax(3) = one/cp(3)*(tbx(3) + dp(3)*(.653_r_kind*tax(2)+ 96.6_r_kind))
  tax(4) = (tbx(4)*cp(5) + tbx(5)*dp(4))/(cp(4)*cp(5) - dp(4)*dp(5))
  tax(5) = (tbx(4)*dp(5) + tbx(5)*cp(4))/(cp(4)*cp(5) - dp(4)*dp(5))
  tax(6) = (tbx(6)*cp(7) + tbx(7)*dp(6))/(cp(6)*cp(7) - dp(6)*dp(7))
  tax(7) = (tbx(6)*dp(7) + tbx(7)*cp(6))/(cp(6)*cp(7) - dp(6)*dp(7))

! TAREMAPPING: Mimic ssmis imager channels to corresponding ssmis channels at ta level
! (So, tay is antenna temperature consistent to f15 ssmi ta)
  do i=1,7
     tay(i) =  ap(i) + bp(i)*tax(i)
  end do

! Get ssmis tb consisitent to f15 ssmi tb
! TA2TB(F15): Coefficients for tay to tby conversion

  tby(1)  =  cp(1)*tay(1) - dp(1)*tay(2);
  tby(2)  =  cp(2)*tay(2) - dp(2)*tay(1);
  tby(3)  =  cp(3)*tay(3) - dp(3)*(.653_r_kind*tay(2) + 96.6_r_kind);
  tby(4)  =  cp(4)*tay(4) - dp(4)*tay(5);
  tby(5)  =  cp(5)*tay(5) - dp(5)*tay(4);
  tby(6)  =  cp(6)*tay(6) - dp(6)*tay(7);
  tby(7)  =  cp(7)*tay(7) - dp(7)*tay(6);

! Calculate parameter for clw
  tpwc = 232.89_r_kind - 0.1486_r_kind*tby(1) - 0.3695_r_kind*tby(4)  & 
       - ( 1.8291_r_kind - 0.006193_r_kind*tby(3) )*tby(3)
     
! TA level CLW algorithm
  if( tay(2)<r285 .and. tay(3)<r285 ) then
     alg1 = -3.20_r_kind*(  &
          log( r290-tay(2) ) - 2.80_r_kind - 0.42_r_kind*log( r290-tay(3) )  )
  end if

  if( tay(5)<r285 .and. tay(3)<r285 ) then
     alg2 = -1.66_r_kind*(  &
          log( r290-tay(5) ) - 2.90_r_kind - 0.349_r_kind*log( r290-tay(3) )  );
  end if

  if( tay(7)<r285 .and. tay(3)<r285 ) then
     alg3 = -0.44_r_kind*( &
          log( r290-tay(7) ) + 1.60_r_kind - 1.354_r_kind*log( r290-tay(3) )  )
  end if

! Determine clw
  if( alg1 > 0.70_r_kind ) then 
     clw = alg1
     iclwflg=ione
  else
     if( alg2 > 0.28_r_kind ) then 
        clw = alg2
        iclwflg=2_i_kind
     else
        if( tpwc < 30.0_r_kind ) then 
           clw = alg3
           iclwflg=3_i_kind
        else
           clw = alg2
           iclwflg=4_i_kind
        end if
     end if
  end if
! clw qc
  if(clw>6.0_r_kind) clw = 6.0_r_kind
  if(clw < 0.0_r_kind) clw = 0.0_r_kind

  return
end subroutine ret_ssmis

