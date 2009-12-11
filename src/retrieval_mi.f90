  subroutine retrieval_mi(tb,nchanl,ssmi,ssmis, no85GHz,   &
                          tpwc,clw,si85,kraintype,ierr)   
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    retrieval_mi    retrieve clw/tpw and identify rain for SSM/I
!   prgmmr: okamoto          org: np23                date: 2003-12-27
!
! abstract: rain identification based on scattering index 
!           retrieve cloud liquid water (clw) and total precipitable water (tpw) 
!   These algorithm are based on  
!    "SSM/I Algorithm Specification Document, 2000, Raytheon"
!   However, to speed-up, rain rate algorithm is reduced to just 
!   rain identification over sea using scattering index 
!
!   tpwc:: column water vapor over ocean  [kg/m2]
!           range 0-80kg/m2, precision:0.5kg/m2
!          Petty,G.,1993,Proceedings shared processing network 
!           DMSP SSM/I Alogorithm symposium Monterey
!   clw :: column cloud water ove ocean  [kg/m2]
!           range 0-6.0kg/m2, precision:0.5kg/m2
!          Weng et.al.,1997,J.Climate vol.10
!   NOTE!  Assume that this code is run only over sea
!   NOTE!  NOTE! if retrieved clw/tpwc not available over ocean, set -9.99e+11
!
!   NOTE!  This code is applicable to SSM/I only (20Jan2000)
!
! program history log:
!   2003-12-27  okamoto
!   2004-07-12  okamoto - simplify rain identification
!   2005-09-20  sienkiewicz - move tb22v test to avoid negative log evaluation
!   2005-10-20  kazumori - delete amsre
!   2006-04-27  derber - clean up
!   2006-12-20  sienkiewicz - add no85GHz workaround for f08 DMSP
!   2008-04-16  safford - rm unused vars
!
!   input argument list:
!     tb      - Observed brightness temperature [K]
!     nchanl  - number of channels per obs
!     ssmi    - logical true if ssmi is processed
!     ssmis   - logical true if ssmis is processed
!     no85GHz - SSM/I 85GHz channels not used
!
!   output argument list:
!     tpwc    - column water vapor over ocean  [kg/m2]
!     clw     - column water vapor over ocean  [kg/m2]
!     si85    - scattering index over ocean
!     kraintype-rain type
!        [0]no rain or undefine  
!        [1]retrieve by emission -> not available now  
!        [2]by scattering
!     ierr    - error flag
!        [0]pass or escape this subroutine without doing anything
!        [1]tbb gross error  [2]polarization gross erro 
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: ione,two,zero,izero

  implicit none
    
! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: nchanl
  real(r_kind),dimension(nchanl),intent(in   ) :: tb
  logical                       ,intent(in   ) :: ssmi,ssmis, no85GHz

  integer(i_kind)               ,intent(  out) :: kraintype,ierr
  real(r_kind)                  ,intent(  out) :: tpwc,clw
  real(r_kind)                  ,intent(  out) :: si85

! Declare local variables
  real(r_kind)::tbpol(3),tb19v,tb19h,tb22v,tb37v,tb37h,tb85v,tb85h
  real(r_kind)::tpw,tpw0,clw19,clw37,clw85
  real(r_kind)::clw2term
  real(r_kind)::rmis=-9.99e11_r_kind
 
  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r20=20.0_r_kind
  real(r_kind),parameter:: r80=80.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  real(r_kind),parameter:: r290=290.0_r_kind
  real(r_kind),parameter:: r285=285.0_r_kind
 
!  ======  Initialize products to missing

  tpwc   = rmis;  clw = rmis ;  si85 = rmis
  clw19  = zero;  clw37  = zero
  ierr = izero
  kraintype  = izero

  if(ssmi) then
     tb19v = tb(1); tb19h = tb(2); tb22v = tb(3)
     tb37v = tb(4); tb37h = tb(5); tb85v = tb(6); tb85h = tb(7)
  else if(ssmis) then
     tb19v = tb(13); tb19h = tb(12); tb22v = tb(14)
     tb37v = tb(16); tb37h = tb(15); tb85v = tb(17); tb85h = tb(18)
  end if
 
  if(ssmis) return   

 
!  =======  initial QC : gross check ===============

! Gross error check on all channels.  If there are any
! bad channels, skip this obs. 
  if ( no85GHz ) then                ! if no 85GHz, only check ch 1-5
     if ( any(tb(1:5)  < 70.0_r_kind) .or. any(tb(1:5) > 320.0_r_kind ) ) then
        ierr = ione
        return
     endif
  else
     if ( any(tb < 70.0_r_kind) .or. any(tb > 320.0_r_kind ) ) then
        ierr = ione
        return
     end if
  endif
 
! Polarization check based on SSMI/EDR Algorithm Specification Document
  tbpol(1) = tb19v-tb19h     !tb19V-tb19h
  tbpol(2) = tb37v-tb37h     !tb37V-tb37h
  tbpol(3) = tb85v-tb85h     !tb85V-tb85h


  if ( no85GHz ) then                ! if no 85GHz, just check 22,37 GHz
     if (tbpol(1) < -two .or. tbpol(2) < -two) then
        ierr = 2_i_kind
     endif
     
  else if ( any(tbpol < -two ) ) then 
     ierr = 2_i_kind
     return
  end if
 
!
! Use scattering index for rain rate, only if 85V channel can be used.
!    note: this expression ~ Ferraro, et al 1988 JAS
!
  if ( .not. no85GHz ) then

!  =======   Rain Rate  ==============

! Generate rain rates over ocean using Sec 3.6 algorithm
!

!    Compute scattering index
     si85 = -174.4_r_kind + 0.715_r_kind*tb19v +  &
          2.439_r_kind*tb22v - 0.00504_r_kind*tb22v*tb22v - tb85v

     if (si85 >= r10) then
        kraintype=2_i_kind
     end if

  endif

!   Skip emission-based rain rate retrieve process kraintype=1 to speedup 
!   Observations contaminated by emission-based rain are tossed by 
!    later clw-QC

  if(kraintype==izero) then 

!  =======   TPW over ocean (when no rain)  ====================
!   Generate total precipitable water (tpw) using Sec 3.1 algorithm.
!   If rain is present, can not retrieve tpw since squared tb22v
!   term greatly increases error in tpw.  

     tpw = 232.89393_r_kind - 0.148596_r_kind*tb19v - 0.36954_r_kind*tb37v - &
           (1.829125_r_kind - 0.006193_r_kind*tb22v)*tb22v

!   Apply cubic correction for high and low values.
     tpwc = -3.75_r_kind + 1.507_r_kind*tpw - 0.01933_r_kind*(tpw**2) &
           + 0.0002191_r_kind*(tpw**3)
     tpwc = max(zero,tpwc)

!  =======   CLW over ocean (when no rain)  ====================
!   Generate cloud liquid water (clw) using algorithm in Sec 3.2.
!   If rain or sea ice is present, operational algorithm does not
!   generate a product.  

     if (tb22v<r285) then
        clw2term=log(r290-tb22v)
!
!      CLW using 37v as primary channel
        if (tb37v<r285) then 
           clw37 = -1.66_r_kind*( log(r290-tb37v ) -  &
                    2.99_r_kind - 0.32_r_kind*clw2term )
           clw = clw37           ! default if none of the other critera satisfied
        end if

!
!      CLW using 19v as primary channel
        if (tb19v<r285) then 
           clw19  = -3.20_r_kind*( log(r290-tb19v) -  &
                     2.84_r_kind - 0.4_r_kind*clw2term  ) 
           if (clw19 > 0.7_r_kind) then 
              clw = clw19
           else if ( .not. no85GHz .and. &
                clw37 <= 0.28_r_kind .and. tb85h<r285) then 
!
!          CLW using 85h as primary channel
              clw85 = -0.44_r_kind*( log(r290-tb85h) + & 
                       1.11_r_kind - 1.26_r_kind*clw2term   )
              tpw0 = tpwc 
              if (tpw0 < 30.0_r_kind .and. clw85>zero ) then 
                 clw = clw85
              end if
           end if
        end if

     end if

!    upper limit of 6.0 kg/m2.  
     clw=min(clw,6.0_r_kind)
!    if(clw>6.0_r_kind) clw=zero

  end if                    
 
  return
end subroutine retrieval_mi
