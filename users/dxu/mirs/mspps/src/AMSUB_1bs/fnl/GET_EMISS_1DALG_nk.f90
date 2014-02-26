!--------------------------------------------------------------------------------
!M+
! NAME:
!    GET_EMISS_1DALG 
!
! PURPOSE:
!       Module containing routines for retrieving emissivity over cold surfaces from
! AMSU measurements at window channels  
!
!
! CALLING SEQUENCE:
!       USE GET_EMISS_1DALG
!
! MODULES:
!
! CONTAINS:
!
!
! EXTERNALS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     BANGHUA YAN, NOAA/NESDIS, JCSDA, Nov., 26, 2006
!
!--------------------------------------------------------------------------------


MODULE GET_EMISS_1DALG

USE CLOUD_OPT_CAL

INTEGER(2), PARAMETER :: SUCCESS = 1,FAILURE = -1
REAL(4), PARAMETER :: MISSING_DATA = -999.0
INTEGER(2), PARAMETER :: AMSU_A = 1, AMSU_B = 2, AMSU_AB = 3
INTEGER(2) :: error_status

PUBLIC :: EMISS_1DVAR_ALG

PUBLIC :: SNOWEM_ESTIMATE
PUBLIC :: tb_bias_remove
PUBLIC :: tb_asymmetry_correction

CONTAINS
 

SUBROUTINE EMISS_1DVAR_ALG(tbo,tb89b,theta_a,theta_b,emiss,ts,tc,iwp,tpw,de)

INTEGER(2), PARAMETER :: nch= 6
REAL(4) :: theta,theta_a,theta_b,tc,iwp,tpw,de,ts
REAL(4), DIMENSION(nch) :: tbo,emiss
REAL(4), DIMENSION(11) :: xou
REAL(4) :: tb89b

INTEGER(2) :: RETRIEVAL_TYPE

! SET RETRIEVAL TYPE

     RETRIEVAL_TYPE = AMSU_A

     tc = MISSING_DATA

     xou(1:11) = MISSING_DATA
  
     RETRIEVAL_TYPE = AMSU_AB

     CALL EMISS_1DVAR_ALG_OPTION(RETRIEVAL_TYPE,tbo,tb89b,theta_a,theta_b,ts,tpw,tc,xou)

     emiss(1:nch) = xou(1:nch)
     tpw          = xou(nch+1)
     iwp          = xou(nch+2)
     de           = xou(nch+3)
     tc           = xou(nch+4)
     ts           = xou(nch+5)

END SUBROUTINE EMISS_1DVAR_ALG



SUBROUTINE EMISS_1DVAR_ALG_OPTION(RETRIEVAL_TYPE,tbo,tb89b_nc,theta_a,theta_b,ts,tpw,tc,xou)

INTEGER(2), PARAMETER :: nvar = 11,nch= 6, nwch = 5
INTEGER(2)  :: particle_shape_index,retrieval_status   
REAL(4), DIMENSION(*) :: tbo
REAL(4) :: theta,theta_a,theta_b,tc,iwp,tpw,de,ts,tb89b_nc,tb89b_c
REAL(4) :: tc_init,iwp_init,iwp_init2,de_init,tpw_init,ts_init
REAL(4), DIMENSION(nch)  :: emiss,emiss_init,tb_c
REAL(4), DIMENSION(nwch) :: tb5, emiss5 ! only 23.8 to 150 GHz
REAL(4), DIMENSION(nvar) :: xin, xou
INTEGER(2) :: RETRIEVAL_TYPE

! INITIALIZATION
   IF(ts /= MISSING_DATA) ts_init = ts
   IF(tpw /=MISSING_DATA) tpw_init = tpw
   tb89b_c = tb89b_nc

     particle_shape_index = 0 
     retrieval_status  = SUCCESS


     tb5(1:nwch) = tbo(1:nwch)
     tb_c(1:nch) = tbo(1:nch)

! Bias corrections

     CALL tb_asymmetry_correction(theta_a,theta_b,tb_c,tb89b_c)

! Compute tc from asymmetry_correction_tb
  
      IF(tc /=MISSING_DATA)  THEN
       tc_init  = tc
      ELSE
       CALL tc_predictor(tb5,tc_init)
      ENDIF
     
    tb5(1:5) = tb_c(1:5)


! Remove bias between FullRTM and OnelayerRTM

     CALL tb_bias_remove(tb_c,tb89b_c)

! Estimate the first guess in retrieved variables (11)
!em23,em31,em50,em89,em150,em183+-7,tpw,icw,De,Tc,ts

! Calculate emissivity for the first guess

    if (xou(1) /= MISSING_DATA) then
       emiss_init(1:nch) = xou(1:nch)
       tpw_init          = xou(nch+1)
       iwp_init          = xou(nch+2)
       de_init           = xou(nch+3)
       tc_init           = xou(nch+4)
       ts_init           = xou(nch+5)
     else 

!      tb5(1:5) = tbo(1:5)      

       call SNOWEM_ESTIMATE(tb5, ts_init,emiss5)
       emiss_init(1:nwch) = emiss5(1:nwch)
       emiss_init(nch)   = emiss5(nwch)
       iwp_init = 0.1  !!!!!!!!!! first guess (maybe modified by calling sub iwp_estimate)
       de_init  = 0.05  !!!!!!!!!!! first guess
       CALL iwp_estimate(tbo,theta_b,iwp_init2)

       if (iwp_init2 <=0.25 .and. iwp_init2 >= 0.0) iwp_init = iwp_init2

    endif

    xin(1:nch) =  emiss_init(1:nch)
    xin(nch+1) =  tpw_init
    xin(nch+2) =  iwp_init
    xin(nch+3) =  de_init
    xin(nch+4) =  tc_init
    xin(nch+5) =  ts_init

! INITIALIZATION

     emiss(1:nch) = emiss_init(1:nch)
     ts = ts_init
     tc = tc_init
     iwp = iwp_init
     de  = de_init
     tpw = tpw_init

    call AMSU_CLOUDEMS_ALGORITHM(RETRIEVAL_TYPE,particle_shape_index,theta_a,theta_b,   &
                                 tb_c,tb89b_c,xin,  &
                                 xou,retrieval_status)
  tpw          = xou(nch+1)
  iwp          = xou(nch+2)
  de           = xou(nch+3)
  tc           = xou(nch+4)
  ts           = xou(nch+5)

  RETURN

END SUBROUTINE EMISS_1DVAR_ALG_OPTION




!*****************************************************************************************
! Retrieve cloud liquid water and total water vapor contents
!

  Subroutine AMSU_CLOUDEMS_ALGORITHM(RETRIEVAL_TYPE,ice_shape_index,theta_a,theta_b,  &
                                     tb,tb89b_c,xin,&
                                     xou,retrieval_status)

! Programmer:
!
!     Banghua Yan and Fuzhong Weng   ORG: NESDIS              Date: 2006-01-24
!
! Abstract:
!
!     Retrieve cloud water path, total precipitable water, cloud particle effective diameter over lands
!              from AMSU measurements
!
!     Please refer to the following papers for details
!
!  (1) Weng. F. and N. C. Grody, 1994: Retrieval of cloud liquid water using the special
!
!      sensor microwave imager (SSM/I), J. Geophys. Res., 99, 25,535 -25, 551.
!
!  (2) Weng. F., L. Zhao, R. R. Ferraro, G. Poe, X. Li., and N. Grody, 2003:
!      Advanced microwave sounding unit cloud and precipitation algorithms, Radio Science, 38,
!
!      Mar 33, 1-12.
!
!  (3) Yan, B. and F. Weng,2004: "Rain and cloud water paths derived from Aqua AMSR-E measurements',
!
!      the 13th conference on satellite meteorolgy and oceanography, Norfolk in VA, Sept. 20-23.
!
!  (4) Yan, B. and F. Weng: New applications of AMSR-E measurements under tropical cyclones,
!
!      Part I: retrievals of sea surface temperature and wind speed
!
!      Part II: retrieval of liquid water path
!
!      to be submitted to J. Geophys. Res., 2005.
!
!
! Program history log:
!
!                                                            :  10/22/04
!      beta evrsion : 01/24/06
!
! Input argument list:
!
!    tbo(1): Vertically polarized AMSR-E brighness temperature at 23.8 GHz
!    tbo(2):                                                      31.4 GHz
!    tbo(3):                                                      50.3 GHz
!    tbo(4):                                                      89.0 GHz
!    tbo(5):                                                      150.0  GHz
!    tbo(6):                                                      183+/-7  GHz
!
!    theta  : local zenith angle in degree

!    DTB_max: A constraint for the maximum bias between observed and simulated TB
!
! Important internal argument list
!
!    x(1)   : initialized sea surface temperature
!    x(2)   :                         wind
!    x(3)   :              up-(down) welling brightness temperature at 6.925 GHz
!    x(4)   :                                                          10.65 GHz
!    ta     : up-(down) welling brightness temperature
!    xi     : atmospheric transmittance
!
! ice_shape_index : classification of ice particle shape
!
! ice_shape_index = 0: sphere
! ice_shape_index = 1: Rosettes
! ice_shape_index = 2: Type-A snowflakes
! ice_shape_index = 3: Type-B snowflakes
! ice_shape_index = 4: cylindrical column; 
! ice_shape_index = 5: 2-cylinder aggregate; 
! ice_shape_index = 6: 3-cylinder aggregate; 
! ice_shape_index = 7: 4-cylinder aggregate
!
! Output argument lists
!
!    ts : sea surface temperature   (K)
!
!    wind : sea surface wind        (m/s)
!
! Code History:
!
! Beta version OF 1DVAR approach using TB from 23 to 150 GHz: Banghua Yan (2005)
!
! added 183 GHz : Huan Meng (2006)
!
! added non-spheric particle calculations : Huan Meng (10/28/2006)
!
! Removed the bugs in the subroutines related to the non-spheric particles: B. Yan (10/30/2006)
!
! added a classification algorithm of ice particles  according to the shape 
!
!       from the cloud temperature : H. Meng (11/?/2006)
!
! added a classification algorithm of ice particles  according to the shape 
!
!       from AMSU TB: B. Yan and H. Meng (11/?/2006)

! Remarks:
!
!  Questions/comments: Please send to Fuzhong.Weng@noaa.gov and Banghua.Yan@noaa.gov
!
! Attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!---------------------------------------------------------------------------

integer, parameter :: m = 6, n = 11

integer(2) :: ice_shape_index,retrieval_status,RETRIEVAL_TYPE

real*4  kv(m),tauo(m),ko2_coe(m,3)

real*4  tb89b_c,ts,tpw,xin(*),xou(*),ICW,V,L,De,tb(m)

real*4  freq(m),em(m),xb(n),emissivity,theta_a,theta_b,theta,angle,mu,ev,eh

real*4  conv_test, conv_crit,DTB_max

real*4  error_sum1,error_sum2,gL,ssalbL,tauL,g,ssalb,tau,tau_nl,tsv,tsh,tc

integer ncoe,i,j,ich,iter,iter_max,dpol(m),ij_index

real*4, dimension(n,n) :: BX

real*4, allocatable,dimension(:)::  y, y_s, x, x_ap, delta_x,dy

real*4, allocatable,dimension(:,:):: A, E

real*4, allocatable,dimension(:,:):: Sa, Sa_inv,Sy, Sy_inv


data freq/23.8, 31.4, 50.3, 89.0, 150.0, 183.31/

! coefficients

data  kv/5.277882e-03,1.924828e-03,3.200650e-03,9.528114e-03,3.051224e-02,1.524409e-01/ ! For 183.31+/-7 uses the average of 176.31 and 190.31

data  (ko2_coe(1,i),i=1,3)/4.021785e-02, -8.845772e-05,  1.174458e-08/ 
data  (ko2_coe(2,i),i=1,3)/6.703062e-02, -1.496973e-04,  2.370985e-08/
data  (ko2_coe(3,i),i=1,3)/7.961397e-01, -1.719601e-03,  3.014933e-07/
data  (ko2_coe(4,i),i=1,3)/1.287606e-01, -3.504918e-04,  1.617206e-07/
data  (ko2_coe(5,i),i=1,3)/4.422645e-02, -1.559169e-04,  1.310151e-07/
data  (ko2_coe(6,i),i=1,3)/2.003414e-02, -7.273177e-05,  6.143024e-08/  ! for 183.31+/-7 use the average of 176.31 and 190.31

! covariance matrix of background error

data (BX(1,i),i=1,n)/0.00772,  0.00609,  0.00359,  0.00201,  0.00299,  0.00299,.0,.0,.0,.0,.0/
data (BX(2,i),i=1,n)/0.00609,  0.00539,  0.00399,  0.00266,  0.00286,  0.00286,.0,.0,.0,.0,.0/
data (BX(3,i),i=1,n)/0.00359,  0.00399,  0.00452,  0.00372,  0.00317,  0.00317,.0,.0,.0,.0,.0/
data (BX(4,i),i=1,n)/0.00201,  0.00266,  0.00372,  0.00441,  0.00409,  0.00409,.0,.0,.0,.0,.0/
data (BX(5,i),i=1,n)/0.00299,  0.00286,  0.00317,  0.00409,  0.00553,  0.00553,.0,.0,.0,.0,.0/
data (BX(6,i),i=1,n)/0.00299,  0.00286,  0.00317,  0.00409,  0.00553,  0.00553,.0,.0,.0,.0,.0/
data (BX(7,i),i=1,n)/.0,.0,.0,.0,.0,.0,0.0002,.0,.0,.0,.0/
data (BX(8,i),i=1,n)/.0,.0,.0,.0,.0,.0,.0,0.0005,.0,.0,.0/
data (BX(9,i),i=1,n)/.0,.0,.0,.0,.0,.0,.0,.0,0.001,.0,.0/
data (BX(10,i),i=1,n)/.0,.0,.0,.0,.0,.0,.0,.0,.0,.0010,.0/
data (BX(11,i),i=1,n)/.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,0.0025/

allocate (y(m),y_s(m), x(n), x_ap(n), delta_x(n),dy(m))

allocate (A(m, n), E(n, n))

allocate (Sa(n,n),Sa_inv(n, n),Sy(n,n),Sy_inv(n,n))

! xin: em23,em31,em50,em89,em150,em183+/-7,tpw,icw,De,te,ts

  em(1:m) =  xin(1:m)

  ts      = xin(n)   !

  retrieval_status = SUCCESS 

! Constants

  iter_max = 6

  if (RETRIEVAL_TYPE == AMSU_A) theta = theta_a

  if (RETRIEVAL_TYPE == AMSU_B) theta = theta_b

  if (RETRIEVAL_TYPE == AMSU_AB) theta = 0.5*(theta_a + theta_b)

  angle = theta*3.14159/180.0

  mu = cos(angle)

! Calculate KW and KL and tau_o2(taut) and emissivity

 do ich = 1, m

     tauo(ich) = ko2_coe(ich,1) +  ko2_coe(ich,2)*ts +  ko2_coe(ich,3)*ts*ts

 enddo

! Initialization

     y = 0.0

     f = 0.0
     x = 0.0

     x_ap = 0.0


     delta_x = 0.0

     Sa = 0.0

     Sy_inv = 0.0

     A = 0.0

     A_tran = 0.0

     E = 0.0

     Dy = 0.0

     DTB_max = 2.5

! Observations

     y = tb

     if (RETRIEVAL_TYPE == AMSU_B) y(4) = tb89b_c

     if (RETRIEVAL_TYPE == AMSU_AB) y(4) = 0.5*(y(4) + tb89b_c)

! 03/09/07

     tb(1:m) = y(1:m)

! 03/09/07: TWOSTREAM_RTM_CORRECTION
     
  


! INITIALIZE

! xin: em23,em31,em50,em89,em150,em183+/-7,tpw,icw,De,tc,ts

    xb(1:n) = xin(1:n) !n=m+4, m=# of channels

    tpw = xin(m+1)
! Define measurement error covariance matrix

 do ich=1,n

    E(ich,ich) = 0.5  !0.25

 enddo

! Set up a priori conditions

     x_ap = xb

!Initialize the vector of retrievables

     x = x_ap

!Start of retrieval loop

     iter = 0

 RETRIEVAL_LOOP: DO

! Calculate em(m)

  em(1:m) =  x(1:m)

! Added a classification algorithm : to be done

!  CALL ICEPARTICLE_SHAPE(tc,tb,ice_shape_index)

! Initialization

! ice_shape_index = 0

 do ich = 1, m

    frequency = freq(ich)

    emissivity = em(ich)

    tc = x(m+4)

!OPTIC(rwp,de,tc,frequency,ssalb,gg,tau)

! x: em23,em31,em50,em89,em150,em183+/-7,tpw,icw,de,tc

!   call OPTIC(ice_shape_index,x(m+2),x(m+3),tc,frequency,ssalbL,gL,tauL)

   CALL CLOUD_OPTOUT(frequency,tc,x(m+2),x(m+3),tauL,ssalbL,gL)

! Weighted optical parameters

   tau_nl = tauo(ich) + kv(ich)*tpw

   tau    = tauo(ich) + kv(ich)*tpw + tauL

   ssalb = tauL *ssalbL/ tau

   g     = tauL * gL /tau

   theta = theta_a


! 03/09/07: TWOSTREAM_RTM_CORRECTION

    CALL TB_2RTM_CORRECTION(m,ich,theta,tau,tb,y)

   if ( ich >=4 .and. RETRIEVAL_TYPE /= AMSU_A) theta = theta_b

   angle = theta*3.14159/180.0

   mu = cos(angle)

   call two_stream_RTsolution(mu,tc,ts,emissivity,g,ssalb,tau,tau_nl,tbs)

   y_s(ich) = tbs


   dy(ich) = y(ich) - y_s(ich)

!   if ( ich >=4 .and. RETRIEVAL_TYPE == AMSU_B) then
!      write(*,1000)iter,ich,y(ich),y_s(ich),x(1:10)
!   else
!      if (ich <=4) write(*,1000)iter,ich,y(ich),y_s(ich),x(1:10)
!   endif

!   write(*,1000)iter,ich,y(ich),y_s(ich),x(1:11)

 enddo

!  pause

  iter = iter + 1


!   if(maxval(abs(dy(1:5))) < DTB_max .and. abs(dy(6)) < (DTB_max+1.5)) then
   

   ij_index=1
   IF (RETRIEVAL_TYPE == AMSU_A) THEN
      if(maxval(abs(dy(1:4))) > DTB_max) ij_index=0
   ELSE
      IF (RETRIEVAL_TYPE == AMSU_B) THEN 
         if(maxval(abs(dy(5:5))) > DTB_max) ij_index=0
      ELSE
         if(maxval(abs(dy(1:5))) > DTB_max) ij_index=0
      ENDIF
   ENDIF

   if(abs(dy(6)) > DTB_max+1.5) ij_index=0

   if(ij_index == 1) exit 

! Determine A matrix   (note: em(ich) <- ev-components

  call get_AAmatrix(RETRIEVAL_TYPE,ice_shape_index,m,n,theta,em,tc,ts,x,A)

! Determine Sa (= A_Tran*A + E) and its inverse matrix
  Sa = ( matmul(transpose(A),A) +  E )
  call gen_inverse(Sa, Sa_inv,n,n,ierr)
  delta_x = matmul( matmul(Sa_inv, transpose(A)), dy )

!  Sa =  matmul( matmul(A,BX),transpose(A)) + E)
!  call gen_inverse(Sa, Sa_inv,n,n,ierr)
! Get new delta_x
!  delta_x = matmul(matmul( matmul(BX,transpose(A)),Sa_inv ), dy )

! Adjust delta_x

      do j = 1, n

       if(abs(delta_x(j)) > abs(x(j))/2.0) then

         delta_x(j) = sign(x(j)/2.0, delta_x(j))

       endif

      end do

! new constraint


! if( maxval(abs(delta_x)) < 0.01 ) then

!    print *, "convergence achieved with measurements", iter, iter_max
!    print *, "x = ", x
!    exit

! endif



! Update retrieved vector

     x = x + delta_x
     if((x(6)-x(5)) .gt. 0.1) x(6) = x(5) + 0.1
     if (x(6)+0.1 <= x(5)) x(6) = x(5) -  0.1

! quality check
! x: em23,em31,em50,em89,em150,em183+/-7,tpw,icw,de,tc

  do ich = 1, m

     if (x(ich) .ge. 1.0) x(ich) = 1.0

     if (x(ich) .le. 0.4) x(ich) = 0.4

 enddo

     if (x(m+2) .le. 0.0)  x(m+2) = 0.0

     if (x(m+3) .le. 0.005) x(m+3) = 0.005

!     if (x(m+4) .ge. (ts-8.0)) x(m+4) = ts - 8.0


! Not convergent

     if (iter == iter_max) then
        print*,'not convergent!'
        retrieval_status = FAILURE
        x = -999.0
        exit
     endif

 END DO RETRIEVAL_LOOP


! retrieval output is from x

  xou(1:n) =  x(1:n)

!  print*,'tpw,icw,de:',xou(7:9)

!  pause

  deallocate (y,y_s, x, x_ap, delta_x,dy)
  deallocate (A, E)
  deallocate (Sa,Sa_inv,Sy,Sy_inv)

 1000 format(2(i3,1x),18(f7.2,1x))

 end subroutine AMSU_CLOUDEMS_ALGORITHM

 SUBROUTINE TB_2RTM_CORRECTION(m,ich,theta,tau,tb,y)

 REAL*4, PARAMETER :: tau_c = 0.4,theta_c = 50.0
 INTEGER,INTENT(IN) :: m
 INTEGER :: ich
 REAL*4, DIMENSION(m) :: tb,y
 REAL*4     :: theta,tau,dtb

 ! Initialization
   y(1:m) = tb(1:m)
   dtb = 0.0

 ! UPDATE
   if (theta <= theta_c) then
      if (tau < tau_c) then
          dtb = (4.84+0.0084*theta-0.0021*theta*theta)*tau/tau_c
      else
          dtb = 4.84+0.0084*theta-0.0021*theta*theta
      endif
   endif
   y(ich) = y(ich) + dtb
   
 END SUBROUTINE TB_2RTM_CORRECTION


!----------------------------------------------------------------------------

  Subroutine two_stream_RTsolution(mu,tc,ts,em,g,ssalb,tau,tau_nl,TB)


    real*4 i0, tc,ts, mu, g,ssalb,tau,gv, gh, ssalb_h, ssalb_v, tau_h,tau_v, tau_nl

    real*4 r23_h, r23_v,em,ev,eh

    real*4 alfa_v, alfa_h, kk_h, kk_v, gamma_h, gamma_v, beta_v, beta_h

    real*4 gamma1_v,gamma2_v,gamma3_v,gamma4_v,gamma1_h,gamma2_h,gamma3_h,gamma4_h

    real*4 TV_D1,TV_D2,TV_D3,TH_D1,TH_D2,TH_D3,TV,TH,TB

! Initializations for various parameters

       i0 = 0.0 !/2.7

       ev = em

       r23_v = 1.0 - ev

       gv = g

       ssalb_v = ssalb

       tau_v = tau

       alfa_v = sqrt((1.0-ssalb_v)/(1.0 - gv*ssalb_v))

       kk_v = sqrt ((1.0-ssalb_v)*(1.0 - gv*ssalb_v))/mu

       beta_v = (1.0 - alfa_v)/(1.0 + alfa_v)

       gamma_v = (beta_v -r23_v)/(1.0-beta_v*r23_v)

       gamma1_v = 1.0 - r23_v/beta_v

       gamma2_v = 1.0 - r23_v*beta_v

       gamma3_v = 1.0/beta_v - r23_v

       gamma4_v = beta_v - r23_v


! Compute TV

       if ( ( (tau-tau_nl) .ge. 0.005 ) .and. (ssalb .ge. 0.001))  then


             TV_D1 = (i0-tc)*( gamma1_v*exp(-kk_v*tau_v) - gamma2_v*exp(kk_v*tau_v) )

             TV_D2 = (ev*ts  - ev*tc )*(1.0/beta_v - beta_v)

             TV_D3 = gamma4_v*exp(-kk_v*tau_v) -  gamma3_v*exp(kk_v*tau_v)


             TV = (TV_D1 - TV_D2)/TV_D3 + tc

      else


             TV =i0*(1.0-ev)*exp(-2.0*tau_v/mu) + tc*( 1.0-(1.0-ev)*exp(-2.0*tau_v/mu) ) +  &

                 ( ev*ts - ev*tc)*exp(-tau_v/mu)

      endif

     TB = TV

    return

    End  Subroutine two_stream_RTsolution



subroutine get_AAmatrix(RETRIEVAL_TYPE,ice_shape_index,m,n,theta,em,tc,ts,xb,A)

parameter (nch=6)

INTEGER(2) :: ice_shape_index

real*4 xb(*),em(*),A(m,n)

real*4 i0,ts,tc,frequency,emissivity,theta,angle,mu

real*4 tpw,V,rwp,de,ssalbL,tauL,gL,ssalb,tau,g,dssalbL_dL,dssalbL_dD,dtauL_dL,dtauL_dD,dgL_dL,dgL_dD

real*4 dTB_dV,dTB_dL,dTB_dD,dTB_dem,dTB_dTC,dTB_dTS

real*4 freq(nch),kv(nch),tau_nl(nch),tauo(nch),ko2_coe(nch,3)

integer(2) :: RETRIEVAL_TYPE

data freq/23.8, 31.4, 50.3, 89.0, 150.0, 183.31/

data  kv/5.277882e-03,1.924828e-03,3.200650e-03,9.528114e-03,3.051224e-02,1.524409e-01/

data  (ko2_coe(1,i),i=1,3)/4.021785e-02, -8.845772e-05,  1.174458e-08/
data  (ko2_coe(2,i),i=1,3)/6.703062e-02, -1.496973e-04,  2.370985e-08/
data  (ko2_coe(3,i),i=1,3)/7.961397e-01, -1.719601e-03,  3.014933e-07/
data  (ko2_coe(4,i),i=1,3)/1.287606e-01, -3.504918e-04,  1.617206e-07/
data  (ko2_coe(5,i),i=1,3)/4.422645e-02, -1.559169e-04,  1.310151e-07/
data  (ko2_coe(6,i),i=1,3)/2.003414e-02, -7.273177e-05,  6.143024e-08/

! Initialization for A

  A =  0.0


! Get inputs
! xin: em23,em31,em50,em89,em150,em183+/-7,tpw,icw,De,te,ts


 V   = xb(m+1)

 rwp = xb(m+2)

 de  = xb(m+3)

 i0 = 0.0

 angle = theta*3.14159/180.0

 mu = cos(angle)



! Calculate KW and KL and tau_o2(taut) and emissivity


 do ich = 1, m

     tauo(ich) = ko2_coe(ich,1) +  ko2_coe(ich,2)*ts +  ko2_coe(ich,3)*ts*ts

     tau_nl(ich) = tauo(ich) + kv(ich) * V

 enddo

 do ich = 1, m

     frequency  = freq(ich)

     emissivity = em(ich)

! compute optical parameters

     call optic_dLD(ice_shape_index,frequency,theta,tc,kv(ich),tauo(ich),V,rwp,de,   &
	 
	                ssalbL,tauL,gL,ssalb,tau,g,dssalbL_dL,dssalbL_dD,dtauL_dL,       &
					
					dtauL_dD,dgL_dL,dgL_dD)


! compute variation ratio


! Scattering atmosphere
    if (abs(tau_nl(ich) - tau) .ge. 0.005 .and. (ssalb .ge. 0.001) ) then

        call Amatrix_Freq(frequency,theta,emissivity,i0,tc,ts,kv(ich),gL,ssalbL,tauL,        &

                        dssalbL_dL,dssalbL_dD,dtauL_dL,dtauL_dD,dgL_dL,dgL_dD,       &

                        g,ssalb,tau,dTB_dV,dTB_dL,dTB_dD,dTB_dem,dTB_dTC,dTB_dTS)

    else

        call Amatrix_emissFreq(frequency,theta,emissivity,i0,tc,ts,kv(ich),gL,ssalbL,tauL,        &

                        dssalbL_dL,dssalbL_dD,dtauL_dL,dtauL_dD,dgL_dL,dgL_dD,       &

                        g,ssalb,tau,dTB_dV,dTB_dL,dTB_dD,dTB_dem,dTB_dTC,dTB_dTS)
    endif

! xin: em23,em31,em50,em89,em150,em183+-7,tpw,icw,De,te,ts

   do j = 1, n

      A(ich,j) = 0.0

      IF (RETRIEVAL_TYPE == AMSU_A) THEN
          if (j .le. 3 .and. ich .eq. j) A(ich,j) = dTB_dem
 !         if (j .eq. (m+1)) A(ich,m+1) = dTB_dV
          if (j .eq. (m+2)) A(ich,m+2) = dTB_dL
 !         if (j .eq. (m+3)) A(ich,m+3) = dTB_dD
 !         if (j .eq. (m+4)) A(ich,m+4) = dTB_dTC
 !         if (j .eq. (m+5)) A(ich,m+5) = dTB_dTS
      ELSE
           if (RETRIEVAL_TYPE == AMSU_B) then
               if (j .ge. 5 .and. ich .eq. j) A(ich,j) = dTB_dem
!              if (j .eq. (m+1)) A(ich,m+1) = dTB_dV
               if (j .eq. (m+2)) A(ich,m+2) = dTB_dL
               if (j .eq. (m+3)) A(ich,m+3) = dTB_dD
           else
               if (j .le. 5 .and. ich .eq. j) A(ich,j) = dTB_dem
               if (j .eq. (m+1)) A(ich,m+1) = dTB_dV
               if (j .eq. (m+2)) A(ich,m+2) = dTB_dL
               if (j .eq. (m+3)) A(ich,m+3) = dTB_dD
               if (j .eq. (m+4)) A(ich,m+4) = dTB_dTC
               if (j .eq. (m+5)) A(ich,m+5) = dTB_dTS
           endif
      ENDIF

   enddo

  enddo   ! all channels end!

  end  subroutine get_AAmatrix



  subroutine optic_dLD(ice_shape_index,frequency,theta,tc,kv,tauo,V,rwp,de,  &
       
	                   ssalbL,tauL,gL,ssalb,tau,g,           &

                       dssalbL_dL,dssalbL_dD,dtauL_dL,dtauL_dD,dgL_dL,dgL_dD)

real*4 frequency,theta,kv,tc,tauo,V,rwp,de,ssalbL,tauL,gL,ssalb,tau,g

real*4 dssalbL_dL,dssalbL_dD,dtauL_dL,dtauL_dD,dgL_dL,dgL_dD

real*4 xin,xout1,xout2,xout3

integer(2) ice_shape_index

! Compute optical parameters of rain drops using Mie theory

!  call OPTIC(ice_shape_index,rwp,de,tc,frequency,ssalbL,gL,tauL)

   CALL CLOUD_OPTOUT(frequency,tc,rwp,de,tauL,ssalbL,gL)

! over RWP

  xin  = rwp + 0.001

!  call OPTIC(ice_shape_index,xin,de,tc,frequency,xout1,xout2,xout3)

   CALL CLOUD_OPTOUT(frequency,tc,xin,de,xout3,xout1,xout2)

  dssalbL_dL = (xout1 - ssalbL)/0.001

  dgL_dL     = (xout2 - gL)/0.001

  dtauL_dL   = (xout3 - tauL)/0.001


! over De

  xin  = de + 0.0001

!  call OPTIC(ice_shape_index,rwp,xin,tc,frequency,xout1,xout2,xout3)

   CALL CLOUD_OPTOUT(frequency,tc,rwp,xin,xout3,xout1,xout2)


  dssalbL_dD = (xout1 - ssalbL)/0.0001

  dgL_dD     = (xout2 - gL)/0.0001

  dtauL_dD   = (xout3 - tauL)/0.0001


! Weighted optical parameters


   tau   = tauo + kv*V + tauL

   ssalb = tauL *ssalbL/ tau

   g     = tauL * gL /tau


end subroutine optic_dLD




subroutine Amatrix_Freq(frequency,theta,em,i0,tc,ts,kv,gL,ssalbL,tauL,               &
                        dssalbL_dL,dssalbL_dD,dtauL_dL,dtauL_dD,dgL_dL,dgL_dD,       &
                        g,ssalb,tau,dTB_dV,dTB_dL,dTB_dD,dTB_dem,dTB_dTC,dTB_dTS)


real*4 frequency,theta,em,i0,tc,ts,kv,gL,ssalbL,tauL,               &

       dssalbL_dL,dssalbL_dD,dtauL_dL,dtauL_dD,dgL_dL,dgL_dD,       &

       g,ssalb,tau,dTB_dV,dTB_dL,dTB_dD,dTB_dem,dTB_dTC,dTB_dTS

! CORRECTION (12/08/06): USE REAL*8

real*8 mu,r23,a,kk,beta,gamma,gamma1,gamma2,gamma3,gamma4,TB1,TB2,F1,F2

real*8 dssalb_dV,dssalb_dL,dssalb_dD,dtau_dV,dtau_dL,dtau_dD,dg_dV,dg_dL,dg_dD

real*8 dgamma1_dbeta,dgamma2_dbeta,dgamma3_dbeta,dgamma4_dbeta

real*8 dgamma1_dV,dgamma2_dV,dgamma3_dV,dgamma4_dV

real*8 dgamma1_dL,dgamma2_dL,dgamma3_dL,dgamma4_dL

real*8 dgamma1_dD,dgamma2_dD,dgamma3_dD,dgamma4_dD

real*8 da_dssalb,da_dg,dkk_dssalb,dkk_dg,dbeta_da,dbeta_dssalb,dbeta_dg

real*8 dkk_dV,dkk_dL,dkk_dD, dbeta_dV,dbeta_dL,dbeta_dD

real*8 X11,X22,X33,dF1_dV,dF2_dV,dF1_dL,dF2_dL,dF1_dD,dF2_dD


!(1)

! Initializations for various parameters

    i0 = 0.0  !2.7

    mu = cos(theta*3.14159/180.0)

    r23 = 1.0 - em

    a = sqrt((1.0 - ssalb)/(1.0 - g*ssalb))


    kk = sqrt ((1.0 - ssalb)*(1.0 -  g*ssalb))/mu

    beta = (1.0 - a)/(1.0 + a)

    gamma = (beta -r23)/(1.0-beta*r23)


    gamma1 = 1.0 - r23/beta

    gamma2 = 1.0 - r23*beta

    gamma3 = 1.0/beta - r23

    gamma4 = beta - r23


! Compute variables related to TB  (= F1/F2 + tc)

    TB1 = (i0-tc)*( gamma1*exp(-kk*tau) - gamma2*exp(kk*tau) )

    TB2 = (em*ts - em*tc )*(1.0/beta - beta)

    F1 = TB1 - TB2

    F2 = gamma4*exp(-kk*tau) -  gamma3*exp(kk*tau)



! *** Compute levelA derivatives


   dtau_dV = kv

   dtau_dL = dtauL_dL

   dtau_dD = dtauL_dD



   dssalb_dV = - (tauL*ssalbL*kv)/tau/tau

   dssalb_dL = (dtauL_dL*tau -  dtau_dL*tauL)*ssalbL/tau/tau + tauL/tau*dssalbL_dL

   dssalb_dD = (dtauL_dD*tau -  dtau_dD*tauL)*ssalbL/tau/tau + tauL/tau*dssalbL_dD

   dg_dV = - (tauL*gL*kv)/tau/tau

   dg_dL = (dtauL_dL*tau -  dtau_dL*tauL)*gL/tau/tau + tauL/tau*dgL_dL

   dg_dD = (dtauL_dD*tau -  dtau_dD*tauL)*gL/tau/tau + tauL/tau*dgL_dD

!

   dgamma1_dbeta = (1.0 - em)/beta/beta

   dgamma2_dbeta = -(1.0 - em)

   dgamma3_dbeta = -1.0/beta/beta

   dgamma4_dbeta = 1.0


!
   dbeta_da  = -2.0/(1.0+a)**2


   da_dssalb = -(1.0-g)/( 2.0*(1.0-ssalb)**0.5*(1.0-ssalb*g)**1.5 )

   da_dg     = (ssalb*(1.0-ssalb)**0.5)/(2.0*(1.0-ssalb*g)**1.5)



   dkk_dssalb =  (2.0*ssalb*g-(1.0+g))/( 2.0*mu* ((1.0-ssalb)*(1.0-ssalb*g))**0.5 )

   dkk_dg     = -(ssalb*(1.0-ssalb)**0.5)/( 2.0*mu*(1.0-ssalb*g)**0.5 )



! these two terms are not necessary?

!   dbeta_dssalb = (1.0-g)/( (1.0+a)**2.0*(1.0-ssalb)**0.5*(1.0-ssalb)**1.5 )

!   dbeta_dg     = - (ssalb*(1.0-ssalb)**0.5)/( (1.0+a)**2*(1.0-ssalb*g)**1.5 )



! *** Compute levelB derivatives

   dkk_dV = dkk_dssalb*dssalb_dV + dkk_dg*dg_dV

   dkk_dL = dkk_dssalb*dssalb_dL + dkk_dg*dg_dL

   dkk_dD = dkk_dssalb*dssalb_dD + dkk_dg*dg_dD


   dbeta_dV = dbeta_da*(da_dssalb *dssalb_dV + da_dg*dg_dV)

   dbeta_dL = dbeta_da*(da_dssalb *dssalb_dL + da_dg*dg_dL)

   dbeta_dD = dbeta_da*(da_dssalb *dssalb_dD + da_dg*dg_dD)



   dgamma1_dV = dgamma1_dbeta*dbeta_da*(da_dssalb*dssalb_dV + da_dg*dg_dV)

   dgamma1_dL = dgamma1_dbeta*dbeta_da*(da_dssalb*dssalb_dL + da_dg*dg_dL)

   dgamma1_dD = dgamma1_dbeta*dbeta_da*(da_dssalb*dssalb_dD + da_dg*dg_dD)


   dgamma2_dV = dgamma2_dbeta*dbeta_da*(da_dssalb*dssalb_dV + da_dg*dg_dV)

   dgamma2_dL = dgamma2_dbeta*dbeta_da*(da_dssalb*dssalb_dL + da_dg*dg_dL)

   dgamma2_dD = dgamma2_dbeta*dbeta_da*(da_dssalb*dssalb_dD + da_dg*dg_dD)

   dgamma3_dV = dgamma3_dbeta*dbeta_da*(da_dssalb*dssalb_dV + da_dg*dg_dV)

   dgamma3_dL = dgamma3_dbeta*dbeta_da*(da_dssalb*dssalb_dL + da_dg*dg_dL)

   dgamma3_dD = dgamma3_dbeta*dbeta_da*(da_dssalb*dssalb_dD + da_dg*dg_dD)



   dgamma4_dV = dbeta_da*(da_dssalb*dssalb_dV + da_dg*dg_dV)

   dgamma4_dL = dbeta_da*(da_dssalb*dssalb_dL + da_dg*dg_dL)

   dgamma4_dD = dbeta_da*(da_dssalb*dssalb_dD + da_dg*dg_dD)


! *** Compute levelC derivatives




    X11 = (i0-tc)*(  (dgamma1_dV*exp(-kk*tau) - dgamma2_dV*exp(kk*tau))                       &
                   - (dkk_dV*tau + dtau_dV*kk)*(gamma1*exp(-kk*tau) + gamma2*exp(kk*tau)) )

    X22 = (1.0+1.0/beta/beta)*(em*ts  - em*tc )*dbeta_dV

    dF1_dV = X11 + X22

    dF2_dV =   (dgamma4_dV*exp(-kk*tau) -  dgamma3_dV*exp(kk*tau))                            &
             - (gamma4*exp(-kk*tau) +  gamma3*exp(kk*tau))*(dkk_dV*tau +  dtau_dV*kk)


    dTB_dV = (F2*dF1_dV - F1*dF2_dV)/F2/F2


    X11 = (i0-tc)*(  (dgamma1_dL*exp(-kk*tau) - dgamma2_dL*exp(kk*tau))                       &
                   - (dkk_dL*tau + dtau_dL*kk)*(gamma1*exp(-kk*tau) + gamma2*exp(kk*tau)) )

    X22 = (1.0+1.0/beta/beta)*(em*ts  - em*tc )*dbeta_dL

    dF1_dL = X11 + X22

    dF2_dL =   (dgamma4_dL*exp(-kk*tau) -  dgamma3_dL*exp(kk*tau))                            &
             - (gamma4*exp(-kk*tau) +  gamma3*exp(kk*tau))*(dkk_dL*tau +  dtau_dL*kk)

!

    X11 = (i0-tc)*(  (dgamma1_dD*exp(-kk*tau) - dgamma2_dD*exp(kk*tau))                       &
                   - (dkk_dD*tau + dtau_dD*kk)*(gamma1*exp(-kk*tau) + gamma2*exp(kk*tau)) )

    X22 = (1.0+1.0/beta/beta)*(em*ts  - em*tc )*dbeta_dD

    X33 = kk*tc*(1.0-em)*exp(-kk*tau)*(1.0/beta - beta)*dtau_dD

    dF1_dD = X11 + X22

    dF2_dD =   (dgamma4_dD*exp(-kk*tau) -  dgamma3_dD*exp(kk*tau))                            &
             - (gamma4*exp(-kk*tau) +  gamma3*exp(kk*tau))*(dkk_dD*tau +  dtau_dD*kk)

!    if (ssalb .ge. 0.001 ) then

        dTB_dL = (F2*dF1_dL - F1*dF2_dL)/F2/F2

        dTB_dD = (F2*dF1_dD - F1*dF2_dD)/F2/F2

        dTB_dTC = ( em*(1.0/beta - beta)-( gamma1*exp(-kk*tau) - gamma2*exp(kk*tau)) ) /F2

        dTB_dTS = -em*(1.0/beta - beta) / F2

!        dTB_dem =(tc - i0)*exp(-2.0*tau/mu)  +  ( ts - tc )*exp(-tau/mu)

! 03/21/07 revised
        dTB_dem = (i0-tc)*(exp(-kk*tau)/beta-exp(kk*tau)*beta) - (ts - tc )*(1.0/beta - beta)

        dTB_dem = dTB_dem/F2 - ( exp(-kk*tau)-exp(kk*tau) )*F1/F2/F2

  end subroutine  Amatrix_Freq

subroutine Amatrix_emissFreq(frequency,theta,em,i0,tc,ts,kv,gL,ssalbL,tauL,               &
                        dssalbL_dL,dssalbL_dD,dtauL_dL,dtauL_dD,dgL_dL,dgL_dD,       &
                        g,ssalb,tau,dTB_dV,dTB_dL,dTB_dD,dTB_dem,dTB_dTC,dTB_dTS)


real*4 frequency,theta,em,i0,tc,ts,kv,gL,ssalbL,tauL,               &

       dssalbL_dL,dssalbL_dD,dtauL_dL,dtauL_dD,dgL_dL,dgL_dD,       &

       g,ssalb,tau,dTB_dV,dTB_dL,dTB_dD,dTB_dem,dTB_dTC,dTB_dTS


real*4 mu,kk,tauV

real*4 dtau_dV



!(1)

! Initializations for various parameters

    i0 = 0.0  !2.7

    mu = cos(theta*3.14159/180.0)

    kk = 1.0/mu

    dtau_dV = kv

    tauV = tau -tauL

    dTB_dV = exp(-kk*tauV)*( 2.0*(1.0-em)/mu*(tc-i0)*exp(-kk*tauV) - em/mu*(ts-tc) )*dtau_dV

    dTB_dem =(tc - i0)*exp(-2.0*tauV*kk)  +  ( ts - tc )*exp(-tauV*kk)

    dTB_dTC = 1.0- (1.0-em)*exp(-2.0*tauV*kk) - em*exp(-tauV*kk)

    dTB_dTS = em * exp(-tauV*kk)

    dTB_dL = 0.0

    dTB_dD = 0.0

  end subroutine  Amatrix_emissFreq



!--------------------------------------------------
! Bias correction algorithms and emissivity estimate

!
  subroutine tb_asymmetry_correction(theta_a,theta_b,tb,tb89b)

  parameter(nch=6, ncoe=6)

  integer ich, k

  real*4  theta_a,theta_b,theta, dtb,tb(*),tb89b

  real*8  coe(nch,ncoe),a(ncoe)

  data (coe(1,k),k=1,6)/-2.81731e+00, -2.87916e+00,  2.72879e+01,  2.80167e+00,  1.63459e-02, -4.48615e-04/
  data (coe(2,k),k=1,6)/-9.55906e+00,  6.93725e+01,  1.83755e+01,  7.68326e-03,  1.69077e-02,  1.24946e-03/
  data (coe(3,k),k=1,6)/1.07910e+01,  9.20260e+01,  4.02427e+01, -7.89822e-01, -5.51752e-02, -5.90390e-05/
  data (coe(4,k),k=1,6)/-2.06602e+00, -1.14214e+01,  1.77791e+01,  1.68082e+00,  6.68674e-03, -1.45331e-04/
  data (coe(5,k),k=1,6)/3.86218e+00, -1.57320e+01,  1.13138e+01, -1.46883e+00,  7.96641e-02, -3.32594e-04/
  data (coe(6,k),k=1,6)/2.47046e+00,  5.75127e+01,  4.05496e+00,  8.38190e-09, -5.78695e-03, -2.86362e-04/

  do ich = 1, nch

     if (ich .le. 4) then   ! amsu-a channels from 23.8 to 89 GHz

         theta = theta_a
     else

         theta = theta_b

     endif

     a(1:ncoe) = coe(ich,1:ncoe)

     dtb = a(1)*exp(-0.5*((theta-a(2))/a(3))*((theta-a(2))/a(3)))    &
           + a(4) + a(5)*theta+a(6)*theta*theta

     tb(ich) = tb(ich) + dtb

  enddo

! amsu_b 89 GHz

    a(1:ncoe) = coe(4,1:ncoe)
    theta = theta_b
    dtb = a(1)*exp(-0.5*((theta-a(2))/a(3))*((theta-a(2))/a(3)))    &
           + a(4) + a(5)*theta+a(6)*theta*theta
    tb89b =  tb89b + dtb
  

  end subroutine tb_asymmetry_correction


! tc estimate

subroutine tc_predictor(tb,tc)

parameter (ncoe=6)

integer k

real*4  tc,tb(*),coe(ncoe)

data coe/1.053965e+002,-5.331981e-002,-1.111307e-001, 7.597022e-001,-1.799424e-001, 1.592840e-001/


tc = coe(1)

do k =2, ncoe

   tc = tc + coe(k)*tb(k-1)

enddo

end subroutine tc_predictor

! Remove bias between obo and onelayerRTM

  subroutine tb_bias_remove(tbo,tb89b_c)

     parameter (nch = 6, ncoe = 7) 
     integer(2)  k,ich
     real*8  coe(nch,ncoe)
     real*4  dtb,tbo(nch),tba(nch),tb89b_c


 data (coe(1,k),k=1,ncoe)/-4.624947e+00,  2.551217e-02, -1.812905e-02,  5.330789e-03, -8.942994e-03,  2.710240e-03,  1.104319e-02/
 data (coe(2,k),k=1,ncoe)/-6.859871e+00,  3.498314e-02, -1.171616e-02,  5.743650e-03, -2.115875e-02,  1.529497e-02,  2.792133e-03/
 data (coe(3,k),k=1,ncoe)/-3.817903e+01,  1.250054e-01, -6.522450e-02,  1.035476e-01, -3.245012e-02,  8.331739e-03,  2.865168e-03/
 data (coe(4,k),k=1,ncoe)/-8.407491e+00,  5.583085e-02, -3.902766e-02,  3.184391e-03, -1.498117e-02,  3.747008e-02, -9.973097e-03/
 data (coe(5,k),k=1,ncoe)/-2.662964e+00,  6.663031e-02, -5.951103e-02,  5.220540e-03, -3.609308e-02,  4.805510e-02, -1.215695e-02/
 data (coe(6,k),k=1,ncoe)/1.054323e+01, -2.835614e-02,  4.439350e-02, -6.644794e-02, -5.514564e-02, -2.711226e-01,  3.573214e-01/

 do ich =1, nch

    dtb = coe(ich,1)

    do k =2, ncoe

       dtb = dtb + coe(ich,k)*tbo(k-1) !ich)

    enddo

    tba(ich) = tbo(ich) - dtb

 enddo

 tbo(1:nch) = tba(1:nch)

! amsu-b 89 GHz

     dtb = coe(4,1)
     do k =2, ncoe

       dtb = dtb + coe(4,k)*tbo(ich)

     enddo

     tb89b_c = tb89b_c - dtb

! REmove systematic bias at 50.3, 89 and 150 GHz
     tbo(3) = tbo(3) - 0.8
     tbo(4) = tbo(4) - 1.0
     tbo(5) = tbo(5) - 1.0
     tb89b_c = tb89b_c - 1.0
     
end subroutine tb_bias_remove

SUBROUTINE SNOWEM_ESTIMATE(tb, ts,emis)

parameter (nch=5, ncoe = 7)
integer ich, k
real*8 coe(nch, ncoe)
real*4 tb(*),ts,emis(nch)

data(coe(1, k), k=1, ncoe)/9.296607e-001, 4.209462e-003, 1.249806e-004,-2.544851e-004,  &
                           7.613141e-005,-1.081820e-004,-3.776079e-003/
data(coe(2, k), k=1, ncoe)/8.922818e-001,-2.808626e-004, 4.674580e-003,-4.086782e-004,  &
                           4.381013e-005,-6.160555e-005,-3.542917e-003/
data(coe(3, k), k=1, ncoe)/1.051474e+000,-2.549655e-003, 4.026361e-003, 3.537244e-003,  &
                           4.868206e-004,-8.998217e-005,-5.488374e-003/
data(coe(4, k), k=1, ncoe)/9.695728e-001,-9.735869e-004, 1.511961e-003,-1.690710e-003,  &
                           4.980554e-003,-2.362081e-004,-3.467518e-003/
data(coe(5, k), k=1, ncoe)/1.202533e+000,-1.110926e-003, 1.812809e-003,-2.080169e-003,  &
                           7.929524e-004, 4.167637e-003,-4.386423e-003/


do ich = 1, nch

    emis(ich) = coe(ich,1)

    do k=2, ncoe-1

       emis(ich) = emis(ich) + coe(ich, k)*tb(k-1)

     enddo

     emis(ich) = emis(ich) + coe(ich,ncoe)*ts

enddo


END SUBROUTINE SNOWEM_ESTIMATE





!---------------------------------------------------------------------------
!***************************************************************
!** compute general inverse of a matrix using svdcmp from
!** Numerical Recipes
!** input:
!**        A - a matrix whose general inverse is sought
!**        m,p dimensions of matrix A
!** output:
!**        A_inv - general inerse of A
!**        ierr - error code from svdcmp
!***************************************************************
      subroutine gen_inverse(A,A_inv,m,p,ierr)

      integer i,j
      integer m,p
      integer ierr
      real A(m,p)
      real A_inv(m,p)
      real W(p,p), V(p,p)
      real U(m,p)
      real ww(p)

      U = A
      if ( p == 0 ) then
           print *,"p = 0 in gen_inv",m,p,shape(W)
      stop
      endif

      CALL svdcmp(U,m,p,m,p,ww,V,ierr)
      W=0.0
      do i = 1,p
        W(i,i)=1.0/ww(i)
      enddo
      A_inv = matmul(V,matmul(W,transpose(U)))
      return
      end subroutine gen_inverse

!----------------------------------------------------------------
      subroutine svdcmp(a,m,n,mp,np,w,v,ierr)

      integer m,mp,n,np
      integer ierr
      integer i,its,j,jj,k,l,nm
!      integer NMAX=500
      real anorm,c,f,g,h,s,sscale,x,y,z
      real a(m,n)
      real w(n)
      real v(n,n)
!      real rv1(NMAX)
      real rv1(n)
!!      real pythag

      ierr = 0
      g=0.0
      sscale=0.0
      anorm=0.0
      twentyfive: do i=1,n
         l=i+1
         rv1(i)=sscale*g
         g=0.0
         s=0.0
         sscale=0.0
         if(i <= m)then
            do k=i,m
               sscale=sscale+abs(a(k,i))
            enddo
            if(sscale/=0.0)then
               do k=i,m
               a(k,i)=a(k,i)/sscale
               s=s+a(k,i)*a(k,i)
               end do
               f=a(i,i)
               g=-sign(sqrt(s),f)
               h=f*g-s
               a(i,i)=f-g
               do j=l,n
                  s=0.0
                  do k=i,m
                     s=s+a(k,i)*a(k,j)
                  end do
                  f=s/h
                  do k=i,m
                     a(k,j)=a(k,j)+f*a(k,i)
                  end do
               end do
               do k=i,m
                  a(k,i)=sscale*a(k,i)
               end do
            endif
         endif
         w(i)=sscale *g
         g=0.0
         s=0.0
         sscale=0.0
         if((i<=m).and.(i/=n))then
           do k=l,n
              sscale=sscale+abs(a(i,k))
           enddo
           if(sscale/=0.0)then
              do k=l,n
                 a(i,k)=a(i,k)/sscale
                 s=s+a(i,k)*a(i,k)
              enddo
              f=a(i,l)
              g=-sign(sqrt(s),f)
              h=f*g-s
              a(i,l)=f-g
              do k=l,n
                 rv1(k)=a(i,k)/h
              enddo
              do j=l,m
                 s=0.0
                 do k=l,n
                    s=s+a(j,k)*a(i,k)
                 enddo
                 do k=l,n
                    a(j,k)=a(j,k)+s*rv1(k)
                 enddo
              enddo
              do k=l,n
                 a(i,k)=sscale*a(i,k)
              enddo
           endif
         endif
         anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
      end do twentyfive
      do i=n,1,-1
         if(i < n)then
             if(g /= 0.0)then
                do j=l,n
                   v(j,i)=(a(i,j)/a(i,l))/g
                enddo
                do j=l,n
                   s=0.0
                   do k=l,n
                      s=s+a(i,k)*v(k,j)
                   enddo
                   do k=l,n
                      v(k,j)=v(k,j)+s*v(k,i)
                   enddo
                enddo
             endif
             do j=l,n
                v(i,j)=0.0
                v(j,i)=0.0
             enddo
         endif
         v(i,i)=1.0
         g=rv1(i)
         l=i
      enddo
      do i=min(m,n),1,-1
        if (i == 0) then
          print *,"error in svdcmp",m,n,i
        endif
        l=i+1
        g=w(i)
        do j=l,n
          a(i,j)=0.0
        enddo
        if(g /= 0.0)then
           g=1.0/g
           do j=l,n
              s=0.0
              do k=l,m
                 s=s+a(k,i)*a(k,j)
              enddo
              f=(s/a(i,i))*g
              do k=i,m
                 a(k,j)=a(k,j)+f*a(k,i)
              enddo
           enddo
           do j=i,m
              a(j,i)=a(j,i)*g
           enddo
        else
           do j= i,m
              a(j,i)=0.0
           end do
        endif
        a(i,i)=a(i,i)+1.0
      enddo
      fortynine: do k=n,1,-1
        fortyeight: do its=1,30
          do l=k,1,-1
             nm=l-1
             if((abs(rv1(l))+anorm) == anorm) goto 2
             if((abs(w(nm))+anorm) == anorm) goto 1
          enddo
1         c=0.0
          s=1.0
          do i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((abs(f)+anorm) == anorm) goto 2
            g=w(i)
            h=pythag(f,g)
            w(i)=h
            h=1.0/h
            c= (g*h)
            s=-(f*h)
            do j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
            enddo
          enddo
2         z=w(k)
          if (l == k)then
            if(z < 0.0)then
              w(k)=-z
              do j=1,n
                 v(j,k)=-v(j,k)
              enddo
            endif
            goto 3
          endif
          if(its == 30) then
             ierr = 1
             return
          endif
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
          g=pythag(f,1.0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0
          s=1.0
          do j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=pythag(f,h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
            enddo
            z=pythag(f,h)
            w(j)=z
            if(z/=0.0)then
              z=1.0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
            end do
          end do
          rv1(l)=0.0
          rv1(k)=f
          w(k)=x
        end do fortyeight
3       continue
        end do fortynine
      end subroutine svdcmp
!***********************************************************************
       real function pythag(a,b)

       real a,b
       real pyth
       real absa,absb

       absa=abs(a)
       absb=abs(b)
       if (absa > absb) then
           pyth = absa * sqrt(1.0+(absb/absa)**2)
       else
           if (absb == 0) then
               pyth = 0.0
           else
               pyth = absb*sqrt(1.0 + (absa/absb)**2)
           endif
       endif

       pythag=pyth

       return
       end function pythag



!*****************************************************************************************



       SUBROUTINE OPTIC(ice_shape_index1,rwp0,de,tc,frequency,SSALB,GG,TAU)
!
!   Declare some constants
       PARAMETER (EP=.622,RG=0.029261,C=0.2302585,SIGMA =1.,MAXANG=180,MXLEG = 15)
!
!   Input Profiling information
       REAL(4)  frequency,rwp0,rwp,de,ssalb,gg,tau,tc

!   Arrays defined for the scattering matrix of mixing phase particles
!
       REAL(4)  A1(MAXANG),A2(MAXANG),A3(MAXANG),A4(MAXANG),     &
     &          B1(MAXANG),B2(MAXANG),AMU(MAXANG),               &
     &          WT(MAXANG),&
     &          X(MAXANG),YP(MAXANG),Y(MAXANG),TEMP(MAXANG)

       REAL(4)  GSF_A1(0:MXLEG),GSF_A2(0:MXLEG),             &
     &          GSF_A3(0:MXLEG),GSF_A4(0:MXLEG),             &
     &          GSF_B1(0:MXLEG),GSF_B2(0:MXLEG)

! ICE PARTICLE SHAPE
     integer(2) :: ice_shape_index1

     integer ice_shape_index

!
!   Arrays defined for scattering matrix of single phase particles
!   Since particles are assumed spherical, only four elements are needed
!   (A1S = A2S, A3S = A4S)

       REAL(4)  A1S(MAXANG),B1S(MAXANG),A3S(MAXANG),B2S(MAXANG)
       real gg_nonsphere
!
!!       DOUBLE PRECISION PL00,PL02,PL22    ! for generlized spherical functions

       NANG = 180
       PI = ACOS( -1.0 )
       C1 = 0.5
       C2 = 1.0E-6 / (PI*1.0E-3)

       ice_shape_index = ice_shape_index1

!      Gaussian quadrature for computing the expansion
!      coefficients of the phase matrix elements
       CALL QGAUS(NANG,AMU,WT)

       WAVELENGTH = 30.0/frequency     ! in GHz --> cm

!      Converting the wavelength from centimeter to milimeter

       SLAM = WAVELENGTH * 10
!      Frequency in GHz
!       F = 300 / SLAM


        NLEG = 15

        rwp = rwp0*0.001

!      looping over to  computing extinction and scattering coeff., and phase
!      function for each layer.

!         Derive the absoption coefficient due to oxygen and water vapor

          CALL ZEROIT(A1,MAXANG)
          CALL ZEROIT(A2,MAXANG)
          CALL ZEROIT(A3,MAXANG)
          CALL ZEROIT(A4,MAXANG)
          CALL ZEROIT(B1,MAXANG)
          CALL ZEROIT(B2,MAXANG)

!           for rain set itype = 2
!           for snow set itype = 5
           itype = 5   !!!!!!!!!!!!!!!!!

            CALL ATHYD(ice_shape_index,SLAM,AMU,rwp,de,itype,tc,EX,SC,NANG,A1S,B1S,A3S,B2S,gg_nonsphere)

            DO J=1,NANG
               A1(J) = A1(J) + A1S(J)
               A2(J) = A2(J) + A1S(J)
               A3(J) = A3(J) + A3S(J)
               A4(J) = A4(J) + A3S(J)
               B1(J) = B1(J) + B1S(J)
               B2(J) = B2(J) + B2S(J)
            END DO

          IF(EX.GT.0.0) THEN
!           single scattering albedo
            SSALB = SC/EX
          ELSE
            SSALB = 0
          END IF
          IF (SSALB .GT. 1.0) SSALB = 1.0


          IF (SSALB .GT. 0.0001) THEN


! line 239  normalized phase function by the scattering coeff.
           DO J = 1, NANG
             A1(J) = SLAM*SLAM*C2*A1(J)/SC
             A2(J) = SLAM*SLAM*C2*A2(J)/SC
             A3(J) = SLAM*SLAM*C2*A3(J)/SC
             A4(J) = SLAM*SLAM*C2*A4(J)/SC
             B1(J) = SLAM*SLAM*C2*B1(J)/SC
             B2(J) = SLAM*SLAM*C2*B2(J)/SC
           ENDDO

!  check the phase matrix to see if it is normlaized

           FACTOR = 0

           DO J = 1,NANG
             FACTOR =  FACTOR + A1(J)*WT(J)
           END DO
           FACTOR = .5*FACTOR - 1.0

!           IF(FACTOR.GT.0.1) PRINT*, 'OPTIC -- PHASE MATRIX NOT NORMALIZED'

           DO L = 0, NLEG
!c             compute the factor of (2*L+1)/2
               bin2l = .5*(2.0*L+1)
               GSF_A1(L) = 0
               GSF_A4(L) = 0
               AL2P3 = 0
               AL2M3 = 0
               GSF_B1(L) = 0
               GSF_B2(L) = 0
               DO J = 1, NANG
                  GSF_A1(L) = GSF_A1(L) + WT(J)* A1(J)*REAL(PL00(dble(AMU(J)),L))
                  GSF_A4(L) = GSF_A4(L) + WT(J)* A4(J)*REAL(PL00(dble(AMU(J)),L))
                  IF(L.LT.2) GOTO 30
                  AL2P3 = AL2P3 + WT(J)*(A2(J) + A3(J))*REAL(PL22(dble(AMU(J)),L,1.d0))
                  AL2M3 = AL2M3 + WT(J)*(A2(J) - A3(J))*REAL(PL22(dble(AMU(J)),L,-1.d0))
                  GSF_B1(L) = GSF_B1(L) + WT(J)*B1(J)*REAL(PL02(dble(AMU(J)),L))
                  GSF_B2(L) = GSF_B2(L) + WT(J)*B2(J)*REAL(PL02(dble(AMU(J)),L))
30             ENDDO

               GG = GSF_A1(1)*0.5/(2*L+1)

               if (ice_shape_index .ge. 1 ) GG = gg_nonsphere			   

               GSF_A1(L) = bin2l*GSF_A1(L)
               GSF_A2(L) = bin2L*.5*(AL2P3 + AL2M3)
               GSF_A3(L) = bin2L*.5*(AL2P3 - AL2M3)
               GSF_A4(L) = bin2l*GSF_A4(L)
               GSF_B1(L) = bin2l*GSF_B1(L)
               GSF_B2(L) = bin2l*GSF_B2(L)

35         END DO
          ELSE
           DO L = 0, NLEG
              GSF_A1(L) = 0
              GSF_A2(L) = 0
              GSF_A3(L) = 0
              GSF_A4(L) = 0
              GSF_B1(L) = 0
              GSF_B2(L) = 0
           END DO
! CORRECTION (12/10/06)
           GG = 0.0
          END IF

          TAU = EX

print*,'?',TAU ,GG

      RETURN

      END SUBROUTINE OPTIC



!
!
       SUBROUTINE ATHYD(ice_shape_index,SLAM,AMU,W,DE,ITYPE,TV,EXQC,SCQC,NAN,  &
	   
	                    A1S,B1S,A3S,B2S,MEAN_GGNS)
!
!      INPUT VARIABLES
!
!      SLAM  -   WAVELENGH (mm)
!      W     -   WATER CONTENT (kg/m^3)
!      ITYPE -   HYDROMETEOR TYPES
!                1   CLOUD LIQUID
!                2   RAIN WATER
!                3   CLOUD ICE
!                4   SNOW
!                5   GRAUPEL
!      AMU   -   COSINE OF SCATTERING ANGLE
!      TV    -   TEMPERATURE (K)
!      NAN   -   NUMBER OF SCATTERING ANGLE
!
!
!      OUTPUT VARIABLES
!
!      A1S   -  INTEGRATED VALUE OF A1P (each particle) OVER THE SIZE DISTRIBUTION
!      A3S   -  INTEGRATED VALUE OF A3P (each particle) OVER THE SIZE DISTRIBUTION
!      B1S   -  INTEGRATED VALUE OF B1P (each particle) OVER THE SIZE DISTRIBUTION
!      B2S   -  INTEGRATED VALUE OF B2P (each particle) OVER THE SIZE DISTRIBUTION
!      EXQC  -  EXTINCTION COEFF. (1/km)
!      SCQC  -  SCATTERING COEFF. (1/km)
!
!
!      INTERNAL VARAIBLES
!
!      DENS  -   MASS DENSITY (kg/M^3) FOR DIFFERENT TYPES OF
!                HYDROMETEORS (Cloud and rain water=1000, cloud ice =
!                              920, snow = 100  and graupel = 600)
!      DE    -   EFFECTIVE DIAMETER IN SIZE DISTRIBUTION (mm)
!      DM    -   MODE DIAMETER IN SIZE DISTRIBUTION (mm)
!      EXPO  -   EXPONENT IN GAMMA SIZE DISTRIBUTION
!      DL    -   LOW LIMIT OF DIAMETER FOR INTEGRATION (mm)   (0.005)
!      DU    -   UPPER LIMIT OF DIAMETER FOR INTEGRATION (mm) (2.005)
!      MPC   -   NUMBER OF PARTICLE DIAMETERS BETWEEN DL AND DU
!                AT WHICH SCATTERING EXTINCTION EFFICIENTS AND PHASE
!                MATRIX ARE COMPUTED
!      A1P   -  A1 FOR PARTICLES WITH DISCRETE DIAMETERS
!      A3P   -  A3 FOR PARTICLES WITH DISCRETE DIAMETERS
!      B1P   -  B1 FOR PARTICLES WITH DISCRETE DIAMETERS
!      B2P   -  B2 FOR PARTICLES WITH DISCRETE DIAMETERS
!
!
       PARAMETER(MPC=401,MAXANG=180,REFMED=1.0)
       REAL AMU(*), QE(MPC), QS(MPC), A1S(*),B1S(*),A3S(*),B2S(*), &
     &      A1P(MAXANG,MPC), B1P(MAXANG,MPC), A3P(MAXANG,MPC), B2P(MAXANG,MPC)
       REAL DENS(5),DE,EXPO(5),DL(5),DU(5)
	   REAL GGNS(MPC),MEAN_GGNS    ! FOR NON-SPHERE
	   real SIZE_PARM,AREA,SLAM,freq,gg_nonsphere
	   integer ice_shape_index

       COMPLEX REF,REFQC,RK
       DATA DENS/1000,1000,920,100,600/
!       DATA DE/10E-3,500E-3,10E-3,500E-3,500E-3/
       DATA EXPO/2,1,2,1,1/
       DATA DL/0.005,0.005,0.005,0.005,0.005/
!       DATA DU/2.005,2.005,2.005,2.005,2.005/
!ybh

      DATA DU/2.005,4.005,2.005,2.005,2.005/

!
!      FOR CLOUD LIQUID WATER  USE AN EMPIRICAL MODEL
       IF(ITYPE.EQ.1) THEN
!        References for this model
         F0=160*EXP(7.2*(1-287.0/TV))
         F = 300/SLAM
         EXQC = .0241*W*1.0E3*F**2*F0/(F**2+F0**2)
         SCQC = 0
         CALL ZEROIT(A1S,NAN)
         CALL ZEROIT(B1S,NAN)
         CALL ZEROIT(A3S,NAN)
         CALL ZEROIT(B2S,NAN)
         RETURN
       ENDIF
! FOR NON-SPHERE
      CALL ZEROIT(GGNS,MPC)

       PI = ACOS(-1.0)
       TEMP = TV - 273.15
!
       IF(ITYPE.LE.2) THEN

         CALL IWREF(1,SLAM,TEMP,REFQC)
         RHOH = 1000    ! water phase droplet
       ELSE
         CALL IWREF(0,SLAM,TEMP,REF)
         RK = (REF*REF - 1.0) / (REF*REF+2.0)
         RHOH = DENS(ITYPE)
         REFQC = CSQRT(1.0+3.0*RHOH*RK/(920-RHOH*RK))
       END IF
       REFQC = REFQC/REFMED
       TU = DU(ITYPE)
       TL = DL(ITYPE)
       DD = (TU-TL)/(MPC - 1)
       DO I = 1, MPC
         SIZE_PARM = PI*(DD*(I-1)+TL)/SLAM     ! size parameter

         CALL SCAT_MIE(SIZE_PARM,REFQC,AMU,NAN,QE(I),QS(I), &
     &                 A1P(1,I),B1P(1,I),A3P(1,I),B2P(1,I))

!!!!!!!! Test the impact of snow shape

         AREA = PI*(DD*(I-1)+TL)*(DD*(I-1)+TL)/4.

         if (ice_shape_index .ge. 1 )  &
		 
		 CALL NONSPHERE(ice_shape_index,SIZE_PARM,AREA,SLAM,freq,QE(I),QS(I),GGNS(I)) !gg_nonsphere)

       ENDDO
!
       RNU  = EXPO(ITYPE)
       DM   = RGAMMA(RNU)*DE/RGAMMA(RNU + 1.0)  ! Mode diamter
       F3   = RGAMMA(RNU+3) / RGAMMA(RNU)
       AN0C = 1.0E9*6.0*W/(RHOH*PI*F3*DM**3)  ! number of particles/m**3/mm**(1-gamma)

!
!      AVERAGE OVER THE ENTIRE SIZE DISTRIBUTION TO GET THE MEAN VALUES
       CALL CLOUD_INT(QE,QS,A1P,B1P,A3P,B2P,TL,TU,MPC,NAN,MAXANG, &
     &                AN0C,DM,RNU,EXQC,SCQC,A1S,B1S,A3S,B2S,GGNS,MEAN_GGNS)

       RETURN
       END SUBROUTINE ATHYD
!
       SUBROUTINE IWREF(IFLAG,WAVEL,TEMP,REIND)
!      FOR WATER AT MICROWAVE FREQUENCIES ONLY
!      ALAM         WAVELENGTH IN CM
!      TEMP         TEMPERATURE IN C
!      REPS,AIEPS   REAL,IMAG EFFECTIVE DIELECTRIC CONST
!      EP           EFFECTIVE DIELECTRIC CONST
!      REIND        REFRACTIVE INDEX
!      IFLAG        1  FOR WATER
!                   0  FOR ICE
       COMPLEX EP,REIND
       CPI = 4.0*ATAN(1.0)
       ALAM = WAVEL / 10.
       IF(TEMP.LE.-55) TEMP = -55


!B.YAN
!       IF(TEMP.LT.-100.0.OR.TEMP.GT.60.0)  &
!     &  CALL ERRMSG('IWREF--TEMPERATURE MIGHT BE OUT OF RANGE',.FALSE.)


       IF (IFLAG.EQ.1) THEN
!        P. S. Ray, 1973, gave Eqs (5) and (6) for computing
!        water refractive index for centimeter waves.
!        The parameteres  are given by Eq. (4) and (7).
!        SET UP CONSTANTS.
         SIGMA=12.5664E08
         EINF=5.27137+.0216474*TEMP-.00131198*(TEMP**2)  ! Eq. (7a)
         ALPHA=-16.8129/(TEMP+273.)+.0609265             ! Eq. (7b)
         BRIN=2513.98D0/(TEMP+273.000)                   ! Eq. (7c)
         ALS=.00033836*EXP(BRIN)
         T=TEMP-25.0
         EPS=78.54*(1.-4.579E-03*T+1.19E-05*(T**2)-2.8E-08*(T**3)) ! Eq. (4)
       ELSE
!        Temperature dependence of ice refractive index needed to be considered
!        The method given by P. S. Ray, 1973, Eq. (1), (5), (6). Parameteres
!        are given by Eq. (12).
!        SET UP CONSTANTS.
         EINF=3.168                                               ! Eq. (12a)
         ALPHA=0.288+0.0052*TEMP+0.00023*TEMP*TEMP                ! Eq. (12b)
         SIGMA=1.26*EXP(-12500/((TEMP+273.0)*1.9869))             ! Eq. (12c)
         ALS=9.990288*1.0E-4*EXP(13200/((TEMP+273.0)*1.9869))     ! Eq. (12d))
         EPS=203.168+2.5*TEMP+0.15*TEMP*TEMP                      ! Eq. (12e)
       ENDIF
!      SET UP COMMON PARAMETERS.
       SLAM=(ALS/ALAM)**(1.0-ALPHA)
       SINAL=SIN(ALPHA*CPI/2.000)
       COSAL=COS(ALPHA*CPI/2.000)
!      CALCULATE RE(EPSLON)
       REPS=(EPS-EINF)*(1.0+SLAM*SINAL)
       X=1.0+2.0*SLAM*SINAL+SLAM**2
       REPS=REPS/X
       REPS=REPS + EINF                            ! EQ. (5)
!      CALCULATE IM(EPSLON)
       AIMEPS=(EPS-EINF)*SLAM*COSAL
       AIMEPS=(AIMEPS/X)+SIGMA*ALAM/18.8496E10   ! EQ. (6)
!      Complex dielectric function (or relative permittivity)
!      epsilon=reps+i*aimeps. Since the complex refractive index is
!      related to epsilon by m=(epsilon)**0.5
!      If (iflag.eq.0) EP=CMPLX(3.17,0.00855)    ! Approximation for ice.
       EP=CMPLX(REPS,AIMEPS)
       REIND=CSQRT(EP)
       RETURN
       END SUBROUTINE IWREF

       SUBROUTINE CLOUD_INT(QE,QS,A1P,B1P,A3P,B2P,TL,TU,N,NANG,MXANG, &
     &                      AN0,DM,RNU,EX,SC,A1S,B1S,A3S,B2S,GGNS,MEAN_GGNS)
!
!      INPUT VARIABLES
!
!        QE       :  EXTINCTION EFFICIENT FACTOR
!        QS       :  SCATTERING EFFICIENT FACTOR
!        GGNS     :  ASYMMETRY FACTOR 
!        A1P      :  ELEMENTS OF PHASE MATRIX FOR DISCRETE PARTICLES
!        B1P      :  ELEMENTS OF PHASE MATRIX
!        A3P      :  ELEMENTS OF PHASE MATRIX
!        B2P      :  ELEMENTS OF PHASE MATRIX
!        X        :  DIAMETERS OF POLYDISPERSE PARTICLES (mm)
!        NANG     :  NO OF SCATTERING ANGLES, 1ST DIMENSION OF S1,...
!        MXANG    :
!        N        :  NO. OF DROPLETS RADIUS, DIMENSION OF X AND ALSO
!                    2ND DIMESION OF S1,S2,S3 AND S4
!        TL,TU    :  RADIUS LOW AND UPPER LIMIT (mm)
!        AN0,DM   :  THREE PARAMETERS RELATED TO PARTICLE DISTRIBUTION
!        RNU         (AN0:m^-3, DM: mm),
!                    N(D)=N0*D**(RNU-1)EXP(-D/DM)/GAMMA(RNU)*DM**RNU
!        SLAM     :  WAVELENGH (mm)
!
!
!      OUTPUT VARIABLES
!
!        EX       :  SCATTERING COEFFICIENT (1/km)
!        SC       :  EXTINCTION COEFFICIENT (1/km)
!        A1S      :  ELEMENT OF PHASE MATRIX AVERAGED OVER SIZE DISTRIBUTION
!        B1S      :  (1/m**3)
!        A3S      :
!        B2S      :
!        MEAN_GGNS
!
!      INTERNAL VARIABLES
!
!        LN       :  INTEROPOLATION
!        RK       :  NORMALIZATION FACTOR  = SLAM^2/(PI*SCATTERING COEFF.)
!+____________________________________________________________________________
       PARAMETER(LN=401)
       REAL A1P(MXANG,N),B1P(MXANG,N),A3P(MXANG,N),B2P(MXANG,N), &
     &      QS(N),QE(N),X(LN),YP(LN),Y(LN),TEMP(LN),             &
     &      A1S(MXANG),B1S(MXANG),A3S(MXANG),B2S(MXANG)
	   REAL GGNS(N),MEAN_GGNS
       PI=2.*ASIN(1.0)
!      For converting SC,EX from 1/m to 1/km and also some contants in
!      modified gamma distribution
       CC=PI*AN0*1.0E-3/(4.0*RGAMMA(RNU)*DM**RNU)
       SIGMA=1.0
       DD=(TU-TL)/(N-1)
       DO I=1,N
         X(I)=DD*(I-1)+TL
         Y(I)=X(I)*X(I)*QE(I)*X(I)**(RNU-1)*EXP(-X(I)/DM)
       ENDDO
       CALL CURV1(N,X,Y,SLP1,SLPN,YP,TEMP,-SIGMA)
       EX = CURVI(TL,TU,N,X,Y,YP,SIGMA)
       DO I = 1, N
         Y(I) = X(I)*X(I)*QS(I)*X(I)**(RNU-1)*EXP(-X(I)/DM)
       ENDDO
       CALL CURV1(N,X,Y,SLP1,SLPN,YP,TEMP,-SIGMA)
       SC=CURVI(TL,TU,N,X,Y,YP,SIGMA)
       IF(SC.EQ.0.0) THEN
         RK=0.0
       ELSE
         RK=AN0/(RGAMMA(RNU)*DM**RNU)
       ENDIF
       SC=CC*SC
       EX=CC*EX

!B.YAN ADDED (10/30/2006)
         DO I=1,N
         X(I)=DD*(I-1)+TL
         Y(I)=X(I)*X(I)*GGNS(I)*X(I)**(RNU-1)*EXP(-X(I)/DM)
       ENDDO
       CALL CURV1(N,X,Y,SLP1,SLPN,YP,TEMP,-SIGMA)
       MEAN_GGNS = CURVI(TL,TU,N,X,Y,YP,SIGMA)
     

       DO NSCA=1,NANG
         DO NR=1,N
            Y(NR)=A1P(NSCA,NR)*X(NR)**(RNU-1)*EXP(-X(NR)/DM)
         ENDDO
         CALL CURV1(N,X,Y,SLP1,SLPN,YP,TEMP,-SIGMA)
         A1S(NSCA)=RK*CURVI(TL,TU,N,X,Y,YP,SIGMA)
         DO NR=1,N
            Y(NR)=B1P(NSCA,NR)*X(NR)**(RNU-1)*EXP(-X(NR)/DM)
         ENDDO
         CALL CURV1(N,X,Y,SLP1,SLPN,YP,TEMP,-SIGMA)
         B1S(NSCA)=RK*CURVI(TL,TU,N,X,Y,YP,SIGMA)
         DO NR=1,N
            Y(NR)=A3P(NSCA,NR)*X(NR)**(RNU-1)*EXP(-X(NR)/DM)
         ENDDO
         CALL CURV1(N,X,Y,SLP1,SLPN,YP,TEMP,-SIGMA)
         A3S(NSCA)=RK*CURVI(TL,TU,N,X,Y,YP,SIGMA)
         DO NR=1,N
            Y(NR)=B2P(NSCA,NR)*X(NR)**(RNU-1)*EXP(-X(NR)/DM)
         ENDDO
         CALL CURV1(N,X,Y,SLP1,SLPN,YP,TEMP,-SIGMA)
         B2S(NSCA)=RK*CURVI(TL,TU,N,X,Y,YP,SIGMA)
       ENDDO
       RETURN
       END SUBROUTINE CLOUD_INT


       SUBROUTINE SCAT_MIE(X,REFREL,AMU,NANG,QEXT,QSCA,A1P,B1P,A3P,B2P)
!
!      CALCULATES EFFICIENCIES FOR EXTINCTION, SCATTERING COEFF.
!      BACKSCATTERING CROSS SECTION, SCATTERING FUNCTION
!      AND THE ELEMENTS OF PHASE MATRIX. FOR GIVEN SIZE PARAMETERS AND
!      RELATIVE REFRACTIVE INDICES, ALL BESSEL FUNCTIONS COMPUTED BY
!      DOWNWARD RECURRECE
!      ALL NOTATIONS ARE CONSISTENT WITH THE ORIGINAL PROGRAMS
!
!      Reference :   Bohren G. and Hoffmen. 1983: Light scattering
!                    by small particles. ...
!                    Dave J. V. 1970: Intensity and Polarization of
!                    the radiation emerging from a plane-parallel
!                    atmosphere containing monodispersed aerosols.
!                    Applied Optics, V. 9, 2673-2684.
!
!      INPUT VARIABLES
!
!        X        :  SIZE PARAMETER
!        REFREL   :  RELATIVE REFRACTIVE INDEX
!        AMU      :  COSIN VALUES OVER SCATTERING ANGLES
!        NANG     :  NO. OF SCATTERING ANGLES
!
!      OUTPUT VARIABLES
!
!        QEXT     :  EXTINCTION EFFICIENT FACTOR
!        QSCA     :  SCATTERING EFFICIENT FACTOR
!
!        A1P      :  AN ELEMENT OF PHASE MATRIX
!        B1P      :  AN ELEMENT OF PHASE MATRIX
!        A3P      :  AN ELEMENT OF PHASE MATRIX
!        B2P      :  AN ELEMENT OF PHASE MATRIX
!        See  following matrix notation
!        (A1P   B1P    0     0 )
!        (B1P   A1P    0     0 )
!        (0      0    A3P   B2P)
!        (0      0   -B2P   A3P)
!
!      INTERNAL VARIABLES
!
!        S1       :  ELEMENTS IN AMPLITUDE SCATTERING MATRIX
!        S2       :  ELEMENTS IN AMPLITUDE SCATTERING MATRIX
!        S3       :  ZERO FOR SPHERICAL PARTICLE
!        S4       :  ZERO FOR SPHERICAL PARTICLE
!
!        (S2    S3)
!        (S4    S1)
!
!+--------------------------------------------------------------------------
       PARAMETER(MM=2000)
       REAL(4) AMU(NANG), A1P(*), B1P(*), A3P(*), B2P(*),  &
      &     PI(MM), TAU(MM), PI0(MM), PI1(MM)
       COMPLEX XI,XI0,XI1,AN,BN,S1(MM),S2(MM),Y,REFREL,D(3000),TEMP
       DOUBLE PRECISION PSI0,PSI1,PSI,DN,DX
       DX=DBLE(X)
       Y=X*REFREL
       XSTOP=X+4*X**0.3333+2.0
       NSTOP=XSTOP
       YMOD=CABS(Y)
       NMX=AMAX1(XSTOP,YMOD)+15
       D(NMX)=CMPLX(0.0,0.0)
       NN=NMX-1
       DO N=1,NN
         RN=NMX-N+1
         D(NMX-N)=(RN/Y)-(1./(D(NMX-N+1)+RN/Y))
       ENDDO
       DO J=1,NANG
        PI0(J)=0.0
        PI1(J)=1.0
       ENDDO
       DO J=1,NANG
        S1(J)=CMPLX(0.0,0.0)
        S2(J)=CMPLX(0.0,0.0)
       ENDDO
       PSI0=DCOS(DX)
       PSI1=DSIN(DX)
       CHI0=-SIN(X)
       CHI1=COS(X)
       APSI0=PSI0
       APSI1=PSI1
       XI0=CMPLX(APSI0,-CHI0)
       XI1=CMPLX(APSI1,-CHI1)
       N=1
       QSCA=0.
       QEXT=0.
10     DN=N
       RN=N
       FN=(2.*RN+1.)/(RN*(RN+1.))
       PSI=(2.0*DN-1.)*PSI1/DX-PSI0
       APSI=PSI
       CHI=(2.0*RN-1.)*CHI1/X-CHI0
       XI=CMPLX(APSI,-CHI)
       AN=(D(N)/REFREL+RN/X)*APSI-APSI1
       AN=AN/((D(N)/REFREL+RN/X)*XI-XI1)
       BN=(REFREL*D(N)+RN/X)*APSI-APSI1
       BN=BN/((REFREL*D(N)+RN/X)*XI-XI1)
       DO J=1,NANG
         PI(J)=PI1(J)
         TAU(J)=RN*AMU(J)*PI(J)-(RN+1.)*PI0(J)
         S1(J)=S1(J)+FN*(AN*PI(J)+BN*TAU(J))
         S2(J)=S2(J)+FN*(AN*TAU(J)+BN*PI(J))
       ENDDO
       QSCA=QSCA+(2.*RN+1.)*(CABS(AN)*CABS(AN)+CABS(BN)*CABS(BN))
       QEXT=QEXT+(2.*RN+1.)*REAL(AN+BN)
       PSI0=PSI1
       PSI1=PSI
       APSI1=PSI1
       CHI0=CHI1
       CHI1=CHI
       XI1=CMPLX(APSI1,-CHI1)
       N=N+1
       RN=N
       DO J=1,NANG
         PI1(J)=((2.*RN-1.)/(RN-1.))*AMU(J)*PI(J)-RN*PI0(J)/(RN-1.)
         PI0(J)=PI(J)
       ENDDO
       IF(N-1-NSTOP) 10,20,20
20     QSCA=(2./(X*X))*QSCA
       QEXT=(2./(X*X))*QEXT
       RK=4.0/(QSCA*X*X)
       DO I = 1, NANG
         A1P(I)=0.5*(S2(I)*CONJG(S2(I))+S1(I)*CONJG(S1(I)))   ! I, Q, U, v
         B1P(I)=0.5*(S2(I)*CONJG(S2(I))-S1(I)*CONJG(S1(I)))
         A3P(I)=REAL(S1(I)*CONJG(S2(I)))
         B2P(I)=-AIMAG(S2(I)*CONJG(S1(I)))
       ENDDO
       RETURN
       END SUBROUTINE SCAT_MIE




      subroutine  zeroit( a, length )
!
!     zeros a real array -a- having -length- elements
!
      integer*4 length

      real  a(*)
!
      do 10  l = 1, length
         a( l ) = 0.0
10    continue
!
      return
      end subroutine  zeroit
!-----------------------------------------------------------
!                  Part V

!     The this utility package contains spline
!     interpolation, differentiation and integration

        subroutine curv1 (n,x,y,slp1,slpn,yp,temp,sigma)
!c
        real x(n),y(n),yp(n),temp(n)
        nn = n
        nm1 = nn-1
        np1 = nn+1
        delx1 = x(2)-x(1)
        dx1 = (y(2)-y(1))/delx1

!c       determine slopes if necessary

        if (sigma .lt. 0.) go to 107
        slpp1 = slp1
        slppn = slpn

!c       denormalize tension factor

103     sigmap = abs(sigma)*float(nn-1)/(x(nn)-x(1))

!c       set up right hand side and tridiagonal system for yp and
!c       perform forward elimination

        dels = sigmap*delx1
        exps = exp(dels)
        sinhs = .5*(exps-1./exps)
        sinhin = 1./(delx1*sinhs)
        diag1 = sinhin*(dels*.5*(exps+1./exps)-sinhs)
        diagin = 1./diag1
        yp(1) = diagin*(dx1-slpp1)
        spdiag = sinhin*(sinhs-dels)
        temp(1) = diagin*spdiag
        if (nn .eq. 2) go to 105
        do 104 i=2,nm1
         delx2 = x(i+1)-x(i)
         dx2 = (y(i+1)-y(i))/delx2
         dels = sigmap*delx2
         exps = exp(dels)
         sinhs = .5*(exps-1./exps)
         sinhin = 1./(delx2*sinhs)
         diag2 = sinhin*(dels*(.5*(exps+1./exps))-sinhs)
         diagin = 1./(diag1+diag2-spdiag*temp(i-1))
         yp(i) = diagin*(dx2-dx1-spdiag*yp(i-1))
         spdiag = sinhin*(sinhs-dels)
         temp(i) = diagin*spdiag
         dx1 = dx2
         diag1 = diag2
104     continue
105     diagin = 1./(diag1-spdiag*temp(nm1))
        yp(nn) = diagin*(slppn-dx1-spdiag*yp(nm1))

!c       perform back substitution

        do 106 i=2,nn
         ibak = np1-i
         yp(ibak) = yp(ibak)-temp(ibak)*yp(ibak+1)
106     continue
        return
107     if (nn .eq. 2) go to 108

!c       if no derivatives are given use second order polynomial
!c       interpolation on input data for values at endpoints.

        delx2 = x(3)-x(2)
        delx12 = x(3)-x(1)
        c1 = -(delx12+delx1)/delx12/delx1
        c2 = delx12/delx1/delx2
        c3 = -delx1/delx12/delx2
        slpp1 = c1*y(1)+c2*y(2)+c3*y(3)
        deln = x(nn)-x(nm1)
        delnm1 = x(nm1)-x(nn-2)
        delnn = x(nn)-x(nn-2)
        c1 = (delnn+deln)/delnn/deln
        c2 = -delnn/deln/delnm1
        c3 = deln/delnn/delnm1
        slppn = c3*y(nn-2)+c2*y(nm1)+c1*y(nn)
        go to 103
!!$c
!!$c       if only two points and no derivatives are given, use
!!$c       straight line for curve
!!$c
108     yp(1) = 0.
        yp(2) = 0.
        return
        end subroutine curv1

        function curvp (t,n,x,y,yp,sigma,it)

!c       interpolates a curve at 'x=t'

        real x(n),y(n),yp(n)
        data i1/2/

        s = x(n)-x(1)
        sigmap = abs(sigma)*float(n-1)/s
        if (it .eq. 1) i1 = 2

!c       search for interval

101     do 102 i=i1,n
         if (x(i)-t) 102,102,103
102     continue
        i = n
103     if (x(i-1).le.t .or. t.le.x(1)) go to 104

        i1 = 2
        go to 101

!c       set up and perform interpolation

104     del1 = t-x(i-1)
        del2 = x(i)-t
        dels = x(i)-x(i-1)
        exps1 = exp(sigmap*del1)
        sinhd1 = .5*(exps1-1./exps1)
        exps = exp(sigmap*del2)
        sinhd2 = .5*(exps-1./exps)
        exps = exps1*exps
        sinhs = .5*(exps-1./exps)
        curvp = (yp(i)*sinhd1+yp(i-1)*sinhd2)/sinhs+ &
     &        ((y(i)-yp(i))*del1+(y(i-1)-yp(i-1))*del2)/dels
        i1 = i
        return
        end function curvp



        function curvd (t,n,x,y,yp,sigma,it)

!c       differentiates a curve at a given point 'x=t'

        real x(n),y(n),yp(n)
        data i1/2/
        s = x(n)-x(1)
!c       denormalize sigma

        sigmap = abs(sigma)*float(n-1)/s
!c       if it.ne.1 start search where previously terminated,
!c       otherwise start from beginning
!c
        if (it .eq. 1) i1 = 2
!c
!c       search for interval
!c
101     do 102 i=i1,n
         if (x(i)-t) 102,102,103
102     continue
        i = n

!c       check to insure correct interval

103     if (x(i-1).le.t .or. t.le.x(1)) go to 104

!!$c       restart search  and reset i1
!!$c       ( input ""it"" was incorrect )
!!$c
        i1 = 2
        go to 101
!!$c
!!$c       set up and perform interpolation
!!$c
104     del1 = t-x(i-1)
        del2 = x(i)-t
        dels = x(i)-x(i-1)
        exps1 = exp(sigmap*del1)
        coshd1 = .5*(exps1+1./exps1)
        exps = exp(sigmap*del2)
        coshd2 = .5*(exps+1./exps)
        exps = exps1*exps
        sinhs = .5*(exps-1./exps)/sigmap
        curvd = (yp(i)*coshd1-yp(i-1)*coshd2)/sinhs+ &
     &        ((y(i)-yp(i))-(y(i-1)-yp(i-1)))/dels
        i1 = i
        return
        end function curvd



       function curvi (xl,xu,n,x,y,yp,sigma)

!c      integrates a curve

       real x(n),y(n),yp(n)
       nn = n
       s = x(nn)-x(1)
       sigmap = abs(sigma)*float(nn-1)/s

!c      determine actual upper and lower bounds

       xxl = xl
       xxu = xu
       ssign = 1.
       if (xl .lt. xu) go to 101
       xxl = xu
       xxu = xl
       ssign = -1.
       if (xl .gt. xu) go to 101

!c      return zero if upper and lower limits equal

       curvi = 0.
       return

!c      search for proper intervals

101    do 102 i=2,nn
         if (x(i)-xxl) 102,103,103
102    continue
       i = nn
103    il = i
       np1 = nn+1
       do 104 i=2,nn
          np1mi = np1-i
          if (x(np1mi)-xxu) 105,105,104
104    continue
       i = nn
105    iu = nn+2-i
       if (il .eq. iu) go to 110

!c      integrate from xxl to x(il)

       sum = 0.
       if (xxl .eq. x(il)) go to 106
       del1 = xxl-x(il-1)
       del2 = x(il)-xxl
       dels = x(il)-x(il-1)
       exps1 = exp(sigmap*del1)
       coshd1 = .5*(exps1+1./exps1)
       exps = exp(sigmap*del2)
       coshd2 = .5*(exps+1./exps)
       exps = exps1*exps
       sinhs = sigmap*.5*(exps-1./exps)
       coshs = .5*(exps+1./exps)
       sum = (yp(il)*(coshs-coshd1)-yp(il-1)*(1.-coshd2))/sinhs+.5* &
     &      ((y(il)-yp(il))*(dels*dels-del1*del1)+ &
     &                                (y(il-1)-yp(il-1))*del2*del2)/dels
106    if (iu-il .eq. 1) go to 108

!c      integrate over interior intervals

       ilp1 = il+1
       ium1 = iu-1
       do 107 i=ilp1,ium1
         dels = x(i)-x(i-1)
         exps = exp(sigmap*dels)
         sinhs = sigmap*.5*(exps-1./exps)
         coshs = .5*(exps+1./exps)
         sum = sum+(yp(i)+yp(i-1))*(coshs-1.)/sinhs- &
     &         .5*((yp(i)+yp(i-1))-(y(i)+y(i-1)))*dels
107    continue
108    if (xxu .eq. x(iu-1)) go to 109
!c
!c      integrate from x(iu-1) to xxu
!c
       del1 = xxu-x(iu-1)
       del2 = x(iu)-xxu
       dels = x(iu)-x(iu-1)
       exps1 = exp(sigmap*del1)
       coshd1 = .5*(exps1+1./exps1)
       exps = exp(sigmap*del2)
       coshd2 = .5*(exps+1./exps)
       exps = exps1*exps
       sinhs = sigmap*.5*(exps-1./exps)
       coshs = .5*(exps+1./exps)
       sum = sum+(yp(iu)*(coshd1-1.)-yp(iu-1)*(coshd2-coshs))/sinhs+.5* &
     &      ((y(iu)-yp(iu))*del1*del1- &
     &                    (y(iu-1)-yp(iu-1))*(del2*del2-dels*dels))/dels
109    curvi = ssign*sum
       return

!c      integrate from xxl to xxu

110    delu1 = xxu-x(iu-1)
       delu2 = x(iu)-xxu
       dell1 = xxl-x(iu-1)
       dell2 = x(iu)-xxl
       dels = x(iu)-x(iu-1)
       exps1 = exp(sigmap*delu1)
       coshu1 = .5*(exps1+1./exps1)
       exps = exp(sigmap*delu2)
       coshu2 = .5*(exps+1./exps)
       exps = exps1*exps
       sinhs = sigmap*.5*(exps-1./exps)
       exps1 = exp(sigmap*dell1)
       coshl1 = .5*(exps1+1./exps1)
       exps = exp(sigmap*dell2)
       coshl2 = .5*(exps+1./exps)
       sum = (yp(iu)*(coshu1-coshl1)-yp(iu-1)* &
     &      (coshu2-coshl2))/sinhs+.5* &
     &      ((y(iu)-yp(iu))*(delu1*delu1-dell1 &
     &      *dell1)-(y(iu-1)-yp(iu-1))*(delu2*delu2 &
     &      -dell2*dell2))/dels
       go to 109
       end function curvi 

!----------------------------------------------------------------
!
!            Part III
!     This part of unitilities includes various quadrature schemes
!
      subroutine qgaus(m,gmu,gwt)
!
!     compute weights and abscissae for ordinary Gaussian quadrature
!     (no weight function inside integral) on the interval [-1,1]
!
!     reference:  Davis, P.J. and P. Rabinowitz, Methods of Numerical
!                 Integration, Academic Press, New York, pp. 113-114,1984.
!
!     method:     compute the abscissae as roots of the Legendre
!                 polynomial p-sub-n using a cubically convergent
!                 refinement of Newton's method.  compute the
!                 weights from eq. 2.7.3.8 of Davis/Rabinowitz.
!
!     accuracy:  at least 13 significant digits
!
!     input variables
!
!       m         : order of quadrature rule
!
!     output variables
!
!       gmu       : array of abscissae
!       gwt       : array of weights
!
!     internal variables
!
!       pm2,pm1,p : 3 successive Legendre polynomials
!       ppr       : derivative of Legendre polynomial
!       p2pri     : 2nd derivative of Legendre polynomial
!       tol       : convergence criterion for Legendre poly root iteration
!       x,xi      : successive iterates in cubically-
!                   convergent version of Newton's method
!                   ( seeking roots of Legendre polynomial )
!+------------------------------------------------------------------+
      real     cona, gmu(*), gwt(*), pi, t
      integer  lim, np1
 !!     double   precision  d1mach
      double   precision  en, nnp1, p, pm1, pm2, ppr, p2pri, prod,&
                         tmp, tol, x, xi
      data     pi / 0.0 /
      if ( pi.eq.0.0 )  then
         pi = 2. * asin(1.0)
         tol = 10. * d1mach(3)
      end if
      if ( m.le.1 )  then
         m = 1
         gmu( 1 ) = 0.5
         gwt( 1 ) = 1.0
         return
      end if
      en   = m
      np1  = m + 1
      nnp1 = m * np1
      cona = float( m-1 ) / ( 8 * m**3 )
!                                        ** initial guess for k-th root
!                                        ** of Legendre polynomial, from
!                                        ** Davis/Rabinowitz (2.7.3.3a)
      lim  = m / 2
      do 30  k = 1, lim
         t = ( 4.0*k - 1.0 ) * pi / ( 4.0*m + 2.0)
         x = cos ( t + cona / tan( t ) )
!                                        ** recursion relation for
!                                        ** Legendre polynomials
10       pm2 = 1.d0
         pm1 = x
         do 20 nn = 2, m
            p   = ( ( 2*nn - 1 ) * x * pm1 - ( nn-1 ) * pm2 ) / nn
            pm2 = pm1
            pm1 = p
20       continue
!
         tmp   = 1.d0 / ( 1.d0 - x**2 )
         ppr   = en * ( pm2 - x * p ) * tmp
         p2pri = ( 2.d0 * x * ppr - nnp1 * p ) * tmp
         xi    = x - ( p / ppr ) * ( 1.d0 +&
                     ( p / ppr ) * p2pri / ( 2.d0 * ppr ) )
!
!                                              ** check for convergence
!         if ( dabs(xi-x) .gt. tol ) then
         if ( dabs(xi-x) .gt. 1.e-7) then
!            print*,dabs(xi-x),tol,xi,x
            x = xi
            go to 10
         end if
!                          ** iteration finished--calc. weights,
!                          ** abscissae for [-1,1]
         gmu( k ) = - x
         gwt( k ) = 2.d0 / ( tmp * ( en * pm2 )**2 )
         gmu( np1 - k ) = - gmu( k )
         gwt( np1 - k ) =   gwt( k )
30    continue
      return
      end subroutine qgaus
!



!------------------------------------------------------
!                    Part VII
!
!     This part contains miscellaneous subroutines

      double precision FUNCTION PL00(X, LL)
      implicit double precision (a-h,o-z)
!     COMPUTES THE ORDINARY LEGENDRE POLYNOMIAL
!     X      :  ARGUMENT OF OLP
!     L      :  SUB OF PL00
!     PL00   :  VALUE OF LEGENDRE POLYNORMIALS
!
      PL000 = 1.d0
      PL001 = X
      IF(LL.EQ.0) THEN
        PL00 = PL000
      ELSE IF(LL.EQ.1) THEN
        PL00 = PL001
      ELSE
        DO 20 l = 2, LL
          fac1 = (2.d0*l -1.d0)/dble(l)
          fac2 = dble(l - 1.d0)/dble(l)
          PL00 = fac1* X* PL001 - fac2 * PL000
          PL000 = PL001
          PL001 = PL00
20      CONTINUE
      ENDIF
      RETURN
      END FUNCTION PL00

      double precision FUNCTION PL02(X, LL)
!     Generalized spherical function with n = +2 and m = 0
!     in P_m^l,n(x)
      implicit double precision (a-h,o-z)
!     X      :  ARGUMENT
!     LL     :  ORDER
!     PL02   :  VALUE

      PL020 = 0.d0
      qroot6 = -.25d0*dsqrt(6.d0)
      PL021 = qroot6*(1.0d0-X*X)
      sql41 = 0.d0
      IF(LL .LT. 2) THEN
        PL02 = 0.d0
      ELSE IF(LL .EQ. 2) THEN
        PL02 = PL021
      ELSE
        DO 20 l = 3, LL
           sql4  = sql41
           sql41 = dsqrt(dble(l*l)-4.d0)
           twol1 = 2.d0*dble(l)-1.d0
           tmp1  = twol1/sql41
           tmp2  = sql4/sql41
           PL02  = tmp1*x*PL021- tmp2*PL020
           PL020 = PL021
           PL021 = PL02
20      CONTINUE
      END IF
      RETURN
      END FUNCTION PL02

      double precision FUNCTION PL22(X, LL,S)
!     Generalized spherical function with n = +(-)2  and m = 2
!     in P_m^l,n(x)
      implicit double precision (a-h,o-z)
!     X      :  ARGUMENT
!     LL     :  ORDER
!     S      :  1   n = + 2
!     S        -1   n = - 2
!     PL22   :  VALUE

      PL220 = 0.d0
      PL221 = .25d0*(1.d0 + s*x)*(1.d0 + s*x)
      IF(LL .LT. 2) THEN
        PL22 = 0.d0
      ELSE IF(LL .EQ. 2) THEN
        PL22 = PL221
      ELSE
        DO 20 l = 3, LL
           twol1 = 2.d0*dble(l)-1.d0
           denom = (dble(l) - 1.d0)*(dble(l*l)-4.d0)
           fac1  = twol1*(dble(l)-1.d0)*dble(l)/denom
           fac2  = 4.d0*twol1/denom
           fac3  = dble(l)*((dble(l)-1.d0)*(dble(l)-1.d0)-4.d0)/denom
           PL22  = (fac1*x-s*fac2)*PL221  - fac3*PL220
           PL220 = PL221
           PL221 = PL22
20      CONTINUE
      END IF
      RETURN
      END FUNCTION PL22
!!!
!     Gamma function
      REAL FUNCTION RGAMMA(X)
      REAL A(0:10)
      DOUBLE PRECISION D1,D2,D3
      DATA A/1.,0.42278433,0.41184025,0.081578217,0.074237907,    &
     &         -0.0002109075,0.010973695,-0.0024667480,0.0015397681,&
     &         -0.0003442342,0.0000677106/
      PI=ASIN(1.0)*2.0
!     X <= 3.0
      IF(X.LE.3.0) THEN
          IF (X.LE.1.0) THEN
             T=X
          ELSE IF(X.LE.2.0) THEN
             T=X-1.0
          ELSE
             T=X-2.0
          ENDIF
          P=A(10)
          DO K=9,0,-1
            P=T*P+A(K)
          ENDDO
          IF (X.LE.1.0) THEN
             RGAMMA=P/(X*(X+1))
          ELSE IF(X.LE.2.0) THEN
             RGAMMA=P/(X)
          ELSE
             RGAMMA=P
          ENDIF
        ELSE
!         STIRLING APPROXIMATION
          D1=360*X**3
          D2=1260*X**5
          D3=1680*X**7
          F=(X-0.5)*ALOG(X)-X+0.5*ALOG(2*PI)+ 1.0/(12.0*X)-1.0/D1+1.0/D2-1.0/D3
          RGAMMA=EXP(F)
      ENDIF
      RETURN
      END FUNCTION RGAMMA
!!!!
      subroutine  errmsg( messag, fatal )
!
!     print out a warning or error message;  abort if error
!
      logical       fatal, once
      character*(*) messag
      integer       maxmsg, nummsg
      save          maxmsg, nummsg, once
      data nummsg / 0 /,  maxmsg / 100 /,  once / .false. /
!
!
      if ( fatal )  then
       write ( 6, '(/,2a)' )  ' ******* Error >>>>>>  ', messag
       stop
      end if
!
      nummsg = nummsg + 1
      if ( nummsg.gt.maxmsg )  then
        if ( .not.once )  write ( 6,99 )
        once = .true.
      else
        write ( 6, '(/,2a)' )  ' ******* Warning >>>>>>  ', messag
      endif
!
      return
!
 99   format( ///,' >>>>>>  Too many warning messages --  ', &
   &   'they will no longer be printed  <<<<<<<', /// )
      end subroutine  errmsg
!


      double precision function  d1mach(i)
!
!     returns double precision machine dependent constants
!
!     d1mach( 1) = b**(emin-1),  smallest positive magnitude.
!
!     d1mach( 2) = b**emax*(1 - b**(-t)),  largest magnitude.
!
!     d1mach(3) = b**(-t),  smallest relative spacing.  i.e.,
!                 smallest number eps such that  1+eps .ne. 1
!
!     double precision  dmach(3)
!     data dmach/0.2225073858507D-307,0.1797693134862D+309, &
!  &           0.2220446049250D-015/
      if (i.eq.1) then
        d1mach = tiny(1.d0)
      else if(i.eq.2) then
        d1mach = huge(1.d0)
      else if(i.eq.3) then
        d1mach = epsilon(1.d0)
      else
        call errmsg('mac02 - trying non-existing machine constants in d1mach', .true. )
      endif
      return
      end function  d1mach

! Initialize the ice water path


   SUBROUTINE iwp_estimate(tb,blza,biwp)

   IMPLICIT NONE
   integer angle_region
   real*4 tb(*)
   real*4 a23, a31, b89, b150, blza,lza0,lza1,b89_0,b89_1,ICE_DEN,b150_1,mu,b150_0
   real*4 pred89, pred150,omega89,omega150,ratio,coef_a,coef_b,coef_d,coef_c,bde
   real*4 omega,coef_iwp_a,coef_iwp_b, coef_iwp_c,omegan,biwp


! Initializations

   if (abs(blza) <10.0 ) angle_region = 1

   if (abs(blza) >=10.0 .and. abs(blza) <20.0  ) angle_region = 2
   if (abs(blza) >=20.0 .and. abs(blza) <30.0  ) angle_region = 3
   if (abs(blza) >=30.0 .and. abs(blza) <40.0  ) angle_region = 4
   if (abs(blza) >=40.0 .and. abs(blza) <50.0  ) angle_region = 5
   if (abs(blza) >=50.0 ) angle_region = 6
   biwp = 0.05
   ICE_DEN = 920.0

! Read inputs

    a23 = tb(1)
    a31 = tb(2)
    b89 = tb(4)
    b150 =tb(5)
    mu = cos(blza * acos(-1.0) / 180.)

   GET_option: SELECT CASE (angle_region)

   CASE (1)
            lza0=0.0
            lza1=10.0
            b89_0=183.073-0.649864*b89+0.00355388*b89*b89
            b150_0=89.4492+0.133525*b150+0.00193974*b150*b150
            b89_1=168.617-0.526129*b89+0.00329590*b89*b89
            b150_1=85.7358+0.169666*b150+0.00185847*b150*b150
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0


   CASE (2)
            lza0=10.0
            lza1=20.0
            b89_0=168.617-0.526129*b89+0.00329590*b89*b89
            b150_0=85.7358+0.169666*b150+0.00185847*b150*b150
            b89_1=135.886-0.239320*b89+0.00268872*b89*b89
            b150_1=72.1034+0.300571*b150+0.00156526*b150*b150
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0


   CASE (3)

            lza0=20.0
            lza1=30.0
            b89_0=135.886-0.239320*b89+0.00268872*b89*b89
            b150_0=72.1034+0.300571*b150+0.00156526*b150*b150
            b89_1=99.8433+0.0911668*b89+0.00196905*b89*b89
            b150_1=51.6176+0.501623*b150+0.00110930*b150*b150
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0
   CASE (4)


            lza0=30.0
            lza1=40.0
            b89_0=99.8433+0.0911668*b89+0.00196905*b89*b89
            b150_0=51.6176+0.501623*b150+0.00110930*b150*b150
            b89_1=52.4938+0.535288*b89+0.000986296*b89*b89
            b150_1=26.8442+0.753185*b150+0.000528123*b150*b150
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0

   CASE (5)
            lza0=40.0
            lza1=50.0
            b89_0=52.4938+0.535288*b89+0.000986296*b89*b89
            b150_0=26.8442+0.753185*b150+0.000528123*b150*b150
            b89_1=7.92203+0.981133*b89-0.0000394*b89*b89
            b150_1=-2.74337+1.06524*b150-0.000209793*b150*b150
            b89=(b89_1-b89_0)*(abs(blza)-lza0)/(lza1-lza0)+b89_0
            b150=(b150_1-b150_0)*(abs(blza)-lza0)/(lza1-lza0)+b150_0


   CASE (6)
            b89=7.92203+0.981133*b89-0.0000394*b89*b89
            b150=-2.74337+1.06524*b150-0.000209793*b150*b150

   END SELECT GET_option


    pred89 = 17.88 + 1.61* a23 - 0.67 * a31
    pred150 = 33.78 + 1.69* a23 - 0.80* a31
    omega89 = (pred89 - b89)/b89
    omega150 = (pred150 - b150)/b150
    ratio = omega89/omega150

    coef_a = -0.300323
    coef_b = 4.30881
    coef_c = -3.98255
    coef_d = 2.78323
    bde = coef_a + coef_b * ratio + coef_c * ratio**2+ coef_d * ratio**3


! /* Calculate the ice water path   */
    omega=omega89
    coef_iwp_a = -1.19301
    coef_iwp_b = 2.08831
    coef_iwp_c = -0.857469
    if(bde <= 1.0) then
        omega=omega150
        coef_iwp_a = -0.294459
        coef_iwp_b = 1.38838
        coef_iwp_c = -0.753624
    endif

    if (bde > 0.0) then
        omegan = exp( coef_iwp_a + coef_iwp_b * alog(bde)+  coef_iwp_c * alog(bde)**2 )
        if(omegan > 0.0) biwp = (omega * bde * 0.001 * mu * ICE_DEN / omegan)

    endif


   END SUBROUTINE iwp_estimate


   SUBROUTINE NONSPHERE(SHAPE_INDEX,SIZE_PARM,AREA,SLAM,freq,QE,QS,gg_nonsphere)

!!!!!!!!!!!!!!!
!  Calculate scattering cross section QS and extinction cross section QE
!  based on Guosheng Liu's (J. Atm. Sci. 2004) polynomial fitting functions 
!  with various snow particle shapes (rosettes and snow flakes).
!!!!!!!!!!!!!!!


   IMPLICIT NONE
   integer shape_index,count,i
   real SIZE_PARM,AREA,SLAM,freq,QE,QS,Qabs,gg_nonsphere
   doubleprecision pi,logx
   doubleprecision    a1,a2,a3,c0,c1,c2,c3
   doubleprecision    coea(4,8),coef(5,8)

   data (coea(i,1),i=1,4)/-0.3353,-0.3533,-0.3597,-0.3432/
   data (coea(i,2),i=1,4)/3.3177,3.3295,3.3643,3.4542/
   data (coea(i,3),i=1,4)/-1.7217,-1.6769,-1.5013,-1.4338/
   data (coea(i,4),i=1,4)/-1.7254,-1.9710,-2.0822,-2.6021/
   data (coea(i,5),i=1,4)/-0.1953,-0.5256,-1.2714,-2.2706/
   data (coea(i,6),i=1,4)/0.7358,1.1379,0.9382,1.1111/
   data (coea(i,7),i=1,4)/0.4084,1.1043,1.6981,2.8529/
   data (coea(i,8),i=1,4)/0.0554,0.2963,0.6088,1.1258/

   data (coef(i,1),i=1,5)/-0.6304,-0.5673,-0.5832,-0.6122,-0.4654/
   data (coef(i,2),i=1,5)/1.5281,1.5418,1.6818,2.3329,-3.9724/
   data (coef(i,3),i=1,5)/-0.2125,-1.0410,-1.0855,3.6036,81.0301/
   data (coef(i,4),i=1,5)/-0.9502,-1.0442,-1.4262,13.9784,-504.904/
   data (coef(i,5),i=1,5)/-1.7090,-0.0600,-0.2155,26.3336,1569.3/
   data (coef(i,6),i=1,5)/0.1557,0.8422,1.0944,26.3125,-2620.1/
   data (coef(i,7),i=1,5)/1.4016,0.6686,0.8690,13.4166,2230.9/
   data (coef(i,8),i=1,5)/0.5477,0.1597,0.1937,2.7443,-757.586/

 !  SHAPE_INDEX = 1  ! 1: rosettes (Liu); 2. snowflake-A (Liu); 3. snowflake-B (Liu); 
 !                   ! 4: cylindrical column; 5: 2-cylinder aggregate; 
 !                   ! 6: 3-cylinder aggregate; 7: 4-cylinder aggregate

! Initialization

   PI = ACOS( -1.0 )

   freq = 300./SLAM
   
   gg_nonsphere  = 0.0

! G. Liu's empirical equations

  if(shape_index .le. 3) then

   GET_option_Liu: SELECT CASE (shape_index)      

   CASE (1)  ! rosettes 

     if (size_parm .le. 2.2) then
       a1 = -0.036379
       a2 = 0.11716
       a3 = 0.18637
     else
       a1 = -0.60643
       a2 = 1.0934
       a3 = -0.1463
     endif

     if (size_parm .le. 1.0) then
       c0 = 0.
       c1 = -0.077361
       c2 = 0.59902
       c3 = -0.0018825
     else
       c0 = 0.30617
       c1 = 0.019795
       c2 = 0.029307
       c3 = -0.00029968
     endif

   CASE (2)  ! type-A snowflake
     if (size_parm .le. 1.4) then
       a1 = -0.036379
       a2 = 0.11716
       a3 = 0.18637
     else
       a1 = -0.1622
       a2 = 0.56253
       a3 = -0.066369
     endif

     if (size_parm .le. 1.0) then
       c0 = 0.
       c1 = -0.077361
       c2 = 0.59902
       c3 = -0.0018825
     else
       c0 = 0.42725
       c1 = 0.062429
       c2 = 0.028416
       c3 = -0.0042245
     endif

   CASE (3)  ! type-B snowflake
     if (size_parm .le. 0.5) then
       a1 = -0.036379
       a2 = 0.11716
       a3 = 0.18637
     else
       a1 = -0.0096948
       a2 = 0.15898
       a3 = 0.01078
     endif

     if (size_parm .le. 1.0) then
       c0 = 0.
       c1 = -0.077361
       c2 = 0.59902
       c3 = -0.0018825
     else
       c0 = 0.42725
       c1 = 0.062429
       c2 = 0.028416
       c3 = -0.0042245
     endif

   END SELECT GET_option_Liu

   QS = AREA*(a1*size_parm+a2*size_parm**2+a3*size_parm**3)

   Qabs = AREA*(0.007446*size_parm+0.010607*size_parm**2-0.0014505*size_parm**3)


!B.YAN added a quality control

   if (QS .lt. 0.0) QS = 0.001

   if (Qabs .lt. 0.0) Qabs = 0.001

   QE = QS + Qabs

   gg_nonsphere = c0+c1*size_parm+c2*size_parm**2+c3*size_parm**3

! B.YAN

   if (gg_nonsphere .lt. 0.0) gg_nonsphere = 0.0

  else

! M. Kim's empirical equations

! Absorption cross section only depends on frequency, not on snow crystal shape.

   call CAL_QABS(size_parm,AREA,freq,Qabs)

! Scattering cross section and asymmetry factor depend on snow crystal shape.

   logx = log10(size_parm)
   QS = 0.0
   gg_nonsphere = 0.0

   GET_option_Kim: SELECT CASE (shape_index)

   CASE (4)  ! Cylindrical column 
     do count=1, 8
       QS = QS + coea(1,count)*logx**(count-1)
       gg_nonsphere = gg_nonsphere + coef(1,count)*logx**(count-1)
     enddo
   CASE (5)  ! 2-Cylinder aggregate
     do count=1, 8
       QS = QS + coea(2,count)*logx**(count-1)
       gg_nonsphere = gg_nonsphere + coef(2,count)*logx**(count-1)
     enddo
   CASE (6)  ! 3-Cylinder aggregate 
     do count=1, 8
       QS = QS + coea(3,count)*logx**(count-1)
       gg_nonsphere = gg_nonsphere + coef(3,count)*logx**(count-1)
     enddo
   CASE (7)  ! 4-Cylinder column 
     do count=1, 8
       QS = QS + coea(4,count)*logx**(count-1)
       if(size_parm .lt. 1.) then
         gg_nonsphere = gg_nonsphere + coef(4,count)*logx**(count-1)
       else
         gg_nonsphere = gg_nonsphere + coef(5,count)*logx**(count-1)
       endif
     enddo
   END SELECT GET_option_Kim
   
   QS = AREA*10.**QS
 
   gg_nonsphere = 10.**gg_nonsphere

  endif
       
   RETURN

   END SUBROUTINE NONSPHERE 
!!
!!

   SUBROUTINE CAL_QABS(size_parm,AREA,freq,Qabs)
!!!!!!!!!!!!!!!
!  Compute absorption cross sections based on M. Kim's empirical equations, JGR 111
!!!!!!!!!!!!!!!
  
   integer	      count,count_fix
   real AREA,freq,Qabs,size_parm,qabs1,qabs2,coeb_freq(5)
   doubleprecision    coeb(5,6)

   data coeb_freq/95., 140., 183., 220., 340./

   data (coeb(i,1),i=1,5)/1.508E-04,1.122E-04,-6.598E-04,0.0019,0.0063/
   data (coeb(i,2),i=1,5)/0.0021,0.0061,0.0153,8.275E-04,-0.0145/
   data (coeb(i,3),i=1,5)/0.0081,0.0086,-0.0032,0.0189,0.0502/
   data (coeb(i,4),i=1,5)/-0.0051,-0.0022,0.0062,-0.0022,-0.0105/
   data (coeb(i,5),i=1,5)/0.002,5.35E-04,-0.0014,-4.49E-05,6.998E-04/
   data (coeb(i,6),i=1,5)/-2.596E-04,-4.82E-05,8.49E-05,1.24E-05,8.68E-07/

! Initialization

  Qabs = 0.0
  qabs1 = 0.0
  qabs2 = 0.0

! Update Qabs

   if(freq.le.coeb_freq(1)) then  ! if frquency is less or equal to 95 GHz
     do count=1,6
       Qabs=Qabs+coeb(1,count)*size_parm**(count-1)
     enddo
     Qabs=AREA*Qabs

   else if(freq.ge.coeb_freq(5)) then ! if frequency is greater or equal to 340 GHz
     do count=1,6
       Qabs=Qabs+coeb(5,count)*size_parm**(count-1)
     enddo
     Qabs=AREA*Qabs

   else ! if frequency falls in between 95 and 340 GHz
     do count=1,4
      if(freq.ge.coeb_freq(count).and.freq.lt.coeb_freq(count+1)) count_fix=count
     enddo
      
     do count=1,6
       qabs1=qabs1+coeb(count_fix,count)*size_parm**(count-1)
       qabs2=qabs2+coeb(count_fix+1,count)*size_parm**(count-1)
     enddo

! Linear interpolation
     Qabs=qabs2+(qabs1-qabs2)*(freq-coeb_freq(count_fix+1))/(coeb_freq(count_fix)-coeb_freq(count_fix+1))
   endif

   RETURN

   END SUBROUTINE CAL_QABS


END MODULE GET_EMISS_1DALG
