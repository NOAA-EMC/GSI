SUBROUTINE GFS_PRECIP_AD(IM, IX, KM, DT, DEL, SL, PS, T_in,Q_in,CWM_in,RHC,&
                     T_out, Q_out, CWM_out, RN_out, &
                     dt_ad, dQ_ad, dCWM_ad, dRN_ad, &  !output
                     dt_out_ad, dq_out_ad, dcwm_out_ad, drn_out_ad,adjoint)   !input

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gfs_precip_ad forward & adjoint model for GFS precipitation physics
!     prgmmr:   Min-Jeong Kim     org: np23                date: 2012-02-19
!
! abstract:  This subroutine contains the forward and adjoint models for the
!            GFS precipitation microphysics scheme 
!
! program history log:
!   2012-02-19  kim - initial routine
!
!$$$

USE kinds, only: r_single, r_kind, i_kind

USE constants, only: HVAP, HFUS, CP, EPS, EPSM1, RCP,TTP, G=>grav, &
              EPSQ, ZERO, ONE, TWO, HALF,climit,cclimit, ke2,cws, cmr, HSUB,rrow, &
              tiny_r_kind, huge_r_kind, init_constants_derived, init_constants
IMPLICIT NONE

real(r_kind), parameter::  ELIV = HVAP + HFUS
real(r_kind), parameter::  ELWV = HVAP
real(r_kind), parameter::  ELIW = ELIV - ELWV


INTEGER(i_kind) :: IM, IX, KM, LAT, IWL1, IWL, IWL_save1(IX, KM)
INTEGER(i_kind), dimension(IM,KM) :: IW, IWL1k

REAL(r_kind) :: DT, Zaodt, precrk0, precsk0, wwn, qk, qs, &
                rq, ccr, precrl_0, precsl_0,precrl1k, precsl1k, &
                tmt0,term,term1,term2, term3, term4,term5, term6, &
                term2s, term2r, term3s, term3r, conde,q2, &
                ww0, ww1, ww2, wmin, ppr1, pres,condt, rconde,es,&
                ers, err, praut0, precsl_1, dum

REAL(r_kind), dimension(IX,KM) :: Q_in, q_out, T_in, T_out, cwm_out, cwm_in, &
                             DEL, PRSL, SL, precrl1ki,precsl1ki
REAL(r_kind), dimension(IM) :: PS, RN_out, SR, TCW, PRECRL, &
                               PRECSL, PRECRL1, PRECSL1, &
                               TT, QQ, WW

REAL(r_kind), dimension(KM) :: CCLIM, WFIX
REAL(r_kind), dimension(IM,KM) :: RHC, RHCI

LOGICAL, dimension(IM) :: comput0
LOGICAL :: comput


REAL(r_kind) :: ke,   rdt,  csm1, pps0,ppr0, &
                crs1, crs2, cr, aa2,     dtcp,   c00, &
                tem,  c1,   c2, onem10, onem20,ppr2, pps1, precsk1,precrk1,&
                precrk, precsk, pres1,   qw,  qi, &
                ai, bi, qint, fiw, wws, cwmk, expf, &
                psaut, psaci, amaxcmr, amaxcms, tem1, tem2, tem3, tem4, tem5, &
                tmt0k, tmt15, psm1, psm2, psm3, ppr,  &
                rprs,  erk,   pps, sid, rid, amaxps, &
                praut, pracw, fi, qc, amaxrq, rqkll, es0,&
                precsl_3, precrl_3, precsl_2, precrl_2, precrl_1,t2, pps2, &
                tin, qin, rke2, const, cwmin, cwmks, qs0

real(r_kind), dimension(ix,km) :: precrl_0_save1, precsl_0_save1, precrk0_save1, precsk0_save1, &
                         wwn_save1,dqs0_dt_save1, qk_save1, qs_save1, rqkll_save1, rq_save1, &
                         ww0_save1, ccr_save1, tmt0_save1, term1_save1, &
                         expf_save1, amaxcms_save1, term2s_save1, ww1_save1, term3s_save1, &
                         cwmks_save1, &
                         cwmk_save1, term2r_save1, term3r_save1, praut_save1,&
                         tem1_save1, tem2_save1, tem3_save1, term5_save1, &
                         amaxcmr_save1, tem6_save1, tem5_save1, term6_save1, tem4_save1, &
                         precrl_1_save1, precsl_1_save1,precrk1_save1, precsk1_save1, &
                         amaxrq_save1, term_save1, tmt0k_save1, ppr0_save1, ppr0_save2,pps0_save1, erk_save1, &
                         rprs_save1, ppr1_save1, pps1_save1, precsl_2_save1, precrl_2_save1, &
                         amaxps_save1, ww2_save1, ww2_save2, precsl_3_save1, precrl_3_save1, qs0_save1
INTEGER(i_kind):: i, k
real(r_kind) :: tmp,dtmp_ad
real(r_kind),dimension(ix,km) :: tin_save1,es_save1 
logical, intent(in) :: adjoint
!-------------------------
! AD related variables
!-------------------------

real(r_kind) :: dprecrl_0_ad, dprecsl_0_ad,  &
 dtin_ad,dqin_ad,dcwmin_ad,dprecrk0_ad,dprecsk0_ad,dwwn_ad,dqk_ad,dtmt0_ad,&
 dqs0_des,des_ad,dqs0_dt, dqs0_ad,dqs_ad,&
 dfi_ad, drq_ad, dccr_ad, drqkll_ad, dww0_ad, dcwmk_ad, dterm1_ad, &
 damaxcms_ad, damaxcmr_ad, dexpf_ad, dterm2s_ad, dpsaut_ad, &
 dww1_ad, dcwmks_ad, dterm3s_ad, dpsaci_ad, dww2_ad, dprecsl_1_ad, &
 dprecrl_1_ad, dtem1_ad, dterm2r_ad, dterm3r_ad, &
 dterm4_ad, dtem2_ad, dtem3_ad, dtem4_ad, dterm5_ad, &
 dterm6_ad, dtem5_ad, dpraut_ad, dpraut0_ad, &
 dtmt0k_ad, dprecrk1_ad, dprecsk1_ad, dterm_ad, damaxrq_ad, &
 dppr0_ad, dpps0_ad, derk_ad, drprs_ad, &
 dppr1_ad, dppr2_ad, dpps1_ad, dpps2_ad, derr_ad,  &
 ders_ad, dprecrl_2_ad, dprecsl_2_ad, damaxps_ad, &
 dpsm1_ad, dpsm2_ad, dpsm3_ad, dprecrl_3_ad, dprecsl_3_ad,&
 dt2_ad, dq2_ad, dprecrl1k_ad, dprecsl1k_ad

real(r_kind), dimension(ix,km) ::  dq_out_ad,dt_out_ad,dcwm_out_ad, &
                                   dq_ad,dt_ad,dCWM_ad
real(r_kind), dimension(ix) :: drn_out_ad, drn_ad


!-----------------------Preliminaries ---------------------------------

call init_constants_derived
call init_constants(.false.)

onem10  = 1.0e-10_r_kind
onem20  = 1.0e-20_r_kind
RDT     = ONE / DT
zaodt   = 800.0_r_kind * RDT
CSM1    = 5.0000E-8_r_kind   * zaodt
CRS1    = 5.00000E-6_r_kind  * zaodt
CRS2    = 6.66600E-10_r_kind * zaodt
CR      = 5.0E-4_r_kind      * zaodt
AA2     = 1.25E-3_r_kind     * zaodt
rke2    = ke2 * sqrt(rdt)
DTCP    = DT * RCP
C00 = 1.0E-4_r_kind * DT          !05/09/2000

do i = 1, im  
  do k = 1, km
     dq_ad(i,k) = zero
     dt_ad(i,k) = zero
     dcwm_ad(i,k) = zero
  enddo
rn_out(i) = zero
drn_ad(i) = zero
enddo

do i = 1, im
  do k = 1, km
    RHCI(i,k) = one/RHC(i,k)
  enddo
enddo


!------------SELECT COLUMNS WHERE RAIN CAN BE PRODUCED--------------

DO I=1,IM
  comput0(i) = .false.
  do k = 1, KM
     tem   = 0.00001_r_kind * sl(i,k) * ps(i) * 0.01_r_kind
     if(cwm_in(i,k) .gt. tem) then
       comput0(i) = .true.
     endif
  enddo

  iwl1 = 0
  precrl1k = zero
  precsl1k = zero
  rn_out(i) = zero
  const = ps(i) * (1000.0_r_kind*dt/G)
   precrl_0 = zero 
   precsl_0 = zero

  if (comput0(i)) then
!***********************************************************************
!-----------------BEGINING OF PRECIPITATION CALCULATION-----------------
!***********************************************************************
      DO K=KM,1,-1
         wmin = 0.00001_r_kind*sl(i,k)*ps(i)*0.01_r_kind
         pres = ps(i) * sl(i,k)

         precrl_0_save1(i,k) = precrl_0
         precsl_0_save1(i,k) = precsl_0


         iwl = 0
         tin = t_in(i,k)
         qin = q_in(i,k)
         cwmin = cwm_in(i,k)
         pres = ps(i) * sl(i,k)

         if (precrl_0 .gt. zero) then
           precrk0 = precrl_0
         else
           precrk0 = zero
         endif

         if (precsl_0 .gt. zero) then
            precsk0 = precsl_0
         else
            precsk0 = zero
         endif
          precrk0_save1(i,k) = precrk0
          precsk0_save1(i,k) = precsk0

         if (cwmin .gt. climit ) then
            wwn = cwmin
         else
            wwn = climit
         endif

           wwn_save1(i,k) = wwn

        if (wwn .gt. climit .or. precrk0+precsk0 .gt. zero) then
            comput = .true.
         else
            comput = .false.
        endif

        if (comput) then
            CONDE  = const * DEL(I,K)
            CONDT  = CONDE * RDT
            RCONDE = ONE / CONDE

            if (qin .gt. epsq ) then
              qk    = qin
            else if(qin .le. epsq) then
              qk = epsq
            endif
            qk_save1(i,k)= qk

            tmt0  = tin - ttp
            tmt0_save1(i,k) = tmt0
            call fpvsx_ad(tin,es,dum, dum, .false.)
            tin_save1(i,k) = tin
            es_save1(i,k) = es
            qs0 = eps*es/(pres+epsm1*es)
            qs0_save1(i,k) = qs0 

            if (qs0 .gt. epsq) then
               qs = qs0
            else
               qs = epsq
            endif
            qs_save1(i,k) = qs

!-------------------ICE-WATER ID NUMBER IW------------------------------
            IF(TMT0 .LT. -15.0_r_kind) THEN
               FI = QK - rhc(i,k) *Qs
               IF(FI.GT.ZERO.OR.WWN.GT.CLIMIT) THEN
                  iwl = 1
               ELSE
                  iwl = 0
               ENDIF
            ELSEIF (tmt0 .GE. ZERO) THEN
               iwl = 0
            ELSE
               iwl = 0
               IF(IWL1 .EQ.1 .AND. WWN.GT.CLIMIT) then
                  iwl = 1
               ENDIF
            ENDIF
            iwl_save1(i,k) = iwl

!----------------THE RELATIVE HUMIDITY----------------------------------

            IF(qs .LE. 1.0E-10_r_kind) THEN
               RQ  = ZERO
            ELSE
               RQ  = QK / QS
            ENDIF
            rq_save1(i,k) = rq
!----------------CLOUD COVER RATIO CCR----------------------------------
            IF(RQ.LT.rhc(i,k)) THEN
              CCR = ZERO
            ELSEIF(RQ.GE.ONE) THEN
              CCR = ONE
            ELSE
              if (RQ .lt. ONE) then
                rqkll = RQ
              else
                rqkll = one
              endif
              rqkll_save1(i,k) = rqkll
              CCR = ONE-SQRT((ONE-RQKLL)/(ONE-RHC(I,K)+tiny_r_kind))
            ENDIF
            ccr_save1(i,k) = ccr

            IF(CCR .gt. ZERO) then
              ww0 = cwmin
              ww0_save1(i,k) = ww0
              if (ww0 .gt. ZERO) then
                 cwmk = ww0
              else
                 cwmk = ZERO
              endif
              cwmk_save1(i,k) = cwmk

              if(IWL .EQ. 1) THEN                 !  Ice Phase
                 term1 = cwmk - wmin
                 term1_save1(i,k) = term1
                 if(term1 .gt. zero) then
                   AMAXCMS = term1
                 else
                   AMAXCMS = zero
                 endif
                  amaxcms_save1(i,k) = amaxcms
                 expf   = DT * EXP(0.025_r_kind*TMT0)
                  expf_save1(i,k) = expf
                 term2s = 0.0004_r_kind*expf*amaxcms
                 term2s_save1(i,k) = term2s

                 if (term2s .lt. cwmk) then
                    psaut = term2s
                 else
                   psaut = cwmk
                 endif
                 ww1 = ww0 - psaut
                 ww1_save1(i,k) = ww1

                 if(ww1 .gt. zero) then
                   cwmks = ww1
                 else
                   cwmks = zero
                 endif
                  cwmks_save1(i,k) = cwmks

                 term3s = aa2 * expf * precsl_0*cwmks
                 term3s_save1(i,k) = term3s

                 if (term3s .lt. cwmks) then
                   psaci = term3s
                 else
                   psaci = cwmks
                 endif
                 ww2 = ww1 - psaci
                 precsl_1 = precsl_0+(ww0-ww2)*condt
                 precrl_1 = precrl_0
              else
!          For using Sundqvist precip formulation of rain
                amaxcmr = cwmk
                amaxcmr_save1(i,k) = amaxcmr
                tem1 = precsl_0 + precrl_0
                tem1 = max(tem1,1.0e-20_r_kind)
                tem1_save1(i,k) = tem1
                term2r = 268.0_r_kind - tin
                term2r_save1(i,k) = term2r
                if(term2r .gt. zero) then
                  term3r = term2r
                else
                  term3r =  tiny_r_kind
                endif
                term3r_save1(i,k) = term3r
                if(term3r .lt. 20_r_kind) then
                  term4 = term3r
                else
                 term4 = 20.0_r_kind
                endif

                tem2 = term4
                tem2 = max(tem2,1.0e-20_r_kind)
                tem2_save1(i,k) = tem2


                tem3 = (ONE + 300.0_r_kind*sqrt(tem1*rdt)) * (1+HALF*sqrt(tem2))
                tem3_save1(i,k) = tem3

                if(ccr .gt. 0.01_r_kind) then
                  term5 = ccr
                else
                  term5 = 0.01_r_kind
                endif
                term5_save1(i,k) = term5

                tem4 = amaxcmr*cmr*tem3/term5
                tem4_save1(i,k) = tem4

                term6 = tem4*tem4
                term6_save1(i,k) = term6

                if(term6 .lt. 50.0_r_kind) then
                  tem5 = term6
                else
                  tem5 = 50.0_r_kind
                endif
                tem5_save1(i,k) = tem5

                praut = c00*tem3*amaxcmr*(one-exp(-tem5))
                praut_save1(i,k) = praut

                if(praut .lt. cwmk) then
                  praut0 = praut
                else
                  praut0 = cwmk
                endif

                ww2 = ww0 - praut0
                precrl_1 = precrl_0 + (ww0-ww2)*condt
                precsl_1 = precsl_0
              endif !if(IWL .EQ. ONE)
            ELSE ! IF(CCR .gt. ZERO) then
               ww2 = cwmin
               precrl_1 = precrl_0
               precsl_1 = precsl_0
            ENDIF  ! IF(CCR .gt. ZERO) then
            precrl_1_save1(i,k) = precrl_1
            precsl_1_save1(i,k) = precsl_1
            ww2_save1(i,k) = ww2
!-----------
! Evaporation
            if (tmt0 .gt. (-30._r_kind)) then
               tmt0k = tmt0
            else
               tmt0k = -30._r_kind
            endif
            tmt0k_save1(i,k) = tmt0k

            if (precrl_1 .gt. zero) then
              precrk1 = precrl_1
            else
              precrk1 =  tiny_r_kind
            endif
            precrk1 = max(precrk1,1.0e-20_r_kind)
            precrk1_save1(i,k) = precrk1

            if (precsl_1 .gt. zero) then
               precsk1 = precsl_1
            else
               precsk1 =  tiny_r_kind
            endif
            precsk1 = max(precsk1,1.0e-20_r_kind)
            precsk1_save1(i,k) = precsk1

            term = rhc(i,k)-rq
            term_save1(i,k) = term
            if (term .gt. zero) then
               amaxrq = term*conde
            else
               amaxrq = zero
            endif
            amaxrq_save1(i,k) = amaxrq

            ppr0 = rke2*amaxrq*sqrt(precrk1)

            if (tmt0 .ge. zero) then
               pps0 = zero
            else
               pps0 = (crs1+crs2*tmt0k)*amaxrq*precsk1*rhci(i,k)
            endif

            ppr0_save1(i,k) = ppr0
            pps0_save1(i,k) = pps0

            erk = precrk1+precsk1
            if (rq .ge. onem10) then
               erk = amaxrq*qk*rdt/rq
            endif
            erk_save1(i,k) = erk

            if (ppr0+pps0 .gt. abs(erk)) then
              rprs = erk/(precrk1+precsk1)
              rprs_save1(i,k) = rprs
              ppr1 = precrk1*rprs
              pps1 = precsk1*rprs
            else
              ppr1 = ppr0
              pps1 = pps0
            endif

            ppr1 = max(ppr1,1.0e-20_r_kind)
            pps1 = max(pps1,1.0e-20_r_kind)

            ppr1_save1(i,k) = ppr1
            pps1_save1(i,k) = pps1

            if (ppr1 .lt. precrk1) then
               ppr2 = ppr1
            else
               ppr2 = precrk1
            endif

            if (pps1 .lt. precsk1) then
               pps2 = pps1
            else
               pps2 = precsk1
            endif

            err = ppr2*rconde
            ers = pps2*rconde

            precrl_2 = precrl_1 - ppr2
            precsl_2 = precsl_1-pps2
            precrl_2_save1(i,k) = precrl_2
            precsl_2_save1(i,k) = precsl_2

!-----------
!  Melting

            if (tmt0 .gt. zero) then
              if (precsl_2 .gt. zero) then
                 amaxps = precsl_2
              else
                 amaxps = zero
              endif
              amaxps_save1(i,k) = amaxps

              psm1 = csm1*tmt0*tmt0*amaxps
              if (ww2 .gt. zero) then
                 psm2 = cws*cr*ww2*amaxps
              else
                 psm2 = zero
              endif
              ppr0 = (psm1+psm2)*conde
              ppr0_save2(i,k) = ppr0

              if (ppr0 .gt. amaxps) then
                 ppr1 = amaxps
                 psm3 = amaxps*rconde
              else
                 ppr1 = ppr0
                 psm3 = psm1
              endif
              precrl_3 = precrl_2 + ppr1
              precsl_3 = precsl_2 - ppr1
            else
              psm3 = zero
              precrl_3 =  precrl_2
              precsl_3 =  precsl_2
            endif ! if (tmt0 .gt. zero)
!-----------
            t2 = tin-dtcp*(hvap*err+hsub*ers+hfus*psm3)
            q2 = qin+dt*(err+ers)
        else   ! if(comput)
            t2 = tin
            q2 = qin
            precrl_3 = precrl_0
            precsl_3 = precsl_0
            ww2 = cwmin
        endif   ! if (comput)
        precrl_3_save1(i,k) = precrl_3
        precsl_3_save1(i,k) = precsl_3
        ww2_save2(i,k) = ww2
!------------------
        iwl1 = iwl
        if (precrl_3 .gt. zero) then
           precrl1k = precrl_3
        else
           precrl1k = zero
        endif
        if (precsl_3 .gt. zero) then
           precsl1k = precsl_3
        else
           precsl1k = zero
        endif

        if (precrl_3 .gt. zero) then
           precrl_0 = precrl_3
        else
           precrl_0 = zero
        endif
        if (precsl_3 .gt. zero) then
           precsl_0 = precsl_3
        else
           precsl_0 = zero
        endif

        if (ww2 .lt. zero ) then
          q_out(i,k) = q2+ww2
          t_out(i,k) = t2-hvap*rcp*ww2
          cwm_out(i,k) = zero
        else
          q_out(i,k) = q2
          t_out(i,k) = t2
          cwm_out(i,k) = ww2
        endif
    ENDDO
    rn_out(i) = (precrl1k + precsl1k)*rrow
  else
    do k = 1, km
         q_out(i,k)   = q_in(i,k)
         t_out(i,k)   = t_in(i,k)
         cwm_out(i,k) = cwm_in(i,k)
    end do
    rn_out(i) = zero
  endif  ! if(comput0(i))
ENDDO ! DO I = 1, IM

 if (.not.adjoint) goto 101

!-----------------------
!!! AD starts here !!
!-----------------------
  DO I =1, IM
    dprecrl_0_ad  = zero
    dprecsl_0_ad = zero
    dtin_ad = zero
    dqin_ad = zero
    dcwmin_ad = zero
    dprecrk0_ad = zero
    dprecsk0_ad = zero
    dwwn_ad = zero
    dqk_ad = zero
    dtmt0_ad = zero
    dqs0_ad = zero
    dqs_ad = zero
    dfi_ad = zero
    drq_ad = zero
    dccr_ad = zero
    drqkll_ad = zero
    dww0_ad = zero
    dcwmk_ad = zero
    damaxcms_ad = zero
    damaxcmr_ad = zero
    dexpf_ad = zero
    dpsaut_ad = zero
    dww1_ad = zero
    dcwmks_ad = zero
    dpsaci_ad = zero
    dww2_ad = zero
    dprecsl_1_ad = zero
    dprecrl_1_ad = zero
    dterm1_ad = zero
    dterm2r_ad = zero
    dterm3r_ad = zero
    dterm2s_ad = zero
    dterm3s_ad = zero
    dterm4_ad = zero
    dterm5_ad = zero
    dterm6_ad = zero
    dtem1_ad = zero
    dtem2_ad = zero
    dtem3_ad = zero
    dtem4_ad = zero
    dtem5_ad = zero
    dpraut_ad = zero
    dpraut0_ad = zero
    dtmt0k_ad = zero
    dprecrk1_ad = zero
    dprecsk1_ad = zero
    dterm_ad = zero
    damaxrq_ad  = zero
    dppr0_ad = zero
    dpps0_ad = zero
    derk_ad =  zero
    drprs_ad = zero
    dppr1_ad = zero
    dppr2_ad = zero
    dpps1_ad = zero
    dpps2_ad = zero
    derr_ad = zero
    ders_ad = zero
    dprecrl_2_ad = zero
    dprecsl_2_ad = zero
    damaxps_ad = zero
    dpsm1_ad = zero
    dpsm2_ad = zero
    dpsm3_ad = zero
    dprecrl_3_ad = zero
    dprecsl_3_ad = zero
    dt2_ad = zero
    dq2_ad = zero
    dprecrl1k_ad = zero
    dprecsl1k_ad = zero
    des_ad = zero

    const = ps(i) * (1000.0_r_kind*dt/G)
    if(comput0(i))  then

      dprecrl1k_ad = dprecrl1k_ad + rrow*drn_out_ad(i)
      dprecsl1k_ad = dprecsl1k_ad + rrow*drn_out_ad(i)
      drn_out_ad(i) = zero

      DO K = 1, KM
        wmin = 0.00001_r_kind*sl(i,k)*ps(i)*0.01_r_kind
        pres = ps(i) * sl(i,k)

        if (wwn_save1(i,k) .gt. climit .or. precrk0_save1(i,k)+precsk0_save1(i,k) .gt. zero) then
           comput = .true.
        else
          comput = .false.
        endif

        if(ww2_save2(i,k) .lt. zero) then
          dcwm_out_ad(i,k)  = zero
          dt2_ad = dt2_ad + dt_out_ad(i,k)
          dww2_ad = dww2_ad -hvap*rcp*dt_out_ad(i,k)
          dt_out_ad(i,k) = zero

          dq2_ad = dq2_ad + dq_out_ad(i,k)
          dww2_ad = dww2_ad + dq_out_ad(i,k)
          dq_out_ad(i,k) = zero
        else
          dww2_ad = dww2_ad + dcwm_out_ad(i,k)
          dcwm_out_ad(i,k) = zero

          dt2_ad = dt2_ad + dt_out_ad(i,k)
          dt_out_ad(i,k) = zero

          dq2_ad = dq2_ad + dq_out_ad(i,k)
          dq_out_ad(i,k) = zero
        endif


        if(precsl_3_save1(i,k) .gt. zero) then
           dprecsl_3_ad = dprecsl_3_ad + dprecsl1k_ad
           dprecsl1k_ad = zero
        else
           dprecsl1k_ad = zero
        endif

        if(precrl_3_save1(i,k) .gt. zero) then
           dprecrl_3_ad = dprecrl_3_ad + dprecrl1k_ad
           dprecrl1k_ad = zero
        else
           dprecrl1k_ad = zero
        endif

        if(precsl_3_save1(i,k) .gt. zero) then
           dprecsl_3_ad = dprecsl_3_ad + dprecsl_0_ad
           dprecsl_0_ad = zero
        else
           dprecsl_0_ad = zero
        endif

        if(precrl_3_save1(i,k) .gt. zero) then
           dprecrl_3_ad = dprecrl_3_ad + dprecrl_0_ad
           dprecrl_0_ad = zero
        else
           dprecrl_0_ad = zero
        endif


        if (comput) then
           CONDE  = const * DEL(I,K)
           CONDT  = CONDE * RDT
           RCONDE = ONE / CONDE

           dqin_ad = dqin_ad + dq2_ad
           derr_ad = derr_ad + dt*dq2_ad
           ders_ad = ders_ad + dt*dq2_ad
           dq2_ad = zero

           dtin_ad = dtin_ad + dt2_ad
           derr_ad = derr_ad - dtcp*hvap*dt2_ad
           ders_ad = ders_ad - dtcp*hsub*dt2_ad
           dpsm3_ad = dpsm3_ad - dtcp*hfus*dt2_ad
           dt2_ad = zero
!
!-------
! Melting
           if(tmt0_save1(i,k) .gt. zero) then
              dprecsl_2_ad = dprecsl_2_ad + dprecsl_3_ad
              dppr1_ad = dppr1_ad - dprecsl_3_ad
              dprecsl_3_ad = zero

              dprecrl_2_ad = dprecrl_2_ad + dprecrl_3_ad
              dppr1_ad = dppr1_ad + dprecrl_3_ad
              dprecrl_3_ad = zero


              if(ppr0_save2(i,k) .gt. amaxps_save1(i,k)) then
                damaxps_ad = damaxps_ad + rconde*dpsm3_ad
                dpsm3_ad = zero
                damaxps_ad = damaxps_ad + dppr1_ad
                dppr1_ad = zero
              else
                dpsm1_ad = dpsm1_ad + dpsm3_ad
                dpsm3_ad = zero
                dppr0_ad = dppr0_ad + dppr1_ad
                dppr1_ad = zero
              endif

              dpsm1_ad = dpsm1_ad + conde*dppr0_ad
              dpsm2_ad = dpsm2_ad + conde*dppr0_ad
              dppr0_ad = zero

              if(ww2_save1(i,k) .gt. zero) then
                dww2_ad = dww2_ad + cws*cr*dpsm2_ad*amaxps_save1(i,k)
                damaxps_ad = damaxps_ad + cws*cr*ww2_save1(i,k)*dpsm2_ad
                dpsm2_ad = zero
              else
                dpsm2_ad = zero
              endif

              dtmt0_ad = dtmt0_ad + TWO*csm1*dpsm1_ad*tmt0_save1(i,k)*amaxps_save1(i,k)
              damaxps_ad = damaxps_ad +  csm1*tmt0_save1(i,k)*tmt0_save1(i,k)*dpsm1_ad
              dpsm1_ad = zero

              if(precsl_2_save1(i,k) .gt. zero) then
                 dprecsl_2_ad = dprecsl_2_ad + damaxps_ad
                 damaxps_ad = zero
              else
                 damaxps_ad = zero
              endif
           else
              dprecsl_2_ad = dprecsl_2_ad + dprecsl_3_ad
              dprecsl_3_ad = zero

              dprecrl_2_ad = dprecrl_2_ad  + dprecrl_3_ad
              dprecrl_3_ad = zero

              dpsm3_ad = zero
           endif

!-------
! Evaporation
           dprecsl_1_ad = dprecsl_1_ad + dprecsl_2_ad
           dpps2_ad = dpps2_ad - dprecsl_2_ad
           dprecsl_2_ad = zero

           dprecrl_1_ad = dprecrl_1_ad + dprecrl_2_ad
           dppr2_ad = dppr2_ad - dprecrl_2_ad
           dprecrl_2_ad = zero


           dpps2_ad = dpps2_ad + rconde*ders_ad
           ders_ad = zero

           dppr2_ad = dppr2_ad + rconde*derr_ad
           derr_ad = zero

           if(pps1_save1(i,k) .lt. precsk1_save1(i,k)) then
                dpps1_ad = dpps1_ad + dpps2_ad
                dpps2_ad = zero
           else
                 dprecsk1_ad = dprecsk1_ad + dpps2_ad
                 dpps2_ad = zero
           endif

           if(ppr1_save1(i,k) .lt. precrk1_save1(i,k)) then
               dppr1_ad = dppr1_ad + dppr2_ad
               dppr2_ad = zero
           else
               dprecrk1_ad = dprecrk1_ad + dppr2_ad
               dppr2_ad = zero
           endif


           if(ppr0_save1(i,k)+pps0_save1(i,k) .gt. abs(erk_save1(i,k))) then
             dprecsk1_ad = dprecsk1_ad + rprs_save1(i,k)*dpps1_ad
             drprs_ad = drprs_ad + precsk1_save1(i,k)*dpps1_ad
             dpps1_ad = zero

             dprecrk1_ad = dprecrk1_ad + rprs_save1(i,k)*dppr1_ad
             drprs_ad = drprs_ad + precrk1_save1(i,k)*dppr1_ad
             dppr1_ad  = zero

             derk_ad = derk_ad + &
               drprs_ad/(precrk1_save1(i,k)+precsk1_save1(i,k))
             dprecrk1_ad = dprecrk1_ad &
        - erk_save1(i,k)*drprs_ad/((precrk1_save1(i,k)+precsk1_save1(i,k))**2)
             dprecsk1_ad = dprecsk1_ad  &
          - erk_save1(i,k)*drprs_ad/((precrk1_save1(i,k)+precsk1_save1(i,k))**2)
              drprs_ad = zero
            else
              dppr0_ad = dppr0_ad + dppr1_ad
              dppr1_ad = zero
              dpps0_ad = dpps0_ad + dpps1_ad
              dpps1_ad = zero
            endif

           if(rq_save1(i,k) .ge. onem10) then
             damaxrq_ad = damaxrq_ad + qk_save1(i,k)*rdt*derk_ad/rq_save1(i,k)
             dqk_ad = dqk_ad + amaxrq_save1(i,k)*rdt*derk_ad/rq_save1(i,k)
             drq_ad = drq_ad - amaxrq_save1(i,k)*qk_save1(i,k)*rdt*derk_ad/(rq_save1(i,k)**2)
             derk_ad = zero
           endif

           dprecrk1_ad = dprecrk1_ad +  derk_ad
           dprecsk1_ad = dprecsk1_ad +  derk_ad
           derk_ad = zero

           if(tmt0_save1(i,k) .ge. zero) then
              dpps0_ad = zero
           else
              dtmt0k_ad = dtmt0k_ad  &
                 + crs2*dpps0_ad*amaxrq_save1(i,k)*precsk1_save1(i,k)*rhci(i,k)
              damaxrq_ad = damaxrq_ad &
                 + (crs1+crs2*tmt0k_save1(i,k))*dpps0_ad*precsk1_save1(i,k)*rhci(i,k)
              dprecsk1_ad = dprecsk1_ad &
                 + (crs1+crs2*tmt0k_save1(i,k))*amaxrq_save1(i,k)*dpps0_ad*rhci(i,k)
              dpps0_ad = zero
           endif


            damaxrq_ad = damaxrq_ad + rke2*sqrt(precrk1_save1(i,k))*dppr0_ad
            dprecrk1_ad = dprecrk1_ad + &
             rke2*amaxrq_save1(i,k)*half/sqrt(precrk1_save1(i,k))*dppr0_ad
            dppr0_ad = zero

            if(term_save1(i,k) .gt. zero) then
               dterm_ad = dterm_ad + conde*damaxrq_ad
               damaxrq_ad = zero
            else
               damaxrq_ad = zero
            endif

            drq_ad = drq_ad - dterm_ad
            dterm_ad = zero


            if(precsl_1_save1(i,k) .gt. zero) then
              dprecsl_1_ad = dprecsl_1_ad + dprecsk1_ad
              dprecsk1_ad = zero
            else
              dprecsk1_ad =zero
            endif

            if(precrl_1_save1(i,k) .gt. zero) then
              dprecrl_1_ad = dprecrl_1_ad + dprecrk1_ad
              dprecrk1_ad = zero
            else
              dprecrk1_ad = zero
            endif

            if(tmt0_save1(i,k) .gt. -30.0_r_kind) then
                dtmt0_ad = dtmt0_ad + dtmt0k_ad
                dtmt0k_ad = zero
            else
                dtmt0k_ad = zero
            endif


!-------
          IF(CCR_save1(i,k) .gt. ZERO) then
             if (IWL_save1(i,k) .EQ. 1) THEN                 !  Ice Phase
                dprecrl_0_ad = dprecrl_0_ad + dprecrl_1_ad
                dprecrl_1_ad = zero

                dprecsl_0_ad = dprecsl_0_ad + dprecsl_1_ad
                dww0_ad = dww0_ad + condt*dprecsl_1_ad
                dww2_ad = dww2_ad - condt*dprecsl_1_ad
                dprecsl_1_ad = zero

                dww1_ad = dww1_ad + dww2_ad
                dpsaci_ad = dpsaci_ad - dww2_ad
                dww2_ad  = zero

                if (term3s_save1(i,k) .lt. cwmks_save1(i,k)) then
                 dterm3s_ad = dterm3s_ad + dpsaci_ad
                 dpsaci_ad = zero
                else
                 dcwmks_ad = dcwmks_ad + dpsaci_ad
                 dpsaci_ad = zero
                endif


                dexpf_ad = dexpf_ad + aa2 * dterm3s_ad * precsl_0_save1(i,k) * cwmks_save1(i,k)
                dprecsl_0_ad = dprecsl_0_ad + aa2 * expf_save1(i,k) * dterm3s_ad *cwmks_save1(i,k)
                dcwmks_ad = dcwmks_ad + aa2 * expf_save1(i,k) * precsl_0_save1(i,k)*dterm3s_ad
                dterm3s_ad = zero

                if(ww1_save1(i,k) .gt. zero) then
                  dww1_ad = dww1_ad + dcwmks_ad
                  dcwmks_ad = zero
                else
                  dcwmks_ad = zero
                endif

                dpsaut_ad = dpsaut_ad - dww1_ad
                dww0_ad = dww0_ad + dww1_ad
                dww1_ad = zero

                if (term2s_save1(i,k) .lt. cwmk_save1(i,k)) then
                   dterm2s_ad = dterm2s_ad + dpsaut_ad
                   dpsaut_ad = zero
                else
                   dcwmk_ad = dcwmk_ad + dpsaut_ad
                   dpsaut_ad = zero
                endif

                dexpf_ad = dexpf_ad + 0.0004_r_kind*dterm2s_ad*amaxcms_save1(i,k)
                damaxcms_ad = damaxcms_ad + 0.0004_r_kind*expf_save1(i,k)*dterm2s_ad
                dterm2s_ad = zero

                dtmt0_ad = dtmt0_ad   &
                   +DT * EXP(0.025_r_kind*TMT0_save1(i,k)) * 0.025_r_kind*dexpf_ad
                dexpf_ad = zero

                if(term1_save1(i,k) .gt. zero) then
                  dterm1_ad = dterm1_ad + dAMAXCMS_ad
                  dAMAXCMS_ad = zero
                else
                  dAMAXCMS_ad = ZERO
                endif

                dcwmk_ad = dcwmk_ad + dterm1_ad
                dterm1_ad = zero

              else
! For using Sundqvist precip

                dprecsl_0_ad = dprecsl_0_ad + dprecsl_1_ad
                dprecsl_1_ad = zero

                dprecrl_0_ad = dprecrl_0_ad + dprecrl_1_ad
                dww0_ad = dww0_ad + condt*dprecrl_1_ad
                dww2_ad = dww2_ad - condt*dprecrl_1_ad
                dprecrl_1_ad = zero

                dww0_ad = dww0_ad + dww2_ad
                dpraut0_ad = dpraut0_ad - dww2_ad
                dww2_ad = zero

                if(praut_save1(i,k) .lt. cwmk_save1(i,k)) then
                   dpraut_ad = dpraut_ad + dpraut0_ad
                   dpraut0_ad = zero
                else
                   dcwmk_ad = dcwmk_ad + dpraut0_ad
                   dpraut0_ad = zero
                endif


                dtem3_ad = dtem3_ad + c00*dpraut_ad*amaxcmr_save1(i,k)*(one-exp(-tem5_save1(i,k)))
                damaxcmr_ad = damaxcmr_ad  &
                     + c00*tem3_save1(i,k)*dpraut_ad*(one-exp(-tem5_save1(i,k)))
                dtem5_ad=  dtem5_ad &
                     + c00*tem3_save1(i,k)*amaxcmr_save1(i,k)*exp(-tem5_save1(i,k))*dpraut_ad
                dpraut_ad = zero


                if(term6_save1(i,k) .lt. 50.0_r_kind) then
                   dterm6_ad = dterm6_ad + dtem5_ad
                   dtem5_ad = zero
                else
                   dtem5_ad = zero
                endif

                dtem4_ad = dtem4_ad + two*tem4_save1(i,k)*dterm6_ad
                dterm6_ad = zero

                damaxcmr_ad = damaxcmr_ad + cmr*tem3_save1(i,k)*dtem4_ad/term5_save1(i,k)
                dtem3_ad = dtem3_ad  &
                   + amaxcmr_save1(i,k)*cmr*term5_save1(i,k)*dtem4_ad/term5_save1(i,k)**2
                dterm5_ad = dterm5_ad  &
                   - amaxcmr_save1(i,k)*cmr*tem3_save1(i,k)*dtem4_ad/term5_save1(i,k)**2
                dtem4_ad = zero

                if(ccr_save1(i,k) .gt. 0.01_r_kind) then
                   dccr_ad = dccr_ad + dterm5_ad
                   dterm5_ad = zero
                else
                   dterm5_ad = zero
                endif

                dtem1_ad = dtem1_ad + &
                  (300.0_r_kind*HALF/sqrt(tem1_save1(i,k)*rdt+tiny_r_kind)*rdt*dtem3_ad)  &
                  *(1+HALF*sqrt(tem2_save1(i,k)))
                dtem2_ad = dtem2_ad + &
                  HALF*HALF/sqrt(tem2_save1(i,k)+tiny_r_kind)*dtem3_ad*(ONE + 300.0_r_kind  &
                *sqrt(tem1_save1(i,k)*rdt))
                dtem3_ad = zero

                dterm4_ad = dterm4_ad + dtem2_ad
                dtem2_ad = zero

                if(term3r_save1(i,k) .lt. 20_r_kind) then
                  dterm3r_ad = dterm3r_ad + dterm4_ad
                  dterm4_ad =zero
                else
                  dterm4_ad = zero
                endif

                if(term2r_save1(i,k) .gt. zero) then
                   dterm2r_ad = dterm2r_ad + dterm3r_ad
                   dterm3r_ad = zero
                else
                   dterm3r_ad = zero
                endif

                dtin_ad = dtin_ad - dterm2r_ad
                dterm2r_ad = zero

                dprecsl_0_ad = dprecsl_0_ad + dtem1_ad
                dprecrl_0_ad = dprecrl_0_ad + dtem1_ad
                dtem1_ad = zero

                dcwmk_ad = dcwmk_ad + damaxcmr_ad
                damaxcmr_ad = zero

             endif ! IF(IWL .EQ. ONE)

             if (ww0_save1(i,k) .gt. ZERO) then
               dww0_ad = dww0_ad + dcwmk_ad
               dcwmk_ad = zero
             else
               dcwmk_ad = ZERO
             endif
             dcwmin_ad = dcwmin_ad + dww0_ad
             dww0_ad = zero

          ELSE ! IF(CCR .gt. ZERO) then

                dprecsl_0_ad = dprecsl_0_ad + dprecsl_1_ad
                dprecsl_1_ad = zero

                dprecrl_0_ad = dprecrl_0_ad + dprecrl_1_ad
                dprecrl_1_ad = zero

                dcwmin_ad = dcwmin_ad + dww2_ad
                dww2_ad = zero
          ENDIF !IF(CCR .gt. ZERO)

          IF(RQ_save1(i,k).LT.rhc(i,k)) THEN
             dCCR_ad = ZERO
          ELSEIF(RQ_save1(i,k).GE.ONE) THEN
             dCCR_ad = ZERO
          ELSE
             dRQKLL_ad = dRQKLL_ad +  &
              HALF/SQRT((ONE-RQKLL_save1(i,k)+tiny_r_kind)/(ONE-RHC(I,K) &
              +tiny_r_kind))*dCCR_ad/(ONE-RHC(I,K)+tiny_r_kind)
             dCCR_ad = ZERO

             if (RQ_save1(i,k) .lt. ONE) then
              dRQ_ad = dRQ_ad + drqkll_ad
              drqkll_ad = zero
             else
              drqkll_ad = zero
             endif

          ENDIF

          IF(qs_save1(i,k) .LE. 1.0E-10_r_kind) THEN
            dRQ_ad  = ZERO
          ELSE
            dQK_ad = dQK_ad + dRQ_ad/qs_save1(i,k)
            dQS_ad = dQS_ad - dRQ_ad*qk_save1(i,k)/(qs_save1(i,k)**2)
            dRQ_ad = zero
          ENDIF

          if(qs0_save1(i,k) .gt. epsq) then
            dqs0_ad = dqs0_ad + dqs_ad
            dqs_ad = zero
          else
            dqs_ad = zero
          endif


          des_ad = des_ad + eps*dqs0_ad*pres/(pres+epsm1*es_save1(i,k))**2
          dqs0_ad = zero
          call fpvsx_ad(tin_save1(i,k),es,dtin_ad,des_ad,.true.)

          dtin_ad = dtin_ad + dtmt0_ad
          dtmt0_ad = zero

          if (q_in(i,k) .gt. epsq) then
            dqin_ad = dqin_ad + dqk_ad
            dqk_ad = zero
          else
            dqk_ad = zero
          endif

        else ! if(comput)
           dcwmin_ad = dcwmin_ad + dww2_ad
           dww2_ad = zero
           dprecsl_0_ad = dprecsl_0_ad + dprecsl_3_ad
           dprecsl_3_ad = zero
           dprecrl_0_ad = dprecrl_0_ad + dprecrl_3_ad
           dprecrl_3_ad = zero
           dqin_ad = dqin_ad + dq2_ad
           dq2_ad = zero
           dtin_ad = dtin_ad + dt2_ad
           dt2_ad = zero

        endif ! if (comput)

        if(cwm_in(i,k) .gt. climit) then
           dcwmin_ad = dcwmin_ad + dwwn_ad
           dwwn_ad = zero
        else
           dwwn_ad = zero
        endif

        if(precsl_0_save1(i,k) .gt. zero) then
           dprecsl_0_ad = dprecsl_0_ad + dprecsk0_ad
           dprecsk0_ad = zero
        else
           dprecsk0_ad = zero
        endif

        if(precrl_0_save1(i,k) .gt. zero) then
           dprecrl_0_ad = dprecrl_0_ad + dprecrk0_ad
           dprecrk0_ad = zero
        else
           dprecrk0_ad = zero
        endif


        dt_ad(i,k) = dt_ad(i,k) + dtin_ad
        dtin_ad = zero

        dq_ad(i,k) = dq_ad(i,k) + dqin_ad
        dqin_ad = zero

        dcwm_ad(i,k) = dcwm_ad(i,k) + dcwmin_ad
        dcwmin_ad = zero

      ENDDO  ! DO K =
    else  !if (comput0)
      drn_out_ad(i) = zero
      do k = 1, km
        dcwm_ad(i,k) = dcwm_ad(i,k) + dcwm_out_ad(i,k)
        dcwm_out_ad(i,k) = zero

        dt_ad(i,k) = dt_ad(i,k) + dt_out_ad(i,k)
        dt_out_ad(i,k) = zero

        dq_ad(i,k) = dq_ad(i,k) + dq_out_ad(i,k)
        dq_out_ad(i,k) = zero
      enddo
    endif ! if (comput0)
  ENDDO

!-----------------------------------------------------------------------
101 END SUBROUTINE GFS_PRECIP_AD
