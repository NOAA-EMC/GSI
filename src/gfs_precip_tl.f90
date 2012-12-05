SUBROUTINE GFS_PRECIP_TL(IM, IX, KM, DT, DEL, SL, PS, T_in,Q_in,CWM_in,RHC,&
                     T_out, Q_out, CWM_out, RN_out,&
                     dT_in, dQ_in, dCWM_in, drn_in, &    !input
                     dt_out, dQ_out, dCWM_out,drn_out) !output

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gfs_precip_tl forward & tangent linear model for GFS precipitation physics 
!     prgmmr:   Min-Jeong Kim     org: np23                date: 2012-02-19
!
! abstract:  This subroutine contains the forward and tangent linear models for the
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


INTEGER(i_kind) :: IM, IX, KM, LAT, IWL1, IWL
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
INTEGER(i_kind):: i, k
REAL(r_kind), dimension(IM,KM) :: rainp, snowp

!-------------------------
! TL related variables
!-------------------------
real(r_kind) :: tmp, dtmp
real(r_kind) :: dprecrl_0, dprecsl_0, dtin,dqin,dcwmin,dprecrk0,dprecsk0, &
 dwwn,dqk,dtmt0,dqs0_des,des,dqs0_dt,dqs0,dqs,&
 dfi, drq, dccr, drqkll, dww0, dcwmk,dterm1, damaxcms, damaxcmr,dexpf, dterm2s, dpsaut, &
 dww1, dcwmks, dterm3s, dpsaci, dww2, dprecsl_1, dprecrl_1, dtem1, dterm2r, dterm3r, &
 dterm4, dtem2, dtem3, dtem4, dterm5,dterm6, dtem5, dpraut, dpraut0, &
 dtmt0k, dprecrk1, dprecsk1, dterm, damaxrq, dppr0, dpps0, derk,drprs, &
 dppr1,dppr2, dpps1, dpps2, derr,ders, dprecrl_2, dprecsl_2, damaxps, &
 dpsm1, dpsm2, dpsm3, dprecrl_3, dprecsl_3,&
 dt2, dq2, dprecrl1k, dprecsl1k
 
real(r_kind), dimension(ix,km) ::dq_out,dt_out,dCWM_out, &
                                 dq_in,dt_in,dCWM_in
real(r_kind), dimension(ix) :: drn_out,drn_in

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
    RHCI(i,k) = one/RHC(i,k)
!initialize output
    dT_out(i,k) =zero
    dQ_out(i,k) =zero
    dCWM_out(i,k) =zero
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
 dprecrl1k = zero
  precsl1k = zero
 dprecsl1k = zero
  rn_out(i) = zero !initialize output
 drn_out(i) = zero
  const = ps(i) * (1000.0_r_kind*dt/G)
      precrl_0 = zero 
      dprecrl_0 = zero 
      precsl_0 =  zero 
      dprecsl_0 = zero 

 if (comput0(i)) then
!***********************************************************************
!-----------------BEGINING OF PRECIPITATION CALCULATION-----------------
!***********************************************************************
   DO K=KM,1,-1
      wmin = 0.00001_r_kind*sl(i,k)*ps(i)*0.01_r_kind

      iwl = 0
      tin = t_in(i,k)
      dtin = dt_in(i,k)
      qin = q_in(i,k)
      dqin = dq_in(i,k)
      cwmin = cwm_in(i,k)
      dcwmin = dcwm_in(i,k)
      pres = ps(i) * sl(i,k)

      if (precrl_0 .gt. zero) then
        precrk0 = precrl_0
        dprecrk0 = dprecrl_0
      else
        precrk0 = zero
        dprecrk0 = zero
      endif

      if (precsl_0 .gt. zero) then
         precsk0 = precsl_0
        dprecsk0 = dprecsl_0
      else
        precsk0 = zero
        dprecsk0 = zero
      endif

      if (cwmin .gt. climit ) then
         wwn = cwmin
         dwwn = dcwmin
      else
         wwn = climit
         dwwn = zero
      endif

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
          dqk    = dqin
        else if(qin .le. epsq) then
          qk = epsq
          dqk = zero
        endif

        tmt0  = tin - ttp
        dtmt0  = dtin
        call fpvsx_tl(tin,es,dtin,des)
        qs0 = eps*es/(pres+epsm1*es)
        dqs0 = eps*des*pres/(pres+epsm1*es)**2

        if (qs0 .gt. epsq) then
          dqs =  dqs0
          qs = qs0
        else
          dqs =  zero 
          qs = epsq
        endif

!-------------------ICE-WATER ID NUMBER IW------------------------------
        IF(TMT0 .LT. -15.0_r_kind) THEN
          FI = QK - rhc(i,k) *Qs
          dFI =  dQK - rhc(i,k)*dqs
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

        IF(qs .LE. 1.0E-10_r_kind) THEN
           RQ  = ZERO
           dRQ  = ZERO
        ELSE
           RQ  = QK / QS
           dRQ = (dQK*QS - dQS*QK)/(QS**2)
        ENDIF

!----------------CLOUD COVER RATIO CCR----------------------------------

        IF(RQ.LT.rhc(i,k)) THEN
          dCCR = ZERO
          CCR = ZERO
        ELSEIF(RQ.GE.ONE) THEN
          dCCR = ZERO
          CCR = ONE
        ELSE
          if (RQ .lt. ONE) then
            drqkll = dRQ
            rqkll = RQ
          else
            drqkll = zero
            rqkll = one
          endif
           dCCR = -HALF/SQRT((ONE-RQKLL)/(ONE-RHC(I,K)+tiny_r_kind))*  &
                    (-drqkll/(ONE-RHC(I,K)+tiny_r_kind))
           CCR = ONE-SQRT((ONE-RQKLL)/(ONE-RHC(I,K)+tiny_r_kind))
        ENDIF


        IF(CCR .gt. ZERO) then
          ww0 = cwmin
          dww0 = dcwmin
          if (ww0 .gt. ZERO) then
            cwmk = ww0
            dcwmk = dww0
          else
            cwmk = ZERO
            dcwmk = ZERO
          endif
          if(IWL .EQ. 1) THEN                 !  Ice Phase
             term1 = cwmk - wmin
             dterm1 = dcwmk
             if(term1 .gt. zero) then
                AMAXCMS = term1
                dAMAXCMS = dterm1
             else
                AMAXCMS = zero
                dAMAXCMS = ZERO
             endif

             expf   = DT * EXP(0.025_r_kind*TMT0)
             dexpf = DT * EXP(0.025_r_kind*TMT0) * 0.025_r_kind*dtmt0

             term2s = 0.0004_r_kind*expf*amaxcms
             dterm2s = 0.0004_r_kind*dexpf*amaxcms + 0.0004_r_kind*expf*damaxcms

             if (term2s .lt. cwmk) then
                psaut = term2s
                dpsaut = dterm2s
             else
                psaut = cwmk
                dpsaut = dcwmk
             endif

             ww1 = ww0 - psaut
             dww1 = dww0-dpsaut

             if(ww1 .gt. zero) then
                cwmks = ww1
                dcwmks = dww1
             else
                cwmks = zero
               dcwmks = zero
             endif
             term3s = aa2 * expf * precsl_0*cwmks
             dterm3s = aa2 * dexpf * precsl_0 * cwmks  &
                  + aa2 * expf * dprecsl_0*cwmks + aa2 * expf * precsl_0*dcwmks

             if (term3s .lt. cwmks) then
               psaci = term3s
               dpsaci = dterm3s
             else
               psaci = cwmks
               dpsaci = dcwmks
             endif

             ww2 = ww1 - psaci
             dww2 = dww1 - dpsaci
             precsl_1 = precsl_0+(ww0-ww2)*condt
             dprecsl_1 = dprecsl_0 + (dww0 - dww2)*condt
             precrl_1 = precrl_0
             dprecrl_1 = dprecrl_0
          else
!          For using Sundqvist precip formulation of rain
             amaxcmr = cwmk
             damaxcmr = dcwmk
             tem1 = precsl_0 + precrl_0
             dtem1 = dprecsl_0 + dprecrl_0

             term2r = 268.0_r_kind - tin
             dterm2r = -dtin

             if(term2r .gt. zero) then
                 term3r = term2r
                dterm3r = dterm2r
             else
                 term3r =  tiny_r_kind
                dterm3r =  zero
             endif

             if(term3r .lt. 20_r_kind) then
                 term4 = term3r
                dterm4 = dterm3r
             else
                 term4 = 20.0_r_kind
                dterm4 = zero
             endif

             tem2 = term4
             dtem2 = dterm4

             tem1 = max(tem1,1.0e-20_r_kind)
             tem2 = max(tem2,1.0e-20_r_kind)

       
             tem3 = (ONE + 300.0_r_kind*sqrt(tem1*rdt)) * (1+HALF*sqrt(tem2))
             dtem3 = (300.0_r_kind*HALF/sqrt(tem1*rdt+tiny_r_kind)*rdt*dtem1) &
              *(1+HALF*sqrt(tem2)) &
             + HALF*HALF/sqrt(tem2+tiny_r_kind)*dtem2*(ONE + 300.0_r_kind*sqrt(tem1*rdt))
      

             if(ccr .gt. 0.01_r_kind) then
               term5 = ccr
               dterm5 = dccr
             else
               term5 = 0.01_r_kind
               dterm5 = zero
             endif

             tem4 = amaxcmr*cmr*tem3/term5
             dtem4 = (damaxcmr*cmr*tem3*term5 + amaxcmr*cmr*dtem3*term5  &
                     - dterm5*amaxcmr*cmr*tem3)/(term5**2)

             term6 = tem4*tem4
             dterm6 = TWO*tem4*dtem4

             if(term6 .lt. 50.0_r_kind) then
               tem5 = term6
               dtem5 = dterm6
             else
               tem5 = 50.0_r_kind
               dtem5 = zero
             endif

             praut = c00*tem3*amaxcmr*(one-exp(-tem5))
             dpraut = c00*dtem3*amaxcmr*(one-exp(-tem5)) &
                + c00*tem3*damaxcmr*(one-exp(-tem5))+c00*tem3*amaxcmr*exp(-tem5)*dtem5

             if(praut .lt. cwmk) then
               praut0 = praut
               dpraut0 = dpraut
             else
               praut0 = cwmk
               dpraut0 = dcwmk
             endif
              ww2 = ww0 - praut0
             dww2 = dww0 - dpraut0
              precrl_1 = precrl_0 + (ww0-ww2)*condt
             dprecrl_1 = dprecrl_0 + (dww0-dww2)*condt
              precsl_1 = precsl_0
             dprecsl_1 = dprecsl_0
          endif !if(IWL .EQ. ONE)
        ELSE ! IF(CCR .gt. ZERO) then
          ww2 = cwmin
          dww2 = dcwmin
          precrl_1 = precrl_0
          dprecrl_1 = dprecrl_0
          precsl_1 = precsl_0
          dprecsl_1 = dprecsl_0
        ENDIF   ! IF(CCR .gt. ZERO) then

! Evaporation
        if (tmt0 .gt. (-30._r_kind)) then
             tmt0k = tmt0
             dtmt0k = dtmt0
        else
             tmt0k = -30._r_kind
             dtmt0k = zero
        endif
        if (precrl_1 .gt. zero) then
             precrk1 = precrl_1
             dprecrk1 = dprecrl_1
        else
             precrk1 = tiny_r_kind
             dprecrk1 = zero
        endif
        precrk1 = max(precrk1,1.0e-20_r_kind) 
        if (precsl_1 .gt. zero) then
             precsk1 = precsl_1
             dprecsk1 = dprecsl_1
        else
!                precsk1 = zero
             precsk1 =  tiny_r_kind
             dprecsk1 = zero
        endif
        precsk1 = max(precsk1,1.0e-20_r_kind) 

        term = rhc(i,k)-rq
        dterm = -drq

        if (term .gt. zero) then
            amaxrq = term*conde
            damaxrq = dterm*conde
        else
            amaxrq = zero
            damaxrq = zero
        endif

        ppr0 = rke2*amaxrq*sqrt(precrk1)
        dppr0 = rke2*damaxrq*sqrt(precrk1)  &
                   + rke2*amaxrq*half/sqrt(precrk1)*dprecrk1

        if (tmt0 .ge. zero) then
             pps0 = zero
             dpps0 = zero
        else
             pps0 = (crs1+crs2*tmt0k)*amaxrq*precsk1*rhci(i,k)
             dpps0 = crs2*dtmt0k*amaxrq*precsk1*rhci(i,k) + &
                  (crs1+crs2*tmt0k)*damaxrq*precsk1*rhci(i,k) &
                  +(crs1+crs2*tmt0k)*amaxrq*dprecsk1*rhci(i,k)
        endif

        erk = precrk1+precsk1
        derk = dprecrk1+dprecsk1


        if (rq .ge. onem10) then
             erk = amaxrq*qk*rdt/rq
             derk = damaxrq*qk*rdt/rq &
                    + amaxrq*dqk*rdt/rq  -amaxrq*qk*rdt*drq/(rq**2)
        endif

         if (ppr0+pps0 .gt. abs(erk)) then
              rprs = erk/(precrk1+precsk1)
              drprs = (derk*(precrk1+precsk1) &
               - (dprecrk1+dprecsk1)*erk)/((precrk1+precsk1)**2)
              ppr1 = precrk1*rprs
              dppr1 = dprecrk1*rprs + precrk1*drprs
              pps1 = precsk1*rprs
              dpps1 = dprecsk1*rprs+ precsk1*drprs
        else
              ppr1 = ppr0
              dppr1 = dppr0
              pps1 = pps0
              dpps1 = dpps0
        endif

        ppr1 = max(ppr1,1.0e-20_r_kind)
        pps1 = max(pps1,1.0e-20_r_kind)

        if (ppr1 .lt. precrk1) then
             ppr2 = ppr1
             dppr2 = dppr1
        else
             ppr2 = precrk1
             dppr2 = dprecrk1
        endif

        if (pps1 .lt. precsk1) then
             pps2 = pps1
             dpps2 = dpps1
        else
             pps2 = precsk1
             dpps2 = dprecsk1
        endif

        err = ppr2*rconde
        derr = dppr2*rconde
        ers = pps2*rconde
        ders = dpps2*rconde

        precrl_2 = precrl_1 - ppr2
        dprecrl_2 = dprecrl_1 -dppr2
        precsl_2 = precsl_1-pps2
        dprecsl_2 = dprecsl_1 - dpps2

!  Melting
        if (tmt0 .gt. zero) then
           if (precsl_2 .gt. zero) then
             amaxps = precsl_2
             damaxps = dprecsl_2
           else
             amaxps = zero
             damaxps = zero
           endif

           psm1 = csm1*tmt0*tmt0*amaxps
           dpsm1 = TWO*csm1*dtmt0*tmt0*amaxps +  csm1*tmt0*tmt0*damaxps
           if (ww2 .gt. zero) then
                psm2 = cws*cr*ww2*amaxps
                dpsm2 = cws*cr*dww2*amaxps +  cws*cr*ww2*damaxps
           else
                psm2 = zero
                dpsm2 = zero
           endif
           ppr0 = (psm1+psm2)*conde
           dppr0 = (dpsm1+dpsm2)*conde

           if (ppr0 .gt. amaxps) then
                ppr1 = amaxps
                dppr1 = damaxps
                psm3 = amaxps*rconde
                dpsm3 = damaxps*rconde
           else
                ppr1 = ppr0
                dppr1 = dppr0
                psm3 = psm1
                dpsm3 = dpsm1
           endif
           precrl_3 = precrl_2 + ppr1
           dprecrl_3 = dprecrl_2 + dppr1
           precsl_3 = precsl_2 - ppr1
           dprecsl_3 = dprecsl_2 - dppr1
        else
             psm3 = zero
             dpsm3 = zero
             precrl_3 =  precrl_2
             dprecrl_3 = dprecrl_2
             precsl_3 =  precsl_2
             dprecsl_3 = dprecsl_2
        endif
          t2 = tin-dtcp*(hvap*err+hsub*ers+hfus*psm3)
          dt2 = dtin-dtcp*(hvap*derr+hsub*ders+hfus*dpsm3)
          q2 = qin+dt*(err+ers)
          dq2 = dqin+dt*(derr+ders)

      else   ! if (comput)
          t2 = tin
          dt2 = dtin
          q2 = qin
          dq2 = dqin
          precrl_3 = precrl_0
          dprecrl_3 = dprecrl_0
          precsl_3 = precsl_0
          dprecsl_3 = dprecsl_0
          ww2 = cwmin
          dww2 = dcwmin
      endif !if(comput)


      iwl1 = iwl

      if (precrl_3 .gt. zero) then
         precrl1k = precrl_3
         dprecrl1k = dprecrl_3
      else
        precrl1k = zero
         dprecrl1k = zero
      endif

      if (precsl_3 .gt. zero) then
         precsl1k = precsl_3
         dprecsl1k = dprecsl_3
     else
         precsl1k = zero
         dprecsl1k = zero
      endif

      if (precrl_3 .gt. zero) then
          precrl_0 = precrl_3
         dprecrl_0 = dprecrl_3
      else
          precrl_0 = zero
         dprecrl_0 = zero
      endif
      if (precsl_3 .gt. zero) then
          precsl_0 = precsl_3
         dprecsl_0 = dprecsl_3
      else
         precsl_0 = zero
        dprecsl_0 = zero
      endif

      if (ww2 .lt. zero ) then
         q_out(i,k) = q2+ww2
         dq_out(i,k) = dq2+dww2
         t_out(i,k) = t2-hvap*rcp*ww2
         dt_out(i,k) = dt2-hvap*rcp*dww2
         cwm_out(i,k) = zero
         dcwm_out(i,k) = zero
      else
         q_out(i,k) = q2
         dq_out(i,k) = dq2
         t_out(i,k) = t2
         dt_out(i,k) = dt2
         cwm_out(i,k) = ww2
         dcwm_out(i,k) = dww2
      endif
   ENDDO 
   rn_out(i) = (precrl1k + precsl1k)*rrow
   drn_out(i) = (dprecrl1k + dprecsl1k)*rrow
 else  ! if (comput0)
   do k = km, 1, -1
     q_out(i,k)   = q_in(i,k)
     dq_out(i,k)   = dq_in(i,k)
     t_out(i,k)   = t_in(i,k)
     dt_out(i,k)   = dt_in(i,k)
     cwm_out(i,k) = cwm_in(i,k)
     dcwm_out(i,k) = dcwm_in(i,k)
   end do
   rn_out(i) = zero
   drn_out(i) = zero
 endif ! if (comput0)
ENDDO

END SUBROUTINE GFS_PRECIP_TL
