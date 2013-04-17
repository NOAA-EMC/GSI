SUBROUTINE GFS_GSCOND_AD(IM,IX,KM,DT, SL,PS, T_in, Q_in, CWM_in, RHC, &
                     advT, advQ, advP, T_out, Q_out, CWM_out, &
                     dadvT_ad,dadvQ_ad, dadvP_ad, &  !output
                     dT_in_ad, dQ_in_ad, dCWM_in_ad, &  !output
                     dT_out_ad, dQ_out_ad, dCWM_out_ad,adjoint)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gfs_gscond_ad    forward & adjoint model for GFS gridscale condensation
!     prgmmr:   Min-Jeong Kim     org: np23                date: 2012-02-16
!
! abstract:  This subroutine contains the forward and adjoint models for the
!            GFS gridscale condensation algorithm
!
! program history log:
!   2012-02-16  kim - initial routine
!
!$$$

use kinds, only: r_single, r_kind, i_kind

use constants, only: HVAP, HFUS, CP,  RD, EPS, EPSM1, &
                     EPSQ, CPR, RCP, ZERO, HALF, ONE, TWO, &
                     EL2ORC, CLIMIT, CCLIMIT, HSUB, TTP, &
                     tiny_r_kind, huge_r_kind,elocp

IMPLICIT NONE

real(r_kind), parameter::  ELIV = HVAP + HFUS 
real(r_kind), parameter::  ELWV = HVAP 
real(r_kind), parameter::  ELIW = ELIV - ELWV

INTEGER(i_kind) :: IM, IX, KM
INTEGER(i_kind) :: I, K, J
logical, intent(in) :: adjoint

REAL(r_kind) :: TIK, TMT0, X, Y, QIK, CWMIK, esk, QW, &
                U00IK, FI,PRES, IWIK, AT, AQ, AP, FIW, ELV, QC, &
                RQIK, RQIKK, CCRIK, CCRIK1, E0,E1,E2,E3,E4, &
                COND, CONDI, COND1, COND2, COND3, CONE0, CONE0_save1, DT, &
                tx1_1, tx1_2, tx1_3, es_1, tsq_1, qs_1, delq_1, &
                tx2_1, tx2_2, tx2_3, es_2, tsq_2, qs_2, delq_2, &
                tx3_1, tx3_2, tx3_3, es_3, tsq_3, qs_3, delq_3, &
                tx2_4, AI, AA, AB, AC, AF, AD, AE, AG, RDT, US00, &
                term1, term2, term3,E0_save1, E0_save2

REAL(r_kind) :: dCONE0_ad,dCOND_ad, dCOND1_ad,dCOND2_ad, dCOND3_ad,dE0_ad,dE1_ad, dE2_ad,dE3_ad,dE4_ad, &
                dQIK_ad, dQC_ad, &
                dterm1_ad, dterm2_ad, dterm3_ad, &
                dterm1x_ad, dterm2x_ad, dterm3x_ad, &
                dAA_ad, dAB_ad, dAC_ad, dAD_ad, dAE_ad, dAF_ad, dAG_ad, dAI_ad, &
                dTIK_ad, dQI_ad, dCWMIK_ad, dCCRIK_ad, dCCRIK1_ad, dfac_ad, &
                dtx2_4_ad, dtx2_3_ad, dtx3_3_ad, ddelq_3_ad, dqs3_ad, dtsq_3_ad, &
                dtx1_3_ad, dtx3_2_ad, ddelq_2_ad, dtx1_2_ad, dtx2_2_ad, dqs2_ad, dtsq_2_ad, &
                dtx3_1_ad, ddelq_1_ad, dtx1_1_ad, dqs1_ad, dtsq_1_ad, &
                dRQIK_ad,dRQIKK_ad, dQW_ad, dFIW_ad, dIWIK_ad, dELV_ad,dTMT0_ad, &
                ddelq_ad, dx_ad, dcondi_ad, dCONDIx_ad


REAL(r_kind) :: des1_ad, des2_ad, dqs2_des2, des3_ad, dqs3_des3, dqs1_des1, desk_ad

real(r_kind) :: rfac, fac

REAL(r_kind), dimension(IX,KM) :: T_in, Q_in, CWM_in, PRSL, SL, &
                  IW,xIW,  RHC, advT, advQ, advP, T_out, Q_out, CWM_out, dIW

REAL(r_kind), dimension(IX,KM) :: dT_out_ad, dT_in_ad, &
                                  dQ_out_ad, dQ_in_ad,  &
                                  dCWM_out_ad, dCWM_in_ad, dIW_ad, &
                                  dT_out_ad0, dQ_out_ad0, dCWM_out_ad0
!REAL(r_kind), dimension(IX,KM) :: dT_tend, dQ_tend, dCWM_tend
REAL(r_kind), dimension(IX,KM) :: dadvT_ad, dadvQ_ad, dadvP_ad
REAL(r_kind) :: dAT_ad, dAQ_ad, dAP_ad
REAL(r_kind) :: qw_tmp1,ccrik_tmp1

REAL(r_kind), dimension(IX) :: PS, QI, QINT
REAL(r_kind) :: dum, CWM_out_save1
!-------------------------------
RDT=ONE/DT
DO I = 1, IM
 IW(I,KM)=ZERO

 DO K = KM,1,-1
  TIK=T_in(I,K)       ![K]

  TMT0=T_in(I,K)-TTP  

  QIK=Q_in(I,K)

  CWMIK = CWM_IN(I,K)
  PRSL(I,K) = PS(I) * SL(I,K) ![cb]
  call fpvsx_ad(Tik,esk,dum,dum,.false.)         ![cb]

  QW = EPS *esk/(PRSL(I,K)+EPSM1*esk)
  QW_tmp1 = QW

  IF (QW .LE. EPSQ) THEN
    QW = EPSQ
  ENDIF

  X = (-15.-TMT0)

  IW(I,K) = HALF*(ONE+tanh(X))

!----- AT, AQ, DP/DT --------------------------------
  IWIK = IW(I,K)

  U00IK = RHC(I,K)
  PRES=PRSL(I,K)*1000.0_r_kind
  AT=advT(I,K)
  AQ=advQ(I,K)
  AP=advP(I,K)

!----- SATURATION SPECIFIC HUMIDITY ----------------
  FIW=IWIK

  ELV=(ONE-FIW)*ELWV+FIW*ELIV

  QC = QW

!----------  RELATIVE  HUMIDITY --------------------
  RQIK=QIK/(QC+tiny_r_kind)

!----------  CLOUD COVER RATIO, CCRIK ---------------
  IF (RQIK .ge. ONE) then
    RQIKK = ONE
  ELSE IF (RQIK .lt. ZERO ) then
    RQIKK = ZERO
  ELSE
    RQIKK = RQIK
  ENDIF
 

  IF(RQIKK .ge. ONE) then
   CCRIK = ONE
  ELSE IF(RQIKK .LT. U00IK) then
   CCRIK = ZERO
  ELSE
    CCRIK=ONE-SQRT((ONE-RQIKK+tiny_r_kind)/(ONE-U00IK+tiny_r_kind))
  ENDIF

  CCRIK_tmp1 = CCRIK
 if(CCRIK .LE. ZERO) then
   CCRIK = tiny_r_kind
 endif

!--- EVAPORATION OF CLOUD WATER, E0 --------------

  !First iteration
    tx1_1 = tik
    tx3_1 = qik

    call fpvsx_ad(TX1_1,es_1,dum,dum,.false.)
    qs_1=U00ik*eps*es_1/(prsl(i,k)+epsm1*es_1)

    tsq_1=tx1_1*tx1_1

    delq_1=HALF*(qs_1-tx3_1)*tsq_1/(tsq_1+el2orc*qs_1)
    fac = ONE + el2orc*qs_1/tsq_1
    rfac = ONE/fac

    tx2_2=delq_1
    tx1_2 = tx1_1 - delq_1*elocp
    tx3_2 = tx3_1 + delq_1

  !Second iteration
    call fpvsx_ad(TX1_2,es_2,dum,dum,.false.)
    qs_2=U00ik*eps*es_2/(prsl(i,k)+epsm1*es_2)

    tsq_2=tx1_2*tx1_2

    delq_2=HALF*(qs_2-tx3_2)*tsq_2/(tsq_2+el2orc*qs_2)
    fac = ONE + el2orc*qs_2/tsq_2
    rfac = one/fac

    tx2_3=delq_2
    tx1_3 = tx1_2 - delq_2*elocp
    tx3_3 = tx3_2 + delq_2

  !Third iteration
    call fpvsx_ad(TX1_3,es_3,dum,dum,.false.)
    qs_3=U00ik*eps*es_3/(prsl(i,k)+epsm1*es_3)

    tsq_3=tx1_3*tx1_3

    delq_3=HALF*(qs_3-tx3_3)*tsq_3/(tsq_3+el2orc*qs_3)
    fac = ONE + el2orc*qs_3/tsq_3
    rfac = one/fac

    tx2_4=tx2_3+delq_3

  E0 = tx2_4*RDT
  E0_save1 = E0

  IF (E0 .lt. ZERO .or. CWMIK .LT. CLIMIT .or. CCRIK .GT. CCLIMIT) THEN
    E0 = ZERO
  ENDIF

  E0_save2 = E0
  IF (E0 .gt. CWMIK*RDT) THEN
    E0 = CWMIK*RDT
  ENDIF

!--- CONDENSATION OF CLOUD WATER, COND --------------
 if (ccrik > cclimit .and. QW > epsq) then
  US00 = ONE - U00IK
  CCRIK1 = ONE - CCRIK

  AA = EPS*ELV*PRES*QIK
  AB = CCRIK*CCRIK1*QC*US00
  AC = AB + HALF*CWMIK
  AD = AB *CCRIK1
  AE = CPR*TIK*TIK
  AF = AE*PRES
  AG = AA*ELV
  AI = CP*AA


  term1 = (AC-AD)*AF*AQ/(AC*(AF+AG)+tiny_r_kind)
  term2 = (AC-AD)*AI*AT/(AC*(AF+AG)+tiny_r_kind)
  term3 = (AC-AD)*AE*QIK*AP/(AC*(AF+AG)+tiny_r_kind)

 if(abs((AC-AD)*(AF*AQ-AI*AT+AE*QIK*AP)) .gt. zero .and. abs(AC*(AF+AG)+tiny_r_kind) .gt. zero) then
  COND1 = (AC-AD)*(AF*AQ-AI*AT+AE*QIK*AP)/(AC*(AF+AG)+tiny_r_kind)
 else
 COND1= zero
 endif

  CONDI = (QIK - U00IK * QC * ONE)*RDT

  IF(COND1 < CONDI) then
    COND2 = COND1
  ELSE
    COND2 = CONDI
  ENDIF

  IF(COND2 .GT. ZERO) THEN
    COND3= COND2
  ELSE
    COND3 = ZERO
  ENDIF

  COND = COND3
else
  COND = ZERO
endif

!C--- UPDATE OF T, Q, CWM   --------------
  CONE0 = (COND-E0)*DT
  CWM_out(I,K) = CWM_in(I,K) + CONE0
  CWM_out_save1 = CWM_out(I,K)

  IF(CWM_out(I,K) .LE. CLIMIT) THEN
    CWM_out(i,k) = ZERO
    CONE0 = -CWM_in(I,K)
  ENDIF
   CONE0_save1 = CONE0
  T_out(I,K) = T_in(I,K) + ELV*RCP*CONE0
  Q_out(I,K) = Q_in(I,K) - CONE0

  dadvT_ad(i,k) = zero
  dadvQ_ad(i,k) = zero
  dadvP_ad(i,k) = zero
  dT_in_ad(i,k) = zero
  dQ_in_ad(i,k) = zero
  dCWM_in_ad(i,k) = zero

 if (.not.adjoint) goto 101 

!------------------------------
! Adjoint starts here....

! Initialize adjoint variables
  dCONE0_ad = ZERO
  dCOND_ad = ZERO
  dCOND1_ad = ZERO
  dCOND2_ad = ZERO
  dCOND3_ad = ZERO
  dCONDI_ad = ZERO
  dCONDIx_ad = ZERO
  dE0_ad = ZERO
  dE1_ad = ZERO
  dE2_ad = ZERO
  dE3_ad = ZERO
  dE4_ad = ZERO
  dTIK_ad = ZERO
  dQIK_ad = ZERO
  dCWMIK_ad = ZERO
  dQC_ad = ZERO
  dQW_ad = ZERO
  dTMT0_ad = ZERO
  dterm1_ad = ZERO
  dterm2_ad = ZERO
  dterm3_ad = ZERO
  dterm1x_ad = ZERO
  dterm2x_ad = ZERO
  dterm3x_ad = ZERO
  dAA_ad = ZERO
  dAB_ad = ZERO
  dAC_ad = ZERO
  dAD_ad = ZERO
  dAE_ad = ZERO
  dAF_ad = ZERO
  dAG_ad = ZERO
  dAI_ad = ZERO
  dCCRIK_ad = ZERO
  dCCRIK1_ad = ZERO
  desk_ad = zero

  dtx3_1_ad = ZERO
  dtx3_2_ad = ZERO
  dtx3_3_ad = ZERO
  dtx1_1_ad = ZERO
  dtx1_2_ad = ZERO
  dtx1_3_ad = ZERO
  dtx2_4_ad = ZERO
  dtx2_3_ad = ZERO

  ddelq_1_ad = ZERO
  ddelq_2_ad = ZERO
  ddelq_3_ad = ZERO

  dqs1_ad = ZERO
  dqs2_ad = ZERO
  dqs3_ad = ZERO

  dtsq_1_ad = ZERO
  dtsq_2_ad = ZERO
  dtsq_3_ad = ZERO

  dfac_ad = ZERO
  dFIW_ad = ZERO
  dELV_ad = ZERO 
  dIWIK_ad = ZERO 
  dX_ad = ZERO 

  dRQIKK_ad = ZERO
  dRQIK_ad = ZERO

  dIW_ad(I,K) = ZERO
  
  dAT_ad = zero
  dAQ_ad = zero
  dAP_ad = zero
  des1_ad = zero
  des2_ad = zero
  des3_ad = zero


!C--- UPDATE OF T, Q, CWM   --------------
  dCONE0_ad = dCONE0_ad - dQ_out_ad(I,K)
  dQ_in_ad(I,K) = dQ_in_ad(I,K) + dQ_out_ad(I,K)
  dQ_out_ad(I,K) = zero

  dCONE0_ad = dCONE0_ad + ELV*RCP*dT_out_ad(I,K)
  dT_in_ad(I,K) = dT_in_ad(I,K) + dT_out_ad(I,K)
  dELV_ad = dELV_ad + RCP*CONE0_save1*dT_out_ad(I,K)
  dT_out_ad(I,K) = zero


  IF(CWM_out_save1 .LE. CLIMIT) THEN
   dCWM_out_ad(i,k) = ZERO
   dCWM_in_ad(I,K) = dCWM_in_ad(I,K) - dCONE0_ad
   dCONE0_ad = ZERO
  ENDIF

  dCONE0_ad = dCONE0_ad + dCWM_out_ad(I,K)
  dCWM_in_ad(I,K) = dCWM_in_ad(I,K) + dCWM_out_ad(I,K)
  dCWM_out_ad(I,K) = zero


  dCOND_ad = dCOND_ad + DT * dCONE0_ad
  dE0_ad = dE0_ad - DT * dCONE0_ad
  dCONE0_ad = zero

!--- CONDENSATION OF CLOUD WATER, COND --------------
 if (ccrik > cclimit .and. QW > epsq) then
   dCOND3_ad = dCOND3_ad + dCOND_ad
   dCOND_ad = ZERO

   IF(COND2 .GT. ZERO) THEN
     dCOND2_ad  = dCOND2_ad + dCOND3_ad
   ELSE
     dCOND3_ad = ZERO
   ENDIF


   IF(COND1 < CONDI) then
     dCOND1_ad = dCOND1_ad +  dCOND2_ad
     dCOND2_ad = zero
   ELSE
     dCONDI_ad = dCONDI_ad+ dCOND2_ad
     dCOND2_ad = zero
   ENDIF

   dQIK_ad = dQIK_ad + RDT*dCONDI_ad
   dQC_ad = dQC_ad -U00IK*ONE*RDT*dCONDI_ad
   dCONDI_ad = zero

   if(abs((AC-AD)*(AF*AQ-AI*AT+AE*QIK*AP)) .gt. zero .and. abs(AC*(AF+AG)+tiny_r_kind) .gt. zero) then
     dterm3x_ad = dterm3x_ad + dCOND1_ad
     dterm2x_ad = dterm2x_ad - dCOND1_ad
     dterm1x_ad = dterm1x_ad + dCOND1_ad
     dCOND1_ad = zero
   else
     dCOND1_ad = zero
   endif

   dterm3_ad = dterm3_ad + dterm3x_ad
   dAP_ad = dAP_ad + (AC-AD)*AE*QIK*dterm3x_ad/(AC*(AF+AG)+tiny(0.))
   dterm3x_ad = zero

   dterm3_ad = dterm3_ad*AP/(AC*AC*(AF+AG)*(AF+AG)+tiny(0.))
   dAC_ad = dAC_ad + (-(AF+AG)*AE*QIK*(AC-AD)+AE*QIK*AC*(AF+AG))*dterm3_ad
   dAF_ad = dAF_ad - AC*AE*QIK*(AC-AD)*dterm3_ad
   dAG_ad = dAG_ad - AC*AE*QIK*(AC-AD)*dterm3_ad
   dAD_ad = dAD_ad - AE*QIK*AC*(AF+AG)*dterm3_ad
   dAE_ad = dAE_ad + (AC-AD)*QIK*AC*(AF+AG)*dterm3_ad
   dQIK_ad = dQIK_ad + (AC-AD)*AE*AC*(AF+AG)*dterm3_ad
   dterm3_ad = ZERO

   dterm2_ad = dterm2_ad + dterm2x_ad
   dAT_ad = dAT_ad + (AC-AD)*AI*dterm2x_ad/(AC*(AF+AG)+tiny(0.))
   dterm2x_ad = zero

   dterm2_ad = dterm2_ad*AT/(AC*AC*(AF+AG)*(AF+AG)+tiny(0.))
   dAC_ad = dAC_ad + (AI*AC*(AF+AG)-(AF+AG)*(AC-AD)*AI)*dterm2_ad
   dAD_ad = dAD_ad -  AI*AC*(AF+AG)*dterm2_ad
   dAI_ad = dAI_ad + (AC-AD)*AC*(AF+AG)*dterm2_ad
   dAF_ad = dAF_ad - AC*(AC-AD)*AI*dterm2_ad
   dAG_ad = dAG_ad - AC*(AC-AD)*AI*dterm2_ad
   dterm2_ad = zero

   dterm1_ad = dterm1_ad + dterm1x_ad
   dAQ_ad = dAQ_ad + (AC-AD)*AF*dterm1x_ad/(AC*(AF+AG)+tiny(0.))
   dterm1x_ad = zero

   dterm1_ad = dterm1_ad * AQ/(AC*AC*(AF+AG)*(AF+AG)+tiny(0.))
   dAC_ad = dAC_ad + (AF*AC*(AF+AG)-(AF+AG)*(AC-AD)*AF)*dterm1_ad
   dAD_ad = dAD_ad - AF*AC*(AF+AG)*dterm1_ad
   dAF_ad = dAF_ad + ((AC-AD)*AC*(AF+AG) - AC*(AC-AD)*AF)*dterm1_ad
   dAG_ad = dAG_ad - AC*(AC-AD)*AF*dterm1_ad
   dterm1_ad = zero

   dAA_ad = dAA_ad + CP*dAI_ad
   dAI_ad = zero

   dAA_ad = dAA_ad + ELV*dAG_ad
   dELV_ad = dELV_ad + AA*dAG_ad
   dAG_ad = zero

   dAE_ad = dAE_ad + PRES*dAF_ad
   dAF_ad = zero

   dTIK_ad = dTIK_ad + CPR*TWO*TIK*dAE_ad
   dAE_ad = zero

   dAB_ad = dAB_ad + CCRIK1*dAD_ad
   dCCRIK1_ad = dCCRIK1_ad + AB*dAD_ad
   dAD_ad = zero

   dCWMIK_ad = dCWMIK_ad + HALF*dAC_ad
   dAB_ad = dAB_ad + dAC_ad
   dAC_ad = zero

   dCCRIK1_ad = dCCRIK1_ad + US00*CCRIK*QC*dAB_ad
   dCCRIK_ad = dCCRIK_ad + US00*CCRIK1*QC*dAB_ad
   dQC_ad = dQC_ad + US00*CCRIK*CCRIK1*dAB_ad
   dAB_ad = zero

   dQIK_ad = dQIK_ad + EPS*ELV*PRES*dAA_ad
   dELV_ad = dELV_ad + EPS*PRES*QIK*dAA_ad
   dAA_ad = zero

   dCCRIK_ad = dCCRIK_ad - dCCRIK1_ad
   dCCRIK1_ad = zero
else
   dCOND_ad = zero
endif

!--- EVAPORATION OF CLOUD WATER, E0 --------------

  IF (E0_save2 .gt. CWMIK*RDT) THEN
   dCWMIK_ad = dCWMIK_ad + RDT*dE0_ad
   dE0_ad = zero
  ENDIF

 IF (E0_save1 .lt. ZERO .or. CWMIK .LT. CLIMIT .or. CCRIK .GT. CCLIMIT) THEN
   dE0_ad = ZERO
  ENDIF

   dtx2_4_ad = dtx2_4_ad + RDT*dE0_ad
   dE0_ad = zero

   dtx2_3_ad = dtx2_3_ad + dtx2_4_ad
   ddelq_3_ad = ddelq_3_ad + dtx2_4_ad
   dtx2_4_ad = zero

   dqs3_ad = dqs3_ad + half*fac*(rfac**2)*ddelq_3_ad
   dtx3_3_ad = dtx3_3_ad - half*fac*(rfac**2)*ddelq_3_ad
   dfac_ad = dfac_ad - half*(qs_3-tx3_3)*(rfac**2)*ddelq_3_ad
   ddelq_3_ad = zero

   dqs3_ad = dqs3_ad + el2orc/tsq_3*dfac_ad
   dtsq_3_ad = dtsq_3_ad - el2orc*qs_3/(tsq_3**2)*dfac_ad
   dfac_ad = zero

   dtx1_3_ad = dtx1_3_ad + TWO*tx1_3*dtsq_3_ad
   dtsq_3_ad = zero

   des3_ad = des3_ad +  U00ik*eps*PRSL(i,k)*dqs3_ad/((PRSL(i,k)+epsm1*es_3)**2)
   dqs3_ad = zero

   call fpvsx_ad(tx1_3,es_3,dtx1_3_ad,des3_ad,.true.)

  !Second iteration
    fac = ONE + el2orc*qs_2/tsq_2
    rfac = one/fac

   dtx3_2_ad = dtx3_2_ad + dtx3_3_ad
   ddelq_2_ad = ddelq_2_ad + dtx3_3_ad
   dtx3_3_ad = zero

   dtx1_2_ad = dtx1_2_ad + dtx1_3_ad
   ddelq_2_ad = ddelq_2_ad - elocp*dtx1_3_ad
   dtx1_3_ad = zero


   ddelq_2_ad = ddelq_2_ad + dtx2_3_ad
   dtx2_3_ad = zero

   dqs2_ad = dqs2_ad + half*fac*(rfac**2)*ddelq_2_ad
   dtx3_2_ad = dtx3_2_ad - half*fac*(rfac**2)*ddelq_2_ad
   dfac_ad = dfac_ad - half*(qs_2-tx3_2)*(rfac**2)*ddelq_2_ad
   ddelq_2_ad = zero

   dqs2_ad = dqs2_ad + el2orc/tsq_2*dfac_ad
   dtsq_2_ad = dtsq_2_ad - el2orc*qs_2/(tsq_2**2)*dfac_ad
   dfac_ad = zero

   dtx1_2_ad = dtx1_2_ad + TWO*tx1_2*dtsq_2_ad
   dtsq_2_ad = zero

   des2_ad = des2_ad +  U00ik*eps*PRSL(i,k)*dqs2_ad/((PRSL(i,k)+epsm1*es_2)**2)
   dqs2_ad = zero

   call fpvsx_ad(tx1_2,es_2,dtx1_2_ad,des2_ad,.true.)

  ! First iteration
  fac = ONE + el2orc*qs_1/tsq_1
  rfac = ONE/fac

   ddelq_1_ad = ddelq_1_ad + dtx3_2_ad
   dtx3_1_ad = dtx3_1_ad + dtx3_2_ad
   dtx3_2_ad = zero

   ddelq_1_ad = ddelq_1_ad - elocp*dtx1_2_ad
   dtx1_1_ad = dtx1_1_ad + dtx1_2_ad
   dtx1_2_ad = zero

   ddelq_1_ad = ddelq_1_ad + dtx2_2_ad
   dtx2_2_ad = zero


   dqs1_ad = dqs1_ad + half*fac*(rfac**2)*ddelq_1_ad
   dtx3_1_ad = dtx3_1_ad  - half*fac*(rfac**2)*ddelq_1_ad
   dfac_ad = dfac_ad - half*(qs_1-tx3_1)*(rfac**2)*ddelq_1_ad
   ddelq_1_ad = zero

   dqs1_ad = dqs1_ad + (el2orc/tsq_1) * dfac_ad
   dtsq_1_ad = dtsq_1_ad - (el2orc*qs_1/(tsq_1**2))*dfac_ad
   dfac_ad = zero


   dtx1_1_ad = dtx1_1_ad + TWO*tx1_1*dtsq_1_ad
   dtsq_1_ad = zero

   
   des1_ad = des1_ad +  U00ik*eps*PRSL(i,k)*dqs1_ad/((PRSL(i,k)+epsm1*es_1)**2)
   dqs1_ad = zero

   call fpvsx_ad(tx1_1,es_1,dtx1_1_ad,des1_ad,.true.)

   dqik_ad = dqik_ad + dtx3_1_ad
   dtx3_1_ad = zero
   dtik_ad = dtik_ad + dtx1_1_ad
   dtx1_1_ad = zero

!-------------------

 if(CCRIK_tmp1 .LE. ZERO) then
  dCCRIK_ad = ZERO
 endif

  IF(RQIKK .ge. ONE) then
   dCCRIK_ad = ZERO
  ELSE IF (RQIKK .LT. U00IK) then
   dCCRIK_ad = ZERO
  ELSE
   dRQIKK_ad = dRQIKK_ad +  &
    HALF/SQRT((ONE-RQIKK+tiny_r_kind)/(ONE-U00IK+tiny_r_kind))*dCCRIK_ad/(ONE-U00IK+tiny_r_kind)
   dCCRIK_ad = zero
  ENDIF


  IF (RQIK .gt. ONE) then
   dRQIKK_ad = ZERO
  ELSE IF (RQIK .lt. ZERO ) then
   dRQIKK_ad = ZERO
  ELSE
   dRQIK_ad = dRQIK_ad + dRQIKK_ad
   dRQIKK_ad = ZERO
  ENDIF

   dQC_ad = dQC_ad - QIK*dRQIK_ad/((QC+tiny_r_kind)**2)
   dQIK_ad = dQIK_ad + dRQIK_ad *(QC+tiny_r_kind)/((QC+tiny_r_kind)**2)
   dRQIK_ad = ZERO

!----- SATURATION SPECIFIC HUMIDITY ----------------

  dQW_ad = dQW_ad + dQC_ad
  dQC_ad = zero

  dFIW_ad = dFIW_ad +(ELIV - ELWV)*dELV_ad
  dELV_ad = zero


  dadvT_ad(I,K) = dadvT_ad(I,K) + dAT_ad
  dAT_ad = zero
  dadvQ_ad(I,K) = dadvQ_ad(I,K) + dAQ_ad
  dAQ_ad = zero
  dadvP_ad(I,K) = dadvP_ad(I,K) + dAP_ad
  dAP_ad = zero

  dIWIK_ad = dIWIK_ad + dFIW_ad
  dFIW_ad = zero


  dIW_ad(I,K) = dIW_ad(I,K) + dIWIK_ad
  dIWIK_ad = zero



  dX_ad = dX_ad + HALF*(ONE-tanh(X)*tanh(X))*dIW_ad(I,K)
  dIW_ad(I,K) = zero

  dTMT0_ad = dTMT0_ad - dX_ad
  dX_ad = zero


  IF (QW_tmp1 .LE. EPSQ) THEN
   dQW_ad = ZERO
  ENDIF
 
  desk_ad = desk_ad + dQW_ad*EPS*PRSL(I,K)/((PRSL(I,K)+epsm1*esk)**2)
  dQW_ad = zero
  call fpvsx_ad(Tik,esk,dTik_ad,desk_ad,.true.)         ![cb]


  dCWM_in_ad(I,K) = dCWM_in_ad(I,K) + dCWMIK_ad
  dCWMIK_ad = zero

  dQ_in_ad(I,K) = dQ_in_ad(I,K)+dQIK_ad
  dQIK_ad = zero

  dT_in_ad(I,K) = dT_in_ad(I,K) + dTMT0_ad
  dTMT0_ad = zero

  dT_in_ad(I,K) = dT_in_ad(I,K) + dTIK_ad
101  dTIK_ad = zero
!--------------------------------

 ENDDO
ENDDO  

END SUBROUTINE GFS_GSCOND_AD
