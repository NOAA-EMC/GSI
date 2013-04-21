SUBROUTINE GFS_GSCOND_TL(IM,IX,KM,DT, SL,PS, T_in, Q_in, CWM_in, RHC, &
                     advT, advQ, advP, &
                     T_out, Q_out, CWM_out, &
                     dadvT,dadvQ, dadvP, &
                     dT_in, dQ_in, dCWM_in, &     !input
                     dT_out, dQ_out, dCWM_out)    !output

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gfs_gscond_tl    forward & tangent linear model for GFS gridscale condensation
!     prgmmr:   Min-Jeong Kim     org: np23                date: 2012-02-16
!
! abstract:  This subroutine contains the forward and tangent linear models for the
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

REAL(r_kind) :: TIK, TMT0, X, Y, QIK, CWMIK, esk, QW, &
                U00IK, FI,PRES, IWIK, AT, AQ, AP, FIW, ELV, QC, &
                RQIK, RQIKK, CCRIK, CCRIK1, E0,E1,E2,E3,E4, &
                COND, CONDI,CONDIx, COND1, COND2, COND3, CONE0, DT, &
                tx1_1, tx1_2, tx1_3, es_1, tsq_1, qs_1, delq_1, &
                tx2_1, tx2_2, tx2_3, es_2, tsq_2, qs_2, delq_2, &
                tx3_1, tx3_2, tx3_3, es_3, tsq_3, qs_3, delq_3, &
                tx2_4, AI, AA, AB, AC, AF, AD, AE, AG, RDT, US00, &
                term1, term2, term3
REAL(r_kind) :: dTIK, dTMT0, dX, dQIK, dCWMIK, desk_dt, dQW, dQW_desk, desk,&
                dFI, dIWIK, dFIW, dELV, dQC, dRQIK, dRQIKK, dCCRIK, dCCRIK1,  &
                dtx1_1, dtx1_2, dtx1_3, dtx2_4, dtx2_1, dtx2_2, dtx2_3, &
                dtx3_1, dtx3_2, dtx3_3, dqs1, dqs2, dqs3, &
                dtsq_1, dtsq_2, dtsq_3, fac, rfac, dfac, ddelq_1, &
                ddelq_2, ddelq_3, dE0,dE1,dE2,dE3,dE4, &
                dCOND, dCOND1,dCOND2, dCOND3, des1, des2, des3,&
                dAA, dAB, dAC, dAD, dAE, dAF, dAG, dAI, &
                dterm1, dterm2, dterm3, &
                dterm1x, dterm2x, dterm3x, &
                dcondI,dCONDIx, dcone,dcone0

REAL(r_kind), dimension(IX,KM) :: T_in, Q_in, CWM_in, PRSL, SL, &
                  IW,xIW,  RHC, advT, advQ, advP, T_out, Q_out, CWM_out, dIW
REAL(r_kind), intent(in), dimension(IX,KM) :: dT_in, dQ_in, dCWM_in
REAL(r_kind), intent(out), dimension(IX,KM) :: dT_out, dQ_out, dCWM_out
REAL(r_kind), dimension(IX,KM) :: dTgs_tend, dQgs_tend, dCWMgs_tend
REAL(r_kind), dimension(IX,KM) :: dadvT, dadvQ, dadvP 
REAL(r_kind) :: dAT, dAQ, dAP 

REAL(r_kind), dimension(IX) :: PS, QI, QINT
REAL(r_kind), dimension(IX) :: dQI, dQINT

!-------------------------------
RDT=ONE/DT

dT_out(:,:) = zero
dQ_out(:,:) = zero
dCWM_out(:,:) = zero

DO I = 1, IM
 IW(I,KM)=ZERO

 DO K = KM, 1, -1
  TIK=T_in(I,K)       ![K]
 dTIK = dT_in(I,K)

  TMT0=T_in(I,K)-TTP  
 dTMT0 = dT_in(I,K)

  QIK=Q_in(I,K)
 dQIK = dQ_in(I,K)

  CWMIK = CWM_IN(I,K)
 dCWMIK = dCWM_in(I,K)

  PRSL(I,K) = PS(I) * SL(I,K) ![cb]
  call fpvsx_tl(tik,esk,dtik,desk)

  QW = EPS *esk/(PRSL(I,K)+EPSM1*esk)
  dQW = desk*EPS*PRSL(I,K)/((PRSL(I,K)+epsm1*esk)**2)

  IF (QW .LE. EPSQ) THEN
    QW = EPSQ
   dQW = ZERO
  ENDIF

  X = (-15.-TMT0)
 dX = -dTMT0

  IW(I,K) = HALF*(ONE+tanh(X))
  dIW(I,K) = HALF*(ONE-tanh(X)*tanh(X))*dX

!----- AT, AQ, DP/DT --------------------------------
  IWIK = IW(I,K)
 dIWIK = dIW(I,K)

  U00IK = RHC(I,K)
  PRES=PRSL(I,K)*1000.0_r_kind
  AT=advT(I,K)
  AQ=advQ(I,K)
  AP=advP(I,K)
 dAT=dadvT(I,K)
 dAQ=dadvQ(I,K)
 dAP=dadvP(I,K)

!----- SATURATION SPECIFIC HUMIDITY ----------------
  FIW=IWIK
 dFIW=dIWIK

  ELV=(ONE-FIW)*ELWV+FIW*ELIV
 dELV = -dFIW*ELWV + dFIW*ELIV

  QC = QW
 dQC = dQW

!----------  RELATIVE  HUMIDITY --------------------
  RQIK=QIK/(QC+tiny_r_kind)
 dRQIK =  (dQIK*(QC+tiny_r_kind) - QIK*dQC) / ((QC+tiny_r_kind)**2)

!----------  CLOUD COVER RATIO, CCRIK ---------------
  IF (RQIK .gt. ONE) then
    RQIKK = ONE
   dRQIKK = ZERO
  ELSE IF (RQIK .lt. ZERO ) then
    RQIKK = ZERO
   dRQIKK = ZERO
  ELSE
    RQIKK = RQIK
   dRQIKK = dRQIK
  ENDIF

  IF(RQIKK .ge. ONE) then
   CCRIK = ONE
   dCCRIK = ZERO
  ELSE IF(RQIKK .LT. U00IK) THEN
   CCRIK = ZERO
   dCCRIK = ZERO
  ELSE
   CCRIK=ONE-SQRT((ONE-RQIKK+tiny_r_kind)/(ONE-U00IK+tiny_r_kind))
   dCCRIK= HALF/SQRT((ONE-RQIKK+tiny_r_kind)/(ONE-U00IK+tiny_r_kind)) * dRQIKK / (ONE-U00IK+tiny_r_kind)
  ENDIF

 if(CCRIK .LE. ZERO) then
   CCRIK = tiny_r_kind
  dCCRIK = ZERO
 endif

!--- EVAPORATION OF CLOUD WATER, E0 --------------

  !First iteration
    tx1_1 = tik
   dtx1_1 = dtik
    tx3_1 = qik
   dtx3_1 = dqik

    call fpvsx_tl(TX1_1,es_1,dtx1_1,des1)
    qs_1=U00ik*eps*es_1/(prsl(i,k)+epsm1*es_1)
    dqs1 = U00ik*eps*PRSL(i,k)*des1/((PRSL(i,k)+epsm1*es_1)**2)

    tsq_1=tx1_1*tx1_1
   dtsq_1=TWO*tx1_1*dtx1_1

    delq_1=HALF*(qs_1-tx3_1)*tsq_1/(tsq_1+el2orc*qs_1)
    fac = ONE + el2orc*qs_1/tsq_1
    rfac = ONE/fac
   dfac = el2orc*(dqs1/tsq_1  - qs_1*dtsq_1/tsq_1**2)
   ddelq_1 = (half*(dqs1-dtx3_1)*fac - half*dfac*(qs_1-tx3_1))* rfac**2

    tx2_2=delq_1
   dtx2_2=ddelq_1
   tx1_2 = tx1_1 - delq_1*elocp
   dtx1_2 = dtx1_1 - ddelq_1*elocp
    tx3_2 = tx3_1 + delq_1
   dtx3_2 = dtx3_1 + ddelq_1

  !Second iteration
    call fpvsx_tl(TX1_2,es_2,dtx1_2,des2)
    qs_2=U00ik*eps*es_2/(prsl(i,k)+epsm1*es_2)
    dqs2 = U00ik*eps*prsl(i,k)*des2/(prsl(i,k)+epsm1*es_2)**2


    tsq_2=tx1_2*tx1_2
   dtsq_2 = TWO*tx1_2*dtx1_2
   delq_2=HALF*(qs_2-tx3_2)*tsq_2/(tsq_2+el2orc*qs_2)

    fac = ONE + el2orc*qs_2/tsq_2
    rfac = one/fac

   dfac = el2orc*(dqs2/tsq_2  - qs_2*dtsq_2/tsq_2**2)
   ddelq_2 = (half*(dqs2-dtx3_2)*fac - half*dfac*(qs_2-tx3_2))* rfac**2

    tx2_3=delq_2
   dtx2_3=ddelq_2

    tx1_3 = tx1_2 - delq_2*elocp
   dtx1_3 = dtx1_2 - ddelq_2*elocp

    tx3_3 = tx3_2 + delq_2
   dtx3_3 = dtx3_2 + ddelq_2



  !Third iteration
    call fpvsx_tl(TX1_3,es_3,dtx1_3,des3)
    qs_3=U00ik*eps*es_3/(prsl(i,k)+epsm1*es_3)
    dqs3 = U00ik*eps*prsl(i,k)*des3/(prsl(i,k)+epsm1*es_3)**2 

    tsq_3=tx1_3*tx1_3
   dtsq_3 = TWO*tx1_3*dtx1_3

    delq_3=HALF*(qs_3-tx3_3)*tsq_3/(tsq_3+el2orc*qs_3)
    fac = ONE + el2orc*qs_3/tsq_3
    rfac = one/fac
   dfac = el2orc*(dqs3/tsq_3  - qs_3*dtsq_3/tsq_3**2)
   ddelq_3 = (half*(dqs3-dtx3_3)*fac - half*dfac*(qs_3-tx3_3))* rfac**2

    tx2_4=tx2_3+delq_3
   dtx2_4=dtx2_3+ddelq_3

  E0 = tx2_4*RDT
 dE0 = dtx2_4*RDT

  IF (E0 .lt. ZERO .or. CWMIK .LT. CLIMIT .or. CCRIK .GT. CCLIMIT) THEN
    E0 = ZERO
   dE0 = ZERO
  ENDIF

  IF (E0 .gt. CWMIK*RDT) THEN
    E0 = CWMIK*RDT
   dE0 = dCWMIK*RDT
  ENDIF

!--- CONDENSATION OF CLOUD WATER, COND --------------
 if (ccrik > cclimit .and. QW > epsq) then

  US00 = ONE - U00IK
  CCRIK1 = ONE - CCRIK
 dCCRIK1 =  -dCCRIK

  AA = EPS*ELV*PRES*QIK
 dAA = EPS*ELV*PRES*dQIK + EPS*dELV*PRES*QIK

  AB = CCRIK*CCRIK1*QC*US00
 dAB = (CCRIK*dCCRIK1*QC + dCCRIK*CCRIK1*QC+ CCRIK*CCRIK1*dQC)*US00

  AC = AB + HALF*CWMIK
 dAC =  dAB + HALF*dCWMIK

  AD = AB *CCRIK1
 dAD = dAB *CCRIK1 + AB*dCCRIK1

  AE = CPR*TIK*TIK
 dAE = CPR*TWO*TIK*dTIK

  AF = AE*PRES
 dAF = dAE*PRES

  AG = AA*ELV
 dAG = dAA*ELV + AA*dELV

  AI = CP*AA
 dAI = CP*dAA

  term1 = (AC-AD)*AF*AQ/(AC*(AF+AG)+tiny_r_kind)
 dterm1 =((dAC-dAD)*AF+dAF*(AC-AD))*AC*(AF+AG)- &
          ((dAF+dAG)*AC + dAC*(AF+AG))*(AC-AD)*AF
 dterm1 = dterm1*AQ/(AC*AC*(AF+AG)*(AF+AG)+tiny_r_kind)
 dterm1x= dterm1 + (AC-AD)*AF*dAQ/(AC*(AF+AG)+tiny_r_kind)

 term2 = (AC-AD)*AI*AT/(AC*(AF+AG)+tiny_r_kind)
 dterm2 = ((dAC-dAD)*AI+(AC-AD)*dAI)*AC*(AF+AG)-&
          ((dAF+dAG)*AC+dAC*(AF+AG))*(AC-AD)*AI
 dterm2 = dterm2*AT/(AC*AC*(AF+AG)*(AF+AG)+tiny_r_kind)
 dterm2x = dterm2 + (AC-AD)*AI*dAT/(AC*(AF+AG))

 term3 = (AC-AD)*AE*QIK*AP/(AC*(AF+AG)+tiny_r_kind)
 dterm3 = ((dAC-dAD)*AE*QIK+(AC-AD)*dAE*QIK+(AC-AD)*AE*dQIK)*AC*(AF+AG)-&
          (dAC*(AF+AG)+(dAF+dAG)*AC)*AE*QIK*(AC-AD)
 dterm3 = dterm3*AP/(AC*AC*(AF+AG)*(AF+AG)+tiny_r_kind)
 dterm3x = dterm3 + (AC-AD)*AE*QIK*dAP/(AC*(AF+AG)+tiny_r_kind)

 if(abs((AC-AD)*(AF*AQ-AI*AT+AE*QIK*AP)) .gt. zero .and. abs(AC*(AF+AG)+tiny_r_kind) .gt. zero) then
  COND1 = (AC-AD)*(AF*AQ-AI*AT+AE*QIK*AP)/(AC*(AF+AG)+tiny_r_kind)
 dCOND1 = dterm1x  -dterm2x + dterm3x
 else
 COND1= zero
 dCOND1= zero
 endif
 
  CONDI = (QIK - U00IK * QC * ONE)*RDT
 dCONDI =  (dQIK - U00IK*dQC*ONE)*RDT


  IF(COND1 < CONDI) then
    COND2 = COND1
   dCOND2 = dCOND1
  ELSE
    COND2 = CONDI
   dCOND2 = dCONDI
  ENDIF

  IF(COND2 .GT. ZERO) THEN
    COND3= COND2
   dCOND3= dCOND2
  ELSE
    COND3 = ZERO
   dCOND3 = ZERO
  ENDIF

  COND = COND3
 dCOND = dCOND3

else
  COND = ZERO
 dCOND = ZERO
endif

!C--- UPDATE OF T, Q, CWM   --------------
  CONE0 = (COND-E0)*DT
 dCONE0 = (dCOND - dE0)*DT

  CWM_out(I,K) = CWM_in(I,K) +CONE0
 dCWM_out(I,K) = dCWM_in(I,K) + dCONE0

  IF(CWM_out(I,K) .LE. CLIMIT) THEN
    CWM_out(i,k) = ZERO
   dCWM_out(i,k) = ZERO
    CONE0 = -CWM_in(I,K)
   dCONE0 = -dCWM_in(I,K)
  ENDIF

  T_out(I,K) = T_in(I,K) + ELV*RCP*CONE0
 dT_out(I,K) = dT_in(I,K) + ELV*RCP*dCONE0 + dELV*RCP*CONE0

  Q_out(I,K) = Q_in(I,K) - CONE0
 dQ_out(I,K) = dQ_in(I,K) - dCONE0

 ENDDO
ENDDO  

END SUBROUTINE GFS_GSCOND_TL
