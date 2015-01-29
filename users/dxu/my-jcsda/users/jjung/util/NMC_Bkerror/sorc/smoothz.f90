subroutine smoothz(p1,n,nc,m,al,be,dss,iflg)
  DIMENSION DSS(N),P1(nc,N),P2(nc,N),BE(M),AL(N,M),GA(nc,M),DE(nc,M)
      nm=n-1
!  recursive filter in the vertical direction
       if(iflg.eq.2)then
       do j=1,n
       do i=1,nc
       p2(i,j)=p1(i,j)*dss(j)
       enddo
       enddo
      CALL RF0V(GA,DE,M*nc)
      CALL RFHV(P2,p1,nc,Nm,M,AL,BE,GA,DE)
       endif
!  adjoint of filter in the vertical direction
        if(iflg.eq.1)then
      CALL RF0V(GA,DE,M*nc)
      CALL RFHV(P1,p2,nc,Nm,M,AL,BE,GA,DE)
       do i=1,n
       do j=1,nc
       p1(j,i)=p2(j,i)*dss(i)
       enddo
       enddo
       endif
      RETURN
      END
      SUBROUTINE RF0V(GAP,DEP,M)
      DIMENSION GAP(M),DEP(M)
      DO I=1,M
       GAP(I)=0.
       DEP(I)=0.
      ENDDO
      RETURN
      END
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!                   SUBROUTINE RFHV
!   Apply pseudo-Gaussian "right-plus-left" smoother to a single "vector"
!   of data.
!
! --> P1:   Input data.
! <-- P2:   Output data.
! --> N:    Number of grid intervals (one less than number of points).
! --> M:    Degree of approximation of smoother to a Gaussian.
! --> AL:   Matrix of "Alpha" coefficients (computed in RFDPARV).
! --> BE:   Vector of "Beta" coefficients (computed in RFDPAR1).
! <-> GAP:  Gamma vector of exponential amplitudes of P decaying to the right.
! <-> DEP:  Delta vector of exponential amplitudes of P decaying to the left.
!------------------------------------------------------------------------------
 SUBROUTINE RFHV(P1,p2,nc,N,M,AL,BE,GA,DE)
  DIMENSION P1(nc,0:N),P2(nc,0:N),AL(0:N,M),BE(M),GA(nc,M),DE(nc,M)
      dimension dss(0:n)
      KMOD2=MOD(M,2)
       do j=0,n
       do i=1,nc
       p2(i,j)=0.
       enddo
       enddo
! Advancing filter:
      DO I=0,N
!      P1I=P1(I)
!      P2(I)=0.
       IF(KMOD2.EQ.1)THEN  ! Treat the real root:
        do j=1,nc
        GA(j,1)=AL(I,1)*GA(j,1)+BE(1)*P1(j,i)
        enddo
        do j=1,nc
        P2(j,I)=P2(j,I)+GA(j,1)
        enddo
       ENDIF
                           ! Treat remaining complex roots:
       DO KR=KMOD2+1,M,2   ! <-- Index of "real" components
        KI=KR+1            ! <-- Index of "imag" components
        do j=1,nc
        GAKR=GA(j,KR)
        GAKI=GA(j,KI)
        GA(j,KR)=AL(I,KR)*GAKR-AL(I,KI)*GAKI+BE(KR)*P1(j,i)
        GA(j,KI)=AL(I,KI)*GAKR+AL(I,KR)*GAKI+BE(KI)*P1(j,i)
        enddo
        do j=1,nc
        P2(j,I)=P2(j,I)+GA(j,KR)
        enddo
       ENDDO
      ENDDO

! Backing filter:
      DO I=N,0,-1
!      P1I=P1(I)
       IF(KMOD2.EQ.1)THEN  ! Treat the real root:
        do j=1,nc
        P2(j,I)=P2(j,I)+DE(j,1)
        enddo
        do j=1,nc
        DE(j,1)=AL(I,1)*(DE(j,1)+BE(1)*P1(j,i))
        enddo
       ENDIF
                           ! Treat remaining complex roots:
       DO KR=KMOD2+1,M,2   ! <-- Index of "real" components
        KI=KR+1            ! <-- Index of "imag" components
        do j=1,nc
        P2(j,I)=P2(j,I)+DE(j,KR)
        enddo
        do j=1,nc
        DEKR=DE(j,KR)+BE(KR)*P1(j,i)
        DEKI=DE(j,KI)+BE(KI)*P1(j,i)
        DE(j,KR)=AL(I,KR)*DEKR-AL(I,KI)*DEKI
        DE(j,KI)=AL(I,KI)*DEKR+AL(I,KR)*DEKI
        enddo
       ENDDO
      ENDDO
      RETURN
      END
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!                   SUBROUTINE RFDPAR1
! Initialize the Beta and exponential Rate vectors characterizing the unit-
! width pseudo-Gaussian of degree M.
!
! <-- BE:   Beta vector of coupling coefficients between input and each
!           exponential component. For complex exponentials (which occur in
!           conjugate pairs) store REAL, then IMAGINARY, of one member of pair.
! <-- RATE: Vector of "decay rates" characteristic of the exponential
!           components. For paired complex rates, same convention as for BE.
! --> M:    Degree of approximation to the Gaussian [FT of this pseudo-
!           Gaussian is proportional to 1./(1+k**2+ ...+k**(2M)/M!)].
!------------------------------------------------------------------------------
      SUBROUTINE RFDPAR1(BE,RATE,M)
      IMPLICIT COMPLEX(C)
      PARAMETER(QCRIT=1.E-3)
      PARAMETER(NN=12)
      LOGICAL POLISH
      DIMENSION COF(0:NN),CROOT(NN),VAN(NN,NN),BE(m),RATE(m)
      DATA POLISH/.TRUE./
      polish=.true.
      KMOD2=MOD(M,2)
      COF(0)=1.
      DO I=1,M
       COF(I)=.5*COF(I-1)/I
      ENDDO
      CALL ZROOTS(COF,M,CROOT,POLISH)
      IF(KMOD2.EQ.1)THEN    ! Treat the single real root:
       R=-REAL(CROOT(1))
       Q=-AIMAG(CROOT(1))
       QA=ABS(Q)
       IF(QA.GT.QCRIT)STOP
       R=SQRT(R)
       RATE(1)=R
       VAN(1,1)=1.
       VAN(2,1)=r
       DO I=3,M
       VAN(I,1)=VAN(I-1,1)*R*R
       ENDDO
      ENDIF
      DO J2=2,M,2         ! Loop over remaining independent complex roots
       JREAL=KMOD2+J2-1
       JIMAG=KMOD2+J2
       CA=-CROOT(J2)
       CB=CSQRT(CA)
       R=REAL(CB)
       Q=AIMAG(CB)
       IF(R.LT.0.)THEN
        CB=-CB
        R=-R
        Q=-Q
       ENDIF
       RATE(JREAL)=R
       RATE(JIMAG)=Q
       VAN(1,JREAL)=1.
       VAN(1,JIMAG)=0.
       DO I=2,M
        IPOW=I*2-3
        CC=CB**IPOW
        VAN(I,JREAL)=REAL(CC)
        VAN(I,JIMAG)=-AIMAG(CC)
       ENDDO
      ENDDO
      BE(1)=1.
      DO I=2,M
       BE(I)=0.
      ENDDO
      CALL LINMM(VAN,BE,M,1,NN,M)
      RETURN
      END
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!                   SUBROUTINE RFDPAR2
! Initialize the "turning" matrix and the amplitude rescaling factor
! of the pseudo-Gaussian smoother of degree M
!
! --> BE:   Beta vector (computed in previous call to RFDPAR1).
! --> RATE: Rate vector of the (in general complex) exponential contributions.
! <-- TURN: Turning matrix for 2nd sweep of filter along a line.
! <-- SAMP: Factor by which the amplitude at second sweep is normalized.
! --> M:    Degree of pseudo-Gaussian
!------------------------------------------------------------------------------
      SUBROUTINE RFDPAR2(BE,RATE,TURN,SAMP,M)
      PARAMETER(NN=12)
      IMPLICIT COMPLEX(C)
      DIMENSION BE(M),RATE(M),TURN(M,M)
      KMOD2=MOD(M,2)
      S=0.
      IF(KMOD2.EQ.1)THEN     ! The first root is real:
       R1=RATE(1)
       BE1=BE(1)
       TURN(1,1)=BE1/(2*R1)
       S=S+TURN(1,1)*BE1
       DO LR=KMOD2+1,M,2
        LI=LR+1
        CBE=CMPLX(BE(LR),BE(LI))
        CRL=CMPLX(RATE(LR),RATE(LI))
        CL1=CBE/(R1+CRL)
        TURN(LR,1)=REAL(CL1)
        TURN(LI,1)=AIMAG(CL1)
        C1L=BE1/(R1+CRL)
        TURN(1,LR)=REAL(C1L)
        TURN(1,LI)=-AIMAG(C1L)
        S=S+TURN(LR,1)*BE1+TURN(1,LR)*BE(LR)+TURN(1,LI)*BE(LI)
       ENDDO
      ENDIF
      DO KR=KMOD2+1,M,2
       KI=KR+1
       CRK=CMPLX(RATE(KR),RATE(KI))
       CRJ=CONJG(CRK)
       BEKR=BE(KR)
       BEKI=BE(KI)
       DO LR=KMOD2+1,M,2
        LI=LR+1
        CBEH=.5*CMPLX(BE(LR),BE(LI))
        CRL=CMPLX(RATE(LR),RATE(LI))
        CLK=CBEH/(CRL+CRK)
        CLJ=CBEH/(CRL+CRJ)
        CLKR=CLK+CLJ
        CLKI=CLK-CLJ
        TURN(LR,KR)=  REAL(CLKR)
        TURN(LI,KR)= AIMAG(CLKR)
        TURN(LR,KI)=-AIMAG(CLKI)
        TURN(LI,KI)=  REAL(CLKI)
        S=S+TURN(LR,KR)*BEKR+TURN(LR,KI)*BEKI
       ENDDO
      ENDDO
      SAMP=.5/S
      RETURN
      END

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!                   SUBROUTINE RFDPARV
! Initialize Alpha coefficients for pseudo-Gaussian smoother of degree M
! along a line with varying effective scale at each point
!
! --> DSH:  Array of "effective" spacing (in stretched space S) between gridpts
! --> RATE: Exponential rates in this degree of approximation to the Gaussian.
! <-- AL:   Array of "transfer function" coefficients to propagate vectors
!           GAP and DEP of amplitudes of the exponential contributions to
!           a filtered field along this grid line.
! --> N:    Number of grid spaces along this line (ie, number of grdpts = N+1)
! --> M:    Degree of approximation to pseudo-Gaussian.
!------------------------------------------------------------------------------

      SUBROUTINE RFDPARV(DSH,RATE,AL,N,M)
      IMPLICIT COMPLEX(C)
      DIMENSION DSH(N),RATE(M),AL(n,m)

      KMOD2=MOD(M,2)
      IF(KMOD2.EQ.1)THEN
       R1=RATE(1)
       DO I=1,N
        AL(I,1)=EXP(-R1*DSH(I))
       ENDDO
      ENDIF
      DO KR=KMOD2+1,M,2
       KI=KR+1
       CRK=CMPLX(RATE(KR),RATE(KI))
       DO I=1,N
        CRDS=CRK*DSH(I)
        CAL=CEXP(-CRDS)
        AL(i,KR)=REAL(CAL)
        AL(i,KI)=AIMAG(CAL)
       ENDDO
      ENDDO
      RETURN
      END


      SUBROUTINE ZROOTS(A,M,ROOTS,POLISH)
      PARAMETER (EPS=1.E-6,MAXM=101)
      COMPLEX A(*),ROOTS(M),AD(MAXM),X,B,C
      LOGICAL POLISH
      DO 11 J=1,M+1
        AD(J)=A(J)
11    CONTINUE
      DO 13 J=M,1,-1
        X=CMPLX(0.,0.)
        CALL LAGUER(AD,J,X,EPS,.FALSE.)
        IF(ABS(AIMAG(X)).LE.2.*EPS**2*ABS(REAL(X))) X=CMPLX(REAL(X),0.)
        ROOTS(J)=X
        B=AD(J+1)
        DO 12 JJ=J,1,-1
          C=AD(JJ)
          AD(JJ)=B
          B=X*B+C
12      CONTINUE
13    CONTINUE
      IF (POLISH) THEN
        DO 14 J=1,M
          CALL LAGUER(A,M,ROOTS(J),EPS,.TRUE.)
14      CONTINUE
      ENDIF
      DO 16 J=2,M
        X=ROOTS(J)
        DO 15 I=J-1,1,-1
          IF(REAL(ROOTS(I)).LE.REAL(X))GO TO 10
          ROOTS(I+1)=ROOTS(I)
15      CONTINUE
        I=0
10      ROOTS(I+1)=X
16    CONTINUE
      RETURN
      END

      SUBROUTINE LAGUER(A,M,X,EPS,POLISH)
      COMPLEX A(*),X,DX,X1,B,D,F,G,H,SQ,GP,GM,G2,ZERO
      LOGICAL POLISH
      PARAMETER (ZERO=(0.,0.),EPSS=6.E-8,MAXIT=100)
      DXOLD=CABS(X)
      DO 12 ITER=1,MAXIT
        B=A(M+1)
        ERR=CABS(B)
        D=ZERO
        F=ZERO
        ABX=CABS(X)
        DO 11 J=M,1,-1
          F=X*F+D
          D=X*D+B
          B=X*B+A(J)
          ERR=CABS(B)+ABX*ERR
11      CONTINUE
        ERR=EPSS*ERR
        IF(CABS(B).LE.ERR) THEN
          RETURN
        ELSE
          G=D/B
          G2=G*G
          H=G2-2.*F/B
          SQ=CSQRT((M-1)*(M*H-G2))
          GP=G+SQ
          GM=G-SQ
          IF(CABS(GP).LT.CABS(GM)) GP=GM
          DX=M/GP
        ENDIF
        X1=X-DX
        IF(X.EQ.X1)RETURN
        X=X1
        CDX=CABS(DX)
        DXOLD=CDX
        IF(.NOT.POLISH)THEN
          IF(CDX.LE.EPS*CABS(X))RETURN
        ENDIF
12    CONTINUE
      PAUSE 'too many iterations'
      RETURN
      END

