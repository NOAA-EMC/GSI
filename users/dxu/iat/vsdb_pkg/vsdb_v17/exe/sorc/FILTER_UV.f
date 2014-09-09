      SUBROUTINE FILTER_UV (IMAXIN,JJMAXIN,JMAXIN,JPASS,KGDSIN,
     &                      UIN,LIN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C   SUBPROGRAM: FILTER_UV
C   PRGMMR: BALDWIN          ORG: NP22        DATE: 98-08-11  
C
C ABSTRACT: FILTER_UV FILTERS A WIND FIELD.  IT TREATS THE STAGGERED
C           E-GRID SPECIALLY (201,203).
C
C PROGRAM HISTORY LOG:
C   98-08-11  BALDWIN     ORIGINATOR
C
C USAGE:  CALL FILTER_UV (IMAXIN,JJMAXIN,JMAXIN,JPASS,KGDSIN,
C    &                      UIN,LIN,IRET)
C
C   INPUT:
C         IMAXIN            INTEGER - MAX X DIMENSION OF INPUT GRID
C         JJMAXIN           INTEGER - MAX Y DIMENSION OF INPUT GRID
C         JMAXIN            INTEGER - MAX DIMENSION OF UIN
C         JPASS             INTEGER - NUMBER OF FILTER PASSES TO MAKE
C         KGDSIN(22)        INTEGER - KGDS FOR THIS GRID
C         UIN(JMAXIN)       REAL    - WIND COMPONENT TO FILTER
C         LIN(JMAXIN)       LOGICAL*1 - BITMAP CORRESPONDING TO UIN
C
C   OUTPUT:
C         UIN(JMAXIN)       REAL    - WIND COMPONENT TO FILTER
C         IRET              INTEGER - RETURN CODE
C
C   RETURN CODES:
C     IRET =   0 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : CRAY J-916
C
C$$$
      INTEGER KGDSIN(22)
      INTEGER IVE(JJMAXIN),IVW(JJMAXIN)
      LOGICAL*1 LIN(JMAXIN)
      LOGICAL*1 LFLT(IMAXIN,JJMAXIN),LFLT2(IMAXIN,JJMAXIN)
      REAL UIN(JMAXIN),Z(IMAXIN,JJMAXIN),Z1(IMAXIN,JJMAXIN)

      IRET=0

      IF (JPASS.GT.0) THEN
C
C
C     CALCULATE THE I-INDEX EAST-WEST INCREMENTS FOR 201/203 GRIDS
C
      IF (KGDSIN(1).EQ.203.OR.KGDSIN(1).EQ.201) THEN
        DO J=1,JJMAXIN
          IVE(J)=MOD(J,2)
          IVW(J)=IVE(J)-1
        ENDDO
      ENDIF

      IMX=KGDSIN(2)
      JMX=KGDSIN(3)
      IF (KGDSIN(1).EQ.201) THEN
       IMX=KGDSIN(7)
       JMX=KGDSIN(8)
      ENDIF
C
C **  FILTER WINDS   25-PT BLECK FILTER               
C
      IF (KGDSIN(1).EQ.201) THEN
       KK=0
       DO J=1,JMX
        IMAX=IMX-MOD(J,2)
        DO I=1,IMX
          IF (I.LE.IMAX) KK=KK+1
          Z1(I,J)=UIN(KK)
          LFLT(I,J)=LIN(KK)
        ENDDO
       ENDDO
      ELSE
       DO J=1,JMX
        DO I=1,IMX
          KK=(J-1)*IMX+I
          Z1(I,J)=UIN(KK)
          LFLT(I,J)=LIN(KK)
        ENDDO
       ENDDO
      ENDIF
C
C  ONLY FILTER IF ALL 25 PTS ARE WITHIN BITMAP
C
      IF (KGDSIN(1).NE.203.AND.KGDSIN(1).NE.201) THEN
C$OMP PARALLEL DO PRIVATE(J)
       DO J = 1,JMX-4
         DO I = 1,IMX-4
             LFLT2(I+2,J+2)=LFLT(I  ,J  ).AND.LFLT(I+1,J  ).AND.
     &    LFLT(I+2,J  ).AND.LFLT(I+3,J  ).AND.LFLT(I+4,J  ).AND.
     &    LFLT(I  ,J+1).AND.LFLT(I+1,J+1).AND.LFLT(I+2,J+1).AND.
     &    LFLT(I+3,J+1).AND.LFLT(I+4,J+1).AND.LFLT(I  ,J+2).AND.
     &    LFLT(I+1,J+2).AND.LFLT(I+2,J+2).AND.LFLT(I+3,J+2).AND.
     &    LFLT(I+4,J+2).AND.LFLT(I  ,J+3).AND.LFLT(I+1,J+3).AND.
     &    LFLT(I+2,J+3).AND.LFLT(I+3,J+3).AND.LFLT(I+4,J+3).AND.
     &    LFLT(I  ,J+4).AND.LFLT(I+1,J+4).AND.LFLT(I+2,J+4).AND.
     &    LFLT(I+3,J+4).AND.LFLT(I+4,J+4)
        ENDDO
       ENDDO
      ELSE
C$OMP PARALLEL DO PRIVATE(J)
       DO J = 1,JMX-8
        DO I = 1,IMX-4
               LFLT2(I+2,J+4) = LFLT(I+2,J+4).AND.
     &    LFLT(I+2+IVE(J),J+3).AND.LFLT(I+2+IVW(J),J+3).AND.
     &    LFLT(I+2+IVE(J),J+5).AND.LFLT(I+2+IVW(J),J+5).AND.
     &    LFLT(I+1,J+6).AND.LFLT(I+1,J+2).AND.
     &    LFLT(I+3,J+6).AND.LFLT(I+3,J+2).AND.
     &    LFLT(I+1,J+4).AND.LFLT(I+3,J+4).AND.
     &    LFLT(I+2,J+2).AND.LFLT(I+2,J+6).AND.
     &    LFLT(I+2+IVE(J),J+1).AND.LFLT(I+2+IVW(J),J+1).AND.
     &    LFLT(I+1+IVW(J),J+3).AND.LFLT(I+3+IVE(J),J+3).AND.
     &    LFLT(I+1+IVW(J),J+5).AND.LFLT(I+3+IVE(J),J+5).AND.
     &    LFLT(I+2+IVE(J),J+7).AND.LFLT(I+2+IVW(J),J+7).AND.
     &    LFLT(I+2,J).AND.LFLT(I+4,J+4).AND.
     &    LFLT(I,J+4).AND.LFLT(I+2,J+8)
        ENDDO
       ENDDO
      ENDIF
C
      DO IP = 1,JPASS
C
       IF (KGDSIN(1).NE.203.AND.KGDSIN(1).NE.201) THEN

C$OMP PARALLEL DO PRIVATE(J)
         DO J = 1,JMX-4
            DO I = 1,IMX-4
              IF (LFLT2(I+2,J+2)) THEN
               Z(I+2,J+2) = 0.279372 * Z1(I+2,J+2)
     +   +0.171943*((Z1(I+1,J+2)+Z1(I+2,J+1))+(Z1(I+3,J+2)+Z1(I+2,J+3)))
     +   -0.006918*((Z1(I  ,J+2)+Z1(I+2,J  ))+(Z1(I+4,J+2)+Z1(I+2,J+4)))
     +   +0.077458*((Z1(I+1,J+1)+Z1(I+3,J+3))+(Z1(I+3,J+1)+Z1(I+1,J+3)))
     +   -0.024693*((Z1(I+1,J  )+Z1(I+3,J  ))+(Z1(I  ,J+1)+Z1(I+4,J+1))
     +             +(Z1(I  ,J+3)+Z1(I+4,J+3))+(Z1(I+1,J+4)+Z1(I+3,J+4)))
     +   -0.01294 *((Z1(I  ,J  )+Z1(I+4,J  ))+(Z1(I  ,J+4)+Z1(I+4,J+4)))
              ELSE
               Z(I+2,J+2) = Z1(I+2,J+2)
              ENDIF
            ENDDO
         ENDDO
C
         DO J= 3,JMX-2
            DO I= 3,IMX-2
               Z1(I,J) = Z(I,J)
            ENDDO
         ENDDO
C
       ELSE
C
C$OMP PARALLEL DO PRIVATE(J)
         DO J = 1,JMX-8
            DO I = 1,IMX-4
              IF (LFLT2(I+2,J+4)) THEN
               Z(I+2,J+4) = 0.279372 * Z1(I+2,J+4)
     &   +0.171943* (Z1(I+2+IVE(J),J+3)+Z1(I+2+IVW(J),J+3)+
     &               Z1(I+2+IVE(J),J+5)+Z1(I+2+IVW(J),J+5))
     &   -0.006918* (Z1(I+1,J+6)+Z1(I+1,J+2)+Z1(I+3,J+6)+Z1(I+3,J+2))
     &   +0.077458* (Z1(I+1,J+4)+Z1(I+3,J+4)+Z1(I+2,J+2)+Z1(I+2,J+6))
     &   -0.024693* (Z1(I+2+IVE(J),J+1)+Z1(I+2+IVW(J),J+1) +
     &               Z1(I+1+IVW(J),J+3)+Z1(I+3+IVE(J),J+3) +
     &               Z1(I+1+IVW(J),J+5)+Z1(I+3+IVE(J),J+5) +
     &               Z1(I+2+IVE(J),J+7)+Z1(I+2+IVW(J),J+7))
     &   -0.01294 * (Z1(I+2,J)+Z1(I+4,J+4)+Z1(I,J+4)+Z1(I+2,J+8))
              ELSE
               Z(I+2,J+4) = Z1(I+2,J+4)
              ENDIF
            ENDDO
         ENDDO
C
         DO J= 5,JMX-4
            DO I= 3,IMX-2
               Z1(I,J) = Z(I,J)
            ENDDO
         ENDDO
C
       ENDIF
      ENDDO
C
      IF (KGDSIN(1).EQ.201) THEN
       KK=0
       DO J=1,JMX
        IMAX=IMX-MOD(J,2)
        DO I=1,IMX
          IF (I.LE.IMAX) KK=KK+1
          UIN(KK)=Z1(I,J)
        ENDDO
       ENDDO
      ELSE
       DO J=1,JMX
        DO I=1,IMX
          KK=(J-1)*IMX+I
          UIN(KK)=Z1(I,J)
        ENDDO
       ENDDO
      ENDIF
C
      ENDIF
      RETURN
      END
