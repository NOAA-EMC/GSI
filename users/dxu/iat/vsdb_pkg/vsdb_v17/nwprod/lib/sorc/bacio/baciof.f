C-----------------------------------------------------------------------
      MODULE BACIO_MODULE
C$$$  F90-MODULE DOCUMENTATION BLOCK
C
C F90-MODULE: BACIO_MODULE   BYTE-ADDRESSABLE I/O MODULE
C   PRGMMR: IREDELL          ORG: NP23        DATE: 98-06-04
C
C ABSTRACT: MODULE TO SHARE FILE DESCRIPTORS
C   IN THE BYTE-ADDESSABLE I/O PACKAGE.
C
C PROGRAM HISTORY LOG:
C   98-06-04  IREDELL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      INTEGER,EXTERNAL:: BACIO,BACIOL
      INTEGER,DIMENSION(999),SAVE:: FD=999*0
      INTEGER,DIMENSION(20),SAVE:: BAOPTS=0
      INCLUDE 'baciof.h'
      END
C-----------------------------------------------------------------------
      SUBROUTINE BASETO(NOPT,VOPT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BASETO         BYTE-ADDRESSABLE SET OPTIONS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: SET OPTIONS FOR BYTE-ADDRESSABLE I/O.
C   ALL OPTIONS DEFAULT TO 0.
C   OPTION 1: BLOCKED READING OPTION
C             IF THE OPTION VALUE IS 1, THEN THE READING IS BLOCKED
C             INTO FOUR 4096-BYTE BUFFERS.  THIS MAY BE EFFICIENT IF
C             THE READS WILL BE REQUESTED IN MUCH SMALLER CHUNKS.
C             OTHERWISE, EACH CALL TO BAREAD INITIATES A PHYSICAL READ.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BASETO(NOPT,VOPT)
C   INPUT ARGUMENTS:
C     NOPT         INTEGER OPTION NUMBER
C     VOPT         INTEGER OPTION VALUE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      INTEGER NOPT,VOPT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(NOPT.GE.1.AND.NOPT.LE.20) BAOPTS(NOPT)=VOPT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPEN(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPEN         BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPEN(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      CHARACTER(80) CMSG
      integer(kind=8) IB,JB,NB,KA
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENRW,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENR(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENR        BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR READ ONLY.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENR(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      INTEGER LU,iret
      integer(kind=8) IB,JB,NB,KA
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENR,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENW(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENW        BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENW(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      integer(kind=8) IB,JB,NB,KA
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENW,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENWT(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENWT       BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY WITH TRUNCATION.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENWT(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      integer(kind=8) IB,JB,NB,KA
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENWT,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENWA(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENWA       BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY WITH APPEND.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENWA(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      integer(kind=8) IB,JB,NB,KA
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENWA,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BACLOSE(LU,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BACLOSE        BYTE-ADDRESSABLE CLOSE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: CLOSE A BYTE-ADDRESSABLE FILE.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BACLOSE(LU,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO CLOSE
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      integer(kind=8) IB,JB,NB,KA
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_CLOSE,IB,JB,1,NB,KA,FD(LU),CFN,A)
      IF(IRET.EQ.0) FD(LU)=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAREAD(LU,IB,NB,KA,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAREAD         BYTE-ADDRESSABLE READ
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: THIS BAREAD IS CALLING BAREADL TO READ A GIVEN NUMBER OF 
C   BYTES FROM AN UNBLOCKED FILE,SKIPPING A GIVEN NUMBER OF BYTES.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C   2009-04-20  J. WANG
C
C USAGE:    CALL BAREAD(LU,IB,NB,KA,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO READ
C     IB           INTEGER NUMBER OF BYTES TO SKIP
C                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
C     NB           INTEGER NUMBER OF BYTES TO READ
C   OUTPUT ARGUMENTS:
C     KA           INTEGER NUMBER OF BYTES ACTUALLY READ
C     A            CHARACTER*1 (NB) DATA READ
C
C SUBPROGRAMS CALLED:
C   BAREADL        BYTE-ADDRESSABLE READ SUBROUTINE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
!
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: LU,IB,NB
        INTEGER,INTENT(OUT) :: KA
        CHARACTER,INTENT(OUT) :: A(NB)
        INTEGER(KIND=8) :: LONG_IB,LONG_NB,LONG_KA
!
        if(IB<0 .or. NB<0 ) THEN
          print *,'WRONG: in BAFRREAD starting postion IB or read '//    &
     &   'data size NB < 0, STOP! '//                                    &
     &   'Consider using bafreadl and long integer'
          KA=0
          return
        ENDIF
        LONG_IB=IB
        LONG_NB=NB
        CALL BAREADL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA

      END SUBROUTINE BAREAD
C-----------------------------------------------------------------------
      SUBROUTINE BAREADL(LU,IB,NB,KA,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAREAD         BYTE-ADDRESSABLE READ
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: THIS SUBROUYTINE IS USING UPDATED BACIOL I/O PACKAGE TO READ 
C   A GIVEN NUMBER OF BYTES FROM AN UNBLOCKED FILE, SKIPPING A GIVEN 
C   NUMBER OF BYTES. 
C   THE PHYSICAL I/O IS BLOCKED INTO FOUR 4096-BYTE BUFFERS
C   IF THE BYTE-ADDRESSABLE OPTION 1 HAS BEEN SET TO 1 BY BASETO.
C   THIS BUFFERED READING IS INCOMPATIBLE WITH NO-SEEK READING.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C   2009-04-20  J. WANG
C
C USAGE:    CALL BAREAD(LU,IB,NB,KA,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO READ
C     IB           INTEGER(8) NUMBER OF BYTES TO SKIP
C                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
C     NB           INTEGER(8) NUMBER OF BYTES TO READ
C   OUTPUT ARGUMENTS:
C     KA           INTEGER(8) NUMBER OF BYTES ACTUALLY READ
C     A            CHARACTER*1 (NB) DATA READ
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIOL         BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
!    
      IMPLICIT NONE
      INTEGER,intent(in)          :: LU
      INTEGER(kind=8),intent(in)  :: IB,NB
      INTEGER(kind=8),intent(out) :: KA
      CHARACTER,intent(out)       :: A(NB)
      CHARACTER CFN
      integer(kind=8),PARAMETER :: NY=4096,MY=4
      INTEGER(KIND=8) NS(MY),NN(MY)
      INTEGER(kind=8) JB,LONG_0,KY,I,K,IY,JY,LUX
      INTEGER IRET
!      INTEGER LU,IB,NB,KA
      CHARACTER Y(NY,MY)
      DATA LUX/0/
      SAVE JY,NS,NN,Y,LUX
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.0) THEN
        KA=0
        RETURN
      ENDIF
      IF(IB.LT.0.AND.BAOPTS(1).EQ.1) THEN
        KA=0
        RETURN
      ENDIF
      IF(NB.LE.0) THEN
        KA=0
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LONG_0=0                                                         
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  UNBUFFERED I/O
      IF(BAOPTS(1).NE.1) THEN
        KA=0
        IF(IB.GE.0) THEN
          IRET=BACIOL(BACIO_READ,IB,JB,1,NB,KA,FD(LU),CFN,A)
        ELSE
          IRET=BACIOL(BACIO_READ+BACIO_NOSEEK,LONG_0,JB,1,NB,KA,        &
     &                FD(LU),CFN,A)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  BUFFERED I/O
C  GET DATA FROM PREVIOUS CALL IF POSSIBLE
      ELSE
        KA=0
        IF(LUX.NE.LU) THEN
          JY=0
          NS=0
          NN=0
        ELSE
          DO I=1,MY
            IY=MOD(JY+I-1,MY)+1
            KY=IB+KA-NS(IY)
            IF(KA.LT.NB.AND.KY.GE.LONG_0.AND.KY.LT.NN(IY)) THEN
              K=MIN(NB-KA,NN(IY)-KY)
              A(KA+1:KA+K)=Y(KY+1:KY+K,IY)
              KA=KA+K
            ENDIF
          ENDDO
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET POSITION AND READ BUFFER AND GET DATA
        IF(KA.LT.NB) THEN
          LUX=ABS(LU)
          JY=MOD(JY,MY)+1
          NS(JY)=IB+KA
          IRET=BACIOL(BACIO_READ,NS(JY),JB,1,NY,NN(JY),
     &               FD(LUX),CFN,Y(1,JY))
          IF(NN(JY).GT.0) THEN
            K=MIN(NB-KA,NN(JY))
            A(KA+1:KA+K)=Y(1:K,JY)
            KA=KA+K
          ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CONTINUE TO READ BUFFER AND GET DATA
          DOWHILE(NN(JY).EQ.NY.AND.KA.LT.NB)
            JY=MOD(JY,MY)+1
            NS(JY)=NS(JY)+NN(JY)
            IRET=BACIOL(BACIO_READ+BACIO_NOSEEK,NS(JY),JB,1,NY,NN(JY),
     &                 FD(LUX),CFN,Y(1,JY))
            IF(NN(JY).GT.0) THEN
              K=MIN(NB-KA,NN(JY))
              A(KA+1:KA+K)=Y(1:K,JY)
              KA=KA+K
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE BAREADL
C-----------------------------------------------------------------------
      SUBROUTINE BAWRITE(LU,IB,NB,KA,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAWRITE        BYTE-ADDRESSABLE WRITE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: THIS PROGRAM IS CALLING BAWRITEL TO WRITE A GIVEN NUMBER OF 
C   BYTES TO AN UNBLOCKED FILE,SKIPPING A GIVEN NUMBER OF BYTES.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C   2009-04-20  J. WANG
C
C USAGE:    CALL BAWRITE(LU,IB,NB,KA,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO WRITE
C     IB           INTEGER NUMBER OF BYTES TO SKIP
C                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
C     NB           INTEGER NUMBER OF BYTES TO WRITE
C     A            CHARACTER*1 (NB) DATA TO WRITE
C   OUTPUT ARGUMENTS:
C     KA           INTEGER NUMBER OF BYTES ACTUALLY WRITTEN
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
!
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: LU,IB,NB
        INTEGER,INTENT(OUT) :: KA
        CHARACTER,INTENT(IN) :: A(NB)
        INTEGER(KIND=8) :: LONG_IB,LONG_NB,LONG_KA
!
        if(IB<0 .or. NB<0 ) THEN
          print *,'WRONG: in BAFRWRITEstarting postion IB or read '//    &
     &   'data size NB <0, STOP! ' //                                    &
     &   'Consider using bafrrwritel and long integer'
          KA=0
          return
        ENDIF
!
        LONG_IB=IB
        LONG_NB=NB
        CALL BAWRITEL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA         

      END SUBROUTINE BAWRITE
C-----------------------------------------------------------------------
      SUBROUTINE BAWRITEL(LU,IB,NB,KA,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAWRITEL       BYTE-ADDRESSABLE WRITE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: THIS SUBROUYTINE IS USING UPDATED BACIOL I/O PACKAGE TO WRITE
C   A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE, SKIPPING A GIVEN NUMBER
C   OF BYTES.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C   2009-04-20  J. WANG
C
C USAGE:    CALL BAWRITEL(LU,IB,NB,KA,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO WRITE
C     IB           INTEGER(8) NUMBER OF BYTES TO SKIP
C                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
C     NB           INTEGER(8) NUMBER OF BYTES TO WRITE
C     A            CHARACTER*1 (NB) DATA TO WRITE
C   OUTPUT ARGUMENTS:
C     KA           INTEGER(8) NUMBER OF BYTES ACTUALLY WRITTEN
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIOL         BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
!
      IMPLICIT NONE
!
      INTEGER,intent(in)         :: LU
      INTEGER(kind=8),intent(in) :: IB,NB
      INTEGER(kind=8),intent(out):: KA
      CHARACTER,intent(in) ::  A(NB)
!
      CHARACTER CFN
      INTEGER(kind=8) :: JB,LONG_0
      INTEGER :: IRET
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.0) THEN
        KA=0
        RETURN
      ENDIF
      IF(NB.LE.0) THEN
        KA=0
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LONG_0=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IB.GE.0) THEN
        KA=0
        IRET=BACIOL(BACIO_WRITE,IB,JB,1,NB,KA,FD(LU),CFN,A)
      ELSE
        KA=0
        IRET=BACIOL(BACIO_WRITE+BACIO_NOSEEK,LONG_0,JB,1,NB,KA,         &
     &              FD(LU),CFN,A)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE  BAWRITEL
C-----------------------------------------------------------------------
      SUBROUTINE WRYTE(LU,NB,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: WRYTE          WRITE DATA OUT BY BYTES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: THSI SUBROUTINE IS CALLING WRYTEL TO WRITE A GIVEN NUMBER OF 
C   BYTES TO AN UNBLOCKED FILE.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   95-10-31  IREDELL     WORKSTATION VERSION
C   1998-06-04  IREDELL   BACIO VERSION
C   2009-04-20  J. WANG  WRYTEL VERSION
C
C USAGE:    CALL WRYTE(LU,NB,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO WHICH TO WRITE
C     NB           INTEGER(4) NUMBER OF BYTES TO WRITE
C     A            CHARACTER*1 (NB) DATA TO WRITE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
!
      USE BACIO_MODULE
!
      IMPLICIT NONE
!
      INTEGER,intent(in) :: LU
      INTEGER,intent(in) :: NB
      CHARACTER,intent(in) ::  A(NB)
      INTEGER(kind=8) :: LONG_NB
!
      IF(NB<0) THEN
       PRINT *,'WRONG: NB: the number of bytes to write  <0, STOP!'
       RETURN
      ENDIF
      LONG_NB=NB
      CALL WRYTEL(LU,LONG_NB,A)
!
      END SUBROUTINE WRYTE
C-----------------------------------------------------------------------
      SUBROUTINE WRYTEL(LU,NB,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: WRYTE          WRITE DATA OUT BY BYTES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: WRITE A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   95-10-31  IREDELL     WORKSTATION VERSION
C   1998-06-04  IREDELL   BACIO VERSION
C   2009-04-20  J. WANG   BACIOL VERSION
C
C USAGE:    CALL WRYTE(LU,NB,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO WHICH TO WRITE
C     NB           INTEGER(8) NUMBER OF BYTES TO WRITE
C     A            CHARACTER*1 (NB) DATA TO WRITE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIOL         BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
!
      IMPLICIT NONE
      INTEGER,intent(in) :: LU
      INTEGER(kind=8),intent(in) :: NB
      CHARACTER,INTENT(in)       :: A(NB)
      INTEGER(kind=8) :: LONG_0,JB,KA
      INTEGER :: IRET
      CHARACTER CFN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.0) THEN
        RETURN
      ENDIF
      IF(NB.LE.0) THEN
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LONG_0=0
      KA=0
      JB=0
      IRET=BACIOL(BACIO_WRITE+BACIO_NOSEEK,LONG_0,JB,1,NB,KA,           &
     &            FD(LU),CFN,A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
