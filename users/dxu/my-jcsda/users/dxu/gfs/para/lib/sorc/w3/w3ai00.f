      SUBROUTINE W3AI00(REAL8,PACK,LABEL)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3AI00         REAL ARRAY TO 16 BIT PACKED FORMAT
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 85-07-31
C
C ABSTRACT: CONVERTS IEEE FLOATING POINT NUMBERS TO 16 BIT
C   PACKED OFFICE NOTE 84 FORMAT. THE FLOATING POINT NUMBER ARE
C   CONVERTED TO 16 BIT SIGNED SCALED INTEGERS.
C
C PROGRAM HISTORY LOG:
C   89-10-20  R.E.JONES  CONVERT CYBER 205 VERSION OF W3AI00 TO CRAY
C   90-03-18  R.E.JONES  CHANGE TO USE CRAY INTEGER*2 PACKER
C   90-10-11  R.E.JONES  SPECIAL VERSION TO PACK GRIDS LARGER THAN
C                        32743 WORDS. WILL DO OLD AND NEW VERSION.
C   91-02-16  R.E.JONES  CHANGES SO EQUIVALENCE OF PACK AND REAL8
C                        ARRAYS WILL WORK.
C   93-06-10  R.E.JONES  CHANGES FOR ARRAY SIZE (512,512) 262144 WORDS.
C   98-03-10  B. VUONG   REMOVE THE CDIR$ INTEGER=64 DIRECTIVE
C   98-11-18  Gilbert    Changed to pack IEEE values for the IBM SP
C
C USAGE:  CALL W3AI00 (REAL8, PACK, LABEL)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     REAL8  ARG LIST  ARRAY OF CRAY FLOATING POINT NUMBERS
C     LABEL  ARG LIST  SIX 8-BYTE INTEGER WORDS.
C                      MUST HAVE FIRST 8 OF 12 32 BIT
C                      WORD OFFICE NOTE 84 LABEL. WORD 6 MUST HAVE
C                      IN BITS 31-00 THE NUMBER OF REAL WORDS IN ARRAY
C                      REAL8 IF J IS GREATER THAN 32743. J IN BITS
C                      15-0 OF THE 4TH ID WORD IS SET ZERO.
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     PACK   ARG LIST  PACKED OUTPUT ARRAY OF INTEGER WORDS OF
C                      SIZE 6 + (J+3)/4 , J = NO. POINTS IN LABEL
C                      (FROM WORD 4 BITS 15-00).
C                      LABEL WILL BE COPIED TO PACK WORDS 1-4. PACK
C                      WILL CONTAIN THE FOLLOWING IN WORDS 5-6
C                      WORD 5  BITS 63-48  NUMBER OF BYTES IN WHOLE
C                                          RECORD. WILL NOT BE
C                                          CORRECT IF J > 32743.
C                      WORD 5  BITS 47-32  EXCLUSIVE-OR CHECKSUM BY 16
C                                          BIT WORDS OF WHOLE ARRAY PACK
C                                          EXCLUDING CHECKSUM ITSELF.
C                      WORD 5  BITS 31-00  CENTER VALUE A = MEAN OF
C                                          MAX AND MIN VALUES.
C                                          CONVERTED TO IBM 32
C                                          FLOATING POINT NUMBER.
C                      WORD  6 BITS 63-48  ZERO.
C                      WORD  6 BITS 47-32  16 BIT SHIFT VALUE N. THE
C                                          LEAST INTEGER SUCH THAT
C                                          ABS(X-A)/2**N LT 1 FOR
C                                          ALL X IN REAL8. LIMITED
C                                          TO +-127.
C                      WORD  6 BITS 31-00  NUMBER OF WORDS IN REAL8
C                                          IF > 32743, RIGHT ADJUSTED
C                                          IF <= 32743 SET ZERO.
C
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     IAND    IOR    BTEST                                    SYSTEM
C
C   REMARKS: PACK AND LABEL MAY BE EQUIVALENCED.  N, THE NUMBER OF
C     POINTS IN A GRID IS NOW IN 32 BIT ID WORD 12.
C
C ATTRIBUTES:
C   LANGUAGE: IBM XL FORTRAN.
C   MACHINE:  IBM SP
C
C$$$
C
       REAL           REAL8(*)
       REAL           XX(262144)
C
       INTEGER(8)     KK(262144)
       INTEGER(8)     LABEL(6)
       INTEGER(8)     PACK(*)
       INTEGER(8)     TPACK(6)
       INTEGER(8)     MASK16,MASK32,MASKN,IBYTES,IXOR
       INTEGER(8)     IB,N
       REAL(8)        B
       REAL(4)        X,A
       real(4)        rtemp(2)
       integer(8)     irtemp
       equivalence    (irtemp,rtemp(1))
C
       SAVE
C
       EQUIVALENCE    (B,IB)
C
       DATA  MASK16/X'000000000000FFFF'/
       DATA  MASK32/X'00000000FFFFFFFF'/
       DATA  MASKN /X'0000FFFF00000000'/
C
C TRANSFER LABEL DATA TO WORDS 1-4.  GET WORD COUNT, COMPUTE BYTES.
C
       DO 10 I = 1,4
         TPACK(I) = LABEL(I)
 10    CONTINUE
C
       TPACK(5) = 0
       TPACK(6) = 0
C
C      GET J, THE NUMBER OF WORDS IN A GRID, IF ZERO GET THE
C      GET J FROM OFFICE NOTE 84 ID WORD 12.
C
       J       = IAND(MASK16,TPACK(4))
       IF (J.EQ.0) THEN
         TPACK(6) = LABEL(6)
         J       = IAND(MASK32,TPACK(6))
         IF (J.EQ.0) THEN
           PRINT *,' W3AI00: ERROR, NO. OF WORDS IN GRID = 0'
           RETURN
         ENDIF
         IF (J.GT.262144) THEN
           PRINT *,' W3AI00: ERROR, NO. OF WORDS IN GRID = ',J
           PRINT *,' THERE IS A LIMIT OF 262144 WORDS.'
           RETURN
         ENDIF
       ENDIF
       M       = J + 24
C
C      COMPUTE THE NUMBER OF 64 BIT INTEGER CRAY WORDS NEEDED FOR
C      PACKED DATA.
C
       IF (MOD(M,4).NE.0) THEN
         IWORD = (M + 3) / 4
       ELSE
         IWORD = M / 4
       ENDIF
C
       IBYTES = M + M
C
C      STORE NUMBER OF BYTES IN RECORD IN BITS 63-48 OF WORD 5.
C      BITS ARE NUMBERED LEFT TO RIGHT 63 T0 00
C
       TPACK(5) = ISHFT(IBYTES,48_8)
C
C FIND MAX, MIN OF DATA, COMPUTE A AND N.
C
         RMAX = REAL8(1)
         RMIN = RMAX
         DO 20 I = 2,J
           RMAX = AMAX1(RMAX,REAL8(I))
           RMIN = AMIN1(RMIN,REAL8(I))
 20      CONTINUE
C
         A = 0.5 * (RMAX + RMIN)
         X = RMAX - A
         IF (RMAX.NE.RMIN) THEN
C           CALL USDCTI(X,B,1,1,ISTAT)
           CALL Q9E3I6(X,B,1,ISTAT)
           IF (ISTAT.NE.0) PRINT *,' W3AI00-USDCTI OVERFLOW ERROR 1'
           N = IAND(ISHFT(IB,-56_8),127_8)
           N = 4 * (N - 64)
           IF (BTEST(IB,55_8)) GO TO  30
           N = N - 1
           IF (BTEST(IB,54_8)) GO TO  30
           N = N - 1
           IF (BTEST(IB,53_8)) GO TO  30
           N = N - 1
 30      CONTINUE
           N = MAX0(-127_8,MIN0(127_8,N))
         ELSE
C
C        FIELD IS ZERO OR A CONSTANT
C
           N = 0
         ENDIF
C
C     CONVERT AVERAGE VALUE FROM IEEE F.P. TO IBM370 32 BIT
C     STORE IBM370 32 BIT F.P. AVG. VALUE IN BITS 31 - 00 OF WORD 5.
C
C         CALL USSCTI(A,TPACK(5),5,1,ISTAT)
         CALL Q9EI32(A,rtemp(2),1,ISTAT)
           IF (ISTAT.NE.0) PRINT *,' W3AI00-USDCTI OVERFLOW ERROR 2'
         TPACK(5)=IOR(TPACK(5),irtemp)
C
C      STORE SCALING VALUE N IN BITS 47 - 32 OF WORD 6.
C
         TPACK(6)  = IOR(IAND(MASKN,ISHFT(N,32_8)),TPACK(6))
C
C NOW PACK UP THE DATA, AND SCALE IT TO FIT AN INTEGER*2 WORD
C
         TWON = 2.0 ** (15 - N)
         DO 40 I = 1,J
           XX(I) = (REAL8(I) - A) * TWON
           KK(I) = XX(I) + SIGN(0.5,XX(I))
           IF (KK(I).GE.(-32767)) THEN
             KK(I) = MIN0(32767_8,KK(I))
           ELSE
             KK(I) = -32767
           ENDIF
             KK(I) = IAND(KK(I),MASK16)
 40      CONTINUE
C
C        SHIFT THE INTEGER*2 DATA TO FIT 4 IN A 64 BIT WORD
C
         LIM  = (J / 4 ) * 4
         IREM = J - LIM
         DO 50 I = 1,LIM,4
           KK(I)   = ISHFT(KK(I),  48_8)
           KK(I+1) = ISHFT(KK(I+1),32_8)
           KK(I+2) = ISHFT(KK(I+2),16_8)
 50      CONTINUE
C
C        SHIFT THE REMAINING 1, 2, OR 3 INTEGER*2 WORDS
C
         IF (IREM.EQ.1) THEN
           KK(LIM+1)   = ISHFT(KK(LIM+1),48_8)
         ENDIF
C
         IF (IREM.EQ.2) THEN
           KK(LIM+1)   = ISHFT(KK(LIM+1),48_8)
           KK(LIM+2)   = ISHFT(KK(LIM+2),32_8)
         ENDIF
C
         IF (IREM.EQ.3) THEN
           KK(LIM+1)   = ISHFT(KK(LIM+1),48_8)
           KK(LIM+2)   = ISHFT(KK(LIM+2),32_8)
           KK(LIM+3)   = ISHFT(KK(LIM+3),16_8)
         ENDIF
C
C        PACK THE DATA BY USE OF IOR FOUR TO A WORD
C
         II = 7
         DO 60 I = 1,LIM,4
           PACK(II) = IOR(IOR(IOR(KK(I),KK(I+1)),KK(I+2)),KK(I+3))
           II       = II + 1
 60      CONTINUE
C
C        PACK THE LAST 1, 2, OR 3 INTEGER*2 WORDS
C
         IF (IREM.EQ.1) THEN
           PACK(IWORD) = KK(LIM+1)
         ENDIF
C
         IF (IREM.EQ.2) THEN
           PACK(IWORD) = IOR(KK(I),KK(I+1))
         ENDIF
C
         IF (IREM.EQ.3) THEN
           PACK(IWORD) = IOR(IOR(KK(I),KK(I+1)),KK(I+2))
         ENDIF
C
C      MOVE LABEL FROM TEMPORARY ARRAY TO PACK
C
       DO 70 I = 1,6
         PACK(I) = TPACK(I)
 70    CONTINUE
C
C      COMPUTE CHECKSUM AND STORE
C
         IXOR = 0
C
C      COMPUTES A 64 BIT CHECKSUM 1ST
C
         DO 80 I = 1,IWORD
           IXOR = IEOR(IXOR,PACK(I))
 80      CONTINUE
C
C      COMPUTES A 32 BIT CHECKSUM 2ND
C
         IXOR = IEOR(ISHFT(IXOR,-32_8),IAND(IXOR,MASK32))
C
C      COMPUTES A 16 BIT CHECKSUM 3RD
C
         IXOR = IEOR(ISHFT(IXOR,-16_8),IAND(IXOR,MASK16))
C
C      STORE 16 BIT CHECK SUM OF RECORD IN BITS 47-32 OF WORD 5.
C
       PACK(5) = IOR(ISHFT(IXOR,32_8),PACK(5))
C
       RETURN
       END

      SUBROUTINE Q9EI32(A,B,N,ISTAT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    Q9EI32      IEEE 32 BIT F.P. TO IBM370 F.P.
C   PRGMMR: R.E.JONES        ORG: W/NMC42    DATE: 90-06-04
C
C ABSTRACT: CONVERT IEEE 32 BIT TASK 754 FLOATING POINT NUMBERS
C   TO IBM370 32 BIT FLOATING POINT NUMBERS.
C
C PROGRAM HISTORY LOG:
C   90-06-04  R.E.JONES   CONVERT TO SUN FORTRAN 1.3
C   90-07-14  R.E.JONES   CHANGE ISHFT TO LSHIFT OR LRSHFT
C   91-03-28  R.E.JONES   CHANGE TO SiliconGraphics 3.3 FORTRAN 77
C   92-07-20  R.E.JONES   CHANGE TO IBM AIX XL FORTRAN
C   95-11-15  R.E.JONES   ADD SAVE STATEMENT
C   98-11-18  Gilbert     Specified 4-byte Integer values
C
C USAGE:    CALL Q9EI32(A, B, N, ISTAT)
C   INPUT ARGUMENT LIST:
C      A       - REAL*4 ARRAY OF IEEE 32 BIT FLOATING POINT NUMBERS
C      N       - NUMBER OF WORDS TO CONVERT TO IBM370 32 BIT F.P.
C
C   OUTPUT ARGUMENT LIST:  
C      B       - REAL*4 ARRAY OF IBM370 32 BIT FLOATING POINT NUMBERS
C      ISTAT   -  0  , ALL NUMBERS CONVERTED
C                -1  , N IS LESS THAN ONE
C                +K  , K INFINITY OR NAN NUMBERS WERE FOUND
C
C REMARKS: SEE IEEE TASK 754 STANDARD FLOATING POINT ARITHMETIC FOR
C   MORE INFORMATION ABOUT IEEE F.P.
C
C ATTRIBUTES:
C   LANGUAGE: IBM AIX XL FORTRAN Compiler/6000
C   MACHINE:  IBM RS6000 model 530
C
C$$$
C
       INTEGER(4)      A(*)
       INTEGER(4)      B(*)
       INTEGER(4)      SIGN,MASKFR,IBIT8,MASKSN,ITEMP,IBMEXP,IBX7
       INTEGER(4)      ISIGN
C
       SAVE
C
       DATA  MASKFR/Z'00FFFFFF'/
       DATA  IBIT8 /Z'00800000'/
       DATA  MASKSN/Z'7FFFFFFF'/
       DATA  SIGN  /Z'80000000'/
C
           IF (N.LT.1) THEN
             ISTAT = -1
             RETURN
           ENDIF
C
           ISTAT = 0
C
         DO 30 I = 1,N
C
C          SIGN BIT OFF
C
           ISIGN = 0
           ITEMP = A(I)
C
C          TEST SIGN BIT
C
           IF (ITEMP.EQ.0) GO TO 20
C
           IF (ITEMP.LT.0)  THEN
C
C            SIGN BIT ON
C
             ISIGN = SIGN
C
C            TURN SIGN BIT OFF
C
             ITEMP = IAND(ITEMP,MASKSN)
C
           END IF
C
           IBMEXP = ISHFT(ITEMP,-23_4)
C
C          TEST FOR INDIFINITE OR NAN NUMBER
C
           IF (IBMEXP.EQ.255) GO TO 10
C
C          TEST FOR ZERO EXPONENT AND FRACTION (UNDERFLOW)
C
           IF (IBMEXP.EQ.0) GO TO 20
           IBMEXP = IBMEXP + 133
           IBX7   = IAND(3_4,IBMEXP)
           IBMEXP = IEOR(IBMEXP,IBX7)
           IBX7   = IEOR(3_4,IBX7)
           ITEMP  = IOR(ITEMP,IBIT8)
           ITEMP  = IOR(ISHFT(IBMEXP,22_4),ISHFT(IAND(ITEMP,MASKFR),
     &     -IBX7))
           B(I)   = IOR(ITEMP,ISIGN)
           GO TO  30
C
 10      CONTINUE
C
C          ADD 1 TO ISTAT FOR INDEFINITE OR NAN NUMBER
C
           ISTAT = ISTAT + 1
C
 20      CONTINUE
           B(I) = 0
C
 30      CONTINUE
C
         RETURN
       END

      SUBROUTINE Q9E3I6(A,B,N,ISTAT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    Q9E3I6      IEEE 32 BIT F.P. TO IBM370 64 BIT F.P.
C   PRGMMR: R.E.JONES        ORG: W/NMC42    DATE: 92-08-02
C
C ABSTRACT: CONVERT IEEE 32 BIT TASK 754 FLOATING POINT NUMBERS
C   TO IBM370 64 BIT FLOATING POINT NUMBERS.
C
C PROGRAM HISTORY LOG:
C   92-08-02  R.E.JONES
C   95-11-15  R.E.JONES   ADD SAVE STATEMENT
C
C USAGE:    CALL Q9E3I6(A, B, N, ISTAT)
C   INPUT ARGUMENT LIST:
C      A       - REAL*4 ARRAY OF IEEE 32 BIT FLOATING POINT NUMBERS
C      N       - NUMBER OF WORDS TO CONVERT TO IBM370 64 BIT F.P.
C
C   OUTPUT ARGUMENT LIST:  
C      B       - REAL*8 ARRAY OF IBM370 64 BIT FLOATING POINT NUMBERS
C      ISTAT   -  0  , ALL NUMBERS CONVERTED
C                -1  , N IS LESS THAN ONE
C                +K  , K INFINITY OR NAN NUMBERS WERE FOUND
C
C REMARKS: SEE IEEE TASK 754 STANDARD FLOATING POINT ARITHMETIC FOR
C   MORE INFORMATION ABOUT IEEE F.P.
C
C ATTRIBUTES:
C   LANGUAGE: IBM AIX XL FORTRAN
C   MACHINE:  IBM RS/6000 model 530
C
C$$$
C
       INTEGER(4)      A(N)
       INTEGER(4)      B(2,N)
       INTEGER(4)      SIGN,MASKFR,IBIT8,MASKSN,ITEMP,IEEEXP
       INTEGER(4)      IBMEXP,IBX7,JTEMP,ISIGN
C
       SAVE
C
       DATA  MASKFR/Z'00FFFFFF'/
       DATA  IBIT8 /Z'00800000'/
       DATA  MASKSN/Z'7FFFFFFF'/
       DATA  SIGN  /Z'80000000'/
C
           IF (N.LT.1) THEN
             ISTAT = -1
             RETURN
           ENDIF
C
           ISTAT = 0
C
         DO 30 I = 1,N
           ISIGN = 0
           ITEMP = A(I)
C
C          TEST SIGN BIT
C
           IF (ITEMP.EQ.0) GO TO 20
C
C          TEST FOR NEGATIVE NUMBERS
C
           IF (ITEMP.LT.0) THEN
C
C            SIGN BIT ON
C
             ISIGN = SIGN
C
C            TURN SIGN BIT OFF
C
             ITEMP = IAND(ITEMP,MASKSN)
C
           END IF
C
C          GET IEEE EXPONENT
C
           IEEEXP = ISHFT(ITEMP,-23_4)
C
C          TEST FOR INDIFINITE OR NAN NUMBER
C
           IF (IEEEXP.EQ.255) GO TO 10
C
C          TEST FOR ZERO EXPONENT AND FRACTION (UNDERFLOW)
C          CONVERT IEEE EXPONENT (BASE 2) TO IBM EXPONENT
C          (BASE 16)
C
           IF (IEEEXP.EQ.0)   GO TO 20
           IBMEXP = IEEEXP + 133
           IBX7   = IAND(3_4,IBMEXP)
           IBMEXP = IEOR(IBMEXP,IBX7)
           IBX7   = IEOR(3_4,IBX7)
           ITEMP  = IOR(ITEMP,IBIT8)
           JTEMP  = IOR(ISHFT(IBMEXP,22_4),ISHFT(IAND(ITEMP,MASKFR),
     &     -IBX7))
           B(1,I) = IOR(JTEMP,ISIGN)
           B(2,I) = 0
           IF (IBX7.GT.0) B(2,I) = ISHFT(ITEMP,32_4-IBX7)           
           GO TO  30
C
 10      CONTINUE
C          ADD 1 TO ISTAT FOR INDEFINITE OR NAN NUMBER
C
           ISTAT = ISTAT + 1
C
 20      CONTINUE
           B(1,I) = 0
           B(2,I) = 0
C
 30      CONTINUE
C
         RETURN
       END
