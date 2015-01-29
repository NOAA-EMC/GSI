       SUBROUTINE W3FT12(COEF,WORK,GRID,TRIGS)                          00010000
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK                                 00020000
C                                                                       00030000
C SUBPROGRAM: W3FT12         FAST FOURIER FOR 2.5 DEGREE GRID           00040000
C   AUTHOR: SELA,JOE         ORG: W323       DATE: 80-11-21             00050000
C                                                                       00060000
C ABSTRACT: FAST FOURIER TO COMPUTE 145 GRID VALUES AT DESIRED          00070000
C   LATITUDE FROM 31 COMPLEX FOURIER COEFFICIENTS. THIS SUBROUTINE      00080000
C   IS SPECIAL PURPOSE FOR CONVERTING COEFFICIENTS TO A 2.5 DEGREE      00090000
C   LAT,LON GRID.                                                       00100000
C                                                                       00110000
C PROGRAM HISTORY LOG:                                                  00120000
C   80-11-21  JOE SELA                                                  00130000
C   84-06-21  R.E.JONES   CHANGE TO IBM VS FORTRAN                      00140000
C   93-04-12  R.E.JONES   CHANGE TO CRAY CFT77 FORTRAN                  00150000
C                                                                       00160000
C USAGE:  CALL W3FT12(COEF,WORK,GRID,TRIGS)                             00170000
C                                                                       00180000
C   INPUT VARIABLES:                                                    00190000
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES               00200000
C     ------ --------- -----------------------------------------------  00210000
C     COEF   ARG LIST  31 COMPLEX FOURIER COEFFICIENTS.                 00220000
C     TRIGS  ARG LIST  216 TRIG FUNCTIONS ASSUMED PRECOMPUTED           00230000
C                      BY W3FA13 BEFORE FIRST CALL TO W3FT12.           00240000
C     WORK   ARG LIST  144 REAL WORK SPACE                              00250000
C                                                                       00260000
C   OUTPUT VARIABLES:                                                   00270000
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES               00280000
C     ------ --------- -----------------------------------------------  00290000
C     GRID   ARG LIST  145 GRID VALUES, GRID(1)=GRID(145)               00300000
C                                                                       00310000
C ATTRIBUTES:                                                           00320000
C   LANGUAGE: CRAY CFT77 FORTRAN 77                                     00330000
C   MACHINE:  CRAY C916-128, Y-MP8/864, Y-MP EL92/256                   00340002
C                                                                       00350000
C$$$                                                                    00360000
C                                                                       00370000
       REAL            COEF( 62 )                                       00380000
       REAL            GRID(145)                                        00390000
       REAL            TRIGS(216)                                       00400000
       REAL            WORK(144)                                        00410000
C                                                                       00411003
       SAVE                                                             00412003
C                                                                       00420000
       DATA  SIN60/0.866025403784437/                                   00430000
C                                                                       00440000
      GRID(1) = COEF(1)                                                 00450000
      GRID(2) = COEF(1)                                                 00460000
      K = 147                                                           00470000
      J = 143                                                           00480000
      DO 100 I=3, 61 ,2                                                 00490000
      TEMP = COEF(I)*TRIGS(K+1) - COEF(I+1)*TRIGS(K)                    00500000
      GRID(I) = COEF(I) - TEMP                                          00510000
      GRID(J) = COEF(I) + TEMP                                          00520000
      TEMP = COEF(I)*TRIGS(K) + COEF(I+1)*TRIGS(K+1)                    00530000
      GRID(I+1) = TEMP - COEF(I+1)                                      00540000
      GRID(J+1) = TEMP + COEF(I+1)                                      00550000
      K = K + 2                                                         00560000
      J = J - 2                                                         00570000
100   CONTINUE                                                          00580000
      DO 110 I= 63 , 84                                                 00590000
      GRID(I) = 0.0                                                     00600000
110   CONTINUE                                                          00610000
C                                                                       00620000
      A0 = GRID(1) + GRID(73)                                           00630000
      A2 = GRID(1) - GRID(73)                                           00640000
      B0 = GRID(2) + GRID(74)                                           00650000
      B2 = GRID(2) - GRID(74)                                           00660000
      A1 = GRID(37) + GRID(109)                                         00670000
      A3 = GRID(37) - GRID(109)                                         00680000
      B1 = GRID(38) + GRID(110)                                         00690000
      B3 = GRID(38) - GRID(110)                                         00700000
      WORK(1) = A0 + A1                                                 00710000
      WORK(5) = A0 - A1                                                 00720000
      WORK(2) = B0 + B1                                                 00730000
      WORK(6) = B0 - B1                                                 00740000
      WORK(3) = A2 - B3                                                 00750000
      WORK(7) = A2 + B3                                                 00760000
      WORK(4) = B2 + A3                                                 00770000
      WORK(8) = B2 - A3                                                 00780000
      KB = 3                                                            00790000
      KC = 5                                                            00800000
      KD = 7                                                            00810000
      J = 75                                                            00820000
      K = 39                                                            00830000
      L = 111                                                           00840000
      M = 9                                                             00850000
      DO 300 I=3,35,2                                                   00860000
      A0 = GRID(I) + GRID(J)                                            00870000
      A2 = GRID(I) - GRID(J)                                            00880000
      B0 = GRID(I+1) + GRID(J+1)                                        00890000
      B2 = GRID(I+1) - GRID(J+1)                                        00900000
      A1 = GRID(K) + GRID(L)                                            00910000
      A3 = GRID(K) - GRID(L)                                            00920000
      B1 = GRID(K+1) + GRID(L+1)                                        00930000
      B3 = GRID(K+1) - GRID(L+1)                                        00940000
      WORK(M  ) = A0 + A1                                               00950000
      WORK(M+4) = A0 - A1                                               00960000
      WORK(M+1) = B0 + B1                                               00970000
      WORK(M+5) = B0 - B1                                               00980000
      WORK(M+2) = A2 - B3                                               00990000
      WORK(M+6) = A2 + B3                                               01000000
      WORK(M+3) = B2 + A3                                               01010000
      WORK(M+7) = B2 - A3                                               01020000
      TEMP = WORK(M+2)*TRIGS(KB) - WORK(M+3)*TRIGS(KB+1)                01030000
      WORK(M+3) = WORK(M+2)*TRIGS(KB+1) + WORK(M+3)*TRIGS(KB)           01040000
      WORK(M+2) = TEMP                                                  01050000
      TEMP = WORK(M+4)*TRIGS(KC) - WORK(M+5)*TRIGS(KC+1)                01060000
      WORK(M+5) = WORK(M+4)*TRIGS(KC+1) + WORK(M+5)*TRIGS(KC)           01070000
      WORK(M+4) = TEMP                                                  01080000
      TEMP = WORK(M+6)*TRIGS(KD) - WORK(M+7)*TRIGS(KD+1)                01090000
      WORK(M+7) = WORK(M+6)*TRIGS(KD+1) + WORK(M+7)*TRIGS(KD)           01100000
      WORK(M+6) = TEMP                                                  01110000
      J = J + 2                                                         01120000
      K = K + 2                                                         01130000
      L = L + 2                                                         01140000
      KB = KB + 2                                                       01150000
      KC = KC + 4                                                       01160000
      KD = KD + 6                                                       01170000
      M = M + 8                                                         01180000
300   CONTINUE                                                          01190000
C                                                                       01200000
      I = 1                                                             01210000
      J = 1                                                             01220000
      K = 73                                                            01230000
      DO 440 L=1,4                                                      01240000
      GRID(I) = WORK(J) + WORK(K)                                       01250000
      GRID(I+8) = WORK(J) - WORK(K)                                     01260000
      GRID(I+1) = WORK(J+1) + WORK(K+1)                                 01270000
      GRID(I+9) = WORK(J+1) - WORK(K+1)                                 01280000
      I = I + 2                                                         01290000
      J = J + 2                                                         01300000
      K = K + 2                                                         01310000
440   CONTINUE                                                          01320000
      DO 500 KB=9,65,8                                                  01330000
      I = I + 8                                                         01340000
      DO 460 L=1,4                                                      01350000
      GRID(I) = WORK(J) + WORK(K)                                       01360000
      GRID(I+8) = WORK(J) - WORK(K)                                     01370000
      GRID(I+1) = WORK(J+1) + WORK(K+1)                                 01380000
      GRID(I+9) = WORK(J+1) - WORK(K+1)                                 01390000
      TEMP = GRID(I+8)*TRIGS(KB) - GRID(I+9)*TRIGS(KB+1)                01400000
      GRID(I+9) = GRID(I+8)*TRIGS(KB+1) + GRID(I+9)*TRIGS(KB)           01410000
      GRID(I+8) = TEMP                                                  01420000
      I = I + 2                                                         01430000
      J = J + 2                                                         01440000
      K = K + 2                                                         01450000
460   CONTINUE                                                          01460000
500   CONTINUE                                                          01470000
C                                                                       01480000
      I = 1                                                             01490000
      L = 1                                                             01500000
      KC = 1                                                            01510000
      J = 49                                                            01520000
      K = 97                                                            01530000
      M = 17                                                            01540000
      N = 33                                                            01550000
      DO 660 LL=1,8                                                     01560000
      A1 = GRID(J) + GRID(K)                                            01570000
      A3 = SIN60*(GRID(J)-GRID(K))                                      01580000
      B1 = GRID(J+1) + GRID(K+1)                                        01590000
      B3 = SIN60*(GRID(J+1)-GRID(K+1))                                  01600000
      WORK(L) = GRID(I) + A1                                            01610000
      A2 = GRID(I) - 0.5*A1                                             01620000
      WORK(L+1) = GRID(I+1) + B1                                        01630000
      B2 = GRID(I+1) - 0.5*B1                                           01640000
      WORK(N) = A2 + B3                                                 01650000
      WORK(M) = A2 - B3                                                 01660000
      WORK(M+1) = B2 + A3                                               01670000
      WORK(N+1) = B2 - A3                                               01680000
      I = I + 2                                                         01690000
      J = J + 2                                                         01700000
      K = K + 2                                                         01710000
      L = L + 2                                                         01720000
      M = M + 2                                                         01730000
      N = N + 2                                                         01740000
660   CONTINUE                                                          01750000
      DO 700 KB=17,33,16                                                01760000
      L = L + 32                                                        01770000
      M = M + 32                                                        01780000
      N = N + 32                                                        01790000
      KC = KC + 32                                                      01800000
      DO 680 LL=1,8                                                     01810000
      A1 = GRID(J) + GRID(K)                                            01820000
      A3 = SIN60*(GRID(J)-GRID(K))                                      01830000
      B1 = GRID(J+1) + GRID(K+1)                                        01840000
      B3 = SIN60*(GRID(J+1)-GRID(K+1))                                  01850000
      WORK(L) = GRID(I) + A1                                            01860000
      A2 = GRID(I) - 0.5*A1                                             01870000
      WORK(L+1) = GRID(I+1) + B1                                        01880000
      B2 = GRID(I+1) - 0.5*B1                                           01890000
      WORK(N) = A2 + B3                                                 01900000
      WORK(M) = A2 - B3                                                 01910000
      WORK(M+1) = B2 + A3                                               01920000
      WORK(N+1) = B2 - A3                                               01930000
      TEMP = WORK(M)*TRIGS(KB) - WORK(M+1)*TRIGS(KB+1)                  01940000
      WORK(M+1) = WORK(M)*TRIGS(KB+1) + WORK(M+1)*TRIGS(KB)             01950000
      WORK(M) = TEMP                                                    01960000
      TEMP = WORK(N)*TRIGS(KC) - WORK(N+1)*TRIGS(KC+1)                  01970000
      WORK(N+1) = WORK(N)*TRIGS(KC+1) + WORK(N+1)*TRIGS(KC)             01980000
      WORK(N) = TEMP                                                    01990000
      I = I + 2                                                         02000000
      J = J + 2                                                         02010000
      K = K + 2                                                         02020000
      L = L + 2                                                         02030000
      M = M + 2                                                         02040000
      N = N + 2                                                         02050000
680   CONTINUE                                                          02060000
700   CONTINUE                                                          02070000
C                                                                       02080000
      J = 49                                                            02090000
      K = 97                                                            02100000
      L = 144                                                           02110000
      M = 96                                                            02120000
      N = 48                                                            02130000
      DO 900 I=1,47,2                                                   02140000
      A1 = WORK(J) + WORK(K)                                            02150000
      A3 = SIN60 * (WORK(J)-WORK(K))                                    02160000
      B3 = SIN60 * (WORK(J+1)-WORK(K+1))                                02170000
      B1 = WORK(J+1) + WORK(K+1)                                        02180000
      GRID(L+1) = WORK(I) + A1                                          02190000
      A2 = WORK(I) - 0.5*A1                                             02200000
      B2 = WORK(I+1) - 0.5*B1                                           02210000
      GRID(L) = WORK(I+1) + B1                                          02220000
      GRID(N+1) = A2 + B3                                               02230000
      GRID(M+1) = A2 - B3                                               02240000
      GRID(M) = B2 + A3                                                 02250000
      GRID(N) = B2 - A3                                                 02260000
      J = J + 2                                                         02270000
      K = K + 2                                                         02280000
      L = L - 2                                                         02290000
      M = M - 2                                                         02300000
      N = N - 2                                                         02310000
900   CONTINUE                                                          02320000
      GRID(1) = GRID(145)                                               02330000
C                                                                       02340000
      RETURN                                                            02350000
      END                                                               02360000
