       BLOCK DATA WNDCON
C                                                   20-FEB-1996/DSS
*          DATA SET WINCON     AT LEVEL 001 AS OF 05/07/93
*$$$  SUBPROGRAM DOCUMENTATION BLOCK
*                .      .    .                                       .
* SUBPROGRAM:  WNDCON       INITIALIZES THE COMMON BLOCK WINCON.
*   PRGMMR: KRISHNA KUMAR     ORG: NP12     DATE:1999-07-01
*
* ABSTRACT: INITIALIZES THE CONSTANTS IN COMMON /WINCON/ FOR
*   SUBROUTINE WNDBRK WHICH PLOTS WIND DIRECTION SHAFTS WITH BARBS
*   ON FAX OR VARIAN MAPS.
*
* PROGRAM HISTORY LOG:
*   74-03-13  ORIGINAL AUTHOR SHIMOMURA.
*   89-05-09  HENRICHSEN  ADD DOCUMENTATION BLOCK.
*   96-02-20  SHIMOMURA: CONVERT FROM IBM ASM TO FORTRAN BLOCK DATA
* 1999-07-01  KRISHNA KUMAR CONVERTED FROM CRAY TO IBM RS/6000
*
*   OUTPUT ARGUMENT LIST:
*     COMMON   - /WINCON/KWNDFL(5,36),KWNDDV(5,36),KWNDBA(10,19)
*
* REMARKS: CHANGED THE MEMBER NAME OF THE FORTRAN BLOCK DATA VERSION
*     FROM WINCON TO WNDCON SO THAT THE LABELLED COMMON NAME COULD
*     BE LEFT THE SAME AS IT WAS.
*          KWNDBA(10,19) WAS CHANGED TO (10,9) SINCE THAT WAS ALL 
*                        THAT WAS DEFINED.
*
* ATTRIBUTES:
*   LANGUAGE: F90
*   MACHINE:  IBM
*
*$$$
C ...         TITLE 'WINCON -- CONSTANTS FOR WNDBRK -- 13 MAR 74 DSS'

       COMMON    /WINCON/KWNDFL,KWNDDV,KWNDBA
       INTEGER           KWNDFL(5,36)
       INTEGER           KWNDDV(5,36)
       INTEGER           KWNDBA(10,9)


C ...  INTEGER  KWNDFL(5,36)
       DATA     KWNDFL       /
     1          +05,+29,+1,+6,2,  +11,+29,+2,+6,2,  +15,+27,+3,+5,2,
     4          +19,+21,+4,+5,4,  +23,+15,+5,+4,4,  +23,+08,+5,+3,4,
     7          +27,+01,+6,+2,3,  +29,-04,+6,+1,1,  +29,-10,+6,00,1,
     A          +29,-16,+6,-1,1,  +29,-22,+6,-2,1,  +27,-26,+5,-3,1,
     D          +21,-30,+5,-4,3,  +15,-34,+4,-5,3,  +08,-34,+3,-5,3,
     6          +01,-35,+2,-6,4,  -04,-34,+1,-6,2,  -10,-34,00,-6,2,
     9          -16,-34,-1,-6,2,  -22,-34,-2,-6,2,  -26,-32,-3,-5,2,
     2          -30,-29,-4,-5,4,  -34,-23,-5,-4,4,  -34,-16,-5,-3,4,
     5          -35,-12,-6,-2,3,  -34,-07,-6,-1,1,  -34,-01,-6,00,1,
     8          -34,+05,-6,+1,1,  -34,+12,-6,+2,1,  -32,+15,-5,+3,1,
     1          -29,+19,-5,+4,3,  -22,+23,-4,+5,3,  -16,+23,-3,+5,3,
     4          -12,+27,-2,+6,4,  -07,+29,-1,+6,2,  -01,+29,00,+6,2 /

C ...  INTEGER  KWNDDV(5,36)
       DATA     KWNDDV       /
     1           15,01,15,30,120,   20,01,20,30,119,  25,01,30,28,105,
     4           28,01,30,24,092,   30,01,30,20,090,  30,02,30,15,097,
     7           30,06,30,15,119,   30,09,30,15,120,  30,11,30,12,120,
     A           30,15,30,15,120,   30,20,30,20,119,  28,25,30,25,105,
     D           24,28,30,28,092,   20,30,20,30,090,  14,30,15,30,097,
     6           10,30,15,30,119,   07,30,15,30,120,  02,30,12,30,120,
     9           01,30,15,30,120,   01,30,20,30,119,  01,28,30,28,105,
     2           01,24,30,24,092,   01,20,30,20,090,  01,14,30,15,097,
     5           01,10,30,15,119,   01,07,30,15,120,  01,02,30,12,120,
     8           01,01,30,15,120,   01,01,30,20,119,  01,01,30,25,105,
     1           01,01,30,28,092,   01,01,20,30,090,  02,01,15,30,097,
     4           06,01,15,30,119,   09,01,15,30,120,  11,01,12,30,120 /

C ...  INTEGER  KWNDBA(10,9)
       DATA     KWNDBA       /

     9           28,32,37,54,59, 76,81,98,103,120,
     1           28,32,37,54,59, 77,81,98,103,120,
     2           28,32,37,54,59, 75,80,97,102,119,
     3           26,30,34,49,52, 67,71,86,090,105,
     4           23,27,30,44,47, 61,64,77,080,092,
     5           22,25,29,42,46, 59,62,75,078,090,
     6           22,25,29,42,46, 59,63,78,082,097,
     7           26,30,35,52,57, 74,79,96,101,119,
     8           28,32,37,54,59, 76,81,99,103,120 /

        END
