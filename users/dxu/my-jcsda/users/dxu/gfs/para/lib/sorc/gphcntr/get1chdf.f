       subroutine get1chdf(ichset,ixfound,LCKPRNTQ,lonechdef,iret_get)
C      ... version of 16-Jan-1997/dss: for addition of one more font
C             to total 44 fonts
C
C      ... copied get1chdf.f to get1ch43.f            7-Oct-1996/dss
C      ...    in order to make a version with all 43 fonts included
C      ...    instead of the previous one which was for PEPFAX only.
C
C      ... Added font15 for Luke                     16-Feb-1996/dss
C      ... Added font16 & 17 for Luke                22-Feb-1996/dss
C      ... FONT TABLES FOR AVPOLAR ...               31-JAN-1995/DSS
C                                                     4-Jan-1996/dss
C      ... to fetch one character's bit-mapped definition ...
C      ... Caution: I will zero out 16 longwords of lonechdef()
C      ...          so do not dimension lonechdef at less than 16
C
C      ... Krishna Kumar converted this code from CRAY to
C      ... IBM RS/6000    
       integer  ichset    		!... Given
       integer  ixfound   		!... Given
       LOGICAL  LCKPRNTQ    		!... Given
       integer  lonechdef(16)      	!... where 16 longwords is max
       integer  iret_get             	!... return code

C      --------------------------------------------------------------
       EXTERNAL     FONT01
       EXTERNAL     FONT02
       EXTERNAL     FONT03
       EXTERNAL     FONT04
       EXTERNAL     FONT05
       EXTERNAL     FONT06
       EXTERNAL     FONT07
       EXTERNAL     FONT08
       EXTERNAL     FONT09
       EXTERNAL     FONT10
       EXTERNAL     FONT11
       EXTERNAL     FONT12
       EXTERNAL     FONT13
       EXTERNAL     FONT15
       EXTERNAL     FONT16
       EXTERNAL     FONT17
       EXTERNAL     FONT18
       EXTERNAL     FONT19
       EXTERNAL     FONT20
       EXTERNAL     FONT21
       EXTERNAL     FONT22
       EXTERNAL     FONT23
       EXTERNAL     FONT24
       EXTERNAL     FONT25
       EXTERNAL     FONT26
       EXTERNAL     FONT27
       EXTERNAL     FONT28
       EXTERNAL     FONT29
       EXTERNAL     FONT30
       EXTERNAL     FONT31
       EXTERNAL     FONT32
       EXTERNAL     FONT33
       EXTERNAL     FONT34
       EXTERNAL     FONT35
       EXTERNAL     FONT36
       EXTERNAL     FONT37
       EXTERNAL     FONT38
       EXTERNAL     FONT39
       EXTERNAL     FONT40
       EXTERNAL     FONT41
       EXTERNAL     FONT42
       EXTERNAL     FONT43
       EXTERNAL     FONT44

C      --------------------------------------------------------------
       COMMON /FONS_01/KHTITLE_01,NUMSET_01,NCHS_01,NWPC_01,KIXTB_01,
     1                KDF_01
       integer   khtitle_01
       integer   numset_01
       integer   nchs_01
       integer   nwpc_01
       integer   kixtb_01(8)
         INTEGER    KDF_01(3,64)
* 1. LUKSTD  OR  CD1=(10X12)   64* CHARACTERS STANDARD SIZE

C      ===============================================================
       COMMON /FONS_02/KHTITLE_02,NUMSET_02,NCHS_02,NWPC_02,
     1                 KIXTB_02,KDF_02
       integer   khtitle_02
       integer   numset_02
       integer   nchs_02
       integer   nwpc_02
       integer   kixtb_02(8)
         INTEGER    KDF_02(4,19)
* 2. LUKHOLL OR  CD2=(12X14)   19* CHARACTERS HOLLOW

C      ===============================================================
       COMMON /FONS_03/KHTITLE_03,NUMSET_03,NCHS_03,NWPC_03,
     1                 KIXTB_03,KDF_03
       integer   khtitle_03
       integer   numset_03
       integer   nchs_03
       integer   nwpc_03
       integer   kixtb_03(8)
         INTEGER    KDF_03(3,51)
* 3. LIKSTD  OR  CD46=(12X-10) 51* CHARACTER STANDARD SIZE -- SIDEWAYS

C      ===============================================================
       COMMON /FONS_04/KHTITLE_04,NUMSET_04,NCHS_04,NWPC_04,
     1                 KIXTB_04,KDF_04
       integer   khtitle_04
       integer   numset_04
       integer   nchs_04
       integer   nwpc_04
       integer   kixtb_04(8)
         INTEGER    KDF_04(1,45)
* 4. LUKTINY OR  CD4=(6X8)     45  CHARACTERS SMALL UPRIGHT

C      ===============================================================
       COMMON /FONS_05/KHTITLE_05,NUMSET_05,NCHS_05,NWPC_05,
     1                 KIXTB_05,KDF_05
       integer   khtitle_05
       integer   numset_05
       integer   nchs_05
       integer   nwpc_05
       integer   kixtb_05(8)
         INTEGER    KDF_05(1,45)
* 5. LIKTINY OR  CD5=(8X-6)    45  CHARACTERS SMALL SIDEWAYS

C      ===============================================================
       COMMON /FONS_06/KHTITLE_06,NUMSET_06,NCHS_06,NWPC_06,
     1                 KIXTB_06,KDF_06
       integer   khtitle_06
       integer   numset_06
       integer   nchs_06
       integer   nwpc_06
       integer   kixtb_06(8)
         INTEGER    KDF_06(2,10)
* 6. LUKFLG1 OR  CD6=(-6X12)   10  WIND FLAG 1

C      ===============================================================
       COMMON /FONS_07/KHTITLE_07,NUMSET_07,NCHS_07,NWPC_07,
     1                 KIXTB_07,KDF_07
       integer   khtitle_07
       integer   numset_07
       integer   nchs_07
       integer   nwpc_07
       integer   kixtb_07(8)
         INTEGER    KDF_07(2,15)
* 7. LUKFLG2 OR  CD7=(-12X6)   15  WIND FLAG 2

C      ===============================================================
       COMMON /FONS_08/KHTITLE_08,NUMSET_08,NCHS_08,NWPC_08,
     1                 KIXTB_08,KDF_08
       integer   khtitle_08
       integer   numset_08
       integer   nchs_08
       integer   nwpc_08
       integer   kixtb_08(8)
          INTEGER    KDF_08(3,8)
* 8. LUKFLG3 OR  CD8=(-9X12)    8  WIND FLAG 3

C      ===============================================================
       COMMON /FONS_09/KHTITLE_09,NUMSET_09,NCHS_09,NWPC_09,
     1                 KIXTB_09,KDF_09
       integer   khtitle_09
       integer   numset_09
       integer   nchs_09
       integer   nwpc_09
       integer   kixtb_09(8)
         INTEGER    KDF_09(3,11)
* 9. LUKFLG4 OR  CD9=(-12X9)   11  WIND FLAG 4

C      ===============================================================
       COMMON /FONS_10/KHTITLE_10,NUMSET_10,NCHS_10,NWPC_10,
     1                 KIXTB_10,KDF_10
       integer   khtitle_10
       integer   numset_10
       integer   nchs_10
       integer   nwpc_10
       integer   kixtb_10(8)
        INTEGER    KDF_10(15,20)
*10. LUKHURR OR  CD10=(-30X30) 20  CHARACTERS CIR,BOX,RT BRACKET,HURR,LOGO

C      ===============================================================
       COMMON /FONS_11/KHTITLE_11,NUMSET_11,NCHS_11,NWPC_11,
     1                 KIXTB_11,KDF_11
       integer   khtitle_11
       integer   numset_11
       integer   nchs_11
       integer   nwpc_11
       integer   kixtb_11(8)
         INTEGER    KDF_11(5,53)
*11. LUKGIAN OR  CD11=(15X20)  53  CHARACTERS GIANT SIZE

C      ===============================================================
       COMMON /FONS_12/KHTITLE_12,NUMSET_12,NCHS_12,NWPC_12,
     1                 KIXTB_12,KDF_12
       integer   khtitle_12
       integer   numset_12
       integer   nchs_12
       integer   nwpc_12
       integer   kixtb_12(8)
         INTEGER    KDF_12(1,26)
*12. LUKLINL OR  CD12=(-4X4)   26  LINE-ELEMENT VECTORS

C      ===============================================================
       COMMON /FONS_13/KHTITLE_13,NUMSET_13,NCHS_13,NWPC_13,
     1                 KIXTB_13,KDF_13
       integer   khtitle_13
       integer   numset_13
       integer   nchs_13
       integer   nwpc_13
       integer   kixtb_13(8)
         INTEGER    KDF_13(1,33)
*13. LUKHLNS OR  CD13=(-32X1)  33  HORIZONTAL LINES
*                                  to draw fronts from Intergraph

C      ===============================================================
*14. LUKRUSS OR  CD14=(15X20)   0  CHARACTERS RUSSIAN

C      ===============================================================
       COMMON /FONS_15/KHTITLE_15,NUMSET_15,NCHS_15,NWPC_15,
     1                 KIXTB_15,KDF_15
       integer   khtitle_15
       integer   numset_15
       integer   nchs_15
       integer   nwpc_15
       integer   kixtb_15(8)
         INTEGER    KDF_15(4,13)
*15. LUKWONB OR  CD15=(12X16)  13  WHITE ON BLACK NUMBERS

C      ===============================================================
       COMMON /FONS_16/KHTITLE_16,NUMSET_16,NCHS_16,NWPC_16,
     1                 KIXTB_16,KDF_16
       integer   khtitle_16
       integer   numset_16
       integer   nchs_16
       integer   nwpc_16
       integer   kixtb_16(8)
          INTEGER    KDF_16(3,1)
*16. LUKBARH OR  CD16=(20X6)    1  HORIZONTAL BAR

C      ===============================================================
       COMMON /FONS_17/KHTITLE_17,NUMSET_17,NCHS_17,NWPC_17,
     1                 KIXTB_17,KDF_17
       integer   khtitle_17
       integer   numset_17
       integer   nchs_17
       integer   nwpc_17
       integer   kixtb_17(8)
          INTEGER    KDF_17(3,1)
*17. LUKBARV OR  CD17=(6X-20)   1  VERTICAL BAR

C      ===============================================================
       COMMON /FONS_18/KHTITLE_18,NUMSET_18,NCHS_18,NWPC_18,
     1                 KIXTB_18,KDF_18
       integer   khtitle_18
       integer   numset_18
       integer   nchs_18
       integer   nwpc_18
       integer   kixtb_18(8)
         INTEGER    KDF_18(4,13)
*18. LUKITAL OR  CD18=(12X14)  13  NUMBERS ITALICS

C      ===============================================================
       COMMON /FONS_19/KHTITLE_19,NUMSET_19,NCHS_19,NWPC_19,
     1                 KIXTB_19,KDF_19
       integer   khtitle_19
       integer   numset_19
       integer   nchs_19
       integer   nwpc_19
       integer   kixtb_19(8)
         INTEGER    KDF_19(2,42)
*19. LUKWDIG OR  CD19=(7X10)   42  CHARACTERS WIND DIRECTION DIGIT + ALPH

C      ===============================================================
       COMMON /FONS_20/KHTITLE_20,NUMSET_20,NCHS_20,NWPC_20,
     1                 KIXTB_20,KDF_20
       integer   khtitle_20
       integer   numset_20
       integer   nchs_20
       integer   nwpc_20
       integer   kixtb_20(8)
         INTEGER    KDF_20(15,1)
*20. LUKERAS OR  CD20=(30X30)   1  ZERO ARRAY USED FOR ERASING

C      ===============================================================
       COMMON /FONS_21/KHTITLE_21,NUMSET_21,NCHS_21,NWPC_21,
     1                 KIXTB_21,KDF_21
       integer   khtitle_21
       integer   numset_21
       integer   nchs_21
       integer   nwpc_21
       integer   kixtb_21(8)
         INTEGER    KDF_21(6,53)    		!... 6*53 =318 words
*21. LIKGIAN OR  CD21=(20X-15) 53* CHARACTER GIANT SIZE SIDEWAYS

C      ===============================================================
       COMMON /FONS_22/KHTITLE_22,NUMSET_22,NCHS_22,NWPC_22,
     1                 KIXTB_22,KDF_22
       integer   khtitle_22
       integer   numset_22
       integer   nchs_22
       integer   nwpc_22
       integer   kixtb_22(8)
         INTEGER    KDF_22(3,13)
*22. LIKWONB OR  CD22=(16X-12) 13  NUMBERS WHITE ON BLACK SIDEWAYS

C      ===============================================================
       COMMON /FONS_23/KHTITLE_23,NUMSET_23,NCHS_23,NWPC_23,
     1                 KIXTB_23,KDF_23
       integer   khtitle_23
       integer   numset_23
       integer   nchs_23
       integer   nwpc_23
       integer   kixtb_23(8)
         INTEGER    KDF_23(3,13)
*23. LIKITAL OR  CD23=(14X-12) 13  NUMBERS ITALICS   SIDEWAYS

C      ===============================================================
       COMMON /FONS_24/KHTITLE_24,NUMSET_24,NCHS_24,NWPC_24,
     1                 KIXTB_24,KDF_24
       integer   khtitle_24
       integer   numset_24
       integer   nchs_24
       integer   nwpc_24
       integer   kixtb_24(8)
         INTEGER    KDF_24(2,42)
*24. LIKWDIG OR  CD24=(10X-7)  42* WIND DIR DIGITS + ALPHA  SIDEWAYS

C      ===============================================================
       COMMON /FONS_25/KHTITLE_25,NUMSET_25,NCHS_25,NWPC_25,
     1                 KIXTB_25,KDF_25
       integer   khtitle_25
       integer   numset_25
       integer   nchs_25
       integer   nwpc_25
       integer   kixtb_25(8)
         INTEGER    KDF_25(3,18)
*25. LIKHOLL OR  CD25=(14X-12) 18  HOLLOW CHARACTERS  SIDEWAYS

C      ===============================================================
       COMMON /FONS_26/KHTITLE_26,NUMSET_26,NCHS_26,NWPC_26,
     1                 KIXTB_26,KDF_26
       integer   khtitle_26
       integer   numset_26
       integer   nchs_26
       integer   nwpc_26
       integer   kixtb_26(8)
         INTEGER    KDF_26(4,36)
*26. LUKAHED OR  CD26=(-16X16) 36  DIRECTION ARROWHEADS

C      ===============================================================
       COMMON /FONS_27/KHTITLE_27,NUMSET_27,NCHS_27,NWPC_27,
     1                 KIXTB_27,KDF_27
       integer   khtitle_27
       integer   numset_27
       integer   nchs_27
       integer   nwpc_27
       integer   kixtb_27(8)
         INTEGER    KDF_27(4,23)
*27. LIKSKYN OR  CD27=(15X-15) 23  MERCATOR SKY COVERAGE SYMBOLS  (N)

C      ===============================================================
       COMMON /FONS_28/KHTITLE_28,NUMSET_28,NCHS_28,NWPC_28,
     1                 KIXTB_28,KDF_28
       integer   khtitle_28
       integer   numset_28
       integer   nchs_28
       integer   nwpc_28
       integer   kixtb_28(8)
         INTEGER    KDF_28(4,11)		!... SEE CD45=
*28. LIKCLDN OR  CD28=(12X-15) 11  LOW CLOUD SYMBOLS     (CL)

C      ===============================================================
       COMMON /FONS_29/KHTITLE_29,NUMSET_29,NCHS_29,NWPC_29,
     1                 KIXTB_29,KDF_29
       integer   khtitle_29
       integer   numset_29
       integer   nchs_29
       integer   nwpc_29
       integer   kixtb_29(8)
         INTEGER    KDF_29(4,11)		
*29. LIKCLDM OR  CD29=(12X-15) 11  MID CLOUD SYMBOLS     (CM)

C      ===============================================================
       COMMON /FONS_30/KHTITLE_30,NUMSET_30,NCHS_30,NWPC_30,
     1                 KIXTB_30,KDF_30
       integer   khtitle_30
       integer   numset_30
       integer   nchs_30
       integer   nwpc_30
       integer   kixtb_30(8)
         INTEGER    KDF_30(4,11)		
*30. LIKCLDH OR  CD30=(12X-15) 11  HI CLOUD SYMBOLS      (CH)

C      ===============================================================
       COMMON /FONS_31/KHTITLE_31,NUMSET_31,NCHS_31,NWPC_31,
     1                 KIXTB_31,KDF_31
       integer   khtitle_31
       integer   numset_31
       integer   nchs_31
       integer   nwpc_31
       integer   kixtb_31(8)
         INTEGER    KDF_31(4,10)		
*31. LIKAOFP OR  CD31=(12X-15) 10  PRESSURE TENDENCY SYMBOLS (A)

C      ===============================================================
       COMMON /FONS_32/KHTITLE_32,NUMSET_32,NCHS_32,NWPC_32,
     1                 KIXTB_32,KDF_32
       integer   khtitle_32
       integer   numset_32
       integer   nchs_32
       integer   nwpc_32
       integer   kixtb_32(8)
         INTEGER    KDF_32(4,10)
*32. LIKWW0  OR  CD32=(15X-15) 10  PAST WEATHER SYMBOLS (WW)

C      ===============================================================
       COMMON /FONS_33/KHTITLE_33,NUMSET_33,NCHS_33,NWPC_33,
     1                 KIXTB_33,KDF_33
       integer   khtitle_33
       integer   numset_33
       integer   nchs_33
       integer   nwpc_33
       integer   kixtb_33(8)
         INTEGER    KDF_33(4,43)
*33. LIKWW1  OR  CD33=(15X-15) 43  SURFACE PRESENT WEATHER SYMBOLS (WW)

C      ===============================================================
       COMMON /FONS_34/KHTITLE_34,NUMSET_34,NCHS_34,NWPC_34,
     1                 KIXTB_34,KDF_34
       integer   khtitle_34
       integer   numset_34
       integer   nchs_34
       integer   nwpc_34
       integer   kixtb_34(8)
         INTEGER    KDF_34(6,23)
*34. LIKWW2  OR  CD34=(20X-15) 23  SURFACE PRESENT WEATHER SYMBOLS (WW)

C      ===============================================================
       COMMON /FONS_35/KHTITLE_35,NUMSET_35,NCHS_35,NWPC_35,
     1                 KIXTB_35,KDF_35
       integer   khtitle_35
       integer   numset_35
       integer   nchs_35
       integer   nwpc_35
       integer   kixtb_35(8)
         INTEGER    KDF_35(5,16)
*35. LIKWW3  OR  CD35=(20X-15) 20  SFC PRESENT WEATHER SYMBOLS (WW)

C      ===============================================================
       COMMON /FONS_36/KHTITLE_36,NUMSET_36,NCHS_36,NWPC_36,
     1                 KIXTB_36,KDF_36
       integer   khtitle_36
       integer   numset_36
       integer   nchs_36
       integer   nwpc_36
       integer   kixtb_36(8)
         INTEGER    KDF_36(8,20)
*36. LIKWW4  OR  CD36=(20X-20) 20  SURFACE PRESENT WEATHER SYMBOLS (WW)

C      ===============================================================
       COMMON /FONS_37/KHTITLE_37,NUMSET_37,NCHS_37,NWPC_37,
     1                 KIXTB_37,KDF_37
       integer   khtitle_37
       integer   numset_37
       integer   nchs_37
       integer   nwpc_37
       integer   kixtb_37(8)
         INTEGER    KDF_37(8,02)
*37. LUKSPOR OR  CD37=(19X19)   2  SPECIAL NON-ERASING CHARACTERS

C      ===============================================================
       COMMON /FONS_38/KHTITLE_38,NUMSET_38,NCHS_38,NWPC_38,
     1                 KIXTB_38,KDF_38
       integer   khtitle_38
       integer   numset_38
       integer   nchs_38
       integer   nwpc_38
       integer   kixtb_38(8)
         INTEGER    KDF_38(1,29)   !... RENAMED FROM FONT28.F
*38. LUKLINM OR  CD38=(-8X8)   29  CHARACTERS VECTOR LINE

C      ===============================================================
       COMMON /FONS_39/KHTITLE_39,NUMSET_39,NCHS_39,NWPC_39,
     1                 KIXTB_39,KDF_39
       integer   khtitle_39
       integer   numset_39
       integer   nchs_39
       integer   nwpc_39
       integer   kixtb_39(8)
         INTEGER    KDF_39(4,36)
*39. LUKAROW OR  CD39=(16X16)  36  DIRECTIONAL ARROWS

C      ===============================================================
       COMMON /FONS_40/KHTITLE_40,NUMSET_40,NCHS_40,NWPC_40,
     1                 KIXTB_40,KDF_40
       integer   khtitle_40
       integer   numset_40
       integer   nchs_40
       integer   nwpc_40
       integer   kixtb_40(8)
         INTEGER    KDF_40(8,36)
*40. LUKPIPW OR  CD40=(20X20)  36  WARM FRONT PIPS

C      ===============================================================
       COMMON /FONS_41/KHTITLE_41,NUMSET_41,NCHS_41,NWPC_41,
     1                 KIXTB_41,KDF_41
       integer   khtitle_41
       integer   numset_41
       integer   nchs_41
       integer   nwpc_41
       integer   kixtb_41(8)
         INTEGER    KDF_41(8,36)
*41. LUKPIPC OR  CD41=(20X20)  36  COLD FRONT PIPS

C      ===============================================================
       COMMON /FONS_42/KHTITLE_42,NUMSET_42,NCHS_42,NWPC_42,
     1                 KIXTB_42,KDF_42
       integer   khtitle_42
       integer   numset_42
       integer   nchs_42
       integer   nwpc_42
       integer   kixtb_42(8)
         INTEGER    KDF_42(2,10)
*42. LIKPARN OR  CD42=(16X-8)  10* SPECIAL CHARACTERS: ()[]. SIDEWAYS

C      ===============================================================
       COMMON /FONS_43/KHTITLE_43,NUMSET_43,NCHS_43,NWPC_43,
     1                 KIXTB_43,KDF_43
       integer   khtitle_43
       integer   numset_43
       integer   nchs_43
       integer   nwpc_43
       integer   kixtb_43(8)
          INTEGER    KDF_43(6,7)     !... USED TO BE IN FONT27.F ???
*43. LUKAVIA OR  CD43=(20X14)   7  PILOT SYMBOLS

C      ===============================================================
       COMMON /FONS_44/KHTITLE_44,NUMSET_44,NCHS_44,NWPC_44,
     1                 KIXTB_44,KDF_44
       integer   khtitle_44
       integer   numset_44
       integer   nchs_44
       integer   nwpc_44
       integer   kixtb_44(8)
          INTEGER    KDF_44(3,55)     !... USED TO BE IN FONT47.F 
*44. LOOK44 OR  CD44=(10X12) 55 TDL WIND-CLOUD SPECIAL SYMBOLS + ALNUM

C      ===============================================================

C      ----------------------------------------------------------------
C      ... CAN I USE POINTER?
C
       integer       KDF_XX(318)         	!... Nwords of CD21 MAX 
       POINTER      (PTR_KDF, KDF_XX)
C
       iret_get = 0
       do  i = 1,16
           lonechdef(i) = 0
       enddo
C
       INC = 0

       if(ichset .gt. 44) then
         iret_get = 1
         go to 900
       endif

       if(ichset .eq. 12) then
         INC = NWPC_12
         PTR_KDF = LOC(KDF_12)

       else if(ichset .eq. 19) then
         INC = NWPC_19
         PTR_KDF = LOC(KDF_19)

       else if(ichset .eq. 20) then
         INC = NWPC_20
         PTR_KDF = LOC(KDF_20)

       else if(ichset .eq. 1) then
         INC = NWPC_01
         PTR_KDF = LOC(KDF_01)

       else if(ichset .eq. 2) then
         INC = NWPC_02
         PTR_KDF = LOC(KDF_02)

       else if(ichset .eq. 3) then
         INC = NWPC_03
         PTR_KDF = LOC(KDF_03)

       else if(ichset .eq. 10) then
         INC = NWPC_10
         PTR_KDF = LOC(KDF_10)

       else if(ichset .eq. 11) then
         INC = NWPC_11
         PTR_KDF = LOC(KDF_11)

       else if(ichset .eq. 13) then
         INC = NWPC_13
         PTR_KDF = LOC(KDF_13)

       else if(ichset .eq. 15) then
         INC = NWPC_15
         PTR_KDF = LOC(KDF_15)

       else if(ichset .eq. 16) then
         INC = NWPC_16
         PTR_KDF = LOC(KDF_16)

       else if(ichset .eq. 17) then
         INC = NWPC_17
         PTR_KDF = LOC(KDF_17)

       else if(ichset .eq. 4) then		!... tiny
         INC = NWPC_04
         PTR_KDF = LOC(KDF_04)

       else if(ichset .eq. 5) then		!... tiny -- SW
         INC = NWPC_05
         PTR_KDF = LOC(KDF_05)

       else if(ichset .eq. 6) then		!... wind flag 1
         INC = NWPC_06
         PTR_KDF = LOC(KDF_06)

       else if(ichset .eq. 7) then		!... wind flag 2
         INC = NWPC_07
         PTR_KDF = LOC(KDF_07)

       else if(ichset .eq. 8) then		!... wind flag 3
         INC = NWPC_08
         PTR_KDF = LOC(KDF_08)

       else if(ichset .eq. 9) then		!... wind flag 4
         INC = NWPC_09
         PTR_KDF = LOC(KDF_09)

       else if(ichset .eq. 18) then		!... italics
         INC = NWPC_18
         PTR_KDF = LOC(KDF_18)

       else if(ichset .eq. 21) then		!... giant -- SW
         INC = NWPC_21
         PTR_KDF = LOC(KDF_21)

       else if(ichset .eq. 22) then		!... white-on-black SW
         INC = NWPC_22
         PTR_KDF = LOC(KDF_22)

       else if(ichset .eq. 23) then		!... italics SW
         INC = NWPC_23
         PTR_KDF = LOC(KDF_23)

       else if(ichset .eq. 24) then		!... wnd dir dig+alf SW
         INC = NWPC_24
         PTR_KDF = LOC(KDF_24)

       else if(ichset .eq. 25) then		!... hollow -- SW
         INC = NWPC_25
         PTR_KDF = LOC(KDF_25)

       else if(ichset .eq. 26) then		!... arrowheads
         INC = NWPC_26
         PTR_KDF = LOC(KDF_26)

       else if(ichset .eq. 39) then		!... directional arrows
         INC = NWPC_39
         PTR_KDF = LOC(KDF_39)

       else if(ichset .eq. 27) then		!... sky cover (N) -- SW
         INC = NWPC_27
         PTR_KDF = LOC(KDF_27)

       else if(ichset .eq. 28) then		!... low cld (CL) -- SW
         INC = NWPC_28
         PTR_KDF = LOC(KDF_28)

       else if(ichset .eq. 29) then		!... mid cld (CM) -- SW
         INC = NWPC_29
         PTR_KDF = LOC(KDF_29)

       else if(ichset .eq. 30) then		!... hi cld (CH) -- SW
         INC = NWPC_30
         PTR_KDF = LOC(KDF_30)

       else if(ichset .eq. 31) then		!... press tendency - SW
         INC = NWPC_31
         PTR_KDF = LOC(KDF_31)

       else if(ichset .eq. 32) then		!... past weather -- SW
         INC = NWPC_32
         PTR_KDF = LOC(KDF_32)

       else if(ichset .eq. 33) then		!... present wx 1 -- SW
         INC = NWPC_33
         PTR_KDF = LOC(KDF_33)

       else if(ichset .eq. 34) then		!... present wx 2 -- SW
         INC = NWPC_34
         PTR_KDF = LOC(KDF_34)

       else if(ichset .eq. 35) then		!... present wx 3 -- SW
         INC = NWPC_35
         PTR_KDF = LOC(KDF_35)

       else if(ichset .eq. 36) then		!... present wx 4 -- SW
         INC = NWPC_36
         PTR_KDF = LOC(KDF_36)

       else if(ichset .eq. 37) then		!... (19x19) spec symbol
         INC = NWPC_37
         PTR_KDF = LOC(KDF_37)

       else if(ichset .eq. 42) then		!...(16x-8) special symb
         INC = NWPC_42
         PTR_KDF = LOC(KDF_42)

       else if(ichset .eq. 43) then		!... Aviation Wx symbol
         INC = NWPC_43
         PTR_KDF = LOC(KDF_43)

       else if(ichset .eq. 38) then		!... (-8x8) line elem
         INC = NWPC_38
         PTR_KDF = LOC(KDF_38)

       else if(ichset .eq. 40) then		!... warm front pips
         INC = NWPC_40
         PTR_KDF = LOC(KDF_40)

       else if(ichset .eq. 41) then		!... cold front pips
         INC = NWPC_41
         PTR_KDF = LOC(KDF_41)

       else if(ichset .eq. 44) then		!... TDL WNDCLDFX symbol
         INC = NWPC_44
         PTR_KDF = LOC(KDF_44)
C      . . . . . . . . . . . . . . . .
       else
         iret_get = 2
         go to 900
       endif


C      .................................................
       if(INC .GT. 16) then
C        ... error ... jump out
         iret_get = 3
         go to 900
       endif

       iofdef = INC*(ixfound-1) + 1
       DO  IDE = 1,INC
         if(LCKPRNTQ) THEN
           WRITE(6,245)IDE,IOFDEF,KDF_XX(iofdef)
  245      FORMAT(1H ,'get1chdf:IDE=',I4,'; IOFDEF=',I4,'... ',Z16.16)
         endif
         lonechdef(ide) = KDF_XX(iofdef)
         iofdef = iofdef + 1
       ENDDO
       go to 999

  900  continue
C      ... comes here on error conditions
       go to 999

  999  continue
       return
       END
