       SUBROUTINE PAK8TO6(ISORCRA,IDESTRA)
C                                                        16-MAY-1996/DSS
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PAK8TO6     PACK EXTENDED 6-BIT TO CONCATENATED 6-BIT
C   PRGMMR: KRISHNA KUMAR     ORG: W/NP12     DATE: 1999-07-01
C
C ABSTRACT: A LIMITED-USE PACKER TO COMPRESS THE GIVEN 1920-BYTE ARRAY
C   (CONTAINING EXTENDED NMC 6-BIT RLE CODE) INTO CONCATENATED 6-BIT
C   GROUPS OF THE RESULTING 1440-BYTE IDESTRA ARRAY. 
C
C PROGRAM HISTORY LOG:
C   96-05-16  ORIGINAL AUTHOR(S)'S NAME: DAVID SHIMOMURA
C 1999-07-01  CONVERTED THIS CODE FROM CRAY TO IBM RS/6000
C
C USAGE:    CALL PAK8TO6(ISORCRA,IDESTRA)
C   INPUT ARGUMENT LIST:
C     I*8 ISORCRA(240) - EACH BYTE CONTAINS GOOD DATA IN LOW-ORDER
C                        6-BITS; THE HI-ORDER 2-BITS OF EVERY BYTE
C                        ARE FILL;
C                        THE SIZE OF ISORCRA MUST BE 240 INT WORDS;
C                        WHERE 240 CRAY INTEGERS = 1920 BYTES;
C
C   OUTPUT ARGUMENT LIST:
C     I*8 IDESTRA(180) - EACH 6-BIT GROUP OF THE SOURCE DATA IS
C                        CONCATENATED INTO THIS PACKED 6-BIT 
C                        DESTINATION ARRAY;
C                        THE SIZE OF IDESTRA MUST BE 180 INT WORDS;
C                        WHERE 180 CRAY INTEGERS = 1440 BYTES;
C
C
C REMARKS:
C     THIS IS A SPECIALIZED PACKER ONLY FOR COMPRESSING FAX RECORDS;
C     THE ARRAY SIZES MUST BE AS SPECIFIED HEREIN;
C     NO BOUNDS CHECKING IS DONE; 
C     THE LOGIC USED:  
C      ...        (1.) UNPACK EACH 8-BIT BYTE INTO ONE INTEGER WORD
C      ...                PER 8-BIT ITEM;  VIA GBYTES();
C      ...        (2.) PACK EACH 6-BIT ITEM VIA SBYTES;
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       INTEGER       NGRPREC 
       PARAMETER    (NGRPREC=1920)

       INTEGER       KINPRECSZWRD
       PARAMETER    (KINPRECSZWRD=NGRPREC/8)   		!...=240

       INTEGER       KOUTRECSZBY
       PARAMETER    (KOUTRECSZBY=1440)

       integer       KOUTRECSZWRD
       PARAMETER    (KOUTRECSZWRD=KOUTRECSZBY/8) 	!...=180

       INTEGER       ISORCRA(KINPRECSZWRD)    	!... (240)=1920/8
       INTEGER       IDESTRA(KOUTRECSZWRD)	!... (180)=1440/8

       INTEGER       JJWORK(NGRPREC)   		!... WORK SPACE(1920)
C      ... following are args to GBYTES() ...
       integer  noffset    			!... Arg(3)

       integer  kbitpgrp    			!... Arg(4)
       data     kbitpgrp  / 8 /

       integer  kpadbits    			!... Arg(5)
       data     kpadbits  / 0 /
C
C      ...Call GBYTES(ISORCRA,JJWORK,noffset,kbitpgrp,kpadbits,ngrps2do)
C                       1     2        3       4        5        6

C      ... ARGS FOR PACKER: SBYTES()

       INTEGER    NOFFSET_S
       DATA       NOFFSET_S    / 0 /

C      ... WHERE SBYTES' NOFFSET IS WITHIN PACKED DESTINATION
       INTEGER    NBITPGRP_S
       DATA       NBITPGRP_S   / 6 /

       INTEGER    NBITSKIP_S
       DATA       NBITSKIP_S   / 0 /
C      ... WHERE SBYTES' NBITSKIP IS WITHIN PACKED DESTINATION
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C        . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C        ... Do every record like an interior record,
         noffset = 0      
         ngrps2do = ngrprec       
C        ...   where  ngrprec = 1920 groups per extended source array
C
         Call GBYTES(ISORCRA,JJWORK,noffset,kbitpgrp,kpadbits,ngrps2do)
C                       1     2        3       4        5        6
C
C
C        ... Now I have spread out one 6-bit group per longword
C        ...     in (JJWORK(i),i=1,ngrps2do)
C     
C
C        ... Next, to concatenate the 6-bit data-groups;
         CALL SBYTES(IDESTRA,JJWORK, NOFFSET_S, NBITPGRP_S,
     1                          NBITSKIP_S, NGRPS2DO)
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
         RETURN
         END
