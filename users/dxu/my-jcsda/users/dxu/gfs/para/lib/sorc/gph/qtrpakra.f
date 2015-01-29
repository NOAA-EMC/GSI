       subroutine qtrpakra(isorcra,nsizsor,idestra,nsizdes,
     1                        nwd_packed,iret_qtr)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    QTRPAKRA    QUARTER-PACKER FOR AN ARRAY
C   PRGMMR: SHIMOMURA        ORG: W/NP12     DATE: 96-04-30
C
C ABSTRACT: GIVEN -- 16-BIT ITEMS IN THE LOW-ORDER 16-BITS OF EVERY WORD
C   IN THE SOURCE ARRAY: INTEGER*8 ISORCRA(NSIZSOR); 
C   TASK -- TO QUARTER-PACK THOSE ITEMS, FOUR ITEMS PER 64-BIT LONGWORD, 
C   INTO THE DESTINATION ARRAY: INTEGER*8 IDESTRA(NSIZDES);
C   THE COUNT OF DATA WORDS USED IN THE DESTINATION ARRAY IS PLACED 
C   IN "NWD_PACKED".
C
C PROGRAM HISTORY LOG:
C   96-04-30  ORIGINAL AUTHOR: DAVID SHIMOMURA
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C
C USAGE:    CALL qtrpakra(isorcra,nsizsor, idestra,nsizdes, nwd_packed,
C                         iret_qtr)
C   INPUT ARGUMENT LIST:
C     I*8 ISORCRA(NSIZSOR) - THE SOURCE ARRAY CONTAINING 16-BIT ITEMS 
C                            IN THE LOW-ORDER 16-BITS OF EVERY WORD;
C                            WHERE THE WORD COUNT OF THE GIVEN DATA 
C                               IS CONTAINED IN "NSIZSOR".
C
C   OUTPUT ARGUMENT LIST:
C     I*8 IDESTRA(NSIZDES) - THE DESTINATION ARRAY FOR THE RESULTING
C                            PACKED DATA;  THE GIVEN DATA WILL BE 
C                            PACKED FOUR ITEMS PER 64-BIT LONGWORD 
C                            INTO THE DESTINATION ARRAY;
C
C     I*8 NWD_PACKED ------- THE COUNT OF DATA WORDS USED IN THE 
C                            DESTINATION ARRAY IS PLACED 
C                            IN "NWD_PACKED".
C
C     I*8 IRET_QTR --------  THE ERROR FLAG
C                       = 0; NORMAL RETURN
C                       = 1; DESTINATION-ARRAY DIMENSION .LE. 0;
C                       = 2; SOURCE-ARRAY DIMENSION .LE. 0;
C                       = 3; DESTINATION-ARRAY SIZE IS NOT BIG ENOUGH
C                                TO HOLD THE GIVEN DATA;
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C      ... If you give me a value for nsizsor which is not evenly-
C      ... divisible by four, then the last data word in idestra will 
C      ... be partially full, and some low-order quads will be zerod.
C 
C      ... User must allocate enough space in destination array;
C      ... or I will error-return without moving any data.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C

       integer     nbitwdsz
       parameter  (nbitwdsz=64)    		!... CRAY int word siz
       integer     nbitshf
       parameter  (nbitshf=nbitwdsz/4)       	!... 64/4 => 16 bits shf
       
       integer     isorcra(nsizsor)
       integer     idestra(nsizdes)
       integer     nwd_packed
       integer     iret_qtr
       
       integer     mskloqtr 
       data        mskloqtr      / X'000000000000FFFF' /

       logical     lanymoreqq

       integer     MQ
       integer     isrcval
       integer     leftover
       integer     nwd_required


C      . . . . .   S T A R T   . . . . . . . . . . . . . . . . . . .

       iret_qtr = 0
   

       if(nsizdes .LE. 0) then
          nwd_packed = 0
          iret_qtr = 1
          go to 999
       endif

       if(nsizsor .LE. 0) then
          nwd_packed = 0
          iret_qtr = 2
          go to 999
       endif

C      ... otherwise, nsizsor .GT. 0,  ...
       nwd_packed = nsizsor / 4
       nwd_required = nwd_packed
       lanymoreqq = .FALSE.
        
       leftover = mod(nsizsor,4)
       if(leftover .NE. 0) then
         lanymoreqq = .TRUE.
         nwd_required = nwd_required + 1
       endif

       if(nwd_required .GT. nsizdes) then
C        ... bad!  Not enough space has been allocated in destination
C        ...       so ERROR EXIT
         nwd_packed = 0
         iret_qtr = 3
         go to 999
       endif
C      ... otherwise, there is enough space allocated for results ...

       if(nwd_packed .GT. 0) then
          do  ide = 1,nwd_packed
             inptr = 4*ide - 4
             MQ = 0
             do iqt = 1,4
                MQ = ishftc(MQ,16,64)    		!... RQL 16
                isrcval = iand(isorcra(inptr+iqt),mskloqtr)
                MQ = ior(MQ,isrcval)
             enddo
             idestra(ide) = MQ
          enddo
       endif

       if(lanymoreqq) then
C         -----------------------------------------------------------
          nwd_packed = nwd_packed + 1
          ide = nwd_packed
             inptr = 4*ide - 4
             MQ = 0
             do iqt = 1,leftover
                !...    RQL 16; .or. VALi
                MQ = ishftc(MQ,16,64)
                isrcval = iand(isorcra(inptr+iqt),mskloqtr)
                MQ = ior(MQ,isrcval)
             enddo
C            ... but that leaves the MQ not RQLed enough ...
             icount = 4 - leftover
             do while (icount .GT. 0)
                MQ = ishftc(MQ,16,64)
                icount = icount - 1
             enddo
             idestra(ide) = MQ
  432     continue
C         -----------------------------------------------------------
       endif

  999  continue
       return
       end       
