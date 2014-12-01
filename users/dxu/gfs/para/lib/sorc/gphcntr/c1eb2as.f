       character*1 function c1eb2as(lonechar)
C                                                        29-JAN-1996/DSS
C      ...   to convert one  EBCDIC LABEL-array text character
C      ...   into ASCII LABEL-array text
C
C



C                                                   29-JAN-1996/DSS
C      ... TRANSLATE TABLE FROM EBCDIC LABEL-ARRAY TEXT TO ASCII
C      ... LABEL-ARRAY TEXT (ONLY FOR USE IN TRANSLATING THE TEXT
C      ... PORTION OF THE LABEL ARRAY ITEM; NOT THE CHAR-SET INDEX,
C      ... NOT THE JIWORD)
C      ... TEXT TERMINATOR:EBCDIC "$" WILL BE TRANSLATED INTO NULL;
C      ... ANY UNEXPECTED TEXT BYTE WILL BE TRANSLATED INTO ASCII "#"
C      ... WHICH WILL BE SUBSEQUENTLY INTERPRETED AS A SPACE-HOLDER.
C      ... X'00':X'3F' WILL BE PASSED THRU UNCHANGED,
C      ...   BECAUSE CHAR-SET_12 USES BINARY INDEX
       integer  keb2astb(32)
       data     keb2astb  / X'0001020304050607', X'08090A0B0C0D0E0F',  
     1                      X'1011121314151617', X'18191A1B1C1D1E1F',
     2                      X'2021222324252627', X'28292A2B2C2D2E2F',
     3                      X'3031323334353637', X'38393A3B3C3D3E3F',
     4                      X'2023232323232323', X'2323232E3C282B7C',
     5                      X'2623232323232323', X'232321002A293B5E',
     6                      X'2D2F232323232323', X'23237C2C255F3E3F',
     7                      X'2323232323232323', X'23603A2340273D22',
     8                      X'2361626364656667', X'6869232323232323',
     9                      X'236A6B6C6D6E6F70', X'7172232323232323',
     A                      X'237E737475767778', X'797A232323232323',
     B                      X'2323232323232323', X'2323232323232323',
     C                      X'7B41424344454647', X'4849232323232323',
     D                      X'7D4A4B4C4D4E4F50', X'5152232323232323',
     E                      X'5C23535455565758', X'595A232323232323',
     F                      X'3031323334353637', X'3839232323232323' /

       CHARACTER*1 CEB2ASTB(256)
       EQUIVALENCE (KEB2ASTB(1),CEB2ASTB(1))             

       character*1    lonechar

       INTEGER        inx


       
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... To translate from EBCDIC to ASCII;  
C      ... that table translates any UNEXPECTED ASCII byte to '#';
C      ... and move from lonechar to c1eb2as

         inx = 1 + mova2i(lonechar)
         c1eb2as = CEB2ASTB(INX)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       return
       end
