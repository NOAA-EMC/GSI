       SUBROUTINE PLTASYM(IPL,JPL,LONECHAR,ICHSET,IROTSPEC,LCKPRNTQ,
     1                    LSRNHEMI,IMAGE,MAXIWORD,MAXJSLINE)
C                                                   28-OCT-1996/DSS
C      ... ADDED LSRNHEMI TO CALL SEQUENCE IN ORDER TO MIRROR-IMAGE
C      ... THE SOUTHERN HEMISPHERE WIND-FLAG; AND
C      ... CHANGED TO CALL SYM2IMGE() WHICH HAS A RETURN CODE;
C
C                                                   28-Mar-1996/dss
C      ... correcting for case of eraser which has a character
C      ... for look-up of an ASCII "A" in new CRAY version, 
C      ... when it had a X'01' in EBCDIC version; so fix is a
C      ... test for char set 20 to skip the look-up.
C
C                                                   19-MAR-1996/DSS

       EXTERNAL  CHINDX
       EXTERNAL  LOOKTLB
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C ...       EXTERNAL  LOOKTLB
       COMMON   /LKTLBS/ LMTSETNUM,LOOKT
       INTEGER   LMTSETNUM
       INTEGER   LOOKT(9,63)
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C ...       EXTERNAL  CHINDX
       COMMON /CHINDEX/LMTNFONTS,NCDEFSPFONT,KIXALL
       INTEGER        LMTNFONTS
       INTEGER        NCDEFSPFONT(63)
       INTEGER        KIXALL(8,63)
       CHARACTER*64  CHIXALL(63)
       EQUIVALENCE   (KIXALL(1,1),CHIXALL(1))

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       COMMON    / PLTSTATS / NITEMPLTED,NSYMPLT
       INTEGER                NITEMPLTED
       INTEGER                NSYMPLT(4,63)    	!... (IROTAT,ICHSET)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... SUBR PLTASYM() CALL SEQUENCE:

       INTEGER     IPL,JPL
       CHARACTER*1 LONECHAR
       INTEGER     ICHSET
       INTEGER     IROTSPEC
       LOGICAL     LCKPRNTQ    		! ... => .T. IF YOU WANT PRINT
       LOGICAL     LSRNHEMI		! ... => .T. WANT TO MIRROR
       INTEGER     IMAGE(MAXIWORD,MAXJSLINE)
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       INTEGER     IROTRA(32)

       INTEGER      LONECHDEF(16)   	!... where 16 longwords is limit
       character*1  conechdef(128)
       equivalence (lonechdef(1),conechdef(1))
       LOGICAL      LERASE

       INTEGER      NEWPXLWID
       INTEGER      NEWPXLHGT
       INTEGER      IRET_S2I

       SAVE
       
C          ... scan for a match in the chixall array
           if((ichset .GT. 0) .AND.
     1        (ichset .LE. LMTNFONTS)) then
C            ... this font is within range of my fontlib ...

             if(ichset .EQ. 20) then
C              ... special case of 30x30 eraser, so 
C              ... zero the bit-mapped chardef work array

                 LERASE = .TRUE.
                 NEWPXLHGT = IABS(LOOKT(3,ICHSET))
                 NEWPXLWID = IABS(LOOKT(8,ICHSET))
                 DO  J = 1,NEWPXLHGT
                   IROTRA(J) = 0
                 ENDDO

C                ... (IROTRA(I),I=1,NEWPXLHGT) CONTAINS THE LEFT-JUSTIF
C                ...                           PIXELS FOR EACH SCANLINE;
C                ... (NEWPXLWID = COUNT OF PIXELS LEFT-JUSTIFIED)
C                ... SO, SHOULD PLACE THESE INTO IMAGE PLANE
C                ... AT POINT(IPL,JPL)

                CALL SYM2IMGE(IPL,JPL,IROTRA,NEWPXLHGT,NEWPXLWID,
     1                        LERASE,IMAGE,MAXIWORD,MAXJSLINE,IRET_S2I)
                go to 999
             endif
               
C            . . . . . . . . . . . . . . . . . . . . . . . . . . .
             maxchkix = ncdefspfont(ichset)
             if(maxchkix .GT. 0) then

               ixfound = index(chixall(ichset)(1:maxchkix),lonechar)

               if(ixfound .NE. 0) then
                 IF(LCKPRNTQ) THEN
C                  ... print the results ...
                   write(6,FMT='(1h ,''PLTASYM: Found "'',A,
     1                       ''" at index='',I4,''; in font no.'',I4)')
     A                     lonechar(1:1),ixfound,ichset
                 ENDIF
C                ... from this info, I want the bitstream font def of
C                ... that one char; 

                 call get1chdf(ichset,ixfound,LCKPRNTQ,lonechdef,
     1                         iret_get)
                 if(iret_get .ne. 0) then
C                  ... Failed in get1chdf ...
                   if(lckprntq) then
                     write(6,FMT='(1h ,''PLTASYM::GET1CHDF: FAILED '',
     1                                 ''with iret_get='',I3,
     2                                 '' = = = = = = ='')')
     A                       iret_get
                   endif
                   go to 999   		!... EXIT on get1chdf FAILURE
                 endif

                 nbytsymb  = lookt(7,ichset)
                 lenlinpxl = lookt(8,ichset)
                 kwidth    = (lookt(4,ichset))
                 if(kwidth .LT. 0) then
C                  ... negative-valued kwidth, so ...
                   LERASE = .FALSE.     	!... .OR. w/o erasing
                 else
C                  ... positive-valued kwidth, so ...
                   LERASE = .TRUE.  		!... erase under
                 endif
                 lenlinbyt = iabs(kwidth)
                 kheight   = iabs(lookt(3,ichset))
                 nwrdsymb  = lookt(9,ichset)
C                ===================================
                 IF(LCKPRNTQ) THEN

                     iorsum = 0
                     do  jwr = 1,nwrdsymb
                       iacc = lonechdef(jwr)
                       iorsum = ior(iorsum,iacc)
                     enddo
                     if(iorsum .eq. 0) then
                       write(6,FMT='(1h ,''PLTASYM: lonechdef from '',
     1                                   ''GET1CHDEF() is all zero '',
     2                                   '' . . . . . . .'')')
                     ENDIF
                 ENDIF
C                ===================================         
                 iquad = IROTSPEC

                 call irotsymb(conechdef,nbytsymb,lenlinpxl,lenlinbyt,
     1                kheight,iquad,irotra,newpxlwid,newpxlhgt,iret_rot)

                 if(iret_rot .NE. 0) then
                   write(6,FMT='(1h ,''PLTASYM::irotsymb: FAILED '',
     1                               ''with return code='',I3)')
     A                     iret_rot
                   go to 999
                 endif
C                ... OTHERWISE, SUCCESSFUL RETURN FROM IROTSYMB

                 IQ4 = MOD(IQUAD,4) + 1
                 NSYMPLT(IQ4,ICHSET) = NSYMPLT(IQ4,ICHSET) + 1
C                ... WHICH COUNT BOX IS BECAUSE I AM LOSING ROTATION
C                ... the stats show that IQUAD = 0; in every case
C                ... at this point in here


                 IF(LCKPRNTQ) THEN

C                  write(6,FMT='(1h ,''PLTASYM::irotsymb: iquad='',I3,
C    1                             '';  iret_rot='',I3,''; new wid/'',
C    2                             ''hgt=('',I4,'' X'',I4,'')'')')
C    A                     iquad,iret_rot,newpxlwid,newpxlhgt
C                  ... FAILED IN FOLLOWING DO LOOP BECAUSE NEWPXLHGT BAD
                   m2 = newpxlhgt
                   if((m2 .gt. 0) .and.
     1                (m2 .le. 32)) then

                     iorsum = 0
                     do  jrow=1,m2
                       iacc = irotra(jrow)
                       iacc = ishft(iacc,-32)
                       iorsum = ior(iorsum,iacc)
                     enddo
                     if(iorsum .eq. 0) then
C                      write(6,FMT='(1h ,''PLTASYM: irotra from '',
C    1                                   ''IROTSYMB() is all zero '',
C    2                                   ''* * * * * * *'')')
                     else
                       do  jrow=1,m2
                         iacc = irotra(jrow)
                         iacc = ishft(iacc,-32)
C                        write(6,FMT='(1h ,''PLTASYM:  jrow='',I4,
C    1                                     '':  '',Z8.8)')
C    A                           jrow,iacc
                       enddo
                     endif

                   endif
                 ENDIF
C                ... (IROTRA(I),I=1,NEWPXLHGT) CONTAINS THE LEFT-JUSTIF
C                ...                           PIXELS FOR EACH SCANLINE;
C                ... (NEWPXLWID = COUNT OF PIXELS LEFT-JUSTIFIED)
C                ...    insert southern hemi wind-flag reversing here ..
                 IF(LSRNHEMI) THEN
C                  ... FOR SOUTHERN HEMISPHERE WIND-FLAGS, MIRROR IT ...
                   DO  J = 1,NEWPXLHGT
                     IROTRA(J) = IRVR(IROTRA(J),NEWPXLWID)
                   ENDDO
                 ENDIF


C                ... SO, SHOULD PLACE THESE INTO IMAGE PLANE
C                ... AT POINT(IPL,JPL)
C                ... WHERE IS THE IMAGE PLANE?

                CALL SYM2IMGE(IPL,JPL,IROTRA,NEWPXLHGT,NEWPXLWID,
     1                        LERASE,IMAGE,MAXIWORD,MAXJSLINE,IRET_S2I)

               endif
             endif
           endif

  999  CONTINUE
       return
       END
