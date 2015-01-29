       integer   function atoi(string)
C                       FOR CRAY          10-JUL-1995/DSS
C                               version of 2-Dec-1993
C      ... to convert ascii string to integer ...
C      ... CAUTION: FAILS WITH SYSTEM-F-INTOVF, integer overflow
C      ...          IF YOU FEED IT A TOO-LARGE-FOR 32-BIT-INTEGER NUMBER
C      ... CAUTION: THE CALLING PROGRAM SHOULD DECLARE THIS FUNCTION
C      ...          WITH STATEMENT:  integer    ATOI
C      ... Caution: 2-Dec-1993 -- this is failing, so I added 
C                                 D-line comments to debug
C                   Deleted the include of global.def
C                   Put isdigit logic in-line, replacing function
C      ... Caution: Expects a NULL-terminated string.
C      Krishna Kumar converted this code to IBM/SP 6000 08/01/1999
C
C
       character*(*)  string
C
       logical        LISDIGIT
C      ... I HAVE PUT LISDIGIT IN LINE INSTEAD OF FUNCTION ...
       integer        i
       integer        il
       integer        msign
       integer        lengivn
       integer        kchzero
       character*1    chone
       LOGICAL        CHECKOUT
       DATA           CHECKOUT   / .FALSE. /
       character*1    nullchr
C
       nullchr = char(0)
C
       atoi = 0
       locnull = 0
       limitscan = 0
       lengivn = len(string)
       if(lengivn .le. 0) go to 999

       IF(CHECKOUT) THEN
         write(6,105)lengivn
  105    format(1h ,'atoi: entered with len(string)=',I5)
       ENDIF

       limitscan = lengivn
       locnull = index(string(1:lengivn),nullchr)
       if(locnull .gt. 0 .and. locnull .le. lengivn) then
         limitscan = locnull - 1
       endif
       if(limitscan .le. 0) then
         atoi = 0
         IF(CHECKOUT) THEN
           write(6,125)limitscan,locnull
  125      format(1h ,'atoi: jumping out at 125, since limitscan=',I5,
     1                '; locnull=',I5)
         ENDIF
         go to 999
       endif

       IF(CHECKOUT) THEN
         WRITE(6,205)limitscan
  205    FORMAT(1H ,'atoi: ready to scan with limitscan=',I5)
       ENDIF
       i = 1
C      ... to reposition pointer beyond leading blanks,
       do  ll = 1,LIMITSCAN
         if(string(i:i) .ne. ' ') go to 222
C        ... otherwise, i-th char is leading blank, so incr
         i = i + 1
       enddo

       if(i .gt. LIMITSCAN) go to 999
C        ... which tested for all blanks in given string
  222  continue
C      ... otherwise, i is pointing at first non-blank char w/i string,
       msign = 1
       if(string(i:i) .eq. '+' .or. string(i:i) .eq. '-') then
          if(string(i:i) .eq. '-') msign = -1
          i = i + 1
       endif
       if(i .gt. LIMITSCAN) go to 999
C      ... which tested for sign-only in given string.
C
       kchzero = mova2i('0')
       do il = i,LIMITSCAN
         chone = string(il:il)
         LISDIGIT = (LGE(CHONE,'0') .AND. LLE(CHONE,'9'))
         if(.not. LISDIGIT) THEN
           IF(CHECKOUT) THEN
             WRITE(6,315)il
  315        FORMAT(1H ,'atoi: jumping out on non-digit at il=',I5)
           ENDIF
           go to 333
C          ... which terminates scan on any non-digit character
         ELSE
           atoi = 10*atoi + (mova2i(chone) - kchzero)
         ENDIF
       enddo
  333  continue
       atoi = msign * atoi
       IF(CHECKOUT) THEN
         WRITE(6,345)atoi
  345    FORMAT(1H ,'atoi: came thru 333 w/ atoi =',I9)
       ENDIF

  999  continue
       return
       end
