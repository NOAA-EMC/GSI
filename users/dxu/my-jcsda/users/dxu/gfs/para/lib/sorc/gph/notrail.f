        integer*4 function notrail(str)
C       ...       mods for CRAY version               8-Dec-1994/dss
C
C       ... notrail() ... to point to the last non-blank character of a 
C       ...               character string (which can have leading
C                         blanks and intermediate blanks); but after 
C                         ignoring all trailing blank characters.
C       ...        String terminators are last BLANK or first NULL 
C       ...        character encountered.
C
C       ...        This differs from LASTCH() which stops on first
C       ...        BLANK encountered when scanning from the start;
C       ...        NOTRAIL() will scan backwards from the end of the 
C       ...        string, skipping over trailing blanks, until the
C       ...        last non-blank character is hit.
C       ...       
C       ... Caution:  I will limit scan on LEN(str)
C                     so you must give me a character string.
C
   
        character*(*) str

        character*1  BLANK
        parameter   (BLANK = ' ')
C
        integer*4    i
        integer*4    limit
        integer*4    limitnl
        character*1  NULLCHR
C
        NULLCHR = char(0)
        i = 0
        limitnl = 0
        limit = len(str)
        if(limit .le. 0) go to 999
C       ... otherwise, at least one char len string ...
        limitnl = index(str(1:limit),NULLCHR)
        if(limitnl .le. 0) then
C         ... no NULLCHR exists in str(1:limit) ...
C         ... so go scan from limit
          go to 300
  
        else if(limitnl .eq. 1) then
          go to 999
C         ... which jumped out w/ pointer=0 if NULL in first position
        else
C         ... a NULLCHR existed within str(1:limit); so
C         ...   I want to scan backwards from before that NULLCHR
C         ...   which is located at limitnl
          limit = limitnl - 1
        endif
        if(limit .le. 0) go to 999
  300   continue
C       ... otherwise, we have a string of at least one char to look at
C       ... which has no NULLCHR in interval (1:limit)
        i = limit
        do while((i .GT. 0) .AND. (str(i:i) .EQ. BLANK))
           i = i - 1
        enddo

  999   continue
        notrail = i
        return
        end
