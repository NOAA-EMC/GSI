        integer function lastch(str)
C       ... lastch() ... to point to the last character of a character
C       ...              string
C       ...        String terminators are first BLANK or NULL character
C       ...        encountered.
C       ... Caution:  I will limit scan on LEN(str)
C                     so you must give me a character string.
C
   
        character*(*) str

        character*1  NULLCHR
        character*1  BLANK
C
        integer*4    i
        integer*4    limit
C
        NULLCHR = char(0)
        BLANK = ' '
        limit = len(str)
        i = 0
        do while(i .LT. limit .AND. str(i+1:i+1) .NE. NULLCHR
     1                        .AND. str(i+1:i+1) .NE. BLANK)
           i = i + 1
        enddo

        lastch = i
        return
        end
