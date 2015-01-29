       character*(*) function chr2hex(cstrng)
C                                                      1-Jun-1995/dss
C      ... To convert given source character string (cstrng) 
C      ... into a HEXADECIMAL character string.
C
C      ... Caution: this limits given cstrng size to a max of 256 chars
C      ... Caution: The destination character string will need twice
C                   the space of the source string, since each 
C                   given character will be converted into two
C                   hexadecimal characters.
C
c      ... Caution: I will blank out to the limit of chr2hex(). 
C      ... Krishna Kumar converted this code from CRAY to IBM RS/6000
C
C      . . . . . . . . . . .   A R G   . . . . . . . . . .
       character*(*)  cstrng
C      . . . . . . . . . . . . . . . . . . . . .

       integer        maxnchwrk
       parameter     (maxnchwrk=256)
       
       integer        lwork(maxnchwrk)
       character*8    cwork(maxnchwrk)
       equivalence   (lwork,cwork)

       character*16   chexcon
       data           chexcon     /  '0123456789ABCDEF' /
      

C      . . . .   S T A R T   . . . . . . . . . . . . . . . . .
C
C
       lmt_inp = len(cstrng)
       if(lmt_inp .LE. 0) go to 999

       lmt_out = len(chr2hex)
       if(lmt_out .LT. 2) go to 999

C      ... initialize destination string to all blanks ...
       chr2hex(1:lmt_out) = ' '

       maxnch = lmt_inp
       if(maxnch .GT. maxnchwrk) then
         maxnch = maxnchwrk
C        ... which truncates the too-long given string to work array sz
       endif

       do  ich = 1,maxnch
         numval = mova2i(cstrng(ich:ich))
         lwork(ich) = numval
         cwork(ich)(1:2) = '00'
         i = 3
         do while (numval .gt. 0)
           i = i - 1
           j = mod(numval,16)
           cwork(ich)(i:i) = chexcon(j+1:j+1)
           numval = numval / 16
         enddo
       enddo
         
       do  ich = 1,maxnch
         idest = 2*ich
         if(idest .GT. lmt_out) go to 999
         chr2hex(idest-1:idest) = cwork(ich)(1:2)
       enddo

  999  continue
       return
       end      
