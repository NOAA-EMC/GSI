       subroutine strpfnam(lwr_or_upr,c1name,nc,cnamtxt,nchres,iretn)
C      ... filename stripper                            29-Aug-1995/dss
C      ...    to extract the innermost name, without the extent name
C      ...    without the pathname.
C      ... copied ~/tools/stripfinam.f into ~/cra/bg/strpfnam.f
C      ...    in order to make a CRAY version.
C
C      ... called from SIF cmd84A to strip the given filename of
C      ...   superfluous info and extract only the unqualified name.
C      Hypotheses:  SIF cmd84A wants the unqualified filename
C          with no extent; no directory names; no leading nor
C          trailing blanks; limited to 12 alphanumeric characters
C          of which only the first 9 characters will actually be
C          used.  Note: only alphanumerics are allowed.  No underbars.
C         (Underbar permitted in CRAY version of 950829)
C          Also, this subroutine will set the alphanumerics to upper-
C          case.
C         (Case change is an option in CRAY version of 950829.
C          lwr_or_upr =0  for no change in Case;
C                     =1  change to Lower Case;
C                     =2  change to Upper Case )
C
C      ... return code = 0  for normal return
C                      = 1  if text string for results is undefined
C                      = 2  if nc dimension of given string is <=0
C                      = 3  if given text is all blanks
C                      = 4  if given text is bad. NULL before any good.
C                              did you put anything into c1name?
C                      = 5  if I am lost.  impossible.
C                      = 6  if given text has no good portion in it.
C                              did you end with a special character?
C
C      ... calls on logical function isalpha(c)
       external       isalpha
       logical        isalpha
C      ... for logical function isalpha ...

C
       integer        lwr_or_upr    	!...=1 lwr; =2 upr; else unchgd
       character*1    c1name(nc)
       character*(*)  cnamtxt
       integer        iretn
C
       logical        lfoundext
       character*1    chone
C
       character*1    underbar
       data           underbar   / '_' /

       character*1    NULL
C
       NULL = char(0)

       iretn = 0
       nchres = 0
       limres = len(cnamtxt)
       if(limres .le. 0) then
         iretn = 1
         go to 999
       endif
C
       cnamtxt = NULL
       limgivn = nc
       if(limgivn .le. 0) then
         iretn = 2
         go to 999
       endif
C
C      ... scan given string for any leading blanks ...
       m1 = 1
       do  i = 1,limgivn
         chone = c1name(i)
         if(chone .eq. NULL) go to 900
         if(chone .ne. ' ') go to 200
C          ... which jumps out of this loop on very first non-blank char
C          ... otherwise, this ith char is a leading blank,
         m1 = i + 1
       enddo
C      ... if it falls thru this do, then given string was all blanks
       iretn = 3
       go to 999
C      ... otherwise, first non-blank char at c1name(m1)
  200  continue
C      .. when it comes here, m1 points to first non-blank char in c1name
C      ...  so scan for other end of string that starts at c1name(m1)
       m2 = m1
       do  i = m1,limgivn
         chone = c1name(i)
         if(chone .eq. NULL .or. chone .eq. ' ') go to 300
C          ... normal end of string if NULL or blank terminated, 
C          ...    m2 pointing at last good char before the terminator
C          ... otherwise, this ith char is non-blank non-NULL
         m2 = i
       enddo
C      ... if it falls thru this do, then given string fill out to 
C      ...   limgivn with non-blank characters
  300  continue
C      ... examine the string from c1name(m1) to (m2) for good stuff
       ncha = m2 - m1 + 1
       if(ncha .le. 0) go to 910
C        ... logic error.  cannot be
C
C      ... look for extent_name behind the last period
       lfoundext = .false.
       locstart = m2
       locterm = m2 + 1
       do  ir = m2,m1,-1
         chone = c1name(ir)
         if((isalpha(chone)) .OR.
     1      (chone .EQ. underbar)) then
C          ... good one ...
           locstart = ir

         else
C          ... here is the non-alphanumeric I was afraid I'd find
           if (.not. lfoundext) then
             if (chone .eq. '.') then
               lfoundext = .true.
               locterm = ir
C              ... where locterm points at the '.' before the extent
             else
C              ... the special char is not a '.', so terminate scan
               locstart = ir + 1
               go to 400
             endif
           else
C            ... comes here on a special char after I had already
C            ...   reset for extent name, so terminate scan
             locstart = ir + 1
             go to 400
           endif
         endif
       enddo
  400  continue
C      ... comes here with good stuff bounded by locstart:(locterm-1)
       locstop = locterm - 1
       ngood = locterm - locstart
       if(ngood .le. 0) go to 920
C      ... can i pass all the good stuff to results string?
C      ...    n2do  is  lesser of limres and ngood and 12

       n2do = min(ngood,limres,12)
       do  i = 1,n2do
         ll = locstart + i - 1
         chone = c1name(ll)
         if(lwr_or_upr .EQ. 1) then
C           ... lower_case it ...
           if(llt(chone,'A') .or. lgt(chone,'Z')) then
             cnamtxt(i:i) = chone
           else
C            ... chone is an Upper_case ltr, so lower it,
             cnamtxt(i:i) = char(mova2i('a') - mova2i('A') + 
     1                           mova2i(chone))
           endif
         else if(lwr_or_upr .EQ. 2) then
C           ... upper_case it ...
           if(lge(chone,'a') .and. lle(chone,'z')) then
             cnamtxt(i:i) = char(mova2i('A') - mova2i('a') + 
     1                           mova2i(chone))
           else
             cnamtxt(i:i) = chone
           endif
         else
C           ... leave _case unchanged ...
             cnamtxt(i:i) = chone
         endif 
       enddo
       nchres = n2do
       if(n2do .lt. limres) then
         cnamtxt(n2do+1:n2do+1) = NULL
C        ... which puts a NULL terminator if there is space ...
       endif
       go to  999
C
  900  continue
       iretn = 4
       go to 999
  910  continue
       iretn = 5
       go to 999
  920  continue
       iretn = 6
       go to 999
C
  999  continue
       return
       end


 
