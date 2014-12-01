       subroutine shlsor(iarr,n)
C      ... Shell sort ...                          28-Nov-1995/dss
C      ... Shell's diminishing increment sort ...
C      ... should be used for arrays of [50 < n < 1000]
C
C      ... for CRAY version of LABEL array in which the sort key
C      ... is in the HI-ORDER 32-bits of the 64-bit longword integer
C
C      ... REF.: p229 of "Numerical Recipes -- The Art of Scientific
C      ... Computing (FORTRAN Version); W.H.Press, B.P.Flannery,
C      ... S.A.Teukolsky, W.T.Vetterling; Cambridge Univ. Press (1989)

C      ... for a 32-bit word machine, iarr(2,n); and I do not mask

       integer    iarr(n)

       REAL       ALN2I
       parameter (ALN2I = 1.0/0.69314718)
       REAL       TINY
       parameter (TINY=1.0E-5)
 
       integer    msksork
       data       msksork    / X'FFFFFFFF00000000' /

       integer    ihold
       INTEGER    IKEY
       INTEGER    LKEY

C      ... test for n .LE. 1 ... for trivial case 
       if(n .LE. 1) then
         go to 999
       endif
C
       LOGNB2 = INT(ALOG(FLOAT(N))*ALN2I + TINY)
       m = n
       do 333  nn = 1,LOGNB2  	!... loop over partial sorts
         m = m / 2
         k = n - m
         do  222 j = 1,k   	!... Straight insertion -- outer loop
           i = j
  111      continue  		!... Straight insertion -- inner loop
           l = i + m
           LKEY = IAND(IARR(L),MSKSORK)
           IKEY = IAND(IARR(I),MSKSORK)
           if(LKEY .LT. IKEY) then
             ihold  = iarr(i)
             iarr(i) = iarr(l)
             iarr(l) = ihold

             i = i - m
             if(i .GE. 1) go to 111
           endif
  222    continue
  333  continue 
C        
  999  continue
       return
       end 
