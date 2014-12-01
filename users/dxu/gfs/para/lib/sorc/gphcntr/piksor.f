       subroutine piksor(iarr,n)
C      ... straight insertion sort ...                28-Nov-1995/dss
C      ... should only be used for small arrays of .LT. 50 items.
C
C      ... for CRAY version of LABEL array in which the sort key
C      ... is in the hi-order 32-bits of the 64-bit integer
C
C      ... REF.: p227 of "Numerical Recipes -- The Art of Scientific
C      ... Computing (FORTRAN Version); W.H.Press, B.P.Flannery,
C      ... S.A.Teukolsky, W.T.Vetterling; Cambridge Univ. Press (1989)

C      ... for a 32-bit word machine, iarr(2,n); and I would not mask 
       integer  iarr(n)

       integer  msklhs  
       data     msklhs     / X'FFFFFFFF00000000' /
       integer  ikeya,ikeyb
       integer  isrca
       integer  iofdest

C      ... test for n .LE. 1 ... for trivial case 
       if(n .LE. 1) then
         go to 999
       endif
C
       do  j = 2,n
         isrca = iarr(j)    	!... pick out each element in turn
         ikeya = iand(isrca,msklhs)
         do  i = j-1,1,-1   	!... look below (j) for where to insert
           iofdest = i
           ikeyb = iand(iarr(i),msklhs)
           if(ikeyb .LE. ikeya) then
              go to 210
           endif
C          ... otherwise, b .GT. a; so push (i)-th  up by one step 
C          ...       to open a space below
           iarr(i+1) = iarr(i)
         enddo
C        ... if it falls thru bottom of do, then isrca was smaller than
C        ...   all items below it; and a space has been opened at the
C        ...   bottom of ladder for isrca to be inserted there
         iofdest = 0
  210    continue
         iarr(iofdest+1) = isrca
       enddo

  999  continue
       return
       end 
