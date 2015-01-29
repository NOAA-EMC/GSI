C
C George Vandenburg developed this routine for IBM RS/6000.
C This routine is the IBM equivalent of function mask on 
C CRAY system.
C
C Krishna Kumar tested this routine on IBM RS/6000 and added
C to the directory /nwprod/gphlib90/gphlib.source 1999/07/01
C
C FORTRAN DESCRIPTION FROM CRAY
C     The MASK function returns a bit mask of 1's.
C
C     On UNICOS systems, the argument is a 64-bit integer.
C
C     On UNICOS/mk systems, the argument is a 32-bit or 64-bit integer.
C
C     On IRIX systems, the argument is an 8-bit, 16-bit, 32-bit, or 64-bit
C     integer.
C
       integer (kind=8)  function   mask(i)
       integer (kind=4) isb,il,itb

       integer (kind=8) iv,iv3,iv2
        data iv/z'0000000000000000'/
        data iv2/z'0000000000000001'/
       iv3=iv
      isb=0
      il=1
       do 10 k=1,i
       itb=64-k
       call mvbits(iv2,isb,il,iv3,itb)
 10    continue
       mask=iv3
       return
       end 
C
