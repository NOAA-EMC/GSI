C
C George Vandenburg developed this routine and Krishna Kumar tested this
C routine in the graphics library on IBM RS/6000 1999-07-01.
C
C     This subroutine is the IBM equivalent to CRAY's STRMOV.
C     STRMOV, MOVBIT, MOVBITZ - Moves bytes or bits from one variable or
C     array to another.
C     STRMOV moves bytes from one variable or array to another.  MOVBIT
C     moves bits from one variable or array to another.
C
C     The default kind is KIND=8 for integer, real,
C     complex, and logical arguments.
C     The following is a list of valid arguments for this routine.
C
C     src       Variable or array of any type except character, and of any
C               length, containing the bytes or string of bits to be moved.
C
C     isb       Starting byte or bit in the src string.  Specify an integer
C               variable, expression, or constant greater than 0.  Bytes and
C               bits are numbered from 1, beginning at the leftmost byte or
C               bit position of src.  isb is one-based for STRMOV and MOVBIT
C               and zero-based for MOVBITZ.
C
C     num       An integer variable, expression, or constant that contains
C               the number of bytes or bits to be moved; it must be greater
C               than 0.
C
C     dest      Variable or array of any type except character, and of any
C               length, that contains the starting byte or bit to receive
C               the data.
C
C     idb       An integer variable, expression, or constant that contains
C               the starting byte or bit to receive the data; must be
C               greater than 0.  Bytes and bits are numbered from 1,
C               beginning at the leftmost byte or bit position of dest.  idb
C               is one-based for STRMOV and MOVBIT and zero-based for
C               MOVBITZ.
C
      subroutine strmovv(x,ix,n,y,iy)
      character*1  x(ix+n),y(iy+n)
ckumar      print *,' from strmov  ix and n ',ix,n
      do 10 k=1,n
      y(iy+k-1)=x(ix+k-1)
 10    continue
      return
      end
C
