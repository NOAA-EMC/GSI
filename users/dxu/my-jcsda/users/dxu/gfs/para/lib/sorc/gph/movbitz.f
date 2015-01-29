C
C George Vandenburg developed this routine and Krishna Kumar tested this
C routine in various graphics jobs on IBM RS/6000 1999-07-01.
C
C     This subroutine is the IBM equivalent to CRAY's MOVBITZ.
C     STRMOV, MOVBIT, MOVBITZ - Moves bytes or bits from one variable or
C     array to another.
C
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
      subroutine movbitz(x,ix,n,y,iy)
c***************************************************************************
c    SPECIAL CASE WHERE N IS INTEGRAL MULTIPLE OF
c    8 and IX  and IY ARE ALSO INTEGRAL MULTIPLES OF 8
c    WARNING:: ANSWERS WILL BE WRONG IF THESE CONDITIONS AREN'T MET
c     in caller x and y are integers and ix and n are bits 
c     divide ix and iy by 8 and add 1 to get byte address
c     Krishna Kumar found, in certain graphics codes where this condition
c     was violated, that the scanlines were shifted a little on the final
c     fax outputs. A generalized code would be developed at a later time. 
c***************************************************************************
      character*1  x(ix+n+1),y(iy+n+1)
       if ( mod(ix,8) .ne. o .or. (mod(ix,8) .ne. 0) .or.
     1 mod(n,8) .ne. 0) then
ckumar       print 1499, 'MOVBITS ASSUMPTION OF 8 BYTE SHIFT VIOLATED ',
ckumar     1 ix,iy,n
ckumar 1499 format(a50,3i10)

ckumar       stop 1999
ckumar       print*,'MAPBACKGROUND WOULD BE SHIFTED'
       endif
      
c      y(iy+1:iy+n)=x(ix+1:ix+n)
c      y(iy+1:iy+n)=x(ix+1:ix+n)
ckumar      print *,' from movbitz ', ix,n
      ixc=ix/8
      iyc=iy/8
      ixc=ixc+1
      iyc=iyc+1
      nc=n/8 
c      write(91)x(ix+1:ix+n)
c     now do the actual BYTE MOTION
      call strmovv(x,ixc,nc,y,iyc)
      return
      end
