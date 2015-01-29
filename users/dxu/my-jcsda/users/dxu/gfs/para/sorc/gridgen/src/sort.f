 subroutine sort (x, iy, n)

!-----------------------------------------------------------------------
!    Example of a Selection Sort   Using a Fortran 90 Intrinsic Function
!
!***PURPOSE  Sort an array and make the same interchanges in
!            an auxiliary array.  The array is sorted in
!            decreasing order.
!
!   Description of Parameters
!      X - array of values to be sorted   (usually abscissas)
!      IY - array to be carried with X (all swaps of X elements are
!          matched in IY .  After the sort IY(J) contains the original
!          postition of the value X(J) in the unsorted X array.
!      N - number of values in array X to be sorted
!      KFLAG - Not used in this implementation
!
!-----------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER                 :: I 
 INTEGER                 :: ISWAP(1)
 INTEGER                 :: ISWAP1
 INTEGER                 :: ITEMP
 INTEGER                 :: IY(N)
 INTEGER                 :: KFLAG
 integer                 :: N

 REAL*4                  :: TEMP
 REAL*4                  :: X(1:N)

 DO I=1,N-1
   ISWAP=MAXLOC(X(I:N))
   ISWAP1=ISWAP(1)+I-1
   IF(ISWAP1.NE.I) THEN
     TEMP=X(I)
     X(I)=X(ISWAP1)
     X(ISWAP1)=TEMP
     ITEMP=IY(I)
     IY(I)=IY(ISWAP1)
     IY(ISWAP1)=ITEMP
   ENDIF
 ENDDO 

 return

 end subroutine sort
