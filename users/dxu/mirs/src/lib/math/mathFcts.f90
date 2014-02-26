!$Id: mathFcts.f90 2974 2012-04-13 20:05:32Z kgarrett $
!-----------------------------------------------------------------------------------------------
! Name:         MathFcts
! 
! Type:         F90 module
!
! Description:
!       This module contains mathematical fcts/procedures that are not 
!       standard in Fortran.
!
! Modules needed:
!       - none
!
! Subroutines contained:
!       - matinv_dbl
!       - matinv_sgl
!
! Data type included:
!       - none
! 
! History:
!       2006   S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE MathFcts
  USE ErrorHandling
  USE type_kinds
  IMPLICIT NONE
  PRIVATE
  !---Publicly available subroutine
  PUBLIC :: matinv_dbl,matinv_sgl,SFFTCF,SFFTCB
  !---Publicly available data/type definitions
  !---INTRINSIC functions used in this module
  INTRINSIC :: ABS,MAXLOC,SELECTED_REAL_KIND,SIZE,SNGL,DBLE,REAL

CONTAINS

!===============================================================
! Name:          matinv_dbl
!
!
! Type:          Function
!
!
! Description:  Inverts a matrix using double precision
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - A                  I              Matrix to invert
!       - matinv_dbl         O              Inverted matrix
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION matinv_dbl(A)
    ! Invert matrix by Gauss method
    ! --------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: n
    REAL( SELECTED_REAL_KIND(15) ), intent(in),dimension(:,:) :: a
    REAL( SELECTED_REAL_KIND(15) ), dimension(size(a,1),size(a,2)) :: b
    REAL( SELECTED_REAL_KIND(15) ), dimension(size(a,1),size(a,2)) :: matinv_dbl
    REAL( SELECTED_REAL_KIND(15) ), dimension(size(a,1)) :: temp
    ! - - - Local Variables - - -
    REAL( SELECTED_REAL_KIND(15) ) :: c, d
    INTEGER :: i, j, k, m, imax(1), ipvt(size(a,1))
    ! - - - - - - - - - - - - - -
    b = a
    n=size(a,1)
    matinv_dbl=a
    ipvt = (/ (i, i = 1, n) /)
    ! Go into loop- b, the inverse, is initialized equal to a above
    DO k = 1,n
       ! Find the largest value and position of that value
       imax = MAXLOC(ABS(b(k:n,k)))
       m = k-1+imax(1)
       !   sigular matrix check
       if(ABS(b(m,k)).LE.(1.D-40)) then
          !CALL ErrHandl(ErrorType,Err_SingularMatrx,'')
          matinv_dbl(1,1) = -99999999.0
          return 
       ENDIF
       ! get the row beneath the current row if the current row will
       ! not compute
       IF (m .ne. k) THEN
          ipvt( (/m,k/) ) = ipvt( (/k,m/) )
          b((/m,k/),:) = b((/k,m/),:)
       END IF
       ! d is a coefficient - brings the pivot value to one and then is applied
       ! to the rest of the row
       d = 1/b(k,k)
       temp = b(:,k)
       DO j = 1, n
          c = b(k,j)*d
          b(:,j) = b(:,j)-temp*c
          b(k,j) = c
       END DO
       b(:,k) = temp*(-d)
       b(k,k) = d
    END DO
    matinv_dbl(:,ipvt) = b
  END FUNCTION matinv_dbl


!===============================================================
! Name:        matinv_sgl
!
!
! Type:         Function
!
!
! Description:  Inverts a matrix using single precision
!
!
! Arguments:
!
!      Name                Type             Description
!      ---------------------------------------------------
!       - A                  I              Matrix to invert
!       - matinv_sgl         O              Inverted matrix
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  FUNCTION matinv_sgl(A)
    ! Invert matrix by Gauss method (single precision version)
    ! --------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER :: n
    REAL,   intent(in),dimension(:,:)      :: a
    REAL,   dimension(size(a,1),size(a,2)) :: matinv_sgl
    REAL( SELECTED_REAL_KIND(15) ), dimension(size(a,1),size(a,2)) :: b
    REAL( SELECTED_REAL_KIND(15) ), dimension(size(a,1)) :: temp

    ! - - - Local Variables - - -
    REAL( SELECTED_REAL_KIND(15) ) :: c, d
    INTEGER :: i, j, k, m, imax(1), ipvt(size(a,1))
    ! - - - - - - - - - - - - - -
    b = dble(a)
    n=size(a,1)
    matinv_sgl=a
    ipvt = (/ (i, i = 1, n) /)
    ! Go into loop- b, the inverse, is initialized equal to a above
    DO k = 1,n
       ! Find the largest value and position of that value
       imax = MAXLOC(ABS(b(k:n,k)))
       m = k-1+imax(1)
       !   sigular matrix check
       if(ABS(b(m,k)).LE.(1.D-40)) then
          !CALL ErrHandl(ErrorType,Err_SingularMatrx,'')
          matinv_sgl(1,1)=-99999999.0
          return 
       ENDIF
       ! get the row beneath the current row if the current row will
       ! not compute
       IF (m .ne. k) THEN
          ipvt( (/m,k/) ) = ipvt( (/k,m/) )
          b((/m,k/),:) = b((/k,m/),:)
       END IF
       ! d is a coefficient - brings the pivot value to one and then is applied
       ! to the rest of the row
       d = 1/b(k,k)
       temp = b(:,k)
       DO j = 1, n
          c = b(k,j)*d
          b(:,j) = b(:,j)-temp*c
          b(k,j) = c
       END DO
       b(:,k) = temp*(-d)
       b(k,k) = d
    END DO
    matinv_sgl(:,ipvt) = REAL(b)
  END FUNCTION matinv_sgl


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  A real-valued, in place, split-radix FFT program
!  Real input and output in data array X
!  Length is N = 2 ** M
!  Decimation-in-time, cos/sin in second loop
!  Output in order:
!         [ Re(0), Re(1), ..., Re(N/2), Im(N/2-1), ..., Im(1) ]
!
!  This FFT computes
!     X(k) = sum_{j=0}^{N-1} x(j)*exp(-2ijk*pi/N)
!
!
!  H.V. Sorensen, Rice University, Oct. 1985
!
!  Reference:  H.V. Sorensen, D.L. Jones, M.T. Heideman, & C.S. Burrus;
!              Real-Valued Fast Fourier Transform Algorithms; IEEE
!              Trans. Acoust., Speech, Signal Process.; Vol ASSP-35,
!              June 1987, pp. 849-863.
!
!  This code was originally named RVFFT.
!
!  History:
!   21/11/2011 Converted to something resembling f90.   A.Collard
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE SFFTCF( X, N, M )

      IMPLICIT NONE

! ... Parameters ...
      REAL(DOUBLE), PARAMETER :: SQRT2 = 1.4142135623730950488
      REAL(DOUBLE), PARAMETER :: TWOPI = 6.2831853071795864769 

! ... Scalar arguments ...
      INTEGER(LONG), INTENT(IN) :: N, M
! ... Array arguments ...
      REAL(DOUBLE), INTENT(INOUT) ::  X(N)
! ... Local scalars ...
      INTEGER(LONG)  J, I, K, IS, ID, I0, I1, I2, I3, I4, I5, I6, I7, I8
      INTEGER(LONG)  N1, N2, N4, N8
      REAL(DOUBLE)  XT, R1, T1, T2, T3, T4, T5, T6
      REAL(DOUBLE)  A, A3, E, CC1, SS1, CC3, SS3
!
! ... Exe. statements ...
!
      IF ( N .EQ. 1 ) RETURN
!
 100  J = 1
      N1 = N - 1
      DO 104, I = 1, N1
         IF ( I .GE. J ) GOTO 101
         XT = X(J)
         X(J) = X(I)
         X(I) = XT
 101     K = N / 2
 102     IF ( K .GE. J ) GOTO 103
            J = J - K
            K = K / 2
            GOTO 102
 103     J = J + K
 104  CONTINUE
! 
      IS = 1
      ID = 4
 70   DO 60, I0 = IS, N, ID
         I1 = I0 + 1
         R1 = X(I0)
         X(I0) = R1 + X(I1)
         X(I1) = R1 - X(I1)
 60   CONTINUE
      IS = 2 * ID - 1
      ID = 4 * ID
      IF ( IS .LT. N ) GOTO 70
!
      N2 = 2
      DO 10, K = 2, M
         N2 = N2 * 2
         N4 = N2 / 4
         N8 = N2 / 8
         E = TWOPI / N2
         IS = 0
         ID = N2 * 2
 40      DO 38, I = IS, N-1, ID
            I1 = I + 1
            I2 = I1 + N4
            I3 = I2 + N4
            I4 = I3 + N4
            T1 = X(I4) + X(I3)
            X(I4) = X(I4) - X(I3)
            X(I3) = X(I1) - T1
            X(I1) = X(I1) + T1
            IF ( N4 .EQ. 1 ) GOTO 38
            I1 = I1 + N8
            I2 = I2 + N8
            I3 = I3 + N8
            I4 = I4 + N8
            T1 = ( X(I3) + X(I4) ) / SQRT2
            T2 = ( X(I3) - X(I4) ) / SQRT2
            X(I4) = X(I2) - T1
            X(I3) = - X(I2) - T1
            X(I2) = X(I1) - T2
            X(I1) = X(I1) + T2
 38      CONTINUE
         IS = 2 * ID - N2
         ID = 4 * ID
         IF ( IS .LT. N ) GOTO 40
         A = E
         DO 32, J = 2, N8
            A3 = 3 * A
            CC1 = COS(A)
            SS1 = SIN(A)
            CC3 = COS(A3)
            SS3 = SIN(A3)
            A = J * E
            IS = 0
            ID = 2 * N2
 36         DO 30, I = IS, N-1, ID
               I1 = I + J
               I2 = I1 + N4
               I3 = I2 + N4
               I4 = I3 + N4
               I5 = I + N4 - J + 2
               I6 = I5 + N4
               I7 = I6 + N4
               I8 = I7 + N4
               T1 = X(I3) * CC1 + X(I7) * SS1
               T2 = X(I7) * CC1 - X(I3) * SS1
               T3 = X(I4) * CC3 + X(I8) * SS3
               T4 = X(I8) * CC3 - X(I4) * SS3
               T5 = T1 + T3
               T6 = T2 + T4
               T3 = T1 - T3
               T4 = T2 - T4
               T2 = X(I6) + T6
               X(I3) = T6 - X(I6)
               X(I8) = T2
               T2 = X(I2) - T3
               X(I7) = - X(I2) - T3
               X(I4) = T2
               T1 = X(I1) + T5
               X(I6) = X(I1) - T5
               X(I1) = T1
               T1 = X(I5) + T4
               X(I5) = X(I5) - T4
               X(I2) = T1
 30         CONTINUE
            IS = 2 * ID - N2
            ID = 4 * ID
            IF ( IS .LT. N ) GOTO 36
 32      CONTINUE
 10   CONTINUE
      RETURN
!
! ... End of subroutine SFFTCF ...
!
END SUBROUTINE SFFTCF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  A real-valued, in place, split-radix IFFT program
!  Hermitian symmetric input and real output in array X
!  Length is N = 2 ** M
!  Decimation-in-frequency, cos/sin in second loop
!  Input order:
!         [ Re(0), Re(1), ..., Re(N/2), Im(N/2-1), ..., Im(1) ]
!
!  This FFT computes
!     x(j) = (1/N) * sum_{k=0}^{N-1} X(k)*exp(2ijk*pi/N)
!
!
!  H.V. Sorensen, Rice University, Nov. 1985
!
!  Reference:  H.V. Sorensen, D.L. Jones, M.T. Heideman, & C.S. Burrus;
!              Real-Valued Fast Fourier Transform Algorithms; IEEE
!              Trans. Acoust., Speech, Signal Process.; Vol ASSP-35,
!              June 1987, pp. 849-863.
!
!  This code was originally named IRVFFT.
!
!  History:
!   21/11/2011 Converted to something resembling f90.   A.Collard
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE SFFTCB( X, N, M )

      IMPLICIT NONE

! ... Parameters ...
      REAL(DOUBLE), PARAMETER :: SQRT2 = 1.4142135623730950488
      REAL(DOUBLE), PARAMETER :: TWOPI = 6.2831853071795864769 

! ... Scalar arguments ...
      INTEGER(LONG), INTENT(IN) :: N, M
! ... Array arguments ...
      REAL(DOUBLE), INTENT(INOUT) ::  X(N)
! ... Local scalars ...
      INTEGER(LONG)  J, I, K, IS, ID, I0, I1, I2, I3, I4, I5, I6, I7, I8
      INTEGER(LONG)  N1, N2, N4, N8
      REAL(DOUBLE)  XT, R1, T1, T2, T3, T4, T5
      REAL(DOUBLE)  A, A3, E, CC1, SS1, CC3, SS3
!
! ... Exe. statements ...
!
      IF ( N .EQ. 1 ) RETURN
!
      N2 = 2 * N
      DO 10, K = 1, M-1
         IS = 0
         ID = N2
         N2 = N2 / 2
         N4 = N2 / 4
         N8 = N4 / 2
         E = TWOPI / N2
 17      DO 15, I = IS, N-1, ID
            I1 = I + 1
            I2 = I1 + N4
            I3 = I2 + N4
            I4 = I3 + N4
            T1 = X(I1) - X(I3)
            X(I1) = X(I1) + X(I3)
            X(I2) = 2 * X(I2)
            X(I3) = T1 - 2 * X(I4)
            X(I4) = T1 + 2 * X(I4)
            IF ( N4 .EQ. 1 ) GOTO 15
            I1 = I1 + N8
            I2 = I2 + N8
            I3 = I3 + N8
            I4 = I4 + N8
            T1 = ( X(I2) - X(I1) ) / SQRT2
            T2 = ( X(I4) + X(I3) ) / SQRT2
            X(I1) = X(I1) + X(I2)
            X(I2) = X(I4) - X(I3)
            X(I3) = 2 * ( - T2 - T1 )
            X(I4) = 2 * ( -T2 + T1 )
 15      CONTINUE
         IS = 2 * ID - N2
         ID = 4 * ID
         IF ( IS .LT. N-1 ) GOTO 17
         A = E
         DO 20, J = 2, N8
            A3 = 3 * A
            CC1 = COS(A)
            SS1 = SIN(A)
            CC3 = COS(A3)
            SS3 = SIN(A3)
            A = J * E
            IS = 0
            ID = 2 * N2
 40         DO 30, I = IS, N-1, ID
               I1 = I + J
               I2 = I1 + N4
               I3 = I2 + N4
               I4 = I3 + N4
               I5 = I + N4 - J + 2
               I6 = I5 + N4
               I7 = I6 + N4
               I8 = I7 + N4
               T1 = X(I1) - X(I6)
               X(I1) = X(I1) + X(I6)
               T2 = X(I5) - X(I2)
               X(I5) = X(I2) + X(I5)
               T3 = X(I8) + X(I3)
               X(I6) = X(I8) - X(I3)
               T4 = X(I4) + X(I7)
               X(I2) = X(I4) - X(I7)
               T5 = T1 - T4
               T1 = T1 + T4
               T4 = T2 - T3
               T2 = T2 + T3
               X(I3) = T5 * CC1 + T4 * SS1
               X(I7) = - T4 * CC1 + T5 * SS1
               X(I4) = T1 * CC3 - T2 * SS3
               X(I8) = T2 * CC3 + T1 * SS3
 30         CONTINUE
            IS = 2 * ID - N2
            ID = 4 * ID
            IF ( IS .LT. N-1 ) GOTO 40
 20      CONTINUE
 10   CONTINUE
!
      IS = 1
      ID = 4
 70   DO 60, I0 = IS, N, ID
         I1 = I0 + 1
         R1 = X(I0)
         X(I0) = R1 + X(I1)
         X(I1) = R1 - X(I1)
 60   CONTINUE
      IS = 2 * ID - 1
      ID = 4 * ID
      IF ( IS .LT. N ) GOTO 70
!
 100  J = 1
      N1 = N - 1
      DO 104, I = 1, N1
         IF ( I .GE. J ) GOTO 101
         XT = X(J)
         X(J) = X(I)
         X(I) = XT
 101     K = N / 2
 102     IF ( K .GE. J ) GOTO 103
            J = J - K
            K = K / 2
            GOTO 102
 103     J = J + K
 104  CONTINUE
      XT = 1.0 / FLOAT( N )
      DO 99, I = 1, N
         X(I) = XT * X(I)
 99   CONTINUE
      RETURN
!
! ... End of subroutine SFFTCB ...
! 
    END SUBROUTINE SFFTCB


END MODULE MathFcts
