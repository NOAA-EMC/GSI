module netlib
!$$$  module documentation block
!
! module: netlib                       Compute eigenvalues and eigenvectors
!                                      of real symmetric matrix
!
! prgmmr: ota              org: np22                   date: 2011-06-03
!
! abstract: Compute eigenvalues and eigenvectors of real symmetric matrix.
!  Basic algorithm and source code are imported from netlib.org libraries
!  and transcripted to f90.
!
! program history log:
!   2011-06-03  Created from netlib.org
!
! attributes:
!   language: f95
!
!$$$

  use kinds,only: i_kind,r_double

  implicit none

  private
  public :: rs

contains

  real(r_double) function pythag(a,b)
    implicit none
    real(r_double) :: a,b
!
!     finds dsqrt(a**2+b**2) without overflow or destructive underflow
!
    real(r_double) :: p,r,s,t,u
    p = dmax1(dabs(a),dabs(b))
    if (p /= 0._r_double) then
       r = (dmin1(dabs(a),dabs(b))/p)**2
       do
          t = 4._r_double + r
          if (t == 4._r_double) exit
          s = r/t
          u = 1._r_double + 2._r_double*s
          p = u*p
          r = (s/u)**2 * r
       end do
    end if
    pythag = p
    return
  end function pythag

  subroutine rs(nm,n,a,w,matz,z,fv1,fv2,ierr)
    implicit none
    integer(i_kind),intent(in) :: n,nm,matz
    real(r_double),intent(inout) :: a(nm,n)
    integer(i_kind),intent(out) :: ierr
    real(r_double),intent(out) :: w(n),z(nm,n),fv1(n),fv2(n)
!
!     this subroutine calls the recommended sequence of
!     subroutines from the eigensystem subroutine package (eispack)
!     to find the eigenvalues and eigenvectors (if desired)
!     of a real symmetric matrix.
!
!     on input
!
!        nm  must be set to the row dimension of the two-dimensional
!        array parameters as declared in the calling program
!        dimension statement.
!
!        n  is the order of the matrix  a.
!
!        a  contains the real symmetric matrix.
!
!        matz  is an integer variable set equal to zero if
!        only eigenvalues are desired.  otherwise it is set to
!        any non-zero integer for both eigenvalues and eigenvectors.
!
!     on output
!
!        w  contains the eigenvalues in ascending order.
!
!        z  contains the eigenvectors if matz is not zero.
!
!        ierr  is an integer output variable set equal to an error
!           completion code described in the documentation for tqlrat
!           and tql2.  the normal completion code is zero.
!
!        fv1  and  fv2  are temporary storage arrays.
!
!     questions and comments should be directed to burton s. garbow,
!     mathematics and computer science div, argonne national laboratory
!
!     this version dated august 1983.
!
!     ------------------------------------------------------------------
!
    if (n > nm) then
       ierr = 10 * n
       return
    end if
!     .......... find both eigenvalues and eigenvectors ..........
    call  tred2(nm,n,a,w,fv1,z)
    call  tql2(nm,n,w,fv1,z,ierr)
    return
  end subroutine rs

  subroutine tql2(nm,n,d,e,z,ierr)
    implicit none
    integer(i_kind),intent(in) :: nm,n
    real(r_double),intent(inout) :: d(n),e(n),z(nm,n)
    integer(i_kind),intent(out) :: ierr
    real(r_double) :: c,c2,c3,dl1,el1,f,g,h,p,r,s,s2,tst1,tst2
    integer(i_kind) :: i,j,k,l,m,ii,l1,l2,mml
!
!     this subroutine is a translation of the algol procedure tql2,
!     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and
!     wilkinson.
!     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).
!
!     this subroutine finds the eigenvalues and eigenvectors
!     of a symmetric tridiagonal matrix by the ql method.
!     the eigenvectors of a full symmetric matrix can also
!     be found if  tred2  has been used to reduce this
!     full matrix to tridiagonal form.
!
!     on input
!
!        nm must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        n is the order of the matrix.
!
!        d contains the diagonal elements of the input matrix.
!
!        e contains the subdiagonal elements of the input matrix
!          in its last n-1 positions.  e(1) is arbitrary.
!
!        z contains the transformation matrix produced in the
!          reduction by  tred2, if performed.  if the eigenvectors
!          of the tridiagonal matrix are desired, z must contain
!          the identity matrix.
!
!      on output
!
!        d contains the eigenvalues in ascending order.  if an
!          error exit is made, the eigenvalues are correct but
!          unordered for indices 1,2,...,ierr-1.
!
!        e has been destroyed.
!
!        z contains orthonormal eigenvectors of the symmetric
!          tridiagonal (or full) matrix.  if an error exit is made,
!          z contains the eigenvectors associated with the stored
!          eigenvalues.
!
!        ierr is set to
!          zero       for normal return,
!          j          if the j-th eigenvalue has not been
!                     determined after 30 iterations.
!
!     calls pythag for  dsqrt(a*a + b*b) .
!
!     questions and comments should be directed to burton s. garbow,
!     mathematics and computer science div, argonne national laboratory
!
!     this version dated august 1983.
!
!     ------------------------------------------------------------------
!
    ierr = 0
    if (n == 1) return
!
    do i = 2, n
       e(i-1) = e(i)
    end do
!
    f = 0._r_double
    tst1 = 0._r_double
    e(n) = 0._r_double
!
    do l = 1, n
       j = 0
       h = dabs(d(l)) + dabs(e(l))
       if (tst1 < h) tst1 = h
!     .......... look for small sub-diagonal element ..........
       do m = l, n
          tst2 = tst1 + dabs(e(m))
          if (tst2 == tst1) exit
!     .......... e(n) is always zero, so there is no exit
!                through the bottom of the loop ..........
       end do
!
       if (m /= l) then
          if (j == 30) then
             ierr = l
             return
          end if
          j = j + 1
!     .......... form shift ..........
          l1 = l + 1
          l2 = l1 + 1
          g = d(l)
          p = (d(l1) - g) / (2._r_double * e(l))
          r = pythag(p,1._r_double)
          d(l) = e(l) / (p + dsign(r,p))
          d(l1) = e(l) * (p + dsign(r,p))
          dl1 = d(l1)
          h = g - d(l)
          if (l2 <= n) then
!
             do i = l2, n
                d(i) = d(i) - h
             end do
          end if
!
          f = f + h
!     .......... ql transformation ..........
          p = d(m)
          c = 1._r_double
          c2 = c
          el1 = e(l1)
          s = 0._r_double
          mml = m - l
!     .......... for i=m-1 step -1 until l do -- ..........
          do ii = 1, mml
             c3 = c2
             c2 = c
             s2 = s
             i = m - ii
             g = c * e(i)
             h = c * p
             r = pythag(p,e(i))
             e(i+1) = s * r
             s = e(i) / r
             c = p / r
             p = c * d(i) - s * g
             d(i+1) = h + s * (c * g + s * d(i))
!     .......... form vector ..........
             do k = 1, n
                h = z(k,i+1)
                z(k,i+1) = s * z(k,i) + c * h
                z(k,i) = c * z(k,i) - s * h
             end do
!
          end do
!
          p = -s * s2 * c3 * el1 * e(l) / dl1
          e(l) = s * p
          d(l) = c * p
          tst2 = tst1 + dabs(e(l))
          if (tst2 > tst1) then
             if (j == 30) then
                ierr = l
                return
             end if
          end if
       end if
       d(l) = d(l) + f
    end do
!     .......... order eigenvalues and eigenvectors ..........
    do ii = 2, n
       i = ii - 1
       k = i
       p = d(i)
!
       do j = ii, n
          if (d(j) >= p) exit
          k = j
          p = d(j)
       end do
!
       if (k == i) exit
       d(k) = d(i)
       d(i) = p
!
       do j = 1, n
          p = z(j,i)
          z(j,i) = z(j,k)
          z(j,k) = p
       end do
!
    end do
!     .......... set error -- no convergence to an
!                eigenvalue after 30 iterations ..........
    return
  end subroutine tql2

  subroutine tred2(nm,n,a,d,e,z)
    implicit none
    integer(i_kind),intent(in) :: nm,n
    real(r_double),intent(inout) :: a(nm,n)
    real(r_double),intent(out) :: d(n),e(n),z(nm,n)
    real(r_double) :: f,g,h,hh,scale
    integer(i_kind) :: i,j,k,l,ii,jp1
!
!     this subroutine is a translation of the algol procedure tred2,
!     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.
!     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
!
!     this subroutine reduces a real symmetric matrix to a
!     symmetric tridiagonal matrix using and accumulating
!     orthogonal similarity transformations.
!
!     on input
!
!        nm must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        n is the order of the matrix.
!
!        a contains the real symmetric input matrix.  only the
!          lower triangle of the matrix need be supplied.
!
!     on output
!
!        d contains the diagonal elements of the tridiagonal matrix.
!
!        e contains the subdiagonal elements of the tridiagonal
!          matrix in its last n-1 positions.  e(1) is set to zero.
!
!        z contains the orthogonal transformation matrix
!          produced in the reduction.
!
!        a and z may coincide.  if distinct, a is unaltered.
!
!     questions and comments should be directed to burton s. garbow,
!     mathematics and computer science div, argonne national laboratory
!
!     this version dated august 1983.
!
!     ------------------------------------------------------------------
!
    do i = 1, n
!
       do j = i, n
          z(j,i) = a(j,i)
       end do
!
       d(i) = a(n,i)
    end do
!
    if (n /= 1) then
!     .......... for i=n step -1 until 2 do -- ..........
       do ii = 2, n
          i = n + 2 - ii
          l = i - 1
          h = 0._r_double
          scale = 0._r_double
          if (l < 2) go to 130
!     .......... scale row (algol tol then not needed) ..........
          do k = 1, l
             scale = scale + dabs(d(k))
          end do
!
          if (scale /= 0._r_double) go to 140
130       e(i) = d(l)
!
          do j = 1, l
             d(j) = z(l,j)
             z(i,j) = 0._r_double
             z(j,i) = 0._r_double
          end do
!
          go to 290
!
140       do k = 1, l
             d(k) = d(k) / scale
             h = h + d(k) * d(k)
          end do
!
          f = d(l)
          g = -dsign(dsqrt(h),f)
          e(i) = scale * g
          h = h - f * g
          d(l) = f - g
!     .......... form a*u ..........
          do j = 1, l
             e(j) = 0._r_double
          end do
!
          do j = 1, l
             f = d(j)
             z(j,i) = f
             g = e(j) + z(j,j) * f
             jp1 = j + 1
             if (l >= jp1) then
!
                do k = jp1, l
                   g = g + z(k,j) * d(k)
                   e(k) = e(k) + z(k,j) * f
                end do
             end if
!
             e(j) = g
          end do
!     .......... form p ..........
          f = 0._r_double
!
          do j = 1, l
             e(j) = e(j) / h
             f = f + e(j) * d(j)
          end do
!
          hh = f / (h + h)
!     .......... form q ..........
          do j = 1, l
             e(j) = e(j) - hh * d(j)
          end do
!     .......... form reduced a ..........
          do j = 1, l
             f = d(j)
             g = e(j)
!
             do k = j, l
                z(k,j) = z(k,j) - f * e(k) - g * d(k)
             end do
!
             d(j) = z(l,j)
             z(i,j) = 0._r_double
          end do
!
290       d(i) = h
       end do
!     .......... accumulation of transformation matrices ..........
       do i = 2, n
          l = i - 1
          z(n,l) = z(l,l)
          z(l,l) = 1._r_double
          h = d(i)
          if (h /= 0._r_double) then
!
             do k = 1, l
                d(k) = z(k,i) / h
             end do
!
             do j = 1, l
                g = 0._r_double
!
                do k = 1, l
                   g = g + z(k,i) * z(k,j)
                end do
!
                do k = 1, l
                   z(k,j) = z(k,j) - g * d(k)
                end do
             end do
!
          end if
          do k = 1, l
             z(k,i) = 0._r_double
          end do
!
       end do
    end if
!
    do i = 1, n
       d(i) = z(n,i)
       z(n,i) = 0._r_double
    end do
!
    z(n,n) = 1._r_double
    e(1) = 0._r_double
    return
  end subroutine tred2
end module netlib
