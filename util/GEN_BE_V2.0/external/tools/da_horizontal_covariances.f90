module da_horizontal_covariances

implicit none

contains
!------------------------------------------------------------------------------------------
subroutine get_grid_info( ni, nj, nn, stride, nr, jstart, jend )

   implicit none

   integer, intent(in)    :: ni, nj                  ! Grid dimensions.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in)    :: stride                  ! Calculate correlation with every stride point.
   integer, intent(in)    :: jstart, jend            ! Starting and ending j indices
   integer, intent(out)   :: nr(0:nn)                ! Number of points in each bin.

   integer                :: i, j, m, n              ! Indices.
   integer                :: d2                      ! Distance squared.

   nr(0:nn) = 0

!  [1] Get points distribution nr(0:nn):

!$OMP PARALLEL DO PRIVATE(I,J,N,D2) REDUCTION(+:NR)
   do j = jstart, jend, stride                         ! Pick every stride point to save cost.
      do i = 1, ni, stride                             ! Pick every stride point to save cost.

!        Calculate points distribution (i,j) for points that haven't already been computed:

!        Finish off current row first:
         n = j
         do m = i, ni
            d2 = (m-i) * (m-i)                         ! Calculate distance (squared).
            nr(d2) = nr(d2) + 1                        ! Add this point to number at this distance.
         end do

!        Now complete remaining rows:
         do n = j+1, jend
            do m = 1, ni
               d2 = (m-i)*(m-i) + (n-j)*(n-j)          ! Calculate distance (squared)
               nr(d2) = nr(d2) + 1                     ! Add this point to number at this distance.
            end do ! m
         end do ! n
      end do ! i
   end do ! j
!$OMP END PARALLEL DO

end subroutine get_grid_info
!------------------------------------------------------------------------------------------
subroutine get_grid_info_masked( ni, nj, nn, stride, nr, jstart, jend, mask )
  ! 02-26-2009 Yann MICHEL NCAR/MMM --ym--
  implicit none

  integer, intent(in)    :: ni, nj                  ! Grid dimensions.
  integer, intent(in)    :: nn                      ! Dimension of radii bins.
  integer, intent(in)    :: stride                  ! Calculate correlation with every stride point.
  integer, intent(in)    :: jstart, jend            ! Starting and ending j indices
  integer, intent(out)   :: nr(0:nn)                ! Number of points in each bin.
  real, intent(in)       :: mask(1:ni,1:nj)         ! 2D field mask field
  integer                :: i, j, m, n              ! Indices.
  integer                :: d2                      ! Distance squared.
  nr(0:nn) = 0

!  [1] Get points distribution nr(0:nn):

!$OMP PARALLEL DO PRIVATE(I,J,N,D2) REDUCTION(+:NR)
   do j = jstart, jend, stride                         ! Pick every stride point to save cost.
      do i = 1, ni, stride                             ! Pick every stride point to save cost.

!        Calculate points distribution (i,j) for points that haven't already been computed:

!        Finish off current row first:
         n = j
         do m = i, ni
            if (mask(i,j)*mask(m,n)==1) then              ! mask has to be 1 for points to hit
               d2 = (m-i) * (m-i)                         ! Calculate distance (squared).          
               nr(d2) = nr(d2) + 1                        ! Add this point to number at this distance.
            end if
         end do

!        Now complete remaining rows:
         do n = j+1, jend
            do m = 1, ni
               if (mask(i,j)*mask(m,n)==1) then           ! mask has to be 1 for points to hit
                  d2 = (m-i)*(m-i) + (n-j)*(n-j)          ! Calculate distance (squared)
                  nr(d2) = nr(d2) + 1                     ! Add this point to number at this distance.
               end if
            end do ! m
         end do ! n
      end do ! i
   end do ! j
!$OMP END PARALLEL DO

end subroutine get_grid_info_masked
!------------------------------------------------------------------------------------------
subroutine get_covariance( ni, nj, nn, stride, count, nr, jstart, jend, &
      field, mean, var, cov )

!----------------------------------------------------------------------
! Purpose: Calculate the number of points, distance matrx between
!          points on grid.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/05/05 Dale Barker
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: ni, nj                  ! Grid dimensions.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in)    :: stride                  ! Calculate correlation with every stride point.
   real, intent(in)       :: count                   ! Number of times/ensemble members so far.
   integer, intent(in)    :: nr(0:nn)                ! Number of points in each bin.
   integer, intent(in)    :: jstart, jend            ! Starting and ending j indices
   real, intent(in)       :: field(1:ni,1:nj)        ! 2D field.
   real, intent(inout)    :: var(0:nn,1:2 )          ! Mean for each data subset of each bin.
   real, intent(in)       :: mean(0:nn,1:2 )         ! Mean for each data subset of each bin.
   real, intent(inout)    :: cov(0:nn)               ! Covariance of each bin.

   integer                :: i, j, m, n              ! Loop counters.
   integer                :: d2                      ! Distance squared.
   real                   :: count_inv               ! 1 / count.
   real                   :: bb(0:nn)                ! Covariance for this field.
   real                   :: ab(0:nn,1:2)            ! Variance for this field.

   count_inv = 1.0 / count

   bb(0:nn) = 0.0
   ab(0:nn,1:2) = 0.0
!$OMP PARALLEL DO PRIVATE(I,J,N,D2) REDUCTION(+:BB)
   do j = jstart, jend, stride                         ! Pick every stride point to save cost.
      do i = 1, ni, stride                             ! Pick every stride point to save cost.

!        Calculate spatial correlations with point (i,j) that haven't already been computed:

!        Finish off current row first:
         n = j
         do m = i, ni
            d2 = (m-i) * (m-i)                         ! Calculate distance (squared).
            bb(d2) = bb(d2) + (field(i,j)-mean(d2,1)) * (field(m,n)-mean(d2,2))
            ab(d2,1) = ab(d2,1) + (field(i,j)-mean(d2,1)) * (field(i,j)-mean(d2,1))
            ab(d2,2) = ab(d2,2) + (field(m,n)-mean(d2,2)) * (field(m,n)-mean(d2,2))
         end do

!        Now complete remaining rows:
         do n = j+1, jend
            do m = 1, ni
               d2 = (m-i)*(m-i) + (n-j)*(n-j)          ! Calculate distance (squared)
               bb(d2) = bb(d2) + (field(i,j)-mean(d2,1)) * (field(m,n)-mean(d2,2))
               ab(d2,1) = ab(d2,1) + (field(i,j)-mean(d2,1)) * (field(i,j)-mean(d2,1))
               ab(d2,2) = ab(d2,2) + (field(m,n)-mean(d2,2)) * (field(m,n)-mean(d2,2))
            end do ! m
         end do ! n

      end do ! i
   end do ! j
!$OMP END PARALLEL DO
!  Calculate average values for each bin at this time:
!$OMP PARALLEL DO PRIVATE(D2)
   do d2 = 0, nn
      if ( nr(d2) /= 0 ) then
         bb(d2) = bb(d2) / real(nr(d2))
         ab(d2,1) = ab(d2,1) / real(nr(d2))
         ab(d2,2) = ab(d2,2) / real(nr(d2))

!        Calculate accumulating average over members/times:
         cov(d2) = ( (count - 1.0) * cov(d2) + bb(d2) ) * count_inv
         var(d2,1) = ( (count - 1.0) * var(d2,1) + ab(d2,1) ) * count_inv
         var(d2,2) = ( (count - 1.0) * var(d2,2) + ab(d2,2) ) * count_inv

      end if
   end do
!$OMP END PARALLEL DO

end subroutine get_covariance
!------------------------------------------------------------------------------------------
subroutine get_covariance_masked( ni, nj, nn, stride, count, nr, jstart, jend, field, mask, cov )
  ! 02-26-2009 Yann MICHEL NCAR/MMM --ym--
  !  Calculate the number of points, distance matrx between points on grid.

   implicit none

   integer, intent(in)    :: ni, nj                  ! Grid dimensions.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in)    :: stride                  ! Calculate correlation with every stride point.
   real, intent(in)       :: count                   ! Number of times/ensemble members so far.
   integer, intent(in)    :: nr(0:nn)                ! Number of points in each bin.
   integer, intent(in)    :: jstart, jend            ! Starting and ending j indices
   real, intent(in)       :: field(1:ni,1:nj)        ! 2D field.
   real, intent(in)       :: mask(1:ni,1:nj)         ! 2D field mask field
   real, intent(inout)    :: cov(0:nn)               ! Covariance of each bin.

   integer                :: i, j, m, n              ! Loop counters.
   integer                :: d2                      ! Distance squared.
   real                   :: count_inv               ! 1 / count.
   real                   :: bb(0:nn)                ! Covariance for this field.

   count_inv = 1.0 / count
   bb(0:nn) = 0.0

!$OMP PARALLEL DO PRIVATE(I,J,N,D2) REDUCTION(+:BB)
   do j = jstart, jend, stride                         ! Pick every stride point to save cost.
      do i = 1, ni, stride                             ! Pick every stride point to save cost.

!        Calculate spatial correlations with point (i,j) that haven't already been computed:

!        Finish off current row first:
         n = j
         do m = i, ni           
            if (mask(i,j)*mask(m,n)==1) then           ! mask has to be 1 for points to hit
               d2 = (m-i) * (m-i)                         ! Calculate distance (squared).
               bb(d2) = bb(d2) + field(i,j) * field(m,n)
            end if
         end do

!        Now complete remaining rows:
         do n = j+1, jend
            do m = 1, ni
               if (mask(i,j)*mask(m,n)==1) then           ! mask has to be 1 for points to hit     
                  d2 = (m-i)*(m-i) + (n-j)*(n-j)          ! Calculate distance (squared)
                  bb(d2) = bb(d2) + field(i,j) * field(m,n)
               end if
            end do ! m
         end do ! n

      end do ! i
   end do ! j
!$OMP END PARALLEL DO

!  Calculate average values for each bin at this time:
!$OMP PARALLEL DO PRIVATE(D2)
   do d2 = 0, nn
      if ( nr(d2) /= 0 ) then
         bb(d2) = bb(d2) / real(nr(d2))

!        Calculate accumulating average over members/times:
         cov(d2) = ( (count - 1.0) * cov(d2) + bb(d2) ) * count_inv

      end if
   end do
!$OMP END PARALLEL DO

end subroutine get_covariance_masked
!------------------------------------------------------------------------------------------
subroutine gauss_scale_length( sl, variable, ck, nn, nr, cov )

!----------------------------------------------------------------------
! Purpose: Calculate fit of input covariance data to Gaussian
!          correlation function
!
! Note   : cov(r) = cov(0) * exp(-r**2 / 8*sl)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 17/05/05 Dale Barker
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   character*10, intent(in):: variable               ! Variable name
   character*2, intent(in):: ck                      ! Level index -> character.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in )   :: nr(0:nn)                ! Number of points in each bin.
   real, intent(in)       :: cov(0:nn)               ! Covariance of each bin.

   real(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
   real(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
   real(kind=8)           :: d(0:nn)                 ! Distance for each bin.

   integer                :: n, d2                   ! Loop counters.
   integer                :: nmax                    ! Number of points available for curve fitting.
   real                   :: yr_cutoff               ! Noise cut-off criterion.
   real                   :: corr_min                ! Corresponding correlation value.
   real(kind=8)           :: coeff1, coeff2          ! Curve-fitting coefficients.
   real(kind=8)           :: ml, sl                  ! Gradient, scalelength.

   yr(0:nn) = 0.0
   nrr(0:nn) = 0.0
   d(0:nn) = 0.0

   yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
   corr_min = exp( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)

   write(UNIT=6,FMT='(a,1pe15.5)') &
      ' Fit Gaussian curve to data for correlations >= ', corr_min

!   write(UNIT=6,FMT='(5a)') &
!      '  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '
   n = 0
   nrr(n) = real(nr(n))
!   write(UNIT=6,FMT='(i6,4e13.5)') n, nrr(n), d(0), cov(n), yr(n)

   do d2 = 1, nn

      if ( nr(d2) > 0 .and. cov(d2) < cov(0) ) then ! Omit bins with no data and negative logs.

         if ( cov(d2) / cov(0) < corr_min ) exit ! Yong-Run's noise cut-off criterion.
         n = n + 1
         yr(n) = sqrt( 8.0 * log(cov(0) / cov(d2)) )
         nrr(n) = real(nr(d2))
         d(n) = sqrt(real(d2))                  ! Distance
!         write(UNIT=6,FMT='(i6,4e13.5)') n, nrr(n), d(n), cov(d2), yr(n)
       end if
   end do
   nmax = n
   if (nmax > 0) then

!  Now perform curve-fitting when more than 2 points available:

!-----Steps of fitting Gaussian Distribution:

!     B(r) = B(0) exp(-d**2/(8*s**2)      (1)

!     Log at both side of (1):

!        ln[B(0)/B(d)] = d**2/(8*s**2)   (2)

!        {8*ln[B(0)/B(d)]}**0.5 = d/s = m * d

!     Let:

!        y(d) = {8*ln[B(0)/B(d)]}**0.5

!        m = sum[d * y(d)]/sum[d*d]

      coeff1 = 0.0
      coeff2 = 0.0

      do n = 1, nmax
!     WRITE(UNIT=6,FMT='("n, nrr, d, yr:",i3,3e15.6)') n, nrr(n), d(n), yr(n)
         coeff1 = coeff1 + nrr(n) * d(n) * yr(n)
         coeff2 = coeff2 + nrr(n) * d(n) * d(n)
      end do

      if (coeff2 > 0.0) then
         ml = coeff1 / coeff2
         sl = 1.0 / ml
      else
!        When no fitting could be completed, set the missing value = 0.0 (YRG 06/30/2005):
         ml = 0.0
         sl = 0.0
      endif

   else

       write(UNIT=6,FMT='(a)') &
      'All corelation values lower than corr_min. Setting SL to 0'
      sl = 0.0
      ml = 0.0

   end if

   write(UNIT=6,FMT='(/3a,3e20.8/)') trim(variable),&
      ' gaussian scale-length at mode: ', ck, ml, sl

end subroutine gauss_scale_length
!------------------------------------------------------------------------------------------
subroutine soar_scale_length( sl, variable, ck, nn, nr, cov )

!----------------------------------------------------------------------
! Purpose: Calculate fit of input covariance data to a SOAR
!          correlation function
!
! Note   : cov(r) = cov(0) (1 + r/sl) * exp(-r/ sl)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   character*10, intent(in):: variable               ! Variable name
   character*2, intent(in):: ck                      ! Level index -> character.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in )   :: nr(0:nn)                ! Number of points in each bin.
   real, intent(in)       :: cov(0:nn)               ! Covariance of each bin.
   real(kind=8), intent(inout):: sl                  ! Scalelength.

   real(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
   real(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
   real(kind=8)           :: d(0:nn)                 ! Distance for each bin.

   integer                :: n, d2                   ! Loop counters.
   integer                :: nmax                    ! Number of points available for curve fitting.
   real                   :: yr_cutoff               ! Noise cut-off criterion.
   real                   :: corr_min                ! Corresponding correlation value.

   integer                :: ilist, ierr
   real                   :: minv

   yr(0:nn) = 0.0
   nrr(0:nn) = 0.0
   d(0:nn) = 0.0

   yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
   corr_min = exp( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)

   write(UNIT=6,FMT='(a,1pe15.5)') &
      ' Fit SOAR curve to data for correlations >= ', corr_min

!   write(UNIT=6,FMT='(5a)') &
!      '  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '

   n = 0
   nrr(n) = real(nr(n))
!   write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(0), cov(n)

   do d2 = 1, nn

      if ( nr(d2) > 0 .and. cov(d2) < cov(0) ) then ! Omit bins with no data and overshoot in covariances.
         if ( cov(d2) / cov(0)  < corr_min ) exit ! Yong-Run's noise cut-off criterion.
         n = n + 1
         yr(n) = cov(d2)
         nrr(n) = real(nr(d2))
         d(n) = sqrt(real(d2))                  ! Distance
!         write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(n), yr(n)
       end if

   end do
   nmax = n
   if (nmax > 0) then

!     Now perform curve-fitting on the selected data:
      ilist=1
      minv=0.01
      ierr=0
      if ( sl == 0.0 ) sl = 10.0 ! prevent using 0 value if gaussian fit failed
      call soarfit(d,yr,nrr,nn,nmax,cov(0),sl,minv,ilist,ierr)

   else

       write(UNIT=6,FMT='(a)') &
      'All corelation values lower than corr_min. Setting SL to 0'
      sl = 0.0

   end if

   write(UNIT=6,FMT='(/3a,3e30.8/)') trim(variable),&
      ' SOAR    scale-length at mode:', ck, sl

end subroutine soar_scale_length

!------------------------------------------------------------------------------


subroutine soarfit(x,y,w,n,nmax,a,param,minv,ilist,ierr)

!----------------------------------------------------------------------
! Purpose: This subroutine compute the lenghtscale (sl, here: param) of a
!          SOAR function that best fit (in a least-square perspective) a
!          data set. The method follow routine CURVEFIT from:
!          Heeswijk, M.V., and C.G. Fox, 1988: Iterative Method and Fortran
!          Code for Nonlinear Curve Fitting, Computers and Geosciences, 14,
!          4, pp. 489-503.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   integer,            intent(in)  :: n
   integer,            intent(in)  :: nmax
   integer,            intent(in)  :: ilist
   integer,            intent(inout)  :: ierr

   real(kind=8),       intent(in) :: x(0:n)
   real(kind=8),       intent(in) :: y(0:n)
   real(kind=8),       intent(in) :: w(0:n)
   real,               intent(in) :: minv
   real(kind=8),       intent(inout) :: param

   real,               intent(in) :: a

   integer, parameter    :: icon = 100   ! max iteration
   integer, parameter    :: lbis = 10    ! max bisection

   real(kind=8)      :: d(0:n)
   real(kind=8)      :: g(0:n)
   real(kind=8)      :: m
   real(kind=8)      :: gtd
   real(kind=8)      :: gtg
   real(kind=8)      :: gtginv
   real(kind=8)      :: parold
   real(kind=8)      :: ssqold, ssq, rootmsq

   integer           :: iter,nbis,i                     ! Loop counters.

   ! Initialize variables
   ierr   = 0          ! Error flags
   iter   = 0          ! Iteration counter
   nbis   = 0          ! Bisection counter
   m      = 9.9E9      ! dummy value to initialize m
   ssqold = 9.9E9      ! dummy value to initialize ssqold

   print*
   print*,' SOAR function fitting using an iterative method'
   print*

   do while ( (abs(m).gt.minv) .and. (iter.le.icon) .and. (nbis.le.lbis) )

      ! Form vector D; i.e. a data point minus the isolated Taylor Series
      ! term for that data point. In addition calculate the sum of the
      ! square of the residuals

      ssq = 0.d0
      do i = 0, nmax
         d(i)=y(i)-f0(x(i),a,param)   ! f0 = SOAR function
         ssq = ssq + d(i)**2 *w(i)
      end do
!rizvi      rootmsq=dsqrt(ssq)
      rootmsq=sqrt(ssq)

      if (ssq.lt.ssqold) then         ! Convergence

         ! The matrix formation in the following program segment follows
         ! equation 3.12 on page 41 of Geophysical Data Analysis: Discrete
         ! Inverse Theory by Menke (1984)

         ! Form matrix G
         do i = 0, nmax
            g(i)=f1(x(i),a,param)     ! f1 = SOAR function derivative wrt to param
         end do

         ! Form matrix GTD
         gtd=0.0
         do i = 0, nmax
            gtd=gtd+d(i)*g(i)*w(i)
         end do

         ! Form matrix GTG
         gtg=0.0
         do i = 0, nmax
            gtg=gtg+g(i)*g(i)*w(i)
         end do

         ! Find the inverse of matrix GTG
         gtginv=1.0/gtg

         ! Find vector M (i.e. increment of PARAM)
         m=gtginv*gtd

         ssqold=ssq          ! keep

         iter=iter+1

      else                           ! divergence

         m = m/2.0           ! Bisect the correction
         nbis = nbis + 1

      endif

      ! Update the parameter
      param=param+m

      if (ilist.eq.1) print 20,'i=',iter,'n=',nbis,'m=',m,'rmsd=',rootmsq,'s=',param
20 format(2X,A2,I6,2X,A2,I3,2X,A2,G10.4,2X,A5,G10.4,2X,A2,G10.4)

   end do

   print*
   print*," SOAR estimated lenghtscale = ",param
   print*

end subroutine soarfit
!------------------------------------------------------------------------------------------
function f0(x,a,param)  ! calculate SOAR function
  integer       :: l
  real(kind=8)  :: x
  real(kind=8)  :: param
  real          :: a
  real(kind=8)  :: f0
  f0 = a * (1.0 + x/param) * exp(-1.0*x/param)
  return
end function f0
!------------------------------------------------------------------------------------------
function f1(x,a,param)  ! calculate SOAR function derivative wrt param
  real(kind=8)  :: x
  real(kind=8)  :: param
  real          :: a
  real(kind=8)  :: f1
  f1 = (a * x**2 / (param)**3) * exp(-1.0*x/param)
  return
end function f1

  !------------------------------------------------------------------------------------------------
  ! A fast self-contained routine to find the median

  SUBROUTINE median(x, n, xmed)

    ! Find the median of X(1), ... , X(N), using as much of the quicksort
    ! algorithm as is needed to isolate it.
    ! N.B. On exit, the array X is partially ordered.

    !     Latest revision - 26 November 1996
    IMPLICIT NONE

    INTEGER, INTENT(IN)                :: n
    REAL, INTENT(IN OUT), DIMENSION(:) :: x
    REAL, INTENT(OUT)                  :: xmed

    ! Local variables

    REAL    :: temp, xhi, xlo, xmax, xmin
    LOGICAL :: odd
    INTEGER :: hi, lo, nby2, nby2p1, mid, i, j, k

    nby2 = n / 2
    nby2p1 = nby2 + 1
    odd = .true.

    !     HI & LO are position limits encompassing the median.

    IF (n == 2 * nby2) odd = .false.
    lo = 1
    hi = n
    IF (n < 3) THEN
       IF (n < 1) THEN
          xmed = 0.0
          RETURN
       END IF
       xmed = x(1)
       IF (n == 1) RETURN
       xmed = 0.5*(xmed + x(2))
       RETURN
    END IF

    !     Find median of 1st, middle & last values.

10  mid = (lo + hi)/2
    xmed = x(mid)
    xlo = x(lo)
    xhi = x(hi)
    IF (xhi < xlo) THEN          ! Swap xhi & xlo
       temp = xhi
       xhi = xlo
       xlo = temp
    END IF
    IF (xmed > xhi) THEN
       xmed = xhi
    ELSE IF (xmed < xlo) THEN
       xmed = xlo
    END IF

    ! The basic quicksort algorithm to move all values <= the sort key (XMED)
    ! to the left-hand end, and all higher values to the other end.

    i = lo
    j = hi
50  DO
       IF (x(i) >= xmed) EXIT
       i = i + 1
    END DO
    DO
       IF (x(j) <= xmed) EXIT
       j = j - 1
    END DO
    IF (i < j) THEN
       temp = x(i)
       x(i) = x(j)
       x(j) = temp
       i = i + 1
       j = j - 1

       !     Decide which half the median is in.

       IF (i <= j) GO TO 50
    END IF

    IF (.NOT. odd) THEN
       IF (j == nby2 .AND. i == nby2p1) GO TO 130
       IF (j < nby2) lo = i
       IF (i > nby2p1) hi = j
       IF (i /= j) GO TO 100
       IF (i == nby2) lo = nby2
       IF (j == nby2p1) hi = nby2p1
    ELSE
       IF (j < nby2p1) lo = i
       IF (i > nby2p1) hi = j
       IF (i /= j) GO TO 100

       ! Test whether median has been isolated.

       IF (i == nby2p1) RETURN
    END IF
100 IF (lo < hi - 1) GO TO 10

    IF (.NOT. odd) THEN
       xmed = 0.5*(x(nby2) + x(nby2p1))
       RETURN
    END IF
    temp = x(lo)
    IF (temp > x(hi)) THEN
       x(lo) = x(hi)
       x(hi) = temp
    END IF
    xmed = x(nby2p1)
    RETURN

    ! Special case, N even, J = N/2 & I = J + 1, so the median is
    ! between the two halves of the series.   Find max. of the first
    ! half & min. of the second half, then average.

130 xmax = x(1)
    DO k = lo, j
       xmax = MAX(xmax, x(k))
    END DO
    xmin = x(n)
    DO k = i, hi
       xmin = MIN(xmin, x(k))
    END DO
    xmed = 0.5*(xmin + xmax)

    RETURN
  END SUBROUTINE median

end module da_horizontal_covariances
