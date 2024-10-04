module da_be_spectral

   use da_control, only : da_zero_complex,ierr, &
      pi,gaussian_lats
   use da_reporting, only : da_error,message
   use da_tools_serial, only : da_free_unit, da_get_unit

   implicit none

contains

subroutine da_asslegpol (l, m, sinlat, coslat, alp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)            :: l       ! Legendre wavenumber.
   integer, intent(in)            :: m       ! Fourier wavenumber.
   real,    intent(in)            :: sinlat  ! sin(latitude).
   real,    intent(in)            :: coslat  ! cos(latitude).
   real,    intent(out)           :: alp     ! Associated Legendre Polynomial.

   integer                        :: i, loop
   real                           :: half_co
   real                           :: alp1, alp2

   half_co = 0.5 * coslat

   ! Calculate ALP:

   if (l < m) then
      alp = 0.0
   else
      alp = 1.0
      do i = m+1, 2*m
         alp = alp * real(i) * half_co
      end do
      if (mod(m,2) /= 0) then
         alp = -alp
      end if

      if (l > m) then
         alp1 = alp
         alp = real(2*m+1) * sinlat * alp1
         if (l /= m+1) then
            do loop = m+2,l
               alp2 = alp1
               alp1 = alp
               alp = (real(2*loop-1) * sinlat * alp1 - real(loop-1+m) * alp2) &
                  / real(loop-m)
            end do
         end if
      end if
   end if

end subroutine da_asslegpol


subroutine da_calc_power(max_wavenumber, r_cvsize, rcv, power)

   !----------------------------------------------------------------------
   ! Purpose: Performs spectral to gridpoint transformation on a sphere.
   !----------------------------------------------------------------------

   implicit none

   integer, intent(in) :: max_wavenumber ! Smallest scale required (ni/2 - 1)
   integer, intent(in) :: r_cvsize               ! Size of rcv vector.
   real, intent(in)    :: rcv(1:r_cvsize)        ! Spectral modes.
   real, intent(out)   :: power(0:max_wavenumber)! Power spectrum.

   integer             :: n, m                   ! Loop counters.
   integer             :: index, indexx          ! Position markers in cv.
   integer             :: cv_size                ! Complex cv size.
   complex, allocatable:: ccv(:)                 ! Complex control variable.

   ! Create complex array:
   cv_size = r_cvsize / 2
   allocate(ccv(1:cv_size))
   do index = 1, cv_size
      indexx = 2 * index - 1
      ccv(index) = cmplx(rcv(indexx), rcv(indexx+1))
   end do

   power(:) = 0.0

   ! Calculate power spectrum from input 1D spectral mode vector:

   do n = 0, max_wavenumber

      ! First consider m=0:
      m = 0
      index = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 + n - m
      power(n) = real(ccv(index))**2

      ! Now add m>0 terms:
      do m = 1, n
         index = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 + n - m
         power(n) = power(n) + 2.0 *(real(ccv(index))**2 + aimag(ccv(index))**2)
      end do

   end do

   deallocate(ccv)
      
end subroutine da_calc_power


subroutine da_get_gausslats( nj, glats, gwgts, sinlat, coslat)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   !  Calculates nj Gaussian latitudes i.e. latitudes at which the Legendre
   !  polynomial Pn(sin(lat)) = 0.0, n=nj, m=0.
   !  The integral from -1 to +1 of f(x)*Pn(x) where f is a polynomial
   !  of degree <= 2n-1 can be calculated using
   !  0.5 * sum(GaussWgts(:)*Pn(:)*f(:)) with the values at Gaussian latitudes.
   !  See eqns 77-79 of 'The Spectral Technique' M.Jarraud and A.J.Simmons
   ! (1983 ECMWF Seminar or 1990 ECMWF Lecture Notes).
   !  The orthogonality and normalisation of the Legendre polynomials
   !  checked in this way are very accurate on the Cray, but somewhat
   !  less accurate on the HPs(32-bit arithmetic).
   !  Starting with a regular latitude grid, use Newton-Raphson interpolation
   ! (with bisection steps to add robustness)
   !  to find the zeros of the Legendre polynomial Pn(x), 0 <= x < 1,
   !  the negative roots(-1 < x < 0) are set by symmetry.
   !  ASin(x) gives the Gaussian latitudes.
   !  This gives slightly better results than finding the roots of Pn(sin(lat))
   ! (Algorithm from Numerical Recipies(Fortran version), 1989, p 258)

   integer, intent(in)            :: nj           ! Gridpoints in N-S direction.
   real,    intent(out)           :: glats(1:nj)  ! Gaussian latitudes(S->N, radians).
   real,    intent(out)           :: gwgts(1:nj)  ! Gaussian weights.
   real,    intent(out), optional :: sinlat(1:nj) ! sin(Latitude).
   real,    intent(out), optional :: coslat(1:nj) ! cos(Latitude).

   integer, parameter     :: maxiter = 100     ! Maximum number of iterations.
   integer                :: i, j, k           ! Loop counters.

   real                   :: fj, fjold         ! Pn(x) on search grid
   real                   :: xj, xjold         ! search grid
   real                   :: x1, x2            ! bounds on root
   real                   :: x                 ! iterated values of x
   real                   :: z                 ! = sqrt(1-x*x)
   real                   :: fn                ! Pn(x)
   real                   :: fn1               ! Pn-1(x)
   real                   :: dfr               ! 1/Pn'(x)
   real                   :: dx, dxold         ! step size, previous step

   k =(nj + 2) / 2
   xj = 0.0
   z  = 1.0

   call da_asslegpol(nj, 0, xj, z, fj)

   if (mod(nj,2) == 1) then
      call da_asslegpol(nj-1,0,xj,z,fn1)
      glats(k) = 0.0
      gwgts(k) = 2.0 *(1.0 - xj * xj) /(real(nj) * fn1)**2
      k = k+1
   end if

   ! Search interval 0 < x <= 1 for zeros of Legendre polynomials:
   do j = 2, nj * 2
      xjold = xj
      fjold = fj

      ! Roots are approximately equally spaced in asin(x)
      xj = Sin(real(j)*Pi/real(nj*4))
      z  = sqrt(1.0-xj*xj)
      call da_asslegpol(nj, 0, xj, z, fj)

      if (fj >= 0.0 .AND. fjold < 0.0 .OR. fj <  0.0 .AND. fjold >= 0.0) then

         ! Perform simple interpolation to improve roots(find Gaussian latitudes)
         if (fjold < 0.0) then  ! Orient the search so that fn(x1) < 0
            x1 = xjold
            x2 = xj
         else
            x1 = xj
            x2 = xjold
         end if

         x = 0.5*(x1 + x2)     ! Initialise the guess for the root
         dxold = ABS(x1 - x2)  ! the step size before last
         dx    = dxold         ! and the last step
         z = sqrt(1.0-x*x)
         call da_asslegpol(nj, 0, x, z, fn)
         call da_asslegpol(nj-1,0,x,z,fn1)
         dfr =(1.0 - x * x) /(real(nj)*(fn1 - x * fn))

         do i = 1, maxiter

            ! Bisect if Newton out of range or not decreasing fast enough
            if (((x-x1)-fn*dfr)*((x-x2)-fn*dfr) > 0.0 &
               .OR. ABS(2.0*fn) > ABS(dxold/dfr)) then
               dxold = dx
               dx = 0.5 *(x1 - x2)
               x = x2 + dx
            else ! Newton-Raphson step
               dxold  = dx
               dx = fn * dfr
               x = x - dx
            end if

            if (ABS(dx) < 2.0*SPACinG(x)) exit
            z = sqrt(1.0-x*x)
            call da_asslegpol(nj,0,x,z,fn)
            call da_asslegpol(nj-1,0,x,z,fn1)
            dfr =(1.0 - x * x) /(real(nj)*(fn1 - x * fn))

            if (fn < 0.0) then   ! Maintain the bracket on the root
               x1 = x
            else
               x2 = x
            end if
         end do

         if (i >= MaxIter) then
            call da_error("da_get_gausslats.inc",118, &
             (/"No convergence finding Gaussian latitudes"/))
         end if

         glats(k) = ASin(x)
         z = sqrt(1.0-x*x)
         call da_asslegpol(nj-1,0,x,z,fn1)
         gwgts(k) = 2.0*(1.0 - x * x) /(real(nj) * fn1)**2
         glats(nj+1-k) = -glats(k)
         gwgts(nj+1-k) = gwgts(k)
         k=k+1
      end if
   end do

   if (k /= nj+1) then
      call da_error("da_get_gausslats.inc",133,(/"Not all roots found"/))
   end if

   ! Calculate sin, cosine:

   do j = 1, nj / 2
      sinlat(j) = sin(glats(j))
      coslat(j) = cos(glats(j))

      ! use symmetry for northern hemisphere:
      sinlat(nj+1-j) = -sinlat(j)
      coslat(nj+1-j) = coslat(j)
   end do

   if ((nj+1) / 2 == nj/2 + 1) then  ! Odd, then equator point:
      glats(nj/2+1) = 0.0
      sinlat(nj/2+1) = 0.0
      coslat(nj/2+1) = 1.0
   end if

end subroutine da_get_gausslats


subroutine da_get_reglats( nj, lat, sinlat, coslat, int_wgts)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: nj             ! Number of latitudes
   real,    intent(out) :: lat(1:nj)      ! Latitude(radians, from south).
   real,    intent(out) :: sinlat(1:nj)   ! sin(Latitude).
   real,    intent(out) :: coslat(1:nj)   ! cos(Latitude).
   real,    intent(out) :: int_wgts(1:nj) ! Legendre Integration weights. 

   integer :: j              ! Loop counter.
   real    :: delta_phi      ! Regular latitude interval.

   delta_phi = pi / real(nj-1) 

   do j = 1, nj / 2
      lat(j) = -0.5 * pi + delta_phi * real(j - 1)
      sinlat(j) = sin(lat(j))
      coslat(j) = cos(lat(j))
      int_wgts(j) = coslat(j) * delta_phi

      ! use symmetry for northern hemisphere:
      lat(nj+1-j) = -lat(j)
      sinlat(nj+1-j) = -sinlat(j)
      coslat(nj+1-j) = coslat(j)
      int_wgts(nj+1-j) = int_wgts(j)
   end do

   if ((nj+1) / 2 == nj/2 + 1) then  ! Odd, then equator point:
      lat(nj/2+1) = 0.0
      sinlat(nj/2+1) = 0.0
      coslat(nj/2+1) = 1.0
      int_wgts(nj/2+1) = delta_phi
   end if

end subroutine da_get_reglats


subroutine da_initialize_h(ni, nj, max_wavenumber, lensav, alp_size, wsave, lon, sinlon, coslon, &
   lat, sinlat, coslat, int_wgts, alp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: ni                         ! Number of longitudes.
   integer, intent(in)  :: nj                         ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer, intent(in)  :: lensav                     ! Size of FFTs wsave array.
   integer, intent(in)  :: alp_size                   ! Size of ALP array.
   real,    intent(out) :: wsave(1:lensav)            ! Primes for FFT.
   real,    intent(out) :: lon(1:ni)                  ! Longitude (radians).
   real,    intent(out) :: sinlon(1:ni)               ! sine(longitude).
   real,    intent(out) :: coslon(1:ni)               ! cosine(longitude).
   real,    intent(out) :: lat(1:nj)                  ! Latitude (radians, from south).
   real,    intent(out) :: sinlat(1:nj)               ! sine(latitude).
   real,    intent(out) :: coslat(1:nj)               ! cosine(latitude).
   real,    intent(out) :: int_wgts(1:nj)             ! Legendre integration weights.
   real,    intent(out) :: alp(1:alp_size)            ! Associated Legendre Polynomial.

   integer :: i                          ! Loop counters.

   !----------------------------------------------------------------------------
   ! [1] Initialize FFT coefficients.'
   !----------------------------------------------------------------------------

   wsave(:) = 0.0
   call rfft1i(ni, wsave, lensav, ierr)

   if (ierr /= 0) then
     write(unit=message(1),fmt='(A,I4)') &
        "Fourier initialization failed. ierr = ", ierr
     call da_error("da_initialize_h.inc",43,message(1:1))
   end if

   !----------------------------------------------------------------------------
   ! [2] Calculate latitudes, and their sines/cosines.'
   !---------------------------------------------------------------------------
 
   if (gaussian_lats) then
      call da_get_gausslats(nj, lat, int_wgts, sinlat, coslat)
   else
      call da_get_reglats(nj, lat, sinlat, coslat, int_wgts)
   end if

   do i = 1, ni
      lon(i) = 2.0 * pi / real(ni) * real(i - 1)
      sinlon(i) = sin(lon(i))
      coslon(i) = cos(lon(i))
   end do

   !----------------------------------------------------------------------------
   ! [3] Initialize Legendre coefficients.'
   !----------------------------------------------------------------------------

   call da_setlegpol(nj, max_wavenumber, alp_size, sinlat, coslat, alp)

end subroutine da_initialize_h


subroutine da_legtra_inv(jds, jde, jts, jte, max_wavenumber, alp_size, m, &
   alp, v, r_leg)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: jds, jde            ! Number of latitudes.
   integer, intent(in)  :: jts, jte            ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber      ! Maximum wavenumber.
   integer, intent(in)  :: alp_size            ! Dimension of ALPs.
   integer, intent(in)  :: m                   ! Zonal wavenumber.
   real,    intent(in)  :: alp(1:alp_size)     ! Associated Legendre Polynomials

   complex, intent(in)  :: v(m:max_wavenumber) ! Output spectral coefficient.
   complex, intent(out) :: r_leg(jts:jte)         ! Field to transform.

   integer              :: l, j, js, je        ! Loop counters.
   integer              :: index_m, index_j
   complex              :: sum_legtra          ! Summation scalars.

   integer              :: jc, iequator

   index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m

   jc = (jde-jds+1)/2

   iequator = mod(jde-jds+1, 2)

   je = min(jc+iequator, jte)

   do j = jts, je
      index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2

      r_leg(j) = sum(v(m:max_wavenumber) * &
         alp(index_j+index_m+m:index_j+index_m+max_wavenumber))
   end do

   js = max(jts, jc+iequator+1)

   do j = js, jte
      index_j = (jds+jde - j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2

      sum_legtra = da_zero_complex
      do l = m, max_wavenumber
         ! Calculate second quadrant values:
         if(mod(l+m,2) == 1) then
            sum_legtra = sum_legtra - v(l) * alp(index_j + index_m + l)
         else
            sum_legtra = sum_legtra + v(l) * alp(index_j + index_m + l)
         end if
      end do
      r_leg(j) = sum_legtra
   end do

end subroutine da_legtra_inv


subroutine da_legtra (nj, max_wavenumber, alp_size, m, int_wgts, alp, r_leg, v)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: nj                      ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber          ! Maximum wavenumber.
   integer, intent(in)  :: alp_size                ! Dimension of ALPs.
   integer, intent(in)  :: m                       ! Zonal wavenumber.
   real,    intent(in)  :: int_wgts(1:nj)          ! Integration weights.
   real,    intent(in)  :: alp(1:alp_size)         ! Associated Legendre Polynomials.
   complex, intent(in)  :: r_leg(1:nj)             ! Field to transform.
   complex, intent(out) :: v(m:max_wavenumber)     ! Output spectral coefficient.

   integer              :: l, j, j1                ! Loop counters.
   integer              :: index_m, index_j, index ! Markers.
   integer              :: sign_switch             ! make use of symmetry of ALPs.
   real                 :: eq_coeff                ! 1 if equator point, 0 otherwise.
   complex              :: sum_legtra              ! Summation scalar.
   complex              :: eq_term                 ! Summation scalar.

   index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m

   if ((nj+1) / 2 == nj/2 + 1) then
      eq_coeff = 1.0 ! Odd latitudes
   else
      eq_coeff = 0.0 ! Even latitudes
      eq_term  = 0.0
   end if

   do l = m, max_wavenumber

      sign_switch = (-1)**(l + m)
      sum_legtra = da_zero_complex

      do j = 1, nj / 2
         index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2
         index = index_j + index_m + l

         ! Sum first quadrant:
         sum_legtra = sum_legtra + int_wgts(j) * r_leg(j) * alp(index)

         ! Add second quadrant (use symmetry ALP(-mu)=(-1)^{n+|m|}ALP(mu)):
         j1 = nj + 1 - j
         sum_legtra = sum_legtra + sign_switch * int_wgts(j1) * r_leg(j1) * &
            alp(index)
      end do
     
      if (eq_coeff > 0.0) then
         ! Skip this step for Even lats    ! Syed RH Rizvi
         ! Add equator term (wrong if even nj, but then eq_coeff = 0.0 so OK):
         j = nj/2 + 1
         index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber+2) / 2
         index = index_j + index_m + l
         eq_term = int_wgts(j) * r_leg(j) * alp(index)
      end if

      v(l) = 0.5 * (sum_legtra + eq_coeff * eq_term)
   end do

end subroutine da_legtra


subroutine da_setlegpol_test (nj, max_wavenumber, alp_size, int_wgts, alp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: nj              ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber  ! Maximum wavenumber.
   integer, intent(in)  :: alp_size        ! Dimension of ALPs.
   real,    intent(in)  :: int_wgts(1:nj)  ! Integration weights.
   real,    intent(in)  :: alp(1:alp_size) ! Associated Legendre Polynomials.

   real, parameter :: tolerance = 1.0e-6  ! warn if normalization error exceeds

   integer :: m, l1, l2, j, j1    ! Loop counters.
   integer :: index_m, index_j    ! Markers.
   integer :: index1, index2      ! Markers.
   integer :: sign_switch1        ! Defined to make use of symmetry of ALPs.
   integer :: sign_switch2        ! Defined to make use of symmetry of ALPs.
   real    :: eq_coeff            ! 1 if equator point, 0 otherwise.
   real    :: alp_norm_test       ! Summation scalar.
   real    :: eq_term             ! Summation scalar.
   integer :: spec_unit

   call da_get_unit(spec_unit)
   open(unit=spec_unit,file="spec_pol",status="replace")

   if ((nj+1) / 2 == nj/2 + 1) then
      eq_coeff = 1.0 ! Odd latitudes
   else
      eq_coeff = 0.0 ! Even latitudes
      eq_term  = 0.0
   end if

   ! Test 0.5 * integral_-1^1 alp(j,l1,m) * alp(j,l2,m) = 1 if l1=l2, 
   ! 0 otherwise:

   do m = 0, max_wavenumber
      index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m
      do l1 = m, max_wavenumber
         do l2 = m, max_wavenumber

            sign_switch1 = (-1)**(l1 + m)
            sign_switch2 = (-1)**(l2 + m)

            alp_norm_test = 0.0
            do j = 1, nj / 2
               index_j = (j - 1) * (max_wavenumber+1) * (max_wavenumber+2) /2
               index1 = index_j + index_m + l1
               index2 = index_j + index_m + l2

               ! Sum first quadrant:
               alp_norm_test = alp_norm_test + int_wgts(j) * alp(index1) &
                  * alp(index2)

               ! Add second quadrant (use symmetry ALP(-mu)=(-1)^{n+|m|}ALP(mu)):
               j1 = nj + 1 - j
               alp_norm_test = alp_norm_test + int_wgts(j1) * &
                  sign_switch1 * alp(index1) * sign_switch2 * alp(index2)
            end do

            if (eq_coeff > 0.0) then   
               ! Skip this step for even lats       R! Syed RH Rizvi! S
               ! Add equator term (wrong if even nj, but then eq_coeff = 0.0 
               ! so OK):
               j = nj/2 + 1
               index_j = (j - 1) * (max_wavenumber+1) * (max_wavenumber+2) /2
               index1 = index_j + index_m + l1
               index2 = index_j + index_m + l2

               eq_term = int_wgts(j) * alp(index1) * alp(index2)
            end if
            alp_norm_test = 0.5 * (alp_norm_test + eq_coeff * eq_term)

            ! if (l1 /= l2 .and. abs(alp_norm_test) >= tolerance) then
            !    write(unit=stdout,fmt='(a,3i6,f15.10,a,f15.10)')
            !      ' warning: ALP normalization error (m, l1, l2) = ', !&
            !                                      m, l1, l2, alp_norm_test, &
            !                                      ', > tolerance = ', tolerance
            !            end if
            if (l1 == l2 .and. abs(alp_norm_test-1.0) >= tolerance) then
               write(unit=spec_unit,fmt='(a,3i6,f15.10,a,f15.10)') &
                 ' warning: ALP normalization error (m, l1, l2) = ', &
                 m, l1, l2, alp_norm_test - 1.0, &
                 ', > tolerance = ', tolerance

            end if
         end do
      end do
   end do

   close(spec_unit)
   call da_free_unit(spec_unit)

end subroutine da_setlegpol_test


subroutine da_setlegpol(nj, max_wavenumber, alp_size, sinlat, coslat, alp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   !  Method:
   !  Uses ECMWF recursion relation as opposed to Num Rec. one which can only go
   !  to m about 12 with single precision). However, still use NumRec one for
   !  m = 0 and 1 as ECMWF one requires m-2 for asslegpol of order m.
   !  Reference: Jarraud and Simmons (1990) and Belousov (1962).

   integer, intent(in)  :: nj                         ! Latitude dimension.
   integer, intent(in)  :: max_wavenumber             ! Maximum zonal wavenumber.
   integer, intent(in)  :: alp_size                   ! Ass. Leg. Pol. array size.
   real,    intent(in)  :: sinlat(1:nj)               ! sine(latitude).
   real,    intent(in)  :: coslat(1:nj)               ! cosine(latitude).
   real,    intent(out) :: alp(1:alp_size)            ! Associated Legendre Polynomial.

   integer              :: j, l, m, mm                ! Loop counters.
   integer              :: l1, l2, m2                 ! Markers.
   integer              :: index_j, index_m, index    ! Indexing counters for ALP array.
   integer              :: index_m2, index_l2m2       ! Indexing counters for ALP array.
   integer              :: index_l1m2, index_l1m      ! Indexing counters for ALP array.
   real                 :: s
   real                 :: c(2:max_wavenumber,2:max_wavenumber) ! Recursion coefficient.
   real                 :: d(2:max_wavenumber,2:max_wavenumber) ! Recursion coefficient.
   real                 :: e(2:max_wavenumber,2:max_wavenumber) ! Recursion coefficient.

   alp(:) = 0.0

   ! Compute Associated Legendre polynomials for latitude range:

   do j = 1, (nj + 1) / 2
      index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2

      ! use Num. Rec. recursion relation for m = 0, 1:

      do m = 0, 1
         index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m
         do l = m, max_wavenumber
            index = index_m + index_j + l
            call da_asslegpol(l, m, sinlat(j), coslat(j), alp(index))

            ! Multiply by normalization constant 
            ! (to ensure 1/2 integral^1_-1 Pml Pml1 = 1 if l = l1):

            s = 1.0
            do mm = l-m+1, l+m
               s = s * real(mm)
            end do
            alp(index) = sqrt(real(2*l+1) / s) * alp(index)
         end do
      end do
   end do

   ! Jarraud recursion relation coefficients:

   do m = 2, max_wavenumber
      do l = m, max_wavenumber
         c(l,m) = sqrt ((real(2*l+1)/real(2*l-3)) * (real(l+m-1)/real(l+m)) * &
                  (real(l+m-3)/real(l+m-2)))
         d(l,m) = sqrt ((real(2*l+1)/real(2*l-1)) * (real(l+m-1)/real(l+m)) * &
                  (real(l-m+1)/real(l+m-2)))
         e(l,m) = sqrt ((real(2*l+1)/real(2*l-1)) * (real(l-m)  /real(l+m)))
      end do
   end do

   ! use Jarraud recursion relation for m>=2:

   do j = 1, (nj + 1) / 2
      index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2

      do m = 2, max_wavenumber
         index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m
         m2 = m - 2
         index_m2 = m2 * (max_wavenumber + 1 - m2) + m2 * (m2+1) / 2 + 1 - m2

         do l = m, max_wavenumber
            l1 = l - 1
            l2 = l - 2
            index = index_j + index_m + l
            index_l2m2 = index_j + index_m2 + l2
            index_l1m2 = index_j + index_m2 + l1
            index_l1m  = index_j + index_m  + l1

            alp(index) = c(l,m) * alp(index_l2m2) - d(l,m) *  sinlat(j) * &
               alp(index_l1m2) + e(l,m) * sinlat(j) * alp(index_l1m)
         end do
      end do
   end do

end subroutine da_setlegpol


subroutine da_vv_to_v_spectral(ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                                alp_size, r_cvsize, alp, wsave, int_wgts, rcv, field)

   !-------------------------------------------------------------------------
   ! Purpose: Performs gridpoint to spectral transformation on a sphere.
   ! Note: Routine works for both regular and Gaussian latitude (latitude 
   ! integration weights contain necessary info).
   !-------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: ni               ! Number of longitudes.
   integer, intent(in) :: nj               ! Number of latitudes.
   integer, intent(in) :: r_cvsize         ! Size of real control cv-array.
   integer, intent(in) :: max_wavenumber   ! Smallest scale required (ni/2 - 1).
   integer, intent(in) :: inc              ! Jump between elements of vector in array.
   integer, intent(in) :: lenr             ! FFT array dimension (at least inc*(n-1)+1).
   real, intent(in)    :: field(1:ni,1:nj) ! Gridpoint field.
   real, intent(out)   :: rcv(1:r_cvsize)  ! Spectral modes.
   integer, intent(in) :: lensav           ! wsave dimension (n+int(log(real(ni)))+4).
   integer, intent(in) :: lenwrk           ! Dimension of work array.
   integer, intent(in) :: alp_size         ! Size of ALP vector.
   real, intent(in)    :: alp(1:alp_size)  ! Associated Legendre Polynomials.
   real, intent(in)    :: wsave(1:lensav)  ! Primes for FFT.
   real, intent(in)    :: int_wgts(1:nj)   ! Legendre integration weights.

   integer             :: i, j, m, mm            ! Loop counters.
   integer             :: sizec                  ! Size of complex cv-array
   integer             :: index_r, index_c       ! Array index for complex v_fou
   integer             :: index_start, index_end ! Position markers in cv.
   real                :: r_fou(1:lenr)          ! FFT array.
   logical             :: odd_longitudes
   complex             :: v_fou(1:nj,0:max_wavenumber)! Intermediate Fourier state.
   complex             :: r_leg(1:nj)                 ! Intermediate Fourier state.
   complex, allocatable:: ccv(:)                      ! Spectral modes.

   real                :: work(1:lenwrk)         ! FFT work array. 

   sizec = int(0.5 * r_cvsize)
   allocate (ccv(1:sizec))

   if ((ni+1) / 2 == ni/2 + 1) then  ! Odd number of longitudes:
      odd_longitudes = .true.
   else                                ! Even number of longitudes:
      odd_longitudes = .false.
   end if

   !-------------------------------------------------------------------------
   ! [1] Perform Adjoint of inverse Fourier decomposition in E-W direction:
   !-------------------------------------------------------------------------
 
   if ((ni+1) / 2 == ni/2 + 1) then  ! Odd number of longitudes:
      odd_longitudes = .true.
   else                                ! Even number of longitudes:
      odd_longitudes = .false.
   end if

   ! [1] Perform Fourier decomposition in E-W direction:

   do j = 1, nj
      r_fou(1:ni) = field(1:ni,j)
      call rfft1f(ni, inc, r_fou, lenr, wsave, lensav, work, lenwrk, ierr)

      if (odd_longitudes) then
         v_fou(j,0) = CMPLX(r_fou(1), 0.0) ! m = 0 is real.
      else
         ! m = 0 is real, but pack R(NI/2) in imag m = 0:
         v_fou(j,0) = CMPLX(r_fou(1), r_fou(ni))
      end if

      do m = 1, max_wavenumber
         index_r = 2 * m
         index_c = 2 * m + 1
         v_fou(j,m) = CMPLX(r_fou(index_r), r_fou(index_c)) ! 2.0 * Fourier mode.
      end do
   end do

   ! [2] Perform Legendre decomposition in N-S direction:

   do m = 0, max_wavenumber
      index_start = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1
      index_end   = index_start + max_wavenumber - m
      r_leg(1:nj) = v_fou(1:nj,m)
      call da_legtra (nj, max_wavenumber, alp_size, m, int_wgts, alp, r_leg, &
                        ccv(index_start:index_end))
   end do

   do i=1,sizec
      mm = 2*i - 1
      rcv(mm ) = real (ccv(i))
      rcv(mm+1) = aimag(ccv(i))
   end do
   deallocate (ccv)

end subroutine da_vv_to_v_spectral


subroutine da_legtra_inv_adj(jds, jde, jts, jte, max_wavenumber, alp_size, &
   m, alp, v, r_leg)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: jds, jde            ! Number of latitudes.
   integer, intent(in)  :: jts, jte            ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber      ! Maximum wavenumber.
   integer, intent(in)  :: alp_size            ! Dimension of ALPs.
   integer, intent(in)  :: m                   ! Zonal wavenumber.
   real,    intent(in)  :: alp(1:alp_size)     ! Associated Legendre Polynomials

   complex, intent(out) :: v(m:max_wavenumber) ! Output spectral coefficient.
   complex, intent(in)  :: r_leg(jts:jte)      ! Field to transform.

   integer              :: l, j, js, je        ! Loop counters.
   integer              :: index_m, index_j    ! Markers.
   complex              :: sum_legtra          ! Summation scalars.

   integer              :: jc, iequator, temp

   index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m

   jc = (jde-jds+1)/2

   iequator = mod(jde-jds+1, 2)

   js = max(jts, jc+iequator+1)
   je = min(jc+iequator, jte)

   temp = (max_wavenumber + 1) * (max_wavenumber + 2) / 2

   do l = m, max_wavenumber
      sum_legtra = da_zero_complex

      if (mod(l+m,2) == 1) then
         do j = js, jte
            index_j = (jds+jde - j - 1) * temp
            sum_legtra = sum_legtra - r_leg(j) * alp(index_j + index_m + l)
         end do
      else
         do j = js, jte
            index_j = (jds+jde - j - 1) * temp
            sum_legtra = sum_legtra + r_leg(j) * alp(index_j + index_m + l)
         end do
      end if

      do j = jts, je
         index_j = (j - 1) * temp
         sum_legtra = sum_legtra + r_leg(j) * alp(index_j + index_m + l) 
      end do

      v(l) = sum_legtra
   end do

end subroutine da_legtra_inv_adj


subroutine da_apply_power (power, max_wavenumber, ccv, c_cvsize) 

   !-------------------------------------------------------------------------
   ! Purpose: Adjust power spectrum for the control variable
   !-------------------------------------------------------------------------

   implicit none

   integer, intent(in)   :: max_wavenumber ! Smallest scale required (ni/2 - 1).
   integer, intent(in)   :: c_cvsize                ! Size of complex cv-array
   real*8,  intent(in)   :: power(0:max_wavenumber) ! Power Spectrum
   complex, intent(inout):: ccv(1:c_cvsize)         ! complex control  array   
  
   integer             :: m, n, index

   ! Adjust Power spectrum

   do n =0, max_wavenumber
      
      ccv(n+1) = ccv(n+1) * sqrt (power(n)/(2*n+1))

      do m=1, n
         index = m*(max_wavenumber+1- m) + m*(m+1)/2+n-m + 1
         ccv(index) = ccv(index) * sqrt (power(n)/(2*n+1))
      end do
   end do

end subroutine da_apply_power           



end module da_be_spectral

