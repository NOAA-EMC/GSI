module da_fft_initialize

use da_control, only : num_fft_factors, pi
use da_tools_serial, only : da_find_fft_factors, da_find_fft_trig_funcs

implicit none

integer, parameter    :: nrange = 50               ! Range to search for efficient FFT.

contains

!------------------------------------------------------------------------------

subroutine da_fft_initialize1( dim1, dim2, n1, n2, ifax1, ifax2 )

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   integer, intent(out):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   integer, intent(out):: ifax1(1:num_fft_factors)     ! FFT factors.
   integer, intent(out):: ifax2(1:num_fft_factors)     ! FFT factors.

   integer            :: n                            ! n+1 is the length of the data.
   integer            :: fft_pad1, fft_pad2           ! Range to search for efficient FFT.
   logical            :: found_magic                  ! True if 2**p 3**p 5**r dimension found..

   integer            :: fft_factors(1:num_fft_factors)! FFT factors. 

!  Ensure efficient FFT dimensions by padding if necessary:
   n1 = dim1 - 1 
   do n = n1, n1 + nrange
      call da_find_fft_factors( n, found_magic, fft_factors ) 
      if ( found_magic .and. mod(n,2) == 0 ) then ! Even magic number found.
         fft_pad1 = n - n1
         ifax1 = fft_factors
         exit
      end if 
   end do
   n1 = n1 + fft_pad1

   n2 = dim2 - 1
   do n = n2, n2 + nrange
      call da_find_fft_factors( n, found_magic, fft_factors )
      if ( found_magic .and. mod(n,2) == 0 ) then ! Even magic number found.
         fft_pad2 = n - n2
         ifax2 = fft_factors
         exit
      end if
   end do
   n2 = n2 + fft_pad2

end subroutine da_fft_initialize1

!------------------------------------------------------------------------------

subroutine da_fft_initialize2( n1, n2, ds, trigs1, trigs2, fft_coeffs )

!  Need to split fft_initialize as array dimensions need to be calculated first.

   implicit none

   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   real, intent(in)   :: ds                           ! Grid resolution.
   real, intent(out)  :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(out)  :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(out)  :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.

   integer            :: i, j                         ! Loop counters.
   real               :: const                        ! Multiplicative constant.
   real               :: coeff_nx                     ! Multiplicative constant.
   real               :: coeff_ny                     ! Multiplicative constant.
   real               :: cos_coeff_nx                 ! Multiplicative constant.
   real               :: cos_coeff_ny                 ! Multiplicative constant.

   const = -0.5 * ds * ds
   coeff_nx = pi / real(n1)
   coeff_ny = pi / real(n2)

!  Calculate spectral Del**2 coefficients for C-grid (all pts. except i=j=1):
   fft_coeffs(1,1) = 0.0 ! Not used?
   do j = 2, n2+1
      cos_coeff_ny = cos(coeff_ny * real(j - 1))
      do i = 1, n1+1
         cos_coeff_nx = cos(coeff_nx * real(i - 1))
         fft_coeffs(i,j) = const / ( 2.0 - cos_coeff_nx - cos_coeff_ny)
      end do
   end do
   j = 1
   cos_coeff_ny = cos(coeff_ny * real(j - 1))
   do i = 2, n1+1
      cos_coeff_nx = cos(coeff_nx * real(i - 1))
      fft_coeffs(i,j) = const / ( 2.0 - cos_coeff_nx - cos_coeff_ny)
   end do

   call da_find_fft_trig_funcs( n1, trigs1 )
   call da_find_fft_trig_funcs( n2, trigs2 )

end subroutine da_fft_initialize2

!------------------------------------------------------------------------------

end module da_fft_initialize
