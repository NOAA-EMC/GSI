module da_recursive_filters 
implicit none
contains
!-----------------------------------------------------------------------------------------!
subroutine da_perform_2drf(ni, nj, num_passes, rf_scale, field)

   implicit none

   integer, intent(in)    :: ni               ! Array dimension 1.
   integer, intent(in)    :: nj               ! Array dimension 2.
   integer, intent(in)    :: num_passes       ! Number of passes of RF.
   real,    intent(in)    :: rf_scale         ! Recursive filter scaling parameter.
   real*8,  intent(inout) :: field(1:ni,1:nj) ! Field to be filtered.

   integer               :: i, j, pass       ! Loop counters.
   real*8                :: e, alpha         ! Recursive filter parameters.
   real                  :: mean_field       ! Mean field.

   e = 0.25 * num_passes / (rf_scale * rf_scale)
   alpha = 1 + e - sqrt(e * (e + 2.0))

   mean_field = sum(field(1:ni,1:nj)) / real(ni*nj)

   do pass = 1, num_passes
      ! Perform filter in I-direction:
      do j = 1, nj
         call da_recursive_filter_1d(pass, alpha, field(1:ni,j), ni)
      end do

      ! Perform filter in J-direction:
      do i = 1, ni
         call da_recursive_filter_1d(pass, alpha, field(i,1:nj), nj)
      end do
   end do

end subroutine da_perform_2drf
!-----------------------------------------------------------------------------------------!
subroutine da_recursive_filter_1d(pass, alpha, field, n)

   !---------------------------------------------------------------------------
   ! Purpose: Perform one pass of recursive filter on 1D array.
   !
   ! Method:  Perform right-moving filter followed by left-moving filter.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: pass           ! Current pass of filter.
   real*8,  intent(in)    :: alpha          ! Alpha coefficient for RF.
   real*8,  intent(inout) :: field(:)       ! Array to be filtered.
   integer, intent(in)    :: n              ! Size of field array.

   integer :: j              ! Loop counter.
   real    :: one_alpha      ! 1 - alpha.
   real    :: a(1:n)         ! Input field.
   real    :: b(1:n)         ! Field after left-right pass.
   real    :: c(1:n)         ! Field after right-left pass.

   !-------------------------------------------------------------------------
   ! [1.0] Initialise:
   !-------------------------------------------------------------------------

   one_alpha = 1.0 - alpha
   
   a(1:n) = field(1:n)

   !-------------------------------------------------------------------------
   ! [2.0] Perform right-moving filter:
   !-------------------------------------------------------------------------

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   if (pass == 1) then
      b(1) = one_alpha * a(1)
   else if (pass == 2) then
      b(1) = a(1) / (1.0 + alpha)
   else
      b(1) = one_alpha * (a(1) - alpha**3 * a(2)) / (1.0 - alpha**2)**2
   end if

   ! [2.2] Perform pass left to right:

   do j = 2, n
      b(j) = alpha * b(j-1) + one_alpha * a(j)
   end do

   !-------------------------------------------------------------------------
   ! [3.0] Perform left-moving filter:
   !-------------------------------------------------------------------------

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   if (pass == 1) then
      c(n) = b(n) / (1.0 + alpha)
   else
      c(n) = one_alpha * (b(n) - alpha**3 * b(n-1)) / (1.0 - alpha**2)**2
   end if

   ! [3.2] Perform pass left to right:

   do j = n-1, 1, -1
      c(j) = alpha * c(j+1) + one_alpha * b(j)
   end do
        
   field(1:n) = c(1:n)

end subroutine da_recursive_filter_1d
!-----------------------------------------------------------------------------------------!

end module da_recursive_filters
