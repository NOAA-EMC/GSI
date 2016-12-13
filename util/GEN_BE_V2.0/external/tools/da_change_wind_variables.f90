module da_change_wind_variables
 
   use da_control, only : num_fft_factors
   use module_ffts, only : fft551, fft661
   

implicit none

contains

!------------------------------------------------------------------------------

subroutine da_uv_to_div_c( dim1, dim2, ds, &
                           mapfac_m, mapfac_u, mapfac_v, & 
                           u, v, div )
   
!------------------------------------------------------------------------------
!  PURPOSE: Calculate divergence on a co-ordinate surface, given an input
!           wind field on an Arakawa C-grid.
!  
!  NOTE: No boundary conditions required on the WRF Arakawa C-grid as
!        divergence (mass) points are all within the outer u/v pts.
!
!           Div = m^2 *[---(---) + ---(---) ]
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
   real, intent(in)   :: u(1:dim1+1,1:dim2)           ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2+1)           ! v wind.
   real, intent(out)  :: div(1:dim1,1:dim2)           ! Divergence.

   integer            :: i, j                         ! Loop counters.
   real               :: ds_inv                       ! 1/ds.
   real               :: coeff(1:dim1,1:dim2)         ! Coefficient.
   real               :: um(1:dim1+1,1:dim2)          ! u-wind copy.
   real               :: vm(1:dim1,1:dim2+1)          ! v-wind copy. 

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   ds_inv = 1.0 / ds
!   do j = 1, dim2
!      do i = 1, dim1
!         coeff(i,j) = ( mapfac_m(i,j) * mapfac_m(i,j) ) * ds_inv
!      end do
!   end do
   coeff(:,:) = ds_inv ! Calculate f.d. Del**2 Chi, rather than divergence.

!------------------------------------------------------------------------------
!  [2] Calculate divergence field:
!------------------------------------------------------------------------------

   do j = 1, dim2
      do i = 1, dim1+1
         um(i,j) = u(i,j) / mapfac_u(i,j)
      end do
   end do

   do j = 1, dim2+1
      do i = 1, dim1
         if (mapfac_v(i,j) > 0.000001) then
            vm(i,j) = v(i,j) / mapfac_v(i,j)
         else
            vm(i,j)=0.0
         end if
      end do
   end do

   do j = 1, dim2
      do i = 1, dim1
         div(i,j) = coeff(i,j) * ( um(i+1,j) - um(i,j) + vm(i,j+1) - vm(i,j) )
      end do
   end do

end subroutine da_uv_to_div_c


subroutine da_uv_to_vor_c( dim1, dim2, ds, &
                           mapfac_m, mapfac_u, mapfac_v, &
                           u, v, vor )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate vorticity on a co-ordinate surface, given an input
!           wind field on an Arakawa C-grid.
!  
!  NOTE: Zero vorticity boundary conditions.
!
!                        d   V      d   U
!           Vor = m^2 *[---(---) - ---(---) ]
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                ! Dimensions.
   real, intent(in)   :: ds                        ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)   ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2) ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1) ! Map factor - u points.
   real, intent(in)   :: u(1:dim1+1,1:dim2)        ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2+1)        ! v wind.
   real, intent(out)  :: vor(1:dim1+1,1:dim2+1)    ! Vorticity.

   integer            :: i, j                      ! Loop counters.
   real               :: ds_inv                    ! 1/ds.
   ! real               :: mapfac_vor               ! Map factor (vorticity pts)
   real               :: coeff(1:dim1,1:dim2)      ! Coefficient.
   real               :: um(1:dim1+1,1:dim2)       ! u-wind copy.
   real               :: vm(1:dim1,1:dim2+1)       ! v-wind copy. 

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   vor(:,:) = 0.0

   ds_inv = 1.0 / ds
!   do j = 1, dim2
!      do i = 1, dim1
!         mapfac_vor = 0.25 * ( mapfac_u(i,j-1) + mapfac_u(i,j) + &
!                               mapfac_v(i-1,j) + mapfac_v(i,j) ) ! Average.
!         coeff(i,j) = ( mapfac_vor * mapfac_vor ) * ds_inv
!      end do
!   end do
   coeff(:,:) = ds_inv ! Calculate f.d. Del**2 Chi, rather than divergence.

!------------------------------------------------------------------------------
!  [2] Calculate vorticity field:
!------------------------------------------------------------------------------

   do j = 1, dim2  
      do i = 1, dim1+1
         um(i,j) = u(i,j) / mapfac_u(i,j)
      end do
   end do

   do j = 1, dim2+1
      do i = 1, dim1  
         if (mapfac_v(i,j) > 0.000001) then
            vm(i,j) = v(i,j) / mapfac_v(i,j)
         else
            vm(i,j) = 0.0
         end if
      end do
   end do

   do j = 2, dim2
      do i = 2, dim1
         vor(i,j) = coeff(i,j) * ( vm(i,j) - vm(i-1,j) - um(i,j) + um(i,j-1) )
      end do
   end do

!  Boundary values (extrapolation):
!  Note - not used in Del**2 calculation if overwritten with bcs there).
!   vor(1,1:dim2+1)      = 2.0 * vor(2,1:dim2+1) - vor(3,1:dim2+1)         ! West
!   vor(dim1+1,1:dim2+1) = 2.0 * vor(dim1,1:dim2+1) - vor(dim1-1,1:dim2+1) ! East
!   vor(1:dim1+1,1)      = 2.0 * vor(1:dim1+1,2) - vor(1:dim1+1,3)         ! South
!   vor(1:dim1+1,dim2+1) = 2.0 * vor(1:dim1+1,dim2) - vor(1:dim1+1,dim2-1) ! South

!  Boundary values (zero gradient):
!  Note - not used in Del**2 calculation if overwritten with bcs there).
   vor(1,2:dim2)        = vor(2,2:dim2)      ! West
   vor(dim1+1,2:dim2)   = vor(dim1,2:dim2)   ! East
   vor(1:dim1+1,1)      = vor(1:dim1+1,2)    ! South
   vor(1:dim1+1,dim2+1) = vor(1:dim1+1,dim2) ! South

end subroutine da_uv_to_vor_c

subroutine da_psichi_to_uv_c( dim1, dim2, ds, &
                              mapfac_u, mapfac_v, &
                              psi, chi, u, v )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate u and v wind components on an Arakawa C-grid.
!
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velcoity potential.
   real, intent(out)  :: u(1:dim1+1,1:dim2)           ! v wind.
   real, intent(out)  :: v(1:dim1,1:dim2+1)           ! v wind.

   integer            :: i, j                         ! Loop counters.
   integer            :: its, ite, jts, jte           ! WRF dims (dummies for now).
   real               :: ds_inv                       ! 1/ds.
   real               :: one_third                    ! 1/3.

   ds_inv = 1.0 / ds
   one_third = 1.0 / 3.0

!  u-wind component:
   its = 2
   ite = dim1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite
         u(i,j) = mapfac_u(i,j) * ds_inv * &
                  ( -psi(i,j+1) + psi(i,j) + chi(i,j) - chi(i-1,j) )
      end do
   end do

!  Remaining points on E/W boundaries (extrapolation):
   u(1,jts:jte) = 2.0 * u(2,jts:jte) - u(3,jts:jte)
   u(dim1+1,jts:jte) = 2.0 * u(dim1,jts:jte) - u(dim1-1,jts:jte)

!  v-wind component:
   its = 1
   ite = dim1
   jts = 2
   jte = dim2

   do j = jts, jte
      do i = its, ite
         v(i,j) = mapfac_v(i,j) * ds_inv * &
                  ( psi(i+1,j) - psi(i,j) + chi(i,j) - chi(i,j-1) )
      end do
   end do

!  Remaining points on S/N boundaries (extrapolation):
   v(its:ite,1) = 2.0 * v(its:ite,2) - v(its:ite,3)
   v(its:ite,dim2+1) = 2.0 * v(its:ite,dim2) - v(its:ite,dim2-1)

end subroutine da_psichi_to_uv_c

!------------------------------------------------------------------------------

subroutine da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, &
                          fft_coeffs, del2a, a )

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   integer, intent(in):: fft_method                   ! 1=Cosine, 2=Sine transform.
   integer, intent(in):: ifax1(1:num_fft_factors)     ! FFT factors.
   integer, intent(in):: ifax2(1:num_fft_factors)     ! FFT factors.
   real, intent(in)   :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(in)   :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(in)   :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.
   real, intent(in)   :: del2a(1:dim1,1:dim2)         ! Del**2 a.
   real, intent(out)  :: a(1:dim1,1:dim2)             ! Field a.

   integer            :: i, j                         ! Loop counters.
   integer            :: ij                           ! 1D array counter.
   integer            :: isign                        ! -1=Grid>spec, 1=Spec>Grid.
   integer            :: inc                          ! Stride between data points.
   integer            :: jump                         ! Increment between start of data vectors.
   integer            :: lot                          ! Number of data vectors.
   integer            :: n                            ! n+1 is the length of the data.
   integer            :: work_area                    ! Dimension of workspace.
   real               :: a2d(1:n1+1,1:n2+1)           ! 2D data array.
   real               :: a1d(1:(n1+1)*(n2+1))         ! 1D data array.

   work_area = ( n1 + 1 ) * ( n2 + 1 )

!  Fill 2D array structure
   do j = 1, dim2
      do i = 1, dim1
         a2d(i,j) = del2a(i,j)
      end do

!     Fill pad zone (and force b.c.s to satisfy solution type):
      if ( fft_method == 1 ) then ! Cosine transform.
         a2d(1,j) = a2d(2,j)
         do i = dim1, n1+1
            a2d(i,j) = a2d(dim1-1,j)
         end do
      else if ( fft_method == 2 ) then ! Sine transform:
         a2d(1,j) = 0.0
         do i = dim1, n1+1
            a2d(i,j) = 0.0
         end do
      end if
   end do

   if ( fft_method == 1 ) then ! Cosine transform.
      do i = 1, n1+1
         a2d(i,1) = a2d(i,2)
         do j = dim2, n2+1
            a2d(i,j) = a2d(i,dim2-1)
         end do
      end do
   else if ( fft_method == 2 ) then ! Sine transform:
      do i = 1, n1+1
         a2d(i,1) = 0.0
         do j = dim2, n2+1
            a2d(i,j) = 0.0
         end do
      end do
   end if

!  Transfer to data array:
   do j = 1, n2+1
      do i = 1, n1+1
         ij = (j-1) * (n1+1) + i
         a1d(ij) = a2d(i,j)
      end do
   end do

!------------------------------------------------------------------------------
!     Perform double fast sine/cosine transform to get spectral del2a:
!------------------------------------------------------------------------------

   isign = -1 ! Grid to spectral

!  1st dimension:
   inc = 1    ! Stride between data points.
   jump = n1+1! Increment between start of data vectors.
   lot = n2+1 ! Number of data vectors.
   n = n1     ! n+1 is the length of the data.
   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax2, trigs2, a1d, work_area )
   end if

!------------------------------------------------------------------------------
!  Perform conversion from del2a to a in spectral space:
!------------------------------------------------------------------------------

!  Note fft_coeffs(1,1)=0 so a(k=0,l=0) is also 0.
   do j = 1, n2+1
      do i = 1, n1+1
         ij = (j-1) * (n1+1) + i
         a1d(ij) = fft_coeffs(i,j) * a1d(ij)
      end do
   end do

!------------------------------------------------------------------------------
!  Perform double fast sine/cosine transform to get gridpoint a:
!------------------------------------------------------------------------------

   isign = 1 ! Spectral to grid.

!  1st dimension:
   inc = 1    ! Stride between data points.
   jump = n1+1! Increment between start of data vectors.
   lot = n2+1 ! Number of data vectors.
   n = n1     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax2, trigs2, a1d, work_area )
   end if

!  Transfer grid-point chi to 2D-array (throwing away pad):
   do j = 1, dim2
      do i = 1, dim1
         ij = (j-1) * (n1+1) + i
         a(i,j) = a1d(ij)
      end do
   end do

end subroutine da_del2a_to_a

!---------------------------------------------------------------------------------------------
subroutine da_test_inverse( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                            n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, fft_coeffs, &
                            n1s, n2s, ifax1s, ifax2s, trigs1s, trigs2s, fft_coeffss, &
                            u1, v1, psi, chi )

!------------------------------------------------------------------------------
!  PURPOSE: Test u, v -> psi, chi calculation by performing inverse test.
!
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   integer, intent(in):: fft_method                   ! 1=Cosine, 2=Sine transform.
   integer, intent(in):: ifax1(1:num_fft_factors)     ! FFT factors.
   integer, intent(in):: ifax2(1:num_fft_factors)     ! FFT factors.
   real, intent(in)   :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(in)   :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(in)   :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.
   integer, intent(in):: n1s, n2s                     ! Padded dimensions (n=dim-1+pad).
   integer, intent(in):: ifax1s(1:num_fft_factors)    ! FFT factors.
   integer, intent(in):: ifax2s(1:num_fft_factors)    ! FFT factors.
   real, intent(in)   :: trigs1s(1:3*n1)              ! FFT trig functions.
   real, intent(in)   :: trigs2s(1:3*n2)              ! FFT trig functions.
   real, intent(in)   :: fft_coeffss(1:n1+1,1:n2+1)   ! FFT coefficients.

   real, intent(in)   :: u1(1:dim1+1,1:dim2)          ! u
   real, intent(in)   :: v1(1:dim1,1:dim2+1)          ! v
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velocity potential.

   real               :: div(1:dim1,1:dim2)           ! Divergence.
   real               :: vor(1:dim1+1,1:dim2+1)       ! Vorticity.

   real               :: u2(1:dim1+1,1:dim2)          ! u
   real               :: v2(1:dim1,1:dim2+1)          ! v
   real               :: u3(1:dim1+1,1:dim2)          ! u
   real               :: v3(1:dim1,1:dim2+1)          ! v
   real               :: psi1(1:dim1+1,1:dim2+1)      ! streamfunction
   real               :: chi1(1:dim1,1:dim2)          ! divergence
  !--ym--!
  integer               :: dim1s                     ! Dimensions of grid (vor/psi pts).
  integer               :: dim2s                     ! Dimensions of grid (vor/psi pts).
  !--ym--!

   write(6,'(a,i4)')' Using FFT method (1=Cosine, 2=Sine): ', fft_method

   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi, chi, u2, v2 )

   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field u = ', &
                         sqrt(sum( ( u1(:,:) -  u2(:,:) )**2 ) / sum( u1(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field v = ', &
                         sqrt(sum( ( v1(:,:) -  v2(:,:) )**2 ) / sum( v1(:,:)**2 ))

   call da_uv_to_div_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, div )
   call da_uv_to_vor_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, vor )
   !--ym--!		
   dim1s = dim1+1 ! Vorticity/streamfunction array 1 larger.
   dim2s = dim2+1 ! Vorticity/streamfunction array 1 larger.
   !--ym--!   
   call da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, &
                       fft_coeffs, div, chi1 )
   call da_del2a_to_a( dim1s, dim2s, n1s, n2s, fft_method, ifax1s, ifax2s, trigs1s, trigs2s, &
                       fft_coeffss, vor, psi1 )

   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field psi = ', &
                         sqrt(sum( ( psi(:,:) -  psi1(:,:) )**2 ) / sum( psi(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field chi = ', &
                         sqrt(sum( ( chi(:,:) -  chi1(:,:) )**2 ) / sum( chi(:,:)**2 ))

   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi1, chi1, u3, v3 )

   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field u = ', &
                         sqrt(sum( ( u3(:,:) -  u2(:,:) )**2 ) / sum( u2(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field v = ', &
                         sqrt(sum( ( v3(:,:) -  v2(:,:) )**2 ) / sum( v2(:,:)**2 ))

end subroutine da_test_inverse

!---------------------------------------------------------------------------------------------
subroutine da_laplacian_a( ni, nj, n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, &
       fft_coeffs, mapfac, field_a, laplacian_field_a )
    implicit none
    integer, intent(in):: ni, nj                   ! Dimensions.
    integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
    integer, intent(in):: fft_method                   ! 1=Cosine, 2=Sine transform.
    integer, intent(in):: ifax1(1:num_fft_factors)     ! FFT factors.
    integer, intent(in):: ifax2(1:num_fft_factors)     ! FFT factors.
    real, intent(in)   :: trigs1(1:3*n1)               ! FFT trig functions.
    real, intent(in)   :: trigs2(1:3*n2)               ! FFT trig functions.
    real, intent(in)   :: fft_coeffs(1:(n1+1),1:(n2+1))    ! FFT coefficients.
    real, intent(in)   :: mapfac(1:ni,1:nj)            ! Grid map factor
    real, intent(in)   :: field_a(1:ni,1:nj)           ! Field a.
    real, intent(out)  :: laplacian_field_a(1:ni,1:nj) ! Laplacian of Field a.

    integer            :: i, j                         ! Loop counters.
    integer            :: ij                           ! 1D array counter.
    integer            :: isign                        ! -1=Grid>spec, 1=Spec>Grid.
    integer            :: inc                          ! Stride between data points.
    integer            :: jump                         ! Increment between start of data vectors.
    integer            :: lot                          ! Number of data vectors.
    integer            :: n                            ! n+1 is the length of the data.
    integer            :: work_area                    ! Dimension of workspace.
    real               :: a2d(1:(n1+1),1:(n2+1))           ! 2D data array.
    real               :: a1d(1:(n1+1)*(n2+1))         ! 1D data array.

    work_area = ( n1 + 1 ) * ( n2 + 1 )

    !  Fill 2D array structure
    do j = 1, nj
       do i = 1, ni
          a2d(i,j) = field_a(i,j)
       end do

       !     Fill pad zone (and force b.c.s to satisfy solution type):
       if ( fft_method == 1 ) then ! Cosine transform.
          a2d(1,j) = a2d(2,j)
          do i = ni, n1+1
             a2d(i,j) = a2d(ni-1,j)
          end do
       else if ( fft_method == 2 ) then ! Sine transform:
          a2d(1,j) = 0.0
          do i = ni, n1+1
             a2d(i,j) = 0.0
          end do
       end if
    end do

    if ( fft_method == 1 ) then ! Cosine transform.
       do i = 1, n1+1
          a2d(i,1) = a2d(i,2)
          do j = nj, n2+1
             a2d(i,j) = a2d(i,nj-1)
          end do
       end do
    else if ( fft_method == 2 ) then ! Sine transform:
       do i = 1, n1+1
          a2d(i,1) = 0.0
          do j = nj, n2+1
             a2d(i,j) = 0.0
          end do
       end do
    end if

    !  Transfer to data array:
    do j = 1, n2+1
       do i = 1, n1+1
          ij = (j-1) * (n1+1) + i
          a1d(ij) = a2d(i,j)
       end do
    end do

    !------------------------------------------------------------------------------
    !     Perform double fast sine/cosine transform to get spectral field_a:
    !------------------------------------------------------------------------------

    isign = -1 ! Grid to spectral

    !  1st dimension:
    inc = 1    ! Stride between data points.
    jump = n1+1! Increment between start of data vectors.
    lot = n2+1 ! Number of data vectors.
    n = n1     ! n+1 is the length of the data.
    if ( fft_method == 1 ) then
       call fft551( isign, inc, jump, lot, n, &
            ifax1, trigs1, a1d, work_area )
    else if ( fft_method == 2 ) then
       call fft661( isign, inc, jump, lot, n, &
            ifax1, trigs1, a1d, work_area )
    end if

    !  2nd dimension:
    inc = n1+1 ! Stride between data points.
    jump = 1   ! Increment between start of data vectors.
    lot = n1+1 ! Number of data vectors.
    n = n2     ! n+1 is the length of the data.

    if ( fft_method == 1 ) then
       call fft551( isign, inc, jump, lot, n, &
            ifax2, trigs2, a1d, work_area )
    else if ( fft_method == 2 ) then
       call fft661( isign, inc, jump, lot, n, &
            ifax2, trigs2, a1d, work_area )
    end if

    !------------------------------------------------------------------------------
    !  Perform conversion from field_a to laplacian(field_a) in spectral space:
    !------------------------------------------------------------------------------

    !  Note fft_coeffs(1,1)=0 so a(k=0,l=0) is also 0.
    do j = 1, n2+1
       do i = 1, n1+1
          ij = (j-1) * (n1+1) + i
          !         a1d(ij) = fft_coeffs(i,j) * a1d(ij)
          if (fft_coeffs(i,j).eq.0) then                    
             a1d(ij) = 0.0
          else
             a1d(ij) = a1d(ij)/fft_coeffs(i,j)
          end if
       end do
    end do
    !
    !------------------------------------------------------------------------------
    !  Perform double fast sine/cosine transform to get gridpoint a:
    !------------------------------------------------------------------------------

    isign = 1 ! Spectral to grid.

    !  1st dimension:
    inc = 1    ! Stride between data points.
    jump = n1+1! Increment between start of data vectors.
    lot = n2+1 ! Number of data vectors.
    n = n1     ! n+1 is the length of the data.

    if ( fft_method == 1 ) then
       call fft551( isign, inc, jump, lot, n, &
            ifax1, trigs1, a1d, work_area )
    else if ( fft_method == 2 ) then
       call fft661( isign, inc, jump, lot, n, &
            ifax1, trigs1, a1d, work_area )
    end if

    !  2nd dimension:
    inc = n1+1 ! Stride between data points.
    jump = 1   ! Increment between start of data vectors.
    lot = n1+1 ! Number of data vectors.
    n = n2     ! n+1 is the length of the data.

    if ( fft_method == 1 ) then
       call fft551( isign, inc, jump, lot, n, &
            ifax2, trigs2, a1d, work_area )
    else if ( fft_method == 2 ) then
       call fft661( isign, inc, jump, lot, n, &
            ifax2, trigs2, a1d, work_area )
    end if

    !  Transfer grid-point chi to 2D-array (throwing away pad + map factor )
    do j = 1, nj
       do i = 1, ni
          ij = (j-1) * (n1+1) + i
          laplacian_field_a(i,j) = a1d(ij)/mapfac(i,j)**2.0
       end do
    end do

! rizvi fix for zero field at (1,1) 
          laplacian_field_a(1,1) = laplacian_field_a(1,2) 

  end subroutine da_laplacian_a
  
end module da_change_wind_variables
