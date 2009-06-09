subroutine psichi2uvt_reg( u, v,  psi, chi)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    psichi2uvt_reg
!     prgmmr:    barker, d   org:  np22              date:  2000-02-03
!
! abstract:  Calculate adjoint of psi and chi from adjoint of 
!            wind components u and v.
!
! program history log:
!   2000/02/03   barker, dale - creation of F90 version
!   2001/10/30   barker       - parallel version
!   2003/09/05   parrish      - adapted to unified NCEP 3dvar
!   2003/10/17   wu, wanshu   - first index Y second X
!   2004-06-22   treadon      - update documentation
!   2008-04-23   safford      - rm unused vars
!   2009-04-19   derber       - modify to fit gsi
!
!   input argument list:
!     u - adjoint of zonal wind component
!     v - adjoint of meridional wind component
!
!   output argument list:
!     psi - adjoint of streamfunction
!     chi - adjoint of velocity potential
!
! remarks:
!    The method used is 
!       u = ( -dpsi/dy + dchi/dx )
!       v = (  dpsi/dx + dchi/dy )
!
!    The assumptions made in this routine are:
!       - unstaggered grid,
!       - lateral boundary conditions - dpsi/dn, dchi/dn = 0 (FCT)
!       - dy=rearth*dph , dx=cos(ph)*rearth*dlm (dx,dy is rotated grid)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,half
  use gridmod, only:  coeffx,coeffy,nlat,nlon
  implicit none
  
  
  real(r_kind),intent(inout):: u(nlat,nlon)   ! u wind comp (m/s)
  real(r_kind),intent(inout):: v(nlat,nlon)   ! v wind comp (m/s)
  real(r_kind),intent(inout):: psi(nlat,nlon) ! Stream function
  real(r_kind),intent(inout):: chi(nlat,nlon) ! Velocity potential
  
  integer(i_kind)           :: i, j           ! Loop counters.
  real(r_kind)      :: coeffx_u               ! Multiplicative coefficient.
  real(r_kind)      :: coeffy_u               ! Multiplicative coefficient.
  real(r_kind)      :: coeffx_v               ! Multiplicative coefficient.
  real(r_kind)      :: coeffy_v               ! Multiplicative coefficient.
  
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

  psi=zero
  chi=zero

   
!------------------------------------------------------------------------------
!     [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

        u(2,1) = u(2,1) + half * u(1,1)
        u(1,2) = u(1,2) + half * u(1,1)
        v(2,1) = v(2,1) + half * v(1,1)
        v(1,2) = v(1,2) + half * v(1,1)
     
!    [4.2] Top-left point:

        u(nlat-1,1) = u(nlat-1,1) + half * u(nlat,nlon)
        u(nlat  ,2) = u(nlat  ,2) + half * u(nlat,nlon)
        v(nlat-1,1) = v(nlat-1,1) + half * v(nlat,nlon)
        v(nlat  ,2) = v(nlat  ,2) + half * v(nlat,nlon)
   
!    [4.3] Bottom-right point:

        u(2,nlon  ) = u(2,nlon)   + half * u(1,nlon)
        u(1,nlon-1) = u(1,nlon-1) + half * u(1,nlon)
        v(2,nlon  ) = v(2,nlon)   + half * v(1,nlon)
        v(1,nlon-1) = v(1,nlon-1) + half * v(1,nlon)

!    [4.4] Top-right point:

        u(nlat-1,nlon) = u(nlat-1,nlon) + half * u(nlat,nlon)
        u(nlat,nlon-1) = u(nlat,nlon-1) + half * u(nlat,nlon)
        v(nlat-1,nlon) = v(nlat-1,nlon) + half * v(nlat,nlon)
        v(nlat,nlon-1) = v(nlat,nlon-1) + half * v(nlat,nlon)
     
!------------------------------------------------------------------------------
! [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

   
!    [3.4] Northern boundaries:

        
        do j = 2,nlon-1
           coeffy_u = coeffy(nlat,j) * u(nlat,j)
           coeffx_u = coeffx(nlat,j) * u(nlat,j)
           coeffy_v = coeffy(nlat,j) * v(nlat,j)
           coeffx_v = coeffx(nlat,j) * v(nlat,j)
           
           psi(nlat,j+1) = psi(nlat,j+1) + coeffx_v
           psi(nlat,j-1) = psi(nlat,j-1) - coeffx_v
           chi(nlat  ,j) = chi(nlat  ,j) + coeffy_v
           chi(nlat-2,j) = chi(nlat-2,j) - coeffy_v
           
           psi(nlat  ,j) = psi(nlat  ,j) - coeffy_u
           psi(nlat-2,j) = psi(nlat-2,j) + coeffy_u
           chi(nlat,j+1) = chi(nlat,j+1) + coeffx_u
           chi(nlat,j-1) = chi(nlat,j-1) - coeffx_u
        end do
      
!    [3.3] Southern boundaries:

        
        do j = 2,nlon-1
           coeffy_u = coeffy(1,j) * u(1,j)
           coeffx_u = coeffx(1,j) * u(1,j)
           coeffy_v = coeffy(1,j) * v(1,j)
           coeffx_v = coeffx(1,j) * v(1,j)
           
           psi(1,j+1) = psi(1,j+1) + coeffx_v
           psi(1,j-1) = psi(1,j-1) - coeffx_v
           chi(3,j)   = chi(3,  j) + coeffy_v
           chi(1,j)   = chi(1  ,j) - coeffy_v
           
           psi(3,j)   = psi(3,  j) - coeffy_u
           psi(1  ,j) = psi(1  ,j) + coeffy_u
           chi(1,j+1) = chi(1,j+1) + coeffx_u
           chi(1,j-1) = chi(1,j-1) - coeffx_u
           
        end do
      
!    [3.2] Eastern boundaries:

        
        do i = 2,nlat-1
           coeffy_u = coeffy(i,nlon) * u(i,nlon)
           coeffx_u = coeffx(i,nlon) * u(i,nlon)
           coeffy_v = coeffy(i,nlon) * v(i,nlon)
           coeffx_v = coeffx(i,nlon) * v(i,nlon)
           
           psi(i,nlon  ) = psi(i,nlon  ) + coeffx_v
           psi(i,nlon-2) = psi(i,nlon-2) - coeffx_v
           chi(i+1,nlon) = chi(i+1,nlon) + coeffy_v
           chi(i-1,nlon) = chi(i-1,nlon) - coeffy_v
           
           psi(i+1,nlon) = psi(i+1,nlon) - coeffy_u
           psi(i-1,nlon) = psi(i-1,nlon) + coeffy_u
           chi(i,nlon  ) = chi(i,nlon  ) + coeffx_u
           chi(i,nlon-2) = chi(i,nlon-2) - coeffx_u
           
        end do
     
!    [3.1] Western boundaries:
        
        do i = 2,nlat-1
           coeffy_u = coeffy(i,1) * u(i,1)
           coeffx_u = coeffx(i,1) * u(i,1)
           coeffy_v = coeffy(i,1) * v(i,1)
           coeffx_v = coeffx(i,1) * v(i,1)
           
           psi(i,3  ) = psi(i,3  ) + coeffx_v
           psi(i,1  ) = psi(i,1  ) - coeffx_v
           chi(i+1,1) = chi(i+1,1) + coeffy_v
           chi(i-1,1) = chi(i-1,1) - coeffy_v
           
           psi(i+1,1) = psi(i+1,1) - coeffy_u
           psi(i-1,1) = psi(i-1,1) + coeffy_u
           chi(i,3  ) = chi(i,3  ) + coeffx_u
           chi(i,1  ) = chi(i,1  ) - coeffx_u
           
        end do
     
!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = 2,nlon-1
        do i = 2,nlat-1
           coeffy_u = coeffy(i,j) * u(i,j)
           coeffx_u = coeffx(i,j) * u(i,j)
           coeffy_v = coeffy(i,j) * v(i,j)
           coeffx_v = coeffx(i,j) * v(i,j)
           
           psi(i+1,j) = psi(i+1,j) - coeffy_u
           psi(i-1,j) = psi(i-1,j) + coeffy_u
           chi(i,j+1) = chi(i,j+1) + coeffx_u
           chi(i,j-1) = chi(i,j-1) - coeffx_u
           
           psi(i,j+1) = psi(i,j+1) + coeffx_v
           psi(i,j-1) = psi(i,j-1) - coeffx_v
           chi(i+1,j) = chi(i+1,j) + coeffy_v
           chi(i-1,j) = chi(i-1,j) - coeffy_v
           
        end do
     end do
  
end subroutine psichi2uvt_reg

subroutine tdelx_reg( u,  chi) 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    psichi2uvt_reg
!     prgmmr:    barker, d   org:  np22              date:  2000-02-03
!
! abstract:  Calculate adjoint of psi and chi from adjoint of 
!            wind components u and v.
!
! program history log:
!   2000/02/03   barker, dale - creation of F90 version
!   2001/10/30   barker       - parallel version
!   2003/09/05   parrish      - adapted to unified NCEP 3dvar
!   2003/10/17   wu, wanshu   - first index Y second X
!   2004-06-22   treadon      - update documentation
!   2009-04-19   derber       - modify to fit gsi
!
!   input argument list:
!     u - adjoint of zonal wind component
!
!
!   output argument list:
!     chi - adjoint of x derivative
!
! remarks:
!    The method used is 
!
!    The assumptions made in this routine are:
!       - unstaggered grid,
!       - lateral boundary conditions - dchi/dn = 0 (FCT)
!       - dy=rearth*dph , dx=cos(ph)*rearth*dlm (dx,dy is rotated grid)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,half
  use gridmod, only:  coeffx,coeffy,nlat,nlon
  implicit none
  
  
  real(r_kind),intent(inout):: u(nlat,nlon)   ! u wind comp (m/s)
  real(r_kind),intent(inout):: chi(nlat,nlon) ! Velocity potential
  
  integer(i_kind)           :: i, j                       ! Loop counters.
  real(r_kind)      :: coeffx_u                      ! Multiplicative coefficient.
  
  
   
!------------------------------------------------------------------------------
!     [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

        u(2,1) = u(2,1) + half * u(1,1)
        u(1,2) = u(1,2) + half * u(1,1)
        u(1,1)=zero
     
!    [4.2] Top-left point:

        u(nlat-1,1) = u(nlat-1,1) + half * u(nlat,1)
        u(nlat  ,2) = u(nlat  ,2) + half * u(nlat,1)
        u(nlat,1)=zero
   
!    [4.3] Bottom-right point:

        u(2,nlon  ) = u(2,nlon  ) + half * u(1,nlon)
        u(1,nlon-1) = u(1,nlon-1) + half * u(1,nlon)
        u(1,nlon)=zero

!    [4.4] Top-right point:

        u(nlat-1,nlon) = u(nlat-1,nlon) + half * u(nlat,nlon)
        u(nlat,nlon-1) = u(nlat,nlon-1) + half * u(nlat,nlon)
        u(nlat,nlon)=zero

!------------------------------------------------------------------------------
! [3.0] Compute u at domain boundaries:
!------------------------------------------------------------------------------
  
      
!    [3.2] Eastern boundaries:

        j = nlon
        
        do i = 1,nlat
           coeffx_u = coeffx(i,j) * u(i,j)
           
           chi(i,j  ) = chi(i,j  ) + coeffx_u
           chi(i,j-2) = chi(i,j-2) - coeffx_u
           
        end do
     
!    [3.1] Western boundaries:
        j = 1
        
        do i = 1,nlat
           coeffx_u = coeffx(i,j) * u(i,j)
           
           chi(i,j+2) = chi(i,j+2) + coeffx_u
           chi(i,j  ) = chi(i,j  ) - coeffx_u
           
        end do
     
!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = 2,nlon-1
        do i = 1,nlat
           coeffx_u = coeffx(i,j) * u(i,j)
           
           chi(i,j+1) = chi(i,j+1) + coeffx_u
           chi(i,j-1) = chi(i,j-1) - coeffx_u
           
           
        end do
     end do
  
end subroutine tdelx_reg


subroutine tdely_reg( v,  chi)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    psichi2uvt_reg
!     prgmmr:    barker, d   org:  np22              date:  2000-02-03
!
! abstract:  Calculate adjoint of psi and chi from adjoint of 
!            wind components u and v.
!
! program history log:
!   2000/02/03   barker, dale - creation of F90 version
!   2001/10/30   barker       - parallel version
!   2003/09/05   parrish      - adapted to unified NCEP 3dvar
!   2003/10/17   wu, wanshu   - first index Y second X
!   2004-06-22   treadon      - update documentation
!   2005-09-28   parrish      - fix bug in computing derivatives
!                               along western boundaries
!   2009-04-19   derber       - modify to fit gsi
!
!   input argument list:
!     v - adjoint of meridional wind component
!
!   output argument list:
!     chi - adjoint of y derivative
!
! remarks:
!    The method used is 
!       v = (  dchi/dy )
!
!    The assumptions made in this routine are:
!       - unstaggered grid,
!       - lateral boundary conditions - dpsi/dn, dchi/dn = 0 (FCT)
!       - dy=rearth*dph , dx=cos(ph)*rearth*dlm (dx,dy is rotated grid)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,half
  use gridmod, only:  coeffx,coeffy,nlat,nlon
  implicit none
  
  real(r_kind),intent(inout):: v(nlat,nlon)   ! v wind comp (m/s)
  real(r_kind),intent(inout):: chi(nlat,nlon) ! Velocity potential
  
  integer(i_kind)           :: i, j                       ! Loop counters.
  real(r_kind)      :: coeffy_v                      ! Multiplicative coefficient.
  
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------


! Computation to check for edge of domain:
  
!------------------------------------------------------------------------------
!     [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

        v(2,1) = v(2,1) + half * v(1,1)
        v(1,2) = v(1,2) + half * v(1,1)
        v(1,1)=zero
     
!    [4.2] Top-left point:

        v(nlat-1,1) = v(nlat-1,1) + half * v(nlat,1)
        v(nlat  ,2) = v(nlat  ,2) + half * v(nlat,1)
        v(nlat,1)=zero
   
!    [4.3] Bottom-right point:

        v(2,nlon  ) = v(2,nlon  ) + half * v(1,nlon)
        v(1,nlon-1) = v(1,nlon-1) + half * v(1,nlon)
        v(1,nlon)=zero

!    [4.4] Top-right point:

        v(nlat-1,nlon) = v(nlat-1,nlon) + half * v(nlat,nlon)
        v(nlat,nlon-1) = v(nlat,nlon-1) + half * v(nlat,nlon)
        v(nlat,nlon)=zero
     
!------------------------------------------------------------------------------
! [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

        i=nlat
        do j = 1,nlon
           coeffy_v = coeffy(i,j) * v(i,j)
           
           chi(i  ,j) = chi(i  ,j) + coeffy_v
           chi(i-2,j) = chi(i-2,j) - coeffy_v
           
        end do
      
!    [3.3] Southern boundaries:

        i = 1
        
        do j = 1,nlon
           coeffy_v = coeffy(i,j) * v(i,j)
           
           chi(i+2,j) = chi(i+2,j) + coeffy_v
           chi(i  ,j) = chi(i  ,j) - coeffy_v
           
        end do
      
     
!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = 1,nlon
        do i = 2,nlat-1
           coeffy_v = coeffy(i,j) * v(i,j)
           
           chi(i+1,j) = chi(i+1,j) + coeffy_v
           chi(i-1,j) = chi(i-1,j) - coeffy_v
           
        end do
     end do
  
end subroutine tdely_reg
