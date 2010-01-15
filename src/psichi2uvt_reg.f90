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
  use constants, only: ione,zero,half
  use gridmod, only:  coeffx,coeffy,nlat,nlon
  implicit none
  
  
  real(r_kind),intent(inout) :: u(nlat,nlon)   ! u wind comp (m/s)
  real(r_kind),intent(inout) :: v(nlat,nlon)   ! v wind comp (m/s)
  real(r_kind),intent(inout) :: psi(nlat,nlon) ! Stream function
  real(r_kind),intent(inout) :: chi(nlat,nlon) ! Velocity potential
  
  integer(i_kind)           :: i, j           ! Loop counters.
  real(r_kind)              :: coeffx_u       ! Multiplicative coefficient.
  real(r_kind)              :: coeffy_u       ! Multiplicative coefficient.
  real(r_kind)              :: coeffx_v       ! Multiplicative coefficient.
  real(r_kind)              :: coeffy_v       ! Multiplicative coefficient.
  
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

  u(nlat-ione,1) = u(nlat-ione,1) + half * u(nlat,1)
  u(nlat     ,2) = u(nlat     ,2) + half * u(nlat,1)
  v(nlat-ione,1) = v(nlat-ione,1) + half * v(nlat,1)
  v(nlat     ,2) = v(nlat     ,2) + half * v(nlat,1)
   
!    [4.3] Bottom-right point:

  u(2,nlon     ) = u(2,nlon     ) + half * u(1,nlon)
  u(1,nlon-ione) = u(1,nlon-ione) + half * u(1,nlon)
  v(2,nlon     ) = v(2,nlon     ) + half * v(1,nlon)
  v(1,nlon-ione) = v(1,nlon-ione) + half * v(1,nlon)

!    [4.4] Top-right point:

  u(nlat-ione,nlon     ) = u(nlat-ione,nlon     ) + half * u(nlat,nlon)
  u(nlat     ,nlon-ione) = u(nlat     ,nlon-ione) + half * u(nlat,nlon)
  v(nlat-ione,nlon     ) = v(nlat-ione,nlon     ) + half * v(nlat,nlon)
  v(nlat     ,nlon-ione) = v(nlat     ,nlon-ione) + half * v(nlat,nlon)
     
!------------------------------------------------------------------------------
! [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

   
!    [3.4] Northern boundaries:

        
  do j = 2,nlon-ione
     coeffy_u = coeffy(nlat,j) * u(nlat,j)
     coeffx_u = coeffx(nlat,j) * u(nlat,j)
     coeffy_v = coeffy(nlat,j) * v(nlat,j)
     coeffx_v = coeffx(nlat,j) * v(nlat,j)
 
     psi(nlat         ,j+ione) = psi(nlat         ,j+ione) + coeffx_v
     psi(nlat         ,j-ione) = psi(nlat         ,j-ione) - coeffx_v
     chi(nlat         ,j     ) = chi(nlat         ,j     ) + coeffy_v
     chi(nlat-2_i_kind,j     ) = chi(nlat-2_i_kind,j     ) - coeffy_v
           
     psi(nlat         ,j     ) = psi(nlat         ,j     ) - coeffy_u
     psi(nlat-2_i_kind,j     ) = psi(nlat-2_i_kind,j     ) + coeffy_u
     chi(nlat         ,j+ione) = chi(nlat         ,j+ione) + coeffx_u
     chi(nlat         ,j-ione) = chi(nlat         ,j-ione) - coeffx_u
  end do
      
!    [3.3] Southern boundaries:

 
  do j = 2,nlon-ione
     coeffy_u = coeffy(1,j) * u(1,j)
     coeffx_u = coeffx(1,j) * u(1,j)
     coeffy_v = coeffy(1,j) * v(1,j)
     coeffx_v = coeffx(1,j) * v(1,j)
 
     psi(1,j+ione) = psi(1,j+ione) + coeffx_v
     psi(1,j-ione) = psi(1,j-ione) - coeffx_v
     chi(3,j     ) = chi(3,j     ) + coeffy_v
     chi(1,j     ) = chi(1,j     ) - coeffy_v
 
     psi(3,j     ) = psi(3,j     ) - coeffy_u
     psi(1,j     ) = psi(1,j     ) + coeffy_u
     chi(1,j+ione) = chi(1,j+ione) + coeffx_u
     chi(1,j-ione) = chi(1,j-ione) - coeffx_u
 
  end do
      
!    [3.2] Eastern boundaries:

 
  do i = 2,nlat-ione
     coeffy_u = coeffy(i,nlon) * u(i,nlon)
     coeffx_u = coeffx(i,nlon) * u(i,nlon)
     coeffy_v = coeffy(i,nlon) * v(i,nlon)
     coeffx_v = coeffx(i,nlon) * v(i,nlon)
 
     psi(i     ,nlon         ) = psi(i     ,nlon         ) + coeffx_v
     psi(i     ,nlon-2_i_kind) = psi(i     ,nlon-2_i_kind) - coeffx_v
     chi(i+ione,nlon         ) = chi(i+ione,nlon         ) + coeffy_v
     chi(i-ione,nlon         ) = chi(i-ione,nlon         ) - coeffy_v
 
     psi(i+ione,nlon         ) = psi(i+ione,nlon         ) - coeffy_u
     psi(i-ione,nlon         ) = psi(i-ione,nlon         ) + coeffy_u
     chi(i     ,nlon         ) = chi(i     ,nlon         ) + coeffx_u
     chi(i     ,nlon-2_i_kind) = chi(i     ,nlon-2_i_kind) - coeffx_u

  end do
 
!    [3.1] Western boundaries:
 
  do i = 2,nlat-ione
     coeffy_u = coeffy(i,1) * u(i,1)
     coeffx_u = coeffx(i,1) * u(i,1)
     coeffy_v = coeffy(i,1) * v(i,1)
     coeffx_v = coeffx(i,1) * v(i,1)

     psi(i     ,3) = psi(i     ,3) + coeffx_v
     psi(i     ,1) = psi(i     ,1) - coeffx_v
     chi(i+ione,1) = chi(i+ione,1) + coeffy_v
     chi(i-ione,1) = chi(i-ione,1) - coeffy_v
 
     psi(i+ione,1) = psi(i+ione,1) - coeffy_u
     psi(i-ione,1) = psi(i-ione,1) + coeffy_u
     chi(i     ,3) = chi(i     ,3) + coeffx_u
     chi(i     ,1) = chi(i     ,1) - coeffx_u
 
  end do
     
!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

  do j = 2,nlon-ione
     do i = 2,nlat-ione
        coeffy_u = coeffy(i,j) * u(i,j)
        coeffx_u = coeffx(i,j) * u(i,j)
        coeffy_v = coeffy(i,j) * v(i,j)
        coeffx_v = coeffx(i,j) * v(i,j)
 
        psi(i+ione,j     ) = psi(i+ione,j     ) - coeffy_u
        psi(i-ione,j     ) = psi(i-ione,j     ) + coeffy_u
        chi(i     ,j+ione) = chi(i     ,j+ione) + coeffx_u
        chi(i     ,j-ione) = chi(i     ,j-ione) - coeffx_u
 
        psi(i     ,j+ione) = psi(i     ,j+ione) + coeffx_v
        psi(i     ,j-ione) = psi(i     ,j-ione) - coeffx_v
        chi(i+ione,j     ) = chi(i+ione,j     ) + coeffy_v
        chi(i-ione,j     ) = chi(i-ione,j     ) - coeffy_v
 
     end do
  end do
  
end subroutine psichi2uvt_reg

subroutine tdelx_reg( u,  chi,vector) 
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
!     vector
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
  use constants, only: ione,zero,half
  use gridmod, only: coeffx,nlat,nlon,region_dy,region_dyi
  implicit none
  
  
  logical     ,intent(in   ) :: vector
  real(r_kind),intent(inout) :: u(nlat,nlon)   ! u wind comp (m/s)
  real(r_kind),intent(inout) :: chi(nlat,nlon) ! Velocity potential
  
  integer(i_kind)            :: i, j           ! Loop counters.
  real(r_kind)               :: coeffx_u       ! Multiplicative coefficient.
  real(r_kind)               :: ch2(nlat,nlon)
  real(r_kind)               :: u2(nlat,nlon)
  
  if(vector) then
     do j=1,nlon
        do i=1,nlat
           u2(i,j)=u(i,j)*region_dyi(i,j)
        end do
     end do
  else
     do j=1,nlon
        do i=1,nlat
           u2(i,j)=u(i,j)
        end do
     end do
  end if
  
   
!------------------------------------------------------------------------------
!     [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

  u2(2,1) = u2(2,1) + half * u2(1,1)
  u2(1,2) = u2(1,2) + half * u2(1,1)
  u2(1,1) = zero

!    [4.2] Top-left point:

  u2(nlat-ione,1) = u2(nlat-ione,1) + half * u2(nlat,1)
  u2(nlat     ,2) = u2(nlat     ,2) + half * u2(nlat,1)
  u2(nlat     ,1) = zero
 
!    [4.3] Bottom-right point:

  u2(2,nlon     ) = u2(2,nlon     ) + half * u2(1,nlon)
  u2(1,nlon-ione) = u2(1,nlon-ione) + half * u2(1,nlon)
  u2(1,nlon)      = zero

!    [4.4] Top-right point:

  u2(nlat-ione,nlon     ) = u2(nlat-ione,nlon     ) + half * u2(nlat,nlon)
  u2(nlat     ,nlon-ione) = u2(nlat     ,nlon-ione) + half * u2(nlat,nlon)
  u2(nlat,nlon)=zero

  ch2=zero

!------------------------------------------------------------------------------
! [3.0] Compute u at domain boundaries:
!------------------------------------------------------------------------------
 
 
!    [3.2] Eastern boundaries:

  j = nlon
 
  do i = 1,nlat
     coeffx_u = coeffx(i,j) * u2(i,j)
 
     ch2(i,j         ) = ch2(i,j         ) + coeffx_u
     ch2(i,j-2_i_kind) = ch2(i,j-2_i_kind) - coeffx_u
 
  end do

!    [3.1] Western boundaries:
  j = ione
 
  do i = 1,nlat
     coeffx_u = coeffx(i,j) * u2(i,j)
 
     ch2(i,j+2_i_kind) = ch2(i,j+2_i_kind) + coeffx_u
     ch2(i,j         ) = ch2(i,j         ) - coeffx_u
 
  end do
 
!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

  do j = 2,nlon-ione
     do i = 1,nlat
        coeffx_u = coeffx(i,j) * u2(i,j)

        ch2(i,j+ione) = ch2(i,j+ione) + coeffx_u
        ch2(i,j-ione) = ch2(i,j-ione) - coeffx_u
 
 
     end do
  end do

  if(vector) then
     do j=1,nlon
        do i=1,nlat
           chi(i,j)=chi(i,j)+ch2(i,j)*region_dy(i,j)
        end do
     end do
  else
     do j=1,nlon
        do i=1,nlat
           chi(i,j)=chi(i,j)+ch2(i,j)
        end do
     end do
  end if
  
end subroutine tdelx_reg


subroutine tdely_reg( v,  chi,vector)
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
!     vector
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
  use constants, only: ione,zero,half
  use gridmod, only: coeffy,nlat,nlon,region_dx,region_dxi
  implicit none
  
  logical     ,intent(in   ) :: vector
  real(r_kind),intent(inout) :: v(nlat,nlon)   ! v wind comp (m/s)
  real(r_kind),intent(inout) :: chi(nlat,nlon) ! Velocity potential
  
  integer(i_kind)           :: i, j                       ! Loop counters.
  real(r_kind)              :: coeffy_v                   ! Multiplicative coefficient.
  real(r_kind)              :: ch2(nlat,nlon)
  real(r_kind)              :: v2(nlat,nlon)
  
  if(vector) then
     do j=1,nlon
        do i=1,nlat
           v2(i,j)=v(i,j)*region_dxi(i,j)
        end do
     end do
  else
     do j=1,nlon
        do i=1,nlat
           v2(i,j)=v(i,j)
        end do
     end do
  end if


!------------------------------------------------------------------------------
!     [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

  v2(2,1) = v2(2,1) + half * v2(1,1)
  v2(1,2) = v2(1,2) + half * v2(1,1)
  v2(1,1) = zero
 
!    [4.2] Top-left point:

  v2(nlat-ione,1) = v2(nlat-ione,1) + half * v2(nlat,1)
  v2(nlat     ,2) = v2(nlat     ,2) + half * v2(nlat,1)
  v2(nlat,1)      = zero
 
!    [4.3] Bottom-right point:

  v2(2,nlon     ) = v2(2,nlon     ) + half * v2(1,nlon)
  v2(1,nlon-ione) = v2(1,nlon-ione) + half * v2(1,nlon)
  v2(1,nlon     ) = zero

!    [4.4] Top-right point:

  v2(nlat-ione,nlon     ) = v2(nlat-ione,nlon     ) + half * v2(nlat,nlon)
  v2(nlat     ,nlon-ione) = v2(nlat     ,nlon-ione) + half * v2(nlat,nlon)
  v2(nlat     ,nlon     ) = zero
 
!------------------------------------------------------------------------------
! [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

  ch2=zero

  i=nlat
  do j = 1,nlon
     coeffy_v = coeffy(i,j) * v2(i,j)
 
     ch2(i         ,j) = ch2(i         ,j) + coeffy_v
     ch2(i-2_i_kind,j) = ch2(i-2_i_kind,j) - coeffy_v
 
  end do
      
!    [3.3] Southern boundaries:

  i = ione

  do j = 1,nlon
     coeffy_v = coeffy(i,j) * v2(i,j)
 
     ch2(i+2_i_kind,j) = ch2(i+2_i_kind,j) + coeffy_v
     ch2(i         ,j) = ch2(i         ,j) - coeffy_v
 
  end do
 
 
!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

  do j = 1,nlon
     do i = 2,nlat-ione
        coeffy_v = coeffy(i,j) * v2(i,j)

        ch2(i+ione,j) = ch2(i+ione,j) + coeffy_v
        ch2(i-ione,j) = ch2(i-ione,j) - coeffy_v
 
     end do
  end do

  if(vector) then
     do j=1,nlon
        do i=1,nlat
           chi(i,j)=chi(i,j)+ch2(i,j)*region_dx(i,j)
        end do
     end do
  else
     do j=1,nlon
        do i=1,nlat
           chi(i,j)=chi(i,j)+ch2(i,j)
        end do
     end do
  end if
  
end subroutine tdely_reg
