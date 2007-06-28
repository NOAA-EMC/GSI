subroutine psichi2uvt_reg( u, v,  psi, chi, &
     ids,ide, jds,jde, kds,kde,  &
     ims,ime, jms,jme, kms,kme,  &
     its,ite, jts,jte, kts,kte )
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
!
!   input argument list:
!     u - adjoint of zonal wind component
!     v - adjoint of meridional wind component
!
!     ids - starting index of first  dimension (latitude)  for full domain grid
!     ide - ending   index of first  dimension (latitude)  for full domain grid
!     jds - starting index of second dimension (longitude) for full domain grid
!     jde - ending   index of second dimension (longitude) for full domain grid
!     kds - starting index of third  dimension (vertical)  for full domain grid
!     kde - ending   index of third  dimension (vertical)  for full domain grid
!
!     ims - starting index of first  dimension for memory allocation
!     ime - ending   index of first  dimension for memory allocation
!     jms - starting index of second dimension for memory allocation
!     jme - ending   index of second dimension for memory allocation
!     kms - starting index of third  dimension for memory allocation
!     kme - ending   index of third  dimension for memory allocation
!
!     its - starting index of first  dimension for sub-domain (tile) grid
!     ite - ending   index of first  dimension for sub-domain (tile) grid
!     jts - starting index of second dimension for sub-domain (tile) grid
!     jte - ending   index of second dimension for sub-domain (tile) grid
!     kts - starting index of third  dimension for sub-domain (tile) grid
!     kte - ending   index of third  dimension for sub-domain (tile) grid
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
  use gridmod, only:  region_dx,region_dy
  implicit none
  
  integer(i_kind), intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
  integer(i_kind), intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
  integer(i_kind), intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.
  
  real(r_kind),intent(inout):: u(ims:ime,jms:jme,kms:kme)   ! u wind comp (m/s)
  real(r_kind),intent(inout):: v(ims:ime,jms:jme,kms:kme)   ! v wind comp (m/s)
  real(r_kind),intent(inout):: psi(ims:ime,jms:jme,kms:kme) ! Stream function
  real(r_kind),intent(inout):: chi(ims:ime,jms:jme,kms:kme) ! Velocity potential
  
  integer(i_kind)           :: i, j, k                       ! Loop counters.
  integer(i_kind)           :: is, ie                        ! 1st dim. end points.
  integer(i_kind)           :: js, je                        ! 2nd dim. end points.
  integer(i_kind)           :: ks, ke                        ! 3rd dim. end points.
  real(r_kind)      :: coeffx(its:ite,jts:jte)       ! Multiplicative coefficient.
  real(r_kind)      :: coeffy(its:ite,jts:jte)       ! Multiplicative coefficient.
  real(r_kind)      :: coeffx_u                      ! Multiplicative coefficient.
  real(r_kind)      :: coeffy_u                      ! Multiplicative coefficient.
  real(r_kind)      :: coeffx_v                      ! Multiplicative coefficient.
  real(r_kind)      :: coeffy_v                      ! Multiplicative coefficient.
  
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

  psi=zero
  chi=zero

! Computation to check for edge of domain:
  is = its; ie = ite; js = jts; je = jte
  if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
  if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1
  
  coeffx(its:ite,jts:jte) = half / region_dx(its:ite,jts:jte)
  coeffy(its:ite,jts:jte) = half / region_dy(its:ite,jts:jte)
  
  do k = kts, kte
   
!------------------------------------------------------------------------------
!     [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

     if ( its == ids .AND. jts == jds ) then
        u(its+1,jts,k) = u(its+1,jts,k) + half * u(its,jts,k)
        u(its,jts+1,k) = u(its,jts+1,k) + half * u(its,jts,k)
        v(its+1,jts,k) = v(its+1,jts,k) + half * v(its,jts,k)
        v(its,jts+1,k) = v(its,jts+1,k) + half * v(its,jts,k)
     end if
     
!    [4.2] Top-left point:

     if ( ite == ide .AND. jts == jds ) then
        u(ite-1,jts,k) = u(ite-1,jts,k) + half * u(ite,jts,k)
        u(ite,jts+1,k) = u(ite,jts+1,k) + half * u(ite,jts,k)
        v(ite-1,jts,k) = v(ite-1,jts,k) + half * v(ite,jts,k)
        v(ite,jts+1,k) = v(ite,jts+1,k) + half * v(ite,jts,k)
     end if
   
!    [4.3] Bottom-right point:

     if ( its == ids .AND. jte == jde ) then
        u(its+1,jte,k) = u(its+1,jte,k) + half * u(its,jte,k)
        u(its,jte-1,k) = u(its,jte-1,k) + half * u(its,jte,k)
        v(its+1,jte,k) = v(its+1,jte,k) + half * v(its,jte,k)
        v(its,jte-1,k) = v(its,jte-1,k) + half * v(its,jte,k)
     end if

!    [4.4] Top-right point:

     if ( ite == ide .AND. jte == jde ) then
        u(ite-1,jte,k) = u(ite-1,jte,k) + half * u(ite,jte,k)
        u(ite,jte-1,k) = u(ite,jte-1,k) + half * u(ite,jte,k)
        v(ite-1,jte,k) = v(ite-1,jte,k) + half * v(ite,jte,k)
        v(ite,jte-1,k) = v(ite,jte-1,k) + half * v(ite,jte,k)
     end if
     
  enddo

!------------------------------------------------------------------------------
! [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

  is = its-1; ie = ite+1; js = jts-1; je = jte+1
  if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
  if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1
  
  do k = kts, kte
   
!    [3.4] Northern boundaries:

     if ( ite == ide ) then
        i = ite
        
        do j = je, js, -1
           coeffy_u = coeffy(i,j) * u(i,j,k)
           coeffx_u = coeffx(i,j) * u(i,j,k)
           coeffy_v = coeffy(i,j) * v(i,j,k)
           coeffx_v = coeffx(i,j) * v(i,j,k)
           
           psi(i,j+1,k) = psi(i,j+1,k) + coeffx_v
           psi(i,j-1,k) = psi(i,j-1,k) - coeffx_v
           chi(i  ,j,k) = chi(i  ,j,k) + coeffy_v
           chi(i-2,j,k) = chi(i-2,j,k) - coeffy_v
           
           psi(i  ,j,k) = psi(i  ,j,k) - coeffy_u
           psi(i-2,j,k) = psi(i-2,j,k) + coeffy_u
           chi(i,j+1,k) = chi(i,j+1,k) + coeffx_u
           chi(i,j-1,k) = chi(i,j-1,k) - coeffx_u
        end do
     end if
      
!    [3.3] Southern boundaries:

     if ( its == ids ) then
        i = its
        
        do j = je, js, -1
           coeffy_u = coeffy(i,j) * u(i,j,k)
           coeffx_u = coeffx(i,j) * u(i,j,k)
           coeffy_v = coeffy(i,j) * v(i,j,k)
           coeffx_v = coeffx(i,j) * v(i,j,k)
           
           psi(i,j+1,k) = psi(i,j+1,k) + coeffx_v
           psi(i,j-1,k) = psi(i,j-1,k) - coeffx_v
           chi(i+2,j,k) = chi(i+2,j,k) + coeffy_v
           chi(i  ,j,k) = chi(i  ,j,k) - coeffy_v
           
           psi(i+2,j,k) = psi(i+2,j,k) - coeffy_u
           psi(i  ,j,k) = psi(i  ,j,k) + coeffy_u
           chi(i,j+1,k) = chi(i,j+1,k) + coeffx_u
           chi(i,j-1,k) = chi(i,j-1,k) - coeffx_u
           
        end do
     end if
      
!    [3.2] Eastern boundaries:

     if ( jte == jde ) then
        j = jte
        
        do i = ie, is, -1
           coeffy_u = coeffy(i,j) * u(i,j,k)
           coeffx_u = coeffx(i,j) * u(i,j,k)
           coeffy_v = coeffy(i,j) * v(i,j,k)
           coeffx_v = coeffx(i,j) * v(i,j,k)
           
           psi(i,j  ,k) = psi(i,j  ,k) + coeffx_v
           psi(i,j-2,k) = psi(i,j-2,k) - coeffx_v
           chi(i+1,j,k) = chi(i+1,j,k) + coeffy_v
           chi(i-1,j,k) = chi(i-1,j,k) - coeffy_v
           
           psi(i+1,j,k) = psi(i+1,j,k) - coeffy_u
           psi(i-1,j,k) = psi(i-1,j,k) + coeffy_u
           chi(i,j  ,k) = chi(i,j  ,k) + coeffx_u
           chi(i,j-2,k) = chi(i,j-2,k) - coeffx_u
           
        end do
     end if
     
!    [3.1] Western boundaries:
     if ( jts == jds ) then
        j = jts
        
        do i = ie, is, -1
           coeffy_u = coeffy(i,j) * u(i,j,k)
           coeffx_u = coeffx(i,j) * u(i,j,k)
           coeffy_v = coeffy(i,j) * v(i,j,k)
           coeffx_v = coeffx(i,j) * v(i,j,k)
           
           psi(i,j+2,k) = psi(i,j+2,k) + coeffx_v
           psi(i,j  ,k) = psi(i,j  ,k) - coeffx_v
           chi(i+1,j,k) = chi(i+1,j,k) + coeffy_v
           chi(i-1,j,k) = chi(i-1,j,k) - coeffy_v
           
           psi(i+1,j,k) = psi(i+1,j,k) - coeffy_u
           psi(i-1,j,k) = psi(i-1,j,k) + coeffy_u
           chi(i,j+2,k) = chi(i,j+2,k) + coeffx_u
           chi(i,j  ,k) = chi(i,j  ,k) - coeffx_u
           
        end do
     end if
     
!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = je, js, -1
        do i = ie, is, -1
           coeffy_u = coeffy(i,j) * u(i,j,k)
           coeffx_u = coeffx(i,j) * u(i,j,k)
           coeffy_v = coeffy(i,j) * v(i,j,k)
           coeffx_v = coeffx(i,j) * v(i,j,k)
           
           psi(i+1,j,k) = psi(i+1,j,k) - coeffy_u
           psi(i-1,j,k) = psi(i-1,j,k) + coeffy_u
           chi(i,j+1,k) = chi(i,j+1,k) + coeffx_u
           chi(i,j-1,k) = chi(i,j-1,k) - coeffx_u
           
           psi(i,j+1,k) = psi(i,j+1,k) + coeffx_v
           psi(i,j-1,k) = psi(i,j-1,k) - coeffx_v
           chi(i+1,j,k) = chi(i+1,j,k) + coeffy_v
           chi(i-1,j,k) = chi(i-1,j,k) - coeffy_v
           
        end do
     end do
  end do
  
end subroutine psichi2uvt_reg

subroutine tdelx_reg( u,  chi, &
     ids,ide, jds,jde, kds,kde,  &
     ims,ime, jms,jme, kms,kme,  &
     its,ite, jts,jte, kts,kte )
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
!
!   input argument list:
!     u - adjoint of zonal wind component
!     v - adjoint of meridional wind component
!
!     ids - starting index of first  dimension (latitude)  for full domain grid
!     ide - ending   index of first  dimension (latitude)  for full domain grid
!     jds - starting index of second dimension (longitude) for full domain grid
!     jde - ending   index of second dimension (longitude) for full domain grid
!     kds - starting index of third  dimension (vertical)  for full domain grid
!     kde - ending   index of third  dimension (vertical)  for full domain grid
!
!     ims - starting index of first  dimension for memory allocation
!     ime - ending   index of first  dimension for memory allocation
!     jms - starting index of second dimension for memory allocation
!     jme - ending   index of second dimension for memory allocation
!     kms - starting index of third  dimension for memory allocation
!     kme - ending   index of third  dimension for memory allocation
!
!     its - starting index of first  dimension for sub-domain (tile) grid
!     ite - ending   index of first  dimension for sub-domain (tile) grid
!     jts - starting index of second dimension for sub-domain (tile) grid
!     jte - ending   index of second dimension for sub-domain (tile) grid
!     kts - starting index of third  dimension for sub-domain (tile) grid
!     kte - ending   index of third  dimension for sub-domain (tile) grid
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
  use gridmod, only:  region_dx,region_dy
  implicit none
  
  integer(i_kind), intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
  integer(i_kind), intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
  integer(i_kind), intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.
  
  real(r_kind),intent(inout):: u(ims:ime,jms:jme,kms:kme)   ! u wind comp (m/s)
  real(r_kind),intent(inout):: chi(ims:ime,jms:jme,kms:kme) ! Velocity potential
  
  integer(i_kind)           :: i, j, k                       ! Loop counters.
  integer(i_kind)           :: is, ie                        ! 1st dim. end points.
  integer(i_kind)           :: js, je                        ! 2nd dim. end points.
  integer(i_kind)           :: ks, ke                        ! 3rd dim. end points.
  real(r_kind)      :: coeffx(its:ite,jts:jte)       ! Multiplicative coefficient.
  real(r_kind)      :: coeffy(its:ite,jts:jte)       ! Multiplicative coefficient.
  real(r_kind)      :: coeffx_u                      ! Multiplicative coefficient.
  real(r_kind)      :: coeffy_u                      ! Multiplicative coefficient.
  real(r_kind)      :: coeffx_v                      ! Multiplicative coefficient.
  real(r_kind)      :: coeffy_v                      ! Multiplicative coefficient.
  
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

  chi=zero

! Computation to check for edge of domain:
  is = its; ie = ite; js = jts; je = jte
  if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
  if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1
  
  coeffx(its:ite,jts:jte) = half / region_dx(its:ite,jts:jte)
  coeffy(its:ite,jts:jte) = half / region_dy(its:ite,jts:jte)
  
  do k = kts, kte
   
!------------------------------------------------------------------------------
!     [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

     if ( its == ids .AND. jts == jds ) then
        u(its+1,jts,k) = u(its+1,jts,k) + half * u(its,jts,k)
        u(its,jts+1,k) = u(its,jts+1,k) + half * u(its,jts,k)
     end if
     
!    [4.2] Top-left point:

     if ( ite == ide .AND. jts == jds ) then
        u(ite-1,jts,k) = u(ite-1,jts,k) + half * u(ite,jts,k)
        u(ite,jts+1,k) = u(ite,jts+1,k) + half * u(ite,jts,k)
     end if
   
!    [4.3] Bottom-right point:

     if ( its == ids .AND. jte == jde ) then
        u(its+1,jte,k) = u(its+1,jte,k) + half * u(its,jte,k)
        u(its,jte-1,k) = u(its,jte-1,k) + half * u(its,jte,k)
     end if

!    [4.4] Top-right point:

     if ( ite == ide .AND. jte == jde ) then
        u(ite-1,jte,k) = u(ite-1,jte,k) + half * u(ite,jte,k)
        u(ite,jte-1,k) = u(ite,jte-1,k) + half * u(ite,jte,k)
     end if
     
  enddo

!------------------------------------------------------------------------------
! [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

  is = its-1; ie = ite+1; js = jts-1; je = jte+1
  if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
  if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1
  
  do k = kts, kte
   
!    [3.4] Northern boundaries:

     if ( ite == ide ) then
        i = ite
        
        do j = je, js, -1
           coeffx_u = coeffx(i,j) * u(i,j,k)
           
           chi(i,j+1,k) = chi(i,j+1,k) + coeffx_u
           chi(i,j-1,k) = chi(i,j-1,k) - coeffx_u
        end do
     end if
      
!    [3.3] Southern boundaries:

     if ( its == ids ) then
        i = its
        
        do j = je, js, -1
           coeffx_u = coeffx(i,j) * u(i,j,k)
           
           chi(i,j+1,k) = chi(i,j+1,k) + coeffx_u
           chi(i,j-1,k) = chi(i,j-1,k) - coeffx_u
           
        end do
     end if
      
!    [3.2] Eastern boundaries:

     if ( jte == jde ) then
        j = jte
        
        do i = ie, is, -1
           coeffx_u = coeffx(i,j) * u(i,j,k)
           
           chi(i,j  ,k) = chi(i,j  ,k) + coeffx_u
           chi(i,j-2,k) = chi(i,j-2,k) - coeffx_u
           
        end do
     end if
     
!    [3.1] Western boundaries:
     if ( jts == jds ) then
        j = jts
        
        do i = ie, is, -1
           coeffx_u = coeffx(i,j) * u(i,j,k)
           
           chi(i,j+2,k) = chi(i,j+2,k) + coeffx_u
           chi(i,j  ,k) = chi(i,j  ,k) - coeffx_u
           
        end do
     end if
     
!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = je, js, -1
        do i = ie, is, -1
           coeffx_u = coeffx(i,j) * u(i,j,k)
           
           chi(i,j+1,k) = chi(i,j+1,k) + coeffx_u
           chi(i,j-1,k) = chi(i,j-1,k) - coeffx_u
           
           
        end do
     end do
  end do
  
end subroutine tdelx_reg

subroutine tdely_reg( v,  chi, &
     ids,ide, jds,jde, kds,kde,  &
     ims,ime, jms,jme, kms,kme,  &
     its,ite, jts,jte, kts,kte )
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
!
!   input argument list:
!     u - adjoint of zonal wind component
!     v - adjoint of meridional wind component
!
!     ids - starting index of first  dimension (latitude)  for full domain grid
!     ide - ending   index of first  dimension (latitude)  for full domain grid
!     jds - starting index of second dimension (longitude) for full domain grid
!     jde - ending   index of second dimension (longitude) for full domain grid
!     kds - starting index of third  dimension (vertical)  for full domain grid
!     kde - ending   index of third  dimension (vertical)  for full domain grid
!
!     ims - starting index of first  dimension for memory allocation
!     ime - ending   index of first  dimension for memory allocation
!     jms - starting index of second dimension for memory allocation
!     jme - ending   index of second dimension for memory allocation
!     kms - starting index of third  dimension for memory allocation
!     kme - ending   index of third  dimension for memory allocation
!
!     its - starting index of first  dimension for sub-domain (tile) grid
!     ite - ending   index of first  dimension for sub-domain (tile) grid
!     jts - starting index of second dimension for sub-domain (tile) grid
!     jte - ending   index of second dimension for sub-domain (tile) grid
!     kts - starting index of third  dimension for sub-domain (tile) grid
!     kte - ending   index of third  dimension for sub-domain (tile) grid
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
  use gridmod, only:  region_dx,region_dy
  implicit none
  
  integer(i_kind), intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
  integer(i_kind), intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
  integer(i_kind), intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.
  
  real(r_kind),intent(inout):: v(ims:ime,jms:jme,kms:kme)   ! v wind comp (m/s)
  real(r_kind),intent(inout):: chi(ims:ime,jms:jme,kms:kme) ! Velocity potential
  
  integer(i_kind)           :: i, j, k                       ! Loop counters.
  integer(i_kind)           :: is, ie                        ! 1st dim. end points.
  integer(i_kind)           :: js, je                        ! 2nd dim. end points.
  integer(i_kind)           :: ks, ke                        ! 3rd dim. end points.
  real(r_kind)      :: coeffx(its:ite,jts:jte)       ! Multiplicative coefficient.
  real(r_kind)      :: coeffy(its:ite,jts:jte)       ! Multiplicative coefficient.
  real(r_kind)      :: coeffx_u                      ! Multiplicative coefficient.
  real(r_kind)      :: coeffy_u                      ! Multiplicative coefficient.
  real(r_kind)      :: coeffx_v                      ! Multiplicative coefficient.
  real(r_kind)      :: coeffy_v                      ! Multiplicative coefficient.
  
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

  chi=zero

! Computation to check for edge of domain:
  is = its; ie = ite; js = jts; je = jte
  if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
  if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1
  
  coeffx(its:ite,jts:jte) = half / region_dx(its:ite,jts:jte)
  coeffy(its:ite,jts:jte) = half / region_dy(its:ite,jts:jte)
  
  do k = kts, kte
   
!------------------------------------------------------------------------------
!     [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

     if ( its == ids .AND. jts == jds ) then
        v(its+1,jts,k) = v(its+1,jts,k) + half * v(its,jts,k)
        v(its,jts+1,k) = v(its,jts+1,k) + half * v(its,jts,k)
     end if
     
!    [4.2] Top-left point:

     if ( ite == ide .AND. jts == jds ) then
        v(ite-1,jts,k) = v(ite-1,jts,k) + half * v(ite,jts,k)
        v(ite,jts+1,k) = v(ite,jts+1,k) + half * v(ite,jts,k)
     end if
   
!    [4.3] Bottom-right point:

     if ( its == ids .AND. jte == jde ) then
        v(its+1,jte,k) = v(its+1,jte,k) + half * v(its,jte,k)
        v(its,jte-1,k) = v(its,jte-1,k) + half * v(its,jte,k)
     end if

!    [4.4] Top-right point:

     if ( ite == ide .AND. jte == jde ) then
        v(ite-1,jte,k) = v(ite-1,jte,k) + half * v(ite,jte,k)
        v(ite,jte-1,k) = v(ite,jte-1,k) + half * v(ite,jte,k)
     end if
     
  enddo

!------------------------------------------------------------------------------
! [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

  is = its-1; ie = ite+1; js = jts-1; je = jte+1
  if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
  if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1
  
  do k = kts, kte
   
!    [3.4] Northern boundaries:

     if ( ite == ide ) then
        i = ite
        
        do j = je, js, -1
           coeffy_v = coeffy(i,j) * v(i,j,k)
           
           chi(i  ,j,k) = chi(i  ,j,k) + coeffy_v
           chi(i-2,j,k) = chi(i-2,j,k) - coeffy_v
           
        end do
     end if
      
!    [3.3] Southern boundaries:

     if ( its == ids ) then
        i = its
        
        do j = je, js, -1
           coeffy_v = coeffy(i,j) * v(i,j,k)
           
           chi(i+2,j,k) = chi(i+2,j,k) + coeffy_v
           chi(i  ,j,k) = chi(i  ,j,k) - coeffy_v
           
        end do
     end if
      
!    [3.2] Eastern boundaries:

     if ( jte == jde ) then
        j = jte
        
        do i = ie, is, -1
           coeffy_v = coeffy(i,j) * v(i,j,k)
           
           chi(i+1,j,k) = chi(i+1,j,k) + coeffy_v
           chi(i-1,j,k) = chi(i-1,j,k) - coeffy_v
           
        end do
     end if
     
!    [3.1] Western boundaries:
     if ( jts == jds ) then
        j = jts
        
        do i = ie, is, -1
           coeffy_v = coeffy(i,j) * v(i,j,k)
           
           chi(i+1,j,k) = chi(i+1,j,k) + coeffy_v
           chi(i-1,j,k) = chi(i-1,j,k) - coeffy_v
           
        end do
     end if
     
!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = je, js, -1
        do i = ie, is, -1
           coeffy_v = coeffy(i,j) * v(i,j,k)
           
           chi(i+1,j,k) = chi(i+1,j,k) + coeffy_v
           chi(i-1,j,k) = chi(i-1,j,k) - coeffy_v
           
        end do
     end do
  end do
  
end subroutine tdely_reg
