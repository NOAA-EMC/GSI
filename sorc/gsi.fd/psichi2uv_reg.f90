subroutine psichi2uv_reg( psi, chi,  u, v, &
     ids,ide, jds,jde, kds,kde,  &
     ims,ime, jms,jme, kms,kme,  &
     its,ite, jts,jte, kts,kte )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    psichi2uv_reg
!     prgmmr:    barker, d   org:  np22              date:  2000-02-03
!
! abstract:  Calculate wind components u and v from psi and chi 
!            (streamfunction and velocity potential, respectively)
!
! program history log:
!   2000/02/03   barker, dale - creation of F90 version
!   2001/10/30   barker       - parallel version
!   2003/09/05   parrish      - adapted to unified NCEP 3dvar
!   2003/10/17   wu, wanshu   - first index Y second X
!   2004-06-22   treadon      - update documentation
!
!   input argument list:
!     psi - streamfunction
!     chi - velocity potential
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
!     u - zonal wind component
!     v - meridional wind component
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
  use constants, only: half
  use gridmod, only: region_dx,region_dy
  
  implicit none
  
  integer(i_kind), intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
  integer(i_kind), intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
  integer(i_kind), intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.
  
  real(r_kind), intent(in)   :: psi(ims:ime,jms:jme,kms:kme) ! Stream function
  real(r_kind), intent(in)   :: chi(ims:ime,jms:jme,kms:kme) ! Velocity potential
  real(r_kind), intent(out)  :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp (m/s)
  real(r_kind), intent(out)  :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp (m/s)
  
  integer(i_kind)            :: i, j, k                      ! Loop counters.
  integer(i_kind)            :: is, ie                       ! 1st dim. end points.
  integer(i_kind)            :: js, je                       ! 2nd dim. end points.
  integer(i_kind)            :: ks, ke                       ! 3rd dim. end points.
  real(r_kind)       :: coeffx(its:ite,jts:jte)      ! Multiplicative coeff.
  real(r_kind)       :: coeffy(its:ite,jts:jte)      ! Multiplicative coeff.
  
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

!  Computation to check for edge of domain:
  is = its; ie = ite; js = jts; je = jte
  if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
  if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

  coeffx(its:ite,jts:jte) = half / region_dx(its:ite,jts:jte)
  coeffy(its:ite,jts:jte) = half / region_dy(its:ite,jts:jte)

  do k = kts, kte

!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = js, je
        do i = is, ie
           u(i,j,k) = -( psi(i+1,j  ,k) - psi(i-1,j  ,k) )*coeffy(i,j) + &
                ( chi(i  ,j+1,k) - chi(i  ,j-1,k) )*coeffx(i,j)
           
           v(i,j,k) = ( psi(i  ,j+1,k) - psi(i  ,j-1,k) )*coeffx(i,j) + &
                ( chi(i+1,j  ,k) - chi(i-1,j  ,k) ) * coeffy(i,j)
        end do
     end do
     

!------------------------------------------------------------------------------
!  [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

!    [3.1] Western boundaries:

     if ( jts == jds ) then
        j = jts
        do i = is, ie
           u(i,j,k) = -( psi(i+1,j  ,k) - psi(i-1,j  ,k) )*coeffy(i,j) + &
                ( chi(i  ,j+2,k) - chi(i  ,j  ,k) )*coeffx(i,j)
           v(i,j,k) = ( psi(i  ,j+2,k) - psi(i  ,j  ,k) )*coeffx(i,j) + &
                ( chi(i+1,j  ,k) - chi(i-1,j  ,k) ) * coeffy(i,j)
        end do
     end if
     
!    [3.2] Eastern boundaries:

     if ( jte == jde ) then
        j = jte
        do i = is, ie
           u(i,j,k) = -( psi(i+1,j  ,k) - psi(i-1,j  ,k) )*coeffy(i,j) + &
                ( chi(i  ,j  ,k) - chi(i  ,j-2,k) )*coeffx(i,j)
           v(i,j,k) = ( psi(i  ,j  ,k) - psi(i  ,j-2,k) )*coeffx(i,j) + &
                ( chi(i+1,j  ,k) - chi(i-1,j  ,k) ) * coeffy(i,j)
        end do
     end if
     
!    [3.3] Southern boundaries:

     if ( its == ids ) then
        i = its
        do j = js, je
           u(i,j,k) = -( psi(i+2,j  ,k) - psi(i  ,j  ,k) )*coeffy(i,j) + &
                ( chi(i  ,j+1,k) - chi(i  ,j-1,k) )*coeffx(i,j)
           
           v(i,j,k) = ( psi(i  ,j+1,k) - psi(i  ,j-1,k) )*coeffx(i,j) + &
                ( chi(i+2,j  ,k) - chi(i  ,j  ,k) ) * coeffy(i,j)
           
        end do
     end if
     
!    [3.4] Northern boundaries:

     if ( ite == ide ) then
        i = ite
        do j = js, je
           u(i,j,k) = -( psi(i  ,j  ,k) - psi(i-2,j  ,k) )*coeffy(i,j) + &
                ( chi(i  ,j+1,k) - chi(i  ,j-1,k) )*coeffx(i,j)
           
           v(i,j,k) = ( psi(i  ,j+1,k) - psi(i  ,j-1,k) )*coeffx(i,j) + &
                ( chi(i  ,j  ,k) - chi(i-2,j  ,k) ) * coeffy(i,j)
        end do
     end if
     
!------------------------------------------------------------------------------
!    [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

     if ( its == ids .AND. jts == jds ) then
        u(its,jts,k) = half * ( u(its+1,jts,k) + u(its,jts+1,k) )
        v(its,jts,k) = half * ( v(its+1,jts,k) + v(its,jts+1,k) )
     end if
  
!    [4.2] Top-left point:

     if ( ite == ide .AND. jts == jds ) then
        u(ite,jts,k) = half * ( u(ite-1,jts,k) + u(ite,jts+1,k) )
        v(ite,jts,k) = half * ( v(ite-1,jts,k) + v(ite,jts+1,k) )
     end if
     
!    [4.3] Bottom-right point:

     if ( its == ids .AND. jte == jde ) then
        u(its,jte,k) = half * ( u(its+1,jte,k) + u(its,jte-1,k) )
        v(its,jte,k) = half * ( v(its+1,jte,k) + v(its,jte-1,k) )
     end if
     
!    [4.4] Top-right point:

     if ( ite == ide .AND. jte == jde ) then
        u(ite,jte,k) = half * ( u(ite-1,jte,k) + u(ite,jte-1,k) )
        v(ite,jte,k) = half * ( v(ite-1,jte,k) + v(ite,jte-1,k) )
     end if
     
  end do
  
end subroutine psichi2uv_reg

subroutine delx_reg( chi,  u,  &
     ids,ide, jds,jde, kds,kde,  &
     ims,ime, jms,jme, kms,kme,  &
     its,ite, jts,jte, kts,kte )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    psichi2uv_reg
!     prgmmr:    barker, d   org:  np22              date:  2000-02-03
!
! abstract:  Calculate wind components u and v from psi and chi 
!            (streamfunction and velocity potential, respectively)
!
! program history log:
!   2000/02/03   barker, dale - creation of F90 version
!   2001/10/30   barker       - parallel version
!   2003/09/05   parrish      - adapted to unified NCEP 3dvar
!   2003/10/17   wu, wanshu   - first index Y second X
!   2004-06-22   treadon      - update documentation
!
!   input argument list:
!     psi - streamfunction
!     chi - velocity potential
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
!     u - zonal wind component
!     v - meridional wind component
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
  use constants, only: half
  use gridmod, only: region_dx,region_dy
  
  implicit none
  
  integer(i_kind), intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
  integer(i_kind), intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
  integer(i_kind), intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.
  
  real(r_kind), intent(in)   :: chi(ims:ime,jms:jme,kms:kme) ! Velocity potential
  real(r_kind), intent(out)  :: u(ims:ime,jms:jme,kms:kme)   ! v wind comp (m/s)
  
  integer(i_kind)            :: i, j, k                      ! Loop counters.
  integer(i_kind)            :: is, ie                       ! 1st dim. end points.
  integer(i_kind)            :: js, je                       ! 2nd dim. end points.
  integer(i_kind)            :: ks, ke                       ! 3rd dim. end points.
  real(r_kind)       :: coeffx(its:ite,jts:jte)      ! Multiplicative coeff.
  real(r_kind)       :: coeffy(its:ite,jts:jte)      ! Multiplicative coeff.
  
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

!  Computation to check for edge of domain:
  is = its; ie = ite; js = jts; je = jte
  if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
  if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

  coeffx(its:ite,jts:jte) = half / region_dx(its:ite,jts:jte)
  coeffy(its:ite,jts:jte) = half / region_dy(its:ite,jts:jte)

  do k = kts, kte

!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = js, je
        do i = is, ie
           u(i,j,k) =  &
                ( chi(i  ,j+1,k) - chi(i  ,j-1,k) )*coeffx(i,j)
           
        end do
     end do
     

!------------------------------------------------------------------------------
!  [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

!    [3.1] Western boundaries:

     if ( jts == jds ) then
        j = jts
        do i = is, ie
           u(i,j,k) = &
                ( chi(i  ,j+2,k) - chi(i  ,j  ,k) )*coeffx(i,j)
        end do
     end if
     
!    [3.2] Eastern boundaries:

     if ( jte == jde ) then
        j = jte
        do i = is, ie
           u(i,j,k) = &
                ( chi(i  ,j  ,k) - chi(i  ,j-2,k) )*coeffx(i,j)
        end do
     end if
     
!    [3.3] Southern boundaries:

     if ( its == ids ) then
        i = its
        do j = js, je
           u(i,j,k) = &
                ( chi(i  ,j+1,k) - chi(i  ,j-1,k) )*coeffx(i,j)
           
           
        end do
     end if
     
!    [3.4] Northern boundaries:

     if ( ite == ide ) then
        i = ite
        do j = js, je
           u(i,j,k) = &
                ( chi(i  ,j+1,k) - chi(i  ,j-1,k) )*coeffx(i,j)
           
        end do
     end if
     
!------------------------------------------------------------------------------
!    [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

     if ( its == ids .AND. jts == jds ) then
        u(its,jts,k) = half * ( u(its+1,jts,k) + u(its,jts+1,k) )
     end if
  
!    [4.2] Top-left point:

     if ( ite == ide .AND. jts == jds ) then
        u(ite,jts,k) = half * ( u(ite-1,jts,k) + u(ite,jts+1,k) )
     end if
     
!    [4.3] Bottom-right point:

     if ( its == ids .AND. jte == jde ) then
        u(its,jte,k) = half * ( u(its+1,jte,k) + u(its,jte-1,k) )
     end if
     
!    [4.4] Top-right point:

     if ( ite == ide .AND. jte == jde ) then
        u(ite,jte,k) = half * ( u(ite-1,jte,k) + u(ite,jte-1,k) )
     end if
     
  end do
  
end subroutine delx_reg

subroutine dely_reg( chi,  v, &
     ids,ide, jds,jde, kds,kde,  &
     ims,ime, jms,jme, kms,kme,  &
     its,ite, jts,jte, kts,kte )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    psichi2uv_reg
!     prgmmr:    barker, d   org:  np22              date:  2000-02-03
!
! abstract:  Calculate wind components u and v from psi and chi 
!            (streamfunction and velocity potential, respectively)
!
! program history log:
!   2000/02/03   barker, dale - creation of F90 version
!   2001/10/30   barker       - parallel version
!   2003/09/05   parrish      - adapted to unified NCEP 3dvar
!   2003/10/17   wu, wanshu   - first index Y second X
!   2004-06-22   treadon      - update documentation
!
!   input argument list:
!     psi - streamfunction
!     chi - velocity potential
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
!     u - zonal wind component
!     v - meridional wind component
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
  use constants, only: half
  use gridmod, only: region_dx,region_dy
  
  implicit none
  
  integer(i_kind), intent(in):: ids,ide, jds,jde, kds,kde    ! domain dims.
  integer(i_kind), intent(in):: ims,ime, jms,jme, kms,kme    ! memory dims.
  integer(i_kind), intent(in):: its,ite, jts,jte, kts,kte    ! tile   dims.
  
  real(r_kind), intent(in)   :: chi(ims:ime,jms:jme,kms:kme) ! Velocity potential
  real(r_kind), intent(out)  :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp (m/s)
  
  integer(i_kind)            :: i, j, k                      ! Loop counters.
  integer(i_kind)            :: is, ie                       ! 1st dim. end points.
  integer(i_kind)            :: js, je                       ! 2nd dim. end points.
  integer(i_kind)            :: ks, ke                       ! 3rd dim. end points.
  real(r_kind)       :: coeffx(its:ite,jts:jte)      ! Multiplicative coeff.
  real(r_kind)       :: coeffy(its:ite,jts:jte)      ! Multiplicative coeff.
  
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

!  Computation to check for edge of domain:
  is = its; ie = ite; js = jts; je = jte
  if ( its == ids ) is = ids+1; if ( ite == ide ) ie = ide-1
  if ( jts == jds ) js = jds+1; if ( jte == jde ) je = jde-1

  coeffx(its:ite,jts:jte) = half / region_dx(its:ite,jts:jte)
  coeffy(its:ite,jts:jte) = half / region_dy(its:ite,jts:jte)

  do k = kts, kte

!------------------------------------------------------------------------------
!  [2.0] Compute u, v at interior points (2nd order central finite diffs):
!------------------------------------------------------------------------------

     do j = js, je
        do i = is, ie
           
           v(i,j,k) =  &
                ( chi(i+1,j  ,k) - chi(i-1,j  ,k) ) * coeffy(i,j)
        end do
     end do
     

!------------------------------------------------------------------------------
!  [3.0] Compute u, v at domain boundaries:
!------------------------------------------------------------------------------

!    [3.1] Western boundaries:

     if ( jts == jds ) then
        j = jts
        do i = is, ie
           v(i,j,k) =  &
                ( chi(i+1,j  ,k) - chi(i-1,j  ,k) ) * coeffy(i,j)
        end do
     end if
     
!    [3.2] Eastern boundaries:

     if ( jte == jde ) then
        j = jte
        do i = is, ie
           v(i,j,k) =  &
                ( chi(i+1,j  ,k) - chi(i-1,j  ,k) ) * coeffy(i,j)
        end do
     end if
     
!    [3.3] Southern boundaries:

     if ( its == ids ) then
        i = its
        do j = js, je
           
           v(i,j,k) = &
                ( chi(i+2,j  ,k) - chi(i  ,j  ,k) ) * coeffy(i,j)
           
        end do
     end if
     
!    [3.4] Northern boundaries:

     if ( ite == ide ) then
        i = ite
        do j = js, je
           
           v(i,j,k) = &
                ( chi(i  ,j  ,k) - chi(i-2,j  ,k) ) * coeffy(i,j)
        end do
     end if
     
!------------------------------------------------------------------------------
!    [4.0] Corner points (assume average of surrounding points - poor?):
!------------------------------------------------------------------------------

!    [4.1] Bottom-left point:

     if ( its == ids .AND. jts == jds ) then
        v(its,jts,k) = half * ( v(its+1,jts,k) + v(its,jts+1,k) )
     end if
  
!    [4.2] Top-left point:

     if ( ite == ide .AND. jts == jds ) then
        v(ite,jts,k) = half * ( v(ite-1,jts,k) + v(ite,jts+1,k) )
     end if
     
!    [4.3] Bottom-right point:

     if ( its == ids .AND. jte == jde ) then
        v(its,jte,k) = half * ( v(its+1,jte,k) + v(its,jte-1,k) )
     end if
     
!    [4.4] Top-right point:

     if ( ite == ide .AND. jte == jde ) then
        v(ite,jte,k) = half * ( v(ite-1,jte,k) + v(ite,jte-1,k) )
     end if
     
  end do
  
end subroutine dely_reg
