! subroutines in this file handle the surface mask dependent interpolations
! int22_msk_glb     : global area, interpolate from 2-d to 2-d
! int21_msk_glb     : global area, interpolate from 2-d to one point
! int22_msk_sub     : sub-domain, interpolate from 2-d to 2-d
! int21_msk_sub     : sub-domain, interpolate from 2-d to one point
! int2_msk_glb_prep : global area, expand the area (grids) with a specified surface type
! int2_msk_sub_prep : global area, expand the area (grids) with a specified surface type

 subroutine int22_msk_glb(a,isli_a,rlats_a,rlons_a,nlat_a,nlon_a, &
                          b,isli_b,rlats_b,rlons_b,nlat_b,nlon_b,istyp)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    int22_msk_glb ---(global 2-d array)
!                interpolates from a to b with ancillary surface mask (e.g.,
!                analysis grid => surface grid) for global arrays
!                to gurantee the interpolated value (b) is determined by the
!                candidates (a) with the identical surface type from (a)
!
! prgrmmr:     li -  initial version; org: np2. 02/01/2014
!
! abstract :      This routine interpolates a grid to b grid with surface mask accounted
! notes    :      (1) Here is a 2-d to 2-d interpolation
!                 (2) The interpolation is performed for specified surface types (istyp)
!
! program history log:
!
!  input argument list:
!    a        - real: 2-d array such as analysis increment at analysis grids
!    isli_a   - integer: 2-d array: surface mask (0 = water, 1 = land, 2 = sea ice) for a grids
!    rlats_a  - real: 1-d array: the latitudes of a
!    rlons_a  - real: 1-d array: the logitudes of a
!    nlat_a   - integer: number of latitude of a
!    nlon_a   - integer: number of longitude of a

!    isli_b   - integer: 2-d array: Analysis surface mask (0 = water, 1 = land, 2 = sea ice) for b grids
!    rlats_b  - real: 1-d array: the latitudes of b
!    rlons_b  - real: 1-d array: the logitudes of b
!    nlat_b   - integer: number of latitude of b
!    nlon_b   - integer: number of longitude of b
!    istyp    - integer: target surface type value
!
!  output argument list:
!    b       - real: 2-d array such as analysis increment at surface grids
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
! USES:
 use kinds, only: r_kind,i_kind,r_single
 use constants, only: zero,one,two

 implicit none

! INPUT:
 real   (r_kind), dimension(nlat_a,nlon_a), intent(in   ) :: a
 integer(i_kind), dimension(nlat_a,nlon_a), intent(in   ) :: isli_a

 real   (r_kind), dimension(nlat_a       ), intent(in   ) :: rlats_a
 real   (r_kind), dimension(nlon_a       ), intent(in   ) :: rlons_a

 integer(i_kind), dimension(nlat_b,nlon_b), intent(in   ) :: isli_b
 real   (r_kind), dimension(nlat_b       ), intent(in   ) :: rlats_b
 real   (r_kind), dimension(nlon_b       ), intent(in   ) :: rlons_b

 integer(i_kind), intent(in   ) :: nlat_a,nlon_a,nlat_b,nlon_b,istyp

!OUTPUT:
 real   (r_kind), dimension(nlat_b,nlon_b), intent(  out) :: b

!Declare local variables
 integer(i_kind) :: i,j,ix,iy,ii,jj,ixa,iya,sfctyp_b
 integer(i_kind) :: nwsum,nfinal
 real(r_kind)    :: dx0,dx1,dx2,dx3,dy0,dy1,dy2,dy3,dx,dy,dr
 real(r_kind)    :: dx0s,dx1s,dx2s,dx3s,dy0s,dy1s,dy2s,dy3s
 real(r_kind)    :: ds00,ds01,ds02,ds03,ds10,ds11,ds12,ds13,ds20,ds21,ds22,ds23,ds30,ds31,ds32,ds33

 real(r_kind)    :: bavg,bout,dlat,dlon,wsum4,wsum16
 real(r_kind), dimension(0:1,0:1) :: w4
 real(r_kind), dimension(0:3,0:3) :: w16

 dr = 8.0_r_kind    ! square of the search radius for 16-point cressman-type analysis

 nwsum  = 0
 nfinal = 0
 b=zero
!Loop over all grids of array b to get interpolated value
 do j = 1, nlon_b
   do i = 1, nlat_b
     sfctyp_b = istyp
     if ( isli_b(i,j) == sfctyp_b ) then
       dlon = rlons_b(j)
       call grdcrd1(dlon,rlons_a,nlon_a,1)
       iy = int(dlon); dy = dlon-iy; dy1 = one-dy
 
       iy  = min(max(0,iy),nlon_a); if(iy == 0) iy = nlon_a

       dlat = rlats_b(i)
       call grdcrd1(dlat,rlats_a,nlat_a,1)
       ix = int(dlat); dx = dlat-ix; dx1 = one-dx

       ix = min(max(1,ix),nlat_a)

       w4(0,0) = dx1*dy1; w4(0,1) = dx1*dy; w4(1,0) = dx*dy1; w4(1,1) = dx*dy
!
!      get the interpolated value with the nearby 4-grids (in a) which has
!      the identical surface mask (in b) only

       wsum4 = zero
       bavg  = zero
       bout  = zero
       do jj = 0, 1
         iya = iy + jj
         if ( iya == nlon_a + 1 ) iya = 1
         do ii = 0, 1
           ixa = min(nlat_a,ix + ii)
           bavg  = bavg + w4(ii,jj)*a(ixa,iya)
           if ( isli_a(ixa,iya) == sfctyp_b ) then
               wsum4 = wsum4 + w4(ii,jj)
             bout  = bout  + w4(ii,jj)*a(ixa,iya)
           endif
         enddo
       enddo

       if ( wsum4 > zero ) then
         bout = bout/wsum4
       else

         nwsum = nwsum + 1

!    use more candidates from a (extending one more grid futher in both x and y
!    direction, which means 16 grids from a will be used) when no same
!    surface type candidates can be found in the nearby 4 grids
!    to perform a Cressman_type Analysis

         ix = ix -1; if(ix == 0) ix = 1
         iy = iy -1; if(iy == 0) iy = nlon_a

         dx0 = dx + one; dx1 = dx; dx2 = one - dx; dx3 = two - dx
         dy0 = dy + one; dy1 = dy; dy2 = one - dy; dy3 = two - dy

         dx0s = dx0*dx0; dx1s = dx1*dx1; dx2s = dx2*dx2; dx3s = dx3*dx3
         dy0s = dy0*dy0; dy1s = dy1*dy1; dy2s = dy2*dy2; dy3s = dy3*dy3

         ds00 = dx0s + dy0s; ds01 = dx0s + dy1s; ds02 = dx0s + dy2s; ds03 = dx0s + dy3s
         ds10 = dx1s + dy0s; ds11 = dx1s + dy1s; ds12 = dx1s + dy2s; ds13 = dx1s + dy3s
         ds20 = dx2s + dy0s; ds21 = dx2s + dy1s; ds22 = dx2s + dy2s; ds23 = dx2s + dy3s
         ds30 = dx3s + dy0s; ds31 = dx3s + dy1s; ds32 = dx3s + dy2s; ds33 = dx3s + dy3s

         w16(0,0) = (dr - ds00)/(dr + ds00)
         w16(0,1) = (dr - ds01)/(dr + ds01)
         w16(0,2) = (dr - ds02)/(dr + ds02)
         w16(0,3) = (dr - ds03)/(dr + ds03)

         w16(1,0) = (dr - ds10)/(dr + ds10)
         w16(1,1) = (dr - ds11)/(dr + ds11)
         w16(1,2) = (dr - ds12)/(dr + ds12)
         w16(1,3) = (dr - ds13)/(dr + ds13)

         w16(2,0) = (dr - ds20)/(dr + ds20)
         w16(2,1) = (dr - ds21)/(dr + ds21)
         w16(2,2) = (dr - ds22)/(dr + ds22)
         w16(2,3) = (dr - ds23)/(dr + ds23)
  
         w16(3,0) = (dr - ds30)/(dr + ds30)
         w16(3,1) = (dr - ds31)/(dr + ds31)
         w16(3,2) = (dr - ds32)/(dr + ds32)
         w16(3,3) = (dr - ds33)/(dr + ds33)

         wsum16 = zero
         do jj = 0, 3
           iya = iy + jj
           if ( iya == nlon_a + 1 ) iya = 1
           if ( iya == nlon_a + 2 ) iya = 2
           if ( iya == nlon_a + 3 ) iya = 3
           do ii = 0, 3
             ixa = min(nlat_a,ix + ii)
             if ( isli_a(ixa,iya) == sfctyp_b ) then
             wsum16  = wsum16  + w16(ii,jj)
             bout  = bout  + w16(ii,jj)*a(ixa,iya)
           endif
          enddo
         enddo

         if ( wsum16 > zero ) then
           bout = bout/wsum16
         else
           nfinal = nfinal + 1
         endif

       endif                  ! if ( wsum4 > zero )

       b(i,j)=bout

     endif                ! if ( isli_b(i,j) == sfctyp_b ) then
   enddo                    ! do i = 1, nlat_b
 enddo                      ! do j = 1, nlon_b

  write(*,'(a,I3,I5)') 'Number of grids without specified adjacent surface type,istyp,nwsum ; ',istyp,nwsum
  write(*,'(a,I3,I5)') 'Number of grids without interpolted value,istyp,nwsum ; ',istyp,nwsum

 end subroutine int22_msk_glb

 subroutine int21_msk_sub(a,isli,alats,alons,nx,ny,x,istyp,lat,lon)
                        
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    intrp21_msk_sub ---(from 2-d array to obs. location)
!
! prgrmmr:     li -  initial version; org: np2. 03/01/2014
!
! abstract :      This routine interpolates a (2-d array) to a single point with surface mask accounted
! notes    :      (1) Here is a 2-d to one point interpolation
!                 (2) The mask is availabe for both 2-d array and the single point
!
! program history log:
!
!  input argument list:
!    a      - real: 2-d array such as analysis increment at analysis grids
!    isli   - integer: 2-d array: surface mask (0 = water, 1 = land, 2 = sea ice) for a grids
!    alats  - real: 1-d array: the latitudes of a
!    alons  - real: 1-d array: the logitudes of a
!    nx     - integer: number of latitude of a
!    ny     - integer: number of longitude of a
!    istyp  - integer: surface type of point x
!    lat    - real: latitude of x 
!    lon    - real: longitude of x 
!    output argument list:
!    x       - real: a variable (same type of a) at a single point 
!$$$ end documentation block
! USES:
 use kinds, only: r_kind,i_kind,r_single
 use constants, only: zero,one,two

 implicit none

! INPUT:
 real   (r_kind), dimension(nx,ny), intent(in   ) :: a
 integer(i_kind), dimension(nx,ny), intent(in   ) :: isli

 real   (r_kind), dimension(nx), intent(in   ) :: alats
 real   (r_kind), dimension(ny), intent(in   ) :: alons

 integer(i_kind), intent(in   ) :: nx,ny,istyp,lat,lon

!OUTPUT:
 real   (r_kind), intent(  out) :: x

!Declare local variables
 integer(i_kind) :: ix,iy,ii,jj,ixa,iya
 integer(i_kind) :: nwsum,nfinal
 real(r_kind)    :: dx0,dx1,dx2,dx3,dy0,dy1,dy2,dy3,dx,dy,dr
 real(r_kind)    :: dx0s,dx1s,dx2s,dx3s,dy0s,dy1s,dy2s,dy3s
 real(r_kind)    :: ds00,ds01,ds02,ds03,ds10,ds11,ds12,ds13,ds20,ds21,ds22,ds23,ds30,ds31,ds32,ds33

 real(r_kind)    :: bavg,bout,dlat,dlon,wsum4,wsum16
 real(r_kind), dimension(0:1,0:1) :: w4
 real(r_kind), dimension(0:3,0:3) :: w16

 dr = 8.0_r_kind    ! square of the search radius for 16-point cressman-type analysis

 nwsum  = 0
 nfinal = 0
 x=zero
!
!to get interpolated value of x with a (array) and mask info
!
 dlon = lon
 call grdcrd1(dlon,alons,ny,1)
 iy = int(dlon); dy = dlon-iy; dy1 = one-dy
 iy = min(max(0,iy),ny); if(iy == 0) iy = ny

 dlat = lat
 call grdcrd1(dlat,alats,nx,1)
 ix = int(dlat); dx = dlat-ix; dx1 = one-dx
 ix = min(max(1,ix),nx)

 w4(0,0) = dx1*dy1; w4(0,1) = dx1*dy; w4(1,0) = dx*dy1; w4(1,1) = dx*dy
!
!get the interpolated value with the nearby 4-grids (in a) which has
!the identical surface mask (istyp for x) only

 wsum4 = zero
 bavg  = zero
 bout  = zero
 do jj = 0, 1
   iya = iy + jj
   if ( iya == ny + 1 ) iya = 1
   do ii = 0, 1
     ixa = min(nx,ix + ii)
     bavg  = bavg + w4(ii,jj)*a(ixa,iya)
     if ( isli(ixa,iya) == istyp ) then
       wsum4 = wsum4 + w4(ii,jj)
       bout  = bout  + w4(ii,jj)*a(ixa,iya)
     endif
   enddo
 enddo

 if ( wsum4 > zero ) then
   bout = bout/wsum4
 else

   nwsum = nwsum + 1

!  use more candidates from a (extending one more grid futher in both x and y
!  direction, which means 16 grids from a will be used) when no same
!  surface type candidates can be found in the nearby 4 grids
!  to perform a Cressman_type Analysis

   ix = ix -1; if(ix == 0) ix = 1
   iy = iy -1; if(iy == 0) iy = ny

   dx0 = dx + one; dx1 = dx; dx2 = one - dx; dx3 = two - dx
   dy0 = dy + one; dy1 = dy; dy2 = one - dy; dy3 = two - dy

   dx0s = dx0*dx0; dx1s = dx1*dx1; dx2s = dx2*dx2; dx3s = dx3*dx3
   dy0s = dy0*dy0; dy1s = dy1*dy1; dy2s = dy2*dy2; dy3s = dy3*dy3

   ds00 = dx0s + dy0s; ds01 = dx0s + dy1s; ds02 = dx0s + dy2s; ds03 = dx0s + dy3s
   ds10 = dx1s + dy0s; ds11 = dx1s + dy1s; ds12 = dx1s + dy2s; ds13 = dx1s + dy3s
   ds20 = dx2s + dy0s; ds21 = dx2s + dy1s; ds22 = dx2s + dy2s; ds23 = dx2s + dy3s
   ds30 = dx3s + dy0s; ds31 = dx3s + dy1s; ds32 = dx3s + dy2s; ds33 = dx3s + dy3s

   w16(0,0) = (dr - ds00)/(dr + ds00)
   w16(0,1) = (dr - ds01)/(dr + ds01)
   w16(0,2) = (dr - ds02)/(dr + ds02)
   w16(0,3) = (dr - ds03)/(dr + ds03)

   w16(1,0) = (dr - ds10)/(dr + ds10)
   w16(1,1) = (dr - ds11)/(dr + ds11)
   w16(1,2) = (dr - ds12)/(dr + ds12)
   w16(1,3) = (dr - ds13)/(dr + ds13)

   w16(2,0) = (dr - ds20)/(dr + ds20)
   w16(2,1) = (dr - ds21)/(dr + ds21)
   w16(2,2) = (dr - ds22)/(dr + ds22)
   w16(2,3) = (dr - ds23)/(dr + ds23)
  
   w16(3,0) = (dr - ds30)/(dr + ds30)
   w16(3,1) = (dr - ds31)/(dr + ds31)
   w16(3,2) = (dr - ds32)/(dr + ds32)
   w16(3,3) = (dr - ds33)/(dr + ds33)

   wsum16 = zero
   do jj = 0, 3
     iya = iy + jj
     if ( iya == ny + 1 ) iya = 1
     if ( iya == ny + 2 ) iya = 2
     if ( iya == ny + 3 ) iya = 3
     do ii = 0, 3
       ixa = min(nx,ix + ii)
       if ( isli(ixa,iya) == istyp ) then
       wsum16  = wsum16  + w16(ii,jj)
       bout  = bout  + w16(ii,jj)*a(ixa,iya)
     endif
    enddo
   enddo

   if ( wsum16 > zero ) then
     bout = bout/wsum16
   else
     nfinal = nfinal + 1
   endif

 endif                  ! if ( wsum4 > zero )

 x=bout

 write(*,'(a,I3,I5)') 'No specified adjacent surface type,istyp,nwsum ; ',istyp,nwsum
 write(*,'(a,I3,I5)') 'No interpolted value,istyp,nwsum ; ',istyp,nwsum

 end subroutine int21_msk_sub

 subroutine int2_msk_glb_prep(a,isli_a,b,isli_b,nx,ny,sfctyp_b,nprep)
!$$$  subprogram documentation block
!                .      .    .
! subroutine:    int2_msk_glb_prep --- (global 2-d array)
!                for a specified surface type (sfctyp_b), expanding the area (grids) with this
!                surface type using the surounded 8 grids with the same surface type
!
!  prgrmmr:     li -  initial version; org: np2
!
!  input argument list:
!    a        - real: 2-d array
!    isli_a   - integer: 2-d array: surface mask (0 = water, 1 = land, 2 = seaice) for a grids
!    nx       - integer: number of grids in x-direction
!    ny       - integer: number of grids in y-direction
!    sfctyp_b - integer: the targeted surface type (0=water, 1=land, 2-sea ice)
!    nprep    - integer: number of times to do the extension

!
!  output argument list:
!    b        - real: 2-d array such as analysis increment at surface grids
!    isli_b   - integer: 2-d array such as analysis surface mask
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
! USES:
 use kinds, only: r_kind,i_kind,r_single
 use constants, only: zero,one,two

 implicit none

! INPUT:
 real   (r_kind), dimension(nx,ny), intent(in   ) :: a
 integer(i_kind), dimension(nx,ny), intent(in   ) :: isli_a
 integer(i_kind),                   intent(in   ) :: nx,ny,sfctyp_b,nprep

!OUTPUT:
 real   (r_kind), dimension(nx,ny), intent(  out) :: b
 integer(i_kind), dimension(nx,ny), intent(  out) :: isli_b

!Declare local variables
 integer(i_kind) :: i,j,k,ii,jj,ix,iy,n
 real(r_kind)    :: bout
 real(r_kind),    dimension(nx,ny) :: wb
 integer(r_kind), dimension(nx,ny) :: wmsk

!
! Initialize b and isli_b
!
  b = a; isli_b = isli_a

  do k = 1, nprep
!
! Initialize/update work array for the value and mask
!
    wb = b; wmsk  = isli_b
!
!   Loop over all grids of array b to update the grids nearby the sfctyp_b surface type
!
    do j = 1, ny
      do i = 1, nx
        if ( wmsk(i,j) /= sfctyp_b ) then
          n = 0
          bout = zero
          do jj = j - 1, j + 1
            do ii = i - 1, i + 1
              iy = jj; if (iy == ny + 1) iy = 1; if (iy == 0) iy = ny
              ix = min(max(ii,1),nx)
              if ( wmsk(ix,iy) == sfctyp_b ) then
                n = n + 1
                bout = bout + wb(ix,iy)
              endif
            enddo
          enddo
          if ( n > 0 ) then
            b(i,j)      = bout/real(n)
            isli_b(i,j) = sfctyp_b
          endif
        endif
      enddo
    enddo
  enddo                  ! do k = 1, nprep

 end subroutine int2_msk_glb_prep

 subroutine reset_nst(nst_fld,dim1,dim2,ii,jj)

! prgrmmr:     li -  initial version; org: np2. 04/01/2014
!
! abctract: reset the NSST model variables for a new open water grid (sea ice just melted)

!  input argument list:
!    dim1        - integer: 1st dimension of a 2-d array, e.g., nst_fld%xt(dim1,dim2)
!    dim2        - integer: 2nd dimension of a 2-d array, e.g., nst_fld%xt(dim1,dim2)
!    ii          - integer: location of the new water grid in dim1
!    jj          - integer: location of the new water grid in dim2
!   
!  in & output argument list:
!    nst_fld   - Nst_Var_Data: a type variable of NSST 
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP

  use kinds, only: r_kind,i_kind
  use Nst_Var_ESMFMod, only: Nst_Var_Data,nstvar_aldata
  use constants, only: zero,z_w_max,tfrozen
  implicit none

  type(Nst_Var_Data), intent(inout) :: nst_fld
  integer(i_kind),    intent(in   ) :: dim1,dim2,ii,jj

  integer(i_kind) :: iret

!
! allocate nst_fld
!
  call nstvar_aldata(dim1,dim2,nst_fld,iret)

!
! reset nst_fld at grid (ii,jj)
!
  nst_fld%xt(ii,jj)      = zero
  nst_fld%xs(ii,jj)      = zero 
  nst_fld%xu(ii,jj)      = zero 
  nst_fld%xv(ii,jj)      = zero 
  nst_fld%xz(ii,jj)      = z_w_max
  nst_fld%zm(ii,jj)      = zero 
  nst_fld%xtts(ii,jj)    = zero 
  nst_fld%xzts(ii,jj)    = zero 
  nst_fld%dt_cool(ii,jj) = zero 
  nst_fld%z_c(ii,jj)     = zero 
  nst_fld%c_0(ii,jj)     = zero 
  nst_fld%c_d(ii,jj)     = zero 
  nst_fld%w_0(ii,jj)     = zero 
  nst_fld%w_d(ii,jj)     = zero 
  nst_fld%d_conv(ii,jj)  = zero 
  nst_fld%ifd(ii,jj)     = zero 
  nst_fld%tref(ii,jj)    = tfrozen
  nst_fld%qrain(ii,jj)   = zero 

end subroutine reset_nst

