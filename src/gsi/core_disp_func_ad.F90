#ifdef FCA_REF_MOD
module core_disp_func_ad_m
!$$$ module documentation block
!           .      .    .                                       .
! module:   core_disp_func_ad_m
!  prgmmr: Nehrkorn
!
! abstract: Core displacement functionality routines, adjoint version
!
! program history log:
!
! subroutines included:
!   sub bicub_interp_ad
!   sub cubicInterpolate_ad
!   sub bilin_interp_ad
!   sub compute_xy_orig_ad
!   sub apply_disp_2d_ad
!   sub APPLY_VERT_AD
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind, i_kind
  use core_disp_types_m, only: fca_gridded_meta, fca_gridded_disp, &
       ids,ide,jds,jde,kds,kde, ims,ime,jms,jme,kms,kme, &
       its,ite,jts,jte,kts,kte, ips,ipe,jps,jpe,kps,kpe, &
       bilinear, bicubic
  use core_disp_func_m, only: cubicInterpolate
  implicit none
  private
  public :: bicub_interp_ad, bilin_interp_ad, compute_xy_orig_ad, apply_disp_2d_ad, apply_vert_ad
  contains
#else
#define TRACE_USE
#endif

subroutine bicub_interp_ad(fin,i,j,dx,dy,adj_ad,dx_ad,dy_ad)
  implicit none
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bicub_interp_ad
!   prgmmr: Nehrkorn
!
! abstract:
!
! program history log:
!
!   input argument list:
!     fin, dx, dy, adj_ad
!     dx_ad, dy_ad
!     i, j
!
!   output argument list:
!     dx_ad, dy_ad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  real(r_kind), intent(in) :: fin(ims:ime,jms:jme), dx, dy, adj_ad
  real(r_kind), intent(inout) :: dx_ad, dy_ad
  integer(i_kind), intent(in) :: i, j

  real(r_kind) :: yy(-1:2),yy_ad(-1:2),f_ad(-1:2)
  integer(i_kind) :: jj

  yy_ad(:)=0.
  f_ad(:)=0.

  do jj=-1,2
     yy(jj) = cubicInterpolate(fin((i-1):(i+2),j+jj),dx)
  end do

  call cubicInterpolate_ad(yy,dy,adj_ad,yy_ad,dy_ad)

  do jj=2,-1,-1
     call cubicInterpolate_ad(fin((i-1):(i+2),j+jj),dx,yy_ad(jj),f_ad,dx_ad)
  end do
end subroutine bicub_interp_ad

subroutine cubicInterpolate_ad(f,x,adj_ad,f_ad,x_ad)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cubicInterpolate_ad
!   prgmmr: Nehrkorn
!
! abstract:
!
! program history log:
!
!   input argument list:
!     f
!     x, adj_ad
!     f_ad, x_ad
!
!   output argument list:
!     f_ad, x_ad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  real(r_kind), intent(in) :: f(-1:2) !values at x=-1,0,1,2
  real(r_kind), intent(in) :: x, adj_ad ! value in [0,1]
  real(r_kind), intent(inout) :: f_ad(-1:2), x_ad

  ! Adjoint of:
!!$         x_tl     * 0.5 * (f(1) - f(-1)) + &
!!$         x_tl*x         * (2.0*f(-1) - 5.0*f(0) + 4.0*f(1) - f(2)) + &
!!$         x_tl*x*x * 1.5 * (3.0*(f(0) - f(1)) + f(2) - f(-1)) + &

  x_ad = x_ad + adj_ad * 0.5_r_kind * (f(1) - f(-1))
  x_ad = x_ad + adj_ad * x   * (2.0_r_kind*f(-1) - 5.0_r_kind*f(0) + 4.0_r_kind*f(1) - f(2))
  x_ad = x_ad + adj_ad * x*x * 1.5_r_kind * (3.0_r_kind*(f(0) - f(1)) + f(2) - f(-1))

  ! Adjoint of:
!!$         f_tl(0) + &
!!$         x     * 0.5 * (f_tl(1) - f_tl(-1)) + &
!!$         x*x   * 0.5 * (2.0*f_tl(-1) - 5.0*f_tl(0) + 4.0*f_tl(1) - f_tl(2)) + &
!!$         x*x*x * 0.5 * (3.0*(f_tl(0) - f_tl(1)) + f_tl(2) - f_tl(-1))
  ! equivalent of:
!!$         f_tl(-1) * (       -0.5*x +     x*x - 0.5*x*x*x) + &
!!$         f_tl(0)  * ( 1            - 2.5*x*x + 1.5*x*x*x) + &
!!$         f_tl(1)  * (        0.5*x +   2*x*x - 1.5*x*x*x) + &
!!$         f_tl(2)  * (              - 0.5*x*x + 0.5*x*x*x)

  f_ad(-1) = f_ad(-1) + adj_ad * (       -0.5_r_kind*x +     x*x - 0.5_r_kind*x*x*x)
  f_ad(0)  = f_ad(0)  + adj_ad * ( 1            - 2.5_r_kind*x*x + 1.5_r_kind*x*x*x)
  f_ad(1)  = f_ad(1)  + adj_ad * (        0.5_r_kind*x +   2_r_kind*x*x - 1.5_r_kind*x*x*x)
  f_ad(2)  = f_ad(2)  + adj_ad * (              - 0.5_r_kind*x*x + 0.5_r_kind*x*x*x)

end subroutine cubicInterpolate_ad

subroutine bilin_interp_ad(f00, f10, f01, f11, dx, dy, adj_ad, dx_ad, dy_ad)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bilin_interp_ad
!   prgmmr: Nehrkorn
!
! abstract: bilinear interpolation of 4 nearest neighbors to a point
!
! program history log:
!
!   input argument list:
!     f00, f10, f01, f11, dx, dy, adj_ad
!     dx_ad,dy_ad
!
!   output argument list:
!     dx_ad,dy_ad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  ! bilinear interpolation of 4 nearest neighbors to a point
  implicit none
  ! bilinear interpolate in the unit box.
  real(r_kind), intent(in) :: f00, f10, f01, f11, dx, dy, adj_ad
  real(r_kind), intent(inout) :: dx_ad,dy_ad
  !  Adjoint of:
!!$    bilin_interp_tl = &
!!$         dx_tl*(-f00*(1.-dy) + f10*(1.-dy) - f01*dy + f11*dy) + &
!!$         dy_tl*(-f00*(1.-dx) - f10*dx + f01*(1.-dx) + f11*dx)
  dx_ad = dx_ad + adj_ad*(-f00*(1._r_kind-dy) + f10*(1._r_kind-dy) - f01*dy + f11*dy)
  dy_ad = dy_ad + adj_ad*(-f00*(1._r_kind-dx) - f10*dx + f01*(1._r_kind-dx) + f11*dx)
  return
end subroutine bilin_interp_ad

subroutine compute_xy_orig_ad(x_disp, y_disp, ix_orig, iy_orig, dx_n, dy_n,&
	   x_disp_ad, y_disp_ad, dx_n_ad, dy_n_ad, meta, order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: compute_xy_orig_ad
!   prgmmr: Nehrkorn
!
! abstract: apply displacements to a gridded 2d field
!  NOTE: gridded fields assumed to be on the same grid as the displacements
!
! program history log:
!
!   input argument list: original field in bg, displacements in disp
!     x_disp, y_disp
!     x_disp_ad, y_disp_ad,  dx_n_ad, dy_n_ad, dx_n, dy_n
!     ix_orig, iy_orig
!     meta
!     order
!
!   output argument list: displaced field in adj
!     x_disp, y_disp
!     x_disp_ad, y_disp_ad,  dx_n_ad, dy_n_ad, dx_n, dy_n
!     ix_orig, iy_orig
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  real(r_kind), dimension(:,:), intent(inout) :: x_disp, y_disp, &
  	    x_disp_ad, y_disp_ad,  dx_n_ad, dy_n_ad, dx_n, dy_n
  integer(i_kind), dimension(:,:), intent(inout) :: ix_orig, iy_orig
  type (fca_gridded_meta), intent(in) :: meta		! meta data
  integer(i_kind), intent(in) :: order
  real(r_kind), dimension(size(x_disp,1),size(x_disp,2)) :: x, y, x_ad, y_ad
  integer(i_kind) :: i, j, status, buf, Nx, Ny

  
#ifdef TRACE_USE
  if (trace_use) call da_trace_entry("compute_xy_orig_ad")
#endif

  Nx = size(x_disp,1)
  Ny = size(x_disp,2)

  !Initialize local adjoint:
  x_ad(:,:)=0._r_kind
  y_ad(:,:)=0._r_kind

  !recompute NLM:

  forall(i=1:Nx,j=1:Ny) 
     x(i,j) = its-1+(i-1)*meta%dx + x_disp(i,j) + meta%x0
     y(i,j) = jts-1+(j-1)*meta%dy + y_disp(i,j) + meta%y0
  end forall

  ! new array indices.
  ix_orig = floor(x/meta%dx)+1 ! x index of lower-left corner of containing value to bring back here
  iy_orig = floor(y/meta%dy)+1 ! y ""

  ! Beginning of adjoint:

  !	use constant extrapolation for points outside the grid:
  if (order .eq. bilinear) then
     buf=1
  elseif (order .eq. bicubic) then
     buf=2
  end if
  where (ix_orig < ids+buf-1)
     dx_n_ad = 0_r_kind 
     dx_n = 0_r_kind 
     ix_orig = ids+buf-1
  elsewhere (ix_orig > (ide-1-buf))
     dx_n_ad = 0_r_kind 
     dx_n = 1_r_kind
     ix_orig = ide-1-buf
  elsewhere
     ! x weight for interpolating values from box (i.e. 0.25 -> 25% from bottom, 75% from top points)
     x_ad = x_ad + dx_n_ad/meta%dx
     dx_n = mod(x,meta%dx)/meta%dx
  end where

  where (iy_orig < jds+buf-1)
     dy_n_ad = 0_r_kind 
     dy_n = 0_r_kind
     iy_orig = jds+buf-1
  elsewhere (iy_orig > (jde-1-buf))
     dy_n_ad = 0_r_kind 
     dy_n = 1_r_kind
     iy_orig = jde-1-buf
  elsewhere
     y_ad = y_ad + dy_n_ad/meta%dy
     dy_n = mod(y,meta%dy)/meta%dy
  end where

  forall(i=1:Nx,j=1:Ny) 
     x_disp_ad(i,j) = x_disp_ad(i,j) + x_ad(i,j)
     y_disp_ad(i,j) = y_disp_ad(i,j) + y_ad(i,j)
  end forall

#ifdef TRACE_USE
  if (trace_use) call da_trace_exit("compute_xy_orig_ad")
#endif
end subroutine compute_xy_orig_ad

subroutine apply_disp_2d_ad(bg,disp,adj_ad,disp_ad, order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    apply_disp_2d_ad
!   prgmmr: Nehrkorn
!
! abstract: apply displacements to a gridded 2d field
!   NOTE: gridded fields assumed to be on the same grid as the displacements
!
! program history log:
!
!   input argument list: original field in bg, displacements in disp
!     bg
!     adj_ad
!     disp
!     disp_ad
!     order
!
!   output argument list: displaced field in adj
!     adj_ad
!     disp_ad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  real(r_kind), intent(in) :: bg(ims:ime,jms:jme) ! gridded 2d field: original
  real(r_kind), intent(inout) :: adj_ad(ims:ime,jms:jme) ! gridded 2d field: displaced
  type (fca_gridded_disp), intent(in) :: disp	! displacements
  type (fca_gridded_disp), intent(inout) ::  disp_ad	! displacements
  integer(i_kind), intent(in) :: order
  integer(i_kind) :: i, j, id, jd
  integer(i_kind) :: i1, i2, j1, j2

#ifdef TRACE_USE
  if (trace_use) call da_trace_entry("apply_disp_2d_ad")
#endif
  if (order .eq. bilinear) then
     ! apply displacements up to and including domain boundary
     j1=1
     j2=size(disp%ix_orig,2)
     i1=1
     i2=size(disp%ix_orig,1)
  elseif (order .eq. bicubic) then
     ! apply displacements one gridpoint in from domain boundary
     j1=max(jds+1,jts)-jts+1
     j2=min(jde-1,jte)-jts+1
     i1=max(ids+1,its)-its+1
     i2=min(ide-1,ite)-its+1
     if (i1 .gt. 1)         adj_ad(its,jts:jte) = 0._r_kind
     if (i2 .lt. ite-its+1) adj_ad(ite,jts:jte) = 0._r_kind
     if (j1 .gt. 1)         adj_ad(its:ite,jts) = 0._r_kind
     if (j2 .lt. jte-jts+1) adj_ad(its:ite,jte) = 0._r_kind
  end if

  do j = j1, j2
     jd = j+jts-1
     do i = i1, i2
        id = i+its-1
        if (order .eq. bilinear) then
	! save to adj after bilinear interpolation of displaced field
           call bilin_interp_ad(bg(disp%ix_orig(i,j),disp%iy_orig(i,j)),&
                bg(disp%ix_orig(i,j)+1,disp%iy_orig(i,j)),&
                bg(disp%ix_orig(i,j),disp%iy_orig(i,j)+1), &
                bg(disp%ix_orig(i,j)+1,disp%iy_orig(i,j)+1),&
                disp%dx_n(i,j),disp%dy_n(i,j),&
                adj_ad(id,jd),disp_ad%dx_n(i,j),disp_ad%dy_n(i,j))
        elseif (order .eq. bicubic) then
           call bicub_interp_ad(bg, disp%ix_orig(i,j), disp%iy_orig(i,j), &
                disp%dx_n(i,j),disp%dy_n(i,j),&
                adj_ad(id,jd),disp_ad%dx_n(i,j),disp_ad%dy_n(i,j))
        end if
	adj_ad(id,jd) = 0._r_kind
     end do !i
  end do !j
#ifdef TRACE_USE
  if (trace_use) call da_trace_exit("apply_disp_2d_ad")
#endif
end subroutine apply_disp_2d_ad

SUBROUTINE APPLY_VERT_AD(var, var_ad, stag, work2d, work2d_ad, fcadisp, fcadisp_ad, interp_order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    APPLY_VERT_AD
!   prgmmr: Nehrkorn
!
! abstract:
!
! program history log:
!
!   input argument list:
!     stag
!     var, var_ad
!
!   output argument list:
!     var, var_ad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  IMPLICIT NONE
  ! variable to displace
  INTEGER(I_KIND), INTENT(IN) :: stag
  real(r_kind), dimension(ims:ime,jms:jme,kms:kme), intent(inout) :: var, var_ad
  ! work space for 2d original/displaced fields
  real(r_kind), DIMENSION(ims:ime,jms:jme, 2), INTENT(INOUT) :: work2d
  real(r_kind), DIMENSION(ims:ime,jms:jme, 2) :: work2d_ad
  type (fca_gridded_disp), intent(in) :: fcadisp			! displacement field generated by FCA routine
  type (fca_gridded_disp), intent(inout) :: fcadisp_ad		! displacement field generated by FCA routine
  integer(i_kind), intent(in) :: interp_order
  INTEGER(I_KIND) :: i
  ! dimension along which var is staggered

#ifdef TRACE_USE
  if (trace_use) call da_trace_entry("apply_vert_ad")
#endif
  DO i=kts, kte+stag
     ! Loop through all vertical levels and apply displacements -- watch out for staggering
     !     Simplifications:
     ! zero out work2d_ad: (since it is really just local work space)
     ! also simply do a single loop through vertical levels, since there is no dependence vertically
     ! Only need undisplaced var, displaced vars not needed, so no recomputations are needed here
     ! Also: work2d_ad(:,:,1) not used anywhere:
     !       apply_displ_ad only input is work2d_ad(:,:,1), only output is fcadisp_ad
     work2d_ad(:, :, 1:2) = 0._r_kind
     ! vertical staggering is the same as no stagger since it adjust the number of vertical levels
     work2d_ad(:, :, 2) = work2d_ad(:, :, 2) + var_ad(:, :, i)
     var_ad(:,:,i) = 0._r_kind
     work2d(:, :, 1) = var(:, :, i)
     CALL APPLY_DISP_2D_AD(work2d(:, :, 1), fcadisp, work2d_ad(:, :, 2), fcadisp_ad, interp_order)
  END DO
#ifdef TRACE_USE
  if (trace_use) call da_trace_exit("apply_vert_ad")
#endif
END SUBROUTINE APPLY_VERT_AD

#ifdef FCA_REF_MOD
end module core_disp_func_ad_m
#endif
