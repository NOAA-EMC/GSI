#ifdef FCA_REF_MOD
module core_disp_func_tl_m
!$$$ module documentation block
!           .      .    .                                       .
! module:   core_disp_func_tl_m
!  prgmmr: Nehrkorn
!
! abstract: Core displacement functionality routines, tangent linear version
!
! program history log:
!
! subroutines included:
!   sub compute_xy_orig_tl
!   sub apply_disp_2d_tl
!   SUB APPLY_VERT_TL
!
! functions included:
!   bicub_interp_tl
!   cubicInterpolate_tl
!   bilin_interp_tl
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use fp_types_m, only: fp
  use core_disp_types_m, only: fca_gridded_meta, fca_gridded_disp, &
       ids,ide,jds,jde,kds,kde, ims,ime,jms,jme,kms,kme, &
       its,ite,jts,jte,kts,kte, ips,ipe,jps,jpe,kps,kpe, &
       bilinear, bicubic
  use core_disp_func_m, only : bicub_interp, bilin_interp, cubicInterpolate
  implicit none
  private
  public :: bicub_interp_tl, bilin_interp_tl, compute_xy_orig_tl, apply_disp_2d_tl, apply_vert_tl
  contains
#else
#define TRACE_USE
#endif

real(fp) function bicub_interp_tl(fin,i,j,dx,dy,dx_tl,dy_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bicub_interp_tl
!   prgmmr: Nehrkorn
!
! abstract:
!
! program history log:
!
!   input argument list:
!     fin, dx, dy
!     dx_tl, dy_tl
!     i, j
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  real(fp), intent(in) :: fin(ims:ime,jms:jme), dx, dy, dx_tl, dy_tl
  integer, intent(in) :: i, j

  real(fp) :: yy(-1:2), yy_tl(-1:2), f_tl(-1:2)
  integer :: jj

  f_tl(:) = 0.
  do jj=-1,2
     yy_tl(jj) = cubicInterpolate_tl(fin((i-1):(i+2),j+jj),dx,f_tl,dx_tl)
     yy(jj) = cubicInterpolate(fin((i-1):(i+2),j+jj),dx)
  end do
  bicub_interp_tl = cubicInterpolate_tl(yy,dy,yy_tl,dy_tl)

end function bicub_interp_tl

real(fp) function cubicInterpolate_tl(f,x,f_tl,x_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cubicInterpolate_tl
!   prgmmr: Nehrkorn
!
! abstract:
!
! program history log:
!
!   input argument list:
!     f
!     x
!     f_tl, x_tl
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  real(fp), intent(in) :: f(-1:2) !values at x=-1,0,1,2
  real(fp), intent(in) :: x ! value in [0,1]
  real(fp), intent(in) :: f_tl(-1:2), x_tl

  cubicInterpolate_tl = &
       x_tl     * 0.5 * (f(1) - f(-1)) + &
       x_tl*x         * (2.0*f(-1) - 5.0*f(0) + 4.0*f(1) - f(2)) + &
       x_tl*x*x * 1.5 * (3.0*(f(0) - f(1)) + f(2) - f(-1)) + &
       f_tl(0) + &
       x     * 0.5 * (f_tl(1) - f_tl(-1)) + &
       x*x   * 0.5 * (2.0*f_tl(-1) - 5.0*f_tl(0) + 4.0*f_tl(1) - f_tl(2)) + &
       x*x*x * 0.5 * (3.0*(f_tl(0) - f_tl(1)) + f_tl(2) - f_tl(-1))

end function cubicInterpolate_tl

real(fp) function bilin_interp_tl(f00, f10, f01, f11, dx, dy, dx_tl, dy_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bilin_interp_tl
!   prgmmr: Nehrkorn
!
! abstract: bilinear interpolation of 4 nearest neighbors to a point
!
! program history log:
!
!   input argument list:
!     f00, f10, f01, f11, dx, dy
!     dx_tl,dy_tl
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    ! bilinear interpolation of 4 nearest neighbors to a point
    implicit none
    ! bilinear interpolate in the unit box.
    real(fp), intent(in) :: f00, f10, f01, f11, dx, dy, dx_tl,dy_tl
    bilin_interp_tl = f00*(-dy_tl*(1.-dx)-dx_tl*(1.-dy)) &
         + f10*(dx_tl*(1.-dy)-dy_tl*dx) &
         + f01*(dy_tl*(1.-dx)-dx_tl*dy) &
         + f11*(dx_tl*dy+dx*dy_tl)
    return
end function bilin_interp_tl

subroutine compute_xy_orig_tl(x_disp, y_disp, ix_orig, iy_orig, dx_n, dy_n, meta, &
	   x_disp_tl, y_disp_tl, dx_n_tl, dy_n_tl, order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: compute_xy_orig_tl
!   prgmmr: Nehrkorn
!
! abstract: apply displacements to a gridded 2d field
!  NOTE: gridded fields assumed to be on the same grid as the displacements
!
! program history log:
!
!   input argument list: original field in bg, displacements in disp
!     x_disp, y_disp
!     x_disp_tl, y_disp_tl,  dx_n_tl, dy_n_tl, dx_n, dy_n
!     ix_orig, iy_orig
!     meta
!     order
!
!   output argument list: displaced field in adj
!     x_disp, y_disp
!     x_disp_tl, y_disp_tl,  dx_n_tl, dy_n_tl, dx_n, dy_n
!     ix_orig, iy_orig
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  real(fp), dimension(:,:), intent(inout) :: x_disp, y_disp, x_disp_tl, y_disp_tl, dx_n, dy_n, dx_n_tl, dy_n_tl
  integer, dimension(:,:), intent(inout) :: ix_orig, iy_orig
  type (fca_gridded_meta), intent(in) :: meta		! meta data
  integer, intent(in) :: order
  real(fp), dimension(size(x_disp,1),size(x_disp,2)) :: x, y, x_tl, y_tl
  integer :: i, j, status, buf, Nx, Ny

#ifdef TRACE_USE
  if (trace_use) call da_trace_entry("compute_xy_orig_tl")
#endif

  Nx = size(x_disp,1)
  Ny = size(x_disp,2)

  forall(i=1:Nx,j=1:Ny) 
     x_tl(i,j) = x_disp_tl(i,j)
     y_tl(i,j) = y_disp_tl(i,j)
     x(i,j) = its-1+(i-1)*meta%dx + x_disp(i,j) + meta%x0
     y(i,j) = jts-1+(j-1)*meta%dy + y_disp(i,j) + meta%y0
  end forall

  ! new array indices.
  ix_orig = floor(x/meta%dx)+1 ! x index of lower-left corner of containing value to bring back here
  iy_orig = floor(y/meta%dy)+1 ! y ""
  !	use constant extrapolation for points outside the grid:
  if (order .eq. bilinear) then
     buf=1
  elseif (order .eq. bicubic) then
     buf=2
  end if
  where (ix_orig < ids+buf-1)
     dx_n_tl = 0 
     dx_n = 0 
     ix_orig = ids+buf-1
  elsewhere (ix_orig > (ide-1-buf))
     dx_n_tl = 0 
     dx_n = 1
     ix_orig = ide-1-buf
  elsewhere
     ! x weight for interpolating values from box (i.e. 0.25 -> 25% from bottom, 75% from top points)
     dx_n_tl = x_tl/meta%dx
     dx_n = mod(x,meta%dx)/meta%dx
  end where

  where (iy_orig < jds+buf-1)
     dy_n_tl = 0 
     dy_n = 0
     iy_orig = jds+buf-1
  elsewhere (iy_orig > (jde-1-buf))
     dy_n_tl = 0 
     dy_n = 1
     iy_orig = jde-1-buf
  elsewhere
     dy_n_tl = y_tl/meta%dy
     dy_n = mod(y,meta%dy)/meta%dy
  end where
#ifdef TRACE_USE
  if (trace_use) call da_trace_exit("compute_xy_orig_tl")
#endif
end subroutine compute_xy_orig_tl

subroutine apply_disp_2d_tl(bg,adj,disp,adj_tl,disp_tl, order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    apply_disp_2d_tl
!   prgmmr: Nehrkorn
!
! abstract: apply displacements to a gridded 2d field
!   NOTE: gridded fields assumed to be on the same grid as the displacements
!
! program history log:
!
!   input argument list: original field in bg, displacements in disp
!     bg
!     disp
!     disp_tl
!     order
!
!   output argument list: displaced field in adj
!     adj, adj_tl
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none
    real(fp), intent(in) :: bg(ims:ime,jms:jme) ! gridded 2d field: original
    real(fp), intent(out) :: adj(ims:ime,jms:jme), adj_tl(ims:ime,jms:jme) ! gridded 2d field: displaced
    type (fca_gridded_disp), intent(in) :: disp, disp_tl	! displacements
    integer, intent(in) :: order
    integer :: i, j, id, jd
    integer :: i1, i2, j1, j2

#ifdef TRACE_USE
    if (trace_use) call da_trace_entry("apply_disp_2d_tl")
#endif
    ! Initialize displaced arrays so the halos are also initialized:
    adj(:,:) = bg(:,:)
    adj_tl(:,:) = 0.
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
       if (i1 .gt. 1)         adj_tl(its,jts:jte) = 0.
       if (i2 .lt. ite-its+1) adj_tl(ite,jts:jte) = 0.
       if (j1 .gt. 1)         adj_tl(its:ite,jts) = 0.
       if (j2 .lt. jte-jts+1) adj_tl(its:ite,jte) = 0.
    end if
    
    do j = j1, j2
       jd = j+jts-1
       do i = i1, i2
       	  id = i+its-1
          if (order .eq. bilinear) then
             adj_tl(id,jd) = &
                  bilin_interp_tl(&
                  bg(disp%ix_orig(i,j),disp%iy_orig(i,j)),&
                  bg(disp%ix_orig(i,j)+1,disp%iy_orig(i,j)),&
                  bg(disp%ix_orig(i,j),disp%iy_orig(i,j)+1), &
                  bg(disp%ix_orig(i,j)+1,disp%iy_orig(i,j)+1),&
                  disp%dx_n(i,j),disp%dy_n(i,j),disp_tl%dx_n(i,j),disp_tl%dy_n(i,j))
             adj(id,jd) = &
                  bilin_interp(&
                  bg(disp%ix_orig(i,j),disp%iy_orig(i,j)),&
                  bg(disp%ix_orig(i,j)+1,disp%iy_orig(i,j)),&
                  bg(disp%ix_orig(i,j),disp%iy_orig(i,j)+1), &
                  bg(disp%ix_orig(i,j)+1,disp%iy_orig(i,j)+1),&
                  disp%dx_n(i,j),disp%dy_n(i,j))
          elseif (order .eq. bicubic) then
             adj_tl(id,jd) = bicub_interp_tl(bg, disp%ix_orig(i,j), disp%iy_orig(i,j), &
                  disp%dx_n(i,j),disp%dy_n(i,j),disp_tl%dx_n(i,j),disp_tl%dy_n(i,j))
             adj(id,jd) = bicub_interp(bg, disp%ix_orig(i,j), disp%iy_orig(i,j), &
                  disp%dx_n(i,j),disp%dy_n(i,j))
          end if
       end do !i
    end do !j
#ifdef TRACE_USE
    if (trace_use) call da_trace_exit("apply_disp_2d_tl")
#endif
end subroutine apply_disp_2d_tl

SUBROUTINE APPLY_VERT_TL(var, var_tl, stag, work2d, work2d_tl, fcadisp, fcadisp_tl, interp_order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    APPLY_VERT_TL
!   prgmmr: Nehrkorn
!
! abstract:
!
! program history log:
!
!   input argument list:
!     stag
!     var, var_tl
!
!   output argument list:
!     var, var_tl
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    IMPLICIT NONE
! variable to displace
    INTEGER, INTENT(IN) :: stag
    real(fp), dimension(ims:ime,jms:jme,kms:kme), intent(inout) :: var, var_tl
    ! work space for 2d original/displaced fields
    REAL(fp), DIMENSION(ims:ime,jms:jme,2), INTENT(INOUT) :: work2d
    REAL(fp), DIMENSION(ims:ime,jms:jme,2), INTENT(INOUT) :: work2d_tl
    type (fca_gridded_disp), intent(in) :: fcadisp			! displacement field generated by FCA routine
    type (fca_gridded_disp), intent(in) :: fcadisp_tl			! displacement field generated by FCA routine
    integer, intent(in) :: interp_order
    INTEGER :: i
! dimension along which var is staggered
    INTRINSIC SIZE
#ifdef TRACE_USE
    if (trace_use) call da_trace_entry("apply_vert_tl")
#endif
    DO i=kts, kte + stag
! Loop through all vertical levels and apply displacements -- watch out for staggering
        work2d_tl(:, :, 1) = var_tl(:, :, i)
        work2d(:, :, 1) = var(:, :, i)
        CALL APPLY_DISP_2D_TL(work2d(:, :, 1), work2d(:, :, 2), fcadisp, &
                         work2d_tl(:, :, 2), fcadisp_tl, interp_order)
        var_tl(:, :, i) = work2d_tl(:, :, 2)
        var(:, :, i) = work2d(:, :, 2)
    END DO
#ifdef TRACE_USE
    if (trace_use) call da_trace_exit("apply_vert_tl")
#endif
END SUBROUTINE APPLY_VERT_TL


#ifdef FCA_REF_MOD
end module core_disp_func_tl_m
#endif
