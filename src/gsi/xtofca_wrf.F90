#ifdef FCA_REF_MOD
module xtofca_wrf_m
!$$$ module documentation block
!           .      .    .                                       .
! module:   xtofca_wrf_m
!  prgmmr: Nehrkorn
!
! abstract:  convert displacement vectors (stored in disp, disp_tl) to gridded analysis increments
!
! program history log:
!
! subroutines included:
!   sub  xtofca_wrf
!
! functions included:
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind, i_kind
  use core_disp_types_m, only: fca_gridded_disp, &
       its,ite,jts,jte,kts,kte
  use core_disp_func_m, only: init_meta, compute_xy_orig
#ifdef DM_PARALLEL
  use core_disp_func_m, only : compute_xy_orig_dm
#endif
  use core_disp_func_tl_m, only: compute_xy_orig_tl

  use fca_wrf_grid_types_m, only: fca_wrf_grid
  use fca_wrf_grid_func_m, only: diff_wrf_state, fca_zero_wrf_grid, fca_copy_wrf_grid, &
       fca_allocate_wrf_grid, fca_deallocate_wrf_grid
  use displace_wrf_m, only: displace_wrf_fields
  use displace_wrf_tl_m, only: displace_wrf_fields_tl
  implicit none
  private
  public :: xtofca_wrf
  contains
#else
#define TRACE_USE
#endif

subroutine xtofca_wrf(bg_state,disp,disp_tl,inc_state,flag_linear,p_qv,fca_interp_order,th_compute_par)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: xtofca_wrf
!   prgmmr: Nehrkorn
!
! abstract: convert displacement vectors (stored in disp, disp_tl) to gridded analysis increments
!    This routine specific to WRF-ARW, but independent of DA infrastructure (WRFDA, GSI, ...)
!
! program history log:
!
!   input argument list: 
!    bg_state
!    disp
!    disp_tl
!    flag_linear
!    p_qv
!    fca_interp_order
!    th_compute_par
!
!   output argument list:
!    bg_state
!    disp
!    disp_tl
!    inc_state
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  type (fca_gridded_disp), intent(inout) :: disp, disp_tl
  logical, intent(in) :: flag_linear !.TRUE. for TLM, .FALSE. for NLM
  integer(i_kind), intent(in) :: p_qv ! index for qv in moist array
  integer(i_kind), intent(in) :: fca_interp_order ! 1 for bilinear, 3 for bicubic
  integer(i_kind), intent(in) :: th_compute_par ! 0 = displace theta, 1 = displace N2

  type (fca_wrf_grid), intent(inout) :: bg_state !really just in for TLM, displaced full_state for NLM
  type (fca_wrf_grid), intent(out) :: inc_state

  type (fca_wrf_grid) :: wrf_state_saved
  integer(i_kind) :: istep = 10000  ! only used for step-wise testing of fca_disp_tl/ad

  ! Arrays allocated in compute_xy_orig_dm (and routines called by it):
  integer(i_kind), allocatable :: needed_ij_in(:,:), needed_ij_out(:,:)
  integer(i_kind), allocatable :: index_in(:)
  real(r_kind), allocatable :: needed_wgts_out(:,:)
  integer(i_kind) :: num_needed_out, num_glob_needed

  integer(i_kind) :: num_needed_in

  integer(i_kind) :: i, status
  
#ifdef TRACE_USE
  if (trace_use) call da_trace_entry("xtofca_wrf")
#endif

  call fca_copy_wrf_grid(bg_state, wrf_state_saved, status)
  if (status /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** xtofca_wrf: failed to allocate wrf_state_saved ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if
    if (.not. flag_linear) then
       ! compute the origins
#ifdef DM_PARALLEL
       call compute_xy_orig_dm(disp%x_disp, disp%y_disp, disp%ix_orig, disp%iy_orig, &
            disp%dx_n, disp%dy_n, disp%meta, &
            needed_ij_in, index_in, needed_ij_out, needed_wgts_out, num_glob_needed, fca_interp_order)
#else
       num_needed_in=0
       num_glob_needed=0
       allocate(needed_ij_in(4,num_needed_in), needed_ij_out(4,num_needed_in), &
            index_in(num_needed_in), needed_wgts_out(2,num_needed_in))
       call compute_xy_orig(disp%x_disp, disp%y_disp, disp%ix_orig, disp%iy_orig, disp%dx_n, disp%dy_n, disp%meta, fca_interp_order)
#endif

       call displace_wrf_fields(th_compute_par, istep, &
            p_qv, bg_state%moist, size(bg_state%moist,4), bg_state%p, bg_state%pb, bg_state%t, &
            bg_state%ph, bg_state%phb, bg_state%u, bg_state%v, bg_state%w, &
            bg_state%hgt, bg_state%mub, bg_state%mu, bg_state%psfc, &
            bg_state%znu, bg_state%znw, bg_state%c1h, bg_state%c2h, &
            bg_state%c3h, bg_state%c4h, bg_state%c3f, bg_state%c4f, bg_state%ptop, disp,&
            needed_ij_in, needed_ij_out, index_in, needed_wgts_out, num_glob_needed, fca_interp_order)

       ! initialize inc_state: differences between displaced and saved wrf_state
       call diff_wrf_state(wrf_state_saved, bg_state, inc_state, status)
       if (status /= 0) then
#ifndef FCA_REF_MOD
          write(unit=message(1),fmt='(A)') '*** xtofca_wrf: error in diff_wrf_state ***'
          call da_error(__FILE__,__LINE__,message(1:1))
#endif
       endif
       
       deallocate(needed_ij_in, needed_ij_out, &
            index_in, needed_wgts_out)
    else
       ! first allocate inc_state:
       call fca_allocate_wrf_grid(inc_state, size(bg_state%moist,dim=4),status)
       if (status /= 0) then
#ifndef FCA_REF_MOD
          write(unit=message(1),fmt='(A)') '*** xtofca_wrf: failed to allocate inc_state ***'
          call da_error(__FILE__,__LINE__,message(1:1))
#endif
       end if
       ! zero inc_state
       call fca_zero_wrf_grid(inc_state)

       ! compute the origins (NLM, all zero disp)
       ! NOTE: because all disp are zero, we do not need to use compute_xy_orig_dm or its associated arrays
       disp%x_disp(:,:) = 0_r_kind
       disp%y_disp(:,:) = 0_r_kind
       call compute_xy_orig(disp%x_disp, disp%y_disp, disp%ix_orig, disp%iy_orig, disp%dx_n, disp%dy_n, disp%meta, fca_interp_order)
       
       ! compute the origins (TLM, from disp_tl)
       call compute_xy_orig_tl(disp%x_disp, disp%y_disp, disp%ix_orig, disp%iy_orig, disp%dx_n, disp%dy_n, disp%meta, &
	   disp_tl%x_disp, disp_tl%y_disp, disp_tl%dx_n, disp_tl%dy_n, fca_interp_order)

       call displace_wrf_fields_tl(th_compute_par, istep, &
            p_qv, wrf_state_saved%moist, size(inc_state%moist,4), inc_state%moist, wrf_state_saved%p, inc_state%p, wrf_state_saved%pb, &
	    wrf_state_saved%t, inc_state%t, wrf_state_saved%ph, inc_state%ph, wrf_state_saved%phb, &
	    wrf_state_saved%u, inc_state%u, wrf_state_saved%v, inc_state%v, wrf_state_saved%w, inc_state%w, &
            wrf_state_saved%hgt, wrf_state_saved%mub, wrf_state_saved%mu, inc_state%mu, wrf_state_saved%psfc, &
            inc_state%psfc, wrf_state_saved%znu, wrf_state_saved%znw, wrf_state_saved%c1h, wrf_state_saved%c2h, &
            wrf_state_saved%c3h, wrf_state_saved%c4h, wrf_state_saved%c3f, wrf_state_saved%c4f, wrf_state_saved%ptop, disp, disp_tl, fca_interp_order)

    end if

    call fca_deallocate_wrf_grid(wrf_state_saved, status)
    if (status /= 0) then
#ifndef FCA_REF_MOD
       write(unit=message(1),fmt='(A)') '*** xtofca_wrf: failed to deallocate wrf_state_saved ***'
       call da_error(__FILE__,__LINE__,message(1:1))
#endif
    end if
#ifdef TRACE_USE
    if (trace_use) call da_trace_exit("xtofca_wrf")
#endif

end subroutine xtofca_wrf
  
#ifdef FCA_REF_MOD
end module xtofca_wrf_m
#endif
