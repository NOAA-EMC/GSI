module fca_xtofca_mod
!$$$ module documentation block
!           .      .    .                                       .
! module:   fca_xtofca_mod
!  prgmmr: Nehrkorn
!
! abstract: convert GSI bg, displacement vectors to gridded analysis increments
!
! program history log:
!
! subroutines included:
!   sub  xtofca
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

  use fp_types_m, only: fp
  use core_disp_types_m, only: fca_gridded_disp, &
       ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
  use core_disp_func_m, only: allocate_disp, deallocate_disp, init_meta
  use fca_wrf_grid_types_m, only: fca_wrf_grid
  use fca_wrf_grid_func_m, only: fca_allocate_wrf_grid, fca_deallocate_wrf_grid
  use xtofca_wrf_m, only: xtofca_wrf

  use fca_gsi_inter_m, only: ges_to_fca_wrf, idebug, sval_to_disp_grid, fca_state_to_sval, &
       uv_zlevel_par, fca_interp_order, th_compute_par, p_qv, nmoist

  use file_utility, only : get_lun
  use gsi_bundlemod, only: gsi_bundle
  use jfunc, only: jiter
  use mpimod, only: mype

  implicit none

  private

  public :: xtofca

contains

subroutine xtofca(sval, flag_linear)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: xtofca
!   prgmmr: Nehrkorn
!
! abstract: GSI-FCA interface routines: xfer of WRF model state between GSI and FCA
!    This is a GSI-specific wrapper for the generic (indendent of DA) xtofca_wrf, WRF-specific routine
!
! program history log:
!
!   input argument list: 
!    sval, flag_linear
!   output argument list:
!    sval
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    implicit none

    type(gsi_bundle) ,intent(inout) :: sval(:)
    logical, intent(in) :: flag_linear

    real(fp), parameter :: dx=1,dy=1,x0=0,y0=0			! grid spacing paramters, set manually for now
    type (fca_wrf_grid) :: bg_state, inc_state
    type (fca_gridded_disp) :: disp, disp_tl
    integer :: istep = 10000  ! only used for step-wise testing of fca_disp_tl/ad
    integer :: status, Nx, Ny, uv_zlevel

    integer :: i, j, unit_fcauv
    character (len=80) :: fname_fcauv

    real (fp) :: work2d(ims:ime,jms:jme,3)

    ! find the domain size
    Nx = ite - its + 1 ; Ny = jte - jts + 1
    uv_zlevel = min(kte,max(kts,int(kts+(kte-kts)*uv_zlevel_par+0.5)))

    !  copy the bg from GSI to our FCA type
    !  for DM_PARALLEL, any needed halo exchanges are done inside ges_to_fca_wrf
    call fca_allocate_wrf_grid(bg_state,nmoist,status)
    if (status .ne. 0) write(*,*) '*** xtofca: failed to allocate bg_state ***, status=',status
    call ges_to_fca_wrf(bg_state,status)
    if (status .ne. 0) write(*,*) '*** xtofca: ges_to_fca_wrf failed ***, status=',status
!     write(*,*) '*** xtofca debug: flag_linear, ges_to_fca_wrf status=',flag_linear, status

    !initialize all of the displaced fields
    disp%meta = init_meta(ite-its+1, jte-jts+1, dx, dy, x0, y0)
    ! initialize disp_tl with same dimensions and metadata
    call allocate_disp(disp,.TRUE.,status)
    if (status .ne. 0) write(*,*) '*** xtofca: failed to allocate disp ***, status=',status

    if (.not. flag_linear) then
       ! initialize the (nonlinear) displacements as the u,v components at the uv_zlevel:
       call sval_to_disp_grid(sval,disp,uv_zlevel,status)
       if (status .ne. 0) write(*,*) '*** xtofca: sval_to_disp_grid failed for disp ***, status=',status
       ! NOTE: disp_tl remains uninitialized, not used in xtofca_wrf if flag_linear=.FALSE.
       if (idebug .ge. 1) &
            write (*,'(a,1x,L1,2i4,2e15.6)') '*** FCA ***  flag_linear, Nx, Ny, meansq', flag_linear, Nx, Ny, &
            SUM(disp%x_disp(:,:)**2)/(Nx*Ny), SUM(disp%y_disp(:,:)**2)/(Nx*Ny)
       ! output nonlinear displacement vectors to file (filename specific to processor node and outer iteration number)
       ! We revert back to original WRF ordering here: disp_lon first, disp_lat second, indices for (lon,lat)
       write(fname_fcauv,'(a,i2.2,a,i4.4,a)') 'dispvect_',jiter,'_',mype,'.txt'
       unit_fcauv=get_lun()
       open(unit=unit_fcauv,file=trim(fname_fcauv),form='formatted')
       write (unit_fcauv,'(a,i2,a,i4,a)') 'Displacement vectors for outer iteration ',jiter,&
            ' for subdomain of node ',mype,' with subdomain i,j (in original WRF coordinates) grid indices:'
       write (unit_fcauv,'(4i10)') jts, jte, its, ite
       write (unit_fcauv,'(6e20.10)') ((disp%y_disp(i,j),j=1,Ny),i=1,Nx)
       write (unit_fcauv,'(6e20.10)') ((disp%x_disp(i,j),j=1,Ny),i=1,Nx)
       close(unit_fcauv)
    else
       ! initialize disp_tl with same dimensions and metadata, initialize displacements from increments
       disp_tl%meta = disp%meta
       call allocate_disp(disp_tl,.FALSE.,status)
       if (status .ne. 0) write(*,*) '*** xtofca: failed to allocate disp_tl ***, status=',status
       call sval_to_disp_grid(sval,disp_tl,uv_zlevel,status)
       if (status .ne. 0) write(*,*) '*** xtofca: sval_to_disp_grid failed for disp_tl ***, status=',status
       ! NOTE: disp is allocated, but values (zero displacements) will be assigned in xtofca_wrf if flag_linear=.TRUE.
       if (idebug .ge. 1) &
            write (*,'(a,1x,L1,2i4,2e15.6)') '*** FCA *** flag_linear, Nx, Ny, meansq', flag_linear, Nx, Ny, &
            SUM(disp_tl%x_disp(:,:)**2)/(Nx*Ny), SUM(disp_tl%y_disp(:,:)**2)/(Nx*Ny)
    end if
!     write(*,*) '*** xtofca debug: sval_to_disp_grid status=', status

    call xtofca_wrf(bg_state,disp,disp_tl,inc_state,flag_linear,p_qv,fca_interp_order,th_compute_par)

!     write(*,*) '*** xtofca debug: returned from xtofca_wrf'
    !Not done here: zero out sval first (WRFDA: call da_zero_x(grid%xa), handled inside state_to_sval
    !copy inc_state back to sval
    !  for DM_PARALLEL, any needed halo exchanges are done inside fca_state_to_sval
    call fca_state_to_sval(bg_state, inc_state, flag_linear, sval, status)
    if (status .ne. 0) write(*,*) '*** xtofca: fca_state_to_sval failed ***, status=',status
!     write(*,*) '*** xtofca debug: fca_state_to_sval', status

    ! deallocate: disp, disp_tl, bg_state, inc_state
    call deallocate_disp(disp,status)
    if (status .ne. 0) write(*,*) '*** xtofca: failed to deallocate disp ***, status=',status
!     write(*,*) '*** xtofca debug: deallocate disp', status
    if (flag_linear) then
       call deallocate_disp(disp_tl,status)
       if (status .ne. 0) write(*,*) '*** xtofca: failed to deallocate disp_tl ***, status=',status
!        write(*,*) '*** xtofca debug: deallocate disp_tl', status
    end if
    call fca_deallocate_wrf_grid(bg_state,status)
    if (status .ne. 0) write(*,*) '*** xtofca: failed to deallocate bg_state ***, status=',status
!     write(*,*) '*** xtofca debug: deallocate bg_state', status
    call fca_deallocate_wrf_grid(inc_state,status)
    if (status .ne. 0) write(*,*) '*** xtofca: failed to deallocate inc_state ***, status=',status
!     write(*,*) '*** xtofca debug: deallocate inc_state', status

end subroutine xtofca

end module fca_xtofca_mod
