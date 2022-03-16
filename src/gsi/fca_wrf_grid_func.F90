#ifdef FCA_REF_MOD
module fca_wrf_grid_func_m
!$$$ module documentation block
!           .      .    .                                       .
! module:   fca_wrf_grid_func_m
!  prgmmr: Nehrkorn
!
! abstract: Routines that operate on the fca_wrf_grid_types derived type
!
! program history log:
!
! subroutines included:
!   sub  diff_wrf_state
!   sub  fca_zero_wrf_grid
!   sub  fca_copy_wrf_grid
!   sub  fca_allocate_wrf_grid
!   sub  fca_deallocate_wrf_grid
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
  use core_disp_types_m, only: ims, ime, jms, jme, kms, kme
  use fca_wrf_grid_types_m, only: fca_wrf_grid
  implicit none
  private
  public :: diff_wrf_state, fca_zero_wrf_grid, fca_copy_wrf_grid, fca_allocate_wrf_grid, &
       fca_deallocate_wrf_grid
  contains
#else
#define TRACE_USE
#endif

subroutine diff_wrf_state(old,new,diff,ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: diff_wrf_state
!   prgmmr: Nehrkorn
!
! abstract: Compute the difference between two wrf_states
!    Assumes that all array dimensions (in old, new) are compatible)
!    diff is initialized with all fields from old, and then diffs are computed
!    for selected fields, as diff = new - old
!
! program history log:
!
!   input argument list: 
!    old,new
!   output argument list:
!    diff,ierror
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    type (fca_wrf_grid), intent(in) :: old, new
    type (fca_wrf_grid), intent(out) :: diff
    integer(i_kind), intent(out) :: ierror

    call fca_copy_wrf_grid(old,diff,ierror)
    if (ierror /= 0) return
    diff%MOIST = new%MOIST - old%MOIST
    diff%MU = new%MU - old%MU
    diff%P = new%P - old%P
    diff%PH = new%PH - old%PH
    diff%PSFC = new%PSFC - old%PSFC
    diff%T = new%T - old%T
    diff%U = new%U - old%U
    diff%V = new%V - old%V
    diff%W = new%W - old%W
    return

end subroutine diff_wrf_state

subroutine fca_zero_wrf_grid(wrf_grid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: fca_zero_wrf_grid
!   prgmmr: Nehrkorn
!
! abstract:  zero out the contents of the wrf model state
!    assumes all arrays are already allocated
!
! program history log:
!
!   input argument list: 
!    wrf_grid
!   output argument list:
!    wrf_grid
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none
    type (fca_wrf_grid), intent(inout) :: wrf_grid
    real(r_kind) :: zero=0._r_kind
    wrf_grid%P=zero
    wrf_grid%PB=zero
    wrf_grid%PH=zero
    wrf_grid%PHB=zero
    wrf_grid%T=zero
    wrf_grid%U=zero
    wrf_grid%V=zero
    wrf_grid%W=zero
    wrf_grid%MU=zero
    wrf_grid%MUB=zero
    wrf_grid%HGT=zero
    wrf_grid%PSFC=zero
    wrf_grid%PH_NL=zero
    wrf_grid%ZNU=zero
    wrf_grid%ZNW=zero
    wrf_grid%C1H=zero
    wrf_grid%C2H=zero
    wrf_grid%C3H=zero
    wrf_grid%C4H=zero
    wrf_grid%C3F=zero
    wrf_grid%C4F=zero
    wrf_grid%MOIST=zero
end subroutine fca_zero_wrf_grid

subroutine fca_copy_wrf_grid(wrf_grid_from,wrf_grid_to,ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: fca_copy_wrf_grid
!   prgmmr: Nehrkorn
!
! abstract: copy the contents of the wrf model state, allocate arrays in target if needed
!    will allocate arrays in wrf_grid_to as needed
!
! program history log:
!
!   input argument list: 
!    wrf_grid_from,wrf_grid_to
!   output argument list:
!    wrf_grid_to,ierror
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  type (fca_wrf_grid), intent(in) :: wrf_grid_from
  type (fca_wrf_grid), intent(inout) :: wrf_grid_to
  integer(i_kind), intent(out) :: ierror
  real(r_kind) :: zero=0._r_kind
  integer(i_kind) :: status

#ifdef TRACE_USE
  if (trace_use) call da_trace_entry("fca_copy_wrf_grid")
#endif
  wrf_grid_to%ptop = wrf_grid_from%ptop
  ierror = 1 ! "eta values and c1,c2,c3,c4"
  if(.not.allocated(wrf_grid_to%ZNU)) then
     allocate(wrf_grid_to%ZNU(kms:kme),wrf_grid_to%ZNW(kms:kme), &
          wrf_grid_to%C1H(kms:kme),wrf_grid_to%C2H(kms:kme), &
          wrf_grid_to%C3H(kms:kme),wrf_grid_to%C4H(kms:kme), &
          wrf_grid_to%C3F(kms:kme),wrf_grid_to%C4F(kms:kme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%ZNU(:) = wrf_grid_from%ZNU(:)
  wrf_grid_to%ZNW(:) = wrf_grid_from%ZNW(:)
  wrf_grid_to%C1H(:) = wrf_grid_from%C1H(:)
  wrf_grid_to%C2H(:) = wrf_grid_from%C2H(:)
  wrf_grid_to%C3H(:) = wrf_grid_from%C3H(:)
  wrf_grid_to%C4H(:) = wrf_grid_from%C4H(:)
  wrf_grid_to%C3F(:) = wrf_grid_from%C3F(:)
  wrf_grid_to%C4F(:) = wrf_grid_from%C4F(:)

  ierror = ierror + 1 ! 2: surface topography /terrain height
  if(.not.allocated(wrf_grid_to%HGT)) then
     allocate(wrf_grid_to%HGT(ims:ime,jms:jme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%HGT(:,:) = wrf_grid_from%HGT(:,:)

  ierror = ierror + 1 ! 3: mass field
  if(.not.allocated(wrf_grid_to%MUB)) then
     allocate(wrf_grid_to%MUB(ims:ime,jms:jme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%MUB(:,:) = wrf_grid_from%MUB(:,:)
  if(.not.allocated(wrf_grid_to%MU)) then
     allocate(wrf_grid_to%MU(ims:ime,jms:jme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%MU(:,:) = wrf_grid_from%MU(:,:)

  ierror = ierror + 1 ! 4: surface pressure
  if(.not.allocated(wrf_grid_to%PSFC)) then
     allocate(wrf_grid_to%PSFC(ims:ime,jms:jme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%PSFC(:,:) = wrf_grid_from%PSFC(:,:)

  ierror = ierror + 1 ! 5: base pressure
  if(.not.allocated(wrf_grid_to%PB)) then
     allocate(wrf_grid_to%PB(ims:ime,jms:jme,kms:kme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%PB(:,:,:) = wrf_grid_from%PB(:,:,:)

  ierror = ierror + 1 ! 6: perturbation pressure
  if(.not.allocated(wrf_grid_to%P)) then
     allocate(wrf_grid_to%P(ims:ime,jms:jme,kms:kme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%P(:,:,:) = wrf_grid_from%P(:,:,:)

  ierror = ierror + 1 ! 7: base geopotential
  if(.not.allocated(wrf_grid_to%PHB)) then
     allocate(wrf_grid_to%PHB(ims:ime,jms:jme,kms:kme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%PHB(:,:,:) = wrf_grid_from%PHB(:,:,:)

  ierror = ierror + 1 ! 8: perturbation geoptoential
  if(.not.allocated(wrf_grid_to%PH)) then
     allocate(wrf_grid_to%PH(ims:ime,jms:jme,kms:kme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%PH(:,:,:) = wrf_grid_from%PH(:,:,:)

  ierror = ierror + 1 ! 9: non-linear geopotential
  if(.not.allocated(wrf_grid_to%PH_NL)) then
     allocate(wrf_grid_to%PH_NL(ims:ime,jms:jme,kms:kme),STAT=status)
     if (status /= 0) return
  endif

  ierror = ierror + 1 ! 10: potential temperature
  if(.not.allocated(wrf_grid_to%T)) then
     allocate(wrf_grid_to%T(ims:ime,jms:jme,kms:kme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%T(:,:,:) = wrf_grid_from%T(:,:,:)

  ierror = ierror + 1 ! 11: wind components
  if(.not.allocated(wrf_grid_to%U)) then
     allocate(wrf_grid_to%U(ims:ime,jms:jme,kms:kme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%U(:,:,:) = wrf_grid_from%U(:,:,:)
  if(.not.allocated(wrf_grid_to%V)) then
     allocate(wrf_grid_to%V(ims:ime,jms:jme,kms:kme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%V(:,:,:) = wrf_grid_from%V(:,:,:)
  if(.not.allocated(wrf_grid_to%W)) then
     allocate(wrf_grid_to%W(ims:ime,jms:jme,kms:kme),STAT=status)
     if (status /= 0) return
  endif
  wrf_grid_to%W(:,:,:) = wrf_grid_from%W(:,:,:)

  ierror = ierror + 1 ! 12: Now try to handle the hydrometeors
  if(.not.allocated(wrf_grid_to%MOIST)) then
     allocate(wrf_grid_to%MOIST(ims:ime,jms:jme,kms:kme,size(wrf_grid_from%moist,dim=4)),stat=status)
     if (status /= 0) return
  end if
  wrf_grid_to%MOIST(:,:,:,:) = wrf_grid_from%MOIST(:,:,:,:)

  ierror=0 !successful return

#ifdef TRACE_USE
  if (trace_use) call da_trace_exit("fca_copy_wrf_grid")
#endif

end subroutine fca_copy_wrf_grid

subroutine fca_allocate_wrf_grid(wrf_grid_to,nmoist,ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: fca_allocate_wrf_grid
!   prgmmr: Nehrkorn
!
! abstract:   ! allocate arrays in wrf_grid (will fail if they are already allocated)
!    will allocate arrays in wrf_grid_to as needed
!
! program history log:
!
!   input argument list: 
!    wrf_grid_to,nmoist
!   output argument list:
!    wrf_grid_to,ierror
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  type (fca_wrf_grid), intent(inout) :: wrf_grid_to
  integer(i_kind), intent(in) :: nmoist
  integer(i_kind), intent(out) :: ierror
  real(r_kind) :: zero=0._r_kind
  integer(i_kind) :: status

#ifdef TRACE_USE
  if (trace_use) call da_trace_entry("fca_allocate_wrf_grid")
#endif
  ierror = 1 ! "eta values and c1,c2,c3,c4" 
  allocate(wrf_grid_to%ZNU(kms:kme),wrf_grid_to%ZNW(kms:kme), &
       wrf_grid_to%C1H(kms:kme),wrf_grid_to%C2H(kms:kme), &
       wrf_grid_to%C3H(kms:kme),wrf_grid_to%C4H(kms:kme), &
       wrf_grid_to%C3F(kms:kme),wrf_grid_to%C4F(kms:kme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 2: surface topography /terrain height
  allocate(wrf_grid_to%HGT(ims:ime,jms:jme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 3: mass field
  allocate(wrf_grid_to%MUB(ims:ime,jms:jme),STAT=status)
  if (status /= 0) return
  allocate(wrf_grid_to%MU(ims:ime,jms:jme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 4: surface pressure
  allocate(wrf_grid_to%PSFC(ims:ime,jms:jme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 5: base pressure
  allocate(wrf_grid_to%PB(ims:ime,jms:jme,kms:kme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 6: perturbation pressure
  allocate(wrf_grid_to%P(ims:ime,jms:jme,kms:kme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 7: base geopotential
  allocate(wrf_grid_to%PHB(ims:ime,jms:jme,kms:kme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 8: perturbation geoptoential
  allocate(wrf_grid_to%PH(ims:ime,jms:jme,kms:kme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 9: non-linear geopotential
  allocate(wrf_grid_to%PH_NL(ims:ime,jms:jme,kms:kme),STAT=status)
  if (status /= 0) return
  ierror = ierror + 1 ! 10: potential temperature
  allocate(wrf_grid_to%T(ims:ime,jms:jme,kms:kme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 11: wind components
  allocate(wrf_grid_to%U(ims:ime,jms:jme,kms:kme),STAT=status)
  if (status /= 0) return
  allocate(wrf_grid_to%V(ims:ime,jms:jme,kms:kme),STAT=status)
  if (status /= 0) return
  allocate(wrf_grid_to%W(ims:ime,jms:jme,kms:kme),STAT=status)
  if (status /= 0) return

  ierror = ierror + 1 ! 12: Now try to handle the hydrometeors
  allocate(wrf_grid_to%MOIST(ims:ime,jms:jme,kms:kme,nmoist),stat=status)
  if (status /= 0) return

  ierror=0 !successful return

#ifdef TRACE_USE
  if (trace_use) call da_trace_exit("fca_allocate_wrf_grid")
#endif

end subroutine fca_allocate_wrf_grid

subroutine fca_deallocate_wrf_grid(wrf_grid,ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: fca_deallocate_wrf_grid
!   prgmmr: Nehrkorn
!
! abstract: deallocate the contents of the wrf model state
!
! program history log:
!
!   input argument list: 
!    wrf_grid
!   output argument list:
!    wrf_grid,ierror
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    type (fca_wrf_grid), intent(inout) :: wrf_grid
    integer(i_kind), intent(out) :: ierror

    integer(i_kind) :: status = 0

#ifdef TRACE_USE
    if (trace_use) call da_trace_entry("fca_deallocate_wrf_grid")
#endif
    ierror = 1 ! "eta values on half (mass) levels" 
    if(allocated(wrf_grid%ZNU)) then
       deallocate(wrf_grid%ZNU,wrf_grid%ZNW, wrf_grid%C1H,wrf_grid%C2H, &
            wrf_grid%C3H,wrf_grid%C4H, wrf_grid%C3F,wrf_grid%C4F,STAT=status)
       if (status /= 0) return
    end if

    ierror = ierror + 1 ! 2: surface topography /terrain height
    if(allocated(wrf_grid%HGT)) then
        deallocate(wrf_grid%HGT,stat=status)
    	if (status /= 0) return
    end if

    ierror = ierror + 1 ! 3: mass field
    if(allocated(wrf_grid%MUB)) then
        deallocate(wrf_grid%MUB,stat=status)
    	if (status /= 0) return
    end if
    if(allocated(wrf_grid%MU)) then
        deallocate(wrf_grid%MU,stat=status)
        if (status /= 0) return
    end if

    ierror = ierror + 1 ! 4: surface pressure
    if(allocated(wrf_grid%PSFC)) then
        deallocate(wrf_grid%PSFC,stat=status)
    	if (status /= 0) return
    end if

    ierror = ierror + 1 ! 5: base pressure
    if(allocated(wrf_grid%PB)) then
        deallocate(wrf_grid%PB,stat=status)
	if (status /= 0) return
    end if

    ierror = ierror + 1 ! 6: perturbation pressure
    if(allocated(wrf_grid%P)) then
        deallocate(wrf_grid%P,stat=status)
    	if (status /= 0) return
    end if
    
    ierror = ierror + 1 ! 7: base geopotential
    if(allocated(wrf_grid%PHB)) then
        deallocate(wrf_grid%PHB,stat=status)
    	if (status /= 0) return
    end if

    ierror = ierror + 1 ! 8: perturbation geoptoential
    if(allocated(wrf_grid%PH)) then
        deallocate(wrf_grid%PH,stat=status)
    	if (status /= 0) return
    end if

    ierror = ierror + 1 ! 9: non-linear geopotential
    if(allocated(wrf_grid%PH_NL)) then
        deallocate(wrf_grid%PH_NL,stat=status)
    	if (status /= 0) return
    end if

    ierror = ierror + 1 ! 10: potential temperature
    if(allocated(wrf_grid%T)) then
        deallocate(wrf_grid%T,stat=status)
    	if (status /= 0) return
    end if

    ierror = ierror + 1 ! 11: wind components
    if(allocated(wrf_grid%U)) then
        deallocate(wrf_grid%U,stat=status)
    	if (status /= 0) return
    end if
    if(allocated(wrf_grid%V)) then
        deallocate(wrf_grid%V,stat=status)
    	if (status /= 0) return
    end if
    if(allocated(wrf_grid%W)) then
        deallocate(wrf_grid%W,stat=status)
    	if (status /= 0) return
    end if

    ierror = ierror + 1 ! 12: Now try to handle the hydrometeors
    if(allocated(wrf_grid%MOIST)) then
        deallocate(wrf_grid%MOIST,stat=status)
    	if (status /= 0) return
    end if

    ierror=0 !successful return
    
#ifdef TRACE_USE
    if (trace_use) call da_trace_exit("fca_deallocate_wrf_grid")
#endif
end subroutine fca_deallocate_wrf_grid

#ifdef FCA_REF_MOD
end module fca_wrf_grid_func_m
#endif


