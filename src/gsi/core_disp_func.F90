#ifdef FCA_REF_MOD
module core_disp_func_m
!$$$ module documentation block
!           .      .    .                                       .
! module:   core_disp_func_m
!  prgmmr: Nehrkorn
!
! abstract: Core displacement functionality routines, nonlinear (NLM) version
!
! program history log:
!
! subroutines included:
!   sub compute_xy_orig
!   sub compute_xy_orig_dm
!   sub exchange_needed
!   sub exchange_interpolated
!   sub apply_disp_2d
!   SUB APPLY_VERT
!   sub allocate_disp
!   sub deallocate_disp
!
! functions included:
!   bicub_interp
!   cubicInterpolate
!   bilin_interp
!   init_meta
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
  implicit none
  private
  public :: bilin_interp, bicub_interp, cubicInterpolate, compute_xy_orig, apply_disp_2d, apply_vert
  public :: init_meta, allocate_disp, deallocate_disp
#ifdef DM_PARALLEL
  public :: compute_xy_orig_dm
#endif
  contains
#else
#define TRACE_USE
#endif

real(r_kind) function bicub_interp(fin,i,j,dx,dy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bicub_interp
!   prgmmr: Nehrkorn
!
! abstract:
!
! program history log:
!
!   input argument list:
!     fin, dx, dy
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
  real(r_kind), intent(in) :: fin(ims:ime,jms:jme), dx, dy
  integer(i_kind), intent(in) :: i, j

  real(r_kind) :: yy(-1:2)
  integer(i_kind) :: jj

  do jj=-1,2
     yy(jj) = cubicInterpolate(fin((i-1):(i+2),j+jj),dx)
  end do
  bicub_interp = cubicInterpolate(yy,dy)
end function bicub_interp

real(r_kind) function cubicInterpolate(f,x)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cubicInterpolate
!   prgmmr: Nehrkorn
!
! abstract:
!
! program history log:
!
!   input argument list:
!     f
!     x
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  real(r_kind), intent(in) :: f(-1:2) !values at x=-1,0,1,2
  real(r_kind), intent(in) :: x ! value in [0,1]

  cubicInterpolate = f(0) + &
       x     * 0.5_r_kind * (f(1) - f(-1)) + &
       x*x   * 0.5_r_kind * (2.0_r_kind*f(-1) - 5.0_r_kind*f(0) + 4.0_r_kind*f(1) - f(2)) + &
       x*x*x * 0.5_r_kind * (3.0_r_kind*(f(0) - f(1)) + f(2) - f(-1))

end function cubicInterpolate

real(r_kind) function bilin_interp(f00, f10, f01, f11, dx, dy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bilin_interp
!   prgmmr: Nehrkorn
!
! abstract: bilinear interpolation of 4 nearest neighbors to a point
!
! program history log:
!
!   input argument list:
!     f00, f10, f01, f11, dx, dy
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
    real(r_kind), intent(in) :: f00, f10, f01, f11, dx, dy
    bilin_interp = f00*((1._r_kind-dx)*(1._r_kind-dy)) + f10*(dx*(1._r_kind-dy)) + f01*((1._r_kind-dx)*dy) + f11*(dx*dy)
    return
end function bilin_interp

subroutine compute_xy_orig(x_disp, y_disp, ix_orig, iy_orig, dx_n, dy_n, meta, order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: compute_xy_orig
!   prgmmr: Nehrkorn
!
! abstract: apply displacements to a gridded 2d field
!  NOTE: gridded fields assumed to be on the same grid as the displacements
!
! program history log:
!
!   input argument list: original field in bg, displacements in disp
!     x_disp, y_disp,  dx_n, dy_n
!     ix_orig, iy_orig
!     meta
!     order
!
!   output argument list: displaced field in adj
!     x_disp, y_disp,  dx_n, dy_n, dx_n, dy_n
!     ix_orig, iy_orig
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  real(r_kind), dimension(:,:), intent(inout) :: x_disp, y_disp, dx_n, dy_n
  integer(i_kind), dimension(:,:), intent(inout) :: ix_orig, iy_orig
  type (fca_gridded_meta), intent(in) :: meta		! meta data
  integer(i_kind), intent(in) :: order

  real(r_kind), dimension(size(x_disp,1),size(x_disp,2)) :: x, y
  integer(i_kind) :: i, j, status, buf, Nx, Ny

#ifdef TRACE_USE
  if (trace_use) call da_trace_entry("compute_xy_orig")
#endif

  Nx = size(x_disp,1)
  Ny = size(x_disp,2)
  forall(i=1:Nx,j=1:Ny) 
     x(i,j) = its-1+(i-1)*meta%dx + x_disp(i,j) + meta%x0
     y(i,j) = jts-1+(j-1)*meta%dy + y_disp(i,j) + meta%y0
  end forall

  ! new array indices.

  ix_orig(1:Nx,1:Ny) = int(floor(x(1:Nx,1:Ny)/meta%dx)+1) ! x index of lower-left corner of containing value to bring back here
  iy_orig(1:Nx,1:Ny) = int(floor(y(1:Nx,1:Ny)/meta%dy)+1) ! y ""

  ! use constant extrapolation for points outside the grid: 
  ! (Note that since ide/jde is padded by one for staggered grids, we need to use ide-1 in the comparison below)

  if (order == bilinear) then
     buf=1
  elseif (order == bicubic) then
     buf=2
  end if
  where(ix_orig < ids+buf-1)
     dx_n = 0_r_kind
     ix_orig = ids+buf-1
  elsewhere (ix_orig > (ide-1-buf))
     dx_n = 1_r_kind
     ix_orig = ide-1-buf
  elsewhere
     ! x weight for interpolating values from box (i.e. 0.25 -> 25% from bottom, 75% from top points)
     dx_n = mod(x,meta%dx)/meta%dx
  end where

  where(iy_orig < jds+buf-1)
     dy_n = 0_r_kind
     iy_orig = jds+buf-1
  elsewhere (iy_orig > (jde-1-buf))
     dy_n = 1_r_kind
     iy_orig = jde-1-buf
  elsewhere
     dy_n = mod(y,meta%dy)/meta%dy
  end where
#ifdef TRACE_USE
  if (trace_use) call da_trace_exit("compute_xy_orig")
#endif
end subroutine compute_xy_orig

#ifdef DM_PARALLEL
subroutine compute_xy_orig_dm(x_disp, y_disp, ix_orig, iy_orig, dx_n, dy_n, meta, &
     needed_ij_in, index_in, needed_ij_out, needed_wgts_out, num_glob_needed, order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: compute_xy_orig_dm
!   prgmmr: Nehrkorn
!
! abstract: apply displacements to a gridded 2d field
!  NOTE: gridded fields assumed to be on the same grid as the displacements
!
! program history log:
!
!   input argument list: original field in bg, displacements in disp
!     x_disp, y_disp,  dx_n, dy_n
!     ix_orig, iy_orig
!     meta
!     order
!
!   output argument list: displaced field in adj
!     x_disp, y_disp,  dx_n, dy_n, dx_n, dy_n
!     ix_orig, iy_orig
!     needed_ij_in, needed_ij_out, index_in
!     needed_wgts_out
!     num_glob_needed
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  real(r_kind), dimension(:,:), intent(inout) :: x_disp, y_disp, dx_n, dy_n
  integer(i_kind), dimension(:,:), intent(inout) :: ix_orig, iy_orig
  type (fca_gridded_meta), intent(in) :: meta		! meta data
  integer(i_kind), allocatable, intent(out) :: needed_ij_in(:,:), needed_ij_out(:,:), index_in(:)
  real(r_kind), allocatable, intent(out) :: needed_wgts_out(:,:)
  integer(i_kind), intent(out) :: num_glob_needed
  integer(i_kind), intent(in) :: order
  real(r_kind), allocatable :: needed_wgts_in(:,:)
  integer(i_kind) :: num_needed_out

  real(r_kind), dimension(size(x_disp,1),size(x_disp,2)) :: x, y
  integer(i_kind) :: needed_ij_local(size(x_disp,1),size(x_disp,2),2)
  integer(i_kind) :: i, j, status, num_needed_in, ierr, offset, buf, Nx, Ny
  integer(i_kind), parameter :: BAD_INT=-999999
  integer(i_kind) :: i1, i2, j1, j2

#ifdef TRACE_USE
  if (trace_use) call da_trace_entry("compute_xy_orig_dm")
#endif

  Nx = size(x_disp,1)
  Ny = size(x_disp,2)

  forall(i=1:Nx,j=1:Ny) 
     x(i,j) = its-1+(i-1)*meta%dx + x_disp(i,j) + meta%x0
     y(i,j) = jts-1+(j-1)*meta%dy + y_disp(i,j) + meta%y0
  end forall

  ! new array indices.
  ! the r code achieves this by performing integer division on two floats, which of course were implicitly cast.

  ix_orig(1:Nx,1:Ny) = int(floor(x(1:Nx,1:Ny)/meta%dx)+1) ! x index of lower-left corner of containing value to bring back here
  iy_orig(1:Nx,1:Ny) = int(floor(y(1:Nx,1:Ny)/meta%dy)+1) ! y ""

  !	use constant extrapolation for points outside the grid:
  if (order == bilinear) then
     buf=1
  elseif (order == bicubic) then
     buf=2
  end if
  where(ix_orig < ids+buf-1)
     dx_n = 0_r_kind 
     ix_orig = ids+buf-1
  elsewhere (ix_orig > (ide-1-buf))
     dx_n = 1_r_kind
     ix_orig = ide-1-buf
  elsewhere
     ! x weight for interpolating values from box (i.e. 0.25 -> 25% from bottom, 75% from top points)
     dx_n = mod(x,meta%dx)/meta%dx
  end where

  where(iy_orig < jds+buf-1)
     dy_n = 0_r_kind
     iy_orig = jds+buf-1
  elsewhere (iy_orig > (jde-1-buf))
     dy_n = 1_r_kind
     iy_orig = jde-1-buf
  elsewhere
     dy_n = mod(y,meta%dy)/meta%dy
  end where

  ! Build up a list of origin points and associated weights for which
  ! interpolation will have to computed in another patch

  ! prefill:
  needed_ij_local(1:Nx,1:Ny,1:2)=BAD_INT
  num_needed_in=0

  ! Only apply displacements within same limits as in apply_disp_2d:

  if (order == bilinear) then
     ! apply displacements up to and including domain boundary
     j1=1
     j2=Ny
     i1=1
     i2=Nx
  elseif (order == bicubic) then
     ! apply displacements one gridpoint in from domain boundary
     j1=max(jds+1,jts)-jts+1
     j2=min(jde-1,jte)-jts+1
     i1=max(ids+1,its)-its+1
     i2=min(ide-1,ite)-its+1
  end if
    
  do j = j1, j2
     do i = i1, i2
        if (ix_orig(i,j) < ips .or. ix_orig(i,j) > ipe .or. &
             iy_orig(i,j) < jps .or. iy_orig(i,j) > jpe) then
           ! outside patch:
           needed_ij_local(i,j,1:2) = (/ix_orig(i,j), iy_orig(i,j)/)
           num_needed_in=num_needed_in+1
        end if
     end do
  end do

  allocate(needed_ij_in(4,num_needed_in), needed_wgts_in(2,num_needed_in),index_in(num_needed_in))
  offset=0
  do j = j1, j2
     do i = i1, i2
        if (needed_ij_local(i,j,1) /= BAD_INT) then
           offset=offset+1
           needed_ij_in(1:4,offset) = (/i+its-1,j+jts-1,needed_ij_local(i,j,1),needed_ij_local(i,j,2)/)
           needed_wgts_in(1:2,offset) = (/dx_n(i,j),dy_n(i,j)/)
        end if
     end do
  end do

  ! Call a routine that exchanges this info with all other patches:
  ! NOTE: needed_ij_out, needed_wgts_out is allocated within exchange_needed

  call exchange_needed(needed_ij_in, needed_wgts_in, num_needed_in, &
       ips, ipe, jps, jpe, needed_ij_out, needed_wgts_out, num_needed_out, &
       num_glob_needed, index_in)

  deallocate(needed_wgts_in,stat=ierr)

#ifdef TRACE_USE
  if (trace_use) call da_trace_exit("compute_xy_orig_dm")
#endif
end subroutine compute_xy_orig_dm

subroutine exchange_needed(needed_ij_in, needed_wgts_in, num_needed_in, &
     dum_ips, dum_ipe, dum_jps, dum_jpe, needed_ij_out, needed_wgts_out, &
     num_needed_out, num_glob_needed, index_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: exchange_needed
!   prgmmr: Nehrkorn
!
! abstract: This routine exchanges information about displacement vectors (starting and
!           end points, and interpolation weights) between processors
!           patch dimensions: dum_ips:dum_ipe, dum_jps:dum_jpe

! program history log:
!
!   input argument list: 
!      needed_ij_in:
!       displacement vectors with endpoints in current patch (i,j stored in columns 1,2)
!       and origin points in other patches (ix_orig, iy_orig stored in columns 3,4)
!       NOTE: reordered upon output
!      needed_wgts_in
!       associated interpolation weights (x,y stored in columns 1,2)
!       NOTE: not needed after this routine
!      index_in:
!       ith element points to location in re-broadcasted global array of needed_out points/values
!       for the ith row of (reordered) needed_ij_in
!
!   output argument list:
!      needed_ij_out:
!       displacement vectors with endpoints in other patches (i,j stored in columns 1,2)
!       and origin points in current patch (ix_orig, iy_orig stored in columns 3,4)
!      needed_wgts_out
!       associated interpolation weights (x,y stored in columns 1,2)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use mpi, only: MPI_COMM_WORLD, MPI_TYPECLASS_REAL, MPI_TYPECLASS_INTEGER, MPI_SIZEOF

  implicit none

  integer(i_kind), intent(in)			:: num_needed_in, dum_ips, dum_ipe, dum_jps, dum_jpe
  integer(i_kind), intent(inout)			:: needed_ij_in(4, num_needed_in)
  real(r_kind), intent(in)			:: needed_wgts_in(2, num_needed_in)
  integer(i_kind), intent(out)                :: index_in(num_needed_in)
  integer(i_kind), allocatable, intent(out)	:: needed_ij_out(:, :)
  real(r_kind), allocatable, intent(out)	:: needed_wgts_out(:, :)
  integer(i_kind), intent(out)					:: num_needed_out, num_glob_needed

  integer(i_kind)						:: num_proc, my_id, ierr, ip, offset_ip
  integer(i_kind)						:: i, count, x, y, offset
  integer(i_kind), dimension(:), allocatable			:: packed_needed_ij
  real(r_kind), dimension(:), allocatable			:: packed_needed_wgts
  real(r_kind), dimension(:), allocatable			:: needed_all_wgts
  integer(i_kind), dimension(:), allocatable			:: needed_all_ij
  integer(i_kind), dimension(:), allocatable			:: proc_needed, proc_needed_ij, proc_needed_wgts, buf_disp

  ! variables to hold info for MPI_FLOAT, MPI_INT
  integer(i_kind) :: float_class, float_size, float_type
  integer(i_kind) :: fixed_class, fixed_size, fixed_type

  ! pack up the input arrays
  allocate(packed_needed_ij(4*num_needed_in), &
       packed_needed_wgts(2*num_needed_in), stat=ierr)
  if (ierr /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** Allocation failure of "packed_needed_" ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if
  packed_needed_ij(:) = reshape(needed_ij_in,(/4*num_needed_in/))
  packed_needed_wgts(:) = reshape(needed_wgts_in,(/2*num_needed_in/))

  ! get the MPI info
  call MPI_COMM_RANK (MPI_COMM_WORLD, my_id, ierr)
  call MPI_COMM_SIZE (MPI_COMM_WORLD, num_proc, ierr)
  allocate(proc_needed(num_proc),proc_needed_ij(num_proc), &
       proc_needed_wgts(num_proc),buf_disp(num_proc),stat=ierr)
  if (ierr /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** Allocation failure of "proc_needed_" ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if

  ! given a count of the number of needed cells, we need to get this same count from the other processors
  call MPI_SIZEOF(num_needed_in,fixed_size,ierr)
  fixed_class = MPI_TYPECLASS_INTEGER
  call MPI_TYPE_MATCH_SIZE(fixed_class, fixed_size, fixed_type, ierr)
  call MPI_ALLGATHER(num_needed_in, 1, fixed_type, proc_needed, 1, fixed_type, MPI_COMM_WORLD, ierr)
  num_glob_needed = sum(proc_needed)

  ! send the ijs:
  ! create the buffer pointers to start the output from each processor
  proc_needed_ij(:)=4*proc_needed(:)
  buf_disp(1) = 0
  do ip=2,num_proc
     buf_disp(ip) = buf_disp(ip-1) + proc_needed_ij(ip-1)
  end do
  ! create the array for the globally needed points and weights
  allocate(needed_all_ij(4*num_glob_needed),stat=ierr)
  if (ierr /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** Allocation failure of "needed_all_ij" ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if
  call MPI_ALLGATHERV(packed_needed_ij, 4*num_needed_in, fixed_type, &
       needed_all_ij, proc_needed_ij, buf_disp, fixed_type, MPI_COMM_WORLD, ierr)
  ! send the wgts:
  ! create the buffer pointers to start the output from each processor
  proc_needed_wgts(:)=2_r_kind*proc_needed(:)
  buf_disp(1) = 0
  do ip=2,num_proc
     buf_disp(ip) = buf_disp(ip-1) + proc_needed_wgts(ip-1)
  end do
  allocate(needed_all_wgts(2*num_glob_needed),stat=ierr)
  if (ierr /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** Allocation failure of "needed_all_wgts_" ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if
  call MPI_SIZEOF(packed_needed_wgts,float_size,ierr)
  float_class = MPI_TYPECLASS_REAL
  call MPI_TYPE_MATCH_SIZE(float_class, float_size, float_type, ierr)
  call MPI_ALLGATHERV(packed_needed_wgts, 2*num_needed_in, float_type, &
       needed_all_wgts, proc_needed_wgts, buf_disp, float_type, MPI_COMM_WORLD, ierr)

  ! count the number of globally needed points that are in our patch
  num_needed_out = 0
  offset_ip=0
  ip=1
  do i = 1, num_glob_needed
     offset=(i-1)*4
     x = needed_all_ij(offset+3)
     y = needed_all_ij(offset+4)
     if (x >= dum_ips .and. x <= dum_ipe .and. y >= dum_jps .and. y <= dum_jpe) then
        num_needed_out = num_needed_out +1
     end if
  end do
  ! allocate output arrays
  allocate(needed_ij_out(4, num_needed_out), stat=ierr)
  if (ierr /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** Allocation failure of "needed_ij_out" ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if
  allocate(needed_wgts_out(2, num_needed_out), stat=ierr)
  if (ierr /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** Allocation failure of "needed_wgts_out" ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if
  ! fill the output arrays
  count = 0
  do i = 1, num_glob_needed
     offset=(i-1)*4
     x = needed_all_ij(offset+3)
     y = needed_all_ij(offset+4)
     if (x >= dum_ips .and. x <= dum_ipe .and. y >= dum_jps .and. y <= dum_jpe) then
        count = count+1
        needed_ij_out(1,count) = needed_all_ij(offset+1)
        needed_ij_out(2,count) = needed_all_ij(offset+2)
        needed_ij_out(3,count) = x
        needed_ij_out(4,count) = y
        needed_wgts_out(1,count) = needed_all_wgts((i-1)*2+1)
        needed_wgts_out(2,count) = needed_all_wgts((i-1)*2+2)
     end if
  end do
  if (count /= num_needed_out) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** Needed points output counts do not match ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if

  ! Finally, determine the mapping from the assembled global needed_ij_out
  ! to the patch needed_ij_in arrays:

  deallocate(packed_needed_ij,stat=ierr)
  if (ierr /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** deAllocation failure of packed_needed_ij ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if
  allocate(packed_needed_ij(4*num_needed_out),stat=ierr)
  if (ierr /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** Allocation failure of packed_needed_ij ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if
  packed_needed_ij(:) = reshape(needed_ij_out,(/4*num_needed_out/))
  call MPI_ALLGATHER(num_needed_out, 1, fixed_type, proc_needed, 1, fixed_type, MPI_COMM_WORLD, ierr)
  num_glob_needed = sum(proc_needed)

  ! send the ijs:
  ! create the buffer pointers to start the output from each processor
  proc_needed_ij(:)=4*proc_needed(:)
  buf_disp(1) = 0
  do ip=2,num_proc
     buf_disp(ip) = buf_disp(ip-1) + proc_needed_ij(ip-1)
  end do
  ! create the array for the globally needed points
  call MPI_ALLGATHERV(packed_needed_ij, 4*num_needed_out, fixed_type, &
       needed_all_ij, proc_needed_ij, buf_disp, fixed_type, MPI_COMM_WORLD, ierr)

  ! Now find where in the global _out array the _in points are for this patch:
  ! re-order needed_ij_in accordingly

  count = 0
  do i = 1, num_glob_needed
     offset=(i-1)*4
     x = needed_all_ij(offset+1) !NOTE: we are looking for endpoints in current patch
     y = needed_all_ij(offset+2)
     if (x >= dum_ips .and. x <= dum_ipe .and. y >= dum_jps .and. y <= dum_jpe) then
        count = count+1
        index_in(count) = i
        needed_ij_in(1:4,count)=needed_all_ij((offset+1):(offset+4))
     end if
  end do

  if (count /= num_needed_in) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** Needed points output counts do not match ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if

  deallocate(packed_needed_ij,packed_needed_wgts, &
       proc_needed,proc_needed_ij,proc_needed_wgts,buf_disp, &
       needed_all_ij, needed_all_wgts,stat=ierr)
  if (ierr /= 0) then
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** deAllocation failure of packed_needed ... ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if

end subroutine exchange_needed

subroutine exchange_interpolated(needed_adj_out, needed_adj_in, index_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: exchange_interpolated
!   prgmmr: Nehrkorn
!
! abstract:  This routine exchanges interpolated values at the displacement vectors
!            origin points between processors

! program history log:
!
!   input argument list: 
!     index_in: (input)
!      maps the global array of needed_adj_in to patch array needed_adj_in
!     needed_adj_out: (input to this routine)
!      values for displacement vectors with endpoints in other patches
!      and origin points in current patch
!
!   output argument list:
!     needed_adj_in: (output from this routine)
!      values for displacement vectors with endpoints in current patch
!      and origin points in other patches
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use mpi, only: MPI_COMM_WORLD, MPI_TYPECLASS_REAL, MPI_TYPECLASS_INTEGER, MPI_SIZEOF

  implicit none
  integer(i_kind), dimension(:), intent(in)	:: index_in
  real(r_kind), dimension(:), intent(in)	:: needed_adj_out
  real(r_kind), dimension(:), intent(out)	:: needed_adj_in

  integer(i_kind), dimension(:), allocatable	:: proc_needed, buf_disp
  real(r_kind), dimension(:), allocatable	:: needed_allval
  integer(i_kind)				:: i, ip, num_proc, my_id, ierr
  integer(i_kind)				:: num_needed_out, num_needed_in, num_glob_needed, count

  ! variables to hold info for MPI_FLOAT, MPI_INT
  integer(i_kind) :: float_class, float_size, float_type
  integer(i_kind) :: fixed_class, fixed_size, fixed_type

  ! get the MPI info
  call MPI_COMM_RANK (MPI_COMM_WORLD, my_id, ierr)
  call MPI_COMM_SIZE (MPI_COMM_WORLD, num_proc, ierr)
  allocate(proc_needed(num_proc),buf_disp(num_proc),stat=ierr)
  if (ierr /= 0)  then 
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** failed to allocate proc_needed ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if

  num_needed_out = SIZE(needed_adj_out)
  num_needed_in = SIZE(needed_adj_in)
  ! given a count of the number of needed cells, we need to get this same count from the other processors
  call MPI_SIZEOF(num_needed_out,fixed_size,ierr)
  fixed_class = MPI_TYPECLASS_INTEGER
  call MPI_TYPE_MATCH_SIZE(fixed_class, fixed_size, fixed_type, ierr)
  call MPI_ALLGATHER (num_needed_out, 1, fixed_type, proc_needed, 1, fixed_type, MPI_COMM_WORLD, ierr)
  num_glob_needed = sum(proc_needed)
  ! create the buffer pointers to start the output from each processor
  buf_disp(1) = 0
  do ip=2,num_proc
     buf_disp(ip) = buf_disp(ip-1) + proc_needed(ip-1)
  end do

  ! exchange the interformation on the interpolated points
  allocate(needed_allval(num_glob_needed),stat=ierr)
  if (ierr /= 0)  then 
#ifndef FCA_REF_MOD
     write(unit=message(1),fmt='(A)') '*** failed to allocate needed_allval ***'
     call da_error(__FILE__,__LINE__,message(1:1))
#endif
  end if

  call MPI_SIZEOF(needed_adj_out,float_size,ierr)
  float_class = MPI_TYPECLASS_REAL
  call MPI_TYPE_MATCH_SIZE(float_class, float_size, float_type, ierr)
  call MPI_ALLGATHERV(needed_adj_out, num_needed_out, float_type, needed_allval, &
       proc_needed, buf_disp, float_type, MPI_COMM_WORLD, ierr)

  ! extract the information from the global array:
  do count=1,num_needed_in
     needed_adj_in(count)=needed_allval(index_in(count))
  end do

  deallocate(proc_needed, buf_disp, needed_allval)

end subroutine exchange_interpolated

#endif

subroutine apply_disp_2d(bg,adj,disp, &
     needed_ij_in, needed_ij_out, index_in, needed_wgts_out, num_glob_needed, order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    apply_disp_2d
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
!     order
!     num_glob_needed, needed_ij_in, needed_ij_out, index_in
!     needed_wgts_out
!
!   output argument list: displaced field in adj
!     adj
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none
    real(r_kind), intent(in) :: bg(ims:ime,jms:jme) ! gridded 2d field: original
    real(r_kind), intent(out) :: adj(ims:ime,jms:jme) ! gridded 2d field: displaced
    type (fca_gridded_disp), intent(in) :: disp		! displacements
    integer(i_kind), intent(in) :: num_glob_needed, needed_ij_in(:,:), needed_ij_out(:,:), index_in(:)
    real(r_kind), intent(in) :: needed_wgts_out(:,:)
    integer(i_kind), intent(in) :: order

    integer(i_kind) :: i, j, id, jd, inum_in, num_needed_in, iout, num_needed_out
    integer(i_kind) :: i1, i2, j1, j2
    real(r_kind) :: needed_adj_in (size(needed_ij_in,dim=2))
    real(r_kind) :: needed_adj_out (size(needed_ij_out,dim=2))

#ifdef TRACE_USE
    if (trace_use) call da_trace_entry("apply_disp_2d")
#endif

#ifdef DM_PARALLEL
    num_needed_in=size(needed_ij_in,dim=2)
    num_needed_out=size(needed_ij_out,dim=2)
    if (num_glob_needed > 0) then
       ! First compute the output needed by other patches
       do iout=1,num_needed_out
          if (order == bilinear) then
             needed_adj_out(iout)=bilin_interp( &
                  bg(needed_ij_out(3,iout),needed_ij_out(4,iout)), &
                  bg(needed_ij_out(3,iout)+1,needed_ij_out(4,iout)), &
                  bg(needed_ij_out(3,iout),needed_ij_out(4,iout)+1), &
                  bg(needed_ij_out(3,iout)+1,needed_ij_out(4,iout)+1), &
                  needed_wgts_out(1,iout), needed_wgts_out(2,iout))
          elseif (order == bicubic) then
             needed_adj_out(iout) = bicub_interp(bg, needed_ij_out(3,iout),needed_ij_out(4,iout), &
                  needed_wgts_out(1,iout), needed_wgts_out(2,iout))
          end if
       enddo

       ! Exchange this info with other patches
       call exchange_interpolated(needed_adj_out, needed_adj_in, index_in)
    end if
#endif

    ! Initialize displaced arrays so the halos are also initialized:
    adj(:,:) = bg(:,:)
    if (order == bilinear) then
       ! apply displacements up to and including domain boundary
       j1=1
       j2=size(disp%ix_orig,2)
       i1=1
       i2=size(disp%ix_orig,1)
    elseif (order == bicubic) then
       ! apply displacements one gridpoint in from domain boundary
       j1=max(jds+1,jts)-jts+1
       j2=min(jde-1,jte)-jts+1
       i1=max(ids+1,its)-its+1
       i2=min(ide-1,ite)-its+1
       if (i1 > 1)         adj(its,jts:jte) = bg(its,jts:jte)
       if (i2 < ite-its+1) adj(ite,jts:jte) = bg(ite,jts:jte)
       if (j1 > 1)         adj(its:ite,jts) = bg(its:ite,jts)
       if (j2 < jte-jts+1) adj(its:ite,jte) = bg(its:ite,jte)
    end if
    
    do j = j1, j2
       jd = j+jts-1
       do i = i1, i2
       	  id = i+its-1
          ! save to adj after bilinear interpolation of displaced field
          if (disp%ix_orig(i,j) >= ips .and. disp%ix_orig(i,j) <= ipe .and. &
              disp%iy_orig(i,j) >= jps .and. disp%iy_orig(i,j) <= jpe) then
             if (order == bilinear) then
                adj(id,jd) = &
                     bilin_interp(&
                     bg(disp%ix_orig(i,j),disp%iy_orig(i,j)),&
                     bg(disp%ix_orig(i,j)+1,disp%iy_orig(i,j)),&
                     bg(disp%ix_orig(i,j),disp%iy_orig(i,j)+1), &
                     bg(disp%ix_orig(i,j)+1,disp%iy_orig(i,j)+1),&
                     disp%dx_n(i,j),disp%dy_n(i,j))
             elseif (order == bicubic) then
                adj(id,jd) = bicub_interp(bg, disp%ix_orig(i,j), disp%iy_orig(i,j), &
                     disp%dx_n(i,j),disp%dy_n(i,j))
             end if
          end if
       end do !i
    end do !j
    if (num_glob_needed > 0) then
     
       do inum_in=1,num_needed_in
          adj(needed_ij_in(1,inum_in), needed_ij_in(2,inum_in)) = needed_adj_in(inum_in)
       end do

    end if

#ifdef TRACE_USE
    if (trace_use) call da_trace_exit("apply_disp_2d")
#endif
end subroutine apply_disp_2d

subroutine apply_vert(var,stag,work2d,fcadisp,&
     needed_ij_in, needed_ij_out, index_in, needed_wgts_out, num_glob_needed, interp_order)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    APPLY_VERT
!   prgmmr: Nehrkorn
!
! abstract:
!
! program history log:
!
!   input argument list:
!     stag
!     var
!
!   output argument list:
!     var
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    integer(i_kind), intent(in) :: stag				! dimension along which var is staggered
    real(r_kind), dimension(ims:ime,jms:jme,kms:kme), intent(inout) :: var		! variable to displace
    real(r_kind), dimension(ims:ime,jms:jme,2), intent(inout) :: work2d		! work space for 2d original/displaced fields
    type (fca_gridded_disp), intent(in) :: fcadisp			! displacement field generated by FCA routine
    integer(i_kind), intent(in) :: num_glob_needed, needed_ij_in(:,:), needed_ij_out(:,:), index_in(:)
    real(r_kind), intent(in) :: needed_wgts_out(:,:)
    integer(i_kind), intent(in) :: interp_order
    integer(i_kind) :: i
    
#ifdef TRACE_USE
    if (trace_use) call da_trace_entry("apply_vert")
#endif
    do i = kts, kte+stag
       ! Loop through all vertical levels and apply displacements -- watch out for staggering
       work2d(:,:,1) = var(:,:,i)
       call apply_disp_2d(work2d(:,:,1),work2d(:,:,2),fcadisp,&
            needed_ij_in, needed_ij_out, index_in, needed_wgts_out, num_glob_needed, interp_order)
       var(:,:,i) = work2d(:,:,2)
    end do
#ifdef TRACE_USE
    if (trace_use) call da_trace_exit("apply_vert")
#endif
end subroutine apply_vert

type (fca_gridded_meta) function init_meta(nx, ny, dx, dy, x0, y0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_meta
!   prgmmr: Nehrkorn
!
! abstract: Meta-data constructor
!
! program history log:
!
!   input argument list:
!     nx, ny, dx, dy, x0, y0
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none
    integer(i_kind), intent(in) :: nx, ny
    real(r_kind), intent(in) :: dx, dy, x0, y0
    integer(i_kind) :: status
    init_meta%nx = nx
    init_meta%ny = ny
    init_meta%dx = dx
    init_meta%dy = dy
    init_meta%x0 = x0
    init_meta%y0 = y0
end function init_meta

subroutine allocate_disp(disp, nlm_flag, ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    allocate_disp
!   prgmmr: Nehrkorn
!
! abstract: allocate fca_gridded_disp object
!
! program history log:
!
!   input argument list:
!     disp
!     nlm_flag
!
!   output argument list:
!     disp
!     ierror
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(fca_gridded_disp), intent(inout) :: disp
  logical, intent(in) :: nlm_flag
  integer(i_kind), intent(out) :: ierror

  integer(i_kind) :: status

  ierror = 1 ! 1: uninitialized meta
  if (disp%meta%nx < 1 .or. disp%meta%ny < 1) return

  ierror=ierror+1 ! 2: error allocating x_disp
  allocate(disp%x_disp(disp%meta%nx,disp%meta%ny), stat=status)
  if (status /= 0) return
  ierror=ierror+1 ! 3: error allocating y_disp
  allocate(disp%y_disp(disp%meta%nx,disp%meta%ny), stat=status)
  if (status /= 0) return
  ierror=ierror+1 ! 4: error allocating dx_n
  allocate(disp%dx_n(disp%meta%nx,disp%meta%ny), stat=status)
  if (status /= 0) return
  ierror=ierror+1 ! 5: error allocating dy_n
  allocate(disp%dy_n(disp%meta%nx,disp%meta%ny), stat=status)
  if (status /= 0) return
  if (nlm_flag) then ! i[xy]_orig only needed for nonlinear disp structure
     ierror=ierror+1 ! 6: error allocating ix_orig
     allocate(disp%ix_orig(disp%meta%nx,disp%meta%ny), stat=status)
     if (status /= 0) return
     ierror=ierror+1 ! 7: error allocating iy_orig
     allocate(disp%iy_orig(disp%meta%nx,disp%meta%ny), stat=status)
     if (status /= 0) return
  end if
  ierror=0 ! 0: successful exit
  return
end subroutine allocate_disp

subroutine deallocate_disp(disp, ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deallocate_disp
!   prgmmr: Nehrkorn
!
! abstract: deallocate fca_gridded_disp object
!
! program history log:
!
!   input argument list:
!     disp
!
!   output argument list:
!     disp
!     ierror
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(fca_gridded_disp), intent(inout) :: disp
  integer(i_kind), intent(out) :: ierror

  integer(i_kind) :: status

  ierror = 1 ! 1: not applicable
  ierror=ierror+1 ! 2: error deallocating x_disp
  if (allocated(disp%x_disp)) then
     deallocate(disp%x_disp, stat=status)
     if (status /= 0) return
  end if
  ierror=ierror+1 ! 3: error deallocating y_disp
  if (allocated(disp%y_disp)) then
     deallocate(disp%y_disp, stat=status)
     if (status /= 0) return
  end if
  ierror=ierror+1 ! 4: error deallocating dx_n
  if (allocated(disp%dx_n)) then
     deallocate(disp%dx_n, stat=status)
     if (status /= 0) return
  end if
  ierror=ierror+1 ! 5: error deallocating dy_n
  if (allocated(disp%dy_n)) then
     deallocate(disp%dy_n, stat=status)
     if (status /= 0) return
  end if
  ierror=ierror+1 ! 6: error deallocating ix_orig
  if (allocated(disp%ix_orig)) then
     deallocate(disp%ix_orig, stat=status)
     if (status /= 0) return
  end if
  ierror=ierror+1 ! 7: error deallocating iy_orig
  if (allocated(disp%iy_orig)) then
     deallocate(disp%iy_orig, stat=status)
     if (status /= 0) return
  end if
  ierror=0 ! 0: successful exit
  return
end subroutine deallocate_disp

#ifdef FCA_REF_MOD
end module core_disp_func_m
#endif
