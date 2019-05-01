module fv3_netcdf_mod

use kinds, only: r_single, i_kind, r_double, r_single
use netcdf, only: nf90_get_var,nf90_inq_varid
use netcdf_mod, only: nc_check

implicit none

private
public :: read_fv3_restart_data1d, read_fv3_restart_data2d, &
          read_fv3_restart_data3d, read_fv3_restart_data4d
contains

 subroutine read_fv3_restart_data1d(varname,filename,file_id,data_arr)
    real(r_single), intent(inout), dimension(:) :: data_arr
    character(len=24),parameter :: myname_ = 'read_fv3_restart_data1d'
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: filename
    integer(i_kind), intent(in) :: file_id
    integer(i_kind) :: var_id
    include "read_fv3_restart_data.f90"
    data_arr=data_arr(ubound(data_arr,1):lbound(data_arr,1):-1)
 end subroutine read_fv3_restart_data1d

 subroutine read_fv3_restart_data2d(varname,filename,file_id,data_arr)
    real(r_single), intent(inout), dimension(:,:) :: data_arr
    character(len=24),parameter :: myname_ = 'read_fv3_restart_data2d'
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: filename
    integer(i_kind), intent(in) :: file_id
    integer(i_kind) :: var_id
    include "read_fv3_restart_data.f90"
    data_arr=data_arr(ubound(data_arr,1):lbound(data_arr,1):-1,ubound(data_arr,2):lbound(data_arr,2):-1)
 end subroutine read_fv3_restart_data2d

 subroutine read_fv3_restart_data3d(varname,filename,file_id,data_arr)
    real(r_single), intent(inout), dimension(:,:,:) :: data_arr
    character(len=24),parameter :: myname_ = 'read_fv3_restart_data3d'
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: filename
    integer(i_kind), intent(in) :: file_id
    integer(i_kind) :: var_id
    include "read_fv3_restart_data.f90"
    data_arr=data_arr(ubound(data_arr,1):lbound(data_arr,1):-1,ubound(data_arr,2):lbound(data_arr,2):-1, &
                      ubound(data_arr,3):lbound(data_arr,3):-1)
 end subroutine read_fv3_restart_data3d

 subroutine read_fv3_restart_data4d(varname,filename,file_id,data_arr)
    real(r_single), intent(inout), dimension(:,:,:,:) :: data_arr
    character(len=24),parameter :: myname_ = 'read_fv3_restart_data4d'
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: filename
    integer(i_kind), intent(in) :: file_id
    integer(i_kind) :: var_id
    include "read_fv3_restart_data.f90"
    data_arr=data_arr(ubound(data_arr,1):lbound(data_arr,1):-1,ubound(data_arr,2):lbound(data_arr,2):-1, &
                      ubound(data_arr,3):lbound(data_arr,3):-1,lbound(data_arr,4):ubound(data_arr,4))
!Notice, the 4th dimension is not reversed

 end subroutine read_fv3_restart_data4d

end module fv3_netcdf_mod
