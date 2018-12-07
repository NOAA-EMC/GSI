module fv3_netcdf_mod

use kinds, only: r_kind, i_kind, r_double, r_single
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
    include "read_fv3_restart_data.f90"
 end subroutine read_fv3_restart_data1d

 subroutine read_fv3_restart_data2d(varname,filename,file_id,data_arr)
    real(r_single), intent(inout), dimension(:,:) :: data_arr
    character(len=24),parameter :: myname_ = 'read_fv3_restart_data2d'
    include "read_fv3_restart_data.f90"
 end subroutine read_fv3_restart_data2d

 subroutine read_fv3_restart_data3d(varname,filename,file_id,data_arr)
    real(r_single), intent(inout), dimension(:,:,:) :: data_arr
    character(len=24),parameter :: myname_ = 'read_fv3_restart_data3d'
    include "read_fv3_restart_data.f90"
 end subroutine read_fv3_restart_data3d

 subroutine read_fv3_restart_data4d(varname,filename,file_id,data_arr)
    real(r_single), intent(inout), dimension(:,:,:,:) :: data_arr
    character(len=24),parameter :: myname_ = 'read_fv3_restart_data4d'
    include "read_fv3_restart_data.f90"
 end subroutine read_fv3_restart_data4d

end module fv3_netcdf_mod
