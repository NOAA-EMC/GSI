subroutine convert_fv3_regional 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_fv3_regional  read single fv3 nest  
!   prgmmr: parrish          org: np22                date: 2017-04-09
!
! abstract: using routines from gsi_rfv3io_mod.f90 module to setup for
!           reading tile of forecast fields from an fv3 forecast.
!         NOTE: run on single processor, with information stored on unit lendian_out
!
!#################################################################################
!#################################################################################
!    Use subroutine convert_nems_nmmb (in wrf_binary_interface.F90) as pattern.
!#################################################################################
!#################################################################################
!
! program history log:
!   2017-04-08  parrish
!   2018-02-16  wu   - read in grid and time infor from fv3 files
!                      read directly from fv3 files and not writeout GSI internal file 
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_single,r_kind,i_kind
  use gsi_rfv3io_mod, only: gsi_rfv3io_get_grid_specs
  use gsi_io, only: lendian_out
  use gridmod, only: nsig,regional_time,regional_fhr,nlon_regional,nlat_regional,nsig
  use mpimod, only: mype

  implicit none
  integer(i_kind) ierr
  character(128) grid_spec,ak_bk


!!!!!!!!!!! get grid specs !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  grid_spec='fv3_grid_spec'            ! horizontal grid information
  ak_bk='fv3_akbk'                     ! vertical grid information
  call gsi_rfv3io_get_grid_specs(grid_spec,ak_bk,ierr)
  if(ierr/=0)then
     write(6,*)' problem in convert_fv3_regional - get_grid_specs   Status = ',ierr
     call stop2 (555)
  endif
end subroutine convert_fv3_regional

