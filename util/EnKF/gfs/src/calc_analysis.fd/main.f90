!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! calc_analysis
! read in: 1. netCDF FV3 increment file on gaussian grid
!          2. NEMSIO background file on gaussian grid
! write out: 1. NEMSIO analysis file
! Original:  2019-09-16  Martin - Original version
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program calc_analysis_main
  use init_calc_analysis, only: read_nml
  use nemsio_interface, only: init_read_bg, init_write_anl
  !use nemsio_interface, only: read_bg, write_anl
  !use netcdf_interface, only: read_inc
  !use inc2anl, only: compute_anl  
  implicit none
  write(6,*) "calc_analysis.x starting"
  call read_nml
  call init_read_bg
  call init_write_anl
  !call read_inc
  !call compute_anl
  !call write_anl
end program calc_analysis_main
