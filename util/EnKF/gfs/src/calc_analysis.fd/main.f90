!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! calc_analysis
! read in: 1. netCDF FV3 increment file on gaussian grid
!          2. NEMSIO background file on gaussian grid
! write out: 1. NEMSIO analysis file
! Original:  2019-09-16  Martin - Original version
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program calc_analysis_main
  use init_calc_analysis, only: read_nml
  use init_nemsio_io, only: init_read_bg, init_write_anl
  use inc2anl, only: compute_anl  
  implicit none
  write(6,*) "calc_analysis.x starting"
  call read_nml
  call init_read_bg
  call init_write_anl
  call compute_anl
  ! call cleanup
end program calc_analysis_main
