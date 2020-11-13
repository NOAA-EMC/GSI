!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! calc_analysis
! read in: 1. netCDF FV3 increment file on gaussian grid
!          2. NEMSIO background file on gaussian grid
! write out: 1. NEMSIO analysis file
! Original:  2019-09-18  Martin - Original version
!            2019-10-24  Martin - rewrote to support netCDF background and write
!                                 either NEMSIO or netCDF analysis output
!            2019-11-14  Martin - modified to support MPI for IAU
!            2020-01-17  Martin - parallel IO support added
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program calc_analysis_main
  use mpi
  use init_calc_analysis, only: read_nml
  use init_io, only: init_read_bg, init_write_anl
  use inc2anl, only: gen_anl, close_files 
  use vars_calc_analysis, only: mype, npes
  implicit none
  integer :: ierr
  call mpi_init(ierr)
  call mpi_comm_rank(mpi_comm_world, mype, ierr)
  call mpi_comm_size(mpi_comm_world, npes, ierr)
  if (mype==0) call w3tagb('CALC_ANALYSIS', 2019, 300, 0, 'EMC')
  call read_nml
  call init_read_bg
  call init_write_anl
  call gen_anl
  call close_files
  if (mype==0) call w3tage('CALC_ANALYSIS')
  call mpi_barrier(mpi_comm_world, ierr)
  call mpi_finalize(ierr)
end program calc_analysis_main
