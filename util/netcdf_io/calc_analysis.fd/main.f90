!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! calc_analysis
! read in: 1. netCDF FV3 increment file on gaussian grid
!          2. NEMSIO background file on gaussian grid
! write out: 1. NEMSIO analysis file
! Original:  2019-09-18  Martin - Original version
!            2019-10-24  Martin - rewrote to support netCDF background and write
!                                 either NEMSIO or netCDF analysis output
!            2019-11-14  Martin - modified to support MPI for IAU
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program calc_analysis_main
  use mpi
  use init_calc_analysis, only: read_nml
  use init_io, only: init_read_bg, init_write_anl
  use inc2anl, only: gen_anl  
  use vars_calc_analysis, only: nhrs_assim
  implicit none
  integer :: mype, npes, ierr
  call mpi_init(ierr)
  call mpi_comm_rank(mpi_comm_world, mype, ierr)
  call mpi_comm_size(mpi_comm_world, npes, ierr)
  if (mype==0) call w3tagb('CALC_ANALYSIS', 2019, 300, 0, 'EMC')
  call read_nml(mype)
  if ( npes < nhrs_assim ) then
    if ( mype == 0 ) then
       write(6,*) 'npes, nhrs_assim = ', npes, nhrs_assim
       write(6,*) 'npes must be atleast equal to nhrs_assim, ABORT!'
    endif
    call mpi_abort(mpi_comm_world, 99, ierr)
  endif
  if (mype < nhrs_assim) then 
    call init_read_bg
    call init_write_anl
    call gen_anl
  end if
  call mpi_barrier(mpi_comm_world, ierr)
  if (mype==0) call w3tage('CALC_ANALYSIS')
  call mpi_finalize(ierr)
end program calc_analysis_main
