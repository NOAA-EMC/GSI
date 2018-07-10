program calc_increment_pmain

  use mpi
  use namelist_def, only : read_namelist, write_namelist
  use namelist_def, only : analysis_filename, firstguess_filename, increment_filename
  use namelist_def, only : datapath
  use namelist_def, only : debug
  use namelist_def, only : max_vars, incvars_to_zero
  use namelist_def, only : nens
  use calc_increment_interface, only: calc_increment

  implicit none

  character(len=3) :: memchar
  integer :: mype, mype1, npes, ierr
  integer :: i

  call mpi_init(ierr)

  call mpi_comm_rank(mpi_comm_world, mype, ierr)
  call mpi_comm_size(mpi_comm_world, npes, ierr)

  if (mype==0) call w3tagb('CALC_INCREMENT_ENS',2018,0177,0055,'NP20')

  call read_namelist
  if ( mype == 0 ) call write_namelist

  if ( npes < nens ) then
    if ( mype == 0 ) then
       write(6,*) 'npes, nens = ', npes, nens
       write(6,*) 'npes must be atleast equal to nens, ABORT!'
    endif
    call mpi_abort(mpi_comm_world, 99, ierr)
  endif

  mype1 = mype + 1
  write(memchar,'(I3.3)') mype1

  analysis_filename = trim(adjustl(datapath)) // trim(adjustl(analysis_filename)) // '_mem' // trim(adjustl(memchar))
  firstguess_filename = trim(adjustl(datapath)) // trim(adjustl(firstguess_filename)) // '_mem' // trim(adjustl(memchar))
  increment_filename = trim(adjustl(datapath)) // trim(adjustl(increment_filename)) // '_mem' // trim(adjustl(memchar))

  if ( mype == 0 ) then
    write(6,*) 'DATAPATH        = ', trim(datapath)
    write(6,*) 'ANALYSIS FILE   = ', trim(analysis_filename)
    write(6,*) 'FIRSTGUESS FILE = ', trim(firstguess_filename)
    write(6,*) 'INCREMENT FILE  = ', trim(increment_filename)
    write(6,*) 'DEBUG           = ', debug
    write(6,*) 'NENS            = ', nens
    do i=1,max_vars
      if ( trim(incvars_to_zero(i)) /= 'NONE' ) then
        write(6,*) 'INCVARS_TO_ZERO = ', trim(incvars_to_zero(i))
      else
        cycle
      endif
    enddo
  endif

  call mpi_barrier(mpi_comm_world, ierr)

  if ( mype < nens ) then

    write(6,*) 'task mype = ', mype, ' process ', trim(increment_filename)

    call calc_increment(mype)

  else

    write(6,*) 'no files to process for mpi task = ', mype

  endif

  call mpi_barrier(mpi_comm_world, ierr)

  if (mype==0) call w3tage('CALC_INCREMENT_ENS')
  
  call mpi_finalize(ierr)

  stop
end program calc_increment_pmain
