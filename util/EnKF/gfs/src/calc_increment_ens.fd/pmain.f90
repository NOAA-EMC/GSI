program calc_increment_pmain

  use mpi
  use kinds
  use namelist_def, only : datapath, analysis_filename, firstguess_filename, increment_filename, debug
  use calc_increment_interface

  implicit none

  character(len=10) :: bufchar
  character(len=3) :: memchar
  character(len=500) :: analysis_tmpl, firstguess_tmpl, increment_tmpl
  integer :: nens
  integer :: mype, npes, ierr
  integer :: mype1

  call mpi_init(ierr)

  call mpi_comm_rank(mpi_comm_world, mype, ierr)
  call mpi_comm_size(mpi_comm_world, npes, ierr)

  call getarg(1, datapath)
  call getarg(2, analysis_tmpl)
  call getarg(3, firstguess_tmpl)
  call getarg(4, increment_tmpl)
  call getarg(5, bufchar)
  read(bufchar,'(L)') debug
  call getarg(6, bufchar)
  read(bufchar,'(I5)') nens

  if ( mype == 0 ) then
    write(6,*) 'DATAPATH        = ', trim(datapath)
    write(6,*) 'ANALYSIS TMPL   = ', trim(analysis_tmpl)
    write(6,*) 'FIRSTGUESS TMPL = ', trim(firstguess_tmpl)
    write(6,*) 'INCREMENT TMPL  = ', trim(increment_tmpl)
    write(6,*) 'DEBUG           = ', debug
    write(6,*) 'NENS            = ', nens
  endif

  call mpi_barrier(mpi_comm_world, ierr)

  if ( npes < nens ) then
    if ( mype == 0 ) then
       write(6,*) 'npes, nens = ', npes, nens
       write(6,*) 'npes must be greater than nens, ABORT!'
    endif
    call mpi_abort(mpi_comm_world, 99, ierr)
  endif

  mype1 = mype + 1
  if ( mype1 <= nens ) then

    write(memchar,'(I3.3)') mype1

    analysis_filename = trim(adjustl(datapath)) // trim(adjustl(analysis_tmpl)) // '_mem' // trim(adjustl(memchar))
    firstguess_filename = trim(adjustl(datapath)) // trim(adjustl(firstguess_tmpl)) // '_mem' // trim(adjustl(memchar))
    increment_filename = trim(adjustl(datapath)) // trim(adjustl(increment_tmpl)) // '_mem' // trim(adjustl(memchar))
 
    write(6,*) 'task mype = ', mype, ' process ', trim(increment_filename)

    call calc_increment()

  else

    write(6,*) 'no files to process for mpi task = ', mype

  endif

100 continue

  call mpi_barrier(mpi_comm_world, ierr)
  call mpi_finalize(ierr)

  stop
end program calc_increment_pmain
