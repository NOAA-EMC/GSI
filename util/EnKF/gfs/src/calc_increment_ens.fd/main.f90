program calc_increment_main

  use namelist_def, only : read_namelist, write_namelist
  use namelist_def, only : analysis_filename, firstguess_filename, increment_filename
  use namelist_def, only : datapath
  use namelist_def, only : debug
  use namelist_def, only : max_vars, incvars_to_zero
  use calc_increment_interface, only: calc_increment

  implicit none

  integer :: i

  call read_namelist
  call write_namelist

  analysis_filename = trim(adjustl(datapath)) // trim(adjustl(analysis_filename))
  firstguess_filename = trim(adjustl(datapath)) // trim(adjustl(firstguess_filename))
  increment_filename = trim(adjustl(datapath)) // trim(adjustl(increment_filename))

  write(6,*) 'DATAPATH        = ', trim(datapath)
  write(6,*) 'ANALYSIS FILE   = ', trim(analysis_filename)
  write(6,*) 'FIRSTGUESS FILE = ', trim(firstguess_filename)
  write(6,*) 'INCREMENT FILE  = ', trim(increment_filename)
  write(6,*) 'DEBUG           = ', debug

  do i=1,max_vars
    if ( trim(incvars_to_zero(i)) /= 'NONE' ) then
      write(6,*) 'INCVARS_TO_ZERO = ', trim(incvars_to_zero(i))
    else
      cycle
    endif
  enddo

  call calc_increment(0)

end program calc_increment_main
