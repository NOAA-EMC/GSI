program calc_increment

  use kinds
!!use namelist_def, only : datapath
  use namelist_def, only : analysis_filename, firstguess_filename, increment_filename, debug,&
   zero_mpinc, imp_physics
  use calc_increment_interface

  implicit none

  character(len=10) :: bufchar

  call getarg(1, analysis_filename)
  call getarg(2, firstguess_filename)
  call getarg(3, increment_filename)
  call getarg(4, bufchar)
  read(bufchar,'(L)') debug
  call getarg(5, bufchar)
  read(bufchar,'(L)') zero_mpinc
  call getarg(6, bufchar)
  read(bufchar,'(i5)') imp_physics

!!write(6,*) 'DATAPATH        = ', trim(datapath)
  write(6,*) 'ANALYSIS FILENAME   = ', trim(analysis_filename)
  write(6,*) 'FIRSTGUESS FILENAME = ', trim(firstguess_filename)
  write(6,*) 'INCREMENT FILENAME  = ', trim(increment_filename)
  write(6,*) 'DEBUG           = ', debug
  write(6,*) 'ZERO_MPINC      = ', zero_mpinc
  write(6,*) 'IMP_PHYSICS     = ', imp_physics
  call calculate_increment()

end program calc_increment
