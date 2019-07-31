module namelist_def

  use kinds

  implicit none
  
  ! Define global variables

  character(len=500)                             :: analysis_filename                       = 'NOT USED'
  character(len=500)                             :: firstguess_filename                     = 'NOT USED'
  character(len=500)                             :: increment_filename                      = 'fv3_increment.nc'
  character(len=500)                             :: datapath                                = './'
  logical                                        :: debug,zero_mpinc                        = .false.
  logical                                        :: write_delz_inc                          = .false.
! 11 GFDLMP, 99 ZCMP, < 0 no microphysics vars in increment file
  integer                                        :: imp_physics = 11
  
end module namelist_def
