module namelist_def

  use kinds

  implicit none
  
  ! Define global variables

  character(len=500)                             :: analysis_filename                       = 'NOT USED'
  character(len=500)                             :: firstguess_filename                     = 'NOT USED'
  character(len=500)                             :: increment_filename                      = 'fv3_increment.nc'
  character(len=500)                             :: datapath                                = './'
  logical                                        :: debug                                   = .false.
  
end module namelist_def
