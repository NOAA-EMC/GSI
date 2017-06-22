module data
  implicit none

  private
  public :: data_t
  public :: data_ptr

  integer, parameter,public :: max_rdiag_reals = 25

  !--------------------------------------------------
  !  index numbers into the rdiagbuf structure
  !  (see setupt.f90 for more info)
  !
  integer, parameter, public :: idx_obs_type    = 1     ! obs type
  integer, parameter, public :: idx_obs_subtype = 2     ! obs subtype
  integer, parameter, public :: idx_obs_lat     = 3     ! obs latitude
  integer, parameter, public :: idx_obs_lon     = 4     ! obs longitude
  integer, parameter, public :: idx_stn_elev    = 5     ! stn elevation
  integer, parameter, public :: idx_pres        = 6     ! obs pressure (hPa)
  integer, parameter, public :: idx_hgt         = 7     ! obs height (meters)
  integer, parameter, public :: idx_time        = 8     ! obs time (hrs relative
                                                        !       to analysis time)
  integer, parameter, public :: idx_iqc         = 9     ! prepbufr qc or event mark
  integer, parameter, public :: idx_var_jb      = 10    ! non-linear qc param
  integer, parameter, public :: idx_iuse        = 11    ! read_prepbufr data usage flag
  integer, parameter, public :: idx_anl_use     = 12    ! analysis usage flag
  integer, parameter, public :: idx_rwgt        = 13    ! non-linear qc relative weight
  integer, parameter, public :: idx_err_input   = 14    ! prepbufr invers obs error
                                                        !                        (m/s)**-1
  integer, parameter, public :: idx_errinv      = 15    ! read_prepbufr invers obs error
                                                        !                        (m/s)**-1
  integer, parameter, public :: idx_errinv_fnl  = 16    ! final invers obs error (m/s)**-1
  integer, parameter, public :: idx_iuob        = 17    ! u wind component observation
  integer, parameter, public :: idx_udiff       = 18    ! u obs-ges used in analysis (m/s)
  integer, parameter, public :: idx_uob_ugesin  = 19    ! u obs-ges w/o bias correction
                                                        !                            (m/s)
  integer, parameter, public :: idx_ivob        = 20    ! v wind component observation
  integer, parameter, public :: idx_vdiff       = 21    ! v obs-ges used in analysis (m/s)
  integer, parameter, public :: idx_vob_vgesin  = 22    ! v obs-ges w/o bias correction


  ! Data is stored in data_t
  type :: data_t
     character(8)       :: stn_id
     real,dimension(25) :: rdiag
  end type data_t

  ! A container for storing data_t pointers
  type :: data_ptr
     type(data_t), pointer :: p
  end type data_ptr

end module data
