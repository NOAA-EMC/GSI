subroutine skindepth(obstype,sd_rad)
!
! abstract: Get skin depth (instrument dependent). Ideally, a skin-depth model calculates the channel dependent sd
!
! program history log:
!   2011-04-08  li


  use kinds, only: r_kind
  implicit none
  character(10), intent(in) :: obstype
  real(kind=r_kind), intent(out) :: sd_rad

  sd_rad = 0.000015_r_kind
  if ( obstype == 'amsre' ) then
    sd_rad = 0.03_r_kind
  elseif ( obstype == 'amsua' .or. obstype == 'amsub' .or.  obstype == 'ssmis' .or.  obstype == 'ssmi' .or. &
           obstype == 'mhs' .or.  obstype == 'msu' .or.  obstype == 'hsb' ) then
    sd_rad = 0.001_r_kind
  endif

end subroutine skindepth
