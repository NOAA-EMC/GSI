module write_incr
!$$$ module documentation block
!           .      .    .                                       .
! module:   write_incr 
!   prgmmr: Martin       org:                 date: 2019-09-04
!
! abstract: This module contains routines which write out  
!           the atmospheric increment rather than analysis
!
! program history log:
!   2019-09-04 Martin    Initial version.  Based on ncepnems_io
!
! Subroutines Included:
!   sub write_fv3_increment - writes netCDF increment for FV3 global model
!
!$$$ end documentation block

  implicit none
  private
  public write_fv3_increment

  interface write_fv3_increment
     module procedure write_fv3_inc_
  end interface

contains

  subroutine write_fv3_inc_ (grd,sp_a,filename,mype_out,gfs_bundle,ibin)

  end subroutine write_fv3_inc_

end module write_incr
