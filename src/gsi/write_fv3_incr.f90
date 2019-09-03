module write_fv3_incr

  interface write_fv3_increment
     module procedure write_inc_
  end interface

contains

subroutine write_inc_ (grd,sp_a,filename,mype_out,gfs_bundle,ibin)

end subroutine write_inc_

end module write_fv3_incr
