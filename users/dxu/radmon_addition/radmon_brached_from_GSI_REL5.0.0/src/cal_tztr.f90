 subroutine cal_tztr(dt_warm,c_0,c_d,w_0,w_d,zc,zw,z,tztr)
!
! abstract: calculate d(Tz)/d(Ts) with T-Profile info from NSST Model
!
!   prgmmr: li, xu           org: np23                date: 2011-04-08
! input variables
!
! dt_warm :       diurnal warming amount at the surface
! xz      :       DTL depth                           (M)
! c_0     :       coefficint 1 to calculate d(Tc)/d(Ts)
! c_d     :       coefficint 2 to calculate d(Tc)/d(Ts)
! w_0     :       coefficint 1 to calculate d(Tw)/d(Ts)
! w_d     :       coefficint 2 to calculate d(Tw)/d(Ts)
!
! output variables
!
! tztr     :      d(Tz)/d(Tr)

  use kinds, only: r_kind
  use constants, only: one,half,zero
  use radinfo, only: fac_dtl,fac_tsl
  real(kind=r_kind), intent(in)  :: dt_warm,c_0,c_d,w_0,w_d,zc,zw,z
  real(kind=r_kind), intent(out) :: tztr
! local variables
  real(kind=r_kind) :: c1,c2

  c1 = one-fac_dtl*w_0+fac_tsl*c_0
  c2 = one+fac_tsl*c_0

  tztr = one

  if ( dt_warm > zero .and.  c1 /= zero ) then
     if ( z <= zc  ) then
       tztr = (one+z*(fac_dtl*w_d-fac_tsl*c_d))/c1
     elseif ( z > zc .and. z < zw ) then
       tztr = (one+fac_tsl*c_0+z*fac_dtl*w_d)/c1
     endif
   elseif ( dt_warm == zero .and. c2 /= zero ) then
     if ( z <= zc ) then
       tztr = (one-z*fac_tsl*c_d)/c2
     endif
   endif

   if ( tztr <= one .and. tztr > half ) then
     tztr = tztr
   else
!    write(*,'(a,2I2,2F12.6,F9.3,5F12.6,F8.3,F9.6,F8.3)') ' cal_tztr : ',fac_dtl,fac_tsl,c1,c2,dt_warm,c_0,c_d,w_0,w_d,zc,zw,z,tztr
   endif

 end subroutine cal_tztr


