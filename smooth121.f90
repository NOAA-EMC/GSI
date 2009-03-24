subroutine smooth121(npassh,npassv,lat1,lon1,nlev,fld0,fld2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smooth121                        apply 1-2-1 smoother
!   prgmmr: treadon          org: np20                date: 2003-12-18
!
! abstract:  This routine applies a 1-2-1 smoother to a 3-dimensional
!            field.  The smoother may be applied in either the 
!            horizontal or veritical or both.  The number of passes
!            in each plane is controlled via input parameters.
!
! program history log:
!   2003-12-18 treadon
!   2004-06-21 treadon - update documentation
!
!   input argument list:
!     npassh - number of horizontal passes of smoother (0=no filter)
!     npassv - number of vertical passes of smoother (0=no filter)
!     lat1   - number of latitudes in sub-domain
!     lon1   - number of longitudes in sub-domain
!     nlev   - number of vertical levels in data field 
!     fld0   - data field to be filtered (smoothed)
!
!   output argument list:
!     fld2   - data field following filtering (smoothing)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,quarter
  implicit none

  real(r_kind),parameter:: eigth=0.125_r_kind

  integer(i_kind) npassh,npassv,lat1,lon1,nlev,i,j,k,ipass,ip1,im1
  integer(i_kind) kp1,km1,jm1,jp1
  real(r_kind),dimension(lat1,lon1,nlev):: fld0,fld2,work

! Apply 1-2-1 smoother to guess fld in horizontal
  do k=1,nlev
     do j=1,lon1
        do i=1,lat1
           work(i,j,k)=fld0(i,j,k)
        end do
     end do
  end do
  do ipass = 1,npassh
     do k = 1,nlev
        do j = 1,lon1
           do i = 1,lat1
              ip1=min(i+1,lat1); jp1=min(j+1,lon1)
              im1=max(1,i-1); jm1=max(1,j-1)
              fld2(i,j,k)=half*work(i,j,k) + eigth * ( &
                   work(ip1,j,k) + work(im1,j,k) + &
                   work(i,jp1,k) + work(i,jm1,k) )
           end do
        end do
     end do
     do k=1,nlev
        do j=1,lon1
           do i=1,lat1
              work(i,j,k)=fld2(i,j,k)
           end do
        end do
     end do
  end do


! Apply 1-2-1 smoother in vertical
  do ipass = 1,npassv
     do k = 1,nlev
        kp1 = min(k+1,nlev); km1=max(1,k-1)
        do j = 1,lon1
           do i = 1,lat1
              fld2(i,j,k)= half*work(i,j,k) + quarter * ( &
                   work(i,j,kp1) + work(i,j,km1) )
           end do
        end do
     end do
     do k=1,nlev
        do j=1,lon1
           do i=1,lat1
              work(i,j,k)=fld2(i,j,k)
           end do
        end do
     end do
  end do

! End of routine
   return
 end subroutine smooth121
