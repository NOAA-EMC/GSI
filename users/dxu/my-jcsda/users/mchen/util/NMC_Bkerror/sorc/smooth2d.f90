 subroutine smooth2d(grd)
   use variables,only: nlat,nlon
   use kinds,only: r_kind
   implicit none

   real(r_kind),dimension(nlat,nlon):: grd
   real(r_kind),dimension(nlat,0:nlon+1):: grd2
   real(r_kind) corn,cent,side,temp,c2,c3,c4
   integer i,j

   corn=0.3_r_kind
   cent=1.0_r_kind
   side=0.5_r_kind
   c4=4.0_r_kind
   c3=3.0_r_kind
   c2=2.0_r_kind

! first load grd2 which is used in computing the smoothed fields
  do j=1,nlon
    do i=1,nlat
      grd2(i,j)=grd(i,j)
    end do 
  end do

! load wrapper rows
  do i=1,nlat
    grd2(i,0)=grd(i,nlon)
    grd2(i,nlon+1)=grd(i,1)
  end do

   do j=1,nlon
     do i=2,nlat-1
! Southern border smoothing
!       if (i.eq.1) then
!         temp = cent*grd2(i,j) + side*(grd2(i+1,j) + &
!            grd2(i,j+1) + grd2(i,j-1)) + &
!            corn*(grd2(i+1,j+1) + grd2(i+1,j-1))
!         grd(i,j) = temp/(cent + c3*side + c2*corn)
! Northern border smoothing
!       elseif (i.eq.nlat) then
!         temp = cent*grd2(i,j) + side*( &
!            grd2(i-1,j) + grd2(i,j+1) + grd2(i,j-1)) + &
!            corn*(grd2(i-1,j-1) + grd2(i-1,j+1))
!         grd(i,j) = temp/(cent + c4*side + c4*corn)
! Interior smoothing
!       else
         temp = cent*grd2(i,j) + side*(grd2(i+1,j) + &
            grd2(i-1,j) + grd2(i,j+1) + grd2(i,j-1)) + &
            corn*(grd2(i+1,j+1) + grd2(i+1,j-1) + grd2(i-1,j-1) + &
            grd2(i-1,j+1))
         grd(i,j) = temp/(cent + c4*side + c4*corn)
!       end if
     end do
   end do
 
!  do j=1,nlon
!    grd(1,j)=grd(2,j)
!    grd(nlat,j)=grd(nlat-1,j)
!  end do

  return
end subroutine smooth2d

