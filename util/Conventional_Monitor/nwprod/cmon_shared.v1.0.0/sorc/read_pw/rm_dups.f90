!-------------------------------------------------------
! rm_dups
!
!   This subroutine removes duplicate data by comparing
!   lat, lin, time, and pressure values.  Differences
!   of < 10.0e-5 are assumed to be matches.
!-------------------------------------------------------

subroutine rm_dups(duparr,n,m,ilat,ilon,ipress,itime,iweight,nndup)

   implicit none

   real(4),intent(inout),dimension(m,n) :: duparr

!   integer ihash(2*n)
!   integer ifn
   integer n,m,ilat,ilon,iweight,nndup,i
   integer itime,jj,ipress
   real dlat,dlon,dtime,dpress,match

   data match / 10.0e-5 /


   nndup=0
   do i = 1, n
      do jj=i+1,n
         dlat=abs(duparr(ilat,i)-duparr(ilat,jj))
         dlon=abs(duparr(ilon,i)-duparr(ilon,jj))
         dtime=abs(duparr(itime,i)-duparr(itime,jj))
         dpress=abs(duparr(ipress,i)-duparr(ipress,jj))

         if (dlat < match .and. dlon < match .and. dtime < match &
             .and. dpress < match ) then 

            duparr(iweight,i)=-1.0
            nndup=nndup+1
         endif
      enddo
   enddo

   write(*,*) 'num dups found by rm_dups is ',nndup
   return
end 
