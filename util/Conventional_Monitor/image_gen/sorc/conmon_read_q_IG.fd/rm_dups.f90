!-------------------------------------------------------
! rm_dups
!
!   This subroutine removes duplicate data by comparing
!   lat, lin, time, and pressure values.  Differences
!   of < 10.0e-5 are assumed to be matches.
!-------------------------------------------------------

subroutine rm_dups( duparr, nn, mm, ilat, ilon, ipress, itime, iweight, nndup )

   implicit none

   !-------------
   !  interface
   !
   real(4), intent(inout), dimension(mm,nn) :: duparr
   integer, intent(in)                      :: nn, mm, ilat, ilon, ipress
   integer, intent(in)                      :: itime, iweight
   integer, intent(out)                     :: nndup

   !-------------
   !  local vars
   !
   integer ii, jj
   real    dlat, dlon, dtime, dpress, match

   data match / 10.0e-5 /


   nndup=0
   do ii = 1, nn
      do jj = ii+1, nn

         dlat   = abs( duparr( ilat,   ii) - duparr( ilat,   jj ))
         dlon   = abs( duparr( ilon,   ii) - duparr( ilon,   jj ))
         dtime  = abs( duparr( itime,  ii) - duparr( itime,  jj ))
         dpress = abs( duparr( ipress, ii) - duparr( ipress, jj ))

         if (dlat < match .and. dlon < match .and. dtime < match &
             .and. dpress < match ) then 

            duparr( iweight, ii )= -1.0
            nndup = nndup + 1

         endif
      enddo
   enddo

   write(*,*) 'num dups found by rm_dups is ', nndup
   return
end 
