subroutine setupyobs()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupyobs
!   prgmmr:      tremolet
!
! abstract:  Setup observation vectors (ie the "y" the in "H(x)-y" )
!            In 3D-Var, it contains all observations, in 4D-Var, each
!            y contains all the observations in a given time slot.
!
! program history log:
!   2007-04-17  tremolet - initial code
!   2009-01-08  todling  - remove reference to ozohead
!   2009-03-05  meunier  - add pointer to lagrangean data
!   2009-08-11  lueken   - updated documentation
!   2010-04-22  tangborn - updated reference to co
!   2010-07-10  todling  - add aerosols pointer
!   2010-10-15  pagowski  - add pm2_5 pointer
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use kinds, only: i_kind
use obsmod, only: pshead, thead, whead, qhead, spdhead, srwhead, rwhead, &
                & dwhead, ssthead, radhead, pcphead, pwhead, gpshead, &
                & ozhead, o3lhead, tcphead, laghead, colvkhead, aerohead, &
                & aerolhead, pm2_5head, yobs
use gsi_4dvar, only: nobs_bins
implicit none

! Declare local variables
integer(i_kind) :: ii

!******************************************************************************

do ii=1,nobs_bins
   yobs(ii)%t   => thead(ii)%head
   yobs(ii)%pw  => pwhead(ii)%head
   yobs(ii)%q   => qhead(ii)%head
   yobs(ii)%w   => whead(ii)%head
   yobs(ii)%srw => srwhead(ii)%head
   yobs(ii)%dw  => dwhead(ii)%head
   yobs(ii)%rw  => rwhead(ii)%head
   yobs(ii)%spd => spdhead(ii)%head
   yobs(ii)%oz  => ozhead(ii)%head
   yobs(ii)%o3l => o3lhead(ii)%head
   yobs(ii)%ps  => pshead(ii)%head
   yobs(ii)%gps => gpshead(ii)%head
   yobs(ii)%sst => ssthead(ii)%head
   yobs(ii)%rad => radhead(ii)%head
   yobs(ii)%pcp => pcphead(ii)%head
   yobs(ii)%tcp => tcphead(ii)%head
   yobs(ii)%lag => laghead(ii)%head
   yobs(ii)%colvk=> colvkhead(ii)%head
   yobs(ii)%aero=> aerohead(ii)%head
   yobs(ii)%aerol=>aerolhead(ii)%head
   yobs(ii)%pm2_5=>pm2_5head(ii)%head
end do

return
end subroutine setupyobs
