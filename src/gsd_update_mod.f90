module gsd_update_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    gsd_update_mod module for updating surface, soil, moisture from GSD for RR
!   prgmmr: Hu              org: gsd                date: 2012-01-12
!
! abstract: module for updating surface, soil, moisture from GSD for RR
!
! program history log:
!   2012-01-12  parrish
!
! subroutines included:
!   sub gsd_limit_ocean_q   - limits to analysis increments over oceans 
!
! Variable Definitions:

  implicit none

! set default to private
  private
! set subroutines to public
  public :: gsd_limit_ocean_q
! set passed variables to public

contains

subroutine gsd_limit_ocean_q(qinc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsd_limit_ocean_q Rlimits to analysis increments over oceans
!   prgmmr: Hu          org: GSD                date: 2011-08-31
!
! abstract:  This routine does the following things:
! 
! 
! program history log:
!   2011-08-31  Hu - original code
!
!   input argument list:
!    qinc : moisture analysis increment
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use jfunc, only:  qsatg,qoption
  use constants, only: zero,one,fv,rd_over_cp_mass,one_tenth,deg2rad, rad2deg, pi
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: ges_tsen,ges_prsl,nfldsig,ntguessig
  use guess_grids, only: ges_q,isli
  
  implicit none
  
! Declare passed variables
  integer(i_kind) is_q
  real(r_kind),dimension(lat2,lon2,nsig), intent(inout) :: qinc

! Declare local variables
  logical ice
  integer(i_kind) :: i,j,k,iderivative,it
  real(r_kind),allocatable,dimension(:,:,:):: rhgues
  real(r_kind) :: qinc_rh


! Compute saturation specific humidity.   

  iderivative = 0
  if(qoption == 1 )then
      iderivative = 1 
  else
      iderivative = 2
  end if

  ice=.true.
  call genqsat(qsatg,ges_tsen(1,1,1,ntguessig),ges_prsl(1,1,1,ntguessig),lat2,lon2,nsig,ice,iderivative)
  allocate(rhgues(lat2,lon2,nsig))

  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           rhgues(i,j,k)=ges_q(i,j,k,ntguessig)/qsatg(i,j,k)
        end do
     end do
  end do

  do it=1,nfldsig
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              if (isli(i,j,1) ==  0) then
                 qinc_rh=qinc(i,j,k)/qsatg(i,j,k)
! -- limit pseudo-RH change over water to +/- 0.1
                 if( k <= 10) then
                    qinc_rh=max(-0.1_r_kind,min(0.1_r_kind,qinc_rh))
                 elseif(  k >= 11.and.k <=18 ) then
                    qinc_rh=max(-0.2_r_kind,min(0.2_r_kind,qinc_rh))
                 endif
! -- Limit further drying out over water and near surface.
                 if(rhgues(i,j,k) < 0.6_r_kind .and. k <=4 .and. qinc_rh < zero) then
                    qinc_rh=qinc_rh*rhgues(i,j,k)/1.0_r_kind
                 endif
                 qinc(i,j,k)=qinc_rh*qsatg(i,j,k) 
              else
                 qinc(i,j,k)=qinc(i,j,k) 
              end if   ! isli(i,j,1)
           end do
        end do
     end do
  end do

  deallocate(rhgues)
end subroutine gsd_limit_ocean_q 


end module gsd_update_mod
