module pairs

use kinds, only: r_kind
use obs_tools, only: dist

implicit none
public :: make_pairs

contains
subroutine make_pairs(ges_locs,anl_loc,ges_times,anl_time,Tg,obs_pairs,Lt)
!For a given analysis omg, this subroutine searches through an array of
!background omg's and identifies all that are within a certain distance and time 
!of the analysis omg
implicit none
real(r_kind), dimension(:),intent(in):: anl_loc  !location of analysis omg (lat,lon)
real(r_kind),intent(in):: anl_time               !time of analysis omg (minutes)
real(r_kind),dimension(:,:),intent(in)::ges_locs !locations of background omg's (lat,lon)
real(r_kind),dimension(:),intent(in):: ges_times !times of background omg's (minutes)
integer, dimension(:), intent(out):: obs_pairs   !indicies of ges that correspond to pairs
integer,intent(in):: Tg                          !length of ges
integer,intent(out):: Lt                         !number of pairs found
real(r_kind),dimension(2):: p1,p2
integer:: g
real(r_kind):: d1
real(r_kind)::dt
real(r_kind), parameter:: time_threshold=60.0_r_kind !minutes, max time between the omg's
real(r_kind), parameter:: dist_threshold=25.0_r_kind !km, max distance between the omg's

Lt=0
do g=1,Tg
   dt=abs(ges_times(g)-anl_time)
      if (dt<=time_threshold) then
         p1=ges_locs(g,:)
         p2=anl_loc(:)
         d1=dist(p1,p2)
         if (d1<=dist_threshold) then
            Lt=Lt+1
            obs_pairs(Lt)=g
        end if
   end if
end do

end subroutine make_pairs

end module pairs


