!This module bins observations, as required by either the
!Desroziers method or Hollingsworth-Lonnberg method
!Kristen Bathmann
!5-2015
module pairs

use kinds, only: r_kind
use obs_tools, only: dist

implicit none
public :: make_pairs

contains
subroutine make_pairs(ges_locs,anl_loc,ges_times,anl_time,Tg,dist_threshold,time_threshold,obs_pairs,n_pair)
!For a given analysis omg, this subroutine searches through an array of
!background omg's and identifies all that are within a certain distance and time 
!of the analysis omg
!This subroutine is used with Desroziers method.
!Kristen Bathmann
!5-2015
implicit none
real(r_kind), dimension(:),intent(in):: anl_loc  !location of analysis omg (lat,lon)
real(r_kind),intent(in):: anl_time               !time of analysis omg (minutes)
real(r_kind),dimension(:,:),intent(in)::ges_locs !locations of background omg's (lat,lon)
real(r_kind),dimension(:),intent(in):: ges_times !times of background omg's (minutes)
real(r_kind), intent(in):: time_threshold        !minutes, max time between the omg's
real(r_kind), intent(in):: dist_threshold        !km, max distance between the omg's
integer, intent(out):: obs_pairs                 !indicies of ges that correspond to pairs
integer,intent(in):: Tg                          !length of ges
integer,intent(out):: n_pair                     !number of pairs found
real(r_kind),dimension(2):: p1,p2
integer:: g
real(r_kind):: d1
real(r_kind)::dt
obs_pairs=0
n_pair=0
do g=1,Tg
   dt=abs(ges_times(g)-anl_time)
   if (dt<=time_threshold) then
      p1=ges_locs(g,:)
      p2=anl_loc(:)
      d1=dist(p1,p2)
      if (d1<=dist_threshold) then
         n_pair=n_pair+1
         obs_pairs=g
         return
     end if
   end if
end do

end subroutine make_pairs
subroutine make_pairs_hl(ges_locs,current_loc,ges_times,current_time,Tg,dist_threshold, time_threshold,num_bins, obs_pairs, n_pair)
!For a given background omg, this subroutine searches through an array of
!background omg's and bins the pairs based on distance between the two
!This subroutine is used with the Hollingsworth-Lonnberg method.
!Kristen Bathmann
!9-2016
implicit none
real(r_kind), dimension(:,:), intent(in):: ges_locs
real(r_kind), dimension(:), intent(in):: current_loc
real(r_kind), dimension(:), intent(in):: ges_times
real(r_kind), intent(in):: current_time
integer, intent(in):: Tg
real(r_kind), dimension(:), intent(in):: dist_threshold
integer, intent(in):: num_bins
real(r_kind), intent(in):: time_threshold  !minutes, max time between the omg's
integer, dimension(:,:), intent(out):: obs_pairs
integer, dimension(:), intent(out):: n_pair
real(r_kind),dimension(2):: p1,p2
integer:: g, dis
real(r_kind):: d1
real(r_kind)::dt

n_pair=0
obs_pairs=0
do g=1,Tg
   dt=abs(ges_times(g)-current_time)
   if (dt<=time_threshold) then
      p1=ges_locs(g,:)
      p2=current_loc(:)
      d1=dist(p1,p2)
      do dis=1,num_bins,2
         if (dis==1) then
            if((d1<=dist_threshold(dis))) then
              n_pair(dis)=n_pair(dis)+1
              obs_pairs(n_pair(dis),dis)=g
            end if
         else
            if ((dist_threshold(dis-1)<=d1).and.(d1<dist_threshold(dis))) then
              n_pair(dis-1)=n_pair(dis-1)+1
              obs_pairs(n_pair(dis-1),dis-1)=g
            end if
         end if
      end do
   end if
end do

end subroutine make_pairs_hl
end module pairs


