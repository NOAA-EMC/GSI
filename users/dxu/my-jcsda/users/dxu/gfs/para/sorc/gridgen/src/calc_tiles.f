 subroutine calc_tiles (ijdim, dominate_cat, prcnt_each_cat, &
                        prcnt_each_group, num_groups, num_categories, &
                        cat_groups, max_tiles, max_tiles_grid, &
                        remaining_tot_tiles, tile_output, num_tiles) 

 implicit none

 integer                       :: cat
 integer, intent(in)           :: cat_groups(num_categories)
 integer                       :: count_grps
 integer, intent(in)           :: dominate_cat(ijdim)  ! dominate category
 integer                       :: ij, ii
 integer, intent(in)           :: ijdim
 integer                       :: index
 integer, intent(in)           :: max_tiles 
 integer, intent(in)           :: max_tiles_grid(ijdim)
 integer                       :: n
 integer, intent(in)           :: num_categories
 integer, intent(in)           :: num_groups   
 integer, intent(out)          :: num_tiles(ijdim)
 integer, intent(inout)        :: remaining_tot_tiles(ijdim)
 integer, allocatable          :: temp_cat(:)
 integer                       :: temp_groups(num_groups)
 integer, allocatable          :: temp_index(:)

 real*4, intent(in)            :: prcnt_each_cat(ijdim,num_categories)
 real*4, intent(in)            :: prcnt_each_group(ijdim,num_groups) 
 real*4                        :: maxvalue
 real*4                        :: temp_gpercent(num_groups)
 real*4                        :: tempsum
 real*4, allocatable           :: temp_pct(:)

 type tile_data
   sequence
   integer    :: category
   real*4     :: percent
 end type tile_data 

 type(tile_data), intent(out) :: tile_output(ijdim,max_tiles)

 tile_output%category  = 0  
 tile_output%percent   = 0.0
 num_tiles             = 0

 IJLOOP : do ij = 1, ijdim

!---------------------------------------------------------------
!  each grid point may have one or more categories.  multiple
!  categories may be spread between one or more groups.
!  count the number groups within the grid cell in order to
!  determine how to do the tiling. 
!---------------------------------------------------------------

   count_grps = 0

   do n = 1, num_groups
     if (prcnt_each_group(ij,n) > 0.0) then
       count_grps = count_grps + 1
     end if
   enddo

!---------------------------------------------------------------
! only one group, pick the dominate category calculated 
! in other routine.
!---------------------------------------------------------------
 
   TILING : if (count_grps == 1 .or. max_tiles_grid(ij) == 1) then

     num_tiles(ij)     = 1
     tile_output(ij,1)%category = dominate_cat(ij)
     tile_output(ij,1)%percent  = 100

!---------------------------------------------------------------
!  since the number of groups is less than or equal to the
!  maximum number of tiles we want, we choose a number of tiles
!  equal to the number of groups.  find the dominate category
!  in each group and set that to the tile category.  then
!  set the % for each tile equal to the group %.
!  this allows for tiles that span all the groups while 
!  preserving the group percentages in each grid box. 
!---------------------------------------------------------------

   elseif (count_grps <= max_tiles_grid(ij)) then

     index = 0

     do n = 1, num_groups

!  print*,'n/index/groups ',n,index,prcnt_each_group(ij,n)

       if (prcnt_each_group(ij,n) > 0.0) then

         maxvalue = 0.0
         index = index + 1

         do cat = 1, num_categories

           if (cat_groups(cat) == n ) then

             if (prcnt_each_cat(ij,cat) > maxvalue) then

               tile_output(ij,index)%category = cat
               tile_output(ij,index)%percent  = prcnt_each_group(ij,n)
 
               maxvalue = prcnt_each_cat(ij,cat)

             end if

           end if

         enddo

       end if  

     enddo
   
     num_tiles(ij) = index

!   print*,'2 or 3 ',tile_output(ij,:)

!---------------------------------------------------------------------
! number of groups exceeds the maximum number of tiles.  should
! work to ensure this situation is rare or expand the number of 
! tiles you want to use.
!
! in this case, find the predominate group types, then rescale the
! percentages so they sum to 100%.  Then pick the predominate
! category in each group as above.
!---------------------------------------------------------------------

   else

     num_tiles(ij) = max_tiles_grid(ij)

     temp_gpercent(1:num_groups) = prcnt_each_group(ij,1:num_groups)

     do ii = 1, num_groups
       temp_groups(ii) = ii
     enddo

     call sort(temp_gpercent, temp_groups, num_groups)

     tempsum = sum(temp_gpercent(1:max_tiles_grid(ij)))

     temp_gpercent(1:max_tiles_grid(ij)) = temp_gpercent(1:max_tiles_grid(ij)) / tempsum

     do n = 1, max_tiles_grid(ij)

         maxvalue = 0.0

         do cat = 1, num_categories

           if (cat_groups(cat) == temp_groups(n)) then

             if (prcnt_each_cat(ij,cat) > maxvalue) then

               tile_output(ij,n)%category = cat
               tile_output(ij,n)%percent  = temp_gpercent(n) * 100.0

               maxvalue = prcnt_each_cat(ij,cat)

             end if

           end if

         enddo

     enddo

    print*,'more than max at i/j ',ij,tile_output(ij,1:max_tiles)

   end if TILING

 enddo IJLOOP

!---------------------------------------------------------------------
! sort tile_output array based on % of each category
!---------------------------------------------------------------------

 do ij = 1, ijdim

   if (num_tiles(ij) > 1) then

      allocate(temp_cat(num_tiles(ij)))
      allocate(temp_index(num_tiles(ij)))
      allocate(temp_pct(num_tiles(ij)))

      do ii = 1, num_tiles(ij)     
        temp_index(ii) = ii
        temp_cat(ii)   = tile_output(ij,ii)%category
        temp_pct(ii)   = tile_output(ij,ii)%percent
      enddo

      call sort(temp_pct, temp_index, num_tiles(ij))

      do ii = 1, num_tiles(ij)     
        tile_output(ij,ii)%category = temp_cat(temp_index(ii))
        tile_output(ij,ii)%percent  = temp_pct(ii)
      enddo
         
      deallocate(temp_pct)
      deallocate(temp_cat)
      deallocate(temp_index)

   end if

 enddo

!---------------------------------------------------------------------
! reduce the number of tiles remaining for further fields.
!---------------------------------------------------------------------

 do ij = 1, ijdim
   remaining_tot_tiles(ij) = remaining_tot_tiles(ij) / num_tiles(ij)
   remaining_tot_tiles(ij) = max(remaining_tot_tiles(ij),1)
 enddo

 end subroutine calc_tiles
