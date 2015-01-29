 subroutine interp_tiles(lon_11_src, lat_11_src, &
                        srcdat, isrc, dlon_src, dlat_src, & 
                        num_categories, num_groups, &
                        water_category, default_category, &
                        tile_threshold, cat_groups, &
                        out_cat, out_group, dominate_cat,  &
                        count_land, ijsave, isave, jsave, &
                        lat_mdl, lon_mdl, jsrc_start, jsrc_end)

 use ll2xy_utils, only       : ll2xy_egrid

 use program_setup,  only    : imdl,         &
                               jmdl,         &
                               dx_mdl,       &
                               dy_mdl,       &
                               centlat_mdl,  &
                               centlon_mdl,  &
                               domain_type  

 use gribit, only            : kgds_mdl
             
 implicit none

 integer                    :: cat
 integer                    :: category
 integer, intent(in)        :: cat_groups(num_categories)
 integer                    :: count_cat(count_land,num_categories) ! for each land category
 integer                    :: count_group(count_land,num_groups)
 integer, intent(in)        :: count_land
 integer, intent(in)        :: default_category
 integer, parameter         :: default_group = 1
 integer, intent(out)       :: dominate_cat(count_land)
 integer                    :: group, grp
 integer                    :: i, j, nret, ii, ij, jj, iii, jjj, istart, iend
 integer                    :: isrc_start, isrc_end
 integer*2, intent(in)      :: isave(count_land), jsave(count_land)
 integer, intent(in)        :: ijsave(imdl,jmdl)
 integer                    :: jstart, jend, spiral_rad, krad, igrid, jgrid
 integer, intent(in)        :: isrc ! i-dimension of data source grid
 integer                    :: jsrc
 integer                    :: jsrc_start, jsrc_end, nearest_i,nearest_j, itmp(1),jtmp(1)
 integer, intent(in)        :: num_categories
 integer, intent(in)        :: num_groups 
 integer                    :: pred_group(1)
 integer*1, intent(in)      :: srcdat(isrc,jsrc_start:jsrc_end)
 integer                    :: total_count, total_count_sav
 integer, intent(in)        :: water_category

 real, intent(in)           :: dlon_src, dlat_src  ! lat/lon increment of the source grid
 real, intent(in)           :: lon_11_src, lat_11_src ! lon/lat of point (1,1) of
                                                      ! data source grid
 real, intent(in)           :: lat_mdl(count_land), lon_mdl(count_land)
 real                       :: minlon, maxlon
 real*4                     :: maxcat
 real*4, intent(out)        :: out_cat(count_land,num_categories) 
 real*4, intent(out)        :: out_group(count_land,num_groups)   
 real                       :: percent_cat
 real, intent(in)           :: tile_threshold
 real, allocatable          :: lats_src(:), lons_src(:)
 real, allocatable          :: dum(:), ypts(:)
 real                       :: srclat(1),srclon(1)

!------------------------------------------------------------------
! loop over high-res source grid.  find the nearest neighbor
! model point.  then store a count of the each cat and group
! at each model point.
!------------------------------------------------------------------

 out_cat   = 0.0
 out_group = 0.0
 count_cat   = 0
 count_group = 0

 if (trim(domain_type) == 'egrid') then
   maxlon = min(maxval(lon_mdl) + 2.0,180.0)    ! 2 degree cushion
   minlon = max(minval(lon_mdl) - 2.0,-180.0)
! note: crude method for determining istart/iend
! will not work well for domains that cross lon_11.
   isrc_end    = int( (maxlon - lon_11_src) / dlon_src + 1.0)
   isrc_start  = nint( (minlon - lon_11_src) / dlon_src + 1.0)
   do j = jsrc_start, jsrc_end
! try to limit the i loop bounds based on the longitude ranges
   do i = isrc_start, isrc_end
      category = srcdat(i,j)
      if (category /= water_category) then
        srclat(1) = lat_11_src + (j-1)*dlat_src
        srclon(1) = lon_11_src + (i-1)*dlon_src
        call ll2xy_egrid(srclat, srclon, imdl, jmdl, &
                         centlat_mdl, centlon_mdl,   &
                         -dx_mdl, dy_mdl, &
                          1, 1, itmp, jtmp)
        nearest_i = itmp(1)
        nearest_j = jtmp(1)
        if (nearest_i >= 1 .and. nearest_i <= imdl .and. &
            nearest_j >= 1 .and. nearest_j <= jmdl) then
          ij = ijsave(nearest_i,nearest_j)
          if (ij > 0) then    ! model points for this task  
            count_cat(ij,category) = count_cat(ij,category) + 1
            group                  = cat_groups(category)
            count_group(ij,group)  = count_group(ij,group) + 1
          end if
        end if
      end if
   enddo
   enddo
 elseif (trim(domain_type) == "gaussian") then
   allocate (lats_src(jsrc_start:jsrc_end))
   allocate (lons_src(jsrc_start:jsrc_end))
   lons_src = 0.0
   do j = jsrc_start, jsrc_end
     lats_src(j) = lat_11_src + (j-1)*dlat_src
   enddo
   allocate (dum(jsrc_start:jsrc_end))
   allocate (ypts(jsrc_start:jsrc_end))
   jsrc = jsrc_end - jsrc_start + 1
   call gdswiz(kgds_mdl,-1,jsrc,-999.9,dum,ypts,lons_src,lats_src, &
               nret, 0, dum, dum)
   deallocate (dum, lons_src, lats_src)
   do j = jsrc_start, jsrc_end
   do i = 1, isrc
     category = srcdat(i,j)
     if (category /= water_category) then
       nearest_j = nint(ypts(j))
       srclon(1) = lon_11_src + (i-1)*dlon_src
       nearest_i = nint(srclon(1) / dx_mdl + 1.0)
       if (nearest_i > imdl) then
         nearest_i = nearest_i - imdl
       else if (nearest_i < 1) then
         nearest_i = nearest_i + imdl
       end if
       if (nearest_j >= 1 .and. nearest_j <= jmdl) then
         ij = ijsave(nearest_i,nearest_j)
         if (ij > 0) then    ! model points for this task  
           count_cat(ij,category) = count_cat(ij,category) + 1
           group = cat_groups(category)
           count_group(ij,group) =  count_group(ij,group) + 1
         end if
       end if
     end if
   enddo
   enddo
   deallocate (ypts)
 else
   print*,'- ROUTINE INTERP_TILES: UNRECOGNIZED DOMAIN TYPE'
   stop
 end if

 IJLOOP : do ij = 1, count_land

!  ------------------------------------------------------------------
!  if we found valid categories in the box, calculate the percentage
!  of each.  otherwise use a default category.
!
!  do not include any categories that represent less than the
!  tile threshold in this percentage.
!  ------------------------------------------------------------------

   total_count     = sum(count_cat(ij,:))
   total_count_sav = total_count

   CALC_TILES : if (total_count > 0) then

     do cat = 1, num_categories

       percent_cat = float(count_cat(ij,cat)) / float(total_count_sav)

       if (percent_cat > 0.0 .and. percent_cat < tile_threshold) then
         total_count            = total_count - count_cat(ij,cat)
         group                  = cat_groups(cat)
         count_group(ij,group)  = count_group(ij,group) - count_cat(ij,cat)
         count_cat(ij,cat)      = 0
       end if

       if (total_count == 0) then
! might want to pick dominate category in this instance.
! however, this does not happen if you pick a low threshold, 
! say 10% or less. 
         print*,'no cats above threshold at ',ij
         stop
       end if

     enddo

     pred_group = maxloc(count_group(ij,:))
     maxcat     = -999.9

!-------------------------------------------------------------------
! calculate final % of each category in each grid box.
! calculate dominate category - the predominate category within
! the predominate group.
!-------------------------------------------------------------------

     do cat = 1, num_categories 
       if (count_cat(ij,cat) > 0) then
         out_cat(ij,cat) = float(count_cat(ij,cat))  / &
                           float(total_count) * 100.0
         if (cat_groups(cat) == pred_group(1) .and. &
             out_cat(ij,cat) > maxcat ) then
           dominate_cat(ij) = cat
           maxcat           = out_cat(ij,cat)
         end if
       else
         out_cat(ij,cat) = 0.0
       end if
     enddo

     do grp = 1, num_groups
       out_group(ij,grp) = float(count_group(ij,grp)) /  & 
                           float(sum(count_group(ij,:))) * 100.0
     enddo

   else  ! total count is zero

! do a spiral search to find data.

     out_cat(ij,:) = 0.0
     out_group(ij,:) = 0.0

     jgrid  = nint( (lat_mdl(ij) - lat_11_src) / dlat_src + 1.0 )
     igrid  = nint( (lon_mdl(ij) - lon_11_src) / dlon_src + 1.0 )

     if (igrid > isrc) then     ! cross dateline
       igrid = igrid - isrc
     else if (igrid < 1) then
       igrid = igrid + isrc
     end if

     spiral_rad = nint(2.0 / abs(dlat_src))  ! 2 degree search rad
                                            
     SPIRAL_SEARCH : do krad = 1, spiral_rad

       istart = igrid - krad
       iend   = igrid + krad
       jstart = jgrid - krad
       jend   = jgrid + krad

       do jj = jstart, jend
       do ii = istart, iend

!-----------------------------------------------------------------------
!        search only along outer square.
!-----------------------------------------------------------------------

         if ((jj == jstart) .or. (jj == jend) .or.   &
             (ii == istart) .or. (ii == iend))  then

!-----------------------------------------------------------------------
!           ensure that point being investigated is within
!           the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

            if ((jj >= jsrc_start) .and. (jj <= jsrc_end)) then

              jjj = jj

!-----------------------------------------------------------------------
!             adjust i-index on source grid when search
!             crosses the date line.
!-----------------------------------------------------------------------

              if (ii <= 0) then
                iii = isrc + ii
              else if (ii >= (isrc+1)) then
                iii = ii - isrc
              else
                iii = ii
              end if

!-----------------------------------------------------------------------
!             a valid value was found.
!-----------------------------------------------------------------------

              if (srcdat(iii,jjj) /= water_category) then
                category = srcdat(iii,jjj)
                out_cat(ij,category) = 100.0
                dominate_cat(ij) = category
                group = cat_groups(category)
                out_group(ij,group) = 100.0
                write (6, 6000) isave(ij), jsave(ij), krad
                cycle IJLOOP
              end if
            end if

          end if

        enddo
        enddo

      enddo SPIRAL_SEARCH

!--------------------------------------------------------------------------
!     there was no source data within the search area, assign a default
!     value. 
!--------------------------------------------------------------------------

      out_cat(ij,default_category) = 100.0
      dominate_cat(ij)             = default_category
      out_group(ij,default_group)  = 100.0
      write(6,6100) isave(ij),jsave(ij)

    end if CALC_TILES

  enddo IJLOOP

 return

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I4)
 6100 FORMAT (1X, '-- DEFAULT CATEGORY ASSIGNED AT PNT ', 1X,I4,1X,I4)

 end subroutine interp_tiles
