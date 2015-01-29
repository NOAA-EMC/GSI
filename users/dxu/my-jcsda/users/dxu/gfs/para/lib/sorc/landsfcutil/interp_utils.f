 module interp_utils
!$$$  module documentation block
!
! module:    interp_utils
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: collection of routines that interpolate
!           data on a lat/lon grid to a regional or global grid.
!
! program history log:
!   2005-05-20  gayno   - initial version
!
! usage: use interp_utils
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  ibm sp
!
!$$$

 contains

 subroutine interp_aavg_egrid(imdl, jmdl, lat_mdl, lon_mdl, &
                        dx_mdl, dy_mdl, lsmask_mdl, &
                        dx_src, dy_src, lat_11_src, lon_11_src, &
                        centlat_mdl, centlon_mdl,   &
                        default_value, undef_value,  &
                        data_mdl, data_src, isrc, jsrc)

 use ll2xy_utils, only              : ll2xy_egrid

 implicit none

 integer                           :: count(imdl,jmdl)
 integer                           :: i, j, ii, jj, iii, jjj
 integer                           :: iend, istart, jend, jstart
 integer                           :: igrid
 integer, intent(in)               :: imdl
 integer, intent(in)               :: isrc
 integer                           :: jgrid
 integer, intent(in)               :: jmdl
 integer, intent(in)               :: jsrc
 integer                           :: jsrc_start, jsrc_end
 integer                           :: krad
 integer                           :: nearest_i(1), nearest_j(1)
 integer                           :: spiral_rad
 integer                           :: test1, test2

 real, intent(in)                  :: centlat_mdl
 real, intent(in)                  :: centlon_mdl
 real, intent(in)                  :: data_src    (isrc,jsrc)
 real, intent(out)                 :: data_mdl    (imdl,jmdl)
 real, intent(in)                  :: default_value
 real, intent(in)                  :: dx_mdl, dy_mdl 
 real, intent(in)                  :: dx_src, dy_src 
 real                              :: gridi
 real                              :: gridj
 real, intent(in)                  :: lat_11_src, lon_11_src
 real, intent(in)                  :: lat_mdl  (imdl, jmdl)
 real, intent(in)                  :: lon_mdl  (imdl, jmdl)
 real, intent(in)                  :: lsmask_mdl  (imdl, jmdl)
 real                              :: srclat(1), srclon(1)
 real                              :: sum(imdl,jmdl)
 real                              :: test
 real, intent(in)                  :: undef_value

!----------------------------------------------------------------------
! routine assumes source data is on a global lat/lon grid.
!
! from the max and min values of latitude on the model grid, 
! determine the corresponding "j" bounds on the source grid.
!---------------------------------------------------------------------- 

 test  = (maxval(lat_mdl) + dy_mdl - lat_11_src) / dy_src + 1.0
 test1 = nint(test)
 if (test1 < 1)    test1 = 1
 if (test1 > jsrc) test1 = jsrc 
 test  = (minval(lat_mdl) - dy_mdl - lat_11_src) / dy_src + 1.0
 test2 = nint(test)
 if (test2 < 1)    test2 = 1
 if (test2 > jsrc) test2 = jsrc 

 jsrc_start = min(test1,test2)
 jsrc_end   = max(test1,test2)

 count = 0
 sum   = 0.0

 do j = jsrc_start, jsrc_end
   do i = 1, isrc

      if (data_src(i,j) /= undef_value) then

        srclat(1) = lat_11_src + (j-1)*dy_src
        srclon(1) = lon_11_src + (i-1)*dx_src

        call ll2xy_egrid(srclat, srclon, imdl, jmdl, &
                         centlat_mdl, centlon_mdl,   &
                         -dx_mdl, dy_mdl, &
                          1, 1, nearest_i, nearest_j)

        if (nearest_i(1) >= 1 .and. nearest_i(1) <= imdl .and. &
            nearest_j(1) >= 1 .and. nearest_j(1) <= jmdl) then
          count(nearest_i(1),nearest_j(1)) = count(nearest_i(1),nearest_j(1)) + 1
          sum(nearest_i(1),nearest_j(1)) = sum(nearest_i(1),nearest_j(1)) + &
                                           data_src(i,j)

        end if

      end if

   enddo
 enddo

 JLOOP : do j = 1, jmdl
   ILOOP : do i = 1, imdl

     LAND_TEST : if (lsmask_mdl(i,j) > 0.0) then

!---------------------------------------------------------------------- 
!      there were valid source data within the model grid box.
!---------------------------------------------------------------------- 

       VALID_COUNT : if (count(i,j) > 0) then
 
         data_mdl(i,j) = sum(i,j) / float(count(i,j))

!---------------------------------------------------------------------- 
!      there were no valid source data within the model grid box,
!      do a spiral search to find a valid value.
!---------------------------------------------------------------------- 

       else

         gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
         gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0

         jgrid = nint (gridj)
         igrid = nint (gridi)

         if (jgrid > jsrc) then
           jgrid = jsrc
         elseif (jgrid < 1) then
           jgrid = 1
         end if

         if (igrid > isrc) then
           igrid = igrid - isrc
         else if (igrid < 1) then
           igrid = igrid + isrc
         end if

         spiral_rad = nint(5.0 / dx_src)

         SPIRAL_SEARCH : do krad = 1, spiral_rad

           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           do jj = jstart, jend
           do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jsrc)) then

                 jjj = jj

!-----------------------------------------------------------------------
!                adjust i-index on source grid when search
!                crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   iii = isrc + ii
                 else if (ii .ge. (isrc+1)) then
                   iii = ii - isrc
                 else
                   iii = ii
                 end if

!-----------------------------------------------------------------------
!                a valid value was found.
!-----------------------------------------------------------------------

                 if (data_src(iii,jjj) /= undef_value) then
                   data_mdl(i,j) = data_src(iii,jjj)
                   write (6, 6000) i, j, krad
                   cycle ILOOP
                 end if

               end if

             end if

           enddo
           enddo

       enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!      the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

       data_mdl(i,j) = default_value
       write (6, 6100) i, j

       end if VALID_COUNT

     end if LAND_TEST

   enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_aavg_egrid

 subroutine interp_aavg(imdl, jmdl, lat_mdl, lon_mdl, lsmask, &
                        dx_src, dy_src, lat_11_src, lon_11_src, search_rad, &
                        default_value, undef_value,  &
                        data_mdl, data_src, isrc, jsrc)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate data to the model grid by taking the
!   nearest neighbor or area average of the source data.  the
!   simple area averaging technique only works well for the
!   gaussian grid.  for the e-grid, use routine interp_aavg_egrid.
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call interp_aavg(imdl, jmdl, lat_mdl, lon_mdl, lsmask,
!                         dx_src, dy_src, lat_11_src, lon_11_src, search_rad,
!                         default_value, undef_value, 
!                         data_mdl, data_src, isrc, jsrc)
!
!   input argument list:
!     data_src       - data on source grid
!     default_value  - flag value for model points for which source
!                      was not found
!     dx/dy_src      - x/y direction resolution of source data in deg
!     imdl           - model grid i-dimension
!     isrc           - source grid i-dimension
!     jmdl           - model grid j-dimension
!     jsrc           - source grid j-dimension
!     lat_mdl        - latitudes on model grid
!     lat_11_src     - latitude of point (1,1) on source grid
!     lon_11_src     - longitude of point (1,1) on source grid
!     lon_mdl        - longitudes on model grid
!     lsmask         - land mask of model grid (0-nonland;>0 land)
!     search_rad     - radius in grid point over which the area
!                      averaging will occur 
!     undef_value    - flag value indicating source data point is
!                      non-land and to be ingored during interpolation
!
!   output argument list:
!     data_mdl           - data on the model grid
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                  :: count
 integer                  :: iend, istart
 integer, intent(in)      :: imdl
 integer                  :: igrid, jgrid
 integer, intent(in)      :: jmdl
 integer                  :: i, ii, iii, j, jj, jjj
 integer, intent(in)      :: isrc, jsrc
 integer                  :: jend, jstart
 integer                  :: krad
 integer                  :: spiral_rad
 integer, intent(in)      :: search_rad

 real, intent(out)        :: data_mdl    (imdl,jmdl)
 real, intent(in)         :: data_src    (isrc,jsrc)
 real, intent(in)         :: default_value
 real, intent(in)         :: dx_src, dy_src
 real                     :: gridj, gridi
 real, intent(in)         :: lat_mdl     (imdl,jmdl)
 real, intent(in)         :: lat_11_src, lon_11_src
 real, intent(in)         :: lon_mdl     (imdl,jmdl)
 real, intent(in)         :: lsmask      (imdl,jmdl)
 real                     :: sum
 real, intent(in)         :: undef_value

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / dx_src)

 JLOOP : do j = 1, jmdl
   ILOOP : do i = 1, imdl

!-----------------------------------------------------------------------
!    interpolate at land points.
!-----------------------------------------------------------------------

     LAND_TEST : if (lsmask(i,j) > 0.0) then

!-----------------------------------------------------------------------
!      ensure we don't go off the source grid.
!-----------------------------------------------------------------------

       gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
       gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0

       jgrid = nint (gridj)
       igrid = nint (gridi)

       if (jgrid > jsrc) then
         jgrid = jsrc
       elseif (jgrid < 1) then
         jgrid = 1
       end if

       if (igrid > isrc) then
         igrid = igrid - isrc
       else if (igrid < 1) then
         igrid = igrid + isrc
       end if

!-----------------------------------------------------------------------
!      take an average based on the resolutions of both the model
!      and source grids (incorporated in variable search_rad).
!-----------------------------------------------------------------------

       count = 0
       sum   = 0.0

       do ii = (igrid-search_rad), (igrid+search_rad)
         do jj = (jgrid-search_rad), (jgrid+search_rad)

           if (jj >= 1 .and. jj <= jsrc) then

             iii = ii
             jjj = jj

             if (iii .ge. (isrc+1)) iii = iii - isrc
             if (iii .lt. 1)        iii = iii + isrc

             if (data_src(iii,jjj) /= undef_value) then
               count = count + 1
               sum = sum + data_src(iii,jjj)
             end if

           end if

         enddo
       enddo

       VALID_COUNT : if ( count .gt. 0 ) then

         data_mdl(i,j) = (sum / float(count))

       else

!-----------------------------------------------------------------------
!        source data is undefined at this point, do a spiral
!        search for valid value.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = (search_rad+1), spiral_rad

           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           do jj = jstart, jend
           do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jsrc)) then

                 jjj = jj

!-----------------------------------------------------------------------
!                adjust i-index on source grid when search
!                crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   iii = isrc + ii
                 else if (ii .ge. (isrc+1)) then
                   iii = ii - isrc
                 else
                   iii = ii
                 end if

!-----------------------------------------------------------------------
!                a valid value was found.
!-----------------------------------------------------------------------

                 if (data_src(iii,jjj) /= undef_value) then
                   data_mdl(i,j) = data_src(iii,jjj)
                   write (6, 6000) i, j, krad
                   cycle ILOOP
                 end if

               end if

             end if

           enddo
           enddo

       enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!      the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

       data_mdl(i,j) = default_value
       write (6, 6100) i, j

       end if VALID_COUNT

     end if LAND_TEST

   enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_aavg

 subroutine find_nn (imdl_input, jmdl_input, lsmask_input, &
                     imdl_output, jmdl_output, lsmask_output, & 
                     flag_value, grid_type, res_input, &
                     xpts, ypts, ipts, jpts)
!$$$  subprogram documentation block
!
! subprogram:   find_nn
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  for all land (water) points on the output grid,
!   find the nearest neighbor land (water) point on the input grid.
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call find_nn (imdl_input, jmdl_input, lsmask_input, 
!                      imdl_output, jmdl_output, lsmask_output, 
!                      flag_value, grid_type, res_input, 
!                      xpts, ypts, ipts, jpts)
!
!   input argument list:
!     flag_value     - indicates search for nearest neighbor failed
!     grid_type      - flag indicating global or regional input grid
!     imdl_input     - i-dimension of model input grid
!     imdl_output    - i-dimension of model output grid
!     jmdl_input     - j-dimension of model input grid
!     jmdl_output    - j-dimension of model output grid
!     lsmask_input   - land mask of model input grid (0-nonland;>0-land)
!     lsmask_output  - land mask of model output grid (0-nonland;>0-land)
!     res_input      - resolution of input grid in degrees
!     x/ypnts        - x/y indices of output grid
!
!   output argument list:
!     ipts           - nearest neighbor point (i index) with
!                      respect to the input grid
!     jpts           - nearest neighbor point (j index) with
!                      respect to the input grid
!
! remarks: only used by utility that cold starts nmm from edasx.
!          want to get rid of this.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 character*6, intent(in)  :: grid_type

 integer, intent(in)      :: flag_value
 integer                  :: i, ii, iii, j, jj, jjj
 integer                  :: iend, istart
 integer, intent(in)      :: imdl_input
 integer, intent(in)      :: imdl_output
 integer                  :: igrid, jgrid
 integer, intent(out)     :: ipts          (imdl_output,jmdl_output)
 integer, intent(in)      :: jmdl_input
 integer, intent(in)      :: jmdl_output
 integer, intent(out)     :: jpts          (imdl_output,jmdl_output)
 integer                  :: jend, jstart
 integer                  :: krad
 integer                  :: spiral_rad

 real                     :: gridi
 real                     :: gridj
 real, intent(in)         :: lsmask_input  (imdl_input,jmdl_input)
 real, intent(in)         :: lsmask_output (imdl_output,jmdl_output)
 real, intent(in)         :: res_input
 real, intent(in)         :: xpts          (imdl_output,jmdl_output)
 real, intent(in)         :: ypts          (imdl_output,jmdl_output)

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / res_input)

 JLOOP : do j = 1, jmdl_output
   ILOOP : do i = 1, imdl_output

!-----------------------------------------------------------------------
!      ensure we don't go off the source grid.
!-----------------------------------------------------------------------

       gridi = xpts(i,j)
       gridj = ypts(i,j)

       jgrid = nint (gridj)
       igrid = nint (gridi)

       if (jgrid > jmdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID ", jmdl_input
           stop
         else
           jgrid = jmdl_input
         end if

       elseif (jgrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID "
           stop
         else
           jgrid = 1
         end if

       end if

       if (igrid > imdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE J BOUNDS OF INPUT GRID ", imdl_input
           stop
         else
           igrid = igrid - imdl_input
         end if

       else if (igrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE I BOUNDS OF INPUT GRID "
           stop
         else
           igrid = igrid + imdl_input
         end if

       end if

!-----------------------------------------------------------------------
!      input/output point of the same type.
!-----------------------------------------------------------------------

       if ( (lsmask_input(igrid,jgrid) > 0.0   .and.  &
             lsmask_output(i,j)        > 0.0)   .or.  &
            (lsmask_input(igrid,jgrid) == 0.0  .and.  &
             lsmask_output(i,j)        == 0.0) ) then

         ipts(i,j) = igrid
         jpts(i,j) = jgrid

       else

!-----------------------------------------------------------------------
!        search for point of same type.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = 1, spiral_rad
                
           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           JJ_LOOP : do jj = jstart, jend
           II_LOOP : do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &            
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jmdl_input)) then 

                 jjj = jj

!-----------------------------------------------------------------------
!                for global grids, adjust i-index on source grid
!                when search crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   if (grid_type == "global") then
                     iii = imdl_input + ii
                   else
                     cycle II_LOOP
                   end if
                 else if (ii .ge. (imdl_input+1)) then
                   if (grid_type == "global") then
                     iii = ii - imdl_input                  
                   else
                     cycle II_LOOP
                   end if
                 else
                   iii = ii
                 end if 

!-----------------------------------------------------------------------
!                 a similar type is found.
!-----------------------------------------------------------------------

                  if ( (lsmask_input(iii,jjj) >  0.0    .and.  &
                        lsmask_output(i,j)    >  0.0)    .or.  &
                       (lsmask_input(iii,jjj) == 0.0    .and.  &
                        lsmask_output(i,j)    == 0.0) ) then
                   ipts(i,j) = iii
                   jpts(i,j) = jjj
                   write (6, 6000) i, j, krad
                   cycle ILOOP
                 end if

               end if

             end if

           enddo II_LOOP
           enddo JJ_LOOP

       enddo SPIRAL_SEARCH
 
!-----------------------------------------------------------------------
!      the sprial search failed.  therefore assign a flag value,
!      so rest of program can handle this point.
!-----------------------------------------------------------------------

       ipts(i,j) = flag_value
       jpts(i,j) = flag_value

       write (6, 6100) i, j
 
     end if

   enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I4) 
             
 6100 FORMAT (1X, '-- FLAG VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)     
   
 end subroutine find_nn

 subroutine find_nn_1d (imdl_input, jmdl_input, lsmask_input, &
                        ijmdl_output, lsmask_output,          & 
                        flag_value, grid_type, res_input,     &
                        iindx_output, jindx_output,           &
                        xindx_wrt_input_grid,                 &
                        yindx_wrt_input_grid,                 &
                        nn_iindx_wrt_input_grid,              &
                        nn_jindx_wrt_input_grid)
!$$$  subprogram documentation block
!
! subprogram:   find_nn_1d
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  for all land (water) points on a grid,
!   find the nearest neighbor land (water) point on another grid.
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call find_nn_1d(imdl_input, jmdl_input, lsmask_input, 
!                        ijmdl_output, lsmask_output,          
!                        flag_value, grid_type, res_input,     
!                        iindx_output, jindx_output,           
!                        xindx_wrt_input_grid,                 
!                        yindx_wrt_input_grid,                 
!                        nn_iindx_wrt_input_grid,              
!                        nn_jindx_wrt_input_grid)
!
!   input argument list:
!     flag_value             - indicates search for nearest neighbor failed
!     grid_type              - flag indicating global or regional input grid
!     imdl_input             - i-dimension of model input grid
!     ijmdl_output           - dimension of model output grid
!     i/jindx_output         - i/j index of output grid
!     jmdl_input             - j-dimension of model input grid
!     lsmask_input           - land mask of model input grid (0-nonland;>0-land)
!     lsmask_output          - land mask of model output grid (0-nonland;>0-land)
!     res_input              - resolution of input grid in degrees
!     x/yindx_wrt_input_grid - x/y indices with respect to input grid
!
!   output argument list:
!     nn_i/jindx_wrt_input_grid - nearest neighbor point (i/j index) with
!                                 the same land mask value
!
! remarks: want to keep this one.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 character*6, intent(in)  :: grid_type

 integer, intent(in)      :: flag_value
 integer                  :: ij, ii, iii, jj, jjj
 integer                  :: iend, istart
 integer                  :: igrid, jgrid
 integer, intent(in)      :: iindx_output (ijmdl_output)
 integer, intent(in)      :: ijmdl_output
 integer, intent(in)      :: imdl_input
 integer, intent(in)      :: jindx_output (ijmdl_output)
 integer, intent(in)      :: jmdl_input
 integer                  :: jend, jstart
 integer                  :: krad
 integer, intent(out)     :: nn_iindx_wrt_input_grid   (ijmdl_output)
 integer, intent(out)     :: nn_jindx_wrt_input_grid   (ijmdl_output)
 integer                  :: spiral_rad

 real                     :: gridi
 real                     :: gridj
 real, intent(in)         :: lsmask_input  (imdl_input,jmdl_input)
 real, intent(in)         :: lsmask_output (ijmdl_output)
 real, intent(in)         :: res_input
 real, intent(in)         :: xindx_wrt_input_grid      (ijmdl_output)
 real, intent(in)         :: yindx_wrt_input_grid      (ijmdl_output)

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / res_input)

 MAIN : do ij = 1, ijmdl_output

!-----------------------------------------------------------------------
!      ensure we don't go off the source grid.
!-----------------------------------------------------------------------

       gridi = xindx_wrt_input_grid(ij)
       gridj = yindx_wrt_input_grid(ij)

       jgrid = nint (gridj)
       igrid = nint (gridi)

       if (jgrid > jmdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID ", jmdl_input
           stop
         else
           jgrid = jmdl_input
         end if

       elseif (jgrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID "
           stop
         else
           jgrid = 1
         end if

       end if

       if (igrid > imdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE J BOUNDS OF INPUT GRID ", imdl_input
           stop
         else
           igrid = igrid - imdl_input
         end if

       else if (igrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE I BOUNDS OF INPUT GRID "
           stop
         else
           igrid = igrid + imdl_input
         end if

       end if

!-----------------------------------------------------------------------
!      input/output point of the same type.
!-----------------------------------------------------------------------

       if ( (lsmask_input(igrid,jgrid) > 0.0   .and.  &
             lsmask_output(ij)         > 0.0)   .or.  &
            (lsmask_input(igrid,jgrid) == 0.0  .and.  &
             lsmask_output(ij)         == 0.0) ) then

         nn_iindx_wrt_input_grid(ij) = igrid
         nn_jindx_wrt_input_grid(ij) = jgrid

       else

!-----------------------------------------------------------------------
!        search for point of same type.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = 1, spiral_rad
                
           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           JJ_LOOP : do jj = jstart, jend
           II_LOOP : do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &            
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jmdl_input)) then 

                 jjj = jj

!-----------------------------------------------------------------------
!                for global grids, adjust i-index on source grid
!                when search crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   if (grid_type == "global") then
                     iii = imdl_input + ii
                   else
                     cycle II_LOOP
                   end if
                 else if (ii .ge. (imdl_input+1)) then
                   if (grid_type == "global") then
                     iii = ii - imdl_input                  
                   else
                     cycle II_LOOP
                   end if
                 else
                   iii = ii
                 end if 

!-----------------------------------------------------------------------
!                 a similar type is found.
!-----------------------------------------------------------------------

                  if ( (lsmask_input(iii,jjj) >  0.0    .and.  &
                        lsmask_output(ij)     >  0.0)    .or.  &
                       (lsmask_input(iii,jjj) == 0.0    .and.  &
                        lsmask_output(ij)     == 0.0) ) then
                   nn_iindx_wrt_input_grid(ij) = iii
                   nn_jindx_wrt_input_grid(ij) = jjj
                   write (6, 6000) iindx_output(ij), jindx_output(ij), krad
                   cycle MAIN
                 end if

               end if

             end if

           enddo II_LOOP
           enddo JJ_LOOP

       enddo SPIRAL_SEARCH
 
!-----------------------------------------------------------------------
!      the sprial search failed.  therefore assign a flag value,
!      so rest of program can handle this point.
!-----------------------------------------------------------------------

       nn_iindx_wrt_input_grid(ij) = flag_value
       nn_jindx_wrt_input_grid(ij) = flag_value

       write (6, 6100) iindx_output(ij), jindx_output(ij)
 
     end if

 enddo MAIN

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X,'-- CIRCULAR SEARCH AT POINT ',I5,' ',I5,' ITERATIONS ',I4) 
             
 6100 FORMAT (1X,'-- FLAG VALUE ASSIGNED AT POINT ',I5,' ',I5)     
   
 end subroutine find_nn_1d

! new routine

 subroutine find_nn_new (imdl_input, jmdl_input, lsmask_input, &
                        ijmdl_output, lsmask_output,          & 
                        flag_value, grid_type, res_input,     &
                        iindx_output, jindx_output,           &
                        xindx_wrt_input_grid,                 &
                        yindx_wrt_input_grid,                 &
                        nn_iindx_wrt_input_grid,              &
                        nn_jindx_wrt_input_grid)
!$$$  subprogram documentation block
!
! subprogram:   find_nn_new
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  for all land (water) points on a grid,
!   find the nearest neighbor land (water) point on another grid.
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call find_nn_1d(imdl_input, jmdl_input, lsmask_input, 
!                        ijmdl_output, lsmask_output,          
!                        flag_value, grid_type, res_input,     
!                        iindx_output, jindx_output,           
!                        xindx_wrt_input_grid,                 
!                        yindx_wrt_input_grid,                 
!                        nn_iindx_wrt_input_grid,              
!                        nn_jindx_wrt_input_grid)
!
!   input argument list:
!     flag_value             - indicates search for nearest neighbor failed
!     grid_type              - flag indicating global or regional input grid
!     imdl_input             - i-dimension of model input grid
!     ijmdl_output           - dimension of model output grid
!     i/jindx_output         - i/j index of output grid
!     jmdl_input             - j-dimension of model input grid
!     lsmask_input           - land mask of model input grid (0-nonland;>0-land)
!     lsmask_output          - land mask of model output grid (0-nonland;>0-land)
!     res_input              - resolution of input grid in degrees
!     x/yindx_wrt_input_grid - x/y indices with respect to input grid
!
!   output argument list:
!     nn_i/jindx_wrt_input_grid - nearest neighbor point (i/j index) with
!                                 the same land mask value
!
! remarks: want to keep this one.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 character*6, intent(in)  :: grid_type

 integer, intent(in)      :: flag_value
 integer                  :: ij, ii, iii, jj, jjj
 integer                  :: iend, istart
 integer                  :: igrid, jgrid
 integer, intent(in)      :: iindx_output (ijmdl_output)
 integer, intent(in)      :: ijmdl_output
 integer, intent(in)      :: imdl_input
 integer, intent(in)      :: jindx_output (ijmdl_output)
 integer, intent(in)      :: jmdl_input
 integer                  :: jend, jstart
 integer                  :: krad
 integer, intent(out)     :: nn_iindx_wrt_input_grid   (ijmdl_output)
 integer, intent(out)     :: nn_jindx_wrt_input_grid   (ijmdl_output)
 integer                  :: spiral_rad

 real                     :: gridi
 real                     :: gridj
 integer, intent(in)         :: lsmask_input  (imdl_input,jmdl_input)
 integer, intent(in)         :: lsmask_output (ijmdl_output)
 real, intent(in)         :: res_input
 real, intent(in)         :: xindx_wrt_input_grid      (ijmdl_output)
 real, intent(in)         :: yindx_wrt_input_grid      (ijmdl_output)

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / res_input)

 MAIN : do ij = 1, ijmdl_output

!-----------------------------------------------------------------------
!      ensure we don't go off the source grid.
!-----------------------------------------------------------------------

       gridi = xindx_wrt_input_grid(ij)
       gridj = yindx_wrt_input_grid(ij)

       jgrid = nint (gridj)
       igrid = nint (gridi)

       if (jgrid > jmdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID ", jmdl_input
           stop
         else
           jgrid = jmdl_input
         end if

       elseif (jgrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID "
           stop
         else
           jgrid = 1
         end if

       end if

       if (igrid > imdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE J BOUNDS OF INPUT GRID ", imdl_input
           stop
         else
           igrid = igrid - imdl_input
         end if

       else if (igrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE I BOUNDS OF INPUT GRID "
           stop
         else
           igrid = igrid + imdl_input
         end if

       end if

!-----------------------------------------------------------------------
!      input/output point of the same type.
!-----------------------------------------------------------------------

       if ( (lsmask_input(igrid,jgrid) == 0   .and.  &
             lsmask_output(ij)         == 0)   .or.  &
            (lsmask_input(igrid,jgrid) == 1  .and.  &
             lsmask_output(ij)         == 1)   .or.  &
            (lsmask_input(igrid,jgrid) == 2  .and.  &
             lsmask_output(ij)         == 2) ) then

         nn_iindx_wrt_input_grid(ij) = igrid
         nn_jindx_wrt_input_grid(ij) = jgrid

       else

!-----------------------------------------------------------------------
!        search for point of same type.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = 1, spiral_rad
                
           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           JJ_LOOP : do jj = jstart, jend
           II_LOOP : do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &            
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jmdl_input)) then 

                 jjj = jj

!-----------------------------------------------------------------------
!                for global grids, adjust i-index on source grid
!                when search crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   if (grid_type == "global") then
                     iii = imdl_input + ii
                   else
                     cycle II_LOOP
                   end if
                 else if (ii .ge. (imdl_input+1)) then
                   if (grid_type == "global") then
                     iii = ii - imdl_input                  
                   else
                     cycle II_LOOP
                   end if
                 else
                   iii = ii
                 end if 

!-----------------------------------------------------------------------
!                 a similar type is found.
!-----------------------------------------------------------------------

                 if ( (lsmask_input(iii,jjj) == 0   .and.  &
                       lsmask_output(ij)         == 0)   .or.  &
                      (lsmask_input(iii,jjj) == 1  .and.  &
                       lsmask_output(ij)         == 1)   .or.  &
                      (lsmask_input(iii,jjj) == 2  .and.  &
                       lsmask_output(ij)         == 2) ) then
                   nn_iindx_wrt_input_grid(ij) = iii
                   nn_jindx_wrt_input_grid(ij) = jjj
                   write (6, 6000) iindx_output(ij), jindx_output(ij), krad
                   cycle MAIN
                 end if

               end if

             end if

           enddo II_LOOP
           enddo JJ_LOOP

       enddo SPIRAL_SEARCH
 
!-----------------------------------------------------------------------
!      the sprial search failed.  therefore assign a flag value,
!      so rest of program can handle this point.
!-----------------------------------------------------------------------

       nn_iindx_wrt_input_grid(ij) = flag_value
       nn_jindx_wrt_input_grid(ij) = flag_value

       write (6, 6100) iindx_output(ij), jindx_output(ij)
 
     end if

 enddo MAIN

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X,'-- CIRCULAR SEARCH AT POINT ',I5,' ',I5,' ITERATIONS ',I4) 
             
 6100 FORMAT (1X,'-- FLAG VALUE ASSIGNED AT POINT ',I5,' ',I5)     
   
 end subroutine find_nn_new

 subroutine interp_bilinear (imdl, jmdl, lat_mdl, lon_mdl, lsmask, & 
                             dx_src, dy_src, lat_11_src, lon_11_src, &
                             default_value, undef_value,  &
                             data_mdl, data_src, isrc, jsrc)
!$$$  subprogram documentation block
!
! subprogram:   interp_bilinear
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate data to the model grid by 
!   bilinear interpolation
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call interp_bilinear(imdl, jmdl, lat_mdl, lon_mdl, lsmask,
!                             dx_src, dy_src, lat_11_src, lon_11_src,
!                             default_value, undef_value, 
!                             data_mdl, data_src, isrc, jsrc)
!
!   input argument list:
!     data_src       - data on source grid
!     default_value  - flag value for model points for which source
!                      was not found
!     dx/dy_src      - x/y direction resolution of source data in deg
!     imdl           - model grid i-dimension
!     isrc           - source grid i-dimension
!     jmdl           - model grid j-dimension
!     jsrc           - source grid j-dimension
!     lat_mdl        - latitudes on model grid
!     lat_11_src     - latitude of point (1,1) on source grid
!     lon_11_src     - longitude of point (1,1) on source grid
!     lon_mdl        - longitudes on model grid
!     lsmask         - land mask of model grid (0-nonland;>0 land)
!     undef_value    - flag value indicating source data point is
!                      non-land and to be ingored during interpolation
!
!   output argument list:
!     data_mdl           - data on the model grid
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                  :: iend, istart
 integer, intent(in)      :: imdl
 integer                  :: igrid, jgrid, nigrid, njgrid
 integer                  :: igridp1, jgridp1
 integer, intent(in)      :: jmdl
 integer                  :: i, ii, iii, j, jj, jjj
 integer, intent(in)      :: isrc, jsrc
 integer                  :: jend, jstart
 integer                  :: krad
 integer                  :: spiral_rad

 real, intent(out)        :: data_mdl    (imdl,jmdl) 
 real, intent(in)         :: data_src    (isrc,jsrc)
 real, intent(in)         :: default_value
 real, intent(in)         :: dx_src, dy_src
 real, intent(in)         :: lat_mdl     (imdl,jmdl)
 real, intent(in)         :: lon_mdl     (imdl,jmdl)
 real                     :: gridj, gridi
 real, intent(in)         :: lat_11_src, lon_11_src
 real, intent(in)         :: lsmask      (imdl,jmdl)
 real                     :: term1, term2
 real, intent(in)         :: undef_value
 real                     :: w1, w2

!-----------------------------------------------------------------------
! for spiral searchs, use a generous radius of 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / dx_src)

 JLOOP : do j = 1, jmdl
   ILOOP : do i = 1, imdl

!-----------------------------------------------------------------------
!    interpolate at land points.
!-----------------------------------------------------------------------

     LAND_TEST : if (lsmask(i,j) > 0.0) then

!-----------------------------------------------------------------------
!      find corresponding "i" and "j" indices on the source grid. 
!      account for cases when you are the poles/dateline.
!-----------------------------------------------------------------------

       gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0

       jgrid  = int (gridj)

       if (jgrid >= jsrc) then
         jgrid   = jsrc
         jgridp1 = jsrc
         njgrid  = jsrc
         w2      = 1
       elseif (jgrid == 0) then
         jgrid   = 1
         jgridp1 = 1
         njgrid  = 1
         w2      = 0
       else
         jgridp1 = jgrid + 1
         njgrid  = nint(gridj)
         w2      = mod(gridj,1.0)
       end if

       gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0
       if (gridi > isrc) gridi = gridi - isrc
       if (gridi < 1.0)  gridi = gridi + isrc 

       w1 = mod(gridi,1.0)

       igrid  = int (gridi)
       if (igrid > isrc) igrid = igrid - isrc
       if (igrid < 1)    igrid = igrid + isrc

       igridp1 = igrid + 1
       if (igridp1 > isrc) igridp1 = igridp1 - isrc

       nigrid = nint(gridi)
       if (nigrid > isrc) nigrid = nigrid - isrc
       if (nigrid < 1)    nigrid = nigrid + isrc

!-----------------------------------------------------------------------
!      the four surrounding points have valid values.  do a bilinear
!      interpolation.
!-----------------------------------------------------------------------

       if ((data_src(igrid,jgrid)     /= undef_value)  .and. &
           (data_src(igridp1,jgrid)   /= undef_value)  .and. &
           (data_src(igrid,jgridp1)   /= undef_value)  .and. &
           (data_src(igridp1,jgridp1) /= undef_value) ) then

         term1 = ( (1.0 - w1) * data_src(igrid,jgrid) ) +  &
                   w1 * data_src(igridp1,jgrid)
         term2 = ( (1.0 - w1) * data_src(igrid,jgridp1) ) + &
                   w1 * data_src(igridp1,jgridp1) 

         data_mdl(i,j) = ((1.0 - w2) * term1) + &
                           w2 * term2

!-----------------------------------------------------------------------
!      all four surrounding points do not have valid values.  pick
!      the nearest neighbor if valid.
!-----------------------------------------------------------------------

       elseif (data_src(nigrid,njgrid) /= undef_value) then

         data_mdl(i,j) = data_src(nigrid,njgrid)

       else

!-----------------------------------------------------------------------
!        source data is undefined at this point, do a spiral
!        search for valid value.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = 1, spiral_rad
                
           istart = nigrid - krad
           iend   = nigrid + krad
           jstart = njgrid - krad
           jend   = njgrid + krad

           do jj = jstart, jend
           do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &            
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jsrc)) then 

                 jjj = jj

!-----------------------------------------------------------------------
!                adjust i-index on source grid when search
!                crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   iii = isrc + ii
                 else if (ii .ge. (isrc+1)) then
                   iii = ii - isrc                  
                 else
                   iii = ii
                 end if 

!-----------------------------------------------------------------------
!                a valid value was found.
!-----------------------------------------------------------------------

                 if (data_src(iii,jjj) /= undef_value) then
                   data_mdl(i,j) = data_src(iii,jjj)
                   write (6, 6000) i, j, krad
                   cycle ILOOP
                 end if

               end if

             end if

           enddo 
           enddo 

         enddo SPIRAL_SEARCH
 
!-----------------------------------------------------------------------
!        the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

         data_mdl(i,j) = default_value
         write (6, 6100) i, j

       end if

     end if LAND_TEST

   enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3) 

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4) 
   
 end subroutine interp_bilinear

 end module interp_utils
