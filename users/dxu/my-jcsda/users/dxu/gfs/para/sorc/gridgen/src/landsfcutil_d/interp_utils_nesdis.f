 module interp_utils_nesdis

 contains

!-----------------------------------------------------------------------
! interpolates data to model grid by taking an area average
! of the surrounding source grid points.
!
! note: by setting variable "search_rad" to zero, the routine
! will do a nearest neighbor interpolation.
!
! note: this routine only works for source data on the afwa/nesdis
! polar stereographic grids.
!-----------------------------------------------------------------------

 subroutine interp_aavg_nesdis(imdl, jmdl, lat_mdl, lon_mdl, lsmask, & 
                               search_rad, mesh, &
                               default_value, undef_value,  &
                               data_mdl, data_src, isrc, jsrc)

 use ll2xy_utils, only     : ll2xy_nesdis,    &
                             ll2xy_nesdis_4km

 implicit none

 integer                  :: count
 integer                  :: hemi
 integer                  :: i, ii, j, jj 
 integer                  :: iend, istart
 integer, intent(in)      :: imdl
 integer                  :: igrid, jgrid
 integer, intent(in)      :: jmdl
 integer, intent(in)      :: isrc, jsrc
 integer                  :: jend, jstart
 integer                  :: krad
 integer, intent(in)      :: mesh
 integer, intent(in)      :: search_rad
 integer                  :: spiral_rad

 real, intent(out)        :: data_mdl  (imdl,jmdl) 
 real, intent(in)         :: data_src  (isrc ,jsrc)
 real, intent(in)         :: default_value
 real                     :: gridi, gridj
 real, intent(in)         :: lat_mdl   (imdl,jmdl)
 real, intent(in)         :: lon_mdl   (imdl,jmdl)
 real, intent(in)         :: lsmask    (imdl,jmdl)
 real                     :: sum
 real, intent(in)         :: undef_value
 
!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
! grid resolution in km is 381.0 / mesh.  divide by 111 km to get
! approx resolution in degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / (381.0/mesh/111.0))

 JLOOP : do j = 1, jmdl
   ILOOP : do i = 1, imdl

!-----------------------------------------------------------------------
!    interpolate at land points.
!-----------------------------------------------------------------------

     LAND_TEST : if (lsmask(i,j) > 0.0) then

!-----------------------------------------------------------------------
!      ensure we don't go off the source grid.
!-----------------------------------------------------------------------

       if (lat_mdl(i,j) > 5.0) then
         hemi = 1
       else if (lat_mdl(i,j) < -5.0) then
         hemi = 2
       else
         cycle ILOOP
       end if
 
       if (mesh == 96) then
         call ll2xy_nesdis_4km(lat_mdl(i,j),lon_mdl(i,j),gridi,gridj)
       else
         call ll2xy_nesdis(hemi, mesh, lat_mdl(i,j), lon_mdl(i,j), &
                           gridi, gridj)
       end if

       if (gridi > isrc .or. gridi < 1) then
         print*,'gridi out of bounds ',gridi
         call abort
       end if

       if (gridj > jsrc .or. gridj < 1) then
         print*,'gridj out of bounds ',gridj
         call abort
       end if

       jgrid = nint (gridj)
       igrid = nint (gridi)

!-----------------------------------------------------------------------
!      take an average based on the resolutions of both the model
!      and source grids (incorporated in variable search_rad).
!-----------------------------------------------------------------------

       count = 0
       sum   = 0.0

       do ii = (igrid-search_rad), (igrid+search_rad)
         do jj = (jgrid-search_rad), (jgrid+search_rad)

           if (jj >= 1 .and. jj <= jsrc  .and.  &
               ii >= 1 .and. ii <= isrc ) then

             if (data_src(ii,jj) /= undef_value) then
               count = count + 1
               sum = sum + data_src(ii,jj)
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

               if ((jj .ge. 1) .and. (jj .le. jsrc) .and.  &
                   (ii .ge. 1) .and. (ii .le. isrc) ) then 

!-----------------------------------------------------------------------
!                a valid value was found.
!-----------------------------------------------------------------------

                 if (data_src(ii,jj) /= undef_value) then
                   data_mdl(i,j) = data_src(ii,jj)
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
   
 end subroutine interp_aavg_nesdis

!-----------------------------------------------------------------------
!
! bilinearly interpolates data to the model grid.
!
! note: this routine only works for source data on the afwa/nesdis
! polar stereographic grids.
!
!-----------------------------------------------------------------------

 subroutine interp_bilinear_nesdis (imdl, jmdl, lat_mdl, lon_mdl, lsmask, & 
                                    mesh, default_value, undef_value,  &
                                    data_mdl, data_src, isrc, jsrc)

 use ll2xy_utils,  only    : ll2xy_nesdis,    &
                             ll2xy_nesdis_4km

 implicit none

 integer                  :: hemi
 integer                  :: i, ii, j, jj
 integer                  :: iend, istart
 integer                  :: igrid, jgrid, nigrid, njgrid
 integer                  :: igridp1, jgridp1
 integer, intent(in)      :: imdl
 integer, intent(in)      :: isrc, jsrc
 integer                  :: jend, jstart
 integer, intent(in)      :: jmdl
 integer                  :: krad
 integer, intent(in)      :: mesh
 integer                  :: spiral_rad

 real, intent(out)        :: data_mdl ( imdl, jmdl ) 
 real, intent(in)         :: data_src ( isrc, jsrc )
 real, intent(in)         :: default_value
 real, intent(in)         :: lat_mdl  ( imdl, jmdl )
 real, intent(in)         :: lon_mdl  ( imdl, jmdl )
 real                     :: gridj, gridi
 real, intent(in)         :: lsmask   ( imdl, jmdl )
 real                     :: term1, term2
 real                     :: w1, w2
 real, intent(in)         :: undef_value

!-----------------------------------------------------------------------
! for spiral searchs, use a generous radius of 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / (381.0/mesh/111.0))

 JLOOP : do j = 1, jmdl
   ILOOP : do i = 1, imdl

!-----------------------------------------------------------------------
!    interpolate at land points.
!-----------------------------------------------------------------------

     LAND_TEST : if (lsmask(i,j) > 0.0) then

!-----------------------------------------------------------------------
!      find corresponding "i" and "j" indices on the source grid. 
!      ensure model grid is not outside the source grid. 
!-----------------------------------------------------------------------

       if (lat_mdl(i,j) > 5.0) then
         hemi = 1
       else if (lat_mdl(i,j) < -5.0) then
         hemi = 2
       else
         cycle ILOOP
       end if

       if (mesh == 96) then
         call ll2xy_nesdis_4km(lat_mdl(i,j),lon_mdl(i,j),gridi,gridj)
       else
         call ll2xy_nesdis(hemi, mesh, lat_mdl(i,j), lon_mdl(i,j), &
                           gridi, gridj)
       end if

       jgrid   = int (gridj)
      
       if (jgrid < 1 .or. jgrid >= jsrc) then
         print*,'model grid outside bounds of source data grid'
         call abort
       end if

       jgridp1 = jgrid + 1
       njgrid  = nint(gridj)
       w2      = mod(gridj,1.0)

       igrid   = int (gridi)

       if (igrid < 1 .or. igrid >= isrc) then
         print*,'model grid outside bounds of source data grid'
         call abort
       end if

       igridp1 = igrid + 1
       nigrid  = nint(gridi)
       w1      = mod(gridi,1.0)

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

               if ((jj .ge. 1) .and. (jj .le. jsrc) .and.  &
                   (ii .ge. 1) .and. (ii .le. isrc) ) then 

!-----------------------------------------------------------------------
!                a valid value was found.
!-----------------------------------------------------------------------

                 if (data_src(ii,jj) /= undef_value) then
                   data_mdl(i,j) = data_src(ii,jj)
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
   
 end subroutine interp_bilinear_nesdis

 end module interp_utils_nesdis
