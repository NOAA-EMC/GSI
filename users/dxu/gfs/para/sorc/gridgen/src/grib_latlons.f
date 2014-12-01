 module grib_latlons

 use gribit, only                : kpds_mdl,  &
                                   kgds_mdl

 use calc_latlons, only          : lat_mdl,       &
                                   lon_mdl,       &
                                   lat_vpnts_mdl, &
                                   lon_vpnts_mdl

 use program_setup, only         : imdl,      &
                                   jmdl
 contains

!-----------------------------------------------------------------------
! output data in grib format.
!-----------------------------------------------------------------------

 subroutine grib_latlons_mdl

 implicit none

 character*9                  :: fngrib

 integer                      :: iret
 integer                      :: lugb
 integer                      :: kgds(200)
 integer                      :: kpds(200)

 logical*1                    :: lbms(imdl,jmdl)

 kpds = kpds_mdl
 kgds = kgds_mdl

 lugb = 29
 fngrib = "./fort.29"
 call baopenw(lugb,fngrib,iret)

 if (iret /= 0) then
   print*,"Error opening: ", trim(fngrib), " IRET IS ", iret
   stop
 end if

 kpds(5)  = 176 ! latitude
 kpds(22) = 6   ! scaling factor
 kpds(4)  = 128 ! don't use bitmap for this field
 lbms     = .false.

 print*,"- OUTPUT LATITUDES IN GRIB FORMAT."

 call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms,    &
             lat_mdl, iret)

 if (iret /= 0) then
   print*,"Error gribbing: ", trim(fngrib), " IRET IS ", iret
   stop
 end if

 call baclose(lugb, iret)

 lugb = 30
 fngrib = "./fort.30"
 call baopenw(lugb,fngrib,iret)

 if (iret /= 0) then
   print*,"Error opening: ", trim(fngrib), " IRET IS ", iret
   stop
 end if

 kpds(5)  = 177  ! longitude

 print*,"- OUTPUT LONGITUDES IN GRIB FORMAT."

 call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms,    &
             lon_mdl, iret)

 if (iret /= 0) then
   print*,"Error gribbing: ", trim(fngrib), " IRET IS ", iret
   stop
 end if

 call baclose(lugb, iret)

!-----------------------------------------------------------------------
! for staggered grids, output lat/lon on the velocity points.
! for non-staggered grids, these arrays would not have been
! allocated or processed.
!-----------------------------------------------------------------------

 if (allocated(lat_vpnts_mdl) .and. allocated(lon_vpnts_mdl)) then

   lugb = 51
   fngrib = "./fort.51"
   call baopenw(lugb,fngrib,iret)

   if (iret /= 0) then
     print*,"Error opening: ", trim(fngrib), " IRET IS ", iret
     stop
   end if

   kpds(5)  = 176  ! latitude

   print*,"- OUTPUT VELOCITY POINT LATITUDES IN GRIB FORMAT."

   call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms,    &
               lat_vpnts_mdl, iret)

   if (iret /= 0) then
     print*,"Error gribbing: ", trim(fngrib), " IRET IS ", iret
     stop
   end if

   call baclose(lugb, iret)

   lugb = 52
   fngrib = "./fort.52"
   call baopenw(lugb,fngrib,iret)

   if (iret /= 0) then
     print*,"Error opening: ", trim(fngrib), " IRET IS ", iret
     stop
   end if

   kpds(5)  = 177  ! longitude

   print*,"- OUTPUT VELOCITY POINT LONGITUDES IN GRIB FORMAT."

   call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms,    &
               lon_vpnts_mdl, iret)

   if (iret /= 0) then
     print*,"Error gribbing: ", trim(fngrib), " IRET IS ", iret
     stop
   end if

   call baclose(lugb, iret)

 end if

 return

 end subroutine grib_latlons_mdl

 end module grib_latlons
