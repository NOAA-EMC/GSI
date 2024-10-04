 program fov
!----------------------------------------------------------------------
!
! Abstract: Run the GSI's fov routines for crosstrack instruments in 
!           stand-alone mode.  Output the FOV edge and returned
!           antenna power to separate Grads station files.
! 
! Usage:
!   input files:
!      config.nml - configuration namelist (see below for variable
!                   definitions and their possible values).
!  
!   output files:
!      ellipse.dat - Grads station file containing FOV edge.
!      power.dat   - Grads station file containing returned antenna power.
!
!      For some sensors, the FOV size and/or the returned antenna power
!      varies by channel number.  In that case the station file will
!      contain multiple Grads 'time' records.  Ex: to view channel
!      3, type 'set t 3' during your Grads session.
!
! Namelist entries:
!   instr - Instrument number (integer)
!     Crosstack scanners
!         1 = AVHRR-2 LAC/HRPT
!         2 = AVHRR-3 LAC/HRPT
!         3 = AVHRR-3 LAC/HRPT on NOAA-16
!         4 = HIRS-2
!         5 = HIRS-2I
!         6 = HIRS-3 NOAA-K
!         7 = HIRS-3 NOAA-L,M
!         8 = HIRS-4
!         9 = SSU
!        10 = MSU
!        11 = AMSU-A
!        12 = AMSU-B, HSB
!        13 = MHS
!        14 = ATMS 5.2 DEG
!        15 = ATMS 2.2 DEG
!        16 = ATMS 1.1 DEG
!        17 = AIRS
!        18 = IASI
!     Conical scanners
!        25 = DEFAULT CIRCULAR FOV
!        26 = F16 SSMIS (CAN ALSO BE USED FOR AMSRE)
!        27 = F17 SSMIS
!        28 = F18 SSMIS
!        29 = F19 SSMIS
!        30 = F20 SSMIS
!   satid - Satellite id (character string).  Valid choices are:
!        'tirosn'
!        'dmsp'
!        'f13', 'f14', 'f15', 'f16', 'f17'
!        'trmm'
!        'aura'
!        'aqua'
!        'metop-a' 'metop-b', 'metop-c'
!        'n05', 'n06', 'n07', 'n08', 'n09'
!        'n10', 'n11', 'n12', 'n14', 'n15'
!        'n16', 'n17', 'n18', 'n19'
!   fov_num - Field of view number (integer). Crosstrack sensors only.
!      Valid ranges are:
!        All AVHRR: 1 thru 2048
!        All HIRS:  1 thru 56
!        SSU :      1 thru 8
!        MSU:       1 thru 11
!        MHS:       1 thru 13
!        AMSU-A:    1 thru 30
!        AMSU-B:    1 thru 90
!        AIRS:      1 thru 90
!        All ATMS:  1 thru 96
!        IASI:      1 thru 120
!        OTHERS:    NOT USED FOR CONICAL SENSORS.  SET TO -999
!   sat_az  - Satellite azimuth angle (real in degrees)
!   lat_cent_fov - Latitude of center of field of view (real in degrees)
!   lon_cent_fov - Longitude of center of field of view (real in degrees)
!
! Condition codes:
!   0 - normal run.
!   1 - error reading configuration namelist.
!   2 - invalid data passed to routine instrument_init.
!   3 - invalid data passed to routine fov_check.
!  11 - invalid instrument number.
!  31 - number of channels not set.
!  32 - expansion factor not set.
!  44 - FOV too close to pole.
!----------------------------------------------------------------------

 use kinds, only : i_kind, r_kind, r_single

 use constants

 use calc_fov_crosstrk, only   : instrument_init_crosstrack => instrument_init, &
                                 npoly, &
                                 fov_check, &
                                 fov_cleanup, &
                                 fov_ellipse_crosstrk, &
                                 inside_fov_crosstrk

 use calc_fov_conical, only    : instrument_init_conical => instrument_init, &
                                 inside_fov_conical, &
                                 fov_ellipse_conical

 implicit none

 character*8                  :: satid, stnid

 integer(i_kind)              :: fov_num, instr, nlev, nflag
 integer(i_kind)              :: ichan, ichan_tot
 integer(i_kind)              :: n
 integer(i_kind)              :: iret

 logical                      :: conical, crosstrack
 logical                      :: valid

 real(r_kind)                 :: expansion
 real(r_kind)                 :: lats(npoly), lons(npoly)
 real(r_kind), allocatable    :: lats_edge_fov(:,:), lons_edge_fov(:,:)
 real(r_kind)                 :: power, lat_cent_fov, lon_cent_fov
 real(r_single)               :: tim
 real(r_kind)                 :: sat_az
 real(r_kind)                 :: lat_mdl, lon_mdl
 real(r_kind)                 :: start_lat, end_lat, start_lon, end_lon, dlat, dlon

 namelist /setup/ instr, satid, fov_num, sat_az, lat_cent_fov, lon_cent_fov

!----------------------------------------------------------------------
! Initialize some constants and read configuration namelist.
!----------------------------------------------------------------------

 call init_constants_derived
 call init_constants(.false.)

 print*,'- READ NAMELIST'
 open (44, file="./config.nml", iostat=iret)
 read (44, nml=setup, iostat=iret)
 if (iret/=0) then
   print*,'** ERROR READING NAMELIST, IRET: ', iret
   print*,'** STOP.'
   stop 1
 endif

 select case (instr)
   case (1:18)
     conical=.false.
     crosstrack=.true.
   case (25:30)
     conical=.true.
     crosstrack=.false.
   case default
     print*,'** ERROR. INVALID SATELLITE INSTRUMENT NUMBER: ', instr
     print*,'** STOP.'
     stop 11
 end select

 print*,' '
 print*,'- WILL COMPUTE FOV FIELDS FOR:'
 write(6,65) '- INSTRUMENT NUMBER:    ', instr
 write(6,64) '- SATELLITE ID:         ', satid
 64 format(a25,5x,a8)
 if (crosstrack) write(6,65) '- FOV NUMBER:           ', fov_num
 65 format(a25,1x,i6)
 write(6,66) '- AZIMUTH ANGLE:        ', sat_az
 66 format(a25,1x,f8.3)
 write(6,67) '- LAT/LON OF FOV CENTER:', lat_cent_fov, lon_cent_fov
 67 format(a25,1x,f8.3,3x,f8.3)

!----------------------------------------------------------------------
! algorithm breaks down if fov crosses pole.  so, stop processing.
!----------------------------------------------------------------------

 if (abs(lat_cent_fov) > 88.0) then
   print*,' '
   print*,'** ALGORITHM BREAKS DOWN NEAR POLE. STOP.'
   stop 44
 endif

!----------------------------------------------------------------------
! amsua and mhs have channel specific antenna power 
! coefficients.  ssmis have channel specific fov sizes and power
! coefficients.
!----------------------------------------------------------------------

 select case (instr)
   case (1:10, 12, 14:18)
     ichan_tot=1
   case (11)    ! amsua
     ichan_tot=15
   case (13)    ! mhs
     ichan_tot=5
   case (25:30) ! ssmis
     ichan_tot=24
   case default
     print*,' '
     print*,'** ERROR. SET NUMBER OF CHANNELS FOR INSTRUMENT NUMBER: ', instr
     print*,'** STOP.'
     stop 31
 end select

!----------------------------------------------------------------------
! Expansion sets the boundary of the fov.   Use 1.0 for IR and
! almost 3 for microwave.  Algorithm breaks down at >= 3.
!----------------------------------------------------------------------

 select case (instr)
   case (1:9, 17:18)
     expansion=1.0_r_kind
   case (10:16, 25:30)
     expansion=2.9_r_kind
   case default
     print*,' '
     print*,'** ERROR. SET EXPANSION FACTOR FOR INSTRUMENT NUMBER: ', instr
     print*,'** STOP.'
     stop 32
 end select

!----------------------------------------------------------------------
! Check for invalid instrument or satellite settings.
!----------------------------------------------------------------------

 if (crosstrack) then
   call instrument_init_crosstrack(instr, satid, expansion, valid)
 elseif (conical) then
   call instrument_init_conical(instr, satid, expansion, valid)
 endif

 if (.not.valid) then
   print*,' '
   print*,'** PROBLEM IN ROUTINE INSTRUMENT_INIT. STOP.'
   stop 2
 endif

!----------------------------------------------------------------------
! Check FOV number for crosstrack instruments.  Conical instruments
! don't use an FOV number.
!----------------------------------------------------------------------

 if (crosstrack) then
   call fov_check(fov_num,instr,ichan_tot,valid)
   if (.not.valid) then
     print*,'** PROBLEM IN ROUTINE FOV_CHECK. STOP.'
     stop 3
   endif
 endif

!----------------------------------------------------------------------
! Determine the lat/lons of the fov boundary.  This boundary is
! a polygon with npoly vertices.  npoly is set in the 
! calc_fov_crosstrk module.  Output to Grads station file.
!----------------------------------------------------------------------

 open (65, file="./ellipse.dat", form="unformatted")
 stnid = "aaaaaaa"
 tim = 0.0_r_single
 nflag = 1

 print*,' '
 print*,'- COMPUTE LAT/LONS OF FOV EDGE'

 allocate(lats_edge_fov(ichan_tot,npoly))
 allocate(lons_edge_fov(ichan_tot,npoly))

 do ichan = 1, ichan_tot

   if (crosstrack) then
     call fov_ellipse_crosstrk(fov_num, instr, sat_az, lat_cent_fov, lon_cent_fov, &
                               lats, lons)
   elseif (conical) then
     print*,' '
     print*,'- PROCESS CHANNEL NUMBER: ',ichan
     call fov_ellipse_conical(ichan, sat_az, lat_cent_fov, lon_cent_fov, &
                              lats, lons)
   endif

   lats_edge_fov(ichan,:)=lats
   lons_edge_fov(ichan,:)=lons

   print*,' '
   nlev=1
   do n = 1, npoly
     write(6,100) "- LAT/LON OF FOV EDGE: ", n, lats_edge_fov(ichan,n), lons_edge_fov(ichan,n)
     write(65) stnid, real(lats_edge_fov(ichan,n),4), real(lons_edge_fov(ichan,n),4), tim, nlev, nflag
     write(65) real(0.0,4)
   enddo
   nlev = 0   ! end of record marker
   write(65) stnid, real(lats_edge_fov(1,1),4), real(lons_edge_fov(1,1),4), tim, nlev, nflag

 enddo

 close (65)

 100 format(1x,a23,1x,i3,1x,f9.3,3x,f9.3)

!----------------------------------------------------------------------
! Compute antenna power for a grid of points within the FOV.
! The number of points may be increased/decreased by adjusting the
! denominator of the dlat/dlon calculation.
!
! Write out data to Grads station file.
!----------------------------------------------------------------------

 open (9, file="./power.dat", form="unformatted")
 stnid = "aaaaaaa"
 tim = 0.0_r_single
 nflag = 1

 print*,' '
 CHANNEL : do ichan = 1, ichan_tot

   if (ichan_tot > 1) then
     print*,'- DETERMINE RETURNED POWER FOR CHANNEL ',ichan
   else
     print*,'- DETERMINE RETURNED POWER'
   endif

   start_lat=minval(lats_edge_fov(ichan,:))
   end_lat=maxval(lats_edge_fov(ichan,:))
   start_lon=minval(lons_edge_fov(ichan,:))
   end_lon=maxval(lons_edge_fov(ichan,:))

   dlat = (end_lat-start_lat) / 8.0_r_kind
   dlon = (end_lon-start_lon) / 8.0_r_kind

   lat_mdl = start_lat - (dlat * 2.0_r_kind)
   do while (lat_mdl < (end_lat + (dlat*2.1_r_kind)))
     lon_mdl = start_lon - (dlon * 2.0_r_kind)
     do while (lon_mdl < (end_lon + (dlon*2.1_r_kind)))
       if (crosstrack) then
         call inside_fov_crosstrk(instr,fov_num,sat_az, &
                                  lat_cent_fov,lon_cent_fov,&
                                  lat_mdl,    lon_mdl,  &
                                  expansion, ichan, power )
       elseif (conical) then
         call inside_fov_conical(instr, ichan, sat_az, &
                                 lat_cent_fov, lon_cent_fov,&
                                 lat_mdl, lon_mdl, expansion, power)
       endif
       if (power>=0.005_r_kind) then
         nlev = 1
         write(9) stnid, real(lat_mdl,4), real(lon_mdl,4), tim, nlev, nflag
         write(9) real(power*100.,4)
       endif
       lon_mdl = lon_mdl + dlon
     enddo
     lat_mdl = lat_mdl + dlat
   enddo

! end of record marker for grads station file.
   nlev = 0
   write(9) stnid, real(lat_mdl,4), real(lon_mdl,4), tim, nlev, nflag
 enddo CHANNEL

 close (9)

 if (crosstrack) call fov_cleanup

 deallocate (lats_edge_fov, lons_edge_fov)

 print*,' '
 print*,'**********************'
 print*,'* NORMAL TERMINATION *'
 print*,'**********************'

 stop 0

 end program fov
