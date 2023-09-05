subroutine read_fed(nread,ndata,nodata,infile,obstype,lunout,twind,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! ABSTRACT: 
!     This routine reads in netcdf or prepbufr flash-extent density (FED) data. 
!
! PROGRAM HISTORY LOG:
!    2018-07-25  Rong Kong (CAPS/OU) - modified based on read_radarref_mosaic.f90  
!    2019-09-20  Yaping Wang (CIMMS/OU)
!    2021-07-01  David Dowell (DCD; NOAA GSL) - added maximum flashes/min for observed FED
!
!   input argument list:
!     infile   - unit from which to read observation information file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     twind    - input group time window (hours)
!     sis      - observation variable name
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,one,rad2deg,deg2rad
  use convinfo, only: nconvtype,ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype
  use gsi_4dvar, only: l4dvar,l4densvar,winlen
  use gridmod, only: tll2xy
  use mod_wrfmass_to_a, only: wrfmass_obs_to_a8
  use mpimod, only: npe
  use obsmod, only: perturb_obs,iadatemn,time_window

  use netcdf
  implicit none

  include 'netcdf.inc'
!
  character(len=*), intent(in)    :: infile,obstype
  integer(i_kind),  intent(in)    :: lunout
  integer(i_kind),  intent(inout) :: nread,ndata
  integer(i_kind),  intent(inout) :: nodata
  integer(i_kind),  dimension(npe) ,intent(inout) :: nobs
  real(r_kind),     intent(in   ) :: twind
  character(len=*), intent(in)    :: sis

! Declare local parameters
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: oe_fed = 1.0_r_kind
  real(r_kind),parameter:: fed_lowbnd = 0.1_r_kind    ! use fed == fed_lowbnd
  real(r_kind),parameter:: fed_lowbnd2 = 0.1_r_kind   ! use fed >= fed_lowbnd2
!  real(r_kind),parameter:: fed_highbnd = 18.0_r_kind  ! 18 flashes/min from Sebok and Back (2021, unpublished)
  real(r_kind),parameter:: fed_highbnd = 8.0_r_kind  ! 8 flashes/min from Back (2023) for regional FV3 tests

!
!  For fed observations
!
  integer(i_kind) nreal,nchanl

  integer(i_kind) ifn,i
 
  real(r_kind)  :: maxfed
  integer(i_kind) :: ilon,ilat

  logical :: fedobs, fedob
  real(r_kind),allocatable,dimension(:,:):: cdata_out
  real(r_kind) :: federr, thiserr
  real(r_kind) :: hgt_fed(1)
  data hgt_fed / 6500.0 /

  real(r_kind) :: i_maxloc,j_maxloc,k_maxloc
  integer(i_kind) :: kint_maxloc
  real(r_kind) :: fed_max
  integer(i_kind) :: ndata2
  integer(i_kind) :: ppp

!
!  for read in bufr 
!
    real(r_kind) :: hdr(5),obs(1,3)  
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='FED'

    character(8) subset
    character(8) station_id
    real(r_double) :: rstation_id
    equivalence(rstation_id,station_id)
    integer(i_kind)  :: lunin,idate
    integer(i_kind)  :: ireadmg,ireadsb

    integer(i_kind)  ::  maxlvl
    integer(i_kind)  ::  numlvl,numfed,numobsa,nmsgmax,maxobs
    integer(i_kind)  ::  k,iret
    integer(i_kind)  ::  nmsg,ntb

    real(r_kind),allocatable,dimension(:,:) :: fed3d_column  ! 3D fed in column
    real(r_kind),allocatable,dimension(:)   :: utime         ! time 

    integer(i_kind)  :: ikx
    real(r_kind)     :: timeo,t4dv

    character*128   :: myname='read_fed'

    real(r_kind) :: dlat, dlon              ! rotated corrdinate
    real(r_kind) :: dlat_earth, dlon_earth  ! in unit of degree
    real(r_kind) :: rlat00, rlon00          ! in unit of rad

    logical      :: l_psot_fed
    logical      :: l_latlon_fedobs
    logical      :: outside
    integer      :: unit_table

!  for read netcdf   
    integer(i_kind)    :: idate5(5), sec70,mins_an,mins_ob
    integer(i_kind)    :: varID, ncdfID, status
    character(4)       :: idate5s(5)
    real(r_kind)       :: timeb,twindm,rmins_an,rmins_ob
    

    unit_table = 23
!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
   write(6,*) "r_kind=",r_kind
   l_psot_fed = .FALSE.
   l_latlon_fedobs = .TRUE.

   fedob = obstype == 'fed'
   if(fedob) then
      nreal=25
   else 
      write(6,*) ' illegal obs type in read_fed : obstype=',obstype
      call stop2(94)
   end if
   if(perturb_obs .and. fedob)nreal=nreal+1
   write(6,*)'read_fed: nreal=',nreal

   fedobs = .false.
   ikx=0
   do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
           fedobs=.true.
           ikx=i
           federr  = oe_fed          ! Obs error (flashes per minute)
           thiserr = federr
           exit                   ! Exit loop when finished with initial convinfo fields     
       else if (i == nconvtype ) then
          write(6,*) 'read_fed: Obs Type for fed is not in CONVINFO !'
          write(6,*) 'read_fed: PLEASE modify the CONVINFO file !'
          write(6,*) 'read_fed: abort read_fed !'
          return
       endif
   end do
   write(6,'(1x,A,A30,I4,A15,F7.3,A7)') &
       trim(myname),': fed in convinfo-->ikx=',ikx,' fed ob err:',thiserr," (fed)"

   nread=0
   ndata=0
   nchanl=0
   ifn = 15

  if(fedobs) then
      maxlvl= 1                          ! fed only has one level  
  
    if(trim(infile) .eq. "fedbufr") then   ! prebufr or netcdf format
 !! get message and subset counts
       ! nmsgmax and maxobs are read in from BUFR data file, not pre-set.
      call getcount_bufr(infile,nmsgmax,maxobs)
      write(6,*)'read_fed: nmsgmax=',nmsgmax,'    maxobs=',maxobs

!     read in fed obs in bufr code format
      lunin = 10            
      allocate(fed3d_column(maxlvl+2+2,maxobs))

      open  ( unit = lunin, file = trim(infile),form='unformatted',err=200)
      call openbf  ( lunin, 'IN', lunin )
      open(unit_table,file='prepobs_kr.bufrtable')  !temporily dump the bufr table, which is already saved in file
      call dxdump(lunin,unit_table)
      call datelen  ( 10 )

      nmsg=0
      ntb = 0

      ndata =0
      ppp = 0
      msg_report: do while (ireadmg(lunin,subset,idate) == 0)
         nmsg=nmsg+1
         if (nmsg>nmsgmax) then
            write(6,*)'read_fed: messages exceed maximum ',nmsgmax
            call stop2(50)
         endif
         loop_report: do while (ireadsb(lunin) == 0)
            ntb = ntb+1
            if (ntb>maxobs) then
                write(6,*)'read_fed: reports exceed maximum ',maxobs
                call stop2(50)
            endif

            ! Extract type, date, and location information from BUFR file
            call ufbint(lunin,hdr,5,1,iret,hdrstr)
            if(hdr(3) .gt. 90 ) write(6,*) "Inside read_fed.f90, hdr(2)=",hdr(2),"hdr(3)=",hdr(3)
            if ( l_latlon_fedobs ) then
	    	if(abs(hdr(3))>r90 .or. abs(hdr(2))>r360) cycle loop_report
           	if(hdr(2)== r360)hdr(2)=hdr(2)-r360
            	if(hdr(2) < zero)hdr(2)=hdr(2)+r360
            end if

! check time window in subset
            if (l4dvar.or.l4densvar) then
               t4dv=hdr(4)
               if (t4dv<zero .OR. t4dv>winlen) then
                  write(6,*)'read_fed:      time outside window ',&
                       t4dv,' skip this report'
                  cycle loop_report
               endif
            else
               timeo=hdr(4)
               if (abs(timeo)>ctwind(ikx) .or. abs(timeo) > twind) then
                  write(6,*)'read_fed:  time outside window ',&
                       timeo,' skip this report'
                  cycle loop_report
               endif
            endif
! read in observations
            call ufbint(lunin,obs,1,3,iret,obsstr)  !Single level bufr data, Rong Kong
            if(obs(1,1) .gt. 5 ) write(6,*) "Inside read_fed.f90, obs(1,1)=",obs(1,1)
            numlvl=min(iret,maxlvl)
            if (numlvl .ne. maxlvl) then
                write(6,*)' read_fed: numlvl is not equalt to maxlvl:',numlvl,maxlvl
            end if
            if(hdr(3) .gt. 90) write(6,*) "hdr(3)=",hdr(3)
            if ( l_latlon_fedobs ) then
           	if(hdr(2)>= r360)hdr(2)=hdr(2)-r360
            	if(hdr(2) < zero)hdr(2)=hdr(2)+r360
                fed3d_column(1,ntb)=hdr(2)                   ! observation location,  earth lon
                fed3d_column(2,ntb)=hdr(3)                   ! observation location,  earth lat  
!                write(6,*) "Inside read_fed.f90, fed3d_column(1,ntb)=",fed3d_column(1,ntb),"fed3d_column(2,ntb)=",fed3d_column(2,ntb)
            else
                fed3d_column(1,ntb)=hdr(2)*10.0_r_kind       ! observation location, grid index i
                fed3d_column(2,ntb)=hdr(3)*10.0_r_kind       ! observation location, grid index j
            end if

            if (l_psot_fed .and. .NOT. l_latlon_fedobs ) then
                do k=1,numlvl
                    if (NINT(fed3d_column(1,ntb)) .eq. 175 .and. NINT(fed3d_column(2,ntb)) .eq. 105 .and.  &
                        NINT(hgt_fed(k)) .ge. 100 ) then
                        write(6,*) 'read_fed: single point/column obs run on grid: 175 105'
                        write(6,*) 'read_fed: found the pseudo single(column) fed obs:',fed3d_column(1:2,ntb),hgt_fed(k)
                    else
                        obs(1,1) = -999.0
                    end if
                end do
            end if

                fed3d_column(3,ntb)=obs(1,1)              
                fed3d_column(4,ntb)=obs(1,2)             
                fed3d_column(5,ntb)=obs(1,3)            
                if (obs(1,1) == fed_lowbnd .or. obs(1,1) >= fed_lowbnd2 ) then  
                    if (obs(1,1) == 0.0) then 
                        ppp = ppp + 1
                    endif
                    ndata = ndata + 1
                 endif

         enddo loop_report
      enddo msg_report

      write(6,*)'read_fed: messages/reports = ',nmsg,'/',ntb
      print*,'number of Z that is less than 0 is ppp = ', ppp
      numfed=ntb

! - Finished reading fed observations from BUFR format data file
!
    call closbf(lunin)
    close(lunin)

   else  !  NETCDF format
!!!! Start reading fed observations from NETCDF format data file 
        ! CHECK IF DATA FILE EXISTS

      ! OPEN NETCDF FILE
      status = nf90_open(TRIM(infile), NF90_NOWRITE, ncdfID)
      print*, '*** OPENING GOES FED OBS  NETCDF FILE: ', infile, status
      

      !------------------------
      ! Get date information
      !-------------------------
  !    status = nf90_get_att( ncdfID, nf90_global, 'year', idate5s(1) )
  !    print*, 'year ',status
  !    status = nf90_get_att( ncdfID, nf90_global, 'month', idate5s(2) )
  !    status = nf90_get_att( ncdfID, nf90_global, 'day', idate5s(3) )
  !    status = nf90_get_att( ncdfID, nf90_global, 'hour', idate5s(4) )
  !    status = nf90_get_att( ncdfID, nf90_global, 'minute', idate5s(5) )
  !    read(idate5s(:) , *) idate5(:)
  !    print*, idate5

      !------------------------
      ! Get Dimension Info (1-D)
      !-------------------------
      status = nf90_inq_varid( ncdfID, 'numobs', varID )
      status = nf90_get_var( ncdfID, varID, maxobs )

     !------------------------
     ! Allocate data arrays
     !-------------------------
     ALLOCATE( fed3d_column( 5, maxobs ) )
     ALLOCATE( utime( 1 ) )  ! seconds since from 2000-01-01 12:00
     
     !------------------------
     ! Get useful data arrays
     !-------------------------
     ! LON
     status = nf90_inq_varid( ncdfID, 'lon', varID )
     status = nf90_get_var( ncdfID, varID, fed3d_column(1, :) )
     ! LAT
     status = nf90_inq_varid( ncdfID, 'lat', varID )
     status = nf90_get_var( ncdfID, varID, fed3d_column(2, :) )
     ! FED value
     status = nf90_inq_varid( ncdfID, 'value', varID )
     status = nf90_get_var( ncdfID, varID, fed3d_column(3, :) )
     ! TIME
     status = nf90_inq_varid( ncdfID, 'time', varID )
     status = nf90_get_var( ncdfID, varID, utime )

     ! CLOSE NETCDF FILE
     status = nf90_close( ncdfID )


     !-Obtain analysis time in minutes since reference date
     sec70 = 694267200.0  ! seconds since from 1978-01-01 00:00 to 2000-01-01 12:00    
                          ! because the official GOES prescribed epoch time for GLM data is 2000-01-01 12:00:00

     call w3fs21(iadatemn,mins_an)  !mins_an -integer number of mins snce 01/01/1978
     rmins_an=mins_an             !convert to real number

     ! SINCE ALL OBS WILL HAVE THE SAME TIME, CHECK TIME HERE:
     rmins_ob = ( utime(1) + sec70 )/60   !Convert to Minutes from seconds
     twindm = twind*60.    !Convert to Minutes from hours
     timeb = rmins_ob-rmins_an

     if(abs(timeb) > abs(twindm)) then
        print*, 'WARNING: ALL FED OBSERVATIONS OUTSIDE ASSIMILATION TIME WINDOW: ', timeb, twindm
    !    goto 314
     endif
     numfed = maxobs
     do i=1,numfed
        if (fed3d_column( 3, i ) >= fed_lowbnd2 .or. fed3d_column( 3, i ) == fed_lowbnd ) then 
          ndata = ndata + 1
        end if
     end do
   end if  ! end if prebufr or netcdf format

      write(6,*)'read_fed: total no. of obs = ',ndata
      nread=ndata
      nodata=ndata
!!! - Finished reading fed observations from NETCDF format data file



      allocate(cdata_out(nreal,ndata))
!
!
      do i=1,numfed
          do k=1,maxlvl

!             DCD 1 July 2021
              if (fed3d_column(k+2,i) .gt. fed_highbnd) fed3d_column(k+2,i) = fed_highbnd 

          end do
      end do

      write(6,*) ' ------- check max and min value of OBS: bufr fed -------'
      write(6,*) ' level      maxval(fed)       minval(fed)'
      do k=1,maxlvl
          write(6,*) k,maxval(fed3d_column(k+2,:)),minval(fed3d_column(k+2,:))
      end do


      i_maxloc=-1.0
      j_maxloc=-1.0
      k_maxloc=-1.0
      kint_maxloc=-1
      fed_max=-999.99
      ndata2=0 
      do i=1,numfed
        do k=1,maxlvl
          if( fed3d_column(k+2,i) >= fed_lowbnd2 .or. fed3d_column(k+2,i) == fed_lowbnd) then !Rong Kong
            dlon_earth = fed3d_column(1,i)                ! longitude (degrees) of observation
                                                       ! ilone=18    ! index of longitude (degrees)
            dlat_earth = fed3d_column(2,i)                ! latitude (degrees) of observation
                                                       ! ilate=19    ! index of latitude (degrees)
           !-Check format of longitude and correct if necessary
           if(dlon_earth>=r360) dlon_earth=dlon_earth-r360
           if(dlon_earth<zero ) dlon_earth=dlon_earth+r360
           !if(dlon_earth>=r360 .or. dlat_earth >90.0_r_kind) cycle

           !-Convert back to radians                         
            rlon00 = dlon_earth*deg2rad
            rlat00 = dlat_earth*deg2rad
            call tll2xy(rlon00,rlat00,dlon,dlat,outside)
            if (outside) cycle

                                           !If observation is outside the domain
                                           ! then cycle, but don't increase
                                           ! range right away.
                                           ! Domain could be rectangular, so ob
                                           ! may be out of
                                           ! range at one end, but not the
                                           ! other.   

            ndata2=ndata2+1
            cdata_out( 1,ndata2) = thiserr             ! obs error (flashes/min) - inflated/adjusted
                                                        
            cdata_out( 2,ndata2) = dlon                ! 
                                                       
            cdata_out( 3,ndata2) = dlat
                                                       
            cdata_out( 4,ndata2) = hgt_fed(k)          ! obs absolute height (m) above MSL
                                                       ! ipres=4     ! index of pressure
            cdata_out( 5,ndata2) = fed3d_column(k+2,i) ! FED value
                                                       ! idbzob=5    ! index of dbz observation
            cdata_out( 6,ndata2) = rstation_id         ! station id (charstring equivalent to real double)
                                                       ! id=6        ! index of station id

            cdata_out( 7,ndata2) = 0.0_r_kind          ! observation time in data array
                                                       ! itime=7     ! index of observation time in data array
            cdata_out( 8,ndata2) = ikx                 ! ob type
                                                       ! ikxx=8      ! index of ob type
            cdata_out( 9,ndata2) = thiserr*2.0_r_kind  ! max error
                                                       ! iqmax=9     ! index of max error
            cdata_out(10,ndata2) = 273.0_r_kind        ! dry temperature
                                                       ! itemp=10    ! index of dry temperature
            cdata_out(11,ndata2) = 1.0_r_kind          ! quality mark
                                                       ! iqc=11      ! index of quality mark
            cdata_out(12,ndata2) = thiserr             ! original-original obs error ratio 
                                                       ! ier2=12    ! index of original-original obs error ratio
            cdata_out(13,ndata2) = icuse(ikx)          ! index of use parameter
                                                       ! iuse=13     ! index of use parameter
            cdata_out(14,ndata2) = icuse(ikx)          ! dominant surface type
                                                       ! idomsfc=14  ! index of dominant surface type
            cdata_out(15,ndata2) = 273.0_r_kind        ! index of surface skin temperature
                                                       ! iskint=15   ! index of surface skin temperature
            cdata_out(16,ndata2) = 0.5_r_kind          ! 10 meter wind factor
                                                       ! iff10=16    ! index of 10 meter wind factor
            cdata_out(17,ndata2) = 0.5_r_kind          ! surface roughness
                                                       ! isfcr=17    ! index of surface roughness

            cdata_out(18,ndata2) = dlon_earth          ! longitude (degrees)

            cdata_out(19,ndata2) = dlat_earth          ! latitude (degrees)

            cdata_out(20,ndata2) = hgt_fed(k)          ! station elevation (m)
                                                       ! istnelv=20  ! index of station elevation (m)
            cdata_out(21,ndata2) = hgt_fed(k)          ! observation height (m)
                                                       ! iobshgt=21  ! index of observation height (m)
            cdata_out(22,ndata2) = hgt_fed(k)          ! surface height
                                                       ! izz=22      ! index of surface height
            cdata_out(23,ndata2) = fed3d_column(4,i)   ! i index of obs grid for bufr resolution (i.e.,8km)  

            cdata_out(24,ndata2) = fed3d_column(5,i)   ! j index of obs grid for bufr resolution

            cdata_out(25,ndata2) = hgt_fed(k)          ! data level category
                                                       ! icat =25    ! index of data level category
            if(perturb_obs .and. fedob)then
               cdata_out(26,ndata2) = 1.0_r_kind       ! obs perturbation
                                                       ! iptrb=26    ! index of q perturbation
            end if
!          print*,'cdata_out(:,ndata2)=',cdata_out(:,ndata2)
            if(fed3d_column(k+2,i) > fed_max)then
               kint_maxloc=k
               k_maxloc=real(k,r_kind)
               j_maxloc=fed3d_column(2,i)
               i_maxloc=fed3d_column(1,i)
               fed_max =fed3d_column(k+2,i)
            end if
          endif
        enddo
      enddo

!---all looping done now print diagnostic output
  write(6,*)'READ_FED: Reached eof on FED file'
  write(6,*)'READ_FED: # read in obs. number               =',nread
  write(6,*)'READ_FED: # read in obs. number for further processing  =',ndata2
 !  write(6,*)'READ_FED: dlon_earth', cdata_out(18,10:15)

      ilon=2         ! array index for longitude
      ilat=3         ! array index for latitude  in obs information array
      ndata=ndata2
      nodata=ndata2

  !---Write observations to scratch file---!

!      if(ndata > 0 ) then
        call count_obs(ndata,nreal,ilat,ilon,cdata_out,nobs)
        write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
        write(lunout) ((cdata_out(k,i),k=1,nreal),i=1,ndata)
        !  print*,'cdata_out',cdata_out
!      endif

      deallocate(cdata_out)
      if (allocated(fed3d_column)) deallocate(fed3d_column)

      write(6,'(1x,A,F12.5,1x,A,3(1x,F8.3),1x,I4)') &
          'read_fed: max fed =',fed_max, '@ i j k =', &
          i_maxloc,j_maxloc,k_maxloc,kint_maxloc

    end if
!   close(lunout)    ! ????
    return

200 continue
    write(6,*) 'read_fed, Warning : cannot find or open bufr fed data file: ', trim(infile)

314 continue
print* ,'FINISHED WITH READ_FED'
end subroutine read_fed
!
!
