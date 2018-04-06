module readsatobs
!$$$  module documentation block
!
! module: readsatobs                   read data from satellite radiance
!                                      diag* files.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: read data from satellite radiance diag* files written out
!  by GSI forward operator code.
!
! Public Subroutines:
!  get_num_satobs: determine the number of observations to read.
!  get_satobs_data: read the data and calculate H(x) for ensemble members.
!  write_satobs_data: output diag file with spread
!   
! Public Variables: 
!
! Modules Used: read_diag
!
! program history log:
!   2009-02-23  Initial version.
!   2016-06-03  Collard - Added changes to allow for historical naming conventions
!   2016-11-29 shlyaeva - updated read routine to calculate linearized H(x)
!                         added write_ozvobs_data to output ensemble spread
!   2017-12-13  shlyaeva - added netcdf diag read/write capability
!
! attributes:
!   language: f95
!
!$$$

use kinds, only: r_kind,i_kind,r_single,r_double
use read_diag, only: diag_data_fix_list,diag_header_fix_list,diag_header_chan_list, &
    diag_data_chan_list,diag_data_extra_list,read_radiag_data,read_radiag_header, &
    diag_data_name_list, open_radiag, close_radiag
use params, only: nsats_rad, nsatmax_rad, dsis, sattypes_rad, npefiles, netcdf_diag

implicit none

private
public :: get_satobs_data, get_num_satobs, write_satobs_data

contains

! get number of radiance observations
subroutine get_num_satobs(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    implicit none
    character(len=500), intent(in)  :: obspath
    character(len=10),  intent(in)  :: id, datestring
    integer(i_kind),    intent(out) :: num_obs_tot, num_obs_totdiag

    if (netcdf_diag) then
       call get_num_satobs_nc(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    else
       call get_num_satobs_bin(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    endif
end subroutine get_num_satobs

! get number of radiance observations from binary file
subroutine get_num_satobs_bin(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    use radinfo, only: iuse_rad,nusis,jpch_rad,npred
    implicit none

    character(len=500), intent(in)  :: obspath
    character(len=10),  intent(in)  :: id, datestring
    integer(i_kind),    intent(out) :: num_obs_tot, num_obs_totdiag

    character(len=500) obsfile
    character(len=20) ::  sat_type
    character(len=4) :: pe_name
    integer(i_kind) iunit, iflag, nsat, n, nkeep, i, jpchstart,indxsat,ipe
    integer(i_kind) npred_radiag
    logical fexist,lretrieval,lverbose,init_pass
    real(r_kind) :: errorlimit,errorlimit2

    type(diag_header_fix_list )         :: header_fix0
    type(diag_header_chan_list),allocatable :: header_chan0(:)
    type(diag_data_fix_list   )         :: data_fix0
    type(diag_data_chan_list  ),allocatable :: data_chan0(:)
    type(diag_data_extra_list) ,allocatable :: data_extra0(:,:)
    type(diag_data_name_list)           :: data_name0

!  make consistent with screenobs
    errorlimit=1._r_kind/sqrt(1.e9_r_kind)
    errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)
    iunit = 7
    lretrieval=.false.
    npred_radiag=npred
    lverbose=.false.

    num_obs_tot = 0
    num_obs_totdiag = 0

    do nsat=1,nsats_rad
        jpchstart=0
        do i=1,jpch_rad
          write(sat_type,'(a20)') adjustl(dsis(nsat))
          ! The following is to sort out some historical naming conventions
          select case (sat_type(1:4))
             case ('airs')
               sat_type='airs_aqua'
             case ('iasi')
               if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
               if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
               if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
          end select
    
          if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
            jpchstart=i
            exit
          end if
        end do
        if(jpchstart == 0) cycle
        init_pass = .true.
        peloop: do ipe=0,npefiles
           write(pe_name,'(i4.4)') ipe
           if (npefiles .eq. 0) then
               ! read diag file (concatenated pe* files)
               obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))
               inquire(file=obsfile,exist=fexist)
               if (.not. fexist .or. datestring .eq. '0000000000') &
               obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))
           else ! read raw, unconcatenated pe* files.
               obsfile =&
               trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'
           endif

           inquire(file=obsfile,exist=fexist)
           if (.not.fexist) cycle peloop
           nkeep = 0
           call open_radiag(obsfile,iunit)

           if (init_pass) then
              call read_radiag_header(iunit,npred_radiag,lretrieval,header_fix0,header_chan0,data_name0,iflag,lverbose)
              if (iflag /= 0) exit
              init_pass = .false.
           endif

           do
              call read_radiag_data(iunit,header_fix0,lretrieval,data_fix0,data_chan0,data_extra0,iflag)
              if( iflag /= 0 )exit
              chan: do n=1,header_fix0%nchan
                num_obs_totdiag = num_obs_totdiag + 1
                if(header_chan0(n)%iuse<1) cycle chan
                indxsat=header_chan0(n)%iochan
                if(data_chan0(n)%qcmark < 0. .or. data_chan0(n)%errinv < errorlimit &
                         .or. data_chan0(n)%errinv > errorlimit2 &
                         .or. indxsat == 0) cycle chan
                if (header_fix0%iextra > 0) then
                   if(data_extra0(1,n)%extra <= 0.001_r_kind .or.  &
                      data_extra0(1,n)%extra > 1200._r_kind  .or. &
                      abs(data_chan0(n)%tbobs) > 1.e9_r_kind) cycle chan
                else
                   if(abs(data_chan0(n)%tbobs) > 1.e9_r_kind) cycle chan
                endif
                nkeep = nkeep + 1
              end do chan
           enddo
           num_obs_tot = num_obs_tot + nkeep
           call close_radiag(obsfile,iunit)
           if (ipe .eq. npefiles) then
              write(6,100) nsat,trim(sattypes_rad(nsat)),num_obs_tot
100           format(2x,i3,2x,a20,2x,'num_obs_tot= ',i9)
           endif
        enddo peloop ! ipe
    enddo ! satellite
end subroutine get_num_satobs_bin

! get number of radiance observations from netcdf file
subroutine get_num_satobs_nc(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
  use radinfo, only: iuse_rad,nusis,jpch_rad
  use nc_diag_read_mod, only: nc_diag_read_get_var
  use nc_diag_read_mod, only: nc_diag_read_get_dim
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close
  implicit none

    character(len=500), intent(in)  :: obspath
    character(len=10),  intent(in)  :: id, datestring
    integer(i_kind),    intent(out) :: num_obs_tot, num_obs_totdiag

    character(len=500) obsfile
    character(len=20) ::  sat_type
    character(len=4) :: pe_name
    integer(i_kind) iunit, nsat, nobs, nchans, ipe, nkeep, i, jpchstart
    logical fexist
    real(r_kind) :: errorlimit,errorlimit2

    integer(i_kind), dimension(:), allocatable :: Satinfo_Chan, Use_Flag, chind
    real(r_single), dimension(:), allocatable :: Pressure, QC_Flag, Inv_Error, Observation


!  make consistent with screenobs
    errorlimit=1._r_kind/sqrt(1.e9_r_kind)
    errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)
    iunit = 7

    num_obs_tot = 0
    num_obs_totdiag = 0

    do nsat=1,nsats_rad
        jpchstart=0
        do i=1,jpch_rad
          write(sat_type,'(a20)') adjustl(dsis(nsat))
          ! The following is to sort out some historical naming conventions
          select case (sat_type(1:4))
             case ('airs')
               sat_type='airs_aqua'
             case ('iasi')
               if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
               if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
               if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
          end select

          if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
            jpchstart=i
            exit
          end if
        end do
        if(jpchstart == 0) cycle
        peloop: do ipe=0,npefiles
           write(pe_name,'(i4.4)') ipe
           if (npefiles .eq. 0) then
               ! read diag file (concatenated pe* files)
               obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))//".nc4"
               inquire(file=obsfile,exist=fexist)
               if (.not. fexist .or. datestring .eq. '0000000000') &
               obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))//".nc4"
           else ! read raw, unconcatenated pe* files.
               obsfile =&
               trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'//".nc4"
           endif


           inquire(file=obsfile,exist=fexist)
           if (.not.fexist) cycle peloop

           nkeep = 0

           call nc_diag_read_init(obsfile, iunit)

           nobs = nc_diag_read_get_dim(iunit,'nobs')

           if (nobs <= 0) then
              call nc_diag_read_close(obsfile)
              cycle peloop
           endif

           nchans = nc_diag_read_get_dim(iunit,'nchans')
           allocate(Satinfo_Chan(nchans), Use_Flag(nchans), Pressure(nobs), QC_Flag(nobs),     &
                    Inv_Error(nobs), Observation(nobs), chind(nobs))

           call nc_diag_read_get_var(iunit, 'satinfo_chan', Satinfo_Chan)
           call nc_diag_read_get_var(iunit, 'use_flag', Use_Flag)
           call nc_diag_read_get_var(iunit, 'Channel_Index', chind)
           call nc_diag_read_get_var(iunit, 'Press_Max_Weight_Function', Pressure)
           call nc_diag_read_get_var(iunit, 'QC_Flag', QC_Flag)
           call nc_diag_read_get_var(iunit, 'Inverse_Observation_Error', Inv_Error)
           call nc_diag_read_get_var(iunit, 'Observation', Observation)

           call nc_diag_read_close(obsfile)


           do i = 1, nobs
              num_obs_totdiag = num_obs_totdiag + 1
              if(Use_Flag(chind(i)) < 1 ) cycle 
              if(QC_Flag(i) < 0. .or. Inv_Error(i) < errorlimit &
                 .or. Inv_Error(i) > errorlimit2 &
                 .or. Satinfo_Chan(chind(i)) == 0) cycle
              if(Pressure(i) <= 0.001_r_kind .or.  &
                 Pressure(i) > 1200._r_kind  .or. &
                  abs(Observation(i)) > 1.e9_r_kind) cycle 
              nkeep = nkeep + 1
           enddo
           num_obs_tot = num_obs_tot + nkeep

           if (ipe .eq. npefiles) then
              write(6,100) nsat,trim(sattypes_rad(nsat)),num_obs_tot
100           format(2x,i3,2x,a20,2x,'num_obs_tot= ',i9)
           endif

           deallocate(Satinfo_Chan, Use_Flag, Pressure, QC_Flag, Inv_Error, Observation, chind)
        enddo peloop ! ipe
    enddo ! satellite

end subroutine get_num_satobs_nc

! read radiance data
subroutine get_satobs_data(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx_mean_nobc, hx, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_channum, x_errorig, x_type, x_biaspred, x_indx, x_used, id, nanal)
  use radinfo, only: npred
  implicit none

  character*500, intent(in)     :: obspath
  character(len=10), intent(in) ::  datestring

  integer(i_kind), intent(in) :: nobs_max, nobs_maxdiag

  real(r_single), dimension(nobs_max), intent(out) :: hx_mean,hx_mean_nobc, hx
  real(r_single), dimension(nobs_max), intent(out) :: x_obs
  real(r_single), dimension(nobs_max), intent(out) :: x_err, x_errorig
  real(r_single), dimension(nobs_max), intent(out) :: x_lon, x_lat
  real(r_single), dimension(nobs_max), intent(out) :: x_press, x_time
  integer(i_kind), dimension(nobs_max), intent(out) :: x_channum, x_indx
  character(len=20), dimension(nobs_max), intent(out) :: x_type
  real(r_single), dimension(npred+1,nobs_max), intent(out) :: x_biaspred
  integer(i_kind), dimension(nobs_maxdiag), intent(out) :: x_used

  character(len=10), intent(in) :: id
  integer(i_kind), intent(in)   :: nanal

  if (netcdf_diag) then
    call get_satobs_data_nc(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx_mean_nobc, hx, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_channum, x_errorig, x_type, x_biaspred, x_indx, x_used, id, nanal)
  else
    call get_satobs_data_bin(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx_mean_nobc, hx, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_channum, x_errorig, x_type, x_biaspred, x_indx, x_used, id, nanal)
  endif

end subroutine get_satobs_data

! read radiance data from binary file
subroutine get_satobs_data_bin(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx_mean_nobc, hx, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_channum, x_errorig, x_type, x_biaspred, x_indx, x_used, id, nanal)
  use radinfo, only: iuse_rad,nusis,jpch_rad,npred,adp_anglebc,emiss_bc
  use params, only: nanals, lobsdiag_forenkf
  use statevec, only: state_d
  use constants, only: deg2rad, zero
  use mpisetup, only: nproc, mpi_wtime
  use observer_enkf, only: calc_linhx
  implicit none

  character*500, intent(in)     :: obspath
  character(len=10), intent(in) ::  datestring

  integer(i_kind), intent(in) :: nobs_max, nobs_maxdiag

  real(r_single), dimension(nobs_max), intent(out) :: hx_mean,hx_mean_nobc, hx
  real(r_single), dimension(nobs_max), intent(out) :: x_obs
  real(r_single), dimension(nobs_max), intent(out) :: x_err, x_errorig
  real(r_single), dimension(nobs_max), intent(out) :: x_lon, x_lat
  real(r_single), dimension(nobs_max), intent(out) :: x_press, x_time
  integer(i_kind), dimension(nobs_max), intent(out) :: x_channum, x_indx
  character(len=20), dimension(nobs_max), intent(out) :: x_type
  real(r_single), dimension(npred+1,nobs_max), intent(out) :: x_biaspred
  integer(i_kind), dimension(nobs_maxdiag), intent(out) :: x_used


  character(len=10), intent(in) :: id
  integer(i_kind), intent(in)   :: nanal

  character*500 obsfile, obsfile2
  character(len=10) :: id2
  character(len=4) pe_name

  character(len=20) ::  sat_type

  integer(i_kind) iunit, iflag,nobs,nobsdiag, n,nsat,ipe,i,jpchstart,indxsat
  integer(i_kind) iunit2, iflag2
  integer(i_kind) npred_radiag
  logical fexist,lretrieval,lverbose,init_pass
  logical twofiles,fexist2,init_pass2
  real(r_kind) :: errorlimit,errorlimit2
  real(r_double) t1,t2,tsum,tsum2

  type(diag_header_fix_list )         :: header_fix, header_fix2
  type(diag_header_chan_list),allocatable :: header_chan(:), header_chan2(:)
  type(diag_data_fix_list   )         :: data_fix, data_fix2
  type(diag_data_chan_list  ),allocatable :: data_chan(:), data_chan2(:)
  type(diag_data_extra_list) ,allocatable :: data_extra(:,:), data_extra2(:,:)
  type(diag_data_name_list)           :: data_name, data_name2

! make consistent with screenobs
  errorlimit=1._r_kind/sqrt(1.e9_r_kind)
  errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)

  tsum = 0; tsum2 = 0
  iunit = 7
  iunit2 = 17
  lretrieval=.false.
  npred_radiag=npred
  lverbose=.false.

  twofiles = (.not. lobsdiag_forenkf) .and. (nanal <= nanals)
  id2 = 'ensmean'
  if (nanal <= nanals) then
     write(id2,'(a3,(i3.3))') 'mem',nanal
  endif

  hx = zero
  nobs = 0
  nobsdiag = 0
  x_used = 0

  do nsat=1,nsats_rad
     jpchstart=0
     do i=1,jpch_rad
       write(sat_type,'(a20)') adjustl(dsis(nsat))
       ! The following is to sort out some historical naming conventions
       select case (sat_type(1:4))
          case ('airs')
            sat_type='airs_aqua'
          case ('iasi')
            if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
            if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
            if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
       end select
    
      if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
         jpchstart = i
         exit
       end if
     end do
     if(jpchstart == 0) cycle
     init_pass = .true.; init_pass2 = .true.
     peloop: do ipe=0,npefiles
     write(pe_name,'(i4.4)') ipe
     if (npefiles .eq. 0) then
         ! read diag file (concatenated pe* files)
         obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))
         inquire(file=obsfile,exist=fexist)
         if (.not. fexist .or. datestring .eq. '0000000000') &
         obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))
     else ! read raw, unconcatenated pe* files.
         obsfile =&
         trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'
     endif

     inquire(file=obsfile,exist=fexist)
     if(.not.fexist) cycle peloop
     call open_radiag(obsfile,iunit)

     if (init_pass) then
        call read_radiag_header(iunit,npred_radiag,lretrieval,header_fix,header_chan,data_name,iflag,lverbose)
        if( iflag /= 0 ) exit
        init_pass = .false.
     endif

     if(twofiles)then
        if (npefiles .eq. 0)  then
          ! read diag file (concatenated pe* files)
          obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id2))
          inquire(file=obsfile2,exist=fexist2)
          if (.not. fexist2 .or. datestring .eq. '0000000000') &
          obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id2))
       else ! read raw, unconcatenated pe* files.
          obsfile2 =&
          trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id2))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'
       endif

       call open_radiag(obsfile2, iunit2)

       if (init_pass2) then
          call read_radiag_header(iunit2,npred_radiag,lretrieval,header_fix2,header_chan2,data_name2,iflag2,lverbose)
          init_pass2 = .false.
       endif
     end if
     do
      t1 = mpi_wtime()
      call read_radiag_data(iunit,header_fix,lretrieval,data_fix,data_chan,data_extra,iflag )
      t2 = mpi_wtime()
      tsum2 = tsum2 + t2-t1
      if( iflag /= 0 ) then
       exit
      end if
      if(twofiles)then
         call read_radiag_data(iunit2,header_fix2,lretrieval,data_fix2,data_chan2,data_extra2,iflag2 )
         if( header_fix%nchan /= header_fix2%nchan .or. abs(data_fix%lat-data_fix2%lat) .gt. 1.e-5 .or.  &
            abs(data_fix%lon-data_fix2%lon) .gt. 1.e-5 .or. abs(data_fix%obstime-data_fix2%obstime) .gt. 1.e-5) then
           write(6,*) 'inconsistent files',trim(obsfile2)
           write(6,*) 'nchan',header_fix%nchan,header_fix2%nchan
           write(6,*) 'lat',data_fix%lat,data_fix2%lat
           write(6,*) 'lon',data_fix%lon,data_fix2%lon
           write(6,*) 'obstim',data_fix%obstime,data_fix2%obstime
           call stop2(-99)
        end if
      end if
      chan:do n=1,header_fix%nchan

         nobsdiag = nobsdiag + 1
         if(header_chan(n)%iuse<1) cycle chan
         indxsat=header_chan(n)%iochan
         if(data_chan(n)%qcmark < 0. .or. data_chan(n)%errinv < errorlimit &
                  .or. data_chan(n)%errinv > errorlimit2 &
                  .or. indxsat == 0) cycle chan
         if (header_fix%iextra > 0) then
            if(data_extra(1,n)%extra <= 0.001_r_kind .or.  &
               data_extra(1,n)%extra > 1200._r_kind  .or.  &
               abs(data_chan(n)%tbobs) > 1.e9_r_kind) cycle chan
         else
            if(abs(data_chan(n)%tbobs) > 1.e9_r_kind) cycle chan
         endif
         nobs = nobs + 1 

         x_used(nobsdiag) = 1
         if (nobs > nobs_max) then
             print *,'warning:  exceeding array bounds in readinfo_from_file',&
             nobs,nobs_max
         end if
         x_type(nobs)= sat_type
         x_channum(nobs) = n
         x_indx(nobs) = indxsat
         x_lon(nobs) = data_fix%lon
         x_lat(nobs) = data_fix%lat
         x_time(nobs) = data_fix%obstime
         x_obs(nobs) = data_chan(n)%tbobs 
         ! bias corrected Hx
         hx_mean(nobs) = x_obs(nobs) - data_chan(n)%omgbc 
         ! un-bias corrected Hx
         hx_mean_nobc(nobs) = x_obs(nobs) - data_chan(n)%omgnbc

         if (nanal <= nanals) then
            ! read full Hx
            if (.not. lobsdiag_forenkf) then
               hx(nobs) = x_obs(nobs) - data_chan2(n)%omgnbc
            ! run linearized Hx
            else
               t1 = mpi_wtime()
               call calc_linhx(hx_mean_nobc(nobs), state_d,              &
                             real(x_lat(nobs)*deg2rad,r_single),  &
                             real(x_lon(nobs)*deg2rad,r_single),  &
                             x_time(nobs),                        &
                             data_chan(n)%dhx_dx, hx(nobs))
               t2 = mpi_wtime()
               tsum = tsum + t2-t1
            endif
         endif

         ! data_chan%errinv is inverse error variance.
         x_errorig(nobs) = header_chan(n)%varch**2
         x_err(nobs) = (1._r_kind/data_chan(n)%errinv)**2
         if (header_fix%iextra > 0) then
           x_press(nobs) = data_extra(1,n)%extra
         else
           x_press(nobs) = 99999
         endif

!! DTK:  **NOTE**
!!       The bifix term will need to be expanded if/when the GSI/GDAS goes to using
!!       a higher polynomial version of the angle dependent bias correction (if
!!       and when it is moved into part of the varbc)
!!         x_biaspred(1,nobs) = data_chan1(n)%bifix! fixed angle dependent bias
         x_biaspred(1,nobs) = data_chan(n)%bifix(1) ! fixed angle dependent bias
         x_biaspred(2,nobs) = data_chan(n)%bicons ! constant bias correction
         x_biaspred(3,nobs) = data_chan(n)%biang ! scan angle bias correction
         x_biaspred(4,nobs) = data_chan(n)%biclw ! CLW bias correction
         x_biaspred(5,nobs) = data_chan(n)%bilap2 ! square lapse rate bias corr
         x_biaspred(6,nobs) = data_chan(n)%bilap ! lapse rate bias correction
         if (npred == 7) then
           x_biaspred(7,nobs) = data_chan(n)%bicos ! node*cos(lat) bias correction for SSMIS
           x_biaspred(8,nobs) = data_chan(n)%bisin ! sin(lat) bias correction for SSMIS                    
         endif
         if (emiss_bc) x_biaspred(9,nobs) = data_chan(n)%biemis

         if (adp_anglebc) then
            x_biaspred( 1,nobs)  = data_chan(n)%bifix(5) ! fixed angle dependent bias correction
            x_biaspred(npred-2,nobs)  = data_chan(n)%bifix(1) ! 4th order scan angle (predictor)
            x_biaspred(npred-1,nobs)  = data_chan(n)%bifix(2) ! 3rd order scan angle (predictor)
            x_biaspred(npred,nobs)  = data_chan(n)%bifix(3) ! 2nd order scan angle (predictor)
            x_biaspred(npred+1,nobs)    = data_chan(n)%bifix(4) ! 1st order scan angle (predictor)
         endif

      enddo chan
     enddo

     call close_radiag(obsfile,iunit)

     if (twofiles) call close_radiag(obsfile2,iunit2)
     enddo peloop ! ipe
 enddo ! satellite
 if (nanal == nanals) print *,'time in calc_linhx for sat obs on proc',nproc,' = ',tsum
 if (nanal == nanals) print *,'time in read_raddiag_data for sat obs on proc',nproc,' = ',tsum2

  if (nobs /= nobs_max) then
      print *,'number of obs not what expected in get_satobs_data',nobs,nobs_max
      call stop2(92)
  end if
  if (nobsdiag /= nobs_maxdiag) then
      print *,'number of total diag obs not what expected in get_satobs_data',nobsdiag,nobs_maxdiag
      call stop2(92)
  end if

 end subroutine get_satobs_data_bin

! read radiance data from netcdf file
subroutine get_satobs_data_nc(obspath, datestring, nobs_max, nobs_maxdiag, hx_mean, hx_mean_nobc, hx, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_channum, x_errorig, x_type, x_biaspred, x_indx, x_used, id, nanal)
  use nc_diag_read_mod, only: nc_diag_read_get_var
  use nc_diag_read_mod, only: nc_diag_read_get_dim, nc_diag_read_get_global_attr
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close

  use radinfo, only: iuse_rad,nusis,jpch_rad,npred,adp_anglebc,emiss_bc
  use params, only: nanals, lobsdiag_forenkf
  use statevec, only: state_d
  use constants, only: deg2rad, zero
  use mpisetup, only: nproc, mpi_wtime
  use observer_enkf, only: calc_linhx
  use sparsearr, only: sparr, assignment(=), delete, sparr2, new

  implicit none

  character*500, intent(in)     :: obspath
  character(len=10), intent(in) ::  datestring

  integer(i_kind), intent(in) :: nobs_max, nobs_maxdiag

  real(r_single), dimension(nobs_max), intent(out) :: hx_mean,hx_mean_nobc, hx
  real(r_single), dimension(nobs_max), intent(out) :: x_obs
  real(r_single), dimension(nobs_max), intent(out) :: x_err, x_errorig
  real(r_single), dimension(nobs_max), intent(out) :: x_lon, x_lat
  real(r_single), dimension(nobs_max), intent(out) :: x_press, x_time
  integer(i_kind), dimension(nobs_max), intent(out) :: x_channum, x_indx
  character(len=20), dimension(nobs_max), intent(out) :: x_type
  real(r_single), dimension(npred+1,nobs_max), intent(out) :: x_biaspred
  integer(i_kind), dimension(nobs_maxdiag), intent(out) :: x_used


  character(len=10), intent(in) :: id
  integer(i_kind), intent(in)   :: nanal

  character*500 obsfile, obsfile2
  character(len=10) :: id2
  character(len=4) pe_name

  character(len=20) ::  sat_type

  integer(i_kind) iunit, iob, iobdiag, i, nsat, ipe, jpchstart, nchans
  integer(i_kind) iunit2, nobs, nobs2, nnz, nind
  integer(i_kind) npred_radiag, angord
  logical fexist
  logical twofiles,fexist2
  real(r_kind) :: errorlimit,errorlimit2
  real(r_double) t1,t2,tsum,tsum2

  type(sparr2)    :: dhx_dx_read
  type(sparr)     :: dhx_dx

  integer(i_kind), dimension(:), allocatable :: Satinfo_Chan, Use_Flag, chind, chaninfoidx
  real(r_kind), dimension(:), allocatable :: error_variance
  real(r_single), dimension(:), allocatable :: Pressure, QC_Flag, Inv_Error, Observation
  real(r_single), dimension(:), allocatable :: Latitude, Longitude, Time
  real(r_single), dimension(:), allocatable :: Obs_Minus_Forecast_adjusted
  real(r_single), dimension(:), allocatable :: Obs_Minus_Forecast_unadjusted, Obs_Minus_Forecast_unadjusted2
  integer(i_kind), allocatable, dimension (:,:) :: Observation_Operator_Jacobian_stind
  integer(i_kind), allocatable, dimension (:,:) :: Observation_Operator_Jacobian_endind
  real(r_single), allocatable, dimension (:,:) :: Observation_Operator_Jacobian_val
  real(r_single), dimension(:), allocatable :: BC_Fixed_Scan_Position, BC_Constant, BC_Scan_Angle
  real(r_single), dimension(:), allocatable :: BC_Cloud_Liquid_Water, BC_Lapse_Rate_Squared, BC_Lapse_Rate
  real(r_single), dimension(:), allocatable :: BC_Cosine_Latitude_times_Node, BC_Sine_Latitude
  real(r_single), dimension(:), allocatable :: BC_Emissivity
  real(r_single), allocatable, dimension (:,:) :: BC_angord

! make consistent with screenobs
  errorlimit=1._r_kind/sqrt(1.e9_r_kind)
  errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)

  tsum = 0; tsum2 = 0
  npred_radiag=npred

  twofiles = (.not. lobsdiag_forenkf) .and. (nanal <= nanals)
  id2 = 'ensmean'
  if (nanal <= nanals) then
     write(id2,'(a3,(i3.3))') 'mem',nanal
  endif

  hx = zero
  iob = 0
  iobdiag = 0
  x_used = 0

  do nsat=1,nsats_rad
     jpchstart=0
     do i=1,jpch_rad
       write(sat_type,'(a20)') adjustl(dsis(nsat))
       ! The following is to sort out some historical naming conventions
       select case (sat_type(1:4))
          case ('airs')
            sat_type='airs_aqua'
          case ('iasi')
            if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
            if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
            if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
       end select

      if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
         jpchstart = i
         exit
       end if
     end do
     if(jpchstart == 0) cycle

     peloop: do ipe=0,npefiles
     write(pe_name,'(i4.4)') ipe
     if (npefiles .eq. 0) then
         ! read diag file (concatenated pe* files)
         obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))//".nc4"
         inquire(file=obsfile,exist=fexist)
         if (.not. fexist .or. datestring .eq. '0000000000') &
         obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))//".nc4"
     else ! read raw, unconcatenated pe* files.
         obsfile =&
         trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01.nc4'
     endif

     inquire(file=obsfile,exist=fexist)
     if(.not.fexist) cycle peloop

     t1 = mpi_wtime()
     call nc_diag_read_init(obsfile, iunit)

     nobs = nc_diag_read_get_dim(iunit,'nobs')

     if (nobs <= 0) then
        call nc_diag_read_close(obsfile)
        cycle peloop
     endif

     nchans = nc_diag_read_get_dim(iunit,'nchans')
     allocate(Satinfo_Chan(nchans), Use_Flag(nchans), error_variance(nchans))
     allocate(Pressure(nobs), QC_Flag(nobs), Inv_Error(nobs), Latitude(nobs), &
              Longitude(nobs), Time(nobs), Observation(nobs), chind(nobs),    &
              Obs_Minus_Forecast_unadjusted(nobs), Obs_Minus_Forecast_adjusted(nobs))
     call nc_diag_read_get_var(iunit, 'satinfo_chan', Satinfo_Chan)
     call nc_diag_read_get_var(iunit, 'use_flag', Use_Flag)
     call nc_diag_read_get_var(iunit, 'error_variance', error_variance)
     call nc_diag_read_get_var(iunit, 'chaninfoidx', chaninfoidx)

     call nc_diag_read_get_var(iunit, 'Channel_Index', chind)
     call nc_diag_read_get_var(iunit, 'Press_Max_Weight_Function', Pressure)
     call nc_diag_read_get_var(iunit, 'QC_Flag', QC_Flag)
     call nc_diag_read_get_var(iunit, 'Inverse_Observation_Error', Inv_Error)
     call nc_diag_read_get_var(iunit, 'Latitude', Latitude)
     call nc_diag_read_get_var(iunit, 'Longitude', Longitude)
     call nc_diag_read_get_var(iunit, 'Obs_Time', Time)
     call nc_diag_read_get_var(iunit, 'Observation', Observation)
     call nc_diag_read_get_var(iunit, 'Obs_Minus_Forecast_unadjusted', Obs_Minus_Forecast_unadjusted)
     call nc_diag_read_get_var(iunit, 'Obs_Minus_Forecast_adjusted', Obs_Minus_Forecast_adjusted)

     allocate(BC_Fixed_Scan_Position(nobs), BC_Constant(nobs), BC_Scan_Angle(nobs), &
              BC_Cloud_Liquid_Water(nobs), BC_Lapse_Rate_Squared(nobs),             &
              BC_Lapse_Rate(nobs))
     call nc_diag_read_get_var(iunit, 'BC_Fixed_Scan_Position', BC_Fixed_Scan_Position)
     call nc_diag_read_get_var(iunit, 'BC_Constant', BC_Constant)
     call nc_diag_read_get_var(iunit, 'BC_Scan_Angle', BC_Scan_Angle)
     call nc_diag_read_get_var(iunit, 'BC_Cloud_Liquid_Water', BC_Cloud_Liquid_Water)
     call nc_diag_read_get_var(iunit, 'BC_Lapse_Rate_Squared', BC_Lapse_Rate_Squared)
     call nc_diag_read_get_var(iunit, 'BC_Lapse_Rate', BC_Lapse_Rate)

     if (npred == 7) then
        allocate(BC_Cosine_Latitude_times_Node(nobs), BC_Sine_Latitude(nobs))
        call nc_diag_read_get_var(iunit, 'BC_Cosine_Latitude_times_Node', BC_Cosine_Latitude_times_Node) 
        call nc_diag_read_get_var(iunit, 'BC_Sine_Latitude', BC_Sine_Latitude)
     endif

     if (emiss_bc) then
        allocate(BC_Emissivity(nobs))
        call nc_diag_read_get_var(iunit, 'BC_Emissivity', BC_Emissivity)
     endif

     if (adp_anglebc) then
        call nc_diag_read_get_global_attr(iunit, "angord", angord)
        allocate(BC_angord(angord, nobs))
        call nc_diag_read_get_var(iunit, 'BC_angord', BC_angord)
     endif

     if (lobsdiag_forenkf) then
        call nc_diag_read_get_global_attr(iunit, "jac_nnz", nnz)
        call nc_diag_read_get_global_attr(iunit, "jac_nind", nind)
        allocate(Observation_Operator_Jacobian_stind(nind, nobs))
        allocate(Observation_Operator_Jacobian_endind(nind, nobs))
        allocate(Observation_Operator_Jacobian_val(nnz, nobs))
        call nc_diag_read_get_var(iunit, 'Observation_Operator_Jacobian_stind', Observation_Operator_Jacobian_stind)
        call nc_diag_read_get_var(iunit, 'Observation_Operator_Jacobian_endind', Observation_Operator_Jacobian_endind)
        call nc_diag_read_get_var(iunit, 'Observation_Operator_Jacobian_val', Observation_Operator_Jacobian_val)
     endif

     call nc_diag_read_close(obsfile)

     t2 = mpi_wtime()
     tsum2 = tsum2 + t2-t1


     if(twofiles)then
        if (npefiles .eq. 0)  then
          ! read diag file (concatenated pe* files)
          obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id2))//".nc4"
          inquire(file=obsfile2,exist=fexist2)
          if (.not. fexist2 .or. datestring .eq. '0000000000') &
          obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id2))//".nc4"
       else ! read raw, unconcatenated pe* files.
          obsfile2 =&
          trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id2))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'//".nc4"
       endif

       call nc_diag_read_init(obsfile2, iunit2)

       nobs2 = nc_diag_read_get_dim(iunit2,'nobs')

       if (nobs2 /= nobs) print *, nanal, trim(obsfile), nobs, nobs2

       allocate(Obs_Minus_Forecast_unadjusted2(nobs))
       call nc_diag_read_get_var(iunit2, 'Obs_Minus_Forecast_unadjusted', Obs_Minus_Forecast_unadjusted2)

       call nc_diag_read_close(obsfile2)

     end if

     do i = 1, nobs
        iobdiag = iobdiag + 1
        if (Use_Flag(chind(i)) < 1) cycle
        if (QC_Flag(i) < 0. .or. Inv_Error(i) < errorlimit &
                  .or. Inv_Error(i) > errorlimit2 &
                  .or. Satinfo_Chan(chind(i)) == 0) cycle
        if (Pressure(i) <= 0.001_r_kind .or.  &
            Pressure(i) > 1200._r_kind  .or.  &
            abs(Observation(i)) > 1.e9_r_kind) cycle 

        iob = iob + 1

        x_used(iobdiag) = 1
        x_type(iob)= sat_type

        x_channum(iob) = chaninfoidx(chind(i))
        x_indx(iob) = Satinfo_Chan(chind(i))

        x_lon(iob) = Longitude(i)
        x_lat(iob) = Latitude(i)
        x_time(iob) = Time(i)
        x_obs(iob) = Observation(i)
        ! bias corrected Hx
        hx_mean(iob) = x_obs(iob) - Obs_Minus_Forecast_adjusted(i)
        ! un-bias corrected Hx
        hx_mean_nobc(iob) = x_obs(iob) - Obs_Minus_Forecast_unadjusted(i)

        if (nanal <= nanals) then
           ! read full Hx
           if (.not. lobsdiag_forenkf) then
              hx(iob) = x_obs(iob) - Obs_Minus_Forecast_unadjusted2(i)
           ! run linearized Hx
           else
              call new(dhx_dx_read, nnz, nind)
              dhx_dx_read%st_ind = Observation_Operator_Jacobian_stind(:,i)
              dhx_dx_read%end_ind = Observation_Operator_Jacobian_endind(:,i)
              dhx_dx_read%val = Observation_Operator_Jacobian_val(:,i)
              dhx_dx = dhx_dx_read
              t1 = mpi_wtime()
              call calc_linhx(hx_mean_nobc(iob), state_d,        &
                             real(x_lat(iob)*deg2rad,r_single),  &
                             real(x_lon(iob)*deg2rad,r_single),  &
                             x_time(iob),                        &
                             dhx_dx, hx(iob))
              t2 = mpi_wtime()
              tsum = tsum + t2-t1
              call delete(dhx_dx)
              call delete(dhx_dx_read)
           endif
        endif

        ! data_chan%errinv is inverse error variance.
        x_errorig(iob) = error_variance(chind(i))**2
        x_err(iob) = (1._r_kind/Inv_Error(i))**2
        x_press(iob) = Pressure(i)

! DTK:  **NOTE**
!       The bifix term will need to be expanded if/when the GSI/GDAS goes to using
!       a higher polynomial version of the angle dependent bias correction (if
!       and when it is moved into part of the varbc)
         x_biaspred(1,iob) = BC_Fixed_Scan_Position(i) ! fixed angle dependent bias
         x_biaspred(2,iob) = BC_Constant(i) ! constant bias correction
         x_biaspred(3,iob) = BC_Scan_Angle(i) ! scan angle bias correction
         x_biaspred(4,iob) = BC_Cloud_Liquid_Water(i) ! CLW bias correction
         x_biaspred(5,iob) = BC_Lapse_Rate_Squared(i) ! square lapse rate bias corr
         x_biaspred(6,iob) = BC_Lapse_Rate(i) ! lapse rate bias correction
         if (npred == 7) then
           x_biaspred(7,iob) = BC_Cosine_Latitude_times_Node(i) ! node*cos(lat) bias correction for SSMIS
           x_biaspred(8,iob) = BC_Sine_Latitude(i) ! sin(lat) bias correction for SSMIS
         endif
         if (emiss_bc) x_biaspred(9,iob) = BC_Emissivity(i)

         if (adp_anglebc) then
            x_biaspred( 1,iob)  = BC_angord(5,i) ! fixed angle dependent bias correction
            x_biaspred(npred-2,iob)  = BC_angord(1,i) ! 4th order scan angle (predictor)
            x_biaspred(npred-1,iob)  = BC_angord(2,i) ! 3rd order scan angle (predictor)
            x_biaspred(npred,iob)    = BC_angord(3,i) ! 2nd order scan angle (predictor)
            x_biaspred(npred+1,iob)  = BC_angord(4,i) ! 1st order scan angle (predictor)
         endif

     enddo

     deallocate(Satinfo_Chan, Use_Flag, error_variance, chaninfoidx)
     deallocate(Pressure, QC_Flag, Inv_Error, Latitude, Longitude, Time, &
                Observation, chind, Obs_Minus_Forecast_unadjusted,       &
                Obs_Minus_Forecast_adjusted)
     deallocate(BC_Fixed_Scan_Position, BC_Constant, BC_Scan_Angle,      &
               BC_Cloud_Liquid_Water, BC_Lapse_Rate_Squared,             &
               BC_Lapse_Rate)
     if (npred == 7)  deallocate(BC_Cosine_Latitude_times_Node, BC_Sine_Latitude)
     if (emiss_bc)    deallocate(BC_Emissivity)
     if (adp_anglebc) deallocate(BC_angord)
     if (twofiles) deallocate(Obs_Minus_Forecast_unadjusted2)
     if (lobsdiag_forenkf) then
        deallocate(Observation_Operator_Jacobian_stind, Observation_Operator_Jacobian_endind, &
                   Observation_Operator_Jacobian_val)
     endif

     enddo peloop ! ipe
 enddo ! satellite
 if (nanal == nanals) print *,'time in calc_linhx for sat obs on proc',nproc,' = ',tsum
 if (nanal == nanals) print *,'time in read_raddiag_data for sat obs on proc',nproc,' = ',tsum2

  if (iob /= nobs_max) then
      print *,'number of obs not what expected in get_satobs_data',iob,nobs_max
      call stop2(92)
  end if
  if (iobdiag /= nobs_maxdiag) then
      print *,'number of total diag obs not what expected in get_satobs_data',iobdiag,nobs_maxdiag
      call stop2(92)
  end if

 end subroutine get_satobs_data_nc

! write spread diagnostics
subroutine write_satobs_data(obspath, datestring, nobs_max, nobs_maxdiag, x_fit, x_sprd, x_used, id, id2, gesid2)
  implicit none
  character*500,     intent(in) :: obspath
  character(len=10), intent(in) :: datestring
  integer(i_kind),   intent(in) :: nobs_max, nobs_maxdiag
  real(r_single),  dimension(nobs_max),     intent(in) :: x_fit, x_sprd
  integer(i_kind), dimension(nobs_maxdiag), intent(in) :: x_used
  character(len=10), intent(in) :: id, id2, gesid2


  if (netcdf_diag) then
     call write_satobs_data_nc(obspath, datestring, nobs_max, nobs_maxdiag, x_fit, x_sprd, x_used, id, gesid2)
  else
     call write_satobs_data_bin(obspath, datestring, nobs_max, nobs_maxdiag, x_fit, x_sprd, x_used, id, id2, gesid2)
  endif

end subroutine write_satobs_data

! write spread diagnostics to binary file
subroutine write_satobs_data_bin(obspath, datestring, nobs_max, nobs_maxdiag, x_fit, x_sprd, x_used, id, id2, gesid2)
  use radinfo, only: iuse_rad,jpch_rad,nusis
  use read_diag, only: iversion_radiag_2, ireal_radiag, ireal_old_radiag
  implicit none

  character*500,     intent(in) :: obspath
  character(len=10), intent(in) :: datestring
  integer(i_kind),   intent(in) :: nobs_max, nobs_maxdiag
  real(r_single),  dimension(nobs_max),     intent(in) :: x_fit, x_sprd
  integer(i_kind), dimension(nobs_maxdiag), intent(in) :: x_used
  character(len=10), intent(in) :: id, id2, gesid2

  character*500 obsfile,obsfile2
  character(len=4) pe_name

  character(len=20) ::  sat_type

  integer(i_kind) iunit,iunit2,iflag,nobs, nobsdiag,n,nsat,ipe,i,jpchstart
  logical fexist,init_pass

  character(len=10):: satid,sentype
  character(len=20):: sensat

  integer(i_kind):: jiter,nchanl,npred,ianldate,ireal,ipchan,iextra,jextra
  integer(i_kind):: idiag,angord,iversion,inewpc,isens,ijacob
  integer(i_kind):: iuse_tmp,nuchan_tmp,iochan_tmp
  real(r_single) :: freq_tmp,polar_tmp,wave_tmp,varch_tmp,tlapmean_tmp

  real(r_single),dimension(:,:),allocatable :: data_tmp
  real(r_single),dimension(:),allocatable   :: fix_tmp
  real(r_single),dimension(:,:),allocatable :: extra_tmp


  iunit = 7
  iunit2 = 17

  nobs = 0
  nobsdiag = 0

  do nsat=1,nsats_rad
     jpchstart=0
     do i=1,jpch_rad
       write(sat_type,'(a20)') adjustl(dsis(nsat))
          ! The following is to sort out some historical naming conventions
          select case (sat_type(1:4))
             case ('airs')
               sat_type='airs_aqua'
             case ('iasi')
               if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
               if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
               if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
          end select
       if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
         jpchstart = i
         exit
       end if
     end do
     if(jpchstart == 0) cycle
     init_pass = .true.
     if (datestring .eq. '0000000000') then
        obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_"//trim(adjustl(gesid2))//"."//trim(adjustl(id2))
     else 
        obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_"//trim(adjustl(gesid2))//"."//datestring//'_'//trim(adjustl(id2))
     endif
     peloop: do ipe=0,npefiles
     write(pe_name,'(i4.4)') ipe
     if (npefiles .eq. 0) then
        ! diag file (concatenated pe* files)
        obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))
        inquire(file=obsfile,exist=fexist)
        if (.not. fexist .or. datestring .eq. '0000000000') then
           obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))
        endif
     else ! raw, unconcatenated pe* files.
        obsfile = trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'
     endif
     inquire(file=obsfile,exist=fexist)
     if(.not.fexist) cycle peloop

     open(iunit,form="unformatted",file=obsfile)
     rewind(iunit)
     if (init_pass) then
        open(iunit2,form="unformatted",file=obsfile2)
        ! Read header (fixed_part).
         read(iunit,IOSTAT=iflag)  sensat,satid,sentype,jiter,nchanl,npred,ianldate,&
               ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc,isens,ijacob
         if (iflag == 0) then
            write(iunit2) sensat,satid,sentype,jiter,nchanl,npred,ianldate,&
               ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc,isens,ijacob
         else
            rewind(iunit)
            read(iunit,IOSTAT=iflag) sensat,satid,sentype,jiter,nchanl,npred,ianldate,&
                 ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc,isens
            ijacob=0
            if (iflag==0) then
               write(iunit2) sensat,satid,sentype,jiter,nchanl,npred,ianldate, &
                     ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc,isens
            else
               rewind(iunit)
               read(iunit,IOSTAT=iflag) sensat,satid,sentype,jiter,nchanl,npred,ianldate,    &
                    ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc
               if (iflag==0) then
                  write(iunit2) sensat,satid,sentype,jiter,nchanl,npred,ianldate, &
                       ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc
               else
                  rewind(iunit)
                  read(iunit,IOSTAT=iflag) sensat,satid,sentype,jiter,nchanl,npred,ianldate, &
                         ireal,ipchan,iextra,jextra
                  if (iflag==0) then
                     write(iunit2) sensat,satid,sentype,jiter,nchanl,npred,ianldate, &
                          ireal,ipchan,iextra,jextra
                  else
                     write(6,*)'READ_RADIAG_HEADER:  ***ERROR*** Unknown file format.Cannot read'
                     call stop2(5555)
                  endif
              endif
           endif
        endif
        ! read header (channel part)
        do n=1, nchanl
           read(iunit,IOSTAT=iflag) freq_tmp,polar_tmp,wave_tmp,varch_tmp,tlapmean_tmp,  &
                   iuse_tmp,nuchan_tmp,iochan_tmp
           write(iunit2) freq_tmp,polar_tmp,wave_tmp,varch_tmp,tlapmean_tmp,iuse_tmp,    &
                   nuchan_tmp,iochan_tmp
           if (iflag/=0) return
        end do
        init_pass = .false.
     endif
     allocate(data_tmp(idiag,nchanl))
     if (iversion < iversion_radiag_2) then
        allocate( fix_tmp( ireal_old_radiag ) )
     else
        allocate( fix_tmp( ireal_radiag ) )
     end if
     if (iextra > 0) then
        allocate(extra_tmp(iextra,jextra))
     endif

     do
        if (iextra == 0) then
           read(iunit,IOSTAT=iflag) fix_tmp, data_tmp
        else
           read(iunit,IOSTAT=iflag) fix_tmp, data_tmp, extra_tmp
        endif
        if( iflag /= 0 ) then
           exit
        end if
        chan:do n=1,nchanl
           if (data_tmp(5,n) == 0)  data_tmp(5,n) = 100 ! qcmark: not used in EnKF
           nobsdiag = nobsdiag + 1
           if (x_used(nobsdiag) == 1) then
              nobs = nobs + 1
              data_tmp(5,n) = 0
              data_tmp(3,n) = x_fit(nobs) + data_tmp(3,n)-data_tmp(2,n)
              data_tmp(2,n) = x_fit(nobs)
              data_tmp(16+angord+3,n) = x_sprd(nobs)
           endif
        enddo chan
        if (iextra == 0) then
           write(iunit2) fix_tmp, data_tmp
        else
           write(iunit2) fix_tmp, data_tmp, extra_tmp
        endif
     enddo
     if (allocated(data_tmp)) deallocate(data_tmp)
     if (allocated(fix_tmp)) deallocate(fix_tmp)
     if (allocated(extra_tmp)) deallocate(extra_tmp)

     close(iunit)
     enddo peloop ! ipe
   close(iunit2)
 enddo ! satellite

  if (nobs /= nobs_max) then
      print *,'number of obs not what expected in get_satobs_data',nobs,nobs_max
      call stop2(92)
  end if
  if (nobsdiag /= nobs_maxdiag) then
      print *,'number of total diag obs not what expected in get_satobs_data',nobsdiag,nobs_maxdiag
      call stop2(92)
  end if
 end subroutine write_satobs_data_bin

! writing spread diagnostics to netcdf file
subroutine write_satobs_data_nc(obspath, datestring, nobs_max, nobs_maxdiag, &
                                 x_fit, x_sprd, x_used, id, gesid)
  use netcdf, only: nf90_inq_dimid, nf90_open, nf90_close, NF90_NETCDF4, &
                    nf90_inquire_dimension, NF90_WRITE, nf90_create, nf90_def_dim
  use ncdw_climsg, only: nclayer_check

  use radinfo, only: iuse_rad,nusis,jpch_rad
  use constants, only: r_missing
  implicit none

  character*500,     intent(in) :: obspath
  character(len=10), intent(in) :: datestring
  integer(i_kind),   intent(in) :: nobs_max, nobs_maxdiag
  real(r_single),  dimension(nobs_max),     intent(in) :: x_fit, x_sprd
  integer(i_kind), dimension(nobs_maxdiag), intent(in) :: x_used
  character(len=10), intent(in) :: id, gesid

  character*500 obsfile, obsfile2
  character(len=4) pe_name
  character(len=20) ::  sat_type

  integer(i_kind) :: iunit, nobsid
  integer(i_kind) :: nob, nobdiag, nobs, ipe, i, nsat, jpchstart
  integer(i_kind), dimension(:), allocatable :: enkf_use_flag
  real(r_single),  dimension(:), allocatable :: enkf_fit, enkf_sprd
  logical :: fexist

  nob  = 0
  nobdiag = 0

  do nsat=1,nsats_rad
     jpchstart=0
     do i=1,jpch_rad
       write(sat_type,'(a20)') adjustl(dsis(nsat))
          ! The following is to sort out some historical naming conventions
          select case (sat_type(1:4))
             case ('airs')
               sat_type='airs_aqua'
             case ('iasi')
               if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
               if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
               if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
          end select
       if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
         jpchstart = i
         exit
       end if
     end do
     if(jpchstart == 0) cycle
     peloop: do ipe=0,npefiles
     write(pe_name,'(i4.4)') ipe
     if (npefiles .eq. 0) then
        ! diag file (concatenated pe* files)
        obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))//".nc4"
        obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))//"_spread.nc4"
        inquire(file=obsfile,exist=fexist)
        if (.not. fexist .or. datestring .eq. '0000000000') then
           obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))//".nc4"
           obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))//"_spread.nc4"
        endif
     else ! raw, unconcatenated pe* files.
        obsfile = trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'//".nc4"
        obsfile2 = trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'//"_spread.nc4"
     endif

     inquire(file=obsfile,exist=fexist)
     if (.not. fexist) cycle peloop


     call nclayer_check(nf90_open(obsfile, NF90_WRITE, iunit))
     call nclayer_check(nf90_inq_dimid(iunit, "nobs", nobsid))
     call nclayer_check(nf90_inquire_dimension(iunit, nobsid, len = nobs))
     call nclayer_check(nf90_close(iunit))

     if (nobs <= 0) cycle peloop

     allocate(enkf_use_flag(nobs), enkf_fit(nobs), enkf_sprd(nobs))

     do i = 1, nobs
        nobdiag = nobdiag + 1
 
        ! skip if not used in EnKF
        if (x_used(nobdiag) == 1) then
           ! update if it is used in EnKF
           nob = nob + 1
           enkf_use_flag(i) = 1
           enkf_fit(i)  = x_fit(nob)
           enkf_sprd(i) = x_sprd(nob)
        else
           enkf_use_flag(i) = -1
           enkf_fit(i) = r_missing
           enkf_sprd(i) = r_missing
        endif
     enddo

     inquire(file=obsfile2,exist=fexist)
     if (.not. fexist) then
        call nclayer_check(nf90_create(trim(obsfile2), NF90_NETCDF4, &
                           iunit))
        call nclayer_check(nf90_def_dim(iunit, "nobs", nobs, nobsid))
     else
        call nclayer_check(nf90_open(obsfile2, NF90_WRITE, iunit))
        call nclayer_check(nf90_inq_dimid(iunit, "nobs", nobsid))
     endif

     call write_ncvar_int(iunit, nobsid, "EnKF_use_flag", enkf_use_flag)
     call write_ncvar_single(iunit, nobsid, "EnKF_fit_"//trim(gesid), enkf_fit)
     call write_ncvar_single(iunit, nobsid, "EnKF_spread_"//trim(gesid), enkf_sprd)

     call nclayer_check(nf90_close(iunit))

     deallocate(enkf_use_flag, enkf_fit, enkf_sprd)

     enddo peloop ! ipe loop
  enddo

  if (nob .ne. nobs_max) then
      print *,'number of obs not what expected in write_satobs_data',nob,nobs_max
      call stop2(94)
  end if
  if (nobdiag /= nobs_maxdiag) then
      print *,'number of total obs in diag not what expected in write_satobs_data',nobdiag, nobs_maxdiag
      call stop2(94)
  endif

  contains
  subroutine write_ncvar_single(iunit, dimid, varname, field)
    use netcdf, only: nf90_def_var, nf90_put_var, nf90_inq_varid,  &
                      nf90_def_var_deflate,NF90_FLOAT, NF90_ENOTVAR
    use ncdw_climsg, only: nclayer_check
    use ncdw_types, only: NLAYER_COMPRESSION
    implicit none
    integer(i_kind), intent(in)  :: iunit, dimid
    character(*), intent(in)     :: varname
    real(r_single), dimension(:), allocatable :: field

    integer :: ierr, varid

    ierr = nf90_inq_varid(iunit, varname, varid)
    if (ierr == NF90_ENOTVAR) then
       call nclayer_check(nf90_def_var(iunit, varname, NF90_FLOAT, dimid, varid))
       call nclayer_check(nf90_def_var_deflate(iunit, varid, 1, 1, int(NLAYER_COMPRESSION)))
    endif
    call nclayer_check(nf90_put_var(iunit, varid, field))
  end subroutine write_ncvar_single

  subroutine write_ncvar_int(iunit, dimid, varname, field)
    use netcdf, only: nf90_def_var, nf90_put_var, nf90_inq_varid,  &
                      nf90_def_var_deflate,NF90_INT, NF90_ENOTVAR
    use ncdw_climsg, only: nclayer_check
    use ncdw_types, only: NLAYER_COMPRESSION
    implicit none
    integer(i_kind), intent(in)  :: iunit, dimid
    character(*), intent(in)     :: varname
    integer(i_kind), dimension(:), allocatable :: field

    integer :: ierr, varid

    ierr = nf90_inq_varid(iunit, varname, varid)
    if (ierr == NF90_ENOTVAR) then
       call nclayer_check(nf90_def_var(iunit, varname, NF90_INT, dimid, varid))
       call nclayer_check(nf90_def_var_deflate(iunit, varid, 1, 1, int(NLAYER_COMPRESSION)))
    endif
    call nclayer_check(nf90_put_var(iunit, varid, field))
  end subroutine write_ncvar_int


end subroutine write_satobs_data_nc

end module readsatobs
