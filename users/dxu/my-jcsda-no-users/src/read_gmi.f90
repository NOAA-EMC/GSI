subroutine read_gmi(mype,val_gmi,ithin,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub)

!$$$  subprogram documentation block
! subprogram:    read_gmi           read  GMI  bufr data
!   prgmmr: j.jin    copied from read_tmi.f90    date: 2014-5-08
!
! abstract:  This routine reads BUFR format  GMI radiance 
!            (brightness temperature) files.  Optionally, the data 
!            are thinned to a specified resolution using simple 
!            quality control (QC) checks.
!            QC performed in this subroutine are
!             1.obs time check  |obs-anal|<time_window
!             2.remove overlap orbit
!             3.climate check  reject for tb<tbmin or tb>tbmax 
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2014-03-29  j.jin   - read gmis1_gpm and gmis2_gpm obs.
!   2014-05-08  j.jin   - copy/separate from read_tmi.f90. Needs clean up.
!   2014-09-03  j.jin   - read GMI 1CR data, obstype=gmi. Increase the size 
!                         of data_all for channel 10-13 geo-information.
!
!   input argument list:
!     mype     - mpi task id
!     val_gmi - weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read  ex. 'f15'
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!
!   output argument list:
!     nread    - number of BUFR  observations read (after eliminating orbit overlap)
!     ndata    - number of BUFR  profiles retained for further processing (thinned)
!     nodata   - number of BUFR  observations retained for further processing (thinned)
!
! attributes:
!   language: f90
!
! Note:
!   2013-10-21  j.jin   - there is not a procedure for isfcalc.
!                         (isfcalc - specifies method to determine surface fields
!                         within a FOV. When it is equal to one, integrate
!                         model fields over a FOV. When it is not equal to one, bilinearly
!                         interpolate model fields at a FOV center.)
!$$$  end documentation block
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use radinfo, only: iuse_rad,jpch_rad,nusis,nuchan,nst_gsi,nstinfo,use_edges,radedge1,radedge2
  use gridmod, only: diagnostic_reg,regional,rlats,rlons,nlat,nlon,&
      tll2xy,txy2ll
  use constants, only: deg2rad,rad2deg,zero,one,two,three,four,r60inv,rearth
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen
  use deter_sfc_mod, only: deter_sfc
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter

  use clw_mod,  only:  retrieval_mi

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=*),intent(in   ) :: sis
  integer(i_kind), intent(in   ) :: mype,lunout,ithin
  integer(i_kind), intent(in   ) :: mype_root
  integer(i_kind), intent(in   ) :: mype_sub
  integer(i_kind), intent(in   ) :: npe_sub
  integer(i_kind), intent(in   ) :: mpi_comm_sub
  real(r_kind)   , intent(in   ) :: rmesh,gstime,twind
  real(r_kind)   , intent(inout) :: val_gmi
  integer(i_kind),intent(inout)  :: nread
  integer(i_kind),intent(inout)  :: ndata,nodata

! Declare local parameters
  integer(i_kind)           :: maxinfo
  integer(i_kind)           :: maxchanl,ngs

  real(r_kind),  allocatable, dimension(:) :: tbob
  integer(i_kind),allocatable,dimension(:) :: tbmin     ! different tbmin for the channels.
  real(r_double), allocatable,dimension(:) :: mirad, gmichq   ! TBB from strtmbr
  real(r_double)            :: fovn      ! FOVN 
  real(r_double)            :: gmichqcr                       !-erin debug stuff
  real(r_kind),parameter    :: r360=360.0_r_kind
  character(80),parameter   :: satinfo='SAID SIID OGCE GSES SACV'          !use for ufbint()
  character(80),parameter   :: hdr1b='YEAR MNTH DAYS HOUR MINU SECO ORBN'  !use for ufbint()
  integer(i_kind),parameter :: ntime=7, ninfo=5                                     !time header
  real(r_kind)              :: tbmax, satinfo_v(ninfo)
  real(r_double),dimension(ntime):: bfr1bhdr

  character(40)             :: strscan                                     !for GMI, 'GMISQ SCLAT SCLON HMSL'
  integer(i_kind),parameter :: nloc=4                                      !location dat used for ufbint()
  real(r_double),dimension(nloc) :: midat                                  !location data from 

  character(40),parameter   :: strloc='CLATH CLONH'                        !use for ufbint() 
  character(40),parameter   :: strsaza='SAZA'                              !use for ufbint() 
  real(r_double)            :: pixelloc(2)                                 !location data
  character(40),parameter   :: strtmbr='TMBR', strfovn='FOVN'              !use for ufbrep()  
  character(8)              :: subset
  real(r_kind), parameter   :: bmiss=990_r_kind   ! miss values are 999 in bufr
                                                  ! undefined value is 1.0e+11 in bufr data files.

  character(40),parameter   :: str_angls='SAMA SZA  SMA SGA'               ! for gmi bufr only
  integer(i_kind),parameter :: n_angls=4
  real(r_double),allocatable:: val_angls(:,:),pixelsaza(:)

! Declare local variables
  logical        :: assim,outside,iuse, no85GHz, gmi,gmi_low,gmi_hig

  integer(i_kind):: i,k,ntest,ireadsb,ireadmg,irec,isub,next,j
  integer(i_kind):: iret,idate,nchanl,nchanla
  integer(i_kind):: isflg,nreal,idomsfc
  integer(i_kind):: nmind,itx,nele,itt
  integer(i_kind):: iskip
  integer(i_kind):: lnbufr
  integer(i_kind):: ilat,ilon

  real(r_kind) :: sfcr
  real(r_kind) :: pred
  real(r_kind) :: sstime,tdiff,t4dv
  real(r_kind) :: crit1,dist1
  real(r_kind) :: timedif
  real(r_kind),allocatable,dimension(:,:):: data_all

  real(r_kind) :: disterr,disterrmax,dlon00,dlat00

  integer(i_kind) :: nscan,jc,bufsat,js,ij,npos,n, npos_bin
  integer(i_kind),dimension(5):: iobsdate
  real(r_kind):: flgch
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  real(r_kind):: dlat,dlon,dlon_earth,dlat_earth
  real(r_kind):: sat_def_ang,sat_zen_ang,sat_def_ang2,sat_zen_ang2        ! default and derived satellite zenith angle
  real(r_kind):: sat_scan_ang,sat_azimuth_ang,sat_scan_ang2,sat_azimuth_ang2

! ---- sun glint ----
  integer(i_kind):: doy,mlen(12),mday(12),mon,m
  real(r_kind)   :: time_4_sun_glint_calc,clath_sun_glint_calc,clonh_sun_glint_calc
  real(r_kind)   :: sun_zenith,sun_azimuth_ang
  data  mlen/31,28,31,30,31,30, &
             31,31,30,31,30,31/


  integer(i_kind) :: ang_nn, pos_max 
  integer(i_kind),allocatable :: pos_statis(:), npos_all(:,:)

! ---- check clw ----
  real(r_kind)    :: clw,tpwc
  integer(i_kind) :: kraintype,ierrret,nchanl2
! ---- skip some obs at the beginning and end of a scan ----
  integer(i_kind):: radedge_min,radedge_max,iscan_pos,iedge_log,j2


! ------------ erin debug stuff -----------
  integer(i_kind):: cycle1,cycle2,cycle3,cycle4,cycle5,cycle6,cycle7,cycle8
  integer(i_kind):: cycle_rmi


!**************************************************************************


! ------------- erin debug stuff ----------
  cycle1=0
  cycle2=0
  cycle3=0
  cycle4=0
  cycle5=0
  cycle6=0
  cycle7=0
  cycle8=0

  cycle_rmi=0


! Initialize variables
  lnbufr = 15
  disterrmax=zero
  ntest=0
  iscan_pos = 8     ! id in data_all for scan positions
  iedge_log  = 32    ! id in data_all for log if obs is to be obleted beause of locating near scan edges.

  ndata  = 0
  nodata = 0
  nread  = 0
  sat_def_ang =52.8_r_kind   ! default TMI/GMI satellite zenith angle.
  sat_def_ang2=49.2_r_kind   ! default GMI channel 10-13 satellite zenith angle.

  ilon=3 
  ilat=4

  if(nst_gsi>0) then   
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif
  m = 0
  do mon=1,12
     mday(mon) = m
     m = m + mlen(mon)
  end do


! Set various variables depending on type of data to be read

  gmi_low  = obstype  == 'gmi_low'
  gmi_hig  = obstype  == 'gmi_hig'
  gmi      = obstype  == 'gmi'

write(6,*)'****GMI obstype=', obstype      !- erin debug


     nscan  = 221                          ! number of pixels 
     npos_bin = 3                          ! max number of high resolution pixels at a position 
                                           !     (for grouping the obs at a position)
     if(jsatid == 'gpm')bufsat=288         ! Satellite ID (WMO as of 03Jun2014)
     tbmax = 320.0_r_kind                  ! one value for all tmi channels (see data document).
     strscan='GMISQ SCLAT SCLON HMSL' 

     if (gmi_low) then
       maxinfo=34
       maxchanl = 9                        ! number of channels
       nchanl = 9                          ! 9 channls
       nchanla = nchanl
       ngs=1
       allocate (tbmin(maxchanl))
       tbmin = (/50,50,50,50,50,50,50,50,50/) ! copied from GPM_ATBD_1C_V1.2_201404
     else if (gmi_hig) then
       maxinfo=34
       maxchanl = 4                        ! number of channels
       nchanl = 4                          ! 4 channls
       nchanla = nchanl
       ngs=1
       allocate (tbmin(maxchanl))
       tbmin = (/70,70,70,70/)             !
     else if (gmi) then
       maxinfo=39
       maxchanl = 13                       ! number of channels
       nchanl = 13                         ! 13 channls
       nchanla = 9                         ! first 9 channels
       ngs=2
       allocate (tbmin(maxchanl))
       tbmin = (/50,50,50,50,50,50,50,50,50,70,70,70,70/)             !
     endif
     ang_nn=nscan/npos_bin+1
     allocate (tbob(maxchanl), mirad(maxchanl),gmichq(maxchanl))
     allocate (val_angls(n_angls,ngs), pixelsaza(ngs))
     rlndsea(0) = zero
     rlndsea(1) = 30._r_kind
     rlndsea(2) = 30._r_kind
     rlndsea(3) = 30._r_kind
     rlndsea(4) = 100._r_kind

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  radedge_min = 0
  radedge_max = 1000
  assim=.false.
  search: do i=1,jpch_rad
    if (trim(nusis(i))==trim(sis)) then
        if (radedge1(i)/=-1 .and. radedge2(i)/=-1) then
           radedge_min=radedge1(i)
           radedge_max=radedge2(i)
        end if
        if (iuse_rad(i)>=0) then
           if (iuse_rad(i)>0) assim=.true.
           if (assim) exit
        endif
     endif
  end do search
  if (.not.assim) val_gmi=zero

  nchanl2=7    ! cha 1&2 (10 GHz) for GMI are not available for SSMI (clw/tpwc retrievals).
  no85GHz=.false.

! Make thinning grids
  call makegrids(rmesh,ithin)

! Open unit to satellite bufr file
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Allocate arrays to hold data
  nreal  = maxinfo + nstinfo
  nele   = nreal   + nchanl
  allocate(data_all(nele,itxmax))

!       Extract satellite id from the 1st MG.  If it is not the one we want, exit reading.
        call readmg(lnbufr, subset, iret, idate)
        rd_loop: do while (ireadsb(lnbufr)==0)

!write(6,*)'****GMI reading header'               !-erin debug

          call ufbint(lnbufr,satinfo_v,ninfo,1,iret,satinfo)
          if(nint(satinfo_v(1)) /= bufsat) then 
            write(6,*) 'READ_GMI: Bufr satellie ID SAID', nint(satinfo_v(1)), &
                       ' does not match ', bufsat
            go to 690
          endif
        enddo rd_loop
! Big loop to read data file
  next=0
  read_subset: do while(ireadmg(lnbufr,subset,idate)>=0) ! GMI scans
     next=next+1
     if(next == npe_sub)next=0
     if(next /= mype_sub)cycle
     read_loop: do while (ireadsb(lnbufr)==0)            ! GMI pixels
!write(6,*)'****GMI reading data'               !-erin debug

        call ufbrep(lnbufr,fovn,1, 1,iret, strfovn)

!write(6,*)'****GMI fovn=',fovn                 !-erin debug

        ! npos must .LE. 90 because nstep=90 in bias_angle correction code
        !   ../../../../Applications/NCEP_Etc/NCEP_bias/main.f90
        npos = fovn/npos_bin + 1  ! always group scan positions according channel-9's positions. 
        if (.not. use_edges .and. &
             (npos < radedge_min .OR. npos > radedge_max )) then ! cycle read_loop
             cycle1=cycle1+1             ! -erin debug
             cycle read_loop             ! -erin debug
        endif                            ! -erin debug

! ----- extract time information  
        call ufbint(lnbufr,bfr1bhdr,ntime,1,iret,hdr1b)

!write(6,*)'****GMI bfr1bhdr=',bfr1bhdr                 !-erin debug

!       calc obs seqential time. If time is outside window, skip this obs
        iobsdate(1:5) = bfr1bhdr(1:5) !year,month,day,hour,min
        call w3fs21(iobsdate,nmind)
        t4dv=(real(nmind-iwinbgn,r_kind) + real(bfr1bhdr(6),r_kind)*r60inv)*r60inv
        if (l4dvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           sstime=real(nmind,r_kind) + real(bfr1bhdr(6),r_kind)*r60inv
           tdiff=(sstime-gstime)*r60inv
           if(abs(tdiff) > twind) then ! cycle read_loop
             cycle2=cycle2+1             ! -erin debug
             cycle read_loop             ! -erin debug
           endif                         ! -erin debug

        endif

! ----- Read header record to extract obs location information  
        if(gmi_low .or. gmi_hig) then
          call ufbint(lnbufr,midat,nloc,1,iret,strscan)
          gmichq(1:nchanl) = midat(1)
          call ufbint(lnbufr,pixelsaza,1,ngs,iret,strsaza)
          call ufbint(lnbufr,val_angls,n_angls,ngs,iret,str_angls)
        else if (gmi) then
          call ufbint(lnbufr,midat(2:4),nloc,1,iret,'SCLAT SCLON HMSL')
!write(6,*)'****GMI midat(2:4)',midat(2:4)             !-erin debug
          call ufbrep(lnbufr,gmichq,1,nchanl,iret,'GMICHQ')
!          call ufbrep(lnbufr,gmichqcr,1,1,iret,'GMICHQ')     !-erin debug
!write(6,*)'****GMI gmichq',gmichqcr             !-erin debug
          call ufbrep(lnbufr,pixelsaza,1,ngs,iret,strsaza)
          call ufbrep(lnbufr,val_angls,n_angls,ngs,iret,str_angls)
        endif
        if(val_angls(4,1) >=0_i_kind .and. val_angls(4,1) <=20_i_kind) cycle read_loop
        call ufbint(lnbufr,pixelloc,2, 1,iret,strloc)

!---    Extract brightness temperature data.  Apply gross check to data. 
!       If obs fails gross check, reset to missing obs value.
        call ufbrep(lnbufr,mirad,1,nchanl,iret,strtmbr)
!write(6,*)'****GMI mirad',mirad                      !-erin debug
!          Regional case
           dlat_earth = pixelloc(1)  !deg
           dlon_earth = pixelloc(2)  !deg
           if(abs(dlat_earth)>90.0_r_kind .or. abs(dlon_earth)>r360) then ! cycle read_loop
             cycle3=cycle3+1             ! -erin debug
             cycle read_loop             ! -erin debug
           endif                            ! -erin debug

           if(dlon_earth< zero) dlon_earth = dlon_earth+r360
           if(dlon_earth==r360) dlon_earth = dlon_earth-r360
           dlat_earth = dlat_earth*deg2rad
           dlon_earth = dlon_earth*deg2rad

           if(regional)then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if(diagnostic_reg) then
                 call txy2ll(dlon,dlat,dlon00,dlat00)
                 ntest=ntest+1
                 disterr=acos(sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                      (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00)))*rad2deg
                 disterrmax=max(disterrmax,disterr)
              end if

!             Check to see if in domain
              if(outside) cycle read_loop

!          Global case
           else
              dlat = dlat_earth  
              dlon = dlon_earth  
              call grdcrd(dlat,1,rlats,nlat,1)
              call grdcrd(dlon,1,rlons,nlon,1)
           endif
!          If available, set value of zenith angle
           if (pixelsaza(1) < bmiss ) then
              sat_zen_ang = pixelsaza(1)*deg2rad
           else
              sat_zen_ang = sat_def_ang*deg2rad
           endif
           sat_azimuth_ang = val_angls(1,1)*deg2rad
           sun_zenith      = val_angls(2,1)
           sun_azimuth_ang = val_angls(3,1)
           sat_scan_ang = asin( sin(sat_zen_ang)*rearth/(rearth+midat(4)) )
           if(gmi) then
             if (pixelsaza(ngs) < bmiss ) then
               sat_zen_ang2 = pixelsaza(ngs)*deg2rad
             else
               sat_zen_ang2 = sat_def_ang2*deg2rad
             endif
             sat_scan_ang2 = asin( sin(sat_zen_ang2)*rearth/(rearth+midat(4)) )
             sat_azimuth_ang2 = val_angls(1,ngs)*deg2rad
           endif
        
!          Transfer observed brightness temperature to work array.  
!          If any temperature exceeds limits, or data_quality /= 0
!          reset observation to "bad" value.
           iskip=0
           do jc=1, nchanla    ! only does such check the first 9 channels for GMI 1C-R data
! -erin debug comment out check of data quality, since the GSI seems to be
! reading bad values

              if(mirad(jc)<tbmin(jc) .or. mirad(jc)>tbmax ) then ! &
!                .or. gmichqcr > 0 ) then                !-erin debug stuff
                !.or. gmichqcr(jc) > 0 ) then           !JJJ, skip data with data_quality > 0.
                 iskip = iskip + 1
              else
                 nread=nread+1
              end if
           enddo

           do jc=10,nchanl
              if(mirad(jc)>1000.0) then             !-erin stuff
                 mirad(jc) = 500.0                   !-replace missing tbs(ch10-13, scan edge) and toss out in gross check
              endif
           enddo


           if(iskip == nchanla) then !cycle read_loop
             cycle4=cycle4+1             ! -erin debug
             cycle read_loop             ! -erin debug
           endif                            ! -erin debug

 
           tbob = mirad 
           if(gmi) nread=nread + (nchanl - nchanla)

           flgch = 0
           if (l4dvar) then
              crit1 = 0.01_r_kind+ flgch
           else
              timedif = 6.0_r_kind*abs(tdiff) ! range: 0 to 18
              crit1 = 0.01_r_kind+timedif + flgch
           endif

!          Map obs to thinning grid
           call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
           if(.not. iuse) then  !cycle read_loop
             cycle5=cycle5+1             ! -erin debug
             cycle read_loop             ! -erin debug
           endif                            ! -erin debug



           ! if the obs is far from the grid box center, do not use it.
           if(ithin .ne. 0) then
             if(.not. regional .and. dist1 > 0.75_r_kind) cycle read_loop  
           endif

           crit1 = crit1 + 10._r_kind * float(iskip)
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse) then  !cycle read_loop
             cycle6=cycle6+1             ! -erin debug
             cycle read_loop             ! -erin debug
           endif                            ! -erin debug


!          Locate the observation on the analysis grid.  Get sst and land/sea/ice
!          mask.  

!       isflg    - surface flag
!                  0 sea
!                  1 land
!                  2 sea ice
!                  3 snow
!                  4 mixed                     

           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
              ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

!          Only keep obs over ocean    - ej
           if(isflg .ne. 0) cycle read_loop

!------------------ use for erin's work (temporarily)
           if(.not. regional .and. isflg==0 .and. &
              (gmi_low .or. gmi) ) then
             ! retrieve tpwc and clw

             call retrieval_mi(tbob(3:9),nchanl2,no85GHz, &
               tpwc,clw,kraintype,ierrret )


             ! don't use obsercation for cases that there are thick clouds or rain, or clw/twc cannot be retrieved.
             ! Increse the clw threshold 3 => 5 in order to read more data, but these data don't pass QC.  8/28/2014
             if(tpwc<0 .or. clw > 5 .or. kraintype /=0_i_kind .or. ierrret > 0) then !cycle read_loop
                cycle_rmi=cycle_rmi+1                          ! erin debug
!                cycle read_loop                                ! erin debug
             endif                                             ! erin debug
           endif
!--------------------- used for erin's work

           crit1 = crit1 + rlndsea(isflg)
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse) then   !cycle read_loop
             cycle7=cycle7+1             ! -erin debug
             cycle read_loop             ! -erin debug
           endif                            ! -erin debug



           call finalcheck(dist1,crit1,itx,iuse)
           if(.not. iuse) then !cycle read_loop
             cycle8=cycle8+1             ! -erin debug
             cycle read_loop             ! -erin debug
           endif                            ! -erin debug



!          interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
!
           if(nst_gsi>0) then
              tref  = ts(0)
              dtw   = zero
              dtc   = zero
              tz_tr = one
              if(sfcpct(0)>zero) then
                 call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
              endif
           endif

!          Transfer observation parameters to output array.  
           data_all( 1,itx) = bufsat              ! satellite id
           data_all( 2,itx) = t4dv                ! time diff between obs and anal (min)
           data_all( 3,itx) = dlon                ! grid relative longitude
           data_all( 4,itx) = dlat                ! grid relative latitude
           data_all( 5,itx) = sat_zen_ang         ! local (satellite) zenith angle (radians)
           data_all( 6,itx) = sat_azimuth_ang     ! local (satellite) azimuth_ang angle (radians)
           data_all( 7,itx) = sat_scan_ang        ! scan(look) angle (rad)
           data_all( 8,itx) = npos                ! scan position,  .le. 90
           data_all( 9,itx) = sun_zenith          ! solar zenith angle (deg)
           data_all(10,itx) = sun_azimuth_ang         ! solar azimuth_ang angle (deg)
           data_all(11,itx) = sfcpct(0)           ! sea percentage of
           data_all(12,itx) = sfcpct(1)           ! land percentage
           data_all(13,itx) = sfcpct(2)           ! sea ice percentage
           data_all(14,itx) = sfcpct(3)           ! snow percentage
           data_all(15,itx)= ts(0)                ! ocean skin temperature
           data_all(16,itx)= ts(1)                ! land skin temperature
           data_all(17,itx)= ts(2)                ! ice skin temperature
           data_all(18,itx)= ts(3)                ! snow skin temperature
           data_all(19,itx)= tsavg                ! average skin temperature
           data_all(20,itx)= vty                  ! vegetation type
           data_all(21,itx)= vfr                  ! vegetation fraction
           data_all(22,itx)= sty                  ! soil type
           data_all(23,itx)= stp                  ! soil temperature
           data_all(24,itx)= sm                   ! soil moisture
           data_all(25,itx)= sn                   ! snow depth
           data_all(26,itx)= zz                   ! surface height
           data_all(27,itx)= idomsfc + 0.001_r_kind ! dominate surface type
           data_all(28,itx)= sfcr                 ! surface roughness
           data_all(29,itx)= ff10                 ! ten meter wind factor
           data_all(30,itx)= dlon_earth*rad2deg   ! earth relative longitude (degrees)
           data_all(31,itx)= dlat_earth*rad2deg   ! earth relative latitude (degrees)
           data_all(iedge_log,itx) = 0            ! =0, not to be obsoleted as at scan edges
           if(gmi) then                           ! for channels 10-13
             data_all(33,itx) = sat_zen_ang2      ! local (satellite) zenith angle (radians)
             data_all(34,itx) = sat_azimuth_ang2  ! local (satellite) azimuth_ang angle (radians)
             data_all(35,itx) = sat_scan_ang2     ! scan(look) angle (rad)
             data_all(36,itx) = val_angls(2,ngs)  ! solar zenith angle (deg)
             data_all(37,itx) = val_angls(3,ngs)  ! solar azimuth_ang angle (deg)
           endif

           data_all(maxinfo-1,itx)= val_gmi
           data_all(maxinfo,itx)= itt

           if(nst_gsi>0) then
              data_all(maxinfo+1,itx) = tref       ! foundation temperature
              data_all(maxinfo+2,itx) = dtw        ! dt_warm at zob
              data_all(maxinfo+3,itx) = dtc        ! dt_cool at zob
              data_all(maxinfo+4,itx) = tz_tr      ! d(Tz)/d(Tr)
           endif

           do i=1,nchanl
              data_all(i+nreal,itx)=tbob(i)
           end do

     end do read_loop
  end do read_subset
690 continue
  call closbf(lnbufr)

!------------------ erin debug stuff----------------
write(6,*)'****GMI cycle1,2,3,4,5,6,7,8,rmi'&
,cycle1,cycle2,cycle3,cycle4,cycle5,cycle6,cycle7,cycle8,cycle_rmi



! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit)
  if ( gmi_low ) then
     write(6,*) 'READ_GMI_LOW: after combine_obs, nread,ndata is ',nread,ndata
  elseif ( gmi_hig ) then
     write(6,*) 'READ_GMI_HIG: after combine_obs, nread,ndata is ',nread,ndata
  elseif ( gmi ) then
     write(6,*) 'READ_GMI: after combine_obs, nread,ndata is ',nread,ndata
  endif

!=========================================================================================================
  if( use_edges .and. (radedge_min > 1 .or. radedge_max < ang_nn).and. mype_sub==mype_root )then
    ! Obsolete some obs at the beginning and end positions of a scan by flagging
    !       obs at these positions with negative NPOS values.
    ! Note: This is an arbitary process. Just want to phase out part of these obs
    !       at the scan edges in the QC process (qc_ssmi, ifail_scanedge_qc=58).
    !       However, there is not a known quality issue at the edge of scans.
    !       JJJ, 2/12/2014
     pos_max=ndata  
     allocate(pos_statis(ang_nn))
     allocate(npos_all(pos_max,ang_nn))
     npos_all = 0
     pos_statis = 0
     do n=1,ndata
        i = nint(data_all(iscan_pos,n))
        pos_statis(i) = pos_statis(i) + 1
        npos_all(pos_statis(i), i) = n
     enddo

     do n=1, ndata
        i = nint(data_all(iscan_pos,n))
        if(i < radedge_min .or. i > radedge_max) then
          data_all(iedge_log,n) = 1     ! assume all at scan edges at the beginning.
        endif
     enddo
     if( radedge_min > 1 )then
       pos_max = sum(pos_statis(radedge_min : (radedge_min+1)))/2 
       do i=radedge_min-1, 1, -1
         if(pos_max==0) then
           j2=1
         else
           j2=nint(float(pos_statis(i))/pos_max)
           j2=max(1,j2)
         endif
         do j=1,pos_statis(i),j2
           n = npos_all(j,i)
           data_all(iedge_log,n)= 0     ! flag back
         enddo      
       enddo
     endif

     if( radedge_max < ang_nn )then
       pos_max = sum(pos_statis((radedge_max-1) : radedge_max))/2 
       do i=radedge_max+1,ang_nn
         if(pos_max==0) then
           j2=1
         else
           j2=nint(float(pos_statis(i))/pos_max)
           j2=max(1,j2)
         endif
         do j=1,pos_statis(i),j2
           n = npos_all(j,i)
           data_all(iedge_log,n)= 0     ! flag back
         enddo      
       enddo
     endif

     ! new pos_statis
     pos_statis=0
     do n=1,ndata
        i = nint(data_all(iscan_pos,n))
        if(data_all(iedge_log,n)>0) cycle
        pos_statis(i) = pos_statis(i) + 1
     enddo
     write(6,*) 'READ_', trim(obstype), ': after obsolete_obs near edges, ndata ', sum(pos_statis)
     !write(6,*) 'READ_', trim(obstype), ': number of observations: '
     !write(6, '(5x, 10I10)')  pos_statis
     deallocate(pos_statis, npos_all)
  endif ! use_edges, but flag part of obs at the scan edges with negative FOV values.
!=========================================================================================================

! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root.and.ndata>0) then

!    Identify "bad" observation (unreasonable brightness temperatures).
!    Update superobs sum according to observation location

     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin(i) .and. &
              data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
        itt=nint(data_all(maxinfo,n))
        super_val(itt)=super_val(itt)+val_gmi
     end do
!    Write final set of "best" observations to output file
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif

! Deallocate data arrays
  deallocate(data_all)


! Deallocate satthin arrays
1000 continue
  call destroygrids

  if(diagnostic_reg .and. ntest>0 .and. mype_sub==mype_root) &
     write(6,*)'READ_GMI:  mype,ntest,disterrmax=',&
        mype,ntest,disterrmax

  deallocate(tbmin, tbob, mirad, gmichq)
  deallocate(val_angls, pixelsaza)
! End of routine
 return
end subroutine read_gmi


