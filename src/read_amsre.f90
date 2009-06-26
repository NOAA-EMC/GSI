subroutine read_amsre(mype,val_amsre,ithin,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub)

! subprogram:    read_amsre                  read bufr format amsre data
! prgmmr :   okamoto         org: np20                date: 2004-10-12
!
! abstract:  This routine reads BUFR format AQUA radiance (brightness
!            temperature) files.  Optionally, the data are thinned to 
!            a specified resolution using simple quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! Special Notes:
!     10/14/04  okamoto  looks like AMSRE overlap problem is not as bad as SSM/I
!
! program history log:
!   2004-10-12  okamoto
!   2005-10-07  Xu and Pawlak - modify the code related to ityp determination to
!                  use routine deter_ityp, created special notes section, removed
!                  GrADS specific code, fixed indentation
!   2005-10-10  treadon - replace deter_ityp with deter_sfc, modify rlndsea to be
!                         consistent with other read_* routines
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-20  kazumori - modify to read real AMSR-E bufr data and add zensun
!   2005-11-17  kazumori - add deter_sfc_amsre_low for AMSR-E low frequency channel
!   2005-11-29  parrish -  modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-02  kazumori - modify the threshold of surface determination and change
!                          the origin of satellite azimuth angle for diag file
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-26  kazumori - bug fix of order of polarization, timedif, tbmax
!   2006-05-17  kazumori - modify for new bufrtable change
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-08-05  kazumori - add good fov selection for amsre low channel data
!   2006-09-20  treadon - remove good fov selection for amsre low channel
!                         data in order to add mpi_io for data read
!   2006-10-22  kazumori - bug fix for the type of zensun subroutine argument
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-05-28  safford - rm unused vars
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!
! input argument list:
!     mype     - mpi task id
!     val_amsre- weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read  ex.49
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
! output argument list:
!     nread    - number of BUFR AQUA observations read
!     ndata    - number of BUFR AQUA profiles retained for further processing
!     nodata   - number of BUFR AQUA observations retained for further processing
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
              checkob,finalcheck,score_crit
  use radinfo, only: iuse_rad,cbias,nusis,jpch_rad
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,rlats,rlons,&
       tll2xy,txy2ll
  use constants, only: deg2rad,rad2deg,zero,one,two,three,izero,r60inv
  use gsi_4dvar, only: l4dvar, idmodel, iwinbgn, winlen
  implicit none

! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: N_AMSRCH  =  12
! integer(i_kind),parameter :: N_MAXCH   =  20 
  integer(i_kind) :: said, AQUA_SAID  = 784  !WMO satellite identifier 
  integer(i_kind) :: siid, AMSRE_SIID = 345  !WMO instrument identifier 
  integer(i_kind),parameter :: maxinfo    =  34

! Input variables
  character(len=*) ,intent(in) :: infile
  character(len=*) ,intent(in) :: obstype
  integer(i_kind)  ,intent(in) :: mype
  integer(i_kind)  ,intent(in) :: ithin
  character(len=*) ,intent(in) :: jsatid
  integer(i_kind)  ,intent(in) :: lunout
  real(r_kind)     ,intent(inout) :: val_amsre
  real(r_kind)     ,intent(in) :: gstime,twind
  real(r_kind)     ,intent(in) :: rmesh
  character(len=*) ,intent(in) :: sis
  integer(i_kind)  ,intent(in) :: mype_root
  integer(i_kind)  ,intent(in) :: mype_sub
  integer(i_kind)  ,intent(in) :: npe_sub
  integer(i_kind)  ,intent(in) :: mpi_comm_sub

! Output variables
  integer(i_kind)  ,intent(inout) :: nread
  integer(i_kind)  ,intent(inout) :: ndata,nodata

! BUFR file sequencial number
  character(len=8)  :: subset,subfgn
  character(len=4)  :: senname
  character(len=8)  :: spotname
  character(len=8)  :: channame
  integer(i_kind) :: lnbufr = 10
  integer(i_kind)   :: nchanl
  integer(i_kind)   :: iret,isflg,idomsfc

! Work variables for time
  character(len=10) :: date
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  integer(i_kind)   :: nmind
  integer(i_kind)   :: iy, im, idd, ihh
  real(r_kind)     :: sstime, tdiff, t4dv

! Other work variables
  logical           :: outside,iuse,assim
  integer(i_kind)   :: nreal, kidsat
  integer(i_kind)   :: itx, k, nele, itt, iobsout
  integer(i_kind)   :: ifov, ilat, ilon
  integer(i_kind)   :: i, l, n
  integer(i_kind)   :: file_handle,ierror,nblocks
  integer(i_kind),dimension(n_amsrch) :: kchamsre
  real(r_kind)     :: sfcr
  real(r_kind)     :: dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth
  real(r_kind)     :: timedif, pred, crit1, dist1
  real(r_kind),allocatable,dimension(:,:):: data_all
  integer(i_kind):: isubset,irec,isub,next
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10

  real(r_kind) :: clath_sun_glint_calc , clonh_sun_glint_calc 
  real(r_kind) :: date5_4_sun_glint_calc

! Set standard parameters
  logical       :: amsre_low
  logical       :: amsre_mid
  logical       :: amsre_hig
  integer(i_kind) ntest
  integer(i_kind) :: nscan,iskip,kskip,kch,kchanl
  real(r_kind),parameter :: POINT001 =   0.001_r_kind
! real(r_kind),parameter :: POINT01  =   0.01_r_kind
! real(r_kind),parameter :: TEN      =  10._r_kind
! real(r_kind),parameter :: R45      =  45._r_kind
  real(r_kind),parameter :: R90      =  90._r_kind
! real(r_kind),parameter :: R180     = 180._r_kind
  real(r_kind),parameter :: R360     = 360._r_kind
  real(r_kind),parameter :: tbmin    = 70._r_kind
  real(r_kind),parameter :: tbmax    = 330._r_kind       !tbmax is larger than same as ssmiqc
  real(r_kind),parameter :: tbbad    = -9.99e11_r_kind                                        
  real(r_kind) disterrmax
  real(r_kind),dimension(N_AMSRCH) :: tbob_org
  real(r_kind) :: clath, clonh, fovn, saza, soza
  real(r_kind) :: flgch  !used for thinning priority  range:1-36

! AMSR-E-bufr
! BUFR format for AQUASPOT
! integer(i_kind),parameter :: N_AQUASPOT_LIST = 25
  type aquaspot_list
     sequence
     real(r_kind) :: said
     real(r_kind) :: orbn
     real(r_kind) :: slnm
     real(r_kind) :: mjfc
     real(r_kind) :: selv
     real(r_kind) :: soza
     real(r_kind) :: solazi
     real(r_kind) :: intms(9)
  end type aquaspot_list

! BUFR format for AMSRSPOT
  integer(i_kind),parameter :: N_AMSRSPOT_LIST = 12
  type amsrspot_list
     sequence
     real(r_kind) :: siid
     real(r_kind) :: year
     real(r_kind) :: mnth
     real(r_kind) :: days
     real(r_kind) :: hour
     real(r_kind) :: minu
     real(r_kind) :: seco
     real(r_kind) :: clath
     real(r_kind) :: clonh
     real(r_kind) :: saza
     real(r_kind) :: bearaz
     real(r_kind) :: fovn
  end type amsrspot_list

! BUFR format for AMSRCHAN
  integer(i_kind),parameter :: N_AMSRCHAN_LIST = 4
  type amsrchan_list
     sequence
     real(r_kind) :: chnm
     real(r_kind) :: logrcw
     real(r_kind) :: acqf
     real(r_kind) :: tmbr
  end type amsrchan_list

! BUFR format for AMSRDICE
! integer(i_kind),parameter :: N_AMSRDICE_LIST = 21
  type amsrdice_list
     sequence
     real(r_kind) :: siid
     real(r_kind) :: year
     real(r_kind) :: mnth
     real(r_kind) :: days
     real(r_kind) :: hour
     real(r_kind) :: minu
     real(r_kind) :: seco
     real(r_kind) :: clath
     real(r_kind) :: clonh
     real(r_kind) :: saza
     real(r_kind) :: bearaz
     real(r_kind) :: fovn

     real(r_kind) :: d1_fovn

     real(r_kind) :: d1_chnm
     real(r_kind) :: d1_logrcw
     real(r_kind) :: d1_acqf
     real(r_kind) :: d1_tmbr

     real(r_kind) :: d2_chnm
     real(r_kind) :: d2_logrcw
     real(r_kind) :: d2_acqf
     real(r_kind) :: d2_tmbr
  end type amsrdice_list

! Variables for BUFR IO
  real(r_double),dimension(3):: aquaspot_d
  real(r_double),dimension(12):: amsrspot_d
  real(r_double),dimension(4,12):: amsrchan_d
  real(r_double),dimension(2,5):: amsrdice_latlon
  real(r_double),dimension(20) :: amsrdice_tmbr

  type(aquaspot_list) :: aquaspot
  type(amsrspot_list) :: amsrspot
  type(amsrchan_list) :: amsrchan(12)
  type(amsrdice_list) :: amsrdice(4)

! ---- sun glint ----
  integer(i_kind) doy,mlen(12),mday(12),mon,m
  real(r_kind) bearaz,sun_zenith,sun_azimuth,sun_gangle
  data  mlen/31,28,31,30,31,30,31,31,30,31,30,31/ 

! Orbit
! logical :: remove_ovlporbit = .true. !looks like AMSRE overlap problem is not as bad as SSM/I 10/14/04  kozo
  logical :: first_scen
  integer(i_kind) :: orbit, old_orbit, iorbit, ireadsb, ireadmg
  integer(i_kind) :: nscen0_reject(20) 
  real(r_kind) :: saz

! data selection
  real(r_kind) tb_threshold_fov_inside 

! tmp
  integer(i_kind):: ipass(6)

! Initialize variables
  ilon = 3
  ilat = 4
  m = 0 
  do mon=1,12 
    mday(mon) = m 
    m = m + mlen(mon) 
  end do 
  disterrmax=zero
  ntest = 0
  nreal = maxinfo
  ndata = 0
  nodata = 0
  amsre_low=     obstype == 'amsre_low'
  amsre_mid=     obstype == 'amsre_mid'
  amsre_hig=     obstype == 'amsre_hig'
  ipass(:)=0
  orbit = -1
  old_orbit=-1
  iorbit = 0
  nscen0_reject(:) = 0
  first_scen=.true.
  sstime = 0.0
  if(amsre_low)then
    kchanl=4
    kchamsre(1:4)=(/1,2,3,4/)
    tb_threshold_fov_inside = 3.0_r_kind
  else if(amsre_mid) then
    kchanl=6
    kchamsre(1:6)=(/5,6,7,8,9,10/)
    tb_threshold_fov_inside = 300.0_r_kind
  else if(amsre_hig)then
    kchanl=2
    kchamsre(1:2)=(/11,12/)
    tb_threshold_fov_inside = 300.0_r_kind
  end if

  if(amsre_low .or. amsre_mid .or. amsre_hig)then
     senname = 'AMSR'
     nchanl  = N_AMSRCH
     subfgn = 'NC021254'
     nscan  = 196  !for low frequency ch
!    nscan  = 392  !for 89.0GHz ch
     kidsat = 549  
     rlndsea(0) = 0._r_kind
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  endif
  spotname = trim(senname)//'SPOT'
  channame = trim(senname)//'CHAN'

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((nusis(i)==sis) .and. (iuse_rad(i)>0)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_amsre=zero


! Make thinning grids
  call makegrids(rmesh,ithin)


! Open BUFR file
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Allocate local array to contain observation information
  nele=nreal+nchanl
  allocate(data_all(nele,itxmax))

! Big loop to read data file
  next=mype_sub+1
  do while(ireadmg(lnbufr,subset,idate)>=0)
  call ufbcnt(lnbufr,irec,isub)
  if(irec/=next)cycle; next=next+npe_sub
  read_loop: do while (ireadsb(lnbufr)==0)

!    Retrieve bufr 1/4 :get aquaspot (said,orbn,soza)
       call ufbint(lnbufr,aquaspot_d,3,1,iret,'SAID ORBN SOZA')
       aquaspot%said=aquaspot_d(1)
       aquaspot%orbn=aquaspot_d(2)
       aquaspot%soza=aquaspot_d(3)

        said = int( aquaspot%said  + POINT001 )
        if(said /= AQUA_SAID)  cycle read_loop

!    Retrieve bufr 2/4 :get amsrspot (siid,ymdhs,lat,lon)
     call ufbrep(lnbufr,amsrspot_d,N_AMSRSPOT_LIST,1,iret, &
         'SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ FOVN')
     amsrspot%siid  = amsrspot_d(01)
     amsrspot%year  = amsrspot_d(02)
     amsrspot%mnth  = amsrspot_d(03)
     amsrspot%days  = amsrspot_d(04)
     amsrspot%hour  = amsrspot_d(05)
     amsrspot%minu  = amsrspot_d(06)
     amsrspot%seco  = amsrspot_d(07)
     amsrspot%clath = amsrspot_d(08)
     amsrspot%clonh = amsrspot_d(09)
     amsrspot%saza  = amsrspot_d(10)
     amsrspot%bearaz= amsrspot_d(11)
     amsrspot%fovn  = amsrspot_d(12)

        siid = int( amsrspot%siid + POINT001 ) 
        if(siid /= AMSRE_SIID)  cycle read_loop

!    Retrieve bufr 3/4 : get amsrchan (chnm,tbb)
     call ufbrep(lnbufr,amsrchan_d,N_AMSRCHAN_LIST,12,iret,'CHNM LOGRCW ACQF TMBR')
     do i=1,12
        amsrchan(i)%chnm  =amsrchan_d(1,i)
        amsrchan(i)%logrcw=amsrchan_d(2,i)
        amsrchan(i)%acqf  =amsrchan_d(3,i)
        amsrchan(i)%tmbr  =amsrchan_d(4,i)
     end do

!    Retrieve bufr 4/4 : get amsrfovn (fovn)
     call ufbrep(lnbufr,amsrdice_latlon,2, 5,iret,'CLATH CLONH')
     call ufbrep(lnbufr,amsrdice_tmbr,  1,20,iret,'TMBR')

     amsrdice(3)%d1_tmbr = amsrdice_tmbr(17)
     amsrdice(3)%d2_tmbr = amsrdice_tmbr(18)
     amsrdice(3)%clath   = amsrdice_latlon(1,4)
     amsrdice(3)%clonh   = amsrdice_latlon(2,4)

     tbob_org(1)=amsrchan(2)%tmbr
     tbob_org(2)=amsrchan(1)%tmbr
     tbob_org(3)=amsrchan(4)%tmbr
     tbob_org(4)=amsrchan(3)%tmbr
     tbob_org(5)=amsrchan(6)%tmbr
     tbob_org(6)=amsrchan(5)%tmbr
     tbob_org(7)=amsrchan(8)%tmbr
     tbob_org(8)=amsrchan(7)%tmbr
     tbob_org(9)=amsrchan(10)%tmbr
     tbob_org(10)=amsrchan(9)%tmbr
     tbob_org(11)=amsrdice(3)%d2_tmbr
     tbob_org(12)=amsrdice(3)%d1_tmbr
     if(amsre_low .or. amsre_mid) then
       clath=amsrspot%clath
       clonh=amsrspot%clonh
     else if(amsre_hig)then
       clath = amsrdice(3)%clath
       clonh = amsrdice(3)%clonh
     endif
     idate5(1) = amsrspot%year ! year
     idate5(2) = amsrspot%mnth ! month
     idate5(3) = amsrspot%days ! day
     idate5(4) = amsrspot%hour ! hour
     idate5(5) = amsrspot%minu ! sec
     soza = aquaspot%soza    !solar zenith angle
     bearaz = amsrspot%bearaz

!    Set brightness temerature
!    Retrieve Tb

!    Check obs time
           if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
             idate5(2) <    1 .or. idate5(2) >   12 .or. &
             idate5(3) <    1 .or. idate5(3) >   31 .or. &
             idate5(4) <    0 .or. idate5(4) >   24 .or. &
             idate5(5) <    0 .or. idate5(5) >   60 )then
             write(6,*)'READ_AMSRE:  ### ERROR IN READING BUFR DATA:', &
               ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
             cycle read_loop
           endif
           call w3fs21(idate5,nmind)
           t4dv = (real((nmind-iwinbgn),r_kind) + real(idate5(5),r_kind)*r60inv)*r60inv ! add in seconds
           if (l4dvar) then
             if (t4dv<zero .OR. t4dv>winlen) exit
           else
             sstime = real(nmind,r_kind) + real(idate5(5),r_kind)*r60inv ! add in seconds
             tdiff  = (sstime - gstime)*r60inv
             if (abs(tdiff)>twind) exit
           endif

           ipass(1) = ipass(1)+1

!     --- Check observing position -----
           if( abs(clath) > R90  .or. abs(clonh) > R360 .or. &
             (abs(clath) == R90 .and. clonh /= ZERO) )  then
!              write(6,*)'READ_AMSRE:  ### ERROR IN READING BUFR DATA:',&
!               ' STRANGE OBS POINT (LAT,LON):', clath, clonh
              cycle read_loop
           endif

!    Pick up every three scene  3,6,9,,,,195 (num=65)
!    because of too small scene size and too many scene numbers
!    (low-freq ch FOV are overlapped side by side)
           fovn = amsrspot%fovn
           ifov = int( fovn + POINT001 )

           ipass(2) = ipass(2)+1

           ipass(3) = ipass(3)+1


!    Retrieve sat.zenith andgle and solar zenith angle

!    Check observational info 
           if( soza < -180._r_kind .or. soza > 180._r_kind )then
             write(6,*)'READ_AMSRE:  ### ERROR IN READING BUFR DATA:', &
                ' STRANGE OBS INFO(FOV,SAZA,SOZA):', fovn, saza, soza
             cycle read_loop
           endif
           ipass(4) = ipass(4)+1
     
!    Set position in a given region
           if(clonh >= R360)then
             clonh = clonh - R360
           else if(clonh < ZERO)then
             clonh = clonh + R360
           endif
     
!    If regional, map obs lat,lon to rotated grid.
           dlat_earth = clath * deg2rad
           dlon_earth = clonh * deg2rad
           if(regional)then
        
!       Convert to rotated coordinate.  dlon centered on 180 (pi),
!       so always positive for limited area
             call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)

!       Check to see if in domain.  outside=.true. if dlon_earth,
!       dlat_earth outside domain, =.false. if inside
             if(outside) cycle read_loop

!    Global case 
           else
             dlat=dlat_earth
             dlon=dlon_earth
             call grdcrd(dlat,1,rlats,nlat,1)
             call grdcrd(dlon,1,rlons,nlon,1)
           endif
     
!  -------- Retreive Sun glint angle -----------
           if ((mod( int(idate5(1)),4).eq.0).and.( int(idate5(2)) .gt.2))  then 
             doy = mday( int(idate5(2)) ) + int(idate5(3)) + 1 
           else 
             doy = mday( int(idate5(2)) ) + int(idate5(3))
           end if 

           bearaz=amsrspot%bearaz-180.0_r_kind    

            clath_sun_glint_calc = clath
            clonh_sun_glint_calc = clonh
            if(clonh>180) clonh_sun_glint_calc = clonh -360.0_r_kind
            date5_4_sun_glint_calc = idate5(4)
            call zensun(doy,date5_4_sun_glint_calc,clath_sun_glint_calc,clonh_sun_glint_calc,sun_zenith,sun_azimuth)

           sun_gangle =  &
             acos(       &
             cos( (90.0_r_kind-55.0_r_kind)*deg2rad ) * &
             cos( (90.0_r_kind-bearaz)*deg2rad ) * &
             cos( sun_zenith*deg2rad ) * &
             cos( (90.0_r_kind-sun_azimuth)*deg2rad ) + &
             cos( (90.0_r_kind-55.0_r_kind)*deg2rad ) * &
             sin( (90.0_r_kind-bearaz)*deg2rad ) * &
             cos( sun_zenith*deg2rad ) * &
             sin( (90.0_r_kind-sun_azimuth)*deg2rad ) + &
             sin( (90.0_r_kind-55.0_r_kind)*deg2rad ) * &
             sin( sun_zenith*deg2rad ) &
             ) * rad2deg

           if( sun_gangle > 25.0_r_kind ) ipass(5) = ipass(5)+1  


!    Set obs information

           iskip = 0 
           do l=1,nchanl
             if(tbob_org(l)<tbmin .or. tbob_org(l)>tbmax)then
               tbob_org(l) = tbbad
               iskip = iskip + 1
             end if
           end do
           kskip = 0 
           do l=1,kchanl
             kch=kchamsre(l)
             if(tbob_org(kch)<tbmin .or. tbob_org(kch)>tbmax)then
               kskip = kskip + 1
             else
               nread=nread+1
             endif
           end do
           if(kskip == kchanl .or. iskip == nchanl) cycle read_loop
           if (l4dvar) then
             timedif = 0.0
           else
             timedif = 6.0_r_kind*abs(tdiff) ! range:  0 to 18
           endif
           flgch=iskip*3.0  !used for thin, range 0 to 36
           crit1 = 0.01_r_kind+timedif + flgch
           call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
           if (.not.iuse) cycle read_loop

           ipass(6) = ipass(6)+1

!    QC:  "Score" observation.  We use this information to identify "best" obs

!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask   

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                       

           call deter_sfc_type(dlat_earth,dlon_earth,t4dv,isflg,tsavg)

           if (amsre_low) then
             call deter_sfc_amsre_low(dlat_earth,dlon_earth,isflg,sfcpct)
           endif

           crit1 = crit1 + rlndsea(isflg)
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

!    Set data quality predictor ***NEED TO COME UP WITH THIS***
           pred = zero

!    Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"

!    Map obs to grids
           crit1 = crit1+pred
           call finalcheck(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop
     
             if(amsre_low .or. amsre_mid) then
               saz = 55.0_r_kind*deg2rad   ! satellite zenith angle (rad) 
             else if (amsre_hig) then
               saz = 54.5_r_kind*deg2rad   ! satellite zenith angle (rad) 
             end if
!            saz = amsrspot(10,1)*deg2rad   ! satellite zenith angle (rad) 
!               ==> not use this value but fixed values(55.0 deg)   10/12/04
!               because BUFR saza value looks strange (raging -3 to 25),

             data_all(1,itx) = 49                      ! satellite ID
             data_all(2,itx) = t4dv                    ! time diff (obs - anal) (hours)
             data_all(3,itx) = dlon                    ! grid relative longitude
             data_all(4,itx) = dlat                    ! grid relative latitude
             data_all(5,itx) = saz                     ! satellite zenith angle (rad)
             data_all(6,itx) = amsrspot%bearaz         ! satellite azimuth angle
             data_all(7,itx) = zero                    ! look angle (rad)
!            data_all(8,itx) = ifov                    ! fov number    1-196
             data_all(8,itx) = ifov/3 + 1              ! fov number/3  1-65 !kozo
             data_all(9,itx) = sun_zenith              ! solar zenith angle (deg)
             data_all(10,itx)= sun_azimuth             ! solar azimuth angle (deg)
             data_all(30,itx)= dlon_earth              ! earth relative longitude (rad)
             data_all(31,itx)= dlat_earth              ! earth relative latitude (rad)
             data_all(32,itx)= sun_gangle              ! sun glint angle

             data_all(33,itx)= val_amsre
             data_all(34,itx)= itt

             do l=1,nchanl
               data_all(l+nreal,itx) = tbob_org(l)
             end do


  enddo read_loop
  enddo
  call closbf(lnbufr)

! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  call combine_radobs(mype,mype_sub,mype_root,npe_sub,mpi_comm_sub,&
          nele,itxmax,nread,ndata,data_all,score_crit)


! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root.and.ndata>0) then

!    Identify "bad" observation (unreasonable brightness temperatures).
!    Update superobs sum according to observation location

     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin .and. &
                data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
        itt=nint(data_all(nreal,n))
        super_val(itt)=super_val(itt)+val_amsre
        tdiff = data_all(2,n)                ! time (hours)
        dlon=data_all(3,n)                   ! grid relative longitude
        dlat=data_all(4,n)                   ! grid relative latitude
        dlon_earth = data_all(30,n)  ! earth relative longitude (degrees)
        dlat_earth = data_all(31,n)  ! earth relative latitude (degrees)

        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,tdiff,isflg,idomsfc,sfcpct, &
            ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
        if (amsre_low) then
          call deter_sfc_amsre_low(dlat_earth,dlon_earth,isflg,sfcpct)
        endif
        data_all(11,n) = sfcpct(0)           ! sea percentage of
        data_all(12,n) = sfcpct(1)           ! land percentage
        data_all(13,n) = sfcpct(2)           ! sea ice percentage
        data_all(14,n) = sfcpct(3)           ! snow percentage
        data_all(15,n)= ts(0)                ! ocean skin temperature
        data_all(16,n)= ts(1)                ! land skin temperature
        data_all(17,n)= ts(2)                ! ice skin temperature
        data_all(18,n)= ts(3)                ! snow skin temperature
        data_all(19,n)= tsavg                ! average skin temperature
        data_all(20,n)= vty                  ! vegetation type
        data_all(21,n)= vfr                  ! vegetation fraction
        data_all(22,n)= sty                  ! soil type
        data_all(23,n)= stp                  ! soil temperature
        data_all(24,n)= sm                   ! soil moisture
        data_all(25,n)= sn                   ! snow depth
        data_all(26,n)= zz                   ! surface height
        data_all(27,n)= idomsfc + 0.001      ! dominate surface type
        data_all(28,n)= sfcr                 ! surface roughness
        data_all(29,n)= ff10                 ! ten meter wind factor
        data_all(30,n)= data_all(30,n)*rad2deg  ! earth relative longitude (degrees)
        data_all(31,n)= data_all(31,n)*rad2deg  ! earth relative latitude (degrees)

     end do

!    Write final set of "best" observations to output file
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif

  deallocate(data_all) ! Deallocate data arrays
  call destroygrids    ! Deallocate satthin arrays

  if(diagnostic_reg.and.ntest.gt.0 .and. mype_sub==mype_root) &
       write(6,*)'READ_AMSRE:  ',&
       'mype,ntest,disterrmax=',mype,ntest,disterrmax

  return
end subroutine read_amsre

subroutine zensun(day,time,lat,lon,sun_zenith,sun_azimuth)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  zensun         make sun zenith and sun azimuth angle
!
!   prgmmr: Paul Ricchiazzi org: Earth Space Research Group,UCSB  date: 1992-10-23
!
! abstract: 
!       Compute solar position information as a function of
!      geographic coordinates, date and time.
!
! program history log:
!   2005-10-21  kazumori - reformatted for GSI
!
!   input argument list:
!     day -     Julian day (positive scalar or vector)
!               (spring equinox =  80)
!               (summer solstice= 171)
!               (fall equinox   = 266)
!               (winter solstice= 356)
!     time -    Universal Time in hours (scalar or vector)
!     lat  -    geographic latitude of point on earth's surface (degrees)
!     lon  -    geographic longitude of point on earth's surface (degrees)
!
!   output argument list:
!     sun_zenith  - solar zenith angle
!     sun_azimuth - solar azimuth angle
!
!   comments:
!
!
!     PROCEDURE:
!
!  1. Calculate the subsolar point latitude and longitude, based on
!     DAY and TIME. Since each year is 365.25 days long the exact
!     value of the declination angle changes from year to year.  For
!     precise values consult THE AMERICAN EPHEMERIS AND NAUTICAL
!     ALMANAC published yearly by the U.S. govt. printing office.  The
!     subsolar coordinates used in this code were provided by a
!     program written by Jeff Dozier.
!
!  2. Given the subsolar latitude and longitude, spherical geometry is
!     used to find the solar zenith, azimuth and flux multiplier.
!
!  eqt = equation of time (minutes)  ! solar longitude correction = -15*eqt
!  dec = declination angle (degrees) = solar latitude
!
! LOWTRAN v7 data (25 points)
!     The LOWTRAN solar position data is characterized by only 25 points.
!     This should predict the subsolar angles within one degree.  For
!     increased accuracy add more data points.
!
!nday=[   1.,    9.,   21.,   32.,   44.,   60.,  91.,  121.,  141.,  152.,$
!       160.,  172.,  182.,  190.,  202.,  213., 244.,  274.,  305.,  309.,$
!       325.,  335.,  343.,  355.,  366.]
!
!eqt=[ -3.23, -6.83,-11.17,-13.57,-14.33,-12.63, -4.2,  2.83,  3.57,  2.45,$
!       1.10, -1.42, -3.52, -4.93, -6.25, -6.28,-0.25, 10.02, 16.35, 16.38,$
!       14.3, 11.27,  8.02,  2.32, -3.23]
!
!dec=[-23.07,-22.22,-20.08,-17.32,-13.62, -7.88, 4.23, 14.83, 20.03, 21.95,$
!      22.87, 23.45, 23.17, 22.47, 20.63, 18.23, 8.58, -2.88,-14.18,-15.45,$
!     -19.75,-21.68,-22.75,-23.43,-23.07]
!
! Analemma information from Jeff Dozier
!     This data is characterized by 74 points
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!
  use kinds, only: r_kind,i_kind
  use constants, only: pi

  implicit none

  integer(i_kind)   di,day
  real(r_kind)      time,lat,lon
  real(r_kind)      ut,noon
  real(r_kind)      y(5),y2(5),x(2,5),Tx(5,2),xTx(2,2),aTx(5,2),det
  real(r_kind)      tt,eqtime,decang,latsun,lonsun
  real(r_kind)      nday(74),eqt(74),dec(74)
  real(r_kind)      beta(2), beta2(2), a(2,2)
  real(r_kind)      t0,t1,p0,p1,zz,xx,yy
  real(r_kind)      sun_zenith,sun_azimuth

  data   nday/1.0,   6.0,  11.0,  16.0,  21.0,  26.0,  31.0,  36.0,  41.0,  46.0,&
             51.0,  56.0,  61.0,  66.0,  71.0,  76.0,  81.0,  86.0,  91.0,  96.0,&
             101.0, 106.0, 111.0, 116.0, 121.0, 126.0, 131.0, 136.0, 141.0, 146.0,&
             151.0, 156.0, 161.0, 166.0, 171.0, 176.0, 181.0, 186.0, 191.0, 196.0,&
             201.0, 206.0, 211.0, 216.0, 221.0, 226.0, 231.0, 236.0, 241.0, 246.0,&
             251.0, 256.0, 261.0, 266.0, 271.0, 276.0, 281.0, 286.0, 291.0, 296.0,&
             301.0, 306.0, 311.0, 316.0, 321.0, 326.0, 331.0, 336.0, 341.0, 346.0,&
             351.0, 356.0, 361.0, 366.0/

  data  eqt/ -3.23, -5.49, -7.60, -9.48,-11.09,-12.39,-13.34,-13.95,-14.23,-14.19,&
            -13.85,-13.22,-12.35,-11.26,-10.01, -8.64, -7.18, -5.67, -4.16, -2.69,&
             -1.29, -0.02,  1.10,  2.05,  2.80,  3.33,  3.63,  3.68,  3.49,  3.09,&
              2.48,  1.71,  0.79, -0.24, -1.33, -2.41, -3.45, -4.39, -5.20, -5.84,&
             -6.28, -6.49, -6.44, -6.15, -5.60, -4.82, -3.81, -2.60, -1.19,  0.36,&
              2.03,  3.76,  5.54,  7.31,  9.04, 10.69, 12.20, 13.53, 14.65, 15.52,&
             16.12, 16.41, 16.36, 15.95, 15.19, 14.09, 12.67, 10.93,  8.93,  6.70,&
              4.32,  1.86, -0.62, -3.23/

  data dec/ -23.06,-22.57,-21.91,-21.06,-20.05,-18.88,-17.57,-16.13,-14.57,-12.91,&
            -11.16, -9.34, -7.46, -5.54, -3.59, -1.62,  0.36,  2.33,  4.28,  6.19,&
              8.06,  9.88, 11.62, 13.29, 14.87, 16.34, 17.70, 18.94, 20.04, 21.00,&
             21.81, 22.47, 22.95, 23.28, 23.43, 23.40, 23.21, 22.85, 22.32, 21.63,&
             20.79, 19.80, 18.67, 17.42, 16.05, 14.57, 13.00, 11.33,  9.60,  7.80,&
              5.95,  4.06,  2.13,  0.19, -1.75, -3.69, -5.62, -7.51, -9.36,-11.16,&
            -12.88,-14.53,-16.07,-17.50,-18.81,-19.98,-20.99,-21.85,-22.52,-23.02,&
            -23.33,-23.44,-23.35,-23.06/

!
! compute the subsolar coordinates
!


  tt= mod(real((int(day)+time/24.-1.)),365.25) +1.  ! fractional day number
                                                    ! with 12am 1jan = 1.
  do di = 1, 73
    if ((tt .ge. nday(di)) .and. (tt .le. nday(di+1))) exit
  end do

!============== Perform a least squares regression on doy**3 ==============

  x(1,:) = 1.0

  if ((di .ge. 3) .and. (di .le. 72)) then
    y(:) = eqt(di-2:di+2)
    y2(:) = dec(di-2:di+2)

    x(2,:) = nday(di-2:di+2)**3
  end if
  if (di .eq. 2) then
    y(1) = eqt(73)
    y(2:5) = eqt(di-1:di+2)
    y2(1) = dec(73)
    y2(2:5) = dec(di-1:di+2)

    x(2,1) = nday(73)**3
    x(2,2:5) = (365.+nday(di-1:di+2))**3
  end if
  if (di .eq. 1) then
    y(1:2) = eqt(72:73)
    y(3:5) = eqt(di:di+2)
    y2(1:2) = dec(72:73)
    y2(3:5) = dec(di:di+2)

    x(2,1:2) = nday(72:73)**3
    x(2,3:5) = (365.+nday(di:di+2))**3
  end if
  if (di .eq. 73) then
    y(1:4) = eqt(di-2:di+1)
    y(5) = eqt(2)
    y2(1:4) = dec(di-2:di+1)
    y2(5) = dec(2)

    x(2,1:4) = nday(di-2:di+1)**3
    x(2,5) = (365.+nday(2))**3
  end if
  if (di .eq. 74) then
    y(1:3) = eqt(di-2:di)
    y(4:5) = eqt(2:3)
    y2(1:3) = dec(di-2:di)
    y2(4:5) = dec(2:3)

    x(2,1:3) = nday(di-2:di)**3
    x(2,4:5) = (365.+nday(2:3))**3
  end if

!  Tx = transpose(x)
  Tx(1:5,1)=x(1,1:5)
  Tx(1:5,2)=x(2,1:5)
!  xTx = MATMUL(x,Tx)
  xTx(1,1)=x(1,1)*Tx(1,1)+x(1,2)*Tx(2,1)+x(1,3)*Tx(3,1)+x(1,4)*Tx(4,1)+x(1,5)*Tx(5,1)
  xTx(1,2)=x(1,1)*Tx(1,2)+x(1,2)*Tx(2,2)+x(1,3)*Tx(3,2)+x(1,4)*Tx(4,2)+x(1,5)*Tx(5,2)
  xTx(2,1)=x(2,1)*Tx(1,1)+x(2,2)*Tx(2,1)+x(2,3)*Tx(3,1)+x(2,4)*Tx(4,1)+x(2,5)*Tx(5,1)
  xTx(2,2)=x(2,1)*Tx(1,2)+x(2,2)*Tx(2,2)+x(2,3)*Tx(3,2)+x(2,4)*Tx(4,2)+x(2,5)*Tx(5,2)

  det = xTx(1,1)*xTx(2,2) - xTx(1,2)*xTx(2,1)
  a(1,1) = xTx(2,2)/det
  a(1,2) = -xTx(1,2)/det
  a(2,1) = -xTx(2,1)/det
  a(2,2) = xTx(1,1)/det

!  aTx = MATMUL(Tx,a)
  aTx(1,1)=Tx(1,1)*a(1,1)+Tx(1,2)*a(2,1) 
  aTx(2,1)=Tx(2,1)*a(1,1)+Tx(2,2)*a(2,1) 
  aTx(3,1)=Tx(3,1)*a(1,1)+Tx(3,2)*a(2,1) 
  aTx(4,1)=Tx(4,1)*a(1,1)+Tx(4,2)*a(2,1) 
  aTx(5,1)=Tx(5,1)*a(1,1)+Tx(5,2)*a(2,1) 
  aTx(1,2)=Tx(1,1)*a(1,2)+Tx(1,2)*a(2,2) 
  aTx(2,2)=Tx(2,1)*a(1,2)+Tx(2,2)*a(2,2) 
  aTx(3,2)=Tx(3,1)*a(1,2)+Tx(3,2)*a(2,2) 
  aTx(4,2)=Tx(4,1)*a(1,2)+Tx(4,2)*a(2,2) 
  aTx(5,2)=Tx(5,1)*a(1,2)+Tx(5,2)*a(2,2) 

!  beta = MATMUL(y,aTx)
  beta(1) = y(1)*aTx(1,1)+y(2)*aTx(2,1)+y(3)*aTx(3,1)+y(4)*aTx(4,1)+y(5)*aTx(5,1)
  beta(2) = y(1)*aTx(1,2)+y(2)*aTx(2,2)+y(3)*aTx(3,2)+y(4)*aTx(4,2)+y(5)*aTx(5,2)

!  beta2 = MATMUL(y2,aTx)
  beta2(1) = y2(1)*aTx(1,1)+y2(2)*aTx(2,1)+y2(3)*aTx(3,1)+y2(4)*aTx(4,1)+y2(5)*aTx(5,1)
  beta2(2) = y2(1)*aTx(1,2)+y2(2)*aTx(2,2)+y2(3)*aTx(3,2)+y2(4)*aTx(4,2)+y2(5)*aTx(5,2)

!============== finished least squares regression on doy**3 ==============

  if ((di .lt. 3) .or. (di .gt. 72)) tt = tt + 365.

  eqtime=(beta(1) + beta(2)*tt**3)/60.
  decang=beta2(1) + beta2(2)*tt**3
  latsun=decang

  ut=time
  noon=12.-lon/15.                             ! universal time of noon

  lonsun=-15.*(ut-12.+eqtime)

  t0=(90.-lat)*pi/180.
  t1=(90.-latsun)*pi/180.

  p0=lon*pi/180.
  p1=lonsun*pi/180.

  zz=cos(t0)*cos(t1)+sin(t0)*sin(t1)*cos(p1-p0)
!  zz2=sin(t0)*sin(t1)+cos(t0)*cos(t1)*cos(p1-p0)
  xx=sin(t1)*sin(p1-p0)
  yy=sin(t0)*cos(t1)-cos(t0)*sin(t1)*cos(p1-p0)

  sun_zenith=90-acos(zz)/(pi/180)
  sun_azimuth=atan2(xx,yy)/(pi/180)

  return
end subroutine zensun

subroutine deter_sfc_amsre_low(dlat_earth,dlon_earth,isflg,sfcpct)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc_amsre_low           determine land surface type
!   prgmmr: kazumori          org: np2                date: 2005-10-20
!
! abstract:  determines land surface type based on surrounding land
!            surface types for AMSR-E large FOV observation
!
! program history log:
!   2005-10-20 kazumori - refered from ( subroutine deter_sfc )
!   2006-02-01 parrish  - change names of sno,isli,sst
!   2008-05-28 safford  - rm unused vars
!
!   input argument list:
!     dlat_earth   - latitude
!     dlon_earth   - longitude
!
!   output argument list:
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
!      sfcpct(0:3)- percentage of 4 surface types
!                 (0) - sea percentage
!                 (1) - land percentage
!                 (2) - sea ice percentage
!                 (3) - snow percentage
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
   use kinds, only: r_kind,i_kind
   use satthin, only: sno_full,isli_full,sst_full
   use constants, only: zero,one
   use gridmod, only: rlats,rlons,nlat,nlon,regional,tll2xy,nlat_sfc,nlon_sfc,rlats_sfc,rlons_sfc
   use guess_grids, only: ntguessfc
   implicit none
   integer(i_kind),intent(out):: isflg
   real(r_kind),intent(in) :: dlat_earth,dlon_earth
   real(r_kind) :: dlat,dlon
   real(r_kind),dimension(0:3),intent(out) :: sfcpct

   integer(i_kind) jsli,it

   integer(i_kind):: klat1,klon1,klatp1,klonp1
   real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11
   logical :: outside
   integer(i_kind):: klat2,klon2,klatp2,klonp2

!
!  For interpolation, we usually use o points (4points for land sea decision)
!  In case of lowfreq channel (Large FOV), add the check of x points(8 points)
!                                          (klatp2,klon1),(klatp2,klonp1)
!       ---#---x---x---#---  klatp2        (klatp1,klon2),(klatp1,klonp2)
!          |   |   |   |                   (klat1,klon2),(klat1,klonp2)
!       ---x---o---o---x---  klatp1        (klat2,klon1),(klat2,klonp1)
!          |   | + |   |
!       ---x---o---o---x---  klat1
!          |   |   |   |
!       ---#---x---x---#---  klat2
!            klon1   klonp2
!       klon2    klonp1
!
!  In total, 12 points are used to make mean sst and sfc percentage.
!
     it=ntguessfc

     if(regional)then
       call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
     else
       dlat=dlat_earth
       dlon=dlon_earth
       call grdcrd(dlat,1,rlats_sfc,nlat_sfc,1)
       call grdcrd(dlon,1,rlons_sfc,nlon_sfc,1)
     end if

     klon1=int(dlon); klat1=int(dlat)
     dx  =dlon-klon1; dy  =dlat-klat1
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

     klat1=min(max(1,klat1),nlat_sfc); klon1=min(max(0,klon1),nlon_sfc)
     if(klon1==0) klon1=nlon_sfc
     klatp1=min(nlat_sfc,klat1+1); klonp1=klon1+1
     if(klonp1==nlon_sfc+1) klonp1=1

     if (klat1 /= 1)then
        klat2=klat1 -1
     else
        klat2=klat1
     end if
     if (klatp1 /= nlat_sfc)then
        klatp2=klatp1 + 1
     else
        klatp2=nlat_sfc
     end if

     if (klon1 /= 1)then
        klon2=klon1-1
     else
        klon2=klon1
     end if
     if (klonp1 /= nlon_sfc)then
        klonp2=klonp1+1
     else
        klonp2=klonp1
     end if

!    Set surface type flag.  Begin by assuming obs over ice-free water

     sfcpct = zero

     jsli = isli_full(klat1 ,klon1 )
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klat1 ,klon1 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klatp1,klon1 )
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klatp1 ,klon1 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klat1 ,klonp1)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klat1 ,klonp1 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klatp1,klonp1)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klatp1 ,klonp1 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klatp2,klon1)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klatp2 ,klon1 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klatp2,klonp1)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klatp2 ,klonp1 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klatp1,klon2)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klatp1 ,klon2 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klatp1,klonp2)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klatp1 ,klonp2 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klat1,klon2)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klat1 ,klon2 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klat1,klonp2)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klat1 ,klonp2 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klat2,klon1)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klat2 ,klon1 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     jsli = isli_full(klat2,klonp1)
     sfcpct(jsli)=sfcpct(jsli)+one
     if(sno_full(klat2 ,klonp1 ,it) > one .and. jsli == 1)then
        sfcpct(3)=sfcpct(3)+one
        sfcpct(jsli)=sfcpct(jsli)-one
     endif

     sfcpct=sfcpct/12.0_r_kind

!     sfcpct(3)=min(sfcpct(3),sfcpct(1))
!     sfcpct(1)=max(zero,sfcpct(1)-sfcpct(3))

     isflg = 0
     if(sfcpct(0) > 0.99_r_kind)then
        isflg = 0
     else if(sfcpct(1) > 0.99_r_kind)then
        isflg = 1
     else if(sfcpct(2) > 0.99_r_kind)then
        isflg = 2
     else if(sfcpct(3) > 0.99_r_kind)then
        isflg = 3
     else
        isflg = 4
     end if

     return

   end subroutine deter_sfc_amsre_low
