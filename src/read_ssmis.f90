subroutine read_ssmis(mype,val_ssmis,ithin,isfcalc,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub)

! subprogram:    read_ssmis            read ssmis data
! prgmmr: okamoto          org: np23                date: 2005-01-05
!
! abstract:  This routine reads BUFR format SSM/IS radiance 
!     (brightness temperature) files.  Optionally, the data 
!     are thinned to a specified resolution using simple 
!     quality control checks.
!
!     When running the gsi in regional mode, the code only
!     retains those observations that fall within the regional
!     domain
!     QC performed in this subroutine:
!         1) obs time check  |obs-anal|<time_window;
!         2) remove overlap orbit; 
!         3)climate check  reject for tb<tbmin or tb>tbmax
!
! program history log:
!   2005-01-05 okamoto 
!    2005-10-07 Xu & Pawlak - modify the code related to ityp determination to
!                     use routine  deter_ityp, added values for constants 
!                     rlndsea for four ssmis instruments, fixed indentation
!   2005-10-10 treadon - replace deter_ityp with deter_sfc, modify rlndsea to be
!                        consistent with other read_* routines
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2005-12-15  treadon - patch to constrain ssmi_img scan positions to be in 1-90 range
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-27  derber  - some efficiency modifications
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!   2007-01-24  kazumori- modify to read UKMO preprocessed SSMIS data
!   2008-04-08  Yan     - fix bug in calculation of ifov for UPP SSMIS data
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-05-27  safford - rm unused vars and uses
!   2009-01-09  gayno   - new option to calculate surface fields within FOV
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!
! input argument list:
!     mype     - mpi task id
!     val_ssmis- weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - flag to specify method to calculate sfc fields within FOV
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read  ex.15
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
!     nread    - number of BUFR MI 1b observations read
!     ndata    - number of BUFR MI 1b profiles retained for further processing
!     nodata   - number of BUFR MI 1b observations retained for further processing
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!$$$ end documentation block

  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
             checkob,finalcheck,score_crit
  use radinfo, only: iuse_rad,jpch_rad,nusis
  use gridmod, only: diagnostic_reg,regional,rlats,rlons,nlat,nlon,&
       tll2xy,txy2ll
  use constants, only: deg2rad,rad2deg,zero,half,one,two,four,r60inv
  use gsi_4dvar, only: l4dvar, iwinbgn, winlen
  use calc_fov_conical, only: instrument_init
  
  implicit none

! Number of channels for sensors in BUFR
  character(7),parameter:: fov_flag="conical"
  integer(i_kind),parameter :: maxchanl  =  24
  integer(i_kind),parameter :: maxinfo   =  33
  integer(i_kind),parameter :: mxscen_img = 180   !img
  integer(i_kind),parameter :: mxscen_env = 90    !env
  integer(i_kind),parameter :: mxscen_las = 60    !las
  integer(i_kind),parameter :: mxscen_uas = 30    !uas
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=70.0_r_kind
  real(r_kind),parameter:: tbmax=320.0_r_kind
  real(r_kind),parameter:: tbbad=-9.99e11_r_kind


! Declare passed variables
  character(len=*),intent(in) :: infile,obstype,jsatid
  character(len=*),intent(in) :: sis
  integer(i_kind),intent(in) :: mype,lunout,ithin,isfcalc
  integer(i_kind),intent(inout):: nread
  integer(i_kind),intent(inout):: ndata,nodata
  real(r_kind),intent(in):: rmesh,gstime,twind
  real(r_kind),intent(inout):: val_ssmis
  integer(i_kind)  ,intent(in) :: mype_root
  integer(i_kind)  ,intent(in) :: mype_sub
  integer(i_kind)  ,intent(in) :: npe_sub
  integer(i_kind)  ,intent(in) :: mpi_comm_sub


! Declare local variables
  logical :: ssmis_las,ssmis_uas,ssmis_img,ssmis_env
  logical :: outside,iuse,assim
  character(len=8)  :: subset,subfgn
  integer(i_kind) :: i,k,ifov,ifovoff,ntest
  integer(i_kind) :: nlv,idate,nchanl,nreal
  integer(i_kind) :: n,ireadsb,ireadmg,irec,isub,next
  integer(i_kind) :: nmind,itx,nele,itt
  integer(i_kind) :: iskip
  integer(i_kind) :: lnbufr,isflg,idomsfc
  integer(i_kind) :: ilat,ilon
  integer(i_kind) :: nscan,jc,bufsat,incangl,said
  integer(i_kind) :: nfov_bad
  integer(i_kind) :: ichan, instr
  integer(i_kind) isflg_1,isflg_2,isflg_3,isflg_4
  integer(i_kind),dimension(5):: iobsdate
  real(r_kind) sfcr,r07
  real(r_kind) pred
  real(r_kind) rsat,sstime,tdiff,t4dv
  real(r_kind) crit1,dist1
  real(r_kind) timedif
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind) disterr,disterrmax,dlon00,dlat00
  real(r_kind) :: fovn

! BUFR variables
  integer(i_kind) :: kchanl,jch,bch
  integer(i_kind),dimension(maxchanl) :: kchssmis
  real(r_double),dimension(7)::   bufrinit
  real(r_double),dimension(3,5):: bufrymd
  real(r_double),dimension(2,2):: bufrhm
  real(r_double),dimension(2,29)::bufrloc
  real(r_double),dimension(2,maxchanl):: bufrtbb
  real(r_kind) :: MILLI = 0.001_r_kind

! For qc
  real(r_kind):: flgch
  real(r_kind),dimension(maxchanl):: tbob
  real(r_kind) :: dlat,dlon,dlon_earth,dlat_earth
  real(r_kind) :: dlon_earth_deg,dlat_earth_deg,expansion,sat_aziang

  real(r_kind) sstx_1,sstx_2,sstx_3,sstx_4
  real(r_kind),dimension(0:3):: sfcpct_1,sfcpct_2,sfcpct_3,sfcpct_4


!----------------------------------------------------------------------
! Initialize variables
  lnbufr = 15
  disterrmax=zero
  ntest  = 0
  nreal  = maxinfo
  nchanl = maxchanl
  ndata  = 0
  nodata = 0
  nread  = 0
  nfov_bad = 0
  ilon=3
  ilat=4
  r07 = 0.7_r_kind * deg2rad

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
  if (.not.assim) val_ssmis=zero


! Make thinning grids
  call makegrids(rmesh,ithin)


! Set various variables depending on type of data to be read
  ssmis_uas=     obstype == 'ssmis_uas'
  ssmis_las=     obstype == 'ssmis_las'
  ssmis_img=     obstype == 'ssmis_img'
  ssmis_env=     obstype == 'ssmis_env'
  
  kchssmis(:) = 0

! Common
  bufsat = 249

! Humidity imager:180
  if(ssmis_img) then
     nscan  = mxscen_img
     ifovoff = 0
     kchanl = 6
     kchssmis(1:6)=(/8,9,10,11,17,18/)
!     subfgn = 'NC021201'
     subfgn = 'NC021203'
     incangl = 53.0_r_kind
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind

! env:90
  else if(ssmis_env) then
     nscan  = mxscen_env
     ifovoff = 180
     kchanl = 5
     kchssmis(1:5)=(/12,13,14,15,16/)
     subfgn = 'NC021202'
     incangl = 53.1_r_kind
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
        
! las:60
  else if(ssmis_las) then
     nscan  = mxscen_las
     ifovoff = 270
!     kchanl = 8
!     kchssmis(1:8)=(/1,2,3,4,5,6,7,24/)
!     subfgn = 'NC021203'
     kchanl = 24
     kchssmis(1:24)=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24/)
     subfgn = 'NC021201'
     incangl = 53.0_r_kind
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind

! uas:30
  else if(ssmis_uas) then
     nscan  = mxscen_uas
     ifovoff = 330
     kchanl = 5
     kchssmis(1:5)=(/19,20,21,22,23/)
     subfgn = 'NC021204'
     incangl = 52.4_r_kind
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
     
  end if

! Open unit to satellite bufr file
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Write header record to scratch file.  Also allocate array
! to hold all data for given satellite
  nele=nreal+nchanl
  allocate(data_all(nele,itxmax))

  if (isfcalc == 1) then
    if (trim(jsatid) == 'f16') instr=26
    if (trim(jsatid) == 'f17') instr=27
! right now, all ssmis data is mapped to a common fov -
! that of the las channels.
    ichan = 1
    expansion = 2.9_r_kind
    sat_aziang = 90.0_r_kind  ! 'fill' value; need to get this from file
    call instrument_init(instr, jsatid, expansion)
  endif


! Big loop to read data file
  next=mype_sub+1
  do while(ireadmg(lnbufr,subset,idate)>=0)
   call ufbcnt(lnbufr,irec,isub)
   if(irec/=next    ) cycle
   next=next+npe_sub
   if(subset/=subfgn) cycle
   read_loop: do while(ireadsb(lnbufr)==0)

!       BUFR read 1/3
        call ufbint(lnbufr,bufrinit,7,1,nlv, &
             "SAID SECO SLNM FOVN RSURF RAINF ORBN" )

!       Extract satellite id.  If not the one we want, read next record
        said = int( bufrinit(1) + MILLI ) 
        if( said /= bufsat) exit read_loop
        
        rsat=bufsat
        
        fovn = bufrinit(4)
        ifov = int(fovn+MILLI)

        if(ifov>nscan) then
!           write(6,*) 'READ_SSMIS(',obstype,'): unreliable FOV number fovn=',fovn, &
!                ' ifov=',ifov
           nfov_bad = nfov_bad+1
           cycle read_loop
        end if

        if( bufrinit(6) == one) cycle read_loop


!       SSMIS imager has 180 scan positions.  Select every other position.
!        if(ssmis_img) then
!           if (mod(ifov,2)/=0) then
!              cycle read_loop
!           else
!              ifov = min(max(1,nint(float(ifov)/two + half)),90)
!           endif
!        endif

!       BUFR read 2/3
        call ufbrep(lnbufr,bufrymd,3,5,nlv,"YEAR MNTH DAYS" )
        call ufbrep(lnbufr,bufrhm, 2,2,nlv,"HOUR MINU" )

        
!       Calc obs seqential time  If time outside window, skip this obs
        iobsdate(1:3) = bufrymd(1:3,1) !year,month,day for scan start time  kozo
        iobsdate(4:5) = bufrhm(1:2,1)  !hour,min for scan start time  kozo
        call w3fs21(iobsdate,nmind)
        t4dv=(real(nmind-iwinbgn,r_kind) + real(bufrinit(2),r_kind)*r60inv)*r60inv
        if (l4dvar) then
          if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
          sstime=real(nmind,r_kind) + real(bufrinit(2),r_kind)*r60inv
          tdiff=(sstime-gstime)*r60inv
          if(abs(tdiff) > twind)  then 
             write(6,*) 'READ_SSMIS(',obstype,'): time check fail: obstime=',iobsdate
             cycle read_loop
          end if
        endif
        
!       Extract obs location, TBB, other information

!       BUFR read 3/3
        call ufbrep(lnbufr,bufrloc,  2,29,      nlv,"CLAT CLON" )
        call ufbrep(lnbufr,bufrtbb,  2,maxchanl,nlv,"CHNM TMBR" )
!       call ufbrep(lnbufr,bufrinfo, 1,3,       nlv,"SELV" )
!       call ufbrep(lnbufr,bufrlleaa,2,28,      nlv,"RAIA BEARAZ" )
        
!       Regional case
        dlat_earth = bufrloc(1,1)  !degrees
        dlon_earth = bufrloc(2,1)  !degrees
        if(dlon_earth<zero ) dlon_earth = dlon_earth+r360
        if(dlon_earth>=r360) dlon_earth = dlon_earth-r360
        dlat_earth_deg = dlat_earth
        dlon_earth_deg = dlon_earth

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
           
!          Check to see if in domain
           if(outside) cycle read_loop

!       Global case
        else
           dlat=dlat_earth
           dlon=dlon_earth
           call grdcrd(dlat,1,rlats,nlat,1)
           call grdcrd(dlon,1,rlons,nlon,1)
        endif

!       Transfer observed brightness temperature to work array.  
!       If any temperature exceeds limits, reset observation 
!       to "bad" value
        iskip=0
        tbob(:) = tbbad
        do jc=1,kchanl
           jch=kchssmis(jc)  !ch index specified in this code
           bch=int( bufrtbb(1,jch)+MILLI ) !ch index from bufr
           tbob(jch) = bufrtbb(2,jch)
           if(tbob(jch)<tbmin .or. tbob(jch)>tbmax .or. bch/=jch) then
              tbob(jch) = tbbad 
              iskip = iskip + 1
           else
              nread=nread+1
           end if
        end do
        
        if(iskip>=kchanl)  cycle read_loop!if all ch for any posion is bad, skip 


        flgch = iskip*two   !used for thinning priority range 0-14
        if (l4dvar) then
          crit1 = 0.01_r_kind+ flgch
        else
          timedif = 6.0_r_kind*abs(tdiff) ! range:  0 to 18
          crit1 = 0.01_r_kind+timedif + flgch
        endif
!       Map obs to thinning grid
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
        if(.not. iuse)cycle read_loop

!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask.  

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
        if (isfcalc==1) then

          call deter_sfc_fov(fov_flag,ifov,instr,ichan,sat_aziang,dlat_earth_deg,&
                             dlon_earth_deg,expansion,t4dv,isflg,idomsfc, &
                             sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
        else
          call deter_sfc(dlat,dlon,dlat_earth+r07,dlon_earth+r07,t4dv,isflg_1, &
             idomsfc,sfcpct_1,ts,sstx_1,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
          call deter_sfc(dlat,dlon,dlat_earth+r07,dlon_earth-r07,t4dv,isflg_2, &
             idomsfc,sfcpct_2,ts,sstx_2,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
          call deter_sfc(dlat,dlon,dlat_earth-r07,dlon_earth+r07,t4dv,isflg_3, &
             idomsfc,sfcpct_3,ts,sstx_3,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
          call deter_sfc(dlat,dlon,dlat_earth-r07,dlon_earth-r07,t4dv,isflg_4, &
             idomsfc,sfcpct_4,ts,sstx_4,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

          call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
                  ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)


          sfcpct(0)= (sfcpct_1(0)+ sfcpct_2(0)+ sfcpct_3(0)+ sfcpct_4(0))/4.0_r_kind
          sfcpct(1)= (sfcpct_1(1)+ sfcpct_2(1)+ sfcpct_3(1)+ sfcpct_4(1))/4.0_r_kind
          sfcpct(2)= (sfcpct_1(2)+ sfcpct_2(2)+ sfcpct_3(2)+ sfcpct_4(2))/4.0_r_kind
          sfcpct(3)= (sfcpct_1(3)+ sfcpct_2(3)+ sfcpct_3(3)+ sfcpct_4(3))/4.0_r_kind
        endif ! isfcalc==1



        crit1 = crit1 + rlndsea(isflg)
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

!       Set common predictor parameters
        pred = zero
        
!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
        crit1 = crit1+pred 

        call finalcheck(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

        data_all( 1,itx)= rsat                 ! satellite id
        data_all( 2,itx)= t4dv                 ! time diff between obs and anal (min)
        data_all( 3,itx)= dlon                 ! grid relative longitude
        data_all( 4,itx)= dlat                 ! grid relative latitude
        data_all( 5,itx)= incangl*deg2rad      ! local zenith angle (rad)
        data_all( 6,itx)= zero                 ! local azimuth angle (missing)
        data_all( 7,itx)= zero                 ! look angle (rad)
        data_all( 8,itx)= ifov                 ! FOV scan position
        data_all( 9,itx)= zero                 ! solar zenith angle (deg) : not used for MW-RT calc
        data_all(10,itx)= zero                 ! solar azimuth angle (deg) : not used for MW-RT calc
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
        data_all(27,itx)= idomsfc + 0.001      ! dominate surface type
        data_all(28,itx)= sfcr                 ! surface roughness
        data_all(29,itx)= ff10                 ! ten meter wind factor
        data_all(30,itx)= dlon_earth_deg       ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth_deg       ! earth relative latitude (degrees)

        data_all(nreal-1,itx)=val_ssmis
        data_all(nreal,itx)=itt
        do jc=1,maxchanl
           data_all(nreal+jc,itx) = tbob(jc)
        end do

     end do read_loop
  end do
  call closbf(lnbufr)

! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
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
        super_val(itt)=super_val(itt)+val_ssmis

     end do

!    Write final set of "best" observations to output file
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)

  endif

! Deallocate local arrays
  deallocate(data_all)

! Deallocate satthin arrays
1000 continue
  call destroygrids
    
  if(diagnostic_reg .and. ntest>0 .and. mype_sub==mype_root) &
       write(6,*)'READ_SSMIS:  mype,ntest,disterrmax=',&
       mype,ntest,disterrmax
  if (nfov_bad>0) &
       write(6,*)'READ_SSMIS(',obstype,'):  found ',nfov_bad,' questionable fov'
  
! End of routine
  return

end subroutine read_ssmis
