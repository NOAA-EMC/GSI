subroutine read_ssmis(mype,val_ssmis,ithin,rmesh,jsatid,gstime,&
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
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!
! input argument list:
!     mype     - mpi task id
!     val_ssmis- weighting factor applied to super obs
!     ithin    - flag to thin data
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
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
             checkob,finalcheck
  use radinfo, only: iuse_rad,newchn,cbias
  use gridmod, only: diagnostic_reg,regional,rlats,rlons,nlat,nlon,&
       tll2xy,txy2ll
  use constants, only: deg2rad,rad2deg,zero,half,one,two,three,izero
  use obsmod, only: iadate
  use mpi_bufr_mod, only: mpi_openbf,mpi_closbf,nblocks,mpi_nextblock,&
       mpi_readmg,mpi_ireadsb,mpi_ufbint,mpi_ufbrep,mpi_ireadmg
  
  implicit none

! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: maxchanl  =  24
  integer(i_kind),parameter :: maxinfo   =  18
  integer(i_kind),parameter :: maxtype   =   4
  integer(i_kind),parameter :: mxscen_img = 180   !img
  integer(i_kind),parameter :: mxscen_env = 90    !env
  integer(i_kind),parameter :: mxscen_las = 60    !las
  integer(i_kind),parameter :: mxscen_uas = 30    !uas
  integer(i_kind),parameter :: ione = 1
  integer(i_kind),parameter :: itwo = 2
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=70.0_r_kind
  real(r_kind),parameter:: tbmax=320.0_r_kind
  real(r_kind),parameter:: tbbad=-9.99e11_r_kind


! Declare passed variables
  character(10),intent(in) :: infile,obstype,jsatid
  character(20),intent(in) :: sis
  integer(i_kind),intent(in) :: mype,lunout,ithin
  integer(i_kind),intent(inout):: nread
  integer(i_kind),intent(inout):: ndata,nodata
  real(r_kind),intent(in):: val_ssmis,rmesh,gstime,twind
  integer(i_kind)  ,intent(in) :: mype_root
  integer(i_kind)  ,intent(in) :: mype_sub
  integer(i_kind)  ,intent(in) :: npe_sub
  integer(i_kind)  ,intent(in) :: mpi_comm_sub


! Declare local variables
  logical :: ssmis_las,ssmis_uas,ssmis_img,ssmis_env
  logical :: mixed,outside,iuse
  character(len=10) :: date
  character(len=8)  :: subset,subfgn
  integer(i_kind) :: ihh,i,j,k,ifov,ifovoff,idd,jdate,isc,ireadmg,ireadsb,ntest
  integer(i_kind) :: iret,nlv,idate,im,iy,iyr,nchanl,klatp1,klonp1,nreal
  integer(i_kind) :: klon1,klat1,n
  integer(i_kind) :: nmind,itx,nele,nk,itt,iout
  integer(i_kind) :: iskip,lndsea
  integer(i_kind) :: lnbufr,isflg
  integer(i_kind) :: ilat,ilon
  integer(i_kind) :: nscan,jc,bufsat,incangl,said
  integer(i_kind),dimension(5):: iobsdate
  real(r_kind) w00,w10,w01,w11,dx,dy,dx1,dy1,cosza
  real(r_kind) pred,rflag
  real(r_kind) rsat,panglr,rato,sstime,tdiff,rmask
  real(r_kind) crit1,step,start,sstx,dist1
  real(r_kind) terrain,timedif,oneover60
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind),allocatable,dimension(:):: data_crit
  integer(i_kind),allocatable,dimension(:):: idata_itx
  integer(i_kind):: mmblocks
  integer(i_kind):: isubset
  real(r_kind) disterr,disterrmax,dlon00,dlat00
  real(r_kind) :: fovn

! BUFR variables
  integer(i_kind) :: kchanl,jch,bch
  integer(i_kind),dimension(maxchanl) :: kchssmis
  real(r_double),dimension(7)::   bufrinit
  real(r_double),dimension(3,5):: bufrymd
  real(r_double),dimension(2,2):: bufrhm
  real(r_double),dimension(2,29)::bufrloc
  real(r_double),dimension(1,3):: bufrinfo
  real(r_double),dimension(2,28)::bufrlleaa
  real(r_double),dimension(2,maxchanl):: bufrtbb
  real(r_kind) :: MILLI = 0.001_r_kind

! For qc
  integer(i_kind)      :: kqcflow,kraintype,ierr,ij
  real(r_kind):: tb19v,tb22v,tb85v,si85,flgch
  real(r_kind),dimension(maxchanl):: tbob
  real(r_kind) :: sftg
  real(r_kind) :: dlat,dlon,dlon_earth,dlat_earth


!----------------------------------------------------------------------
! Initialize variables
  lnbufr = 15
  oneover60=1./60._r_kind
  disterrmax=0._r_kind
  ntest  = 0
  nreal  = maxinfo
  nchanl = maxchanl
  ndata  = 0
  nodata = 0
  nread  = 0
  ilon=3
  ilat=4

! Make thinning grids
  call makegrids(rmesh)


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
     subfgn = 'NC021201'
     incangl = 53.0_r_kind
     rlndsea(0) = 0._r_kind
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
     rlndsea(0) = 0._r_kind
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
        
! las:60
  else if(ssmis_las) then
     nscan  = mxscen_las
     ifovoff = 270
     kchanl = 8
     kchssmis(1:8)=(/1,2,3,4,5,6,7,24/)
     subfgn = 'NC021203'
     incangl = 53.0_r_kind
     rlndsea(0) = 0._r_kind
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
     rlndsea(0) = 0._r_kind
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
     
  end if

! Open unit to satellite bufr file
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Read header
  call readmg(lnbufr,subset,idate,iret)
  call closbf(lnbufr)
  close(lnbufr)
  if( iret /= 0 ) goto 1000     ! no data?

  write(date,'( i10)') idate
  read(date,'(i4,3i2)') iy,im,idd,ihh
  if (mype_sub==mype_root) &
       write(6,*) 'READ_SSMIS:     bufr file date is ',iy,im,idd,ihh

! Write header record to scratch file.  Also allocate array
! to hold all data for given satellite
  nele=nreal+nchanl
  allocate(data_all(nele,itxmax),data_crit(itxmax),idata_itx(itxmax))


! Open up bufr file for mpi-io access
  call mpi_openbf(infile,npe_sub,mype_sub,mpi_comm_sub)


! Big loop to read data file
  mpi_loop: do mmblocks=0,nblocks-1,npe_sub

     if(mmblocks+mype_sub.gt.nblocks-1) exit
     call mpi_nextblock(mmblocks+mype_sub)

     block_loop: do while (mpi_ireadmg(lnbufr,subset,idate)==0)

        read_loop: do while (mpi_ireadsb(lnbufr)==0 .and. subset==subfgn)

!       BUFR read 1/3
        call mpi_ufbint(lnbufr,bufrinit,7,1,nlv, &
             "SAID SECO SLNM FOVN RSURF RAINF ORBN" )

!       Extract satellite id.  If not the one we want, read next record
        said = int( bufrinit(1) + MILLI ) 
        if( said /= bufsat) exit read_loop
        
        rsat=bufsat
        
        fovn = bufrinit(4)
        ifov = int(fovn+MILLI)-ifovoff

        if(ifov>nscan) then
           write(6,*) 'READ_SSMIS(',obstype,'): unreliable FOV number fovn=',fovn, &
                ' ifov=',ifov
           cycle read_loop
        end if

!       SSMIS imager has 180 scan positions.  Select every other position.
        if(ssmis_img) then
           if (mod(ifov,2)/=0) then
              cycle read_loop
           else
              ifov = min(max(1,nint(float(ifov)/two + half)),90)
           endif
        endif

!       BUFR read 2/3
        call mpi_ufbrep(lnbufr,bufrymd,3,5,nlv,"YEAR MNTH DAYS" )
        call mpi_ufbrep(lnbufr,bufrhm, 2,2,nlv,"HOUR MINU" )

        
!       Calc obs seqential time  If time outside window, skip this obs
        iobsdate(1:3) = bufrymd(1:3,1) !year,month,day for scan start time  kozo
        iobsdate(4:5) = bufrhm(1:2,1)  !hour,min for scan start time  kozo
        isc         = bufrinit(2) !second for scan start time kozo
        call w3fs21(iobsdate,nmind)
        sstime=float(nmind) + isc*oneover60
        tdiff=(sstime-gstime)*oneover60
        if(abs(tdiff) > twind)  then 
           write(6,*) 'READ_SSMIS(',obstype,'): time check fail: obstime=',iobsdate
           cycle read_loop
        end if
        
!       Extract obs location, TBB, other information

!       BUFR read 3/3
        call mpi_ufbrep(lnbufr,bufrloc,  2,29,      nlv,"CLAT CLON" )
        call mpi_ufbrep(lnbufr,bufrtbb,  2,maxchanl,nlv,"CHNM TMBR" )
!       call mpi_ufbrep(lnbufr,bufrinfo, 1,3,       nlv,"SELV" )
!       call mpi_ufbrep(lnbufr,bufrlleaa,2,28,      nlv,"RAIA BEARAZ" )
        
!       Regional case
        dlat_earth = bufrloc(1,1)  !degrees
        dlon_earth = bufrloc(2,1)  !degrees
        if(dlon_earth<zero ) dlon_earth = dlon_earth+r360
        if(dlon_earth>=r360) dlon_earth = dlon_earth-r360
        dlat_earth = dlat_earth*deg2rad
        dlon_earth = dlon_earth*deg2rad

        if(regional)then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if(diagnostic_reg) then
              call txy2ll(dlon,dlat,dlon00,dlat00)
              ntest=ntest+1
              disterr=acosd(sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                   (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00)))
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
        timedif = 6.0_r_kind*abs(tdiff) ! range:  0 to 18
        crit1 = 0.01_r_kind+timedif + flgch
!       Map obs to thinning grid
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse)
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

        call deter_sfc(dlat,dlon,isflg,sfcpct,sstx)

        crit1 = crit1 + rlndsea(isflg)
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

!       Set common predictor parameters
        pred = zero
        
!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
        crit1 = crit1+pred 

        call finalcheck(dist1,crit1,ndata,itx,iout,iuse,sis)
        if(.not. iuse)cycle read_loop

        data_all( 1,iout)= rsat                 !satellite id
        data_all( 2,iout)= tdiff                !time diff between obs and anal (min)
        data_all( 3,iout)= dlon                 !grid relative longitude
        data_all( 4,iout)= dlat                 !grid relative latitude
        data_all( 5,iout)= incangl*deg2rad      !local zenith angle (rad)
        data_all( 6,iout)= 999.00               !local azimuth angle (missing)
        data_all( 7,iout)= zero                 !look angle (rad)
        data_all( 8,iout)= ifov                 !FOV scan position 
        data_all( 9,iout)= zero                 !solar zenith angle (deg) : not used for MW-RT calc
        data_all(10,iout)= 999.00               !solar zenith angle (deg) : not used for MW-RT calc
        data_all(11,iout)= sfcpct(0)            ! ocean percentage
        data_all(12,iout)= sfcpct(1)            ! land percentage
        data_all(13,iout)= sfcpct(2)            ! ice percentage
        data_all(14,iout)= sfcpct(3)            ! snow percentage
        data_all(15,iout)= dlon_earth*rad2deg   ! earth relative longitude (degrees)
        data_all(16,iout)= dlat_earth*rad2deg   ! earth relative latitude (degrees)

        data_all(nreal-1,iout)=val_ssmis
        data_all(nreal,iout)=itt
        do jc=1,maxchanl
           data_all(nreal+jc,iout) = tbob(jc)
        end do

        idata_itx(iout) = itx                  ! thinning grid location
        data_crit(iout) = crit1*dist1          ! observation "score"

     end do read_loop
  end do block_loop
end do mpi_loop
  
! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  if (npe_sub>1) &
       call combine_radobs(mype,mype_sub,mype_root,npe_sub,mpi_comm_sub,&
       nele,itxmax,nread,ndata,data_all,data_crit,idata_itx)


! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root) then

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

! Close bufr file
  call mpi_closbf
  call closbf(lnbufr)

! Deallocate local arrays
  deallocate(data_all,data_crit,idata_itx)

! Deallocate satthin arrays
1000 continue
  call destroygrids
    
  if(diagnostic_reg .and. ntest>0 .and. mype_sub==mype_root) &
       write(6,*)'READ_SSMIS:  mype,ntest,disterrmax=',&
       mype,ntest,disterrmax
  
! End of routine
  return
end subroutine read_ssmis
