subroutine read_ssmi(mype,val_mi,ithin,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub)
!$$$  subprogram documentation block

! subprogram:    read_ssmi           read SSM/I  bufr1b data
!   prgmmr: okamoto          org: np23                date: 2003-12-27
!
! abstract:  This routine reads BUFR format SSM/I 1b radiance 
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
!   2003-12-27 okamoto
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info - clean up
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc, refs to sno,sli,sst,isli (not used)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-03-07  derber - correct error in nodata count
!   2006-04-27  derber - some efficiency modifications
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!
!   input argument list:
!     mype     - mpi task id
!     val_mi   - weighting factor applied to super obs
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
!   output argument list:
!     nread    - number of BUFR SSM/I observations read (after eliminating orbit overlap)
!     ndata    - number of BUFR SSM/I profiles retained for further processing (thinned)
!     nodata   - number of BUFR SSM/I observations retained for further processing (thinned)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
            checkob,finalcheck
  use radinfo, only: iuse_rad,newchn,cbias
  use gridmod, only: diagnostic_reg,regional,rlats,rlons,nlat,nlon,&
       tll2xy,txy2ll
  use constants, only: deg2rad,rad2deg,zero,one,two,three,four,izero,ione
  use obsmod, only: iadate
  use mpi_bufr_mod, only: mpi_openbf,mpi_closbf,nblocks,mpi_nextblock,&
       mpi_readmg,mpi_ireadsb,mpi_ufbint,mpi_ufbrep

  implicit none

! Declare passed variables
  character(10),intent(in):: infile,obstype,jsatid
  character(20),intent(in):: sis
  integer(i_kind),intent(in)      :: mype,lunout,ithin
  integer(i_kind)  ,intent(in) :: mype_root
  integer(i_kind)  ,intent(in) :: mype_sub
  integer(i_kind)  ,intent(in) :: npe_sub
  integer(i_kind)  ,intent(in) :: mpi_comm_sub
  real(r_kind),intent(in) :: val_mi,rmesh,gstime,twind

  integer(i_kind),intent(inout):: nread

  integer(i_kind),intent(inout):: ndata,nodata


! Declare local parameters
  integer(i_kind),parameter :: n1bhdr=14
  integer(i_kind),parameter :: maxinfo=18
  integer(i_kind),parameter :: maxchanl=30

  integer(i_kind),parameter :: ntime=8      !time header
  integer(i_kind),parameter :: nloc=4       !location dat used for ufbint()
  integer(i_kind),parameter :: maxscan=64   !possible max of scan positons
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=70.0_r_kind
  real(r_kind),parameter:: tbmax=320.0_r_kind
  real(r_kind),parameter:: tbbad=-9.99e11_r_kind
  character(80),parameter:: hdr1b='SAID YEAR MNTH DAYS HOUR MINU SECO ORBN'   !use for ufbint()
  character(40),parameter:: str1='CLAT CLON SFTG POSN'   !use for ufbint()
  character(40),parameter:: str2='TMBR'                  !use for ufbrep()

! Declare local variables
  logical ssmi
  logical mixed,outside,iuse

  character(10) date
  character(8) subset,subfgn

  integer ihh,i,j,k,ifov,idd,jdate,isc,ireadmg,ireadsb,ntest
  integer iret,idate,im,iy,iyr,nchanl
  integer isflg,nreal
  integer nmind,itx,nele,nk,itt,iout
  integer iskip,lndsea
  integer lnbufr
  integer ilat,ilon

  real(r_kind) w00,w10,w01,w11,dx,dy,dx1,dy1,cosza

  real(r_kind) pred,rflag
  real(r_kind) panglr,rato,sstime,tdiff
  real(r_kind) crit1,step,start,sstx,dist1
  real(r_kind) timedif
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind),allocatable,dimension(:):: data_crit
  integer(i_kind),allocatable,dimension(:):: idata_itx
  integer(i_kind):: mmblocks
  integer(i_kind):: isubset

  real(r_kind) disterr,disterrmax,dlon00,dlat00

!  ---- bufr argument -----
  real(r_double),dimension(n1bhdr):: bfr1bhdr
  real(r_double),dimension(nloc,maxscan) :: midat  !location data from str1
  real(r_double),dimension(maxchanl*maxscan) :: mirad !TBB from str2



  integer(i_kind) :: nscan,jc,bufsat,js,ij,npos,n
  integer(i_kind),dimension(5):: iobsdate
  real(r_kind):: tb19v,tb22v,tb85v,si85,flgch
  real(r_kind),dimension(maxchanl):: tbob
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:4):: rlndsea

  real(r_kind):: lat0,lon0,oneover60
  real(r_kind):: dlat,dlon,dlon_earth,dlat_earth

!**************************************************************************
! Initialize variables
  lnbufr = 15
  disterrmax=zero
  ntest=0
  nreal  = maxinfo
  nchanl = 30
  ndata  = 0
  nodata  = 0
  nread  = 0
  oneover60 = one/60._r_kind

  ilon=3
  ilat=4

! Make thinning grids
  call makegrids(rmesh)


! Set various variables depending on type of data to be read

  ssmi  = obstype  == 'ssmi'

  if ( ssmi ) then
     nscan  = 64   !for A-scan
!    nscan  = 128  !for B-scan
     nchanl = 7
     subfgn = 'NC012001'
     if(jsatid == 'f13')bufsat=246
     if(jsatid == 'f14')bufsat=247
     if(jsatid == 'f15')bufsat=248
     rlndsea(0) = zero
     rlndsea(1) = 30._r_kind
     rlndsea(2) = 30._r_kind
     rlndsea(3) = 30._r_kind
     rlndsea(4) = 100._r_kind
  end if

! Open unit to satellite bufr file
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)
  call closbf(lnbufr)
  close(lnbufr)
  if( subset /= subfgn) then
     write(6,*) 'READ_SSMI:  *** WARNING: ',&
          'THE FILE TITLE NOT MATCH DATA SUBSET'
     write(6,*) '  infile=', lnbufr, infile,' subset=',&
          subset, ' subfgn=',subfgn
     write(6,*) 'SKIP PROCESSING OF THIS 1B FILE'
     go to 1000
  end if

  iy=0; im=0; idd=0; ihh=0
  write(date,'( i10)') idate
  read(date,'(i4,3i2)') iy,im,idd,ihh
  if (mype_sub==mype_root) &
       write(6,*) 'READ_SSMI: bufr file data is ',iy,im,idd,ihh,infile
  if(im/=iadate(2).or.idd/=iadate(3)) then
     write(6,*)'***READ_SSMI ERROR*** ',&
          'incompatable analysis and observation date/time'
     write(6,*)' year  anal/obs ',iadate(1),iy
     write(6,*)' month anal/obs ',iadate(2),im
     write(6,*)' day   anal/obs ',iadate(3),idd
     write(6,*)' hour  anal/obs ',iadate(4),ihh
     go to 1000
  end if



! Allocate arrays to hold data
  nele=nreal+nchanl
  allocate(data_all(nele,itxmax),data_crit(itxmax),idata_itx(itxmax))


! Open up bufr file for mpi-io access
  call mpi_openbf(infile,npe_sub,mype_sub,mpi_comm_sub)

! Big loop to read data file
  mpi_loop: do mmblocks=0,nblocks-1,npe_sub
     if(mmblocks+mype_sub.gt.nblocks-1) then
        exit
     endif
     call mpi_nextblock(mmblocks+mype_sub)
     block_loop: do
        call mpi_readmg(lnbufr,subset,idate,iret)
        if (iret /=0) exit
        read(subset,'(2x,i6)')isubset
        read_loop: do while (mpi_ireadsb(lnbufr)==0)


! ----- Read header record to extract satid,time information  
!       SSM/I data are stored in groups of nscan, hence the loop.  
        call mpi_ufbint(lnbufr,bfr1bhdr,ntime,1,iret,hdr1b)

!       Extract satellite id.  If not the one we want, read next record
        if(bfr1bhdr(1) /= bufsat) cycle read_loop


!       calc obs seqential time  If time outside window, skip this obs
        iobsdate(1:5) = bfr1bhdr(2:6) !year,month,day,hour,min
        isc         = bfr1bhdr(7) !second
        call w3fs21(iobsdate,nmind)
        sstime=float(nmind) + isc*oneover60
        if(abs(tdiff) > twind)  cycle read_loop

! ----- Read header record to extract obs location information  
!       SSM/I data are stored in groups of nscan, hence the loop.  

        call mpi_ufbint(lnbufr,midat,nloc,nscan,iret,str1)


!---    Extract brightness temperature data.  Apply gross check to data. 
!       If obs fails gross check, reset to missing obs value.

        call mpi_ufbrep(lnbufr,mirad,1,nchanl*nscan,iret,str2)


        ij=0
        scan_loop:   do js=1,nscan


!         Regional case
          dlat_earth = midat(1,js)  !deg
          dlon_earth = midat(2,js)  !deg
          if(dlon_earth< zero) dlon_earth = dlon_earth+r360
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

!            Check to see if in domain
             if(outside) cycle read_loop

!         Global case
          else
             dlat = dlat_earth  
             dlon = dlon_earth  
             call grdcrd(dlat,1,rlats,nlat,1)
             call grdcrd(dlon,1,rlons,nlon,1)
          endif

        
!         Transfer observed brightness temperature to work array.  
!         If any temperature exceeds limits, reset observation to "bad" value
!         mirad(1:maxchanl*nscan) => data1b(1:6+nchanl)
          iskip=0
          do jc=1,nchanl
             ij = ij+1
             if(mirad(ij)<tbmin .or. mirad(ij)>tbmax ) then
                mirad(ij) = tbbad
                iskip = iskip + 1
                if(jc == 1 .or. jc == 3 .or. jc == 6)iskip=iskip+nchanl
             else
                nread=nread+1
             end if
             tbob(jc) = mirad(ij) 
  
          end do   !jc_loop
          if(iskip >= nchanl)  cycle scan_loop  !if all ch for any posion is bad, skip 
          flgch = iskip*two   !used for thinning priority range 0-14
          
          timedif = 6.0_r_kind*abs(tdiff) ! range: 0 to 18
          crit1 = 0.01_r_kind+timedif + flgch
!         Map obs to thinning grid
          call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse)
          if(.not. iuse)cycle scan_loop


!         Locate the observation on the analysis grid.  Get sst and land/sea/ice
!         mask.  

!       isflg    - surface flag
!                  0 sea
!                  1 land
!                  2 sea ice
!                  3 snow
!                  4 mixed                     
!        sfcpct(0:3)- percentage of 4 surface types
!                   (0) - sea percentage
!                   (1) - land percentage
!                   (2) - sea ice percentage
!                   (3) - snow percentage
          call deter_sfc(dlat,dlon,isflg,sfcpct,sstx)
  
          crit1 = crit1 + rlndsea(isflg)
          call checkob(dist1,crit1,itx,iuse)
          if(.not. iuse)cycle scan_loop
  
!    ---- Set data quality predictor for initial qc -------------
!      -  simple si index : taken out from ssmiqc()
!            note! it exclude emission rain
          tb19v=tbob(1);  tb22v=tbob(3); tb85v=tbob(6)
          if(isflg/=0)  then !land+snow+ice
            si85 = 451.9_r_kind - 0.44_r_kind*tb19v - 1.775_r_kind*tb22v + &
               0.00574_r_kind*tb22v*tb22v - tb85v
          else    !sea
            si85 = -174.4_r_kind + 0.715_r_kind*tb19v + 2.439_r_kind*tb22v -  &
                 0.00504_r_kind*tb22v*tb22v - tb85v
          end if

!         Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
          pred = abs(si85)*three  !range: 0 to 30
          crit1 = crit1 + pred 

          call finalcheck(dist1,crit1,ndata,itx,iout,iuse,sis)
          if(.not. iuse)cycle scan_loop

          npos = (midat(4,js)-one)/four+one !original scan position 1.0->253.0 =4*(n-1)+1

!         Transfer observation parameters to output array.  
          data_all( 1,iout) = bufsat              !satellite id
          data_all( 2,iout) = tdiff               !time diff between obs and anal (min)
          data_all( 3,iout) = dlon                !grid relative longitude
          data_all( 4,iout) = dlat                !grid relative latitude
          data_all( 5,iout) = 53.1_r_kind*deg2rad !local zenith angle (rad)
          data_all( 6,iout) = 999.00              !local azimuth angle (missing)
          data_all( 7,iout) = zero                !look angle (rad) 
!+>       data_all( 7,iout) =  45.0*deg2rad       !look angle (rad)
          data_all( 8,iout) = npos                !scan position 1->64 
          data_all( 9,iout) = zero                !solar zenith angle (deg) : not used
          data_all(10,iout) = 999.00              !solar azimuth angle (missing) : not used
          data_all(11,iout)= sfcpct(0)            ! ocean percentage
          data_all(12,iout)= sfcpct(1)            ! land percentage
          data_all(13,iout)= sfcpct(2)            ! ice percentage
          data_all(14,iout)= sfcpct(3)            ! snow percentage
          data_all(15,iout)= dlon_earth*rad2deg   ! earth relative longitude (degrees)
          data_all(16,iout)= dlat_earth*rad2deg   ! earth relative latitude (degrees)

          data_all(nreal-1,iout)=val_mi
          data_all(nreal,iout)=itt
          do i=1,nchanl
             data_all(i+nreal,iout)=tbob(i)
          end do

          idata_itx(iout) = itx                  ! thinning grid location
          data_crit(iout) = crit1*dist1          ! observation "score"

        end do  scan_loop    !js_loop end

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
        super_val(itt)=super_val(itt)+val_mi
     end do

!    Write final set of "best" observations to output file
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif

! Close bufr file
  call mpi_closbf
  call closbf(lnbufr)

! Deallocate data arrays
  deallocate(data_all,data_crit,idata_itx)


! Deallocate satthin arrays
1000 continue
  call destroygrids

  if(diagnostic_reg .and. ntest>0 .and. mype_sub==mype_root) &
       write(6,*)'READ_SSMI:  mype,ntest,disterrmax=',&
       mype,ntest,disterrmax


! End of routine
 return
end subroutine read_ssmi
