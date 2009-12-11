subroutine read_avhrr(mype,val_avhrr,ithin,rmesh,jsatid,&
     gstime,infile,lunout,obstype,nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_avhrr_gac                  read gac avhrr data
!   prgmmr: li, xu           org: np23                date: 2005-10-20
!
! abstract:  This routine reads BUFR format AVHRR GAC 1b radiance (brightness
!            temperature) files, which are bufrized from the NESDIS 1b data.  Optionally, the
!            data are thinned to a specified resolution using simple
!            quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2005-10-20  li
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-27  derber  - clean up code
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2007-03-28  derber  - add CLAVR (CLAVR cloud flag)
!   2008-04-21  safford - rm unused vars and uses
!   2008-10-10  derber  - modify to allow mpi_io
!   2008-12-30  todling - memory leak fix (data_crit)
!   2009-04-21  derber  - add ithin to call to makegrids
!
!   input argument list:
!     mype     - mpi task id
!     val_avhrr- weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of BUFR GAC AVHRR observations read
!     ndata    - number of BUFR GAC AVHRR profiles retained for further processing
!     nodata   - number of BUFR GAC AVHRR observations retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
               finalcheck,score_crit
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,tll2xy,txy2ll,rlats,rlons
  use constants, only: deg2rad, izero, ione, zero, one, two, half, rad2deg, r60inv
  use radinfo, only: retrieval,iuse_rad,jpch_rad,nusis
  use gsi_4dvar, only: l4dvar, iwinbgn, winlen
  implicit none


! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=*),intent(in   ) :: sis
  integer(i_kind) ,intent(in   ) :: mype,lunout,ithin
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind) ,intent(inout) :: ndata,nodata
  real(r_kind)    ,intent(in   ) :: rmesh,gstime,twind
  real(r_kind)    ,intent(inout) :: val_avhrr
  integer(i_kind) ,intent(in   ) :: mype_root
  integer(i_kind) ,intent(in   ) :: mype_sub
  integer(i_kind) ,intent(in   ) :: npe_sub
  integer(i_kind) ,intent(in   ) :: mpi_comm_sub


! Declare local parameters
  character(6),parameter:: file_sst='SST_AN'
  integer(i_kind),parameter:: maxinfo = 35_i_kind
  integer(i_kind),parameter:: mlat_sst = 3000_i_kind
  integer(i_kind),parameter:: mlon_sst = 5000_i_kind
  real(r_kind),parameter:: r6=6.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind

  real(r_kind),parameter:: bmiss = 1.0E11_r_kind
  real(r_kind),parameter :: ngac=409.0_r_kind,nfov=90.0_r_kind,cut_spot=11.0_r_kind
  character(len=80),parameter ::  &
    headr='YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAID FOVN SAZA SOZA CLAVR'

! Declare local variables  
  logical outside,iuse,assim
  character(len=8) :: subset

  integer(i_kind) klon1,klatp1,klonp1,klat1
  integer(i_kind) nchanl,iret
  integer(i_kind) idate
  integer(i_kind) ilat,ilon
  integer(i_kind),dimension(5):: idate5
  integer(i_kind) nmind,isflg,idomsfc
  integer(i_kind) itx,k,i,bufsat,n
  integer(i_kind) nele,itt
  integer(i_kind) nlat_sst,nlon_sst

  real(r_kind) dlon,dlat,timedif,rsc
  real(r_kind) dlon_earth,dlat_earth,sfcr
  real(r_kind) w00,w01,w10,w11,dx1,dy1
  real(r_kind) pred,crit1,tdiff,sstime,dx,dy,dist1
  real(r_kind) dlat_sst,dlon_sst,sst_hires
  real(r_kind) t4dv

  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind),dimension(mlat_sst):: rlats_sst
  real(r_kind),dimension(mlon_sst):: rlons_sst
  real(r_kind),dimension(mlat_sst,mlon_sst):: sst_an
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: scan_pos,dfov

  real(r_double), dimension(13) :: hdr
  real(r_double), dimension(3,5) :: bufrf
  integer(i_kind) lnbufr,ireadsb,ireadmg,iskip,irec,isub,next

  real(r_kind) disterr,disterrmax,dlon00,dlat00
  integer(i_kind) ntest

! real(r_kind), dimension(3,2) :: bandcor_a,bandcor_b
! data bandcor_a/-1.70686_r_kind,-0.27201_r_kind,-0.30949_r_kind,-1.70388_r_kind,-0.43725_r_kind,-0.25342_r_kind/
! data bandcor_b/1.002629_r_kind,1.001207_r_kind,1.000989_r_kind,1.003049_r_kind,1.001395_r_kind,1.000944_r_kind/


!**************************************************************************

! Start routine here.  Set constants.  Initialize variables
  lnbufr = 10_i_kind
  disterrmax=zero
  ntest=izero
  ndata  = izero
  nodata  = izero
  nchanl = 3_i_kind

  dfov = (ngac - two*cut_spot)/nfov

  ilon=3_i_kind
  ilat=4_i_kind

  rlndsea(0) = zero
  rlndsea(1) = 30._r_kind
  rlndsea(2) = 20._r_kind
  rlndsea(3) = 30._r_kind
  rlndsea(4) = 30._r_kind

                                        ! 207, 208 or 209 for NOAA-16, 17 & 18 respectively
  if(jsatid == 'n16')bufsat = 207_i_kind
  if(jsatid == 'n17')bufsat = 208_i_kind
  if(jsatid == 'n18')bufsat = 209_i_kind
  if(jsatid == 'n19')bufsat = 223_i_kind

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((nusis(i)==sis) .and. (iuse_rad(i)>izero)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_avhrr=zero


! Make thinning grids
  call makegrids(rmesh,ithin)


! Read hi-res sst analysis
  if (retrieval) call rdgrbsst(file_sst,mlat_sst,mlon_sst,&
       sst_an,rlats_sst,rlons_sst,nlat_sst,nlon_sst)


! Allocate arrays to hold all data for given satellite
  nele=maxinfo+nchanl
  allocate(data_all(nele,itxmax))

  open(lnbufr,file=infile,form='unformatted')         ! open bufr data file

! Associate the tables file with the message file, and identify the 
! latter to BUFRLIB software
  call openbf (lnbufr,'IN',lnbufr)

  next=mype_sub+ione
! Read BUFR AVHRR GAC 1b data
  do while (ireadmg(lnbufr,subset,idate) >= izero)
     call ufbcnt(lnbufr,irec,isub)
     if(irec/=next)cycle
     next=next+npe_sub
     read_loop: do while (ireadsb(lnbufr) == izero)
        call ufbint(lnbufr,hdr,13_i_kind,ione,iret,headr)
        call ufbrep(lnbufr,bufrf, 3_i_kind,5_i_kind,iret,'INCN ALBD TMBR')
        if(iret <= izero) cycle read_loop
        if (hdr(10) <= real(cut_spot) .or. hdr(10) > real(ngac-cut_spot)) cycle read_loop! drop starting and ending pixels
        if (hdr(13) /= zero ) cycle read_loop ! drop pixel with CLAVR partly cloud flag
 
        iskip = izero

        do k=1,nchanl
           if(bufrf(3,2_i_kind+k) < zero .or. bufrf(3,2_i_kind+k) > tbmax) then
              iskip=iskip+ione
           else
              nread=nread+ione
           end if
        end do
        if(iskip >= nchanl)cycle read_loop

!       Extract date information.  If time outside window, skip this obs
        idate5(1) = nint(hdr(1))    !year
        idate5(2) = nint(hdr(2))    !month
        idate5(3) = nint(hdr(3))    !day
        idate5(4) = nint(hdr(4))    !hour
        idate5(5) = nint(hdr(5))    !minute
        rsc       = hdr(6)          !second in real
        call w3fs21(idate5,nmind)
        t4dv=(real((nmind-iwinbgn),r_kind) + rsc*r60inv)*r60inv
        if (l4dvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           sstime=real(nmind,r_kind) + rsc*r60inv
           tdiff=(sstime-gstime)*r60inv
           if(abs(tdiff) > twind) cycle read_loop
        endif

!       Convert obs location to radians
        if (hdr(8)>=r360) hdr(8)=hdr(8)-r360
        if (hdr(8)< zero) hdr(8)=hdr(8)+r360

        dlon_earth = hdr(8)*deg2rad   !convert degrees to radians
        dlat_earth = hdr(7)*deg2rad

!       Regional case
        if(regional)then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if(diagnostic_reg) then
              call txy2ll(dlon,dlat,dlon00,dlat00)
              ntest=ntest+ione
              disterr=acos(sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                   (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00)))*rad2deg
              disterrmax=max(disterrmax,disterr)
           end if
           if(outside) cycle read_loop

!       Global case
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd(dlat,ione,rlats,nlat,ione)
           call grdcrd(dlon,ione,rlons,nlon,ione)
        endif
        if (l4dvar) then
           crit1 = 0.01_r_kind
        else
           timedif = r6*abs(tdiff)        ! range:  0 to 18
           crit1 = 0.01_r_kind+timedif
        endif
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)

        if(.not. iuse)cycle read_loop

!       Interpolate hi-res sst analysis to observation location
        dlat_sst = dlat_earth
        dlon_sst = dlon_earth
        call grdcrd(dlat_sst,ione,rlats_sst,nlat_sst,ione)
        call grdcrd(dlon_sst,ione,rlons_sst,nlon_sst,ione)

        klon1=int(dlon_sst); klat1=int(dlat_sst)
        dx  =dlon_sst-klon1; dy  =dlat_sst-klat1
        dx1 =one-dx;         dy1 =one-dy
        w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

        klat1=min(max(ione,klat1),nlat_sst); klon1=min(max(izero,klon1),nlon_sst)
        if(klon1==izero) klon1=nlon_sst
        klatp1=min(nlat_sst,klat1+ione); klonp1=klon1+ione
        if(klonp1==nlon_sst+ione) klonp1=ione

        sst_hires=w00*sst_an(klat1,klon1 ) + w10*sst_an(klatp1,klon1 ) + &
                  w01*sst_an(klat1,klonp1) + w11*sst_an(klatp1,klonp1)

        if ( sst_hires < zero ) then
           print*,' sst_hires,klat1,klon1 : ',sst_hires,klat1,klon1
        endif


!     "Score" observation.   We use this information to id "best" obs.

!     Locate the observation on the analysis grid.  Get sst and land/sea/ice
!     mask.  

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                          


        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
              ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

        crit1=crit1+rlndsea(isflg)

!       Set common predictor parameters

!test
        pred=zero
!test        
!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"

        crit1 = crit1+pred  
        call finalcheck(dist1,crit1,itx,iuse)

        if(.not. iuse)cycle read_loop
!
!       Get scan position (1 - 90) based on 409 GAC pixels
!
        if ( mod(hdr(10)-cut_spot,dfov) < half*dfov ) then
           scan_pos = real(nint((hdr(10)-cut_spot)/dfov) + ione) 
        else
           scan_pos = real(nint((hdr(10)-cut_spot)/dfov)) 
        endif

        if ( scan_pos > nfov ) scan_pos = nfov
           
!       Transfer information to work array

        data_all(1, itx) = hdr(9)                ! satellite id (207 = NOAA-16, 208 = NOAA-17, 209 = NOAA-18)
        data_all(2, itx) = t4dv                  ! time (hours)
        data_all(3, itx) = dlon                  ! grid relative longitude
        data_all(4, itx) = dlat                  ! grid relative latitude
        data_all(5, itx) = hdr(11)*deg2rad       ! satellite zenith angle (radians)
        data_all(6, itx) = bmiss                 ! satellite azimuth angle
        data_all(7, itx) = bmiss                 ! look angle
        data_all(8, itx) = scan_pos              ! scan position
        data_all(9, itx) = hdr(12)               ! solar zenith angle (radians)
        data_all(10,itx) = bmiss                 ! solar azimuth angle (radians)
        data_all(11,itx) = sfcpct(0)             ! sea percentage of
        data_all(12,itx) = sfcpct(1)             ! land percentage
        data_all(13,itx) = sfcpct(2)             ! sea ice percentage
        data_all(14,itx) = sfcpct(3)             ! snow percentage
        data_all(15,itx)= ts(0)                  ! ocean skin temperature
        data_all(16,itx)= ts(1)                  ! land skin temperature
        data_all(17,itx)= ts(2)                  ! ice skin temperature
        data_all(18,itx)= ts(3)                  ! snow skin temperature
        data_all(19,itx)= tsavg                  ! average skin temperature
        data_all(20,itx)= vty                    ! vegetation type
        data_all(21,itx)= vfr                    ! vegetation fraction
        data_all(22,itx)= sty                    ! soil type
        data_all(23,itx)= stp                    ! soil temperature
        data_all(24,itx)= sm                     ! soil moisture
        data_all(25,itx)= sn                     ! snow depth
        data_all(26,itx)= zz                     ! surface height
        data_all(27,itx)= idomsfc + 0.001_r_kind ! dominate surface type
        data_all(28,itx)= sfcr                   ! surface roughness
        data_all(29,itx)= ff10                   ! ten meter wind factor
        data_all(30,itx)= dlon_earth*rad2deg     ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth*rad2deg     ! earth relative latitude (degrees)

        data_all(32,itx) = hdr(13)               ! CLAVR Cloud flag (only 0 = clear and 1 = probably clear included the data set used now)

        data_all(33,itx) = sst_hires             ! interpolated hires SST (deg K)

        data_all(34,itx) = val_avhrr             ! weighting factor applied to super obs
        data_all(35,itx) = itt                   !

        do k=1,nchanl
           data_all(k+maxinfo,itx)= bufrf(3,2_i_kind+k) ! Tb for avhrr ch-3, ch-4 and ch-5
        end do


!    End of satellite read block

     enddo read_loop
  enddo

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
       nele,itxmax,nread,ndata,data_all,score_crit)

  write(6,*) 'READ_AVHRR:  total number of obs, nread,ndata : ',nread,ndata

! Normal exit
700 continue


! Now that we've identified the "best" observations, pull out best obs
! and write them to the output file

  do n=1,ndata
     do k=1,nchanl
        if(data_all(k+maxinfo,n) > tbmin .and. &
           data_all(k+maxinfo,n) < tbmax) nodata=nodata+ione
     end do
     itt=nint(data_all(maxinfo,n))
     super_val(itt)=super_val(itt)+val_avhrr
  end do

! Write retained data to local file
  write(lunout) obstype,sis,maxinfo,nchanl,ilat,ilon
  write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)

! Deallocate local arrays
  deallocate(data_all)

! Deallocate arrays
900 continue
  call destroygrids
  call closbf(lnbufr)

! Print data counts
  write(6,8000) infile,sis,nread,rmesh,ndata
8000 format(' READ_AVHRR:  infile=',a10,&
          '   sis=',a20,&
          '   nread=',i10, &
          '   rmesh=',f7.3,'   ndata=',i10)

 if(diagnostic_reg.and.ntest>izero) write(6,*)'READ_AVHRR:  ',&
      'mype,ntest,disterrmax=',mype,ntest,disterrmax

! End of routine
  return
end subroutine read_avhrr
