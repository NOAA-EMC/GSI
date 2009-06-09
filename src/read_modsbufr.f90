subroutine read_modsbufr(nread,ndata,nodata,gstime,infile,obstype,lunout, &
          twindin,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_modsbufr                read sst obs from modsbufr file (based on MODS)
!   prgmmr: Xu Li          org: np22                date: 2005-10-20
!
! abstract:  This routine reads conventional sst data from modsbufr
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-12  treadon - remove unused variables from gridmod module
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-04-18  safford - rm unused vars and uses
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     twindin  - input group time window (hours)
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     nodata   - number of individual "obstype" observations retained for further processing
!     sis      - satellite/instrument/sensor indicator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind,r_single
  use constants, only: izero,zero,one_tenth,one,deg2rad,&
       three,rad2deg,r60inv
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,&
       tll2xy,txy2ll,rlats,rlons,twodvar_regional
  use convinfo, only: nconvtype,ctwind, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype
  use obsmod, only: oberrflg
  use gsi_4dvar, only: l4dvar, idmodel, iwinbgn, winlen
  implicit none

! Declare passed variables
  character(len=*),intent(in):: infile,obstype
  character(len=*),intent(in):: sis
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata,nodata
  real(r_kind),intent(in):: gstime,twindin

! Declare local parameters
  real(r_double),parameter:: d250 = 250.0_r_double
  real(r_double),parameter:: d400 = 400.0_r_double
  real(r_kind),parameter:: r0_5 = 0.5_r_kind
  real(r_kind),parameter:: r100 = 100.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind

  real(r_kind),parameter:: bmiss = 1.0E11

! Declare local variables
  logical outside

  integer(i_kind) lunin,i,maxobs
  integer(i_kind) idate,iret,k
  integer(i_kind) kx,nreal,nchanl,ilat,ilon
  integer(i_kind) sstq,nmind
  integer(i_kind):: idomsfc

  integer(i_kind) :: ireadmg,ireadsb
  integer(i_kind), dimension(5) :: idate5
  character(len=5)  :: subset
  character(len=8)  :: crpid
  character(len=80) :: headr
  character(len=5)  :: cid
  real(r_double), dimension(9) :: hdr(9)
  real(r_double)  :: msst,sst
  equivalence (crpid,hdr(9))

  real(r_kind) :: tdiff,sstime,usage,sfcr,tsavg,ff10,t4dv
  real(r_kind) :: dlat,dlon,sstoe,dlat_earth,dlon_earth

  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  integer(i_kind) ntest

  real(r_kind),allocatable,dimension(:,:):: cdata_all
  real(r_single),allocatable::etabl(:,:,:)
  integer(i_kind) ietabl,lcount,itypex
  integer(i_kind) l,m,ikx
  real(r_kind) terrmin,werrmin,perrmin,qerrmin,pwerrmin

  data headr/'YEAR MNTH DAYS HOUR MINU CLATH CLONH SELV RPID'/

  data lunin / 10 /
!**************************************************************************
! Initialize variables
  disterrmax=zero
  ntest=0
  maxobs=2e6
  nread=0
  ndata = 0
  nodata = 0
  nchanl=0
  ilon=2
  ilat=3

  nreal=19

  allocate(cdata_all(nreal,maxobs))


  if(oberrflg)then
     allocate(etabl(300,33,6))
     ietabl=19
     open(ietabl,file='errtable',form='formatted')
     rewind ietabl
     etabl=1.e9_r_kind
     lcount=0
     do l=1,300
        read(ietabl,100,end=120,err=120)itypex
100     format(1x,i3)
        lcount=lcount+1
        do k=1,33
           read(ietabl,110)(etabl(itypex,k,m),m=1,6)
110        format(1x,6e12.5)
        end do
     end do
120  continue
     if(lcount.le.0) then
       write(6,*)'READ_MODSBUFR:  ***WARNING*** obs error table not available to 3dvar.'
       oberrflg=.false.
     end if
     close(ietabl)

!    Set lower limits for observation errors
     terrmin=r0_5
     werrmin=one
     perrmin=r0_5
     qerrmin=one_tenth
     pwerrmin=one
     
  endif

! Open, then read date from bufr data
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
       
! READING EACH REPORT FROM BUFR
       
  do while (ireadmg(lunin,subset,idate) == 0)
    do while (ireadsb(lunin) == 0)
      call ufbint(lunin,hdr,9,1,iret,headr)
       
!          Measurement types
!             0       Ship intake
!             1       Bucket
!             2       Hull contact sensor
!             3       Reversing Thermometer
!             4       STD/CTD sensor
!             5       Mechanical BT
!             6       Expendable BT
!             7       Digital BT
!             8       Thermistor chain
!             9       Infra-red scanner
!             10      Micro-wave scanner
!             11-14   Reserved
! data headr/'YEAR MNTH DAYS HOUR MINU CLATH CLONH SELV RPID'/
!
!     Determine measurement type
!

      if ( subset == 'SHIPS' .or. subset == 'MBUOY' .or. &
           subset == 'LCMAN' ) then
        call ufbint(lunin,msst,1,1,iret,'MSST')      ! for ships, fixed buoy and lcman
      elseif ( subset == 'DBUOY' ) then
        msst = 11.0                                  ! for drifting buoy, assign to be 11
      endif
       
      call ufbint(lunin,sst,1,1,iret,'SST1')         ! read SST
       
      nread = nread + 1
       
!   if ( iret > 0 ) then
    if ( iret > 0 .and. ( sst >d250 .and. sst < d400) ) then
       
!     Extract type, date, and location information
      if(hdr(7) >= r360)  hdr(7) = hdr(7) - r360
      if(hdr(7) <  zero)  hdr(7) = hdr(7) + r360
       
      dlon_earth=hdr(7)*deg2rad
      dlat_earth=hdr(6)*deg2rad
       
      if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
          if(diagnostic_reg) then
            call txy2ll(dlon,dlat,rlon00,rlat00)
            ntest=ntest+1
            cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                 (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
            cdist=max(-one,min(cdist,one))
            disterr=acos(cdist)*rad2deg
            disterrmax=max(disterrmax,disterr)
          end if
        if(outside) go to 10   ! check to see if outside regional domain
      else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,1,rlats,nlat,1)
        call grdcrd(dlon,1,rlons,nlon,1)
      endif

!     Extract date information.  If time outside window, skip this obs
      idate5(1) = nint(hdr(1))    !year
      idate5(2) = nint(hdr(2))    !month
      idate5(3) = nint(hdr(3))    !day
      idate5(4) = nint(hdr(4))    !hour
      idate5(5) = nint(hdr(5))    !minute
       
!     determine platform (ships, dbuoy, fbuoy or lcman) dependent obs. error
!
      if ( subset == 'SHIPS' ) then                                            ! ships
        kx = 180
        sstoe = 1.0_r_kind
      elseif ( subset == 'DBUOY' ) then
       
        kx = 181
        cid = trim(crpid)
       
      if ( cid(3:3) == '5' .or. cid(3:3) == '6' .or. cid(3:3) == '7' .or. &
           cid(3:3) == '8' .or. cid(3:3) == '9' ) then                       ! drifting buoy
        sstoe = 0.75_r_kind
      elseif ( cid(3:3) == '0' .or. cid(3:3) == '1' .or. cid(3:3) == '2' .or. &
               cid(3:3) == '3' .or. cid(3:3) == '4' ) then                     ! fixed buoy
        sstoe = 0.25_r_kind
      endif
       
      elseif ( subset == 'MBUOY' ) then                                        ! fixed buoy
        sstoe = 0.25_r_kind
        kx = 181
      elseif ( subset == 'LCMAN' ) then                                        ! lcman
        kx = 181
        sstoe = 1.0_r_kind
      endif

!
! Determine usage
!
      ikx = izero
      do i = 1, nconvtype
        if(kx == ictype(i) .and. abs(icuse(i))== 1) ikx=i

      end do

      if(ikx == izero) go to 10             ! not ob type used

      call w3fs21(idate5,nmind)
      t4dv=real((nmind-iwinbgn),r_kind)*r60inv
!
      if (l4dvar) then
        if (t4dv<zero .OR. t4dv>winlen) go to 10
      else
        tdiff=(sstime-gstime)*r60inv
        if(abs(tdiff)>twindin .or. abs(tdiff)>ctwind(ikx)) go to 10  ! outside time window
      endif

!    If running in 2d-var (surface analysis) mode, check to see if observation
!    is surface type.  If not, read next observation report from bufr file
!    if ( twodvar_regional .and. &
!         (kx<180 .or. kx>289 .or. (kx>189 .and. kx<280)) ) go to 10

      usage = zero
      if (   icuse(ikx) < izero ) usage = r100
      if ( ncnumgrp(ikx) > izero ) then                                ! cross validation on
        if (mod(ndata+1,ncnumgrp(ikx))== ncgroup(ikx)-1) usage=ncmiter(ikx)
      end if
      nodata = nodata + 1
      ndata = ndata + 1
      if(ndata > maxobs) then
           write(6,*)'READ_MODSBUFR:  ***WARNING*** ndata > maxobs for ',obstype
           ndata = maxobs
      end if
      call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr)

      cdata_all(1,ndata)  = sstoe                   ! sst error
      cdata_all(2,ndata)  = dlon                    ! grid relative longitude
      cdata_all(3,ndata)  = dlat                    ! grid relative latitude
      cdata_all(4,ndata)  = sst                     ! sst obs
      cdata_all(5,ndata)  = hdr(9)                  ! station id
      cdata_all(6,ndata)  = t4dv                    ! time
      cdata_all(7,ndata)  = ikx                     ! type
      cdata_all(8,ndata)  = sstoe*three             ! pw max error
      cdata_all(9,ndata)  = hdr(8)                  ! depth of measurement
      cdata_all(10,ndata) = msst                    ! measurement type
      cdata_all(11,ndata) = sstq                    ! quality mark
      cdata_all(12,ndata) = bmiss                   ! original obs error
      cdata_all(13,ndata) = usage                   ! usage parameter
      cdata_all(14,ndata) = idomsfc+0.001           ! dominate surface type
      cdata_all(15,ndata) = tsavg                   ! skin temperature
      cdata_all(16,ndata) = ff10                    ! 10 meter wind factor
      cdata_all(17,ndata) = sfcr                    ! surface roughness
      cdata_all(18,ndata) = dlon_earth*rad2deg      ! earth relative longitude (degrees)
      cdata_all(19,ndata) = dlat_earth*rad2deg      ! earth relative latitude (degrees)
    end if                                          ! if ( iret > 0 ) then
10 continue
    enddo
  enddo
!
!   End of bufr read loop
       
! Normal exit
1000 continue

! Write header record and data to output file for further processing
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)

! Close unit to bufr file
1020 continue
  if (oberrflg) deallocate(etabl)
  call closbf(lunin)

  if(diagnostic_reg.and.ntest > 0) write(6,*)'READ_MODSBUFR:  ',&
       'ntest,disterrmax=',ntest,disterrmax


! End of routine
  return
end subroutine read_modsbufr
