subroutine read_tcps(nread,ndata,nodata,infile,obstype,lunout,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_tcps                   read tcvitals ascii file
!   prgmmr: kleist             org: np20                date: 2009-02-02
!
! abstract:  This routine reads the ascii TC psmin data file 
!
!
! program history log:
!   2009-02-02  kleist
!
!   input argument list:
!     infile   - unit from which to read ascii file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!
!   output argument list:
!     nread    - number of bogus data read
!     ndata    - number of bogus data retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,rlats,rlons
  use constants, only: deg2rad,rad2deg,izero,ione,zero,one_tenth
  use convinfo, only: nconvtype,ictype,icuse
  use obsmod, only: ianldate
  use tcv_mod, only: get_storminfo,numstorms,stormlat,stormlon,stormpsmin,stormdattim
  use gsi_4dvar, only: time_4dvar
  implicit none

! Declare passed variables
  character(10)  ,intent(in   ) :: obstype,infile
  character(20)  ,intent(in   ) :: sis
  integer(i_kind),intent(in   ) :: lunout
  integer(i_kind),intent(inout) :: nread,ndata,nodata

! Declare local parameters
  real(r_kind),parameter:: r360=360.0_r_kind
  integer(i_kind),parameter:: maxobs=2e6_i_kind
  integer(i_kind),parameter:: maxdat=9_i_kind

! Declare local variables
  real(r_kind) dlat,dlon,dlat_earth,dlon_earth
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  real(r_kind) ohr,olat,olon,psob,pob,oberr,usage,toff

  integer(i_kind) i,k,iret,lunin,nc
  integer(i_kind) ilat,ilon,ikx,nreal,nchanl,nmrecs

  logical endfile

  data lunin / 10_i_kind /

!**************************************************************************
! Initialize variables
  nmrecs=izero
  nreal=maxdat
  nchanl=izero
  ilon=2_i_kind
  ilat=3_i_kind
  endfile=.false.

  allocate(cdata_all(maxdat,maxobs))

  open(lunin,file=infile,form='formatted',action='read')
  call datelen(10)

! Big loop over binary file

  call get_storminfo(lunin)

  write(6,*) 'READ_TCPS:  IANLDATE = ',ianldate

  do i=1,numstorms
     nmrecs=nmrecs+ione
     nread=nread+ione

! Set ikx here...pseudo-mslp tc_vitals obs are assumed to be type 111
     do nc=1,nconvtype
        if (ictype(nc)==112_i_kind) ikx=nc
     end do

! Set usage variable
     usage = zero
     if(icuse(ikx) <= izero)usage=100._r_kind

     if (stormdattim(i)/=ianldate) then
        write(6,*) 'READ_TCPS:  IGNORE TC_VITALS ENTRY # ',i
        write(6,*) 'READ_TCPS:  MISMATCHED FROM ANALYSIS TIME, OBS / ANL DATES = ',stormdattim(i),ianldate
        go to 990
     end if

! Observation occurs at analysis time as per date check above
! Set observation lat, lon, mslp, and default obs-error
     call time_4dvar(ianldate,toff)
     write(6,*)'READ_TCPS: bufr file date is ',ianldate
     write(6,*)'READ_TCPS: time offset is ',toff,' hours.'
     ohr=toff
     olat=stormlat(i)
     olon=stormlon(i)
     psob=stormpsmin(i)
     oberr=0.75_r_kind

! Make sure the psob is reasonable
     if ( (psob<850._r_kind) .or. (psob>1025._r_kind) )then
        usage=100._r_kind
     end if

     if (olon >= r360) olon=olon-r360
     if (olon < zero)  olon=olon+r360
     dlat_earth = olat * deg2rad
     dlon_earth = olon * deg2rad
 
     dlat = dlat_earth
     dlon = dlon_earth
     call grdcrd(dlat,ione,rlats,nlat,ione)
     call grdcrd(dlon,ione,rlons,nlon,ione)

! Extract observation.
     ndata=min(ndata+ione,maxobs)
     nodata=min(nodata+ione,maxobs)

! convert pressure (mb) to log(pres(cb))
     pob=one_tenth*psob
 
     cdata_all(1,ndata)=oberr*one_tenth       ! obs error converted to cb
     cdata_all(2,ndata)=dlon                  ! grid relative longitude
     cdata_all(3,ndata)=dlat                  ! grid relative latitude
     cdata_all(4,ndata)=pob                   ! pressure in cb 
     cdata_all(5,ndata)=toff                  ! obs time (analyis relative hour)
     cdata_all(6,ndata)=ikx                   ! obs type
     cdata_all(7,ndata)=dlon_earth*rad2deg    ! earth relative longitude (degrees)
     cdata_all(8,ndata)=dlat_earth*rad2deg    ! earth relative latitude (degrees)
     cdata_all(9,ndata)=usage                 ! usage parameter

990  continue

! End of loop over number of storms
  end do

! Normal exit
1000 continue

  write(6,*) 'READ_TCPS:  NUMBER OF OBS READ IN = ', ndata

! Write observations to scratch file
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)

! Close unit
1010 continue
  deallocate(cdata_all)
  close(lunin)

! End of routine
  return
end subroutine read_tcps
