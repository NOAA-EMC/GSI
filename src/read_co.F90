subroutine read_co(nread,ndata,nodata,jsatid,infile,gstime,lunout, &
           obstype,twind,sis,ithin,rmesh)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_co                    read co data
!   prgmmr: yang             org: np23                date: 1998-05-15
!
! abstract:  This routine reads CO observations. The initial code is taken 
!            from read_ozone.

! program history log: 

!    2010-03-30  Tangborn, initial code.

!   input argument list:
!     obstype  - observation type to process
!     jsatid   - satellite id to read
!     infile   - unit from which to read co data
!     gstime   - analysis time in minutes from reference date
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!
!   output argument list:
!     nread    - number of co observations read
!     ndata    - number of co profiles retained for further processing
!     nodata   - number of co observations retained for further processing

  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: makegrids,map2tgrid,destroygrids, &
               finalcheck,itxmax
  use gridmod, only: nlat,nlon,regional,tll2xy,rlats,rlons
  use constants, only: deg2rad,zero,rad2deg,one_tenth,r60inv,two
  use obsmod, only: iadate,nlco
  use convinfo, only: nconvtype, &
        icuse,ictype,ioctype
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen
  use qcmod, only: use_poq7
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile,jsatid
  character(len=*),intent(in   ) :: sis
  integer(i_kind) ,intent(in   ) :: lunout,ithin
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind) ,intent(inout) :: ndata,nodata
  real(r_kind)    ,intent(in   ) :: gstime,twind,rmesh

! Declare local parameters
  real(r_kind),parameter:: r6   = 6.0_r_kind
  real(r_kind),parameter:: r76  = 76.0_r_kind
  real(r_kind),parameter:: r84  = 84.0_r_kind

  real(r_kind),parameter:: r360 = 360.0_r_kind

  real(r_kind),parameter:: rmiss = -9999.9_r_kind
  real(r_kind),parameter:: badco = 10000.0_r_kind

! Declare local variables
  logical outside,version6,version8,iuse
  logical lerror,leof,lmax,lok


  character(2) version
  character(8) subset,subset6,subset8
  character(49) costr
  character(63) lcostr
  character(51) cogstr
  character(27) cogstr2
  character(42) costr2

  integer(i_kind) maxobs,ncodat
  integer(i_kind) idate,jdate,ksatid,kk,iy,iret,im,ihh,idd,lunin
  integer(i_kind) nmind,i,j
  integer(i_kind) imin
  integer(i_kind) k,ilat,ilon,nreal,nchanl
! integer(i_kind) ithin,kidsat
  integer(i_kind) kidsat
  integer(i_kind) idate5(5)
  integer(i_kind) JULIAN,IDAYYR,IDAYWK
  integer(i_kind) itype, ikx
  integer(i_kind) isnd, ilev, iflg, mflg
  integer(i_kind) inum,iyear,imonth,iday,ihour,iferror


  integer(i_kind) itx,itt,ipoq7

  real(r_kind) tdiff,sstime,slons,slats,dlon,dlat,t4dv,toq,poq,timedif,crit1,dist1
  real(r_kind) slons0,slats0,rsat,solzen,solzenp,dlat_earth,dlon_earth
  real(r_kind) rsec, ppmv, prec, pres, pob, obserr, usage
  real(r_kind) rlat,rlon,rpress,rsza
  real(r_kind),allocatable,dimension(:):: pco
  real(r_kind),allocatable,dimension(:):: apco
  real(r_kind),allocatable,dimension(:,:):: aker


! maximum number of observations set to
  real(r_kind),allocatable,dimension(:,:):: coout

  real(r_double),dimension(10):: hdrco
  real(r_double),dimension(10):: hdrcog
  real(r_double),dimension(5):: hdrcog2
  real(r_double),dimension(10):: hdrcoo
  real(r_double),dimension(8) :: hdrcoo2

  real(r_double) totco

  logical eof


! Set constants.  Initialize variables
  rsat=999._r_kind
  maxobs=1e6_i_kind
  ilon=3_i_kind
  ilat=4_i_kind
  ipoq7=0
  nreal=nlco*nlco+8_i_kind+nlco

  if (obstype == 'mopitt' )then 

!    Set dependent variables and allocate arrays
!     nchanl=nlco+1
     nchanl=nlco
     ncodat=nreal
     allocate (coout(ncodat+nchanl,maxobs))
     allocate (  pco(nlco))
     allocate(   apco(nlco))
     allocate(   aker(nlco,nlco))


! Read in observations from ascii file 

! Opening file for reading
  open(lunin,file=infile,form='formatted',iostat=iferror)
  lerror = (iferror/=0)

110 continue

! Read the first line of the data file
  if (.not.lerror) then 
     read(lunin,fmt=*,iostat=iferror) &
       inum,iyear,imonth,iday,ihour,imin,rlat,rlon,rpress,rsza
     if(iferror/=0) go to 150
     do i=1,nlco 
       read(lunin,fmt=*,iostat=iferror) (aker(i,j),j=1,nlco)
     enddo 
     read(lunin,fmt=*,iostat=iferror) (apco(j),j=1,nlco)
     read(lunin,fmt=*,iostat=iferror) (pco(j),j=1,nlco)
       
!    lerror=(iferror>0)
     leof  =(iferror<0)
     lmax  =.false.
  end if


     hdrco(2)=rlat
     hdrco(3)=rlon
     hdrco(4)=iyear
     hdrco(5)=imonth
     hdrco(6)=iday
     hdrco(8)=ihour
     hdrco(9)=imin

!    Convert observation location to radians
     slats0= hdrco(2)
     slons0= hdrco(3)
     if(abs(slats0)>90._r_kind .or. abs(slons0)>r360) go to 110
     if(slons0< zero) slons0=slons0+r360
     if(slons0>=r360) slons0=slons0-r360
     dlat_earth = slats0 * deg2rad
     dlon_earth = slons0 * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(outside) go to 110
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,1,rlats,nlat,1)
        call grdcrd(dlon,1,rlons,nlon,1)
     endif

!    Convert observation time to relative time
     idate5(1) = hdrco(4)  !year
     idate5(2) = hdrco(5)  !month
     idate5(3) = hdrco(6)  !day
     idate5(4) = hdrco(7)  !hour
     idate5(5) = hdrco(8)  !minute
     call w3fs21(idate5,nmind)
     t4dv=real((nmind-iwinbgn),r_kind)*r60inv
     sstime=real(nmind,r_kind)
     tdiff=(sstime-gstime)*r60inv
     

!    Check co layer values.  If any layer value is bad, toss entire profile
!     do k=1,nlco
!        if (pco(k)>badco) goto 110
!     end do
     
!    Write co record to output file
     ndata=min(ndata+1,maxobs)
     ndata=1 
     nodata=nlco
     
     coout(1,ndata)=rsat
     coout(2,ndata)=t4dv
     coout(3,ndata)=dlon               ! grid relative longitude
     coout(4,ndata)=dlat               ! grid relative latitude
     coout(5,ndata)=dlon_earth*rad2deg ! earth relative longitude (degrees)
     coout(6,ndata)=dlat_earth*rad2deg ! earth relative latitude (degrees)
     coout(7,ndata)=poq                ! profile co error flag
     coout(8,ndata)=solzen             ! solar zenith angle
     do k=1,nlco
       coout(k+8_i_kind,ndata)=apco(k)
     enddo 
     do i=1,nlco
       do j=1,nlco 
         coout(j+(i-1)*nlco+8_i_kind+nlco,ndata)=aker(i,j)
       enddo 
     enddo 
     do k=1,nlco
       coout(k+8_i_kind+nlco*nlco+nlco,ndata)=pco(k)
     end do

!    Loop back to read next profile
     goto 110

  endif

! Jump here when eof detected
150 continue


! Write header record and data to output file for further processing
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((coout(k,i),k=1,ncodat+nchanl),i=1,ndata)
  nread=10 


! Deallocate local arrays
160 continue
  deallocate(coout)
  if (obstype == 'mopitt') deallocate(pco)
  close(lunin)

  return
  
end subroutine read_co

