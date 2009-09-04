subroutine deter_subdomain(mype)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_subdomain          perform domain decomposition
!   prgmmr: da silva       org: np20                date: 2006-06-28
!
! abstract: The nxPE and nyPE defines the layout, that is, nxPE is the number of
!           processors used to decompose the longitudinal dimensional, while nyPE 
!           the number of processors used to decompose the latitudinal dimension.
!           By construction, nPE = nxPE * nyPE. If a layout is not specified in
!           the namelist, it defaults to nxPE=nyPE=-1 and we revert back to
!           NCEP's original decomposition.
!
! program history log:
!   2006-06-28  da Silva - added option to perform an ESMF-like
!                          domain decomposition based on a layout.
!                          If no layout is defined in mpimod then
!                          it reverts back to NCEP's original algorithm.
!
!   input argument list:
!     mype      - mpi task number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpimod, only: nxPE, nyPE
  implicit none
  integer,intent(in):: mype

! If a layout is provided, use it for the domain decomposition
! ------------------------------------------------------------
  if ( nxPE > 0 .AND. nyPE > 0 ) then

       call deter_subdomain_withLayout ( myPE, nxPE, nyPE ) ! ESMF-like

! Otherwise, use NCEP original algorithm
! --------------------------------------
  else

       call deter_subdomain_noLayout ( mype ) ! NCEP's original algorithm

  endif

end subroutine deter_subdomain

subroutine deter_subdomain_noLayout(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_subdomain_noLayout   perform domain decomposition
!   prgmmr: weiyu yang       org: np20                date: 1998-05-14
!
! abstract: Given an array of the observation computation load and
!           the number of available mpi tasks (npe), this routine 
!           decomposes the total analysis grid into npe subdomains
!
! program history log:
!   1998-05-14  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-01  treadon - simplify algorithm
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2005-10-17  derber - rewrite routine using simpler algorithm
!   2005-10-26  treadon - correct error in 100 format text
!   2008-06-04  safford - rm unused vars
!   2008-09-05  lueken - merged ed's changes into q1fy09 code
!
!   input argument list:
!     mype      - mpi task number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: periodic,periodic_s,lon1,lat1,nlon,nlat,&
       ilat1,istart,jlon1,jstart
  use mpimod, only: npe
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) npts,nrnc,iinum,iileft,jrows,jleft,k,i,jjnum
  integer(i_kind) j,mm1,iicnt,ipts,jjleft
  integer(i_kind),dimension(npe+1):: iiend,jjend,iistart
  real(r_kind):: anperpe

!************************************************************************
! Compute number of points on full grid and target number of
! point per mpi task (pe)
  npts=nlat*nlon
  anperpe=float(npts)/float(npe)

! Start with square subdomains
  nrnc=sqrt(anperpe)
  iinum=nlon/nrnc
  if(iinum==0) iinum=1
  iicnt=nlon/iinum
  iileft=nlon-iicnt*iinum
  jrows=npe/iinum
  jleft=npe-jrows*iinum

! Adjust subdomain boundaries
  k=0
  istart=1
  jstart=1
  iistart(1)=1
  do i=1,iinum
     ipts = iicnt
     if(i <= iileft)ipts=ipts+1
     iiend(i)=iistart(i)+ipts-1
     iistart(i+1)=iiend(i)+1
     jjnum=jrows
     if(i <= jleft)jjnum=jrows+1
     do j=1,jjnum
        k=k+1
        jlon1(k)=ipts
        jstart(k)= iistart(i)
        ilat1(k)=nlat/jjnum
        jjleft=nlat-ilat1(k)*jjnum
        if(j <= jjleft)ilat1(k)=ilat1(k)+1
        if(j > 1)istart(k)=jjend(j-1)+1
        jjend(j)=istart(k)+ilat1(k)-1

        if (jlon1(k)==nlon) then
           periodic=.true.
           periodic_s(k)=.true.
        endif
        if(mype == 0) &
             write(6,100) k-1,istart(k),jstart(k),ilat1(k),jlon1(k)
     end do
  end do
100 format('DETER_SUBDOMAIN:  task,istart,jstart,ilat1,jlon1=',6(i6,1x))


! Set number of latitude and longitude for given subdomain
  mm1=mype+1
  lat1=ilat1(mm1)
  lon1=jlon1(mm1)
  
  return

end subroutine deter_subdomain_noLayout

!-------------------------------------------------------------------------
!BOP

  subroutine deter_subdomain_withLayout(mype,nxpe,nype)

! !USES:

  use kinds, only: i_kind
  use gridmod, only: lon1,lat1,nlon,nlat,&
       ilat1,istart,jlon1,jstart

  implicit none

! !INPUT PARAMETERS:

  integer(i_kind),intent(in) :: mype,nxpe,nype

! !OUTPUT PARAMETERS:

  ! all the variables in "use gridmod" are defined here

! !DESCRIPTION: determine GSI subdomains using a layout
!
! !REVISION HISTORY:
!
!   2006-06-27  cruz
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!    cruz           org: gmao                date: 2006-06-27
!
!EOP
!-------------------------------------------------------------------------

! Declare local variables

  integer(i_kind) i,j,k,iinum,jjnum,iistart,jjstart
  integer(i_kind) lsetx,lsety,nxseg,nyseg
  integer(i_kind),allocatable,dimension(:) :: imxy, jmxy
  integer(i_kind) im,jm,npe,mm1,ierr

! start

  im=nlon; jm=nlat
  npe=nxpe*nype
  allocate(imxy(0:nxpe-1),jmxy(0:nype-1), stat=ierr)
  if(ierr /= 0) then
    write(6,*)' DETER_SUBDOMAIN: ALLOCATE ERROR.'
    call stop2(30)
  end if
 
  call GET_LOCAL_DIMS ( im,imxy,nxpe )
  call GET_LOCAL_DIMS ( jm,jmxy,nype )

! compute subdomain boundaries  (axis indices)

  k=0
  iinum=imxy(0)
  jjnum=jmxy(0)
  nxseg=2
  nyseg=2
  istart=1
  jstart=1
  iistart=1
  jjstart=1
  lsetx=npe/nype
  lsety=npe/nype
  do j=0,nype-1
     do i=0,nxpe-1
       k=k+1
       if(i>0) then
         if(imxy(i)<imxy(i-1)) iinum = imxy(i)
       end if
       if(j>0) then
         if(jmxy(j)<jmxy(j-1)) jjnum = jmxy(j)
       end if
       ilat1(k)=jjnum
       jlon1(k)=iinum
       if(k>1) then
         if(nxseg<=lsetx) then
           jstart(k)=iistart+jlon1(k)
           iistart=jstart(k)
           nxseg=nxseg+1
         else
           jstart(k)=1
           iistart=1
           nxseg=2
         end if
         if(nyseg<=lsety) then
           istart(k)=jjstart
           nyseg=nyseg+1
         else
           if(ilat1(k)<ilat1(k-1)) then
             istart(k)=jjstart+ilat1(k)+1
           else
             istart(k)=jjstart+ilat1(k)
           end if
           jjstart=istart(k)
           nyseg=2
         end if
       end if
       if(mype == 0) &
            write(6,100) k,istart(k),jstart(k),ilat1(k),jlon1(k)
     end do
  end do

100 format('DETER_SUBDOMAIN:  task,istart,jstart,ilat1,jlon1=',5(i6,1x))
  
        
! Set number of latitude and longitude for given subdomain
  mm1=mype+1
  lat1=ilat1(mm1)
  lon1=jlon1(mm1)

  deallocate(imxy,jmxy, stat=ierr)
  if(ierr /= 0) then
    write(6,*)' DETER_SUBDOMAIN: DEALLOCATE ERROR.'
    call stop2(30)
  end if 


  return

  CONTAINS

  subroutine GET_LOCAL_DIMS ( dim_world,dim,NDEs )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    GET_LOCAL_DIMS
!   prgmmr:                  org                      date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    dim_world
!    NDEs
!    dim
!
!   output argument list:
!    dim
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit   none
  integer(i_kind),intent(in)::    dim_world, NDEs
  integer(i_kind),intent(inout):: dim(0:NDEs-1)
  integer(i_kind)    n,im,rm
  im = dim_world/NDEs
  rm = dim_world-NDEs*im
  do n=0,NDEs-1
                    dim(n) = im
    if( n.le.rm-1 ) dim(n) = im+1
  enddo
  end subroutine GET_LOCAL_DIMS

  end subroutine deter_subdomain_withLayout
