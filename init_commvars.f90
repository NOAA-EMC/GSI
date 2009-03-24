subroutine init_commvars(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_commvars                                   
!   prgmmr: kleist           org: np20                date: 2004-01-25
!
! abstract: set up the variables which are utilized in communications
!           between tasks when going between vertical columns on the 
!           subdomains to horizonal slabs (any number of levs and type
!           of variables) on the global domain (nlat x nlon)
!
! program history log:
!   2004-01-25  kleist
!   2004-05-15  kleist, documentation
!   2008-06-02  safford - rm unused vars
!
!   input argument list:
!     mype     - task id
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: displs_s,ird_s,istart,ltosj,&
       jstart,regional,itotsub,nsig,nsig1o,nlon,ltosi_s,ltosj_s,nlat,&
       ijn_s,irc_s,ijn,ilat1,jlon1,displs_g,&
       ltosi,isc_g,isd_g
  use mpimod, only: npe,isduv_s,irduv_s,isduv_g,irduv_g,iscuv_g,&
       ircuv_g,iscuv_s,nuvlevs,ircuv_s,irdsp_g,isdsp_g,ircnt_g,&
       iscnt_g,ircnt_s,isdsp_s,irdsp_s,iscnt_s

  use constants, only: izero
  implicit none

  integer(i_kind) ns,mm1,mype
  integer(i_kind) i,j,n,kchk
  
  mm1=mype+1


! Set number of latitude and longitude for given subdomain
! Transfer/compute indices to global arrays.  Some of these
! arrays are used in the spectral to grid transforms
  do i=1,npe
    ijn(i)=ilat1(i)*jlon1(i)
    ijn_s(i)=(ilat1(i)+2)*(jlon1(i)+2)
  end do
 
  do i=1,npe
    irc_s(i)=ijn_s(mm1)
    isc_g(i)=ijn(mm1)
  end do
  allocate(ltosi(nlat*nlon),ltosj(nlat*nlon))
  do i=1,nlat*nlon
    ltosi(i)=izero
    ltosj(i)=izero
  end do

! Load arrays dealing with global grids
  isd_g(1)=izero
  displs_g(1)=izero
  do n=1,npe
    if(n/=1) then
      isd_g(n)=isd_g(n-1)+isc_g(n-1)
      displs_g(n)=displs_g(n-1)+ijn(n-1)
    end if
    do j=1,jlon1(n)
      ns=displs_g(n)+(j-1)*ilat1(n)
      do i=1,ilat1(n)
        ns=ns+1
        ltosi(ns)=istart(n)+i-1
        ltosj(ns)=jstart(n)+j-1
      end do
    end do
  end do

! Load arrays dealing with subdomain grids
  ird_s(1)=izero
  displs_s(1)=izero
  do n=1,npe
    if(n/=1) then
      ird_s(n)=ird_s(n-1)+irc_s(n-1)
      displs_s(n)=displs_s(n-1)+ijn_s(n-1)
    end if
  end do
! set total number of points from all subdomain grids
  itotsub=displs_s(npe)+ijn_s(npe)     
  allocate(ltosi_s(itotsub),ltosj_s(itotsub))

  do i=1,itotsub
    ltosi_s(i)=izero
    ltosj_s(i)=izero
  end do

if(regional)then

  do n=1,npe
    do j=1,jlon1(n)+2
      ns=displs_s(n)+(j-1)*(ilat1(n)+2)
      do i=1,ilat1(n)+2
        ns=ns+1
        ltosi_s(ns)=istart(n)+i-2
        ltosj_s(ns)=jstart(n)+j-2
        if(ltosi_s(ns)==0) ltosi_s(ns)=1
        if(ltosi_s(ns)==nlat+1) ltosi_s(ns)=nlat
        if(ltosj_s(ns)==0) ltosj_s(ns)=1   
        if(ltosj_s(ns)==nlon+1) ltosj_s(ns)=nlon
      end do
    end do
  end do  ! end do over npe
else
  do n=1,npe
    do j=1,jlon1(n)+2
      ns=displs_s(n)+(j-1)*(ilat1(n)+2)
      do i=1,ilat1(n)+2
        ns=ns+1
        ltosi_s(ns)=istart(n)+i-2
        ltosj_s(ns)=jstart(n)+j-2
        if(ltosi_s(ns)==0) ltosi_s(ns)=1
        if(ltosi_s(ns)==nlat+1) ltosi_s(ns)=nlat
        if(ltosj_s(ns)==0) ltosj_s(ns)=nlon
        if(ltosj_s(ns)==nlon+1) ltosj_s(ns)=1
      end do
    end do
  end do  ! end do over npe
endif


! vertical column / horizontal slice communicator arrays
  isdsp_g(1)=izero
  irdsp_g(1)=izero
  isdsp_s(1)=izero
  irdsp_s(1)=izero

  if (mod((6*nsig)+4,npe)==izero) then
    kchk=npe
  else
    kchk=mod((nsig*6)+4,npe)
  end if

  do n=1,npe
    if (n.le.kchk) then
      iscnt_g(n)=ijn(mm1)*nsig1o
      ircnt_s(n)=ijn_s(mm1)*nsig1o
    else
      iscnt_g(n)=ijn(mm1)*(nsig1o-1)
      ircnt_s(n)=ijn_s(mm1)*(nsig1o-1)
    end if

    if (mm1.le.kchk) then
      ircnt_g(n)=ijn(n)*nsig1o
      iscnt_s(n)=ijn_s(n)*nsig1o
    else
      ircnt_g(n)=ijn(n)*(nsig1o-1)
      iscnt_s(n)=ijn_s(n)*(nsig1o-1)
    end if

    if (n/=1) then
      isdsp_g(n)=isdsp_g(n-1)+iscnt_g(n-1)
      irdsp_g(n)=irdsp_g(n-1)+ijn(n-1)*nsig1o
      isdsp_s(n)=isdsp_s(n-1)+ijn_s(n-1)*nsig1o
      irdsp_s(n)=irdsp_s(n-1)+ircnt_s(n-1)
    end if
  end do

! set up communications to communicate u,v
  isduv_g(1)=izero
  irduv_g(1)=izero
  isduv_s(1)=izero
  irduv_s(1)=izero

! redefine kchk for uv/stvp distribution
  if (mod(nsig,npe)==izero) then
    kchk=npe
  else
    kchk=mod(nsig,npe)
  end if

  do n=1,npe
    if (n.le.kchk) then
      iscuv_g(n)=ijn(mm1)*nuvlevs
      ircuv_s(n)=ijn_s(mm1)*nuvlevs
    else
      iscuv_g(n)=ijn(mm1)*(nuvlevs-1)
      ircuv_s(n)=ijn_s(mm1)*(nuvlevs-1)
    end if

    if (mm1.le.kchk) then
      ircuv_g(n)=ijn(n)*nuvlevs
      iscuv_s(n)=ijn_s(n)*nuvlevs
    else
      ircuv_g(n)=ijn(n)*(nuvlevs-1)
      iscuv_s(n)=ijn_s(n)*(nuvlevs-1)
    end if

    if (n/=1) then
      isduv_g(n)=isduv_g(n-1)+iscuv_g(n-1)
      irduv_g(n)=irduv_g(n-1)+ijn(n-1)*nuvlevs
      isduv_s(n)=isduv_s(n-1)+ijn_s(n-1)*nuvlevs
      irduv_s(n)=irduv_s(n-1)+ircuv_s(n-1)
    end if
  end do

  return
end subroutine init_commvars
