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
!   2009-04-21  derber - add communications for strong balance constraint (bal)
!                        and unified uv (vec) transformation
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
       iscnt_g,ircnt_s,isdsp_s,irdsp_s,iscnt_s,irdbal_g,isdbal_g,ircbal_g, &
       iscbal_g,ircbal_s,isdbal_s,irdbal_s,iscbal_s,nlevsbal,nvarbal_id, &
       ku_gs,kv_gs,kt_gs,kp_gs,nnnvsbal,irdvec_g,isdvec_g,ircvec_g, &
       iscvec_g,ircvec_s,isdvec_s,irdvec_s,iscvec_s,nlevsuv,nnnvsuv,lu_gs,lv_gs

  use constants, only: izero
  implicit none

  integer(i_kind) ns,mm1,mype
  integer(i_kind) i,j,n,kchk,npcount,icount,nstart
  integer(i_kind),dimension(0:npe-1):: nbalpe,nbalpe_uv,kcount,lcount
  
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

! set up communications for balance stuff
  nbalpe=0
  nbalpe_uv=0
  icount=0
! First take care of u,v and st,vp
  do n=1,nsig
    nbalpe(icount)=nbalpe(icount)+2
    nbalpe_uv(icount)=nbalpe_uv(icount)+2
    icount=icount+1
    if(icount == npe)icount=0
  end do
  nstart=icount
! Pressure (nsig+1)
  do n=1,nsig+1
    nbalpe(icount)=nbalpe(icount)+1
    icount=icount+1
    if(icount == npe)then
      icount=nstart
      nstart=0
    end if
  end do
! Temperature (nsig)
  do n=1,nsig
    nbalpe(icount)=nbalpe(icount)+1
    icount=icount+1
    if(icount == npe)then
      icount=nstart
      nstart=0
    end if
  end do
  nlevsbal=0
  do n=0,npe-1
    nlevsbal=max(nlevsbal,nbalpe(n))
  end do
  nnnvsbal=nbalpe(mype)
  nlevsuv=0
  do n=0,npe-1
    nlevsuv=max(nlevsuv,nbalpe_uv(n))
  end do
  nnnvsuv=nbalpe_uv(mype)
  allocate(nvarbal_id(nlevsbal))
  npcount=1
  icount=0
  kcount=0
  lcount=0
  do n=1,npe-1
   kcount(n)=nbalpe(n-1)+kcount(n-1)
   lcount(n)=nbalpe_uv(n-1)+lcount(n-1)
  end do
! First take care of u,v and st,vp
  do n=1,nsig
    if(mype == icount)then
      nvarbal_id(npcount)=1
      nvarbal_id(npcount+1)=2
      npcount=npcount+2
    end if
    ku_gs(n)=kcount(icount)
    kv_gs(n)=kcount(icount)+1
    lu_gs(n)=lcount(icount)
    lv_gs(n)=lcount(icount)+1
    kcount(icount)=kcount(icount)+2
    lcount(icount)=lcount(icount)+2
    icount=icount+1
    if(icount == npe)icount=0
  end do
  nstart=icount
! Pressure (nsig+1)
  do n=1,nsig+1
    if(mype == icount)then
      nvarbal_id(npcount)=3
      npcount=npcount+1
    end if
    kp_gs(n)=kcount(icount)
    kcount(icount)=kcount(icount)+1
    icount=icount+1
    if(icount == npe)then
      icount=nstart
      nstart=0
    end if
  end do
! Temperature (nsig)
  do n=1,nsig
    if(mype == icount)then
      nvarbal_id(npcount)=4
      npcount=npcount+1
    end if
    kt_gs(n)=kcount(icount)
    kcount(icount)=kcount(icount)+1
    icount=icount+1
    if(icount == npe)then
      icount=nstart
      nstart=0
    end if
  end do

  isdbal_g(1)=izero
  irdbal_g(1)=izero
  isdbal_s(1)=izero
  irdbal_s(1)=izero
  do n=1,npe
!  sub to grid communications
      iscbal_g(n)=ijn(mm1)*nbalpe(n-1)
      ircbal_g(n)=ijn(n)*nbalpe(mype)

!  grid to sub communications
      ircbal_s(n)=ijn_s(mm1)*nbalpe(n-1)
      iscbal_s(n)=ijn_s(n)*nbalpe(mype)

    if (n/=1) then
!  sub to grid
      isdbal_g(n)=isdbal_g(n-1)+iscbal_g(n-1)
      irdbal_g(n)=irdbal_g(n-1)+ijn(n-1)*nlevsbal
!  grid to sub
      isdbal_s(n)=isdbal_s(n-1)+ijn_s(n-1)*nlevsbal
      irdbal_s(n)=irdbal_s(n-1)+ircbal_s(n-1)
    end if
  end do

  isdvec_g(1)=izero
  irdvec_g(1)=izero
  isdvec_s(1)=izero
  irdvec_s(1)=izero
  do n=1,npe
!  sub to grid communications
      iscvec_g(n)=ijn(mm1)*nbalpe_uv(n-1)
      ircvec_g(n)=ijn(n)*nbalpe_uv(mype)

!  grid to sub communications
      ircvec_s(n)=ijn_s(mm1)*nbalpe_uv(n-1)
      iscvec_s(n)=ijn_s(n)*nbalpe_uv(mype)

    if (n/=1) then
!  sub to grid
      isdvec_g(n)=isdvec_g(n-1)+iscvec_g(n-1)
      irdvec_g(n)=irdvec_g(n-1)+ijn(n-1)*nlevsuv
!  grid to sub
      isdvec_s(n)=isdvec_s(n-1)+ijn_s(n-1)*nlevsuv
      irdvec_s(n)=irdvec_s(n-1)+ircvec_s(n-1)
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
