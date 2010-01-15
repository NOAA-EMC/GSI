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
  use kinds, only: i_kind
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

  use constants, only: izero,ione
  implicit none

  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) ns,mm1
  integer(i_kind) i,j,n,kchk,npcount,icount,nstart
  integer(i_kind),dimension(0:npe-ione):: nbalpe,nbalpe_uv,kcount,lcount
  
  mm1=mype+ione


! Set number of latitude and longitude for given subdomain
! Transfer/compute indices to global arrays.  Some of these
! arrays are used in the spectral to grid transforms
  do i=1,npe
     ijn(i)=ilat1(i)*jlon1(i)
     ijn_s(i)=(ilat1(i)+2_i_kind)*(jlon1(i)+2_i_kind)
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
     if(n/=ione) then
        isd_g(n)=isd_g(n-ione)+isc_g(n-ione)
        displs_g(n)=displs_g(n-ione)+ijn(n-ione)
     end if
     do j=1,jlon1(n)
        ns=displs_g(n)+(j-ione)*ilat1(n)
        do i=1,ilat1(n)
           ns=ns+ione
           ltosi(ns)=istart(n)+i-ione
           ltosj(ns)=jstart(n)+j-ione
        end do
     end do
  end do

! Load arrays dealing with subdomain grids
  ird_s(1)=izero
  displs_s(1)=izero
  do n=1,npe
     if(n/=ione) then
        ird_s(n)=ird_s(n-ione)+irc_s(n-ione)
        displs_s(n)=displs_s(n-ione)+ijn_s(n-ione)
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
        do j=1,jlon1(n)+2_i_kind
           ns=displs_s(n)+(j-ione)*(ilat1(n)+2_i_kind)
           do i=1,ilat1(n)+2_i_kind
              ns=ns+ione
              ltosi_s(ns)=istart(n)+i-2_i_kind
              ltosj_s(ns)=jstart(n)+j-2_i_kind
              if(ltosi_s(ns)==izero) ltosi_s(ns)=ione
              if(ltosi_s(ns)==nlat+ione) ltosi_s(ns)=nlat
              if(ltosj_s(ns)==izero) ltosj_s(ns)=ione
              if(ltosj_s(ns)==nlon+ione) ltosj_s(ns)=nlon
           end do
        end do
     end do  ! end do over npe
  else
     do n=1,npe
        do j=1,jlon1(n)+2_i_kind
           ns=displs_s(n)+(j-ione)*(ilat1(n)+2_i_kind)
           do i=1,ilat1(n)+2_i_kind
              ns=ns+ione
              ltosi_s(ns)=istart(n)+i-2_i_kind
              ltosj_s(ns)=jstart(n)+j-2_i_kind
              if(ltosi_s(ns)==izero) ltosi_s(ns)=ione
              if(ltosi_s(ns)==nlat+ione) ltosi_s(ns)=nlat
              if(ltosj_s(ns)==izero) ltosj_s(ns)=nlon
              if(ltosj_s(ns)==nlon+ione) ltosj_s(ns)=ione
           end do
        end do
     end do  ! end do over npe
  endif


! vertical column / horizontal slice communicator arrays
  isdsp_g(1)=izero
  irdsp_g(1)=izero
  isdsp_s(1)=izero
  irdsp_s(1)=izero

  if (mod((6*nsig)+4_i_kind,npe)==izero) then
     kchk=npe
  else
     kchk=mod((nsig*6)+4_i_kind,npe)
  end if

  do n=1,npe
     if (n<=kchk) then
        iscnt_g(n)=ijn(mm1)*nsig1o
        ircnt_s(n)=ijn_s(mm1)*nsig1o
     else
        iscnt_g(n)=ijn(mm1)*(nsig1o-ione)
        ircnt_s(n)=ijn_s(mm1)*(nsig1o-ione)
     end if

     if (mm1<=kchk) then
        ircnt_g(n)=ijn(n)*nsig1o
        iscnt_s(n)=ijn_s(n)*nsig1o
     else
        ircnt_g(n)=ijn(n)*(nsig1o-ione)
        iscnt_s(n)=ijn_s(n)*(nsig1o-ione)
     end if

     if (n/=ione) then
        isdsp_g(n)=isdsp_g(n-ione)+iscnt_g(n-ione)
        irdsp_g(n)=irdsp_g(n-ione)+ijn(n-ione)*nsig1o
        isdsp_s(n)=isdsp_s(n-ione)+ijn_s(n-ione)*nsig1o
        irdsp_s(n)=irdsp_s(n-ione)+ircnt_s(n-ione)
     end if
  end do

! set up communications for balance stuff
  nbalpe=izero
  nbalpe_uv=izero
  icount=izero
! First take care of u,v and st,vp
  do n=1,nsig
     nbalpe(icount)=nbalpe(icount)+2_i_kind
     nbalpe_uv(icount)=nbalpe_uv(icount)+2_i_kind
     icount=icount+ione
     if(icount == npe)icount=izero
  end do
  nstart=icount
! Pressure (nsig+ione)
  do n=1,nsig+ione
     nbalpe(icount)=nbalpe(icount)+ione
     icount=icount+ione
     if(icount == npe)then
        icount=nstart
        nstart=izero
     end if
  end do
! Temperature (nsig)
  do n=1,nsig
     nbalpe(icount)=nbalpe(icount)+ione
     icount=icount+ione
     if(icount == npe)then
        icount=nstart
        nstart=izero
     end if
  end do
  nlevsbal=izero
  do n=0,npe-ione
     nlevsbal=max(nlevsbal,nbalpe(n))
  end do
  nnnvsbal=nbalpe(mype)
  nlevsuv=izero
  do n=0,npe-ione
     nlevsuv=max(nlevsuv,nbalpe_uv(n))
  end do
  nnnvsuv=nbalpe_uv(mype)
  allocate(nvarbal_id(nlevsbal))
  npcount=ione
  icount=izero
  kcount=izero
  lcount=izero
  do n=1,npe-ione
     kcount(n)=nbalpe(n-ione)+kcount(n-ione)
     lcount(n)=nbalpe_uv(n-ione)+lcount(n-ione)
  end do
! First take care of u,v and st,vp
  do n=1,nsig
     if(mype == icount)then
        nvarbal_id(npcount)=ione
        nvarbal_id(npcount+ione)=2_i_kind
        npcount=npcount+2_i_kind
     end if
     ku_gs(n)=kcount(icount)
     kv_gs(n)=kcount(icount)+ione
     lu_gs(n)=lcount(icount)
     lv_gs(n)=lcount(icount)+ione
     kcount(icount)=kcount(icount)+2_i_kind
     lcount(icount)=lcount(icount)+2_i_kind
     icount=icount+ione
     if(icount == npe)icount=izero
  end do
  nstart=icount
! Pressure (nsig+ione)
  do n=1,nsig+ione
     if(mype == icount)then
        nvarbal_id(npcount)=3_i_kind
        npcount=npcount+ione
     end if
     kp_gs(n)=kcount(icount)
     kcount(icount)=kcount(icount)+ione
     icount=icount+ione
     if(icount == npe)then
        icount=nstart
        nstart=izero
     end if
  end do
! Temperature (nsig)
  do n=1,nsig
     if(mype == icount)then
        nvarbal_id(npcount)=4_i_kind
        npcount=npcount+ione
     end if
     kt_gs(n)=kcount(icount)
     kcount(icount)=kcount(icount)+ione
     icount=icount+ione
     if(icount == npe)then
        icount=nstart
        nstart=izero
     end if
  end do

  isdbal_g(1)=izero
  irdbal_g(1)=izero
  isdbal_s(1)=izero
  irdbal_s(1)=izero
  do n=1,npe
!  sub to grid communications
     iscbal_g(n)=ijn(mm1)*nbalpe(n-ione)
     ircbal_g(n)=ijn(n)*nbalpe(mype)

!  grid to sub communications
     ircbal_s(n)=ijn_s(mm1)*nbalpe(n-ione)
     iscbal_s(n)=ijn_s(n)*nbalpe(mype)

     if (n/=ione) then
!  sub to grid
        isdbal_g(n)=isdbal_g(n-ione)+iscbal_g(n-ione)
        irdbal_g(n)=irdbal_g(n-ione)+ijn(n-ione)*nlevsbal
!  grid to sub
        isdbal_s(n)=isdbal_s(n-ione)+ijn_s(n-ione)*nlevsbal
        irdbal_s(n)=irdbal_s(n-ione)+ircbal_s(n-ione)
     end if
  end do

  isdvec_g(1)=izero
  irdvec_g(1)=izero
  isdvec_s(1)=izero
  irdvec_s(1)=izero
  do n=1,npe
!  sub to grid communications
     iscvec_g(n)=ijn(mm1)*nbalpe_uv(n-ione)
     ircvec_g(n)=ijn(n)*nbalpe_uv(mype)

!  grid to sub communications
     ircvec_s(n)=ijn_s(mm1)*nbalpe_uv(n-ione)
     iscvec_s(n)=ijn_s(n)*nbalpe_uv(mype)

     if (n/=ione) then
!  sub to grid
        isdvec_g(n)=isdvec_g(n-ione)+iscvec_g(n-ione)
        irdvec_g(n)=irdvec_g(n-ione)+ijn(n-ione)*nlevsuv
!  grid to sub
        isdvec_s(n)=isdvec_s(n-ione)+ijn_s(n-ione)*nlevsuv
        irdvec_s(n)=irdvec_s(n-ione)+ircvec_s(n-ione)
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
     if (n<=kchk) then
        iscuv_g(n)=ijn(mm1)*nuvlevs
        ircuv_s(n)=ijn_s(mm1)*nuvlevs
     else
        iscuv_g(n)=ijn(mm1)*(nuvlevs-ione)
        ircuv_s(n)=ijn_s(mm1)*(nuvlevs-ione)
     end if

     if (mm1<=kchk) then
        ircuv_g(n)=ijn(n)*nuvlevs
        iscuv_s(n)=ijn_s(n)*nuvlevs
     else
        ircuv_g(n)=ijn(n)*(nuvlevs-ione)
        iscuv_s(n)=ijn_s(n)*(nuvlevs-ione)
     end if

     if (n/=ione) then
        isduv_g(n)=isduv_g(n-ione)+iscuv_g(n-ione)
        irduv_g(n)=irduv_g(n-ione)+ijn(n-ione)*nuvlevs
        isduv_s(n)=isduv_s(n-ione)+ijn_s(n-ione)*nuvlevs
        irduv_s(n)=irduv_s(n-ione)+ircuv_s(n-ione)
     end if
  end do

  return
end subroutine init_commvars
