subroutine init_commvars(mype)
  use kinds, only: r_kind,i_kind
  use variables, only: displs_s,ird_s,istart,ltosj,&
       jstart,itotsub,iglobal,nsig,nlon,ltosi_s,ltosj_s,nlat,&
       ijn_s,irc_s,ijn,ilat1,jlon1,displs_g,&
       ltosi,isc_g,isd_g,npe,izero
  use comm_mod, only: nsig1o,irdsp_g,isdsp_g,ircnt_g,&
       iscnt_g,ircnt_s,isdsp_s,irdsp_s,iscnt_s,&
       spec_send,disp_spec
  use specgrid, only: ncin
  implicit none

  integer(i_kind) ns,mm1,mype
  integer(i_kind) i,j,n,kchk
  
  mm1=mype+1
  iglobal=nlat*nlon


! Set number of latitude and longitude for given subdomain
! Transfer/compute indices to global arrays.  Some of these
! arrays are used in the spectral to grid transforms
  do i=1,npe
    ijn(i)=ilat1(i)*jlon1(i)
    ijn_s(i)=(ilat1(i))*(jlon1(i))
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

  do n=1,npe
    do j=1,jlon1(n)
      ns=displs_s(n)+(j-1)*(ilat1(n))
      do i=1,ilat1(n)
        ns=ns+1
        ltosi_s(ns)=istart(n)+i-1
        ltosj_s(ns)=jstart(n)+j-1
      end do
    end do
  end do  ! end do over npe


! vertical column / horizontal slice communicator arrays
  isdsp_g(1)=izero
  irdsp_g(1)=izero
  isdsp_s(1)=izero
  irdsp_s(1)=izero

  if (mod((6*nsig)+1,npe)==izero) then
    kchk=npe
  else
    kchk=mod((nsig*6)+1,npe)
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

  do n=1,npe
    if (n.le.kchk) then
      spec_send(n) = ncin*nsig1o
    else
      spec_send(n) = ncin*(nsig1o-1)
    end if
  end do

  disp_spec(1)=izero
  do n=1,npe
    if(n/=1) then
      disp_spec(n)=disp_spec(n-1)+spec_send(n-1)
    end if
  end do

  return
end subroutine init_commvars
