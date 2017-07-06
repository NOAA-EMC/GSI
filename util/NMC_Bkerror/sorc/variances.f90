subroutine variances(numcases,mype)
  use kinds, only: r_kind,r_single,i_kind
  use postmod, only: smoothlat
  use variables,only: nlat,nlon,nsig,lat1,lon1,filunit1,filunit2,zero,&
      displs_g,ijn,two,db_prec,istart,ilat1,jstart,npe,&
      bbiasz,bbiasd,bbiast,bcorrz,bcorrd,bcorrt,bbiasp,bcorrp,&
      sfvar,vpvar,tvar,qvar,ozvar,cvar,nrhvar,psvar,tcon,vpcon,pscon,&
      iglobal,ltosi,ltosj,half,one,ione,two,smoothdeg,vertavg
  implicit none
  include 'mpif.h'

  integer(i_kind),intent(in):: numcases,mype

  real(r_kind),dimension(lat1,lon1,nsig):: sf1,vp1,t1,rh1,oz1,cw1
  real(r_kind),dimension(lat1,lon1):: ps1,bal1
  real(r_kind),dimension(lat1,lon1,nsig):: sf2,vp2,t2,rh2,oz2,cw2
  real(r_kind),dimension(lat1,lon1):: ps2
  real(r_kind),dimension(lat1,lon1,nsig):: rhave

  real(r_kind),dimension(lat1,lon1,nsig):: sf3,vp3,t3,rh3,oz3,cw3
  real(r_kind),dimension(lat1,lon1):: ps3
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(nlat,nlon):: workgrd

! Normalized RH variance arrays
  real(r_kind),dimension(25,nsig):: qcount,qamp,qcount2
  real(r_kind),dimension(25):: qcavg
  real(r_kind) qctot,qdiff

  real(r_kind) r_norm,r025

  integer(i_kind) i,j,k,m,n,mype_work,mm1,ierror,iret
  integer(i_kind) mpi_rtype,ix,ni1,ni2,kk,ll,ibin

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  mype_work=npe-1
  mm1=mype+1
  r_norm=1./float(numcases)
  ibin=25
  r025=0.25_r_kind

! Initialize subdomain variance arrays
  sfvar=zero ; vpvar=zero ; tvar=zero ; qvar=zero ; ozvar=zero ; cvar=zero ; 
  nrhvar=zero ; psvar=zero
  sf3=zero ; vp3=zero ; t3=zero ; rh3=zero ; oz3=zero ; cw3=zero ; ps3=zero
  qcount=zero ; qamp=zero


  open(filunit1,form='unformatted',action='read')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='read')
  rewind(filunit2)

  do n=1,numcases
    if (mype==0)  write(6,*) 'VARIANCES, PROCESSING PAIR # ',n
! Read in subdomain grids
    read(filunit1) sf1,vp1,t1,rh1,oz1,cw1,ps1
    read(filunit2) sf2,vp2,t2,rh2,oz2,cw2,ps2

    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          rhave(i,j,k) = half*(rh1(i,j,k) + rh2(i,j,k))
        end do
      end do
    end do

    call delvars(sf1,vp1,t1,rh1,oz1,cw1,ps1,sf2,vp2,t2,rh2,oz2,cw2,ps2,mype)

    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          sf3(i,j,k) = sf3(i,j,k) + (sf1(i,j,k)*sf1(i,j,k))*r_norm
          vp3(i,j,k) = vp3(i,j,k) + (vp1(i,j,k)*vp1(i,j,k))*r_norm
           t3(i,j,k) =  t3(i,j,k) + ( t1(i,j,k)* t1(i,j,k))*r_norm
          rh3(i,j,k) = rh3(i,j,k) + (rh1(i,j,k)*rh1(i,j,k))*r_norm
          oz3(i,j,k) = oz3(i,j,k) + (oz1(i,j,k)*oz1(i,j,k))*r_norm
          cw3(i,j,k) = cw3(i,j,k) + (cw1(i,j,k)*cw1(i,j,k))*r_norm
        end do
      end do
    end do
    do j=1,lon1
      do i=1,lat1
        ps3(i,j) = ps3(i,j) + (ps1(i,j)*ps1(i,j))*r_norm
      end do
    end do

! Normalized RH calculations
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          ll=rhave(i,j,k)*20.0+1
          ll=min0(max0(1,ll),21)

          qdiff=min(one,abs(rh1(i,j,k)))**two
          qamp(ll,k)=qamp(ll,k)+qdiff
          qcount(ll,k)=qcount(ll,k)+ione
        end do
      end do
    end do
    do k=1,nsig
      do i=22,ibin
        qamp(i,k)=qamp(21,k)
        qcount(i,k)=qcount(21,k)
      end do
    end do

  end do ! end do over ncases
  close(filunit1)
  close(filunit2)

  do k=1,nsig
    call mpi_gatherv(sf3(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          sfvar(i,k) = sfvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(vp3(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          vpvar(i,k) = vpvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(t3(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          tvar(i,k) = tvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(rh3(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          qvar(i,k) = qvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(oz3(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          ozvar(i,k) = ozvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(cw3(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          cvar(i,k) = cvar(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  call mpi_gatherv(ps3,ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
  if (mype==mype_work) then
    do kk=1,iglobal
      ni1=ltosi(kk); ni2=ltosj(kk)
      workgrd(ni1,ni2)=work1(kk)
    end do
    do i=1,nlat
      do j=1,nlon
        psvar(i) = psvar(i) + workgrd(i,j)/float(nlon)
      end do
    end do
  end if

  call mpi_reduce(qamp,qamp,ibin*nsig,mpi_rtype,mpi_sum,mype_work, &
       mpi_comm_world,ierror)
  call mpi_reduce(qcount,qcount,ibin*nsig,mpi_rtype,mpi_sum,mype_work, &
       mpi_comm_world,ierror)

  if (mype==mype_work) then
    qcavg=zero
    qctot=zero
    do ll=1,ibin
      qctot=qctot+qcount(ll,1)
    end do
    qcount2=qcount
    do k=1,nsig
      do ll=1,ibin
        nrhvar(ll,k)=qamp(ll,k)
      end do
    end do
    do k=1,nsig
      do ll=1,ibin
        do kk=1,ibin
        if(qcount2(ll,k) < 2000._r_kind) then
          if(ll+kk <= ibin)then
              qcount2(ll,k)=qcount2(ll,k)+qcount2(ll+kk,k)
              nrhvar(ll,k)=nrhvar(ll,k)+nrhvar(ll+kk,k)
          end if
          if(ll-kk >= 1) then
              qcount2(ll,k)=qcount2(ll,k)+qcount2(ll-kk,k)
              nrhvar(ll,k)=nrhvar(ll,k)+nrhvar(ll-kk,k)
          end if
        end if
        end do
      end do
    end do
    do k=1,nsig
      do ll=1,ibin
        nrhvar(ll,k)=nrhvar(ll,k)/qcount2(ll,k)
        if(qcount(ll,k) > zero) then
          qamp(ll,k)=qamp(ll,k)/qcount(ll,k)
          qcavg(ll)=qcavg(ll)+qcount(ll,k)/float(nsig)
        end if
      end do
    end do
    write(6,*) qctot
    do k=1,nsig
      write(6,*) k,(qcount(ll,k),ll=1,ibin)
      write(6,*) k,(qcount2(ll,k),ll=1,ibin)
      write(6,*) k,(qcount2(ll,k)/qctot,ll=1,ibin)
      write(6,*) k,(qamp(ll,k),ll=1,ibin)
      write(6,*) k,(nrhvar(ll,k),ll=1,ibin)
    end do
  end if

  if (mype==mype_work) then
    call smoothlat(sfvar,nsig,smoothdeg)
    call smoothlat(vpvar,nsig,smoothdeg)
    call smoothlat(tvar,nsig,smoothdeg)
    call smoothlat(qvar,nsig,smoothdeg)
    call smoothlat(ozvar,nsig,smoothdeg)
    call smoothlat(cvar,nsig,smoothdeg)
    call smoothlat(psvar,1,smoothdeg)
  end if

  call mpi_bcast(sfvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(vpvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(tvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(nrhvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(ozvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(cvar,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(psvar,nlat,mpi_rtype,mype_work,mpi_comm_world,ierror)


  return
end subroutine variances


