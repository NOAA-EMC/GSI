subroutine horizsc(numcases,mype)
  use kinds, only: r_kind,i_kind
  use postmod, only: smoothlat
  use variables,only: nlat,nlon,nsig,lat1,lon1,zero,&
      displs_g,ijn,db_prec,filunit1,filunit2,npe,&
      sfhln,vphln,thln,qhln,ozhln,chln,pshln,&
      sfvar,vpvar,tvar,qvar,ozvar,cvar,psvar,istart,&
      iglobal,ltosi,ltosj,smoothdeg
  use comm_mod, only: sub2grid,grid2sub,nsig1o
  use specgrid, only: enn1,nc,jcap,sptez_s,load_grid,&
      factsml,ncd2,unload_grid
  implicit none
  include 'mpif.h'

  integer(i_kind),intent(in):: numcases,mype

  real(r_kind),dimension(lat1,lon1,nsig):: sf1,vp1,t1,rh1,oz1,cw1
  real(r_kind),dimension(lat1,lon1):: ps1
  real(r_kind),dimension(lat1,lon1,nsig):: sf2,vp2,t2,rh2,oz2,cw2
  real(r_kind),dimension(lat1,lon1):: ps2
  real(r_kind),dimension(lat1,lon1,nsig):: sf3,vp3,t3,rh3,oz3,cw3
  real(r_kind),dimension(lat1,lon1):: ps3

  real(r_kind),dimension(nlat,nsig):: sflap,vplap,tlap,rhlap,ozlap,cwlap
  real(r_kind),dimension(nlat):: pslap

  real(r_kind),dimension(nlat,nlon,nsig1o):: work
  real(r_kind),dimension(nlat,nlon):: workgrd
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(nlon,nlat-2):: grid
  real(r_kind),dimension(nc):: wrkspec
  real(r_kind) eight,quarter,r_norm

  integer(i_kind) i,j,k,m,n,ix,mpi_rtype,mm1,mype_work,ierror
  integer(i_kind) i2,i2m1,jj,j2,kk,ni1,ni2

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  mype_work=npe/2
  mm1=mype+1
  r_norm=1./float(numcases)
  eight=8.0_r_kind
  quarter=0.25_r_kind


  sflap=zero ; vplap=zero ; tlap=zero ; rhlap=zero ; ozlap=zero ; cwlap=zero ; pslap=zero
  sf3=zero ; vp3=zero ; t3=zero ; rh3=zero ; oz3=zero ; cw3=zero ; ps3=zero

  open(filunit1,form='unformatted',action='read')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='read')
  rewind(filunit2)

  do n=1,numcases
    if (mype==0)  write(6,*) 'HORIZSC, PROCESSING PAIR # ',n
! Read in subdomain grids
    read(filunit1) sf1,vp1,t1,rh1,oz1,cw1,ps1
    read(filunit2) sf2,vp2,t2,rh2,oz2,cw2,ps2

    call delvars(sf1,vp1,t1,rh1,oz1,cw1,ps1,sf2,vp2,t2,rh2,oz2,cw2,ps2,mype)

! Normalize by standard deviation
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          ix=istart(mype+1)+i-1
          sf1(i,j,k) = sf1(i,j,k)/sqrt(sfvar(ix,k))
          vp1(i,j,k) = vp1(i,j,k)/sqrt(vpvar(ix,k))
           t1(i,j,k) =  t1(i,j,k)/sqrt(tvar(ix,k))
          rh1(i,j,k) = rh1(i,j,k)/sqrt(qvar(ix,k))
          oz1(i,j,k) = oz1(i,j,k)/sqrt(ozvar(ix,k))
          cw1(i,j,k) = cw1(i,j,k)/sqrt(cvar(ix,k))
        end do
      end do
    end do
    do j=1,lon1
      do i=1,lat1
        ix=istart(mype+1)+i-1
        ps1(i,j) = ps1(i,j)/sqrt(psvar(ix))
      end do
    end do

! Place on evenly distrubuted horizontal slabs
    call sub2grid(work,sf1,vp1,t1,rh1,oz1,cw1,ps1)

! Loop over nsig1o levels
    do k=1,nsig1o

      do j=1,nlon
        do i=1,nlat
          workgrd(i,j)=work(i,j,k)
        end do
      end do

      call load_grid(workgrd,grid)
      wrkspec=zero
! Transform to spectral space
      call sptez_s(wrkspec,grid,-1)
! Take laplacian
      wrkspec(1)=0.
      wrkspec(2)=0.

      call splaplac(0,jcap,enn1,wrkspec,wrkspec,1)

      do i=1,ncd2
        i2=2*i; i2m1=i2-1
        wrkspec(i2)=factsml(i2)*wrkspec(i2)
        wrkspec(i2m1)=factsml(i2m1)*wrkspec(i2m1)
      end do

! Transform back to grid
      call sptez_s(wrkspec,grid,1)
      call unload_grid(grid,workgrd)

      do j=1,nlon
        do i=1,nlat
          work(i,j,k)=workgrd(i,j)
        end do
      end do
    end do  !end do nsig1o loop

! Transform work array back to subdomain
    call grid2sub(work,sf1,vp1,t1,rh1,oz1,cw1,ps1)

! Load into average laplacian arrays
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          sf3(i,j,k) = sf3(i,j,k) + sf1(i,j,k)*sf1(i,j,k)*r_norm
          vp3(i,j,k) = vp3(i,j,k) + vp1(i,j,k)*vp1(i,j,k)*r_norm
           t3(i,j,k) =  t3(i,j,k) +  t1(i,j,k)* t1(i,j,k)*r_norm
          rh3(i,j,k) = rh3(i,j,k) + rh1(i,j,k)*rh1(i,j,k)*r_norm
          oz3(i,j,k) = oz3(i,j,k) + oz1(i,j,k)*oz1(i,j,k)*r_norm
          cw3(i,j,k) = cw3(i,j,k) + cw1(i,j,k)*cw1(i,j,k)*r_norm
        end do
      end do
    end do
    do j=1,lon1
      do i=1,lat1
        ps3(i,j) = ps3(i,j) + ps1(i,j)*ps1(i,j)*r_norm
      end do
    end do           
  end do ! end do over numcases
  close(filunit1)
  close(filunit2)

! Convert to zonal mean quantities
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
          sflap(i,k) = sflap(i,k) + workgrd(i,j)/float(nlon)
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
          vplap(i,k) = vplap(i,k) + workgrd(i,j)/float(nlon)
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
          tlap(i,k) = tlap(i,k) + workgrd(i,j)/float(nlon)
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
          rhlap(i,k) = rhlap(i,k) + workgrd(i,j)/float(nlon)
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
          ozlap(i,k) = ozlap(i,k) + workgrd(i,j)/float(nlon)
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
          cwlap(i,k) = cwlap(i,k) + workgrd(i,j)/float(nlon)
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
        pslap(i) = pslap(i) + workgrd(i,j)/float(nlon)
      end do
    end do
  end if

  if (mype==mype_work) then
    do k=1,nsig
      do i=1,nlat
        sfhln(i,k)=(eight/sflap(i,k))**quarter
        vphln(i,k)=(eight/vplap(i,k))**quarter
        thln(i,k)=(eight/tlap(i,k))**quarter
        qhln(i,k)=(eight/rhlap(i,k))**quarter
        ozhln(i,k)=(eight/ozlap(i,k))**quarter
        chln(i,k)=(eight/cwlap(i,k))**quarter
      end do
    end do
    do i=1,nlat
      pshln(i)=(eight/pslap(i))**quarter
    end do

!! Put bounds on cloud water horizontal scales
    do k=1,nsig
      do i=1,nlat
        chln(i,k)=max(min(5.e5_r_kind,chln(i,k)),2.5e3_r_kind)
      end do
    end do

    call smoothlat(sfhln,nsig,smoothdeg)
    call smoothlat(vphln,nsig,smoothdeg)
    call smoothlat(thln,nsig,smoothdeg)
    call smoothlat(qhln,nsig,smoothdeg)
    call smoothlat(ozhln,nsig,smoothdeg)
    call smoothlat(chln,nsig,smoothdeg)
    call smoothlat(pshln,1,smoothdeg)
  end if ! end if mype_work

  call mpi_bcast(sfhln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(vphln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(thln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qhln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(ozhln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(chln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(pshln,nlat,mpi_rtype,mype_work,mpi_comm_world,ierror)

  return 
end subroutine horizsc
