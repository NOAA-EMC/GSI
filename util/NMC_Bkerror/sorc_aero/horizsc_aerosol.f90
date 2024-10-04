subroutine horizsc_aerosol(numcases,mype)
  use kinds, only: r_kind,i_kind
  use postmod, only: smoothlat
  use variables,only: nlat,nlon,nsig,lat1,lon1,zero,&
      displs_g,ijn,db_prec,filunit1,filunit2,npe,&
      d1hln,d2hln,d3hln,d4hln,d5hln,s1hln,s2hln,s3hln,s4hln, &
      so4hln,oc1hln,oc2hln,bc1hln,bc2hln, &
      d1var,d2var,d3var,d4var,d5var,s1var,s2var,s3var,s4var, &
      so4var,oc1var,oc2var,bc1var,bc2var, &
      istart,iglobal,ltosi,ltosj,smoothdeg
  use comm_mod, only: sub2grid,grid2sub,nsig1o
  use specgrid, only: enn1,nc,jcap,sptez_s,load_grid,&
      factsml,ncd2,unload_grid
  implicit none
  include 'mpif.h'

  integer(i_kind),intent(in):: numcases,mype

  real(r_kind),dimension(lat1,lon1,nsig):: d1a,d2a,d3a,d4a,d5a, &
      s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a
  real(r_kind),dimension(lat1,lon1,nsig):: d1b,d2b,d3b,d4b,d5b, &
      s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b 
  real(r_kind),dimension(lat1,lon1,nsig):: d1c,d2c,d3c,d4c,d5c, &
      s1c,s2c,s3c,s4c,so4c,oc1c,oc2c,bc1c,bc2c

  real(r_kind),dimension(nlat,nsig):: d1lap,d2lap,d3lap,d4lap,d5lap, &
      s1lap,s2lap,s3lap,s4lap,so4lap,oc1lap,oc2lap,bc1lap,bc2lap
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

  d1c=zero ; d2c=zero ; d3c=zero ; d4c=zero ; d5c=zero ; 
  s1c=zero ; s2c=zero ; s3c=zero ; s4c=zero ; so4c=zero ; 
  oc1c=zero ; oc2c=zero ; bc1c=zero ; bc2c=zero

  d1lap=zero ; d2lap=zero ; d3lap=zero ; d4lap=zero ; d5lap=zero ; 
  s1lap=zero ; s2lap=zero ; s3lap=zero ; s4lap=zero ; so4lap=zero ; 
  oc1lap=zero ; oc2lap=zero ; bc1lap=zero ; bc2lap=zero

  open(filunit1,form='unformatted',action='read')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='read')
  rewind(filunit2)

  do n=1,numcases
    if (mype==0)  write(6,*) 'HORIZSC, PROCESSING PAIR # ',n
! Read in subdomain grids
    read(filunit1) d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a
    read(filunit2) d1b,d2b,d3b,d4b,d5b,s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b

    call delvars_aerosol(d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a, &
                         d1b,d2b,d3b,d4b,d5b,s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b,mype)

! Normalize by standard deviation
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          ix=istart(mype+1)+i-1
          d1a(i,j,k) = d1a(i,j,k)/sqrt(d1var(ix,k))
          d2a(i,j,k) = d2a(i,j,k)/sqrt(d2var(ix,k))
          d3a(i,j,k) = d3a(i,j,k)/sqrt(d3var(ix,k))
          d4a(i,j,k) = d4a(i,j,k)/sqrt(d4var(ix,k))
          d5a(i,j,k) = d5a(i,j,k)/sqrt(d5var(ix,k))
          s1a(i,j,k) = s1a(i,j,k)/sqrt(s1var(ix,k))
          s2a(i,j,k) = s2a(i,j,k)/sqrt(s2var(ix,k))
          s3a(i,j,k) = s3a(i,j,k)/sqrt(s3var(ix,k))
          s4a(i,j,k) = s4a(i,j,k)/sqrt(s4var(ix,k))
          so4a(i,j,k) = so4a(i,j,k)/sqrt(so4var(ix,k))
          oc1a(i,j,k) = oc1a(i,j,k)/sqrt(oc1var(ix,k))
          oc2a(i,j,k) = oc2a(i,j,k)/sqrt(oc2var(ix,k))
          bc1a(i,j,k) = bc1a(i,j,k)/sqrt(bc1var(ix,k))
          bc2a(i,j,k) = bc2a(i,j,k)/sqrt(bc2var(ix,k))
        end do
      end do
    end do

! Place on evenly distrubuted horizontal slabs
    call sub2grid(work,d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a)

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
    call grid2sub(work,d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a)

! Load into average laplacian arrays
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          d1c(i,j,k) = d1c(i,j,k) + d1a(i,j,k)*d1a(i,j,k)*r_norm
          d2c(i,j,k) = d2c(i,j,k) + d2a(i,j,k)*d2a(i,j,k)*r_norm
          d3c(i,j,k) = d3c(i,j,k) + d3a(i,j,k)*d3a(i,j,k)*r_norm
          d4c(i,j,k) = d4c(i,j,k) + d4a(i,j,k)*d4a(i,j,k)*r_norm
          d5c(i,j,k) = d5c(i,j,k) + d5a(i,j,k)*d5a(i,j,k)*r_norm
          s1c(i,j,k) = s1c(i,j,k) + s1a(i,j,k)*s1a(i,j,k)*r_norm
          s2c(i,j,k) = s2c(i,j,k) + s2a(i,j,k)*s2a(i,j,k)*r_norm
          s3c(i,j,k) = s3c(i,j,k) + s3a(i,j,k)*s3a(i,j,k)*r_norm
          s4c(i,j,k) = s4c(i,j,k) + s4a(i,j,k)*s4a(i,j,k)*r_norm
          so4c(i,j,k) = so4c(i,j,k) + so4a(i,j,k)*so4a(i,j,k)*r_norm
          oc1c(i,j,k) = oc1c(i,j,k) + oc1a(i,j,k)*oc1a(i,j,k)*r_norm
          oc2c(i,j,k) = oc2c(i,j,k) + oc2a(i,j,k)*oc2a(i,j,k)*r_norm
          bc1c(i,j,k) = bc1c(i,j,k) + bc1a(i,j,k)*bc1a(i,j,k)*r_norm
          bc2c(i,j,k) = bc2c(i,j,k) + bc2a(i,j,k)*bc2a(i,j,k)*r_norm
        end do
      end do
    end do
  end do ! end do over numcases
  close(filunit1)
  close(filunit2)

! Convert to zonal mean quantities
  do k=1,nsig
    call mpi_gatherv(d1c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          d1lap(i,k) = d1lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(d2c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          d2lap(i,k) = d2lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(d3c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          d3lap(i,k) = d3lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(d4c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          d4lap(i,k) = d4lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(d5c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          d5lap(i,k) = d5lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(s1c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          s1lap(i,k) = s1lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(s2c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          s2lap(i,k) = s2lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(s3c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          s3lap(i,k) = s3lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(s4c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          s4lap(i,k) = s4lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(so4c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          so4lap(i,k) = so4lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(oc1c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          oc1lap(i,k) = oc1lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(oc2c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          oc2lap(i,k) = oc2lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(bc1c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          bc1lap(i,k) = bc1lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(bc2c(1,1,k),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        workgrd(ni1,ni2)=work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          bc2lap(i,k) = bc2lap(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  if (mype==mype_work) then
    do k=1,nsig
      do i=1,nlat
        d1hln(i,k)=(eight/d1lap(i,k))**quarter
        d2hln(i,k)=(eight/d2lap(i,k))**quarter
        d3hln(i,k)=(eight/d3lap(i,k))**quarter
        d4hln(i,k)=(eight/d4lap(i,k))**quarter
        d5hln(i,k)=(eight/d5lap(i,k))**quarter
        s1hln(i,k)=(eight/s1lap(i,k))**quarter
        s2hln(i,k)=(eight/s2lap(i,k))**quarter
        s3hln(i,k)=(eight/s3lap(i,k))**quarter
        s4hln(i,k)=(eight/s4lap(i,k))**quarter
        so4hln(i,k)=(eight/so4lap(i,k))**quarter
        oc1hln(i,k)=(eight/oc1lap(i,k))**quarter
        oc2hln(i,k)=(eight/oc2lap(i,k))**quarter
        bc1hln(i,k)=(eight/bc1lap(i,k))**quarter
        bc2hln(i,k)=(eight/bc2lap(i,k))**quarter
      end do
    end do

!! Put bounds on cloud water horizontal scales, consider aerosol
    do k=1,nsig
      do i=1,nlat
         d1hln(i,k)=max(min(5.e5_r_kind,d1hln(i,k)),2.5e3_r_kind)
         d2hln(i,k)=max(min(5.e5_r_kind,d2hln(i,k)),2.5e3_r_kind)
         d3hln(i,k)=max(min(5.e5_r_kind,d3hln(i,k)),2.5e3_r_kind)
         d4hln(i,k)=max(min(5.e5_r_kind,d4hln(i,k)),2.5e3_r_kind)
         d5hln(i,k)=max(min(5.e5_r_kind,d5hln(i,k)),2.5e3_r_kind)
         s1hln(i,k)=max(min(5.e5_r_kind,s1hln(i,k)),2.5e3_r_kind)
         s2hln(i,k)=max(min(5.e5_r_kind,s2hln(i,k)),2.5e3_r_kind)
         s3hln(i,k)=max(min(5.e5_r_kind,s3hln(i,k)),2.5e3_r_kind)
         s4hln(i,k)=max(min(5.e5_r_kind,s4hln(i,k)),2.5e3_r_kind)
         so4hln(i,k)=max(min(5.e5_r_kind,so4hln(i,k)),2.5e3_r_kind)
         oc1hln(i,k)=max(min(5.e5_r_kind,oc1hln(i,k)),2.5e3_r_kind)
         oc2hln(i,k)=max(min(5.e5_r_kind,oc2hln(i,k)),2.5e3_r_kind)
         bc1hln(i,k)=max(min(5.e5_r_kind,bc1hln(i,k)),2.5e3_r_kind)
         bc2hln(i,k)=max(min(5.e5_r_kind,bc2hln(i,k)),2.5e3_r_kind)
      end do
    end do

    call smoothlat(d1hln,nsig,smoothdeg)
    call smoothlat(d2hln,nsig,smoothdeg)
    call smoothlat(d3hln,nsig,smoothdeg)
    call smoothlat(d4hln,nsig,smoothdeg)
    call smoothlat(d5hln,nsig,smoothdeg)
    call smoothlat(s1hln,nsig,smoothdeg)
    call smoothlat(s2hln,nsig,smoothdeg)
    call smoothlat(s3hln,nsig,smoothdeg)
    call smoothlat(s4hln,nsig,smoothdeg)
    call smoothlat(so4hln,nsig,smoothdeg)
    call smoothlat(oc1hln,nsig,smoothdeg)
    call smoothlat(oc2hln,nsig,smoothdeg)
    call smoothlat(bc1hln,nsig,smoothdeg)
    call smoothlat(bc2hln,nsig,smoothdeg)
  end if ! end if mype_work

  call mpi_bcast(d1hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d2hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d3hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d4hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d5hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s1hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s2hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s3hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s4hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(so4hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(oc1hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(oc2hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(bc1hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(bc2hln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)

  return 
end subroutine horizsc_aerosol
