subroutine variances_aerosol(numcases,mype)
  use kinds, only: r_kind,r_single,i_kind
  use postmod, only: smoothlat
  use variables,only: nlat,nlon,nsig,lat1,lon1,filunit1,filunit2,zero,&
      displs_g,ijn,two,db_prec,istart,ilat1,jstart,npe,&
      d1var,d2var,d3var,d4var,d5var,s1var,s2var,s3var,s4var, &
      so4var,oc1var,oc2var,bc1var,bc2var,&
      iglobal,ltosi,ltosj,half,zero,one,ione,two,smoothdeg,vertavg
  implicit none
  include 'mpif.h'

  integer(i_kind),intent(in):: numcases,mype

  real(r_kind),dimension(lat1,lon1,nsig):: d1a,d2a,d3a,d4a,d5a, &
      s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a
  real(r_kind),dimension(lat1,lon1,nsig):: d1b,d2b,d3b,d4b,d5b, &
      s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b

  real(r_kind),dimension(lat1,lon1,nsig):: d1c,d2c,d3c,d4c,d5c, &
      s1c,s2c,s3c,s4c,so4c,oc1c,oc2c,bc1c,bc2c

  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(nlat,nlon):: workgrd

! Normalized RH variance arrays
  real(r_kind),dimension(25,nsig):: qcount,qamp,qamp2,qcount2
  real(r_kind),dimension(25):: qcavg
  real(r_kind) :: qctot,qdiff,tiny_r_kind

  real(r_kind) r_norm,r025

  integer(i_kind) i,j,k,m,n,mype_work,mm1,ierror,iret
  integer(i_kind) mpi_rtype,ix,ni1,ni2,kk,ll,ibin

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  tiny_r_kind = tiny(zero)

  mype_work=npe-1
  mm1=mype+1
  r_norm=1./float(numcases)
  ibin=25
  r025=0.25_r_kind

! Initialize subdomain variance arrays
  d1var=zero ; d2var=zero ; d3var=zero ; d4var=zero ; d5var=zero ;
  s1var=zero ; s2var=zero ; s3var=zero ; s4var=zero ;
  so4var=zero ; oc1var=zero ; oc2var=zero ; bc1var=zero ; bc2var=zero

  d1c=zero ; d2c=zero ; d3c=zero ; d4c=zero ; d5c=zero ; 
  s1c=zero ; s2c=zero ; s3c=zero ; s4c=zero ; 
  so4c=zero ; oc1c=zero ; oc2c=zero ; bc1c=zero ; bc2c=zero

  open(filunit1,form='unformatted',action='read')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='read')
  rewind(filunit2)

  do n=1,numcases
    if (mype==0)  write(6,*) 'VARIANCES, PROCESSING PAIR # ',n
! Read in subdomain grids
    read(filunit1) d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a
    read(filunit2) d1b,d2b,d3b,d4b,d5b,s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b

!    write(200+mype,*) 'after read in',so4a-s4a
!    write(250+mype,*) 'after read in',so4b-s4b
    
    call delvars_aerosol(d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a, &
                         d1b,d2b,d3b,d4b,d5b,s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b,mype)


    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          d1c(i,j,k) = d1c(i,j,k) + (d1a(i,j,k)*d1a(i,j,k))*r_norm
          d2c(i,j,k) = d2c(i,j,k) + (d2a(i,j,k)*d2a(i,j,k))*r_norm
          d3c(i,j,k) = d3c(i,j,k) + (d3a(i,j,k)*d3a(i,j,k))*r_norm
          d4c(i,j,k) = d4c(i,j,k) + (d4a(i,j,k)*d4a(i,j,k))*r_norm
          d5c(i,j,k) = d5c(i,j,k) + (d5a(i,j,k)*d5a(i,j,k))*r_norm
          s1c(i,j,k) = s1c(i,j,k) + (s1a(i,j,k)*s1a(i,j,k))*r_norm
          s2c(i,j,k) = s2c(i,j,k) + (s2a(i,j,k)*s2a(i,j,k))*r_norm
          s3c(i,j,k) = s3c(i,j,k) + (s3a(i,j,k)*s3a(i,j,k))*r_norm
          s4c(i,j,k) = s4c(i,j,k) + (s4a(i,j,k)*s4a(i,j,k))*r_norm
          so4c(i,j,k) = so4c(i,j,k) + (so4a(i,j,k)*so4a(i,j,k))*r_norm
          oc1c(i,j,k) = oc1c(i,j,k) + (oc1a(i,j,k)*oc1a(i,j,k))*r_norm
          oc2c(i,j,k) = oc2c(i,j,k) + (oc2a(i,j,k)*oc2a(i,j,k))*r_norm
          bc1c(i,j,k) = bc1c(i,j,k) + (bc1a(i,j,k)*bc1a(i,j,k))*r_norm
          bc2c(i,j,k) = bc2c(i,j,k) + (bc2a(i,j,k)*bc2a(i,j,k))*r_norm
        end do
      end do
    end do

  end do ! end do over ncases


  close(filunit1)
  close(filunit2)

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
          d1var(i,k) = d1var(i,k) + workgrd(i,j)/float(nlon)
        end do
        d1var(i,k) = max(tiny_r_kind,d1var(i,k))
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
          d2var(i,k) = d2var(i,k) + workgrd(i,j)/float(nlon)
        end do
        d2var(i,k) = max(tiny_r_kind,d2var(i,k))
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
          d3var(i,k) = d3var(i,k) + workgrd(i,j)/float(nlon)
        end do
        d3var(i,k) = max(tiny_r_kind,d3var(i,k))
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
          d4var(i,k) = d4var(i,k) + workgrd(i,j)/float(nlon)
        end do
        d4var(i,k) = max(tiny_r_kind,d4var(i,k))
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
          d5var(i,k) = d5var(i,k) + workgrd(i,j)/float(nlon)
        end do
        d5var(i,k) = max(tiny_r_kind,d5var(i,k))
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
          s1var(i,k) = s1var(i,k) + workgrd(i,j)/float(nlon)
        end do
        s1var(i,k) = max(tiny_r_kind,s1var(i,k))
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
          s2var(i,k) = s2var(i,k) + workgrd(i,j)/float(nlon)
        end do
        s2var(i,k) = max(tiny_r_kind,s2var(i,k))
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
          s3var(i,k) = s3var(i,k) + workgrd(i,j)/float(nlon)
        end do
        s3var(i,k) = max(tiny_r_kind,s3var(i,k))
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
          s4var(i,k) = s4var(i,k) + workgrd(i,j)/float(nlon)
        end do
        s4var(i,k) = max(tiny_r_kind,s4var(i,k))
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
          so4var(i,k) = so4var(i,k) + workgrd(i,j)/float(nlon)
        end do
        so4var(i,k) = max(tiny_r_kind,so4var(i,k))
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
          oc1var(i,k) = oc1var(i,k) + workgrd(i,j)/float(nlon)
        end do
        oc1var(i,k) = max(tiny_r_kind,oc1var(i,k))
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
          oc2var(i,k) = oc2var(i,k) + workgrd(i,j)/float(nlon)
        end do
        oc2var(i,k) = max(tiny_r_kind,oc2var(i,k))
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
          bc1var(i,k) = bc1var(i,k) + workgrd(i,j)/float(nlon)
        end do
        bc1var(i,k) = max(tiny_r_kind,bc1var(i,k))
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
          bc2var(i,k) = bc2var(i,k) + workgrd(i,j)/float(nlon)
        end do
        bc2var(i,k) = max(tiny_r_kind,bc2var(i,k))
      end do
    end if
  end do

  if (mype==mype_work) then
    call smoothlat(d1var,nsig,smoothdeg)
    call smoothlat(d2var,nsig,smoothdeg)
    call smoothlat(d3var,nsig,smoothdeg)
    call smoothlat(d4var,nsig,smoothdeg)
    call smoothlat(d5var,nsig,smoothdeg)
    call smoothlat(s1var,nsig,smoothdeg)
    call smoothlat(s2var,nsig,smoothdeg)
    call smoothlat(s3var,nsig,smoothdeg)
    call smoothlat(s4var,nsig,smoothdeg)
    call smoothlat(so4var,nsig,smoothdeg)
    call smoothlat(oc1var,nsig,smoothdeg)
    call smoothlat(oc2var,nsig,smoothdeg)
    call smoothlat(bc1var,nsig,smoothdeg)
    call smoothlat(bc2var,nsig,smoothdeg)
  end if

  call mpi_bcast(d1var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d2var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d3var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d4var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d5var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s1var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s2var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s3var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s4var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(so4var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(oc1var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(oc2var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(bc1var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(bc2var,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)

  return
end subroutine variances_aerosol


